use std::str::FromStr;

use either::{Either, Either::*};

use crate::{
    FormatLexer, FormatToken, Warning,
    ast::{parsed::*, *},
    ds::ComptimeInt,
    error::{Diagnostics, Error, FileId},
    intern::{StrId, Strings},
    lexer::{Lexer, Located, Precedence, Span, Token},
};

#[derive(Default, Clone, Copy, PartialEq, Eq)]
enum Body {
    #[default]
    Optional,
    Required,
    None,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Safety {
    Safe,
    Unsafe,
}

#[derive(Default, Clone, Copy)]
struct FnConfig {
    abi: FnAbi,
    vis: Visibility,
    is_unsafe: bool,
    body: Body,
    lead_span: Option<Span>,
    unsafe_if_no_body: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum EvalContext {
    For,
    IfWhile,
    Normal,
}

pub struct ModuleAttributes {
    pub attrs: Attributes,
    pub vis: Visibility,
}

impl Default for ModuleAttributes {
    fn default() -> Self {
        Self { attrs: Default::default(), vis: Visibility::Public }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peek: Option<Located<Token>>,
    needs_sync: bool,
    diag: &'a mut Diagnostics,
    strings: &'a mut Strings,
    arena: &'a mut ExprArena,
}

impl<'a> Parser<'a> {
    fn new(
        src: &'a str,
        diag: &'a mut Diagnostics,
        strings: &'a mut Strings,
        arena: &'a mut ExprArena,
        file: FileId,
    ) -> Self {
        Self { diag, strings, lexer: Lexer::new(src, file), peek: None, needs_sync: false, arena }
    }

    pub fn parse(
        src: &'a str,
        name: StrId,
        diag: &'a mut Diagnostics,
        strings: &'a mut Strings,
        arena: &'a mut ExprArena,
        file: FileId,
        attrs: ModuleAttributes,
    ) -> Stmt {
        let mut this = Self::new(src, diag, strings, arena, file);
        let mut stmts = Vec::new();
        while !this.matches(Token::Eof) {
            stmts.push(this.item());
            if this.needs_sync {
                this.synchronize();
            }
        }

        Stmt {
            data: Located::new(
                Span { file, pos: 0, len: src.len() as u32 },
                StmtData::Module {
                    vis: attrs.vis,
                    body: stmts,
                    name: Located::new(Span { file, pos: 0, len: 0 }, name),
                    file: true,
                },
            ),
            attrs: attrs.attrs,
        }
    }

    //

    fn try_item(&mut self) -> Result<Stmt, (Option<Span>, Attributes)> {
        let mut attrs = self.attributes();
        let (tk_public, vis) = self.visiblity();
        let tk_extern = self.next_if(Token::Extern);
        let abi = self.abi(tk_extern.is_some());
        if let Some(begin) = tk_extern
            && self.next_if(Token::LCurly).is_some()
        {
            self.invalid_here(tk_public);
            let mut stmts = vec![];
            let def_abi = abi.map(|abi| abi.data).unwrap_or(FnAbi::C);
            let span = self.next_until(Token::RCurly, begin, |this| {
                let mut fn_attrs = this.attributes();
                fn_attrs.extend(attrs.iter().cloned());

                let (tk_public, vis) = this.visiblity();
                let tk_extern = this.next_if(Token::Extern);
                let fn_abi = this.abi(tk_extern.is_some());
                let abi = abi.map(|abi| abi.data).unwrap_or(FnAbi::C);
                if let Some(mut span) = tk_extern
                    && abi == def_abi
                {
                    if let Some(abi_span) = fn_abi.map(|a| a.span) {
                        span.extend_to(abi_span);
                    }
                    this.diag.report(Warning::useless_token(this.lexer.source(), span));
                }

                let safety = this.safety();
                // if let Some(safety) = safety
                //     && safety.data == Safety::Unsafe
                // {
                //     this.diag.report(Warning::useless_token(this.lexer.source(), safety.span));
                // }

                // TODO: also allow statics
                let func = this.expect_fn(
                    FnConfig {
                        abi: fn_abi.map(|abi| abi.data).unwrap_or(def_abi),
                        vis,
                        is_unsafe: safety.is_none_or(|s| s.data != Safety::Safe),
                        body: Body::None,
                        lead_span: tk_public.or(tk_extern).or(safety.map(|s| s.span)),
                        unsafe_if_no_body: false,
                    },
                    fn_attrs,
                );

                if let Some(func) = func {
                    stmts.push(this.normal_fn(func));
                }
            });
            return Ok(Stmt {
                data: Located::new(span, StmtData::ExternBlock(stmts)),
                attrs: Default::default(),
            });
        }

        let safety = self.safety();
        let is_extern = tk_extern.is_some();
        let mut tk_unsafe = safety.map(|s| s.span);
        let earliest_span = tk_public.or(tk_extern).or(tk_unsafe);
        let conf = FnConfig {
            abi: abi.map(|abi| abi.data).unwrap_or(if is_extern { FnAbi::C } else { FnAbi::Ctl }),
            vis,
            is_unsafe: safety.is_some_and(|s| s.data == Safety::Unsafe),
            body: if is_extern { Body::Optional } else { Body::Required },
            lead_span: earliest_span,
            unsafe_if_no_body: is_extern && !safety.is_some_and(|s| s.data == Safety::Safe),
        };
        if let Some(func) = self.try_function(conf, attrs.clone()) {
            return Ok(self.normal_fn(func));
        }

        if safety.is_some_and(|s| s.data == Safety::Safe) {
            self.invalid_here(tk_unsafe);
            tk_unsafe = None;
        }

        let peek = self.peek();
        let earliest_span = earliest_span.unwrap_or(peek.span);
        match peek.data {
            Token::Type => {
                self.next();
                self.invalid_here(tk_unsafe);
                self.invalid_here(tk_extern);

                let name = self.expect_ident("expected name");
                let type_params = self.type_params();
                self.expect(Token::Assign);
                let ty = self.type_hint();
                self.expect(Token::Semicolon);
                Ok(Stmt {
                    data: Located::new(
                        earliest_span.extended_to(ty.span),
                        StmtData::Alias { vis, name, type_params, ty },
                    ),
                    attrs,
                })
            }
            Token::Struct | Token::Packed => {
                let token = self.next();
                if token.data == Token::Packed {
                    self.expect(Token::Struct);
                }

                self.invalid_here(tk_unsafe);
                self.invalid_here(tk_extern);

                Ok(Stmt {
                    attrs,
                    data: self.structure(vis, earliest_span, false).map(|base| StmtData::Struct {
                        base,
                        packed: matches!(token.data, Token::Packed),
                    }),
                })
            }
            Token::Union => {
                self.next();
                self.invalid_here(tk_extern);

                Ok(Stmt {
                    attrs,
                    data: if tk_unsafe.is_some() {
                        self.structure(vis, earliest_span, true).map(StmtData::UnsafeUnion)
                    } else {
                        self.union(vis, earliest_span)
                    },
                })
            }
            Token::Trait | Token::Sealed => {
                let seal = if self.next().data == Token::Sealed {
                    let library = self.next_if(Token::LParen).map(|_| {
                        self.expect(Token::Lib);
                        self.expect(Token::RParen);
                        Visibility::Library
                    });
                    self.expect(Token::Trait);
                    library.unwrap_or(Visibility::Private)
                } else {
                    Visibility::Public
                };

                self.invalid_here(tk_extern);

                Ok(Stmt {
                    attrs,
                    data: self.r#trait(vis, earliest_span, tk_unsafe.is_some(), seal),
                })
            }
            Token::Extension => {
                let span = self.next().span;
                self.invalid_here(tk_public);
                self.invalid_here(tk_unsafe);
                self.invalid_here(tk_extern);

                Ok(Stmt { attrs, data: self.extension(earliest_span, span) })
            }
            Token::Mod => {
                self.next();
                self.invalid_here(tk_unsafe);
                self.invalid_here(tk_extern);

                let name = self.expect_ident("expected name");
                if self.next_if(Token::LCurly).is_some() {
                    let mut body = Vec::new();
                    let span = self
                        .next_until(Token::RCurly, earliest_span, |this| body.push(this.item()));
                    Ok(Stmt {
                        data: Located::new(
                            earliest_span.extended_to(span),
                            StmtData::Module { vis, file: false, name, body },
                        ),
                        attrs,
                    })
                } else {
                    let end = self.expect(Token::Semicolon);
                    Ok(Stmt {
                        data: Located::new(
                            earliest_span.extended_to(end),
                            StmtData::ModuleOOL { vis, name, resolved: false },
                        ),
                        attrs,
                    })
                }
            }
            Token::Use => {
                self.next();
                self.invalid_here(tk_unsafe);
                self.invalid_here(tk_extern);

                let origin = if let Some(token) = self.next_if(Token::ScopeRes) {
                    UsePathOrigin::Root(token)
                } else if let Some(token) = self.next_if(Token::Super) {
                    self.expect(Token::ScopeRes);
                    UsePathOrigin::Super(token)
                } else {
                    UsePathOrigin::Here
                };
                let path = UsePath { vis, origin, component: self.use_path_component() };
                let end = self.expect(Token::Semicolon);
                Ok(Stmt {
                    data: Located::new(earliest_span.extended_to(end), StmtData::Use(path)),
                    attrs,
                })
            }
            Token::Static | Token::Const => {
                let token = self.next();
                self.invalid_here(tk_unsafe);
                self.invalid_here(tk_extern.filter(|_| token.data.is_const()));

                let mutable = self.next_if(Token::Mut);
                self.invalid_here(mutable.filter(|_| token.data.is_const()));

                let name = self.expect_ident("expected name");
                self.expect(Token::Colon);
                let ty = self.type_hint();
                let value = self.next_if(Token::Assign).map(|_| self.expression());
                let end = self.expect(Token::Semicolon);
                Ok(Stmt {
                    data: Located::new(
                        earliest_span.extended_to(end),
                        StmtData::Binding {
                            vis,
                            constant: matches!(token.data, Token::Const),
                            is_extern: tk_extern.is_some(),
                            mutable: mutable.is_some(),
                            name,
                            ty,
                            value,
                        },
                    ),
                    attrs,
                })
            }
            Token::UnitTest => {
                self.next();
                self.invalid_here(tk_public);
                self.invalid_here(tk_unsafe);
                self.invalid_here(tk_extern);

                let name = self.next();
                let name = if let Token::String(data) = name.data {
                    let name = Located::new(name.span, data);
                    let test = self.strings.get_or_intern_static("test");
                    attrs.push(Attribute {
                        name: Located::nowhere(AttrName::Str(Strings::ATTR_CFG)),
                        props: vec![Attribute {
                            name: Located::nowhere(AttrName::Str(test)),
                            props: vec![],
                        }],
                    });
                    name
                } else {
                    self.error(Error::new("expected string literal", name.span));
                    name.map(|_| Strings::EMPTY)
                };

                let lcurly = self.expect(Token::LCurly);
                let body = self.block_expr(lcurly, None);
                Ok(Stmt {
                    data: Located::new(
                        earliest_span.extended_to(body.span),
                        StmtData::Fn(Fn {
                            attrs: Default::default(),
                            vis: Visibility::Internal,
                            name,
                            abi: FnAbi::Ctl,
                            is_async: false,
                            is_unsafe: false,
                            variadic: false,
                            typ: FunctionType::Test,
                            type_params: vec![],
                            params: vec![],
                            ret: None,
                            body: Some(body),
                        }),
                    ),
                    attrs,
                })
            }
            _ => {
                self.invalid_here(tk_extern);
                Err((tk_unsafe, attrs))
            }
        }
    }

    fn item(&mut self) -> Stmt {
        self.try_item().unwrap_or_else(|(_, attrs)| {
            let span = self.next().span;
            self.error(Error::new("expected item", span));
            Stmt { data: Located::new(span, StmtData::Error), attrs }
        })
    }

    fn statement(&mut self) -> Stmt {
        let stmt = match self.try_item() {
            Ok(item) => item,
            Err((is_unsafe, attrs)) => match self.peek().data {
                Token::Let | Token::Mut => {
                    let token = self.next();
                    self.invalid_here(is_unsafe);

                    let patt = self.pattern(matches!(token.data, Token::Mut));
                    let ty = self.next_if(Token::Colon).map(|_| self.type_hint());
                    let value = self.next_if(Token::Assign).map(|_| self.expression());
                    let end = self.expect(Token::Semicolon);
                    Stmt {
                        data: Located::new(
                            token.span.extended_to(end),
                            StmtData::Let { ty, value, patt },
                        ),
                        attrs,
                    }
                }
                Token::Defer => {
                    let mut span = self.next().span;

                    let (needs_semicolon, expr) = self.block_or_normal_expr(None);
                    if needs_semicolon {
                        span.extend_to(self.expect(Token::Semicolon));
                    } else {
                        span.extend_to(expr.span);
                    }
                    Stmt { attrs, data: Located::new(span, StmtData::Defer(expr)) }
                }
                Token::Guard => {
                    let span = self.next().span;
                    let cond = self.expression();
                    self.expect(Token::Else);
                    let lcurly = self.expect(Token::LCurly);
                    let body = self.block_expr(lcurly, None);
                    Stmt {
                        data: Located::new(
                            span.extended_to(body.span),
                            StmtData::Guard { cond, body },
                        ),
                        attrs,
                    }
                }
                _ => {
                    let (needs_semicolon, mut expr) = self.block_or_normal_expr(None);
                    let mut span = expr.span;
                    if let Some(is_unsafe) = is_unsafe {
                        span = is_unsafe.extended_to(span);
                        expr = self.arena.expr(span, ExprData::Unsafe(expr));
                    }

                    if self.matches(Token::RCurly) {
                        expr = self.arena.expr(expr.span, ExprData::Tail(expr));
                    } else if needs_semicolon {
                        span.extend_to(self.expect(Token::Semicolon));
                    }

                    Stmt { attrs, data: Located::new(span, StmtData::Expr(expr)) }
                }
            },
        };

        self.next_if(Token::Semicolon);
        stmt
    }

    //

    fn expression(&mut self) -> Expr {
        self.precedence(Precedence::Min, EvalContext::Normal)
    }

    fn precedence(&mut self, prec: Precedence, ctx: EvalContext) -> Expr {
        let mut expr = self.prefix(ctx);
        while let Some(token) = self.next_if_pred(|tk| prec < tk.precedence()) {
            expr = self.infix(expr, token, ctx);
        }
        expr
    }

    fn prefix(&mut self, ctx: EvalContext) -> Expr {
        let Located { mut span, data } = self.next();
        match data {
            // Literals
            Token::Void => self.arena.expr(span, ExprData::Void),
            Token::False => self.arena.expr(span, ExprData::Bool(false)),
            Token::True => self.arena.expr(span, ExprData::Bool(true)),
            Token::Int { base, value, width } => {
                let value = self.parse_int(base, value, span);
                self.arena
                    .expr(span, ExprData::Integer(IntPattern { value, width, negative: false }))
            }
            Token::Float { value, suffix } => {
                let mut value = self.strings.resolve(&value).to_string();
                value.retain(|c| c != '_');
                let value = value.parse::<f64>().unwrap_or_else(|_| {
                    self.error_no_sync(Error::new(
                        format!("'{value}' is not a valid float literal"),
                        span,
                    ));
                    0.0
                });
                self.arena
                    .expr(span, ExprData::Float(FloatPattern { negative: false, value, suffix }))
            }
            Token::String(v) => self.arena.expr(span, ExprData::String(v)),
            Token::Char(v) => self.arena.expr(span, ExprData::Char(v)),
            Token::ByteString(v) => self.arena.expr(span, ExprData::ByteString(v)),
            Token::ByteChar(v) => self.arena.expr(span, ExprData::ByteChar(v)),
            Token::This => self
                .arena
                .expr(span, ExprData::Path(Located::new(span, Strings::THIS_PARAM).into())),
            Token::ThisType => {
                let origin = PathOrigin::This(span);
                let data = self.path_components(None, &mut span);
                self.arena.expr(span, ExprData::Path(Path::new(origin, data)))
            }
            Token::Ident(ident) => {
                let data = self.path_components(Some(Located::new(span, ident)), &mut span);
                self.arena.expr(span, ExprData::Path(Path::new(PathOrigin::Normal, data)))
            }
            Token::ScopeRes => {
                let origin = PathOrigin::Root(span);
                let ident = self.expect_ident("expected name");
                let data = self.path_components(Some(ident), &mut span);
                self.arena.expr(span, ExprData::Path(Path::new(origin, data)))
            }
            Token::Super => {
                let origin = PathOrigin::Super(span);
                self.expect(Token::ScopeRes);
                let ident = self.expect_ident("expected name");
                let data = self.path_components(Some(ident), &mut span);
                self.arena.expr(span, ExprData::Path(Path::new(origin, data)))
            }
            Token::Colon => {
                let origin = PathOrigin::Infer(span);
                let ident = self.expect_ident("expected identifier");
                self.arena.expr(
                    span.extended_to(ident.span),
                    ExprData::Path(Path::new(origin, vec![(ident, Default::default())])),
                )
            }
            Token::StringPart(value) => {
                let mut strings = vec![value];
                let mut args = vec![];
                'outer: loop {
                    let expr = self.expression();
                    let specifier = self.next_if(Token::Colon).map(|_| self.format_opts());
                    args.push((expr, specifier));

                    let mut begin = None::<Span>;
                    loop {
                        let peek = self.peek();
                        match peek.data {
                            Token::String(s) | Token::StringPart(s) => {
                                span.extend_to(peek.span);
                                self.next();
                                if let Some(begin) = begin {
                                    self.error(Error::new("expected format specifiers", begin));
                                }

                                strings.push(s);
                                if peek.data.is_string() {
                                    break 'outer;
                                } else {
                                    break;
                                }
                            }
                            Token::Eof => {
                                self.error_no_sync(Error::new(
                                    "unterminated string interpolation",
                                    peek.span,
                                ));
                                break 'outer;
                            }
                            _ => {
                                self.next();
                                if let Some(begin) = &mut begin {
                                    begin.extend_to(peek.span);
                                } else {
                                    begin = Some(peek.span);
                                }
                            }
                        }
                    }
                }

                self.arena.expr(span, ExprData::StringInterpolation { strings, args })
            }
            // prefix operators
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Ampersand
            | Token::Increment
            | Token::Decrement
            | Token::Exclamation
            | Token::Question
            | Token::NoneCoalesce => {
                let mut double_opt = false;
                let op = if data == Token::Ampersand && self.next_if(Token::Mut).is_some() {
                    UnaryOp::AddrMut
                } else if data == Token::Ampersand && self.next_if(Token::Raw).is_some() {
                    if self.next_if(Token::Mut).is_some() {
                        UnaryOp::AddrRawMut
                    } else {
                        UnaryOp::AddrRaw
                    }
                } else if data == Token::NoneCoalesce {
                    double_opt = true;
                    UnaryOp::Option
                } else {
                    UnaryOp::try_from_prefix(data).unwrap()
                };

                let mut expr = self.precedence(Precedence::Prefix, ctx);
                if matches!(op, UnaryOp::Neg)
                    && let ExprData::Integer(patt) = self.arena.get_mut(expr.data)
                {
                    expr.span = span.extended_to(expr.span);
                    patt.negative = true;
                    return expr;
                }

                if double_opt {
                    expr = self.arena.expr(expr.span, ExprData::Unary { op, expr });
                }

                self.arena.expr(span.extended_to(expr.span), ExprData::Unary { op, expr })
            }
            // complex expressions
            Token::LParen => {
                if let Some(end) = self.next_if(Token::RParen) {
                    return self.arena.expr(span.extended_to(end), ExprData::Tuple(vec![]));
                }

                let (label, expr) = self.maybe_labeled_expr();
                if label.is_some() || self.matches(Token::Comma) {
                    let mut next = 0;
                    let first = self.label_or_positional((label, expr), &mut next);
                    self.csv_expr(
                        vec![first],
                        Token::RParen,
                        span,
                        |this| {
                            let res = this.maybe_labeled_expr();
                            this.label_or_positional(res, &mut next)
                        },
                        ExprData::Tuple,
                    )
                } else {
                    let end = self.expect(Token::RParen);
                    if matches!(self.peek().data, Token::LParen | Token::LBrace) {
                        self.arena.expr(span.extended_to(end), ExprData::Grouping(expr))
                    } else {
                        expr
                    }
                }
            }
            Token::Range => {
                if self.is_range_end(ctx) {
                    self.arena
                        .expr(span, ExprData::Range { start: None, end: None, inclusive: false })
                } else {
                    let end = self.precedence(data.precedence(), ctx);
                    self.arena.expr(
                        span.extended_to(end.span),
                        ExprData::Range { start: None, end: Some(end), inclusive: false },
                    )
                }
            }
            Token::RangeInclusive => {
                let end = self.precedence(data.precedence(), ctx);
                self.arena.expr(
                    span.extended_to(end.span),
                    ExprData::Range { start: None, end: Some(end), inclusive: true },
                )
            }
            Token::LCurly => self.block_expr(span, None),
            Token::If => self.if_expr(span),
            Token::While => self.while_expr(span, None),
            Token::Loop => self.loop_expr(span, None),
            Token::For => self.for_expr(span, None),
            Token::Match => self.match_expr(span),
            Token::LBrace => 'out: {
                let expr = if let Some(rbrace) = self.next_if(Token::RBrace) {
                    break 'out self
                        .arena
                        .expr(span.extended_to(rbrace), ExprData::Array(Vec::new()));
                } else if let Some(colon) = self.next_if(Token::Colon) {
                    if let Some(ident) = self.next_if_ident() {
                        let origin = PathOrigin::Infer(colon);
                        self.arena.expr(
                            colon.extended_to(ident.span),
                            ExprData::Path(Path::new(origin, vec![(ident, Default::default())])),
                        )
                    } else {
                        let end = self.expect(Token::RBrace);
                        break 'out self
                            .arena
                            .expr(span.extended_to(end), ExprData::Map(Vec::new()));
                    }
                } else {
                    self.expression()
                };

                if self.next_if(Token::Colon).is_some() {
                    let value = self.expression();
                    self.csv_expr(
                        vec![(expr, value)],
                        Token::RBrace,
                        span,
                        |this| {
                            let key = this.expression();
                            this.expect(Token::Colon);
                            (key, this.expression())
                        },
                        ExprData::Map,
                    )
                } else if self.next_if(Token::Semicolon).is_some() {
                    let count = self.expression();
                    let rbrace = self.expect(Token::RBrace);
                    self.arena.expr(
                        span.extended_to(rbrace),
                        ExprData::ArrayWithInit { init: expr, count },
                    )
                } else if let Some(rbrace) = self.next_if(Token::RBrace) {
                    self.arena.expr(span.extended_to(rbrace), ExprData::Array(vec![expr]))
                } else {
                    self.csv_expr(
                        vec![expr],
                        Token::RBrace,
                        span,
                        Self::expression,
                        ExprData::Array,
                    )
                }
            }
            Token::AtLBrace => {
                if let Some(rbrace) = self.next_if(Token::RBrace) {
                    self.arena.expr(span.extended_to(rbrace), ExprData::Vec(Vec::new()))
                } else {
                    let expr = self.expression();
                    if self.next_if(Token::Semicolon).is_some() {
                        let count = self.expression();
                        let rbrace = self.expect(Token::RBrace);
                        self.arena.expr(
                            span.extended_to(rbrace),
                            ExprData::VecWithInit { init: expr, count },
                        )
                    } else {
                        self.csv_expr(
                            vec![expr],
                            Token::RBrace,
                            span,
                            Self::expression,
                            ExprData::Vec,
                        )
                    }
                }
            }
            Token::At => {
                let label = self.expect_ident("expected label identifier or vector literal");
                self.expect(Token::Colon);
                self.block_or_normal_expr(Some(label)).1
            }
            Token::Hash => {
                self.expect(Token::LBrace);
                self.csv_expr(Vec::new(), Token::RBrace, span, Self::expression, ExprData::Set)
            }
            Token::BitOr | Token::BitOrAssign => self.closure_expr(data, span),
            Token::Return => {
                let (span, expr) = if !self.is_range_end(EvalContext::Normal) {
                    let expr = self.expression();
                    (span.extended_to(expr.span), expr)
                } else {
                    (span, self.arena.expr(span, ExprData::Void))
                };

                self.arena.expr(span, ExprData::Return(expr))
            }
            Token::Break => {
                let label =
                    self.next_if(Token::At).map(|_| self.expect_ident("expected label name"));
                let (span, expr) = if !self.is_range_end(EvalContext::Normal) {
                    let expr = self.expression();
                    (span.extended_to(expr.span), Some(expr))
                } else {
                    (span, None)
                };

                self.arena.expr(span, ExprData::Break(expr, label))
            }
            Token::Unsafe => {
                let expr = self.precedence(Precedence::Min, ctx);
                self.arena.expr(span.extended_to(expr.span), ExprData::Unsafe(expr))
            }
            Token::Continue => {
                let label =
                    self.next_if(Token::At).map(|_| self.expect_ident("expected label name"));
                self.arena.expr(span, ExprData::Continue(label))
            }
            _ => {
                self.error(Error::new("unexpected token", span));
                Located::new(span, ExprArena::EXPR_ERROR)
            }
        }
    }

    fn infix(&mut self, left: Expr, op: Located<Token>, ctx: EvalContext) -> Expr {
        match op.data {
            Token::Increment | Token::Decrement | Token::Exclamation | Token::Question => {
                self.arena.expr(
                    left.span.extended_to(op.span),
                    ExprData::Unary { op: UnaryOp::try_from_postfix(op.data).unwrap(), expr: left },
                )
            }
            Token::Or
            | Token::And
            | Token::BitOr
            | Token::Ampersand
            | Token::Caret
            | Token::Shl
            | Token::Shr
            | Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Div
            | Token::Rem
            | Token::RAngle
            | Token::GtEqual
            | Token::LAngle
            | Token::LtEqual
            | Token::Equal
            | Token::NotEqual
            | Token::Spaceship => {
                let right = self.precedence(op.data.precedence(), ctx);
                self.arena.expr(
                    left.span.extended_to(right.span),
                    ExprData::Binary { op: op.data.try_into().unwrap(), left, right },
                )
            }
            Token::NoneCoalesce
            | Token::Assign
            | Token::AddAssign
            | Token::SubAssign
            | Token::MulAssign
            | Token::DivAssign
            | Token::RemAssign
            | Token::BitAndAssign
            | Token::BitOrAssign
            | Token::XorAssign
            | Token::ShlAssign
            | Token::ShrAssign
            | Token::NoneCoalesceAssign => {
                let right = self.precedence(
                    match op.data.precedence() {
                        Precedence::NoneCoalesce => Precedence::Comparison,
                        Precedence::Assignment => Precedence::Min,
                        prec => prec,
                    },
                    ctx,
                );
                self.arena.expr(
                    left.span.extended_to(right.span),
                    ExprData::Binary { op: op.data.try_into().unwrap(), left, right },
                )
            }
            Token::Range | Token::RangeInclusive => {
                let inclusive = op.data == Token::RangeInclusive;
                if !inclusive && self.is_range_end(ctx) {
                    self.arena.expr(
                        left.span.extended_to(op.span),
                        ExprData::Range { start: Some(left), end: None, inclusive },
                    )
                } else {
                    let right = self.precedence(op.data.precedence(), ctx);
                    self.arena.expr(
                        left.span.extended_to(right.span),
                        ExprData::Range { start: Some(left), end: Some(right), inclusive },
                    )
                }
            }
            Token::Is => {
                let pattern = self.pattern_ex(false, ctx);
                self.arena.expr(left.span, ExprData::Is { expr: left, pattern })
            }
            Token::As => {
                let bang = self.next_if(Token::Exclamation);
                self.invalid_here(bang);

                let ty = self.type_hint();
                self.arena.expr(left.span.extended_to(ty.span), ExprData::As { expr: left, ty })
            }
            Token::Dot => {
                let token = self.peek();
                if let Token::Int { base, value, width } = token.data {
                    let tspan = token.span;
                    let data = self.strings.resolve(&value);
                    if base != 10 || width.is_some() || (data.starts_with('0') && data.len() > 1) {
                        self.error(Error::new(
                            "tuple member access must be an integer with no prefix or suffix",
                            tspan,
                        ));
                    }
                    let span = self.next().span;
                    return self.arena.expr(
                        left.span.extended_to(span),
                        ExprData::Member {
                            source: left,
                            member: Located::new(tspan, value),
                            generics: Vec::new(),
                        },
                    );
                }

                let member = self.expect_ident("expected member name");
                let generics = if self.next_if(Token::ScopeRes).is_some() {
                    self.expect(Token::LAngle);
                    self.rangle_csv_one(left.span, Self::type_hint)
                } else {
                    Located::new(left.span.extended_to(member.span), Vec::new())
                };

                generics.map(|generics| {
                    self.arena.alloc(ExprData::Member { source: left, member, generics })
                })
            }
            Token::LParen | Token::LBrace => {
                let closing = if op.data == Token::LParen { Token::RParen } else { Token::RBrace };
                let args = self.csv(Vec::new(), closing, left.span, Self::maybe_labeled_expr);
                if op.data == Token::LParen {
                    args.map(|args| self.arena.alloc(ExprData::Call { callee: left, args }))
                } else {
                    args.map(|args| self.arena.alloc(ExprData::Subscript { callee: left, args }))
                }
            }
            Token::Then => {
                let if_branch = self.expression();
                let else_branch =
                    self.next_if(Token::Else).map(|_| self.precedence(Precedence::Min, ctx)); /* EvalContext::IfWhile? */
                self.arena.expr(
                    left.span.extended_to(else_branch.map_or(if_branch.span, |e| e.span)),
                    ExprData::If { cond: left, if_branch, else_branch },
                )
            }
            _ => {
                self.error(Error::new("unexpected token", op.span));
                Located::new(op.span, ExprArena::EXPR_ERROR)
            }
        }
    }

    //

    fn label_error(&mut self, span: Span) {
        self.error_no_sync(Error::new(
            "labels are only valid for loop and block expressions",
            span,
        ));
    }

    fn label_or_positional<T>(
        &mut self,
        ipt: (Option<Located<StrId>>, Located<T>),
        next: &mut usize,
    ) -> (Located<StrId>, Located<T>) {
        if let Some(label) = ipt.0 {
            (label, ipt.1)
        } else {
            let label = Located::new(ipt.1.span, self.strings.get_or_intern(format!("{}", *next)));
            *next += 1;
            (label, ipt.1)
        }
    }

    fn maybe_labeled_expr(&mut self) -> (Option<Located<StrId>>, Expr) {
        let mut expr = self.expression();
        let mut name = None;
        if let ExprData::Path(path) = self.arena.get(expr.data)
            && let Some(ident) = path.as_identifier()
            && self.next_if(Token::Colon).is_some()
        {
            name = Some(ident);
            if !matches!(self.peek().data, Token::Comma | Token::RParen) {
                expr = self.expression();
            }
        }
        (name, expr)
    }

    fn block_or_normal_expr(&mut self, label: Option<Located<StrId>>) -> (bool, Expr) {
        let label = label.or_else(|| {
            self.next_if(Token::At).map(|_| {
                let label = self.expect_ident("expected label identifier");
                self.expect(Token::Colon);
                label
            })
        });
        match self.peek().data {
            Token::If => {
                if let Some(label) = label {
                    self.label_error(label.span)
                }
                let token = self.next();
                (false, self.if_expr(token.span))
            }
            Token::While => {
                let token = self.next();
                (false, self.while_expr(token.span, label.map(|l| l.data)))
            }
            Token::Loop => {
                let token = self.next();
                let expr = self.loop_expr(token.span, label.map(|l| l.data));
                (matches!(self.arena.get(expr.data), ExprData::Loop { do_while: true, .. }), expr)
            }
            Token::For => {
                let token = self.next();
                (false, self.for_expr(token.span, label.map(|l| l.data)))
            }
            Token::Match => {
                let token = self.next();
                if let Some(label) = label {
                    self.label_error(label.span)
                }
                (false, self.match_expr(token.span))
            }
            Token::LCurly => {
                let token = self.next();
                (false, self.block_expr(token.span, label.map(|l| l.data)))
            }
            Token::Unsafe => {
                let begin = self.next();
                if let Some(label) = label {
                    self.label_error(label.span)
                }

                let (needs_delim, expr) = self.block_or_normal_expr(None);
                (
                    needs_delim,
                    self.arena.expr(begin.span.extended_to(expr.span), ExprData::Unsafe(expr)),
                )
            }
            _ => {
                if let Some(label) = label {
                    self.label_error(label.span)
                }
                (true, self.expression())
            }
        }
    }

    fn if_expr(&mut self, token: Span) -> Expr {
        let cond = self.precedence(Precedence::Min, EvalContext::IfWhile);
        let lcurly = self.expect(Token::LCurly);
        let if_branch = self.block_expr(lcurly, None);
        let else_branch = self.next_if(Token::Else).map(|_| {
            if !self.matches(Token::If) {
                let lcurly = self.expect(Token::LCurly);
                self.block_expr(lcurly, None)
            } else {
                self.precedence(Precedence::Min, EvalContext::IfWhile)
            }
        });
        self.arena.expr(
            token.extended_to(else_branch.map_or(if_branch.span, |e| e.span)),
            ExprData::If { cond, if_branch, else_branch },
        )
    }

    fn while_expr(&mut self, token: Span, label: Option<StrId>) -> Expr {
        let cond = self.precedence(Precedence::Min, EvalContext::IfWhile);
        let Located { span, data: body } = self.block(None);
        self.arena.expr(
            token.extended_to(span),
            ExprData::Loop { cond: Some(cond), body, do_while: false, label },
        )
    }

    fn loop_expr(&mut self, mut token: Span, label: Option<StrId>) -> Expr {
        let body = self.block(None).data;
        let (cond, do_while) = self
            .next_if(Token::While)
            .map(|_| {
                let cond = self.expression();
                token.extend_to(cond.span);
                (Some(cond), true)
            })
            .unwrap_or_default();

        self.arena.expr(token, ExprData::Loop { cond, body, do_while, label })
    }

    fn for_expr(&mut self, token: Span, label: Option<StrId>) -> Expr {
        let patt = self.pattern(false);
        self.expect(Token::In);
        let iter = self.precedence(Precedence::Min, EvalContext::For);
        let Located { span, data: body } = self.block(None);
        self.arena.expr(token.extended_to(span), ExprData::For { patt, iter, body, label })
    }

    fn match_expr(&mut self, token: Span) -> Expr {
        let expr = self.expression();
        self.expect(Token::LCurly);
        let mut body = Vec::new();
        let span = self.next_until(Token::RCurly, token, |this| {
            let pattern = FullPattern {
                data: this.pattern(false),
                if_expr: this.next_if(Token::If).map(|_| this.expression()),
            };
            this.expect(Token::FatArrow);
            let (needs_comma, expr) = this.block_or_normal_expr(None);
            if needs_comma {
                if !this.matches(Token::RCurly) {
                    this.expect(Token::Comma);
                }
            } else {
                this.next_if(Token::Comma);
            }

            body.push((pattern, expr));
        });

        self.arena.expr(span, ExprData::Match { expr, body })
    }

    fn block_expr(&mut self, token: Span, label: Option<StrId>) -> Expr {
        let block = self.block(Some(token));
        self.arena.expr(block.span, ExprData::Block(block.data, label))
    }

    fn closure_expr(&mut self, token: Token, head: Span) -> Expr {
        use std::cell::Cell;

        let mut captures = vec![];
        let policy = Cell::new(None);
        let set_policy = |this: &mut Self, new_policy: DefaultCapturePolicy, span: Span| {
            if policy.replace(Some(new_policy)).is_some() {
                this.error_no_sync(Error::new("duplicate capture policy", span));
            }
        };

        let capture_expr = |this: &mut Self,
                            ident: Located<StrId>,
                            mutable: bool,
                            is_unsafe: Option<Span>,
                            addr: Option<UnaryOp>,
                            captures: &mut Vec<Capture>| {
            let mut expr = this.arena.expr(ident.span, ExprData::Path(Path::from(ident)));
            if let Some(op) = addr {
                expr = this.arena.expr(ident.span, ExprData::Unary { op, expr });
            }
            if let Some(span) = is_unsafe {
                expr = this.arena.expr(span, ExprData::Unsafe(expr));
            }
            captures.push(Capture { mutable, ident, expr });
        };

        let by_value_helper = |this: &mut Self, start: Span, captures: &mut Vec<Capture>| {
            let is_unsafe = this.next_if(Token::Unsafe);
            let mutable = this.next_if(Token::Mut);
            let ident = this.next_if_map(|_, next| match next.data {
                Token::Ident(i) => Some(Located::new(next.span, i)),
                Token::This => Some(Located::new(next.span, Strings::THIS_PARAM)),
                _ => None,
            });
            let span = start.extended_to(ident.map(|m| m.span).or(mutable).unwrap_or(start));
            let mutable = mutable.is_some();
            if let Some(ident) = ident
                && this.next_if(Token::Assign).is_some()
            {
                captures.push(Capture { mutable, ident, expr: this.expression() });
                return;
            }

            match (mutable, ident) {
                (true, None) => set_policy(this, DefaultCapturePolicy::ByValMut, span),
                (false, None) => set_policy(this, DefaultCapturePolicy::ByVal, span),
                (mutable, Some(ident)) => {
                    capture_expr(this, ident, mutable, is_unsafe, None, captures)
                }
            }
        };

        if token == Token::BitOrAssign {
            let mut span = Span { ..head };
            span.pos += 1;
            span.len -= 1;
            by_value_helper(self, span, &mut captures);
            if !self.matches(Token::BitOr) {
                self.expect(Token::Comma);
            }
        }

        let mut params = vec![];
        self.csv(vec![], Token::BitOr, head, |this| match this.peek().data {
            Token::Ampersand => {
                let start = this.next();
                let is_unsafe = this.next_if(Token::Unsafe);
                let mutable = this.next_if(Token::Mut);
                let ident = this.next_if_map(|_, next| match next.data {
                    Token::Ident(i) => Some(Located::new(next.span, i)),
                    Token::This => Some(Located::new(next.span, Strings::THIS_PARAM)),
                    _ => None,
                });
                let span =
                    start.span.extended_to(ident.map(|m| m.span).or(mutable).unwrap_or(start.span));

                match (mutable.is_some(), ident) {
                    (true, None) => set_policy(this, DefaultCapturePolicy::ByMutPtr, span),
                    (false, None) => set_policy(this, DefaultCapturePolicy::ByPtr, span),
                    (true, Some(ident)) => capture_expr(
                        this,
                        ident,
                        false,
                        is_unsafe,
                        Some(UnaryOp::AddrMut),
                        &mut captures,
                    ),
                    (false, Some(ident)) => capture_expr(
                        this,
                        ident,
                        false,
                        is_unsafe,
                        Some(UnaryOp::Addr),
                        &mut captures,
                    ),
                }
            }
            Token::Assign => {
                let start = this.next();
                by_value_helper(this, start.span, &mut captures);
            }
            Token::Exclamation => {
                let start = this.next();
                set_policy(this, DefaultCapturePolicy::Auto, start.span);
            }
            _ => {
                params.push((
                    this.pattern_impl(false, EvalContext::Normal),
                    this.next_if(Token::Colon).map(|_| this.type_hint()),
                ));
            }
        });

        let ret = self.next_if(Token::Colon).map(|_| self.type_hint());
        let body = if self.next_if(Token::FatArrow).is_some() || ret.is_none() {
            self.expression()
        } else {
            let token = self.expect(Token::LCurly);
            self.block_expr(token, None)
        };

        self.arena.expr(
            head.extended_to(body.span),
            ExprData::Closure { policy: policy.into_inner(), captures, params, ret, body },
        )
    }

    //

    fn use_path_component(&mut self) -> UsePathComponent {
        let token = self.next();
        match token.data {
            Token::Ident(ident) => {
                let ident = Located::new(token.span, ident);
                if self.next_if(Token::As).is_some() {
                    let new_name = self.expect_ident("expected identifier");
                    return UsePathComponent::Rename { ident, new_name };
                }

                UsePathComponent::Ident {
                    ident,
                    next: self.next_if(Token::ScopeRes).map(|_| self.use_path_component().into()),
                }
            }
            Token::Asterisk => UsePathComponent::All(token.span),
            Token::LCurly => {
                let inner = self.csv(vec![], Token::RCurly, token.span, Self::use_path_component);
                UsePathComponent::Multi(inner.data)
            }
            _ => {
                self.error(Error::new("expected path component", token.span));
                UsePathComponent::Error
            }
        }
    }

    fn path_components(
        &mut self,
        first: Option<Located<StrId>>,
        outspan: &mut Span,
    ) -> Vec<PathComponent> {
        let mut data = first.map(|s| vec![(s, Vec::new())]).unwrap_or_default();
        while self.next_if(Token::ScopeRes).is_some() {
            if self.next_if(Token::LAngle).is_some() {
                let params = self.rangle_csv_one(*outspan, Self::type_hint);
                data.last_mut().unwrap().1 = params.data;
                *outspan = params.span;
            } else {
                let name = self.expect_ident("expected name");
                outspan.extend_to(name.span);
                data.push((name, Vec::new()));
            }
        }

        data
    }

    fn type_or_trait_path(&mut self, tr: bool) -> Path {
        let origin = match self.peek().data {
            Token::ScopeRes => PathOrigin::Root(self.next().span),
            Token::Super => {
                let span = self.next().span;
                self.expect(Token::ScopeRes);
                PathOrigin::Super(span)
            }
            Token::ThisType => {
                let span = self.next().span;
                if self.next_if(Token::ScopeRes).is_none() {
                    return Path::this_type(span);
                }
                PathOrigin::This(span)
            }
            _ => PathOrigin::Normal,
        };
        let mut data = Vec::new();
        let fn_like = loop {
            let ident = self.expect_ident("expected type name");
            if tr && let Some(start) = self.next_if(Token::LParen) {
                let mut next = 0;
                let hints = self
                    .csv(vec![], Token::RParen, start, |this| {
                        let item = this.maybe_labeled_type_hint();
                        this.label_or_positional(item, &mut next)
                    })
                    .map(|args| self.arena.hints.alloc(TypeHintData::Tuple(args)));
                let ret = self
                    .next_if(Token::FatArrow)
                    .map(|_| self.type_hint())
                    .unwrap_or(ExprArena::hint_void(hints.span));
                data.push((ident, vec![hints, ret]));
                break true;
            } else if self.next_if(Token::LAngle).is_some() {
                let params = self.rangle_csv_one(ident.span, Self::type_hint);
                data.push((ident, params.data));
            } else {
                data.push((ident, Vec::new()));
            }

            if self.next_if(Token::ScopeRes).is_none() {
                break false;
            }
        };

        Path { origin, components: data, fn_like }
    }

    fn type_path(&mut self) -> Path {
        self.type_or_trait_path(false)
    }

    //

    fn parse_int(&mut self, base: u8, value: StrId, span: Span) -> ComptimeInt {
        let value = self.strings.resolve(&value);
        if base == 10 && value.starts_with("0") && value.len() > 1 {
            self.diag.report(Warning::decimal_leading_zero(span));
        }

        let mut parsable = value.to_string();
        parsable.retain(|c| c != '_');
        match ComptimeInt::from_str_radix(&parsable, base as u32) {
            Some(result) => result,
            None => {
                self.error_no_sync(Error::new(
                    format!("'{value}' is not a valid integer literal"),
                    span,
                ));
                ComptimeInt::default()
            }
        }
    }

    fn int_pattern(
        &mut self,
        negative: Option<Span>,
        t: Located<Token>,
    ) -> Option<Located<IntPattern>> {
        match t.data {
            Token::Int { base, value, width } => Some(Located::new(
                negative.map(|span| span.extended_to(t.span)).unwrap_or(t.span),
                IntPattern {
                    negative: negative.is_some(),
                    value: self.parse_int(base, value, t.span),
                    width,
                },
            )),
            Token::ByteChar(value) => Some(Located::new(
                negative.map(|span| span.extended_to(t.span)).unwrap_or(t.span),
                IntPattern {
                    negative: negative.is_some(),
                    value: ComptimeInt::Small(value as i64),
                    width: Some(self.strings.get_or_intern_static("u8")),
                },
            )),
            _ => None,
        }
    }

    fn maybe_range_pattern(
        &mut self,
        range: Option<Located<Token>>,
        start: Located<IntPattern>,
    ) -> Located<Pattern> {
        let (range, start, end, span) = if let Some(range) = range {
            let span = range.span.extended_to(start.span);
            (range, None, start.data, span)
        } else if let Some(range) =
            self.next_if_pred(|t| matches!(t, Token::Range | Token::RangeInclusive))
        {
            let negative = self.next_if(Token::Minus);
            let Some(end) =
                self.expect_map(|this, t| this.int_pattern(negative, t), "expected number")
            else {
                return Located::new(start.span, Pattern::Error);
            };
            let span = start.span.extended_to(end.span);
            (range, Some(start.data), end.data, span)
        } else {
            return start.map(Pattern::Int);
        };

        Located::new(
            span,
            Pattern::IntRange(RangePattern {
                inclusive: matches!(range.data, Token::RangeInclusive),
                start,
                end,
            }),
        )
    }

    fn literal_pattern(&mut self) -> Option<Located<Pattern>> {
        if let Some(span) = self.next_if(Token::True) {
            return Some(Located::new(span, Pattern::Bool(true)));
        } else if let Some(span) = self.next_if(Token::False) {
            return Some(Located::new(span, Pattern::Bool(false)));
        } else if let Some(span) = self.next_if(Token::Void) {
            return Some(Located::new(span, Pattern::Void));
        } else if let Some(string) =
            self.next_if_map(|_, t| t.data.as_string().map(|value| Located::new(t.span, *value)))
        {
            return Some(string.map(Pattern::String));
        }

        let range = self.next_if_pred(|t| matches!(t, Token::Range | Token::RangeInclusive));
        if let Some(span) = self.next_if(Token::Minus) {
            let Some(start) =
                self.expect_map(|this, t| this.int_pattern(Some(span), t), "expected number")
            else {
                return Some(Located::new(span, Pattern::Error));
            };

            return Some(self.maybe_range_pattern(range, start));
        }

        if let Some(int) = self.next_if_map(|this, t| this.int_pattern(None, t)) {
            return Some(self.maybe_range_pattern(range, int));
        }

        if let Some(char) =
            self.next_if_map(|_, t| t.data.as_char().map(|&ch| Located::new(t.span, ch)))
        {
            let range = if let Some(range) = range {
                range
            } else if let Some(range) =
                self.next_if_pred(|t| matches!(t, Token::Range | Token::RangeInclusive))
            {
                range
            } else {
                return Some(char.map(Pattern::Char));
            };

            let Some(end) = self.expect_map(
                |_, t| t.data.as_char().map(|&ch| Located::new(t.span, ch)),
                "expected char",
            ) else {
                return Some(Located::new(char.span, Pattern::Error));
            };
            return Some(Located::new(
                char.span.extended_to(end.span),
                Pattern::CharRange(RangePattern {
                    inclusive: matches!(range.data, Token::RangeInclusive),
                    start: Some(char.data),
                    end: end.data,
                }),
            ));
        } else if let Some(range) = range {
            self.error(Error::new("expected range to pattern", range.span));
            return Some(Located::new(range.span, Pattern::Error));
        }

        None
    }

    fn struct_like(&mut self, span: Span, mut_var: bool) -> Located<Vec<Destructure>> {
        self.csv_one(Token::RCurly, span, |this| {
            let mutable = this.next_if(Token::Mut);
            if let Some(token) = mutable.filter(|_| mut_var) {
                this.diag.report(Warning::redundant_token(this.lexer.source(), token));
            }
            let mutable = mutable.is_some();
            let name = this.expect_ident("expected name");
            if mutable || this.next_if(Token::Colon).is_none() {
                let span = name.span;
                Destructure {
                    name,
                    mutable: mutable || mut_var,
                    pattern: name.map(|name| Pattern::Path(Path::from(Located::new(span, name)))),
                }
            } else {
                Destructure { name, mutable: mutable || mut_var, pattern: this.pattern(false) }
            }
        })
    }

    fn tuple_like(&mut self, span: Span, mut_var: bool) -> Located<Vec<Located<Pattern>>> {
        self.csv_one(Token::RParen, span, |this| this.pattern(mut_var))
    }

    fn pattern_impl(&mut self, mut_var: bool, ctx: EvalContext) -> Located<Pattern> {
        let path = match self.peek().data {
            Token::Question => {
                // call pattern_impl so `?x | y` is not interpreted as `?(x | y)`
                let start = self.next();
                let inner = self.pattern_impl(false, ctx);
                return Located::new(
                    start.span.extended_to(inner.span),
                    Pattern::Option(inner.into()),
                );
            }
            Token::NoneCoalesce => {
                let start = self.next();
                let inner = self.pattern_impl(false, ctx);
                return Located::new(
                    start.span.extended_to(inner.span),
                    // TODO: fix this span
                    Pattern::Option(Located::new(inner.span, Pattern::Option(inner.into())).into()),
                );
            }
            Token::LParen => {
                let span = self.next().span;
                return self.tuple_like(span, mut_var).map(Pattern::Tuple);
            }
            Token::LCurly => {
                let span = self.next().span;
                return self.struct_like(span, mut_var).map(Pattern::Struct);
            }
            Token::LBrace => {
                let span = self.next().span;
                return self
                    .csv(Vec::new(), Token::RBrace, span, |this| {
                        if let Some(span) = this.next_if(Token::Ellipses) {
                            let pattern = if this.next_if(Token::Mut).is_some() {
                                Some((true, this.expect_ident("expected name")))
                            } else {
                                Some(false).zip(this.next_if_ident())
                            };

                            Located::new(span, Pattern::Rest(pattern))
                        } else {
                            this.pattern(mut_var)
                        }
                    })
                    .map(Pattern::Array);
            }
            Token::Colon => {
                let span = self.next().span;
                let ident = self.expect_ident("expected identifier");
                Path::new(PathOrigin::Infer(span), vec![(ident, Default::default())])
            }
            _ => {
                if mut_var || self.next_if(Token::Mut).is_some() {
                    return self.expect_ident("expected name").map(Pattern::MutBinding);
                }
                if !mut_var && let Some(pattern) = self.literal_pattern() {
                    return pattern;
                }

                self.type_path()
            }
        };

        match self.peek().data {
            Token::LParen => {
                let span = self.next().span;
                self.tuple_like(span, mut_var)
                    .map(|subpatterns| Pattern::TupleLike { path, subpatterns })
            }
            Token::LCurly if ctx != EvalContext::IfWhile => {
                let span = self.next().span;
                self.struct_like(span, mut_var)
                    .map(|subpatterns| Pattern::StructLike { path, subpatterns })
            }
            _ => Located::new(path.span(), Pattern::Path(path)),
        }
    }

    fn pattern_ex(&mut self, mut_var: bool, ctx: EvalContext) -> Located<Pattern> {
        let patt = self.pattern_impl(mut_var, ctx);
        if self.matches(Token::BitOr) {
            let mut span = patt.span;
            let mut patterns = vec![patt];
            while self.next_if(Token::BitOr).is_some() {
                let patt = self.pattern_ex(mut_var, ctx);
                span.extend_to(patt.span);
                patterns.push(patt);
            }
            Located::new(span, Pattern::Or(patterns))
        } else {
            patt
        }
    }

    fn pattern(&mut self, mut_var: bool) -> Located<Pattern> {
        self.pattern_ex(mut_var, EvalContext::Normal)
    }

    fn is_range_end(&mut self, ctx: EvalContext) -> bool {
        let k = self.peek().data;
        matches!(k, Token::Semicolon | Token::Comma | Token::RBrace | Token::RParen)
            || matches!((k, ctx), (Token::LCurly, EvalContext::For))
    }

    fn attribute(&mut self) -> Attribute {
        let mut props = false;
        let name = self.next();
        let span = name.span;
        let name = name.map(|data| match data {
            Token::String(name) => AttrName::Str(name),
            Token::Unsafe => {
                props = true;
                AttrName::Str(Strings::UNSAFE)
            }
            Token::Ident(name) => {
                props = true;
                AttrName::Str(name)
            }
            Token::Int { base, value, width: _ } => {
                AttrName::Int(self.parse_int(base, value, span))
            }
            _ => {
                self.error(Error::new("expected attribute name", span));
                AttrName::Str(Strings::EMPTY)
            }
        });

        Attribute {
            name,
            props: if props {
                self.next_if(Token::LParen)
                    .map(|span| self.csv_one(Token::RParen, span, Self::attribute).data)
                    .unwrap_or_default()
            } else {
                vec![]
            },
        }
    }

    fn attributes(&mut self) -> Attributes {
        let mut attrs = vec![];
        while let Some(span) = self.next_if(Token::Dollar) {
            self.expect(Token::LBrace);
            attrs.extend(self.csv_one(Token::RBrace, span, Self::attribute).data);
        }
        Attributes::new(attrs)
    }

    fn abi(&mut self, is_extern: bool) -> Option<Located<FnAbi>> {
        let span = self.peek().span;
        if is_extern && let Some(ident) = self.next_if_map(|_, tk| tk.data.into_string().ok()) {
            let name = self.strings.resolve(&ident);
            if let Ok(data) = FnAbi::from_str(name) {
                return Some(Located::new(span, data));
            } else {
                let msg = format!("invalid calling convention specification '{name}'");
                self.error_no_sync(Error::new(msg, span));
                return Some(Located::new(span, FnAbi::C));
            }
        }
        None
    }

    fn safety(&mut self) -> Option<Located<Safety>> {
        self.next_if_map(|_, tk| match tk.data {
            Token::Safe => Some(Located::new(tk.span, Safety::Safe)),
            Token::Unsafe => Some(Located::new(tk.span, Safety::Unsafe)),
            _ => None,
        })
    }

    fn visiblity(&mut self) -> (Option<Span>, Visibility) {
        let res = self.next_if_map(|_, tk| match tk.data {
            Token::Pub => Some(Located::new(tk.span, Visibility::Public)),
            Token::Lib => Some(Located::new(tk.span, Visibility::Library)),
            _ => None,
        });
        if let Some(res) = res { (Some(res.span), res.data) } else { (None, Visibility::Private) }
    }

    //

    fn type_params(&mut self) -> TypeParams {
        self.next_if(Token::LAngle)
            .map(|span| {
                self.rangle_csv_one(span, |this| {
                    (this.expect_ident("expected type name"), this.trait_impls())
                })
                .data
            })
            .unwrap_or_default()
    }

    fn derives(&mut self) -> Vec<Path> {
        if let Some(span) = self.next_if(Token::Colon) {
            self.csv_one(Token::LCurly, span, |this| this.type_or_trait_path(true)).data
        } else {
            self.expect(Token::LCurly);
            vec![]
        }
    }

    fn trait_impls(&mut self) -> Vec<Path> {
        let mut impls = Vec::new();
        if self.next_if(Token::Colon).is_some() {
            loop {
                impls.push(self.type_or_trait_path(true));
                if self.next_if(Token::Plus).is_none() {
                    break;
                }
            }
        }
        impls
    }

    fn maybe_labeled_type_hint(&mut self) -> (Option<Located<StrId>>, TypeHint) {
        let hint = self.type_hint();
        if let TypeHintData::Path(path) = self.arena.hints.get(hint.data)
            && let Some(ident) = path.as_identifier()
            && self.next_if(Token::Colon).is_some()
        {
            (Some(ident), self.type_hint())
        } else {
            (None, hint)
        }
    }

    fn type_hint(&mut self) -> TypeHint {
        match self.peek().data {
            Token::Asterisk => {
                let begin = self.next().span;
                if self.next_if(Token::Dyn).is_some() {
                    let mutable = self.next_if(Token::Mut).is_some();
                    let path = self.type_or_trait_path(true);
                    let span = begin.extended_to(path.span());
                    if mutable {
                        self.arena.hint(span, TypeHintData::DynMutPtr(path))
                    } else {
                        self.arena.hint(span, TypeHintData::DynPtr(path))
                    }
                } else {
                    let mutable = self.next_if(Token::Mut).is_some();
                    let inner = self.type_hint();
                    let span = begin.extended_to(inner.span);
                    if mutable {
                        self.arena.hint(span, TypeHintData::MutPtr(inner))
                    } else {
                        self.arena.hint(span, TypeHintData::Ptr(inner))
                    }
                }
            }
            Token::Caret => {
                let begin = self.next().span;
                let mutable = self.next_if(Token::Mut).is_some();
                let inner = self.type_hint();
                let span = begin.extended_to(inner.span);
                if mutable {
                    self.arena.hint(span, TypeHintData::RawMutPtr(inner))
                } else {
                    self.arena.hint(span, TypeHintData::RawPtr(inner))
                }
            }
            Token::Question => {
                let begin = self.next().span;
                let inner = self.type_hint();
                self.arena.hint(begin.extended_to(inner.span), TypeHintData::Option(inner))
            }
            Token::NoneCoalesce => {
                let begin = self.next().span;
                let inner = self.type_hint();
                let total = begin.extended_to(inner.span);
                let inner_span = Span { pos: total.pos, file: total.file, len: total.len - 1 };
                let inner = self.arena.hint(inner_span, TypeHintData::Option(inner));
                self.arena.hint(total, TypeHintData::Option(inner))
            }
            Token::LBrace => {
                let mut span = self.next().span;
                if self.next_if(Token::Mut).is_some() {
                    let inner = self.type_hint();
                    self.expect(Token::Range);
                    span.extend_to(self.expect(Token::RBrace));
                    self.arena.hint(span, TypeHintData::SliceMut(inner))
                } else {
                    let inner = self.type_hint();
                    if let Some(end) = self.next_if(Token::RBrace) {
                        self.arena.hint(span.extended_to(end), TypeHintData::Vec(inner))
                    } else if self.next_if(Token::Range).is_some() {
                        span.extend_to(self.expect(Token::RBrace));
                        self.arena.hint(span, TypeHintData::Slice(inner))
                    } else if self.next_if(Token::Semicolon).is_some() {
                        let count = self.expression();
                        span.extend_to(self.expect(Token::RBrace));
                        self.arena.hint(span, TypeHintData::Array(inner, count))
                    } else if self.next_if(Token::Colon).is_some() {
                        let value = self.type_hint();
                        span.extend_to(self.expect(Token::RBrace));
                        self.arena.hint(span, TypeHintData::Map([inner, value]))
                    } else {
                        let end = self.next().span;
                        self.error(Error::new("expected ']', ';', or ':'", end));
                        Located::new(end, ExprArena::HINT_ERROR)
                    }
                }
            }
            Token::Hash => {
                let begin = self.next().span;
                self.expect(Token::LBrace);
                let inner = self.type_hint();
                let end = self.expect(Token::RBrace);
                self.arena.hint(begin.extended_to(end), TypeHintData::Set(inner))
            }
            Token::LParen => {
                let left = self.next();
                let mut next = 0;
                self.csv(vec![], Token::RParen, left.span, |this| {
                    let item = this.maybe_labeled_type_hint();
                    this.label_or_positional(item, &mut next)
                })
                .map(|args| self.arena.hints.alloc(TypeHintData::Tuple(args)))
            }
            Token::Void => ExprArena::hint_void(self.next().span),
            Token::Extern | Token::Unsafe | Token::Fn => {
                let start = self.next();
                let is_extern = start.data == Token::Extern;
                let abi = self.abi(is_extern);
                let mut is_unsafe = matches!(start.data, Token::Unsafe | Token::Extern);
                if is_extern && start.data != Token::Unsafe {
                    if let Some(span) = self.next_if(Token::Unsafe) {
                        self.diag.report(Warning::useless_token(self.lexer.source(), span));
                    } else if self.next_if(Token::Safe).is_some() {
                        is_unsafe = false;
                    }
                }
                let abi = abi.map(|abi| abi.data).unwrap_or(if is_extern {
                    FnAbi::C
                } else {
                    FnAbi::Ctl
                });

                if start.data != Token::Fn {
                    self.expect(Token::Fn);
                }

                self.expect(Token::LParen);
                let mut params = self.csv(Vec::new(), Token::RParen, start.span, |this| {
                    this.maybe_labeled_type_hint().1
                });
                let ret = if self.next_if(Token::FatArrow).is_some() {
                    let hint = self.type_hint();
                    params.span.extend_to(hint.span);
                    Some(hint)
                } else {
                    None
                };

                self.arena.hint(
                    params.span,
                    TypeHintData::Fn { abi, is_unsafe, params: params.data, ret },
                )
            }
            _ => {
                let path = self.type_path();
                self.arena.hint(path.span(), TypeHintData::Path(path))
            }
        }
    }

    fn block(&mut self, lcurly: Option<Span>) -> Located<Vec<Stmt>> {
        let mut stmts = Vec::new();
        let lcurly = lcurly.unwrap_or_else(|| self.expect(Token::LCurly));
        let span = self.next_until(Token::RCurly, lcurly, |this| {
            while this.next_if(Token::Semicolon).is_some() {}
            if this.matches(Token::RCurly) {
                return;
            }

            stmts.push(this.statement());
        });
        Located::new(span, stmts)
    }

    fn structure(&mut self, vis: Visibility, span: Span, union: bool) -> Located<Struct> {
        let name = self.expect_ident("expected name");
        let type_params = self.type_params();
        let derives = self.derives();

        let mut functions = Vec::new();
        let mut operators = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();
        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            let (tk_public, vis) = this.visiblity();
            let tk_unsafe = this.next_if(Token::Unsafe);
            let config = FnConfig {
                vis,
                is_unsafe: tk_unsafe.is_some(),
                body: Body::Required,
                lead_span: tk_public.or(tk_unsafe),
                ..Default::default()
            };
            if let Some(impl_span) = this.next_if(Token::Impl) {
                this.invalid_here(tk_public);
                impls.push(this.impl_block(attrs, impl_span, tk_unsafe.is_some()));
            } else if let Some(func) = this.try_function(config, attrs) {
                match func {
                    Left(func) => functions.push(func),
                    Right(func) => operators.push(func),
                }
            } else if let Some(name) = this.expect_ident_2("expected name") {
                // TODO: apply the attributes to the next member
                this.invalid_here(tk_public.filter(|_| union));
                this.invalid_here(tk_unsafe);
                this.expect(Token::Colon);
                let ty = this.type_hint();
                let value = this.next_if(Token::Assign).map(|_| this.expression());

                if !this.matches(Token::RCurly) {
                    this.expect(Token::Comma);
                }

                members.push(Member { vis, ty, name, default: value });
            }
        });

        Located::new(
            span,
            Struct { vis, name, type_params, members, impls, functions, operators, derives },
        )
    }

    fn union(&mut self, vis: Visibility, span: Span) -> Located<StmtData> {
        let tag = self.next_if(Token::LParen).map(|_| {
            let path = self.type_path();
            self.expect(Token::RParen);
            path
        });
        let name = self.expect_ident("expected name");
        let type_params = self.type_params();
        let derives = self.derives();
        let mut functions = Vec::new();
        let mut operators = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();
        let mut variants = Vec::new();

        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            let (tk_public, vis) = this.visiblity();
            let tk_unsafe = this.next_if(Token::Unsafe);
            let config = FnConfig {
                vis,
                is_unsafe: tk_unsafe.is_some(),
                body: Body::Required,
                lead_span: tk_public.or(tk_unsafe),
                ..Default::default()
            };
            if let Some(impl_span) = this.next_if(Token::Impl) {
                this.invalid_here(tk_public);
                impls.push(this.impl_block(attrs, impl_span, tk_unsafe.is_some()));
            } else if let Some(func) = this.try_function(config, attrs) {
                // TODO: apply the attributes to the next member
                match func {
                    Left(func) => functions.push(func),
                    Right(func) => operators.push(func),
                }
            } else if this.next_if(Token::Shared).is_some() {
                if let Some(name) = this.expect_ident_2("expected name") {
                    this.invalid_here(tk_public);
                    this.invalid_here(tk_unsafe);
                    this.expect(Token::Colon);
                    let ty = this.type_hint();
                    let value = this.next_if(Token::Assign).map(|_| this.expression());
                    this.expect(Token::Comma);

                    members.push(Member { vis: Visibility::Public, name, ty, default: value });
                }
            } else if let Some(name) = this.expect_ident_2("expected variant name") {
                this.invalid_here(tk_public);
                this.invalid_here(tk_unsafe);
                let data = if let Some(start) = this.next_if(Token::LParen) {
                    let mut hint_data = vec![];
                    let mut next = 0;
                    let members = this.csv(vec![], Token::RParen, start, |this| {
                        let original = this.maybe_labeled_type_hint();
                        let (name, ty) = this.label_or_positional(original, &mut next);
                        let default = this.next_if(Token::Assign).map(|_| this.expression());
                        hint_data.push((name, ty));
                        VariantData { name, ty, default }
                    });
                    let hint =
                        Located::nowhere(this.arena.hints.alloc(TypeHintData::Tuple(hint_data)));
                    Some((members.data, hint))
                } else {
                    None
                };

                let tag = this.next_if(Token::Assign).map(|_| this.expression());
                if !this.matches(Token::RCurly) {
                    this.expect(Token::Comma);
                }

                variants.push(Variant { name, data, tag });
            }
        });

        Located::new(
            span,
            StmtData::Union {
                tag: tag.map(|tag| {
                    Located::new(tag.span(), self.arena.hints.alloc(TypeHintData::Path(tag)))
                }),
                variants,
                base: Struct {
                    vis,
                    name,
                    type_params,
                    members,
                    functions,
                    impls,
                    operators,
                    derives,
                },
            },
        )
    }

    fn r#trait(
        &mut self,
        vis: Visibility,
        span: Span,
        is_unsafe: bool,
        seal: Visibility,
    ) -> Located<StmtData> {
        let name = self.expect_ident("expected name");
        let type_params = self.type_params();
        let super_traits = self.trait_impls();
        self.expect(Token::LCurly);

        let mut functions = Vec::new();
        let mut assoc_types = Vec::new();
        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            let (tk, _) = this.visiblity();
            this.invalid_here(tk);

            if this.next_if(Token::Type).is_some() {
                let ident = this.expect_ident("expected type name");
                let impls = this.trait_impls();
                this.expect(Token::Semicolon);
                assoc_types.push((ident, impls));
                return;
            }

            let tk_unsafe = this.next_if(Token::Unsafe);
            let config = FnConfig {
                vis: Visibility::Public,
                is_unsafe: tk_unsafe.is_some(),
                lead_span: tk_unsafe,
                ..Default::default()
            };
            if let Some(func) = this.expect_fn(config, attrs) {
                let stmt = this.normal_fn(func);
                let StmtData::Fn(func) = stmt.data.data else { unreachable!() };
                functions.push(Located::new(stmt.data.span, func));
            }
        });

        Located::new(
            span,
            StmtData::Trait {
                vis,
                seal,
                is_unsafe,
                name,
                type_params,
                super_traits,
                functions,
                assoc_types,
            },
        )
    }

    fn extension(&mut self, earliest: Span, span: Span) -> Located<StmtData> {
        let type_params = self.type_params();
        let ty = self.type_hint();
        self.expect(Token::LCurly);

        let mut functions = Vec::new();
        let mut operators = Vec::new();
        let mut impls = Vec::new();
        let total_span = self.next_until(Token::RCurly, earliest, |this| {
            let attrs = this.attributes();
            let (tk_public, vis) = this.visiblity();
            let tk_unsafe = this.next_if(Token::Unsafe);
            if let Some(token) = this.next_if(Token::Impl) {
                this.invalid_here(tk_public);
                impls.push(this.impl_block(attrs, token, tk_unsafe.is_some()));
            } else {
                let config = FnConfig {
                    body: Body::Required,
                    vis,
                    is_unsafe: tk_unsafe.is_some(),
                    lead_span: tk_public.or(tk_unsafe),
                    ..Default::default()
                };
                match this.expect_fn(config, attrs) {
                    Some(Left(func)) => functions.push(func),
                    Some(Right(func)) => operators.push(func),
                    _ => {}
                }
            }
        });

        Located::new(
            total_span,
            StmtData::Extension { span, ty, type_params, impls, functions, operators },
        )
    }

    fn impl_block(&mut self, attrs: Attributes, span: Span, is_unsafe: bool) -> Located<ImplBlock> {
        let type_params = self.type_params();
        let path = self.type_or_trait_path(true);
        self.expect(Token::LCurly);

        let mut functions = Vec::new();
        let mut assoc_types = Vec::new();
        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            let (tk, _) = this.visiblity();
            this.invalid_here(tk);

            if this.next_if(Token::Type).is_some() {
                let name = this.expect_ident("expected type name");
                this.expect(Token::Assign);
                let hint = this.type_hint();
                this.expect(Token::Semicolon);
                assoc_types.push((name, hint));
                return;
            }

            let tk_unsafe = this.next_if(Token::Unsafe);
            let config = FnConfig {
                vis: Visibility::Public,
                is_unsafe: tk_unsafe.is_some(),
                body: Body::Required,
                lead_span: tk_unsafe,
                ..Default::default()
            };
            match this.expect_fn(config, attrs) {
                Some(Left(func)) => functions.push(func),
                Some(Right(func)) => {
                    this.error(Error::new(
                        "operator functions are not allowed here",
                        func.data.name.span,
                    ));
                }
                _ => {}
            }
        });

        Located::new(
            span,
            ImplBlock { attrs, type_params, path, functions, assoc_types, is_unsafe },
        )
    }

    fn try_function(
        &mut self,
        cfg: FnConfig,
        attrs: Attributes,
    ) -> Option<Either<Located<Fn>, Located<OperatorFn>>> {
        let (head_span, is_async) = if let Some(token) = self.next_if(Token::Fn) {
            (token, None)
        } else if let Some(token) = self.next_if(Token::Async) {
            self.expect(Token::Fn);
            (token, Some(token))
        } else {
            return None;
        };
        let mut span = cfg.lead_span.unwrap_or(head_span);

        let name = self.expect_fn_name();
        let type_params = self.type_params();
        let mut params = Vec::new();
        let mut count = 0;
        let mut variadic = false;
        let mut has_default = false;
        self.expect(Token::LParen);
        while self.next_if_pred(|t| matches!(t, Token::RParen | Token::Eof)).is_none() {
            if self.next_if(Token::Ellipses).is_some() && name.data.is_left() {
                variadic = true;
                self.expect(Token::RParen);
                break;
            }

            let keyword = self.next_if(Token::Keyword).is_some();
            let my = self.next_if(Token::My);
            let mutable = self.next_if(Token::Mut).is_some();
            if let Some(this_span) = self.next_if(Token::This) {
                if count != 0 {
                    self.invalid_here(Some(this_span));
                }

                let patt = if my.is_some() && mutable {
                    Pattern::MutBinding(Strings::THIS_PARAM)
                } else {
                    Pattern::Path(Path::from(Located::new(this_span, Strings::THIS_PARAM)))
                };

                let span = Span { len: 0, ..this_span };
                let this_ty = self
                    .arena
                    .hint(span, TypeHintData::Path(Path::new(PathOrigin::This(span), vec![])));
                params.push(Param {
                    keyword,
                    patt: Located::new(this_span, patt),
                    ty: if my.is_some() {
                        this_ty
                    } else if mutable {
                        self.arena.hint(this_ty.span, TypeHintData::MutPtr(this_ty))
                    } else {
                        self.arena.hint(this_ty.span, TypeHintData::Ptr(this_ty))
                    },
                    default: None,
                });
            } else {
                self.invalid_here(my);
                let patt = if mutable {
                    self.expect_ident("expected name").map(Pattern::MutBinding)
                } else if keyword {
                    let t = self.expect_ident("expected name");
                    let span = t.span;
                    t.map(|name| Pattern::Path(Path::from(Located::new(span, name))))
                } else {
                    self.pattern(false)
                };
                self.expect(Token::Colon);
                let ty = self.type_hint();
                let default = if self.next_if(Token::Assign).is_some() {
                    has_default = true;
                    Some(self.expression())
                } else {
                    if !keyword && has_default {
                        self.error_no_sync(Error::new(
                            "positional parameters must not follow a default parameter",
                            name.span,
                        ));
                    }

                    None
                };

                params.push(Param { patt, keyword, ty, default });
            }

            if !self.matches(Token::RParen) {
                self.expect(Token::Comma);
            }

            count += 1;
        }

        let ret = self.next_if(Token::Colon).map(|_| self.type_hint());
        let body = if let Some(semi) = self.next_if(Token::Semicolon) {
            if cfg.body == Body::Required {
                self.error(Error::new("expected '{'", semi));
            }
            span.extend_to(semi);
            None
        } else if let Some(lead) = self.next_if(Token::FatArrow) {
            if cfg.body == Body::None {
                self.error(Error::new("expected ';'", lead));
            }

            let expr = self.expression();
            let token = self.expect(Token::Semicolon);
            span.extend_to(token);
            Some(expr)
        } else {
            let lcurly = self.expect(Token::LCurly);
            if cfg.body == Body::None {
                self.error(Error::new("expected ';'", lcurly));
            }

            let expr = self.block_expr(lcurly, None);
            span.extend_to(expr.span);
            Some(expr)
        };

        match name.data {
            Left(ident) => Some(Left(Located::new(
                span,
                Fn {
                    name: Located::new(name.span, ident),
                    abi: cfg.abi,
                    vis: cfg.vis,
                    is_unsafe: cfg.is_unsafe || (cfg.unsafe_if_no_body && body.is_none()),
                    is_async: is_async.is_some(),
                    variadic,
                    type_params,
                    params,
                    ret,
                    body,
                    attrs,
                    typ: FunctionType::Normal,
                },
            ))),
            Right(op) => {
                self.invalid_here(is_async);
                Some(Right(Located::new(
                    span,
                    OperatorFn {
                        name: Located::new(name.span, op),
                        type_params,
                        params,
                        ret,
                        body,
                        attrs,
                    },
                )))
            }
        }
    }

    fn expect_fn(
        &mut self,
        params: FnConfig,
        attrs: Attributes,
    ) -> Option<Either<Located<Fn>, Located<OperatorFn>>> {
        let res = self.try_function(params, attrs);
        if res.is_none() {
            let span = self.next().span;
            self.error(Error::new("expected function", span));
        }
        res
    }

    fn normal_fn(&mut self, func: Either<Located<Fn>, Located<OperatorFn>>) -> Stmt {
        match func {
            Left(func) => Stmt { attrs: Default::default(), data: func.map(StmtData::Fn) },
            Right(func) => {
                self.error(Error::new(
                    "operator functions can only be defined in types and extensions",
                    func.data.name.span,
                ));

                Stmt {
                    attrs: Default::default(),
                    data: func.map(|func| {
                        // TODO: don't use to_string
                        let name = self.strings.get_or_intern(func.name.data.to_string());
                        StmtData::Fn(Fn::from_operator_fn(name, func))
                    }),
                }
            }
        }
    }

    fn expect_fn_name(&mut self) -> Located<Either<StrId, OperatorFnType>> {
        let token = self.next();
        let mut span = token.span;
        let data = match token.data {
            Token::Plus => Right(OperatorFnType::Plus),
            Token::Minus => Right(OperatorFnType::Minus), // unary or binary -
            Token::Asterisk => Right(OperatorFnType::Mul), // binary *
            Token::Div => Right(OperatorFnType::Div),
            Token::Rem => Right(OperatorFnType::Rem),
            Token::Ampersand => Right(OperatorFnType::BitAnd), // bitwise &
            Token::BitOr => Right(OperatorFnType::BitOr),
            Token::Caret => Right(OperatorFnType::Xor),
            Token::Shl => Right(OperatorFnType::Shl),
            Token::Shr => Right(OperatorFnType::Shr),
            Token::Equal => Right(OperatorFnType::Eq),
            Token::Spaceship => Right(OperatorFnType::Cmp),
            Token::Increment => Right(OperatorFnType::Increment),
            Token::Decrement => Right(OperatorFnType::Decrement),
            Token::Exclamation => Right(OperatorFnType::Bang),
            Token::AddAssign => Right(OperatorFnType::AddAssign),
            Token::SubAssign => Right(OperatorFnType::SubAssign),
            Token::MulAssign => Right(OperatorFnType::MulAssign),
            Token::DivAssign => Right(OperatorFnType::DivAssign),
            Token::RemAssign => Right(OperatorFnType::RemAssign),
            Token::BitAndAssign => Right(OperatorFnType::BitAndAssign),
            Token::BitOrAssign => Right(OperatorFnType::BitOrAssign),
            Token::XorAssign => Right(OperatorFnType::XorAssign),
            Token::ShlAssign => Right(OperatorFnType::ShlAssign),
            Token::ShrAssign => Right(OperatorFnType::ShrAssign),
            Token::LBrace => {
                let tk = self.expect(Token::RBrace);
                if let Some(tk) = self.next_if(Token::Assign) {
                    span.extend_to(tk);
                    Right(OperatorFnType::SubscriptAssign)
                } else {
                    span.extend_to(tk);
                    Right(OperatorFnType::Subscript)
                }
            }
            Token::Ident(name) => Left(name),
            _ => {
                self.error(Error::new("expected identifier", token.span));
                Left(Strings::EMPTY)
            }
        };
        Located::new(span, data)
    }

    fn format_opts(&mut self) -> FormatOpts {
        fn next_if_map<T>(
            this: &mut Parser,
            lexer: &mut std::iter::Peekable<FormatLexer>,
            cb: impl FnOnce(&mut Parser, Located<FormatToken>) -> Option<T>,
        ) -> Option<T> {
            if let Some(token) = lexer.peek().copied() {
                let res = cb(this, token);
                if res.is_some() {
                    lexer.next();
                }
                res
            } else {
                None
            }
        }

        fn number_or_ident<'a>(
            this: &mut Parser<'a>,
            lexer: &mut std::iter::Peekable<FormatLexer<'a>>,
        ) -> Option<Expr> {
            let mut was_ident = false;
            let before = lexer.clone();
            let res = next_if_map(this, lexer, |this, t| match t.data {
                FormatToken::Number(v) => {
                    let v = this.strings.get_or_intern(v);
                    let value = this.parse_int(10, v, t.span);
                    Some(this.arena.expr(
                        t.span,
                        ExprData::Integer(IntPattern { negative: false, value, width: None }),
                    ))
                }
                FormatToken::Ident(v) => {
                    was_ident = true;
                    Some(this.arena.expr(
                        t.span,
                        ExprData::Path(Path::new(
                            PathOrigin::Normal,
                            vec![(Located::new(t.span, this.strings.get_or_intern(v)), vec![])],
                        )),
                    ))
                }
                _ => None,
            });
            if was_ident && lexer.next_if(|t| t.data == FormatToken::Dollar).is_none() {
                *lexer = before;
                None
            } else {
                res
            }
        }

        // [[fill]align][sign][alt][zero][width]['.'prec][type]

        // Safe because we just consumed the ':' token, so the Parser peek should be empty
        let mut lexer = self.lexer.create_format_lexer();
        let fill =
            if matches!(lexer.peek_next(), Some('<' | '>' | '^')) { lexer.advance() } else { None };

        let mut lexer = lexer.peekable();
        let align = next_if_map(self, &mut lexer, |_, t| match t.data {
            FormatToken::LAngle => Some(Alignment::Left),
            FormatToken::RAngle => Some(Alignment::Right),
            FormatToken::Caret => Some(Alignment::Center),
            _ => None,
        });
        let sign = next_if_map(self, &mut lexer, |_, t| match t.data {
            FormatToken::Plus => Some(Sign::Positive),
            FormatToken::Minus => Some(Sign::Negative),
            _ => None,
        });
        let alt = lexer.next_if(|t| t.data == FormatToken::Hash).is_some();
        let zero = lexer.next_if(|t| t.data == FormatToken::LeadingZero).is_some();
        let width = number_or_ident(self, &mut lexer);
        // TODO: because of the LeadingZero token, prec will fail if it starts with a zero
        let prec = lexer
            .next_if(|t| t.data == FormatToken::Dot)
            .and_then(|_| number_or_ident(self, &mut lexer));
        let typ = next_if_map(self, &mut lexer, |this, t| match t.data {
            FormatToken::Question => Some(FormatType::Debug),
            FormatToken::Ident(v) => {
                Some(FormatType::Custom(Located::new(t.span, this.strings.get_or_intern(v))))
            }
            _ => None,
        });

        self.lexer.advance_to(lexer.peek().map(|t| t.span.pos));
        FormatOpts { fill, width, prec, align, alt, sign, zero, typ }
    }

    //

    fn csv<T>(
        &mut self,
        mut res: Vec<T>,
        end: Token,
        span: Span,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> Located<Vec<T>> {
        if !res.is_empty() && !self.matches(end) {
            self.expect(Token::Comma);
        }

        let span = self.next_until(end, span, |this| {
            res.push(f(this));

            if !this.matches(end) {
                this.expect(Token::Comma);
            }
        });

        Located::new(span, res)
    }

    fn csv_expr<T>(
        &mut self,
        res: Vec<T>,
        end: Token,
        span: Span,
        f: impl FnMut(&mut Self) -> T,
        map: impl FnOnce(Vec<T>) -> ExprData,
    ) -> Expr {
        self.csv(res, end, span, f).map(|data| self.arena.alloc(map(data)))
    }

    fn csv_one<T>(
        &mut self,
        end: Token,
        span: Span,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> Located<Vec<T>> {
        let first = f(self);
        self.csv(vec![first], end, span, f)
    }

    fn rangle_csv_one<T>(
        &mut self,
        mut span: Span,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> Located<Vec<T>> {
        let mut res = vec![f(self)];
        if !res.is_empty() && !matches!(self.peek().data, Token::RAngle | Token::Shr) {
            self.expect(Token::Comma);
        }

        loop {
            match self.next_if_pred(|t| matches!(t, Token::Eof | Token::RAngle | Token::Shr)) {
                Some(t) if t.data == Token::Shr => {
                    self.peek = Some(Located::new(t.span, Token::RAngle));
                    span.extend_to(t.span);
                    break;
                }
                Some(t) => {
                    span.extend_to(t.span);
                    break;
                }
                None => {}
            }

            res.push(f(self));
            if !matches!(self.peek().data, Token::RAngle | Token::Shr) {
                self.expect(Token::Comma);
            }
        }

        Located::new(span, res)
    }

    //

    fn synchronize(&mut self) {
        use Token as T;
        loop {
            match self.peek().data {
                T::Semicolon => {
                    self.next();
                    break;
                }
                T::Pub
                | T::Struct
                | T::Enum
                | T::Union
                | T::Extension
                | T::Trait
                | T::Fn
                | T::Let
                | T::Static
                | T::Extern
                | T::Loop
                | T::If
                | T::Match
                | T::While
                | T::Return
                | T::Yield
                | T::Break
                | T::Use
                | T::Eof => break,
                _ => {
                    self.next();
                }
            }
        }
        self.needs_sync = false;
    }

    fn error(&mut self, err: Error) {
        if !self.needs_sync {
            self.diag.report(err);
            self.needs_sync = true;
        }
    }

    fn error_no_sync(&mut self, err: Error) {
        if !self.needs_sync {
            self.diag.report(err);
        }
    }

    fn invalid_here(&mut self, span: Option<Span>) {
        if let Some(span) = span {
            self.error_no_sync(Error::not_valid_here(self.lexer.source(), span));
        }
    }

    //

    fn peek(&mut self) -> Located<Token> {
        *self.peek.get_or_insert_with(|| self.lexer.next_skip_comments(self.diag, self.strings))
    }

    fn next(&mut self) -> Located<Token> {
        self.peek.take().unwrap_or_else(|| self.lexer.next_skip_comments(self.diag, self.strings))
    }

    fn next_if(&mut self, kind: Token) -> Option<Span> {
        self.next_if_pred(|t| t == kind).map(|t| t.span)
    }

    fn next_if_pred(&mut self, pred: impl FnOnce(Token) -> bool) -> Option<Located<Token>> {
        pred(self.peek().data).then(|| self.next())
    }

    fn next_if_map<T>(
        &mut self,
        f: impl FnOnce(&mut Self, Located<Token>) -> Option<T>,
    ) -> Option<T> {
        let peek = self.peek();
        let outer = f(self, peek);
        if outer.is_some() {
            self.next();
        }
        outer
    }

    fn next_if_ident(&mut self) -> Option<Located<StrId>> {
        self.next_if_map(|_, t| t.data.as_ident().map(|&i| Located::new(t.span, i)))
    }

    fn next_until(&mut self, token: Token, span: Span, mut f: impl FnMut(&mut Self)) -> Span {
        loop {
            let peek = self.peek();
            if peek.data == Token::Eof {
                self.error_no_sync(Error::expected_found(token, Token::Eof, peek.span));
                return span.extended_to(peek.span);
            } else if peek.data == token {
                self.next();
                return span.extended_to(peek.span);
            }

            f(self);
            if token == Token::RCurly && self.needs_sync {
                // Skip at least one token so we make progress and don't get stuck in an infinite
                // loop
                self.next();
                self.synchronize();
            }
        }
    }

    fn matches(&mut self, kind: Token) -> bool {
        self.peek().data == kind
    }

    fn expect_map<T>(
        &mut self,
        f: impl FnOnce(&mut Self, Located<Token>) -> Option<T>,
        msg: &str,
    ) -> Option<T> {
        let token = self.next();
        if let Some(res) = f(self, token) {
            Some(res)
        } else {
            self.error(Error::new(msg, token.span));
            None
        }
    }

    fn expect(&mut self, kind: Token) -> Span {
        let token = self.next();
        if token.data == kind {
            token.span
        } else {
            self.error(Error::new(format!("expected '{kind}'"), token.span));
            token.span
        }
    }

    fn expect_ident(&mut self, msg: &str) -> Located<StrId> {
        let token = self.next();
        if let Token::Ident(ident) = token.data {
            Located::new(token.span, ident)
        } else {
            self.error(Error::new(msg, token.span));
            Located::new(token.span, Strings::EMPTY)
        }
    }

    fn expect_ident_2(&mut self, msg: &str) -> Option<Located<StrId>> {
        let token = self.peek();
        if let Token::Ident(ident) = token.data {
            self.next();
            Some(Located::new(token.span, ident))
        } else {
            self.error(Error::new(msg, token.span));
            None
        }
    }
}
