use either::{Either, Either::*};

use crate::{
    THIS_PARAM, THIS_TYPE,
    ast::{Attribute, Attributes, UnaryOp, parsed::*},
    error::{Diagnostics, Error, FileId},
    lexer::{Lexer, Located, Precedence, Span, Token},
};

#[derive(Default, Clone, Copy)]
struct FnConfig {
    tk_extern: Option<Span>,
    tk_public: Option<Span>,
    tk_unsafe: Option<Span>,
    require_body: bool,
    forced_pub: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum EvalContext {
    For,
    IfWhile,
    Normal,
}

pub struct Parser<'a, 'b> {
    lexer: Lexer<'a>,
    peek: Option<Located<Token<'a>>>,
    needs_sync: bool,
    diag: &'b mut Diagnostics,
}

impl<'a, 'b> Parser<'a, 'b> {
    fn new(src: &'a str, diag: &'b mut Diagnostics, file: FileId) -> Self {
        Self {
            diag,
            lexer: Lexer::new(src, file),
            peek: None,
            needs_sync: false,
        }
    }

    pub fn parse(src: &'a str, name: String, diag: &'b mut Diagnostics, file: FileId) -> Stmt {
        let mut this = Self::new(src, diag, file);
        let mut stmts = Vec::new();
        while !this.matches(Token::Eof) {
            stmts.push(this.item());
        }

        let span = Span {
            file,
            pos: 0,
            len: 0,
        };

        Stmt {
            data: Located::new(
                span,
                StmtData::Module {
                    public: true,
                    body: stmts,
                    name: Located::new(span, name),
                    file: true,
                },
            ),
            attrs: Default::default(),
        }
    }

    //

    fn try_item(&mut self) -> Result<Stmt, (Option<Located<Token<'a>>>, Attributes)> {
        let attrs = self.attributes();
        let public = self.next_if(Token::Pub);
        let is_extern = self.next_if(Token::Extern);
        let is_unsafe = self.next_if(Token::Unsafe);
        let conf = FnConfig {
            tk_public: public.as_ref().map(|t| t.span),
            tk_extern: is_extern.as_ref().map(|t| t.span),
            tk_unsafe: is_unsafe.as_ref().map(|t| t.span),
            require_body: is_extern.is_none(),
            forced_pub: false,
        };
        match self.try_function(conf, attrs.clone()) {
            Some(Left(func)) => {
                return Ok(Stmt {
                    attrs,
                    data: func.map(StmtData::Fn),
                });
            }
            Some(Right(func)) => {
                self.error(Error::new(
                    "operator functions can only be defined in types and extensions",
                    func.data.name.span,
                ));
                return Ok(Stmt {
                    attrs,
                    data: func.map(|func| {
                        StmtData::Fn(Fn::from_operator_fn(func.name.data.to_string(), func))
                    }),
                });
            }
            None => {}
        };

        let peek = self.peek();
        let earliest_span = conf
            .tk_public
            .or(conf.tk_extern)
            .or(conf.tk_unsafe)
            .unwrap_or(peek.span);

        match peek.data {
            Token::Struct | Token::Packed => {
                let token = self.next();
                if token.data == Token::Packed {
                    self.expect(Token::Struct);
                }

                if let Some(token) = is_unsafe {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                if let Some(token) = is_extern {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                // TODO: packed struct Foo: u64 {}
                Ok(Stmt {
                    attrs,
                    data: self
                        .structure(public.is_some(), earliest_span, false)
                        .map(|base| StmtData::Struct {
                            base,
                            packed: matches!(token.data, Token::Packed),
                        }),
                })
            }
            Token::Union => {
                self.next();
                if let Some(token) = is_extern {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                Ok(Stmt {
                    attrs,
                    data: if is_unsafe.is_some() {
                        self.structure(public.is_some(), earliest_span, true)
                            .map(StmtData::UnsafeUnion)
                    } else {
                        self.union(public.is_some(), earliest_span)
                    },
                })
            }
            Token::Trait | Token::Sealed => {
                let sealed = self.next().data == Token::Sealed;
                if sealed {
                    self.expect(Token::Trait);
                }

                if let Some(token) = is_extern {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                Ok(Stmt {
                    attrs,
                    data: self.r#trait(
                        public.is_some(),
                        earliest_span,
                        is_unsafe.is_some(),
                        sealed,
                    ),
                })
            }
            Token::Extension => {
                self.next();
                if let Some(token) = is_unsafe {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                if let Some(token) = is_extern {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                Ok(Stmt {
                    attrs,
                    data: self.extension(public.is_some(), earliest_span),
                })
            }
            Token::Mod => {
                self.next();
                if let Some(token) = is_unsafe {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                if let Some(token) = is_extern {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                let name = self.expect_ident("expected name");
                let mut body = Vec::new();
                self.expect(Token::LCurly);
                let span =
                    self.next_until(Token::RCurly, earliest_span, |this| body.push(this.item()));
                Ok(Stmt {
                    data: Located::new(
                        span,
                        StmtData::Module {
                            public: public.is_some(),
                            file: false,
                            name,
                            body,
                        },
                    ),
                    attrs,
                })
            }
            Token::Use => {
                self.next();
                if let Some(token) = is_unsafe {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                if let Some(token) = is_extern {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                let mut components = Vec::new();
                let origin = if self.next_if(Token::ScopeRes).is_some() {
                    let ident = self.expect_ident("expected path component");
                    if self.next_if(Token::ScopeRes).is_none() {
                        let end = self.expect(Token::Semicolon);
                        return Ok(Stmt {
                            data: Located::new(
                                earliest_span.extended_to(end.span),
                                StmtData::Use(UsePath {
                                    components,
                                    origin: PathOrigin::Root,
                                    public: public.is_some(),
                                    tail: UsePathTail::Ident(ident),
                                }),
                            ),
                            attrs,
                        });
                    }

                    components.push(ident);
                    PathOrigin::Root
                } else if let Some(token) = self.next_if(Token::Super) {
                    self.expect(Token::ScopeRes);
                    PathOrigin::Super(token.span)
                } else {
                    let ident = self.expect_ident("expected path component");
                    if self.next_if(Token::ScopeRes).is_none() {
                        let end = self.expect(Token::Semicolon);
                        return Ok(Stmt {
                            data: Located::new(
                                earliest_span.extended_to(end.span),
                                StmtData::Use(UsePath {
                                    components,
                                    origin: PathOrigin::Normal,
                                    public: public.is_some(),
                                    tail: UsePathTail::Ident(ident),
                                }),
                            ),
                            attrs,
                        });
                    }

                    components.push(ident);
                    PathOrigin::Normal
                };

                loop {
                    if self.next_if(Token::Asterisk).is_some() {
                        let end = self.expect(Token::Semicolon);
                        return Ok(Stmt {
                            data: Located::new(
                                earliest_span.extended_to(end.span),
                                StmtData::Use(UsePath {
                                    components,
                                    origin,
                                    public: public.is_some(),
                                    tail: UsePathTail::All,
                                }),
                            ),
                            attrs,
                        });
                    }

                    let ident = self.expect_ident("expected path component or '*'");
                    if self.next_if(Token::ScopeRes).is_none() {
                        let end = self.expect(Token::Semicolon);
                        return Ok(Stmt {
                            data: Located::new(
                                earliest_span.extended_to(end.span),
                                StmtData::Use(UsePath {
                                    components,
                                    origin,
                                    public: public.is_some(),
                                    tail: UsePathTail::Ident(ident),
                                }),
                            ),
                            attrs,
                        });
                    }

                    components.push(ident);
                }
            }
            Token::Static | Token::Const => {
                let token = self.next();
                if let Some(token) = is_unsafe {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                if let Some(ext) = &is_extern
                    && token.data.is_const()
                {
                    self.error_no_sync(Error::not_valid_here(ext));
                }

                let name = self.expect_ident("expected name");
                self.expect(Token::Colon);
                let ty = self.type_hint();
                let value = self.next_if(Token::Assign).map(|_| self.expression());
                let end = self.expect(Token::Semicolon);
                Ok(Stmt {
                    data: Located::new(
                        earliest_span.extended_to(end.span),
                        StmtData::Binding {
                            public: public.is_some(),
                            constant: matches!(token.data, Token::Const),
                            is_extern: is_extern.is_some(),
                            name,
                            ty,
                            value,
                        },
                    ),
                    attrs,
                })
            }
            _ => {
                if let Some(token) = is_extern {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                Err((is_unsafe, attrs))
            }
        }
    }

    fn item(&mut self) -> Stmt {
        self.try_item().unwrap_or_else(|(_, attrs)| {
            let span = self.next().span;
            self.error(Error::new("expected item", span));
            Stmt {
                data: Located::new(span, StmtData::Error),
                attrs,
            }
        })
    }

    fn statement(&mut self) -> Stmt {
        let stmt = match self.try_item() {
            Ok(item) => item,
            Err((is_unsafe, attrs)) => match self.peek().data {
                Token::Let | Token::Mut => {
                    let token = self.next();
                    if let Some(is_unsafe) = is_unsafe {
                        self.error_no_sync(Error::not_valid_here(&is_unsafe));
                    }

                    let patt = self.pattern(matches!(token.data, Token::Mut));
                    let ty = self.next_if(Token::Colon).map(|_| self.type_hint());
                    let value = self.next_if(Token::Assign).map(|_| self.expression());
                    let end = self.expect(Token::Semicolon);
                    Stmt {
                        data: Located::new(
                            token.span.extended_to(end.span),
                            StmtData::Let { ty, value, patt },
                        ),
                        attrs,
                    }
                }
                Token::Defer => {
                    let mut span = self.next().span;

                    let (needs_semicolon, expr) = self.block_or_normal_expr(None);
                    if needs_semicolon {
                        span.extend_to(self.expect(Token::Semicolon).span);
                    } else {
                        span.extend_to(expr.span);
                    }
                    Stmt {
                        attrs,
                        data: Located::new(span, StmtData::Defer(expr)),
                    }
                }
                Token::Guard => {
                    let span = self.next().span;
                    let cond = self.expression();
                    self.expect(Token::Else);
                    let lcurly = self.expect(Token::LCurly);
                    let body = self.block_expr(lcurly.span, None);
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
                    if self.matches(Token::RCurly) {
                        expr = Expr::new(expr.span, ExprData::Tail(expr.into()));
                    } else if needs_semicolon {
                        span.extend_to(self.expect(Token::Semicolon).span);
                    }

                    if let Some(is_unsafe) = is_unsafe {
                        expr = Expr::new(
                            is_unsafe.span.extended_to(span),
                            ExprData::Unsafe(expr.into()),
                        );
                    }

                    Stmt {
                        attrs,
                        data: Located::new(span, StmtData::Expr(expr)),
                    }
                }
            },
        };

        self.next_if(Token::Semicolon);
        if self.needs_sync {
            self.synchronize();
        }

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
            Token::Void => Expr::new(span, ExprData::Void),
            Token::False => Expr::new(span, ExprData::Bool(false)),
            Token::True => Expr::new(span, ExprData::Bool(true)),
            Token::Int { base, value, width } => Expr::new(
                span,
                ExprData::Integer(IntPattern {
                    base,
                    value: value.into(),
                    width: width.map(|w| w.into()),
                    negative: false,
                }),
            ),
            Token::Float(value) => Expr::new(span, ExprData::Float(value.into())),
            Token::String(value) => Expr::new(span, ExprData::String(value.into())),
            Token::Char(value) => Expr::new(span, ExprData::Char(value)),
            Token::ByteString(value) => Expr::new(span, ExprData::ByteString(value)),
            Token::ByteChar(value) => Expr::new(span, ExprData::ByteChar(value)),
            Token::This => Expr::new(
                span,
                ExprData::Path(Located::new(span, THIS_PARAM.to_owned()).into()),
            ),
            Token::ThisType => Expr::new(
                span,
                ExprData::Path(Located::new(span, THIS_TYPE.to_owned()).into()),
            ),
            Token::Ident(ident) => {
                let data = self.path_components(Some(Located::new(span, ident.into())), &mut span);
                Expr::new(span, ExprData::Path(Path::new(PathOrigin::Normal, data)))
            }
            Token::ScopeRes => {
                let ident = self.expect_ident("expected name");
                let data = self.path_components(Some(ident), &mut span);
                Expr::new(span, ExprData::Path(Path::new(PathOrigin::Root, data)))
            }
            Token::Super => {
                let original = span;
                let mut data = self.path_components(None, &mut span);
                if data.is_empty() {
                    data.push((Located::new(self.peek().span, "".into()), vec![]));
                }
                Expr::new(
                    span,
                    ExprData::Path(Path::new(PathOrigin::Super(original), data)),
                )
            }
            Token::Colon => {
                let ident = self.expect_ident("expected identifier");
                Expr::new(
                    span.extended_to(ident.span),
                    ExprData::Path(Path::new(
                        PathOrigin::Infer,
                        vec![(ident, Default::default())],
                    )),
                )
            }
            Token::StringPart(value) => {
                let mut parts = vec![
                    Expr::new(span, ExprData::String(value.into())),
                    self.expression(),
                ];
                while let Some(part) = self.next_if_map(|t| {
                    t.data
                        .as_string_part()
                        .map(|s| Located::new(t.span, s.clone().into_owned()))
                }) {
                    if self.matches_pred(|t| matches!(t, Token::StringPart(_) | Token::String(_))) {
                        let span = self.peek().span;
                        self.error(Error::new("expected expression", span));
                        parts.push(part.map(ExprData::String));
                    } else {
                        let expr = self.expression();
                        span.extend_to(expr.span);
                        parts.push(part.map(ExprData::String));
                        parts.push(expr);
                        // TODO: ':' format options
                    }
                }

                match self.next() {
                    Located {
                        span: inner,
                        data: Token::String(data),
                    } => {
                        span.extend_to(inner);
                        parts.push(Expr::new(inner, ExprData::String(data.into())));
                    }
                    token => self.error(Error::new("expected end of string", token.span)),
                }

                Expr::new(span, ExprData::StringInterpolation(parts))
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
                    && let ExprData::Integer(patt) = &mut expr.data
                {
                    expr.span = span.extended_to(expr.span);
                    patt.negative = true;
                    return expr;
                }

                if double_opt {
                    expr = Expr::new(
                        expr.span,
                        ExprData::Unary {
                            op,
                            expr: expr.into(),
                        },
                    );
                }

                Expr::new(
                    span.extended_to(expr.span),
                    ExprData::Unary {
                        op,
                        expr: expr.into(),
                    },
                )
            }
            // complex expressions
            Token::LParen => {
                let expr = self.expression();
                if self.matches(Token::Comma) {
                    self.csv(vec![expr], Token::RParen, span, Self::expression)
                        .map(ExprData::Tuple)
                } else {
                    let end = self.expect(Token::RParen);
                    Expr::new(span.extended_to(end.span), expr.data)
                }
            }
            Token::Range => {
                if self.is_range_end(ctx) {
                    Expr::new(
                        span,
                        ExprData::Range {
                            start: None,
                            end: None,
                            inclusive: false,
                        },
                    )
                } else {
                    let end = self.precedence(data.precedence(), ctx);
                    Expr::new(
                        span.extended_to(end.span),
                        ExprData::Range {
                            start: None,
                            end: Some(end.into()),
                            inclusive: false,
                        },
                    )
                }
            }
            Token::RangeInclusive => {
                let end = self.precedence(data.precedence(), ctx);
                Expr::new(
                    span.extended_to(end.span),
                    ExprData::Range {
                        start: None,
                        end: Some(end.into()),
                        inclusive: true,
                    },
                )
            }
            Token::LCurly => self.block_expr(span, None),
            Token::If => self.if_expr(span),
            Token::While => self.while_expr(span, None),
            Token::Loop => self.loop_expr(span, None),
            Token::For => self.for_expr(span, None),
            Token::Match => self.match_expr(span),
            Token::LBrace => {
                if let Some(rbrace) = self.next_if(Token::RBrace) {
                    Expr::new(span.extended_to(rbrace.span), ExprData::Array(Vec::new()))
                } else if self.next_if(Token::Colon).is_some() {
                    Expr::new(
                        span.extended_to(self.expect(Token::RBrace).span),
                        ExprData::Map(Vec::new()),
                    )
                } else {
                    let expr = self.expression();
                    if self.next_if(Token::Colon).is_some() {
                        let value = self.expression();
                        self.csv(vec![(expr, value)], Token::RBrace, span, |this| {
                            let key = this.expression();
                            this.expect(Token::Colon);
                            (key, this.expression())
                        })
                        .map(ExprData::Map)
                    } else if self.next_if(Token::Semicolon).is_some() {
                        let count = self.expression();
                        let rbrace = self.expect(Token::RBrace);
                        Expr::new(
                            span.extended_to(rbrace.span),
                            ExprData::ArrayWithInit {
                                init: expr.into(),
                                count: count.into(),
                            },
                        )
                    } else if let Some(rbrace) = self.next_if(Token::RBrace) {
                        Expr::new(span.extended_to(rbrace.span), ExprData::Array(vec![expr]))
                    } else {
                        self.csv(vec![expr], Token::RBrace, span, Self::expression)
                            .map(ExprData::Array)
                    }
                }
            }
            Token::At => {
                if self.next_if(Token::LBrace).is_some() {
                    if let Some(rbrace) = self.next_if(Token::RBrace) {
                        Expr::new(span.extended_to(rbrace.span), ExprData::Vec(Vec::new()))
                    } else {
                        let expr = self.expression();
                        if self.next_if(Token::Semicolon).is_some() {
                            let count = self.expression();
                            let rbrace = self.expect(Token::RBrace);
                            Expr::new(
                                span.extended_to(rbrace.span),
                                ExprData::VecWithInit {
                                    init: expr.into(),
                                    count: count.into(),
                                },
                            )
                        } else {
                            self.csv(vec![expr], Token::RBrace, span, Self::expression)
                                .map(ExprData::Vec)
                        }
                    }
                } else {
                    let label = self.expect_ident("expected label identifier or vector literal");
                    self.expect(Token::Colon);
                    self.block_or_normal_expr(Some(label)).1
                }
            }
            Token::Hash => {
                self.expect(Token::LBrace);
                self.csv(Vec::new(), Token::RBrace, span, Self::expression)
                    .map(ExprData::Set)
            }
            Token::Move => {
                let token = self.next();
                if !matches!(token.data, Token::BitOr) {
                    self.error_no_sync(Error::new("expected '|'", token.span));
                }

                self.lambda_expr(token, true)
            }
            Token::BitOr => self.lambda_expr(Located::new(span, data), false),
            Token::Return => {
                let (span, expr) = if !self.is_range_end(EvalContext::Normal) {
                    let expr = self.expression();
                    (span.extended_to(expr.span), expr.into())
                } else {
                    (span, Expr::new(span, ExprData::Void).into())
                };

                Expr::new(span, ExprData::Return(expr))
            }
            Token::Break => {
                let label = self
                    .next_if(Token::At)
                    .map(|_| self.expect_ident("expected label name"));
                let (span, expr) = if !self.is_range_end(EvalContext::Normal) {
                    let expr = self.expression();
                    (span.extended_to(expr.span), Some(expr.into()))
                } else {
                    (span, None)
                };

                Expr::new(span, ExprData::Break(expr, label))
            }
            Token::Unsafe => {
                let expr = self.precedence(Precedence::Min, ctx);
                Expr::new(span.extended_to(expr.span), ExprData::Unsafe(expr.into()))
            }
            Token::Continue => {
                let label = self
                    .next_if(Token::At)
                    .map(|_| self.expect_ident("expected label name"));
                Expr::new(span, ExprData::Continue(label))
            }
            _ => {
                self.error(Error::new("unexpected token", span));
                Expr::new(span, ExprData::Error)
            }
        }
    }

    fn infix(&mut self, left: Expr, op: Located<Token>, ctx: EvalContext) -> Expr {
        match op.data {
            Token::Increment | Token::Decrement | Token::Exclamation | Token::Question => {
                Expr::new(
                    left.span.extended_to(op.span),
                    ExprData::Unary {
                        op: UnaryOp::try_from_postfix(op.data).unwrap(),
                        expr: left.into(),
                    },
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
                Expr::new(
                    left.span.extended_to(right.span),
                    ExprData::Binary {
                        op: op.data.try_into().unwrap(),
                        left: left.into(),
                        right: right.into(),
                    },
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
                Expr::new(
                    left.span.extended_to(right.span),
                    ExprData::Binary {
                        op: op.data.try_into().unwrap(),
                        left: left.into(),
                        right: right.into(),
                    },
                )
            }
            Token::Range | Token::RangeInclusive => {
                let inclusive = op.data == Token::RangeInclusive;
                if self.is_range_end(ctx) {
                    Expr::new(
                        left.span.extended_to(op.span),
                        ExprData::Range {
                            start: Some(left.into()),
                            end: None,
                            inclusive,
                        },
                    )
                } else {
                    let right = self.precedence(op.data.precedence(), ctx);
                    Expr::new(
                        left.span.extended_to(right.span),
                        ExprData::Range {
                            start: Some(left.into()),
                            end: Some(right.into()),
                            inclusive,
                        },
                    )
                }
            }
            Token::Is => Expr::new(
                left.span,
                ExprData::Is {
                    expr: left.into(),
                    pattern: self.pattern_ex(false, ctx),
                },
            ),
            Token::As => {
                let bang = self.next_if(Token::Exclamation);
                let ty = self.type_hint();
                // FIXME: type_hint should just be located
                let mut next = self.peek().span;
                next.len = 0;
                Expr::new(
                    left.span.extended_to(next),
                    ExprData::As {
                        expr: left.into(),
                        ty,
                        throwing: bang.is_some(),
                    },
                )
            }
            Token::Dot => {
                let token = self.peek();
                if let Token::Int { base, value, width } = token.data {
                    let member = Located::new(token.span, value.into());
                    if base != 10 || width.is_some() || (value.starts_with('0') && value.len() > 1)
                    {
                        let span = token.span;
                        self.error(Error::new(
                            "tuple member access must be an integer with no prefix or suffix",
                            span,
                        ));
                    }
                    let span = self.next().span;
                    return Expr::new(
                        left.span.extended_to(span),
                        ExprData::Member {
                            source: left.into(),
                            member,
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

                generics.map(|generics| ExprData::Member {
                    source: left.into(),
                    member,
                    generics,
                })
            }
            Token::LParen | Token::LBrace => {
                let closing = if op.data == Token::LParen {
                    Token::RParen
                } else {
                    Token::RBrace
                };
                let args = self.csv(Vec::new(), closing, left.span, |this| {
                    let mut expr = this.expression();
                    let mut name = None;
                    if let ExprData::Path(path) = &expr.data
                        && let Some(ident) = path
                            .as_identifier_l()
                            .filter(|_| this.next_if(Token::Colon).is_some())
                    {
                        name = Some(ident.clone());
                        if !this.matches_pred(|t| matches!(t, Token::Comma | Token::RParen)) {
                            expr = this.expression();
                        }
                    }
                    (name, expr)
                });

                if op.data == Token::LParen {
                    args.map(|args| ExprData::Call {
                        callee: left.into(),
                        args,
                    })
                } else {
                    args.map(|args| ExprData::Subscript {
                        callee: left.into(),
                        args,
                    })
                }
            }
            Token::Then => {
                let if_branch = self.expression();
                let else_branch = self
                    .next_if(Token::Else)
                    .map(|_| Box::new(self.precedence(op.data.precedence(), ctx))); /* EvalContext::IfWhile? */
                Expr::new(
                    left.span
                        .extended_to(else_branch.as_ref().map_or(if_branch.span, |e| e.span)),
                    ExprData::If {
                        cond: left.into(),
                        if_branch: if_branch.into(),
                        else_branch,
                    },
                )
            }
            _ => {
                self.error(Error::new("unexpected token", op.span));
                Expr::new(op.span, ExprData::Error)
            }
        }
    }

    //

    fn label_error(&mut self, label: Located<String>) {
        self.error_no_sync(Error::new(
            "labels are only valid for loop expressions",
            label.span,
        ));
    }

    fn block_or_normal_expr(&mut self, label: Option<Located<String>>) -> (bool, Expr) {
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
                    self.label_error(label)
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
                (
                    matches!(&expr.data, ExprData::Loop { do_while: true, .. }),
                    expr,
                )
            }
            Token::For => {
                let token = self.next();
                (false, self.for_expr(token.span, label.map(|l| l.data)))
            }
            Token::Match => {
                let token = self.next();
                if let Some(label) = label {
                    self.label_error(label)
                }
                (false, self.match_expr(token.span))
            }
            Token::LCurly => {
                let token = self.next();
                (false, self.block_expr(token.span, label.map(|l| l.data)))
            }
            Token::Unsafe => {
                self.next();
                if let Some(label) = label {
                    self.label_error(label)
                }

                let (needs_delim, expr) = self.block_or_normal_expr(None);
                (
                    needs_delim,
                    Expr::new(expr.span, ExprData::Unsafe(expr.into())),
                )
            }
            _ => {
                if let Some(label) = label {
                    self.label_error(label)
                }
                (true, self.expression())
            }
        }
    }

    fn if_expr(&mut self, token: Span) -> Expr {
        let cond = self.precedence(Precedence::Min, EvalContext::IfWhile);
        let lcurly = self.expect(Token::LCurly);
        let if_branch = self.block_expr(lcurly.span, None);
        let else_branch = self.next_if(Token::Else).map(|_| {
            if !self.matches(Token::If) {
                let lcurly = self.expect(Token::LCurly);
                self.block_expr(lcurly.span, None)
            } else {
                self.precedence(Precedence::Min, EvalContext::IfWhile)
            }
        });
        Expr::new(
            token.extended_to(else_branch.as_ref().map_or(if_branch.span, |e| e.span)),
            ExprData::If {
                cond: cond.into(),
                if_branch: if_branch.into(),
                else_branch: else_branch.map(|e| e.into()),
            },
        )
    }

    fn while_expr(&mut self, token: Span, label: Option<String>) -> Expr {
        let cond = self.precedence(Precedence::Min, EvalContext::IfWhile);
        let Located { span, data: body } = self.block();
        Expr::new(
            token.extended_to(span),
            ExprData::Loop {
                cond: Some(cond.into()),
                body,
                do_while: false,
                label,
            },
        )
    }

    fn loop_expr(&mut self, mut token: Span, label: Option<String>) -> Expr {
        let body = self.block().data;
        let (cond, do_while) = self
            .next_if(Token::While)
            .map(|_| {
                let cond = self.expression();
                token.extend_to(cond.span);
                (Some(cond.into()), true)
            })
            .unwrap_or_default();

        Expr::new(
            token,
            ExprData::Loop {
                cond,
                body,
                do_while,
                label,
            },
        )
    }

    fn for_expr(&mut self, token: Span, label: Option<String>) -> Expr {
        let patt = self.pattern(false);
        self.expect(Token::In);
        let iter = self.precedence(Precedence::Min, EvalContext::For);
        let Located { span, data: body } = self.block();
        Expr::new(
            token.extended_to(span),
            ExprData::For {
                patt,
                iter: iter.into(),
                body,
                label,
            },
        )
    }

    fn match_expr(&mut self, token: Span) -> Expr {
        let expr = self.expression();
        self.expect(Token::LCurly);
        let mut body = Vec::new();
        let span = self.next_until(Token::RCurly, token, |this| {
            let pattern = this.pattern(false).map(|data| FullPattern {
                data,
                if_expr: this.next_if(Token::If).map(|_| this.expression().into()),
            });
            this.expect(Token::FatArrow);
            let (needs_comma, expr) = this.block_or_normal_expr(None);
            if needs_comma {
                this.expect(Token::Comma);
            } else {
                this.next_if(Token::Comma);
            }

            body.push((pattern, expr));
        });

        Expr::new(
            span,
            ExprData::Match {
                expr: expr.into(),
                body,
            },
        )
    }

    fn block_expr(&mut self, token: Span, label: Option<String>) -> Expr {
        let mut stmts = Vec::new();
        let span = self.next_until(Token::RCurly, token, |this| {
            while this.next_if(Token::Semicolon).is_some() {}
            if this.peek().data.is_r_curly() {
                return;
            }

            stmts.push(this.statement());
        });
        Expr::new(span, ExprData::Block(stmts, label))
    }

    fn lambda_expr(&mut self, head: Located<Token>, moves: bool) -> Expr {
        let params = if head.data == Token::BitOr {
            self.csv(Vec::new(), Token::BitOr, head.span, |this| {
                (
                    this.expect_ident("expected parameter name"),
                    this.next_if(Token::Colon).map(|_| this.type_hint()),
                )
            })
            .data
        } else {
            Vec::new()
        };

        let ret = self.next_if(Token::Colon).map(|_| self.type_hint());
        let body = if ret.is_none() {
            self.expression()
        } else {
            let token = self.expect(Token::LCurly);
            self.block_expr(token.span, None)
        };

        Expr::new(
            head.span.extended_to(body.span),
            ExprData::Lambda {
                params,
                ret,
                moves,
                body: body.into(),
            },
        )
    }

    //

    fn path_components(
        &mut self,
        first: Option<Located<String>>,
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

    fn type_path(&mut self) -> Path {
        let origin = match self.peek().data {
            Token::ScopeRes => {
                self.next();
                PathOrigin::Root
            }
            Token::Super => {
                let span = self.next().span;
                self.expect(Token::ScopeRes);
                PathOrigin::Super(span)
            }
            _ => PathOrigin::Normal,
        };
        let mut data = Vec::new();
        loop {
            let ident = self.expect_ident("expected type name");
            if self.next_if(Token::LAngle).is_some() {
                let params = self.rangle_csv_one(ident.span, Self::type_hint);
                data.push((ident, params.data));
            } else {
                data.push((ident, Vec::new()));
            }

            if self.next_if(Token::ScopeRes).is_none() {
                break;
            }
        }

        Path::new(origin, data)
    }

    //

    fn int_pattern(negative: Option<Span>, t: &Located<Token>) -> Option<Located<IntPattern>> {
        match t.data {
            Token::Int { base, value, width } => Some(Located::new(
                negative
                    .map(|span| span.extended_to(t.span))
                    .unwrap_or(t.span),
                IntPattern {
                    negative: negative.is_some(),
                    base,
                    value: value.into(),
                    width: width.map(|w| w.into()),
                },
            )),
            Token::ByteChar(value) => Some(Located::new(
                negative
                    .map(|span| span.extended_to(t.span))
                    .unwrap_or(t.span),
                IntPattern {
                    negative: negative.is_some(),
                    base: 10,
                    value: value.to_string(),
                    width: Some("u8".into()),
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
            let Some(end) = self.expect_map(
                |t| Self::int_pattern(negative.map(|t| t.span), t),
                "expected number",
            ) else {
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
        if let Some(token) = self.next_if(Token::True) {
            return Some(token.map(|_| Pattern::Bool(true)));
        } else if let Some(token) = self.next_if(Token::False) {
            return Some(token.map(|_| Pattern::Bool(false)));
        } else if let Some(token) = self.next_if(Token::Void) {
            return Some(token.map(|_| Pattern::Void));
        } else if let Some(string) = self.next_if_map(|t| {
            t.data
                .as_string()
                .map(|value| Located::new(t.span, value.to_string()))
        }) {
            return Some(string.map(Pattern::String));
        }

        let range = self.next_if_pred(|t| matches!(t, Token::Range | Token::RangeInclusive));
        if let Some(token) = self.next_if(Token::Minus) {
            let Some(start) = self.expect_map(
                |t| Self::int_pattern(Some(token.span), t),
                "expected number",
            ) else {
                return Some(Located::new(token.span, Pattern::Error));
            };

            return Some(self.maybe_range_pattern(range, start));
        }

        if let Some(int) = self.next_if_map(|t| Self::int_pattern(None, t)) {
            return Some(self.maybe_range_pattern(range, int));
        }

        if let Some(char) =
            self.next_if_map(|t| t.data.as_char().map(|&ch| Located::new(t.span, ch)))
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
                |t| t.data.as_char().map(|&ch| Located::new(t.span, ch)),
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
            if let Some(token) = mutable.clone().filter(|_| mut_var) {
                this.diag.warn(Error::new(
                    format!("redundant '{}'", Token::Mut),
                    token.span,
                ));
            }
            let mutable = mutable.is_some();
            let name = this.expect_ident("expected name");
            if mutable || this.next_if(Token::Colon).is_none() {
                let span = name.span;
                Destructure {
                    name: name.clone(),
                    mutable: mutable || mut_var,
                    pattern: name.map(|name| Pattern::Path(Path::from(Located::new(span, name)))),
                }
            } else {
                Destructure {
                    name,
                    mutable: mutable || mut_var,
                    pattern: this.pattern(false),
                }
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
                self.next();
                return self
                    .pattern_impl(false, ctx)
                    .map(|inner| Pattern::Option(inner.into()));
            }
            Token::NoneCoalesce => {
                self.next();
                return self
                    .pattern_impl(false, ctx)
                    .map(|inner| Pattern::Option(Pattern::Option(inner.into()).into()));
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
                        if let Some(token) = this.next_if(Token::Ellipses) {
                            let pattern = if this.next_if(Token::Mut).is_some() {
                                let ident = this.expect_ident("expected name");
                                Some((true, ident))
                            } else {
                                let ident = this.next_if_map(|t| {
                                    t.data
                                        .as_ident()
                                        .map(|&i| Located::new(t.span, i.to_string()))
                                });
                                Some(false).zip(ident)
                            };

                            Located::new(token.span, Pattern::Rest(pattern))
                        } else {
                            this.pattern(mut_var)
                        }
                    })
                    .map(Pattern::Array);
            }
            Token::Colon => {
                self.next();
                let ident = self.expect_ident("expected identifier");
                Path::new(PathOrigin::Infer, vec![(ident, Default::default())])
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
        if self.peek().data == Token::BitOr {
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
        self.matches_pred(|k| {
            matches!(
                k,
                Token::Semicolon | Token::Comma | Token::RBrace | Token::RParen
            ) || matches!((k, ctx), (Token::LCurly, EvalContext::For))
        })
    }

    fn attribute(&mut self) -> Attribute {
        Attribute {
            name: self.expect_ident("expected name"),
            props: self
                .next_if(Token::LParen)
                .map(|tk| self.csv_one(Token::RParen, tk.span, Self::attribute).data)
                .unwrap_or_default(),
        }
    }

    fn attributes(&mut self) -> Attributes {
        let mut attrs = vec![];
        while let Some(token) = self.next_if(Token::AtLParen) {
            let attr = self.csv_one(Token::RParen, token.span, Self::attribute);
            attrs.extend(attr.data);
        }
        Attributes::new(attrs)
    }

    //

    fn type_params(&mut self) -> TypeParams {
        self.next_if(Token::LAngle)
            .map(|tk| {
                self.rangle_csv_one(tk.span, |this| {
                    (this.expect_ident("expected type name"), this.trait_impls())
                })
                .data
            })
            .unwrap_or_default()
    }

    fn trait_impls(&mut self) -> Vec<Path> {
        let mut impls = Vec::new();
        if self.next_if(Token::Colon).is_some() {
            loop {
                impls.push(self.type_path());
                if self.next_if(Token::Plus).is_none() {
                    break;
                }
            }
        }
        impls
    }

    fn type_hint(&mut self) -> TypeHint {
        match self.peek().data {
            Token::Asterisk => {
                self.next();
                if self.next_if(Token::Mut).is_some() {
                    TypeHint::MutPtr(self.type_hint().into())
                } else if self.next_if(Token::Dyn).is_some() {
                    if self.next_if(Token::Mut).is_some() {
                        TypeHint::DynMutPtr(self.type_path())
                    } else {
                        TypeHint::DynPtr(self.type_path())
                    }
                } else {
                    TypeHint::Ptr(self.type_hint().into())
                }
            }
            Token::Caret => {
                self.next();
                if self.next_if(Token::Mut).is_some() {
                    TypeHint::RawMutPtr(self.type_hint().into())
                } else {
                    TypeHint::RawPtr(self.type_hint().into())
                }
            }
            Token::Question => {
                self.next();
                TypeHint::Option(self.type_hint().into())
            }
            Token::NoneCoalesce => {
                self.next();
                TypeHint::Option(TypeHint::Option(self.type_hint().into()).into())
            }
            Token::LBrace => {
                self.next();
                if self.next_if(Token::Mut).is_some() {
                    let inner = self.type_hint();
                    self.expect(Token::Range);
                    self.expect(Token::RBrace);
                    TypeHint::SliceMut(inner.into())
                } else {
                    let inner = self.type_hint();
                    if self.next_if(Token::RBrace).is_some() {
                        TypeHint::Vec(inner.into())
                    } else if self.next_if(Token::Range).is_some() {
                        self.expect(Token::RBrace);
                        TypeHint::Slice(inner.into())
                    } else if self.next_if(Token::Semicolon).is_some() {
                        let count = self.expression();
                        self.expect(Token::RBrace);
                        TypeHint::Array(inner.into(), count.into())
                    } else if self.next_if(Token::Colon).is_some() {
                        let value = self.type_hint();
                        self.expect(Token::RBrace);
                        TypeHint::Map(inner.into(), value.into())
                    } else {
                        let span = self.next().span;
                        self.error(Error::new("expected ']', ';', or ':'", span));
                        TypeHint::Error
                    }
                }
            }
            Token::Hash => {
                self.next();
                self.expect(Token::LBrace);
                let inner = self.type_hint().into();
                self.expect(Token::RBrace);
                TypeHint::Set(inner)
            }
            Token::LParen => {
                let left = self.next();
                TypeHint::Tuple(self.csv_one(Token::RParen, left.span, Self::type_hint).data)
            }
            Token::Void => {
                self.next();
                TypeHint::Void
            }
            Token::ThisType => TypeHint::This(self.next().span),
            Token::Fn => {
                self.next();
                let left = self.expect(Token::LParen);
                let params = self
                    .csv(Vec::new(), Token::RParen, left.span, Self::type_hint)
                    .data;
                let ret = if self.next_if(Token::FatArrow).is_some() {
                    self.type_hint()
                } else {
                    TypeHint::Void
                };

                TypeHint::Fn {
                    is_extern: false,
                    params,
                    ret: ret.into(),
                }
            }
            Token::Struct => {
                let span = self.next().span;
                self.expect(Token::LCurly);
                TypeHint::AnonStruct(
                    self.csv(Vec::new(), Token::RCurly, span, |this| {
                        let name = this.expect_ident("expected member name");
                        this.expect(Token::Colon);
                        let ty = this.type_hint();
                        (name.data, ty)
                    })
                    .data,
                )
            }
            _ => TypeHint::Regular(self.type_path()),
        }
    }

    fn block(&mut self) -> Located<Vec<Stmt>> {
        let mut stmts = Vec::new();
        let lcurly = self.expect(Token::LCurly);
        let span = self.next_until(Token::RCurly, lcurly.span, |this| {
            while this.next_if(Token::Semicolon).is_some() {}
            if this.peek().data.is_r_curly() {
                return;
            }

            stmts.push(this.statement());
        });
        Located::new(span, stmts)
    }

    fn structure(&mut self, public: bool, span: Span, union: bool) -> Located<Struct> {
        let name = self.expect_ident("expected name");
        let type_params = self.type_params();

        self.expect(Token::LCurly);

        let mut functions = Vec::new();
        let mut operators = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();
        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            let public = this.next_if(Token::Pub);
            if let Some(token) = public.as_ref().filter(|_| union) {
                this.error_no_sync(Error::not_valid_here(token));
            }

            let config = FnConfig {
                tk_public: public.as_ref().map(|t| t.span),
                tk_unsafe: this.next_if(Token::Unsafe).map(|t| t.span),
                require_body: true,
                ..Default::default()
            };
            if config.tk_unsafe.is_some() {
                match this.expect_fn(config, attrs) {
                    Some(Left(func)) => functions.push(func),
                    Some(Right(func)) => operators.push(func),
                    _ => {}
                }
            } else if let Some(token) = this.next_if(Token::Impl) {
                if let Some(token) = public {
                    this.error_no_sync(Error::not_valid_here(&token));
                }

                impls.push(this.impl_block(attrs, token.span));
            } else if let Some(func) = this.try_function(config, attrs) {
                // TODO: apply the attributes to the impl block or next member
                match func {
                    Left(func) => functions.push(func),
                    Right(func) => operators.push(func),
                }
            } else {
                let name = this.expect_ident("expected name");
                this.expect(Token::Colon);
                let ty = this.type_hint();
                let value = this.next_if(Token::Assign).map(|_| this.expression());

                if !this.matches(Token::RCurly) {
                    this.expect(Token::Comma);
                }
                members.push(Member {
                    public: config.tk_public.is_some(),
                    ty,
                    name,
                    default: value,
                });
            }
        });

        Located::new(
            span,
            Struct {
                public,
                name,
                type_params,
                members,
                impls,
                functions,
                operators,
            },
        )
    }

    fn union(&mut self, public: bool, span: Span) -> Located<StmtData> {
        let name = self.expect_ident("expected name");
        let type_params = self.type_params();
        let tag = self.next_if(Token::Colon).map(|_| self.type_path());
        let mut functions = Vec::new();
        let mut operators = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();
        let mut variants = Vec::new();

        self.expect(Token::LCurly);
        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            let config = FnConfig {
                tk_public: this.next_if(Token::Pub).map(|t| t.span),
                tk_unsafe: this.next_if(Token::Unsafe).map(|t| t.span),
                require_body: true,
                ..Default::default()
            };
            if config.tk_public.is_some() || config.tk_unsafe.is_some() {
                match this.expect_fn(config, attrs) {
                    Some(Left(func)) => functions.push(func),
                    Some(Right(func)) => operators.push(func),
                    _ => {}
                }
            } else if let Some(token) = this.next_if(Token::Impl) {
                impls.push(this.impl_block(attrs, token.span));
            } else if let Some(func) = this.try_function(config, attrs) {
                // TODO: apply the attributes to the impl block or next member
                match func {
                    Left(func) => functions.push(func),
                    Right(func) => operators.push(func),
                }
            } else if this.next_if(Token::Shared).is_some() {
                // warn if pub was specified that it is useless
                let name = this.expect_ident("expected name");
                this.expect(Token::Colon);
                let ty = this.type_hint();
                let value = this.next_if(Token::Assign).map(|_| this.expression());
                this.expect(Token::Comma);

                members.push(Member {
                    public: true,
                    name,
                    ty,
                    default: value,
                });
            } else {
                let name = this.expect_ident("expected variant name");
                let data = match this.peek().data {
                    Token::LParen => {
                        let span = this.next().span;
                        VariantData::TupleLike(
                            this.csv_one(Token::RParen, span, |this| {
                                (
                                    this.type_hint(),
                                    this.next_if(Token::Assign).map(|_| this.expression()),
                                )
                            })
                            .data,
                        )
                    }
                    Token::LCurly => {
                        let span = this.next().span;
                        VariantData::StructLike(
                            this.csv_one(Token::RCurly, span, |this| {
                                if let Some(token) = this.next_if(Token::Pub) {
                                    this.error_no_sync(Error::not_valid_here(&token));
                                }

                                let name = this.expect_ident("expected name");
                                this.expect(Token::Colon);
                                Member {
                                    public: true,
                                    name,
                                    ty: this.type_hint(),
                                    default: this.next_if(Token::Assign).map(|_| this.expression()),
                                }
                            })
                            .data,
                        )
                    }
                    _ => VariantData::Empty,
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
                tag,
                variants,
                base: Struct {
                    public,
                    name,
                    type_params,
                    members,
                    functions,
                    impls,
                    operators,
                },
            },
        )
    }

    fn r#trait(
        &mut self,
        public: bool,
        span: Span,
        is_unsafe: bool,
        sealed: bool,
    ) -> Located<StmtData> {
        let name = self.expect_ident("expected name");
        let type_params = self.type_params();
        let impls = self.trait_impls();
        self.expect(Token::LCurly);

        let mut functions = Vec::new();
        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            let config = FnConfig {
                tk_unsafe: this.next_if(Token::Unsafe).map(|t| t.span),
                forced_pub: true,
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
            StmtData::Trait {
                public,
                sealed,
                is_unsafe,
                name,
                type_params,
                impls,
                functions,
            },
        )
    }

    fn extension(&mut self, public: bool, span: Span) -> Located<StmtData> {
        let name = self.expect_ident("expected name");
        let type_params = self.type_params();

        self.expect(Token::For);
        let ty = self.type_hint();
        self.expect(Token::LCurly);

        let mut functions = Vec::new();
        let mut operators = Vec::new();
        let mut impls = Vec::new();
        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            if let Some(token) = this.next_if(Token::Impl) {
                impls.push(this.impl_block(attrs, token.span));
            } else {
                let config = FnConfig {
                    require_body: true,
                    tk_public: this.next_if(Token::Pub).map(|c| c.span),
                    tk_unsafe: this.next_if(Token::Unsafe).map(|c| c.span),
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
            span,
            StmtData::Extension {
                public,
                name,
                ty,
                type_params,
                impls,
                functions,
                operators,
            },
        )
    }

    fn impl_block(&mut self, attrs: Attributes, span: Span) -> Located<ImplBlock> {
        let type_params = self.type_params();
        let path = self.type_path();
        self.expect(Token::LCurly);

        let mut functions = Vec::new();
        let span = self.next_until(Token::RCurly, span, |this| {
            let attrs = this.attributes();
            if let Some(token) = this.next_if(Token::Pub) {
                this.error_no_sync(Error::not_valid_here(&token));
            }

            let config = FnConfig {
                tk_unsafe: this.next_if(Token::Unsafe).map(|c| c.span),
                forced_pub: true,
                require_body: true,
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
            ImplBlock {
                attrs,
                type_params,
                path,
                functions,
            },
        )
    }

    fn try_function(
        &mut self,
        cfg: FnConfig,
        attrs: Attributes,
    ) -> Option<Either<Located<Fn>, Located<OperatorFn>>> {
        let (head_token, is_async) = if let Some(token) = self.next_if(Token::Fn) {
            (token, false)
        } else if let Some(token) = self.next_if(Token::Async) {
            self.expect(Token::Fn);
            (token, true)
        } else {
            return None;
        };
        let mut span = cfg
            .tk_public
            .or(cfg.tk_extern)
            .or(cfg.tk_unsafe)
            .unwrap_or(head_token.span);

        let name = self.expect_fn_name();
        let mut variadic = false;
        let type_params = self.type_params();
        let mut params = Vec::new();
        let mut count = 0;
        let mut has_default = false;
        self.expect(Token::LParen);
        while self
            .next_if_pred(|t| matches!(t, Token::RParen | Token::Eof))
            .is_none()
        {
            if self.next_if(Token::Ellipses).is_some() && name.data.is_left() {
                variadic = true;
                self.expect(Token::RParen);
                break;
            }

            let keyword = self.next_if(Token::Keyword).is_some();
            let my = self.next_if(Token::My);
            let mutable = self.next_if(Token::Mut).is_some();
            if let Some(token) = self.next_if(Token::This) {
                if count != 0 {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                let patt = if my.is_some() && mutable {
                    Pattern::MutBinding(THIS_PARAM.into())
                } else {
                    Pattern::Path(Path::from(Located::new(token.span, THIS_PARAM.to_string())))
                };

                params.push(Param {
                    keyword,
                    patt: Located::new(token.span, patt),
                    ty: if my.is_some() {
                        TypeHint::This(token.span)
                    } else if mutable {
                        TypeHint::MutPtr(TypeHint::This(token.span).into())
                    } else {
                        TypeHint::Ptr(TypeHint::This(token.span).into())
                    },
                    default: None,
                });
            } else {
                if let Some(token) = my {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

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

                params.push(Param {
                    patt,
                    keyword,
                    ty,
                    default,
                });
            }

            if !self.matches(Token::RParen) {
                self.expect(Token::Comma);
            }

            count += 1;
        }

        let ret = if self.next_if(Token::Colon).is_some() {
            self.type_hint()
        } else {
            TypeHint::Void
        };

        let body = if let Some(semi) = self.next_if(Token::Semicolon) {
            if cfg.require_body {
                self.error(Error::new("expected '{'", semi.span));
            }
            span.extend_to(semi.span);
            None
        } else {
            let lcurly = self.expect(Token::LCurly);
            let expr = self.block_expr(lcurly.span, None);
            span.extend_to(expr.span);
            Some(expr)
        };

        match name.data {
            Left(ident) => Some(Left(Located::new(
                span,
                Fn {
                    name: Located::new(name.span, ident),
                    public: cfg.forced_pub || cfg.tk_public.is_some(),
                    is_extern: cfg.tk_extern.is_some(),
                    is_async,
                    is_unsafe: cfg.tk_unsafe.is_some(),
                    variadic,
                    type_params,
                    params,
                    ret,
                    body,
                    attrs,
                    assign_subscript: false,
                },
            ))),
            Right(op) => {
                if is_async {
                    self.error(Error::not_valid_here(&head_token));
                }
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

    fn expect_fn_name(&mut self) -> Located<Either<String, OperatorFnType>> {
        let token = self.next();
        Located::new(
            token.span,
            match token.data {
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
                Token::LBrace => {
                    self.expect(Token::RBrace);
                    if self.next_if(Token::Assign).is_some() {
                        Right(OperatorFnType::SubscriptAssign)
                    } else {
                        Right(OperatorFnType::Subscript)
                    }
                }
                Token::Ident(name) => Left(name.into()),
                _ => {
                    self.error(Error::new("expected identifier", token.span));
                    Left(String::new())
                }
            },
        )
    }

    //

    fn csv<T>(
        &mut self,
        mut res: Vec<T>,
        end: Token,
        span: Span,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> Located<Vec<T>> {
        if !res.is_empty() && !self.matches(end.clone()) {
            self.expect(Token::Comma);
        }

        let span = self.next_until(end.clone(), span, |this| {
            res.push(f(this));

            if !this.matches(end.clone()) {
                this.expect(Token::Comma);
            }
        });

        Located::new(span, res)
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
        if !res.is_empty() && !self.matches_pred(|tk| matches!(tk, Token::RAngle | Token::Shr)) {
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
            if !self.matches_pred(|t| matches!(t, Token::RAngle | Token::Shr)) {
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
            self.diag.error(err);
            self.needs_sync = true;
        }
    }

    fn error_no_sync(&mut self, err: Error) {
        if !self.needs_sync {
            self.diag.error(err);
        }
    }

    //

    fn peek(&mut self) -> &Located<Token<'a>> {
        self.peek
            .get_or_insert_with(|| self.lexer.next_skip_comments(self.diag))
    }

    fn next(&mut self) -> Located<Token<'a>> {
        self.peek
            .take()
            .unwrap_or_else(|| self.lexer.next_skip_comments(self.diag))
    }

    fn next_if_l(&mut self, f: impl FnOnce(&Located<Token>) -> bool) -> Option<Located<Token<'a>>> {
        f(self.peek()).then(|| self.next())
    }

    fn next_if_pred(&mut self, pred: impl FnOnce(&Token) -> bool) -> Option<Located<Token<'a>>> {
        self.next_if_l(|tok| pred(&tok.data))
    }

    fn next_if(&mut self, kind: Token) -> Option<Located<Token<'a>>> {
        self.next_if_pred(|t| t == &kind)
    }

    fn next_if_map<T>(&mut self, f: impl FnOnce(&Located<Token>) -> Option<T>) -> Option<T> {
        let mut outer = None;
        self.next_if_l(|t| {
            outer = f(t);
            outer.is_some()
        });
        outer
    }

    fn next_until(&mut self, token: Token, mut span: Span, mut f: impl FnMut(&mut Self)) -> Span {
        while match self.next_if_pred(|t| t == &token || t == &Token::Eof) {
            Some(c) => {
                span.extend_to(c.span);
                false
            }
            None => true,
        } {
            f(self);
        }

        span
    }

    fn matches_pred(&mut self, pred: impl FnOnce(&Token) -> bool) -> bool {
        pred(&self.peek().data)
    }

    fn matches(&mut self, kind: Token) -> bool {
        self.matches_pred(|t| t == &kind)
    }

    fn expect_map<T>(
        &mut self,
        f: impl FnOnce(&Located<Token>) -> Option<T>,
        msg: &str,
    ) -> Option<T> {
        let token = self.peek();
        if let Some(res) = f(token) {
            self.next();
            Some(res)
        } else {
            let span = self.next().span;
            self.error(Error::new(msg, span));
            None
        }
    }

    fn expect(&mut self, kind: Token) -> Located<Token<'a>> {
        let token = self.peek();
        if token.data == kind {
            self.next()
        } else {
            let token = self.next();
            self.error(Error::new(format!("expected '{kind}'"), token.span));
            token
        }
    }

    fn expect_ident(&mut self, msg: &str) -> Located<String> {
        let token = self.peek();
        if let Token::Ident(ident) = token.data {
            let name = Located::new(token.span, ident.into());
            self.next();
            name
        } else {
            let span = self.next().span;
            self.error(Error::new(msg, span));
            Located::new(span, String::new())
        }
    }
}
