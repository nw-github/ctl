use std::{iter::Peekable, path::PathBuf};

use crate::{
    ast::{
        Attribute, Expr, ExprData, Fn, ImplBlock, MemVar, Param, ParsedUserType, Path, Pattern,
        Stmt, StmtData, Struct, TypeHint, UnaryOp,
    },
    lexer::{FileIndex, Lexer, Located, Span, Token},
    Error, THIS_PARAM, THIS_TYPE,
};

macro_rules! binary {
    ($name: ident, $patt: pat, $next: ident) => {
        fn $name(&mut self) -> Expr {
            let mut expr = self.$next();
            while let Some(op) = self.advance_if(|kind| matches!(kind, $patt)) {
                let right = self.$next();
                expr = Expr::new(
                    expr.span.extended_to(right.span),
                    ExprData::Binary {
                        op: op.data.try_into().unwrap(),
                        left: expr.into(),
                        right: right.into(),
                    },
                );
            }

            expr
        }
    };
}

#[derive(Clone, Copy)]
struct ProtoConfig {
    allow_method: bool,
    is_public: bool,
    is_async: bool,
    is_extern: bool,
    is_unsafe: bool,
}

pub struct ParsedFile {
    pub ast: Stmt,
    pub errors: Vec<Error>,
    pub path: PathBuf,
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    needs_sync: bool,
    errors: Vec<Error>,
    attrs: Vec<Attribute>,
    is_unsafe: Option<Located<Token<'a>>>,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str, file: FileIndex) -> Self {
        Self {
            lexer: Lexer::new(src, file).peekable(),
            needs_sync: false,
            errors: Vec::new(),
            attrs: Vec::new(),
            is_unsafe: None,
        }
    }

    pub fn parse(src: &'a str, file: FileIndex, path: PathBuf) -> std::io::Result<ParsedFile> {
        let mut this = Self::new(src, file);
        let mut stmts = Vec::new();
        while !this.matches_kind(Token::Eof) {
            stmts.push(this.item());
        }

        Ok(ParsedFile {
            ast: Stmt {
                data: StmtData::Module {
                    public: true,
                    name: crate::derive_module_name(&path),
                    body: stmts,
                },
                span: Span {
                    loc: Default::default(),
                    len: 0,
                    file,
                },
                attrs: Vec::new(),
            },
            errors: this.errors,
            path,
        })
    }

    //

    fn try_item(&mut self) -> Option<Stmt> {
        while self.advance_if_kind(Token::LBrace).is_some() {
            let attr = self.attribute();
            self.attrs.push(attr);
            self.expect_kind(Token::RBrace, "expected ']'");
        }

        let public = self.advance_if_kind(Token::Pub);
        let is_unsafe = self.advance_if_kind(Token::Unsafe);
        let is_extern = self.advance_if_kind(Token::Extern);
        if let Some((mut proto, span)) = self.try_prototype(ProtoConfig {
            allow_method: false,
            is_public: public.is_some(),
            is_async: false,
            is_extern: is_extern.is_some(),
            is_unsafe: is_unsafe.is_some(),
        }) {
            let attrs = std::mem::take(&mut self.attrs);
            if is_extern.is_some() {
                let semi = self.expect_kind(Token::Semicolon, "expected ';'");
                Some(Stmt {
                    data: StmtData::Fn(proto),
                    span: public.map_or(span, |p| p.span).extended_to(semi.span),
                    attrs,
                })
            } else {
                let (body, span) = self.parse_block();
                proto.body = Some(body);
                Some(Stmt {
                    data: StmtData::Fn(proto),
                    span: public.map_or(span, |p| p.span).extended_to(span),
                    attrs,
                })
            }
        } else if let Some(token) = self.advance_if_kind(Token::Struct) {
            if let Some(token) = is_unsafe {
                self.errors
                    .push(Error::new("unsafe is not valid here", token.span));
            }

            if let Some(token) = is_extern {
                self.errors
                    .push(Error::new("extern is not valid here", token.span));
            }

            Some(self.parse_struct(public.is_some(), token.span))
        } else if let Some(token) = self.advance_if_kind(Token::Union) {
            if let Some(token) = is_extern {
                self.errors
                    .push(Error::new("extern is not valid here", token.span));
            }

            Some(self.parse_union(public.is_some(), token.span, is_unsafe.is_some()))
        } else if let Some(token) = self.advance_if_kind(Token::Trait) {
            if let Some(token) = is_extern {
                self.errors
                    .push(Error::new("extern is not valid here", token.span));
            }

            Some(self.parse_trait(public.is_some(), token.span, is_unsafe.is_some()))
        } else if let Some(token) = self.advance_if_kind(Token::Enum) {
            if let Some(token) = is_unsafe {
                self.errors
                    .push(Error::new("unsafe is not valid here", token.span));
            }

            if let Some(token) = is_extern {
                self.errors
                    .push(Error::new("extern is not valid here", token.span));
            }

            Some(self.parse_enum(public.is_some(), token.span))
        } else if let Some(token) = self.advance_if_kind(Token::Mod) {
            let attrs = std::mem::take(&mut self.attrs);
            if let Some(token) = is_unsafe {
                self.errors
                    .push(Error::new("unsafe is not valid here", token.span));
            }

            if let Some(token) = is_extern {
                self.errors
                    .push(Error::new("extern is not valid here", token.span));
            }

            let name = self.expect_id("expected name");
            let mut body = Vec::new();
            self.expect_kind(Token::LCurly, "expected '{'");
            let span = self.advance_until(Token::RCurly, token.span, |this| body.push(this.item()));
            Some(Stmt {
                data: StmtData::Module {
                    public: public.is_some(),
                    name,
                    body,
                },
                span,
                attrs,
            })
        } else if let Some(token) = self.advance_if_kind(Token::Use) {
            if let Some(token) = is_unsafe {
                self.errors
                    .push(Error::new("unsafe is not valid here", token.span));
            }

            if let Some(token) = is_extern {
                self.errors
                    .push(Error::new("extern is not valid here", token.span));
            }

            let root = self.advance_if_kind(Token::ScopeRes);
            let sup = root
                .is_none()
                .then(|| self.advance_if_kind(Token::Super))
                .flatten()
                .is_some();
            if sup {
                self.expect_kind(Token::ScopeRes, "expected '::'");
            }

            let mut components = Vec::new();
            let mut all = false;
            loop {
                if (sup || !components.is_empty())
                    && self.advance_if_kind(Token::Asterisk).is_some()
                {
                    all = true;
                    break;
                }

                components.push((self.expect_id("expected path component"), Vec::new()));
                if self.advance_if_kind(Token::ScopeRes).is_none() {
                    break;
                }
            }

            let semi = self.expect_kind(Token::Semicolon, "expected ';'");
            Some(Stmt {
                data: StmtData::Use {
                    public: public.is_some(),
                    path: if root.is_some() {
                        Path::Root(components)
                    } else if sup {
                        Path::Super(components)
                    } else {
                        Path::Normal(components)
                    },
                    all,
                },
                span: token.span.extended_to(semi.span),
                attrs: std::mem::take(&mut self.attrs),
            })
        } else if let Some(token) = self.advance_if_kind(Token::Static) {
            if let Some(token) = is_unsafe {
                self.errors
                    .push(Error::new("unsafe is not valid here", token.span));
            }

            if let Some(token) = is_extern {
                self.errors
                    .push(Error::new("extern is not valid here", token.span));
            }

            let name = self.expect_id("expected name");
            self.expect_kind(Token::Colon, "expected ':'");
            let ty = self.parse_type();
            self.expect_kind(
                Token::Assign,
                "expected '=': static variables must be initialized",
            );
            let value = self.expression();
            let semi = self.expect_kind(Token::Semicolon, "expected ';'");

            Some(Stmt {
                data: StmtData::Static {
                    public: public.is_some(),
                    name,
                    ty,
                    value,
                },
                span: token.span.extended_to(semi.span),
                attrs: std::mem::take(&mut self.attrs),
            })
        } else {
            if let Some(public) = public.or(is_extern) {
                self.errors.push(Error::new("expected item", public.span));
            } else {
                self.is_unsafe = is_unsafe;
            }

            None
        }
    }

    fn item(&mut self) -> Stmt {
        self.try_item().unwrap_or_else(|| {
            let span = self.advance().span;
            self.error(Error::new("expected item", span));
            self.is_unsafe = None;
            Stmt {
                data: StmtData::Error,
                attrs: std::mem::take(&mut self.attrs),
                span,
            }
        })
    }

    fn statement(&mut self) -> Stmt {
        let stmt = if let Some(item) = self.try_item() {
            item
        } else if let Some(token) = self.advance_if(|t| matches!(t, Token::Let | Token::Mut)) {
            if let Some(is_unsafe) = self.is_unsafe.take() {
                self.errors
                    .push(Error::new("unsafe is not valid here", is_unsafe.span));
            }

            let name = self.expect_id("expected name");
            let ty = self
                .advance_if_kind(Token::Colon)
                .map(|_| self.parse_type());
            let value = if self.advance_if_kind(Token::Assign).is_some() {
                Some(self.expression())
            } else {
                None
            };
            let semi = self.expect_kind(Token::Semicolon, "expected ';'");
            Stmt {
                data: StmtData::Let {
                    name,
                    ty,
                    mutable: matches!(token.data, Token::Mut),
                    value,
                },
                span: token.span.extended_to(semi.span),
                attrs: std::mem::take(&mut self.attrs),
            }
        } else {
            let is_unsafe = self.is_unsafe.take();
            let mut expr = if let Some(token) = self.advance_if_kind(Token::If) {
                self.if_expr(token.span)
            } else if let Some(token) = self.advance_if_kind(Token::While) {
                self.while_expr(token.span)
            } else if let Some(token) = self.advance_if_kind(Token::Loop) {
                self.loop_expr(token.span)
            } else if let Some(token) = self.advance_if_kind(Token::For) {
                self.for_expr(token.span)
            } else if let Some(token) = self.advance_if_kind(Token::Match) {
                self.match_expr(token.span)
            } else if let Some(token) = self.advance_if_kind(Token::LCurly) {
                self.block_expr(token.span)
            } else {
                let expr = self.expression();
                self.expect_kind(Token::Semicolon, "expected ';'");
                expr
            };

            if let Some(is_unsafe) = is_unsafe {
                expr = Expr::new(
                    is_unsafe.span.extended_to(expr.span),
                    ExprData::Unsafe(expr.into()),
                );
            }

            Stmt {
                span: expr.span,
                data: StmtData::Expr(expr),
                attrs: std::mem::take(&mut self.attrs),
            }
        };

        if self.needs_sync {
            self.synchronize();
        }

        stmt
    }

    //

    fn expression(&mut self) -> Expr {
        self.assignment()
    }

    fn assignment(&mut self) -> Expr {
        let expr = self.range();
        if let Some(assign) = self.advance_if(|k| k.is_assignment()) {
            let value = self.expression();
            return Expr::new(
                expr.span.extended_to(value.span),
                ExprData::Assign {
                    target: expr.into(),
                    binary: assign.data.try_into().ok(),
                    value: value.into(),
                },
            );
        }

        expr
    }

    fn range(&mut self) -> Expr {
        let mut expr = self.logical_or();
        while let Some(op) = self.advance_if(|k| matches!(k, Token::Range | Token::RangeInclusive))
        {
            let inclusive = op.data == Token::RangeInclusive;
            if self.is_range_end() {
                expr = Expr::new(
                    expr.span.extended_to(op.span),
                    ExprData::Range {
                        start: Some(expr.into()),
                        end: None,
                        inclusive,
                    },
                );
            } else {
                let right = self.logical_or();
                expr = Expr::new(
                    expr.span.extended_to(right.span),
                    ExprData::Range {
                        start: Some(expr.into()),
                        end: Some(right.into()),
                        inclusive,
                    },
                );
            }
        }

        expr
    }

    binary!(logical_or, Token::LogicalOr, logical_and);
    binary!(logical_and, Token::LogicalAnd, comparison);

    fn comparison(&mut self) -> Expr {
        let mut expr = self.coalesce();
        while let Some(op) = self.advance_if(|kind| {
            matches!(
                kind,
                Token::RAngle
                    | Token::GtEqual
                    | Token::LAngle
                    | Token::LtEqual
                    | Token::Equal
                    | Token::NotEqual
                    | Token::Is
            )
        }) {
            if op.data == Token::Is {
                let pattern = self.pattern();
                //let span = expr.span.extended_to(pattern.span);
                expr = Expr::new(
                    expr.span,
                    ExprData::Is {
                        expr: expr.into(),
                        pattern,
                    },
                );
            } else {
                let right = self.coalesce();
                expr = Expr::new(
                    expr.span.extended_to(right.span),
                    ExprData::Binary {
                        op: op.data.try_into().unwrap(),
                        left: expr.into(),
                        right: right.into(),
                    },
                );
            }
        }
        expr
    }

    binary!(coalesce, Token::NoneCoalesce, or);
    binary!(or, Token::Or, xor);
    binary!(xor, Token::Caret, and);
    binary!(and, Token::Ampersand, shift);
    binary!(shift, Token::Shl | Token::Shr, term);
    binary!(term, Token::Plus | Token::Minus, factor);
    binary!(factor, Token::Asterisk | Token::Div | Token::Rem, cast);

    fn cast(&mut self) -> Expr {
        let mut expr = self.unary();
        while self.advance_if_kind(Token::As).is_some() {
            let bang = self.advance_if_kind(Token::Exclamation);
            let ty = self.parse_type();
            expr = Expr::new(
                expr.span,
                ExprData::As {
                    expr: expr.into(),
                    ty,
                    throwing: bang.is_some(),
                },
            );
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if let Some(t) = self.advance_if(|k| {
            matches!(
                k,
                Token::Plus
                    | Token::Minus
                    | Token::Asterisk
                    | Token::Ampersand
                    | Token::Increment
                    | Token::Decrement
                    | Token::Exclamation
            )
        }) {
            let op = if t.data == Token::Ampersand && self.advance_if_kind(Token::Mut).is_some() {
                UnaryOp::AddrMut
            } else {
                t.data.try_into().unwrap()
            };

            let expr = self.unary();
            return Expr::new(
                t.span.extended_to(expr.span),
                ExprData::Unary {
                    op,
                    expr: expr.into(),
                },
            );
        }

        self.call()
    }

    fn call(&mut self) -> Expr {
        let mut expr = self.primary();
        loop {
            if self.advance_if_kind(Token::LParen).is_some() {
                let (args, span) = self.csv(Vec::new(), Token::RParen, expr.span, |this| {
                    let mut expr = this.expression();
                    let mut name = None;
                    if let ExprData::Path(path) = &expr.data {
                        if let Some(ident) = path
                            .as_identifier()
                            .filter(|_| this.advance_if_kind(Token::Colon).is_some())
                        {
                            name = Some(ident.to_string());
                            if !this.matches(|t| matches!(t, Token::Comma | Token::RParen)) {
                                expr = this.expression();
                            }
                        }
                    }
                    (name, expr)
                });

                expr = Expr::new(
                    span,
                    ExprData::Call {
                        callee: expr.into(),
                        args,
                    },
                );
            } else if self.advance_if_kind(Token::Dot).is_some() {
                let member = self.expect_located_id("expected member name");
                let (generics, span) = if self.advance_if_kind(Token::ScopeRes).is_some() {
                    self.expect_kind(Token::LAngle, "expected '<'");
                    self.csv_one(Token::RAngle, expr.span, Self::parse_type)
                } else {
                    (Vec::new(), expr.span.extended_to(member.span))
                };

                expr = Expr::new(
                    span,
                    ExprData::Member {
                        source: expr.into(),
                        member: member.data,
                        generics,
                    },
                );
            } else if self.advance_if_kind(Token::LBrace).is_some() {
                let (args, span) = self.csv(Vec::new(), Token::RBrace, expr.span, Self::expression);
                expr = Expr::new(
                    span,
                    ExprData::Subscript {
                        callee: expr.into(),
                        args,
                    },
                );
            } else if let Some(inc) = self.advance_if_kind(Token::Increment) {
                expr = Expr::new(
                    expr.span.extended_to(inc.span),
                    ExprData::Unary {
                        op: UnaryOp::PostIncrement,
                        expr: expr.into(),
                    },
                );
            } else if let Some(dec) = self.advance_if_kind(Token::Decrement) {
                expr = Expr::new(
                    expr.span.extended_to(dec.span),
                    ExprData::Unary {
                        op: UnaryOp::PostDecrement,
                        expr: expr.into(),
                    },
                );
            } else if let Some(dec) = self.advance_if_kind(Token::Exclamation) {
                expr = Expr::new(
                    expr.span.extended_to(dec.span),
                    ExprData::Unary {
                        op: UnaryOp::Unwrap,
                        expr: expr.into(),
                    },
                );
            } else if let Some(dec) = self.advance_if_kind(Token::Question) {
                expr = Expr::new(
                    expr.span.extended_to(dec.span),
                    ExprData::Unary {
                        op: UnaryOp::Try,
                        expr: expr.into(),
                    },
                );
            } else {
                break expr;
            }
        }
    }

    fn primary(&mut self) -> Expr {
        let mut token = self.advance();
        match token.data {
            Token::Void => Expr::new(token.span, ExprData::Void),
            Token::False => Expr::new(token.span, ExprData::Bool(false)),
            Token::True => Expr::new(token.span, ExprData::Bool(true)),
            Token::None => Expr::new(token.span, ExprData::None),
            Token::Int { base, value, width } => Expr::new(
                token.span,
                ExprData::Integer {
                    base,
                    value: value.into(),
                    width: width.map(|w| w.into()),
                },
            ),
            Token::Float(value) => Expr::new(token.span, ExprData::Float(value.into())),
            Token::String(value) => Expr::new(token.span, ExprData::String(value.into())),
            Token::Char(value) => Expr::new(token.span, ExprData::Char(value)),
            Token::ByteString(value) => Expr::new(token.span, ExprData::ByteString(value.into())),
            Token::ByteChar(value) => Expr::new(token.span, ExprData::ByteChar(value)),
            Token::LParen => {
                let expr = self.expression();
                if self.matches_kind(Token::Comma) {
                    let (exprs, span) =
                        self.csv(vec![expr], Token::RParen, token.span, Self::expression);
                    Expr::new(span, ExprData::Tuple(exprs))
                } else {
                    let end = self.expect_kind(Token::RParen, "expected ')'");
                    Expr::new(token.span.extended_to(end.span), expr.data)
                }
            }
            Token::Ident(ident) => {
                let data = self.path_components(Some(ident), &mut token.span);
                Expr::new(token.span, ExprData::Path(Path::Normal(data)))
            }
            Token::ScopeRes => {
                let ident = self.expect_id("expected name");
                let data = self.path_components(Some(&ident), &mut token.span);
                Expr::new(token.span, ExprData::Path(Path::Root(data)))
            }
            Token::Super => {
                let data = self.path_components(None, &mut token.span);
                Expr::new(token.span, ExprData::Path(Path::Super(data)))
            }
            Token::This => Expr::new(token.span, ExprData::Path(THIS_PARAM.to_owned().into())),
            Token::ThisType => Expr::new(token.span, ExprData::Path(THIS_TYPE.to_owned().into())),
            Token::Range => {
                if self.is_range_end() {
                    Expr::new(
                        token.span,
                        ExprData::Range {
                            start: None,
                            end: None,
                            inclusive: false,
                        },
                    )
                } else {
                    let end = self.expression();
                    Expr::new(
                        token.span.extended_to(end.span),
                        ExprData::Range {
                            start: None,
                            end: Some(end.into()),
                            inclusive: false,
                        },
                    )
                }
            }
            Token::RangeInclusive => {
                let end = self.expression();
                Expr::new(
                    token.span.extended_to(end.span),
                    ExprData::Range {
                        start: None,
                        end: Some(end.into()),
                        inclusive: true,
                    },
                )
            }
            Token::LCurly => self.block_expr(token.span),
            Token::If => self.if_expr(token.span),
            Token::While => self.while_expr(token.span),
            Token::Loop => self.loop_expr(token.span),
            Token::For => self.for_expr(token.span),
            Token::Match => self.match_expr(token.span),
            Token::LBrace => {
                if let Some(rbrace) = self.advance_if_kind(Token::RBrace) {
                    Expr::new(
                        token.span.extended_to(rbrace.span),
                        ExprData::Array(Vec::new()),
                    )
                } else if self.advance_if_kind(Token::Colon).is_some() {
                    Expr::new(
                        token
                            .span
                            .extended_to(self.expect_kind(Token::RBrace, "expected ']'").span),
                        ExprData::Map(Vec::new()),
                    )
                } else {
                    let expr = self.expression();
                    if self.advance_if_kind(Token::Colon).is_some() {
                        let value = self.expression();
                        let (exprs, span) =
                            self.csv(vec![(expr, value)], Token::RBrace, token.span, |this| {
                                let key = this.expression();
                                this.expect_kind(Token::Colon, "expected ':'");
                                let value = this.expression();
                                (key, value)
                            });

                        Expr::new(span, ExprData::Map(exprs))
                    } else if self.advance_if_kind(Token::Semicolon).is_some() {
                        let count = self.expression();
                        let rbrace = self.expect_kind(Token::RBrace, "expected ']'");
                        Expr::new(
                            token.span.extended_to(rbrace.span),
                            ExprData::ArrayWithInit {
                                init: expr.into(),
                                count: count.into(),
                            },
                        )
                    } else if let Some(rbrace) = self.advance_if_kind(Token::RBrace) {
                        Expr::new(
                            token.span.extended_to(rbrace.span),
                            ExprData::Array(vec![expr]),
                        )
                    } else {
                        let (exprs, span) =
                            self.csv(vec![expr], Token::RBrace, token.span, Self::expression);
                        Expr::new(span, ExprData::Array(exprs))
                    }
                }
            }
            Token::Fn => {
                self.expect_kind(Token::LParen, "expected '('");
                let (params, _) = self.csv(Vec::new(), Token::RParen, token.span, |this| {
                    (
                        this.expect_located_id("expected parameter name"),
                        this.advance_if_kind(Token::Colon)
                            .map(|_| this.parse_type()),
                    )
                });
                let ret = (!self.matches_kind(Token::Arrow)).then(|| self.parse_type());
                self.expect_kind(Token::Arrow, "expected '->'");

                let body = self.expression();
                Expr::new(
                    token.span.extended_to(body.span),
                    ExprData::Lambda {
                        params,
                        ret,
                        body: body.into(),
                    },
                )
            }
            Token::Return | Token::Break | Token::Yield => {
                let (span, expr) = if !self.matches(|tk| {
                    matches!(
                        tk,
                        Token::Semicolon | Token::Comma | Token::RBrace | Token::RParen
                    )
                }) {
                    let expr = self.expression();
                    (token.span.extended_to(expr.span), expr.into())
                } else {
                    (token.span, Expr::new(token.span, ExprData::Void).into())
                };

                Expr::new(
                    span,
                    match token.data {
                        Token::Return => ExprData::Return(expr),
                        Token::Break => ExprData::Break(expr),
                        Token::Yield => ExprData::Yield(expr),
                        _ => unreachable!(),
                    },
                )
            }
            Token::Unsafe => {
                let expr = self.expression();
                Expr::new(
                    token.span.extended_to(expr.span),
                    ExprData::Unsafe(expr.into()),
                )
            }
            Token::Continue => Expr::new(token.span, ExprData::Continue),
            _ => {
                self.error(Error::new("unexpected token", token.span));
                Expr::new(token.span, ExprData::Error)
            }
        }
    }

    //

    fn if_expr(&mut self, token: Span) -> Expr {
        let cond = self.expression();
        let lcurly = self.expect_kind(Token::LCurly, "expected '{'");
        let if_branch = self.block_expr(lcurly.span);
        let else_branch = self.advance_if_kind(Token::Else).map(|_| {
            if !self.matches_kind(Token::If) {
                let lcurly = self.expect_kind(Token::LCurly, "expected '{'");
                self.block_expr(lcurly.span)
            } else {
                self.expression()
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

    fn while_expr(&mut self, token: Span) -> Expr {
        let cond = self.expression();
        let (body, span) = self.parse_block();
        Expr::new(
            token.extended_to(span),
            ExprData::Loop {
                cond: Some(cond.into()),
                body,
                do_while: false,
            },
        )
    }

    fn loop_expr(&mut self, mut token: Span) -> Expr {
        let body = self.parse_block().0;
        let (cond, do_while) = self
            .advance_if_kind(Token::While)
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
            },
        )
    }

    fn for_expr(&mut self, token: Span) -> Expr {
        let mutable = self.advance_if_kind(Token::Mut);
        let var = self.expect_id("expected variable name");
        self.expect_kind(Token::In, "expected 'in'");
        // TODO: parse for foo in 0.. {} as |0..| |{}| instead of |0..{}|
        let iter = self.expression();
        let (body, span) = self.parse_block();
        Expr::new(
            token.extended_to(span),
            ExprData::For {
                var,
                mutable: mutable.is_some(),
                iter: iter.into(),
                body,
            },
        )
    }

    fn match_expr(&mut self, token: Span) -> Expr {
        let expr = self.expression();
        self.expect_kind(Token::LCurly, "expected block");
        let mut body = Vec::new();
        let span = self.advance_until(Token::RCurly, token, |this| {
            let pattern = this.pattern();
            this.expect_kind(Token::FatArrow, "expected '=>'");
            let expr = this.expression();
            if !expr.data.is_block_expr() {
                this.expect_kind(Token::Comma, "expected ','");
            } else {
                this.advance_if_kind(Token::Comma);
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

    fn block_expr(&mut self, token: Span) -> Expr {
        let mut stmts = Vec::new();
        let span = self.advance_until(Token::RCurly, token, |this| {
            stmts.push(this.statement());
        });
        Expr::new(span, ExprData::Block(stmts))
    }

    //

    fn path_components(
        &mut self,
        first: Option<&str>,
        outspan: &mut Span,
    ) -> Vec<(String, Vec<TypeHint>)> {
        let mut data = first
            .map(|s| vec![(s.into(), Vec::new())])
            .unwrap_or_default();
        while self.advance_if_kind(Token::ScopeRes).is_some() {
            if self.advance_if_kind(Token::LAngle).is_some() {
                let (params, span) = self.csv_one(Token::RAngle, *outspan, Self::parse_type);
                data.last_mut().unwrap().1 = params;
                *outspan = span;
            } else {
                let name = self.expect_located_id("expected name");
                data.push((name.data.to_owned(), Vec::new()));
                outspan.extend_to(name.span);
            }
        }

        data
    }

    fn type_path(&mut self) -> Located<Path> {
        let root = self.advance_if_kind(Token::ScopeRes);
        let sup = root
            .is_none()
            .then(|| self.advance_if_kind(Token::Super))
            .flatten();
        if sup.is_some() {
            self.expect_kind(Token::ScopeRes, "expected '::'");
        }

        let mut data = Vec::new();
        let mut span = root.as_ref().or(sup.as_ref()).map(|t| t.span);
        loop {
            let ident = self.expect_located_id("expected type name");
            if let Some(span) = &mut span {
                span.extend_to(ident.span);
            } else {
                span = Some(ident.span);
            }
            if self.advance_if_kind(Token::LAngle).is_some() {
                let (params, pspan) = self.csv_one(Token::RAngle, span.unwrap(), Self::parse_type);
                data.push((ident.data, params));
                span = Some(pspan);
            } else {
                data.push((ident.data, Vec::new()));
            }

            if self.advance_if_kind(Token::ScopeRes).is_none() {
                break;
            }
        }

        Located::new(
            span.unwrap(),
            if root.is_some() {
                Path::Root(data)
            } else if sup.is_some() {
                Path::Super(data)
            } else {
                Path::Normal(data)
            },
        )
    }

    fn pattern(&mut self) -> Pattern {
        if self.advance_if_kind(Token::Question).is_some() {
            let mutable = self.advance_if_kind(Token::Mut);
            return Pattern::Option(mutable.is_some(), self.expect_located_id("expected name"));
        }

        if let Some(token) = self.advance_if_kind(Token::None) {
            return Pattern::Null(token.span);
        }

        if self.advance_if_kind(Token::Mut).is_some() {
            return Pattern::MutCatchAll(self.expect_located_id("expected name"));
        }

        let path = self.type_path();
        if self.advance_if_kind(Token::LParen).is_some() {
            // TODO: make this a pattern
            let mutable = self.advance_if_kind(Token::Mut);
            let id = self.expect_id("expected identifier");
            self.expect_kind(Token::RParen, "expected ')'");

            Pattern::PathWithBindings {
                path,
                binding: (mutable.is_some(), id),
            }
        } else {
            Pattern::Path(path)
        }
    }

    fn csv<T>(
        &mut self,
        mut res: Vec<T>,
        end: Token,
        span: Span,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> (Vec<T>, Span) {
        if !res.is_empty() && !self.matches_kind(end.clone()) {
            self.expect_kind(Token::Comma, "expected ','");
        }

        let span = self.advance_until(end.clone(), span, |this| {
            res.push(f(this));

            if !this.matches_kind(end.clone()) {
                this.expect_kind(Token::Comma, "expected ','");
            }
        });

        (res, span)
    }

    fn csv_one<T>(
        &mut self,
        end: Token,
        span: Span,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> (Vec<T>, Span) {
        let first = f(self);
        self.csv(vec![first], end, span, f)
    }

    fn parse_generic_params(&mut self) -> Vec<(String, Vec<Located<Path>>)> {
        self.advance_if_kind(Token::LAngle)
            .map(|_| {
                self.csv_one(Token::RAngle, Span::default(), |this| {
                    (
                        this.expect_id("expected type name"),
                        this.parse_trait_impl(),
                    )
                })
                .0
            })
            .unwrap_or_default()
    }

    fn parse_trait_impl(&mut self) -> Vec<Located<Path>> {
        let mut impls = Vec::new();
        if self.advance_if_kind(Token::Colon).is_some() {
            loop {
                impls.push(self.type_path());
                if self.advance_if_kind(Token::Plus).is_none() {
                    break;
                }
            }
        }
        impls
    }

    fn parse_type(&mut self) -> TypeHint {
        if self.advance_if_kind(Token::Asterisk).is_some() {
            if self.advance_if_kind(Token::Mut).is_some() {
                return TypeHint::MutPtr(self.parse_type().into());
            } else {
                return TypeHint::Ptr(self.parse_type().into());
            }
        } else if self.advance_if_kind(Token::Question).is_some() {
            return TypeHint::Option(self.parse_type().into());
        } else if self.advance_if_kind(Token::NoneCoalesce).is_some() {
            // special case for ??
            return TypeHint::Option(TypeHint::Option(self.parse_type().into()).into());
        }

        if self.advance_if_kind(Token::LBrace).is_some() {
            if self.advance_if_kind(Token::Mut).is_some() {
                let inner = self.parse_type();
                self.expect_kind(Token::Range, "expected '..'");
                self.expect_kind(Token::RBrace, "expected ']'");
                TypeHint::SliceMut(inner.into())
            } else {
                let inner = self.parse_type();
                if self.advance_if_kind(Token::RBrace).is_some() {
                    TypeHint::Vec(inner.into())
                } else if self.advance_if_kind(Token::Range).is_some() {
                    self.expect_kind(Token::RBrace, "expected ']'");
                    TypeHint::Slice(inner.into())
                } else if self.advance_if_kind(Token::Semicolon).is_some() {
                    let count = self.expression();
                    self.expect_kind(Token::RBrace, "expected ']'");
                    TypeHint::Array(inner.into(), count.into())
                } else if self.advance_if_kind(Token::Colon).is_some() {
                    let value = self.parse_type();
                    self.expect_kind(Token::RBrace, "expected ']'");
                    TypeHint::Map(inner.into(), value.into())
                } else {
                    let span = self.advance().span;
                    self.error(Error::new("expected ']', ';', or ':'", span));
                    return TypeHint::Error;
                }
            }
        } else if self.advance_if_kind(Token::LCurly).is_some() {
            let inner = self.parse_type().into();
            self.expect_kind(Token::RCurly, "expected '}'");
            TypeHint::Set(inner)
        } else if self.advance_if_kind(Token::LParen).is_some() {
            TypeHint::Tuple(
                self.csv_one(Token::RParen, Span::default(), Self::parse_type)
                    .0,
            )
        } else if self.advance_if_kind(Token::Void).is_some() {
            TypeHint::Void
        } else if let Some(this) = self.advance_if_kind(Token::ThisType) {
            TypeHint::Regular(Located::new(this.span, Path::from(THIS_TYPE.to_owned())))
        } else if self.advance_if_kind(Token::Fn).is_some() {
            self.expect_kind(Token::LParen, "expected '('");
            let (params, _) =
                self.csv(Vec::new(), Token::RParen, Span::default(), Self::parse_type);
            if self.advance_if_kind(Token::Arrow).is_some() {
                let ret = self.parse_type();
                TypeHint::Fn {
                    is_extern: false,
                    params,
                    ret: ret.into(),
                }
            } else {
                TypeHint::Fn {
                    is_extern: false,
                    params,
                    ret: TypeHint::Void.into(),
                }
            }
        } else {
            TypeHint::Regular(self.type_path())
        }
    }

    fn parse_block(&mut self) -> (Vec<Stmt>, Span) {
        let mut stmts = Vec::new();
        let lcurly = self.expect_kind(Token::LCurly, "expected '{'");
        let span = self.advance_until(Token::RCurly, lcurly.span, |this| {
            stmts.push(this.statement());
        });
        (stmts, span)
    }

    fn parse_struct(&mut self, public: bool, span: Span) -> Stmt {
        let attrs = std::mem::take(&mut self.attrs);
        let name = self.expect_located_id("expected name");
        let type_params = self.parse_generic_params();
        let impls = self.parse_trait_impl();

        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        let mut members = Vec::new();
        let mut new_impls = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            let config = ProtoConfig {
                allow_method: true,
                is_public: this.advance_if_kind(Token::Pub).is_some(),
                is_async: false,
                is_extern: false,
                is_unsafe: this.advance_if_kind(Token::Unsafe).is_some(),
            };
            if config.is_unsafe {
                if let Ok((mut func, _)) = this.expect_prototype(config) {
                    func.body = Some(this.parse_block().0);
                    functions.push(func);
                }
            } else if let Some((mut func, _)) = this.try_prototype(config) {
                func.body = Some(this.parse_block().0);
                functions.push(func);
            } else if let Some(token) = this.advance_if_kind(Token::Impl) {
                new_impls.push(this.parse_impl_block(token.span));
            } else {
                let name = this.expect_id("expected name");
                this.expect_kind(Token::Colon, "expected type");
                let ty = this.parse_type();
                let value = this
                    .advance_if_kind(Token::Assign)
                    .map(|_| this.expression());
                this.expect_kind(Token::Comma, "expected ','");
                members.push(MemVar {
                    public: config.is_public,
                    ty,
                    name,
                    default: value,
                    shared: false,
                });
            }
        });

        Stmt {
            span,
            data: StmtData::UserType(ParsedUserType::Struct(Struct {
                public,
                name,
                type_params,
                members,
                impls,
                functions,
                new_impls,
            })),
            attrs,
        }
    }

    fn parse_union(&mut self, public: bool, span: Span, is_unsafe: bool) -> Stmt {
        let attrs = std::mem::take(&mut self.attrs);
        let tag = self.advance_if_kind(Token::LParen).map(|_| {
            let tag = self.type_path();
            self.expect_kind(Token::RParen, "expected ')'");
            tag
        });
        let name = self.expect_located_id("expected name");
        let type_params = self.parse_generic_params();
        let impls = self.parse_trait_impl();
        let mut functions = Vec::new();
        let mut members = Vec::new();
        let mut new_impls = Vec::new();

        self.expect_kind(Token::LCurly, "expected '{'");
        let span = self.advance_until(Token::RCurly, span, |this| {
            let config = ProtoConfig {
                allow_method: true,
                is_public: this.advance_if_kind(Token::Pub).is_some(),
                is_async: false,
                is_extern: false,
                is_unsafe: this.advance_if_kind(Token::Unsafe).is_some(),
            };
            if config.is_public || config.is_unsafe {
                if let Ok((mut func, _)) = this.expect_prototype(config) {
                    func.body = Some(this.parse_block().0);
                    functions.push(func);
                }
            } else if let Some((mut func, _)) = this.try_prototype(config) {
                func.body = Some(this.parse_block().0);
                functions.push(func);
            } else if this.advance_if_kind(Token::Shared).is_some() {
                // warn if pub was specified that it is useless
                let name = this.expect_id("expected name");
                this.expect_kind(Token::Colon, "expected type");
                let ty = this.parse_type();
                let value = this
                    .advance_if_kind(Token::Assign)
                    .map(|_| this.expression());
                this.expect_kind(Token::Comma, "expected ','");

                members.push(MemVar {
                    public: true,
                    name,
                    shared: true,
                    ty,
                    default: value,
                });
            } else if let Some(token) = this.advance_if_kind(Token::Impl) {
                new_impls.push(this.parse_impl_block(token.span));
            } else {
                let name = this.expect_id("expected variant name");
                let (ty, value) = if this.advance_if_kind(Token::LParen).is_some() {
                    let ty = this.parse_type();
                    this.expect_kind(Token::RParen, "expected ')'");
                    (
                        ty,
                        this.advance_if_kind(Token::Assign)
                            .map(|_| this.expression()),
                    )
                } else {
                    (TypeHint::Void, None)
                };

                this.expect_kind(Token::Comma, "expected ','");
                members.push(MemVar {
                    public: config.is_public,
                    name,
                    shared: false,
                    ty,
                    default: value,
                });
            }
        });

        Stmt {
            span,
            data: StmtData::UserType(ParsedUserType::Union {
                tag,
                base: Struct {
                    public,
                    name,
                    type_params,
                    members,
                    impls,
                    functions,
                    new_impls,
                },
                is_unsafe,
            }),
            attrs,
        }
    }

    fn parse_trait(&mut self, public: bool, span: Span, is_unsafe: bool) -> Stmt {
        let attrs = std::mem::take(&mut self.attrs);
        let name = self.expect_id("expected name");
        let type_params = self.parse_generic_params();
        let impls = self.parse_trait_impl();
        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            let is_unsafe = this.advance_if_kind(Token::Unsafe).is_some();
            if let Ok((proto, _)) = this.expect_prototype(ProtoConfig {
                allow_method: true,
                is_public: true,
                is_async: false,
                is_extern: false,
                is_unsafe,
            }) {
                this.expect_kind(Token::Semicolon, "expected ';'");
                functions.push(proto);
            }
        });

        Stmt {
            span,
            data: StmtData::UserType(ParsedUserType::Trait {
                public,
                is_unsafe,
                name,
                type_params,
                impls,
                functions,
            }),
            attrs,
        }
    }

    fn parse_enum(&mut self, public: bool, span: Span) -> Stmt {
        let attrs = std::mem::take(&mut self.attrs);
        let name = self.expect_located_id("expected name");
        let impls = self.parse_trait_impl();
        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        let mut variants = Vec::new();
        let mut new_impls = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            let config = ProtoConfig {
                allow_method: true,
                is_public: this.advance_if_kind(Token::Pub).is_some(),
                is_async: false,
                is_extern: false,
                is_unsafe: this.advance_if_kind(Token::Unsafe).is_some(),
            };
            if config.is_unsafe {
                if let Ok((mut func, _)) = this.expect_prototype(config) {
                    func.body = Some(this.parse_block().0);
                    functions.push(func);
                }
            } else if let Some((mut func, _)) = this.try_prototype(config) {
                func.body = Some(this.parse_block().0);
                functions.push(func);
            } else if let Some(token) = this.advance_if_kind(Token::Impl) {
                new_impls.push(this.parse_impl_block(token.span));
            } else {
                variants.push((
                    this.expect_id("expected variant name"),
                    if this.advance_if_kind(Token::Assign).is_some() {
                        Some(this.expression())
                    } else {
                        None
                    },
                ));

                this.expect_kind(Token::Comma, "expected ','");
            }
        });

        Stmt {
            span,
            data: StmtData::UserType(ParsedUserType::Enum {
                public,
                name,
                impls,
                variants,
                functions,
                new_impls,
            }),
            attrs,
        }
    }

    fn parse_impl_block(&mut self, span: Span) -> ImplBlock {
        let type_params = self.parse_generic_params();
        let tr = self.type_path();
        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        self.advance_until(Token::RCurly, span, |this| {
            let is_unsafe = this.advance_if_kind(Token::Unsafe).is_some();
            if let Ok((mut func, _)) = this.expect_prototype(ProtoConfig {
                allow_method: true,
                is_public: true,
                is_async: false,
                is_extern: false,
                is_unsafe,
            }) {
                func.body = Some(this.parse_block().0);
                functions.push(func);
            }
        });

        ImplBlock {
            type_params,
            tr,
            functions,
        }
    }

    fn prototype(
        &mut self,
        ProtoConfig {
            allow_method,
            is_public,
            is_async,
            is_extern,
            is_unsafe,
        }: ProtoConfig,
    ) -> Fn {
        let name = self.expect_located_id("expected name");
        let mut variadic = false;
        let type_params = self.parse_generic_params();
        let mut params = Vec::new();
        let mut count = 0;
        let mut has_default = false;
        self.expect_kind(Token::LParen, "expected parameter list");
        while self
            .advance_if(|t| matches!(t, Token::RParen | Token::Eof))
            .is_none()
        {
            if self.advance_if_kind(Token::Ellipses).is_some() {
                variadic = true;
                self.expect_kind(Token::RParen, "expected ')'");
                break;
            }

            let keyword = self.advance_if_kind(Token::Keyword).is_some();
            let mutable = self.advance_if_kind(Token::Mut).is_some();
            if allow_method && count == 0 && self.advance_if_kind(Token::This).is_some() {
                params.push(Param {
                    mutable: false,
                    keyword,
                    name: THIS_PARAM.into(),
                    ty: if mutable {
                        TypeHint::MutThis
                    } else {
                        TypeHint::This
                    },
                    default: None,
                })
            } else {
                let name = self.expect_located_id("expected name");
                self.expect_kind(Token::Colon, "expected type");
                let ty = self.parse_type();
                let default = if self.advance_if_kind(Token::Assign).is_some() {
                    has_default = true;
                    Some(self.expression())
                } else {
                    if !keyword && has_default {
                        self.errors.push(Error::new(
                            "positional parameters must not follow a default parameter",
                            name.span,
                        ));
                    }

                    None
                };

                params.push(Param {
                    mutable,
                    keyword,
                    name: name.data,
                    ty,
                    default,
                })
            }

            if !self.matches_kind(Token::RParen) {
                self.expect_kind(Token::Comma, "expected ','");
            }

            count += 1;
        }

        Fn {
            name,
            public: is_public,
            is_async,
            is_extern,
            is_unsafe,
            variadic,
            type_params,
            params,
            ret: if !self.matches(|kind| matches!(kind, Token::Semicolon | Token::LCurly)) {
                self.parse_type()
            } else {
                TypeHint::Void
            },
            body: None,
        }
    }

    fn try_prototype(&mut self, params: ProtoConfig) -> Option<(Fn, Span)> {
        if let Some(token) = self.advance_if_kind(Token::Fn) {
            Some((
                self.prototype(ProtoConfig {
                    is_async: false,
                    ..params
                }),
                token.span,
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Async) {
            self.expect_kind(Token::Fn, "expected 'fn'");
            Some((
                self.prototype(ProtoConfig {
                    is_async: true,
                    ..params
                }),
                token.span,
            ))
        } else {
            None
        }
    }

    fn expect_prototype(&mut self, params: ProtoConfig) -> Result<(Fn, Span), ()> {
        loop {
            if let Some(proto) = self.try_prototype(params) {
                return Ok(proto);
            }

            loop {
                let token = self.advance();
                match token.data {
                    Token::Extern | Token::Fn | Token::Async => break,
                    Token::Eof => {
                        self.error(Error::new("expected function", token.span));
                        return Err(());
                    }
                    _ => {}
                }
            }
        }
    }

    fn is_range_end(&mut self) -> bool {
        self.matches(|k| {
            matches!(
                k,
                Token::Semicolon | Token::Comma | Token::RBrace | Token::RParen
            )
        })
    }

    fn attribute(&mut self) -> Attribute {
        Attribute {
            name: self.expect_located_id("expected ';'"),
            props: self
                .advance_if_kind(Token::LParen)
                .map(|_| {
                    self.csv_one(Token::RParen, Span::default(), Self::attribute)
                        .0
                })
                .unwrap_or_default(),
        }
    }

    //

    fn synchronize(&mut self) {
        use Token::*;

        loop {
            match self.lexer.peek() {
                Some(Ok(token)) if token.data == Semicolon => {
                    self.advance();
                    break;
                }
                Some(Ok(token))
                    if matches!(
                        token.data,
                        Pub | Struct
                            | Enum
                            | Union
                            | Trait
                            | Fn
                            | Let
                            | Static
                            | Loop
                            | If
                            | Match
                            | While
                            | Return
                            | Yield
                            | Break
                            | Eof
                    ) =>
                {
                    break
                }
                Option::None => break,
                _ => {
                    self.advance();
                }
            }
        }
        self.needs_sync = false;
    }

    fn error(&mut self, err: Error) {
        if !self.needs_sync {
            self.errors.push(err);
            self.needs_sync = true;
        }
    }

    fn advance(&mut self) -> Located<Token<'a>> {
        loop {
            match self.lexer.next().unwrap() {
                Ok(tok) => break tok,
                Err(err) => self.error(Error::new(err.data.tell(), err.span)),
            }
        }
    }

    fn advance_if(&mut self, pred: impl FnOnce(&Token) -> bool) -> Option<Located<Token<'a>>> {
        self.lexer
            .next_if(|tok| matches!(tok, Ok(tok) if pred(&tok.data)))
            .map(|token| token.unwrap())
    }

    fn advance_if_kind(&mut self, kind: Token) -> Option<Located<Token<'a>>> {
        self.advance_if(|t| t == &kind)
    }

    fn advance_until(
        &mut self,
        token: Token,
        mut span: Span,
        mut f: impl FnMut(&mut Self),
    ) -> Span {
        while match self.advance_if(|t| t == &token || t == &Token::Eof) {
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

    fn expect<T>(
        &mut self,
        pred: impl FnOnce(Located<Token<'a>>) -> Result<T, Located<Token<'a>>>,
        msg: &str,
    ) -> Result<T, Located<Token<'a>>> {
        match pred(self.advance()) {
            Ok(t) => Ok(t),
            Err(e) => {
                self.error(Error::new(msg, e.span));
                Err(e)
            }
        }
    }

    fn expect_kind(&mut self, kind: Token, msg: &str) -> Located<Token<'a>> {
        let (Ok(t) | Err(t)) = self.expect(
            |t| {
                if t.data == kind {
                    Ok(t)
                } else {
                    Err(t)
                }
            },
            msg,
        );
        t
    }

    fn expect_id(&mut self, msg: &str) -> String {
        self.expect(
            |t| {
                let Token::Ident(ident) = t.data else { return Err(t); };
                Some(ident.into()).ok_or(t)
            },
            msg,
        )
        .unwrap_or(String::new())
    }

    fn expect_located_id(&mut self, msg: &str) -> Located<String> {
        self.expect(
            |t| {
                let Token::Ident(ident) = t.data else { return Err(t); };
                Some(Located::new(t.span, ident.into())).ok_or(t)
            },
            msg,
        )
        .unwrap_or_else(|err| Located::new(err.span, String::new()))
    }

    fn matches(&mut self, pred: impl FnOnce(&Token) -> bool) -> bool {
        self.lexer.peek().map_or(
            false,
            |token| matches!(token, Ok(token) if pred(&token.data)),
        )
    }

    fn matches_kind(&mut self, kind: Token) -> bool {
        self.matches(|t| t == &kind)
    }
}
