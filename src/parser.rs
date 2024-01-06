use std::path::PathBuf;

use crate::{
    ast::{
        parsed::{
            Destructure, Expr, ExprData, Fn, ImplBlock, IntPattern, MemVar, Param, Path, Pattern,
            RangePattern, Stmt, StmtData, Struct, TypeHint,
        },
        Attribute, UnaryOp,
    },
    error::{Diagnostics, Error, FileId},
    lexer::{Lexer, LexerResult, Located, Precedence, Span, Token},
    THIS_PARAM, THIS_TYPE,
};

#[derive(Clone, Copy)]
struct FnConfig {
    allow_method: bool,
    is_public: bool,
    is_async: bool,
    is_extern: bool,
    is_unsafe: bool,
    body: bool,
}

pub struct Parser<'a, 'b> {
    lexer: Lexer<'a>,
    peek: Option<LexerResult<'a>>,
    needs_sync: bool,
    diag: &'b mut Diagnostics,
    attrs: Vec<Attribute>,
    is_unsafe: Option<Located<Token<'a>>>,
}

impl<'a, 'b> Parser<'a, 'b> {
    fn new(src: &'a str, diag: &'b mut Diagnostics, file: FileId) -> Self {
        Self {
            diag,
            lexer: Lexer::new(src, file),
            peek: None,
            needs_sync: false,
            attrs: Vec::new(),
            is_unsafe: None,
        }
    }

    pub fn parse(src: &'a str, diag: &'b mut Diagnostics, path: PathBuf) -> std::io::Result<Stmt> {
        let file = diag.add_file(path);
        let mut this = Self::new(src, diag, file);
        let mut stmts = Vec::new();
        while !this.matches_kind(Token::Eof) {
            stmts.push(this.item());
        }

        Ok(Stmt {
            data: StmtData::Module {
                public: true,
                body: stmts,
                name: crate::derive_module_name(this.diag.file_path(file)),
            },
            span: Span {
                file,
                pos: 0,
                len: 0,
            },
            attrs: Vec::new(),
        })
    }

    //

    fn try_item(&mut self) -> Option<Stmt> {
        while let Some(token) = self.advance_if_kind(Token::HashLCurly) {
            let attr = self.csv_one(Token::RCurly, token.span, Self::attribute);
            self.attrs.extend(attr.data);
        }

        let public = self.advance_if_kind(Token::Pub);
        let is_unsafe = self.advance_if_kind(Token::Unsafe);
        let is_extern = self.advance_if_kind(Token::Extern);
        if let Some(Located { span, data }) = self.try_parse_fn(FnConfig {
            allow_method: false,
            is_public: public.is_some(),
            is_async: false,
            is_extern: is_extern.is_some(),
            is_unsafe: is_unsafe.is_some(),
            body: is_extern.is_none(),
        }) {
            let attrs = std::mem::take(&mut self.attrs);
            Some(Stmt {
                data: StmtData::Fn(data),
                span: public.map_or(span, |p| p.span).extended_to(span),
                attrs,
            })
        } else if let Some(token) = self.advance_if_kind(Token::Struct) {
            if let Some(token) = is_unsafe {
                self.error_unconditional(Error::not_valid_here("unsafe", token.span));
            }

            if let Some(token) = is_extern {
                self.error_unconditional(Error::not_valid_here("extern", token.span));
            }

            Some(self.parse_struct(public.is_some(), token.span))
        } else if let Some(token) = self.advance_if_kind(Token::Union) {
            if let Some(token) = is_extern {
                self.error(Error::not_valid_here("extern", token.span));
            }

            Some(self.parse_union(public.is_some(), token.span, is_unsafe.is_some()))
        } else if let Some(token) = self.advance_if_kind(Token::Trait) {
            if let Some(token) = is_extern {
                self.error_unconditional(Error::not_valid_here("extern", token.span));
            }

            Some(self.parse_trait(public.is_some(), token.span, is_unsafe.is_some()))
        } else if let Some(token) = self.advance_if_kind(Token::Enum) {
            if let Some(token) = is_unsafe {
                self.error_unconditional(Error::not_valid_here("unsafe", token.span));
            }

            if let Some(token) = is_extern {
                self.error_unconditional(Error::not_valid_here("extern", token.span));
            }

            Some(self.parse_enum(public.is_some(), token.span))
        } else if let Some(token) = self.advance_if_kind(Token::Extension) {
            if let Some(token) = is_unsafe {
                self.error_unconditional(Error::not_valid_here("unsafe", token.span));
            }

            if let Some(token) = is_extern {
                self.error_unconditional(Error::not_valid_here("extern", token.span));
            }

            Some(self.parse_extension(public.is_some(), token.span))
        } else if let Some(token) = self.advance_if_kind(Token::Mod) {
            let attrs = std::mem::take(&mut self.attrs);
            if let Some(token) = is_unsafe {
                self.error_unconditional(Error::not_valid_here("unsafe", token.span));
            }

            if let Some(token) = is_extern {
                self.error_unconditional(Error::not_valid_here("extern", token.span));
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
                self.error_unconditional(Error::not_valid_here("unsafe", token.span));
            }

            if let Some(token) = is_extern {
                self.error_unconditional(Error::not_valid_here("extern", token.span));
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
                self.error_unconditional(Error::not_valid_here("unsafe", token.span));
            }

            if let Some(token) = is_extern {
                self.error_unconditional(Error::not_valid_here("extern", token.span));
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
                self.error_unconditional(Error::new("expected item", public.span));
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
                self.error_unconditional(Error::not_valid_here("unsafe", is_unsafe.span));
            }

            let patt = self.pattern(matches!(token.data, Token::Mut));
            let ty = self
                .advance_if_kind(Token::Colon)
                .map(|_| self.parse_type());
            let value = self
                .advance_if_kind(Token::Assign)
                .map(|_| self.expression());
            let semi = self.expect_kind(Token::Semicolon, "expected ';'");
            Stmt {
                data: StmtData::Let { ty, value, patt },
                span: token.span.extended_to(semi.span),
                attrs: std::mem::take(&mut self.attrs),
            }
        } else {
            let is_unsafe = self.is_unsafe.take();
            let (needs_semicolon, mut expr) = self.block_or_normal_expr();
            if self.matches_kind(Token::RCurly) {
                expr = Expr::new(expr.span, ExprData::Tail(expr.into()));
            } else if needs_semicolon {
                self.expect_kind(Token::Semicolon, "expected ';'");
            } else {
                self.advance_if_kind(Token::Semicolon);
            }

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
        self.precedence(Precedence::Min)
    }

    fn precedence(&mut self, prec: Precedence) -> Expr {
        let mut expr = self.prefix();
        while let Some(token) = self.advance_if(|tk| prec < tk.precedence()) {
            expr = self.infix(expr, token);
        }
        expr
    }

    fn prefix(&mut self) -> Expr {
        let Located { mut span, data } = self.advance();
        match data {
            // Literals
            Token::Void => Expr::new(span, ExprData::Void),
            Token::False => Expr::new(span, ExprData::Bool(false)),
            Token::True => Expr::new(span, ExprData::Bool(true)),
            Token::None => Expr::new(span, ExprData::None),
            Token::Int { base, value, width } => Expr::new(
                span,
                ExprData::Integer {
                    base,
                    value: value.into(),
                    width: width.map(|w| w.into()),
                },
            ),
            Token::Float(value) => Expr::new(span, ExprData::Float(value.into())),
            Token::String(value) => Expr::new(span, ExprData::String(value.into())),
            Token::Char(value) => Expr::new(span, ExprData::Char(value)),
            Token::ByteString(value) => Expr::new(span, ExprData::ByteString(value.into())),
            Token::ByteChar(value) => Expr::new(span, ExprData::ByteChar(value)),
            Token::Ident(ident) => {
                let data = self.path_components(Some(ident), &mut span);
                Expr::new(span, ExprData::Path(Path::Normal(data)))
            }
            Token::This => Expr::new(span, ExprData::Path(THIS_PARAM.to_owned().into())),
            Token::ThisType => Expr::new(span, ExprData::Path(THIS_TYPE.to_owned().into())),
            Token::ScopeRes => {
                let ident = self.expect_id("expected name");
                let data = self.path_components(Some(&ident), &mut span);
                Expr::new(span, ExprData::Path(Path::Root(data)))
            }
            Token::Super => {
                let data = self.path_components(None, &mut span);
                Expr::new(span, ExprData::Path(Path::Super(data)))
            }
            // prefix operators
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Ampersand
            | Token::Increment
            | Token::Decrement
            | Token::Exclamation => {
                let op = if data == Token::Ampersand && self.advance_if_kind(Token::Mut).is_some() {
                    UnaryOp::AddrMut
                } else {
                    UnaryOp::try_from_prefix(data).unwrap()
                };

                let expr = self.precedence(Precedence::Prefix);
                Expr::new(
                    span.extended_to(expr.span),
                    ExprData::Unary {
                        op,
                        expr: expr.into(),
                    },
                )
            }
            Token::LogicalAnd => {
                let op = if self.advance_if_kind(Token::Mut).is_some() {
                    UnaryOp::AddrMut
                } else {
                    UnaryOp::Addr
                };

                let expr = self.precedence(Precedence::Prefix);
                Expr::new(
                    span.extended_to(expr.span),
                    ExprData::Unary {
                        op: UnaryOp::Addr,
                        expr: Box::new(Located::new(
                            span,
                            ExprData::Unary {
                                op,
                                expr: expr.into(),
                            },
                        )),
                    },
                )
            }
            // complex expressions
            Token::LParen => {
                let expr = self.expression();
                if self.matches_kind(Token::Comma) {
                    self.csv(vec![expr], Token::RParen, span, Self::expression)
                        .map(ExprData::Tuple)
                } else {
                    let end = self.expect_kind(Token::RParen, "expected ')'");
                    Expr::new(span.extended_to(end.span), expr.data)
                }
            }
            Token::Range => {
                if self.is_range_end() {
                    Expr::new(
                        span,
                        ExprData::Range {
                            start: None,
                            end: None,
                            inclusive: false,
                        },
                    )
                } else {
                    let end = self.precedence(data.precedence());
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
                let end = self.precedence(data.precedence());
                Expr::new(
                    span.extended_to(end.span),
                    ExprData::Range {
                        start: None,
                        end: Some(end.into()),
                        inclusive: true,
                    },
                )
            }
            Token::LCurly => self.block_expr(span),
            Token::If => self.if_expr(span),
            Token::While => self.while_expr(span),
            Token::Loop => self.loop_expr(span),
            Token::For => self.for_expr(span),
            Token::Match => self.match_expr(span),
            Token::LBrace => {
                if let Some(rbrace) = self.advance_if_kind(Token::RBrace) {
                    Expr::new(span.extended_to(rbrace.span), ExprData::Array(Vec::new()))
                } else if self.advance_if_kind(Token::Colon).is_some() {
                    Expr::new(
                        span.extended_to(self.expect_kind(Token::RBrace, "expected ']'").span),
                        ExprData::Map(Vec::new()),
                    )
                } else {
                    let expr = self.expression();
                    if self.advance_if_kind(Token::Colon).is_some() {
                        let value = self.expression();
                        self.csv(vec![(expr, value)], Token::RBrace, span, |this| {
                            let key = this.expression();
                            this.expect_kind(Token::Colon, "expected ':'");
                            (key, this.expression())
                        })
                        .map(ExprData::Map)
                    } else if self.advance_if_kind(Token::Semicolon).is_some() {
                        let count = self.expression();
                        let rbrace = self.expect_kind(Token::RBrace, "expected ']'");
                        Expr::new(
                            span.extended_to(rbrace.span),
                            ExprData::ArrayWithInit {
                                init: expr.into(),
                                count: count.into(),
                            },
                        )
                    } else if let Some(rbrace) = self.advance_if_kind(Token::RBrace) {
                        Expr::new(span.extended_to(rbrace.span), ExprData::Array(vec![expr]))
                    } else {
                        self.csv(vec![expr], Token::RBrace, span, Self::expression)
                            .map(ExprData::Array)
                    }
                }
            }
            Token::At => {
                self.expect_kind(Token::LBrace, "expected '['");
                if let Some(rbrace) = self.advance_if_kind(Token::RBrace) {
                    Expr::new(span.extended_to(rbrace.span), ExprData::Vec(Vec::new()))
                } else {
                    let expr = self.expression();
                    if self.advance_if_kind(Token::Semicolon).is_some() {
                        let count = self.expression();
                        let rbrace = self.expect_kind(Token::RBrace, "expected ']'");
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
            }
            Token::Hash => {
                self.expect_kind(Token::LBrace, "expected '['");
                self.csv(Vec::new(), Token::RBrace, span, Self::expression)
                    .map(|params| {
                        if data == Token::At {
                            ExprData::Vec(params)
                        } else {
                            ExprData::Set(params)
                        }
                    })
            }
            Token::Fn => {
                self.expect_kind(Token::LParen, "expected '('");
                let params = self
                    .csv(Vec::new(), Token::RParen, span, |this| {
                        (
                            this.expect_located_id("expected parameter name"),
                            this.advance_if_kind(Token::Colon)
                                .map(|_| this.parse_type()),
                        )
                    })
                    .data;
                let ret = self
                    .advance_if_kind(Token::Colon)
                    .map(|_| self.parse_type());
                self.expect_kind(Token::FatArrow, "expected '=>'");

                let body = self.expression();
                Expr::new(
                    span.extended_to(body.span),
                    ExprData::Lambda {
                        params,
                        ret,
                        body: body.into(),
                    },
                )
            }
            Token::Return | Token::Break | Token::Yield => {
                let (span, expr) = if !self.is_range_end() {
                    let expr = self.expression();
                    (span.extended_to(expr.span), expr.into())
                } else {
                    (span, Expr::new(span, ExprData::Void).into())
                };

                Expr::new(
                    span,
                    match data {
                        Token::Return => ExprData::Return(expr),
                        Token::Break => ExprData::Break(expr),
                        Token::Yield => ExprData::Yield(expr),
                        _ => unreachable!(),
                    },
                )
            }
            Token::Unsafe => {
                let expr = self.expression();
                Expr::new(span.extended_to(expr.span), ExprData::Unsafe(expr.into()))
            }
            Token::Continue => Expr::new(span, ExprData::Continue),
            _ => {
                self.error(Error::new("unexpected token", span));
                Expr::new(span, ExprData::Error)
            }
        }
    }

    fn infix(&mut self, left: Expr, op: Located<Token>) -> Expr {
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
            Token::LogicalOr
            | Token::LogicalAnd
            | Token::NoneCoalesce
            | Token::Or
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
            | Token::NotEqual => {
                let right = self.precedence(op.data.precedence());
                Expr::new(
                    left.span.extended_to(right.span),
                    ExprData::Binary {
                        op: op.data.try_into().unwrap(),
                        left: left.into(),
                        right: right.into(),
                    },
                )
            }
            Token::Assign
            | Token::AddAssign
            | Token::SubAssign
            | Token::MulAssign
            | Token::DivAssign
            | Token::RemAssign
            | Token::AndAssign
            | Token::OrAssign
            | Token::XorAssign
            | Token::ShlAssign
            | Token::ShrAssign
            | Token::NoneCoalesceAssign => {
                let right = self.precedence(Precedence::Min);
                Expr::new(
                    left.span.extended_to(right.span),
                    ExprData::Assign {
                        target: left.into(),
                        binary: op.data.try_into().ok(),
                        value: right.into(),
                    },
                )
            }
            Token::Range | Token::RangeInclusive => {
                let inclusive = op.data == Token::RangeInclusive;
                if self.is_range_end() {
                    Expr::new(
                        left.span.extended_to(op.span),
                        ExprData::Range {
                            start: Some(left.into()),
                            end: None,
                            inclusive,
                        },
                    )
                } else {
                    let right = self.precedence(op.data.precedence());
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
                    pattern: self.pattern(false),
                },
            ),
            Token::As => {
                let bang = self.advance_if_kind(Token::Exclamation);
                let ty = self.parse_type();
                Expr::new(
                    left.span,
                    ExprData::As {
                        expr: left.into(),
                        ty,
                        throwing: bang.is_some(),
                    },
                )
            }
            Token::Dot => {
                let member = self.expect_located_id("expected member name");
                let generics = if self.advance_if_kind(Token::ScopeRes).is_some() {
                    self.expect_kind(Token::LAngle, "expected '<'");
                    self.rangle_csv_one(left.span, Self::parse_type)
                } else {
                    Located::new(left.span.extended_to(member.span), Vec::new())
                };

                generics.map(|generics| ExprData::Member {
                    source: left.into(),
                    member: member.data,
                    generics,
                })
            }
            Token::LParen => {
                let args = self.csv(Vec::new(), Token::RParen, left.span, |this| {
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

                args.map(|args| ExprData::Call {
                    callee: left.into(),
                    args,
                })
            }
            Token::LBrace => self
                .csv(Vec::new(), Token::RBrace, left.span, Self::expression)
                .map(|args| ExprData::Subscript {
                    callee: left.into(),
                    args,
                }),
            _ => {
                self.error(Error::new("unexpected token", op.span));
                Expr::new(op.span, ExprData::Error)
            }
        }
    }

    //

    fn block_or_normal_expr(&mut self) -> (bool, Expr) {
        if let Some(token) = self.advance_if_kind(Token::If) {
            (false, self.if_expr(token.span))
        } else if let Some(token) = self.advance_if_kind(Token::While) {
            (false, self.while_expr(token.span))
        } else if let Some(token) = self.advance_if_kind(Token::Loop) {
            let expr = self.loop_expr(token.span);
            (
                matches!(&expr.data, ExprData::Loop { do_while: true, .. }),
                expr,
            )
        } else if let Some(token) = self.advance_if_kind(Token::For) {
            (false, self.for_expr(token.span))
        } else if let Some(token) = self.advance_if_kind(Token::Match) {
            (false, self.match_expr(token.span))
        } else if let Some(token) = self.advance_if_kind(Token::LCurly) {
            (false, self.block_expr(token.span))
        } else {
            (true, self.expression())
        }
    }

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
        let Located { span, data: body } = self.parse_block();
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
        let body = self.parse_block().data;
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
        let patt = self.pattern(false);
        self.expect_kind(Token::In, "expected 'in'");
        // TODO: parse for foo in 0.. {} as |0..| |{}| instead of |0..{}|
        let iter = self.expression();
        let Located { span, data: body } = self.parse_block();
        Expr::new(
            token.extended_to(span),
            ExprData::For {
                patt,
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
            let pattern = this.pattern(false);
            this.expect_kind(Token::FatArrow, "expected '=>'");
            let (needs_comma, expr) = this.block_or_normal_expr();
            if needs_comma {
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
                let params = self.rangle_csv_one(*outspan, Self::parse_type);
                data.last_mut().unwrap().1 = params.data;
                *outspan = params.span;
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
                let params = self.rangle_csv_one(span.unwrap(), Self::parse_type);
                data.push((ident.data, params.data));
                span = Some(params.span);
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

    fn maybe_range_pattern(&mut self, start: Located<IntPattern>) -> Located<Pattern> {
        let Some(range) = self.advance_if(|t| matches!(t, Token::Range | Token::RangeInclusive))
        else {
            return start.map(Pattern::Int);
        };

        let negative = self.advance_if_kind(Token::Minus);
        let Ok(end) = self.expect(
            |t| Self::int_pattern(negative.map(|t| t.span), &t).ok_or(t),
            "expected number",
        ) else {
            return Located::new(start.span, Pattern::Error);
        };

        Located::new(
            start.span.extended_to(end.span),
            Pattern::IntRange(RangePattern {
                inclusive: matches!(range.data, Token::RangeInclusive),
                start: start.data,
                end: end.data,
            }),
        )
    }

    fn literal_pattern(&mut self) -> Option<Located<Pattern>> {
        let string = self.advance_if_map(|t| {
            t.data
                .as_string()
                .map(|value| Located::new(t.span, value.to_string()))
        });

        if let Some(string) = string {
            return Some(string.map(Pattern::String));
        }

        if let Some(token) = self.advance_if_kind(Token::Minus) {
            let Ok(start) = self.expect(
                |t| Self::int_pattern(Some(token.span), &t).ok_or(t),
                "expected number",
            ) else {
                return Some(Located::new(token.span, Pattern::Error));
            };

            return Some(self.maybe_range_pattern(start));
        }

        let int = self.advance_if_map(|t| Self::int_pattern(None, t));
        if let Some(int) = int {
            return Some(self.maybe_range_pattern(int));
        }

        let char = self.advance_if_map(|t| t.data.as_char().map(|&ch| Located::new(t.span, ch)));
        if let Some(char) = char {
            let Some(range) =
                self.advance_if(|t| matches!(t, Token::Range | Token::RangeInclusive))
            else {
                return Some(char.map(Pattern::Char));
            };

            let Ok(end) = self.expect(
                |t| {
                    t.data
                        .as_char()
                        .map(|&ch| Located::new(t.span, ch))
                        .ok_or(t)
                },
                "expected char",
            ) else {
                return Some(Located::new(char.span, Pattern::Error));
            };
            return Some(Located::new(
                char.span.extended_to(end.span),
                Pattern::CharRange(RangePattern {
                    inclusive: matches!(range.data, Token::RangeInclusive),
                    start: char.data,
                    end: end.data,
                }),
            ));
        }

        None
    }

    fn pattern(&mut self, mut_var: bool) -> Located<Pattern> {
        if self.advance_if_kind(Token::Question).is_some() {
            return self
                .pattern(false)
                .map(|inner| Pattern::Option(inner.into()));
        }

        if let Some(token) = self.advance_if_kind(Token::None) {
            return Located::new(token.span, Pattern::Null);
        }

        if let Some(token) = self.advance_if_kind(Token::LCurly) {
            return self
                .csv_one(Token::RCurly, token.span, |this| {
                    let mutable = this.advance_if_kind(Token::Mut).is_some();
                    // if mut_var, this mutable is redundant
                    let name = this.expect_located_id("expected name");
                    Destructure {
                        name,
                        mutable: mutable || mut_var,
                        pattern: this
                            .advance_if_kind(Token::Colon)
                            .map(|_| this.pattern(true)),
                    }
                })
                .map(Pattern::StructDestructure);
        }

        if let Some(token) = self.advance_if_kind(Token::LBrace) {
            return self
                .csv(Vec::new(), Token::RBrace, token.span, |this| {
                    if let Some(token) = this.advance_if_kind(Token::Ellipses) {
                        let pattern = if this.advance_if_kind(Token::Mut).is_some() {
                            let ident = this.expect_id("expected name");
                            Some((true, ident))
                        } else {
                            let ident =
                                this.advance_if_map(|t| t.data.as_ident().map(|&i| i.into()));
                            Some(false).zip(ident)
                        };

                        Located::new(token.span, Pattern::Rest(pattern))
                    } else {
                        this.pattern(false)
                    }
                })
                .map(Pattern::Array);
        }

        if mut_var || self.advance_if_kind(Token::Mut).is_some() {
            return self
                .expect_located_id("expected name")
                .map(Pattern::MutBinding);
        }

        if !mut_var {
            if let Some(pattern) = self.literal_pattern() {
                return pattern;
            }
        }

        let path = self.type_path();
        if self.advance_if_kind(Token::LParen).is_some() {
            self.csv(Vec::new(), Token::RParen, path.span, |this| {
                this.pattern(false)
            })
            .map(|subpatterns| Pattern::TupleLike { path, subpatterns })
        } else {
            path.map(Pattern::Path)
        }
    }

    fn csv<T>(
        &mut self,
        mut res: Vec<T>,
        end: Token,
        span: Span,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> Located<Vec<T>> {
        if !res.is_empty() && !self.matches_kind(end.clone()) {
            self.expect_kind(Token::Comma, "expected ','");
        }

        let span = self.advance_until(end.clone(), span, |this| {
            res.push(f(this));

            if !this.matches_kind(end.clone()) {
                this.expect_kind(Token::Comma, "expected ','");
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
        if !res.is_empty() && !self.matches(|tk| matches!(tk, Token::RAngle | Token::Shr)) {
            self.expect_kind(Token::Comma, "expected ','");
        }

        loop {
            match self.advance_if(|t| matches!(t, Token::Eof | Token::RAngle | Token::Shr)) {
                Some(t) if t.data == Token::Shr => {
                    self.peek = Some(Ok(Located::new(t.span, Token::RAngle)));
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
            if !self.matches(|t| matches!(t, Token::RAngle | Token::Shr)) {
                self.expect_kind(Token::Comma, "expected ','");
            }
        }

        Located::new(span, res)
    }

    fn parse_type_params(&mut self) -> Vec<(String, Vec<Located<Path>>)> {
        self.advance_if_kind(Token::LAngle)
            .map(|_| {
                self.rangle_csv_one(Span::default(), |this| {
                    (
                        this.expect_id("expected type name"),
                        this.parse_trait_impl(),
                    )
                })
                .data
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
                    .data,
            )
        } else if self.advance_if_kind(Token::Void).is_some() {
            TypeHint::Void
        } else if let Some(this) = self.advance_if_kind(Token::ThisType) {
            TypeHint::Regular(Located::new(this.span, Path::from(THIS_TYPE.to_owned())))
        } else if self.advance_if_kind(Token::Fn).is_some() {
            self.expect_kind(Token::LParen, "expected '('");
            let params = self
                .csv(Vec::new(), Token::RParen, Span::default(), Self::parse_type)
                .data;
            let ret = if self.advance_if_kind(Token::FatArrow).is_some() {
                self.parse_type()
            } else {
                TypeHint::Void
            };

            TypeHint::Fn {
                is_extern: false,
                params,
                ret: ret.into(),
            }
        } else {
            TypeHint::Regular(self.type_path())
        }
    }

    fn parse_block(&mut self) -> Located<Vec<Stmt>> {
        let mut stmts = Vec::new();
        let lcurly = self.expect_kind(Token::LCurly, "expected '{'");
        let span = self.advance_until(Token::RCurly, lcurly.span, |this| {
            stmts.push(this.statement());
        });
        Located::new(span, stmts)
    }

    fn parse_struct(&mut self, public: bool, span: Span) -> Stmt {
        let attrs = std::mem::take(&mut self.attrs);
        let name = self.expect_located_id("expected name");
        let type_params = self.parse_type_params();

        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            let config = FnConfig {
                allow_method: true,
                is_public: this.advance_if_kind(Token::Pub).is_some(),
                is_async: false,
                is_extern: false,
                is_unsafe: this.advance_if_kind(Token::Unsafe).is_some(),
                body: true,
            };
            if config.is_unsafe {
                if let Ok(func) = this.expect_fn(config) {
                    functions.push(func.data);
                }
            } else if let Some(func) = this.try_parse_fn(config) {
                functions.push(func.data);
            } else if let Some(token) = this.advance_if_kind(Token::Impl) {
                if config.is_public {
                    // TODO: use the span for `pub`
                    this.error(Error::not_valid_here("pub", token.span));
                }

                impls.push(this.parse_impl_block(token.span));
            } else {
                let name = this.expect_id("expected name");
                this.expect_kind(Token::Colon, "expected type");
                let ty = this.parse_type();
                let value = this
                    .advance_if_kind(Token::Assign)
                    .map(|_| this.expression());

                if !this.matches_kind(Token::RCurly) {
                    this.expect_kind(Token::Comma, "expected ','");
                }
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
            data: StmtData::Struct(Struct {
                public,
                name,
                type_params,
                members,
                impls,
                functions,
            }),
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
        let type_params = self.parse_type_params();
        let mut functions = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();

        self.expect_kind(Token::LCurly, "expected '{'");
        let span = self.advance_until(Token::RCurly, span, |this| {
            let config = FnConfig {
                allow_method: true,
                is_public: this.advance_if_kind(Token::Pub).is_some(),
                is_async: false,
                is_extern: false,
                is_unsafe: this.advance_if_kind(Token::Unsafe).is_some(),
                body: true,
            };
            if config.is_public || config.is_unsafe {
                if let Ok(func) = this.expect_fn(config) {
                    functions.push(func.data);
                }
            } else if let Some(func) = this.try_parse_fn(config) {
                functions.push(func.data);
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
                impls.push(this.parse_impl_block(token.span));
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

                if !this.matches_kind(Token::RCurly) {
                    this.expect_kind(Token::Comma, "expected ','");
                }
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
            data: StmtData::Union {
                tag,
                base: Struct {
                    public,
                    name,
                    type_params,
                    members,
                    functions,
                    impls,
                },
                is_unsafe,
            },
            attrs,
        }
    }

    fn parse_trait(&mut self, public: bool, span: Span, is_unsafe: bool) -> Stmt {
        let attrs = std::mem::take(&mut self.attrs);
        let name = self.expect_located_id("expected name");
        let type_params = self.parse_type_params();
        let impls = self.parse_trait_impl();
        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            let is_unsafe = this.advance_if_kind(Token::Unsafe).is_some();
            if let Ok(proto) = this.expect_fn(FnConfig {
                body: false,
                allow_method: true,
                is_public: true,
                is_async: false,
                is_extern: false,
                is_unsafe,
            }) {
                functions.push(proto.data);
            }
        });

        Stmt {
            span,
            data: StmtData::Trait {
                public,
                is_unsafe,
                name,
                type_params,
                impls,
                functions,
            },
            attrs,
        }
    }

    fn parse_enum(&mut self, public: bool, span: Span) -> Stmt {
        let attrs = std::mem::take(&mut self.attrs);
        let name = self.expect_located_id("expected name");
        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        let mut variants = Vec::new();
        let mut impls = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            let config = FnConfig {
                allow_method: true,
                is_public: this.advance_if_kind(Token::Pub).is_some(),
                is_async: false,
                is_extern: false,
                body: true,
                is_unsafe: this.advance_if_kind(Token::Unsafe).is_some(),
            };
            if config.is_public || config.is_unsafe {
                if let Ok(func) = this.expect_fn(config) {
                    functions.push(func.data);
                }
            } else if let Some(func) = this.try_parse_fn(config) {
                functions.push(func.data);
            } else if let Some(token) = this.advance_if_kind(Token::Impl) {
                impls.push(this.parse_impl_block(token.span));
            } else {
                variants.push((
                    this.expect_id("expected variant name"),
                    if this.advance_if_kind(Token::Assign).is_some() {
                        Some(this.expression())
                    } else {
                        None
                    },
                ));

                if !this.matches_kind(Token::RCurly) {
                    this.expect_kind(Token::Comma, "expected ','");
                }
            }
        });

        Stmt {
            span,
            data: StmtData::Enum {
                public,
                name,
                impls,
                variants,
                functions,
            },
            attrs,
        }
    }

    fn parse_extension(&mut self, public: bool, span: Span) -> Stmt {
        let attrs = std::mem::take(&mut self.attrs);
        let type_params = self.parse_type_params();
        let name = self.expect_id("expected name");

        self.expect_kind(Token::For, "expected 'for'");
        let ty = self.parse_type();
        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        let mut impls = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            if let Some(token) = this.advance_if_kind(Token::Impl) {
                impls.push(this.parse_impl_block(token.span));
            } else {
                let config = FnConfig {
                    allow_method: true,
                    is_public: this.advance_if_kind(Token::Pub).is_some(),
                    is_async: false,
                    is_extern: false,
                    is_unsafe: this.advance_if_kind(Token::Unsafe).is_some(),
                    body: true,
                };
                if let Ok(func) = this.expect_fn(config) {
                    functions.push(func.data);
                }
            }
        });

        Stmt {
            span,
            data: StmtData::Extension {
                public,
                name,
                ty,
                type_params,
                impls,
                functions,
            },
            attrs,
        }
    }

    fn parse_impl_block(&mut self, span: Span) -> ImplBlock {
        let type_params = self.parse_type_params();
        let path = self.type_path();
        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        self.advance_until(Token::RCurly, span, |this| {
            if let Some(token) = this.advance_if_kind(Token::Pub) {
                this.error_unconditional(Error::not_valid_here("pub", token.span));
            }

            let is_unsafe = this.advance_if_kind(Token::Unsafe).is_some();
            if let Ok(func) = this.expect_fn(FnConfig {
                allow_method: true,
                is_public: true,
                is_async: false,
                is_extern: false,
                body: true,
                is_unsafe,
            }) {
                functions.push(func.data);
            }
        });

        ImplBlock {
            type_params,
            path,
            functions,
        }
    }

    fn parse_fn(
        &mut self,
        FnConfig {
            allow_method,
            is_public,
            is_async,
            is_extern,
            is_unsafe,
            body,
        }: FnConfig,
    ) -> Fn {
        let name = self.expect_located_id("expected name");
        let mut variadic = false;
        let type_params = self.parse_type_params();
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
                        self.error_unconditional(Error::new(
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

        let ret = if self.advance_if_kind(Token::Colon).is_some() {
            self.parse_type()
        } else {
            TypeHint::Void
        };

        let body = if body {
            Some(self.parse_block().data)
        } else {
            self.expect_kind(Token::Semicolon, "expected ';'");
            None
        };

        Fn {
            name,
            public: is_public,
            is_async,
            is_extern,
            is_unsafe,
            variadic,
            type_params,
            params,
            ret,
            body,
        }
    }

    fn try_parse_fn(&mut self, params: FnConfig) -> Option<Located<Fn>> {
        if let Some(token) = self.advance_if_kind(Token::Fn) {
            Some(Located::new(
                token.span,
                self.parse_fn(FnConfig {
                    is_async: false,
                    ..params
                }),
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Async) {
            self.expect_kind(Token::Fn, "expected 'fn'");
            Some(Located::new(
                token.span,
                self.parse_fn(FnConfig {
                    is_async: true,
                    ..params
                }),
            ))
        } else {
            None
        }
    }

    fn expect_fn(&mut self, params: FnConfig) -> Result<Located<Fn>, ()> {
        loop {
            if let Some(proto) = self.try_parse_fn(params) {
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
            name: self.expect_located_id("expected name"),
            props: self
                .advance_if_kind(Token::LParen)
                .map(|_| {
                    self.csv_one(Token::RParen, Span::default(), Self::attribute)
                        .data
                })
                .unwrap_or_default(),
        }
    }

    //

    fn synchronize(&mut self) {
        use Token::*;

        loop {
            match self.peek() {
                Ok(token) if token.data == Semicolon => {
                    self.advance();
                    break;
                }
                Ok(token)
                    if matches!(
                        token.data,
                        Pub | Struct
                            | Enum
                            | Union
                            | Extension
                            | Trait
                            | Fn
                            | Let
                            | Static
                            | Extern
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
                _ => {
                    self.advance();
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

    fn error_unconditional(&mut self, err: Error) {
        self.diag.error(err);
    }

    fn advance(&mut self) -> Located<Token<'a>> {
        loop {
            match self.next() {
                Ok(tok) => break tok,
                Err(err) => self.error(Error::new(err.data.tell(), err.span)),
            }
        }
    }

    fn advance_if(&mut self, pred: impl FnOnce(&Token) -> bool) -> Option<Located<Token<'a>>> {
        self.next_if(|tok| matches!(tok, Ok(tok) if pred(&tok.data)))
            .map(|token| token.unwrap())
    }

    fn advance_if_kind(&mut self, kind: Token) -> Option<Located<Token<'a>>> {
        self.advance_if(|t| t == &kind)
    }

    fn advance_if_map<T>(&mut self, pred: impl FnOnce(&Located<Token>) -> Option<T>) -> Option<T> {
        let mut outer = None;
        self.next_if(|t| {
            if let Ok(t) = t {
                outer = pred(t);
                outer.is_some()
            } else {
                false
            }
        });
        outer
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
            |t| t.data.as_ident().map(|&ident| ident.into()).ok_or(t),
            msg,
        )
        .unwrap_or_default()
    }

    fn expect_located_id(&mut self, msg: &str) -> Located<String> {
        self.expect(
            |t| {
                t.data
                    .as_ident()
                    .map(|&ident| Located::new(t.span, ident.into()))
                    .ok_or(t)
            },
            msg,
        )
        .unwrap_or_else(|err| Located::new(err.span, String::new()))
    }

    fn matches(&mut self, pred: impl FnOnce(&Token) -> bool) -> bool {
        matches!(self.peek(), Ok(token) if pred(&token.data))
    }

    fn matches_kind(&mut self, kind: Token) -> bool {
        self.matches(|t| t == &kind)
    }

    fn peek(&mut self) -> &LexerResult<'a> {
        self.peek.get_or_insert_with(|| self.lexer.token())
    }

    fn next(&mut self) -> LexerResult<'a> {
        self.peek.take().unwrap_or_else(|| self.lexer.token())
    }

    fn next_if(&mut self, f: impl FnOnce(&LexerResult) -> bool) -> Option<LexerResult<'a>> {
        f(self.peek()).then(|| self.next())
    }
}
