use std::path::PathBuf;

use crate::{
    ast::{
        parsed::{
            Destructure, Expr, ExprData, Fn, FullPattern, ImplBlock, IntPattern, Linkage, Member,
            Param, Pattern, RangePattern, Stmt, StmtData, Struct, TypeHint, TypePath,
            TypePathComponent,
        },
        Attribute, UnaryOp,
    },
    error::{Diagnostics, Error, FileId},
    lexer::{Lexer, Located, Precedence, Span, Token},
    THIS_PARAM, THIS_TYPE,
};

#[derive(Clone, Copy)]
struct FnConfig {
    allow_method: bool,
    linkage: Linkage,
    is_public: bool,
    is_unsafe: bool,
    body: bool,
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

    pub fn parse(src: &'a str, diag: &'b mut Diagnostics, path: PathBuf) -> Stmt {
        let file = diag.add_file(path);
        let mut this = Self::new(src, diag, file);
        let mut stmts = Vec::new();
        while !this.matches_kind(Token::Eof) {
            stmts.push(this.item());
        }

        Stmt {
            data: StmtData::Module {
                public: true,
                body: stmts,
                name: Located::new(
                    Span {
                        file,
                        pos: 0,
                        len: 0,
                    },
                    crate::derive_module_name(this.diag.file_path(file)),
                ),
            },
            attrs: Vec::new(),
        }
    }

    //

    fn try_item(&mut self) -> Result<Stmt, (Option<Located<Token<'a>>>, Vec<Attribute>)> {
        let mut attrs = vec![];
        while let Some(token) = self.next_if_kind(Token::HashLCurly) {
            let attr = self.csv_one(Token::RCurly, token.span, Self::attribute);
            attrs.extend(attr.data);
        }

        let public = self.next_if_kind(Token::Pub);
        let is_unsafe = self.next_if_kind(Token::Unsafe);
        let is_import = self.next_if_kind(Token::Import);
        let is_export = self.next_if_kind(Token::Export);
        if let Some((_, export)) = is_import.as_ref().zip(is_export.as_ref()) {
            self.error_no_sync(Error::new(
                "cannot combine 'import' and 'export' modifiers",
                export.span,
            ));
        }

        if let Some(func) = self.try_function(FnConfig {
            allow_method: false,
            linkage: match (&is_export, &is_import) {
                (Some(_), None) => Linkage::Export,
                (None, Some(_)) => Linkage::Import,
                _ => Linkage::Internal,
            },
            is_public: public.is_some(),
            is_unsafe: is_unsafe.is_some(),
            body: is_import.is_none(),
        }) {
            Ok(Stmt {
                attrs,
                data: StmtData::Fn(func.data),
            })
        } else if let Some(token) = self.next_if_kind(Token::Struct) {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Ok(Stmt {
                attrs,
                data: self.structure(public.is_some(), token.span),
            })
        } else if let Some(token) = self.next_if_kind(Token::Union) {
            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Ok(Stmt {
                attrs,
                data: self.union(public.is_some(), token.span, is_unsafe.is_some()),
            })
        } else if let Some(token) = self.next_if_kind(Token::Trait) {
            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Ok(Stmt {
                attrs,
                data: self.r#trait(public.is_some(), token.span, is_unsafe.is_some()),
            })
        } else if let Some(token) = self.next_if_kind(Token::Enum) {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Ok(Stmt {
                attrs,
                data: self.enumeration(public.is_some(), token.span),
            })
        } else if let Some(token) = self.next_if_kind(Token::Extension) {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Ok(Stmt {
                attrs,
                data: self.extension(public.is_some(), token.span),
            })
        } else if let Some(token) = self.next_if_kind(Token::Mod) {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            let name = self.expect_id_l("expected name");
            let mut body = Vec::new();
            self.expect_kind(Token::LCurly);
            self.next_until(Token::RCurly, token.span, |this| body.push(this.item()));
            Ok(Stmt {
                data: StmtData::Module {
                    public: public.is_some(),
                    name,
                    body,
                },
                attrs,
            })
        } else if let Some(token) = self.next_if_kind(Token::Use) {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            let root = self.next_if_kind(Token::ScopeRes);
            let sup = root
                .is_none()
                .then(|| self.next_if_kind(Token::Super))
                .flatten()
                .is_some();
            if sup {
                self.expect_kind(Token::ScopeRes);
            }

            let mut components = Vec::new();
            let mut all = false;
            loop {
                if (sup || !components.is_empty()) && self.next_if_kind(Token::Asterisk).is_some() {
                    all = true;
                    break;
                }

                components.push(TypePathComponent(
                    self.expect_id("expected path component"),
                    Vec::new(),
                ));
                if self.next_if_kind(Token::ScopeRes).is_none() {
                    break;
                }
            }

            let semi = self.expect_kind(Token::Semicolon);
            Ok(Stmt {
                data: StmtData::Use {
                    public: public.is_some(),
                    path: Located::new(
                        token.span.extended_to(semi.span),
                        if root.is_some() {
                            TypePath::Root(components)
                        } else if sup {
                            TypePath::Super(components)
                        } else {
                            TypePath::Normal(components)
                        },
                    ),
                    all,
                },
                attrs,
            })
        } else if self.next_if_kind(Token::Static).is_some() {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            let name = self.expect_id("expected name");
            self.expect_kind(Token::Colon);
            let ty = self.type_hint();
            self.expect_kind(Token::Assign);
            let value = self.expression();
            self.expect_kind(Token::Semicolon);
            Ok(Stmt {
                data: StmtData::Static {
                    public: public.is_some(),
                    name,
                    ty,
                    value,
                },
                attrs,
            })
        } else {
            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Err((is_unsafe, attrs))
        }
    }

    fn item(&mut self) -> Stmt {
        self.try_item().unwrap_or_else(|(_, attrs)| {
            let span = self.next().span;
            self.error(Error::new("expected item", span));
            Stmt {
                data: StmtData::Error,
                attrs,
            }
        })
    }

    fn statement(&mut self) -> Stmt {
        let stmt = match self.try_item() {
            Ok(item) => item,
            Err((is_unsafe, attrs)) => {
                if let Some(token) = self.next_if(|t| matches!(t, Token::Let | Token::Mut)) {
                    if let Some(is_unsafe) = is_unsafe {
                        self.error_no_sync(Error::not_valid_here(&is_unsafe));
                    }

                    let patt = self.pattern(matches!(token.data, Token::Mut));
                    let ty = self.next_if_kind(Token::Colon).map(|_| self.type_hint());
                    let value = self.next_if_kind(Token::Assign).map(|_| self.expression());
                    self.expect_kind(Token::Semicolon);
                    Stmt {
                        data: StmtData::Let { ty, value, patt },
                        attrs,
                    }
                } else {
                    let (needs_semicolon, mut expr) = self.block_or_normal_expr();
                    if self.matches_kind(Token::RCurly) {
                        expr = Expr::new(expr.span, ExprData::Tail(expr.into()));
                    } else if needs_semicolon {
                        self.expect_kind(Token::Semicolon);
                    } else {
                        self.next_if_kind(Token::Semicolon);
                    }

                    if let Some(is_unsafe) = is_unsafe {
                        expr = Expr::new(
                            is_unsafe.span.extended_to(expr.span),
                            ExprData::Unsafe(expr.into()),
                        );
                    }

                    Stmt {
                        attrs,
                        data: StmtData::Expr(expr),
                    }
                }
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
        while let Some(token) = self.next_if(|tk| prec < tk.precedence()) {
            expr = self.infix(expr, token);
        }
        expr
    }

    fn prefix(&mut self) -> Expr {
        let Located { mut span, data } = self.next();
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
            Token::ByteString(value) => Expr::new(span, ExprData::ByteString(value)),
            Token::ByteChar(value) => Expr::new(span, ExprData::ByteChar(value)),
            Token::Ident(ident) => {
                let data = self.path_components(Some(ident), &mut span);
                Expr::new(span, ExprData::Path(TypePath::Normal(data)))
            }
            Token::This => Expr::new(span, ExprData::Path(THIS_PARAM.to_owned().into())),
            Token::ThisType => Expr::new(span, ExprData::Path(THIS_TYPE.to_owned().into())),
            Token::ScopeRes => {
                let ident = self.expect_id("expected name");
                let data = self.path_components(Some(&ident), &mut span);
                Expr::new(span, ExprData::Path(TypePath::Root(data)))
            }
            Token::Super => {
                let data = self.path_components(None, &mut span);
                Expr::new(span, ExprData::Path(TypePath::Super(data)))
            }
            // prefix operators
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Ampersand
            | Token::Increment
            | Token::Decrement
            | Token::Exclamation => {
                let op = if data == Token::Ampersand && self.next_if_kind(Token::Mut).is_some() {
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
                let op = if self.next_if_kind(Token::Mut).is_some() {
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
                    let end = self.expect_kind(Token::RParen);
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
                if let Some(rbrace) = self.next_if_kind(Token::RBrace) {
                    Expr::new(span.extended_to(rbrace.span), ExprData::Array(Vec::new()))
                } else if self.next_if_kind(Token::Colon).is_some() {
                    Expr::new(
                        span.extended_to(self.expect_kind(Token::RBrace).span),
                        ExprData::Map(Vec::new()),
                    )
                } else {
                    let expr = self.expression();
                    if self.next_if_kind(Token::Colon).is_some() {
                        let value = self.expression();
                        self.csv(vec![(expr, value)], Token::RBrace, span, |this| {
                            let key = this.expression();
                            this.expect_kind(Token::Colon);
                            (key, this.expression())
                        })
                        .map(ExprData::Map)
                    } else if self.next_if_kind(Token::Semicolon).is_some() {
                        let count = self.expression();
                        let rbrace = self.expect_kind(Token::RBrace);
                        Expr::new(
                            span.extended_to(rbrace.span),
                            ExprData::ArrayWithInit {
                                init: expr.into(),
                                count: count.into(),
                            },
                        )
                    } else if let Some(rbrace) = self.next_if_kind(Token::RBrace) {
                        Expr::new(span.extended_to(rbrace.span), ExprData::Array(vec![expr]))
                    } else {
                        self.csv(vec![expr], Token::RBrace, span, Self::expression)
                            .map(ExprData::Array)
                    }
                }
            }
            Token::At => {
                self.expect_kind(Token::LBrace);
                if let Some(rbrace) = self.next_if_kind(Token::RBrace) {
                    Expr::new(span.extended_to(rbrace.span), ExprData::Vec(Vec::new()))
                } else {
                    let expr = self.expression();
                    if self.next_if_kind(Token::Semicolon).is_some() {
                        let count = self.expression();
                        let rbrace = self.expect_kind(Token::RBrace);
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
                self.expect_kind(Token::LBrace);
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
                self.expect_kind(Token::LParen);
                let params = self
                    .csv(Vec::new(), Token::RParen, span, |this| {
                        (
                            this.expect_id_l("expected parameter name"),
                            this.next_if_kind(Token::Colon).map(|_| this.type_hint()),
                        )
                    })
                    .data;
                let ret = self.next_if_kind(Token::Colon).map(|_| self.type_hint());
                self.expect_kind(Token::FatArrow);

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
            Token::Return => {
                let (span, expr) = if !self.is_range_end() {
                    let expr = self.expression();
                    (span.extended_to(expr.span), expr.into())
                } else {
                    (span, Expr::new(span, ExprData::Void).into())
                };

                Expr::new(span, ExprData::Return(expr))
            }
            Token::Break => {
                let (span, expr) = if !self.is_range_end() {
                    let expr = self.expression();
                    (span.extended_to(expr.span), Some(expr.into()))
                } else {
                    (span, None)
                };

                Expr::new(span, ExprData::Break(expr))
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
                    pattern: self.full_pattern(),
                },
            ),
            Token::As => {
                let bang = self.next_if_kind(Token::Exclamation);
                let ty = self.type_hint();
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
                let member = self.expect_id_l("expected member name");
                let generics = if self.next_if_kind(Token::ScopeRes).is_some() {
                    self.expect_kind(Token::LAngle);
                    self.rangle_csv_one(left.span, Self::type_hint)
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
                            .filter(|_| this.next_if_kind(Token::Colon).is_some())
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
        if let Some(token) = self.next_if_kind(Token::If) {
            (false, self.if_expr(token.span))
        } else if let Some(token) = self.next_if_kind(Token::While) {
            (false, self.while_expr(token.span))
        } else if let Some(token) = self.next_if_kind(Token::Loop) {
            let expr = self.loop_expr(token.span);
            (
                matches!(&expr.data, ExprData::Loop { do_while: true, .. }),
                expr,
            )
        } else if let Some(token) = self.next_if_kind(Token::For) {
            (false, self.for_expr(token.span))
        } else if let Some(token) = self.next_if_kind(Token::Match) {
            (false, self.match_expr(token.span))
        } else if let Some(token) = self.next_if_kind(Token::LCurly) {
            (false, self.block_expr(token.span))
        } else if self.next_if_kind(Token::Unsafe).is_some() {
            let (is_unsafe, expr) = self.block_or_normal_expr();
            (
                is_unsafe,
                Expr::new(expr.span, ExprData::Unsafe(expr.into())),
            )
        } else {
            (true, self.expression())
        }
    }

    fn if_expr(&mut self, token: Span) -> Expr {
        let cond = self.expression();
        let lcurly = self.expect_kind(Token::LCurly);
        let if_branch = self.block_expr(lcurly.span);
        let else_branch = self.next_if_kind(Token::Else).map(|_| {
            if !self.matches_kind(Token::If) {
                let lcurly = self.expect_kind(Token::LCurly);
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
        let Located { span, data: body } = self.block();
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
        let body = self.block().data;
        let (cond, do_while) = self
            .next_if_kind(Token::While)
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
        self.expect_kind(Token::In);
        // TODO: parse for foo in 0.. {} as |0..| |{}| instead of |0..{}|
        let iter = self.expression();
        let Located { span, data: body } = self.block();
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
        self.expect_kind(Token::LCurly);
        let mut body = Vec::new();
        let span = self.next_until(Token::RCurly, token, |this| {
            let pattern = this.full_pattern();
            this.expect_kind(Token::FatArrow);
            let (needs_comma, expr) = this.block_or_normal_expr();
            if needs_comma {
                this.expect_kind(Token::Comma);
            } else {
                this.next_if_kind(Token::Comma);
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
        let span = self.next_until(Token::RCurly, token, |this| {
            stmts.push(this.statement());
        });
        Expr::new(span, ExprData::Block(stmts))
    }

    //

    fn path_components(
        &mut self,
        first: Option<&str>,
        outspan: &mut Span,
    ) -> Vec<TypePathComponent> {
        let mut data = first
            .map(|s| vec![TypePathComponent(s.into(), Vec::new())])
            .unwrap_or_default();
        while self.next_if_kind(Token::ScopeRes).is_some() {
            if self.next_if_kind(Token::LAngle).is_some() {
                let params = self.rangle_csv_one(*outspan, Self::type_hint);
                data.last_mut().unwrap().1 = params.data;
                *outspan = params.span;
            } else {
                let name = self.expect_id_l("expected name");
                data.push(TypePathComponent(name.data.to_owned(), Vec::new()));
                outspan.extend_to(name.span);
            }
        }

        data
    }

    fn type_path(&mut self) -> Located<TypePath> {
        let root = self.next_if_kind(Token::ScopeRes);
        let sup = root
            .is_none()
            .then(|| self.next_if_kind(Token::Super))
            .flatten();
        if sup.is_some() {
            self.expect_kind(Token::ScopeRes);
        }

        let mut data = Vec::new();
        let mut span = root.as_ref().or(sup.as_ref()).map(|t| t.span);
        loop {
            let ident = self.expect_id_l("expected type name");
            if let Some(span) = &mut span {
                span.extend_to(ident.span);
            } else {
                span = Some(ident.span);
            }
            if self.next_if_kind(Token::LAngle).is_some() {
                let params = self.rangle_csv_one(span.unwrap(), Self::type_hint);
                data.push(TypePathComponent(ident.data, params.data));
                span = Some(params.span);
            } else {
                data.push(TypePathComponent(ident.data, Vec::new()));
            }

            if self.next_if_kind(Token::ScopeRes).is_none() {
                break;
            }
        }

        Located::new(
            span.unwrap(),
            if root.is_some() {
                TypePath::Root(data)
            } else if sup.is_some() {
                TypePath::Super(data)
            } else {
                TypePath::Normal(data)
            },
        )
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

    fn maybe_range_pattern(&mut self, start: Located<IntPattern>) -> Located<Pattern> {
        let Some(range) = self.next_if(|t| matches!(t, Token::Range | Token::RangeInclusive))
        else {
            return start.map(Pattern::Int);
        };

        let negative = self.next_if_kind(Token::Minus);
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
        let string = self.next_if_map(|t| {
            t.data
                .as_string()
                .map(|value| Located::new(t.span, value.to_string()))
        });

        if let Some(string) = string {
            return Some(string.map(Pattern::String));
        }

        if let Some(token) = self.next_if_kind(Token::Minus) {
            let Ok(start) = self.expect(
                |t| Self::int_pattern(Some(token.span), &t).ok_or(t),
                "expected number",
            ) else {
                return Some(Located::new(token.span, Pattern::Error));
            };

            return Some(self.maybe_range_pattern(start));
        }

        let int = self.next_if_map(|t| Self::int_pattern(None, t));
        if let Some(int) = int {
            return Some(self.maybe_range_pattern(int));
        }

        let char = self.next_if_map(|t| t.data.as_char().map(|&ch| Located::new(t.span, ch)));
        if let Some(char) = char {
            let Some(range) = self.next_if(|t| matches!(t, Token::Range | Token::RangeInclusive))
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
        if self.next_if_kind(Token::Question).is_some() {
            return self
                .pattern(false)
                .map(|inner| Pattern::Option(inner.into()));
        }

        if let Some(token) = self.next_if_kind(Token::None) {
            return Located::new(token.span, Pattern::Null);
        }

        if let Some(token) = self.next_if_kind(Token::LCurly) {
            return self
                .csv_one(Token::RCurly, token.span, |this| {
                    let mutable = this.next_if_kind(Token::Mut).is_some();
                    // if mut_var, this mutable is redundant
                    let name = this.expect_id_l("expected name");
                    Destructure {
                        name,
                        mutable: mutable || mut_var,
                        pattern: this.next_if_kind(Token::Colon).map(|_| this.pattern(true)),
                    }
                })
                .map(Pattern::StructDestructure);
        }

        if let Some(token) = self.next_if_kind(Token::LBrace) {
            return self
                .csv(Vec::new(), Token::RBrace, token.span, |this| {
                    if let Some(token) = this.next_if_kind(Token::Ellipses) {
                        let pattern = if this.next_if_kind(Token::Mut).is_some() {
                            let ident = this.expect_id("expected name");
                            Some((true, ident))
                        } else {
                            let ident = this.next_if_map(|t| t.data.as_ident().map(|&i| i.into()));
                            Some(false).zip(ident)
                        };

                        Located::new(token.span, Pattern::Rest(pattern))
                    } else {
                        this.pattern(mut_var)
                    }
                })
                .map(Pattern::Array);
        }

        if mut_var || self.next_if_kind(Token::Mut).is_some() {
            return self.expect_id_l("expected name").map(Pattern::MutBinding);
        }

        if !mut_var {
            if let Some(pattern) = self.literal_pattern() {
                return pattern;
            }
        }

        let path = self.type_path();
        if self.next_if_kind(Token::LParen).is_some() {
            self.csv(Vec::new(), Token::RParen, path.span, |this| {
                this.pattern(false)
            })
            .map(|subpatterns| Pattern::TupleLike { path, subpatterns })
        } else {
            path.map(Pattern::Path)
        }
    }

    fn full_pattern(&mut self) -> Located<FullPattern> {
        self.pattern(false).map(|data| FullPattern {
            data,
            if_expr: self
                .next_if_kind(Token::If)
                .map(|_| self.expression().into()),
        })
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
            name: self.expect_id_l("expected name"),
            props: self
                .next_if_kind(Token::LParen)
                .map(|_| {
                    self.csv_one(Token::RParen, Span::default(), Self::attribute)
                        .data
                })
                .unwrap_or_default(),
        }
    }

    //

    fn type_params(&mut self) -> Vec<(String, Vec<Located<TypePath>>)> {
        self.next_if_kind(Token::LAngle)
            .map(|_| {
                self.rangle_csv_one(Span::default(), |this| {
                    (this.expect_id("expected type name"), this.trait_impls())
                })
                .data
            })
            .unwrap_or_default()
    }

    fn trait_impls(&mut self) -> Vec<Located<TypePath>> {
        let mut impls = Vec::new();
        if self.next_if_kind(Token::Colon).is_some() {
            loop {
                impls.push(self.type_path());
                if self.next_if_kind(Token::Plus).is_none() {
                    break;
                }
            }
        }
        impls
    }

    fn type_hint(&mut self) -> TypeHint {
        if self.next_if_kind(Token::Asterisk).is_some() {
            if self.next_if_kind(Token::Mut).is_some() {
                return TypeHint::MutPtr(self.type_hint().into());
            } else {
                return TypeHint::Ptr(self.type_hint().into());
            }
        } else if self.next_if_kind(Token::Question).is_some() {
            return TypeHint::Option(self.type_hint().into());
        } else if self.next_if_kind(Token::NoneCoalesce).is_some() {
            // special case for ??
            return TypeHint::Option(TypeHint::Option(self.type_hint().into()).into());
        }

        if self.next_if_kind(Token::LBrace).is_some() {
            if self.next_if_kind(Token::Mut).is_some() {
                let inner = self.type_hint();
                self.expect_kind(Token::Range);
                self.expect_kind(Token::RBrace);
                TypeHint::SliceMut(inner.into())
            } else {
                let inner = self.type_hint();
                if self.next_if_kind(Token::RBrace).is_some() {
                    TypeHint::Vec(inner.into())
                } else if self.next_if_kind(Token::Range).is_some() {
                    self.expect_kind(Token::RBrace);
                    TypeHint::Slice(inner.into())
                } else if self.next_if_kind(Token::Semicolon).is_some() {
                    let count = self.expression();
                    self.expect_kind(Token::RBrace);
                    TypeHint::Array(inner.into(), count.into())
                } else if self.next_if_kind(Token::Colon).is_some() {
                    let value = self.type_hint();
                    self.expect_kind(Token::RBrace);
                    TypeHint::Map(inner.into(), value.into())
                } else {
                    let span = self.next().span;
                    self.error(Error::new("expected ']', ';', or ':'", span));
                    return TypeHint::Error;
                }
            }
        } else if self.next_if_kind(Token::LCurly).is_some() {
            let inner = self.type_hint().into();
            self.expect_kind(Token::RCurly);
            TypeHint::Set(inner)
        } else if self.next_if_kind(Token::LParen).is_some() {
            TypeHint::Tuple(
                self.csv_one(Token::RParen, Span::default(), Self::type_hint)
                    .data,
            )
        } else if self.next_if_kind(Token::Void).is_some() {
            TypeHint::Void
        } else if let Some(this) = self.next_if_kind(Token::ThisType) {
            TypeHint::Regular(Located::new(
                this.span,
                TypePath::from(THIS_TYPE.to_owned()),
            ))
        } else if self.next_if_kind(Token::Fn).is_some() {
            self.expect_kind(Token::LParen);
            let params = self
                .csv(Vec::new(), Token::RParen, Span::default(), Self::type_hint)
                .data;
            let ret = if self.next_if_kind(Token::FatArrow).is_some() {
                self.type_hint()
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

    fn block(&mut self) -> Located<Vec<Stmt>> {
        let mut stmts = Vec::new();
        let lcurly = self.expect_kind(Token::LCurly);
        let span = self.next_until(Token::RCurly, lcurly.span, |this| {
            stmts.push(this.statement());
        });
        Located::new(span, stmts)
    }

    fn structure(&mut self, public: bool, span: Span) -> StmtData {
        let name = self.expect_id_l("expected name");
        let type_params = self.type_params();

        self.expect_kind(Token::LCurly);

        let mut functions = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();
        self.next_until(Token::RCurly, span, |this| {
            let public = this.next_if_kind(Token::Pub);
            let config = FnConfig {
                allow_method: true,
                is_public: public.is_some(),
                linkage: Linkage::Internal,
                is_unsafe: this.next_if_kind(Token::Unsafe).is_some(),
                body: true,
            };
            if config.is_unsafe {
                if let Ok(func) = this.expect_fn(config) {
                    functions.push(func.data);
                }
            } else if let Some(func) = this.try_function(config) {
                functions.push(func.data);
            } else if let Some(token) = this.next_if_kind(Token::Impl) {
                if let Some(token) = public {
                    this.error_no_sync(Error::not_valid_here(&token));
                }

                impls.push(this.impl_block(token.span));
            } else {
                let name = this.expect_id_l("expected name");
                this.expect_kind(Token::Colon);
                let ty = this.type_hint();
                let value = this.next_if_kind(Token::Assign).map(|_| this.expression());

                if !this.matches_kind(Token::RCurly) {
                    this.expect_kind(Token::Comma);
                }
                members.push(Member {
                    public: config.is_public,
                    ty,
                    name,
                    default: value,
                    shared: false,
                });
            }
        });

        StmtData::Struct(Struct {
            public,
            name,
            type_params,
            members,
            impls,
            functions,
        })
    }

    fn union(&mut self, public: bool, span: Span, is_unsafe: bool) -> StmtData {
        let name = self.expect_id_l("expected name");
        let type_params = self.type_params();
        let tag = self.next_if_kind(Token::Colon).map(|_| self.type_path());
        let mut functions = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();

        self.expect_kind(Token::LCurly);
        self.next_until(Token::RCurly, span, |this| {
            let config = FnConfig {
                allow_method: true,
                linkage: Linkage::Internal,
                is_public: this.next_if_kind(Token::Pub).is_some(),
                is_unsafe: this.next_if_kind(Token::Unsafe).is_some(),
                body: true,
            };
            if config.is_public || config.is_unsafe {
                if let Ok(func) = this.expect_fn(config) {
                    functions.push(func.data);
                }
            } else if let Some(func) = this.try_function(config) {
                functions.push(func.data);
            } else if this.next_if_kind(Token::Shared).is_some() {
                // warn if pub was specified that it is useless
                let name = this.expect_id_l("expected name");
                this.expect_kind(Token::Colon);
                let ty = this.type_hint();
                let value = this.next_if_kind(Token::Assign).map(|_| this.expression());
                this.expect_kind(Token::Comma);

                members.push(Member {
                    public: true,
                    name,
                    shared: true,
                    ty,
                    default: value,
                });
            } else if let Some(token) = this.next_if_kind(Token::Impl) {
                impls.push(this.impl_block(token.span));
            } else {
                let name = this.expect_id_l("expected variant name");
                let (ty, value) = if this.next_if_kind(Token::LParen).is_some() {
                    let ty = this.type_hint();
                    this.expect_kind(Token::RParen);
                    (
                        ty,
                        this.next_if_kind(Token::Assign).map(|_| this.expression()),
                    )
                } else {
                    (TypeHint::Void, None)
                };

                if !this.matches_kind(Token::RCurly) {
                    this.expect_kind(Token::Comma);
                }
                members.push(Member {
                    public: config.is_public,
                    name,
                    shared: false,
                    ty,
                    default: value,
                });
            }
        });

        StmtData::Union {
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
        }
    }

    fn r#trait(&mut self, public: bool, span: Span, is_unsafe: bool) -> StmtData {
        let name = self.expect_id_l("expected name");
        let type_params = self.type_params();
        let impls = self.trait_impls();
        self.expect_kind(Token::LCurly);

        let mut functions = Vec::new();
        self.next_until(Token::RCurly, span, |this| {
            let is_unsafe = this.next_if_kind(Token::Unsafe).is_some();
            if let Ok(proto) = this.expect_fn(FnConfig {
                body: false,
                allow_method: true,
                linkage: Linkage::Internal,
                is_public: true,
                is_unsafe,
            }) {
                functions.push(proto.data);
            }
        });

        StmtData::Trait {
            public,
            is_unsafe,
            name,
            type_params,
            impls,
            functions,
        }
    }

    fn enumeration(&mut self, public: bool, span: Span) -> StmtData {
        let name = self.expect_id_l("expected name");
        let base_ty = self.next_if_kind(Token::Colon).map(|_| self.type_path());
        self.expect_kind(Token::LCurly);

        let mut functions = Vec::new();
        let mut variants = Vec::new();
        let mut impls = Vec::new();
        self.next_until(Token::RCurly, span, |this| {
            let config = FnConfig {
                allow_method: true,
                body: true,
                linkage: Linkage::Internal,
                is_public: this.next_if_kind(Token::Pub).is_some(),
                is_unsafe: this.next_if_kind(Token::Unsafe).is_some(),
            };
            if config.is_public || config.is_unsafe {
                if let Ok(func) = this.expect_fn(config) {
                    functions.push(func.data);
                }
            } else if let Some(func) = this.try_function(config) {
                functions.push(func.data);
            } else if let Some(token) = this.next_if_kind(Token::Impl) {
                impls.push(this.impl_block(token.span));
            } else {
                variants.push((
                    this.expect_id_l("expected variant name"),
                    if this.next_if_kind(Token::Assign).is_some() {
                        Some(this.expression())
                    } else {
                        None
                    },
                ));

                if !this.matches_kind(Token::RCurly) {
                    this.expect_kind(Token::Comma);
                }
            }
        });

        StmtData::Enum {
            public,
            base_ty,
            name,
            impls,
            variants,
            functions,
        }
    }

    fn extension(&mut self, public: bool, span: Span) -> StmtData {
        let type_params = self.type_params();
        let name = self.expect_id("expected name");

        self.expect_kind(Token::For);
        let ty = self.type_hint();
        self.expect_kind(Token::LCurly);

        let mut functions = Vec::new();
        let mut impls = Vec::new();
        self.next_until(Token::RCurly, span, |this| {
            if let Some(token) = this.next_if_kind(Token::Impl) {
                impls.push(this.impl_block(token.span));
            } else {
                let config = FnConfig {
                    allow_method: true,
                    body: true,
                    linkage: Linkage::Internal,
                    is_public: this.next_if_kind(Token::Pub).is_some(),
                    is_unsafe: this.next_if_kind(Token::Unsafe).is_some(),
                };
                if let Ok(func) = this.expect_fn(config) {
                    functions.push(func.data);
                }
            }
        });

        StmtData::Extension {
            public,
            name,
            ty,
            type_params,
            impls,
            functions,
        }
    }

    fn impl_block(&mut self, span: Span) -> ImplBlock {
        let type_params = self.type_params();
        let path = self.type_path();
        self.expect_kind(Token::LCurly);

        let mut functions = Vec::new();
        self.next_until(Token::RCurly, span, |this| {
            if let Some(token) = this.next_if_kind(Token::Pub) {
                this.error_no_sync(Error::not_valid_here(&token));
            }

            let is_unsafe = this.next_if_kind(Token::Unsafe).is_some();
            if let Ok(func) = this.expect_fn(FnConfig {
                allow_method: true,
                is_public: true,
                body: true,
                linkage: Linkage::Internal,
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

    fn try_function(
        &mut self,
        FnConfig {
            allow_method,
            is_public,
            is_unsafe,
            linkage,
            body,
        }: FnConfig,
    ) -> Option<Located<Fn>> {
        let (span, is_async) = if let Some(token) = self.next_if_kind(Token::Fn) {
            (token.span, false)
        } else if let Some(token) = self.next_if_kind(Token::Async) {
            self.expect_kind(Token::Fn);
            (token.span, true)
        } else {
            return None;
        };

        let name = self.expect_id_l("expected name");
        let mut variadic = false;
        let type_params = self.type_params();
        let mut params = Vec::new();
        let mut count = 0;
        let mut has_default = false;
        self.expect_kind(Token::LParen);
        while self
            .next_if(|t| matches!(t, Token::RParen | Token::Eof))
            .is_none()
        {
            if self.next_if_kind(Token::Ellipses).is_some() {
                variadic = true;
                self.expect_kind(Token::RParen);
                break;
            }

            let keyword = self.next_if_kind(Token::Keyword).is_some();
            let mutable = self.next_if_kind(Token::Mut).is_some();
            if let Some(token) = self.next_if_kind(Token::This) {
                if !allow_method || count != 0 {
                    self.error_no_sync(Error::not_valid_here(&token));
                }

                params.push(Param {
                    keyword,
                    patt: Located::new(
                        token.span,
                        Pattern::Path(TypePath::from(THIS_PARAM.to_string())),
                    ),
                    ty: if mutable {
                        TypeHint::MutPtr(TypeHint::This.into())
                    } else {
                        TypeHint::Ptr(TypeHint::This.into())
                    },
                    default: None,
                });
            } else {
                let patt = if mutable {
                    self.expect_id_l("expected name").map(Pattern::MutBinding)
                } else if keyword {
                    self.expect_id_l("expected name")
                        .map(|name| Pattern::Path(TypePath::from(name)))
                } else {
                    self.pattern(false)
                };
                self.expect_kind(Token::Colon);
                let ty = self.type_hint();
                let default = if self.next_if_kind(Token::Assign).is_some() {
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

            if !self.matches_kind(Token::RParen) {
                self.expect_kind(Token::Comma);
            }

            count += 1;
        }

        let ret = if self.next_if_kind(Token::Colon).is_some() {
            self.type_hint()
        } else {
            TypeHint::Void
        };

        let body = if body {
            Some(self.block().data)
        } else {
            self.expect_kind(Token::Semicolon);
            None
        };

        Some(Located::new(
            span,
            Fn {
                name,
                public: is_public,
                linkage,
                is_async,
                is_unsafe,
                variadic,
                type_params,
                params,
                ret,
                body,
            },
        ))
    }

    fn expect_fn(&mut self, params: FnConfig) -> Result<Located<Fn>, ()> {
        loop {
            if let Some(proto) = self.try_function(params) {
                return Ok(proto);
            }

            loop {
                let token = self.next();
                match token.data {
                    Token::Extern | Token::Fn | Token::Async => break,
                    Token::Eof => {
                        self.error_no_sync(Error::new("expected function", token.span));
                        return Err(());
                    }
                    _ => {}
                }
            }
        }
    }

    //

    fn csv<T>(
        &mut self,
        mut res: Vec<T>,
        end: Token,
        span: Span,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> Located<Vec<T>> {
        if !res.is_empty() && !self.matches_kind(end.clone()) {
            self.expect_kind(Token::Comma);
        }

        let span = self.next_until(end.clone(), span, |this| {
            res.push(f(this));

            if !this.matches_kind(end.clone()) {
                this.expect_kind(Token::Comma);
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
            self.expect_kind(Token::Comma);
        }

        loop {
            match self.next_if(|t| matches!(t, Token::Eof | Token::RAngle | Token::Shr)) {
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
            if !self.matches(|t| matches!(t, Token::RAngle | Token::Shr)) {
                self.expect_kind(Token::Comma);
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
        self.diag.error(err);
    }

    //

    fn peek(&mut self) -> &Located<Token<'a>> {
        self.peek.get_or_insert_with(|| self.lexer.token(self.diag))
    }

    fn next(&mut self) -> Located<Token<'a>> {
        self.peek
            .take()
            .unwrap_or_else(|| self.lexer.token(self.diag))
    }

    fn next_if_l(&mut self, f: impl FnOnce(&Located<Token>) -> bool) -> Option<Located<Token<'a>>> {
        f(self.peek()).then(|| self.next())
    }

    fn next_if(&mut self, pred: impl FnOnce(&Token) -> bool) -> Option<Located<Token<'a>>> {
        self.next_if_l(|tok| pred(&tok.data))
    }

    fn next_if_kind(&mut self, kind: Token) -> Option<Located<Token<'a>>> {
        self.next_if(|t| t == &kind)
    }

    fn next_if_map<T>(&mut self, pred: impl FnOnce(&Located<Token>) -> Option<T>) -> Option<T> {
        let mut outer = None;
        self.next_if_l(|t| {
            outer = pred(t);
            outer.is_some()
        });
        outer
    }

    fn next_until(&mut self, token: Token, mut span: Span, mut f: impl FnMut(&mut Self)) -> Span {
        while match self.next_if(|t| t == &token || t == &Token::Eof) {
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

    fn matches(&mut self, pred: impl FnOnce(&Token) -> bool) -> bool {
        pred(&self.peek().data)
    }

    fn matches_kind(&mut self, kind: Token) -> bool {
        self.matches(|t| t == &kind)
    }

    fn expect<T>(
        &mut self,
        pred: impl FnOnce(Located<Token<'a>>) -> Result<T, Located<Token<'a>>>,
        msg: &str,
    ) -> Result<T, Located<Token<'a>>> {
        match pred(self.next()) {
            Ok(t) => Ok(t),
            Err(e) => {
                self.error(Error::new(msg, e.span));
                Err(e)
            }
        }
    }

    fn expect_kind(&mut self, kind: Token) -> Located<Token<'a>> {
        let (Ok(t) | Err(t)) = self.expect(
            |t| {
                if t.data == kind {
                    Ok(t)
                } else {
                    Err(t)
                }
            },
            &format!("expected '{kind}'"),
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

    fn expect_id_l(&mut self, msg: &str) -> Located<String> {
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
}
