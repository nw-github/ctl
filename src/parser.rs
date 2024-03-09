use crate::{
    ast::{parsed::*, Attribute, Attributes, UnaryOp},
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
    body: Option<bool>,
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

    pub fn parse(src: &'a str, diag: &'b mut Diagnostics, file: FileId) -> Stmt {
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
                file: true,
            },
            attrs: Default::default(),
        }
    }

    //

    fn try_item(&mut self) -> Result<Stmt, (Option<Located<Token<'a>>>, Attributes)> {
        let attrs = self.attributes();
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

        let attrs = match self.try_function(
            FnConfig {
                allow_method: false,
                linkage: match (&is_export, &is_import) {
                    (Some(_), None) => Linkage::Export,
                    (None, Some(_)) => Linkage::Import,
                    _ => Linkage::Internal,
                },
                is_public: public.is_some(),
                is_unsafe: is_unsafe.is_some(),
                body: Some(is_import.is_none()),
            },
            attrs,
        ) {
            Ok(func) => {
                return Ok(Stmt {
                    attrs: Default::default(),
                    data: StmtData::Fn(func.data),
                })
            }
            Err(attrs) => attrs,
        };

        if let Some(token) = self.next_if_kind(Token::Struct) {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Ok(Stmt {
                attrs,
                data: StmtData::Struct(self.structure(public.is_some(), token.span)),
            })
        } else if let Some(token) = self.next_if_kind(Token::Union) {
            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Ok(Stmt {
                attrs,
                data: if is_unsafe.is_some() {
                    StmtData::UnsafeUnion(self.structure(public.is_some(), token.span))
                } else {
                    self.union(public.is_some(), token.span)
                },
            })
        } else if let Some(token) = self.next_if_kind(Token::Trait) {
            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            Ok(Stmt {
                attrs,
                data: self.r#trait(public.is_some(), token.span, is_unsafe.is_some()),
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
                    file: false,
                    name,
                    body,
                },
                attrs,
            })
        } else if self.next_if_kind(Token::Use).is_some() {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            let mut components = Vec::new();
            let origin = if self.next_if_kind(Token::ScopeRes).is_some() {
                let ident = self.expect_id_l("expected path component");
                if self.next_if_kind(Token::ScopeRes).is_none() {
                    self.expect_kind(Token::Semicolon);
                    return Ok(Stmt {
                        data: StmtData::Use(UsePath {
                            components,
                            origin: PathOrigin::Root,
                            public: public.is_some(),
                            tail: UsePathTail::Ident(ident),
                        }),
                        attrs,
                    });
                }

                components.push(ident);
                PathOrigin::Root
            } else if let Some(token) = self.next_if_kind(Token::Super) {
                self.expect_kind(Token::ScopeRes);
                PathOrigin::Super(token.span)
            } else {
                let ident = self.expect_id_l("expected path component");
                if self.next_if_kind(Token::ScopeRes).is_none() {
                    self.expect_kind(Token::Semicolon);
                    return Ok(Stmt {
                        data: StmtData::Use(UsePath {
                            components,
                            origin: PathOrigin::Normal,
                            public: public.is_some(),
                            tail: UsePathTail::Ident(ident),
                        }),
                        attrs,
                    });
                }

                components.push(ident);
                PathOrigin::Normal
            };

            loop {
                if self.next_if_kind(Token::Asterisk).is_some() {
                    self.expect_kind(Token::Semicolon);
                    return Ok(Stmt {
                        data: StmtData::Use(UsePath {
                            components,
                            origin,
                            public: public.is_some(),
                            tail: UsePathTail::All,
                        }),
                        attrs,
                    });
                }

                let ident = self.expect_id_l("expected path component or '*'");
                if self.next_if_kind(Token::ScopeRes).is_none() {
                    self.expect_kind(Token::Semicolon);
                    return Ok(Stmt {
                        data: StmtData::Use(UsePath {
                            components,
                            origin,
                            public: public.is_some(),
                            tail: UsePathTail::Ident(ident),
                        }),
                        attrs,
                    });
                }

                components.push(ident);
            }
        } else if self.next_if_kind(Token::Static).is_some() {
            if let Some(token) = is_unsafe {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            if let Some(token) = is_import.or(is_export) {
                self.error_no_sync(Error::not_valid_here(&token));
            }

            let name = self.expect_id_l("expected name");
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
                let data = self.path_components(Some(Located::new(span, ident.into())), &mut span);
                Expr::new(span, ExprData::Path(Path::new(PathOrigin::Normal, data)))
            }
            Token::This => Expr::new(
                span,
                ExprData::Path(Located::new(span, THIS_PARAM.to_owned()).into()),
            ),
            Token::ThisType => Expr::new(
                span,
                ExprData::Path(Located::new(span, THIS_TYPE.to_owned()).into()),
            ),
            Token::ScopeRes => {
                let ident = self.expect_id_l("expected name");
                let data = self.path_components(Some(ident), &mut span);
                Expr::new(span, ExprData::Path(Path::new(PathOrigin::Root, data)))
            }
            Token::Super => {
                let original = span;
                let data = self.path_components(None, &mut span);
                Expr::new(
                    span,
                    ExprData::Path(Path::new(PathOrigin::Super(original), data)),
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
                    let expr = self.expression();
                    span.extend_to(expr.span);
                    parts.push(part.map(ExprData::String));
                    parts.push(expr);
                    // TODO: ':' format options
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
            | Token::Exclamation => {
                let op = if data == Token::Ampersand && self.next_if_kind(Token::Mut).is_some() {
                    UnaryOp::AddrMut
                } else if data == Token::Ampersand && self.next_if_kind(Token::Raw).is_some() {
                    UnaryOp::AddrRaw
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
                } else if self.next_if_kind(Token::Raw).is_some() {
                    UnaryOp::AddrRaw
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
            Token::Move => {
                let token = self.next();
                if !matches!(token.data, Token::Or | Token::LogicalOr) {
                    self.error_no_sync(Error::new("expected '|'", token.span));
                }

                self.lambda_expr(token, true)
            }
            Token::Or => self.lambda_expr(Located::new(span, data), false),
            Token::LogicalOr => self.lambda_expr(Located::new(span, data), false),
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
            | Token::NotEqual
            | Token::Spaceship
            | Token::Assign
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

                let member = self.expect_id_l("expected member name");
                let generics = if self.next_if_kind(Token::ScopeRes).is_some() {
                    self.expect_kind(Token::LAngle);
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

    fn lambda_expr(&mut self, head: Located<Token>, moves: bool) -> Expr {
        let params = if head.data == Token::Or {
            self.csv(Vec::new(), Token::Or, head.span, |this| {
                (
                    this.expect_id_l("expected parameter name"),
                    this.next_if_kind(Token::Colon).map(|_| this.type_hint()),
                )
            })
            .data
        } else {
            Vec::new()
        };

        let ret = self.next_if_kind(Token::Colon).map(|_| self.type_hint());
        let body = if ret.is_none() {
            self.expression()
        } else {
            let token = self.expect_kind(Token::LCurly);
            self.block_expr(token.span)
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
        while self.next_if_kind(Token::ScopeRes).is_some() {
            if self.next_if_kind(Token::LAngle).is_some() {
                let params = self.rangle_csv_one(*outspan, Self::type_hint);
                data.last_mut().unwrap().1 = params.data;
                *outspan = params.span;
            } else {
                let name = self.expect_id_l("expected name");
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
                self.expect_kind(Token::ScopeRes);
                PathOrigin::Super(span)
            }
            _ => PathOrigin::Normal,
        };
        let mut data = Vec::new();
        loop {
            let ident = self.expect_id_l("expected type name");
            if self.next_if_kind(Token::LAngle).is_some() {
                let params = self.rangle_csv_one(ident.span, Self::type_hint);
                data.push((ident, params.data));
            } else {
                data.push((ident, Vec::new()));
            }

            if self.next_if_kind(Token::ScopeRes).is_none() {
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
        if let Some(token) = self.next_if_kind(Token::True) {
            return Some(token.map(|_| Pattern::Bool(true)));
        } else if let Some(token) = self.next_if_kind(Token::False) {
            return Some(token.map(|_| Pattern::Bool(false)));
        } else if let Some(token) = self.next_if_kind(Token::Void) {
            return Some(token.map(|_| Pattern::Void));
        }

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

    fn struct_like(&mut self, span: Span, mut_var: bool) -> Located<Vec<Destructure>> {
        self.csv_one(Token::RCurly, span, |this| {
            let mutable = this.next_if_kind(Token::Mut);
            if let Some(token) = mutable.clone().filter(|_| mut_var) {
                this.diag.warn(Error::new(
                    format!("redundant '{}'", Token::Mut),
                    token.span,
                ));
            }
            let mutable = mutable.is_some();
            let name = this.expect_id_l("expected name");
            if mutable || this.next_if_kind(Token::Colon).is_none() {
                let span = name.span;
                Destructure {
                    name: name.clone(),
                    mutable,
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

    fn pattern(&mut self, mut_var: bool) -> Located<Pattern> {
        match self.peek().data {
            Token::Question => {
                self.next();
                self.pattern(false)
                    .map(|inner| Pattern::Option(inner.into()))
            }
            Token::None => Located::new(self.next().span, Pattern::Null),
            Token::LParen => {
                let span = self.next().span;
                self.tuple_like(span, mut_var).map(Pattern::Tuple)
            }
            Token::LCurly => {
                let span = self.next().span;
                self.struct_like(span, mut_var).map(Pattern::Struct)
            }
            Token::LBrace => {
                let span = self.next().span;
                self.csv(Vec::new(), Token::RBrace, span, |this| {
                    if let Some(token) = this.next_if_kind(Token::Ellipses) {
                        let pattern = if this.next_if_kind(Token::Mut).is_some() {
                            let ident = this.expect_id_l("expected name");
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
                .map(Pattern::Array)
            }
            _ => {
                if mut_var || self.next_if_kind(Token::Mut).is_some() {
                    return self.expect_id_l("expected name").map(Pattern::MutBinding);
                }
                if !mut_var {
                    if let Some(pattern) = self.literal_pattern() {
                        return pattern;
                    }
                }

                let path = self.type_path();
                match self.peek().data {
                    Token::LParen => {
                        let span = self.next().span;
                        self.tuple_like(span, mut_var)
                            .map(|subpatterns| Pattern::TupleLike { path, subpatterns })
                    }
                    Token::LCurly => {
                        let span = self.next().span;
                        self.struct_like(span, mut_var)
                            .map(|subpatterns| Pattern::StructLike { path, subpatterns })
                    }
                    _ => Located::new(path.span(), Pattern::Path(path)),
                }
            }
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
                .map(|tk| self.csv_one(Token::RParen, tk.span, Self::attribute).data)
                .unwrap_or_default(),
        }
    }

    fn attributes(&mut self) -> Attributes {
        let mut attrs = vec![];
        while let Some(token) = self.next_if_kind(Token::HashLParen) {
            let attr = self.csv_one(Token::RParen, token.span, Self::attribute);
            attrs.extend(attr.data);
        }
        Attributes::new(attrs)
    }

    //

    fn type_params(&mut self) -> TypeParams {
        self.next_if_kind(Token::LAngle)
            .map(|tk| {
                self.rangle_csv_one(tk.span, |this| {
                    (this.expect_id_l("expected type name"), this.trait_impls())
                })
                .data
            })
            .unwrap_or_default()
    }

    fn trait_impls(&mut self) -> Vec<Path> {
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
        match self.peek().data {
            Token::Asterisk => {
                self.next();
                if self.next_if_kind(Token::Mut).is_some() {
                    TypeHint::MutPtr(self.type_hint().into())
                } else if self.next_if_kind(Token::Raw).is_some() {
                    TypeHint::RawPtr(self.type_hint().into())
                } else if self.next_if_kind(Token::Dyn).is_some() {
                    if self.next_if_kind(Token::Mut).is_some() {
                        TypeHint::DynMutPtr(self.type_path())
                    } else {
                        TypeHint::DynPtr(self.type_path())
                    }
                } else {
                    TypeHint::Ptr(self.type_hint().into())
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
                        TypeHint::Error
                    }
                }
            }
            Token::LCurly => {
                self.next();
                let inner = self.type_hint().into();
                self.expect_kind(Token::RCurly);
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
                let left = self.expect_kind(Token::LParen);
                let params = self
                    .csv(Vec::new(), Token::RParen, left.span, Self::type_hint)
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
            }
            Token::Struct => {
                let span = self.next().span;
                self.expect_kind(Token::LCurly);
                TypeHint::AnonStruct(
                    self.csv(Vec::new(), Token::RCurly, span, |this| {
                        let name = this.expect_id("expected member name");
                        this.expect_kind(Token::Colon);
                        let ty = this.type_hint();
                        (name, ty)
                    })
                    .data,
                )
            }
            _ => TypeHint::Regular(self.type_path()),
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

    fn structure(&mut self, public: bool, span: Span) -> Struct {
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
                body: Some(true),
            };
            if config.is_unsafe {
                if let Ok(func) = this.expect_fn(config, Default::default()) {
                    functions.push(func.data);
                }
            } else if let Ok(func) = this.try_function(config, Default::default()) {
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
                });
            }
        });

        Struct {
            public,
            name,
            type_params,
            members,
            impls,
            functions,
        }
    }

    fn union(&mut self, public: bool, span: Span) -> StmtData {
        let name = self.expect_id_l("expected name");
        let type_params = self.type_params();
        let tag = self.next_if_kind(Token::Colon).map(|_| self.type_path());
        let mut functions = Vec::new();
        let mut members = Vec::new();
        let mut impls = Vec::new();
        let mut variants = Vec::new();

        self.expect_kind(Token::LCurly);
        self.next_until(Token::RCurly, span, |this| {
            let config = FnConfig {
                allow_method: true,
                linkage: Linkage::Internal,
                is_public: this.next_if_kind(Token::Pub).is_some(),
                is_unsafe: this.next_if_kind(Token::Unsafe).is_some(),
                body: Some(true),
            };
            if config.is_public || config.is_unsafe {
                if let Ok(func) = this.expect_fn(config, Default::default()) {
                    functions.push(func.data);
                }
            } else if let Ok(func) = this.try_function(config, Default::default()) {
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
                    ty,
                    default: value,
                });
            } else if let Some(token) = this.next_if_kind(Token::Impl) {
                impls.push(this.impl_block(token.span));
            } else {
                let name = this.expect_id_l("expected variant name");
                let data = match this.peek().data {
                    Token::LParen => {
                        let span = this.next().span;
                        VariantData::TupleLike(
                            this.csv_one(Token::RParen, span, |this| {
                                (
                                    this.type_hint(),
                                    this.next_if_kind(Token::Assign).map(|_| this.expression()),
                                )
                            })
                            .data,
                        )
                    }
                    Token::LCurly => {
                        let span = this.next().span;
                        VariantData::StructLike(
                            this.csv_one(Token::RCurly, span, |this| {
                                if let Some(token) = this.next_if_kind(Token::Pub) {
                                    this.error_no_sync(Error::not_valid_here(&token));
                                }

                                let name = this.expect_id_l("expected name");
                                this.expect_kind(Token::Colon);
                                Member {
                                    public: true,
                                    name,
                                    ty: this.type_hint(),
                                    default: this
                                        .next_if_kind(Token::Assign)
                                        .map(|_| this.expression()),
                                }
                            })
                            .data,
                        )
                    }
                    _ => VariantData::Empty,
                };

                if !this.matches_kind(Token::RCurly) {
                    this.expect_kind(Token::Comma);
                }
                variants.push(Variant { name, data });
            }
        });

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
            },
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
            if let Ok(proto) = this.expect_fn(
                FnConfig {
                    body: None,
                    allow_method: true,
                    linkage: Linkage::Internal,
                    is_public: true,
                    is_unsafe,
                },
                Default::default(),
            ) {
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

    fn extension(&mut self, public: bool, span: Span) -> StmtData {
        let type_params = self.type_params();
        let name = self.expect_id_l("expected name");

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
                    body: Some(true),
                    linkage: Linkage::Internal,
                    is_public: this.next_if_kind(Token::Pub).is_some(),
                    is_unsafe: this.next_if_kind(Token::Unsafe).is_some(),
                };
                if let Ok(func) = this.expect_fn(config, Default::default()) {
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
            let attrs = this.attributes();
            if let Some(token) = this.next_if_kind(Token::Pub) {
                this.error_no_sync(Error::not_valid_here(&token));
            }

            let is_unsafe = this.next_if_kind(Token::Unsafe).is_some();
            if let Ok(func) = this.expect_fn(
                FnConfig {
                    allow_method: true,
                    is_public: true,
                    body: Some(true),
                    linkage: Linkage::Internal,
                    is_unsafe,
                },
                attrs,
            ) {
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
        attrs: Attributes,
    ) -> Result<Located<Fn>, Attributes> {
        let (span, is_async) = if let Some(token) = self.next_if_kind(Token::Fn) {
            (token.span, false)
        } else if let Some(token) = self.next_if_kind(Token::Async) {
            self.expect_kind(Token::Fn);
            (token.span, true)
        } else {
            return Err(attrs);
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
                        Pattern::Path(Path::from(Located::new(token.span, THIS_PARAM.to_string()))),
                    ),
                    ty: if mutable {
                        TypeHint::MutPtr(TypeHint::This(token.span).into())
                    } else {
                        TypeHint::Ptr(TypeHint::This(token.span).into())
                    },
                    default: None,
                });
            } else {
                let patt = if mutable {
                    self.expect_id_l("expected name").map(Pattern::MutBinding)
                } else if keyword {
                    let t = self.expect_id_l("expected name");
                    let span = t.span;
                    t.map(|name| Pattern::Path(Path::from(Located::new(span, name))))
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

        let body = match body {
            Some(true) => Some(self.block().data),
            Some(false) => {
                self.expect_kind(Token::Semicolon);
                None
            }
            None => {
                if self.next_if_kind(Token::Semicolon).is_some() {
                    None
                } else {
                    Some(self.block().data)
                }
            }
        };

        Ok(Located::new(
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
                attrs,
            },
        ))
    }

    fn expect_fn(&mut self, params: FnConfig, attrs: Attributes) -> Result<Located<Fn>, ()> {
        match self.try_function(params, attrs) {
            Ok(proto) => Ok(proto),
            Err(_) => {
                let span = self.next().span;
                self.error(Error::new("expected function", span));
                Err(())
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
        self.diag.error(err);
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
