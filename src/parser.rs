use std::iter::Peekable;

use crate::{
    ast::{
        expr::{Expr, UnaryOp},
        stmt::{self, Fn, Param, Prototype, Stmt, TypeHint, UserType},
        Path,
    },
    lexer::{Lexer, Located as L, Location, Span, Token},
    Error, Result, THIS_PARAM, THIS_TYPE,
};

macro_rules! binary {
    ($name: ident, $patt: pat, $next: ident) => {
        fn $name(&mut self) -> Result<L<Expr>> {
            let mut expr = self.$next()?;
            while let Some(op) = self.advance_if(|kind| matches!(kind, $patt)) {
                let right = self.$next()?;
                let span = Span::combine(expr.span, right.span);
                expr = L::new(
                    Expr::Binary {
                        op: op.data.try_into().unwrap(),
                        left: expr.into(),
                        right: right.into(),
                    },
                    span,
                );
            }

            Ok(expr)
        }
    };
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    src: &'a str,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            lexer: Lexer::new(src).peekable(),
            src,
        }
    }

    pub fn parse(src: &'a str) -> (L<Stmt>, Vec<Error>) {
        let mut this = Self::new(src);
        let mut stmts = Vec::new();
        let mut errors = Vec::new();
        while this.lexer.peek().is_some() {
            match this.item() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    errors.push(err);
                    this.synchronize();
                }
            }
        }

        (
            L::new(
                Stmt::Module {
                    public: true,
                    name: "tmpname".into(),
                    body: stmts,
                },
                Span {
                    loc: Location {
                        row: 1,
                        col: 1,
                        pos: 0,
                    },
                    len: src.len(),
                },
            ),
            errors,
        )
    }

    fn synchronize(&mut self) {
        use Token::*;
        loop {
            match self.lexer.peek() {
                Some(Ok(token)) if token.data == Semicolon => {
                    let _ = self.advance();
                    break;
                }
                Some(Ok(token))
                    if matches!(
                        token.data,
                        Pub | Struct | Enum | Union | Interface | Fn | Let | Loop | If | Return
                    ) =>
                {
                    break
                }
                Option::None => break,
                _ => {
                    let _ = self.advance();
                }
            }
        }
    }

    fn error<T>(&mut self, diagnostic: &str) -> Result<T> {
        Err(Error::new(diagnostic, self.advance()?.span))
    }

    // helpers

    fn path_with_one(&mut self, root: bool, mut ident: (&'a str, Span)) -> Result<L<Path>> {
        let mut data = vec![(ident.0.to_owned(), self.parse_generic_params()?)];
        while self.advance_if_kind(Token::ScopeRes).is_some() {
            let (id, span) = self.expect_id_with_span("expected name")?;
            data.push((id.to_owned(), self.parse_generic_params()?));
            ident.1.extend_to(span);
        }

        Ok(L::new(Path { data, root }, ident.1))
    }

    fn path(&mut self) -> Result<L<Path>> {
        let root = self.advance_if_kind(Token::ScopeRes).is_some();
        let ident = self.expect_id_with_span("expected name")?;
        self.path_with_one(root, ident)
    }

    fn comma_separated<T>(
        &mut self,
        end: Token,
        msg: &str,
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<(Vec<T>, Span)> {
        let mut v = Vec::new();
        loop {
            v.push(f(self)?);
            if self.advance_if_kind(Token::Comma).is_none() {
                break;
            }
        }

        let tok = self.expect_kind(end, msg)?;
        Ok((v, tok.span))
    }

    fn try_prototype(
        &mut self,
        is_public: bool,
        allow_method: bool,
    ) -> Option<Result<(Prototype, Span)>> {
        if let Some(token) = self.advance_if_kind(Token::Extern) {
            Some((|| {
                self.expect_kind(Token::Fn, "expected 'fn'")?;
                self.prototype(allow_method, is_public, false, true)
                    .map(|r| (r, token.span))
            })())
        } else if let Some(token) = self.advance_if_kind(Token::Fn) {
            Some(
                self.prototype(allow_method, is_public, false, false)
                    .map(|r| (r, token.span)),
            )
        } else {
            self.advance_if_kind(Token::Async).map(|token| {
                (|| {
                    self.expect_kind(Token::Fn, "expected 'fn'")?;
                    self.prototype(allow_method, is_public, true, false)
                        .map(|r| (r, token.span))
                })()
            })
        }
    }

    fn parse_generic_params(&mut self) -> Result<Vec<String>> {
        if self.advance_if_kind(Token::LAngle).is_some() {
            self.comma_separated(Token::RAngle, "expected '>'", |this| {
                this.expect_id("expected type name").map(|id| id.into())
            })
            .map(|t| t.0)
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_interface_impl(&mut self) -> Result<Vec<L<Path>>> {
        let mut impls = Vec::new();
        if self.advance_if_kind(Token::Colon).is_some() {
            loop {
                impls.push(self.path()?);
                if self.advance_if_kind(Token::Plus).is_none() {
                    break;
                }
            }
        }

        Ok(impls)
    }

    fn parse_type(&mut self) -> Result<TypeHint> {
        if self.advance_if_kind(Token::Asterisk).is_some() {
            if self.advance_if_kind(Token::Mut).is_some() {
                return Ok(TypeHint::RefMut(self.parse_type()?.into()));
            } else {
                return Ok(TypeHint::Ref(self.parse_type()?.into()));
            }
        } else if self.advance_if_kind(Token::Question).is_some() {
            return Ok(TypeHint::Option(self.parse_type()?.into()));
        } else if self.advance_if_kind(Token::NoneCoalesce).is_some() {
            // special case for ??
            return Ok(TypeHint::Option(
                TypeHint::Option(self.parse_type()?.into()).into(),
            ));
        }

        let ty = if self.advance_if_kind(Token::LBrace).is_some() {
            let inner = self.parse_type()?;
            if self.advance_if_kind(Token::RBrace).is_some() {
                TypeHint::Slice(inner.into())
            } else if self.advance_if_kind(Token::Semicolon).is_some() {
                //let count = self.expression()?;
                let count = self.expect(
                    |t| {
                        let Token::Int { base: 10, value, width: None } = t.data else { return None; };
                        Some(value)
                    },
                    "expected array size",
                )?;

                self.expect_kind(Token::RBrace, "expected ']'")?;
                TypeHint::Array(inner.into(), count.parse().unwrap())
            } else if self.advance_if_kind(Token::Colon).is_some() {
                let value = self.parse_type()?;
                self.expect_kind(Token::RBrace, "expected ']'")?;
                TypeHint::Map(inner.into(), value.into())
            } else {
                return self.error("expected ']', ';', or ':'");
            }
        } else if self.advance_if_kind(Token::LParen).is_some() {
            TypeHint::Tuple(
                self.comma_separated(Token::RParen, "expected ')'", Self::parse_type)?
                    .0,
            )
        } else if self.advance_if_kind(Token::Void).is_some() {
            TypeHint::Void
        } else {
            let is_dyn = self.advance_if_kind(Token::Dyn).is_some();
            if let Some(this) = self.advance_if_kind(Token::ThisType) {
                TypeHint::Regular {
                    path: L::new(Path::from(THIS_TYPE.to_owned()), this.span),
                    is_dyn,
                }
            } else {
                TypeHint::Regular {
                    path: self.path()?,
                    is_dyn,
                }
            }
        };

        if self.advance_if_kind(Token::Exclamation).is_some() {
            Ok(TypeHint::Result(ty.into(), self.parse_type()?.into()))
        } else {
            Ok(ty)
        }
    }

    fn parse_var_name(&mut self) -> Result<(String, Option<TypeHint>)> {
        let name = self.expect_id("expected name")?;
        let ty = if self.advance_if_kind(Token::Colon).is_some() {
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok((name.into(), ty))
    }

    fn parse_block(&mut self, span: Span) -> Result<(Vec<L<Stmt>>, Span)> {
        let mut stmts = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            stmts.push(this.declaration()?);
            Ok(())
        })?;

        Ok((stmts, span))
    }

    fn parse_struct_body(&mut self, span: &mut Span) -> Result<stmt::Struct> {
        let type_params = self.parse_generic_params()?;
        let impls = self.parse_interface_impl()?;

        self.expect_kind(Token::LCurly, "expected '{'")?;

        let mut functions = Vec::new();
        let mut members = Vec::new();
        *span = self.advance_until(Token::RCurly, *span, |this| {
            let public = this.advance_if_kind(Token::Pub).is_some();
            if let Some(header) = this.try_prototype(public, true) {
                let (header, _) = header?;
                let lcurly = this.expect_kind(Token::LCurly, "expected '{'")?;
                let (body, _) = this.parse_block(lcurly.span)?;

                functions.push(Fn {
                    proto: header,
                    body,
                });
            } else {
                let (name, ty) = this.parse_var_name()?;
                let ty = match ty {
                    Some(ty) => ty,
                    None => return this.error("expected type"),
                };

                let value = if this.advance_if_kind(Token::Assign).is_some() {
                    Some(this.expression()?)
                } else {
                    None
                };
                this.expect_kind(Token::Comma, "expected ','")?;

                members.push((
                    name,
                    stmt::MemVar {
                        public,
                        ty,
                        default: value,
                    },
                ));
            }

            Ok(())
        })?;

        Ok(stmt::Struct {
            public: false,
            name: String::new(),
            type_params,
            members,
            impls,
            functions,
        })
    }

    fn prototype(
        &mut self,
        allow_method: bool,
        is_public: bool,
        is_async: bool,
        is_extern: bool,
    ) -> Result<Prototype> {
        let name = self.expect_id("expected name")?;
        let type_params = self.parse_generic_params()?;
        self.expect_kind(Token::LParen, "expected parameter list")?;
        let params = if self.advance_if_kind(Token::RParen).is_none() {
            let mut count = 0;
            self.comma_separated(Token::RParen, "expected ')'", |this| {
                count += 1;

                let keyword = this.advance_if_kind(Token::Keyword).is_some();
                let mutable = this.advance_if_kind(Token::Mut).is_some();
                let mut has_default = false;
                if allow_method && count == 1 && this.advance_if_kind(Token::This).is_some() {
                    Ok(Param {
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
                    let name = this.expect_id("expected name")?.into();
                    this.expect_kind(Token::Colon, "expected type")?;
                    let ty = this.parse_type()?;
                    let default = if this.advance_if_kind(Token::Assign).is_some() {
                        has_default = true;
                        Some(this.expression()?)
                    } else {
                        if !keyword && has_default {
                            todo!("positional parameters must not follow a default parameter")
                        }

                        None
                    };

                    Ok(Param {
                        mutable,
                        keyword,
                        name,
                        ty,
                        default,
                    })
                }
            })?
            .0
        } else {
            Vec::new()
        };

        Ok(Prototype {
            name: name.into(),
            public: is_public,
            is_async,
            is_extern,
            type_params,
            params,
            ret: if !self.matches(|kind| matches!(kind, Token::Semicolon | Token::LCurly)) {
                self.parse_type()?
            } else {
                TypeHint::Void
            },
        })
    }

    fn expect_prototype(
        &mut self,
        is_public: bool,
        allow_method: bool,
    ) -> Result<(Prototype, Span)> {
        match self.try_prototype(is_public, allow_method) {
            Some(f) => Ok(f?),
            None => self.error("expected function"),
        }
    }

    //

    fn try_item(&mut self) -> Option<Result<L<Stmt>>> {
        let public = self.advance_if_kind(Token::Pub);
        if let Some(header) = self.try_prototype(public.is_some(), false) {
            Some((|| {
                let (header, span) = header?;
                if header.is_extern {
                    let semi = self.expect_kind(Token::Semicolon, "expected ';'")?;
                    Ok(L::new(
                        Stmt::Fn(Fn {
                            proto: header,
                            body: Vec::new(),
                        }),
                        Span::combine(public.map_or(span, |p| p.span), semi.span),
                    ))
                } else {
                    let lcurly = self.expect_kind(Token::LCurly, "expected '{'")?;
                    let (body, body_span) = self.parse_block(lcurly.span)?;

                    Ok(L::new(
                        Stmt::Fn(Fn {
                            proto: header,
                            body,
                        }),
                        Span::combine(public.map_or(span, |p| p.span), body_span),
                    ))
                }
            })())
        } else if let Some(mut token) = self.advance_if_kind(Token::Struct) {
            Some((|| {
                Ok(L::new(
                    Stmt::UserType(UserType::Struct(stmt::Struct {
                        name: self.expect_id("expected name")?.into(),
                        public: public.is_some(),
                        ..self.parse_struct_body(&mut token.span)?
                    })),
                    token.span,
                ))
            })())
        } else if let Some(mut token) = self.advance_if_kind(Token::Union) {
            Some((|| {
                let tag = if self.advance_if_kind(Token::LParen).is_some() {
                    let tag = self.path()?;
                    self.expect_kind(Token::RParen, "expected ')'")?;
                    Some(tag)
                } else {
                    None
                };

                Ok(L::new(
                    Stmt::UserType(UserType::Union {
                        tag,
                        base: stmt::Struct {
                            name: self.expect_id("expected name")?.into(),
                            public: public.is_some(),
                            ..self.parse_struct_body(&mut token.span)?
                        },
                    }),
                    token.span,
                ))
            })())
        } else if let Some(token) = self.advance_if_kind(Token::Interface) {
            Some((|| {
                let name = self.expect_id("expected name")?.into();
                let type_params = self.parse_generic_params()?;
                let impls = self.parse_interface_impl()?;
                self.expect_kind(Token::LCurly, "expected '{'")?;

                let mut functions = Vec::new();
                let span = self.advance_until(Token::RCurly, token.span, |this| {
                    let header = this.expect_prototype(true, true)?;
                    this.expect_kind(Token::Semicolon, "expected ';'")?;

                    functions.push(header.0);
                    Ok(())
                })?;

                Ok(L::new(
                    Stmt::UserType(UserType::Interface {
                        public: public.is_some(),
                        name,
                        type_params,
                        impls,
                        functions,
                    }),
                    span,
                ))
            })())
        } else if let Some(token) = self.advance_if_kind(Token::Enum) {
            Some((|| {
                let name = self.expect_id("expected name")?.into();
                let impls = self.parse_interface_impl()?;
                self.expect_kind(Token::LCurly, "expected '{'")?;

                let mut functions = Vec::new();
                let mut variants = Vec::new();
                let span = self.advance_until(Token::RCurly, token.span, |this| {
                    if this.advance_if_kind(Token::Pub).is_some() {
                        let header = match this.try_prototype(true, true) {
                            Some(header) => header,
                            None => {
                                return this.error("expected function");
                            }
                        };
                        let (header, _) = header?;
                        let lcurly = this.expect_kind(Token::LCurly, "expected '{'")?;
                        let (body, _) = this.parse_block(lcurly.span)?;
                        functions.push(Fn {
                            proto: header,
                            body,
                        });
                    } else if let Some(header) = this.try_prototype(false, true) {
                        let (header, _) = header?;
                        let lcurly = this.expect_kind(Token::LCurly, "expected '{'")?;
                        let (body, _) = this.parse_block(lcurly.span)?;
                        functions.push(Fn {
                            proto: header,
                            body,
                        });
                    } else {
                        variants.push((
                            this.expect_id("expected variant name")?.into(),
                            if this.advance_if_kind(Token::Assign).is_some() {
                                Some(this.expression()?)
                            } else {
                                None
                            },
                        ));

                        this.expect_kind(Token::Comma, "expected ','")?;
                    }

                    Ok(())
                })?;

                Ok(L::new(
                    Stmt::UserType(UserType::Enum {
                        public: public.is_some(),
                        name,
                        impls,
                        variants,
                        functions,
                    }),
                    span,
                ))
            })())
        } else if let Some(token) = self.advance_if_kind(Token::Mod) {
            Some((|| {
                let name = self.expect_id("expected name")?.into();
                let mut body = Vec::new();
                self.expect_kind(Token::LCurly, "expected '{'")?;
                let span = self.advance_until(Token::RCurly, token.span, |this| {
                    body.push(this.item()?);
                    Ok(())
                })?;

                Ok(L::new(
                    Stmt::Module {
                        public: public.is_some(),
                        name,
                        body,
                    },
                    span,
                ))
            })())
        } else {
            self.advance_if_kind(Token::Static).map(|token| {
                (|| {
                    let (name, ty) = self.parse_var_name()?;
                    self.expect_kind(
                        Token::Assign,
                        "expected '=': static variables must be initialized",
                    )?;
                    let value = self.expression()?;
                    let semi = self.expect_kind(Token::Semicolon, "expected ';'")?;

                    Ok(L::new(
                        Stmt::Static {
                            public: public.is_some(),
                            name,
                            ty,
                            value,
                        },
                        Span::combine(token.span, semi.span),
                    ))
                })()
            })
        }
    }

    fn item(&mut self) -> Result<L<Stmt>> {
        self.try_item()
            .unwrap_or_else(|| self.error("expected item"))
    }

    fn declaration(&mut self) -> Result<L<Stmt>> {
        if let Some(item) = self.try_item() {
            item
        } else if let Some(token) = self.advance_if(|t| matches!(t, Token::Let | Token::Mut)) {
            let mutable = matches!(token.data, Token::Mut);
            let (name, ty) = self.parse_var_name()?;

            let value = if self.advance_if_kind(Token::Assign).is_some() {
                Some(self.expression()?)
            } else {
                None
            };
            let semi = self.expect_kind(Token::Semicolon, "expected ';'")?;

            Ok(L::new(
                Stmt::Let {
                    name,
                    ty,
                    mutable,
                    value,
                },
                Span::combine(token.span, semi.span),
            ))
        } else {
            self.statement()
        }
    }

    fn statement(&mut self) -> Result<L<Stmt>> {
        let expr = self.expression()?;
        let mut span = expr.span;
        if !(matches!(
            expr.data,
            Expr::If { .. } | Expr::For { .. } | Expr::Block(_)
        ) || matches!(expr.data, Expr::Loop { do_while, .. } if !do_while ))
        {
            span.extend_to(self.expect_kind(Token::Semicolon, "expected ';'")?.span);
        }

        Ok(L::new(Stmt::Expr(expr), span))
    }

    //

    fn expression(&mut self) -> Result<L<Expr>> {
        self.jump()
    }

    fn jump(&mut self) -> Result<L<Expr>> {
        if let Some(token) =
            self.advance_if(|tk| matches!(tk, Token::Return | Token::Break | Token::Yield))
        {
            let (expr, span) = if !self
                .matches(|tk| matches!(tk, Token::Semicolon | Token::Comma | Token::RBrace))
            {
                let expr = self.assignment()?;
                let span = Span::combine(token.span, expr.span);
                (expr.into(), span)
            } else {
                (L::new(Expr::Void, token.span).into(), token.span)
            };

            Ok(L::new(
                match token.data {
                    Token::Return => Expr::Return(expr),
                    Token::Break => Expr::Break(expr),
                    Token::Yield => Expr::Yield(expr),
                    _ => unreachable!(),
                },
                span,
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Continue) {
            Ok(L::new(Expr::Continue, token.span))
        } else {
            self.assignment()
        }
    }

    fn assignment(&mut self) -> Result<L<Expr>> {
        let expr = self.range()?;
        if let Some(assign) = self.advance_if(|k| k.is_assignment()) {
            match expr.data {
                Expr::Path(_) | Expr::Subscript { .. } | Expr::Member { .. } => {}
                Expr::Unary { op, .. } if op == UnaryOp::Deref => {}
                _ => return Err(Error::new("invalid assignment target", expr.span)),
            }

            let value = self.expression()?;
            let span = Span::combine(expr.span, value.span);
            return Ok(L::new(
                Expr::Assign {
                    target: expr.into(),
                    binary: assign.data.try_into().ok(),
                    value: value.into(),
                },
                span,
            ));
        }

        Ok(expr)
    }

    fn range(&mut self) -> Result<L<Expr>> {
        let mut expr = self.logical_or()?;
        while let Some(op) = self.advance_if(|k| matches!(k, Token::Range | Token::RangeInclusive))
        {
            let inclusive = op.data == Token::RangeInclusive;
            if self.is_range_end() {
                let span = Span::combine(expr.span, op.span);
                expr = L::new(
                    Expr::Range {
                        start: Some(expr.into()),
                        end: None,
                        inclusive,
                    },
                    span,
                );
            } else {
                let right = self.logical_or()?;
                let span = Span::combine(expr.span, right.span);
                expr = L::new(
                    Expr::Range {
                        start: Some(expr.into()),
                        end: Some(right.into()),
                        inclusive,
                    },
                    span,
                );
            }
        }

        Ok(expr)
    }

    binary!(logical_or, Token::LogicalOr, logical_and);
    binary!(logical_and, Token::LogicalAnd, comparison);
    binary!(
        comparison,
        Token::RAngle
            | Token::GtEqual
            | Token::LAngle
            | Token::LtEqual
            | Token::Equal
            | Token::NotEqual,
        coalesce
    );
    binary!(coalesce, Token::NoneCoalesce | Token::ErrCoalesce, or);
    binary!(or, Token::Or, xor);
    binary!(xor, Token::Caret, and);
    binary!(and, Token::Ampersand, shift);
    binary!(shift, Token::Shl | Token::Shr, term);
    binary!(term, Token::Plus | Token::Minus, factor);
    binary!(factor, Token::Asterisk | Token::Div | Token::Rem, unary);

    fn unary(&mut self) -> Result<L<Expr>> {
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
                    | Token::Sizeof
            )
        }) {
            let op = if t.data == Token::Ampersand && self.advance_if_kind(Token::Mut).is_some() {
                UnaryOp::AddrMut
            } else {
                t.data.try_into().unwrap()
            };

            let expr = self.unary()?;
            let span = Span::combine(t.span, expr.span);
            return Ok(L::new(
                Expr::Unary {
                    op,
                    expr: expr.into(),
                },
                span,
            ));
        }

        self.call()
    }

    fn call(&mut self) -> Result<L<Expr>> {
        let mut expr = self.primary()?;
        loop {
            if self.advance_if_kind(Token::LParen).is_some() {
                let (args, span) = if let Some(rparen) = self.advance_if_kind(Token::RParen) {
                    (Vec::new(), rparen.span)
                } else {
                    self.comma_separated(Token::RParen, "expected ')'", |this| {
                        let mut expr = this.expression()?;
                        if let Expr::Path(path) = expr.data {
                            if path.as_symbol().is_some()
                                && this.advance_if_kind(Token::Colon).is_some()
                            {
                                return Ok((
                                    Some(path.data.into_iter().next().unwrap().0),
                                    this.expression()?,
                                ));
                            }

                            expr.data = Expr::Path(path);
                        }

                        Ok((None, expr))
                    })?
                };

                let span = Span::combine(expr.span, span);
                expr = L::new(
                    Expr::Call {
                        callee: expr.into(),
                        args,
                    },
                    span,
                );
            } else if self.advance_if_kind(Token::Dot).is_some() {
                let (member, span) = self.expect_id_with_span("expected member name")?;
                let span = Span::combine(expr.span, span);
                expr = L::new(
                    Expr::Member {
                        source: expr.into(),
                        member: member.into(),
                    },
                    span,
                );
            } else if self.advance_if_kind(Token::LBrace).is_some() {
                let (args, span) =
                    self.comma_separated(Token::RBrace, "expected ']'", Self::expression)?;
                let span = Span::combine(expr.span, span);
                expr = L::new(
                    Expr::Subscript {
                        callee: expr.into(),
                        args,
                    },
                    span,
                );
            } else if let Some(inc) = self.advance_if_kind(Token::Increment) {
                let span = Span::combine(expr.span, inc.span);
                expr = L::new(
                    Expr::Unary {
                        op: UnaryOp::PostIncrement,
                        expr: expr.into(),
                    },
                    span,
                );
            } else if let Some(dec) = self.advance_if_kind(Token::Decrement) {
                let span = Span::combine(expr.span, dec.span);
                expr = L::new(
                    Expr::Unary {
                        op: UnaryOp::PostDecrement,
                        expr: expr.into(),
                    },
                    span,
                );
            } else if let Some(dec) = self.advance_if_kind(Token::Exclamation) {
                let span = Span::combine(expr.span, dec.span);
                expr = L::new(
                    Expr::Unary {
                        op: UnaryOp::IntoError,
                        expr: expr.into(),
                    },
                    span,
                );
            } else if let Some(dec) = self.advance_if_kind(Token::Question) {
                let span = Span::combine(expr.span, dec.span);
                expr = L::new(
                    Expr::Unary {
                        op: UnaryOp::Try,
                        expr: expr.into(),
                    },
                    span,
                );
            } else {
                break Ok(expr);
            }
        }
    }

    fn primary(&mut self) -> Result<L<Expr>> {
        let token = self.advance()?;
        Ok(match token.data {
            Token::Void => L::new(Expr::Void, token.span),
            Token::False => L::new(Expr::Bool(false), token.span),
            Token::True => L::new(Expr::Bool(true), token.span),
            Token::None => L::new(Expr::None, token.span),
            Token::Int { base, value, width } => L::new(
                Expr::Integer {
                    base,
                    value: value.into(),
                    width: width.map(|w| w.into()),
                },
                token.span,
            ),
            Token::Float(value) => L::new(Expr::Float(value.into()), token.span),
            Token::String(value) => L::new(Expr::String(value.into()), token.span),
            Token::LParen => {
                let expr = self.expression()?;
                if self.advance_if_kind(Token::Comma).is_some() {
                    let mut exprs = vec![expr];
                    let span = self.advance_until(Token::RParen, token.span, |this| {
                        exprs.push(this.expression()?);
                        Ok(())
                    })?;

                    L::new(Expr::Tuple(exprs), span)
                } else {
                    let end = self.expect_kind(Token::RParen, "exprected ')'")?;
                    L::new(expr.data, Span::combine(token.span, end.span))
                }
            }
            Token::Ident(ident) => {
                let result = self.path_with_one(false, (ident, token.span))?;
                L::new(Expr::Path(result.data), result.span)
            }
            Token::ScopeRes => {
                let ident = self.expect_id_with_span("expected name")?;
                let result = self.path_with_one(false, ident)?;
                L::new(Expr::Path(result.data), result.span)
            }
            Token::This => L::new(Expr::Path(THIS_PARAM.to_owned().into()), token.span),
            Token::ThisType => L::new(Expr::Path(THIS_TYPE.to_owned().into()), token.span),
            Token::Range => {
                if self.is_range_end() {
                    L::new(
                        Expr::Range {
                            start: None,
                            end: None,
                            inclusive: false,
                        },
                        token.span,
                    )
                } else {
                    let end = self.expression()?;
                    let span = Span::combine(token.span, end.span);
                    L::new(
                        Expr::Range {
                            start: None,
                            end: Some(end.into()),
                            inclusive: false,
                        },
                        span,
                    )
                }
            }
            Token::LCurly => self.block_expr(token.span)?,
            Token::If => {
                let cond = self.expression()?;
                let lcurly = self.expect_kind(Token::LCurly, "expected block")?;
                let if_branch = self.block_expr(lcurly.span)?;
                let else_branch = if self.advance_if_kind(Token::Else).is_some() {
                    if !self.matches_kind(Token::If) {
                        let lcurly = self.expect_kind(Token::LCurly, "expected block")?;
                        Some(self.block_expr(lcurly.span)?)
                    } else {
                        Some(self.expression()?)
                    }
                } else {
                    None
                };
                let span = Span::combine(
                    token.span,
                    else_branch.as_ref().map_or(if_branch.span, |e| e.span),
                );
                L::new(
                    Expr::If {
                        cond: cond.into(),
                        if_branch: if_branch.into(),
                        else_branch: else_branch.map(|e| e.into()),
                    },
                    span,
                )
            }
            Token::Loop => {
                let (cond, lcurly) = if let Some(lcurly) = self.advance_if_kind(Token::LCurly) {
                    (None, lcurly)
                } else {
                    (
                        Some(self.expression()?),
                        self.expect_kind(Token::LCurly, "expected '{'")?,
                    )
                };

                let mut span = token.span;
                let (body, _) = self.parse_block(lcurly.span)?;
                let (cond, do_while) = if let Some(cond) = cond {
                    (cond, false)
                } else if self.advance_if_kind(Token::While).is_some() {
                    let cond = self.expression()?;
                    span.extend_to(cond.span);
                    (cond, true)
                } else {
                    (L::new(Expr::Bool(true), token.span), false)
                };

                L::new(
                    Expr::Loop {
                        cond: cond.into(),
                        body,
                        do_while,
                    },
                    span,
                )
            }
            Token::For => {
                let var = self.expect_id("expected variable name")?;
                self.expect_kind(Token::In, "expected 'in'")?;
                // TODO: parse for foo in 0.. {} as |0..| |{}| instead of |0..{}|
                let iter = self.expression()?;
                let lcurly = self.expect_kind(Token::LCurly, "expected '{'")?;
                let (body, span) = self.parse_block(lcurly.span)?;
                L::new(
                    Expr::For {
                        var: var.into(),
                        iter: iter.into(),
                        body,
                    },
                    Span::combine(token.span, span),
                )
            }
            Token::LBrace => {
                if let Some(rbrace) = self.advance_if_kind(Token::RBrace) {
                    L::new(
                        Expr::Array(Vec::new()),
                        Span::combine(token.span, rbrace.span),
                    )
                } else if self.advance_if_kind(Token::Colon).is_some() {
                    let rbrace = self.expect_kind(Token::RBrace, "expected ']'")?;
                    L::new(
                        Expr::Map(Vec::new()),
                        Span::combine(token.span, rbrace.span),
                    )
                } else {
                    let expr = self.expression()?;
                    if self.advance_if_kind(Token::Colon).is_some() {
                        let mut exprs = vec![(expr, self.expression()?)];
                        let span = if self.advance_if_kind(Token::Comma).is_some() {
                            self.advance_until(Token::RBrace, token.span, |this| {
                                let key = this.expression()?;
                                this.expect_kind(Token::Colon, "expected ':'")?;
                                let value = this.expression()?;
                                exprs.push((key, value));

                                if !this.matches_kind(Token::RBrace) {
                                    this.expect_kind(Token::Comma, "expected ','")?;
                                }
                                Ok(())
                            })?
                        } else {
                            self.expect_kind(Token::RBrace, "expected ']' or ','")?.span
                        };

                        L::new(Expr::Map(exprs), span)
                    } else if self.advance_if_kind(Token::Semicolon).is_some() {
                        // let count = self.expression()?;
                        let count = self.expect(
                            |t| {
                                let Token::Int { base: 10, value, width: None } = t.data else {
                                    return None;
                                };
                                Some(value)
                            },
                            "expected array size",
                        )?;

                        let rbrace = self.expect_kind(Token::RBrace, "expected ']'")?;
                        L::new(
                            Expr::ArrayWithInit {
                                init: expr.into(),
                                count: count.parse().unwrap(),
                            },
                            Span::combine(token.span, rbrace.span),
                        )
                    } else {
                        self.expect_kind(Token::Comma, "expected ':', ';', or ','")?;
                        let mut exprs = vec![expr];
                        let span = self.advance_until(Token::RBrace, token.span, |this| {
                            exprs.push(this.expression()?);
                            if !this.matches_kind(Token::RBrace) {
                                this.expect_kind(Token::Comma, "expected ','")?;
                            }

                            Ok(())
                        })?;

                        L::new(Expr::Array(exprs), span)
                    }
                }
            }
            _ => {
                return Err(Error::new("unexpected token", token.span));
            }
        })
    }

    //

    fn block_expr(&mut self, lcurly: Span) -> Result<L<Expr>> {
        let (expr, span) = self.parse_block(lcurly)?;
        Ok(L::new(Expr::Block(expr), span))
    }

    fn is_range_end(&mut self) -> bool {
        self.matches(|k| {
            matches!(
                k,
                Token::Semicolon | Token::Comma | Token::RBrace | Token::RParen
            )
        })
    }

    fn advance(&mut self) -> Result<L<Token<'a>>> {
        match self.lexer.next() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(err)) => Err(Error::new(err.data.tell(), err.span)),
            None => Err(Error::new(
                "unexpected eof ",
                Span {
                    loc: Location {
                        row: 0,
                        col: 0,
                        pos: self.src.len(),
                    },
                    len: 0,
                },
            )),
        }
    }

    fn advance_if(&mut self, pred: impl FnOnce(&Token) -> bool) -> Option<L<Token<'a>>> {
        self.lexer
            .next_if(|tok| matches!(tok, Ok(tok) if pred(&tok.data)))
            .map(|token| token.unwrap())
    }

    fn advance_if_kind(&mut self, kind: Token) -> Option<L<Token<'a>>> {
        self.advance_if(|t| t == &kind)
    }

    fn advance_until(
        &mut self,
        token: Token,
        mut span: Span,
        mut f: impl FnMut(&mut Self) -> Result<()>,
    ) -> Result<Span> {
        while match self.advance_if_kind(token.clone()) {
            Some(c) => {
                span.extend_to(c.span);
                false
            }
            None => true,
        } {
            f(self)?;
        }

        Ok(span)
    }

    fn expect<T>(&mut self, pred: impl FnOnce(L<Token<'a>>) -> Option<T>, msg: &str) -> Result<T> {
        let token = self.advance()?;
        let span = token.span;
        match pred(token) {
            Some(t) => Ok(t),
            None => Err(Error::new(msg, span)),
        }
    }

    fn expect_kind(&mut self, kind: Token, msg: &str) -> Result<L<Token<'a>>> {
        self.expect(|t| (t.data == kind).then_some(t), msg)
    }

    fn expect_id(&mut self, msg: &str) -> Result<&'a str> {
        self.expect(
            |t| {
                let Token::Ident(ident) = t.data else { return None; };
                Some(ident)
            },
            msg,
        )
    }

    fn expect_id_with_span(&mut self, msg: &str) -> Result<(&'a str, Span)> {
        self.expect(
            |t| {
                let Token::Ident(ident) = t.data else { return None; };
                Some((ident, t.span))
            },
            msg,
        )
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_type() {
        let mut parser = Parser::new("&[dyn Into<T>!(i44, [u70: str], E?); 3]?");
        let _ty = dbg!(parser.parse_type().unwrap());
    }

    #[test]
    fn parse_fn() {
        let mut parser = Parser::new(
            "
            fn hello<T, E>(a: T, mut b: E) T!E {}
        ",
        );

        _ = dbg!(parser.item());
    }

    #[test]
    fn parse_struct() {
        let mut parser = Parser::new(
            "
struct Hello<T, E> : Add + Sub {
    foo: T,
    bar: E,

    fn new(a: T, b: C) Hello {}
}
            ",
        );

        _ = dbg!(parser.item());
    }

    #[test]
    fn square_bracket_literals() {
        let mut parser = Parser::new("let x = [5; 10];");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = [1, 2, 3, 4];");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = [1, 2, 3, 4, ];");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = [1: 2, 3: 4];");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = [1: 2, 3: 4, ];");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = [];");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = [:];");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);
    }

    #[test]
    fn function_calls() {
        let mut parser = Parser::new("let x = foo(a, b);");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = foo(a: bar, b: quux);");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = foo(a: bar, b, c: quux);");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);
    }

    #[test]
    fn instances() {
        let mut parser = Parser::new("let x = Foo { bar: 10, quux: 15 };");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);

        parser = Parser::new("let x = Foo { bar: 10, quux: 15, };");
        crate::pretty::print_stmt(&parser.declaration().unwrap(), 0);
    }
}
