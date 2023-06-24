use std::iter::Peekable;

use crate::{
    ast::{
        expr::Expr,
        stmt::{self, Fn, FnDecl, Param, Stmt, Type},
    },
    lexer::{Lexer, Located as L, Location, Span, Token},
};

#[derive(Debug)]
pub struct Error {
    diagnostic: &'static str,
}

type Result<T> = std::result::Result<T, L<Error>>;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    src: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: Lexer::new(src).peekable(),
            src,
        }
    }

    pub fn parse(mut self) -> std::result::Result<Vec<L<Stmt>>, Vec<L<Error>>> {
        let mut stmts = Vec::new();
        let mut errors = Vec::new();
        while self.lexer.peek().is_some() {
            match self.item() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => errors.push(err),
            }
        }

        errors.is_empty().then_some(stmts).ok_or(errors)
    }

    // helpers

    fn comma_separated<T>(
        &mut self,
        end: Token,
        msg: &'static str,
        mut f: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut v = Vec::new();
        loop {
            v.push(f(self)?);
            if self.advance_if_kind(Token::Comma).is_none() {
                break;
            }
        }

        self.expect_kind(end, msg)?;
        Ok(v)
    }

    fn try_function_decl(&mut self, allow_method: bool) -> Option<Result<(FnDecl, Span)>> {
        if let Some(token) = self.advance_if_kind(Token::Extern) {
            Some((|| {
                self.expect_kind(Token::Fn, "expected 'fn'")?;
                self.function_decl(allow_method, false, true)
                    .map(|r| (r, token.span))
            })())
        } else if let Some(token) = self.advance_if_kind(Token::Fn) {
            Some(
                self.function_decl(allow_method, false, false)
                    .map(|r| (r, token.span)),
            )
        } else {
            self.advance_if_kind(Token::Async).map(|token| {
                (|| {
                    self.expect_kind(Token::Fn, "expected 'fn'")?;
                    self.function_decl(allow_method, true, false)
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
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_interface_impl(&mut self) -> Result<Vec<String>> {
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

    fn parse_type(&mut self) -> Result<Type> {
        let wrapper = self.advance_if_kind(Token::Asterisk).map(|_| {
            if self.advance_if_kind(Token::Mut).is_some() {
                Type::RefMut
            } else {
                Type::Ref
            }
        });

        let ty = if self.advance_if_kind(Token::LBrace).is_some() {
            let inner = self.parse_type()?;
            if self.advance_if_kind(Token::RBrace).is_some() {
                Type::Slice(inner.into())
            } else if self.advance_if_kind(Token::Semicolon).is_some() {
                let count = self.expect(
                    |t| {
                        let Token::Int(10, num) = t.data else { return None; };
                        Some(num)
                    },
                    "expected array size",
                )?;

                self.expect_kind(Token::RBrace, "expected ']'")?;
                Type::Array(
                    inner.into(),
                    count
                        .parse()
                        .expect("base 10 integer literal should be convertible to usize"),
                )
            } else if self.advance_if_kind(Token::Colon).is_some() {
                let value = self.parse_type()?;
                self.expect_kind(Token::RBrace, "expected ']'")?;
                Type::Map(inner.into(), value.into())
            } else {
                return Err(self.advance()?.map(|_| Error {
                    diagnostic: "expected ']', ';', or ':'",
                }));
            }
        } else if self.advance_if_kind(Token::LParen).is_some() {
            Type::Tuple(self.comma_separated(Token::RParen, "expected ')'", Self::parse_type)?)
        } else if self.advance_if_kind(Token::Void).is_some() {
            Type::Void
        } else {
            let is_dyn = self.advance_if_kind(Token::Dyn);
            let name = self.expect_id("expected type name")?;
            Type::Regular {
                name: name.into(),
                is_dyn: is_dyn.is_some(),
                type_params: self.parse_generic_params()?,
            }
        };

        let ty = if let Some(wrapper) = wrapper {
            wrapper(ty.into())
        } else {
            ty
        };
        if self.advance_if_kind(Token::Question).is_some() {
            Ok(Type::Option(ty.into()))
        } else if self.advance_if_kind(Token::Exclamation).is_some() {
            Ok(Type::Result(ty.into(), self.parse_type()?.into()))
        } else {
            Ok(ty)
        }
    }

    fn parse_var_name(&mut self) -> Result<(String, Option<Type>)> {
        let name = self.expect_id("expected name")?;
        let ty = if self.advance_if_kind(Token::Colon).is_some() {
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok((name.into(), ty))
    }

    fn parse_block(&mut self, mut span: Span) -> Result<(Vec<L<Stmt>>, Span)> {
        let mut stmts = Vec::new();
        if self.advance_if_kind(Token::RCurly).is_none() {
            loop {
                stmts.push(self.declaration()?);
                if self.matches_kind(Token::RCurly) {
                    break;
                }
            }

            span.extend_to(self.expect_kind(Token::RCurly, "expected '}'")?.span);
        }

        Ok((stmts, span))
    }

    fn parse_struct_body(&mut self, span: &mut Span) -> Result<stmt::Struct> {
        let type_params = self.parse_generic_params()?;
        let impls = self.parse_interface_impl()?;

        self.expect_kind(Token::LCurly, "expected '{'")?;

        let mut functions = Vec::new();
        let mut members = Vec::new();
        while match self.advance_if_kind(Token::RCurly) {
            Some(c) => {
                span.extend_to(c.span);
                false
            }
            None => true,
        } {
            let public = self.advance_if_kind(Token::Pub);
            if let Some(header) = self.try_function_decl(true) {
                let (header, _) = header?;
                let lcurly = self.expect_kind(Token::LCurly, "expected '{'")?;
                let (body, _) = self.parse_block(lcurly.span)?;

                functions.push(Fn { header, body });
            } else {
                let (name, ty) = self.parse_var_name()?;
                let value = if self.advance_if_kind(Token::Assign).is_some() {
                    Some(self.expression()?)
                } else {
                    None
                };
                self.expect_kind(Token::Comma, "expected ','")?;

                members.push(stmt::MemVar { name, ty, value });
            }
        }

        Ok(stmt::Struct {
            name: String::new(),
            type_params,
            members,
            impls,
            functions,
        })
    }

    fn function_decl(
        &mut self,
        allow_method: bool,
        is_async: bool,
        is_extern: bool,
    ) -> Result<FnDecl> {
        let name = self.expect_id("expected name")?;
        let type_params = if self.advance_if_kind(Token::LAngle).is_some() {
            self.comma_separated(Token::RAngle, "expected '>'", |this| {
                this.expect_id("expected type name").map(|id| id.into())
            })?
        } else {
            Vec::new()
        };

        self.expect_kind(Token::LParen, "expected parameter list")?;
        let params = if self.advance_if_kind(Token::RParen).is_none() {
            let mut count = 0;
            self.comma_separated(Token::RParen, "expected ')'", |this| {
                count += 1;

                let mutable = this.advance_if_kind(Token::Mut).is_some();
                if allow_method && count == 1 && this.advance_if_kind(Token::This).is_some() {
                    Ok(Param {
                        mutable,
                        name: "this".into(),
                        ty: Type::This,
                    })
                } else {
                    let name = this.expect_id("expected name")?.into();
                    this.expect_kind(Token::Colon, "expected type")?;
                    Ok(Param {
                        mutable,
                        name,
                        ty: this.parse_type()?,
                    })
                }
            })?
        } else {
            Vec::new()
        };

        Ok(FnDecl {
            name: name.into(),
            is_async,
            is_extern,
            type_params,
            params,
            ret: if !self.matches(|kind| matches!(kind, Token::Semicolon | Token::LCurly)) {
                self.parse_type()?
            } else {
                Type::Void
            },
        })
    }

    fn expect_function_decl(&mut self, allow_method: bool) -> Result<(FnDecl, Span)> {
        match self.try_function_decl(allow_method) {
            Some(f) => Ok(f?),
            None => Err(self.advance()?.map(|_| Error {
                diagnostic: "expected function",
            })),
        }
    }

    //

    fn try_item(&mut self) -> Option<Result<L<Stmt>>> {
        let public = self.advance_if_kind(Token::Pub);
        if let Some(header) = self.try_function_decl(false) {
            Some((|| {
                let (header, span) = header?;
                if header.is_extern {
                    let semi = self.expect_kind(Token::Semicolon, "expected ';'")?;
                    Ok(L::new(
                        Stmt::Fn(Fn {
                            header,
                            body: Vec::new(),
                        }),
                        Span::combine(public.map_or(span, |p| p.span), semi.span),
                    ))
                } else {
                    let lcurly = self.expect_kind(Token::LCurly, "expected '{'")?;
                    let (body, body_span) = self.parse_block(lcurly.span)?;

                    Ok(L::new(
                        Stmt::Fn(Fn { header, body }),
                        Span::combine(public.map_or(span, |p| p.span), body_span),
                    ))
                }
            })())
        } else if let Some(mut token) = self.advance_if_kind(Token::Struct) {
            Some((|| {
                Ok(L::new(
                    Stmt::Struct(stmt::Struct {
                        name: self.expect_id("expected name")?.into(),
                        ..self.parse_struct_body(&mut token.span)?
                    }),
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
                    Stmt::Union {
                        tag,
                        base: stmt::Struct {
                            name: self.expect_id("expected name")?.into(),
                            ..self.parse_struct_body(&mut token.span)?
                        },
                    },
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
                while self.advance_if_kind(Token::RCurly).is_none() {
                    let header = self.expect_function_decl(true)?;
                    self.expect_kind(Token::Semicolon, "expected ';'")?;

                    functions.push(header.0);
                }

                Ok(L::new(
                    Stmt::Interface {
                        name,
                        type_params,
                        impls,
                        functions,
                    },
                    token.span,
                ))
            })())
        } else if let Some(mut token) = self.advance_if_kind(Token::Enum) {
            Some((|| {
                let name = self.expect_id("expected name")?.into();
                let impls = self.parse_interface_impl()?;
                self.expect_kind(Token::LCurly, "expected '{'")?;

                let mut functions = Vec::new();
                let mut variants = Vec::new();
                while match self.advance_if_kind(Token::RCurly) {
                    Some(c) => {
                        token.span.extend_to(c.span);
                        false
                    }
                    None => true,
                } {
                    if let Some(header) = self.try_function_decl(true) {
                        let (header, _) = header?;
                        let lcurly = self.expect_kind(Token::LCurly, "expected '{'")?;
                        let (body, _) = self.parse_block(lcurly.span)?;
                        functions.push(Fn { header, body });
                    } else {
                        variants.push((
                            self.expect_id("expected variant name")?.into(),
                            if self.advance_if_kind(Token::Assign).is_some() {
                                Some(self.expression()?)
                            } else {
                                None
                            },
                        ));

                        self.expect_kind(Token::Comma, "expected ','")?;
                    }
                }

                Ok(L::new(
                    Stmt::Enum {
                        name,
                        impls,
                        variants,
                        functions,
                    },
                    token.span,
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
                        Stmt::Static { name, ty, value },
                        Span::combine(token.span, semi.span),
                    ))
                })()
            })
        }
    }

    fn item(&mut self) -> Result<L<Stmt>> {
        self.try_item().unwrap_or_else(|| {
            Err(self.advance()?.map(|_| Error {
                diagnostic: "expected item",
            }))
        })
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
        if !matches!(
            expr.data,
            Expr::Loop { .. } | Expr::If { .. } | Expr::Block(_)
        ) {
            span.extend_to(self.expect_kind(Token::Semicolon, "expected ';'")?.span);
        }

        Ok(L::new(Stmt::Expr(expr), span))
    }

    //

    fn expression(&mut self) -> Result<L<Expr>> {
        todo!()
    }

    fn path(&mut self) -> Result<String> {
        self.expect_id("expected path").map(|s| s.into())
    }

    //

    fn advance(&mut self) -> Result<L<Token<'a>>> {
        match self.lexer.next() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(err)) => Err(err.map(|data| Error {
                diagnostic: data.tell(),
            })),
            None => Err(L::new(
                Error {
                    diagnostic: "unexpected eof ",
                },
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

    fn expect<T>(
        &mut self,
        pred: impl FnOnce(L<Token<'a>>) -> Option<T>,
        msg: &'static str,
    ) -> Result<T> {
        let token = self.advance()?;
        let span = token.span;
        match pred(token) {
            Some(t) => Ok(t),
            None => Err(L::new(Error { diagnostic: msg }, span)),
        }
    }

    fn expect_kind(&mut self, kind: Token, msg: &'static str) -> Result<L<Token<'a>>> {
        self.expect(|t| (t.data == kind).then_some(t), msg)
    }

    fn expect_id(&mut self, msg: &'static str) -> Result<&'a str> {
        self.expect(
            |t| {
                let Token::Ident(ident) = t.data else { return None; };
                Some(ident)
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
        let ty = dbg!(parser.parse_type().unwrap());
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
    bar: E

    fn new(a: T, b: C) Hello {}
}
            ",
        );

        _ = dbg!(parser.item());
    }
}
