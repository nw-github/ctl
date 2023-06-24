use std::iter::Peekable;

use crate::{
    ast::{
        expr::Expr,
        stmt::{Param, Stmt, Type},
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

    //

    fn parse_type(&mut self) -> Result<Type> {
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
                return Err(self.advance()?.map(Error {
                    diagnostic: "expected ']', ';', or ':'",
                }));
            }
        } else if self.advance_if_kind(Token::LParen).is_some() {
            let mut members = Vec::new();
            loop {
                members.push(self.parse_type()?);
                if self.advance_if_kind(Token::Comma).is_none() {
                    break;
                }
            }

            self.expect_kind(Token::RParen, "expected ')'")?;
            Type::Tuple(members)
        } else {
            let is_dyn = self.advance_if_kind(Token::Dyn);
            let name = self.expect_id("expected type name")?;
            if self.advance_if_kind(Token::LAngle).is_some() {
                let mut params = Vec::new();
                loop {
                    params.push(self.expect_id("expected type name")?.into());
                    if self.advance_if_kind(Token::Comma).is_none() {
                        break;
                    }
                }

                self.expect_kind(Token::RAngle, "expected '>'")?;
                Type::Regular {
                    name: name.into(),
                    is_dyn: is_dyn.is_some(),
                    type_params: params,
                }
            } else {
                Type::Regular {
                    name: name.into(),
                    is_dyn: is_dyn.is_some(),
                    type_params: Vec::new(),
                }
            }
        };

        if self.advance_if_kind(Token::Question).is_some() {
            Ok(Type::Option(ty.into()))
        } else if self.advance_if_kind(Token::Exclamation).is_some() {
            Ok(Type::Result(ty.into(), self.parse_type()?.into()))
        } else {
            Ok(ty)
        }
    }

    fn parse_block(&mut self, mut span: Span) -> Result<(Vec<L<Stmt>>, Span)> {
        let mut stmts = Vec::new();
        if self.advance_if_kind(Token::RCurly).is_none() {
            loop {
                stmts.push(self.declaration()?);
                if let Some(token) = self.advance_if_kind(Token::RCurly) {
                    span.extend_to(token.span);
                    break;
                }
            }

            self.expect_kind(Token::RCurly, "expected '}'")?;
        }

        Ok((stmts, span))
    }

    fn try_item(&mut self) -> Option<Result<L<Stmt>>> {
        let public = self.advance_if_kind(Token::Pub);
        if let Some(token) = self.advance_if_kind(Token::Fn) {
            Some(self.function(
                false,
                false,
                public.is_some(),
                public.map(|p| p.span).unwrap_or(token.span),
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Struct) {
            todo!()
        } else if let Some(token) = self.advance_if_kind(Token::Enum) {
            todo!()
        } else if let Some(token) = self.advance_if_kind(Token::Interface) {
            todo!()
        } else if let Some(token) = self.advance_if_kind(Token::Static) {
            todo!()
        } else {
            None
        }
    }

    fn function(
        &mut self,
        _allow_method: bool,
        _is_async: bool,
        _is_pub: bool,
        start: Span,
    ) -> Result<L<Stmt>> {
        let name = self.expect_id("expected name")?;
        let type_params = Vec::new();
//         if self.advance_if_kind(Token::LAngle).is_some() {
//             loop {
//                 
// 
//                 if self.advance_if_kind(Token::Comma).is_none() {
//                     break;
//                 }
//             }
// 
//             self.expect_kind(Token::RAngle, "expected '>'")?;
//         }


        self.expect_kind(Token::LParen, "expected parameter list")?;

        let mut params = Vec::new();
        if self.advance_if_kind(Token::RParen).is_none() {
            loop {
                let mutable = self.advance_if_kind(Token::Mut).is_some();
                let name = self.expect_id("expected name")?.into();
                self.expect_kind(Token::Colon, "expected type")?;
                params.push(Param {
                    mutable,
                    name,
                    ty: self.parse_type()?,
                });

                if self.advance_if_kind(Token::Comma).is_none() {
                    break;
                }
            }

            self.expect_kind(Token::RParen, "expected ')'")?;
        }

        let lcurly = self.expect_kind(Token::LCurly, "expected function body")?;
        let (body, end) = self.parse_block(lcurly.span)?;

        Ok(L::new(
            Stmt::Fn {
                name: name.into(),
                is_async: false,
                type_params,
                params,
                body,
            },
            Span::combine(start, end),
        ))
    }

    //

    fn item(&mut self) -> Result<L<Stmt>> {
        self.try_item().unwrap_or_else(|| {
            Err(self.advance()?.map(Error {
                diagnostic: "expected item",
            }))
        })
    }

    fn declaration(&mut self) -> Result<L<Stmt>> {
        if let Some(item) = self.try_item() {
            item
        } else if let Some(token) = self.advance_if(|t| matches!(t, Token::Let | Token::Mut)) {
            let mutable = matches!(token.data, Token::Mut);
            let name = self.expect_id("expected name")?;
            let value = if self.advance_if_kind(Token::Assign).is_some() {
                Some(self.expression()?)
            } else {
                None
            };
            let semi = self.expect_kind(Token::Semicolon, "expected ';'")?;

            Ok(L::new(
                Stmt::Let {
                    name: name.into(),
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
        todo!()
    }

    //

    fn expression(&mut self) -> Result<L<Expr>> {
        todo!()
    }

    //

    fn advance(&mut self) -> Result<L<Token<'a>>> {
        match self.lexer.next() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(err)) => Err(err.map(Error {
                diagnostic: err.data.tell(),
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
    fn it_works() {
        let parser = Parser::new("
fn everything(x: [dyn Into<i32>!(i44, [u70: str], str?); 3]?) {}
        ");
        _ = dbg!(parser.parse());
    }
}

