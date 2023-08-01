use std::{iter::Peekable, path::PathBuf};

use crate::{
    ast::{
        expr::{Expr, UnaryOp},
        stmt::{Fn, MemVar, Param, ParsedUserType, Stmt, Struct, TypeHint},
        Path, Pattern,
    },
    lexer::{Lexer, Located as L, Span, Token, FileIndex},
    Error, THIS_PARAM, THIS_TYPE,
};

macro_rules! binary {
    ($name: ident, $patt: pat, $next: ident) => {
        fn $name(&mut self) -> L<Expr> {
            let mut expr = self.$next();
            while let Some(op) = self.advance_if(|kind| matches!(kind, $patt)) {
                let right = self.$next();
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

            expr
        }
    };
}

pub struct ParsedFile {
    pub ast: L<Stmt>,
    pub errors: Vec<Error>,
    pub path: PathBuf,
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    needs_sync: bool,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str, file: FileIndex) -> Self {
        Self {
            lexer: Lexer::new(src, file).peekable(),
            needs_sync: false,
            errors: Vec::new(),
        }
    }

    pub fn parse(src: &'a str, file: FileIndex, path: PathBuf) -> std::io::Result<ParsedFile> {
        let mut this = Self::new(src, file);
        let mut stmts = Vec::new();
        while !this.matches_kind(Token::Eof) {
            stmts.push(this.item());
        }

        Ok(ParsedFile {
            ast: L::new(
                Stmt::Module {
                    public: true,
                    name: crate::derive_module_name(&path),
                    body: stmts,
                },
                Span {
                    loc: Default::default(),
                    len: 0,
                    file,
                },
            ),
            errors: this.errors,
            path,
        })
    }

    //

    fn try_item(&mut self) -> Option<L<Stmt>> {
        let public = self.advance_if_kind(Token::Pub);
        if let Some((mut proto, span)) = self.try_prototype(public.is_some(), false) {
            if proto.is_extern {
                let semi = self.expect_kind(Token::Semicolon, "expected ';'");
                Some(L::new(
                    Stmt::Fn(proto),
                    Span::combine(public.map_or(span, |p| p.span), semi.span),
                ))
            } else {
                let lcurly = self.expect_kind(Token::LCurly, "expected '{'");
                let (body, body_span) = self.parse_block(lcurly.span);
                proto.body = Some(body);
                Some(L::new(
                    Stmt::Fn(proto),
                    Span::combine(public.map_or(span, |p| p.span), body_span),
                ))
            }
        } else if let Some(token) = self.advance_if_kind(Token::Struct) {
            Some(self.parse_struct(public.is_some(), token.span))
        } else if let Some(token) = self.advance_if_kind(Token::Union) {
            Some(self.parse_union(public.is_some(), token.span, false))
        } else if let Some(token) = self.advance_if_kind(Token::Unsafe) {
            self.expect_kind(Token::Union, "expected 'union'");
            Some(self.parse_union(public.is_some(), token.span, true))
        } else if let Some(token) = self.advance_if_kind(Token::Trait) {
            let name = self.expect_id("expected name");
            let type_params = self.parse_generic_params();
            let impls = self.parse_trait_impl();
            self.expect_kind(Token::LCurly, "expected '{'");

            let mut functions = Vec::new();
            let span = self.advance_until(Token::RCurly, token.span, |this| {
                if let Ok((proto, _)) = this.expect_prototype(true, true) {
                    this.expect_kind(Token::Semicolon, "expected ';'");
                    functions.push(proto);
                }
            });

            Some(L::new(
                Stmt::UserType(ParsedUserType::Trait {
                    public: public.is_some(),
                    name,
                    type_params,
                    impls,
                    functions,
                }),
                span,
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Enum) {
            let name = self.expect_located_id("expected name");
            let impls = self.parse_trait_impl();
            self.expect_kind(Token::LCurly, "expected '{'");

            let mut functions = Vec::new();
            let mut variants = Vec::new();
            let span = self.advance_until(Token::RCurly, token.span, |this| {
                if this.advance_if_kind(Token::Pub).is_some() {
                    if let Ok((mut func, _)) = this.expect_prototype(true, true) {
                        let lcurly = this.expect_kind(Token::LCurly, "expected '{'");
                        func.body = Some(this.parse_block(lcurly.span).0);
                        functions.push(func);
                    }
                } else if let Some((mut func, _)) = this.try_prototype(false, true) {
                    let lcurly = this.expect_kind(Token::LCurly, "expected '{'");
                    func.body = Some(this.parse_block(lcurly.span).0);
                    functions.push(func);
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

            Some(L::new(
                Stmt::UserType(ParsedUserType::Enum {
                    public: public.is_some(),
                    name,
                    impls,
                    variants,
                    functions,
                }),
                span,
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Mod) {
            let name = self.expect_id("expected name");
            let mut body = Vec::new();
            self.expect_kind(Token::LCurly, "expected '{'");
            let span = self.advance_until(Token::RCurly, token.span, |this| body.push(this.item()));
            Some(L::new(
                Stmt::Module {
                    public: public.is_some(),
                    name,
                    body,
                },
                span,
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Use) {
            let mut components = vec![(self.expect_id("expected path"), vec![])];
            let span = self.advance_until(Token::Semicolon, token.span, |this| {
                this.expect_kind(Token::ScopeRes, "expected '::'");
                components.push((this.expect_id("expected path component"), vec![]));
            });

            Some(L::new(
                Stmt::Use {
                    public: public.is_some(),
                    path: Path::Root(components),
                },
                span,
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Static) {
            let (name, ty) = self.parse_var_name();
            self.expect_kind(
                Token::Assign,
                "expected '=': static variables must be initialized",
            );
            let value = self.expression();
            let semi = self.expect_kind(Token::Semicolon, "expected ';'");

            Some(L::new(
                Stmt::Static {
                    public: public.is_some(),
                    name,
                    ty,
                    value,
                },
                Span::combine(token.span, semi.span),
            ))
        } else {
            None
        }
    }

    fn item(&mut self) -> L<Stmt> {
        self.try_item().unwrap_or_else(|| {
            let token = self.advance();
            self.error(Error::new("expected item", token.span));
            L::new(Stmt::Error, token.span)
        })
    }

    fn declaration(&mut self) -> L<Stmt> {
        let stmt = if let Some(item) = self.try_item() {
            item
        } else if let Some(token) = self.advance_if(|t| matches!(t, Token::Let | Token::Mut)) {
            let mutable = matches!(token.data, Token::Mut);
            let (name, ty) = self.parse_var_name();

            let value = if self.advance_if_kind(Token::Assign).is_some() {
                Some(self.expression())
            } else {
                None
            };
            let semi = self.expect_kind(Token::Semicolon, "expected ';'");
            L::new(
                Stmt::Let {
                    name,
                    ty,
                    mutable,
                    value,
                },
                Span::combine(token.span, semi.span),
            )
        } else {
            self.statement()
        };

        if self.needs_sync {
            self.synchronize();
        }

        stmt
    }

    fn statement(&mut self) -> L<Stmt> {
        let expr = if let Some(token) = self.advance_if_kind(Token::If) {
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

        let span = expr.span;
        L::new(Stmt::Expr(expr), span)
    }

    //

    fn expression(&mut self) -> L<Expr> {
        self.jump()
    }

    fn jump(&mut self) -> L<Expr> {
        if let Some(token) =
            self.advance_if(|tk| matches!(tk, Token::Return | Token::Break | Token::Yield))
        {
            let (expr, span) = if !self
                .matches(|tk| matches!(tk, Token::Semicolon | Token::Comma | Token::RBrace))
            {
                let expr = self.assignment();
                let span = Span::combine(token.span, expr.span);
                (expr.into(), span)
            } else {
                (L::new(Expr::Void, token.span).into(), token.span)
            };

            L::new(
                match token.data {
                    Token::Return => Expr::Return(expr),
                    Token::Break => Expr::Break(expr),
                    Token::Yield => Expr::Yield(expr),
                    _ => unreachable!(),
                },
                span,
            )
        } else if let Some(token) = self.advance_if_kind(Token::Continue) {
            L::new(Expr::Continue, token.span)
        } else {
            self.assignment()
        }
    }

    fn assignment(&mut self) -> L<Expr> {
        let expr = self.range();
        if let Some(assign) = self.advance_if(|k| k.is_assignment()) {
            let value = self.expression();
            let span = Span::combine(expr.span, value.span);
            return L::new(
                Expr::Assign {
                    target: expr.into(),
                    binary: assign.data.try_into().ok(),
                    value: value.into(),
                },
                span,
            );
        }

        expr
    }

    fn range(&mut self) -> L<Expr> {
        let mut expr = self.logical_or();
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
                let right = self.logical_or();
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

        expr
    }

    binary!(logical_or, Token::LogicalOr, logical_and);
    binary!(logical_and, Token::LogicalAnd, comparison);

    fn comparison(&mut self) -> L<Expr> {
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
                //let span = Span::combine(expr.span, pattern.span);
                let span = expr.span;
                expr = L::new(
                    Expr::Is {
                        expr: expr.into(),
                        pattern,
                    },
                    span,
                );
            } else {
                let right = self.coalesce();
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
        }
        expr
    }

    binary!(coalesce, Token::NoneCoalesce | Token::ErrCoalesce, or);
    binary!(or, Token::Or, xor);
    binary!(xor, Token::Caret, and);
    binary!(and, Token::Ampersand, shift);
    binary!(shift, Token::Shl | Token::Shr, term);
    binary!(term, Token::Plus | Token::Minus, factor);
    binary!(factor, Token::Asterisk | Token::Div | Token::Rem, cast);

    fn cast(&mut self) -> L<Expr> {
        let mut expr = self.unary();
        while self.advance_if_kind(Token::As).is_some() {
            let bang = self.advance_if_kind(Token::Exclamation);
            let ty = self.parse_type();
            let span = expr.span;
            expr = L::new(
                Expr::As {
                    expr: expr.into(),
                    ty,
                    throwing: bang.is_some(),
                },
                span,
            );
        }
        expr
    }

    fn unary(&mut self) -> L<Expr> {
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
            let span = Span::combine(t.span, expr.span);
            return L::new(
                Expr::Unary {
                    op,
                    expr: expr.into(),
                },
                span,
            );
        }

        self.call()
    }

    fn call(&mut self) -> L<Expr> {
        let mut expr = self.primary();
        loop {
            if self.advance_if_kind(Token::LParen).is_some() {
                let mut args = Vec::new();
                let span = self.advance_until(Token::RParen, expr.span, |this| {
                    let mut expr = this.expression();
                    let mut name = None;
                    if let Expr::Path(path) = &expr.data {
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

                    args.push((name, expr));
                    if !this.matches_kind(Token::RParen) {
                        this.expect_kind(Token::Comma, "expected ','");
                    }
                });

                expr = L::new(
                    Expr::Call {
                        callee: expr.into(),
                        args,
                    },
                    span,
                );
            } else if self.advance_if_kind(Token::Dot).is_some() {
                let member = self.expect_located_id("expected member name");
                let (generics, span) = if self.advance_if_kind(Token::ScopeRes).is_some() {
                    self.expect_kind(Token::LAngle, "expected '<'");
                    let (params, span) =
                        self.comma_separated(Token::RAngle, "expected '>'", |this| {
                            this.parse_type()
                        });
                    (params, Span::combine(expr.span, span))
                } else {
                    (Vec::new(), Span::combine(expr.span, member.span))
                };

                expr = L::new(
                    Expr::Member {
                        source: expr.into(),
                        member: member.data,
                        generics,
                    },
                    span,
                );
            } else if self.advance_if_kind(Token::LBrace).is_some() {
                let (args, span) =
                    self.comma_separated(Token::RBrace, "expected ']'", Self::expression);
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
                        op: UnaryOp::Unwrap,
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
                break expr;
            }
        }
    }

    fn primary(&mut self) -> L<Expr> {
        let mut token = self.advance();
        match token.data {
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
            Token::Char(value) => L::new(Expr::Char(value), token.span),
            Token::LParen => {
                let expr = self.expression();
                if self.advance_if_kind(Token::Comma).is_some() {
                    let mut exprs = vec![expr];
                    let span = self.advance_until(Token::RParen, token.span, |this| {
                        exprs.push(this.expression());
                    });

                    L::new(Expr::Tuple(exprs), span)
                } else {
                    let end = self.expect_kind(Token::RParen, "exprected ')'");
                    L::new(expr.data, Span::combine(token.span, end.span))
                }
            }
            Token::Ident(ident) => {
                let data = self.path_components(Some(ident), &mut token.span);
                L::new(Expr::Path(Path::Normal(data)), token.span)
            }
            Token::ScopeRes => {
                let ident = self.expect_id("expected name");
                let data = self.path_components(Some(&ident), &mut token.span);
                L::new(Expr::Path(Path::Root(data)), token.span)
            }
            Token::Super => {
                let data = self.path_components(None, &mut token.span);
                L::new(Expr::Path(Path::Super(data)), token.span)
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
                    let end = self.expression();
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
            Token::LCurly => self.block_expr(token.span),
            Token::If => self.if_expr(token.span),
            Token::While => self.while_expr(token.span),
            Token::Loop => self.loop_expr(token.span),
            Token::For => self.for_expr(token.span),
            Token::Match => self.match_expr(token.span),
            Token::LBrace => {
                if let Some(rbrace) = self.advance_if_kind(Token::RBrace) {
                    L::new(
                        Expr::Array(Vec::new()),
                        Span::combine(token.span, rbrace.span),
                    )
                } else if self.advance_if_kind(Token::Colon).is_some() {
                    let rbrace = self.expect_kind(Token::RBrace, "expected ']'");
                    L::new(
                        Expr::Map(Vec::new()),
                        Span::combine(token.span, rbrace.span),
                    )
                } else {
                    let expr = self.expression();
                    if self.advance_if_kind(Token::Colon).is_some() {
                        let mut exprs = vec![(expr, self.expression())];
                        let span = if self.advance_if_kind(Token::Comma).is_some() {
                            self.advance_until(Token::RBrace, token.span, |this| {
                                let key = this.expression();
                                this.expect_kind(Token::Colon, "expected ':'");
                                let value = this.expression();
                                exprs.push((key, value));

                                if !this.matches_kind(Token::RBrace) {
                                    this.expect_kind(Token::Comma, "expected ','");
                                }
                            })
                        } else {
                            self.expect_kind(Token::RBrace, "expected ']' or ','").span
                        };

                        L::new(Expr::Map(exprs), span)
                    } else if self.advance_if_kind(Token::Semicolon).is_some() {
                        let count = self.expression();
                        let rbrace = self.expect_kind(Token::RBrace, "expected ']'");
                        L::new(
                            Expr::ArrayWithInit {
                                init: expr.into(),
                                count: count.into(),
                            },
                            Span::combine(token.span, rbrace.span),
                        )
                    } else if let Some(rbrace) = self.advance_if_kind(Token::RBrace) {
                        L::new(
                            Expr::Array(vec![expr]),
                            Span::combine(token.span, rbrace.span),
                        )
                    } else {
                        self.expect_kind(Token::Comma, "expected ':', ';', ',', or ']'");
                        let mut exprs = vec![expr];
                        let span = self.advance_until(Token::RBrace, token.span, |this| {
                            exprs.push(this.expression());
                            if !this.matches_kind(Token::RBrace) {
                                this.expect_kind(Token::Comma, "expected ','");
                            }
                        });

                        L::new(Expr::Array(exprs), span)
                    }
                }
            }
            _ => {
                self.error(Error::new("unexpected token", token.span));
                L::new(Expr::Error, token.span)
            }
        }
    }

    //

    fn if_expr(&mut self, token: Span) -> L<Expr> {
        let cond = self.expression();
        let lcurly = self.expect_kind(Token::LCurly, "expected block");
        let if_branch = self.block_expr(lcurly.span);
        let else_branch = self.advance_if_kind(Token::Else).map(|_| {
            if !self.matches_kind(Token::If) {
                let lcurly = self.expect_kind(Token::LCurly, "expected block");
                self.block_expr(lcurly.span)
            } else {
                self.expression()
            }
        });
        let span = Span::combine(
            token,
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

    fn while_expr(&mut self, token: Span) -> L<Expr> {
        let cond = self.expression();
        let lcurly = self.expect_kind(Token::LCurly, "expected '{'");
        let (body, span) = self.parse_block(lcurly.span);

        L::new(
            Expr::Loop {
                cond: Some(cond.into()),
                body,
                do_while: false,
            },
            Span::combine(token, span),
        )
    }

    fn loop_expr(&mut self, mut span: Span) -> L<Expr> {
        let lcurly = self.expect_kind(Token::LCurly, "expected '{'");
        let (body, _) = self.parse_block(lcurly.span);
        let (cond, do_while) = self
            .advance_if_kind(Token::While)
            .map(|_| {
                let cond = self.expression();
                span.extend_to(cond.span);
                (Some(cond.into()), true)
            })
            .unwrap_or_default();

        L::new(
            Expr::Loop {
                cond,
                body,
                do_while,
            },
            span,
        )
    }

    fn for_expr(&mut self, token: Span) -> L<Expr> {
        let mutable = self.advance_if_kind(Token::Mut);
        let var = self.expect_id("expected variable name");
        self.expect_kind(Token::In, "expected 'in'");
        // TODO: parse for foo in 0.. {} as |0..| |{}| instead of |0..{}|
        let iter = self.expression();
        let lcurly = self.expect_kind(Token::LCurly, "expected '{'");
        let (body, span) = self.parse_block(lcurly.span);
        L::new(
            Expr::For {
                var,
                mutable: mutable.is_some(),
                iter: iter.into(),
                body,
            },
            Span::combine(token, span),
        )
    }

    fn match_expr(&mut self, token: Span) -> L<Expr> {
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

        L::new(
            Expr::Match {
                expr: expr.into(),
                body,
            },
            span,
        )
    }

    fn block_expr(&mut self, token: Span) -> L<Expr> {
        let (expr, span) = self.parse_block(token);
        L::new(Expr::Block(expr), span)
    }

    //

    fn path_components(
        &mut self,
        first: Option<&str>,
        outspan: &mut Span,
    ) -> Vec<(String, Vec<TypeHint>)> {
        let mut data = first.map(|s| vec![(s.into(), vec![])]).unwrap_or_default();
        while self.advance_if_kind(Token::ScopeRes).is_some() {
            if self.advance_if_kind(Token::LAngle).is_none() {
                let name = self.expect_located_id("expected name");
                data.push((name.data.to_owned(), Vec::new()));
                outspan.extend_to(name.span);
            } else {
                let (params, span) =
                    self.comma_separated(Token::RAngle, "expected '>'", |this| this.parse_type());
                data.last_mut().unwrap().1 = params;
                outspan.extend_to(span);
            }
        }

        data
    }

    fn type_path(&mut self) -> L<Path> {
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
                let (params, pspan) =
                    self.comma_separated(Token::RAngle, "expected '>'", |this| this.parse_type());
                span.as_mut().unwrap().extend_to(pspan);
                data.push((ident.data, params));
            } else {
                data.push((ident.data, Vec::new()));
            }

            if self.advance_if_kind(Token::ScopeRes).is_none() {
                break;
            }
        }

        L::new(
            if root.is_some() {
                Path::Root(data)
            } else if sup.is_some() {
                Path::Super(data)
            } else {
                Path::Normal(data)
            },
            span.unwrap(),
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

    fn comma_separated<T>(
        &mut self,
        end: Token,
        msg: &str,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> (Vec<T>, Span) {
        let mut v = Vec::new();
        loop {
            v.push(f(self));
            if self.advance_if_kind(Token::Comma).is_none() {
                break;
            }
        }
        (v, self.expect_kind(end, msg).span)
    }

    fn parse_generic_params(&mut self) -> Vec<(String, Vec<L<Path>>)> {
        self.advance_if_kind(Token::LAngle)
            .map(|_| {
                self.comma_separated(Token::RAngle, "expected '>'", |this| {
                    (
                        this.expect_id("expected type name"),
                        this.parse_trait_impl(),
                    )
                })
                .0
            })
            .unwrap_or_default()
    }

    fn parse_trait_impl(&mut self) -> Vec<L<Path>> {
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

        let ty = if self.advance_if_kind(Token::LBrace).is_some() {
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
                self.comma_separated(Token::RParen, "expected ')'", Self::parse_type)
                    .0,
            )
        } else if self.advance_if_kind(Token::Void).is_some() {
            TypeHint::Void
        } else if let Some(this) = self.advance_if_kind(Token::ThisType) {
            TypeHint::Regular(L::new(Path::from(THIS_TYPE.to_owned()), this.span))
        } else {
            TypeHint::Regular(self.type_path())
        };

        if self.advance_if_kind(Token::Exclamation).is_some() {
            TypeHint::Result(ty.into(), self.parse_type().into())
        } else {
            ty
        }
    }

    fn parse_var_name(&mut self) -> (String, Option<TypeHint>) {
        (
            self.expect_id("expected name"),
            self.advance_if_kind(Token::Colon)
                .map(|_| self.parse_type()),
        )
    }

    fn parse_block(&mut self, span: Span) -> (Vec<L<Stmt>>, Span) {
        let mut stmts = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            stmts.push(this.declaration());
        });
        (stmts, span)
    }

    fn parse_struct(&mut self, public: bool, span: Span) -> L<Stmt> {
        let name = self.expect_located_id("expected name");
        let type_params = self.parse_generic_params();
        let impls = self.parse_trait_impl();

        self.expect_kind(Token::LCurly, "expected '{'");

        let mut functions = Vec::new();
        let mut members = Vec::new();
        let span = self.advance_until(Token::RCurly, span, |this| {
            let public = this.advance_if_kind(Token::Pub).is_some();
            if let Some((mut func, _)) = this.try_prototype(public, true) {
                let lcurly = this.expect_kind(Token::LCurly, "expected '{'");
                func.body = Some(this.parse_block(lcurly.span).0);
                functions.push(func);
            } else {
                let name = this.expect_id("expected name");
                this.expect_kind(Token::Colon, "expected type");
                let ty = this.parse_type();
                let value = this
                    .advance_if_kind(Token::Assign)
                    .map(|_| this.expression());
                this.expect_kind(Token::Comma, "expected ','");

                members.push(MemVar {
                    public,
                    ty,
                    name,
                    default: value,
                    shared: false,
                });
            }
        });

        L::new(
            Stmt::UserType(ParsedUserType::Struct(Struct {
                public,
                name,
                type_params,
                members,
                impls,
                functions,
            })),
            span,
        )
    }

    fn parse_union(&mut self, public: bool, span: Span, is_unsafe: bool) -> L<Stmt> {
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

        self.expect_kind(Token::LCurly, "expected '{'");
        let span = self.advance_until(Token::RCurly, span, |this| {
            if this.advance_if_kind(Token::Pub).is_some() {
                if let Ok((mut func, _)) = this.expect_prototype(true, true) {
                    let lcurly = this.expect_kind(Token::LCurly, "expected '{'");
                    func.body = Some(this.parse_block(lcurly.span).0);
                    functions.push(func);
                }
            } else if let Some((mut func, _)) = this.try_prototype(false, true) {
                let lcurly = this.expect_kind(Token::LCurly, "expected '{'");
                func.body = Some(this.parse_block(lcurly.span).0);
                functions.push(func);
            } else if this.advance_if_kind(Token::Shared).is_some() {
                let name = this.expect_id("expected name");
                this.expect_kind(Token::Colon, "expected type");
                let ty = this.parse_type();
                let value = this
                    .advance_if_kind(Token::Assign)
                    .map(|_| this.expression());
                this.expect_kind(Token::Comma, "expected ','");

                members.push(MemVar {
                    public,
                    name,
                    shared: true,
                    ty,
                    default: value,
                });
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
                    public,
                    name,
                    shared: false,
                    ty,
                    default: value,
                });
            }
        });

        L::new(
            Stmt::UserType(ParsedUserType::Union {
                tag,
                base: Struct {
                    public,
                    name,
                    type_params,
                    members,
                    impls,
                    functions,
                },
                is_unsafe,
            }),
            span,
        )
    }

    fn prototype(
        &mut self,
        allow_method: bool,
        is_public: bool,
        is_async: bool,
        is_extern: bool,
    ) -> Fn {
        let name = self.expect_located_id("expected name");
        let mut variadic = false;
        let type_params = self.parse_generic_params();
        self.expect_kind(Token::LParen, "expected parameter list");
        let params = if self.advance_if_kind(Token::RParen).is_none() {
            let mut count = 0;
            let mut params = Vec::new();
            let mut has_default = false;
            loop {
                count += 1;

                let keyword = self.advance_if_kind(Token::Keyword).is_some();
                let mutable = self.advance_if_kind(Token::Mut).is_some();
                if allow_method && count == 1 && self.advance_if_kind(Token::This).is_some() {
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
                } else if self.advance_if_kind(Token::Ellipses).is_some() {
                    variadic = true;
                    break;
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

                if self.advance_if_kind(Token::Comma).is_none() {
                    break;
                }
            }

            self.expect_kind(Token::RParen, "expected ')'");
            params
        } else {
            Vec::new()
        };

        Fn {
            name,
            public: is_public,
            is_async,
            is_extern,
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

    fn try_prototype(&mut self, is_public: bool, allow_method: bool) -> Option<(Fn, Span)> {
        if let Some(token) = self.advance_if_kind(Token::Extern) {
            self.expect_kind(Token::Fn, "expected 'fn'");
            Some((
                self.prototype(allow_method, is_public, false, true),
                token.span,
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Fn) {
            Some((
                self.prototype(allow_method, is_public, false, false),
                token.span,
            ))
        } else if let Some(token) = self.advance_if_kind(Token::Async) {
            self.expect_kind(Token::Fn, "expected 'fn'");
            Some((
                self.prototype(allow_method, is_public, true, false),
                token.span,
            ))
        } else {
            None
        }
    }

    fn expect_prototype(&mut self, is_public: bool, allow_method: bool) -> Result<(Fn, Span), ()> {
        loop {
            if let Some(proto) = self.try_prototype(is_public, allow_method) {
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

    fn advance(&mut self) -> L<Token<'a>> {
        loop {
            match self.lexer.next().unwrap() {
                Ok(tok) => break tok,
                Err(err) => self.error(Error::new(err.data.tell(), err.span)),
            }
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
        pred: impl FnOnce(L<Token<'a>>) -> Result<T, L<Token<'a>>>,
        msg: &str,
    ) -> Result<T, L<Token<'a>>> {
        match pred(self.advance()) {
            Ok(t) => Ok(t),
            Err(e) => {
                self.error(Error::new(msg, e.span));
                Err(e)
            }
        }
    }

    fn expect_kind(&mut self, kind: Token, msg: &str) -> L<Token<'a>> {
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

    fn expect_located_id(&mut self, msg: &str) -> L<String> {
        self.expect(
            |t| {
                let Token::Ident(ident) = t.data else { return Err(t); };
                Some(L::new(ident.into(), t.span)).ok_or(t)
            },
            msg,
        )
        .unwrap_or_else(|err| L::new(String::new(), err.span))
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
