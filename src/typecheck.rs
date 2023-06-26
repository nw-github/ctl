use std::{collections::HashMap, vec};

use crate::{
    ast::{
        expr::{BinaryOp, Expr},
        stmt::{Fn, FnDecl, Stmt, Struct, Type, UserType},
    },
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::{CheckedFn, CheckedFnDecl, CheckedParam, CheckedStmt},
        Block, ResolvedStruct, ResolvedType, ScopeId, TypeId,
    },
    lexer::Located,
    Error,
};

#[derive(Default)]
pub struct Scope {
    parent: Option<ScopeId>,
    vars: HashMap<String, TypeId>,
    types: HashMap<String, TypeId>,
}

pub struct CheckedAst {
    pub types: Vec<ResolvedType>,
    pub scopes: Vec<Scope>,
    pub stmt: CheckedStmt,
}

pub struct TypeChecker {
    types: Vec<ResolvedType>,
    scopes: Vec<Scope>,
    errors: Vec<Error>,
    current: ScopeId,
}

impl TypeChecker {
    pub fn check(stmt: Located<Stmt>) -> (CheckedAst, Vec<Error>) {
        let mut this = Self {
            scopes: vec![Scope::default()],
            types: vec![],
            errors: vec![],
            // we depend on parser wrapping up the generated code in a Stmt::Module
            current: 0,
        };

        this.insert_type("void".into(), ResolvedType::Void);
        this.insert_type("{integer}".into(), ResolvedType::IntGeneric);
        this.insert_type("{float}".into(), ResolvedType::FloatGeneric);
        this.insert_type("f32".into(), ResolvedType::F32);
        this.insert_type("f64".into(), ResolvedType::F64);
        this.insert_type("bool".into(), ResolvedType::Bool);
        for i in 1..=128 {
            if i > 1 {
                this.insert_type(format!("i{i}"), ResolvedType::Int(i));
            }

            this.insert_type(format!("u{i}"), ResolvedType::Uint(i));
        }

        let stmt = this.check_stmt(stmt);
        (
            CheckedAst {
                types: this.types,
                scopes: this.scopes,
                stmt,
            },
            this.errors,
        )
    }

    //     fn forward_declare(&mut self, stmt: &Located<Stmt>) {
    //         match &stmt.data {
    //             Stmt::Fn(_) => {
    //                 self.enter_scope(name.clone(), *public, |this| {
    //                     for stmt in body {
    //                         this.forward_declare(stmt);
    //                     }
    //                 });
    //             }
    //             Stmt::UserType(_) => todo!(),
    //             Stmt::Static { public, name, ty, value } => todo!(),
    //             Stmt::Module { public, name, body } => {
    //                 self.enter_scope(name.clone(), *public, |this| {
    //                     for stmt in body {
    //                         this.forward_declare(stmt);
    //                     }
    //                 });
    //             }
    //             Stmt::Let { .. } | Stmt::Expr(_) => {}
    //         }
    //     }
    //
    //     fn forward_declare_fn(&mut self) {
    //
    //     }

    fn check_stmt(&mut self, stmt: Located<Stmt>) -> CheckedStmt {
        match stmt.data {
            Stmt::Module { public, name, body } => CheckedStmt::Module {
                public,
                name,
                body: self.create_block(body),
            },
            Stmt::UserType(ty) => {
                todo!()
            }
            Stmt::Expr(expr) => CheckedStmt::Expr(self.check_expr(expr, None)),
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(&ty);
                    if let Some(value) = value {
                        let value = self.check_expr(value, Some(ty));
                        if value.ty != ty {
                            return self.error_stmt(Error::new(
                                format!(
                                    "type mismatch: expected type {}, got {}",
                                    self.type_name(&ty),
                                    self.type_name(&value.ty)
                                ),
                                stmt.span,
                            ));
                        }

                        CheckedStmt::Let {
                            name,
                            ty,
                            mutable,
                            value: Some(value),
                        }
                    } else {
                        CheckedStmt::Let {
                            name,
                            ty,
                            mutable,
                            value: None,
                        }
                    }
                } else if let Some(value) = value {
                    let value = self.check_expr(value, None);
                    CheckedStmt::Let {
                        name,
                        ty: value.ty,
                        mutable,
                        value: Some(value),
                    }
                } else {
                    return self.error_stmt(Error::new("cannot infer type", stmt.span));
                }
            }
            Stmt::Fn(Fn {
                header:
                    FnDecl {
                        public,
                        name,
                        is_async,
                        is_extern,
                        type_params,
                        params,
                        ret,
                    },
                body,
            }) => {
                let header = CheckedFnDecl {
                    public,
                    name,
                    is_async,
                    is_extern,
                    type_params,
                    params: params
                        .into_iter()
                        .map(|param| CheckedParam {
                            mutable: param.mutable,
                            keyword: param.keyword,
                            name: param.name,
                            ty: self.resolve_type(&param.ty),
                        })
                        .collect(),
                    ret: self.resolve_type(&ret),
                };
                CheckedStmt::Fn(CheckedFn {
                    body: self.create_block_with(body, |this| {
                        for param in header.params.iter() {
                            this.scopes[this.current]
                                .vars
                                .insert(param.name.clone(), param.ty);
                        }
                    }),
                    header,
                })
            }
            Stmt::Static {
                public,
                name,
                ty,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(&ty);
                    let value = self.check_expr(value, Some(ty));

                    if value.ty != ty {
                        return self.error_stmt(Error::new(
                            format!(
                                "type mismatch: expected type {}, got {}",
                                self.type_name(&ty),
                                self.type_name(&value.ty)
                            ),
                            stmt.span,
                        ));
                    }

                    CheckedStmt::Static {
                        public,
                        name,
                        ty,
                        value,
                    }
                } else {
                    let value = self.check_expr(value, None);
                    CheckedStmt::Static {
                        public,
                        name,
                        ty: value.ty,
                        value,
                    }
                }
            }
        }
    }

    fn check_expr(&mut self, expr: Located<Expr>, target: Option<TypeId>) -> CheckedExpr {
        match expr.data {
            Expr::Binary { op, left, right } => match op {
                BinaryOp::NoneCoalesce => todo!(),
                BinaryOp::ErrCoalesce => todo!(),
                _ => {
                    let left = self.check_expr(*left, target);
                    let right = self.check_expr(*right, Some(left.ty));

                    if !matches!(
                        self.types[right.ty],
                        ResolvedType::F64
                            | ResolvedType::F32
                            | ResolvedType::Int(_)
                            | ResolvedType::Uint(_)
                    ) || left.ty != right.ty
                    {
                        self.error_expr(Error::new(
                            format!(
                                "operator '{}' is invalid for types {} and {}",
                                op.token(),
                                self.type_name(&left.ty),
                                self.type_name(&right.ty)
                            ),
                            expr.span,
                        ))
                    } else {
                        CheckedExpr::new(
                            left.ty,
                            ExprData::Binary {
                                op,
                                left: left.into(),
                                right: right.into(),
                            },
                        )
                    }
                }
            },
            Expr::Unary { op, expr } => todo!(),
            Expr::Call { callee, args } => todo!(),
            Expr::Array(_) => todo!(),
            Expr::ArrayWithInit { init, count } => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::Map(_) => todo!(),
            Expr::Bool(value) => CheckedExpr {
                ty: self.find_type_in_scope("bool", 0).unwrap(),
                data: ExprData::Bool(value),
            },
            Expr::Integer(base, value) => {
                let mut ty = self.find_type_in_scope("{integer}", 0).unwrap();
                if let Some(target) = target {
                    if self.coerces_to(ty, target) {
                        ty = target;
                    }
                } else {
                    // TODO: attempt to promote the literal if its too large for i32
                    ty = self.find_type_in_scope("i32", 0).unwrap();
                }

                match &self.types[ty] {
                    ResolvedType::Int(bits) => {
                        let result = match i128::from_str_radix(&value, *bits as u32) {
                            Ok(result) => result,
                            Err(_) => {
                                return self.error_expr(Error::new(
                                    "Integer literal is too large for any type.",
                                    expr.span,
                                ));
                            }
                        };

                        if result >= 1 << (bits - 1) {
                            return self.error_expr(Error::new(
                                "Integer literal is larger than its type allows",
                                expr.span,
                            ));
                        }
                        if result <= -(1 << (bits - 1)) {
                            return self.error_expr(Error::new(
                                "Integer literal is smaller than its type allows",
                                expr.span,
                            ));
                        }

                        CheckedExpr::new(ty, ExprData::Signed(result))
                    }
                    ResolvedType::Uint(bits) => {
                        let result = match u128::from_str_radix(&value, *bits as u32) {
                            Ok(result) => result,
                            Err(_) => {
                                return self.error_expr(Error::new(
                                    "Integer literal is too large for any type.",
                                    expr.span,
                                ));
                            }
                        };

                        if result >= 1 << bits {
                            return self.error_expr(Error::new(
                                "Integer literal is larger than its type allows",
                                expr.span,
                            ));
                        }

                        CheckedExpr::new(ty, ExprData::Unsigned(result))
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Float(value) => {
                let mut ty = self.find_type_in_scope("{float}", 0).unwrap();
                if let Some(target) = target {
                    if self.coerces_to(ty, target) {
                        ty = target;
                    }
                } else {
                    ty = self.find_type_in_scope("f64", 0).unwrap();
                }

                CheckedExpr::new(ty, ExprData::Float(value))
            }
            Expr::String(_) => todo!(),
            Expr::Symbol(_) => todo!(),
            Expr::Instance { name, members } => todo!(),
            Expr::None => todo!(),
            Expr::Assign {
                target,
                binary,
                value,
            } => todo!(),
            Expr::Block(_) => todo!(),
            Expr::If {
                cond,
                if_branch,
                else_branch,
            } => todo!(),
            Expr::Loop {
                cond,
                body,
                do_while,
            } => todo!(),
            Expr::For { var, iter, body } => todo!(),
            Expr::Member { source, member } => todo!(),
            Expr::Subscript { callee, args } => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Yield(_) => todo!(),
            Expr::Break(_) => todo!(),
            Expr::Range {
                start,
                end,
                inclusive,
            } => todo!(),
            Expr::Continue => todo!(),
        }
    }

    fn coerces_to(&self, ty: TypeId, target: TypeId) -> bool {
        if ty == self.find_type_in_scope("{integer}", 0).unwrap() {
            match self.types[target] {
                ResolvedType::Int(_) | ResolvedType::Uint(_) => return true,
                _ => {}
            }
        } else if ty == self.find_type_in_scope("{float}", 0).unwrap() {
            match self.types[target] {
                ResolvedType::F32 | ResolvedType::F64 => return true,
                _ => {}
            }
        }

        false
    }

    fn resolve_usertype(&mut self, ty: &UserType) -> TypeId {
        // match ty {
        //     UserType::Struct(base) => {
        //         let data = ResolvedType::Struct(self.resolve_struct(base));
        //         self.insert_type(base.name.clone(), data);
        //     }
        //     UserType::Union { tag, base } => {
        //         let data = ResolvedType::Union {
        //             tag: tag.as_ref().map(|ty| self.find_type(ty)),
        //             base: self.resolve_struct(base),
        //         };
        //         self.insert_type(base.name.clone(), data);
        //     }
        //     UserType::Interface {
        //         public,
        //         name,
        //         type_params,
        //         impls,
        //         functions,
        //     } => todo!(),
        //     UserType::Enum {
        //         public,
        //         name,
        //         impls,
        //         variants,
        //         functions,
        //     } => todo!(),
        // }

        todo!()
    }

    fn resolve_struct(&mut self, Struct { members, .. }: &Struct) -> ResolvedStruct {
        todo!()
        // ResolvedStruct {
        //     members: members
        //         .iter()
        //         .map(|m| Member {
        //             public: m.public,
        //             name: m.name.clone(),
        //             ty: self
        //                 .resolve_type(&m.ty)
        //                 .expect("unable to resolve member type"),
        //         })
        //         .collect(),
        // }
    }

    fn resolve_type(&mut self, ty: &Type) -> TypeId {
        match ty {
            Type::Regular { name, .. } => self.find_type(name),
            Type::Array(_, _) => self.find_type_in_scope("Array", 0),
            Type::Slice(_) => self.find_type_in_scope("Slice", 0),
            Type::Tuple(_) => todo!(),
            Type::Map(_, _) => self.find_type_in_scope("Map", 0),
            Type::Option(_) => self.find_type_in_scope("Option", 0),
            Type::Result(_, _) => self.find_type_in_scope("Result", 0),
            Type::Anon(ty) => Some(self.resolve_usertype(ty)),
            Type::Void => self.find_type_in_scope("void", 0),
            Type::Ref(_) | Type::RefMut(_) | Type::This => unreachable!(),
        }
        .unwrap()
    }

    fn create_block(&mut self, body: Vec<Located<Stmt>>) -> Block {
        self.enter_scope(|this| Block {
            body: body.into_iter().map(|stmt| this.check_stmt(stmt)).collect(),
            scope: this.current,
        })
    }

    fn create_block_with(&mut self, body: Vec<Located<Stmt>>, f: impl FnOnce(&mut Self)) -> Block {
        self.enter_scope(|this| {
            f(this);

            Block {
                body: body.into_iter().map(|stmt| this.check_stmt(stmt)).collect(),
                scope: this.current,
            }
        })
    }

    //

    fn error_stmt(&mut self, error: Error) -> CheckedStmt {
        self.errors.push(error);
        CheckedStmt::Error
    }

    fn error_expr(&mut self, error: Error) -> CheckedExpr {
        self.errors.push(error);
        CheckedExpr::new(0, ExprData::Error)
    }

    fn type_name(&self, ty: &TypeId) -> String {
        match &self.types[*ty] {
            ResolvedType::Void => "void".into(),
            ResolvedType::Never => todo!(),
            ResolvedType::Int(bits) => format!("i{bits}"),
            ResolvedType::Uint(bits) => format!("u{bits}"),
            ResolvedType::F32 => "f32".into(),
            ResolvedType::F64 => "f64".into(),
            ResolvedType::Bool => "bool".into(),
            ResolvedType::IntGeneric => "{integer}".into(),
            ResolvedType::FloatGeneric => "{float}".into(),
            ResolvedType::Struct(_) => todo!(),
            ResolvedType::Union { tag, base } => todo!(),
            ResolvedType::Enum {} => todo!(),
            ResolvedType::Interface {} => todo!(),
            ResolvedType::Function {} => todo!(),
        }
    }

    fn find_type(&self, name: &str) -> Option<TypeId> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.types.get(name).copied())
            .next()
    }

    fn find_type_in_scope(&self, name: &str, scope: ScopeId) -> Option<TypeId> {
        self.scopes[scope].types.get(name).copied()
    }

    fn insert_type(&mut self, name: String, data: ResolvedType) -> TypeId {
        let id = self.types.len();
        self.types.push(data);
        self.scopes[self.current].types.insert(name, id);
        id
    }

    fn enter_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes.push(Scope {
            parent: Some(self.current),
            types: Default::default(),
            vars: Default::default(),
        });

        self.current += 1;
        let result = f(self);
        self.current -= 1;
        result
    }
}
