use std::collections::HashMap;

use crate::{
    ast::{
        expr::Expr,
        stmt::{Fn, FnDecl, Stmt, Struct, Type, UserType},
    },
    checked_ast::{
        expr::CheckedExpr,
        stmt::{CheckedFn, CheckedFnDecl, CheckedParam, CheckedStmt},
        Block, ResolvedStruct, ResolvedType, ScopeId, TypeId,
    },
    lexer::Located,
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
    current: ScopeId,
}

impl TypeChecker {
    pub fn check(stmt: Located<Stmt>) -> (CheckedAst, Vec<()>) {
        let mut this = Self {
            scopes: vec![Scope::default()],
            types: vec![],
            // we depend on parser wrapping up the generated code in a Stmt::Module
            current: 0,
        };

        this.insert_type("void".into(), ResolvedType::Void);
        this.insert_type("i32".into(), ResolvedType::Int(32));
        let stmt = this.check_stmt(stmt);
        (
            CheckedAst {
                types: this.types,
                scopes: this.scopes,
                stmt,
            },
            Vec::new(),
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
            Stmt::Expr(expr) => CheckedStmt::Expr(self.check_expr(expr)),
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(&ty);
                    if let Some(value) = value {
                        let value = self.check_expr(value);
                        if value.ty != ty {
                            todo!("type mismatch");
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
                    let value = self.check_expr(value);
                    CheckedStmt::Let {
                        name,
                        ty: value.ty,
                        mutable,
                        value: Some(value),
                    }
                } else {
                    todo!("cannot infer type");
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
                            this.scopes[this.current].vars.insert(param.name.clone(), param.ty);
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
            } => todo!(),
        }
    }

    fn check_expr(&mut self, expr: Located<Expr>) -> CheckedExpr {
        match expr.data {
            Expr::Binary { op, left, right } => todo!(),
            Expr::Unary { op, expr } => todo!(),
            Expr::Call { callee, args } => todo!(),
            Expr::Array(_) => todo!(),
            Expr::ArrayWithInit { init, count } => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::Map(_) => todo!(),
            Expr::Bool(_) => todo!(),
            Expr::Integer(_, _) => todo!(),
            Expr::Float(_) => todo!(),
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
