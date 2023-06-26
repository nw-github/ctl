use std::{collections::HashMap, vec};

use crate::{
    ast::{
        expr::{BinaryOp, Expr, UnaryOp},
        stmt::{Fn, FnDecl, Stmt, Struct, TypeHint, UserType},
    },
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::{CheckedFn, CheckedFnDecl, CheckedParam, CheckedStmt},
        Block, ScopeId, TypeId,
    },
    lexer::{Located, Span},
    Error,
};

#[derive(Debug)]
pub struct Member {
    pub public: bool,
    pub name: String,
    pub ty: TypeId,
}

#[derive(Debug)]
pub struct ResolvedStruct {
    pub members: Vec<Member>,
}

#[derive(Debug)]
pub enum Type {
    Void,
    Never,
    Int(u8),
    Uint(u8),
    F32,
    F64,
    Bool,
    IntGeneric,
    FloatGeneric,
    Struct(ResolvedStruct),
    Union {
        tag: Option<Option<TypeId>>,
        base: ResolvedStruct,
    },
    Enum {},
    Interface {},
    Function {
        params: Vec<(String, TypeId)>,
        ret: TypeId,
    },
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int(_) | Type::Uint(_) | Type::F32 | Type::F64)
    }

    pub fn supports_binop(&self, op: BinaryOp) -> bool {
        match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::Gt
            | BinaryOp::GtEqual
            | BinaryOp::Lt
            | BinaryOp::LtEqual => {
                matches!(self, Type::Int(_) | Type::Uint(_) | Type::F32 | Type::F64)
            }
            BinaryOp::And | BinaryOp::Xor | BinaryOp::Or | BinaryOp::Shl | BinaryOp::Shr => {
                matches!(self, Type::Int(_) | Type::Uint(_))
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                matches!(
                    self,
                    Type::Int(_) | Type::Uint(_) | Type::F32 | Type::F64 | Type::Bool
                )
            }
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                matches!(self, Type::Bool)
            }
            BinaryOp::NoneCoalesce => todo!(),
            BinaryOp::ErrCoalesce => todo!(),
        }
    }
}

#[derive(Default)]
pub struct Scope {
    parent: Option<ScopeId>,
    vars: HashMap<String, TypeId>,
    types: HashMap<String, TypeId>,
}

pub struct CheckedAst {
    pub types: Vec<Type>,
    pub scopes: Vec<Scope>,
    pub stmt: CheckedStmt,
}

pub struct TypeChecker {
    types: Vec<Type>,
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

        this.insert_type_in_scope("void".into(), Type::Void);
        this.insert_type_in_scope("{integer}".into(), Type::IntGeneric);
        this.insert_type_in_scope("{float}".into(), Type::FloatGeneric);
        this.insert_type_in_scope("f32".into(), Type::F32);
        this.insert_type_in_scope("f64".into(), Type::F64);
        this.insert_type_in_scope("bool".into(), Type::Bool);
        for i in 1..=128 {
            if i > 1 {
                this.insert_type_in_scope(format!("i{i}"), Type::Int(i));
            }

            this.insert_type_in_scope(format!("u{i}"), Type::Uint(i));
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
            Stmt::UserType(_) => todo!(),
            Stmt::Expr(expr) => CheckedStmt::Expr(self.check_expr(expr, None)),
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(&ty);
                    self.scopes[self.current].vars.insert(name.clone(), ty);
                    if let Some(value) = value {
                        let value = self.check_expr(value, Some(ty));
                        if value.ty != ty {
                            return self.type_mismatch(ty, value.ty, stmt.span);
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
                    self.scopes[self.current]
                        .vars
                        .insert(name.clone(), value.ty);

                    CheckedStmt::Let {
                        name,
                        ty: value.ty,
                        mutable,
                        value: Some(value),
                    }
                } else {
                    return self.error(Error::new("cannot infer type", stmt.span));
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

                let ty = self.insert_type(Type::Function {
                    params: header
                        .params
                        .iter()
                        .map(|param| (param.name.clone(), param.ty))
                        .collect(),
                    ret: header.ret,
                });

                self.scopes[self.current]
                    .vars
                    .insert(header.name.clone(), ty);
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
                    self.scopes[self.current].vars.insert(name.clone(), ty);

                    let value = self.check_expr(value, Some(ty));
                    if value.ty != ty {
                        return self.type_mismatch(ty, value.ty, stmt.span);
                    }

                    CheckedStmt::Static {
                        public,
                        name,
                        ty,
                        value,
                    }
                } else {
                    let value = self.check_expr(value, None);
                    self.scopes[self.current]
                        .vars
                        .insert(name.clone(), value.ty);

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
        let span = expr.span;
        match expr.data {
            Expr::Binary { op, left, right } => {
                let left = self.check_expr(*left, target);
                let right = self.check_expr(*right, Some(left.ty));

                if left.ty != right.ty {
                    self.type_mismatch(left.ty, right.ty, span)
                } else if !self.types[left.ty].supports_binop(op) {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for values of type {} and {}",
                            self.type_name(left.ty),
                            self.type_name(right.ty)
                        ),
                        span,
                    ))
                } else {
                    CheckedExpr::new(
                        match op {
                            BinaryOp::NoneCoalesce => todo!(),
                            BinaryOp::ErrCoalesce => todo!(),
                            BinaryOp::Gt
                            | BinaryOp::GtEqual
                            | BinaryOp::Lt
                            | BinaryOp::LtEqual
                            | BinaryOp::Equal
                            | BinaryOp::NotEqual
                            | BinaryOp::LogicalOr
                            | BinaryOp::LogicalAnd => self.find_type_in_scope("bool", 0).unwrap(),
                            _ => left.ty,
                        },
                        ExprData::Binary {
                            op,
                            left: left.into(),
                            right: right.into(),
                        },
                    )
                }
            }
            Expr::Unary { op, expr } => {
                use UnaryOp::*;

                let rhs = self.check_expr(*expr, target);
                let matches = match &self.types[rhs.ty] {
                    Type::Int(_) => matches!(
                        op,
                        Plus | Neg
                            | Not
                            | PostIncrement
                            | PostDecrement
                            | PreIncrement
                            | PreDecrement
                    ),
                    Type::Uint(_) => matches!(
                        op,
                        Plus | PostIncrement | PostDecrement | PreIncrement | PreDecrement | Not
                    ),
                    Type::F32 | Type::F64 => matches!(op, Plus | Neg),
                    Type::Bool => matches!(op, Not),
                    _ => false,
                };

                if matches!(rhs.data, ExprData::Unsigned(_) | ExprData::Signed(_))
                    && matches!(
                        op,
                        PostIncrement | PostDecrement | PreIncrement | PreDecrement
                    )
                {
                    self.error(Error::new(
                        format!("operator '{op}' cannot be used on literals"),
                        span,
                    ))
                } else if matches {
                    CheckedExpr::new(
                        rhs.ty,
                        ExprData::Unary {
                            op,
                            expr: rhs.into(),
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for value of type {}",
                            self.type_name(rhs.ty)
                        ),
                        span,
                    ))
                }
            }
            Expr::Call { callee, args } => {
                let callee = self.check_expr(*callee, None);
                if let Type::Function { params, ret } = &self.types[callee.ty] {
                    // TODO: default arguments
                    if params.len() != args.len() {
                        return self.error(Error::new(
                            format!("expected {} arguments, found {}", params.len(), args.len()),
                            span,
                        ));
                    }

                    let ret = *ret;
                    let params = params.clone();
                    let mut result_args = Vec::with_capacity(args.len());
                    // TODO: keyword arguments
                    for ((_, ptype), (_, expr)) in params.into_iter().zip(args.into_iter()) {
                        let expr = self.check_expr(expr, Some(ptype));
                        if expr.ty != ptype {
                            return self.type_mismatch(ptype, expr.ty, span);
                        }
                        result_args.push(expr);
                    }

                    CheckedExpr::new(
                        ret,
                        ExprData::Call {
                            callee: callee.into(),
                            args: result_args,
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!("cannot call value of type {}", self.type_name(callee.ty)),
                        span,
                    ))
                }
            }
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
                    Type::Int(bits) => {
                        let result = match i128::from_str_radix(&value, base as u32) {
                            Ok(result) => result,
                            Err(_) => {
                                return self.error(Error::new(
                                    "Integer literal is too large for any type.",
                                    expr.span,
                                ));
                            }
                        };

                        if result >= 1 << (bits - 1) {
                            return self.error(Error::new(
                                "Integer literal is larger than its type allows",
                                expr.span,
                            ));
                        }
                        if result <= -(1 << (bits - 1)) {
                            return self.error(Error::new(
                                "Integer literal is smaller than its type allows",
                                expr.span,
                            ));
                        }

                        CheckedExpr::new(ty, ExprData::Signed(result))
                    }
                    Type::Uint(bits) => {
                        let result = match u128::from_str_radix(&value, base as u32) {
                            Ok(result) => result,
                            Err(_) => {
                                return self.error(Error::new(
                                    "Integer literal is too large for any type.",
                                    expr.span,
                                ));
                            }
                        };

                        if result >= 1 << bits {
                            return self.error(Error::new(
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
            Expr::Symbol(name) => {
                if let Some(ty) = self.find_var(&name) {
                    CheckedExpr::new(ty, ExprData::Symbol(name))
                } else {
                    self.error(Error::new(format!("undefined variable: {name}"), span))
                }
            }
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
                Type::Int(_) | Type::Uint(_) => return true,
                _ => {}
            }
        } else if ty == self.find_type_in_scope("{float}", 0).unwrap() {
            match self.types[target] {
                Type::F32 | Type::F64 => return true,
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

    fn resolve_type(&mut self, ty: &TypeHint) -> TypeId {
        match ty {
            TypeHint::Regular { name, .. } => self.find_type(name),
            TypeHint::Array(_, _) => self.find_type_in_scope("Array", 0),
            TypeHint::Slice(_) => self.find_type_in_scope("Slice", 0),
            TypeHint::Tuple(_) => todo!(),
            TypeHint::Map(_, _) => self.find_type_in_scope("Map", 0),
            TypeHint::Option(_) => self.find_type_in_scope("Option", 0),
            TypeHint::Result(_, _) => self.find_type_in_scope("Result", 0),
            TypeHint::Anon(ty) => Some(self.resolve_usertype(ty)),
            TypeHint::Void => self.find_type_in_scope("void", 0),
            TypeHint::Ref(_) | TypeHint::RefMut(_) | TypeHint::This => unreachable!(),
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

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.errors.push(error);
        T::default()
    }

    fn type_mismatch<T: Default>(&mut self, a: TypeId, b: TypeId, span: Span) -> T {
        return self.error(Error::new(
            format!(
                "type mismatch: expected type {}, got {}",
                self.type_name(a),
                self.type_name(b)
            ),
            span,
        ));
    }

    //

    fn type_name(&self, ty: TypeId) -> String {
        match &self.types[ty] {
            Type::Void => "void".into(),
            Type::Never => todo!(),
            Type::Int(bits) => format!("i{bits}"),
            Type::Uint(bits) => format!("u{bits}"),
            Type::F32 => "f32".into(),
            Type::F64 => "f64".into(),
            Type::Bool => "bool".into(),
            Type::IntGeneric => "{integer}".into(),
            Type::FloatGeneric => "{float}".into(),
            Type::Struct(_) => todo!(),
            Type::Union { tag, base } => todo!(),
            Type::Enum {} => todo!(),
            Type::Interface {} => todo!(),
            Type::Function { params, ret } => {
                let mut result = "Fn(".to_owned();
                for (i, (name, ty)) in params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&format!("{name}: "));
                    result.push_str(&self.type_name(*ty));
                }
                format!("{result}) {}", self.type_name(*ret))
            }
        }
    }

    fn find_type_in_scope(&self, name: &str, mut id: ScopeId) -> Option<TypeId> {
        loop {
            let scope = &self.scopes[id];
            if let Some(ty) = scope.types.get(name) {
                return Some(*ty);
            }

            id = scope.parent?;
        }
    }

    fn find_var_in_scope(&self, name: &str, mut id: ScopeId) -> Option<TypeId> {
        loop {
            let scope = &self.scopes[id];
            if let Some(ty) = scope.vars.get(name) {
                return Some(*ty);
            }

            id = scope.parent?;
        }
    }

    fn find_type(&self, name: &str) -> Option<TypeId> {
        self.find_type_in_scope(name, self.current)
    }

    fn find_var(&self, name: &str) -> Option<TypeId> {
        self.find_var_in_scope(name, self.current)
    }

    fn insert_type_in_scope(&mut self, name: String, data: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(data);
        self.scopes[self.current].types.insert(name, id);
        id
    }

    fn insert_type(&mut self, data: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(data);
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
