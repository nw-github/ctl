use std::{collections::HashMap, vec};

use crate::{
    ast::{
        expr::{BinaryOp, Expr, UnaryOp},
        stmt::{Fn, FnDecl, MemVar, Stmt, Struct, TypeHint, UserType},
    },
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::{
            CheckedFn, CheckedFnDecl, CheckedMemVar, CheckedParam, CheckedStmt, CheckedStruct,
            CheckedUserType,
        },
        Block, ScopeId,
    },
    lexer::{Located, Span},
    Error,
};

pub type TypeId = usize;

#[derive(Debug)]
pub struct Member {
    pub public: bool,
    pub name: String,
    pub ty: TypeId,
}

#[derive(Debug)]
pub struct ResolvedStruct {
    pub members: Vec<Member>,
    pub name: String,
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
    Ref(TypeId),
    RefMut(TypeId),
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

#[derive(Default, Debug)]
pub enum Target {
    Block(Option<TypeId>),
    Function(TypeId),
    UserType(TypeId),
    #[default]
    None,
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Variable {
    ty: TypeId,
    mutable: bool,
}

#[derive(Default, Debug)]
pub struct Scope {
    parent: Option<ScopeId>,
    vars: HashMap<String, Variable>,
    types: HashMap<String, TypeId>,
    target: Target,
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
        this.insert_type_in_scope("never".into(), Type::Never);
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
                body: self.create_block(body, Target::None),
            },
            Stmt::UserType(data) => match data {
                UserType::Struct(base) => {
                    let id = ResolvedStruct {
                        members: base
                            .members
                            .iter()
                            .map(|member| Member {
                                public: member.public,
                                name: member.name.clone(),
                                ty: self.resolve_type(&member.ty),
                            })
                            .collect(),
                        name: base.name.clone(),
                    };
                    let id = self.insert_type_in_scope(base.name.clone(), Type::Struct(id));
                    self.enter_scope(Target::UserType(id), |this| {
                        let mut members = Vec::new();
                        for member in base.members {
                            let ty = this.resolve_type(&member.ty);
                            let value = if let Some(value) = member.value {
                                let span = value.span;
                                let expr = this.check_expr(value, Some(ty));
                                if ty != expr.ty {
                                    return this.type_mismatch(ty, expr.ty, span);
                                }

                                Some(expr)
                            } else {
                                None
                            };

                            members.push(CheckedMemVar {
                                public: member.public,
                                name: member.name,
                                ty,
                                value,
                            });
                        }

                        CheckedStmt::UserType(CheckedUserType::Struct(CheckedStruct {
                            public: base.public,
                            name: base.name,
                            members,
                            functions: base
                                .functions
                                .into_iter()
                                .map(|f| this.check_fn(f))
                                .collect(),
                        }))
                    })
                }
                UserType::Union { .. } => todo!(),
                UserType::Interface { .. } => todo!(),
                UserType::Enum { .. } => todo!(),
            },
            Stmt::Expr(expr) => CheckedStmt::Expr(self.check_expr(expr, None)),
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(&ty);
                    self.scopes[self.current]
                        .vars
                        .insert(name.clone(), Variable { ty, mutable });
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
                    self.scopes[self.current].vars.insert(
                        name.clone(),
                        Variable {
                            ty: value.ty,
                            mutable,
                        },
                    );

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
            Stmt::Fn(f) => CheckedStmt::Fn(self.check_fn(f)),
            Stmt::Static {
                public,
                name,
                ty,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(&ty);
                    self.scopes[self.current]
                        .vars
                        .insert(name.clone(), Variable { ty, mutable: false });

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
                    self.scopes[self.current].vars.insert(
                        name.clone(),
                        Variable {
                            ty: value.ty,
                            mutable: false,
                        },
                    );

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

                let value_span = expr.span;
                let expr = self.check_expr(*expr, target);
                let mut out_ty = expr.ty;
                let valid = match &self.types[expr.ty] {
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
                    Type::Ref(inner) | Type::RefMut(inner) => {
                        if matches!(op, Deref) {
                            out_ty = *inner;
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                };

                if !self.is_assignable(&expr)
                    && matches!(
                        op,
                        PostIncrement | PostDecrement | PreIncrement | PreDecrement
                    )
                {
                    self.error(Error::new(format!("expression is not assignable"), value_span))
                } else if valid {
                    CheckedExpr::new(
                        out_ty,
                        ExprData::Unary {
                            op,
                            expr: expr.into(),
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for value of type {}",
                            self.type_name(expr.ty)
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
                if let Some(var) = self.find_var(&name) {
                    CheckedExpr::new(var.ty, ExprData::Symbol(name))
                } else {
                    self.error(Error::new(format!("undefined variable: {name}"), span))
                }
            }
            Expr::Instance { name, members } => todo!(),
            Expr::None => todo!(),
            Expr::Assign {
                target: lhs,
                binary,
                value,
            } => {
                // TODO: check the binary ops, like += -= etc..
                let span = lhs.span;
                let lhs = self.check_expr(*lhs, None);
                if !self.is_assignable(&lhs) {
                    // TODO: report a better error here
                    return self.error(Error::new("expression is not assignable", span));
                }

                let rhs = self.check_expr(*value, Some(lhs.ty));
                if lhs.ty != rhs.ty {
                    return self.type_mismatch(lhs.ty, rhs.ty, span);
                }

                CheckedExpr::new(
                    lhs.ty,
                    ExprData::Assign {
                        target: lhs.into(),
                        binary,
                        value: rhs.into(),
                    },
                )
            }
            Expr::Block(body) => {
                let block = self.create_block(body, Target::Block(target));
                let Target::Block(target) = &self.scopes[block.scope].target else {
                    panic!("ICE: target of block changed from block to something else");
                };
                CheckedExpr::new(
                    target.unwrap_or(self.find_type_in_scope("void", 0).unwrap()),
                    ExprData::Block(block),
                )
            }
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
            Expr::Return(expr) => {
                let Some(target) = self.traverse_scopes(self.current, |scope| {
                    if let Target::Function(id) = scope.target {
                        Some(id)
                    } else {
                        None
                    }
                }) else {
                    // the parser ensures return only happens inside functions
                    return self.error(Error::new("return outside of function", span));
                };

                let expr = self.check_expr(*expr, Some(target));
                if expr.ty != target {
                    self.type_mismatch(target, expr.ty, span)
                } else {
                    CheckedExpr::new(
                        self.find_type_in_scope("never", 0).unwrap(),
                        ExprData::Return(expr.into()),
                    )
                }
            }
            Expr::Yield(expr) => {
                let Target::Block(target) = self.scopes[self.current].target else {
                    return self.error(Error::new("yield outside of block", span));
                };

                let expr = self.check_expr(*expr, target);
                if let Some(target) = target {
                    if target != expr.ty {
                        return self.type_mismatch(target, expr.ty, span);
                    }
                } else {
                    self.scopes[self.current].target = Target::Block(Some(expr.ty));
                }

                CheckedExpr::new(expr.ty, ExprData::Yield(expr.into()))
            }
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
        } else if let Type::RefMut(inner) = self.types[ty] {
            match self.types[target] {
                Type::Ref(target) if target == inner => return true,
                _ => {}
            }
        }

        false
    }

    fn check_fn(
        &mut self,
        Fn {
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
        }: Fn,
    ) -> CheckedFn {
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
            .insert(header.name.clone(), Variable { ty, mutable: false });
        CheckedFn {
            body: self.create_block_with(body, Target::Function(header.ret), |this| {
                for param in header.params.iter() {
                    this.scopes[this.current].vars.insert(
                        param.name.clone(),
                        Variable {
                            ty: param.ty,
                            mutable: param.mutable,
                        },
                    );
                }
            }),
            header,
        }
    }

    fn resolve_type(&mut self, ty: &TypeHint) -> TypeId {
        match ty {
            TypeHint::Regular { name, .. } => self.find_type(name),
            TypeHint::Void => self.find_type_in_scope("void", 0),
            TypeHint::Ref(_) | TypeHint::RefMut(_) => unreachable!(),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                self.traverse_scopes(self.current, |scope| {
                    if let Target::UserType(id) = scope.target {
                        Some(id)
                    } else {
                        None
                    }
                })
            }
            _ => todo!(),
        }
        .unwrap()
    }

    fn create_block(&mut self, body: Vec<Located<Stmt>>, target: Target) -> Block {
        self.enter_scope(target, |this| Block {
            body: body.into_iter().map(|stmt| this.check_stmt(stmt)).collect(),
            scope: this.current,
        })
    }

    fn create_block_with(
        &mut self,
        body: Vec<Located<Stmt>>,
        target: Target,
        f: impl FnOnce(&mut Self),
    ) -> Block {
        self.enter_scope(target, |this| {
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
        self.error(Error::new(
            format!(
                "type mismatch: expected type {}, got {}",
                self.type_name(a),
                self.type_name(b)
            ),
            span,
        ))
    }

    //

    fn is_assignable(&self, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(self.types[expr.ty], Type::RefMut(_))
            }
            ExprData::Symbol(name) => self.find_var(name).unwrap().mutable,
            ExprData::Member { .. } => todo!(),
            ExprData::Subscript { .. } => todo!(),
            _ => false,
        }
    }

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
            Type::Ref(id) => format!("*{}", self.type_name(*id)),
            Type::RefMut(id) => format!("*mut {}", self.type_name(*id)),
        }
    }

    fn traverse_scopes<T>(
        &self,
        mut id: ScopeId,
        mut f: impl FnMut(&Scope) -> Option<T>,
    ) -> Option<T> {
        loop {
            let scope = &self.scopes[id];
            if let Some(item) = f(scope) {
                return Some(item);
            }

            id = scope.parent?;
        }
    }

    fn find_type_in_scope(&self, name: &str, id: ScopeId) -> Option<TypeId> {
        self.traverse_scopes(id, |scope| scope.types.get(name).copied())
    }

    fn find_var_in_scope(&self, name: &str, id: ScopeId) -> Option<Variable> {
        self.traverse_scopes(id, |scope| scope.vars.get(name).copied())
    }

    fn find_type(&self, name: &str) -> Option<TypeId> {
        self.find_type_in_scope(name, self.current)
    }

    fn find_var(&self, name: &str) -> Option<Variable> {
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

    fn enter_scope<T>(&mut self, target: Target, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scopes.push(Scope {
            parent: Some(self.current),
            types: Default::default(),
            vars: Default::default(),
            target,
        });

        let prev = self.current;
        self.current = self.scopes.len() - 1;
        let result = f(self);
        self.current = prev;
        result
    }
}
