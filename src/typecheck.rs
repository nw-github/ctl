use std::{collections::HashMap, vec};

use crate::{
    ast::{
        expr::{BinaryOp, Expr, UnaryOp},
        stmt::{Fn, FnDecl, Stmt, TypeHint, UserType},
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

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub enum TypeId {
    #[default]
    Void,
    Never,
    Int(u8),
    Uint(u8),
    F32,
    F64,
    Bool,
    IntGeneric,
    FloatGeneric,
    Type(usize),
    Ref(Box<TypeId>),
    RefMut(Box<TypeId>),
}

impl TypeId {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            TypeId::Int(_) | TypeId::Uint(_) | TypeId::F32 | TypeId::F64
        )
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
                matches!(
                    self,
                    TypeId::Int(_) | TypeId::Uint(_) | TypeId::F32 | TypeId::F64
                )
            }
            BinaryOp::And | BinaryOp::Xor | BinaryOp::Or | BinaryOp::Shl | BinaryOp::Shr => {
                matches!(self, TypeId::Int(_) | TypeId::Uint(_))
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                matches!(
                    self,
                    TypeId::Int(_) | TypeId::Uint(_) | TypeId::F32 | TypeId::F64 | TypeId::Bool
                )
            }
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                matches!(self, TypeId::Bool)
            }
            BinaryOp::NoneCoalesce => todo!(),
            BinaryOp::ErrCoalesce => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Type {
    Function {
        params: Vec<(String, TypeId)>,
        ret: TypeId,
    },
    Struct(ResolvedStruct),
}

#[derive(Default, Debug, Clone)]
pub enum Target {
    Block(Option<TypeId>),
    Function(TypeId),
    UserType(TypeId),
    #[default]
    None,
}

#[derive(Default, Debug, Clone)]
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
                                let expr = this.check_expr(value, Some(&ty));
                                if !this.coerces_to(&expr.ty, &ty) { 
                                    return this.type_mismatch(&ty, &expr.ty, span);
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
                    self.scopes[self.current].vars.insert(
                        name.clone(),
                        Variable {
                            ty: ty.clone(),
                            mutable,
                        },
                    );
                    if let Some(value) = value {
                        let value = self.check_expr(value, Some(&ty));
                        if !self.coerces_to(&value.ty, &ty) {
                            return self.type_mismatch(&ty, &value.ty, stmt.span);
                        }

                        CheckedStmt::Let {
                            name,
                            mutable,
                            value: Ok(value),
                        }
                    } else {
                        CheckedStmt::Let {
                            name,
                            mutable,
                            value: Err(ty),
                        }
                    }
                } else if let Some(value) = value {
                    let value = self.check_expr(value, None);
                    self.scopes[self.current].vars.insert(
                        name.clone(),
                        Variable {
                            ty: value.ty.clone(),
                            mutable,
                        },
                    );

                    CheckedStmt::Let {
                        name,
                        mutable,
                        value: Ok(value),
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
                    self.scopes[self.current].vars.insert(
                        name.clone(),
                        Variable {
                            ty: ty.clone(),
                            mutable: false,
                        },
                    );

                    let value = self.check_expr(value, Some(&ty));
                    if !self.coerces_to(&value.ty, &ty) {
                        return self.type_mismatch(&ty, &value.ty, stmt.span);
                    }

                    CheckedStmt::Static {
                        public,
                        name,
                        value,
                    }
                } else {
                    let value = self.check_expr(value, None);
                    self.scopes[self.current].vars.insert(
                        name.clone(),
                        Variable {
                            ty: value.ty.clone(),
                            mutable: false,
                        },
                    );

                    CheckedStmt::Static {
                        public,
                        name,
                        value,
                    }
                }
            }
        }
    }

    fn check_expr(&mut self, expr: Located<Expr>, target: Option<&TypeId>) -> CheckedExpr {
        let span = expr.span;
        match expr.data {
            Expr::Binary { op, left, right } => {
                let left = self.check_expr(*left, target);
                let right = self.check_expr(*right, Some(&left.ty));

                if !self.coerces_to(&right.ty, &left.ty) {
                    self.type_mismatch(&left.ty, &right.ty, span)
                } else if !left.ty.supports_binop(op) {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for values of type {} and {}",
                            self.type_name(&left.ty),
                            self.type_name(&right.ty)
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
                            | BinaryOp::LogicalAnd => TypeId::Bool,
                            _ => left.ty.clone(),
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
                let mut out_ty = None;
                let valid = match op {
                    Plus => expr.ty.is_numeric(),
                    Neg => matches!(expr.ty, TypeId::Int(_) | TypeId::F32 | TypeId::F64),
                    PostIncrement | PostDecrement | PreIncrement | PreDecrement => {
                        if matches!(expr.ty, TypeId::Int(_) | TypeId::Uint(_)) {
                            if !self.is_assignable(&expr) {
                                return self
                                    .error(Error::new("expression is not assignable", value_span));
                            }

                            true
                        } else {
                            false
                        }
                    }
                    Not => matches!(expr.ty, TypeId::Int(_) | TypeId::Uint(_) | TypeId::Bool),
                    Deref => {
                        if let TypeId::Ref(inner) | TypeId::RefMut(inner) = &expr.ty {
                            out_ty = Some((**inner).clone());
                            true
                        } else {
                            false
                        }
                    }
                    Addr => {
                        out_ty = Some(TypeId::Ref(expr.ty.clone().into()));
                        true
                    }
                    AddrMut => {
                        if !self.is_mutable(&expr) {
                            return self.error(Error::new(
                                "cannot take address of immutable memory location",
                                span,
                            ));
                        }

                        out_ty = Some(TypeId::RefMut(expr.ty.clone().into()));
                        true
                    }
                    IntoError => todo!(),
                    Try => todo!(),
                    Sizeof => todo!(),
                };

                if valid {
                    CheckedExpr::new(
                        out_ty.unwrap_or_else(|| expr.ty.clone()),
                        ExprData::Unary {
                            op,
                            expr: expr.into(),
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for value of type {}",
                            self.type_name(&expr.ty)
                        ),
                        span,
                    ))
                }
            }
            Expr::Call { callee, args } => {
                let callee = self.check_expr(*callee, None);
                if let TypeId::Type(id) = callee.ty {
                    if let Type::Function { params, ret } = &self.types[id] {
                        // TODO: default arguments
                        if params.len() != args.len() {
                            return self.error(Error::new(
                                format!(
                                    "expected {} arguments, found {}",
                                    params.len(),
                                    args.len()
                                ),
                                span,
                            ));
                        }

                        let ret = ret.clone();
                        let params = params.clone();
                        let mut result_args = Vec::with_capacity(args.len());
                        // TODO: keyword arguments
                        for ((_, ptype), (_, expr)) in params.into_iter().zip(args.into_iter()) {
                            let expr = self.check_expr(expr, Some(&ptype));
                            if !self.coerces_to(&expr.ty, &ptype) {
                                return self.type_mismatch(&ptype, &expr.ty, span);
                            }
                            result_args.push(expr);
                        }

                        return CheckedExpr::new(
                            ret,
                            ExprData::Call {
                                callee: callee.into(),
                                args: result_args,
                            },
                        );
                    }
                }

                self.error(Error::new(
                    format!("cannot call value of type {}", self.type_name(&callee.ty)),
                    span,
                ))
            }
            Expr::Array(_) => todo!(),
            Expr::ArrayWithInit { .. } => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::Map(_) => todo!(),
            Expr::Bool(value) => CheckedExpr {
                ty: TypeId::Bool,
                data: ExprData::Bool(value),
            },
            Expr::Integer(base, value) => {
                // TODO: attempt to promote the literal if its too large for i32
                let ty = target
                    .filter(|target| self.coerces_to(&TypeId::IntGeneric, target))
                    .cloned()
                    .unwrap_or(TypeId::Int(32));

                match ty {
                    TypeId::Int(bits) => {
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
                    TypeId::Uint(bits) => {
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
            Expr::Float(value) => CheckedExpr::new(
                target
                    .filter(|target| self.coerces_to(&TypeId::FloatGeneric, target))
                    .cloned()
                    .unwrap_or(TypeId::F64),
                ExprData::Float(value),
            ),
            Expr::String(_) => todo!(),
            Expr::Symbol(name) => {
                if let Some(var) = self.find_var(&name) {
                    CheckedExpr::new(var.ty.clone(), ExprData::Symbol(name))
                } else {
                    self.error(Error::new(format!("undefined variable: {name}"), span))
                }
            }
            Expr::Instance { .. } => todo!(),
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

                let rhs = self.check_expr(*value, Some(&lhs.ty));
                if !self.coerces_to(&rhs.ty, &lhs.ty) {
                    return self.type_mismatch(&lhs.ty, &rhs.ty, span);
                }

                CheckedExpr::new(
                    lhs.ty.clone(),
                    ExprData::Assign {
                        target: lhs.into(),
                        binary,
                        value: rhs.into(),
                    },
                )
            }
            Expr::Block(body) => {
                let block = self.create_block(body, Target::Block(target.cloned()));
                let Target::Block(target) = &self.scopes[block.scope].target else {
                    panic!("ICE: target of block changed from block to something else");
                };
                CheckedExpr::new(
                    target.as_ref().cloned().unwrap_or_default(),
                    ExprData::Block(block),
                )
            }
            Expr::If { .. } => todo!(),
            Expr::Loop { .. } => todo!(),
            Expr::For { .. } => todo!(),
            Expr::Member { .. } => todo!(),
            Expr::Subscript { .. } => todo!(),
            Expr::Return(expr) => {
                let Some(target) = self.traverse_scopes(self.current, |scope| {
                    if let Target::Function(id) = &scope.target {
                        Some(id.clone())
                    } else {
                        None
                    }
                }) else {
                    // the parser ensures return only happens inside functions
                    return self.error(Error::new("return outside of function", span));
                };

                let expr = self.check_expr(*expr, Some(&target));
                if !self.coerces_to(&expr.ty, &target) {
                    self.type_mismatch(&target, &expr.ty, span)
                } else {
                    CheckedExpr::new(TypeId::Never, ExprData::Return(expr.into()))
                }
            }
            Expr::Yield(expr) => {
                let Target::Block(target) = self.scopes[self.current].target.clone() else {
                    return self.error(Error::new("yield outside of block", span));
                };

                let expr = self.check_expr(*expr, target.as_ref());
                if let Some(target) = &target {
                    if !self.coerces_to(&expr.ty, target) {
                        return self.type_mismatch(target, &expr.ty, span);
                    }
                } else {
                    self.scopes[self.current].target = Target::Block(Some(expr.ty.clone()));
                }

                CheckedExpr::new(TypeId::Never, ExprData::Yield(expr.into()))
            }
            Expr::Break(_) => todo!(),
            Expr::Range { .. } => todo!(),
            Expr::Continue => todo!(),
        }
    }

    fn coerces_to(&self, ty: &TypeId, target: &TypeId) -> bool {
        if matches!(ty, TypeId::IntGeneric) {
            match target {
                TypeId::Int(_) | TypeId::Uint(_) => return true,
                _ => {}
            }
        } else if matches!(ty, TypeId::FloatGeneric) {
            match target {
                TypeId::F32 | TypeId::F64 => return true,
                _ => {}
            }
        } else if let TypeId::RefMut(inner) = ty {
            match target {
                TypeId::Ref(target) if target == inner => return true,
                _ => {}
            }
        }

        ty == target
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
                .map(|param| (param.name.clone(), param.ty.clone()))
                .collect(),
            ret: header.ret.clone(),
        });

        self.scopes[self.current]
            .vars
            .insert(header.name.clone(), Variable { ty, mutable: false });
        CheckedFn {
            body: self.create_block_with(body, Target::Function(header.ret.clone()), |this| {
                for param in header.params.iter() {
                    this.scopes[this.current].vars.insert(
                        param.name.clone(),
                        Variable {
                            ty: param.ty.clone(),
                            mutable: param.mutable,
                        },
                    );
                }
            }),
            header,
        }
    }

    fn match_int_type(name: &str) -> Option<TypeId> {
        let mut chars = name.chars();
        let mut i = false;
        let result = match chars.next()? {
            'i' => {
                i = true;
                TypeId::Int
            }
            'u' => TypeId::Uint,
            _ => return None,
        };

        match (
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next(),
        ) {
            (Some(a), None, None, None) => (!i || a > 1).then_some(result(a as u8)),
            (Some(a), Some(b), None, None) => Some(result((a * 10 + b) as u8)),
            (Some(a), Some(b), Some(c), None) => Some(result((a * 100 + b * 10 + c) as u8)),
            _ => None,
        }
    }

    fn resolve_type(&mut self, ty: &TypeHint) -> TypeId {
        match ty {
            TypeHint::Regular { name, .. } => {
                self.find_type(name)
                    .cloned()
                    .unwrap_or_else(|| match name.as_str() {
                        "void" => TypeId::Void,
                        "never" => TypeId::Never,
                        "f32" => TypeId::F32,
                        "f64" => TypeId::F64,
                        "bool" => TypeId::Bool,
                        _ => Self::match_int_type(name).unwrap(),
                    })
            }
            TypeHint::Void => TypeId::Void,
            TypeHint::Ref(ty) => TypeId::Ref(self.resolve_type(ty).into()),
            TypeHint::RefMut(ty) => TypeId::RefMut(self.resolve_type(ty).into()),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                self.traverse_scopes(self.current, |scope| {
                    if let Target::UserType(id) = &scope.target {
                        Some(id)
                    } else {
                        None
                    }
                })
                .map(|ty| TypeId::Ref(ty.clone().into()))
                .unwrap()
            }
            _ => todo!(),
        }
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

    fn type_mismatch<T: Default>(&mut self, a: &TypeId, b: &TypeId, span: Span) -> T {
        self.error(Error::new(
            format!(
                "type mismatch: expected type {}, got {}",
                self.type_name(a),
                self.type_name(b),
            ),
            span,
        ))
    }

    //

    fn is_assignable(&self, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, TypeId::RefMut(_))
            }
            ExprData::Symbol(name) => self.find_var(name).unwrap().mutable,
            ExprData::Member { .. } => todo!(),
            ExprData::Subscript { .. } => todo!(),
            _ => false,
        }
    }

    fn is_mutable(&self, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => !matches!(op, UnaryOp::Deref) || self.is_mutable(expr),
            ExprData::Symbol(name) => self.find_var(name).unwrap().mutable,
            ExprData::Member { source, .. } => self.is_mutable(source),
            ExprData::Subscript { callee, .. } => self.is_mutable(callee),
            _ => true,
        }
    }

    fn type_name(&self, ty: &TypeId) -> String {
        match ty {
            TypeId::Void => "void".into(),
            TypeId::Never => todo!(),
            TypeId::Int(bits) => format!("i{bits}"),
            TypeId::Uint(bits) => format!("u{bits}"),
            TypeId::F32 => "f32".into(),
            TypeId::F64 => "f64".into(),
            TypeId::Bool => "bool".into(),
            TypeId::IntGeneric => "{integer}".into(),
            TypeId::FloatGeneric => "{float}".into(),
            TypeId::Ref(id) => format!("*{}", self.type_name(id)),
            TypeId::RefMut(id) => format!("*mut {}", self.type_name(id)),
            TypeId::Type(id) => match &self.types[*id] {
                Type::Function { params, ret } => {
                    let mut result = "Fn(".to_owned();
                    for (i, (name, ty)) in params.iter().enumerate() {
                        if i > 0 {
                            result.push_str(", ");
                        }

                        result.push_str(&format!("{name}: "));
                        result.push_str(&self.type_name(ty));
                    }
                    format!("{result}) {}", self.type_name(ret))
                }
                Type::Struct(_) => todo!(),
            },
        }
    }

    fn traverse_scopes<'a, T>(
        &'a self,
        mut id: ScopeId,
        mut f: impl FnMut(&'a Scope) -> Option<T>,
    ) -> Option<T> {
        loop {
            let scope = &self.scopes[id];
            if let Some(item) = f(scope) {
                return Some(item);
            }

            id = scope.parent?;
        }
    }

    fn find_type(&self, name: &str) -> Option<&TypeId> {
        self.traverse_scopes(self.current, |scope| scope.types.get(name))
    }

    fn find_var(&self, name: &str) -> Option<&Variable> {
        self.traverse_scopes(self.current, |scope| scope.vars.get(name))
    }

    fn insert_type_in_scope(&mut self, name: String, data: Type) -> TypeId {
        let id = TypeId::Type(self.types.len());
        self.types.push(data);
        self.scopes[self.current].types.insert(name, id.clone());
        id
    }

    fn insert_type(&mut self, data: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(data);
        TypeId::Type(id)
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
