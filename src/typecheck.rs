use std::collections::{hash_map::Entry, HashMap};

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
        Block,
    },
    lexer::{Located, Span},
    scope::{Scope, ScopeId, ScopeKind, Scopes, Variable},
    Error,
};

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub ty: TypeId,
}

#[derive(Debug)]
pub struct ResolvedStruct {
    pub members: HashMap<String, Member>,
    pub name: String,
    pub scope: ScopeId,
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub enum TypeId {
    #[default]
    Unknown,
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
    fn strip_references(&self) -> &TypeId {
        let mut id = self;
        while let TypeId::Ref(inner) | TypeId::RefMut(inner) = id {
            id = inner;
        }
        id
    }
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
    Temporary,
}

pub struct CheckedAst {
    pub types: Vec<Type>,
    pub scopes: Scopes,
    pub stmt: CheckedStmt,
}

#[derive(Default)]
pub struct TypeChecker {
    types: Vec<Type>,
    errors: Vec<Error>,
    scopes: Scopes,
    current: ScopeId,
}

impl TypeChecker {
    pub fn check(stmt: Located<Stmt>) -> (CheckedAst, Vec<Error>) {
        // we depend on parser wrapping up the generated code in a Stmt::Module
        let mut this = Self::default();
        this.scopes.push(Scope::default());
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
            Stmt::Module {
                public: _,
                name,
                body,
            } => CheckedStmt::Module {
                name: name.clone(),
                body: self.create_block(Some(name), body, ScopeKind::Module),
            },
            Stmt::UserType(data) => match data {
                UserType::Struct(base) => {
                    let id = self.insert_type_in_scope(base.name.clone(), Type::Temporary);
                    self.enter_scope(Some(base.name.clone()), ScopeKind::UserType(id), |this| {
                        let mut rs_members = HashMap::with_capacity(base.members.len());
                        let mut members = Vec::new();
                        for (name, member) in base.members {
                            let ty = this.resolve_type(&member.ty);
                            rs_members.insert(
                                name.clone(),
                                Member {
                                    public: member.public,
                                    ty: ty.clone(),
                                },
                            );

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
                                name,
                                ty,
                                value,
                            });
                        }

                        this.types[id] = Type::Struct(ResolvedStruct {
                            members: rs_members,
                            name: base.name.clone(),
                            scope: this.current,
                        });

                        let functions = base
                            .functions
                            .into_iter()
                            .map(|f| this.check_fn(f))
                            .collect();

                        CheckedStmt::UserType(CheckedUserType::Struct(CheckedStruct {
                            public: base.public,
                            name: base.name,
                            members,
                            functions,
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

        #[rustfmt::skip]
        macro_rules! eval_member_to_struct {
            ($source: ident) => {{
                let id = $source.ty.strip_references();
                let TypeId::Type(ty) = id else {
                    return self.error(Error::new(
                        format!("type {} has no members",
                        self.type_name(id)
                    ), span));
                };

                let Type::Struct(s) = &self.types[*ty] else {
                    return self.error(Error::new(
                        format!("cannot get member of type {}",
                        self.type_name(id)
                    ), span));
                };

                (s, id)
            }};
        }

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
                        if !self.can_addrmut(&expr) {
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
                if let Expr::Member { source, member } = callee.data {
                    let source = self.check_expr(*source, None);
                    let (s, id) = eval_member_to_struct!(source);
                    if let Some(f) = self.scopes[s.scope].vars.get(&member) {
                        let Type::Function { params, ret } = ({
                            let TypeId::Type(id) = &f.ty else { unreachable!() };
                            &self.types[*id]
                        }) else { unreachable!() };

                        if let Some((_, ty)) = params.get(0).filter(|(name, _)| name == "$self") {
                            if let TypeId::RefMut(inner) = ty {
                                let mut ty = &source.ty;
                                if ty == inner.as_ref() && !self.can_addrmut(&source) {
                                    return self.error(Error::new(
                                        format!(
                                            "cannot call method '{member}' with immutable receiver"
                                        ),
                                        span,
                                    ));
                                } else {
                                    while let TypeId::RefMut(inner) = ty {
                                        ty = inner;
                                    }

                                    if matches!(ty, TypeId::Ref(_)) {
                                        return self.error(Error::new(
                                            format!(
                                                "cannot call method '{member}' from behind an immutable pointer"
                                            ),
                                            span,
                                        ));
                                    }
                                }
                            }

                            let ty = id.clone();
                            return CheckedExpr::new(
                                ret.clone(),
                                ExprData::MemberCall {
                                    source: source.into(),
                                    member,
                                    ty,
                                    args: self.check_fn_args(args, params.clone(), span),
                                },
                            );
                        }
                    }

                    self.error(Error::new(
                        format!("no method '{member}' found on type {}", self.type_name(id)),
                        span,
                    ))
                } else {
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
                            return CheckedExpr::new(
                                ret,
                                ExprData::Call {
                                    callee: callee.into(),
                                    args: self.check_fn_args(args, params, span),
                                },
                            );
                        }
                    }

                    self.error(Error::new(
                        format!("cannot call value of type {}", self.type_name(&callee.ty)),
                        span,
                    ))
                }
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
                if let Some((id, var)) = self.scopes.find_var(self.current, &name) {
                    CheckedExpr::new(
                        var.ty.clone(),
                        ExprData::Symbol {
                            scope: (id != self.current).then_some(id),
                            symbol: name,
                        },
                    )
                } else {
                    self.error(Error::new(format!("undefined variable: {name}"), span))
                }
            }
            Expr::Instance {
                name,
                members: arguments,
            } => {
                let Some(id) = self.scopes.find_type(self.current, &name).cloned() else {
                    return self.undefined_type(&name, span);
                };

                let TypeId::Type(ty) = id else {
                    return self.error(Error::new(
                        format!("cannot construct an instance of type {}", 
                        self.type_name(&id)
                    ), span));
                };

                let Type::Struct(s) = &self.types[ty] else {
                    return self.error(Error::new(
                        format!("cannot construct an instance of type {}", 
                        self.type_name(&id)
                    ), span));
                };

                let members = s.members.clone();
                if members.iter().any(|member| !member.1.public)
                    && !self.scopes.is_sub_scope(self.current, s.scope)
                {
                    self.error::<()>(Error::new(
                        "cannot construct type with private members",
                        span,
                    ));

                    return CheckedExpr::new(id, ExprData::Error);
                }

                let mut checked: HashMap<_, _> =
                    s.members.keys().map(|name| (name.clone(), None)).collect();
                for (name, value) in arguments {
                    if let Entry::Occupied(mut entry) = checked.entry(name.clone()) {
                        if entry.get().is_some() {
                            self.error::<()>(Error::new(
                                format!("field '{name}' declared multiple times"),
                                value.span,
                            ));
                        } else {
                            let span = value.span;
                            let value = self.check_expr(value, Some(&members[&name].ty));
                            if !self.coerces_to(&value.ty, &members[&name].ty) {
                                entry.insert(Some(self.type_mismatch(
                                    &members[&name].ty,
                                    &value.ty,
                                    span,
                                )));
                            } else {
                                entry.insert(Some(value));
                            }
                        }
                    } else {
                        self.error::<()>(Error::new(
                            format!("no field '{name}' on type {}", self.type_name(&id)),
                            value.span,
                        ));
                    }
                }

                CheckedExpr::new(
                    id,
                    ExprData::Instance {
                        members: checked
                            .into_iter()
                            .map(|(name, value)| {
                                let value = value.unwrap_or_else(|| {
                                    self.error(Error::new(format!("missing field: '{name}'"), span))
                                });
                                (name, value)
                            })
                            .collect(),
                    },
                )
            }
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
                let block = self.create_block(None, body, ScopeKind::Block(target.cloned()));
                let ScopeKind::Block(target) = &self.scopes[block.scope].kind else {
                    panic!("ICE: target of block changed from block to something else");
                };
                CheckedExpr::new(
                    target.as_ref().cloned().unwrap_or_default(),
                    ExprData::Block(block),
                )
            }
            Expr::If {
                cond,
                if_branch,
                else_branch,
            } => {
                let cond = self.check_expr(*cond, Some(&TypeId::Bool));
                let if_branch = self.check_expr(*if_branch, target);
                let else_branch = if let Some(e) = else_branch {
                    let else_branch = self.check_expr(*e, Some(&if_branch.ty));
                    if !self.coerces_to(&else_branch.ty, &if_branch.ty) {
                        return self.type_mismatch(&if_branch.ty, &else_branch.ty, span);
                    }

                    Some(else_branch)
                } else {
                    None
                };

                CheckedExpr::new(
                    if_branch.ty.clone(),
                    ExprData::If {
                        cond: cond.into(),
                        if_branch: if_branch.into(),
                        else_branch: else_branch.map(|e| e.into()),
                    },
                )
            }
            Expr::Loop { .. } => todo!(),
            Expr::For { .. } => todo!(),
            Expr::Member { source, member } => {
                let source = self.check_expr(*source, None);
                let (s, id) = eval_member_to_struct!(source);
                if let Some(t) = s.members.get(&member) {
                    if !t.public && !self.scopes.is_sub_scope(self.current, s.scope) {
                        return self.error(Error::new(
                            format!(
                                "cannot access private member '{member}' of type {}",
                                self.type_name(id)
                            ),
                            span,
                        ));
                    }

                    CheckedExpr::new(
                        t.ty.clone(),
                        ExprData::Member {
                            source: source.into(),
                            member,
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!(
                            "type {} has no member '{member}'",
                            self.type_name(&source.ty)
                        ),
                        span,
                    ))
                }
            }
            Expr::Subscript { .. } => todo!(),
            Expr::Return(expr) => {
                let Some(target) = self.scopes.iter_from(self.current).find_map(|(_, scope)| {
                    if let ScopeKind::Function(id) = &scope.kind {
                        let Type::Function { ret, .. } = &self.types[*id] else { unreachable!() };
                        Some(ret.clone())
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
                let ScopeKind::Block(target) = self.scopes[self.current].kind.clone() else {
                    return self.error(Error::new("yield outside of block", span));
                };

                let expr = self.check_expr(*expr, target.as_ref());
                if let Some(target) = &target {
                    if !self.coerces_to(&expr.ty, target) {
                        return self.type_mismatch(target, &expr.ty, span);
                    }
                } else {
                    self.scopes[self.current].kind = ScopeKind::Block(Some(expr.ty.clone()));
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
            name: name.clone(),
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

        self.scopes[self.current].vars.insert(
            header.name.clone(),
            Variable {
                ty: TypeId::Type(ty),
                mutable: false,
            },
        );
        CheckedFn {
            body: self.create_block_with(Some(name), body, ScopeKind::Function(ty), |this| {
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

    fn check_fn_args(
        &mut self,
        args: Vec<(Option<String>, Located<Expr>)>,
        params: Vec<(String, TypeId)>,
        span: Span,
    ) -> Vec<CheckedExpr> {
        let mut result_args = Vec::with_capacity(args.len());
        // TODO: keyword arguments
        for ((_, ptype), (_, expr)) in params.into_iter().zip(args.into_iter()) {
            let expr = self.check_expr(expr, Some(&ptype));
            if !self.coerces_to(&expr.ty, &ptype) {
                result_args.push(self.type_mismatch(&ptype, &expr.ty, span));
            } else {
                result_args.push(expr);
            }
        }

        result_args
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
                return self
                    .scopes
                    .find_type(self.current, &name.data)
                    .cloned()
                    .or_else(|| match name.data.as_str() {
                        "void" => Some(TypeId::Void),
                        "never" => Some(TypeId::Never),
                        "f32" => Some(TypeId::F32),
                        "f64" => Some(TypeId::F64),
                        "bool" => Some(TypeId::Bool),
                        _ => Self::match_int_type(&name.data),
                    })
                    .unwrap_or_else(|| self.undefined_type(&name.data, name.span));
            }
            TypeHint::Void => TypeId::Void,
            TypeHint::Ref(ty) => TypeId::Ref(self.resolve_type(ty).into()),
            TypeHint::RefMut(ty) => TypeId::RefMut(self.resolve_type(ty).into()),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                self.scopes
                    .iter_from(self.current)
                    .find_map(|(_, scope)| {
                        if let ScopeKind::UserType(id) = &scope.kind {
                            Some(TypeId::Ref(TypeId::Type(*id).into()))
                        } else {
                            None
                        }
                    })
                    .expect("ICE: this outside of method")
            }
            TypeHint::MutThis => self
                .scopes
                .iter_from(self.current)
                .find_map(|(_, scope)| {
                    if let ScopeKind::UserType(id) = &scope.kind {
                        Some(TypeId::RefMut(TypeId::Type(*id).into()))
                    } else {
                        None
                    }
                })
                .expect("ICE: this outside of method"),
            _ => todo!(),
        }
    }

    fn create_block(
        &mut self,
        name: Option<String>,
        body: Vec<Located<Stmt>>,
        target: ScopeKind,
    ) -> Block {
        self.enter_scope(name, target, |this| Block {
            body: body.into_iter().map(|stmt| this.check_stmt(stmt)).collect(),
            scope: this.current,
        })
    }

    fn create_block_with(
        &mut self,
        name: Option<String>,
        body: Vec<Located<Stmt>>,
        target: ScopeKind,
        f: impl FnOnce(&mut Self),
    ) -> Block {
        self.enter_scope(name, target, |this| {
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

    fn type_mismatch<T: Default>(&mut self, expected: &TypeId, actual: &TypeId, span: Span) -> T {
        self.error(Error::new(
            format!(
                "type mismatch: expected type {}, got {}",
                self.type_name(expected),
                self.type_name(actual),
            ),
            span,
        ))
    }

    fn undefined_type<T: Default>(&mut self, name: &str, span: Span) -> T {
        self.error(Error::new(format!("undefined type: {name}"), span))
    }

    //

    fn is_assignable(&self, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, TypeId::RefMut(_))
            }
            ExprData::Symbol { symbol, .. } => {
                self.scopes
                    .find_var(self.current, symbol)
                    .unwrap()
                    .1
                    .mutable
            }
            ExprData::Member { source, .. } => {
                matches!(source.ty, TypeId::RefMut(_)) || self.can_addrmut(source)
            }
            ExprData::Subscript { .. } => todo!(),
            _ => false,
        }
    }

    fn can_addrmut(&self, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                !matches!(op, UnaryOp::Deref) || matches!(expr.ty, TypeId::RefMut(_))
            }
            ExprData::Symbol { symbol, .. } => {
                self.scopes
                    .find_var(self.current, symbol)
                    .unwrap()
                    .1
                    .mutable
            }
            ExprData::Member { source, .. } => {
                matches!(source.ty, TypeId::RefMut(_)) || self.can_addrmut(source)
            }
            ExprData::Subscript { callee, .. } => self.can_addrmut(callee),
            _ => true,
        }
    }

    fn type_name(&self, ty: &TypeId) -> String {
        match ty {
            TypeId::Void => "void".into(),
            TypeId::Never => todo!(),
            TypeId::Int(bits) => format!("i{bits}"),
            TypeId::Uint(bits) => format!("u{bits}"),
            TypeId::Unknown => "{unknown}".into(),
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
                Type::Struct(base) => base.name.clone(),
                Type::Temporary => panic!("ICE: Type::Temporary in type_name"),
            },
        }
    }

    fn insert_type_in_scope(&mut self, name: String, data: Type) -> usize {
        let id = self.types.len();
        self.types.push(data);
        self.scopes[self.current]
            .types
            .insert(name, TypeId::Type(id));
        id
    }

    fn insert_type(&mut self, data: Type) -> usize {
        let id = self.types.len();
        self.types.push(data);
        id
    }

    fn enter_scope<T>(
        &mut self,
        name: Option<String>,
        target: ScopeKind,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.scopes.push(Scope {
            parent: Some(self.current),
            types: Default::default(),
            vars: Default::default(),
            kind: target,
            name,
        });

        let prev = self.current;
        self.current = self.scopes.len() - 1;
        let result = f(self);
        self.current = prev;
        result
    }
}
