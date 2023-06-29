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
    scope::{
        DefinedStruct, Function, FunctionId, Member, ScopeKind, Scopes, Struct, StructId, Variable,
    },
    Error,
};

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub enum TypeId {
    #[default]
    Unknown,
    Void,
    Never,
    Int(u8),
    Uint(u8),
    Isize,
    Usize,
    F32,
    F64,
    Bool,
    IntGeneric,
    FloatGeneric,
    Function(Box<FunctionId>),
    Struct(Box<StructId>),
    Ref(Box<TypeId>),
    RefMut(Box<TypeId>),
    Array(Box<(TypeId, usize)>),
}

impl TypeId {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            TypeId::Int(_)
                | TypeId::Uint(_)
                | TypeId::F32
                | TypeId::F64
                | TypeId::Isize
                | TypeId::Usize
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
                    TypeId::Int(_)
                        | TypeId::Isize
                        | TypeId::Uint(_)
                        | TypeId::Usize
                        | TypeId::F32
                        | TypeId::F64
                )
            }
            BinaryOp::And | BinaryOp::Xor | BinaryOp::Or | BinaryOp::Shl | BinaryOp::Shr => {
                matches!(
                    self,
                    TypeId::Int(_) | TypeId::Uint(_) | TypeId::Isize | TypeId::Usize
                )
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                matches!(
                    self,
                    TypeId::Int(_)
                        | TypeId::Isize
                        | TypeId::Uint(_)
                        | TypeId::Usize
                        | TypeId::F32
                        | TypeId::F64
                        | TypeId::Bool
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

pub struct CheckedAst {
    pub scopes: Scopes,
    pub stmt: CheckedStmt,
}

#[derive(Default)]
pub struct TypeChecker {
    errors: Vec<Error>,
}

macro_rules! type_check_bail {
    ($self: expr, $scopes: expr, $source: expr, $target: expr, $span: expr) => {
        if !$self.coerces_to($source, $target) {
            return $self.type_mismatch($scopes, $target, $source, $span);
        }
    };
}

macro_rules! type_check {
    ($self: expr, $scopes: expr, $source: expr, $target: expr, $span: expr) => {
        if !$self.coerces_to($source, $target) {
            $self.type_mismatch::<()>($scopes, $target, $source, $span)
        }
    };
}

impl TypeChecker {
    pub fn check(stmt: Located<Stmt>) -> (CheckedAst, Vec<Error>) {
        // we depend on parser wrapping up the generated code in a Stmt::Module
        let mut this = Self::default();
        let mut scopes = Scopes::new();
        let stmt = this.check_stmt(&mut scopes, stmt);
        (CheckedAst { scopes, stmt }, this.errors)
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

    fn check_stmt(&mut self, scopes: &mut Scopes, stmt: Located<Stmt>) -> CheckedStmt {
        match stmt.data {
            Stmt::Module {
                public: _,
                name,
                body,
            } => CheckedStmt::Module {
                name: name.clone(),
                body: self.create_block(scopes, Some(name), body, ScopeKind::Module),
            },
            Stmt::UserType(data) => match data {
                UserType::Struct(base) => {
                    let self_id = scopes.insert_type(Struct::Declared(base.name.clone()));
                    scopes.enter(
                        Some(base.name.clone()),
                        ScopeKind::Struct(self_id),
                        |scopes| {
                            let mut rs_members = HashMap::with_capacity(base.members.len());
                            let mut members = Vec::new();
                            for (name, member) in base.members {
                                let target = self.resolve_type(scopes, &member.ty);
                                rs_members.insert(
                                    name.clone(),
                                    Member {
                                        public: member.public,
                                        ty: target.clone(),
                                    },
                                );

                                let value = if let Some(value) = member.value {
                                    let span = value.span;
                                    let expr = self.check_expr(scopes, value, Some(&target));
                                    type_check!(self, scopes, &expr.ty, &target, span);

                                    Some(expr)
                                } else {
                                    None
                                };

                                members.push(CheckedMemVar {
                                    public: member.public,
                                    name,
                                    ty: target,
                                    value,
                                });
                            }

                            scopes[&self_id] = Struct::Defined(DefinedStruct {
                                members: rs_members,
                                name: base.name.clone(),
                                scope: scopes.current_id(),
                            });

                            CheckedStmt::UserType(CheckedUserType::Struct(CheckedStruct {
                                public: base.public,
                                name: base.name,
                                members,
                                functions: base
                                    .functions
                                    .into_iter()
                                    .map(|f| self.check_fn(scopes, f))
                                    .collect(),
                            }))
                        },
                    )
                }
                UserType::Union { .. } => todo!(),
                UserType::Interface { .. } => todo!(),
                UserType::Enum { .. } => todo!(),
            },
            Stmt::Expr(expr) => CheckedStmt::Expr(self.check_expr(scopes, expr, None)),
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty);
                    scopes.insert_var(
                        name.clone(),
                        Variable {
                            ty: ty.clone(),
                            is_static: false,
                            mutable,
                        },
                    );
                    if let Some(value) = value {
                        let span = value.span;
                        let value = self.check_expr(scopes, value, Some(&ty));
                        type_check!(self, scopes, &value.ty, &ty, span);

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
                    let value = self.check_expr(scopes, value, None);
                    scopes.insert_var(
                        name.clone(),
                        Variable {
                            ty: value.ty.clone(),
                            is_static: false,
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
            Stmt::Fn(f) => CheckedStmt::Fn(self.check_fn(scopes, f)),
            Stmt::Static {
                public,
                name,
                ty,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty);
                    scopes.insert_var(
                        name.clone(),
                        Variable {
                            ty: ty.clone(),
                            is_static: true,
                            mutable: false,
                        },
                    );

                    let span = value.span;
                    let value = self.check_expr(scopes, value, Some(&ty));
                    type_check!(self, scopes, &value.ty, &ty, span);

                    CheckedStmt::Static {
                        public,
                        name,
                        value,
                    }
                } else {
                    let value = self.check_expr(scopes, value, None);
                    scopes.insert_var(
                        name.clone(),
                        Variable {
                            ty: value.ty.clone(),
                            is_static: true,
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

    fn check_expr(
        &mut self,
        scopes: &mut Scopes,
        expr: Located<Expr>,
        target: Option<&TypeId>,
    ) -> CheckedExpr {
        let span = expr.span;

        #[rustfmt::skip]
        macro_rules! eval_member_to_struct {
            ($source: ident) => {{
                let mut id = &$source.ty;
                while let TypeId::Ref(inner) | TypeId::RefMut(inner) = id {
                    id = inner;
                }

                let TypeId::Struct(ty) = id else {
                    return self.error(Error::new(
                        format!("cannot get member of type {}",
                        Self::type_name(scopes, id)
                    ), span));
                };

                let Struct::Defined(d) = &scopes[ty.as_ref()] else { unreachable!() };
                (d, id)
            }};
        }

        match expr.data {
            Expr::Binary { op, left, right } => {
                let left = self.check_expr(scopes, *left, target);
                let right = self.check_expr(scopes, *right, Some(&left.ty));
                type_check_bail!(self, scopes, &right.ty, &left.ty, span);

                if !left.ty.supports_binop(op) {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for values of type {} and {}",
                            Self::type_name(scopes, &left.ty),
                            Self::type_name(scopes, &right.ty)
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
                let expr = self.check_expr(scopes, *expr, target);
                let mut out_ty = None;
                let valid = match op {
                    Plus => expr.ty.is_numeric(),
                    Neg => matches!(
                        expr.ty,
                        TypeId::Int(_) | TypeId::Isize | TypeId::F32 | TypeId::F64
                    ),
                    PostIncrement | PostDecrement | PreIncrement | PreDecrement => {
                        if matches!(
                            expr.ty,
                            TypeId::Int(_) | TypeId::Isize | TypeId::Uint(_) | TypeId::Usize
                        ) {
                            if !Self::is_assignable(scopes, &expr) {
                                return self
                                    .error(Error::new("expression is not assignable", value_span));
                            }

                            true
                        } else {
                            false
                        }
                    }
                    Not => matches!(
                        expr.ty,
                        TypeId::Int(_)
                            | TypeId::Isize
                            | TypeId::Uint(_)
                            | TypeId::Usize
                            | TypeId::Bool
                    ),
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
                        if !Self::can_addrmut(scopes, &expr) {
                            self.error::<()>(Error::new(
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
                            Self::type_name(scopes, &expr.ty)
                        ),
                        span,
                    ))
                }
            }
            Expr::Call { callee, args } => {
                if let Expr::Member { source, member } = callee.data {
                    let source = self.check_expr(scopes, *source, None);
                    let (s, id) = eval_member_to_struct!(source);
                    if let Some(Function { params, ret, .. }) = scopes[s.scope].find_fn(&member) {
                        if let Some((_, ty)) = params.get(0).filter(|(name, _)| name == "$self") {
                            if let TypeId::RefMut(inner) = ty {
                                let mut ty = &source.ty;
                                if ty == inner.as_ref() && !Self::can_addrmut(scopes, &source) {
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
                                    args: self.check_fn_args(scopes, args, params.clone(), span),
                                },
                            );
                        }
                    }

                    self.error(Error::new(
                        format!(
                            "no method '{member}' found on type {}",
                            Self::type_name(scopes, id)
                        ),
                        span,
                    ))
                } else {
                    let callee = self.check_expr(scopes, *callee, None);
                    if let TypeId::Function(id) = &callee.ty {
                        let Function { params, ret, .. } = &scopes[id.as_ref()];
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
                                args: self.check_fn_args(scopes, args, params, span),
                            },
                        );
                    }

                    self.error(Error::new(
                        format!(
                            "cannot call value of type {}",
                            Self::type_name(scopes, &callee.ty)
                        ),
                        span,
                    ))
                }
            }
            Expr::Array(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let inner = if let Some(TypeId::Array(inner)) = target {
                    inner.0.clone()
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(scopes, expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    return self.error(Error::new("cannot infer type of array literal", expr.span));
                };

                checked.extend(elements.map(|e| self.check_expr(scopes, e, Some(&inner))));
                CheckedExpr::new(
                    TypeId::Array(Box::new((inner, checked.len()))),
                    ExprData::Array(checked),
                )
            }
            Expr::ArrayWithInit { init, count } => {
                let init = if let Some(TypeId::Array(inner)) = target {
                    let span = init.span;
                    let init = self.check_expr(scopes, *init, Some(&inner.0));
                    type_check!(self, scopes, &init.ty, &inner.0, span);
                    init
                } else {
                    self.check_expr(scopes, *init, None)
                };

                // let span = count.span;
                // let count = self.check_expr(scopes, *count, Some(&TypeId::Isize));
                // type_check!(self, scopes, &count.ty, &TypeId::Isize, span);

                CheckedExpr::new(
                    TypeId::Array(Box::new((init.ty.clone(), count))),
                    ExprData::ArrayWithInit {
                        init: init.into(),
                        count,
                    },
                )
            }
            Expr::Tuple(_) => todo!(),
            Expr::Map(_) => todo!(),
            Expr::String(_) => todo!(),
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

                let (signed, bits) = match ty {
                    TypeId::Int(bits) => (true, bits),
                    TypeId::Uint(bits) => (false, bits),
                    TypeId::Isize => (true, std::mem::size_of::<isize>() as u8 * 8),
                    TypeId::Usize => (false, std::mem::size_of::<usize>() as u8 * 8),
                    _ => unreachable!(),
                };

                if signed {
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
                } else {
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
            }
            Expr::Float(value) => CheckedExpr::new(
                target
                    .filter(|target| self.coerces_to(&TypeId::FloatGeneric, target))
                    .cloned()
                    .unwrap_or(TypeId::F64),
                ExprData::Float(value),
            ),
            Expr::Symbol(name) => {
                if let Some((id, var)) = scopes.find_var(&name) {
                    CheckedExpr::new(
                        var.ty.clone(),
                        ExprData::Symbol {
                            scope: var.is_static.then_some(id),
                            symbol: name,
                        },
                    )
                } else if let Some(id) = scopes.find_fn(&name) {
                    CheckedExpr::new(
                        TypeId::Function(id.into()),
                        ExprData::Symbol {
                            scope: Some(id.scope()),
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
                let Some((id, d)) = scopes.find_struct(&name) else {
                    return self.undefined_type(&name, span);
                };
                let id = TypeId::Struct(id.into());

                let members = d.members.clone();
                if members.iter().any(|member| !member.1.public) && !scopes.is_sub_scope(d.scope) {
                    self.error::<()>(Error::new(
                        "cannot construct type with private members",
                        span,
                    ));

                    return CheckedExpr::new(id, ExprData::Error);
                }

                let mut checked: HashMap<_, _> =
                    members.keys().map(|name| (name.clone(), None)).collect();
                for (name, value) in arguments {
                    if let Entry::Occupied(mut entry) = checked.entry(name.clone()) {
                        if entry.get().is_some() {
                            self.error::<()>(Error::new(
                                format!("field '{name}' declared multiple times"),
                                value.span,
                            ));
                        } else {
                            let span = value.span;
                            let value = self.check_expr(scopes, value, Some(&members[&name].ty));
                            if !self.coerces_to(&value.ty, &members[&name].ty) {
                                entry.insert(Some(self.type_mismatch(
                                    scopes,
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
                            format!("no field '{name}' on type {}", Self::type_name(scopes, &id)),
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
                let lhs = self.check_expr(scopes, *lhs, None);
                if !Self::is_assignable(scopes, &lhs) {
                    // TODO: report a better error here
                    return self.error(Error::new("expression is not assignable", span));
                }

                let rhs = self.check_expr(scopes, *value, Some(&lhs.ty));
                type_check_bail!(self, scopes, &rhs.ty, &lhs.ty, span);

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
                let block =
                    self.create_block(scopes, None, body, ScopeKind::Block(target.cloned()));
                let ScopeKind::Block(target) = &scopes[block.scope].kind else {
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
                let cond = self.check_expr(scopes, *cond, Some(&TypeId::Bool));
                let if_branch = self.check_expr(scopes, *if_branch, target);
                let else_branch = if let Some(e) = else_branch {
                    let else_branch = self.check_expr(scopes, *e, Some(&if_branch.ty));
                    type_check!(self, scopes, &else_branch.ty, &if_branch.ty, span);

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
                let source = self.check_expr(scopes, *source, None);
                let (d, id) = eval_member_to_struct!(source);
                if let Some(t) = d.members.get(&member) {
                    if !t.public && !scopes.is_sub_scope(d.scope) {
                        return self.error(Error::new(
                            format!(
                                "cannot access private member '{member}' of type {}",
                                Self::type_name(scopes, id)
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
                            Self::type_name(scopes, &source.ty)
                        ),
                        span,
                    ))
                }
            }
            Expr::Subscript { callee, args } => {
                if args.len() > 1 {
                    self.error::<()>(Error::new(
                        "multidimensional subscript is not supported",
                        args[1].span,
                    ));
                }

                let callee = self.check_expr(scopes, *callee, None);
                let arg = args.into_iter().next().unwrap();
                let arg_span = arg.span;
                let arg = self.check_expr(scopes, arg, Some(&TypeId::Isize));
                type_check_bail!(self, scopes, &arg.ty, &TypeId::Isize, arg_span);

                if let TypeId::Array(target) = &callee.ty {
                    CheckedExpr::new(
                        target.0.clone(),
                        ExprData::Subscript {
                            callee: callee.into(),
                            args: vec![arg],
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!(
                            "type {} cannot be subscripted",
                            Self::type_name(scopes, &callee.ty)
                        ),
                        span,
                    ))
                }
            }
            Expr::Return(expr) => {
                let Some(target) = scopes.iter().find_map(|(_, scope)| {
                    if let ScopeKind::Function(id) = &scope.kind {
                        Some(id)
                    } else {
                        None
                    }
                }).map(|id| scopes[id].ret.clone()) else {
                    // the parser ensures return only happens inside functions
                    return self.error(Error::new("return outside of function", span));
                };

                let span = expr.span;
                let expr = self.check_expr(scopes, *expr, Some(&target));
                type_check!(self, scopes, &expr.ty, &target, span);

                CheckedExpr::new(TypeId::Never, ExprData::Return(expr.into()))
            }
            Expr::Yield(expr) => {
                let ScopeKind::Block(target) = scopes.current().kind.clone() else {
                    return self.error(Error::new("yield outside of block", span));
                };

                let span = expr.span;
                let expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    type_check!(self, scopes, &expr.ty, target, span);
                } else {
                    scopes.current().kind = ScopeKind::Block(Some(expr.ty.clone()));
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
                TypeId::Int(_) | TypeId::Uint(_) | TypeId::Isize | TypeId::Usize => return true,
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
        scopes: &mut Scopes,
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
                    ty: self.resolve_type(scopes, &param.ty),
                })
                .collect(),
            ret: self.resolve_type(scopes, &ret),
        };

        let id = scopes.insert_fn(Function {
            name: header.name.clone(),
            params: header
                .params
                .iter()
                .map(|param| (param.name.clone(), param.ty.clone()))
                .collect(),
            ret: header.ret.clone(),
        });

        // scopes.insert_var(header.name.clone(), Variable {
        //     ty: TypeId::Function(id.clone().into()),
        //     mutable: false,
        // });
        CheckedFn {
            body: scopes.enter(Some(name), ScopeKind::Function(id), |scopes| {
                for param in header.params.iter() {
                    scopes.insert_var(
                        param.name.clone(),
                        Variable {
                            ty: param.ty.clone(),
                            is_static: false,
                            mutable: param.mutable,
                        },
                    );
                }

                Block {
                    body: body
                        .into_iter()
                        .map(|stmt| self.check_stmt(scopes, stmt))
                        .collect(),
                    scope: scopes.current_id(),
                }
            }),
            header,
        }
    }

    fn check_fn_args(
        &mut self,
        scopes: &mut Scopes,
        args: Vec<(Option<String>, Located<Expr>)>,
        params: Vec<(String, TypeId)>,
        span: Span,
    ) -> Vec<CheckedExpr> {
        let mut result_args = Vec::with_capacity(args.len());
        // TODO: keyword arguments
        for ((_, ptype), (_, expr)) in params.into_iter().zip(args.into_iter()) {
            let expr = self.check_expr(scopes, expr, Some(&ptype));
            if !self.coerces_to(&expr.ty, &ptype) {
                result_args.push(self.type_mismatch(scopes, &ptype, &expr.ty, span));
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

    fn resolve_type(&mut self, scopes: &Scopes, ty: &TypeHint) -> TypeId {
        match ty {
            TypeHint::Regular { name, .. } => {
                return scopes
                    .find_struct(&name.data)
                    .map(|id| TypeId::Struct(id.0.into()))
                    .or_else(|| match name.data.as_str() {
                        "void" => Some(TypeId::Void),
                        "never" => Some(TypeId::Never),
                        "f32" => Some(TypeId::F32),
                        "f64" => Some(TypeId::F64),
                        "usize" => Some(TypeId::Usize),
                        "isize" => Some(TypeId::Isize),
                        "bool" => Some(TypeId::Bool),
                        _ => Self::match_int_type(&name.data),
                    })
                    .unwrap_or_else(|| self.undefined_type(&name.data, name.span));
            }
            TypeHint::Void => TypeId::Void,
            TypeHint::Ref(ty) => TypeId::Ref(self.resolve_type(scopes, ty).into()),
            TypeHint::RefMut(ty) => TypeId::RefMut(self.resolve_type(scopes, ty).into()),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                scopes
                    .iter()
                    .find_map(|(_, scope)| {
                        if let ScopeKind::Struct(id) = &scope.kind {
                            Some(TypeId::Ref(TypeId::Struct((*id).into()).into()))
                        } else {
                            None
                        }
                    })
                    .expect("ICE: this outside of method")
            }
            TypeHint::MutThis => scopes
                .iter()
                .find_map(|(_, scope)| {
                    if let ScopeKind::Struct(id) = &scope.kind {
                        Some(TypeId::RefMut(TypeId::Struct((*id).into()).into()))
                    } else {
                        None
                    }
                })
                .expect("ICE: this outside of method"),
            TypeHint::Array(ty, n) => TypeId::Array(Box::new((self.resolve_type(scopes, ty), *n))),
            _ => todo!(),
        }
    }

    fn create_block(
        &mut self,
        scopes: &mut Scopes,
        name: Option<String>,
        body: Vec<Located<Stmt>>,
        target: ScopeKind,
    ) -> Block {
        scopes.enter(name, target, |scopes| Block {
            body: body
                .into_iter()
                .map(|stmt| self.check_stmt(scopes, stmt))
                .collect(),
            scope: scopes.current_id(),
        })
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.errors.push(error);
        T::default()
    }

    fn type_mismatch<T: Default>(
        &mut self,
        scopes: &Scopes,
        expected: &TypeId,
        actual: &TypeId,
        span: Span,
    ) -> T {
        self.error(Error::new(
            format!(
                "type mismatch: expected type {}, got {}",
                Self::type_name(scopes, expected),
                Self::type_name(scopes, actual),
            ),
            span,
        ))
    }

    fn undefined_type<T: Default>(&mut self, name: &str, span: Span) -> T {
        self.error(Error::new(format!("undefined type: {name}"), span))
    }

    fn is_assignable(scopes: &Scopes, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, TypeId::RefMut(_))
            }
            ExprData::Symbol { symbol, .. } => scopes.find_var(symbol).unwrap().1.mutable,
            ExprData::Member { source, .. } => {
                matches!(source.ty, TypeId::RefMut(_)) || Self::can_addrmut(scopes, source)
            }
            ExprData::Subscript { callee, .. } => Self::is_assignable(scopes, callee),
            _ => false,
        }
    }

    fn can_addrmut(scopes: &Scopes, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                !matches!(op, UnaryOp::Deref) || matches!(expr.ty, TypeId::RefMut(_))
            }
            ExprData::Symbol { symbol, .. } => scopes.find_var(symbol).unwrap().1.mutable,
            ExprData::Member { source, .. } => {
                matches!(source.ty, TypeId::RefMut(_)) || Self::can_addrmut(scopes, source)
            }
            ExprData::Subscript { callee, .. } => Self::can_addrmut(scopes, callee),
            _ => true,
        }
    }

    fn type_name(scopes: &Scopes, ty: &TypeId) -> String {
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
            TypeId::Ref(id) => format!("*{}", Self::type_name(scopes, id)),
            TypeId::RefMut(id) => format!("*mut {}", Self::type_name(scopes, id)),
            TypeId::Function(id) => {
                let Function { name, params, ret } = &scopes[id.as_ref()];
                let mut result = format!("fn {name}(");
                for (i, (name, ty)) in params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&format!("{name}: "));
                    result.push_str(&Self::type_name(scopes, ty));
                }
                format!("{result}) {}", Self::type_name(scopes, ret))
            }
            TypeId::Struct(id) => match &scopes[id.as_ref()] {
                Struct::Declared(name) | Struct::Defined(DefinedStruct { name, .. }) => {
                    name.clone()
                }
            },
            TypeId::Array(inner) => format!("[{}; {}]", Self::type_name(scopes, &inner.0), inner.1),
            TypeId::Isize => "isize".into(),
            TypeId::Usize => "usize".into(),
        }
    }
}
