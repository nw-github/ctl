use std::collections::{hash_map::Entry, HashMap};

use concat_idents::concat_idents;
use derive_more::{Constructor, Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        expr::{BinaryOp, Expr, UnaryOp},
        stmt::{Fn, ParsedUserType, Stmt, TypeHint},
        Path,
    },
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::CheckedStmt,
        Block,
    },
    lexer::{Located, Span},
    Error, THIS_PARAM, THIS_TYPE,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GenericFunc {
    id: FunctionId,
    generics: Vec<TypeId>,
}

#[derive(Debug, Default, PartialEq, Eq, Clone, EnumAsInner)]
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
    String,
    Func(FunctionId),
    GenericFunc(Box<GenericFunc>),
    UserType(UserTypeId),
    Ptr(Box<TypeId>),
    MutPtr(Box<TypeId>),
    Option(Box<TypeId>),
    Array(Box<(TypeId, usize)>),
    GenericPlaceholder(usize),
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
            BinaryOp::Add => matches!(
                self,
                TypeId::Int(_)
                    | TypeId::Isize
                    | TypeId::Uint(_)
                    | TypeId::Usize
                    | TypeId::F32
                    | TypeId::F64
                    | TypeId::String
            ),
            BinaryOp::Sub
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
                        | TypeId::String
                        | TypeId::Option(_) // FIXME: option<T> should be comparable with T
                )
            }
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                matches!(self, TypeId::Bool)
            }
            BinaryOp::NoneCoalesce => todo!(),
            BinaryOp::ErrCoalesce => todo!(),
        }
    }

    pub fn strip_references(&self) -> &TypeId {
        let mut id = self;
        while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = id {
            id = inner;
        }
        id
    }
}

#[derive(From)]
enum ResolveError<'a> {
    Path(&'a Located<Path>),
    #[from]
    Error(Error),
}

macro_rules! id {
    ($name: ident => $output: ident,
     $vec: ident,
     $($parts:ident).+,
     $suffix: ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(usize);

        impl Scopes {
            concat_idents!(fn_name = find_, $suffix, _in {
                pub fn fn_name(&self, name: &str, scope: ScopeId) -> Option<$name> {
                    self[scope].$vec
                        .iter()
                        .find_map(|id| (self.$vec[id.0].$($parts).+ == name).then_some(*id))
                }
            });

            concat_idents!(fn_name = find_, $suffix {
                pub fn fn_name(&self, name: &str) -> Option<$name> {
                    for (id, scope) in self.iter() {
                        concat_idents!(fn_name = find_, $suffix, _in {
                            if let Some(item) = self.fn_name(name, id) {
                                return Some(item);
                            }
                        });

                        if matches!(scope.kind, ScopeKind::Module(_)) {
                            break;
                        }
                    }

                    None
                }
            });

            concat_idents!(fn_name = insert_, $suffix, _in {
                pub fn fn_name(&mut self, item: $output, scope: ScopeId) -> $name {
                    let index = self.$vec.len();
                    self.$vec.push(Scoped::new(item, scope));
                    let itemid = $name(index);
                    self[scope].$vec.push(itemid);
                    itemid
                }
            });

            concat_idents!(fn_name = insert_, $suffix {
                pub fn fn_name(&mut self, item: $output) -> $name {
                    concat_idents!(fn_name = insert_, $suffix, _in {
                        self.fn_name(item, self.current_id())
                    })
                }
            });

            concat_idents!(fn_name = get_, $suffix {
                pub fn fn_name(&self, id: $name) -> &Scoped<$output> {
                    &self.$vec[id.0]
                }
            });

            concat_idents!(fn_name = get_, $suffix, _mut {
                #[allow(dead_code)]
                pub fn fn_name(&mut self, id: $name) -> &mut Scoped<$output> {
                    &mut self.$vec[id.0]
                }
            });
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ScopeId(pub usize);

id!(FunctionId => Function, fns, proto.name, func);
id!(UserTypeId => UserType, types, name, user_type);
id!(VariableId => Variable, vars, name, var);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Block(Option<TypeId>, bool),
    Loop(Option<TypeId>, bool),
    Function(FunctionId),
    UserType(UserTypeId),
    Module(bool),
    #[default]
    None,
}

#[derive(Debug, Clone)]
pub struct CheckedParam {
    pub mutable: bool,
    pub keyword: bool,
    pub name: String,
    pub ty: TypeId,
    //pub default: Option<CheckedExpr>,
}

#[derive(Debug, Clone)]
pub struct CheckedPrototype {
    pub public: bool,
    pub name: String,
    pub is_async: bool,
    pub is_extern: bool,
    pub type_params: Vec<String>,
    pub params: Vec<CheckedParam>,
    pub ret: TypeId,
}

#[derive(Default, Debug, Clone)]
pub struct Variable {
    pub public: bool,
    pub name: String,
    pub ty: TypeId,
    pub is_static: bool,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct Function {
    pub proto: CheckedPrototype,
    pub body: Option<Block>,
    pub inst: bool,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub ty: TypeId,
}

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct(Vec<(String, Member)>),
    Generic(usize),
}

#[derive(Debug)]
pub struct UserType {
    pub public: bool,
    pub name: String,
    pub body_scope: ScopeId,
    pub data: UserTypeData,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Symbol {
    Func(FunctionId),
    GenericFunc(GenericFunc),
    Var(VariableId),
}

#[derive(Deref, DerefMut, Constructor)]
pub struct Scoped<T> {
    #[deref]
    #[deref_mut]
    pub item: T,
    pub scope: ScopeId,
}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub fns: Vec<FunctionId>,
    pub types: Vec<UserTypeId>,
    pub vars: Vec<VariableId>,
    pub name: Option<String>,
    pub children: HashMap<String, ScopeId>,
}

pub struct Scopes {
    scopes: Vec<Scope>,
    current: ScopeId,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            current: ScopeId(0),
            fns: Vec::new(),
            types: Vec::new(),
            vars: Vec::new(),
        }
    }

    pub fn iter_from(&self, id: ScopeId) -> impl Iterator<Item = (ScopeId, &Scope)> {
        pub struct ScopeIter<'a> {
            scopes: &'a Scopes,
            next: Option<ScopeId>,
        }

        impl<'a> Iterator for ScopeIter<'a> {
            type Item = (ScopeId, &'a Scope);

            fn next(&mut self) -> Option<Self::Item> {
                self.next.map(|i| {
                    self.next = self.scopes[i].parent;
                    (i, &self.scopes[i])
                })
            }
        }

        ScopeIter {
            scopes: self,
            next: Some(id),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.iter_from(self.current)
    }

    pub fn full_name(&self, id: ScopeId, ident: &str) -> String {
        let mut name: String = ident.chars().rev().collect();
        for scope_name in self.iter_from(id).flat_map(|scope| scope.1.name.as_ref()) {
            name.reserve(scope_name.len() + 1);
            name.push('_');
            for c in scope_name.chars().rev() {
                name.push(c);
            }
        }

        name.chars().rev().collect::<String>()
    }

    pub fn is_sub_scope(&self, target: ScopeId) -> bool {
        self.iter().any(|(id, _)| id == target)
    }

    pub fn current(&mut self) -> &mut Scope {
        let i = self.current;
        &mut self[i]
    }

    pub fn current_id(&self) -> ScopeId {
        self.current
    }

    pub fn enter<T>(
        &mut self,
        name: Option<String>,
        kind: ScopeKind,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let id = ScopeId(self.scopes.len());
        // blocks are the only unnamed scopes
        self.current()
            .children
            .insert(name.clone().unwrap_or(String::new()), id);
        let parent = Some(self.current);
        self.enter_id(id, |this| {
            this.scopes.push(Scope {
                parent,
                kind,
                name,
                ..Default::default()
            });

            f(this)
        })
    }

    pub fn find_enter<T>(&mut self, name: &str, f: impl FnOnce(&mut Self) -> T) -> T {
        let id = *self.current().children.get(name).unwrap();
        self.enter_id(id, f)
    }

    pub fn enter_id<T>(&mut self, id: ScopeId, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.current;
        self.current = id;
        let result = f(self);
        self.current = prev;
        result
    }

    pub fn current_struct(&self) -> Option<TypeId> {
        self.iter().find_map(|(_, scope)| {
            if let ScopeKind::UserType(id) = scope.kind {
                if self.get_user_type(id).data.is_struct() {
                    return Some(TypeId::UserType(id));
                }
            }
            None
        })
    }

    pub fn current_function(&self) -> Option<FunctionId> {
        self.iter().find_map(|(_, scope)| {
            if let ScopeKind::Function(id) = &scope.kind {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn module_of(&self, id: ScopeId) -> Option<ScopeId> {
        for (id, current) in self.iter_from(id) {
            if matches!(current.kind, ScopeKind::Module(_)) {
                return Some(id);
            }
        }

        None
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }
}

impl std::ops::Index<ScopeId> for Scopes {
    type Output = Scope;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index.0]
    }
}

impl std::ops::IndexMut<ScopeId> for Scopes {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scopes[index.0]
    }
}

impl Default for Scopes {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CheckedAst {
    pub scopes: Scopes,
    pub stmt: CheckedStmt,
}

macro_rules! type_mismatch {
    ($scopes: expr, $expected: expr, $actual: expr, $span: expr) => {
        Error::new(
            format!(
                "type mismatch: expected type '{}', got '{}'",
                Self::type_name($scopes, $expected),
                Self::type_name($scopes, $actual),
            ),
            $span,
        )
    };
}

macro_rules! type_check_bail {
    ($self: expr, $scopes: expr, $source: expr, $target: expr, $span: expr) => {
        if !Self::coerces_to($source, $target) {
            return $self.error(type_mismatch!($scopes, $target, $source, $span));
        }
    };
}

macro_rules! type_check {
    ($self: expr, $scopes: expr, $source: expr, $target: expr, $span: expr) => {
        if !Self::coerces_to($source, $target) {
            $self.error::<()>(type_mismatch!($scopes, $target, $source, $span))
        }
    };
}

macro_rules! resolve_forward_declare {
    ($self: ident, $scopes: expr, $checked: expr, $unchecked: expr) => {
        if $checked == TypeId::Unknown {
            $checked = $self.resolve_type($scopes, $unchecked);
        }
    };
}

pub struct TypeChecker<'a> {
    errors: Vec<Error>,
    generic_fn_insts: Vec<GenericFunc>,
    src: &'a str,
}

impl<'a> TypeChecker<'a> {
    pub fn check(stmt: Located<Stmt>, src: &'a str) -> (CheckedAst, Vec<Error>) {
        // we depend on parser wrapping up the generated code in a Stmt::Module
        let mut this = Self {
            errors: vec![],
            generic_fn_insts: vec![],
            src,
        };
        let mut scopes = Scopes::new();
        this.forward_declare(&mut scopes, &stmt);
        let stmt = this.check_stmt(&mut scopes, stmt);
        (CheckedAst { scopes, stmt }, this.errors)
    }

    fn forward_declare(&mut self, scopes: &mut Scopes, stmt: &Located<Stmt>) {
        match &stmt.data {
            Stmt::Module { public, name, body } => {
                scopes.enter(Some(name.clone()), ScopeKind::Module(*public), |scopes| {
                    for stmt in body {
                        self.forward_declare(scopes, stmt)
                    }
                });
            }
            Stmt::UserType(data) => match data {
                ParsedUserType::Struct(base) => {
                    let mut params = Vec::with_capacity(base.members.len());
                    let mut members = Vec::with_capacity(base.members.len());
                    for (name, member) in base.members.iter() {
                        let target =
                            Self::fd_resolve_type(scopes, &member.ty).unwrap_or(TypeId::Unknown);
                        members.push((
                            name.clone(),
                            Member {
                                public: member.public,
                                ty: target.clone(),
                            },
                        ));

                        params.push(CheckedParam {
                            mutable: false,
                            keyword: true,
                            name: name.clone(),
                            ty: Self::fd_resolve_type(scopes, &member.ty)
                                .unwrap_or(TypeId::Unknown),
                        });
                    }

                    let self_id = scopes.insert_user_type(UserType {
                        name: base.name.clone(),
                        public: base.public,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Struct(members),
                    });
                    let parent = scopes.current_id();
                    scopes.enter(
                        Some(base.name.clone()),
                        ScopeKind::UserType(self_id),
                        |scopes| {
                            scopes.get_user_type_mut(self_id).body_scope = scopes.current_id();
                            scopes.insert_func_in(
                                Function {
                                    proto: CheckedPrototype {
                                        public: base.public,
                                        name: base.name.clone(),
                                        is_async: false,
                                        is_extern: false,
                                        type_params: base.type_params.clone(),
                                        params,
                                        ret: TypeId::UserType(self_id),
                                    },
                                    body: None,
                                    inst: true,
                                },
                                parent,
                            );

                            for f in base.functions.iter() {
                                self.forward_declare_fn(scopes, f);
                            }
                        },
                    )
                }
                ParsedUserType::Union { .. } => todo!(),
                ParsedUserType::Interface { .. } => todo!(),
                ParsedUserType::Enum { .. } => todo!(),
            },
            Stmt::Fn(f) => self.forward_declare_fn(scopes, f),
            Stmt::Static { public, name, .. } => {
                scopes.insert_var(Variable {
                    name: name.clone(),
                    public: *public,
                    ty: TypeId::Unknown,
                    is_static: true,
                    mutable: false,
                });
            }
            _ => {}
        }
    }

    fn check_stmt(&mut self, scopes: &mut Scopes, stmt: Located<Stmt>) -> CheckedStmt {
        match stmt.data {
            Stmt::Module {
                public: _,
                name,
                body,
            } => CheckedStmt::Module {
                name: name.clone(),
                body: scopes.find_enter(&name, |scopes| Block {
                    body: body
                        .into_iter()
                        .map(|stmt| self.check_stmt(scopes, stmt))
                        .collect(),
                    scope: scopes.current_id(),
                }),
            },
            Stmt::UserType(data) => {
                match data {
                    ParsedUserType::Struct(base) => {
                        let self_id = scopes.find_user_type(&base.name).unwrap();
                        let parent = scopes.current_id();
                        scopes.enter_id(scopes.get_user_type(self_id).body_scope, |scopes| {
                        for i in 0..base.members.len() {
                            resolve_forward_declare!(
                                self,
                                scopes,
                                scopes.get_user_type_mut(self_id).data.as_struct_mut().unwrap()[i].1.ty,
                                &base.members[i].1.ty
                            );
                        }

                        let init = *scopes[parent]
                            .fns
                            .iter()
                            .find(|&&f| {
                                let f = scopes.get_func(f);
                                f.inst && matches!(f.proto.ret, TypeId::UserType(id) if id == self_id)
                            })
                            .unwrap();

                        for i in 0..base.members.len() {
                            resolve_forward_declare!(
                                self,
                                scopes,
                                scopes.get_func_mut(init).proto.params[i].ty,
                                &base.members[i].1.ty
                            );
                        }

                        for f in base.functions {
                            self.check_fn(scopes, f);
                        }
                    });
                        CheckedStmt::None
                    }
                    ParsedUserType::Union { .. } => todo!(),
                    ParsedUserType::Interface { .. } => todo!(),
                    ParsedUserType::Enum { .. } => todo!(),
                }
            }
            Stmt::Expr(expr) => CheckedStmt::Expr(self.check_expr(scopes, expr, None)),
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                let (ty, value) = if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty);
                    if let Some(value) = value {
                        let span = value.span;
                        let value = self.check_expr(scopes, value, Some(&ty));
                        type_check!(self, scopes, &value.ty, &ty, span);

                        (
                            ty,
                            CheckedStmt::Let {
                                name: name.clone(),
                                mutable,
                                value: Ok(value),
                            },
                        )
                    } else {
                        (
                            ty.clone(),
                            CheckedStmt::Let {
                                name: name.clone(),
                                mutable,
                                value: Err(ty),
                            },
                        )
                    }
                } else if let Some(value) = value {
                    let value = self.check_expr(scopes, value, None);
                    (
                        value.ty.clone(),
                        CheckedStmt::Let {
                            name: name.clone(),
                            mutable,
                            value: Ok(value),
                        },
                    )
                } else {
                    return self.error(Error::new("cannot infer type", stmt.span));
                };

                scopes.insert_var(Variable {
                    public: false,
                    name,
                    ty,
                    is_static: false,
                    mutable,
                });

                value
            }
            Stmt::Fn(f) => {
                self.check_fn(scopes, f);
                CheckedStmt::None
            }
            Stmt::Static {
                name, ty, value, ..
            } => {
                // FIXME: detect cycles like static X: usize = X;
                if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty);
                    let span = value.span;
                    let value = self.check_expr(scopes, value, Some(&ty));
                    type_check!(self, scopes, &value.ty, &ty, span);

                    CheckedStmt::Static(scopes.find_var(&name).unwrap(), value)
                } else {
                    let value = self.check_expr(scopes, value, None);
                    CheckedStmt::Static(scopes.find_var(&name).unwrap(), value)
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
        match expr.data {
            Expr::Binary { op, left, right } => {
                let left = self.check_expr(scopes, *left, target);
                let right_span = right.span;
                let right = self.check_expr(scopes, *right, Some(&left.ty));
                type_check_bail!(self, scopes, &right.ty, &left.ty, right_span);

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
                        if let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = &expr.ty {
                            out_ty = Some((**inner).clone());
                            true
                        } else {
                            false
                        }
                    }
                    Addr => {
                        out_ty = Some(TypeId::Ptr(expr.ty.clone().into()));
                        true
                    }
                    AddrMut => {
                        if !Self::can_addrmut(scopes, &expr) {
                            self.error::<()>(Error::new(
                                "cannot create mutable pointer to immutable memory location",
                                span,
                            ));
                        }

                        out_ty = Some(TypeId::MutPtr(expr.ty.clone().into()));
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
            Expr::Call { callee, args } => self.check_call(scopes, target, *callee, args, span),
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

                match Self::consteval(scopes, &count, Some(&TypeId::Usize)) {
                    Ok(count) => CheckedExpr::new(
                        TypeId::Array(Box::new((init.ty.clone(), count))),
                        ExprData::ArrayWithInit {
                            init: init.into(),
                            count,
                        },
                    ),
                    Err(err) => self.error(err),
                }
            }
            Expr::Tuple(_) => todo!(),
            Expr::Map(_) => todo!(),
            Expr::Range { .. } => todo!(),
            Expr::String(s) => CheckedExpr::new(TypeId::String, ExprData::String(s)),
            Expr::None => {
                if let Some(TypeId::Option(target)) = target {
                    CheckedExpr::new(TypeId::Option(target.clone()), ExprData::None)
                } else {
                    self.error(Error::new("cannot infer type of option literal none", span))
                }
            }
            Expr::Void => CheckedExpr::new(TypeId::Void, ExprData::Instance(HashMap::new())),
            Expr::Bool(value) => CheckedExpr {
                ty: TypeId::Bool,
                data: ExprData::Bool(value),
            },
            Expr::Integer { base, value, width } => {
                let ty = if let Some(width) = width {
                    Self::match_int_type(&width).unwrap_or_else(|| {
                        self.error(Error::new(
                            format!("invalid integer literal type: {width}"),
                            span,
                        ))
                    })
                } else {
                    // FIXME: attempt to promote the literal if its too large for i32
                    target
                        .map(|mut target| {
                            while let TypeId::Option(ty) | TypeId::MutPtr(ty) | TypeId::Ptr(ty) =
                                target
                            {
                                target = ty;
                            }
                            target
                        })
                        .filter(|target| Self::coerces_to(&TypeId::IntGeneric, target))
                        .cloned()
                        .unwrap_or(TypeId::Int(32))
                };

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
                        Err(e) => {
                            return self.error(Error::new(
                                format!("Integer literal '{value}' is too large: {e}."),
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
                                format!("Integer literal '{value}' is too large."),
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
                    .map(|mut target| {
                        while let TypeId::Option(ty) | TypeId::MutPtr(ty) | TypeId::Ptr(ty) = target
                        {
                            target = ty;
                        }
                        target
                    })
                    .filter(|target| Self::coerces_to(&TypeId::FloatGeneric, target))
                    .cloned()
                    .unwrap_or(TypeId::F64),
                ExprData::Float(value),
            ),
            Expr::Path(path) => match self.resolve_path(scopes, &path, span) {
                Ok(Some(Symbol::Var(id))) => CheckedExpr::new(
                    scopes.get_var(id).ty.clone(),
                    ExprData::Symbol(Symbol::Var(id)),
                ),
                Ok(Some(Symbol::Func(id))) => {
                    CheckedExpr::new(TypeId::Func(id), ExprData::Symbol(Symbol::Func(id)))
                }
                Ok(Some(Symbol::GenericFunc(data))) => CheckedExpr::new(
                    TypeId::GenericFunc(data.clone().into()),
                    ExprData::Symbol(Symbol::GenericFunc(data)),
                ),
                Err(err) => self.error(err),
                Ok(None) => self.error(Error::new(
                    format!("'{}' not found in this scope", span.text(self.src)),
                    span,
                )),
            },
            Expr::Assign {
                target: lhs,
                binary,
                value,
            } => {
                let span = lhs.span;
                let lhs = self.check_expr(scopes, *lhs, None);
                if !Self::is_assignable(scopes, &lhs) {
                    // TODO: report a better error here
                    return self.error(Error::new("expression is not assignable", span));
                }

                let rhs = self.check_expr(scopes, *value, Some(&lhs.ty));
                type_check_bail!(self, scopes, &rhs.ty, &lhs.ty, span);

                if let Some(op) = binary {
                    if !lhs.ty.supports_binop(op) {
                        self.error::<()>(Error::new(
                            format!(
                                "operator '{op}' is invalid for values of type {} and {}",
                                Self::type_name(scopes, &lhs.ty),
                                Self::type_name(scopes, &rhs.ty)
                            ),
                            span,
                        ));
                    }
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
                let block =
                    self.create_block(scopes, None, body, ScopeKind::Block(target.cloned(), false));
                let ScopeKind::Block(target, _) = &scopes[block.scope].kind else {
                    panic!("ICE: target of block changed from block to something else");
                };
                CheckedExpr::new(
                    target.clone().unwrap_or(TypeId::Void),
                    ExprData::Block(block),
                )
            }
            Expr::If {
                cond,
                if_branch,
                else_branch,
            } => {
                /* FIXME: type inference for cases like this:
                    let foo = 5;
                    let x: ?i64 = if foo {
                        yield 10;
                    };
                */
                let cond_span = cond.span;
                let cond = self.check_expr(scopes, *cond, Some(&TypeId::Bool));
                type_check!(self, scopes, &cond.ty, &TypeId::Bool, cond_span);

                let if_branch = self.check_expr(scopes, *if_branch, None);
                let mut out_type = if_branch.ty.clone();
                let else_branch = if let Some(e) = else_branch {
                    let else_branch = self.check_expr(scopes, *e, Some(&if_branch.ty));
                    type_check!(self, scopes, &else_branch.ty, &if_branch.ty, span);

                    Some(else_branch)
                } else {
                    // this separates these two cases:
                    //   let x /* void? */ = if whatever { yield void; };
                    //   let x /* void */ = if whatever { };
                    if matches!(&if_branch.data, ExprData::Block(b) if
                        matches!(scopes[b.scope].kind, ScopeKind::Block(_, yields) if yields))
                    {
                        out_type = TypeId::Option(out_type.into());
                    }
                    None
                };

                CheckedExpr::new(
                    out_type,
                    ExprData::If {
                        cond: cond.into(),
                        if_branch: if_branch.into(),
                        else_branch: else_branch.map(|e| e.into()),
                    },
                )
            }
            Expr::Loop {
                cond,
                body,
                do_while,
            } => {
                /* FIXME: type inference for cases like this:
                    let a = 5;
                    let x: ?i64 = loop 10 < 2 {
                        if a != 2 {
                            break 10;
                        } else {
                            break 11;
                        }
                    };
                */
                let span = cond.span;
                let cond = self.check_expr(scopes, *cond, Some(&TypeId::Bool));
                type_check!(self, scopes, &cond.ty, &TypeId::Bool, span);

                let body = self.create_block(
                    scopes,
                    None,
                    body,
                    ScopeKind::Loop(None, matches!(cond.data, ExprData::Bool(true))),
                );
                let ScopeKind::Loop(target, inf) = &scopes[body.scope].kind else {
                    panic!("ICE: target of loop changed from loop to something else");
                };

                CheckedExpr::new(
                    target
                        .clone()
                        .unwrap_or(if *inf { TypeId::Never } else { TypeId::Void }),
                    ExprData::Loop {
                        cond: cond.into(),
                        body,
                        do_while,
                    },
                )
            }
            Expr::For { .. } => todo!(),
            Expr::Member {
                source,
                member,
                generics,
            } => {
                if !generics.is_empty() {
                    self.error::<()>(Error::new(
                        "member variables cannot be parameterized with generics",
                        span,
                    ));
                }

                let source = self.check_expr(scopes, *source, None);
                let id = source.ty.strip_references();
                let Some(ty) = id.as_user_type() else {
                    return self.error(Error::new(
                        format!("cannot get member of type {}",
                        Self::type_name(scopes, id)
                    ), span));
                };
                let ty = scopes.get_user_type(*ty);

                if let Some(members) = ty.data.as_struct() {
                    if let Some((_, var)) = members.iter().find(|m| m.0 == member) {
                        if !var.public && !scopes.is_sub_scope(ty.scope) {
                            return self.error(Error::new(
                                format!(
                                    "cannot access private member '{member}' of type {}",
                                    Self::type_name(scopes, id)
                                ),
                                span,
                            ));
                        }

                        return CheckedExpr::new(
                            var.ty.clone(),
                            ExprData::Member {
                                source: source.into(),
                                member,
                            },
                        );
                    }
                }

                self.error(Error::new(
                    format!(
                        "type {} has no member '{member}'",
                        Self::type_name(scopes, &source.ty)
                    ),
                    span,
                ))
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
                let Some(target) = scopes.current_function().map(|id| scopes.get_func(id).proto.ret.clone()) else {
                    // the parser ensures return only happens inside functions
                    return self.error(Error::new("return outside of function", span));
                };

                let span = expr.span;
                let expr = self.check_expr(scopes, *expr, Some(&target));
                type_check!(self, scopes, &expr.ty, &target, span);

                CheckedExpr::new(TypeId::Never, ExprData::Return(expr.into()))
            }
            Expr::Yield(expr) => {
                let ScopeKind::Block(target, _) = scopes.current().kind.clone() else {
                    return self.error(Error::new("yield outside of block", span));
                };

                let span = expr.span;
                let expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    type_check!(self, scopes, &expr.ty, target, span);
                    scopes.current().kind = ScopeKind::Block(Some(target.clone()), true);
                } else {
                    scopes.current().kind = ScopeKind::Block(Some(expr.ty.clone()), true);
                }

                CheckedExpr::new(TypeId::Never, ExprData::Yield(expr.into()))
            }
            Expr::Break(expr) => {
                let Some(scope) = scopes.iter().find_map(|(id, scope)| {
                    matches!(scope.kind, ScopeKind::Loop(_, _)).then_some(id)
                }) else {
                    return self.error(Error::new("break outside of loop", span));
                };

                let ScopeKind::Loop(target, inf) = scopes[scope].kind.clone() else {
                    unreachable!()
                };

                let span = expr.span;
                let expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    type_check!(self, scopes, &expr.ty, target, span);
                } else if inf {
                    scopes[scope].kind = ScopeKind::Loop(Some(expr.ty.clone()), inf);
                } else {
                    scopes[scope].kind =
                        ScopeKind::Loop(Some(TypeId::Option(expr.ty.clone().into())), inf);
                }

                CheckedExpr::new(TypeId::Never, ExprData::Break(expr.into()))
            }
            Expr::Continue => {
                if scopes
                    .iter()
                    .find_map(|(id, scope)| {
                        matches!(scope.kind, ScopeKind::Loop(_, _)).then_some(id)
                    })
                    .is_none()
                {
                    return self.error(Error::new("continue outside of loop", span));
                }

                CheckedExpr::new(TypeId::Never, ExprData::Continue)
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_args_with_generics(
        &mut self,
        scopes: &mut Scopes,
        args: Vec<(Option<String>, Located<Expr>)>,
        params: &[CheckedParam],
        type_params: &[String],
        mut generics: Option<Vec<TypeId>>,
        ret: TypeId,
        target: Option<&TypeId>,
        span: Span,
    ) -> (Vec<CheckedExpr>, TypeId) {
        if type_params.is_empty() {
            (
                self.check_fn_args(
                    scopes,
                    args,
                    params,
                    generics.as_deref_mut().unwrap_or(&mut []),
                    span,
                ),
                ret,
            )
        } else {
            let mut generics = generics.unwrap_or_else(|| vec![TypeId::Unknown; type_params.len()]);
            let args = self.check_fn_args(scopes, args, params, &mut generics, span);
            for (i, ty) in generics.iter_mut().enumerate() {
                if ty.is_unknown() {
                    eprintln!("ret = {}", Self::type_name(scopes, &ret));
                    if let Some(target) = ret
                        .as_user_type()
                        .and_then(|&ret| scopes.get_user_type(ret).data.as_generic())
                        .filter(|&&ret| ret == i)
                        .and(target)
                        .cloned()
                    {
                        *ty = target.clone();
                    } else {
                        self.error::<()>(Error::new(
                            format!("cannot infer type of generic parameter {}", type_params[i]),
                            span,
                        ));
                    }
                }
            }

            if let Some(&idx) = ret
                .as_user_type()
                .and_then(|&ret| scopes.get_user_type(ret).data.as_generic())
            {
                (args, generics.into_iter().nth(idx).unwrap())
            } else {
                (args, ret)
            }
        }
    }

    fn check_call(
        &mut self,
        scopes: &mut Scopes,
        target: Option<&TypeId>,
        callee: Located<Expr>,
        args: Vec<(Option<String>, Located<Expr>)>,
        span: Span,
    ) -> CheckedExpr {
        if let Expr::Member {
            source,
            member,
            generics,
        } = callee.data
        {
            let source = self.check_expr(scopes, *source, None);
            let id = source.ty.strip_references();
            let Some(ty) = id.as_user_type().map(|&ty| scopes.get_user_type(ty)) else {
                return self.error(Error::new(
                    format!("cannot get member of type '{}'",
                    Self::type_name(scopes, id)
                ), span));
            };

            if let Some(func) = ty
                .data
                .as_struct()
                .and_then(|_| scopes.find_func_in(&member, ty.body_scope))
            {
                let f = scopes.get_func(func);
                if !f.proto.public && !scopes.is_sub_scope(ty.scope) {
                    return self.error(Error::new(
                        format!(
                            "cannot access private method '{member}' of type '{}'",
                            Self::type_name(scopes, id)
                        ),
                        span,
                    ));
                }

                if let Some(this) = f.proto.params.get(0).filter(|p| p.name == THIS_PARAM) {
                    if let TypeId::MutPtr(inner) = &this.ty {
                        let mut ty = &source.ty;
                        if ty == inner.as_ref() && !Self::can_addrmut(scopes, &source) {
                            return self.error(Error::new(
                                format!("cannot call method '{member}' with immutable receiver"),
                                span,
                            ));
                        } else {
                            while let TypeId::MutPtr(inner) = ty {
                                ty = inner;
                            }

                            if matches!(ty, TypeId::Ptr(_)) {
                                return self.error(Error::new(
                                    format!(
                                        "cannot call method '{member}' through an immutable pointer"
                                    ),
                                    span,
                                ));
                            }
                        }
                    }

                    let symbol = match self.resolve_func(scopes, func, &generics, span) {
                        Ok(symbol) => symbol,
                        Err(_) => return CheckedExpr::default(),
                    };

                    #[allow(clippy::unnecessary_to_owned)]
                    let (args, ret) = self.check_args_with_generics(
                        scopes,
                        args,
                        &f.proto.params[1..].to_vec(),
                        &f.proto.type_params.to_vec(),
                        symbol.into_generic_func().map(|v| v.generics).ok(),
                        f.proto.ret.clone(),
                        target,
                        span,
                    );

                    return CheckedExpr::new(
                        ret,
                        ExprData::MemberCall {
                            this: source.into(),
                            func,
                            args,
                        },
                    );
                }
            }

            self.error(Error::new(
                format!(
                    "no method '{member}' found on type '{}'",
                    Self::type_name(scopes, id)
                ),
                span,
            ))
        } else {
            let callee = self.check_expr(scopes, callee, None);
            match callee.ty {
                TypeId::Func(func) => {
                    let f = scopes.get_func(func);
                    let ret = f.proto.ret.clone();
                    if f.inst {
                        let st = scopes.get_user_type(*ret.as_user_type().unwrap());
                        if st
                            .data
                            .as_struct()
                            .unwrap()
                            .iter()
                            .any(|member| !member.1.public)
                            && !scopes.is_sub_scope(st.scope)
                        {
                            return CheckedExpr::new(
                                ret,
                                self.error(Error::new(
                                    "cannot construct type with private members",
                                    span,
                                )),
                            );
                        }

                        let mut generics = vec![TypeId::Unknown; f.proto.type_params.len()];
                        return CheckedExpr::new(
                            ret,
                            ExprData::Instance(self.check_instance_args(
                                scopes,
                                args,
                                &f.proto.params.clone(),
                                &mut generics,
                                span,
                            )),
                        );
                    }

                    let (args, ret) = self.check_args_with_generics(
                        scopes,
                        args,
                        &f.proto.params.clone(),
                        &f.proto.type_params.to_vec(),
                        None,
                        ret,
                        target,
                        span,
                    );

                    CheckedExpr::new(ret, ExprData::Call { func, args })
                }
                TypeId::GenericFunc(func) => {
                    let f = scopes.get_func(func.id);
                    let (args, ret) = self.check_args_with_generics(
                        scopes,
                        args,
                        &f.proto.params.clone(),
                        &f.proto.type_params.to_vec(),
                        Some(func.generics),
                        f.proto.ret.clone(),
                        target,
                        span,
                    );

                    CheckedExpr::new(
                        ret,
                        ExprData::Call {
                            func: func.id,
                            args,
                        },
                    )
                }
                _ => self.error(Error::new(
                    format!(
                        "cannot call value of type '{}'",
                        Self::type_name(scopes, &callee.ty)
                    ),
                    span,
                )),
            }
        }
    }

    fn consteval(
        scopes: &Scopes,
        expr: &Located<Expr>,
        target: Option<&TypeId>,
    ) -> Result<usize, Error> {
        match &expr.data {
            Expr::Integer { base, value, width } => {
                if let Some(width) = width.as_ref().and_then(|width| Self::match_int_type(width)) {
                    if let Some(target) = target {
                        if target != &width {
                            return Err(type_mismatch!(scopes, target, &width, expr.span));
                        }
                    }
                }

                match usize::from_str_radix(value, *base as u32) {
                    Ok(value) => Ok(value),
                    Err(_) => Err(Error::new("value cannot be converted to usize", expr.span)),
                }
            }
            _ => Err(Error::new(
                "expression is not compile time evaluatable",
                expr.span,
            )),
        }
    }

    fn forward_declare_fn(&mut self, scopes: &mut Scopes, f: &Fn) {
        let checked = Function {
            proto: CheckedPrototype {
                public: f.proto.public,
                name: f.proto.name.clone(),
                is_async: f.proto.is_async,
                is_extern: f.proto.is_extern,
                type_params: f.proto.type_params.clone(),
                params: f
                    .proto
                    .params
                    .iter()
                    .map(|param| CheckedParam {
                        mutable: param.mutable,
                        keyword: param.keyword,
                        name: param.name.clone(),
                        ty: Self::fd_resolve_type(scopes, &param.ty).unwrap_or(TypeId::Unknown),
                    })
                    .collect(),
                ret: Self::fd_resolve_type(scopes, &f.proto.ret).unwrap_or(TypeId::Unknown),
            },
            body: None,
            inst: false,
        };
        let id = scopes.insert_func(checked);
        scopes.enter(
            Some(f.proto.name.clone()),
            ScopeKind::Function(id),
            |scopes| {
                if !f.proto.type_params.is_empty() {
                    for (i, param) in f.proto.type_params.iter().enumerate() {
                        scopes.insert_user_type(UserType {
                            public: false,
                            name: param.clone(),
                            body_scope: scopes.current_id(),
                            data: UserTypeData::Generic(i),
                        });
                    }

                    for (i, original) in f.proto.params.iter().enumerate() {
                        scopes.get_func_mut(id).proto.params[i].ty =
                            Self::fd_resolve_type(scopes, &original.ty).unwrap_or(TypeId::Unknown);
                    }

                    scopes.get_func_mut(id).proto.ret =
                        Self::fd_resolve_type(scopes, &f.proto.ret).unwrap_or(TypeId::Unknown);
                }

                for stmt in f.body.iter() {
                    self.forward_declare(scopes, stmt);
                }

                scopes.get_func_mut(id).body = Some(Block {
                    body: Vec::new(),
                    scope: scopes.current_id(),
                });
            },
        )
    }

    fn check_fn(&mut self, scopes: &mut Scopes, f: Fn) -> FunctionId {
        // TODO: disallow private type in public interface
        let id = scopes.find_func(&f.proto.name).unwrap();
        for i in 0..f.proto.params.len() {
            resolve_forward_declare!(
                self,
                scopes,
                scopes.get_func_mut(id).proto.params[i].ty,
                &f.proto.params[i].ty
            );
        }

        resolve_forward_declare!(
            self,
            scopes,
            scopes.get_func_mut(id).proto.ret,
            &f.proto.ret
        );
        scopes.get_func_mut(id).body.as_mut().unwrap().body =
            scopes.enter_id(scopes.get_func(id).body.as_ref().unwrap().scope, |scopes| {
                let params: Vec<_> = scopes
                    .get_func_mut(id)
                    .proto
                    .params
                    .iter()
                    .map(|param| Variable {
                        name: param.name.clone(),
                        ty: param.ty.clone(),
                        is_static: false,
                        public: false,
                        mutable: param.mutable,
                    })
                    .collect();

                for param in params {
                    scopes.insert_var(param);
                }

                f.body
                    .into_iter()
                    .map(|stmt| self.check_stmt(scopes, stmt))
                    .collect()
            });

        id
    }

    fn check_arg(
        &mut self,
        scopes: &mut Scopes,
        expr: Located<Expr>,
        param: &CheckedParam,
        generics: &mut [TypeId],
    ) -> CheckedExpr {
        if let Some(&index) = param
            .ty
            .as_user_type()
            .and_then(|ty| scopes.get_user_type(*ty).data.as_generic())
        {
            let expr = self.check_expr(scopes, expr, Some(&generics[index]));
            if generics[index].is_unknown() {
                generics[index] = expr.ty.clone();
            }

            return expr;
        }

        let span = expr.span;
        let expr = self.check_expr(scopes, expr, Some(&param.ty));
        if !Self::coerces_to(&expr.ty, &param.ty) {
            self.error(type_mismatch!(scopes, &param.ty, &expr.ty, span))
        } else {
            expr
        }
    }

    fn check_instance_args(
        &mut self,
        scopes: &mut Scopes,
        args: Vec<(Option<String>, Located<Expr>)>,
        params: &[CheckedParam],
        generics: &mut [TypeId],
        span: Span,
    ) -> HashMap<String, CheckedExpr> {
        let mut result = HashMap::with_capacity(args.len());
        let mut last_pos = 0;
        for (name, expr) in args {
            if let Some(name) = name {
                match result.entry(name.clone()) {
                    Entry::Occupied(_) => {
                        self.error::<()>(Error::new(
                            format!("parameter {name} has already been specified"),
                            expr.span,
                        ));
                    }
                    Entry::Vacant(entry) => {
                        if let Some(param) = params.iter().find(|p| p.name == name) {
                            entry.insert(self.check_arg(scopes, expr, param, generics));
                        } else {
                            self.error::<()>(Error::new(
                                format!("unknown parameter: {name}"),
                                expr.span,
                            ));
                        }
                    }
                }
            } else if let Some((i, param)) = params
                .iter()
                .enumerate()
                .skip(last_pos)
                .find(|(_, param)| !param.keyword)
            {
                result.insert(
                    param.name.clone(),
                    self.check_arg(scopes, expr, param, generics),
                );
                last_pos = i + 1;
            } else {
                // TODO: a better error here would be nice
                self.error::<()>(Error::new("too many positional arguments", expr.span));
            }
        }

        // for param in params
        //     .iter()
        //     .filter(|p| !result.contains_key(&p.name))
        //     .collect::<Vec<_>>()
        // {
        //     if let Some(default) = &param.default {
        //         result.insert(param.name.clone(), default.clone());
        //     }
        // }

        if params.len() != result.len() {
            self.error::<()>(Error::new(
                format!(
                    "expected {} argument(s), found {}",
                    params.len(),
                    result.len()
                ),
                span,
            ));
        }

        result
    }

    fn check_fn_args(
        &mut self,
        scopes: &mut Scopes,
        args: Vec<(Option<String>, Located<Expr>)>,
        params: &[CheckedParam],
        generics: &mut [TypeId],
        span: Span,
    ) -> Vec<CheckedExpr> {
        let mut args = self.check_instance_args(scopes, args, params, generics, span);
        if params.len() == args.len() {
            let mut result = Vec::with_capacity(args.len());
            for param in params {
                result.push(args.remove(&param.name).unwrap());
            }
            result
        } else {
            Vec::new()
        }
    }

    fn fd_resolve_type<'b>(scopes: &Scopes, ty: &'b TypeHint) -> Result<TypeId, ResolveError<'b>> {
        Ok(match ty {
            TypeHint::Regular { path, .. } => {
                if let Some(id) = Self::resolve_type_path(scopes, &path.data, path.span)? {
                    return Ok(TypeId::UserType(id));
                }

                if let Some(symbol) = path.data.as_symbol() {
                    return match symbol {
                        symbol if symbol == THIS_TYPE => scopes.current_struct(),
                        "void" => Some(TypeId::Void),
                        "never" => Some(TypeId::Never),
                        "f32" => Some(TypeId::F32),
                        "f64" => Some(TypeId::F64),
                        "bool" => Some(TypeId::Bool),
                        "str" => Some(TypeId::String),
                        _ => Self::match_int_type(symbol),
                    }
                    .ok_or(ResolveError::Path(path));
                }

                return Err(ResolveError::Path(path));
            }
            TypeHint::Void => TypeId::Void,
            TypeHint::Ref(ty) => TypeId::Ptr(Self::fd_resolve_type(scopes, ty)?.into()),
            TypeHint::RefMut(ty) => TypeId::MutPtr(Self::fd_resolve_type(scopes, ty)?.into()),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                scopes
                    .current_struct()
                    .map(|s| TypeId::Ptr(s.into()))
                    .expect("ICE: this outside of method")
            }
            TypeHint::MutThis => scopes
                .current_struct()
                .map(|s| TypeId::MutPtr(s.into()))
                .expect("ICE: this outside of method"),
            TypeHint::Array(ty, count) => {
                let n = Self::consteval(scopes, count, Some(&TypeId::Usize))?;
                TypeId::Array((Self::fd_resolve_type(scopes, ty)?, n).into())
            }
            TypeHint::Option(ty) => TypeId::Option(Self::fd_resolve_type(scopes, ty)?.into()),
            _ => todo!(),
        })
    }

    fn resolve_type(&mut self, scopes: &Scopes, ty: &TypeHint) -> TypeId {
        Self::fd_resolve_type(scopes, ty).unwrap_or_else(|err| match err {
            ResolveError::Error(err) => self.error(err),
            ResolveError::Path(name) => self.error(Error::new(
                format!("undefined type '{}'", name.span.text(self.src)),
                name.span,
            )),
        })
    }

    fn create_block(
        &mut self,
        scopes: &mut Scopes,
        name: Option<String>,
        body: Vec<Located<Stmt>>,
        kind: ScopeKind,
    ) -> Block {
        scopes.enter(name, kind, |scopes| Block {
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
            _ => match name {
                "usize" => Some(TypeId::Usize),
                "isize" => Some(TypeId::Isize),
                _ => None,
            },
        }
    }

    fn coerces_to(ty: &TypeId, target: &TypeId) -> bool {
        match (ty, target) {
            (
                TypeId::IntGeneric,
                TypeId::Int(_) | TypeId::Uint(_) | TypeId::Isize | TypeId::Usize,
            ) => true,
            (TypeId::FloatGeneric, TypeId::F32 | TypeId::F64) => true,
            (TypeId::MutPtr(ty), TypeId::Ptr(target)) if ty == target => true,
            (ty, TypeId::Option(inner)) if Self::coerces_to(ty, inner) => true,
            (TypeId::Never, _) => true,
            (ty, target) => ty == target,
        }
    }

    fn is_assignable(scopes: &Scopes, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, TypeId::MutPtr(_))
            }
            ExprData::Symbol(_) | ExprData::Member { .. } => Self::can_addrmut(scopes, expr),
            ExprData::Subscript { callee, .. } => Self::is_assignable(scopes, callee),
            _ => false,
        }
    }

    fn can_addrmut(scopes: &Scopes, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                !matches!(op, UnaryOp::Deref) || matches!(expr.ty, TypeId::MutPtr(_))
            }
            ExprData::Symbol(symbol) => match symbol {
                Symbol::Func(_) | Symbol::GenericFunc(_) => false,
                Symbol::Var(id) => scopes.get_var(*id).mutable,
            },
            ExprData::Member { source, .. } => {
                matches!(source.ty, TypeId::MutPtr(_)) || Self::can_addrmut(scopes, source)
            }
            ExprData::Subscript { callee, .. } => Self::can_addrmut(scopes, callee),
            _ => true,
        }
    }

    fn type_name(scopes: &Scopes, ty: &TypeId) -> String {
        match ty {
            TypeId::Void => "void".into(),
            TypeId::Never => "never".into(),
            TypeId::Int(bits) => format!("i{bits}"),
            TypeId::Uint(bits) => format!("u{bits}"),
            TypeId::Unknown => "{unknown}".into(),
            TypeId::F32 => "f32".into(),
            TypeId::F64 => "f64".into(),
            TypeId::Bool => "bool".into(),
            TypeId::IntGeneric => "{integer}".into(),
            TypeId::FloatGeneric => "{float}".into(),
            TypeId::String => "str".into(),
            TypeId::Ptr(id) => format!("*{}", Self::type_name(scopes, id)),
            TypeId::MutPtr(id) => format!("*mut {}", Self::type_name(scopes, id)),
            TypeId::Option(id) => format!("?{}", Self::type_name(scopes, id)),
            TypeId::Func(id) => {
                let f = scopes.get_func(*id);
                let mut result = format!("fn {}(", f.proto.name);
                for (i, param) in f.proto.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&format!(
                        "{}: {}",
                        param.name,
                        Self::type_name(scopes, &param.ty)
                    ));
                }
                format!("{result}) {}", Self::type_name(scopes, &f.proto.ret))
            }
            TypeId::GenericFunc(func) => {
                let f = scopes.get_func(func.id);
                let mut result = format!("fn {}<", f.proto.name);
                for (i, (param, concrete)) in f
                    .proto
                    .type_params
                    .iter()
                    .zip(func.generics.iter())
                    .enumerate()
                {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&format!("{param} = {}", Self::type_name(scopes, concrete)));
                }
                result.push_str(">(");
                for (i, param) in f.proto.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&format!(
                        "{}: {}",
                        param.name,
                        Self::type_name(scopes, &param.ty)
                    ));
                }
                format!("{result}) {}", Self::type_name(scopes, &f.proto.ret))
            }
            TypeId::UserType(id) => scopes.get_user_type(*id).name.clone(),
            TypeId::Array(inner) => format!("[{}; {}]", Self::type_name(scopes, &inner.0), inner.1),
            TypeId::Isize => "isize".into(),
            TypeId::Usize => "usize".into(),
            TypeId::GenericPlaceholder(_) => unreachable!(),
        }
    }

    fn resolve_path_to_end(scopes: &Scopes, path: &Path, span: Span) -> Result<ScopeId, Error> {
        let mut scope = ScopeId(0);
        let mut start = 0;
        if !path.root {
            if path.components[0].0 == "super" {
                start = 1;
                if let Some(module) = scopes.module_of(
                    scopes[scopes.module_of(scopes.current_id()).unwrap()]
                        .parent
                        .unwrap(),
                ) {
                    scope = module;
                } else {
                    return Err(Error::new("cannot use super here", span));
                }
            } else {
                scope = scopes.module_of(scopes.current_id()).unwrap();
            }
        }

        for part in path.components[start..path.components.len() - 1].iter() {
            for (name, id) in scopes[scope].children.iter() {
                if name == &part.0 {
                    // TODO: skip the public check if we ended up in our own module
                    match &scopes[*id].kind {
                        ScopeKind::Module(public) => {
                            if !*public {
                                return Err(Error::new(
                                    format!("cannot access private module {name}"),
                                    span,
                                ));
                            }
                        }
                        ScopeKind::UserType(id) => {
                            let ty = scopes.get_user_type(*id);
                            if !ty.public
                                && scopes.module_of(ty.scope)
                                    != scopes.module_of(scopes.current_id())
                            {
                                return Err(Error::new(
                                    format!("cannot access members of private struct {name}"),
                                    span,
                                ));
                            }
                        }
                        _ => continue,
                    }

                    scope = *id;
                    break;
                }
            }
        }

        Ok(scope)
    }

    fn resolve_func(
        &mut self,
        scopes: &Scopes,
        id: FunctionId,
        generics: &[TypeHint],
        span: Span,
    ) -> Result<Symbol, Error> {
        let func = scopes.get_func(id);
        if !func.proto.type_params.is_empty() {
            if generics.is_empty() {
                Ok(Symbol::GenericFunc(GenericFunc {
                    id,
                    generics: vec![TypeId::Unknown; func.proto.type_params.len()],
                }))
            } else if generics.len() != func.proto.type_params.len() {
                Err(Error::new(
                    format!(
                        "expected {} generic arguments, received {}",
                        func.proto.type_params.len(),
                        generics.len()
                    ),
                    span,
                ))
            } else {
                Ok(Symbol::GenericFunc(GenericFunc {
                    id,
                    generics: generics
                        .iter()
                        .map(|ty| self.resolve_type(scopes, ty))
                        .collect(),
                }))
            }
        } else {
            Ok(Symbol::Func(id))
        }
    }

    fn resolve_path(
        &mut self,
        scopes: &Scopes,
        path: &Path,
        span: Span,
    ) -> Result<Option<Symbol>, Error> {
        let last = path.components.last().unwrap();
        let var = if path.components.len() == 1 {
            if let Some(var) = scopes.find_var(&last.0) {
                Some(var)
            } else if let Some(func) = scopes.find_func(&last.0) {
                return Ok(Some(self.resolve_func(scopes, func, &last.1, span)?));
            } else {
                None
            }
        } else {
            let scope = Self::resolve_path_to_end(scopes, path, span)?;
            if let Some(id) = scopes.find_var_in(&last.0, scope) {
                let var = scopes.get_var(id);
                if !var.public {
                    return Err(Error::new(
                        format!("variable '{}' is private", var.name),
                        span,
                    ));
                }

                Some(id)
            } else if let Some(id) = scopes.find_func_in(&last.0, scope) {
                let func = scopes.get_func(id);
                if !func.proto.public {
                    return Err(Error::new(
                        format!("function '{}' is private", func.proto.name),
                        span,
                    ));
                }

                return Ok(Some(self.resolve_func(scopes, id, &last.1, span)?));
            } else {
                None
            }
        };

        if !path.components.last().unwrap().1.is_empty() {
            return Err(Error::new(
                "variables cannot be parameterized with generics",
                span,
            ));
        }

        Ok(var.map(Symbol::Var))
    }

    fn resolve_type_path(
        scopes: &Scopes,
        path: &Path,
        span: Span,
    ) -> Result<Option<UserTypeId>, Error> {
        let last = path.components.last().unwrap();
        let ty = if path.components.len() == 1 {
            scopes.find_user_type(&last.0)
        } else {
            // TODO: struct generic params
            let scope = Self::resolve_path_to_end(scopes, path, span)?;
            if let Some(id) = scopes.find_user_type_in(&last.0, scope) {
                let ty = scopes.get_user_type(id);
                if !ty.public {
                    return Err(Error::new(format!("type '{}' is private", ty.name), span));
                }

                Some(id)
            } else {
                None
            }
        };

        Ok(ty)
        // if let Some(ty) = ty {
        //     // if path.components[0].1.len() != scopes[&ty].data.type_params.len() {
        //     //     return Err(Error::new(
        //     //         format!(
        //     //             "expected {} generic arguments, received {}",
        //     //             scopes[&func].proto.type_params.len(),
        //     //             path.components[0].1.len()
        //     //         ),
        //     //         span,
        //     //     ));
        //     // }
        //     Ok(Some(ty))
        // } else {
        //     Ok(None)
        // }
    }
}
