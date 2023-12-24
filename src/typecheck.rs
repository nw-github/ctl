use std::{collections::HashMap, path::PathBuf};

use derive_more::{Constructor, Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use indexmap::{map::Entry, IndexMap, IndexSet};
use num_bigint::{BigInt, BigUint};
use num_traits::Num;

use crate::{
    ast::{
        checked::{
            Block, CheckedExpr, CheckedExprData, CheckedPattern, CheckedStmt, IrrefutablePattern,
        },
        declared::{DeclaredFn, DeclaredImplBlock, DeclaredStmt, DeclaredStmtData},
        parsed::{
            Destructure, Expr, ExprData, Fn, ImplBlock, Param, Path, Pattern, Stmt, StmtData,
            TypeHint,
        },
        Attribute, BinaryOp, UnaryOp,
    },
    lexer::{Located, Span},
    parser::ParsedFile,
    Error, Pipeline, THIS_PARAM, THIS_TYPE,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash, Constructor)]
pub struct GenericFunc {
    pub id: FunctionId,
    pub ty_args: Vec<TypeId>,
}

impl GenericFunc {
    fn infer_type_args(&mut self, mut src: &TypeId, mut target: &TypeId, scopes: &Scopes) {
        loop {
            match (src, target) {
                (TypeId::Ptr(gi), TypeId::Ptr(ti)) => {
                    src = gi;
                    target = ti;
                }
                (TypeId::Ptr(gi) | TypeId::MutPtr(gi), TypeId::MutPtr(ti)) => {
                    src = gi;
                    target = ti;
                }
                (TypeId::Array(gi), TypeId::Array(ti)) => {
                    src = &gi.0;
                    target = &ti.0;
                }
                (TypeId::FnPtr(src), TypeId::FnPtr(target)) => {
                    for (src, target) in src.params.iter().zip(target.params.iter()) {
                        self.infer_type_args(src, target, scopes);
                    }

                    self.infer_type_args(&src.ret, &target.ret, scopes);
                    break;
                }
                (TypeId::UserType(src), target) => {
                    if let Some(t) = target.as_user_type() {
                        if src.id != t.id {
                            if let Some(inner) = scopes
                                .as_option_inner(target)
                                .and_then(|i| i.as_user_type())
                            {
                                for (src, target) in src.ty_args.iter().zip(inner.ty_args.iter()) {
                                    self.infer_type_args(src, target, scopes);
                                }

                                break;
                            }
                        }

                        if !src.ty_args.is_empty() && !t.ty_args.is_empty() {
                            for (src, target) in src.ty_args.iter().zip(t.ty_args.iter()) {
                                self.infer_type_args(src, target, scopes);
                            }

                            break;
                        }
                    }

                    if let Some(&index) = scopes.get(src.id).data.as_func_template() {
                        if self.ty_args[index].is_unknown() {
                            self.ty_args[index] = target.clone();
                        }
                    }

                    break;
                }
                _ => break,
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Constructor)]
pub struct GenericUserType {
    pub id: UserTypeId,
    pub ty_args: Vec<TypeId>,
}

impl GenericUserType {
    pub fn name(&self, scopes: &Scopes) -> String {
        let mut result = scopes.get(self.id).name.clone();
        if !self.ty_args.is_empty() {
            result.push('<');
            for (i, concrete) in self.ty_args.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&concrete.name(scopes));
            }
            result.push('>');
        }

        result
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum CInt {
    Char,
    Short,
    Int,
    Long,
    LongLong,
}

pub struct IntStats {
    bits: u32,
    signed: bool,
}

impl IntStats {
    pub fn min_signed(&self) -> BigInt {
        -(BigInt::from(1) << (self.bits - 1))
    }

    pub fn max_signed(&self) -> BigInt {
        (BigInt::from(1) << (self.bits - 1)) - 1
    }

    pub fn max_unsigned(&self) -> BigUint {
        (BigUint::from(1u8) << self.bits) - 1u8
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnPtr {
    pub params: Vec<TypeId>,
    pub ret: TypeId,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum TypeId {
    Unknown(Option<Box<(TypeHint, ScopeId)>>),
    Void,
    Never,
    Int(u32),
    Uint(u32),
    CInt(CInt),
    CUint(CInt),
    CVoid,
    Isize,
    Usize,
    F32,
    F64,
    Bool,
    Char,
    FnPtr(Box<FnPtr>),
    UserType(Box<GenericUserType>),
    Ptr(Box<TypeId>),
    MutPtr(Box<TypeId>),
    Array(Box<(TypeId, usize)>),
    TraitSelf,
}

impl Default for TypeId {
    fn default() -> Self {
        Self::Unknown(None)
    }
}

impl PartialEq for TypeId {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unknown(_), Self::Unknown(_)) => true,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Uint(l0), Self::Uint(r0)) => l0 == r0,
            (Self::CInt(l0), Self::CInt(r0)) => l0 == r0,
            (Self::CUint(l0), Self::CUint(r0)) => l0 == r0,
            (Self::UserType(l0), Self::UserType(r0)) => l0 == r0,
            (Self::Ptr(l0), Self::Ptr(r0)) => l0 == r0,
            (Self::MutPtr(l0), Self::MutPtr(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::FnPtr(l0), Self::FnPtr(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::hash::Hash for TypeId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(l0) => l0.hash(state),
            Self::Uint(l0) => l0.hash(state),
            Self::CInt(l0) => l0.hash(state),
            Self::CUint(l0) => l0.hash(state),
            Self::FnPtr(l0) => l0.hash(state),
            Self::UserType(l0) => l0.hash(state),
            Self::Ptr(l0) => l0.hash(state),
            Self::MutPtr(l0) => l0.hash(state),
            Self::Array(l0) => l0.hash(state),
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}

impl Eq for TypeId {}

impl TypeId {
    pub fn supports_binop(&self, scopes: &Scopes, op: BinaryOp) -> bool {
        match op {
            BinaryOp::Add => matches!(
                self,
                TypeId::Int(_)
                    | TypeId::Isize
                    | TypeId::Uint(_)
                    | TypeId::Usize
                    | TypeId::F32
                    | TypeId::F64
                    | TypeId::CInt(_)
                    | TypeId::CUint(_)
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
                        | TypeId::CInt(_)
                        | TypeId::CUint(_)
                )
            }
            BinaryOp::And | BinaryOp::Xor | BinaryOp::Or | BinaryOp::Shl | BinaryOp::Shr => {
                matches!(
                    self,
                    TypeId::Int(_)
                        | TypeId::Uint(_)
                        | TypeId::Isize
                        | TypeId::Usize
                        | TypeId::CInt(_)
                        | TypeId::CUint(_)
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
                        | TypeId::Bool // FIXME: option<T> should be comparable with T without coercion
                        | TypeId::CInt(_)
                        | TypeId::CUint(_)
                        | TypeId::Char
                ) || matches!(self, TypeId::UserType(ut) if scopes.get(ut.id).data.is_enum())
            }
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                matches!(self, TypeId::Bool)
            }
            BinaryOp::NoneCoalesce => todo!(),
        }
    }

    pub fn strip_references(&self) -> &TypeId {
        let mut id = self;
        while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = id {
            id = inner;
        }
        id
    }

    pub fn matched_inner_type(&self, ty: TypeId) -> TypeId {
        if !(self.is_ptr() || self.is_mut_ptr()) {
            return ty;
        }

        let mut id = self;
        while let TypeId::MutPtr(inner) = id {
            id = inner;
        }

        if matches!(id, TypeId::Ptr(_)) {
            TypeId::Ptr(ty.into())
        } else {
            TypeId::MutPtr(ty.into())
        }
    }

    pub fn fill_struct_templates(&mut self, scopes: &Scopes, inst: &GenericUserType) {
        if inst.ty_args.is_empty() {
            return;
        }

        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    if !ty.ty_args.is_empty() {
                        for ty in ty.ty_args.iter_mut() {
                            ty.fill_struct_templates(scopes, inst);
                        }
                    } else if let Some(&index) = scopes.get(ty.id).data.as_struct_template() {
                        *src = inst.ty_args[index].clone();
                    }

                    break;
                }
                TypeId::FnPtr(f) => {
                    for ty in f.params.iter_mut() {
                        ty.fill_struct_templates(scopes, inst);
                    }

                    f.ret.fill_struct_templates(scopes, inst);
                    break;
                }
                _ => break,
            }
        }
    }

    pub fn fill_func_template(&mut self, scopes: &Scopes, func: &GenericFunc) {
        if func.ty_args.is_empty() {
            return;
        }

        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    if !ty.ty_args.is_empty() {
                        for ty in ty.ty_args.iter_mut() {
                            ty.fill_func_template(scopes, func);
                        }
                    } else if let Some(&index) = scopes.get(ty.id).data.as_func_template() {
                        if !func.ty_args[index].is_unknown() {
                            *src = func.ty_args[index].clone();
                        }
                    }

                    break;
                }
                TypeId::FnPtr(f) => {
                    for ty in f.params.iter_mut() {
                        ty.fill_func_template(scopes, func);
                    }

                    f.ret.fill_func_template(scopes, func);
                    break;
                }
                _ => break,
            }
        }
    }

    pub fn fill_this(&mut self, this: &TypeId) {
        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    for ty in ty.ty_args.iter_mut() {
                        ty.fill_this(this);
                    }

                    break;
                }
                TypeId::TraitSelf => {
                    *src = this.clone();
                    break;
                }
                TypeId::FnPtr(f) => {
                    for ty in f.params.iter_mut() {
                        ty.fill_this(this);
                    }

                    f.ret.fill_this(this);
                    break;
                }
                _ => break,
            }
        }
    }

    pub fn name(&self, scopes: &Scopes) -> String {
        match self {
            TypeId::Void => "void".into(),
            TypeId::Never => "never".into(),
            TypeId::Int(bits) => format!("i{bits}"),
            TypeId::Uint(bits) => format!("u{bits}"),
            // for debug purposes, ideally this should never be visible
            TypeId::Unknown(Some(_)) => "{unresolved}".into(),
            TypeId::Unknown(None) => "{unknown}".into(),
            TypeId::F32 => "f32".into(),
            TypeId::F64 => "f64".into(),
            TypeId::Bool => "bool".into(),
            TypeId::Char => "char".into(),
            TypeId::Ptr(id) => format!("*{}", id.name(scopes)),
            TypeId::MutPtr(id) => format!("*mut {}", id.name(scopes)),
            TypeId::FnPtr(f) => {
                let mut result = "fn(".to_string();
                for (i, param) in f.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&param.name(scopes));
                }
                format!("{result}) -> {}", f.ret.name(scopes))
            }
            TypeId::UserType(ty) => ty.name(scopes),
            TypeId::Array(inner) => format!("[{}; {}]", inner.0.name(scopes), inner.1),
            TypeId::Isize => "isize".into(),
            TypeId::Usize => "usize".into(),
            TypeId::CInt(ty) | TypeId::CUint(ty) => {
                let name = match ty {
                    crate::typecheck::CInt::Char => "char",
                    crate::typecheck::CInt::Short => "short",
                    crate::typecheck::CInt::Int => "int",
                    crate::typecheck::CInt::Long => "long",
                    crate::typecheck::CInt::LongLong => "longlong",
                };
                format!(
                    "c_{}{name}",
                    if matches!(self, TypeId::CUint(_)) {
                        "u"
                    } else {
                        ""
                    }
                )
            }
            TypeId::CVoid => "c_void".into(),
            TypeId::TraitSelf => "This".into(),
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            TypeId::Int(_)
                | TypeId::Uint(_)
                | TypeId::F32
                | TypeId::F64
                | TypeId::Isize
                | TypeId::Usize
                | TypeId::CInt(_)
                | TypeId::CUint(_)
        )
    }

    pub fn is_void_like(&self) -> bool {
        match self {
            TypeId::Void | TypeId::Never | TypeId::CVoid => true,
            //TypeId::Array(arr) => arr.1 == 0,
            _ => false,
        }
    }

    pub fn integer_stats(&self) -> Option<IntStats> {
        use std::ffi::*;

        let (bytes, signed) = match self {
            TypeId::Int(bits) | TypeId::Uint(bits) => (*bits / 8, matches!(self, TypeId::Int(_))),
            TypeId::CInt(cint) | TypeId::CUint(cint) => {
                let bits = match cint {
                    CInt::Char => std::mem::size_of::<c_char>(),
                    CInt::Short => std::mem::size_of::<c_short>(),
                    CInt::Int => std::mem::size_of::<c_int>(),
                    CInt::Long => std::mem::size_of::<c_long>(),
                    CInt::LongLong => std::mem::size_of::<c_longlong>(),
                };
                (bits as u32, matches!(self, TypeId::CInt(_)))
            }
            TypeId::Isize => (std::mem::size_of::<isize>() as u32, true),
            TypeId::Usize => (std::mem::size_of::<usize>() as u32, false),
            _ => return None,
        };

        Some(IntStats {
            bits: bytes * 8,
            signed,
        })
    }

    pub fn coerces_to(&self, scopes: &Scopes, target: &TypeId) -> bool {
        match (self, target) {
            (ty, target)
                if scopes
                    .as_option_inner(target)
                    .map_or(false, |inner| ty.coerces_to(scopes, inner)) =>
            {
                true
            }
            (TypeId::Never, _) => true,
            (ty, target) if ty.may_ptr_coerce(target) => true,
            (ty, target) => ty == target,
        }
    }

    pub fn indirection(&self) -> usize {
        let mut count = 0;
        let mut id = self;
        while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = id {
            id = inner;
            count += 1;
        }
        count
    }

    fn may_ptr_coerce(&self, target: &TypeId) -> bool {
        match (self, target) {
            (TypeId::MutPtr(ty), TypeId::Ptr(target)) if ty == target => true,
            (TypeId::MutPtr(ty), TypeId::MutPtr(target) | TypeId::Ptr(target)) => {
                ty.may_ptr_coerce(target)
            }
            (TypeId::Ptr(ty), TypeId::Ptr(target)) => ty.may_ptr_coerce(target),
            _ => false,
        }
    }

    fn from_int_name(name: &str) -> Option<TypeId> {
        let mut chars = name.chars();
        let (i, result): (_, fn(u32) -> TypeId) = match chars.next()? {
            'i' => (true, TypeId::Int),
            'u' => (false, TypeId::Uint),
            _ => return None,
        };

        match (
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next(),
        ) {
            (Some(a), None, None, None) => (!i || a > 1).then_some(result(a)),
            (Some(a), Some(b), None, None) => Some(result(a * 10 + b)),
            (Some(a), Some(b), Some(c), None) => Some(result(a * 100 + b * 10 + c)),
            _ => match name {
                "usize" => Some(TypeId::Usize),
                "isize" => Some(TypeId::Isize),
                _ => None,
            },
        }
    }

    fn resolve(&mut self, scopes: &mut Scopes, checker: &mut TypeChecker) {
        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    for ty in ty.ty_args.iter_mut() {
                        ty.resolve(scopes, checker);
                    }

                    break;
                }
                TypeId::Unknown(Some(hint)) => {
                    *src = scopes.enter_id(hint.1, |scopes| {
                        for UnresolvedUse {
                            public,
                            path,
                            all,
                            span,
                        } in
                            std::mem::take(scopes.unresolved_use_stmts(scopes.current).unwrap())
                        {
                            if let Some(path) = checker.resolve_path(scopes, &path, span) {
                                checker.resolve_use(scopes, public, all, path, span);
                            }
                        }

                        checker.resolve_type(scopes, &hint.0)
                    });
                    break;
                }
                TypeId::FnPtr(f) => {
                    for ty in f.params.iter_mut() {
                        ty.resolve(scopes, checker);
                    }

                    f.ret.resolve(scopes, checker);
                    break;
                }
                _ => break,
            }
        }
    }

    fn implements_trait(&self, scopes: &Scopes, bound: &GenericUserType) -> bool {
        let Some(this) = self.as_user_type() else {
            return false;
        };

        for tr in scopes.get(this.id).impls.iter() {
            let mut tr = tr.as_user_type().unwrap().clone();
            for ut in tr.ty_args.iter_mut() {
                ut.fill_struct_templates(scopes, this);
            }

            if &*tr == bound {
                return true;
            }
        }

        false
    }

    fn get_member_fn(
        &self,
        scopes: &Scopes,
        member: &str,
    ) -> Option<(Option<GenericUserType>, Vis<FunctionId>)> {
        let Some(ut) = self.as_user_type().map(|ut| scopes.get(ut.id)) else {
            return None;
        };

        if ut.data.is_struct() || ut.data.is_union() || ut.data.is_enum() {
            // TODO: trait implement overload ie.
            // impl Eq<f32> { ... } impl Eq<i32> { ... }
            for scope in std::iter::once(ut.body_scope)
                .chain(scopes[ut.body_scope].children.iter().map(|s| s.id))
            {
                if let Some(func) = scopes.find_in(member, scope) {
                    return Some((None, func));
                }
            }
        } else {
            for ut in ut.impls.iter().map(|ut| ut.as_user_type().unwrap()) {
                if let Some(func) = scopes.find_in(member, scopes.get(ut.id).body_scope) {
                    return Some((Some((**ut).clone()), func));
                }
            }
        }

        None
    }
}

macro_rules! id {
    ($name: ident => $output: ident,
     $vec: ident,
     $($parts:ident).+,
     $suffix: ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(usize);

        impl ItemId for $name {
            type Value = $output;

            fn get(self, scopes: &Scopes) -> &Scoped<Self::Value> {
                &scopes.$vec[self.0]
            }

            fn get_mut(self, scopes: &mut Scopes) -> &mut Scoped<Self::Value> {
                &mut scopes.$vec[self.0]
            }

            fn find(scopes: &Scopes, name: &str) -> Option<Vis<Self>> {
                for (id, scope) in scopes.iter() {
                    if let Some(item) = Self::find_in(scopes, name, id) {
                        return Some(item);
                    }

                    if matches!(scope.kind, ScopeKind::Module(_, _)) {
                        break;
                    }
                }

                None
            }

            fn find_in(scopes: &Scopes, name: &str, scope: ScopeId) -> Option<Vis<Self>> {
                scopes[scope].$vec
                    .iter()
                    .rev()
                    .find_map(|id| (scopes.$vec[id.0].$($parts).+ == name).then_some(*id))
            }

            fn insert(scopes: &mut Scopes, value: Self::Value, public: bool) -> Self {
                Self::insert_in(scopes, value, public, scopes.current)
            }

            fn insert_in(scopes: &mut Scopes, value: Self::Value, public: bool, scope: ScopeId) -> Self {
                let index = scopes.$vec.len();
                scopes.$vec.push(Scoped::new(value, scope));
                let id = $name(index);
                scopes[scope].$vec.insert(Vis { id, public });
                id
            }
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ScopeId(pub usize);

id!(FunctionId => Function, fns, name.data, func);
id!(UserTypeId => UserType, types, name, user_type);
id!(VariableId => Variable, vars, name, var);
id!(ExtensionId => Extension, exts, name, ext);

#[derive(Debug, Clone)]
pub struct UnresolvedUse {
    public: bool,
    path: Path,
    all: bool,
    span: Span,
}

#[derive(Default, Debug, Clone, EnumAsInner)]
pub enum ScopeKind {
    Block(Option<TypeId>, bool),
    Loop(Option<TypeId>, bool),
    Lambda(Option<TypeId>, bool),
    Function(FunctionId),
    UserType(UserTypeId),
    Module(String, Vec<UnresolvedUse>),
    Impl(UserTypeId),
    #[default]
    None,
}

impl ScopeKind {
    pub fn name<'a, 'b>(&'a self, scopes: &'b Scopes) -> Option<&'b str>
    where
        'a: 'b,
    {
        match self {
            &ScopeKind::Function(id) => Some(&scopes.get(id).name.data),
            &ScopeKind::UserType(id) => Some(&scopes.get(id).name),
            ScopeKind::Module(name, _) => Some(name),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum DefaultExpr {
    Unchecked(ScopeId, Expr),
    Checked(CheckedExpr),
}

#[derive(Debug, Clone)]
pub struct CheckedParam {
    pub mutable: bool,
    pub keyword: bool,
    pub name: String,
    pub ty: TypeId,
    default: Option<DefaultExpr>,
}

#[derive(Default, Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: TypeId,
    pub is_static: bool,
    pub mutable: bool,
    pub value: Option<CheckedExpr>,
}

#[derive(Debug)]
pub struct Function {
    pub attrs: Vec<Attribute>,
    pub name: Located<String>,
    pub is_async: bool,
    pub is_extern: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    pub type_params: Vec<UserTypeId>,
    pub params: Vec<CheckedParam>,
    pub ret: TypeId,
    pub body: Option<Vec<CheckedStmt>>,
    pub constructor: bool,
    pub body_scope: ScopeId,
    pub returns: bool,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub name: String,
    pub shared: bool,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub variants: Vec<Member>,
    pub is_unsafe: bool,
}

impl Union {
    pub fn tag_type(&self) -> TypeId {
        discriminant_type(self.variants.len())
    }

    pub fn variant_tag(&self, name: &str) -> Option<usize> {
        self.variants
            .iter()
            .filter(|m| !m.shared)
            .position(|m| m.name == name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TT {
    Struct,
    Func,
}

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct {
        members: Vec<Member>,
        init: FunctionId,
    },
    Union(Union),
    Enum(TypeId),
    Template(TT, usize),
    Trait,
}

impl UserTypeData {
    fn as_func_template(&self) -> Option<&usize> {
        if let Some((TT::Func, i)) = self.as_template() {
            Some(i)
        } else {
            None
        }
    }

    fn as_struct_template(&self) -> Option<&usize> {
        if let Some((TT::Struct, i)) = self.as_template() {
            Some(i)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct UserType {
    pub name: String,
    pub body_scope: ScopeId,
    pub data: UserTypeData,
    pub impls: Vec<TypeId>,
    pub type_params: Vec<UserTypeId>,
}

impl UserType {
    pub fn members(&self) -> Option<&[Member]> {
        match &self.data {
            UserTypeData::Struct { members, .. } => Some(members),
            UserTypeData::Union(union) => Some(&union.variants),
            _ => None,
        }
    }

    pub fn members_mut(&mut self) -> Option<&mut [Member]> {
        match &mut self.data {
            UserTypeData::Struct { members, .. } => Some(members),
            UserTypeData::Union(union) => Some(&mut union.variants),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Symbol {
    Func(GenericFunc),
    Var(VariableId),
}

#[derive(Debug)]
pub struct Extension {
    pub ty: TypeId,
    pub name: String,
    pub impls: Vec<TypeId>,
    pub type_params: Vec<UserTypeId>,
    pub body_scope: ScopeId,
}

#[derive(Deref, DerefMut, Constructor)]
pub struct Scoped<T> {
    #[deref]
    #[deref_mut]
    pub item: T,
    pub scope: ScopeId,
}

#[derive(Debug, Deref, DerefMut, Constructor, Copy, Clone)]
pub struct Vis<T> {
    #[deref]
    #[deref_mut]
    pub id: T,
    pub public: bool,
}

impl<T: std::hash::Hash> std::hash::Hash for Vis<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Vis<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(other)
    }
}

impl<T: Eq> Eq for Vis<T> {}

pub trait ItemId: Sized {
    type Value;

    fn get(self, scopes: &Scopes) -> &Scoped<Self::Value>;
    fn get_mut(self, scopes: &mut Scopes) -> &mut Scoped<Self::Value>;

    fn find(scopes: &Scopes, name: &str) -> Option<Vis<Self>>;
    fn find_in(scopes: &Scopes, name: &str, scope: ScopeId) -> Option<Vis<Self>>;

    fn insert(scopes: &mut Scopes, value: Self::Value, public: bool) -> Self;
    fn insert_in(scopes: &mut Scopes, value: Self::Value, public: bool, scope: ScopeId) -> Self;
}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub fns: IndexSet<Vis<FunctionId>>,
    pub types: IndexSet<Vis<UserTypeId>>,
    pub vars: IndexSet<Vis<VariableId>>,
    pub exts: IndexSet<Vis<ExtensionId>>,
    pub children: IndexSet<Vis<ScopeId>>,
}

pub struct Scopes {
    scopes: Vec<Scope>,
    current: ScopeId,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
    exts: Vec<Scoped<Extension>>,
    lang_types: HashMap<String, UserTypeId>,
    intrinsics: HashMap<FunctionId, String>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            current: ScopeId(0),
            fns: Vec::new(),
            types: Vec::new(),
            vars: Vec::new(),
            exts: Vec::new(),
            lang_types: HashMap::new(),
            intrinsics: HashMap::new(),
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
        for scope_name in self
            .iter_from(id)
            .flat_map(|(_, scope)| scope.kind.name(self))
        {
            name.reserve(scope_name.len() + 1);
            name.push('_');
            for c in scope_name.chars().rev() {
                name.push(c);
            }
        }

        name.chars().rev().collect::<String>()
    }

    pub fn current(&mut self) -> &mut Scope {
        let i = self.current;
        &mut self[i]
    }

    pub fn enter<T>(&mut self, kind: ScopeKind, public: bool, f: impl FnOnce(&mut Self) -> T) -> T {
        let id = ScopeId(self.scopes.len());
        self.current().children.insert(Vis { id, public });
        let parent = Some(self.current);
        self.enter_id(id, |this| {
            this.scopes.push(Scope {
                parent,
                kind,
                ..Default::default()
            });

            f(this)
        })
    }

    pub fn enter_id<T>(&mut self, id: ScopeId, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.current;
        self.current = id;
        let result = f(self);
        self.current = prev;
        result
    }

    pub fn this_type_of(&self, id: UserTypeId) -> TypeId {
        TypeId::UserType(
            GenericUserType::new(
                id,
                self.get(id)
                    .type_params
                    .iter()
                    .map(|&id| TypeId::UserType(GenericUserType::new(id, vec![]).into()))
                    .collect(),
            )
            .into(),
        )
    }

    pub fn current_this_type(&self) -> Option<TypeId> {
        self.iter().find_map(|(_, scope)| {
            if let ScopeKind::UserType(id) = scope.kind {
                let ty = self.get(id);
                if !ty.data.is_template() {
                    if ty.data.is_trait() {
                        return Some(TypeId::TraitSelf);
                    }

                    return Some(self.this_type_of(id));
                }
            }
            None
        })
    }

    pub fn current_function(&self) -> Option<FunctionId> {
        self.function_of(self.current)
    }

    pub fn function_of(&self, scope: ScopeId) -> Option<FunctionId> {
        self.iter_from(scope)
            .find_map(|(_, scope)| scope.kind.as_function().copied())
    }

    pub fn module_of(&self, id: ScopeId) -> Option<ScopeId> {
        for (id, current) in self.iter_from(id) {
            if matches!(current.kind, ScopeKind::Module(_, _)) {
                return Some(id);
            }
        }

        None
    }

    pub fn unresolved_use_stmts(&mut self, id: ScopeId) -> Option<&mut Vec<UnresolvedUse>> {
        self.module_of(id)
            .and_then(|id| self[id].kind.as_module_mut().map(|m| m.1))
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn find_module_in(&self, name: &str, scope: ScopeId) -> Option<Vis<ScopeId>> {
        self[scope]
            .children
            .iter()
            .find(|&&id| matches!(&self[*id].kind, ScopeKind::Module(mn, _) if name == mn))
            .copied()
    }

    pub fn find_module(&self, name: &str) -> Option<Vis<ScopeId>> {
        for (id, scope) in self.iter() {
            if let Some(item) = self.find_module_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_, _)) {
                break;
            }
        }

        None
    }

    pub fn find_free_fn(&self, name: &str) -> Option<Vis<FunctionId>> {
        for (id, scope) in self
            .iter()
            .filter(|(_, s)| !matches!(s.kind, ScopeKind::UserType(_)))
        {
            if let Some(item) = self.find_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_, _)) {
                break;
            }
        }

        None
    }

    pub fn get_option_id(&self) -> Option<UserTypeId> {
        self.lang_types.get("option").copied()
    }

    pub fn as_option_inner<'a>(&self, ty: &'a TypeId) -> Option<&'a TypeId> {
        self.get_option_id().and_then(|opt| {
            ty.as_user_type()
                .filter(|ut| ut.id == opt)
                .map(|ut| &ut.ty_args[0])
        })
    }

    pub fn make_lang_type(&self, name: &str, ty_args: Vec<TypeId>) -> Option<TypeId> {
        Some(TypeId::UserType(
            GenericUserType::new(self.lang_types.get(name).copied()?, ty_args).into(),
        ))
    }

    pub fn intrinsic_name(&self, id: FunctionId) -> Option<&str> {
        self.intrinsics.get(&id).map(|s| s.as_str())
    }

    pub fn get<T: ItemId>(&self, id: T) -> &Scoped<T::Value> {
        id.get(self)
    }

    pub fn get_mut<T: ItemId>(&mut self, id: T) -> &mut Scoped<T::Value> {
        id.get_mut(self)
    }

    pub fn find<T: ItemId>(&self, name: &str) -> Option<Vis<T>> {
        T::find(self, name)
    }

    pub fn find_in<T: ItemId>(&self, name: &str, scope: ScopeId) -> Option<Vis<T>> {
        T::find_in(self, name, scope)
    }

    pub fn insert<T: ItemId>(&mut self, value: T::Value, public: bool) -> T {
        T::insert(self, value, public)
    }

    // pub fn insert_in<T: ItemId>(&mut self, value: T::Value, public: bool, scope: ScopeId) -> T {
    //     T::insert_in(self, value, public, scope)
    // }

    fn can_access_privates(&self, scope: ScopeId) -> bool {
        let target = self
            .module_of(scope)
            .expect("root scope passed to can_access_privates()");
        self.iter().any(|(id, _)| id == target)
    }
}

#[derive(Debug, EnumAsInner)]
pub enum ResolvedPath {
    UserType(GenericUserType),
    Func(GenericFunc),
    Var(VariableId),
    Module(ScopeId),
    None(Error),
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

macro_rules! type_mismatch {
    ($scopes: expr, $expected: expr, $actual: expr, $span: expr) => {
        Error::new(
            format!(
                "type mismatch: expected type '{}', got '{}'",
                $expected.name($scopes),
                $actual.name($scopes),
            ),
            $span,
        )
    };
}

macro_rules! private_member {
    ($scopes: expr, $id: expr, $member: expr, $span: expr) => {
        Error::new(
            format!(
                "cannot access private member '{}' of type '{}'",
                $member.name,
                $id.name($scopes)
            ),
            $span,
        )
    };
}

macro_rules! type_check_bail {
    ($self: expr, $scopes: expr, $source: expr, $target: expr) => {{
        let source = $source;
        let span = source.span;
        let source = $self.check_expr($scopes, source, Some($target));
        if !source.ty.coerces_to($scopes, $target) {
            return $self.error(type_mismatch!($scopes, $target, source.ty, span));
        }

        source.coerce_to($target, $scopes)
    }};
}

macro_rules! resolve_type {
    ($self: expr, $scopes: expr, $ty: expr) => {
        let mut ty = $ty.clone();
        ty.resolve($scopes, $self);
        $ty = ty;
    };
}

pub struct Module {
    pub scopes: Scopes,
    pub errors: Vec<Error>,
    pub files: Vec<PathBuf>,
    pub scope: ScopeId,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum Safety {
    #[default]
    Safe,
    Unsafe,
}

pub struct TypeChecker {
    errors: Vec<Error>,
    universal: Vec<ScopeId>,
    safety: Safety,
    decl: bool,
}

impl TypeChecker {
    pub fn check(
        path: &std::path::Path,
        module: Vec<ParsedFile>,
        libs: Vec<PathBuf>,
    ) -> anyhow::Result<Module> {
        let mut this = Self {
            errors: vec![],
            universal: Vec::new(),
            safety: Safety::Safe,
            decl: true,
        };
        let mut scopes = Scopes::new();
        let mut files: Vec<_> = module.iter().map(|file| file.path.clone()).collect();

        for lib in libs {
            let parsed = Pipeline::new(lib, files.len()).parse()?;
            this.check_one(&mut scopes, &parsed.path, parsed.state.0, &mut files);
        }

        Ok(Module {
            scope: this.check_one(&mut scopes, path, module, &mut vec![]),
            scopes,
            errors: this.errors,
            files,
        })
    }

    fn check_one(
        &mut self,
        scopes: &mut Scopes,
        path: &std::path::Path,
        module: Vec<ParsedFile>,
        paths: &mut Vec<PathBuf>,
    ) -> ScopeId {
        let project = crate::derive_module_name(path);
        scopes.enter(
            ScopeKind::Module(project.clone(), Vec::new()),
            true,
            |scopes| {
                paths.reserve(module.len());
                self.decl = true;
                let declared: Vec<_> = module
                    .into_iter()
                    .map(|file| {
                        self.errors.extend(file.errors);
                        paths.push(file.path);

                        let mut ast = Vec::new();
                        match file.ast.data {
                            StmtData::Module { name, body, .. } if name == project => {
                                self.include_universal(scopes);
                                ast.extend(
                                    body.into_iter().map(|stmt| self.declare_stmt(scopes, stmt)),
                                );
                            }
                            _ => ast.push(self.declare_stmt(scopes, file.ast)),
                        }
                        ast
                    })
                    .collect();
                self.decl = false;
                for ast in declared {
                    for stmt in ast {
                        self.check_stmt(scopes, stmt);
                    }
                }

                scopes.current
            },
        )
    }

    fn insert_type(
        &mut self,
        scopes: &mut Scopes,
        ty: UserType,
        public: bool,
        attrs: &[Attribute],
    ) -> UserTypeId {
        let id = scopes.insert(ty, public);
        if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "lang") {
            let Some(name) = attr.props.get(0) else {
                self.error::<()>(Error::new("language item must have name", attr.name.span));
                return id;
            };

            scopes.lang_types.insert(name.name.data.clone(), id);
        }

        id
    }

    fn declare_stmt(
        &mut self,
        scopes: &mut Scopes,
        Stmt { data, span, attrs }: Stmt,
    ) -> DeclaredStmt {
        let data = match data {
            StmtData::Module { public, name, body } => {
                scopes.enter(ScopeKind::Module(name, Vec::new()), public, |scopes| {
                    // FIXME: only allow core and std to define these
                    if attrs.iter().any(|attr| attr.name.data == "autouse") {
                        self.universal.push(scopes.current);
                    }

                    DeclaredStmtData::Module {
                        id: scopes.current,
                        body: body
                            .into_iter()
                            .map(|stmt| self.declare_stmt(scopes, stmt))
                            .collect(),
                    }
                })
            }
            StmtData::Struct(base) => {
                let init = self.declare_fn(
                    scopes,
                    true,
                    Fn {
                        public: base.public && !base.members.iter().any(|m| !m.public),
                        name: base.name.clone(),
                        is_async: false,
                        is_extern: false,
                        variadic: false,
                        is_unsafe: false,
                        type_params: base.type_params.clone(),
                        params: base
                            .members
                            .iter()
                            .map(|member| Param {
                                mutable: false,
                                keyword: true,
                                name: member.name.clone(),
                                ty: member.ty.clone(),
                                default: member.default.clone(),
                            })
                            .collect(),
                        ret: Self::typehint_for_struct(&base.name.data, &base.type_params, span),
                        body: None,
                    },
                    &attrs,
                );
                let id = self.insert_type(
                    scopes,
                    UserType {
                        name: base.name.data,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Struct {
                            members: Vec::new(),
                            init: init.id,
                        },
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    base.public,
                    &attrs,
                );
                scopes.enter(ScopeKind::UserType(id), base.public, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;
                    scopes.get_mut(id).type_params =
                        self.declare_type_params(scopes, TT::Struct, base.type_params);
                    *scopes.get_mut(id).data.as_struct_mut().unwrap().0 = base
                        .members
                        .into_iter()
                        .map(|member| Member {
                            public: member.public,
                            name: member.name,
                            shared: member.shared,
                            ty: self.resolve_type(scopes, &member.ty),
                        })
                        .collect();
                    let (impls, impl_blocks) = self.declare_impl_blocks(scopes, base.impls);
                    scopes.get_mut(id).impls = impls;

                    DeclaredStmtData::Struct {
                        init,
                        id,
                        impl_blocks,
                        functions: base
                            .functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
                            .collect(),
                    }
                })
            }
            StmtData::Union {
                tag: _,
                base,
                is_unsafe,
            } => {
                let tp = base.type_params.clone();
                let ret = Self::typehint_for_struct(&base.name.data, &tp, span);
                let id = self.insert_type(
                    scopes,
                    UserType {
                        name: base.name.data,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Union(Union {
                            variants: Vec::new(),
                            is_unsafe,
                        }),
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    base.public,
                    &attrs,
                );
                scopes.enter(ScopeKind::UserType(id), base.public, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;
                    scopes.get_mut(id).type_params =
                        self.declare_type_params(scopes, TT::Struct, base.type_params);

                    let mut variants = Vec::with_capacity(base.members.len());
                    let mut params = Vec::with_capacity(base.members.len());
                    for member in base.members.iter() {
                        variants.push(Member {
                            public: member.public,
                            name: member.name.clone(),
                            shared: member.shared,
                            ty: self.resolve_type(scopes, &member.ty),
                        });

                        if member.shared && is_unsafe {
                            // FIXME: span should be related to the member
                            self.error(Error::new(
                                "cannot have shared members in an unsafe union",
                                span,
                            ))
                        } else if member.shared {
                            params.push(Param {
                                mutable: false,
                                keyword: true,
                                name: member.name.clone(),
                                ty: member.ty.clone(),
                                default: member.default.clone(),
                            });
                        }
                    }

                    scopes.get_mut(id).data.as_union_mut().unwrap().variants = variants;

                    let (impls, impl_blocks) = self.declare_impl_blocks(scopes, base.impls);
                    scopes.get_mut(id).impls = impls;
                    let member_cons = base
                        .members
                        .into_iter()
                        .map(|member| {
                            let mut params = params.clone();
                            if !matches!(member.ty, TypeHint::Void) {
                                params.push(Param {
                                    mutable: false,
                                    keyword: false,
                                    name: member.name.clone(),
                                    ty: member.ty.clone(),
                                    default: member.default.clone(),
                                });
                            }

                            self.declare_fn(
                                scopes,
                                true,
                                Fn {
                                    public: base.public,
                                    name: Located::new(Span::default(), member.name.clone()),
                                    is_async: false,
                                    is_extern: false,
                                    variadic: false,
                                    is_unsafe: false,
                                    type_params: tp.clone(),
                                    params,
                                    ret: ret.clone(),
                                    body: None,
                                },
                                &[],
                            )
                        })
                        .collect();

                    DeclaredStmtData::Union {
                        member_cons,
                        id,
                        impl_blocks,
                        functions: base
                            .functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
                            .collect(),
                    }
                })
            }
            StmtData::Trait {
                public,
                name,
                type_params,
                impls,
                functions,
                is_unsafe: _,
            } => {
                let id = self.insert_type(
                    scopes,
                    UserType {
                        name,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Trait,
                        impls: Vec::new(),
                        type_params: Vec::new(),
                    },
                    public,
                    &attrs,
                );
                scopes.enter(ScopeKind::UserType(id), public, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;
                    scopes.get_mut(id).type_params =
                        self.declare_type_params(scopes, TT::Struct, type_params);

                    for path in impls.iter() {
                        if let Some(ty) = self.resolve_impl(scopes, path) {
                            scopes.get_mut(id).impls.push(ty);
                        }
                    }

                    DeclaredStmtData::Trait {
                        id,
                        functions: functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
                            .collect(),
                    }
                })
            }
            StmtData::Enum {
                public,
                name,
                impls,
                variants,
                functions,
            } => {
                // TODO: should be the largest variant
                let backing = discriminant_type(variants.len());
                let id = self.insert_type(
                    scopes,
                    UserType {
                        name: name.data,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Enum(backing.clone()),
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    public,
                    &attrs,
                );

                scopes.enter(ScopeKind::UserType(id), public, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;

                    for (i, (name, _)) in variants.iter().enumerate() {
                        scopes.insert::<VariableId>(
                            Variable {
                                name: name.clone(),
                                ty: TypeId::UserType(GenericUserType::new(id, vec![]).into()),
                                is_static: true,
                                mutable: false,
                                value: Some(CheckedExpr::new(
                                    backing.clone(),
                                    CheckedExprData::Unsigned(BigUint::from(i)),
                                )),
                            },
                            true,
                        );
                    }

                    let (impls, impl_blocks) = self.declare_impl_blocks(scopes, impls);
                    scopes.get_mut(id).impls = impls;
                    DeclaredStmtData::Enum {
                        id,
                        variants,
                        impl_blocks,
                        functions: functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
                            .collect(),
                    }
                })
            }
            StmtData::Extension {
                public,
                name,
                ty,
                type_params,
                impls,
                functions,
            } => {
                let id: ExtensionId = scopes.insert(
                    Extension {
                        name,
                        ty: self.resolve_type(scopes, &ty),
                        impls: Vec::new(),
                        type_params: Vec::new(),
                        body_scope: ScopeId(0),
                    },
                    public,
                );
                scopes.enter(ScopeKind::None, false, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;
                    scopes.get_mut(id).type_params =
                        self.declare_type_params(scopes, TT::Struct, type_params);

                    let (impls, impl_blocks) = self.declare_impl_blocks(scopes, impls);
                    scopes.get_mut(id).impls = impls;

                    DeclaredStmtData::Extension {
                        id,
                        impl_blocks,
                        functions: functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
                            .collect(),
                    }
                })
            }
            StmtData::Fn(f) => DeclaredStmtData::Fn(self.declare_fn(scopes, false, f, &attrs)),
            StmtData::Static {
                public,
                name,
                ty,
                value,
            } => {
                let id = scopes.insert(
                    Variable {
                        name,
                        ty: self.resolve_type(scopes, &ty),
                        is_static: true,
                        mutable: false,
                        value: None,
                    },
                    public,
                );

                DeclaredStmtData::Static { id, value }
            }
            StmtData::Use { path, public, all } => {
                if let Some(resolved) = self.resolve_path(scopes, &path, span) {
                    if resolved.is_none()
                        && scopes.module_of(scopes.current) == Some(scopes.current)
                    {
                        scopes
                            .unresolved_use_stmts(scopes.current)
                            .unwrap()
                            .push(UnresolvedUse {
                                path: path.clone(),
                                public,
                                all,
                                span,
                            });
                    }
                    self.resolve_use(scopes, public, all, resolved, span);
                }

                DeclaredStmtData::Use { public, path, all }
            }
            StmtData::Let {
                ty,
                value,
                mutable,
                patt,
            } => DeclaredStmtData::Let {
                ty,
                mutable,
                value,
                patt,
            },
            StmtData::Expr(expr) => DeclaredStmtData::Expr(expr),
            StmtData::Error => DeclaredStmtData::Error,
        };

        DeclaredStmt { data, span, attrs }
    }

    fn declare_fn(
        &mut self,
        scopes: &mut Scopes,
        constructor: bool,
        f: Fn,
        attrs: &[Attribute],
    ) -> DeclaredFn {
        if f.variadic && !f.is_extern {
            self.error(Error::new(
                "only extern functions may be variadic",
                f.name.span,
            ))
        }

        if scopes
            .find_in::<FunctionId>(&f.name.data, scopes.current)
            .is_some()
        {
            self.error(Error::new(
                format!("redeclaration of function '{}'", f.name.data),
                f.name.span,
            ))
        }

        let id = scopes.insert(
            Function {
                attrs: attrs.to_vec(),
                name: f.name,
                is_async: f.is_async,
                is_extern: f.is_extern,
                is_unsafe: f.is_unsafe,
                variadic: f.variadic,
                type_params: Vec::new(),
                params: Vec::new(),
                ret: TypeId::Unknown(None),
                body: None,
                body_scope: ScopeId(0),
                returns: false,
                constructor,
            },
            f.public,
        );

        if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "intrinsic") {
            if let Some(attr) = attr.props.get(0) {
                match attr.name.data.as_str() {
                    "size_of" => {
                        scopes.intrinsics.insert(id, attr.name.data.clone());
                    }
                    _ => self.error(Error::new(
                        format!("intrinsic '{}' is not supported", attr.name.data),
                        attr.name.span,
                    )),
                }
            } else {
                self.error(Error::new(
                    "intrinsic function must have name",
                    attr.name.span,
                ))
            }
        }

        scopes.enter(ScopeKind::Function(id), false, |scopes| {
            scopes.get_mut(id).body_scope = scopes.current;
            scopes.get_mut(id).type_params =
                self.declare_type_params(scopes, TT::Func, f.type_params);
            scopes.get_mut(id).params = f
                .params
                .into_iter()
                .map(|param| CheckedParam {
                    mutable: param.mutable,
                    keyword: param.keyword,
                    name: param.name,
                    ty: self.resolve_type(scopes, &param.ty),
                    default: param
                        .default
                        .map(|expr| DefaultExpr::Unchecked(scopes.current, expr)),
                })
                .collect();
            scopes.get_mut(id).ret = self.resolve_type(scopes, &f.ret);

            DeclaredFn {
                id,
                body: f.body.map(|body| {
                    body.into_iter()
                        .map(|stmt| self.declare_stmt(scopes, stmt))
                        .collect()
                }),
            }
        })
    }

    fn declare_type_params(
        &mut self,
        scopes: &mut Scopes,
        tt: TT,
        type_params: Vec<(String, Vec<Located<Path>>)>,
    ) -> Vec<UserTypeId> {
        type_params
            .into_iter()
            .enumerate()
            .map(|(i, (name, impls))| {
                scopes.insert(
                    UserType {
                        name,
                        body_scope: scopes.current,
                        data: UserTypeData::Template(tt, i),
                        type_params: Vec::new(),
                        impls: impls
                            .iter()
                            .flat_map(|path| self.resolve_impl(scopes, path))
                            .collect(),
                    },
                    false,
                )
            })
            .collect()
    }

    fn declare_impl_blocks(
        &mut self,
        scopes: &mut Scopes,
        blocks: Vec<ImplBlock>,
    ) -> (Vec<TypeId>, Vec<DeclaredImplBlock>) {
        let mut impls = Vec::new();
        let mut declared_blocks = Vec::new();
        for block in blocks {
            let ty = self.resolve_impl(scopes, &block.path).unwrap_or_default();
            let impl_index = impls.len();
            impls.push(ty);

            declared_blocks.push(scopes.enter(ScopeKind::None, false, |scopes| {
                DeclaredImplBlock {
                    impl_index,
                    span: block.path.span,
                    scope: scopes.current,
                    functions: block
                        .functions
                        .into_iter()
                        .map(|f| self.declare_fn(scopes, false, f, &[]))
                        .collect(),
                }
            }));
        }

        (impls, declared_blocks)
    }

    fn check_stmt(&mut self, scopes: &mut Scopes, stmt: DeclaredStmt) -> CheckedStmt {
        match stmt.data {
            DeclaredStmtData::Module { id, body } => {
                return CheckedStmt::Module(scopes.enter_id(id, |scopes| {
                    self.include_universal(scopes);
                    Block {
                        body: body
                            .into_iter()
                            .map(|stmt| self.check_stmt(scopes, stmt))
                            .collect(),
                        scope: scopes.current,
                    }
                }))
            }
            DeclaredStmtData::Struct {
                init,
                id,
                impl_blocks,
                functions,
            } => {
                self.check_fn(scopes, init);
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    self.check_impl_blocks(scopes, id, impl_blocks);
                    for i in 0..scopes.get(id).data.as_struct().unwrap().0.len() {
                        resolve_type!(
                            self,
                            scopes,
                            scopes.get_mut(id).data.as_struct_mut().unwrap().0[i].ty
                        );
                    }

                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Union {
                member_cons,
                id,
                impl_blocks,
                functions,
            } => {
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    self.check_impl_blocks(scopes, id, impl_blocks);
                    for (i, f) in member_cons.into_iter().enumerate() {
                        resolve_type!(
                            self,
                            scopes,
                            scopes.get_mut(id).data.as_union_mut().unwrap().variants[i].ty
                        );

                        self.check_fn(scopes, f);
                    }

                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Trait { id, functions } => {
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Enum {
                id,
                variants,
                functions,
                impl_blocks,
            } => {
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    self.check_impl_blocks(scopes, id, impl_blocks);

                    for (name, expr) in variants {
                        if let Some(expr) = expr {
                            let var: VariableId = *scopes.find(&name).unwrap();
                            scopes.get_mut(var).value =
                                Some(self.check_expr(scopes, expr, Some(&TypeId::Usize)));
                        }
                    }

                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Extension {
                id,
                impl_blocks,
                functions,
            } => {
                todo!()
            }
            DeclaredStmtData::Expr(expr) => {
                return CheckedStmt::Expr(self.check_expr(scopes, expr, None))
            }
            DeclaredStmtData::Let {
                ty,
                mutable,
                value,
                patt,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty);
                    if let Some(value) = value {
                        let value = self.type_check(scopes, value, &ty);
                        return self.check_var_stmt(scopes, ty, Some(value), mutable, patt);
                    } else {
                        return self.check_var_stmt(scopes, ty, None, mutable, patt);
                    }
                } else if let Some(value) = value {
                    let value = self.check_expr(scopes, value, None);
                    return self.check_var_stmt(
                        scopes,
                        value.ty.clone(),
                        Some(value),
                        mutable,
                        patt,
                    );
                } else {
                    return self.error(Error::new("cannot infer type", stmt.span));
                }
            }
            DeclaredStmtData::Fn(f) => self.check_fn(scopes, f),
            DeclaredStmtData::Static { id, value } => {
                // FIXME: detect cycles like static X: usize = X;
                // FIXME: non-const statics should be disallowed
                let mut ty = scopes.get_mut(id).ty.clone();
                ty.resolve(scopes, self);
                (scopes.get_mut(id).ty) = ty.clone();

                let value = self.type_check(scopes, value, &ty);
                let var = scopes.get_mut(id);
                var.value = Some(value);
            }
            DeclaredStmtData::Use { public, path, all } => {
                if let Some(path) = self.resolve_path(scopes, &path, stmt.span) {
                    self.resolve_use(scopes, public, all, path, stmt.span);
                }
            }
            DeclaredStmtData::Error => return CheckedStmt::Error,
        }

        CheckedStmt::None
    }

    fn check_var_stmt(
        &mut self,
        scopes: &mut Scopes,
        ty: TypeId,
        value: Option<CheckedExpr>,
        mutable: bool,
        patt: Pattern,
    ) -> CheckedStmt {
        let span = *patt.span();
        match self.declare_variables(scopes, ty, mutable, patt) {
            Some(IrrefutablePattern::Variable(id)) => {
                scopes.get_mut(id).value = value;
                CheckedStmt::Let(id)
            }
            Some(inner) => {
                let Some(value) = value else {
                    return self.error(Error::new(
                        "must provide a value with a destructuring assignment",
                        span,
                    ));
                };

                CheckedStmt::LetPattern(inner, value)
            }
            None => Default::default(),
        }
    }

    fn declare_variables(
        &mut self,
        scopes: &mut Scopes,
        ty: TypeId,
        mutable: bool,
        patt: Pattern,
    ) -> Option<IrrefutablePattern> {
        match patt {
            Pattern::PathWithBindings { .. } => unreachable!(),
            Pattern::Path(path) => {
                if let Some(name) = path.data.as_identifier() {
                    Some(IrrefutablePattern::Variable(scopes.insert(
                        Variable {
                            name: name.into(),
                            ty,
                            is_static: false,
                            mutable,
                            value: None,
                        },
                        false,
                    )))
                } else {
                    self.error(Error::new("variable name must be an identifier", path.span))
                }
            }
            Pattern::MutBinding(name) => Some(IrrefutablePattern::Variable(scopes.insert(
                Variable {
                    name: name.data,
                    ty,
                    is_static: false,
                    mutable: true,
                    value: None,
                },
                false,
            ))),
            Pattern::Option(_, Located { span, .. }) | Pattern::Null(span) => {
                self.error(Error::new("refutable pattern in variable binding", span))
            }
            Pattern::StructDestructure(patterns) => {
                let Some((ut, (members, _))) =
                    ty.strip_references().as_user_type().and_then(|ut| {
                        let ut = scopes.get(ut.id);
                        Some(ut).zip(ut.data.as_struct())
                    })
                else {
                    return self.error(Error::new(
                        format!(
                            "cannot destructure value of non-struct type '{}'",
                            ty.name(scopes)
                        ),
                        patterns.span,
                    ));
                };

                let cap = scopes.can_access_privates(ut.scope);
                let mut vars = Vec::new();
                for Destructure {
                    name,
                    mutable: p_mutable,
                    pattern,
                } in patterns.data
                {
                    let Some(member) = members.iter().find(|m| m.name == name.data) else {
                        self.error::<()>(Error::new(
                            format!("type '{}' has no member '{}'", ty.name(scopes), name.data),
                            patterns.span,
                        ));
                        continue;
                    };

                    if !member.public && !cap {
                        self.error::<()>(private_member!(scopes, ty, member, name.span));
                        continue;
                    }

                    // TODO: duplicates
                    vars.push((
                        name.data,
                        mutable || p_mutable,
                        ty.matched_inner_type(member.ty.clone()),
                        pattern,
                    ));
                }

                let mut result = Vec::with_capacity(vars.len());
                for (name, mutable, ty, patt) in vars {
                    if let Some(patt) = patt {
                        result.push((name, self.declare_variables(scopes, ty, mutable, patt)?));
                    } else {
                        result.push((
                            name.clone(),
                            IrrefutablePattern::Variable(scopes.insert(
                                Variable {
                                    name,
                                    ty,
                                    is_static: false,
                                    mutable,
                                    value: None,
                                },
                                false,
                            )),
                        ));
                    }
                }

                Some(IrrefutablePattern::Destrucure(result))
            }
        }
    }

    fn signatures_match(
        scopes: &Scopes,
        this: &TypeId,
        lhs_id: FunctionId,
        rhs_id: FunctionId,
        rhs_ty: &GenericUserType,
    ) -> Result<(), String> {
        let lhs = scopes.get(lhs_id);
        let rhs = scopes.get(rhs_id);
        let compare_types = |a: &TypeId, mut b: TypeId| {
            b.fill_func_template(
                scopes,
                &GenericFunc::new(
                    lhs_id,
                    lhs.type_params
                        .iter()
                        .map(|&id| TypeId::UserType(GenericUserType::new(id, vec![]).into()))
                        .collect(),
                ),
            );
            b.fill_struct_templates(scopes, rhs_ty);
            b.fill_this(this);

            if a != &b {
                Err(format!(
                    "expected '{}', got '{}'",
                    rhs.ret.name(scopes),
                    lhs.ret.name(scopes)
                ))
            } else {
                Ok(())
            }
        };

        if let Err(err) = compare_types(&lhs.ret, rhs.ret.clone()) {
            return Err(format!("return type is incorrect: {err}"));
        }

        for (s, t) in lhs.params.iter().zip(rhs.params.iter().cloned()) {
            if let Err(err) = compare_types(&s.ty, t.ty) {
                return Err(format!("parameter '{}' is incorrect: {err}", t.name));
            }
        }

        for (&s, &t) in lhs.type_params.iter().zip(rhs.type_params.iter()) {
            let s = scopes.get(s);
            let t = scopes.get(t);
            let name = &t.name;
            for (s, t) in s.impls.iter().zip(t.impls.iter()) {
                for (s, t) in s
                    .as_user_type()
                    .unwrap()
                    .ty_args
                    .iter()
                    .zip(t.as_user_type().unwrap().ty_args.clone().into_iter())
                {
                    if let Err(err) = compare_types(s, t) {
                        return Err(format!("type parameter '{name}' is incorrect: {err}"));
                    }
                }
            }

            if s.impls.len() != t.impls.len() {
                return Err(format!("type parameter '{name}' is incorrect"));
            }
        }

        if lhs.params.len() != rhs.params.len() {
            return Err(format!(
                "expected {} parameter(s), got {}",
                rhs.params.len(),
                lhs.params.len(),
            ));
        }

        if lhs.type_params.len() != rhs.type_params.len() {
            return Err(format!(
                "expected {} type parameter(s), got {}",
                rhs.type_params.len(),
                lhs.type_params.len(),
            ));
        }

        Ok(())
    }

    fn check_impl_block(
        &mut self,
        scopes: &mut Scopes,
        this: &TypeId,
        tr_ut: &GenericUserType,
        block: DeclaredImplBlock,
    ) {
        // TODO:
        //  - detect and fail on circular trait dependencies
        //  - default implementations
        let tr = scopes.get(tr_ut.id);
        for dep in tr.impls.iter().flat_map(|tr| tr.as_user_type()) {
            if !this.implements_trait(scopes, dep) {
                self.error(Error::new(
                    format!(
                        "trait '{}' requires implementation of trait '{}'",
                        tr_ut.name(scopes),
                        dep.name(scopes)
                    ),
                    block.span,
                ))
            }
        }

        let mut functions: Vec<_> = scopes[tr.body_scope].fns.iter().map(|f| f.id).collect();
        for f in block.functions {
            let Located {
                span: fn_span,
                data: fn_name,
            } = scopes.get(f.id).name.clone();
            let f_id = f.id;

            self.check_fn(scopes, f);
            let Some(pos) = functions
                .iter()
                .position(|&id| scopes.get(id).name.data == fn_name)
            else {
                self.error::<()>(Error::new(
                    format!(
                        "no function '{fn_name}' found in trait '{}'",
                        scopes.get(tr_ut.id).name
                    ),
                    fn_span,
                ));
                continue;
            };

            let tr_fn_id = functions.swap_remove(pos);
            self.resolve_proto(scopes, tr_fn_id);
            if let Err(err) = Self::signatures_match(scopes, this, f_id, tr_fn_id, tr_ut) {
                self.error(Error::new(
                    format!("invalid implementation of function '{fn_name}': {err}"),
                    fn_span,
                ))
            }
        }

        for id in functions {
            self.error(Error::new(
                format!(
                    "must implement '{}::{}'",
                    tr_ut.name(scopes),
                    scopes.get(id).name.data
                ),
                block.span,
            ))
        }
    }

    fn check_fn(&mut self, scopes: &mut Scopes, DeclaredFn { id, body }: DeclaredFn) {
        // TODO: disallow private type in public interface
        scopes.enter_id(scopes.get(id).body_scope, |scopes| {
            self.resolve_proto(scopes, id);
            for param in scopes
                .get(id)
                .params
                .iter()
                .map(|param| Variable {
                    name: param.name.clone(),
                    ty: param.ty.clone(),
                    is_static: false,
                    mutable: param.mutable,
                    value: None,
                })
                .collect::<Vec<_>>()
            {
                scopes.insert::<VariableId>(param, false);
            }

            if let Some(body) = body {
                let old_safety = std::mem::take(&mut self.safety);
                let body = body
                    .into_iter()
                    .map(|stmt| self.check_stmt(scopes, stmt))
                    .collect();
                let func = scopes.get_mut(id);
                func.body = Some(body);
                if !func.returns && !func.ret.is_void() {
                    let span = func.name.span;
                    let name = func.name.data.clone();
                    let ret = func.ret.clone().name(scopes);
                    self.error(Error::new(
                        format!("function '{name}' must return a value of type '{ret}'"),
                        span,
                    ))
                }

                self.safety = old_safety;
            }
        });
    }

    fn check_impl_blocks(
        &mut self,
        scopes: &mut Scopes,
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
    ) {
        for block in impls {
            // TODO:
            // - impl type params (impl<T> Trait<T>)
            // - implement the same trait more than once
            scopes.enter_id(block.scope, |scopes| {
                if let Some(gtr) = scopes.get(id).impls[block.impl_index].as_user_type() {
                    let gtr = gtr.clone();
                    self.check_impl_block(scopes, &scopes.this_type_of(id), &gtr, block);
                    scopes.current().kind = ScopeKind::Impl(gtr.id);
                } else {
                    for f in block.functions {
                        self.check_fn(scopes, f);
                    }
                }
            });
        }
    }

    fn check_expr_inner(
        &mut self,
        scopes: &mut Scopes,
        expr: Expr,
        target: Option<&TypeId>,
    ) -> CheckedExpr {
        macro_rules! lbox {
            ($e: expr) => {
                Located::new(Span::default(), $e).into()
            };
        }

        macro_rules! l {
            ($e: expr) => {
                Located::new(Span::default(), $e)
            };
        }

        let span = expr.span;
        match expr.data {
            ExprData::Binary { op, left, right } => {
                let left = self.check_expr(scopes, *left, target);
                let right = type_check_bail!(self, scopes, *right, &left.ty);
                if !left.ty.supports_binop(scopes, op) {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for values of type {} and {}",
                            &left.ty.name(scopes),
                            &right.ty.name(scopes)
                        ),
                        span,
                    ))
                } else {
                    CheckedExpr::new(
                        match op {
                            BinaryOp::NoneCoalesce => todo!(),
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
                        CheckedExprData::Binary {
                            op,
                            left: left.into(),
                            right: right.into(),
                        },
                    )
                }
            }
            ExprData::Unary { op, expr } => self.check_unary(scopes, *expr, target, op),
            ExprData::Call { callee, args } => self.check_call(scopes, target, *callee, args, span),
            ExprData::Array(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let ty = if let Some(TypeId::Array(inner)) = target {
                    inner.0.clone()
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(scopes, expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    return self.error(Error::new("cannot infer type of array literal", expr.span));
                };

                checked.extend(elements.map(|e| self.type_check(scopes, e, &ty)));
                CheckedExpr::new(
                    TypeId::Array(Box::new((ty, checked.len()))),
                    CheckedExprData::Array(checked),
                )
            }
            ExprData::Vec(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(vec) = scopes.lang_types.get("vec").copied() else {
                    return self.error(Error::new("missing language item: Vec", expr.span));
                };

                let ut = if let Some(ty) = target
                    .and_then(|target| target.as_user_type())
                    .filter(|ut| ut.id == vec)
                    .map(|ut| ut.ty_args[0].clone())
                {
                    ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(scopes, expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    return self
                        .error(Error::new("cannot infer type of vector literal", expr.span));
                };

                checked.extend(elements.map(|e| self.type_check(scopes, e, &ut)));
                CheckedExpr::new(
                    TypeId::UserType(GenericUserType::new(vec, vec![ut]).into()),
                    CheckedExprData::Vec(checked),
                )
            }
            ExprData::Set(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(set) = scopes.lang_types.get("set").copied() else {
                    return self.error(Error::new("missing language item: Set", expr.span));
                };

                let ut = if let Some(ty) = target
                    .and_then(|target| target.as_user_type())
                    .filter(|ut| ut.id == set)
                    .map(|ut| ut.ty_args[0].clone())
                {
                    ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(scopes, expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    return self.error(Error::new("cannot infer type of set literal", expr.span));
                };

                checked.extend(elements.map(|e| self.type_check(scopes, e, &ut)));
                CheckedExpr::new(
                    TypeId::UserType(GenericUserType::new(set, vec![ut]).into()),
                    CheckedExprData::Set(checked),
                )
            }
            ExprData::ArrayWithInit { init, count } => {
                if let Some(TypeId::Array(inner)) = target {
                    let init = self.type_check(scopes, *init, &inner.0);
                    match Self::consteval(scopes, &count, Some(&TypeId::Usize)) {
                        Ok(count) => CheckedExpr::new(
                            TypeId::Array(Box::new((init.ty.clone(), count))),
                            CheckedExprData::ArrayWithInit {
                                init: init.into(),
                                count,
                            },
                        ),
                        Err(err) => self.error(err),
                    }
                } else {
                    let init = self.check_expr(scopes, *init, target);
                    match Self::consteval(scopes, &count, Some(&TypeId::Usize)) {
                        Ok(count) => CheckedExpr::new(
                            TypeId::Array(Box::new((init.ty.clone(), count))),
                            CheckedExprData::ArrayWithInit {
                                init: init.into(),
                                count,
                            },
                        ),
                        Err(err) => self.error(err),
                    }
                }
            }
            ExprData::VecWithInit { init, count } => {
                let Some(vec) = scopes.lang_types.get("vec").copied() else {
                    return self.error(Error::new(
                        "no symbol 'Vec' found in this module",
                        expr.span,
                    ));
                };

                let (init, ty) = if let Some(ty) = target
                    .and_then(|target| target.as_user_type())
                    .filter(|ut| ut.id == vec)
                    .map(|ut| ut.ty_args[0].clone())
                {
                    (self.type_check(scopes, *init, &ty), ty)
                } else {
                    let expr = self.check_expr(scopes, *init, None);
                    let ty = expr.ty.clone();
                    (expr, ty)
                };

                CheckedExpr::new(
                    TypeId::UserType(GenericUserType::new(vec, vec![ty]).into()),
                    CheckedExprData::VecWithInit {
                        init: init.into(),
                        count: self.type_check(scopes, *count, &TypeId::Usize).into(),
                    },
                )
            }
            ExprData::Tuple(_) => todo!(),
            ExprData::Map(elements) => {
                // TODO: make sure the key type respects the trait bounds
                let Some(std_map) = scopes.lang_types.get("map").copied() else {
                    return self.error(Error::new(
                        "no symbol 'Map' found in this module",
                        expr.span,
                    ));
                };

                let mut result = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let (key_ty, val_ty) = if let Some((k, v)) = target
                    .and_then(|target| target.as_user_type())
                    .filter(|ut| ut.id == std_map)
                    .map(|ut| (ut.ty_args[0].clone(), ut.ty_args[1].clone()))
                {
                    (k, v)
                } else if let Some((key, val)) = elements.next() {
                    let key = self.check_expr(scopes, key, None);
                    let val = self.check_expr(scopes, val, None);

                    let k = key.ty.clone();
                    let v = val.ty.clone();
                    result.push((key, val));
                    (k, v)
                } else {
                    return self.error(Error::new("cannot infer type of map literal", expr.span));
                };

                result.extend(elements.map(|(key, val)| {
                    (
                        self.type_check(scopes, key, &key_ty),
                        self.type_check(scopes, val, &val_ty),
                    )
                }));

                CheckedExpr::new(
                    TypeId::UserType(GenericUserType::new(std_map, vec![key_ty, val_ty]).into()),
                    CheckedExprData::Map(result),
                )
            }
            ExprData::Range {
                start,
                end,
                inclusive,
            } => match (start, end) {
                // this could be skipped by just transforming these expressions to calls
                (Some(start), Some(end)) => {
                    let start = self.check_expr(scopes, *start, None);
                    let end = type_check_bail!(self, scopes, *end, &start.ty);
                    let item = if inclusive {
                        "range_inclusive"
                    } else {
                        "range"
                    };
                    CheckedExpr::new(
                        scopes.make_lang_type(item, vec![start.ty.clone()]).unwrap(),
                        CheckedExprData::Instance {
                            members: [("start".into(), start), ("end".into(), end)].into(),
                            variant: None,
                        },
                    )
                }
                (None, Some(end)) => {
                    let end = self.check_expr(scopes, *end, None);
                    let item = if inclusive {
                        "range_to_inclusive"
                    } else {
                        "range_to"
                    };
                    CheckedExpr::new(
                        scopes.make_lang_type(item, vec![end.ty.clone()]).unwrap(),
                        CheckedExprData::Instance {
                            members: [("end".into(), end)].into(),
                            variant: None,
                        },
                    )
                }
                (Some(start), None) => {
                    let start = self.check_expr(scopes, *start, None);
                    CheckedExpr::new(
                        scopes
                            .make_lang_type("range_from", vec![start.ty.clone()])
                            .unwrap(),
                        CheckedExprData::Instance {
                            members: [("start".into(), start)].into(),
                            variant: None,
                        },
                    )
                }
                (None, None) => todo!(),
            },
            ExprData::String(s) => CheckedExpr::new(
                scopes.make_lang_type("string", vec![]).unwrap(),
                CheckedExprData::String(s),
            ),
            ExprData::ByteString(s) => CheckedExpr::new(
                TypeId::Ptr(TypeId::Array((TypeId::Uint(8), s.len()).into()).into()),
                CheckedExprData::ByteString(s),
            ),
            ExprData::Char(s) => CheckedExpr::new(TypeId::Char, CheckedExprData::Char(s)),
            ExprData::ByteChar(c) => {
                CheckedExpr::new(TypeId::Uint(8), CheckedExprData::Unsigned(BigUint::from(c)))
            }
            ExprData::None => {
                if let Some(inner) = target.and_then(|target| scopes.as_option_inner(target)) {
                    CheckedExpr::new(
                        scopes
                            .make_lang_type("option", vec![inner.clone()])
                            .unwrap(),
                        CheckedExprData::Instance {
                            members: [(
                                "None".into(),
                                self.check_expr(scopes, Located::new(span, ExprData::Void), target),
                            )]
                            .into(),
                            variant: Some("None".into()),
                        },
                    )
                } else {
                    self.error(Error::new("cannot infer type of option null literal", span))
                }
            }
            ExprData::Void => CheckedExpr::new(TypeId::Void, CheckedExprData::Void),
            ExprData::Bool(value) => CheckedExpr {
                ty: TypeId::Bool,
                data: CheckedExprData::Bool(value),
            },
            ExprData::Integer { base, value, width } => {
                let ty = if let Some(width) = width {
                    TypeId::from_int_name(&width).unwrap_or_else(|| {
                        self.error(Error::new(
                            format!("invalid integer literal type: {width}"),
                            span,
                        ))
                    })
                } else {
                    // FIXME: attempt to promote the literal if its too large for i32
                    target
                        .map(|mut target| {
                            while let Some(inner) = scopes.as_option_inner(target) {
                                target = inner;
                            }
                            target
                        })
                        .filter(|target| {
                            matches!(
                                target,
                                TypeId::Int(_)
                                    | TypeId::Uint(_)
                                    | TypeId::Isize
                                    | TypeId::Usize
                                    | TypeId::CInt(_)
                                    | TypeId::CUint(_),
                            )
                        })
                        .cloned()
                        .unwrap_or(TypeId::Int(32))
                };

                let stats = ty.integer_stats().unwrap();
                if stats.signed {
                    let result = match BigInt::from_str_radix(&value, base as u32) {
                        Ok(result) => result,
                        Err(e) => {
                            return self.error(Error::new(
                                format!("integer literal '{value}' could not be parsed: {e}"),
                                expr.span,
                            ));
                        }
                    };

                    let max = stats.max_signed();
                    if result > max {
                        return self.error(Error::new(
                            format!("integer literal is larger than its type allows ({max})"),
                            expr.span,
                        ));
                    }

                    let min = stats.min_signed();
                    if result < min {
                        return self.error(Error::new(
                            format!("integer literal is smaller than its type allows ({min})"),
                            expr.span,
                        ));
                    }

                    CheckedExpr::new(ty, CheckedExprData::Signed(result))
                } else {
                    let result = match BigUint::from_str_radix(&value, base as u32) {
                        Ok(result) => result,
                        Err(e) => {
                            return self.error(Error::new(
                                format!("integer literal '{value}' could not be parsed: {e}"),
                                expr.span,
                            ));
                        }
                    };

                    let max = stats.max_unsigned();
                    if result >= max {
                        return self.error(Error::new(
                            format!("integer literal is larger than its type allows ({max})"),
                            expr.span,
                        ));
                    }

                    CheckedExpr::new(ty, CheckedExprData::Unsigned(result))
                }
            }
            ExprData::Float(value) => CheckedExpr::new(
                target
                    .map(|mut target| {
                        while let TypeId::MutPtr(ty) | TypeId::Ptr(ty) = target {
                            target = ty;
                        }
                        target
                    })
                    .filter(|target| matches!(target, TypeId::F32 | TypeId::F64))
                    .cloned()
                    .unwrap_or(TypeId::F64),
                CheckedExprData::Float(value),
            ),
            ExprData::Path(path) => match self.resolve_path(scopes, &path, span) {
                Some(ResolvedPath::Var(id)) => {
                    let var = scopes.get(id);
                    if !var.is_static && scopes.current_function() != scopes.function_of(var.scope)
                    {
                        self.error(Error::new(
                            "cannot reference local variable of enclosing function",
                            span,
                        ))
                    }

                    CheckedExpr::new(var.ty.clone(), CheckedExprData::Symbol(Symbol::Var(id)))
                }
                Some(ResolvedPath::Func(func)) => {
                    let f = scopes.get(func.id);
                    CheckedExpr::new(
                        TypeId::FnPtr(
                            FnPtr {
                                params: f.params.iter().map(|p| p.ty.clone()).collect(),
                                ret: f.ret.clone(),
                            }
                            .into(),
                        ),
                        CheckedExprData::Symbol(Symbol::Func(func)),
                    )
                }
                Some(ResolvedPath::UserType(ut)) => self.error(Error::new(
                    format!("expected expression, found type '{}'", ut.name(scopes)),
                    span,
                )),
                Some(ResolvedPath::Module(id)) => self.error(Error::new(
                    format!(
                        "expected expression, found module '{}'",
                        scopes[id].kind.name(scopes).unwrap()
                    ),
                    span,
                )),
                Some(ResolvedPath::None(err)) => self.error(err),
                None => Default::default(),
            },
            ExprData::Assign {
                target: lhs,
                binary,
                value,
            } => {
                let lhs_span = lhs.span;
                let lhs = self.check_expr(scopes, *lhs, None);
                if !lhs.is_assignable(scopes) {
                    // TODO: report a better error here
                    return self.error(Error::new("expression is not assignable", lhs_span));
                }

                let rhs = type_check_bail!(self, scopes, *value, &lhs.ty);
                if let Some(op) = binary {
                    if !lhs.ty.supports_binop(scopes, op) {
                        self.error::<()>(Error::new(
                            format!(
                                "operator '{op}' is invalid for values of type {} and {}",
                                &lhs.ty.name(scopes),
                                &rhs.ty.name(scopes)
                            ),
                            span,
                        ));
                    }
                }

                CheckedExpr::new(
                    lhs.ty.clone(),
                    CheckedExprData::Assign {
                        target: lhs.into(),
                        binary,
                        value: rhs.into(),
                    },
                )
            }
            ExprData::Block(body) => {
                let block =
                    self.create_block(scopes, body, ScopeKind::Block(target.cloned(), false));
                let (target, yields) = scopes[block.scope].kind.as_block().unwrap();
                CheckedExpr::new(
                    yields
                        .then(|| target.clone())
                        .flatten()
                        .unwrap_or(TypeId::Void),
                    CheckedExprData::Block(block),
                )
            }
            ExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                let cond = self.type_check(scopes, *cond, &TypeId::Bool);
                let target = if else_branch.is_some() {
                    target
                } else {
                    target
                        .and_then(|t| t.as_user_type())
                        .filter(|t| t.id == scopes.get_option_id().unwrap())
                        .map(|target| &target.ty_args[0])
                };

                let if_span = if_branch.span;
                let mut if_branch = self.check_expr_inner(scopes, *if_branch, target);
                if let Some(target) = target {
                    if_branch = self.type_check_checked(scopes, if_branch, target, if_span);
                }

                let mut out_type = if_branch.ty.clone();
                let else_branch = if let Some(expr) = else_branch {
                    if out_type.is_never() {
                        let expr = self.check_expr_inner(scopes, *expr, None);
                        out_type = expr.ty.clone();
                        Some(expr)
                    } else {
                        let span = expr.span;
                        let source = self.check_expr_inner(scopes, *expr, Some(&out_type));
                        Some(self.type_check_checked(scopes, source, &out_type, span))
                    }
                } else {
                    // this separates these two cases:
                    //   let x /* void? */ = if whatever { yield void; };
                    //   let x /* void */ = if whatever { };
                    if matches!(&if_branch.data, CheckedExprData::Block(b) if
                        matches!(scopes[b.scope].kind, ScopeKind::Block(_, yields) if yields))
                    {
                        if out_type.is_never() {
                            out_type = TypeId::Void;
                            Some(CheckedExpr::new(TypeId::Void, CheckedExprData::Void))
                        } else {
                            out_type = scopes.make_lang_type("option", vec![out_type]).unwrap();
                            if_branch = if_branch.coerce_to(&out_type, scopes);
                            Some(self.check_expr_inner(
                                scopes,
                                Located::new(span, ExprData::None),
                                Some(&out_type),
                            ))
                        }
                    } else {
                        None
                    }
                };

                CheckedExpr::new(
                    out_type,
                    CheckedExprData::If {
                        cond: cond.into(),
                        if_branch: if_branch.into(),
                        else_branch: else_branch.map(|e| e.into()),
                    },
                )
            }
            ExprData::Loop {
                cond,
                body,
                do_while,
            } => {
                // if let Some(Expr::Is { expr, pattern }) = cond.map(|cond| cond.data) {
                //
                // }

                let cond = cond.map(|cond| self.type_check(scopes, *cond, &TypeId::Bool));
                let target = if cond.is_none() {
                    target
                } else {
                    target
                        .and_then(|t| t.as_user_type())
                        .filter(|t| t.id == scopes.get_option_id().unwrap())
                        .map(|target| &target.ty_args[0])
                };

                let body = self.create_block(scopes, body, ScopeKind::Loop(target.cloned(), false));
                let ScopeKind::Loop(target, breaks) = &scopes[body.scope].kind else {
                    panic!("ICE: target of loop changed from loop to something else");
                };

                let out_type = if cond.is_none() {
                    breaks
                        .then(|| target.clone().unwrap())
                        .unwrap_or(TypeId::Never)
                } else {
                    // TODO: coerce the break statements
                    breaks
                        .then(|| {
                            scopes
                                .make_lang_type("option", vec![target.clone().unwrap()])
                                .unwrap()
                        })
                        .unwrap_or(TypeId::Void)
                };

                CheckedExpr::new(
                    out_type,
                    CheckedExprData::Loop {
                        cond: cond.map(|cond| cond.into()),
                        iter: None,
                        body,
                        do_while,
                    },
                )
            }
            ExprData::For {
                var,
                mutable,
                iter,
                body,
            } => {
                let span = iter.span;
                let iter = self.check_expr(scopes, *iter, None);
                let iter_id = scopes.lang_types.get("iter").copied().unwrap();
                if iter
                    .ty
                    .as_user_type()
                    .and_then(|ut| {
                        scopes
                            .get(ut.id)
                            .impls
                            .iter()
                            .find(|i| i.as_user_type().unwrap().id == iter_id)
                    })
                    .is_none()
                {
                    self.error(Error::new(
                        format!("type '{}' does not implement 'Iter'", iter.ty.name(scopes)),
                        span,
                    ))
                }

                let id = scopes.insert(
                    Variable {
                        name: "$iter".into(),
                        ty: iter.ty.clone(),
                        is_static: false,
                        mutable: true,
                        value: Some(iter),
                    },
                    false,
                );
                let mut body = self.check_expr(
                    scopes,
                    l!(ExprData::Loop {
                        cond: None,
                        body: vec![Stmt {
                            data: StmtData::Expr(l!(ExprData::Match {
                                expr: lbox!(ExprData::Call {
                                    callee: lbox!(ExprData::Member {
                                        source: lbox!(ExprData::Path(Path::from(
                                            "$iter".to_string()
                                        ))),
                                        generics: vec![],
                                        member: "next".into(),
                                    }),
                                    args: vec![],
                                }),
                                body: vec![
                                    (Pattern::Option(mutable, l!(var)), l!(ExprData::Block(body))),
                                    (
                                        Pattern::Null(Span::default()),
                                        l!(ExprData::Break(lbox!(ExprData::Void)))
                                    )
                                ],
                            })),
                            span: Span::default(),
                            attrs: Vec::new(),
                        }],
                        do_while: false
                    }),
                    Some(&TypeId::Void),
                );

                let CheckedExprData::Loop { iter, .. } = &mut body.data else {
                    unreachable!()
                };
                *iter = Some(id);
                body
            }
            ExprData::Member {
                source,
                member: name,
                generics,
            } => {
                if !generics.is_empty() {
                    self.error(Error::new(
                        "member variables cannot have type arguments",
                        span,
                    ))
                }

                let source = self.check_expr(scopes, *source, None);
                let id = source.ty.strip_references();
                let ut_id = match &id {
                    TypeId::UserType(data) => data.id,
                    _ => {
                        return self.error(Error::new(
                            format!("cannot get member of type '{}'", id.name(scopes)),
                            span,
                        ));
                    }
                };

                if let Some(ut) = scopes.get(ut_id).members() {
                    for i in 0..ut.len() {
                        resolve_type!(
                            self,
                            scopes,
                            scopes.get_mut(ut_id).members_mut().unwrap()[i].ty
                        );
                    }
                }

                let ty = scopes.get(ut_id);
                if let Some(members) = ty.members() {
                    if let Some(member) = members.iter().find(|m| m.name == name) {
                        if let Some(union) = ty.data.as_union() {
                            if !member.shared && !union.is_unsafe {
                                return self.error(Error::new(
                                    "cannot access union variant with '.' (only shared members)",
                                    span,
                                ));
                            }

                            if !member.shared && union.is_unsafe && self.safety != Safety::Unsafe {
                                self.error(Error::new("this operation is unsafe", span))
                            }
                        }

                        if !member.public && !scopes.can_access_privates(ty.scope) {
                            self.error(private_member!(scopes, id, member, span))
                        }

                        let mut ty = member.ty.clone();
                        if let Some(instance) = id.as_user_type() {
                            ty.fill_struct_templates(scopes, instance);
                        }

                        let id = id.clone();
                        return CheckedExpr::new(
                            ty,
                            CheckedExprData::Member {
                                source: source.auto_deref(&id).into(),
                                member: name,
                            },
                        );
                    }
                }

                self.error(Error::new(
                    format!("type {} has no member '{name}'", &source.ty.name(scopes)),
                    span,
                ))
            }
            ExprData::Subscript { callee, args } => {
                if args.len() > 1 {
                    self.error::<()>(Error::new(
                        "multidimensional subscript is not supported",
                        args[1].span,
                    ));
                }

                let callee = self.check_expr(scopes, *callee, None);
                let arg = type_check_bail!(
                    self,
                    scopes,
                    args.into_iter().next().unwrap(),
                    &TypeId::Isize
                );
                if let TypeId::Array(target) = &callee.ty {
                    CheckedExpr::new(
                        target.0.clone(),
                        CheckedExprData::Subscript {
                            callee: callee.into(),
                            args: vec![arg],
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!("type {} cannot be subscripted", &callee.ty.name(scopes)),
                        span,
                    ))
                }
            }
            ExprData::Return(expr) => self.check_return(scopes, *expr),
            ExprData::Yield(expr) => self.check_yield(scopes, *expr),
            ExprData::YieldOrReturn(expr) => match &scopes.current().kind {
                ScopeKind::Function(_) | ScopeKind::Lambda(_, _) => {
                    self.check_return(scopes, *expr)
                }
                _ => self.check_yield(scopes, *expr),
            },
            ExprData::Break(expr) => {
                let Some((id, target)) = scopes.iter().find_map(|(id, scope)| match &scope.kind {
                    ScopeKind::Loop(target, _) => Some((id, target.clone())),
                    _ => None,
                }) else {
                    return self.error(Error::new("break outside of loop", span));
                };

                let span = expr.span;
                let mut expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    expr = self.type_check_checked(scopes, expr, target, span);
                    scopes[id].kind = ScopeKind::Loop(Some(target.clone()), true);
                } else {
                    scopes[id].kind = ScopeKind::Loop(Some(expr.ty.clone()), true);
                }

                CheckedExpr::new(TypeId::Never, CheckedExprData::Break(expr.into()))
            }
            ExprData::Continue => {
                if !scopes
                    .iter()
                    .any(|(_, scope)| matches!(scope.kind, ScopeKind::Loop(_, _)))
                {
                    return self.error(Error::new("continue outside of loop", span));
                }

                CheckedExpr::new(TypeId::Never, CheckedExprData::Continue)
            }
            ExprData::Is { expr, pattern } => self.check_expr(
                scopes,
                l!(ExprData::Match {
                    expr,
                    body: vec![
                        (pattern, l!(ExprData::Bool(true))),
                        (
                            Pattern::Path(l!(Path::from("_".to_string()))),
                            l!(ExprData::Bool(false))
                        )
                    ],
                }),
                Some(&TypeId::Bool),
            ),
            ExprData::Match { expr, body } => {
                let scrutinee = self.check_expr(scopes, *expr, None);
                let mut target = target.cloned();
                let mut result = Vec::new();
                for (pattern, expr) in body.into_iter() {
                    let span = expr.span;
                    let (pattern, mut expr) = scopes.enter(ScopeKind::None, false, |scopes| {
                        (
                            self.check_pattern(scopes, &scrutinee, pattern),
                            self.check_expr(scopes, expr, target.as_ref()),
                        )
                    });

                    if let Some(target) = &target {
                        expr = self.type_check_checked(scopes, expr, target, span);
                    } else {
                        target = Some(expr.ty.clone());
                    }

                    result.push((
                        pattern,
                        CheckedExpr::new(TypeId::Never, CheckedExprData::Yield(expr.into())),
                    ));
                }

                CheckedExpr::new(
                    target.unwrap_or(TypeId::Void),
                    CheckedExprData::Match {
                        expr: scrutinee.into(),
                        body: result,
                    },
                )
            }
            ExprData::As { expr, ty, throwing } => {
                let mut expr = self.check_expr(scopes, *expr, None);
                let ty = self.resolve_type(scopes, &ty);
                if !expr.ty.coerces_to(scopes, &ty) {
                    match (&expr.ty, &ty) {
                        (a, b) if a == b => {}
                        (TypeId::Int(a), TypeId::Int(b) | TypeId::Uint(b)) if a <= b => {}
                        (TypeId::Uint(a), TypeId::Uint(b)) if a <= b => {}
                        (TypeId::Uint(a), TypeId::Int(b)) if (a + 1) <= *b => {}
                        (TypeId::CInt(a), TypeId::CInt(b) | TypeId::CUint(b)) if a <= b => {}
                        (TypeId::Char, TypeId::Uint(num)) if *num >= 32 => {}
                        (TypeId::Char, TypeId::Int(num)) if *num >= 33 => {}
                        (TypeId::F32, TypeId::F64) => {}
                        (TypeId::Ptr(_) | TypeId::MutPtr(_), TypeId::Usize) => {}
                        (
                            TypeId::Bool,
                            TypeId::Int(_)
                            | TypeId::Uint(_)
                            | TypeId::CInt(_)
                            | TypeId::CUint(_)
                            | TypeId::Usize
                            | TypeId::Isize,
                        ) => {}

                        (TypeId::Usize, TypeId::Ptr(_) | TypeId::MutPtr(_))
                        | (
                            TypeId::MutPtr(_) | TypeId::Ptr(_),
                            TypeId::MutPtr(_) | TypeId::Ptr(_),
                        ) => {
                            if self.safety != Safety::Unsafe {
                                self.error(Error::new("this operation is unsafe", span))
                            }
                        }

                        (TypeId::CUint(a), TypeId::CUint(b)) if a <= b => {}
                        (
                            TypeId::Int(_)
                            | TypeId::Uint(_)
                            | TypeId::CInt(_)
                            | TypeId::CUint(_)
                            | TypeId::Usize
                            | TypeId::Isize
                            | TypeId::Char,
                            TypeId::Int(_)
                            | TypeId::Uint(_)
                            | TypeId::CInt(_)
                            | TypeId::CUint(_)
                            | TypeId::Usize
                            | TypeId::Isize
                            | TypeId::Char,
                        ) if throwing => {}
                        (TypeId::F64, TypeId::F32) if throwing => {}
                        _ => {
                            expr = self.error(Error::new(
                                format!(
                                    "cannot{}cast expression of type '{}' to '{}'",
                                    if !throwing { " infallibly " } else { " " },
                                    expr.ty.name(scopes),
                                    ty.name(scopes)
                                ),
                                span,
                            ));
                        }
                    }

                    CheckedExpr::new(ty, CheckedExprData::As(expr.into(), throwing))
                } else {
                    expr.coerce_to(&ty, scopes)
                }
            }
            ExprData::Error => CheckedExpr::default(),
            ExprData::Lambda { params, ret, body } => {
                let ty_is_generic = |scopes: &Scopes, ty: &TypeId| {
                    !ty.as_user_type()
                        .map_or(false, |ut| scopes.get(ut.id).data.is_template())
                };

                let mut lparams = Vec::new();
                let ret = ret.map(|ret| self.resolve_type(scopes, &ret)).or_else(|| {
                    target
                        .as_ref()
                        .and_then(|ty| ty.as_fn_ptr())
                        .map(|f| &f.ret)
                        .filter(|ty| ty_is_generic(scopes, ty))
                        .cloned()
                });
                // TODO: lambdas should have a unique type
                let (id, body) = scopes.enter(ScopeKind::Lambda(ret, false), false, |scopes| {
                    for (i, param) in params.into_iter().enumerate() {
                        let ty = param
                            .1
                            .map(|ty| self.resolve_type(scopes, &ty))
                            .or_else(|| {
                                target
                                    .as_ref()
                                    .and_then(|ty| ty.as_fn_ptr())
                                    .and_then(|f| f.params.get(i))
                                    .filter(|ty| ty_is_generic(scopes, ty))
                                    .cloned()
                            })
                            .unwrap_or_else(|| {
                                self.error(Error::new(
                                    format!("cannot infer type of parameter '{}'", param.0.data),
                                    param.0.span,
                                ))
                            });

                        lparams.push(ty.clone());
                        scopes.insert::<VariableId>(
                            Variable {
                                name: param.0.data,
                                ty,
                                is_static: false,
                                mutable: false,
                                value: None,
                            },
                            false,
                        );
                    }

                    // yield shouldn't work inside lambdas
                    let body = if let ExprData::Block(body) = body.data {
                        body.into_iter()
                            .map(|stmt| {
                                let stmt = self.declare_stmt(scopes, stmt);
                                self.check_stmt(scopes, stmt)
                            })
                            .collect()
                    } else {
                        vec![CheckedStmt::Expr(self.check_expr(
                            scopes,
                            Expr::new(body.span, ExprData::Return(body)),
                            None,
                        ))]
                    };

                    (scopes.current, body)
                });
                let (target, yields) = scopes[id].kind.as_lambda().unwrap();
                CheckedExpr::new(
                    TypeId::FnPtr(
                        FnPtr {
                            params: lparams,
                            ret: yields
                                .then(|| target.clone())
                                .flatten()
                                .unwrap_or(TypeId::Void),
                        }
                        .into(),
                    ),
                    CheckedExprData::Lambda(body),
                )
            }
            ExprData::Unsafe(expr) => {
                let old_safety = std::mem::replace(&mut self.safety, Safety::Unsafe);
                let expr = self.check_expr(scopes, *expr, target);
                self.safety = old_safety;
                expr
            }
        }
    }

    fn check_expr(
        &mut self,
        scopes: &mut Scopes,
        expr: Expr,
        target: Option<&TypeId>,
    ) -> CheckedExpr {
        let expr = self.check_expr_inner(scopes, expr, target);
        if expr.ty.is_never() && !matches!(expr.data, CheckedExprData::Yield(_)) {
            // TODO: lambdas
            match &mut scopes.current().kind {
                ScopeKind::Block(target, yields @ false) => {
                    *target = Some(TypeId::Never);
                    *yields = true;
                }
                &mut ScopeKind::Function(id) => {
                    scopes.get_mut(id).returns = true;
                }
                _ => {}
            }
        }
        expr
    }

    fn check_yield(&mut self, scopes: &mut Scopes, expr: Expr) -> CheckedExpr {
        let ScopeKind::Block(target, _) = &scopes.current().kind else {
            return self.error(Error::new("yield outside of block", expr.span));
        };

        let target = target.clone();
        let span = expr.span;
        let mut expr = self.check_expr(scopes, expr, target.as_ref());
        if let Some(target) = &target {
            expr = self.type_check_checked(scopes, expr, target, span);
            scopes.current().kind = ScopeKind::Block(Some(target.clone()), true);
        } else {
            scopes.current().kind = ScopeKind::Block(Some(expr.ty.clone()), true);
        }

        CheckedExpr::new(TypeId::Never, CheckedExprData::Yield(expr.into()))
    }

    fn check_return(&mut self, scopes: &mut Scopes, expr: Expr) -> CheckedExpr {
        let lambda = scopes.iter().find_map(|(id, scope)| match &scope.kind {
            ScopeKind::Lambda(target, _) => Some((id, target.clone())),
            _ => None,
        });
        if let Some((id, target)) = lambda {
            let span = expr.span;
            let mut expr = self.check_expr(scopes, expr, target.as_ref());
            if let Some(target) = &target {
                expr = self.type_check_checked(scopes, expr, target, span);
                scopes[id].kind = ScopeKind::Lambda(Some(target.clone()), true);
            } else {
                scopes[id].kind = ScopeKind::Lambda(Some(expr.ty.clone()), true);
            }

            CheckedExpr::new(TypeId::Never, CheckedExprData::Return(expr.into()))
        } else {
            let target = scopes
                .current_function()
                .map(|id| scopes.get(id).ret.clone())
                .expect("return should only be possible inside functions");
            CheckedExpr::new(
                TypeId::Never,
                CheckedExprData::Return(self.type_check(scopes, expr, &target).into()),
            )
        }
    }

    fn check_unary(
        &mut self,
        scopes: &mut Scopes,
        expr: Expr,
        target: Option<&TypeId>,
        op: UnaryOp,
    ) -> CheckedExpr {
        use UnaryOp::*;

        let span = expr.span;
        macro_rules! invalid {
            ($ty: expr) => {
                self.error(Error::new(
                    format!(
                        "operator '{op}' is invalid for value of type {}",
                        $ty.name(scopes)
                    ),
                    span,
                ))
            };
        }

        let (out_ty, expr) = match op {
            Plus => {
                let expr = self.check_expr(scopes, expr, target);
                if !expr.ty.is_numeric() {
                    invalid!(expr.ty)
                }
                (expr.ty.clone(), expr)
            }
            Neg => {
                let expr = self.check_expr(scopes, expr, target);
                if !matches!(
                    expr.ty,
                    TypeId::Int(_) | TypeId::Isize | TypeId::F32 | TypeId::F64 | TypeId::CInt(_)
                ) {
                    invalid!(expr.ty)
                }
                (expr.ty.clone(), expr)
            }
            PostIncrement | PostDecrement | PreIncrement | PreDecrement => {
                let span = expr.span;
                let expr = self.check_expr(scopes, expr, target);
                if expr.ty.integer_stats().is_some() {
                    if !expr.is_assignable(scopes) {
                        self.error::<()>(Error::new("expression is not assignable", span));
                    }
                } else {
                    invalid!(expr.ty)
                }

                (expr.ty.clone(), expr)
            }
            Not => {
                let expr = self.check_expr(scopes, expr, target);
                if !(expr.ty.is_bool() || expr.ty.integer_stats().is_some()) {
                    invalid!(expr.ty)
                }
                (expr.ty.clone(), expr)
            }
            Deref => {
                let expr = if let Some(target) = target {
                    self.check_expr(scopes, expr, Some(&TypeId::Ptr(target.clone().into())))
                } else {
                    self.check_expr(scopes, expr, target)
                };

                if let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = &expr.ty {
                    ((**inner).clone(), expr)
                } else {
                    (invalid!(expr.ty), expr)
                }
            }
            Addr => {
                let expr = self.check_expr(
                    scopes,
                    expr,
                    target.and_then(|t| t.as_mut_ptr().or(t.as_ptr()).map(|t| &**t)),
                );
                (TypeId::Ptr(expr.ty.clone().into()), expr)
            }
            AddrMut => {
                let expr = self.check_expr(
                    scopes,
                    expr,
                    target.and_then(|t| t.as_mut_ptr().or(t.as_ptr()).map(|t| &**t)),
                );
                if !expr.can_addrmut(scopes) {
                    self.error::<()>(Error::new(
                        "cannot create mutable pointer to immutable memory location",
                        span,
                    ));
                }

                (TypeId::MutPtr(expr.ty.clone().into()), expr)
            }
            Unwrap => {
                let expr =
                    self.check_expr(scopes, expr, target.and_then(|t| scopes.as_option_inner(t)));

                if let Some(inner) = scopes.as_option_inner(&expr.ty) {
                    let func = scopes.find_in(
                        "unwrap",
                        scopes.get(scopes.get_option_id().unwrap()).body_scope,
                    );

                    return CheckedExpr::new(
                        inner.clone(),
                        CheckedExprData::Call {
                            inst: Some(expr.ty.clone()),
                            args: IndexMap::from([(
                                THIS_PARAM.into(),
                                CheckedExpr::new(
                                    TypeId::Ptr(expr.ty.clone().into()),
                                    CheckedExprData::Unary {
                                        op: UnaryOp::Addr,
                                        expr: expr.into(),
                                    },
                                ),
                            )]),
                            func: GenericFunc::new(*func.unwrap(), vec![]),
                            trait_id: None,
                        },
                    );
                }
                (invalid!(expr.ty), expr)
            }
            Try => todo!(),
        };

        CheckedExpr::new(
            out_ty,
            CheckedExprData::Unary {
                op,
                expr: expr.into(),
            },
        )
    }

    fn check_pattern(
        &mut self,
        scopes: &mut Scopes,
        scrutinee: &CheckedExpr,
        pattern: Pattern,
    ) -> CheckedPattern {
        let (span, path, binding) = match pattern {
            Pattern::PathWithBindings { path, binding } => (
                path.span,
                self.resolve_path(scopes, &path.data, path.span),
                Some(binding),
            ),
            Pattern::Path(path) => match self.resolve_path(scopes, &path.data, path.span) {
                original @ Some(ResolvedPath::None(_)) => {
                    if let Some(ident) = path.data.as_identifier() {
                        return CheckedPattern::Irrefutable(IrrefutablePattern::Variable(
                            scopes.insert(
                                Variable {
                                    name: ident.into(),
                                    ty: scrutinee.ty.clone(),
                                    is_static: false,
                                    mutable: false,
                                    value: None,
                                },
                                false,
                            ),
                        ));
                    }

                    (path.span, original, None)
                }
                p => (path.span, p, None),
            },
            Pattern::Option(mutable, binding) => (
                binding.span,
                self.resolve_path_in(
                    scopes,
                    &[("Some".into(), vec![])],
                    scopes.get(scopes.get_option_id().unwrap()).body_scope,
                    binding.span,
                ),
                Some((mutable, binding.data)),
            ),
            Pattern::Null(span) => (
                span,
                self.resolve_path_in(
                    scopes,
                    &[("None".into(), vec![])],
                    scopes.get(scopes.get_option_id().unwrap()).body_scope,
                    span,
                ),
                None,
            ),
            Pattern::MutBinding(name) => {
                return CheckedPattern::Irrefutable(IrrefutablePattern::Variable(scopes.insert(
                    Variable {
                        name: name.data,
                        ty: scrutinee.ty.clone(),
                        is_static: false,
                        mutable: true,
                        value: None,
                    },
                    false,
                )));
            }
            Pattern::StructDestructure(_) => todo!(),
        };

        let Some(path) = path else {
            return Default::default();
        };

        let Some(ut) = scrutinee
            .ty
            .strip_references()
            .as_user_type()
            .filter(|ut| scopes.get(ut.id).data.is_union())
        else {
            return self.error(Error::new(
                "match scrutinee must be union or pointer to union",
                span,
            ));
        };

        for i in 0..scopes.get_mut(ut.id).members_mut().unwrap().len() {
            resolve_type!(
                self,
                scopes,
                scopes.get_mut(ut.id).members_mut().unwrap()[i].ty
            );
        }

        let mut variant = String::new();
        let Some(union) = path
            .as_func()
            .map(|f| scopes.get(f.id))
            .filter(|f| f.constructor)
            .and_then(|f| {
                variant = f.name.data.clone();
                f.ret.as_user_type()
            })
            .filter(|ty| ty.id == ut.id)
            .and_then(|ty| scopes.get(ty.id).data.as_union())
        else {
            return self.error(Error::new("pattern does not match the scrutinee", span));
        };

        let member = union.variants.iter().find(|m| m.name == variant).unwrap();

        let mut ty = member.ty.clone();
        ty.fill_struct_templates(scopes, ut);
        let ptr = scrutinee.ty.is_ptr() || scrutinee.ty.is_mut_ptr();
        let tag = union.variant_tag(&variant).unwrap();
        if let Some((mutable, binding)) = binding {
            CheckedPattern::UnionMember {
                binding: Some(scopes.insert(
                    Variable {
                        name: binding,
                        ty: scrutinee.ty.matched_inner_type(ty),
                        is_static: false,
                        mutable,
                        value: None,
                    },
                    false,
                )),
                variant: (variant, tag),
                ptr,
            }
        } else if ty.is_void() {
            CheckedPattern::UnionMember {
                binding: None,
                variant: (variant, tag),
                ptr,
            }
        } else {
            self.error(Error::new(
                format!("union variant '{variant}' has data that must be bound"),
                span,
            ))
        }
    }

    fn check_call(
        &mut self,
        scopes: &mut Scopes,
        target: Option<&TypeId>,
        callee: Expr,
        args: Vec<(Option<String>, Expr)>,
        span: Span,
    ) -> CheckedExpr {
        match callee.data {
            ExprData::Member {
                source,
                member,
                generics,
            } => {
                let this = self.check_expr(scopes, *source, None);
                let id = this.ty.strip_references().clone();
                let Some((tr, func)) = id.get_member_fn(scopes, &member) else {
                    return self.error(Error::new(
                        format!("no method '{member}' found on type '{}'", id.name(scopes)),
                        span,
                    ));
                };
                let f = scopes.get(*func);
                let Some(this_param) = f.params.get(0).filter(|p| p.name == THIS_PARAM) else {
                    return self.error(Error::new(
                        format!("associated function '{member}' cannot be used as a method"),
                        span,
                    ));
                };

                if let Some(ty) = id.as_user_type().map(|ut| scopes.get(ut.id)) {
                    // TODO: extension method privacy
                    if !func.public && !scopes.can_access_privates(ty.scope) {
                        return self.error(Error::new(
                            format!(
                                "cannot access private method '{member}' of type '{}'",
                                id.name(scopes)
                            ),
                            span,
                        ));
                    }
                }

                if this_param.ty.is_mut_ptr() {
                    let mut ty = &this.ty;
                    if !ty.is_ptr() && !ty.is_mut_ptr() && !this.can_addrmut(scopes) {
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

                let this = if !matches!(this.ty, TypeId::Ptr(_) | TypeId::MutPtr(_)) {
                    if matches!(this_param.ty, TypeId::Ptr(_)) {
                        CheckedExpr::new(
                            TypeId::Ptr(this.ty.clone().into()),
                            CheckedExprData::Unary {
                                op: UnaryOp::Addr,
                                expr: this.into(),
                            },
                        )
                    } else {
                        CheckedExpr::new(
                            TypeId::MutPtr(this.ty.clone().into()),
                            CheckedExprData::Unary {
                                op: UnaryOp::AddrMut,
                                expr: this.into(),
                            },
                        )
                    }
                } else {
                    this.auto_deref(&this_param.ty)
                };

                let mut func = GenericFunc::new(
                    *func,
                    self.resolve_type_args(scopes, f.type_params.len(), &generics, span),
                );
                let (args, ret) = self.check_fn_args(
                    tr.as_ref().or(id.as_user_type().map(|ut| &**ut)),
                    &mut func,
                    Some(this),
                    args,
                    target,
                    scopes,
                    span,
                );

                return CheckedExpr::new(
                    ret,
                    CheckedExprData::Call {
                        func,
                        inst: Some(id),
                        args,
                        trait_id: tr.map(|ut| ut.id),
                    },
                );
            }
            ExprData::Path(ref path) => {
                match self.resolve_path(scopes, path, callee.span) {
                    Some(ResolvedPath::UserType(ty)) => {
                        let ut = scopes.get(ty.id);
                        let Some(st) = ut.data.as_struct() else {
                            return self.error(Error::new(
                                format!("cannot construct type '{}'", ut.name),
                                span,
                            ));
                        };

                        // TODO: check privacy
                        let (args, ret) = self.check_fn_args(
                            None,
                            &mut GenericFunc::new(*st.1, ty.ty_args),
                            None,
                            args,
                            target,
                            scopes,
                            span,
                        );

                        return CheckedExpr::new(
                            ret,
                            CheckedExprData::Instance {
                                members: args,
                                variant: None,
                            },
                        );
                    }
                    Some(ResolvedPath::Func(mut func)) => {
                        let f = scopes.get(func.id);
                        let constructor = f.constructor;
                        let variant = constructor.then(|| f.name.data.clone());
                        let (args, ret) =
                            self.check_fn_args(None, &mut func, None, args, target, scopes, span);

                        return CheckedExpr::new(
                            ret,
                            if constructor {
                                CheckedExprData::Instance {
                                    members: args,
                                    variant,
                                }
                            } else {
                                CheckedExprData::Call {
                                    func,
                                    args,
                                    inst: None,
                                    trait_id: None,
                                }
                            },
                        );
                    }
                    Some(ResolvedPath::Var(_)) => {}
                    Some(ResolvedPath::Module(scope)) => {
                        return self.error(Error::new(
                            format!(
                                "cannot call module '{}'",
                                scopes[scope].kind.name(scopes).unwrap()
                            ),
                            span,
                        ))
                    }
                    Some(ResolvedPath::None(err)) => return self.error(err),
                    None => return Default::default(),
                }
            }
            _ => {}
        }

        let callee = self.check_expr(scopes, callee, None);
        if callee.ty.is_fn_ptr() {
            return self.call_fn_ptr(scopes, callee, args, span);
        }

        self.error(Error::new(
            format!("expected callable item, got '{}'", &callee.ty.name(scopes)),
            span,
        ))
    }

    fn call_fn_ptr(
        &mut self,
        scopes: &mut Scopes,
        callee: CheckedExpr,
        args: Vec<(Option<String>, Expr)>,
        span: Span,
    ) -> CheckedExpr {
        let f = callee.ty.as_fn_ptr().unwrap();

        let mut result = vec![];
        for (i, (name, arg)) in args.into_iter().enumerate() {
            if let Some(param) = f.params.get(i) {
                if name.is_some() {
                    self.error(Error::new(
                        "keyword parameters are not allowed here",
                        arg.span,
                    ))
                }

                result.push(self.type_check(scopes, arg, param));
            } else {
                self.error::<()>(Error::new("too many positional arguments", span));
                break;
            }
        }

        if result.len() < f.params.len() {
            self.error(Error::new("too few positional arguments", span))
        }

        CheckedExpr::new(
            f.ret.clone(),
            CheckedExprData::CallFnPtr {
                expr: callee.into(),
                args: result,
            },
        )
    }

    fn check_arg(
        &mut self,
        func: &mut GenericFunc,
        scopes: &mut Scopes,
        expr: Expr,
        param: &CheckedParam,
        inst: Option<&GenericUserType>,
    ) -> CheckedExpr {
        let mut target = param.ty.clone();
        target.fill_func_template(scopes, func);

        if let Some(inst) = inst {
            target.fill_struct_templates(scopes, inst);
        }

        let span = expr.span;
        let expr = self.check_expr(scopes, expr, Some(&target));
        if !func.ty_args.is_empty() {
            func.infer_type_args(&param.ty, &expr.ty, scopes);
            target.fill_func_template(scopes, func);
        }

        self.type_check_checked(scopes, expr, &target, span)
    }

    #[allow(clippy::too_many_arguments)]
    fn check_fn_args(
        &mut self,
        inst: Option<&GenericUserType>,
        func: &mut GenericFunc,
        this: Option<CheckedExpr>,
        args: Vec<(Option<String>, Expr)>,
        target: Option<&TypeId>,
        scopes: &mut Scopes,
        span: Span,
    ) -> (IndexMap<String, CheckedExpr>, TypeId) {
        self.resolve_proto(scopes, func.id);

        if let Some(target) = target {
            func.infer_type_args(&scopes.get(func.id).ret, target, scopes);
        }

        let mut result = IndexMap::with_capacity(args.len());
        let mut last_pos = 0;
        if let Some(this) = this {
            result.insert(THIS_PARAM.into(), this);
            last_pos += 1;
        }

        let variadic = scopes.get(func.id).variadic;
        let mut num = 0;
        for (name, expr) in args {
            if let Some(name) = name {
                match result.entry(name.clone()) {
                    Entry::Occupied(_) => {
                        self.error::<()>(Error::new(
                            format!("parameter '{name}' has already been specified"),
                            expr.span,
                        ));
                    }
                    Entry::Vacant(entry) => {
                        if let Some(param) =
                            scopes.get(func.id).params.iter().find(|p| p.name == name)
                        {
                            entry.insert(self.check_arg(func, scopes, expr, &param.clone(), inst));
                        } else {
                            self.error::<()>(Error::new(
                                format!("unknown parameter: '{name}'"),
                                expr.span,
                            ));
                        }
                    }
                }
            } else if let Some((i, param)) = scopes
                .get(func.id)
                .params
                .iter()
                .enumerate()
                .skip(last_pos)
                .find(|(_, param)| !param.keyword)
            {
                result.insert(
                    param.name.clone(),
                    self.check_arg(func, scopes, expr, &param.clone(), inst),
                );
                last_pos = i + 1;
            } else if !variadic {
                // TODO: a better error here would be nice
                self.error::<()>(Error::new("too many positional arguments", expr.span));
            } else {
                num += 1;
                result.insert(format!("${num}"), self.check_expr(scopes, expr, None));
            }
        }

        for param in scopes
            .get(func.id)
            .params
            .iter()
            .filter(|p| !result.contains_key(&p.name))
            .collect::<Vec<_>>()
        {
            if let Some(DefaultExpr::Checked(expr)) = &param.default {
                result.insert(param.name.clone(), expr.clone());
            }
        }

        if scopes.get(func.id).params.len() > result.len() {
            let mut missing = String::new();
            for param in scopes
                .get(func.id)
                .params
                .iter()
                .filter(|p| !result.contains_key(&p.name))
            {
                if !missing.is_empty() {
                    missing.push_str(", ");
                }

                missing.push_str(&param.name);
            }

            self.error::<()>(Error::new(
                format!(
                    "expected {} argument(s), found {} (missing {missing})",
                    scopes.get(func.id).params.len(),
                    result.len()
                ),
                span,
            ));
        }

        let mut ret = scopes.get(func.id).ret.clone();
        if !func.ty_args.is_empty() {
            if let Some(target) = target {
                func.infer_type_args(&ret, target, scopes);
            }

            ret.fill_func_template(scopes, func);
            for (i, ty) in func.ty_args.iter().enumerate() {
                if ty.is_unknown() {
                    self.error::<()>(Error::new(
                        format!(
                            "cannot infer type for type parameter '{}'",
                            scopes.get(scopes.get(func.id).type_params[i]).name
                        ),
                        span,
                    ));

                    continue;
                }

                let f = scopes.get(func.id);
                self.check_bounds(
                    scopes,
                    Some(func),
                    ty,
                    &scopes.get(f.type_params[i]).impls,
                    inst,
                    span,
                );
            }
        }

        if let Some(inst) = inst {
            ret.fill_struct_templates(scopes, inst);
        }

        if scopes.get(func.id).is_unsafe && self.safety != Safety::Unsafe {
            self.error(Error::new("this operation is unsafe", span))
        }

        (result, ret)
    }

    fn check_bounds(
        &mut self,
        scopes: &Scopes,
        func: Option<&GenericFunc>,
        ty: &TypeId,
        bounds: &[TypeId],
        inst: Option<&GenericUserType>,
        span: Span,
    ) {
        for bound in bounds.iter() {
            let mut bound = bound.as_user_type().unwrap().clone();
            if let Some(func) = func {
                for bty in bound.ty_args.iter_mut() {
                    bty.fill_func_template(scopes, func);
                }
            }

            if let Some(inst) = inst {
                for bty in bound.ty_args.iter_mut() {
                    bty.fill_struct_templates(scopes, inst);
                }
            }

            if ty.implements_trait(scopes, &bound) {
                continue;
            }

            self.error(Error::new(
                format!(
                    "type '{}' does not implement '{}'",
                    ty.name(scopes),
                    bound.name(scopes),
                ),
                span,
            ))
        }
    }

    fn create_block(&mut self, scopes: &mut Scopes, body: Vec<Stmt>, kind: ScopeKind) -> Block {
        scopes.enter(kind, false, |scopes| Block {
            body: body
                .into_iter()
                .map(|stmt| {
                    let stmt = self.declare_stmt(scopes, stmt);
                    self.check_stmt(scopes, stmt)
                })
                .collect(),
            scope: scopes.current,
        })
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.errors.push(error);
        T::default()
    }

    fn type_check(&mut self, scopes: &mut Scopes, expr: Expr, target: &TypeId) -> CheckedExpr {
        let span = expr.span;
        let source = self.check_expr(scopes, expr, Some(target));
        self.type_check_checked(scopes, source, target, span)
    }

    fn type_check_checked(
        &mut self,
        scopes: &mut Scopes,
        source: CheckedExpr,
        target: &TypeId,
        span: Span,
    ) -> CheckedExpr {
        if !source.ty.coerces_to(scopes, target) {
            self.error(type_mismatch!(scopes, target, source.ty, span))
        }

        source.coerce_to(target, scopes)
    }

    fn resolve_lang_type(
        &mut self,
        scopes: &Scopes,
        name: &str,
        ty_args: &[TypeHint],
        fwd: Option<&TypeHint>,
    ) -> TypeId {
        if let Some(ty) = scopes.lang_types.get(name).copied() {
            TypeId::UserType(
                GenericUserType::new(
                    ty,
                    self.resolve_type_args(scopes, ty_args.len(), ty_args, Span::default()),
                )
                .into(),
            )
        } else if let Some(hint) = fwd {
            TypeId::Unknown(Some((hint.clone(), scopes.current).into()))
        } else {
            self.error(Error::new(
                format!("missing language item: '{name}'"),
                Span::default(),
            ))
        }
    }

    fn resolve_type(&mut self, scopes: &Scopes, hint: &TypeHint) -> TypeId {
        match hint {
            TypeHint::Regular(path) => {
                return match self.resolve_path(scopes, &path.data, path.span) {
                    Some(ResolvedPath::UserType(ut)) => TypeId::UserType(ut.into()),
                    Some(ResolvedPath::Func(_)) => {
                        if self.decl {
                            TypeId::Unknown(Some((hint.clone(), scopes.current).into()))
                        } else {
                            self.error(Error::new("expected type, got function", path.span))
                        }
                    }
                    Some(ResolvedPath::Var(_)) => {
                        if self.decl {
                            TypeId::Unknown(Some((hint.clone(), scopes.current).into()))
                        } else {
                            self.error(Error::new("expected type, got variable", path.span))
                        }
                    }
                    Some(ResolvedPath::Module(_)) => {
                        if self.decl {
                            TypeId::Unknown(Some((hint.clone(), scopes.current).into()))
                        } else {
                            self.error(Error::new("expected type, got module", path.span))
                        }
                    }
                    Some(ResolvedPath::None(err)) => {
                        let res = path.data.as_identifier().and_then(|symbol| match symbol {
                            symbol if symbol == THIS_TYPE => scopes.current_this_type(),
                            "void" => Some(TypeId::Void),
                            "never" => Some(TypeId::Never),
                            "f32" => Some(TypeId::F32),
                            "f64" => Some(TypeId::F64),
                            "bool" => Some(TypeId::Bool),
                            "char" => Some(TypeId::Char),
                            "c_void" => Some(TypeId::CVoid),
                            "c_char" => Some(TypeId::CInt(CInt::Char)),
                            "c_short" => Some(TypeId::CInt(CInt::Short)),
                            "c_int" => Some(TypeId::CInt(CInt::Int)),
                            "c_long" => Some(TypeId::CInt(CInt::Long)),
                            "c_longlong" => Some(TypeId::CInt(CInt::LongLong)),
                            "c_uchar" => Some(TypeId::CUint(CInt::Char)),
                            "c_ushort" => Some(TypeId::CUint(CInt::Short)),
                            "c_uint" => Some(TypeId::CUint(CInt::Int)),
                            "c_ulong" => Some(TypeId::CUint(CInt::Long)),
                            "c_ulonglong" => Some(TypeId::CUint(CInt::LongLong)),
                            _ => TypeId::from_int_name(symbol),
                        });

                        match res {
                            Some(res) => res,
                            None if self.decl => {
                                TypeId::Unknown(Some((hint.clone(), scopes.current).into()))
                            }
                            None => self.error(err),
                        }
                    }
                    None => TypeId::Unknown(None),
                };
            }
            TypeHint::Void => TypeId::Void,
            TypeHint::Ptr(ty) => TypeId::Ptr(self.resolve_type(scopes, ty).into()),
            TypeHint::MutPtr(ty) => TypeId::MutPtr(self.resolve_type(scopes, ty).into()),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                scopes
                    .current_this_type()
                    .map(|s| TypeId::Ptr(s.into()))
                    .expect("ICE: this outside of method")
            }
            TypeHint::MutThis => scopes
                .current_this_type()
                .map(|s| TypeId::MutPtr(s.into()))
                .expect("ICE: this outside of method"),
            TypeHint::Array(ty, count) => {
                let n = match Self::consteval(scopes, count, Some(&TypeId::Usize)) {
                    Ok(n) => n,
                    Err(err) => return self.error(err),
                };
                TypeId::Array((self.resolve_type(scopes, ty), n).into())
            }
            TypeHint::Option(ty) => self.resolve_lang_type(
                scopes,
                "option",
                &[(**ty).clone()],
                self.decl.then_some(hint),
            ),
            TypeHint::Vec(ty) => {
                self.resolve_lang_type(scopes, "vec", &[(**ty).clone()], self.decl.then_some(hint))
            }
            TypeHint::Map(key, value) => self.resolve_lang_type(
                scopes,
                "map",
                &[(**key).clone(), (**value).clone()],
                self.decl.then_some(hint),
            ),
            TypeHint::Set(ty) => {
                self.resolve_lang_type(scopes, "set", &[(**ty).clone()], self.decl.then_some(hint))
            }
            TypeHint::Slice(ty) => {
                self.resolve_lang_type(scopes, "span", &[(**ty).clone()], self.decl.then_some(hint))
            }
            TypeHint::SliceMut(ty) => self.resolve_lang_type(
                scopes,
                "span_mut",
                &[(**ty).clone()],
                self.decl.then_some(hint),
            ),
            TypeHint::Tuple(_) => todo!(),
            TypeHint::Fn {
                is_extern: _,
                params,
                ret,
            } => TypeId::FnPtr(
                FnPtr {
                    params: params
                        .iter()
                        .map(|p| self.resolve_type(scopes, p))
                        .collect(),
                    ret: self.resolve_type(scopes, ret),
                }
                .into(),
            ),
            TypeHint::Error => TypeId::Unknown(None),
        }
    }

    fn resolve_use(
        &mut self,
        scopes: &mut Scopes,
        public: bool,
        all: bool,
        path: ResolvedPath,
        span: Span,
    ) {
        match path {
            ResolvedPath::UserType(ut) => {
                if !all {
                    scopes.current().types.insert(Vis { id: ut.id, public });
                } else if !self.decl {
                    self.error(Error::new(
                        format!(
                            "cannot import all items of type '{}'",
                            scopes.get(ut.id).name
                        ),
                        span,
                    ))
                }
            }
            ResolvedPath::Func(func) => {
                if !all {
                    scopes.current().fns.insert(Vis {
                        id: func.id,
                        public,
                    });
                } else if !self.decl {
                    self.error(Error::new(
                        format!(
                            "cannot import all items of function '{}'",
                            scopes.get(func.id).name.data
                        ),
                        span,
                    ))
                }
            }
            ResolvedPath::Var(id) => {
                if !scopes.get(id).is_static && !self.decl {
                    return self.error(Error::new(
                        format!("cannot import local variable '{}'", scopes.get(id).name),
                        span,
                    ));
                }

                if !all {
                    scopes.current().vars.insert(Vis { id, public });
                } else if !self.decl {
                    self.error(Error::new(
                        format!(
                            "cannot import all items of variable '{}'",
                            scopes.get(id).name
                        ),
                        span,
                    ))
                }
            }
            ResolvedPath::Module(id) => {
                if !all {
                    scopes.current().children.insert(Vis { id, public });
                } else {
                    for id in scopes[id].children.clone() {
                        scopes.current().children.insert(Vis { id: id.id, public });
                    }

                    for func in scopes[id].fns.clone() {
                        scopes.current().fns.insert(Vis {
                            id: func.id,
                            public,
                        });
                    }

                    for ty in scopes[id].types.clone() {
                        scopes.current().types.insert(Vis { id: ty.id, public });
                    }

                    for var in scopes[id].vars.clone() {
                        scopes.current().vars.insert(Vis { id: var.id, public });
                    }
                }
            }
            ResolvedPath::None(err) => {
                if !self.decl {
                    self.error(err)
                }
            }
        }
    }

    fn resolve_impl(&mut self, scopes: &Scopes, path: &Located<Path>) -> Option<TypeId> {
        match self.resolve_path(scopes, &path.data, path.span)? {
            ResolvedPath::UserType(ty) if scopes.get(ty.id).data.is_trait() => {
                Some(TypeId::UserType(ty.into()))
            }
            ResolvedPath::None(err) if !self.decl => self.error(err),
            ResolvedPath::None(_) => Some(TypeId::Unknown(Some(
                (TypeHint::Regular(path.clone()), scopes.current).into(),
            ))),
            _ => self.error(Error::new("expected trait", path.span)),
        }
    }

    fn resolve_impls(&mut self, scopes: &mut Scopes, id: UserTypeId) {
        for i in 0..scopes.get(id).type_params.len() {
            self.resolve_impls(scopes, scopes.get(id).type_params[i]);
        }

        let mut removals = Vec::new();
        for i in 0..scopes.get(id).impls.len() {
            resolve_type!(self, scopes, scopes.get_mut(id).impls[i]);
            let imp = &scopes.get(id).impls[i];
            if !imp
                .as_user_type()
                .map_or(false, |t| scopes.get(t.id).data.is_trait())
            {
                if !imp.is_unknown() {
                    self.error(Error::new("expected trait", Span::default()))
                }
                removals.push(i);
            }
        }

        for i in removals.into_iter().rev() {
            scopes.get_mut(id).impls.remove(i);
        }
    }

    fn resolve_proto(&mut self, scopes: &mut Scopes, id: FunctionId) {
        for i in 0..scopes.get(id).params.len() {
            resolve_type!(self, scopes, scopes.get_mut(id).params[i].ty);
            match std::mem::take(&mut scopes.get_mut(id).params[i].default) {
                Some(DefaultExpr::Unchecked(scope, expr)) => {
                    scopes.enter_id(scope, |scopes| {
                        let target = scopes.get(id).params[i].ty.clone();
                        scopes.get_mut(id).params[i].default =
                            Some(DefaultExpr::Checked(self.type_check(scopes, expr, &target)));
                    });
                }
                other => scopes.get_mut(id).params[i].default = other,
            }
        }

        resolve_type!(self, scopes, scopes.get_mut(id).ret);

        for i in 0..scopes.get(id).type_params.len() {
            self.resolve_impls(scopes, scopes.get(id).type_params[i]);
        }
    }

    fn resolve_path(&mut self, scopes: &Scopes, path: &Path, span: Span) -> Option<ResolvedPath> {
        match path {
            Path::Root(data) => self.resolve_path_in(scopes, data, ScopeId(0), span),
            Path::Super(data) => {
                if let Some(module) = scopes.module_of(
                    scopes[scopes.module_of(scopes.current).unwrap()]
                        .parent
                        .unwrap(),
                ) {
                    self.resolve_path_in(scopes, data, module, span)
                } else {
                    self.error(Error::new("cannot use super here", span))
                }
            }
            Path::Normal(data) => {
                let (name, ty_args) = data.first().unwrap();
                let is_end = data.len() == 1;
                if let Some(id) = scopes.find(name) {
                    if is_end {
                        return Some(ResolvedPath::Var(*id));
                    }

                    if !ty_args.is_empty() {
                        return self
                            .error(Error::new("variables cannot have type arguments", span));
                    }

                    self.error(Error::new(format!("'{name}' is a variable"), span))
                } else if let Some(id) = scopes.find(name) {
                    if is_end {
                        let ut = GenericUserType::new(
                            *id,
                            self.resolve_type_args(
                                scopes,
                                scopes.get(*id).type_params.len(),
                                ty_args,
                                span,
                            ),
                        );
                        //self.resolve_impls(scopes, id);
                        //self.check_bounds(scopes, None, &ut, &scopes.get(id).impls, span);
                        return Some(ResolvedPath::UserType(ut));
                    }

                    self.resolve_path_in(scopes, &data[1..], scopes.get(*id).body_scope, span)
                } else if let Some(id) = scopes.find_free_fn(name) {
                    if is_end {
                        let f = scopes.get(*id);
                        return Some(ResolvedPath::Func(GenericFunc::new(
                            *id,
                            self.resolve_type_args(scopes, f.type_params.len(), ty_args, span),
                        )));
                    }

                    self.error(Error::new(format!("'{name}' is a function"), span))
                } else if let Some(id) = scopes.find_module(name) {
                    if is_end {
                        return Some(ResolvedPath::Module(*id));
                    }

                    if !ty_args.is_empty() {
                        return self.error(Error::new(
                            "modules cannot be parameterized with type arguments",
                            span,
                        ));
                    }

                    self.resolve_path_in(scopes, &data[1..], *id, span)
                } else {
                    self.resolve_path_in(scopes, data, ScopeId(0), span)
                }
            }
        }
    }

    fn resolve_path_in(
        &mut self,
        scopes: &Scopes,
        data: &[(String, Vec<TypeHint>)],
        mut scope: ScopeId,
        span: Span,
    ) -> Option<ResolvedPath> {
        for (i, (name, ty_args)) in data.iter().enumerate() {
            let is_end = i + 1 == data.len();
            if let Some(id) = scopes.find_in(name, scope) {
                if !id.public && !scopes.can_access_privates(scope) {
                    self.error(Error::new(format!("variable '{name}' is private"), span))
                }

                if !ty_args.is_empty() {
                    return self.error(Error::new(
                        "variables cannot be parameterized with type arguments",
                        span,
                    ));
                }

                if is_end {
                    return Some(ResolvedPath::Var(*id));
                }

                return self.error(Error::new(format!("'{name}' is a variable"), span));
            } else if let Some(id) = scopes.find_in(name, scope) {
                if !id.public && !scopes.can_access_privates(scope) {
                    self.error(Error::new(format!("type '{name}' is private"), span))
                }

                let ty = scopes.get(*id);
                if is_end {
                    return Some(ResolvedPath::UserType(GenericUserType::new(
                        *id,
                        self.resolve_type_args(scopes, ty.type_params.len(), ty_args, span),
                    )));
                }

                scope = ty.body_scope;
            } else if let Some(id) = scopes.find_in(name, scope) {
                if !id.public && !scopes.can_access_privates(scope) {
                    self.error(Error::new(format!("function '{name}' is private"), span))
                }

                if is_end {
                    return Some(ResolvedPath::Func(GenericFunc::new(
                        *id,
                        self.resolve_type_args(
                            scopes,
                            scopes.get(*id).type_params.len(),
                            ty_args,
                            span,
                        ),
                    )));
                }

                return self.error(Error::new(format!("'{name}' is a function"), span));
            } else if let Some(id) = scopes.find_module_in(name, scope) {
                if !id.public && !scopes.can_access_privates(*id) {
                    self.error(Error::new(format!("module '{name}' is private"), span))
                }

                if !ty_args.is_empty() {
                    return self.error(Error::new(
                        "modules cannot be parameterized with type arguments",
                        span,
                    ));
                }

                if is_end {
                    return Some(ResolvedPath::Module(*id));
                }

                scope = *id;
            } else {
                return Some(ResolvedPath::None(Error::new(
                    format!("no symbol '{name}' found in this module"),
                    span,
                )));
            }
        }

        unreachable!()
    }

    fn resolve_type_args(
        &mut self,
        scopes: &Scopes,
        params: usize,
        args: &[TypeHint],
        span: Span,
    ) -> Vec<TypeId> {
        if args.is_empty() {
            vec![TypeId::Unknown(None); params]
        } else if args.len() != params {
            self.error(Error::new(
                format!(
                    "expected {} type arguments, received {}",
                    params,
                    args.len()
                ),
                span,
            ))
        } else {
            args.iter()
                .map(|ty| self.resolve_type(scopes, ty))
                .collect()
        }
    }

    fn include_universal(&mut self, scopes: &mut Scopes) {
        for scope in self.universal.clone() {
            self.resolve_use(
                scopes,
                false,
                true,
                ResolvedPath::Module(scope),
                Span::default(),
            );
        }
    }

    fn consteval(scopes: &Scopes, expr: &Expr, target: Option<&TypeId>) -> Result<usize, Error> {
        match &expr.data {
            ExprData::Integer { base, value, width } => {
                if let Some(width) = width
                    .as_ref()
                    .and_then(|width| TypeId::from_int_name(width))
                {
                    if let Some(target) = target.filter(|&target| target != &width) {
                        return Err(type_mismatch!(scopes, target, &width, expr.span));
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

    fn typehint_for_struct(
        name: &str,
        type_params: &[(String, Vec<Located<Path>>)],
        span: Span,
    ) -> TypeHint {
        TypeHint::Regular(Located::new(
            span,
            Path::Normal(vec![(
                name.into(),
                type_params
                    .iter()
                    .map(|(n, _)| TypeHint::Regular(Located::new(span, Path::from(n.clone()))))
                    .collect(),
            )]),
        ))
    }
}

fn discriminant_type(max: usize) -> TypeId {
    TypeId::Uint((max as f64).log2().ceil() as u32)
}
