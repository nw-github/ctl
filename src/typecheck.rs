use std::{collections::HashMap, path::PathBuf};

use concat_idents::concat_idents;
use derive_more::{Constructor, Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use indexmap::{map::Entry, IndexMap, IndexSet};
use num_bigint::{BigInt, BigUint};
use num_traits::Num;

use crate::{
    ast::{
        Attribute, BinaryOp, Expr, ExprData, Fn, Param, ParsedUserType, Path, Pattern, Stmt,
        StmtData, Struct, TypeHint, UnaryOp,
    },
    checked_ast::{Block, CheckedExpr, CheckedExprData, CheckedPattern, CheckedStmt},
    lexer::{Located, Span},
    parser::ParsedFile,
    Error, Pipeline, THIS_PARAM, THIS_TYPE,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash, Constructor)]
pub struct GenericFunc {
    pub id: FunctionId,
    pub generics: Vec<TypeId>,
}

impl GenericFunc {
    fn infer_generics(&mut self, mut src: &TypeId, mut target: &TypeId, scopes: &Scopes) {
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
                (TypeId::UserType(src), target) => {
                    if let Some(t) = target.as_user_type() {
                        if src.id != t.id {
                            if let Some(inner) = scopes
                                .as_option_inner(target)
                                .and_then(|i| i.as_user_type())
                            {
                                for (src, target) in src.generics.iter().zip(inner.generics.iter())
                                {
                                    self.infer_generics(src, target, scopes);
                                }

                                break;
                            }
                        }

                        if !src.generics.is_empty() && !t.generics.is_empty() {
                            for (src, target) in src.generics.iter().zip(t.generics.iter()) {
                                self.infer_generics(src, target, scopes);
                            }

                            break;
                        }
                    }

                    if let Some(&index) = scopes.get_user_type(src.id).data.as_func_generic() {
                        if self.generics[index].is_unknown() {
                            self.generics[index] = target.clone();
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
    pub generics: Vec<TypeId>,
}

impl GenericUserType {
    pub fn name(&self, scopes: &Scopes) -> String {
        let mut result = scopes.get_user_type(self.id).name.clone();
        if !self.generics.is_empty() {
            result.push('<');
            for (i, concrete) in self.generics.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&concrete.name(scopes));
            }
            result.push('>');
        }

        result
    }

    fn implements_trait(&self, scopes: &Scopes, bound: &GenericUserType) -> bool {
        for tr in scopes.get_user_type(self.id).impls.iter() {
            let mut tr = tr.as_user_type().unwrap().clone();
            for ut in tr.generics.iter_mut() {
                ut.fill_type_generics(scopes, self);
            }

            if &*tr == bound {
                return true;
            }
        }

        false
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
    IntGeneric,
    FloatGeneric,
    Char,
    Func(Box<GenericFunc>),
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
            (Self::Func(l0), Self::Func(r0)) => l0 == r0,
            (Self::UserType(l0), Self::UserType(r0)) => l0 == r0,
            (Self::Ptr(l0), Self::Ptr(r0)) => l0 == r0,
            (Self::MutPtr(l0), Self::MutPtr(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
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
            Self::Func(l0) => l0.hash(state),
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
                )
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

    pub fn fill_type_generics(&mut self, scopes: &Scopes, inst: &GenericUserType) {
        if inst.generics.is_empty() {
            return;
        }

        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    if !ty.generics.is_empty() {
                        for ty in ty.generics.iter_mut() {
                            ty.fill_type_generics(scopes, inst);
                        }
                    } else if let Some(&index) =
                        scopes.get_user_type(ty.id).data.as_struct_generic()
                    {
                        *src = inst.generics[index].clone();
                    }

                    break;
                }
                _ => break,
            }
        }
    }

    pub fn fill_func_generics(&mut self, scopes: &Scopes, func: &GenericFunc) {
        if func.generics.is_empty() {
            return;
        }

        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    if !ty.generics.is_empty() {
                        for ty in ty.generics.iter_mut() {
                            ty.fill_func_generics(scopes, func);
                        }
                    } else if let Some(&index) = scopes.get_user_type(ty.id).data.as_func_generic()
                    {
                        if !func.generics[index].is_unknown() {
                            *src = func.generics[index].clone();
                        }
                    }

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
                    for ty in ty.generics.iter_mut() {
                        ty.fill_this(this);
                    }

                    break;
                }
                TypeId::TraitSelf => {
                    *src = this.clone();
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
            TypeId::IntGeneric => "{integer}".into(),
            TypeId::FloatGeneric => "{float}".into(),
            TypeId::Char => "char".into(),
            TypeId::Ptr(id) => format!("*{}", id.name(scopes)),
            TypeId::MutPtr(id) => format!("*mut {}", id.name(scopes)),
            TypeId::Func(func) => {
                let f = scopes.get_func(func.id);

                let mut result = format!("fn {}", f.name);
                if !func.generics.is_empty() {
                    result.push('<');
                    for (i, (param, concrete)) in
                        f.type_params.iter().zip(func.generics.iter()).enumerate()
                    {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(&format!("{param} = {}", concrete.name(scopes)));
                    }
                    result.push('>');
                }

                result.push('(');
                for (i, param) in f.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&format!("{}: {}", param.name, param.ty.name(scopes)));
                }
                format!("{result}) {}", f.ret.name(scopes))
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
            (
                TypeId::IntGeneric,
                TypeId::Int(_)
                | TypeId::Uint(_)
                | TypeId::Isize
                | TypeId::Usize
                | TypeId::CInt(_)
                | TypeId::CUint(_),
            ) => true,
            (TypeId::FloatGeneric, TypeId::F32 | TypeId::F64) => true,
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

    fn resolve(&mut self, scopes: &Scopes, checker: &mut TypeChecker) {
        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    for ty in ty.generics.iter_mut() {
                        ty.resolve(scopes, checker);
                    }

                    break;
                }
                TypeId::Unknown(Some(hint)) => {
                    *src = checker.resolve_type_from(scopes, &hint.0, hint.1, false);
                    break;
                }
                _ => break,
            }
        }
    }
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
                pub fn fn_name(&self, name: &str, scope: ScopeId) -> Option<Vis<$name>> {
                    self[scope].$vec
                        .iter()
                        .rev()
                        .find_map(|id| (self.$vec[id.0].$($parts).+ == name).then_some(*id))
                }
            });

            concat_idents!(fn_name = find_, $suffix, _from {
                pub fn fn_name(&self, name: &str, scope: ScopeId) -> Option<Vis<$name>> {
                    for (id, scope) in self.iter_from(scope) {
                        concat_idents!(fn_name = find_, $suffix, _in {
                            if let Some(item) = self.fn_name(name, id) {
                                return Some(item);
                            }
                        });

                        if matches!(scope.kind, ScopeKind::Module) {
                            break;
                        }
                    }

                    None
                }
            });

            concat_idents!(fn_name = find_, $suffix {
                #[allow(dead_code)]
                pub fn fn_name(&self, name: &str) -> Option<Vis<$name>> {
                    concat_idents!(fn_name = find_, $suffix, _from {
                        self.fn_name(name, self.current)
                    })
                }
            });

            concat_idents!(fn_name = insert_, $suffix, _in {
                pub fn fn_name(&mut self, item: $output, public: bool, scope: ScopeId) -> $name {
                    let index = self.$vec.len();
                    self.$vec.push(Scoped::new(item, scope));
                    let itemid = $name(index);
                    self[scope].$vec.insert(Vis {
                        item: itemid,
                        public,
                    });
                    itemid
                }
            });

            concat_idents!(fn_name = insert_, $suffix {
                #[allow(dead_code)]
                pub fn fn_name(&mut self, item: $output, public: bool) -> $name {
                    concat_idents!(fn_name = insert_, $suffix, _in {
                        self.fn_name(item, public, self.current_id())
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

id!(FunctionId => Function, fns, name, func);
id!(UserTypeId => UserType, types, name, user_type);
id!(VariableId => Variable, vars, name, var);

#[derive(Default, Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum ScopeKind {
    Block(Option<TypeId>, bool),
    Loop(Option<TypeId>, bool),
    Function(FunctionId),
    UserType(UserTypeId),
    Module,
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
    pub name: String,
    pub is_async: bool,
    pub is_extern: bool,
    pub variadic: bool,
    pub type_params: Vec<String>,
    pub params: Vec<CheckedParam>,
    pub ret: TypeId,
    pub body: Option<Vec<CheckedStmt>>,
    pub constructor: bool,
    pub body_scope: ScopeId,
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
        TypeId::Uint(8)
    }

    pub fn variant_tag(&self, name: &str) -> Option<usize> {
        self.variants
            .iter()
            .filter(|m| !m.shared)
            .position(|m| m.name == name)
    }
}

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct {
        members: Vec<Member>,
        init: FunctionId,
    },
    Union(Union),
    Enum(TypeId),
    FuncGeneric(usize),
    StructGeneric(usize),
    Trait,
}

#[derive(Debug)]
pub struct UserType {
    pub name: String,
    pub body_scope: ScopeId,
    pub data: UserTypeData,
    pub impls: Vec<TypeId>,
    pub type_params: usize,
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
    Func,
    Var(VariableId),
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
    pub item: T,
    pub public: bool,
}

impl<T: std::hash::Hash> std::hash::Hash for Vis<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.item.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Vis<T> {
    fn eq(&self, other: &Self) -> bool {
        self.item.eq(other)
    }
}

impl<T: Eq> Eq for Vis<T> {}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub name: Option<String>,
    pub parent: Option<ScopeId>,
    pub fns: IndexSet<Vis<FunctionId>>,
    pub types: IndexSet<Vis<UserTypeId>>,
    pub vars: IndexSet<Vis<VariableId>>,
    pub children: IndexMap<String, Vis<ScopeId>>,
}

pub struct Scopes {
    scopes: Vec<Scope>,
    current: ScopeId,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
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
        for scope_name in self.iter_from(id).flat_map(|scope| scope.1.name.as_ref()) {
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

    pub fn current_id(&self) -> ScopeId {
        self.current
    }

    pub fn enter<T>(
        &mut self,
        name: Option<String>,
        kind: ScopeKind,
        public: bool,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let id = ScopeId(self.scopes.len());
        // blocks are the only unnamed scopes
        self.current().children.insert(
            name.clone().unwrap_or(String::new()),
            Vis { item: id, public },
        );
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
        self.enter_id(*id, f)
    }

    pub fn enter_id<T>(&mut self, id: ScopeId, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.current;
        self.current = id;
        let result = f(self);
        self.current = prev;
        result
    }

    pub fn this_type(&self) -> Option<TypeId> {
        self.iter().find_map(|(_, scope)| {
            if let ScopeKind::UserType(id) = scope.kind {
                let ty = self.get_user_type(id);
                if !ty.data.is_func_generic() && !ty.data.is_struct_generic() {
                    if ty.data.is_trait() {
                        return Some(TypeId::TraitSelf);
                    }

                    return Some(TypeId::UserType(
                        GenericUserType::new(
                            id,
                            self[ty.body_scope]
                                .types
                                .iter()
                                .filter(|&&id| self.get_user_type(*id).data.is_struct_generic())
                                .map(|&id| {
                                    TypeId::UserType(
                                        GenericUserType {
                                            id: *id,
                                            generics: vec![],
                                        }
                                        .into(),
                                    )
                                })
                                .collect(),
                        )
                        .into(),
                    ));
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
            if matches!(current.kind, ScopeKind::Module) {
                return Some(id);
            }
        }

        None
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn find_module_in(&self, name: &str, scope: ScopeId) -> Option<Vis<ScopeId>> {
        self[scope]
            .children
            .get(name)
            .filter(|&&id| self[*id].kind.is_module())
            .copied()
    }

    pub fn find_module_from(&self, name: &str, scope: ScopeId) -> Option<Vis<ScopeId>> {
        for (id, scope) in self.iter_from(scope) {
            if let Some(item) = self.find_module_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module) {
                break;
            }
        }

        None
    }

    pub fn find_module(&self, name: &str) -> Option<Vis<ScopeId>> {
        self.find_module_from(name, self.current)
    }

    pub fn find_free_fn_from(&self, name: &str, scope: ScopeId) -> Option<Vis<FunctionId>> {
        for (id, scope) in self
            .iter_from(scope)
            .filter(|(_, s)| !matches!(s.kind, ScopeKind::UserType(_)))
        {
            if let Some(item) = self.find_func_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module) {
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
                .map(|ut| &ut.generics[0])
        })
    }

    pub fn make_lang_type(&self, name: &str, generics: Vec<TypeId>) -> Option<TypeId> {
        Some(TypeId::UserType(
            GenericUserType::new(self.lang_types.get(name).copied()?, generics).into(),
        ))
    }

    pub fn intrinsic_name(&self, id: FunctionId) -> Option<&str> {
        self.intrinsics.get(&id).map(|s| s.as_str())
    }

    fn can_access_privates(&self, here: ScopeId, scope: ScopeId) -> bool {
        let target = self
            .module_of(scope)
            .expect("root scope passed to can_access_privates()");
        self.iter_from(here).any(|(id, _)| id == target)
    }

    fn get_member_fn(
        &self,
        member: &str,
        ut: &UserType,
    ) -> Option<(Option<GenericUserType>, Vis<FunctionId>)> {
        if let Some(func) = (ut.data.is_struct() || ut.data.is_union() || ut.data.is_enum())
            .then(|| self.find_func_in(member, ut.body_scope))
            .flatten()
        {
            return Some((None, func));
        }

        for ut in ut.impls.iter().map(|ut| ut.as_user_type().unwrap()) {
            if let Some(func) = self.find_func_in(member, self.get_user_type(ut.id).body_scope) {
                return Some((Some((**ut).clone()), func));
            }
        }

        None
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

pub struct TypeChecker {
    errors: Vec<Error>,
    universal: Vec<ScopeId>,
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
        };
        let mut scopes = Scopes::new();
        let mut files: Vec<_> = module.iter().map(|file| file.path.clone()).collect();

        for lib in libs {
            let parsed = Pipeline::new(lib, files.len()).parse()?;
            this.check_one(&mut scopes, &parsed.path, parsed.state.0, &mut files);
        }

        let scope = this.check_one(&mut scopes, path, module, &mut vec![]);
        this.check_trait_impls(&scopes);

        Ok(Module {
            scope,
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
        scopes.enter(Some(project.clone()), ScopeKind::Module, true, |scopes| {
            for file in module.iter() {
                match &file.ast.data {
                    StmtData::Module { name, body, .. } if name == &project => {
                        self.include_universal(scopes);
                        for stmt in body {
                            self.forward_declare(stmt, scopes);
                        }
                    }
                    _ => self.forward_declare(&file.ast, scopes),
                }
            }

            paths.reserve(module.len());
            for file in module {
                self.errors.extend(file.errors);
                paths.push(file.path);

                match file.ast.data {
                    StmtData::Module { name, body, .. } if name == project => {
                        self.include_universal(scopes);
                        for stmt in body {
                            self.check_stmt(scopes, stmt);
                        }
                    }
                    _ => {
                        self.check_stmt(scopes, file.ast);
                    }
                }
            }

            scopes.current_id()
        })
    }

    fn insert_type(
        &mut self,
        scopes: &mut Scopes,
        ty: UserType,
        public: bool,
        attrs: &[Attribute],
    ) -> UserTypeId {
        let id = scopes.insert_user_type(ty, public);
        if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "lang") {
            let Some(name) = attr.props.get(0) else {
                self.error::<()>(Error::new("language item must have name", attr.name.span));
                return id;
            };

            scopes.lang_types.insert(name.name.data.clone(), id);
        }

        id
    }

    fn forward_declare(&mut self, stmt: &Stmt, scopes: &mut Scopes) {
        match &stmt.data {
            StmtData::Module { public, name, body } => {
                scopes.enter(Some(name.clone()), ScopeKind::Module, *public, |scopes| {
                    // FIXME: only allow core and std to define these
                    if stmt.attrs.iter().any(|attr| attr.name.data == "autouse") {
                        self.universal.push(scopes.current);
                    }

                    for stmt in body {
                        self.forward_declare(stmt, scopes);
                    }
                });
            }
            StmtData::UserType(data) => match data {
                ParsedUserType::Struct(base) => {
                    let init = self.forward_declare_fn(
                        scopes,
                        true,
                        &Fn {
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
                            ret: Self::typehint_for_struct(base, stmt.span),
                            body: None,
                        },
                        &stmt.attrs,
                    );
                    let id = self.insert_type(
                        scopes,
                        UserType {
                            name: base.name.data.clone(),
                            body_scope: ScopeId(0),
                            data: UserTypeData::Struct {
                                members: Vec::new(),
                                init,
                            },
                            type_params: base.type_params.len(),
                            impls: Vec::new(),
                        },
                        base.public,
                        &stmt.attrs,
                    );
                    scopes.enter(
                        Some(base.name.data.clone()),
                        ScopeKind::UserType(id),
                        base.public,
                        |scopes| {
                            scopes.get_user_type_mut(id).body_scope = scopes.current_id();
                            *scopes.get_user_type_mut(id).data.as_struct_mut().unwrap().0 = base
                                .members
                                .iter()
                                .map(|member| Member {
                                    public: member.public,
                                    name: member.name.clone(),
                                    shared: member.shared,
                                    ty: self.resolve_type(scopes, &member.ty, true),
                                })
                                .collect();

                            for (i, (name, impls)) in base.type_params.iter().enumerate() {
                                scopes.insert_user_type(
                                    UserType {
                                        name: name.clone(),
                                        body_scope: scopes.current_id(),
                                        data: UserTypeData::StructGeneric(i),
                                        type_params: 0,
                                        impls: impls
                                            .iter()
                                            .flat_map(|path| self.resolve_impl(scopes, path, true))
                                            .collect(),
                                    },
                                    false,
                                );
                            }

                            for path in base.impls.iter() {
                                if let Some(ty) = self.resolve_impl(scopes, path, true) {
                                    scopes.get_user_type_mut(id).impls.push(ty);
                                }
                            }

                            for f in base.functions.iter() {
                                self.forward_declare_fn(scopes, false, f, &[]);
                            }
                        },
                    )
                }
                ParsedUserType::Union {
                    tag: _,
                    base,
                    is_unsafe,
                } => {
                    let id = self.insert_type(
                        scopes,
                        UserType {
                            name: base.name.data.clone(),
                            body_scope: ScopeId(0),
                            data: UserTypeData::Union(Union {
                                variants: Vec::new(),
                                is_unsafe: *is_unsafe,
                            }),
                            type_params: base.type_params.len(),
                            impls: Vec::new(),
                        },
                        base.public,
                        &stmt.attrs,
                    );
                    scopes.enter(
                        Some(base.name.data.clone()),
                        ScopeKind::UserType(id),
                        base.public,
                        |scopes| {
                            scopes.get_user_type_mut(id).body_scope = scopes.current_id();
                            let mut variants = Vec::with_capacity(base.members.len());
                            let mut params = Vec::with_capacity(base.members.len());
                            for member in base.members.iter() {
                                variants.push(Member {
                                    public: member.public,
                                    name: member.name.clone(),
                                    shared: member.shared,
                                    ty: self.resolve_type(scopes, &member.ty, true),
                                });

                                if member.shared && *is_unsafe {
                                    // FIXME: span should be related to the member
                                    self.error::<()>(Error::new(
                                        "cannot have shared members in an unsafe union",
                                        stmt.span,
                                    ));
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

                            scopes
                                .get_user_type_mut(id)
                                .data
                                .as_union_mut()
                                .unwrap()
                                .variants = variants;

                            for (i, (name, impls)) in base.type_params.iter().enumerate() {
                                scopes.insert_user_type(
                                    UserType {
                                        name: name.clone(),
                                        body_scope: scopes.current_id(),
                                        data: UserTypeData::StructGeneric(i),
                                        type_params: 0,
                                        impls: impls
                                            .iter()
                                            .flat_map(|path| self.resolve_impl(scopes, path, true))
                                            .collect(),
                                    },
                                    false,
                                );
                            }

                            for path in base.impls.iter() {
                                if let Some(ty) = self.resolve_impl(scopes, path, true) {
                                    scopes.get_user_type_mut(id).impls.push(ty);
                                }
                            }

                            for member in base.members.iter() {
                                let mut params = params.clone();
                                if !matches!(member.ty, TypeHint::Void) {
                                    params.push(Param {
                                        mutable: false,
                                        keyword: false,
                                        name: member.name.clone(),
                                        ty: member.ty.clone(),
                                        default: None,
                                    });
                                }

                                self.forward_declare_fn(
                                    scopes,
                                    true,
                                    &Fn {
                                        public: base.public,
                                        name: Located::new(member.name.clone(), Span::default()),
                                        is_async: false,
                                        is_extern: false,
                                        variadic: false,
                                        is_unsafe: false,
                                        type_params: base.type_params.clone(),
                                        params,
                                        ret: Self::typehint_for_struct(base, stmt.span),
                                        body: None,
                                    },
                                    &[],
                                );
                            }

                            for f in base.functions.iter() {
                                self.forward_declare_fn(scopes, false, f, &[]);
                            }
                        },
                    )
                }
                ParsedUserType::Trait {
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
                            name: name.clone(),
                            body_scope: ScopeId(0),
                            data: UserTypeData::Trait,
                            type_params: type_params.len(),
                            impls: Vec::new(),
                        },
                        *public,
                        &stmt.attrs,
                    );
                    scopes.enter(
                        Some(name.clone()),
                        ScopeKind::UserType(id),
                        *public,
                        |scopes| {
                            scopes.get_user_type_mut(id).body_scope = scopes.current_id();

                            for (i, (name, impls)) in type_params.iter().enumerate() {
                                scopes.insert_user_type(
                                    UserType {
                                        name: name.clone(),
                                        body_scope: scopes.current_id(),
                                        data: UserTypeData::StructGeneric(i),
                                        type_params: 0,
                                        impls: impls
                                            .iter()
                                            .flat_map(|path| self.resolve_impl(scopes, path, true))
                                            .collect(),
                                    },
                                    false,
                                );
                            }

                            for path in impls.iter() {
                                if let Some(ty) = self.resolve_impl(scopes, path, true) {
                                    scopes.get_user_type_mut(id).impls.push(ty);
                                }
                            }

                            for f in functions.iter() {
                                self.forward_declare_fn(scopes, false, f, &[]);
                            }
                        },
                    )
                }
                ParsedUserType::Enum {
                    public,
                    name,
                    impls,
                    variants,
                    functions,
                } => {
                    let backing = TypeId::Uint(8);
                    let impls = impls
                        .iter()
                        .flat_map(|path| self.resolve_impl(scopes, path, true))
                        .collect();
                    let id = self.insert_type(
                        scopes,
                        UserType {
                            name: name.data.clone(),
                            body_scope: ScopeId(0),
                            data: UserTypeData::Enum(backing.clone()),
                            type_params: 0,
                            impls,
                        },
                        *public,
                        &stmt.attrs,
                    );

                    scopes.enter(
                        Some(name.data.clone()),
                        ScopeKind::UserType(id),
                        *public,
                        |scopes| {
                            scopes.get_user_type_mut(id).body_scope = scopes.current_id();

                            for (i, (name, _)) in variants.iter().enumerate() {
                                scopes.insert_var(
                                    Variable {
                                        name: name.clone(),
                                        ty: TypeId::UserType(
                                            GenericUserType::new(id, vec![]).into(),
                                        ),
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

                            for f in functions.iter() {
                                self.forward_declare_fn(scopes, false, f, &[]);
                            }
                        },
                    );
                }
            },
            StmtData::Fn(f) => _ = self.forward_declare_fn(scopes, false, f, &stmt.attrs),
            StmtData::Static {
                public, name, ty, ..
            } => {
                scopes.insert_var(
                    Variable {
                        name: name.clone(),
                        ty: ty
                            .as_ref()
                            .map(|ty| self.resolve_type(scopes, ty, true))
                            .unwrap_or(TypeId::Unknown(None)),
                        is_static: true,
                        mutable: false,
                        value: None,
                    },
                    *public,
                );
            }
            StmtData::Use { path, public, all } => {
                if let Some(path) = self.resolve_path(scopes, path, stmt.span, true) {
                    self.resolve_use(scopes, *public, *all, path, stmt.span, true);
                }
            }
            _ => {}
        }
    }

    fn forward_declare_fn(
        &mut self,
        scopes: &mut Scopes,
        constructor: bool,
        f: &Fn,
        attrs: &[Attribute],
    ) -> FunctionId {
        if f.variadic && !f.is_extern {
            self.error(Error::new(
                "only extern functions may be variadic",
                f.name.span,
            ))
        }

        let id = scopes.insert_func_in(
            Function {
                attrs: attrs.to_vec(),
                name: f.name.data.clone(),
                is_async: f.is_async,
                is_extern: f.is_extern,
                variadic: f.variadic,
                type_params: f.type_params.iter().map(|x| x.0.clone()).collect(),
                params: Vec::new(),
                ret: TypeId::Unknown(None),
                body: None,
                body_scope: ScopeId(0),
                constructor,
            },
            f.public,
            scopes.current_id(),
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

        scopes.enter(
            Some(f.name.data.clone()),
            ScopeKind::Function(id),
            false,
            |scopes| {
                if !f.type_params.is_empty() {
                    for (i, (name, impls)) in f.type_params.iter().enumerate() {
                        let id = scopes.insert_user_type(
                            UserType {
                                name: name.clone(),
                                body_scope: scopes.current_id(),
                                data: UserTypeData::FuncGeneric(i),
                                type_params: 0,
                                impls: Vec::new(),
                            },
                            false,
                        );

                        scopes.get_user_type_mut(id).impls = impls
                            .iter()
                            .flat_map(|path| self.resolve_impl(scopes, path, true))
                            .collect();
                    }
                }

                scopes.get_func_mut(id).body_scope = scopes.current_id();
                scopes.get_func_mut(id).params = f
                    .params
                    .iter()
                    .map(|param| CheckedParam {
                        mutable: param.mutable,
                        keyword: param.keyword,
                        name: param.name.clone(),
                        ty: self.resolve_type(scopes, &param.ty, true),
                    })
                    .collect();
                scopes.get_func_mut(id).ret = self.resolve_type(scopes, &f.ret, true);

                if let Some(body) = &f.body {
                    for stmt in body.iter() {
                        self.forward_declare(stmt, scopes);
                    }
                }
            },
        );

        id
    }

    fn check_stmt(&mut self, scopes: &mut Scopes, stmt: Stmt) -> CheckedStmt {
        macro_rules! fn_by_name {
            ($scopes: expr, $name: expr) => {
                *$scopes.find_func($name).unwrap()
            };
        }

        match stmt.data {
            StmtData::Module {
                public: _,
                name,
                body,
            } => {
                return CheckedStmt::Module(scopes.find_enter(&name, |scopes| {
                    self.include_universal(scopes);
                    Block {
                        body: body
                            .into_iter()
                            .map(|stmt| self.check_stmt(scopes, stmt))
                            .collect(),
                        scope: scopes.current_id(),
                    }
                }))
            }
            StmtData::UserType(data) => match data {
                ParsedUserType::Struct(base) => {
                    self.check_fn(scopes, fn_by_name!(scopes, &base.name.data), None);

                    let id = *scopes.find_user_type(&base.name.data).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        for (name, _) in base.type_params.iter() {
                            self.resolve_impls(scopes, *scopes.find_user_type(name).unwrap());
                        }

                        self.resolve_impls(scopes, id);
                        for i in 0..base.members.len() {
                            resolve_type!(
                                self,
                                scopes,
                                scopes.get_user_type_mut(id).data.as_struct_mut().unwrap().0[i].ty
                            );
                        }

                        for f in base.functions {
                            self.check_fn(scopes, fn_by_name!(scopes, &f.name.data), f.body);
                        }
                    });
                }
                ParsedUserType::Union { tag: _, base, .. } => {
                    let id = *scopes.find_user_type(&base.name.data).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        for (name, _) in base.type_params.iter() {
                            self.resolve_impls(scopes, *scopes.find_user_type(name).unwrap());
                        }

                        self.resolve_impls(scopes, id);
                        for i in 0..base.members.len() {
                            resolve_type!(
                                self,
                                scopes,
                                scopes
                                    .get_user_type_mut(id)
                                    .data
                                    .as_union_mut()
                                    .unwrap()
                                    .variants[i]
                                    .ty
                            );

                            self.check_fn(scopes, fn_by_name!(scopes, &base.members[i].name), None);
                        }

                        for f in base.functions {
                            self.check_fn(scopes, fn_by_name!(scopes, &f.name.data), f.body);
                        }
                    });
                }
                ParsedUserType::Trait {
                    name,
                    type_params,
                    functions,
                    ..
                } => {
                    let id = *scopes.find_user_type(&name).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        for (name, _) in type_params.iter() {
                            self.resolve_impls(scopes, *scopes.find_user_type(name).unwrap());
                        }

                        self.resolve_impls(scopes, id);
                        for f in functions {
                            self.check_fn(scopes, fn_by_name!(scopes, &f.name.data), f.body);
                        }
                    });
                }
                ParsedUserType::Enum {
                    name,
                    variants,
                    functions,
                    ..
                } => {
                    let id = *scopes.find_user_type(&name.data).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        self.resolve_impls(scopes, id);

                        for (name, expr) in variants {
                            scopes.get_var_mut(*scopes.find_var(&name).unwrap()).value = expr
                                .map(|expr| self.check_expr(scopes, expr, Some(&TypeId::Usize)));
                        }

                        for f in functions {
                            self.check_fn(scopes, fn_by_name!(scopes, &f.name.data), f.body);
                        }
                    });
                }
            },
            StmtData::Expr(expr) => return CheckedStmt::Expr(self.check_expr(scopes, expr, None)),
            StmtData::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty, false);
                    if let Some(value) = value {
                        let value = self.type_check(scopes, value, &ty);
                        return CheckedStmt::Let(scopes.insert_var(
                            Variable {
                                name,
                                ty,
                                is_static: false,
                                mutable,
                                value: Some(value),
                            },
                            false,
                        ));
                    } else {
                        return CheckedStmt::Let(scopes.insert_var(
                            Variable {
                                name,
                                ty,
                                is_static: false,
                                mutable,
                                value: None,
                            },
                            false,
                        ));
                    }
                } else if let Some(value) = value {
                    let value = self.check_expr(scopes, value, None);
                    return CheckedStmt::Let(scopes.insert_var(
                        Variable {
                            name,
                            ty: value.ty.clone(),
                            is_static: false,
                            mutable,
                            value: Some(value),
                        },
                        false,
                    ));
                } else {
                    return self.error(Error::new("cannot infer type", stmt.span));
                }
            }
            StmtData::Fn(f) => self.check_fn(scopes, fn_by_name!(scopes, &f.name.data), f.body),
            StmtData::Static {
                name, ty, value, ..
            } => {
                // FIXME: detect cycles like static X: usize = X;
                // FIXME: non-const statics should be disallowed
                let (value, ty) = if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty, false);
                    (self.type_check(scopes, value, &ty), ty)
                } else {
                    let value = self.check_expr(scopes, value, None);
                    let ty = value.ty.clone();
                    (value, ty)
                };

                let var = scopes.get_var_mut(*scopes.find_var(&name).unwrap());
                var.ty = ty;
                var.value = Some(value);
            }
            StmtData::Use { public, path, all } => {
                if let Some(path) = self.resolve_path(scopes, &path, stmt.span, false) {
                    self.resolve_use(scopes, public, all, path, stmt.span, false);
                }
            }
            StmtData::Error => return CheckedStmt::Error,
        }

        CheckedStmt::None
    }

    fn check_trait_impls(&mut self, scopes: &Scopes) {
        for tr_id in scopes
            .types
            .iter()
            .enumerate()
            .filter(|(_, ut)| ut.data.is_trait())
            .map(|(i, _)| UserTypeId(i))
        {
            for (i, ut) in
                scopes.types.iter().enumerate().filter(|(_, ut)| {
                    ut.data.is_struct() || ut.data.is_enum() || ut.data.is_union()
                })
            {
                if let Some(im) = ut
                    .impls
                    .iter()
                    .find(|im| im.as_user_type().unwrap().id == tr_id)
                {
                    self.check_impl(
                        scopes,
                        &TypeId::UserType(
                            GenericUserType::new(
                                UserTypeId(i),
                                scopes[ut.body_scope]
                                    .types
                                    .iter()
                                    .filter(|&&id| {
                                        scopes.get_user_type(*id).data.is_struct_generic()
                                    })
                                    .map(|&id| {
                                        TypeId::UserType(GenericUserType::new(*id, vec![]).into())
                                    })
                                    .collect(),
                            )
                            .into(),
                        ),
                        im.as_user_type().unwrap(),
                        Span::default(),
                    );
                }
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
        let lhs = scopes.get_func(lhs_id);
        let rhs = scopes.get_func(rhs_id);
        if lhs.params.len() != rhs.params.len() || lhs.type_params.len() != rhs.type_params.len() {
            return Err("type parameters are incorrect".into());
        }

        let compare_types = |a: &TypeId, mut b: TypeId| {
            b.fill_func_generics(
                scopes,
                &GenericFunc::new(
                    lhs_id,
                    lhs.type_params
                        .iter()
                        .map(|name| {
                            TypeId::UserType(
                                GenericUserType::new(
                                    *scopes.find_user_type_in(name, lhs.body_scope).unwrap(),
                                    vec![],
                                )
                                .into(),
                            )
                        })
                        .collect(),
                ),
            );
            b.fill_type_generics(scopes, rhs_ty);
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

        for (i, (s, t)) in lhs
            .params
            .iter()
            .zip(rhs.params.iter().cloned())
            .enumerate()
        {
            if let Err(err) = compare_types(&s.ty, t.ty) {
                return Err(format!("parameter {} is incorrect: {err}", i + 1));
            }
        }

        for (i, (s, t)) in lhs
            .type_params
            .iter()
            .zip(rhs.type_params.iter())
            .enumerate()
        {
            let s = scopes.get_user_type(*scopes.find_user_type_in(s, lhs.body_scope).unwrap());
            let t = scopes.get_user_type(*scopes.find_user_type_in(t, rhs.body_scope).unwrap());

            if s.impls.len() != t.impls.len() {
                return Err(format!("generic parameter {} is incorrect", i + 1));
            }

            for (s, t) in s.impls.iter().zip(t.impls.iter()) {
                for (s, t) in s
                    .as_user_type()
                    .unwrap()
                    .generics
                    .iter()
                    .zip(t.as_user_type().unwrap().generics.clone().into_iter())
                {
                    if let Err(err) = compare_types(s, t) {
                        return Err(format!("generic parameter {} is incorrect: {err}", i + 1));
                    }
                }
            }
        }

        Ok(())
    }

    fn check_impl(&mut self, scopes: &Scopes, this: &TypeId, gtr: &GenericUserType, span: Span) {
        // TODO: detect and fail on circular trait dependencies
        // TODO: default implementations
        let ut = scopes.get_user_type(this.as_user_type().unwrap().id);
        let tr = scopes.get_user_type(gtr.id);
        for tr in tr.impls.iter() {
            self.check_impl(scopes, this, tr.as_user_type().unwrap(), span);
        }

        for &rhs in scopes[tr.body_scope].fns.iter() {
            let rhs_func = scopes.get_func(*rhs);
            if let Some(lhs) = scopes.find_func_in(&rhs_func.name, ut.body_scope) {
                if let Err(err) = Self::signatures_match(scopes, this, *lhs, *rhs, gtr) {
                    self.error::<()>(Error::new(
                        format!(
                            "must implement '{}::{}': {err}",
                            gtr.name(scopes),
                            rhs_func.name
                        ),
                        span,
                    ));
                }
            } else {
                self.error::<()>(Error::new(
                    format!("must implement '{}::{}'", gtr.name(scopes), rhs_func.name),
                    span,
                ));
            }
        }
    }

    fn check_fn(&mut self, scopes: &mut Scopes, id: FunctionId, body: Option<Vec<Stmt>>) {
        // TODO: disallow private type in public interface
        scopes.enter_id(scopes.get_func(id).body_scope, |scopes| {
            for i in 0..scopes.get_func(id).params.len() {
                resolve_type!(self, scopes, scopes.get_func_mut(id).params[i].ty);
            }

            resolve_type!(self, scopes, scopes.get_func_mut(id).ret);
            for i in 0..scopes.get_func(id).type_params.len() {
                let id = scopes
                    .find_user_type(&scopes.get_func(id).type_params[i])
                    .unwrap();
                self.resolve_impls(scopes, *id);
            }

            for param in scopes
                .get_func(id)
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
                scopes.insert_var(param, false);
            }

            if let Some(body) = body {
                scopes.get_func_mut(id).body = Some(
                    body.into_iter()
                        .map(|stmt| self.check_stmt(scopes, stmt))
                        .collect(),
                );
            }
        });
    }

    fn check_expr(
        &mut self,
        scopes: &mut Scopes,
        expr: Expr,
        target: Option<&TypeId>,
    ) -> CheckedExpr {
        macro_rules! lbox {
            ($e: expr) => {
                Located::new($e, Span::default()).into()
            };
        }

        macro_rules! l {
            ($e: expr) => {
                Located::new($e, Span::default())
            };
        }

        let span = expr.span;
        match expr.data {
            ExprData::Binary { op, left, right } => {
                let left = self.check_expr(scopes, *left, target);
                let right = type_check_bail!(self, scopes, *right, &left.ty);
                if !left.ty.supports_binop(op) {
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
                let (ut, ty) = if let Some(TypeId::Array(inner)) = target {
                    (None, inner.0.clone())
                } else {
                    let Some(vec) = scopes.lang_types.get("vec").copied() else {
                        return self.error(Error::new("missing language item: Vec", expr.span));
                    };

                    let Some(set) = scopes.lang_types.get("set").copied() else {
                        return self.error(Error::new("missing language item: Set", expr.span));
                    };

                    if let Some(ty) = target
                        .and_then(|target| target.as_user_type())
                        .filter(|ut| ut.id == vec)
                        .map(|ut| ut.generics[0].clone())
                    {
                        (Some(vec), ty)
                    } else if let Some(ty) = target
                        .and_then(|target| target.as_user_type())
                        .filter(|ut| ut.id == set)
                        .map(|ut| ut.generics[0].clone())
                    {
                        (Some(set), ty)
                    } else if let Some(expr) = elements.next() {
                        let expr = self.check_expr(scopes, expr, None);
                        let ty = expr.ty.clone();
                        checked.push(expr);
                        (Some(vec), ty)
                    } else {
                        return self
                            .error(Error::new("cannot infer type of array literal", expr.span));
                    }
                };

                checked.extend(elements.map(|e| self.type_check(scopes, e, &ty)));
                if let Some(ut) = ut {
                    CheckedExpr::new(
                        TypeId::UserType(GenericUserType::new(ut, vec![ty]).into()),
                        if Some(&ut) == scopes.lang_types.get("set") {
                            CheckedExprData::Set(checked)
                        } else {
                            CheckedExprData::Vec(checked)
                        },
                    )
                } else {
                    CheckedExpr::new(
                        TypeId::Array(Box::new((ty, checked.len()))),
                        CheckedExprData::Array(checked),
                    )
                }
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
                    let Some(vec) = scopes.lang_types.get("vec").copied() else {
                        return self.error(Error::new("no symbol 'Vec' found in this module", expr.span));
                    };

                    let (init, ty) = if let Some(ty) = target
                        .and_then(|target| target.as_user_type())
                        .filter(|ut| ut.id == vec)
                        .map(|ut| ut.generics[0].clone())
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
            }
            ExprData::Tuple(_) => todo!(),
            ExprData::Map(elements) => {
                // TODO: make sure the key type respects the trait bounds
                let Some(std_map) = scopes.lang_types.get("map").copied() else {
                    return self.error(Error::new("no symbol 'Map' found in this module", expr.span));
                };

                let mut result = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let (key_ty, val_ty) = if let Some((k, v)) = target
                    .and_then(|target| target.as_user_type())
                    .filter(|ut| ut.id == std_map)
                    .map(|ut| (ut.generics[0].clone(), ut.generics[1].clone()))
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
                                self.check_expr(scopes, Located::new(ExprData::Void, span), target),
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
                        .filter(|target| TypeId::IntGeneric.coerces_to(scopes, target))
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
                    .filter(|target| TypeId::FloatGeneric.coerces_to(scopes, target))
                    .cloned()
                    .unwrap_or(TypeId::F64),
                CheckedExprData::Float(value),
            ),
            ExprData::Path(path) => match self.resolve_path(scopes, &path, span, false) {
                Some(ResolvedPath::Var(id)) => CheckedExpr::new(
                    scopes.get_var(id).ty.clone(),
                    CheckedExprData::Symbol(Symbol::Var(id)),
                ),
                Some(ResolvedPath::Func(id)) => CheckedExpr::new(
                    TypeId::Func(id.into()),
                    CheckedExprData::Symbol(Symbol::Func),
                ),
                Some(ResolvedPath::UserType(ut)) => {
                    let Some(st) = scopes.get_user_type(ut.id).data.as_struct() else {
                        return self.error(Error::new("expected value", span));
                    };

                    CheckedExpr::new(
                        TypeId::Func(GenericFunc::new(*st.1, ut.generics).into()),
                        CheckedExprData::Symbol(Symbol::Func),
                    )
                }
                Some(ResolvedPath::Module(_)) => {
                    self.error(Error::new("expected value, found module", span))
                }
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
                    if !lhs.ty.supports_binop(op) {
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
                let ScopeKind::Block(target, yields) = &scopes[block.scope].kind else {
                    panic!("ICE: target of block changed from block to something else");
                };
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
                        .map(|target| &target.generics[0])
                };

                let if_span = if_branch.span;
                let mut if_branch = self.check_expr(scopes, *if_branch, target);
                if let Some(target) = target {
                    if_branch = self.type_check_checked(scopes, if_branch, target, if_span);
                }

                let mut out_type = if_branch.ty.clone();
                let else_branch = if let Some(e) = else_branch {
                    Some(self.type_check(scopes, *e, &out_type))
                } else {
                    // this separates these two cases:
                    //   let x /* void? */ = if whatever { yield void; };
                    //   let x /* void */ = if whatever { };
                    if matches!(&if_branch.data, CheckedExprData::Block(b) if
                        matches!(scopes[b.scope].kind, ScopeKind::Block(_, yields) if yields))
                    {
                        out_type = scopes.make_lang_type("option", vec![out_type]).unwrap();
                        if_branch = if_branch.coerce_to(&out_type, scopes);

                        Some(self.check_expr(
                            scopes,
                            Located::new(ExprData::None, span),
                            Some(&out_type),
                        ))
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
                        .map(|target| &target.generics[0])
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
                            .get_user_type(ut.id)
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

                let id = scopes.insert_var(
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

                let CheckedExprData::Loop { iter, .. } = &mut body.data else { unreachable!() };
                *iter = Some(id);
                body
            }
            ExprData::Member {
                source,
                member: name,
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
                let ut_id = match &id {
                    TypeId::UserType(data) => data.id,
                    _ => {
                        return self.error(Error::new(
                            format!("cannot get member of type '{}'", id.name(scopes)),
                            span,
                        ));
                    }
                };

                if let Some(ut) = scopes.get_user_type(ut_id).members() {
                    for i in 0..ut.len() {
                        resolve_type!(
                            self,
                            scopes,
                            scopes.get_user_type_mut(ut_id).members_mut().unwrap()[i].ty
                        );
                    }
                }

                let ty = scopes.get_user_type(ut_id);
                if let Some(members) = ty.members() {
                    if let Some(member) = members.iter().find(|m| m.name == name) {
                        if let Some(union) = ty.data.as_union() {
                            if !member.shared && !union.is_unsafe {
                                // TODO: access to unsafe union members should be unsafe
                                return self.error(Error::new(
                                    "cannot access union variant with '.' (only shared members)",
                                    span,
                                ));
                            }
                        }

                        if !member.public && !scopes.can_access_privates(scopes.current, ty.scope) {
                            self.error(Error::new(
                                format!(
                                    "cannot access private member '{}' of type '{}'",
                                    member.name,
                                    id.name(scopes)
                                ),
                                span,
                            ))
                        }

                        let mut ty = member.ty.clone();
                        if let Some(instance) = id.as_user_type() {
                            ty.fill_type_generics(scopes, instance);
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
            ExprData::Return(expr) => {
                let target = scopes
                    .current_function()
                    .map(|id| scopes.get_func(id).ret.clone())
                    .expect("return should only be possible inside functions");
                CheckedExpr::new(
                    TypeId::Never,
                    CheckedExprData::Return(self.type_check(scopes, *expr, &target).into()),
                )
            }
            ExprData::Yield(expr) => {
                let ScopeKind::Block(target, _) = scopes.current().kind.clone() else {
                    return self.error(Error::new("yield outside of block", span));
                };

                let span = expr.span;
                let mut expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    expr = self.type_check_checked(scopes, expr, target, span);
                    scopes.current().kind = ScopeKind::Block(Some(target.clone()), true);
                } else {
                    scopes.current().kind = ScopeKind::Block(Some(expr.ty.clone()), true);
                }

                CheckedExpr::new(TypeId::Never, CheckedExprData::Yield(expr.into()))
            }
            ExprData::Break(expr) => {
                let Some(scope) = scopes.iter().find_map(|(id, scope)| {
                    matches!(scope.kind, ScopeKind::Loop(_, _)).then_some(id)
                }) else {
                    return self.error(Error::new("break outside of loop", span));
                };

                let ScopeKind::Loop(target, _) = scopes[scope].kind.clone() else {
                    unreachable!()
                };

                let span = expr.span;
                let mut expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    expr = self.type_check_checked(scopes, expr, target, span);
                    scopes[scope].kind = ScopeKind::Loop(Some(target.clone()), true);
                } else {
                    scopes[scope].kind = ScopeKind::Loop(Some(expr.ty.clone()), true);
                }

                CheckedExpr::new(TypeId::Never, CheckedExprData::Break(expr.into()))
            }
            ExprData::Continue => {
                if scopes
                    .iter()
                    .find_map(|(id, scope)| {
                        matches!(scope.kind, ScopeKind::Loop(_, _)).then_some(id)
                    })
                    .is_none()
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
                    let (pattern, mut expr) =
                        scopes.enter(None, ScopeKind::None, false, |scopes| {
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
                let ty = self.resolve_type(scopes, &ty, false);
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
                        // TODO: these should only be allowable with an unsafe pointer type, or
                        // should be unsafe to do
                        (TypeId::Usize, TypeId::Ptr(_) | TypeId::MutPtr(_)) => {}
                        (TypeId::Ptr(_), TypeId::Ptr(_) | TypeId::Usize) => {}
                        (TypeId::MutPtr(_), TypeId::Ptr(_) | TypeId::MutPtr(_) | TypeId::Usize) => {
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
                    let func = scopes.find_func_in(
                        "unwrap",
                        scopes
                            .get_user_type(scopes.get_option_id().unwrap())
                            .body_scope,
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
                            trait_fn: false,
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
                self.resolve_path(scopes, &path.data, path.span, false),
                Some(binding),
            ),
            Pattern::Path(path) => match self.resolve_path(scopes, &path.data, path.span, false) {
                original @ Some(ResolvedPath::None(_)) => {
                    if let Some(ident) = path.data.as_identifier() {
                        return CheckedPattern::CatchAll(scopes.insert_var(
                            Variable {
                                name: ident.into(),
                                ty: scrutinee.ty.clone(),
                                is_static: false,
                                mutable: false,
                                value: None,
                            },
                            false,
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
                    scopes
                        .get_user_type(scopes.get_option_id().unwrap())
                        .body_scope,
                    binding.span,
                    false,
                    scopes.current,
                ),
                Some((mutable, binding.data)),
            ),
            Pattern::Null(span) => (
                span,
                self.resolve_path_in(
                    scopes,
                    &[("None".into(), vec![])],
                    scopes
                        .get_user_type(scopes.get_option_id().unwrap())
                        .body_scope,
                    span,
                    false,
                    scopes.current,
                ),
                None,
            ),
            Pattern::MutCatchAll(name) => {
                return CheckedPattern::CatchAll(scopes.insert_var(
                    Variable {
                        name: name.data,
                        ty: scrutinee.ty.clone(),
                        is_static: false,
                        mutable: true,
                        value: None,
                    },
                    false,
                ));
            }
        };

        let Some(path) = path else { return Default::default(); };

        let Some(ut) = scrutinee.ty
            .strip_references()
            .as_user_type()
            .filter(|ut| scopes.get_user_type(ut.id).data.is_union()) else {
            return self.error(Error::new("match scrutinee must be union or pointer to union", span));
        };

        for i in 0..scopes.get_user_type_mut(ut.id).members_mut().unwrap().len() {
            resolve_type!(
                self,
                scopes,
                scopes.get_user_type_mut(ut.id).members_mut().unwrap()[i].ty
            );
        }

        let mut variant = String::new();
        let Some(union) = path
            .as_func()
            .map(|f| scopes.get_func(f.id))
            .filter(|f| f.constructor)
            .and_then(|f| {
                variant = f.name.clone();
                f.ret.as_user_type()
            })
            .filter(|ty| ty.id == ut.id)
            .and_then(|ty| scopes.get_user_type(ty.id).data.as_union()) else {
            return self.error(Error::new("pattern does not match the scrutinee", span));
        };

        let member = union.variants.iter().find(|m| m.name == variant).unwrap();

        let mut ty = member.ty.clone();
        ty.fill_type_generics(scopes, ut);
        let ptr = scrutinee.ty.is_ptr() || scrutinee.ty.is_mut_ptr();
        let tag = union.variant_tag(&variant).unwrap();
        if let Some((mutable, binding)) = binding {
            if ptr {
                let mut s = &scrutinee.ty;
                while let TypeId::MutPtr(inner) = s {
                    s = inner;
                }

                if matches!(s, TypeId::Ptr(_)) {
                    ty = TypeId::Ptr(ty.into());
                } else {
                    ty = TypeId::MutPtr(ty.into());
                }
            }

            CheckedPattern::UnionMember {
                binding: Some(scopes.insert_var(
                    Variable {
                        name: binding,
                        ty,
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
                let Some(ut) = id.as_user_type() else {
                    return self.error(Error::new(
                        format!("cannot get member of type '{}'", id.name(scopes)),
                        span,
                    ));
                };
                let ty = scopes.get_user_type(ut.id);
                if let Some((tr, func)) = scopes.get_member_fn(&member, ty) {
                    let f = scopes.get_func(*func);
                    if !func.public && !scopes.can_access_privates(scopes.current, ty.scope) {
                        return self.error(Error::new(
                            format!(
                                "cannot access private method '{member}' of type '{}'",
                                id.name(scopes)
                            ),
                            span,
                        ));
                    }

                    if let Some(this_param) = f.params.get(0).filter(|p| p.name == THIS_PARAM) {
                        if this_param.ty.is_mut_ptr() {
                            let mut ty = &this.ty;
                            if !ty.is_ptr() && !ty.is_mut_ptr() && !this.can_addrmut(scopes) {
                                return self.error(Error::new(
                                    format!(
                                        "cannot call method '{member}' with immutable receiver"
                                    ),
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
                            self.resolve_generics_from(
                                scopes,
                                f.type_params.len(),
                                &generics,
                                span,
                                false,
                                scopes.current_id(),
                            ),
                        );
                        let (args, ret) = self.check_fn_args(
                            tr.as_ref().or(Some(ut)),
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
                                trait_fn: tr.is_some(),
                            },
                        );
                    }
                }

                self.error(Error::new(
                    format!("no method '{member}' found on type '{}'", id.name(scopes)),
                    span,
                ))
            }
            ExprData::Path(path) => {
                match self.resolve_path(scopes, &path, callee.span, false) {
                    Some(ResolvedPath::UserType(ty)) => {
                        let ut = scopes.get_user_type(ty.id);
                        let Some(st) = ut.data.as_struct() else {
                            return self.error(Error::new(
                                format!("cannot construct type '{}'", ut.name),
                                span,
                            ))
                        };

                        let (args, ret) = self.check_fn_args(
                            None,
                            &mut GenericFunc::new(*st.1, ty.generics),
                            None,
                            args,
                            target,
                            scopes,
                            span,
                        );

                        CheckedExpr::new(
                            ret,
                            CheckedExprData::Instance {
                                members: args,
                                variant: None,
                            },
                        )
                    }
                    Some(ResolvedPath::Func(mut func)) => {
                        let f = scopes.get_func(func.id);
                        let constructor = f.constructor;
                        let variant = constructor.then(|| f.name.clone());
                        let (args, ret) =
                            self.check_fn_args(None, &mut func, None, args, target, scopes, span);

                        CheckedExpr::new(
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
                                    trait_fn: false,
                                }
                            },
                        )
                    }
                    Some(ResolvedPath::Var(id)) => {
                        // closure, fn pointer
                        self.error(Error::new(
                            format!(
                                "cannot call value of type '{}'",
                                &scopes.get_var(id).ty.name(scopes)
                            ),
                            span,
                        ))
                    }
                    Some(ResolvedPath::Module(scope)) => self.error(Error::new(
                        format!(
                            "cannot call module '{}'",
                            scopes[scope].name.as_ref().unwrap()
                        ),
                        span,
                    )),
                    Some(ResolvedPath::None(err)) => self.error(err),
                    None => Default::default(),
                }
            }
            _ => {
                let expr = self.check_expr(scopes, callee, target);
                // closure, fn pointer
                self.error(Error::new(
                    format!("cannot call value of type '{}'", &expr.ty.name(scopes)),
                    span,
                ))
            }
        }
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
        target.fill_func_generics(scopes, func);

        if let Some(inst) = inst {
            target.fill_type_generics(scopes, inst);
        }

        let span = expr.span;
        let expr = self.check_expr(scopes, expr, Some(&target));
        if !func.generics.is_empty() {
            func.infer_generics(&param.ty, &expr.ty, scopes);
            target.fill_func_generics(scopes, func);
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
        for i in 0..scopes.get_func(func.id).params.len() {
            resolve_type!(self, scopes, scopes.get_func_mut(func.id).params[i].ty);
        }

        resolve_type!(self, scopes, scopes.get_func_mut(func.id).ret);

        if let Some(target) = target {
            func.infer_generics(&scopes.get_func(func.id).ret, target, scopes);
        }

        let mut result = IndexMap::with_capacity(args.len());
        let mut last_pos = 0;
        if let Some(this) = this {
            result.insert(THIS_PARAM.into(), this);
            last_pos += 1;
        }

        let variadic = scopes.get_func(func.id).variadic;
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
                        if let Some(param) = scopes
                            .get_func(func.id)
                            .params
                            .iter()
                            .find(|p| p.name == name)
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
                .get_func(func.id)
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

        // for param in params
        //     .iter()
        //     .filter(|p| !result.contains_key(&p.name))
        //     .collect::<Vec<_>>()
        // {
        //     if let Some(default) = &param.default {
        //         result.insert(param.name.clone(), default.clone());
        //     }
        // }

        if scopes.get_func(func.id).params.len() > result.len() {
            let mut missing = String::new();
            for param in scopes
                .get_func(func.id)
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
                    scopes.get_func(func.id).params.len(),
                    result.len()
                ),
                span,
            ));
        }

        let mut ret = scopes.get_func(func.id).ret.clone();
        if !func.generics.is_empty() {
            if let Some(target) = target {
                func.infer_generics(&ret, target, scopes);
            }

            ret.fill_func_generics(scopes, func);
            for (i, ty) in func.generics.iter().enumerate() {
                if ty.is_unknown() {
                    self.error::<()>(Error::new(
                        format!(
                            "cannot infer type of generic parameter '{}'",
                            scopes.get_func(func.id).type_params[i]
                        ),
                        span,
                    ));

                    continue;
                }

                if let Some(ty) = ty.as_user_type() {
                    let f = scopes.get_func(func.id);
                    let param = *scopes
                        .find_user_type_in(&f.type_params[i], f.body_scope)
                        .unwrap();
                    self.resolve_impls(scopes, param);
                    self.check_bounds(
                        scopes,
                        Some(func),
                        ty,
                        &scopes.get_user_type(param).impls,
                        inst,
                        span,
                    );
                }
            }
        }

        if let Some(inst) = inst {
            ret.fill_type_generics(scopes, inst);
        }

        (result, ret)
    }

    fn check_bounds(
        &mut self,
        scopes: &Scopes,
        func: Option<&GenericFunc>,
        ty: &GenericUserType,
        bounds: &[TypeId],
        inst: Option<&GenericUserType>,
        span: Span,
    ) {
        for bound in bounds.iter() {
            let mut bound = bound.as_user_type().unwrap().clone();
            if let Some(func) = func {
                for bty in bound.generics.iter_mut() {
                    bty.fill_func_generics(scopes, func);
                }
            }

            if let Some(inst) = inst {
                for bty in bound.generics.iter_mut() {
                    bty.fill_type_generics(scopes, inst);
                }
            }

            if !ty.implements_trait(scopes, &bound) {
                self.error::<()>(Error::new(
                    format!(
                        "type '{}' does not implement '{}'",
                        ty.name(scopes),
                        bound.name(scopes),
                    ),
                    span,
                ));
            }
        }
    }

    fn create_block(&mut self, scopes: &mut Scopes, body: Vec<Stmt>, kind: ScopeKind) -> Block {
        scopes.enter(None, kind, false, |scopes| Block {
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

    fn type_check(&mut self, scopes: &mut Scopes, expr: Expr, target: &TypeId) -> CheckedExpr {
        let span = expr.span;
        let source = self.check_expr(scopes, expr, Some(target));
        if !source.ty.coerces_to(scopes, target) {
            self.error(type_mismatch!(scopes, target, source.ty, span))
        }

        source.coerce_to(target, scopes)
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
        generics: &[TypeHint],
        fwd: Option<&TypeHint>,
        here: ScopeId,
    ) -> TypeId {
        if let Some(ty) = scopes.lang_types.get(name).copied() {
            TypeId::UserType(
                GenericUserType::new(
                    ty,
                    self.resolve_generics_from(
                        scopes,
                        generics.len(),
                        generics,
                        Span::default(),
                        fwd.is_some(),
                        here,
                    ),
                )
                .into(),
            )
        } else if let Some(hint) = fwd {
            TypeId::Unknown(Some((hint.clone(), here).into()))
        } else {
            self.error(Error::new(
                format!("missing language item: '{name}'"),
                Span::default(),
            ))
        }
    }

    fn resolve_type(&mut self, scopes: &Scopes, ty: &TypeHint, fwd: bool) -> TypeId {
        self.resolve_type_from(scopes, ty, scopes.current, fwd)
    }

    fn resolve_type_from(
        &mut self,
        scopes: &Scopes,
        hint: &TypeHint,
        here: ScopeId,
        fwd: bool,
    ) -> TypeId {
        match hint {
            TypeHint::Regular(path) => {
                return match self.resolve_path_from(scopes, &path.data, path.span, here, fwd) {
                    Some(ResolvedPath::UserType(ut)) => TypeId::UserType(ut.into()),
                    Some(ResolvedPath::Func(_)) => {
                        if fwd {
                            TypeId::Unknown(Some((hint.clone(), here).into()))
                        } else {
                            self.error(Error::new("expected type, got function", path.span))
                        }
                    }
                    Some(ResolvedPath::Var(_)) => {
                        if fwd {
                            TypeId::Unknown(Some((hint.clone(), here).into()))
                        } else {
                            self.error(Error::new("expected type, got variable", path.span))
                        }
                    }
                    Some(ResolvedPath::Module(_)) => {
                        if fwd {
                            TypeId::Unknown(Some((hint.clone(), here).into()))
                        } else {
                            self.error(Error::new("expected type, got module", path.span))
                        }
                    }
                    Some(ResolvedPath::None(err)) => {
                        let res = path.data.as_identifier().and_then(|symbol| match symbol {
                            symbol if symbol == THIS_TYPE => scopes.this_type(),
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
                            None if fwd => TypeId::Unknown(Some((hint.clone(), here).into())),
                            None => self.error(err),
                        }
                    }
                    None => TypeId::Unknown(None),
                };
            }
            TypeHint::Void => TypeId::Void,
            TypeHint::Ptr(ty) => TypeId::Ptr(self.resolve_type_from(scopes, ty, here, fwd).into()),
            TypeHint::MutPtr(ty) => {
                TypeId::MutPtr(self.resolve_type_from(scopes, ty, here, fwd).into())
            }
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                scopes
                    .this_type()
                    .map(|s| TypeId::Ptr(s.into()))
                    .expect("ICE: this outside of method")
            }
            TypeHint::MutThis => scopes
                .this_type()
                .map(|s| TypeId::MutPtr(s.into()))
                .expect("ICE: this outside of method"),
            TypeHint::Array(ty, count) => {
                let n = match Self::consteval(scopes, count, Some(&TypeId::Usize)) {
                    Ok(n) => n,
                    Err(err) => return self.error(err),
                };
                TypeId::Array((self.resolve_type_from(scopes, ty, here, fwd), n).into())
            }
            TypeHint::Option(ty) => self.resolve_lang_type(
                scopes,
                "option",
                &[(**ty).clone()],
                fwd.then_some(hint),
                here,
            ),
            TypeHint::Vec(ty) => {
                self.resolve_lang_type(scopes, "vec", &[(**ty).clone()], fwd.then_some(hint), here)
            }
            TypeHint::Map(key, value) => self.resolve_lang_type(
                scopes,
                "map",
                &[(**key).clone(), (**value).clone()],
                fwd.then_some(hint),
                here,
            ),
            TypeHint::Set(ty) => {
                self.resolve_lang_type(scopes, "set", &[(**ty).clone()], fwd.then_some(hint), here)
            }
            TypeHint::Slice(ty) => {
                self.resolve_lang_type(scopes, "span", &[(**ty).clone()], fwd.then_some(hint), here)
            }
            TypeHint::SliceMut(ty) => self.resolve_lang_type(
                scopes,
                "span_mut",
                &[(**ty).clone()],
                fwd.then_some(hint),
                here,
            ),
            TypeHint::Tuple(_) => todo!(),
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
        fwd: bool,
    ) {
        match path {
            ResolvedPath::UserType(ut) => {
                if !all {
                    scopes.current().types.insert(Vis {
                        item: ut.id,
                        public,
                    });
                } else if !fwd {
                    self.error(Error::new(
                        format!(
                            "cannot import all items of type '{}'",
                            scopes.get_user_type(ut.id).name
                        ),
                        span,
                    ))
                }
            }
            ResolvedPath::Func(func) => {
                if !all {
                    scopes.current().fns.insert(Vis {
                        item: func.id,
                        public,
                    });
                } else if !fwd {
                    self.error(Error::new(
                        format!(
                            "cannot import all items of function '{}'",
                            scopes.get_func(func.id).name
                        ),
                        span,
                    ))
                }
            }
            ResolvedPath::Var(id) => {
                if !all {
                    scopes.current().vars.insert(Vis { item: id, public });
                } else if !fwd {
                    self.error(Error::new(
                        format!(
                            "cannot import all items of variable '{}'",
                            scopes.get_var(id).name
                        ),
                        span,
                    ))
                }
            }
            ResolvedPath::Module(id) => {
                if !all {
                    let name = scopes[id].name.as_ref().unwrap().clone();
                    scopes
                        .current()
                        .children
                        .insert(name, Vis { item: id, public });
                } else {
                    for (name, id) in scopes[id].children.clone() {
                        scopes.current().children.insert(
                            name,
                            Vis {
                                item: id.item,
                                public,
                            },
                        );
                    }

                    for func in scopes[id].fns.clone() {
                        scopes.current().fns.insert(Vis {
                            item: func.item,
                            public,
                        });
                    }

                    for ty in scopes[id].types.clone() {
                        scopes.current().types.insert(Vis {
                            item: ty.item,
                            public,
                        });
                    }

                    for var in scopes[id].vars.clone() {
                        scopes.current().vars.insert(Vis {
                            item: var.item,
                            public,
                        });
                    }
                }
            }
            ResolvedPath::None(err) => {
                if !fwd {
                    self.error(err)
                }
            }
        }
    }

    fn resolve_impl(&mut self, scopes: &Scopes, path: &Located<Path>, fwd: bool) -> Option<TypeId> {
        match self.resolve_path(scopes, &path.data, path.span, fwd)? {
            ResolvedPath::UserType(ty) if scopes.get_user_type(ty.id).data.is_trait() => {
                Some(TypeId::UserType(ty.into()))
            }
            ResolvedPath::None(err) if !fwd => self.error(err),
            ResolvedPath::None(_) => Some(TypeId::Unknown(Some(
                (TypeHint::Regular(path.clone()), scopes.current).into(),
            ))),
            _ => self.error(Error::new("expected trait", path.span)),
        }
    }

    fn resolve_impls(&mut self, scopes: &mut Scopes, id: UserTypeId) {
        let mut removals = Vec::new();
        for i in 0..scopes.get_user_type(id).impls.len() {
            resolve_type!(self, scopes, scopes.get_user_type_mut(id).impls[i]);
            let imp = &scopes.get_user_type(id).impls[i];
            if !imp
                .as_user_type()
                .map_or(false, |t| scopes.get_user_type(t.id).data.is_trait())
            {
                if !imp.is_unknown() {
                    self.error(Error::new("expected trait", Span::default()))
                }
                removals.push(i);
            }
        }

        for i in removals.into_iter().rev() {
            scopes.get_user_type_mut(id).impls.remove(i);
        }
    }

    fn resolve_path(
        &mut self,
        scopes: &Scopes,
        path: &Path,
        span: Span,
        fwd: bool,
    ) -> Option<ResolvedPath> {
        self.resolve_path_from(scopes, path, span, scopes.current, fwd)
    }

    fn resolve_path_from(
        &mut self,
        scopes: &Scopes,
        path: &Path,
        span: Span,
        here: ScopeId,
        fwd: bool,
    ) -> Option<ResolvedPath> {
        match path {
            Path::Root(data) => self.resolve_path_in(scopes, data, ScopeId(0), span, fwd, here),
            Path::Super(data) => {
                if let Some(module) = scopes.module_of(
                    scopes[scopes.module_of(scopes.current_id()).unwrap()]
                        .parent
                        .unwrap(),
                ) {
                    self.resolve_path_in(scopes, data, module, span, fwd, here)
                } else {
                    self.error(Error::new("cannot use super here", span))
                }
            }
            Path::Normal(data) => {
                let (name, generics) = data.first().unwrap();
                let is_end = data.len() == 1;
                if let Some(id) = scopes.find_var_from(name, here) {
                    if is_end {
                        return Some(ResolvedPath::Var(*id));
                    }

                    if !generics.is_empty() {
                        return self.error(Error::new(
                            "variables cannot be parameterized with generics",
                            span,
                        ));
                    }

                    self.error(Error::new(format!("'{name}' is a variable"), span))
                } else if let Some(id) = scopes.find_user_type_from(name, here) {
                    if is_end {
                        let ut = GenericUserType::new(
                            *id,
                            self.resolve_generics_from(
                                scopes,
                                scopes.get_user_type(*id).type_params,
                                generics,
                                span,
                                fwd,
                                here,
                            ),
                        );
                        //self.resolve_impls(scopes, id);
                        //self.check_bounds(scopes, None, &ut, &scopes.get_user_type(id).impls, span);
                        return Some(ResolvedPath::UserType(ut));
                    }

                    self.resolve_path_in(
                        scopes,
                        &data[1..],
                        scopes.get_user_type(*id).body_scope,
                        span,
                        fwd,
                        here,
                    )
                } else if let Some(id) = scopes.find_free_fn_from(name, here) {
                    if is_end {
                        let f = scopes.get_func(*id);
                        return Some(ResolvedPath::Func(GenericFunc::new(
                            *id,
                            self.resolve_generics_from(
                                scopes,
                                f.type_params.len(),
                                generics,
                                span,
                                fwd,
                                here,
                            ),
                        )));
                    }

                    self.error(Error::new(format!("'{name}' is a function"), span))
                } else if let Some(id) = scopes.find_module_from(name, here) {
                    if is_end {
                        return Some(ResolvedPath::Module(*id));
                    }

                    if !generics.is_empty() {
                        return self.error(Error::new(
                            "modules cannot be parameterized with generics",
                            span,
                        ));
                    }

                    self.resolve_path_in(scopes, &data[1..], *id, span, fwd, here)
                } else {
                    self.resolve_path_in(scopes, data, ScopeId(0), span, fwd, here)
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
        fwd: bool,
        here: ScopeId,
    ) -> Option<ResolvedPath> {
        for (i, (name, generics)) in data.iter().enumerate() {
            let is_end = i + 1 == data.len();
            if let Some(id) = scopes.find_var_in(name, scope) {
                if !id.public && !scopes.can_access_privates(here, scope) {
                    self.error(Error::new(format!("variable '{name}' is private"), span))
                }

                if !generics.is_empty() {
                    return self.error(Error::new(
                        "variables cannot be parameterized with generics",
                        span,
                    ));
                }

                if is_end {
                    return Some(ResolvedPath::Var(*id));
                }

                return self.error(Error::new(format!("'{name}' is a variable"), span));
            } else if let Some(id) = scopes.find_user_type_in(name, scope) {
                if !id.public && !scopes.can_access_privates(here, scope) {
                    self.error(Error::new(format!("type '{name}' is private"), span))
                }

                let ty = scopes.get_user_type(*id);
                if is_end {
                    return Some(ResolvedPath::UserType(GenericUserType::new(
                        *id,
                        self.resolve_generics_from(
                            scopes,
                            ty.type_params,
                            generics,
                            span,
                            fwd,
                            here,
                        ),
                    )));
                }

                scope = ty.body_scope;
            } else if let Some(id) = scopes.find_func_in(name, scope) {
                if !id.public && !scopes.can_access_privates(here, scope) {
                    self.error(Error::new(format!("function '{name}' is private"), span))
                }

                if is_end {
                    return Some(ResolvedPath::Func(GenericFunc::new(
                        *id,
                        self.resolve_generics_from(
                            scopes,
                            scopes.get_func(*id).type_params.len(),
                            generics,
                            span,
                            fwd,
                            here,
                        ),
                    )));
                }

                return self.error(Error::new(format!("'{name}' is a function"), span));
            } else if let Some(id) = scopes.find_module_in(name, scope) {
                if !id.public && !scopes.can_access_privates(here, *id) {
                    self.error(Error::new(format!("module '{name}' is private"), span))
                }

                if !generics.is_empty() {
                    return self.error(Error::new(
                        "modules cannot be parameterized with generics",
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

    fn resolve_generics_from(
        &mut self,
        scopes: &Scopes,
        params: usize,
        args: &[TypeHint],
        span: Span,
        fwd: bool,
        here: ScopeId,
    ) -> Vec<TypeId> {
        if args.is_empty() {
            vec![TypeId::Unknown(None); params]
        } else if args.len() != params {
            self.error(Error::new(
                format!(
                    "expected {} generic arguments, received {}",
                    params,
                    args.len()
                ),
                span,
            ))
        } else {
            args.iter()
                .map(|ty| self.resolve_type_from(scopes, ty, here, fwd))
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
                false,
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

    fn typehint_for_struct(base: &Struct, span: Span) -> TypeHint {
        TypeHint::Regular(Located::new(
            Path::Normal(vec![(
                base.name.data.clone(),
                base.type_params
                    .iter()
                    .map(|(n, _)| TypeHint::Regular(Located::new(Path::from(n.clone()), span)))
                    .collect(),
            )]),
            span,
        ))
    }
}
