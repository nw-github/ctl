use crate::{
    ast::{parsed::TypeHint, BinaryOp},
    nearest_pow_of_two,
    sym::{FunctionId, HasTypeParams, ItemId, ScopeId, Scopes, UserTypeId},
};
use derive_more::{Constructor, Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use indexmap::{map::Entry, IndexMap};
use num_bigint::BigInt;

#[derive(Default, Debug, PartialEq, Eq, Clone, Deref, DerefMut)]
pub struct TypeArgs(pub IndexMap<UserTypeId, Type>);

impl std::hash::Hash for TypeArgs {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for entry in self.0.iter() {
            entry.hash(state);
        }
    }
}

impl TypeArgs {
    pub fn copy_args(&mut self, rhs: &TypeArgs) {
        self.extend(rhs.iter().map(|(x, y)| (*x, y.clone())));
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Constructor)]
pub struct WithTypeArgs<T> {
    pub id: T,
    pub ty_args: TypeArgs,
}

impl<T: ItemId + Clone + Copy> WithTypeArgs<T>
where
    T::Value: HasTypeParams,
{
    pub fn first_type_arg(&self) -> Option<&Type> {
        self.ty_args.values().next()
    }

    pub fn from_type_args(scopes: &Scopes, id: T, types: impl IntoIterator<Item = Type>) -> Self {
        Self::new(
            id,
            TypeArgs(
                scopes
                    .get(id)
                    .get_type_params()
                    .iter()
                    .copied()
                    .zip(types)
                    .collect(),
            ),
        )
    }
}

pub type GenericFunc = WithTypeArgs<FunctionId>;

impl GenericFunc {
    pub fn infer_type_args(&mut self, mut src: &Type, mut target: &Type) {
        loop {
            match (src, target) {
                (Type::Ptr(gi), Type::Ptr(ti)) => {
                    src = gi;
                    target = ti;
                }
                (Type::Ptr(gi) | Type::MutPtr(gi), Type::MutPtr(ti)) => {
                    src = gi;
                    target = ti;
                }
                (Type::Array(gi), Type::Array(ti)) => {
                    src = &gi.0;
                    target = &ti.0;
                }
                (Type::FnPtr(src), Type::FnPtr(target)) => {
                    for (src, target) in src.params.iter().zip(target.params.iter()) {
                        self.infer_type_args(src, target);
                    }

                    self.infer_type_args(&src.ret, &target.ret);
                    break;
                }
                (Type::User(src), target) => {
                    // TODO: T => ?T
                    if let Entry::Occupied(mut entry) = self.ty_args.entry(src.id) {
                        if !entry.get().is_unknown() {
                            return;
                        }

                        entry.insert(target.clone());
                    } else if let Some(target) = target.as_user() {
                        if src.id == target.id {
                            for (src, target) in src.ty_args.values().zip(target.ty_args.values()) {
                                self.infer_type_args(src, target);
                            }
                        }
                    }

                    break;
                }
                _ => break,
            }
        }
    }

    pub fn from_id(scopes: &Scopes, id: FunctionId) -> Self {
        Self::new(
            id,
            TypeArgs(
                scopes
                    .get(id)
                    .type_params
                    .iter()
                    .map(|&id| (id, Type::Unknown))
                    .collect(),
            ),
        )
    }
}

pub type GenericUserType = WithTypeArgs<UserTypeId>;

impl GenericUserType {
    pub fn name(&self, scopes: &Scopes) -> String {
        let mut result = scopes.get(self.id).name.data.clone();
        if !self.ty_args.is_empty() {
            result.push('<');
            for (i, concrete) in self.ty_args.values().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&concrete.name(scopes));
            }
            result.push('>');
        }

        result
    }

    pub fn from_id(scopes: &Scopes, id: UserTypeId) -> Self {
        Self::new(
            id,
            TypeArgs(
                scopes
                    .get(id)
                    .type_params
                    .iter()
                    .map(|&id| (id, Type::User(Self::new(id, Default::default()).into())))
                    .collect(),
            ),
        )
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
    pub bits: u32,
    pub signed: bool,
}

impl IntStats {
    pub fn min(&self) -> BigInt {
        if self.signed {
            -(BigInt::from(1) << (self.bits - 1))
        } else {
            BigInt::default()
        }
    }

    pub fn max(&self) -> BigInt {
        if self.signed {
            (BigInt::from(1) << (self.bits - 1)) - 1
        } else {
            (BigInt::from(1) << self.bits) - 1u8
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnPtr {
    pub params: Vec<Type>,
    pub ret: Type,
}

#[derive(Debug, Default, Clone, EnumAsInner)]
pub enum Type {
    #[default]
    Unknown,
    Unresolved(Box<(TypeHint, ScopeId)>),
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
    User(Box<GenericUserType>),
    Ptr(Box<Type>),
    MutPtr(Box<Type>),
    Array(Box<(Type, usize)>),
    TraitSelf,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Uint(l0), Self::Uint(r0)) => l0 == r0,
            (Self::CInt(l0), Self::CInt(r0)) => l0 == r0,
            (Self::CUint(l0), Self::CUint(r0)) => l0 == r0,
            (Self::User(l0), Self::User(r0)) => l0 == r0,
            (Self::Ptr(l0), Self::Ptr(r0)) => l0 == r0,
            (Self::MutPtr(l0), Self::MutPtr(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::FnPtr(l0), Self::FnPtr(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(l0) => l0.hash(state),
            Self::Uint(l0) => l0.hash(state),
            Self::CInt(l0) => l0.hash(state),
            Self::CUint(l0) => l0.hash(state),
            Self::FnPtr(l0) => l0.hash(state),
            Self::User(l0) => l0.hash(state),
            Self::Ptr(l0) => l0.hash(state),
            Self::MutPtr(l0) => l0.hash(state),
            Self::Array(l0) => l0.hash(state),
            _ => core::mem::discriminant(self).hash(state),
        }
    }
}

impl Eq for Type {}

impl Type {
    pub fn discriminant_for(max: usize) -> Type {
        Type::Uint((max as f64).log2().ceil() as u32)
    }

    pub fn supports_binop(&self, _scopes: &Scopes, op: BinaryOp) -> bool {
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
                    Type::Int(_)
                        | Type::Isize
                        | Type::Uint(_)
                        | Type::Usize
                        | Type::F32
                        | Type::F64
                        | Type::CInt(_)
                        | Type::CUint(_)
                )
            }
            BinaryOp::And | BinaryOp::Xor | BinaryOp::Or | BinaryOp::Shl | BinaryOp::Shr => {
                matches!(
                    self,
                    Type::Int(_)
                        | Type::Uint(_)
                        | Type::Isize
                        | Type::Usize
                        | Type::CInt(_)
                        | Type::CUint(_)
                )
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                matches!(
                    self,
                    Type::Int(_)
                        | Type::Isize
                        | Type::Uint(_)
                        | Type::Usize
                        | Type::F32
                        | Type::F64
                        | Type::Bool // FIXME: option<T> should be comparable with T without coercion
                        | Type::CInt(_)
                        | Type::CUint(_)
                        | Type::Char
                )
            }
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => matches!(self, Type::Bool),
            BinaryOp::NoneCoalesce => todo!(),
        }
    }

    pub fn strip_references(&self) -> &Type {
        let mut id = self;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = id {
            id = inner;
        }
        id
    }

    pub fn strip_options(&self, scopes: &Scopes) -> &Type {
        let mut id = self;
        while let Some(inner) = id.as_option_inner(scopes) {
            id = inner;
        }
        id
    }

    pub fn matched_inner_type(&self, ty: Type) -> Type {
        if !(self.is_ptr() || self.is_mut_ptr()) {
            return ty;
        }

        let mut id = self;
        while let Type::MutPtr(inner) = id {
            id = inner;
        }

        if matches!(id, Type::Ptr(_)) {
            Type::Ptr(ty.into())
        } else {
            Type::MutPtr(ty.into())
        }
    }

    pub fn fill_this(&mut self, this: &Type) {
        let mut src = self;
        loop {
            match src {
                Type::Array(t) => src = &mut t.0,
                Type::Ptr(t) | Type::MutPtr(t) => src = t,
                Type::User(ty) => {
                    for ty in ty.ty_args.values_mut() {
                        ty.fill_this(this);
                    }

                    break;
                }
                Type::TraitSelf => {
                    *src = this.clone();
                    break;
                }
                Type::FnPtr(f) => {
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

    pub fn fill_templates(&mut self, map: &TypeArgs) {
        if map.is_empty() {
            return;
        }

        let mut src = self;
        loop {
            match src {
                Type::Array(t) => src = &mut t.0,
                Type::Ptr(t) | Type::MutPtr(t) => src = t,
                Type::User(ut) => {
                    if let Some(ty) = map.get(&ut.id) {
                        if !ty.is_unknown() {
                            *src = ty.clone();
                        }
                    } else if !ut.ty_args.is_empty() {
                        for ty in ut.ty_args.values_mut() {
                            ty.fill_templates(map);
                        }
                    }
                    break;
                }
                Type::FnPtr(f) => {
                    for ty in f.params.iter_mut() {
                        ty.fill_templates(map);
                    }

                    f.ret.fill_templates(map);
                    break;
                }
                _ => break,
            }
        }
    }

    pub fn name(&self, scopes: &Scopes) -> String {
        match self {
            Type::Unknown => "{unknown}".into(),
            // for debug purposes, ideally this should never be visible
            Type::Unresolved(_) => "{unresolved}".into(),
            Type::Void => "void".into(),
            Type::Never => "never".into(),
            Type::Int(bits) => format!("i{bits}"),
            Type::Uint(bits) => format!("u{bits}"),
            Type::F32 => "f32".into(),
            Type::F64 => "f64".into(),
            Type::Bool => "bool".into(),
            Type::Char => "char".into(),
            Type::Ptr(id) => format!("*{}", id.name(scopes)),
            Type::MutPtr(id) => format!("*mut {}", id.name(scopes)),
            Type::FnPtr(f) => {
                let mut result = "fn(".to_string();
                for (i, param) in f.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&param.name(scopes));
                }
                format!("{result}) -> {}", f.ret.name(scopes))
            }
            Type::User(ty) => ty.name(scopes),
            Type::Array(inner) => format!("[{}; {}]", inner.0.name(scopes), inner.1),
            Type::Isize => "int".into(),
            Type::Usize => "uint".into(),
            Type::CInt(ty) | Type::CUint(ty) => {
                let name = match ty {
                    CInt::Char => "char",
                    CInt::Short => "short",
                    CInt::Int => "int",
                    CInt::Long => "long",
                    CInt::LongLong => "longlong",
                };
                format!(
                    "c_{}{name}",
                    if matches!(self, Type::CUint(_)) {
                        "u"
                    } else {
                        ""
                    }
                )
            }
            Type::CVoid => "c_void".into(),
            Type::TraitSelf => "This".into(),
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Int(_)
                | Type::Uint(_)
                | Type::F32
                | Type::F64
                | Type::Isize
                | Type::Usize
                | Type::CInt(_)
                | Type::CUint(_)
        )
    }

    pub fn is_any_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_) | Type::MutPtr(_))
    }

    pub fn is_any_int(&self) -> bool {
        matches!(
            self,
            Type::Int(_)
                | Type::Uint(_)
                | Type::CInt(_)
                | Type::CUint(_)
                | Type::Isize
                | Type::Usize
        )
    }

    pub fn integer_stats(&self) -> Option<IntStats> {
        use std::ffi::*;

        let (bits, signed) = match self {
            Type::Int(bits) | Type::Uint(bits) => (*bits, matches!(self, Type::Int(_))),
            Type::CInt(cint) | Type::CUint(cint) => {
                let bytes = match cint {
                    CInt::Char => std::mem::size_of::<c_char>(),
                    CInt::Short => std::mem::size_of::<c_short>(),
                    CInt::Int => std::mem::size_of::<c_int>(),
                    CInt::Long => std::mem::size_of::<c_long>(),
                    CInt::LongLong => std::mem::size_of::<c_longlong>(),
                };
                (bytes as u32 * 8, matches!(self, Type::CInt(_)))
            }
            Type::Isize => (std::mem::size_of::<isize>() as u32 * 8, true),
            Type::Usize => (std::mem::size_of::<usize>() as u32 * 8, false),
            _ => return None,
        };

        Some(IntStats { bits, signed })
    }

    pub fn indirection(&self) -> usize {
        let mut count = 0;
        let mut id = self;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = id {
            id = inner;
            count += 1;
        }
        count
    }

    pub fn may_ptr_coerce(&self, target: &Type) -> bool {
        match (self, target) {
            (Type::MutPtr(ty), Type::Ptr(target)) if ty == target => true,
            (Type::MutPtr(ty), Type::MutPtr(target) | Type::Ptr(target)) => {
                ty.may_ptr_coerce(target)
            }
            (Type::Ptr(ty), Type::Ptr(target)) => ty.may_ptr_coerce(target),
            _ => false,
        }
    }

    pub fn from_int_name(name: &str, ty: bool) -> Option<Type> {
        let mut chars = name.chars();
        let (i, result): (_, fn(u32) -> Type) = match chars.next()? {
            'i' => (true, Type::Int),
            'u' => (false, Type::Uint),
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
                "u" if !ty => Some(Type::Usize),
                "i" if !ty => Some(Type::Isize),
                "uint" if ty => Some(Type::Usize),
                "int" if ty => Some(Type::Isize),
                _ => None,
            },
        }
    }

    pub fn as_option_inner<'a>(&'a self, scopes: &Scopes) -> Option<&'a Type> {
        scopes.get_option_id().and_then(|opt| {
            self.as_user()
                .filter(|ut| ut.id == opt)
                .and_then(|ut| ut.first_type_arg())
        })
    }

    pub fn with_templates(&self, args: &TypeArgs) -> Type {
        let mut ty = self.clone();
        ty.fill_templates(args);
        ty
    }

    pub fn size_of(&self, scopes: &Scopes) -> usize {
        use std::ffi::*;

        match self {
            Type::Int(bits) | Type::Uint(bits) => nearest_pow_of_two(*bits),
            Type::CInt(inner) | Type::CUint(inner) => match inner {
                CInt::Char => std::mem::size_of::<c_char>(),
                CInt::Short => std::mem::size_of::<c_short>(),
                CInt::Int => std::mem::size_of::<c_int>(),
                CInt::Long => std::mem::size_of::<c_long>(),
                CInt::LongLong => std::mem::size_of::<c_longlong>(),
            },
            Type::Ptr(_) | Type::MutPtr(_) => std::mem::size_of::<*const ()>(),
            Type::Isize => std::mem::size_of::<isize>(),
            Type::Usize => std::mem::size_of::<usize>(),
            Type::F32 => 4,
            Type::F64 => 8,
            Type::Bool => 1,
            Type::Char => 4,
            Type::FnPtr(_) => std::mem::size_of::<fn()>(),
            Type::User(ut) => {
                // TODO: padding and alignment
                let item = scopes.get(ut.id);
                match &item.data {
                    crate::sym::UserTypeData::Struct { members, .. } => members
                        .iter()
                        .map(|m| m.ty.with_templates(&ut.ty_args).size_of(scopes))
                        .sum(),
                    crate::sym::UserTypeData::Union(union) => {
                        let shared: usize = union
                            .variants
                            .iter()
                            .filter(|v| v.shared)
                            .map(|m| m.ty.with_templates(&ut.ty_args).size_of(scopes))
                            .sum();
                        let unshared = union
                            .variants
                            .iter()
                            .filter(|v| !v.shared)
                            .map(|m| m.ty.with_templates(&ut.ty_args).size_of(scopes))
                            .max()
                            .unwrap_or(0);
                        shared + unshared
                    }
                    _ => 0,
                }
            }
            Type::Array(data) => data.0.size_of(scopes) * data.1,
            Type::Void | Type::CVoid | Type::Never => 0,
            Type::Unknown => todo!(),
            Type::Unresolved(_) => todo!(),
            Type::TraitSelf => todo!(),
        }
    }
}
