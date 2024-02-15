use crate::{
    ast::{parsed::TypeHint, BinaryOp},
    nearest_pow_of_two,
    sym::{FunctionId, HasTypeParams, ItemId, ScopeId, Scopes, TraitId, UserTypeId},
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

impl<T> WithTypeArgs<T> {
    pub fn first_type_arg(&self) -> Option<&Type> {
        self.ty_args.values().next()
    }

    pub fn infer_type_args(&mut self, mut src: &Type, mut target: &Type) {
        loop {
            match (src, target) {
                (
                    Type::Ptr(gi) | Type::MutPtr(gi) | Type::RawPtr(gi),
                    Type::Ptr(ti) | Type::MutPtr(ti) | Type::RawPtr(ti),
                ) => {
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
}

pub type GenericFunc = WithTypeArgs<FunctionId>;

impl GenericFunc {
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

    pub fn as_fn_ptr(&self, scopes: &Scopes) -> FnPtr {
        let f = scopes.get(self.id);
        FnPtr {
            params: f
                .params
                .iter()
                .map(|p| p.ty.with_templates(&self.ty_args))
                .collect(),
            ret: f.ret.with_templates(&self.ty_args),
        }
    }
}

pub type GenericUserType = WithTypeArgs<UserTypeId>;

impl GenericUserType {
    pub fn name(&self, scopes: &Scopes) -> String {
        match &scopes.get(self.id).data {
            crate::sym::UserTypeData::AnonStruct => {
                let mut result = "struct {".to_string();
                for (i, concrete) in self.ty_args.values().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(&format!(
                        " {}: {}",
                        scopes.get(self.id).members.get_index(i).unwrap().0,
                        concrete.name(scopes)
                    ));
                }
                format!("{result} }}")
            }
            crate::sym::UserTypeData::Tuple => {
                let mut result = "(".to_string();
                for (i, concrete) in self.ty_args.values().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&concrete.name(scopes));
                }
                format!("{result})")
            }
            _ => {
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
        }
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

pub type GenericTrait = WithTypeArgs<TraitId>;

impl GenericTrait {
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
    RawPtr(Box<Type>),
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
            (Self::RawPtr(l0), Self::RawPtr(r0)) => l0 == r0,
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
            Self::RawPtr(l0) => l0.hash(state),
            Self::Array(l0) => l0.hash(state),
            _ => {}
        }
        core::mem::discriminant(self).hash(state);
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
                        | Type::RawPtr(_)
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
                Type::Ptr(t) | Type::MutPtr(t) | Type::RawPtr(t) => src = t,
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
                Type::Ptr(t) | Type::MutPtr(t) | Type::RawPtr(t) => src = t,
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
            Type::RawPtr(id) => format!("*raw {}", id.name(scopes)),
            Type::FnPtr(f) => {
                let mut result = "fn(".to_string();
                for (i, param) in f.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&param.name(scopes));
                }
                format!("{result}) => {}", f.ret.name(scopes))
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
        matches!(self, Type::Ptr(_) | Type::MutPtr(_) | Type::RawPtr(_))
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

    pub fn size_and_align(&self, scopes: &Scopes) -> (usize, usize) {
        use std::ffi::*;

        let sz = match self {
            Type::Int(bits) | Type::Uint(bits) => nearest_pow_of_two(*bits) / 8,
            Type::CInt(inner) | Type::CUint(inner) => match inner {
                CInt::Char => std::mem::size_of::<c_char>(),
                CInt::Short => std::mem::size_of::<c_short>(),
                CInt::Int => std::mem::size_of::<c_int>(),
                CInt::Long => std::mem::size_of::<c_long>(),
                CInt::LongLong => std::mem::size_of::<c_longlong>(),
            },
            Type::Ptr(_) | Type::MutPtr(_) | Type::RawPtr(_) => std::mem::size_of::<*const ()>(),
            Type::Isize => std::mem::size_of::<isize>(),
            Type::Usize => std::mem::size_of::<usize>(),
            Type::F32 => std::mem::size_of::<f32>(),
            Type::F64 => std::mem::size_of::<f64>(),
            Type::Bool => std::mem::size_of::<bool>(),
            Type::Char => std::mem::size_of::<char>(),
            Type::FnPtr(_) => std::mem::size_of::<fn()>(),
            Type::User(ut) => {
                struct SizeAndAlign {
                    size: usize,
                    align: usize,
                }

                impl SizeAndAlign {
                    fn new() -> Self {
                        Self { size: 0, align: 1 }
                    }

                    fn next(&mut self, (s, a): (usize, usize)) {
                        self.size += (a - self.size % a) % a + s;
                        self.align = self.align.max(a);
                    }
                }

                let ty = scopes.get(ut.id);
                let mut sa = SizeAndAlign::new();
                if let Some(union) = ty.data.as_union() {
                    if self.can_omit_tag(scopes).is_none() {
                        sa.next(union.tag_type().size_and_align(scopes));
                    }
                    for member in ty.members.values() {
                        sa.next(member.ty.with_templates(&ut.ty_args).size_and_align(scopes));
                    }

                    sa.next(union.values().flatten().fold((0, 1), |(sz, align), ty| {
                        let (s, a) = ty.with_templates(&ut.ty_args).size_and_align(scopes);
                        (sz.max(s), align.max(a))
                    }));
                } else {
                    let mut sa = SizeAndAlign::new();
                    for member in ty.members.values() {
                        sa.next(member.ty.with_templates(&ut.ty_args).size_and_align(scopes));
                    }
                }
                return (sa.size, sa.align);
            }
            Type::Array(data) => {
                let (s, a) = data.0.size_and_align(scopes);
                return (s * data.1, a);
            }
            _ => 0,
        };

        // assume self-alignment or 1 for 0 size types
        (sz, sz.max(1))
    }

    pub fn can_omit_tag(&self, scopes: &Scopes) -> Option<&Type> {
        self.as_option_inner(scopes).filter(|inner| {
            matches!(
                inner,
                Type::FnPtr(_) | Type::RawPtr(_) | Type::Ptr(_) | Type::MutPtr(_)
            )
        })
    }
}
