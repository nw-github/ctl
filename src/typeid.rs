use crate::{
    ast::{parsed::TypeHint, BinaryOp, UnaryOp},
    nearest_pow_of_two,
    sym::{ExtensionId, FunctionId, HasTypeParams, ItemId, ScopeId, Scopes, TraitId, UserTypeId},
};
use derive_more::{Constructor, Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use indexmap::{map::Entry, IndexMap, IndexSet};
use num_bigint::BigInt;

#[derive(Default, Debug, PartialEq, Eq, Clone, Deref, DerefMut)]
pub struct TypeArgs(pub IndexMap<UserTypeId, TypeId>);

impl std::hash::Hash for TypeArgs {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for entry in self.0.iter() {
            entry.hash(state);
        }
    }
}

impl TypeArgs {
    pub fn copy_args(&mut self, rhs: &TypeArgs) {
        self.extend(rhs.iter().map(|(x, y)| (*x, *y)));
    }

    pub fn copy_args_with(&mut self, types: &mut Types, rhs: &TypeArgs, map: &TypeArgs) {
        self.extend(rhs.iter().map(|(x, y)| (*x, y.with_templates(types, map))));
    }

    pub fn in_order<T: ItemId>(
        scopes: &Scopes,
        id: T,
        types: impl IntoIterator<Item = TypeId>,
    ) -> Self
    where
        T::Value: HasTypeParams,
    {
        TypeArgs(
            scopes
                .get(id)
                .get_type_params()
                .iter()
                .copied()
                .zip(types)
                .collect(),
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Constructor)]
pub struct WithTypeArgs<T> {
    pub id: T,
    pub ty_args: TypeArgs,
}

impl<T: ItemId> WithTypeArgs<T>
where
    T::Value: HasTypeParams,
{
    pub fn from_type_args(scopes: &Scopes, id: T, args: impl IntoIterator<Item = TypeId>) -> Self {
        Self::new(id, TypeArgs::in_order(scopes, id, args))
    }

    pub fn from_type_params(scopes: &Scopes, types: &mut Types, id: T) -> Self {
        Self::new(
            id,
            TypeArgs(
                scopes
                    .get(id)
                    .get_type_params()
                    .iter()
                    .map(|&id| {
                        (
                            id,
                            types.insert_ty(Type::User(GenericUserType::new(
                                id,
                                Default::default(),
                            ))),
                        )
                    })
                    .collect(),
            ),
        )
    }
}

impl<T> WithTypeArgs<T> {
    pub fn first_type_arg(&self) -> Option<TypeId> {
        self.ty_args.values().next().cloned()
    }

    pub fn infer_type_args(&mut self, types: &Types, mut src: TypeId, mut target: TypeId) {
        loop {
            match (types.get_ty(src), types.get_ty(target)) {
                (
                    Type::Ptr(gi) | Type::MutPtr(gi) | Type::RawPtr(gi),
                    Type::Ptr(ti) | Type::MutPtr(ti) | Type::RawPtr(ti),
                ) => {
                    src = *gi;
                    target = *ti;
                }
                (Type::Array(gi, _), Type::Array(ti, _)) => {
                    src = *gi;
                    target = *ti;
                }
                (Type::FnPtr(src), Type::FnPtr(target)) => {
                    for (&src, &target) in src.params.iter().zip(target.params.iter()) {
                        self.infer_type_args(types, src, target);
                    }

                    self.infer_type_args(types, src.ret, target.ret);
                    break;
                }
                (Type::User(src), target_ty) => {
                    // TODO: T => ?T
                    if let Entry::Occupied(mut entry) = self.ty_args.entry(src.id) {
                        if entry.get() != &TypeId::UNKNOWN {
                            return;
                        }

                        entry.insert(target);
                    } else if let Some(target) = target_ty.as_user() {
                        if src.id == target.id {
                            for (&src, &target) in src.ty_args.values().zip(target.ty_args.values())
                            {
                                self.infer_type_args(types, src, target);
                            }
                        }
                    }

                    break;
                }
                _ => break,
            }
        }
    }

    pub fn fill_templates(&mut self, types: &mut Types, map: &TypeArgs) {
        for ty in self.ty_args.values_mut() {
            *ty = ty.with_templates(types, map);
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
                    .map(|&id| (id, TypeId::UNKNOWN))
                    .collect(),
            ),
        )
    }

    pub fn as_fn_ptr(&self, scopes: &Scopes, types: &mut Types) -> FnPtr {
        let f = scopes.get(self.id);
        FnPtr {
            params: f
                .params
                .iter()
                .map(|p| p.ty.with_templates(types, &self.ty_args))
                .collect(),
            ret: f.ret.with_templates(types, &self.ty_args),
        }
    }
}

pub type GenericUserType = WithTypeArgs<UserTypeId>;

impl GenericUserType {
    pub fn name(&self, scopes: &Scopes, types: &Types) -> String {
        match &scopes.get(self.id).kind {
            crate::sym::UserTypeKind::AnonStruct => {
                let mut result = "struct {".to_string();
                for (i, concrete) in self.ty_args.values().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(&format!(
                        " {}: {}",
                        scopes
                            .get(self.id)
                            .members
                            .get_index(i)
                            .map(|m| &m.0[..])
                            .unwrap_or("???"),
                        concrete.name(scopes, types)
                    ));
                }
                format!("{result} }}")
            }
            crate::sym::UserTypeKind::Tuple => {
                let mut result = "(".to_string();
                for (i, concrete) in self.ty_args.values().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&concrete.name(scopes, types));
                }
                format!("{result})")
            }
            _ => {
                let is_lang_type =
                    |name: &str| scopes.lang_types.get(name).is_some_and(|&id| id == self.id);
                if is_lang_type("option") {
                    return format!("?{}", self.ty_args[0].name(scopes, types));
                } else if is_lang_type("span") {
                    return format!("[{}..]", self.ty_args[0].name(scopes, types));
                } else if is_lang_type("span_mut") {
                    return format!("[mut {}..]", self.ty_args[0].name(scopes, types));
                } else if is_lang_type("vec") {
                    return format!("[{}]", self.ty_args[0].name(scopes, types));
                } else if is_lang_type("set") {
                    return format!("{{{}}}", self.ty_args[0].name(scopes, types));
                } else if is_lang_type("map") {
                    return format!(
                        "[{}: {}]",
                        self.ty_args[0].name(scopes, types),
                        self.ty_args[1].name(scopes, types)
                    );
                }

                let mut result = scopes.get(self.id).name.data.clone();
                if !self.ty_args.is_empty() {
                    result.push('<');
                    for (i, concrete) in self.ty_args.values().enumerate() {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(&concrete.name(scopes, types));
                    }
                    result.push('>');
                }

                result
            }
        }
    }

    pub fn from_id(scopes: &Scopes, types: &mut Types, id: UserTypeId) -> Self {
        Self::from_type_params(scopes, types, id)
    }

    pub fn as_option_inner(&self, scopes: &Scopes) -> Option<TypeId> {
        scopes
            .get_option_id()
            .filter(|opt| self.id == *opt)
            .and_then(|_| self.first_type_arg())
    }

    pub fn can_omit_tag(&self, scopes: &Scopes, types: &Types) -> Option<TypeId> {
        self.as_option_inner(scopes).filter(|&inner| {
            matches!(
                types.get_ty(inner),
                Type::FnPtr(_) | Type::RawPtr(_) | Type::Ptr(_) | Type::MutPtr(_)
            )
        })
    }
}

pub type GenericTrait = WithTypeArgs<TraitId>;

pub type GenericExtension = WithTypeArgs<ExtensionId>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum CInt {
    Char,
    Short,
    Int,
    Long,
    LongLong,
}

pub struct Integer {
    pub bits: u32,
    pub signed: bool,
}

impl Integer {
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
    pub params: Vec<TypeId>,
    pub ret: TypeId,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum Type {
    #[default]
    Unknown,
    Unresolved(UnresolvedTypeId),
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
    Func(GenericFunc, FnPtr),
    FnPtr(FnPtr),
    User(GenericUserType),
    Ptr(TypeId),
    MutPtr(TypeId),
    RawPtr(TypeId),
    DynPtr(GenericTrait),
    DynMutPtr(GenericTrait),
    Array(TypeId, usize),
}

impl Type {
    pub fn as_dyn_pointee(&self) -> Option<&GenericTrait> {
        if let Type::DynMutPtr(tr) | Type::DynPtr(tr) = self {
            Some(tr)
        } else {
            None
        }
    }

    pub fn name(&self, scopes: &Scopes, types: &Types) -> String {
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
            &Type::Ptr(id) => format!("*{}", types.get_ty(id).name(scopes, types)),
            &Type::MutPtr(id) => format!("*mut {}", types.get_ty(id).name(scopes, types)),
            &Type::RawPtr(id) => format!("*raw {}", types.get_ty(id).name(scopes, types)),
            Type::DynPtr(id) => format!("*dyn {}", id.name(scopes, types)),
            Type::DynMutPtr(id) => format!("*dyn mut {}", id.name(scopes, types)),
            Type::FnPtr(f) => {
                let mut result = "fn(".to_string();
                for (i, &param) in f.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&types.get_ty(param).name(scopes, types));
                }
                format!("{result}) => {}", types.get_ty(f.ret).name(scopes, types))
            }
            Type::Func(func, fptr) => {
                let f = scopes.get(func.id);
                let mut result = format!("fn {}(", f.name.data);
                for (i, param) in fptr.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&param.name(scopes, types));
                }
                format!("{result}): {}", fptr.ret.name(scopes, types))
            }
            Type::User(ty) => ty.name(scopes, types),
            &Type::Array(ty, count) => {
                format!("[{}; {}]", types.get_ty(ty).name(scopes, types), count)
            }
            Type::Isize => "int".into(),
            Type::Usize => "uint".into(),
            Type::CInt(ty) | Type::CUint(ty) => {
                format!(
                    "c_{}{}",
                    if matches!(self, Type::CUint(_)) {
                        "u"
                    } else {
                        ""
                    },
                    match ty {
                        CInt::Char => "char",
                        CInt::Short => "short",
                        CInt::Int => "int",
                        CInt::Long => "long",
                        CInt::LongLong => "longlong",
                    }
                )
            }
            Type::CVoid => "c_void".into(),
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

    pub fn is_integral(&self) -> bool {
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

    pub fn as_integral(&self) -> Option<Integer> {
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

        Some(Integer { bits, signed })
    }

    pub fn from_int_name(name: &str, ty: bool) -> Option<Type> {
        let result = match name.chars().next()? {
            'i' => Type::Int,
            'u' => Type::Uint,
            _ => return None,
        };

        if let Ok(bits) = name[1..].parse::<u32>() {
            Some(result(bits))
        } else if ty {
            match name {
                "uint" => Some(Type::Usize),
                "int" => Some(Type::Isize),
                _ => None,
            }
        } else {
            match name {
                "u" => Some(Type::Usize),
                "i" => Some(Type::Isize),
                "icc" => Some(Type::CInt(CInt::Char)),
                "ics" => Some(Type::CInt(CInt::Short)),
                "ic" => Some(Type::CInt(CInt::Int)),
                "icl" => Some(Type::CInt(CInt::Long)),
                "icll" => Some(Type::CInt(CInt::LongLong)),
                "ucc" => Some(Type::CUint(CInt::Char)),
                "ucs" => Some(Type::CUint(CInt::Short)),
                "uci" => Some(Type::CUint(CInt::Int)),
                "ucl" => Some(Type::CUint(CInt::Long)),
                "ucll" => Some(Type::CUint(CInt::LongLong)),
                _ => None,
            }
        }
    }

    pub fn as_option_inner(&self, scopes: &Scopes) -> Option<TypeId> {
        self.as_user().and_then(|s| s.as_option_inner(scopes))
    }
}

pub struct Types {
    types: IndexSet<Type>,
    unresolved_types: Vec<(TypeHint, ScopeId)>,
}

impl Types {
    pub fn new() -> Self {
        Self {
            unresolved_types: Vec::new(),
            types: [
                Type::Unknown,
                Type::Void,
                Type::Never,
                Type::Isize,
                Type::Usize,
                Type::Bool,
                Type::Uint(8),
                Type::Char,
                Type::F32,
                Type::F64,
            ]
            .into(),
        }
    }

    pub fn insert_ty(&mut self, ty: Type) -> TypeId {
        TypeId(self.types.insert_full(ty).0)
    }

    pub fn get_ty(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    pub fn add_unresolved(&mut self, hint: TypeHint, scope: ScopeId) -> TypeId {
        let id = UnresolvedTypeId(self.unresolved_types.len());
        self.unresolved_types.push((hint, scope));
        self.insert_ty(Type::Unresolved(id))
    }

    pub fn take_unresolved(&mut self, id: UnresolvedTypeId) -> (TypeHint, ScopeId) {
        std::mem::take(&mut self.unresolved_types[id.0])
    }
}

impl Default for Types {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct UnresolvedTypeId(usize);

impl TypeId {
    pub const UNKNOWN: TypeId = TypeId(0);
    pub const VOID: TypeId = TypeId(1);
    pub const NEVER: TypeId = TypeId(2);
    pub const ISIZE: TypeId = TypeId(3);
    pub const USIZE: TypeId = TypeId(4);
    pub const BOOL: TypeId = TypeId(5);
    pub const U8: TypeId = TypeId(6);
    pub const CHAR: TypeId = TypeId(7);
    pub const F32: TypeId = TypeId(7);
    pub const F64: TypeId = TypeId(7);

    pub fn name(self, scopes: &Scopes, types: &Types) -> String {
        types.get_ty(self).name(scopes, types)
    }

    pub fn as_option_inner(self, scopes: &Scopes, types: &Types) -> Option<TypeId> {
        types.get_ty(self).as_option_inner(scopes)
    }

    pub fn strip_references(self, types: &Types) -> TypeId {
        let mut id = self;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = types.get_ty(id) {
            id = *inner;
        }
        id
    }

    pub fn strip_references_r(self, types: &Types) -> &Type {
        types.get_ty(self.strip_references(types))
    }

    pub fn strip_options(self, scopes: &Scopes, types: &Types) -> TypeId {
        let mut id = self;
        while let Some(inner) = id.as_option_inner(scopes, types) {
            id = inner;
        }
        id
    }

    pub fn supports_binary(self, types: &Types, op: BinaryOp) -> bool {
        use BinaryOp::*;
        let this = types.get_ty(self);
        match op {
            Assign => true,
            Add | AddAssign | Sub | SubAssign => this.is_numeric() || this.is_raw_ptr(),
            Mul | Div | Rem | MulAssign | DivAssign | RemAssign => this.is_numeric(),
            BitAnd | Xor | BitOr | BitAndAssign | XorAssign | BitOrAssign => {
                this.is_integral() || this.is_bool()
            }
            Shl | Shr | ShlAssign | ShrAssign => this.is_integral(),
            Gt | GtEqual | Lt | LtEqual | Cmp => {
                matches!(
                    this,
                    Type::Int(_)
                        | Type::Isize
                        | Type::Uint(_)
                        | Type::Usize
                        | Type::F32
                        | Type::F64
                        | Type::CInt(_)
                        | Type::CUint(_)
                        | Type::Char
                        | Type::RawPtr(_)
                        | Type::Bool,
                )
            }
            Equal | NotEqual => {
                matches!(
                    this,
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
            LogicalOr | LogicalAnd => this.is_bool(),
            NoneCoalesce | NoneCoalesceAssign => false,
        }
    }

    pub fn supports_unary(self, scopes: &Scopes, types: &Types, op: UnaryOp) -> bool {
        use UnaryOp::*;
        let this = types.get_ty(self);
        match op {
            Neg => {
                this.as_integral().is_some_and(|s| s.signed)
                    || matches!(this, Type::F32 | Type::F64)
            }
            PostIncrement | PostDecrement | PreIncrement | PreDecrement => {
                this.is_integral() || this.is_raw_ptr()
            }
            Not => this.is_integral() || this.is_bool(),
            Try => this.as_option_inner(scopes).is_some(),
            Plus => this.is_numeric(),
            Deref => this.is_any_ptr(),
            Addr | AddrMut | AddrRaw => true,
            Unwrap => false,
        }
    }

    pub fn as_pointee(self, types: &Types) -> Option<TypeId> {
        if let Type::Ptr(inner) | Type::MutPtr(inner) | Type::RawPtr(inner) = types.get_ty(self) {
            Some(*inner)
        } else {
            None
        }
    }

    pub fn size_and_align(self, scopes: &Scopes, types: &mut Types) -> (usize, usize) {
        use std::ffi::*;

        let sz = match types.get_ty(self) {
            Type::Int(bits) | Type::Uint(bits) => nearest_pow_of_two(*bits) / 8,
            Type::CInt(inner) | Type::CUint(inner) => match inner {
                CInt::Char => std::mem::size_of::<c_char>(),
                CInt::Short => std::mem::size_of::<c_short>(),
                CInt::Int => std::mem::size_of::<c_int>(),
                CInt::Long => std::mem::size_of::<c_long>(),
                CInt::LongLong => std::mem::size_of::<c_longlong>(),
            },
            Type::Ptr(_) | Type::MutPtr(_) | Type::RawPtr(_) => std::mem::size_of::<*const ()>(),
            Type::DynPtr(_) | Type::DynMutPtr(_) => std::mem::size_of::<*const ()>() * 2,
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

                let mut sa = SizeAndAlign::new();
                let ut = ut.clone();
                let ut_data = scopes.get(ut.id);
                if let Some(union) = ut_data.kind.as_union() {
                    if self.can_omit_tag(scopes, types).is_none() {
                        sa.next(union.tag.size_and_align(scopes, types));
                    }
                    for member in ut_data.members.values() {
                        let ty = member.ty.with_templates(types, &ut.ty_args);
                        sa.next(ty.size_and_align(scopes, types));
                    }

                    sa.next(union.variants.values().flat_map(|v| &v.0).fold(
                        (0, 1),
                        |(sz, align), &ty| {
                            let (s, a) = ty
                                .with_templates(types, &ut.ty_args)
                                .size_and_align(scopes, types);
                            (sz.max(s), align.max(a))
                        },
                    ));
                } else {
                    for member in ut_data.members.values() {
                        let ty = member.ty.with_templates(types, &ut.ty_args);
                        sa.next(ty.size_and_align(scopes, types));
                    }
                }
                sa.next((0, sa.align));
                return (sa.size, sa.align);
            }
            Type::Array(ty, len) => {
                let (s, a) = ty.size_and_align(scopes, types);
                return (s * len, a);
            }
            _ => 0,
        };

        // assume self-alignment or 1 for 0 size types
        (sz, sz.max(1))
    }

    pub fn matched_inner_type(self, types: &mut Types, ty: TypeId) -> TypeId {
        let mut id = types.get_ty(self);
        if !matches!(id, Type::Ptr(_) | Type::MutPtr(_)) {
            return ty;
        }

        while let Type::MutPtr(inner) = id {
            id = types.get_ty(*inner);
        }

        if matches!(id, Type::Ptr(_)) {
            types.insert_ty(Type::Ptr(ty))
        } else {
            types.insert_ty(Type::MutPtr(ty))
        }
    }

    pub fn can_omit_tag(self, scopes: &Scopes, types: &Types) -> Option<TypeId> {
        types
            .get_ty(self)
            .as_user()
            .and_then(|s| s.can_omit_tag(scopes, types))
    }

    pub fn with_templates(self, types: &mut Types, map: &TypeArgs) -> TypeId {
        if map.is_empty() {
            return self;
        }

        match types.get_ty(self) {
            &Type::Array(ty, len) => {
                types.insert_ty(Type::Array(ty.with_templates(types, map), len))
            }
            Type::Ptr(t) => types.insert_ty(Type::Ptr(t.with_templates(types, map))),
            Type::MutPtr(t) => types.insert_ty(Type::MutPtr(t.with_templates(types, map))),
            Type::RawPtr(t) => types.insert_ty(Type::RawPtr(t.with_templates(types, map))),
            Type::User(ut) => {
                if let Some(&ty) = map.get(&ut.id) {
                    if ty != TypeId::UNKNOWN {
                        ty
                    } else {
                        self
                    }
                } else if !ut.ty_args.is_empty() {
                    let mut ut = ut.clone();
                    ut.fill_templates(types, map);
                    types.insert_ty(Type::User(ut))
                } else {
                    self
                }
            }
            Type::FnPtr(f) => types.insert_ty(Type::FnPtr(FnPtr {
                params: f
                    .params
                    .iter()
                    .map(|p| p.with_templates(types, map))
                    .collect(),
                ret: f.ret.with_templates(types, map),
            })),
            Type::DynPtr(tr) => {
                let mut tr = tr.clone();
                tr.fill_templates(types, map);
                types.insert_ty(Type::DynPtr(tr))
            }
            Type::DynMutPtr(tr) => {
                let mut tr = tr.clone();
                tr.fill_templates(types, map);
                types.insert_ty(Type::DynMutPtr(tr))
            }
            _ => self,
        }
    }

    pub fn with_ut_templates(self, types: &mut Types, id: TypeId) -> TypeId {
        if let Some(ut) = types.get_ty(id).as_user() {
            self.with_templates(types, &ut.ty_args)
        } else {
            self
        }
    }
}
