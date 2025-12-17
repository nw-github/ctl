use std::{fmt::Display, ops::Index};

use crate::{
    Located,
    ast::{BinaryOp, UnaryOp, parsed::TypeHint},
    comptime_int::ComptimeInt,
    hash::IndexMap,
    intern::Strings,
    nearest_pow_of_two,
    project::Project,
    sym::{
        ExtensionId, FunctionId, HasTypeParams, ItemId, ScopeId, Scopes, TraitId, UserTypeId,
        UserTypeKind,
    },
};
use derive_more::{Constructor, Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use indexmap::{IndexSet, map::Entry};

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
        TypeArgs(scopes.get(id).get_type_params().iter().copied().zip(types).collect())
    }

    pub fn unknown(v: &impl HasTypeParams) -> Self {
        TypeArgs(v.get_type_params().iter().map(|&t| (t, TypeId::UNKNOWN)).collect())
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
                        (id, types.insert(Type::User(GenericUserType::new(id, Default::default()))))
                    })
                    .collect(),
            ),
        )
    }

    pub fn from_id_unknown(scopes: &Scopes, id: T) -> Self {
        Self::from_type_args(scopes, id, std::iter::repeat(TypeId::UNKNOWN))
    }
}

impl<T> WithTypeArgs<T> {
    pub fn first_type_arg(&self) -> Option<TypeId> {
        self.ty_args.values().next().cloned()
    }

    pub fn infer_type_args(&mut self, types: &Types, mut src: TypeId, mut target: TypeId) {
        loop {
            match (&types[src], &types[target]) {
                (
                    Type::Ptr(gi) | Type::MutPtr(gi) | Type::RawPtr(gi) | Type::RawMutPtr(gi),
                    Type::Ptr(ti) | Type::MutPtr(ti) | Type::RawPtr(ti) | Type::RawMutPtr(ti),
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
                    } else if let Type::User(target) = target_ty
                        && src.id == target.id
                    {
                        for (&src, &target) in src.ty_args.values().zip(target.ty_args.values()) {
                            self.infer_type_args(types, src, target);
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

pub type GenericFn = WithTypeArgs<FunctionId>;

impl GenericFn {
    pub fn from_id(scopes: &Scopes, id: FunctionId) -> Self {
        Self::from_id_unknown(scopes, id)
    }

    pub fn as_fn_ptr(&self, scopes: &Scopes, types: &mut Types) -> FnPtr {
        let f = scopes.get(self.id);
        FnPtr {
            is_extern: f.is_extern,
            params: f.params.iter().map(|p| p.ty.with_templates(types, &self.ty_args)).collect(),
            ret: f.ret.with_templates(types, &self.ty_args),
        }
    }
}

pub type GenericUserType = WithTypeArgs<UserTypeId>;

impl GenericUserType {
    pub fn name(&self, scopes: &Scopes, types: &mut Types, strings: &Strings) -> String {
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
                            .map(|m| strings.resolve(m.0))
                            .unwrap_or("???"),
                        concrete.name(scopes, types, strings)
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
                    result.push_str(&concrete.name(scopes, types, strings));
                }
                format!("{result})")
            }
            _ => {
                let is_lang_type = |name: &str| {
                    let key = strings.get(name);
                    key.and_then(|key| scopes.lang_types.get(&key)).is_some_and(|&id| id == self.id)
                };
                if is_lang_type("option") {
                    return format!("?{}", self.ty_args[0].name(scopes, types, strings));
                } else if is_lang_type("span") {
                    return format!("[{}..]", self.ty_args[0].name(scopes, types, strings));
                } else if is_lang_type("span_mut") {
                    return format!("[mut {}..]", self.ty_args[0].name(scopes, types, strings));
                } else if is_lang_type("vec") {
                    return format!("[{}]", self.ty_args[0].name(scopes, types, strings));
                } else if is_lang_type("set") {
                    return format!("#[{}]", self.ty_args[0].name(scopes, types, strings));
                } else if is_lang_type("map") {
                    return format!(
                        "[{}: {}]",
                        self.ty_args[0].name(scopes, types, strings),
                        self.ty_args[1].name(scopes, types, strings)
                    );
                }

                let mut result = strings.resolve(&scopes.get(self.id).name.data).to_string();
                if !self.ty_args.is_empty() {
                    result.push('<');
                    for (i, concrete) in self.ty_args.values().enumerate() {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(&concrete.name(scopes, types, strings));
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
        scopes.get_option_id().filter(|opt| self.id == *opt).and_then(|_| self.first_type_arg())
    }

    pub fn can_omit_tag(&self, scopes: &Scopes, types: &Types) -> Option<TypeId> {
        self.as_option_inner(scopes).filter(|&inner| {
            matches!(
                types[inner],
                Type::FnPtr(_)
                    | Type::RawPtr(_)
                    | Type::RawMutPtr(_)
                    | Type::Ptr(_)
                    | Type::MutPtr(_)
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

impl CInt {
    pub const fn size(&self) -> usize {
        use std::ffi::*;
        match self {
            CInt::Char => std::mem::size_of::<c_char>(),
            CInt::Short => std::mem::size_of::<c_short>(),
            CInt::Int => std::mem::size_of::<c_int>(),
            CInt::Long => std::mem::size_of::<c_long>(),
            CInt::LongLong => std::mem::size_of::<c_longlong>(),
        }
    }
}

impl Display for CInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CInt::Char => write!(f, "char"),
            CInt::Short => write!(f, "short"),
            CInt::Int => write!(f, "int"),
            CInt::Long => write!(f, "long"),
            CInt::LongLong if f.alternate() => write!(f, "longlong"),
            CInt::LongLong => write!(f, "long long"),
        }
    }
}

#[derive(Debug)]
pub struct Integer {
    pub bits: u32,
    pub signed: bool,
    pub char: bool,
}

impl Integer {
    pub fn min(&self) -> ComptimeInt {
        if !self.signed || self.bits == 0 {
            ComptimeInt::new(0)
        } else {
            -(ComptimeInt::new(1) << (self.bits - 1))
        }
    }

    pub fn max(&self) -> ComptimeInt {
        if self.bits == 0 {
            ComptimeInt::new(0)
        } else if self.char {
            ComptimeInt::from(char::MAX as u32)
        } else {
            (ComptimeInt::new(1) << (self.bits - self.signed as u32)) - 1
        }
    }

    pub const fn from_cint(cint: CInt, signed: bool) -> Self {
        Self { bits: cint.size() as u32 * 8, signed, char: false }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnPtr {
    pub is_extern: bool,
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
    Isize,
    Usize,
    F32,
    F64,
    Bool,
    Char,
    Fn(GenericFn),
    FnPtr(FnPtr),
    User(GenericUserType),
    Ptr(TypeId),
    MutPtr(TypeId),
    RawPtr(TypeId),
    RawMutPtr(TypeId),
    DynPtr(GenericTrait),
    DynMutPtr(GenericTrait),
    Array(TypeId, usize),
}

impl Type {
    pub fn as_dyn_pointee(&self) -> Option<&GenericTrait> {
        if let Type::DynMutPtr(tr) | Type::DynPtr(tr) = self { Some(tr) } else { None }
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
        matches!(self, Type::Ptr(_) | Type::MutPtr(_) | Type::RawPtr(_) | Type::RawMutPtr(_))
    }

    pub fn is_safe_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_) | Type::MutPtr(_))
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

    pub fn as_integral(&self, extra: bool) -> Option<Integer> {
        let (bits, signed, char) = match self {
            &Type::Int(bits) => (bits, true, false),
            &Type::Uint(bits) => (bits, false, false),
            &Type::CInt(cint) => return Some(Integer::from_cint(cint, true)),
            &Type::CUint(cint) => return Some(Integer::from_cint(cint, false)),
            Type::Isize => (std::mem::size_of::<isize>() as u32 * 8, true, false),
            Type::Usize => (std::mem::size_of::<usize>() as u32 * 8, false, false),
            Type::Char if extra => (32 - (char::MAX as u32).leading_zeros(), false, true),
            Type::Bool if extra => (1, false, false),
            _ => return None,
        };

        Some(Integer { bits, signed, char })
    }

    pub fn from_int_name(name: &str, ty: bool) -> Option<Type> {
        let result = match name.chars().next()? {
            'i' => Type::Int,
            'u' => Type::Uint,
            _ => return None,
        };

        if let Ok(bits @ 0..=65535) = name[1..].parse::<u32>() {
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
    unresolved_types: Vec<(Located<TypeHint>, ScopeId)>,
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
                Type::Uint(32),
                Type::Char,
                Type::F32,
                Type::F64,
                Type::Uint(16),
            ]
            .into(),
        }
    }

    pub fn insert(&mut self, ty: Type) -> TypeId {
        TypeId(self.types.insert_full(ty).0)
    }

    pub fn get(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    pub fn add_unresolved(&mut self, hint: Located<TypeHint>, scope: ScopeId) -> TypeId {
        let id = UnresolvedTypeId(self.unresolved_types.len());
        self.unresolved_types.push((hint, scope));
        self.insert(Type::Unresolved(id))
    }

    pub fn take_unresolved(&mut self, id: UnresolvedTypeId) -> (Located<TypeHint>, ScopeId) {
        std::mem::take(&mut self.unresolved_types[id.0])
    }
}

impl Default for Types {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<TypeId> for Types {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        self.get(index)
    }
}

// TODO
pub const MAX_ALIGN: usize = core::mem::align_of::<usize>();

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeId(usize);

impl TypeId {
    pub fn as_raw(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct UnresolvedTypeId(usize);

#[derive(Debug, Clone, Copy, EnumAsInner)]
pub enum BitSizeResult {
    Size(u32),
    Tag(TypeId, u32),
    NonEnum,
    Bad,
}

impl TypeId {
    pub const UNKNOWN: TypeId = TypeId(0);
    pub const VOID: TypeId = TypeId(1);
    pub const NEVER: TypeId = TypeId(2);
    pub const ISIZE: TypeId = TypeId(3);
    pub const USIZE: TypeId = TypeId(4);
    pub const BOOL: TypeId = TypeId(5);
    pub const U8: TypeId = TypeId(6);
    pub const U32: TypeId = TypeId(7);
    pub const CHAR: TypeId = TypeId(8);
    pub const F32: TypeId = TypeId(9);
    pub const F64: TypeId = TypeId(10);
    pub const U16: TypeId = TypeId(11);

    pub fn name(self, scopes: &Scopes, types: &mut Types, strings: &Strings) -> String {
        match &types[self] {
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
            &Type::Ptr(id) => format!("*{}", id.name(scopes, types, strings)),
            &Type::MutPtr(id) => format!("*mut {}", id.name(scopes, types, strings)),
            &Type::RawPtr(id) => format!("^{}", id.name(scopes, types, strings)),
            &Type::RawMutPtr(id) => format!("^mut {}", id.name(scopes, types, strings)),
            Type::DynPtr(id) => format!("*dyn {}", id.clone().name(scopes, types, strings)),
            Type::DynMutPtr(id) => format!("*dyn mut {}", id.clone().name(scopes, types, strings)),
            Type::FnPtr(f) => {
                let f = f.clone();
                let mut result = String::new();
                if f.is_extern {
                    result.push_str("extern ");
                }

                result.push_str("fn(");
                for (i, &param) in f.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&param.name(scopes, types, strings));
                }
                format!("{result}) => {}", f.ret.name(scopes, types, strings))
            }
            Type::Fn(func) => {
                let ty_args = func.ty_args.clone();
                let f = scopes.get(func.id);
                let mut result = format!("fn {}(", strings.resolve(&f.name.data));
                let ret = f.ret.with_templates(types, &ty_args);
                let params: Vec<_> =
                    f.params.iter().map(|p| p.ty.with_templates(types, &ty_args)).collect();
                for (i, ty) in params.into_iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&ty.name(scopes, types, strings));
                }
                format!("{result}): {}", ret.name(scopes, types, strings))
            }
            Type::User(ty) => ty.clone().name(scopes, types, strings),
            &Type::Array(ty, count) => format!("[{}; {}]", ty.name(scopes, types, strings), count),
            Type::Isize => "int".into(),
            Type::Usize => "uint".into(),
            id @ (Type::CInt(ty) | Type::CUint(ty)) => {
                format!("c_{}{ty:#}", ["", "u"][matches!(id, Type::CUint(_)) as usize])
            }
        }
    }

    pub fn as_option_inner(self, proj: &Project) -> Option<TypeId> {
        proj.types[self].as_option_inner(&proj.scopes)
    }

    pub fn strip_references(self, types: &Types) -> TypeId {
        let mut id = self;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = &types[id] {
            id = *inner;
        }
        id
    }

    pub fn strip_references_r(self, types: &Types) -> &Type {
        &types[self.strip_references(types)]
    }

    pub fn strip_options(self, proj: &Project) -> TypeId {
        let mut id = self;
        while let Some(inner) = id.as_option_inner(proj) {
            id = inner;
        }
        id
    }

    pub fn supports_binary(self, types: &Types, op: BinaryOp) -> bool {
        use BinaryOp::*;
        let this = &types[self];
        match op {
            Assign => true,
            Add | AddAssign | Sub | SubAssign => {
                this.is_numeric() || this.is_raw_ptr() || this.is_raw_mut_ptr()
            }
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
                        | Type::RawMutPtr(_)
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
                        | Type::RawMutPtr(_)
                )
            }
            LogicalOr | LogicalAnd => this.is_bool(),
            NoneCoalesce | NoneCoalesceAssign => false,
        }
    }

    pub fn supports_unary(self, scopes: &Scopes, types: &Types, op: UnaryOp) -> bool {
        use UnaryOp::*;
        let this = &types[self];
        match op {
            Neg => {
                this.as_integral(false).is_some_and(|s| s.signed)
                    || matches!(this, Type::F32 | Type::F64)
            }
            PostIncrement | PostDecrement | PreIncrement | PreDecrement => {
                this.is_integral() || this.is_raw_ptr() || this.is_raw_mut_ptr()
            }
            Not => this.is_integral() || this.is_bool(),
            Try => this.as_option_inner(scopes).is_some(),
            Plus => this.is_numeric(),
            Deref => this.is_any_ptr(),
            Addr | AddrMut | AddrRaw | AddrRawMut | Option => true,
            Unwrap => false,
        }
    }

    pub fn as_pointee(self, types: &Types) -> Option<TypeId> {
        if let Type::Ptr(id) | Type::MutPtr(id) | Type::RawPtr(id) | Type::RawMutPtr(id) =
            &types[self]
        {
            Some(*id)
        } else {
            None
        }
    }

    pub fn size_and_align(self, scopes: &Scopes, types: &mut Types) -> (usize, usize) {
        let sz = match &types[self] {
            Type::Int(0) | Type::Uint(0) => 0,
            Type::Int(bits) | Type::Uint(bits) => nearest_pow_of_two(*bits) / 8,
            Type::CInt(inner) | Type::CUint(inner) => inner.size(),
            Type::Ptr(_) | Type::MutPtr(_) | Type::RawPtr(_) | Type::RawMutPtr(_) => {
                std::mem::size_of::<*const ()>()
            }
            Type::DynPtr(_) | Type::DynMutPtr(_) => std::mem::size_of::<*const ()>() * 2,
            Type::Isize => std::mem::size_of::<isize>(),
            Type::Usize => std::mem::size_of::<usize>(),
            Type::F32 => std::mem::size_of::<f32>(),
            Type::F64 => std::mem::size_of::<f64>(),
            Type::Bool => std::mem::size_of::<bool>(),
            Type::Char => std::mem::size_of::<char>(),
            Type::FnPtr(_) => std::mem::size_of::<fn()>(),
            Type::User(ut) if !scopes.get(ut.id).recursive => {
                let ut = ut.clone();
                struct SizeAndAlign {
                    size: usize,
                    align: usize,
                }

                impl SizeAndAlign {
                    fn next(&mut self, (s, a): (usize, usize)) {
                        self.size += (a - self.size % a) % a + s;
                        self.align = self.align.max(a);
                    }
                }

                let mut sa = SizeAndAlign { size: 0, align: 1 };
                let ut_data = scopes.get(ut.id);
                match &ut_data.kind {
                    UserTypeKind::Union(union) => {
                        if self.can_omit_tag(scopes, types).is_none() {
                            sa.next(union.tag.size_and_align(scopes, types));
                        }
                        for member in ut_data.members.values() {
                            let ty = member.ty.with_templates(types, &ut.ty_args);
                            sa.next(ty.size_and_align(scopes, types));
                        }

                        sa.next(union.variants.values().flat_map(|v| v.ty).fold(
                            (0, 1),
                            |(sz, align), ty| {
                                let (s, a) = ty
                                    .with_templates(types, &ut.ty_args)
                                    .size_and_align(scopes, types);
                                (sz.max(s), align.max(a))
                            },
                        ));
                    }
                    UserTypeKind::UnsafeUnion => {
                        sa.next(ut_data.members.values().fold((0, 1), |(sz, align), m| {
                            let (s, a) =
                                m.ty.with_templates(types, &ut.ty_args)
                                    .size_and_align(scopes, types);
                            (sz.max(s), align.max(a))
                        }));
                    }
                    UserTypeKind::PackedStruct(data) => sa.next((data.size, data.align)),
                    _ => {
                        for member in ut_data.members.values() {
                            let ty = member.ty.with_templates(types, &ut.ty_args);
                            sa.next(ty.size_and_align(scopes, types));
                        }
                    }
                }

                sa.next((0, sa.align));
                return (sa.size, sa.align);
            }
            &Type::Array(ty, len) => {
                let (s, a) = ty.size_and_align(scopes, types);
                return (s * len, a);
            }
            _ => 0,
        };

        // assume self-alignment or 1 for 0 size types
        (sz, sz.clamp(1, MAX_ALIGN))
    }

    pub fn bit_size(self, scopes: &Scopes, types: &Types) -> BitSizeResult {
        if let Some(int) = self.as_integral(types, true) {
            BitSizeResult::Size(int.bits)
        } else {
            match &types[self] {
                Type::Unknown | Type::Unresolved(_) => BitSizeResult::Size(0),
                Type::User(ut) => {
                    let ut = scopes.get(ut.id);
                    if let Some(u) = ut.kind.as_union().filter(|u| u.enum_union) {
                        match u.tag.bit_size(scopes, types) {
                            BitSizeResult::NonEnum | BitSizeResult::Bad => BitSizeResult::Size(0),
                            BitSizeResult::Size(n) => BitSizeResult::Tag(u.tag, n),
                            res => res,
                        }
                    } else {
                        BitSizeResult::NonEnum
                    }
                }
                _ => BitSizeResult::Bad,
            }
        }
    }

    pub fn matched_inner_type(self, types: &mut Types, ty: TypeId) -> TypeId {
        let mut id = &types[self];
        if !matches!(id, Type::Ptr(_) | Type::MutPtr(_)) {
            return ty;
        }

        while let Type::MutPtr(inner) = id {
            id = &types[*inner];
        }

        if matches!(id, Type::Ptr(_)) {
            types.insert(Type::Ptr(ty))
        } else {
            types.insert(Type::MutPtr(ty))
        }
    }

    pub fn can_omit_tag(self, scopes: &Scopes, types: &Types) -> Option<TypeId> {
        types.get(self).as_user().and_then(|s| s.can_omit_tag(scopes, types))
    }

    pub fn with_templates(self, types: &mut Types, map: &TypeArgs) -> TypeId {
        if map.is_empty() {
            return self;
        }

        match &types[self] {
            &Type::Array(ty, len) => {
                let ty = Type::Array(ty.with_templates(types, map), len);
                types.insert(ty)
            }
            Type::Ptr(t) => {
                let ty = Type::Ptr(t.with_templates(types, map));
                types.insert(ty)
            }
            Type::MutPtr(t) => {
                let ty = Type::MutPtr(t.with_templates(types, map));
                types.insert(ty)
            }
            Type::RawPtr(t) => {
                let ty = Type::RawPtr(t.with_templates(types, map));
                types.insert(ty)
            }
            Type::RawMutPtr(t) => {
                let ty = Type::RawMutPtr(t.with_templates(types, map));
                types.insert(ty)
            }
            Type::User(ut) => {
                if let Some(&ty) = map.get(&ut.id).filter(|&&ty| ty != TypeId::UNKNOWN) {
                    ty
                } else if !ut.ty_args.is_empty() {
                    let mut ut = ut.clone();
                    ut.fill_templates(types, map);
                    types.insert(Type::User(ut))
                } else {
                    self
                }
            }
            Type::Fn(f) => {
                let mut tr = f.clone();
                tr.fill_templates(types, map);
                types.insert(Type::Fn(tr))
            }
            Type::FnPtr(f) => {
                let f = f.clone();
                let fnptr = Type::FnPtr(FnPtr {
                    is_extern: f.is_extern,
                    params: f.params.iter().map(|p| p.with_templates(types, map)).collect(),
                    ret: f.ret.with_templates(types, map),
                });
                types.insert(fnptr)
            }
            Type::DynPtr(tr) => {
                let mut tr = tr.clone();
                tr.fill_templates(types, map);
                types.insert(Type::DynPtr(tr))
            }
            Type::DynMutPtr(tr) => {
                let mut tr = tr.clone();
                tr.fill_templates(types, map);
                types.insert(Type::DynMutPtr(tr))
            }
            _ => self,
        }
    }

    pub fn with_ut_templates(self, types: &mut Types, id: TypeId) -> TypeId {
        if let Type::User(ut) = &types[id] {
            self.with_templates(types, &ut.ty_args.clone())
        } else {
            self
        }
    }

    pub fn is_void(self) -> bool {
        matches!(self, TypeId::NEVER | TypeId::VOID)
    }

    pub fn is_packed_struct(self, proj: &Project) -> bool {
        matches!(&proj.types[self], Type::User(ut)
            if proj.scopes.get(ut.id).kind.is_packed_struct())
    }

    pub fn as_integral(self, types: &Types, extra: bool) -> Option<Integer> {
        types[self].as_integral(extra)
    }
}
