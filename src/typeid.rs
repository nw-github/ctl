use crate::{
    ast::{parsed::TypeHint, BinaryOp},
    sym::{FunctionId, ScopeId, Scopes, UserTypeId, Vis},
};
use derive_more::Constructor;
use enum_as_inner::EnumAsInner;
use num_bigint::BigInt;

#[derive(Debug, PartialEq, Eq, Clone, Hash, Constructor)]
pub struct GenericFunc {
    pub id: FunctionId,
    pub ty_args: Vec<Type>,
}

impl GenericFunc {
    pub fn infer_type_args(&mut self, mut src: &Type, mut target: &Type, scopes: &Scopes) {
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
                        self.infer_type_args(src, target, scopes);
                    }

                    self.infer_type_args(&src.ret, &target.ret, scopes);
                    break;
                }
                (Type::User(src), target) => {
                    if let Some(t) = target.as_user() {
                        if src.id != t.id {
                            if let Some(inner) =
                                target.as_option_inner(scopes).and_then(|i| i.as_user())
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
    pub ty_args: Vec<Type>,
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
    pub bits: u32,
    pub signed: bool,
}

impl IntStats {
    pub fn min_signed(&self) -> BigInt {
        -(BigInt::from(1) << (self.bits - 1))
    }

    pub fn max_signed(&self) -> BigInt {
        (BigInt::from(1) << (self.bits - 1)) - 1
    }

    pub fn max_unsigned(&self) -> BigInt {
        (BigInt::from(1u8) << self.bits) - 1u8
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnPtr {
    pub params: Vec<Type>,
    pub ret: Type,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Type {
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
    User(Box<GenericUserType>),
    Ptr(Box<Type>),
    MutPtr(Box<Type>),
    Array(Box<(Type, usize)>),
    TraitSelf,
}

impl Default for Type {
    fn default() -> Self {
        Self::Unknown(None)
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unknown(_), Self::Unknown(_)) => true,
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
    pub fn from_typehint(hint: TypeHint, scopes: &Scopes) -> Self {
        Self::Unknown(Some((hint, scopes.current).into()))
    }

    pub fn discriminant_for(max: usize) -> Type {
        Type::Uint((max as f64).log2().ceil() as u32)
    }

    pub fn supports_binop(&self, scopes: &Scopes, op: BinaryOp) -> bool {
        match op {
            BinaryOp::Add => matches!(
                self,
                Type::Int(_)
                    | Type::Isize
                    | Type::Uint(_)
                    | Type::Usize
                    | Type::F32
                    | Type::F64
                    | Type::CInt(_)
                    | Type::CUint(_)
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
                ) || matches!(self, Type::User(ut) if scopes.get(ut.id).data.is_enum())
            }
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                matches!(self, Type::Bool)
            }
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

    pub fn fill_struct_templates(&mut self, scopes: &Scopes, inst: &GenericUserType) {
        if inst.ty_args.is_empty() {
            return;
        }

        let mut src = self;
        loop {
            match src {
                Type::Array(t) => src = &mut t.0,
                Type::Ptr(t) | Type::MutPtr(t) => src = t,
                Type::User(ty) => {
                    if !ty.ty_args.is_empty() {
                        for ty in ty.ty_args.iter_mut() {
                            ty.fill_struct_templates(scopes, inst);
                        }
                    } else if let Some(&index) = scopes.get(ty.id).data.as_struct_template() {
                        *src = inst.ty_args[index].clone();
                    }

                    break;
                }
                Type::FnPtr(f) => {
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
                Type::Array(t) => src = &mut t.0,
                Type::Ptr(t) | Type::MutPtr(t) => src = t,
                Type::User(ty) => {
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
                Type::FnPtr(f) => {
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

    pub fn fill_this(&mut self, this: &Type) {
        let mut src = self;
        loop {
            match src {
                Type::Array(t) => src = &mut t.0,
                Type::Ptr(t) | Type::MutPtr(t) => src = t,
                Type::User(ty) => {
                    for ty in ty.ty_args.iter_mut() {
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

    pub fn name(&self, scopes: &Scopes) -> String {
        match self {
            Type::Void => "void".into(),
            Type::Never => "never".into(),
            Type::Int(bits) => format!("i{bits}"),
            Type::Uint(bits) => format!("u{bits}"),
            // for debug purposes, ideally this should never be visible
            Type::Unknown(Some(_)) => "{unresolved}".into(),
            Type::Unknown(None) => "{unknown}".into(),
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
            Type::Isize => "isize".into(),
            Type::Usize => "usize".into(),
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

    pub fn is_void_like(&self) -> bool {
        matches!(self, Type::Void | Type::Never | Type::CVoid)
    }

    pub fn integer_stats(&self) -> Option<IntStats> {
        use std::ffi::*;

        let (bytes, signed) = match self {
            Type::Int(bits) | Type::Uint(bits) => (*bits / 8, matches!(self, Type::Int(_))),
            Type::CInt(cint) | Type::CUint(cint) => {
                let bits = match cint {
                    CInt::Char => std::mem::size_of::<c_char>(),
                    CInt::Short => std::mem::size_of::<c_short>(),
                    CInt::Int => std::mem::size_of::<c_int>(),
                    CInt::Long => std::mem::size_of::<c_long>(),
                    CInt::LongLong => std::mem::size_of::<c_longlong>(),
                };
                (bits as u32, matches!(self, Type::CInt(_)))
            }
            Type::Isize => (std::mem::size_of::<isize>() as u32, true),
            Type::Usize => (std::mem::size_of::<usize>() as u32, false),
            _ => return None,
        };

        Some(IntStats {
            bits: bytes * 8,
            signed,
        })
    }

    pub fn coerces_to(&self, scopes: &Scopes, target: &Type) -> bool {
        match (self, target) {
            (Type::Never | Type::Unknown(_), _) => true,
            (_, Type::Unknown(_)) => true,
            (ty, target)
                if target
                    .as_option_inner(scopes)
                    .map_or(false, |inner| ty.coerces_to(scopes, inner)) =>
            {
                true
            }
            (ty, target) if ty.may_ptr_coerce(target) => true,
            (ty, target) => ty == target,
        }
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

    pub fn from_int_name(name: &str) -> Option<Type> {
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
                "usize" => Some(Type::Usize),
                "isize" => Some(Type::Isize),
                _ => None,
            },
        }
    }

    pub fn implements_trait(&self, scopes: &Scopes, bound: &GenericUserType) -> bool {
        if self.is_unknown() {
            return true;
        }

        let search = |this, impls: &[Type]| {
            impls.iter().any(|tr| {
                eprintln!("impl: {} bound: {}", tr.name(scopes), bound.name(scopes));
                let mut tr = tr.clone();
                if let Some(this) = this {
                    tr.fill_struct_templates(scopes, this);
                }
                &**tr.as_user().unwrap() == bound
            })
        };

        if self
            .as_user()
            .map_or(false, |this| search(Some(this), &scopes.get(this.id).impls))
        {
            return true;
        }

        scopes
            .extensions_in_scope_for(self)
            .any(|ext| search(self.as_user().map(|ty| &**ty), &ext.impls))
    }

    pub fn as_option_inner<'a>(&'a self, scopes: &Scopes) -> Option<&'a Type> {
        scopes.get_option_id().and_then(|opt| {
            self.as_user()
                .filter(|ut| ut.id == opt)
                .map(|ut| &ut.ty_args[0])
        })
    }

    pub fn get_trait_impl<'a, 'b>(
        &'b self,
        scopes: &'a Scopes,
        id: UserTypeId,
    ) -> Option<&'b GenericUserType>
    where
        'a: 'b,
    {
        let search = |impls: &'a [Type]| {
            impls
                .iter()
                .flat_map(|i| i.as_user().map(|ut| &**ut))
                .find(|ut| ut.id == id)
        };

        if let Some(ty) = self
            .as_user()
            .and_then(|ut| search(&scopes.get(ut.id).impls))
        {
            return Some(ty);
        }

        scopes
            .extensions_in_scope_for(self)
            .find_map(|ext| search(&ext.impls))
    }

    pub fn get_member_fn(
        &self,
        scopes: &Scopes,
        member: &str,
    ) -> Option<(Option<GenericUserType>, Vis<FunctionId>, ScopeId)> {
        let search = |src_scope: ScopeId, scope: ScopeId| {
            // TODO: trait implement overload ie.
            // impl Eq<f32> { ... } impl Eq<i32> { ... }
            std::iter::once(scope)
                .chain(scopes[scope].children.iter().map(|s| s.id))
                .find_map(|scope| {
                    scopes
                        .find_in(member, scope)
                        .map(|func| (None, func, src_scope))
                })
        };

        if let Some(ut) = self.as_user().map(|ut| scopes.get(ut.id)) {
            let src_scope = ut.scope;
            if ut.data.is_template() {
                for ut in ut.impls.iter().map(|ut| ut.as_user().unwrap()) {
                    if let Some(func) = scopes.find_in(member, scopes.get(ut.id).body_scope) {
                        return Some((Some((**ut).clone()), func, src_scope));
                    }
                }

                return None;
            }

            if let Some(result) = search(src_scope, ut.body_scope) {
                return Some(result);
            }
        }

        scopes
            .extensions_in_scope_for(self)
            .find_map(|ext| search(ext.scope, ext.body_scope))
    }
}
