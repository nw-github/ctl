use crate::{
    ast::parsed::{
        ExprArena, Path, PathOrigin, Pattern, TypeHint, TypeHintData, UsePath, UsePathComponent,
        UsePathOrigin,
    },
    intern::Strings,
    project::Project,
    typeid::{GenericUserType, Type, TypeId},
};

#[derive(Clone, Copy, derive_more::Constructor)]
pub struct FmtTy<'a> {
    ty: TypeId,
    p: &'a Project,
}

impl std::fmt::Display for FmtTy<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let p = self.p;
        match &p.types[self.ty] {
            Type::Unknown => write!(f, "{{unknown}}"),
            // for debug purposes, ideally this should never be visible
            Type::Unresolved(_, _) => write!(f, "{{unresolved}}"),
            Type::Void => write!(f, "void"),
            Type::Never => write!(f, "never"),
            Type::Int(bits) => write!(f, "i{bits}"),
            Type::Uint(bits) => write!(f, "u{bits}"),
            Type::Isize => write!(f, "int"),
            Type::Usize => write!(f, "uint"),
            Type::CInt(ty) => write!(f, "c_{ty:#}"),
            Type::CUint(ty) => write!(f, "c_u{ty:#}"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            &Type::Ptr(id) => write!(f, "*{}", p.fmt_ty(id)),
            &Type::MutPtr(id) => write!(f, "*mut {}", p.fmt_ty(id)),
            &Type::RawPtr(id) => write!(f, "^{}", p.fmt_ty(id)),
            &Type::RawMutPtr(id) => write!(f, "^mut {}", p.fmt_ty(id)),
            Type::DynPtr(id) => write!(f, "*dyn {}", p.fmt_ut(id)),
            Type::DynMutPtr(id) => write!(f, "*dyn mut {}", p.fmt_ut(id)),
            Type::FnPtr(func) => {
                if func.is_extern {
                    write!(f, "extern ")?;
                }

                if func.is_unsafe {
                    write!(f, "unsafe ")?;
                }

                write!(f, "fn(")?;
                for (i, &param) in func.params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p.fmt_ty(param))?;
                }
                write!(f, ") => {}", p.fmt_ty(func.ret))
            }
            Type::Fn(ofn) => {
                let func = p.scopes.get(ofn.id);
                write!(f, "fn {}(", p.strings.resolve(&func.name.data))?;
                for (i, param) in func.params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p.fmt_ty(param.ty.with_templates(&p.types, &ofn.ty_args)))?;
                }
                write!(f, "): {}", p.fmt_ty(func.ret.with_templates(&p.types, &ofn.ty_args)))
            }
            Type::User(ty) => write!(f, "{}", p.fmt_ut(ty)),
            &Type::Array(ty, count) => write!(f, "[{}; {count}]", p.fmt_ty(ty)),
        }
    }
}

#[derive(Clone, Copy, derive_more::Constructor)]
pub struct FmtUt<'a, 'b> {
    ut: &'a GenericUserType,
    p: &'b Project,
}

impl std::fmt::Display for FmtUt<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let p = self.p;
        let ut = self.ut;
        match &p.scopes.get(ut.id).kind {
            crate::sym::UserTypeKind::Tuple => {
                write!(f, "(")?;
                for (i, (name, member)) in p.scopes.get(ut.id).members.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let name = p.strings.resolve(name);
                    let ty = member.ty.with_templates(&p.types, &ut.ty_args);
                    if name.starts_with(|ch: char| ch.is_ascii_digit()) {
                        write!(f, "{}", p.fmt_ty(ty))?;
                    } else {
                        write!(f, "{name}: {}", p.fmt_ty(ty))?;
                    }
                }
                write!(f, ")")
            }
            _ => {
                let is_lang_type = |name: &str| {
                    let key = p.strings.get(name);
                    key.and_then(|key| p.scopes.lang_types.get(&key)).is_some_and(|&id| id == ut.id)
                };
                if is_lang_type("option") {
                    return write!(f, "?{}", p.fmt_ty(ut.ty_args[0]));
                } else if is_lang_type("span") {
                    return write!(f, "[{}..]", p.fmt_ty(ut.ty_args[0]));
                } else if is_lang_type("span_mut") {
                    return write!(f, "[mut {}..]", p.fmt_ty(ut.ty_args[0]));
                } else if is_lang_type("vec") {
                    return write!(f, "[{}]", p.fmt_ty(ut.ty_args[0]));
                } else if is_lang_type("set") {
                    return write!(f, "#[{}]", p.fmt_ty(ut.ty_args[0]));
                } else if is_lang_type("map") {
                    return write!(f, "[{}: {}]", p.fmt_ty(ut.ty_args[0]), p.fmt_ty(ut.ty_args[1]));
                }

                write!(f, "{}", p.strings.resolve(&p.scopes.get(ut.id).name.data))?;
                if !ut.ty_args.is_empty() {
                    write!(f, "<")?;
                    for (i, concrete) in ut.ty_args.values().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", p.fmt_ty(*concrete))?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Copy, derive_more::Constructor)]
pub struct FmtHint<'a> {
    ty: TypeHint,
    strings: &'a Strings,
    arena: &'a ExprArena,
}

impl FmtHint<'_> {
    fn subtype(&self, ty: TypeHint) -> Self {
        Self { ty, ..*self }
    }
}

impl std::fmt::Display for FmtHint<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.arena.hints.get(self.ty.data) {
            TypeHintData::Path(path) => {
                write!(f, "{}", FmtPath::new(path, self.strings, self.arena))
            }
            &TypeHintData::Array(ty, _) => write!(f, "[{}; <expr>]", self.subtype(ty)),
            &TypeHintData::Vec(ty) => write!(f, "[{}]", self.subtype(ty)),
            &TypeHintData::Slice(ty) => write!(f, "[{}..]", self.subtype(ty)),
            &TypeHintData::SliceMut(ty) => write!(f, "[mut {}..]", self.subtype(ty)),
            TypeHintData::Tuple(vals) => {
                write!(f, "(")?;
                for (i, (name, ty)) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if !self.strings.resolve(&name.data).starts_with(|ch: char| ch.is_ascii_digit())
                    {
                        write!(f, "{}: ", self.strings.resolve(&name.data))?;
                    }
                    write!(f, "{}", self.subtype(*ty))?;
                }
                write!(f, ")")
            }
            &TypeHintData::Set(ty) => write!(f, "#[{}]", self.subtype(ty)),
            &TypeHintData::Map([k, v]) => write!(f, "[{}: {}]", self.subtype(k), self.subtype(v)),
            &TypeHintData::Option(ty) => write!(f, "?{}", self.subtype(ty)),
            &TypeHintData::Ptr(ty) => write!(f, "*{}", self.subtype(ty)),
            &TypeHintData::MutPtr(ty) => write!(f, "*mut {}", self.subtype(ty)),
            &TypeHintData::RawPtr(ty) => write!(f, "^{}", self.subtype(ty)),
            &TypeHintData::RawMutPtr(ty) => write!(f, "^mut {}", self.subtype(ty)),
            TypeHintData::DynPtr(ty) => {
                write!(f, "*dyn {}", FmtPath::new(ty, self.strings, self.arena))
            }
            TypeHintData::DynMutPtr(ty) => {
                write!(f, "*dyn mut {}", FmtPath::new(ty, self.strings, self.arena))
            }
            TypeHintData::Fn { is_extern, is_unsafe, params, ret } => {
                crate::write_if!(*is_extern, f, "extern ");
                crate::write_if!(*is_unsafe, f, "unsafe ");
                write!(f, "fn (")?;
                for (i, ty) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", self.subtype(*ty))?;
                }
                if let Some(ret) = ret {
                    write!(f, "): {}", self.subtype(*ret))
                } else {
                    write!(f, ")")
                }
            }
            TypeHintData::Void => write!(f, "void"),
            TypeHintData::Error => write!(f, "Error"),
        }
    }
}

#[derive(derive_more::Constructor)]
pub struct FmtPath<'a> {
    path: &'a Path,
    strings: &'a Strings,
    arena: &'a ExprArena,
}

impl std::fmt::Display for FmtPath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

        let mut tmp = String::new();

        write!(tmp, "{}", self.path.origin)?;
        for (i, (name, ty_args)) in self.path.components.iter().enumerate() {
            if i > 0 || matches!(self.path.origin, PathOrigin::This(_)) {
                write!(tmp, "::")?;
            }

            write!(tmp, "{}", self.strings.resolve(&name.data))?;
            if ty_args.is_empty() {
                continue;
            }

            write!(tmp, "::<")?;
            for (i, generic) in ty_args.iter().enumerate() {
                if i > 0 {
                    write!(tmp, ", ")?;
                }
                write!(tmp, "{}", FmtHint::new(*generic, self.strings, self.arena))?;
            }
            write!(tmp, ">")?;
        }

        if tmp.is_empty() { write!(f, "<empty>") } else { write!(f, "{tmp}") }
    }
}

#[derive(derive_more::Constructor)]
pub struct FmtPatt<'a> {
    patt: &'a Pattern,
    strings: &'a Strings,
    arena: &'a ExprArena,
}

impl std::fmt::Display for FmtPatt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.patt {
            Pattern::Path(path) => write!(f, "{}", FmtPath::new(path, self.strings, self.arena)),
            Pattern::MutBinding(v) => write!(f, "mut {}", self.strings.resolve(v)),
            Pattern::Option(v) => {
                write!(f, "?({})", FmtPatt::new(&v.data, self.strings, self.arena))
            }
            _ => write!(f, "<TODO: Pattern>"),
        }
    }
}

#[derive(derive_more::Constructor)]
pub struct FmtUsePath<'a> {
    strings: &'a Strings,
    path: &'a UsePath,
}

impl std::fmt::Display for FmtUsePath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.path.public {
            write!(f, "pub ")?;
        }

        match self.path.origin {
            UsePathOrigin::Root(_) => write!(f, "::")?,
            UsePathOrigin::Super(_) => write!(f, "super::")?,
            UsePathOrigin::Here => {}
        }

        write!(f, "{}", FmtUsePathComponent::new(self.strings, &self.path.component))
    }
}

#[derive(derive_more::Constructor)]
pub struct FmtUsePathComponent<'a> {
    strings: &'a Strings,
    comp: &'a UsePathComponent,
}

impl std::fmt::Display for FmtUsePathComponent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.comp {
            UsePathComponent::Multi(comps) => {
                write!(f, "{{")?;
                for (i, comp) in comps.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", FmtUsePathComponent::new(self.strings, comp))?;
                }
                write!(f, "}}")
            }
            UsePathComponent::Ident { ident, next } => {
                write!(f, "{}", self.strings.resolve(&ident.data))?;
                if let Some(next) = next {
                    write!(f, "::{}", FmtUsePathComponent::new(self.strings, next))?;
                }
                Ok(())
            }
            UsePathComponent::Rename { ident, new_name } => {
                write!(
                    f,
                    "{} as {}",
                    self.strings.resolve(&ident.data),
                    self.strings.resolve(&new_name.data),
                )
            }
            UsePathComponent::All(_) => write!(f, "*"),
            UsePathComponent::Error => write!(f, "<error>"),
        }
    }
}
