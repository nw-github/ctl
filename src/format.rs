use crate::{project::Project, typeid::{GenericUserType, Type, TypeId}};

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
            crate::sym::UserTypeKind::AnonStruct => {
                write!(f, "struct {{")?;
                for (i, concrete) in ut.ty_args.values().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(
                        f,
                        " {}: {}",
                        p.scopes
                            .get(ut.id)
                            .members
                            .get_index(i)
                            .map(|m| p.strings.resolve(m.0))
                            .unwrap_or("???"),
                        p.fmt_ty(*concrete),
                    )?;
                }
                write!(f, " }}")
            }
            crate::sym::UserTypeKind::Tuple => {
                write!(f, "(")?;
                for (i, concrete) in ut.ty_args.values().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p.fmt_ty(*concrete))?;
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




