use std::fmt::{Display, Write};

use crate::{
    CachingSourceProvider, CodegenFlags, Diagnostics, OffsetMode, SourceProvider, Span,
    ast::{Alignment, BinaryOp, Sign, UnaryOp, checked::*, parsed::RangePattern},
    ds::{ComptimeInt, Dependencies, DependencyGraph, HashMap, HashSet, IndexMap},
    intern::{StrId, Strings},
    nearest_pow_of_two,
    project::Project,
    sym::*,
    typecheck::{ExtensionCache, MemberFn, MemberFnType, SharedStuff},
    typeid::{
        BitSizeResult, CInt, FnPtr, GenericFn, GenericTrait, GenericUserType, Integer, Type,
        TypeArgs, TypeId, Types,
    },
    write_de,
};

#[macro_export]
macro_rules! write_if {
    ($cond: expr, $dst:expr, $($arg:tt)*) => {{
        if $cond {
            _ = write!($dst, $($arg)*)
        }
    }};
}

#[macro_export]
macro_rules! write_nm {
    ($self: expr, $($arg:tt)*) => {
        if !$self.flags.minify {
            _ = write!($self.buffer, $($arg)*)
        }
    };
}

const UNION_TAG_NAME: &str = "tag";
const ARRAY_DATA_NAME: &str = "data";
const VOID_INSTANCE: &str = "CTL_VOID";
const NULLPTR: &str = "((void*)0)";
const VTABLE_TRAIT_START: &str = "$Start";
const VTABLE_TRAIT_LEN: &str = "$Len";
const LOOP_CONT_PREFIX: &str = "$c";
const SCOPE_VAR_PREFIX: &str = "$s";

#[derive(Default)]
struct TypeGen {
    types: DependencyGraph<TypeId>,
    traits: HashSet<UserTypeId>,
}

impl TypeGen {
    fn gen_fnptr(buffer: &mut Buffer, flags: &CodegenFlags, f: &FnPtr) {
        write_de!(buffer, "typedef ");
        if f.ret.is_void_like() {
            write_de!(buffer, "void");
        } else {
            buffer.emit_type(f.ret, flags.minify);
        }
        write_de!(buffer, "(*");
        buffer.emit_fnptr_name(f, flags.minify);
        write_de!(buffer, ")(");

        let types = &buffer.1.types;
        for (i, &param) in f.params.iter().enumerate() {
            if i > 0 {
                write_de!(buffer, ",");
            }

            if i == 0 && !f.is_extern && types[param].is_safe_ptr() {
                if types[param].is_ptr() {
                    buffer.emit("void const*");
                } else {
                    buffer.emit("void*");
                }
            } else {
                buffer.emit_type(param, flags.minify);
            }
        }
        write_de!(buffer, ");");
    }

    fn gen_vtable_info(flags: &CodegenFlags, buf: &mut Buffer, tr: UserTypeId) {
        // TODO: not sure if losing type information on the vtables will affect optimization in any
        // way. if it does, we can go back to emitting full vtable structs per trait.
        let Some(Dependencies::Resolved(deps)) = buf.1.trait_deps.get(&tr) else {
            panic!(
                "ICE: Dyn pointer for trait '{}' has invalid dependencies",
                buf.1.strings.resolve(&tr.name(&buf.1.scopes).data),
            );
        };

        write_de!(buf, "enum {{");

        let mut offset = 0;
        for id in std::iter::once(tr).chain(deps.iter().cloned()) {
            buf.emit_vtable_prefix(tr, flags.minify);
            buf.emit_vtable_prefix(id, flags.minify);
            write_de!(buf, "{VTABLE_TRAIT_START}={offset},");

            for f in vtable_methods(&buf.1.scopes, &buf.1.types, id) {
                buf.emit_vtable_prefix(tr, flags.minify);
                buf.emit_vtable_fn_name(f.id, flags.minify);
                write_de!(buf, "={offset},");
                offset += 1;
            }
        }

        buf.emit_vtable_prefix(tr, flags.minify);
        write_de!(buf, "{VTABLE_TRAIT_LEN}={offset}}};");
    }

    fn gen_usertype(
        flags: &CodegenFlags,
        decls: &mut Buffer,
        defs: &mut Buffer,
        ut: &GenericUserType,
    ) {
        fn emit_member(
            ut: &GenericUserType,
            name: StrId,
            ty: TypeId,
            buffer: &mut Buffer,
            min: bool,
        ) {
            let types = &buffer.1.types;
            let scopes = &buffer.1.scopes;

            let ty = ty.with_templates(types, &ut.ty_args);
            if ty.size_and_align(scopes, types).0 == 0 {
                write_de!(buffer, "CTL_ZST ");
            }

            buffer.emit_type(ty, min);
            write_de!(buffer, " {};", member_name(buffer.1, Some(ut.id), name));
        }

        let types = &decls.1.types;
        let scopes = &decls.1.scopes;

        let ut_data = scopes.get(ut.id);
        let decltype = if ut_data.kind.is_unsafe_union() { "union" } else { "struct" };

        write_de!(decls, "typedef {decltype} ");
        decls.emit_type_name(ut, flags.minify);
        write_de!(decls, " ");
        decls.emit_type_name(ut, flags.minify);
        write_de!(decls, ";");

        let members = &ut_data.members;
        if members.is_empty() && ut_data.kind.as_union().is_some_and(|u| u.variants.is_empty()) {
            return;
        }

        write_de!(defs, "{decltype} ");
        defs.emit_type_name(ut, flags.minify);
        write_de!(defs, "{{");

        match &ut_data.kind {
            UserTypeKind::Union(union) => {
                if !matches!(types[union.tag], Type::Uint(0) | Type::Int(0)) {
                    defs.emit_type(union.tag, flags.minify);
                    write_de!(defs, " {UNION_TAG_NAME};");
                }

                for (&name, member) in members {
                    emit_member(ut, name, member.ty, defs, flags.minify);
                }

                if union.variants.iter().any(|v| v.1.ty.is_some()) {
                    write_de!(defs, "union{{");
                    for (&name, variant) in union.variants.iter() {
                        if let Some(ty) = variant.ty {
                            emit_member(ut, name, ty, defs, flags.minify);
                        }
                    }
                    write_de!(defs, "}};");
                }
            }
            UserTypeKind::PackedStruct(data) => {
                write_de!(
                    defs,
                    "u{} {ARRAY_DATA_NAME}[{}];",
                    data.align * 8,
                    data.size / data.align
                );
            }
            _ => {
                if members.is_empty() {
                    write_de!(defs, "CTL_DUMMY_MEMBER;");
                }

                for (&name, member) in members {
                    emit_member(ut, name, member.ty, defs, flags.minify);
                }
            }
        }

        write_de!(defs, "}};");
    }

    fn emit(&self, decls: &mut Buffer, flags: &CodegenFlags) {
        let types = &decls.1.types;
        let scopes = &decls.1.scopes;

        let mut defs = Buffer::new(decls.1);
        self.types.visit_all(|&id| match &types[id] {
            Type::Fn(f) => Self::gen_fnptr(&mut defs, flags, &f.as_fn_ptr(scopes, types)),
            Type::FnPtr(f) => Self::gen_fnptr(&mut defs, flags, f),
            Type::User(ut) => Self::gen_usertype(flags, decls, &mut defs, ut),
            Type::DynPtr(tr) | Type::DynMutPtr(tr) => {
                Self::gen_vtable_info(flags, &mut defs, tr.id)
            }
            &Type::Int(bits) if bits != 0 => {
                let nearest = nearest_pow_of_two(bits);
                if flags.no_bit_int || nearest == bits as usize {
                    write_de!(decls, "typedef int{nearest}_t s{bits};",);
                } else {
                    write_de!(decls, "typedef _BitInt({bits}) s{bits};",);
                }
            }
            &Type::Uint(bits) if bits != 0 => {
                let nearest = nearest_pow_of_two(bits);
                if flags.no_bit_int || nearest == bits as usize {
                    write_de!(decls, "typedef uint{nearest}_t u{bits};");
                } else {
                    write_de!(decls, "typedef unsigned _BitInt({bits}) u{bits};");
                }
            }
            &Type::Array(ty, len) => {
                write_de!(decls, "typedef struct ");
                decls.emit_array_struct_name(ty, len, flags.minify);
                write_de!(decls, " ");
                decls.emit_array_struct_name(ty, len, flags.minify);
                write_de!(decls, ";");

                write_de!(defs, "struct ");
                defs.emit_array_struct_name(ty, len, flags.minify);
                write_de!(defs, "{{");
                defs.emit_type(ty, flags.minify);
                write_de!(defs, " {ARRAY_DATA_NAME}[{len}];}};");
            }
            _ => {}
        });
        decls.emit(defs.finish());
    }

    fn add_type(&mut self, scopes: &Scopes, types: &Types, ty: TypeId) {
        if self.types.contains_key(&ty) {
            return;
        }

        let mut deps = Vec::new();
        macro_rules! dependency {
            ($ty: expr) => {{
                let dep = $ty;
                let mut inner = dep;
                while let Some(id) = inner.as_pointee(types) {
                    inner = id;
                }
                if matches!(types[inner], Type::Fn(_) | Type::FnPtr(_)) {
                    deps.push(inner);
                } else if matches!(
                    types[dep],
                    Type::User(_)
                        | Type::Array(_, _)
                        | Type::Int(_)
                        | Type::Uint(_)
                        | Type::DynMutPtr(_)
                        | Type::DynPtr(_)
                ) {
                    deps.push(dep);
                }

                self.add_type(scopes, types, dep);
            }};
        }

        macro_rules! fnptr_dependency {
            ($ty: expr) => {{
                let dep = $ty;
                let mut inner = dep;
                while let Some(id) = inner.as_pointee(types) {
                    inner = id;
                }
                if matches!(types[inner], Type::Fn(_) | Type::FnPtr(_)) {
                    deps.push(inner);
                }

                self.add_type(scopes, types, dep);
            }};
        }

        match &types[ty] {
            Type::Int(_) | Type::Uint(_) => {}
            Type::DynMutPtr(tr) | Type::DynPtr(tr) => {
                if !self.traits.insert(tr.id) {
                    return;
                }
            }
            Type::FnPtr(f) => {
                for param in f.params.iter() {
                    fnptr_dependency!(*param);
                }
                fnptr_dependency!(f.ret);
            }
            Type::Fn(f) => {
                let func = scopes.get(f.id);
                for p in func.params.iter() {
                    fnptr_dependency!(p.ty.with_templates(types, &f.ty_args));
                }
                fnptr_dependency!(func.ret.with_templates(types, &f.ty_args));
            }
            &Type::Array(ty, _) => dependency!(ty),
            Type::User(ut) => {
                self.types.insert(ty, Dependencies::Resolving);
                for m in scopes.get(ut.id).members.values() {
                    dependency!(m.ty.with_templates(types, &ut.ty_args));
                }

                match &scopes.get(ut.id).kind {
                    UserTypeKind::Union(union) => {
                        dependency!(union.tag);
                        for ty in union.variants.values().flat_map(|v| v.ty) {
                            dependency!(ty.with_templates(types, &ut.ty_args));
                        }
                    }
                    UserTypeKind::PackedStruct(data) => {
                        dependency!(types.insert(Type::Uint(data.align as u32 * 8)))
                    }
                    _ => {}
                }
            }
            Type::Ptr(id) | Type::MutPtr(id) | Type::RawPtr(id) | Type::RawMutPtr(id) => {
                return self.add_type(scopes, types, *id);
            }
            _ => return,
        }

        self.types.insert(ty, Dependencies::Resolved(deps));
    }
}

#[derive(Eq, Clone)]
struct State {
    func: GenericFn,
    tmpvar: usize,
    caller: ScopeId,
    emitted_names: HashMap<StrId, VariableId>,
}

impl State {
    pub fn new(func: GenericFn, caller: ScopeId) -> Self {
        Self { func, caller, tmpvar: 0, emitted_names: Default::default() }
    }

    pub fn in_body_scope(func: GenericFn, scopes: &Scopes) -> Self {
        let scope = scopes.get(func.id).body_scope;
        Self::new(func, scope)
    }

    pub fn from_non_generic(id: FunctionId, scopes: &Scopes) -> Self {
        Self::new(GenericFn::from_id(scopes, id), scopes.get(id).body_scope)
    }

    pub fn with_inst(mut func: GenericFn, types: &Types, inst: TypeId, caller: ScopeId) -> Self {
        if let Type::User(ut) = &types[inst] {
            func.ty_args.copy_args(&ut.ty_args);
        }

        Self::new(func, caller)
    }

    pub fn tmpvar(&mut self) -> String {
        let v = format!("${}", self.tmpvar);
        self.tmpvar += 1;
        v
    }
}

impl std::hash::Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.func.hash(state);
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.func == other.func
    }
}

#[derive(Clone)]
struct Buffer<'a>(String, &'a Project);

impl<'a> Buffer<'a> {
    pub fn new(proj: &'a Project) -> Self {
        Self(Default::default(), proj)
    }

    pub fn take(&mut self) -> Self {
        Self(std::mem::take(&mut self.0), self.1)
    }

    fn emit(&mut self, source: impl AsRef<str>) {
        _ = self.write_str(source.as_ref());
    }

    fn emit_str(&mut self, source: StrId) {
        _ = self.write_str(self.1.strings.resolve(&source));
    }

    fn emit_mangled_name(&mut self, id: TypeId, min: bool) {
        match &self.1.types[id] {
            Type::Void => write_de!(self, "v"),
            Type::Never => write_de!(self, "n"),
            Type::Int(bits) => write_de!(self, "i{bits}"),
            Type::Uint(bits) => write_de!(self, "u{bits}"),
            Type::CInt(inner) => write_de!(self, "c{inner:#}"),
            Type::CUint(inner) => write_de!(self, "C{inner:#}"),
            Type::Isize => write_de!(self, "I"),
            Type::Usize => write_de!(self, "U"),
            Type::F32 => write_de!(self, "f"),
            Type::F64 => write_de!(self, "F"),
            Type::Bool => write_de!(self, "B"),
            Type::Char => write_de!(self, "X"),
            &Type::Ptr(inner) => {
                self.emit("p");
                self.emit_mangled_name(inner, min);
            }
            &Type::MutPtr(inner) => {
                self.emit("P");
                self.emit_mangled_name(inner, min);
            }
            &Type::RawPtr(inner) => {
                self.emit("r");
                self.emit_mangled_name(inner, min);
            }
            &Type::RawMutPtr(inner) => {
                self.emit("R");
                self.emit_mangled_name(inner, min);
            }
            Type::DynPtr(tr) => {
                self.emit("d");
                self.emit_type_name(tr, min);
            }
            Type::DynMutPtr(tr) => {
                self.emit("D");
                self.emit_type_name(tr, min);
            }
            Type::FnPtr(f) => self.emit_fnptr_name(f, min),
            Type::Fn(f) => self.emit_fnptr_name(&f.as_fn_ptr(&self.1.scopes, &self.1.types), min),
            Type::User(ut) => self.emit_type_name(ut, min),
            &Type::Array(ty, len) => self.emit_array_struct_name(ty, len, min),
            Type::Unknown => {
                write_de!(self, "__Unknown");
                eprintln!("ICE: TypeId::Unknown in emit_generic_mangled_name")
            }
            Type::Unresolved(_, _) => {
                panic!("ICE: TypeId::Unresolved in emit_generic_mangled_name")
            }
        }
    }

    fn emit_fnptr_name(&mut self, f: &FnPtr, min: bool) {
        write_de!(self, "N");
        for &param in f.params.iter() {
            let mut buf = Buffer::new(self.1);
            buf.emit_mangled_name(param, min);
            self.write_len_prefixed(&buf.finish());
        }
        write_de!(self, "n");
        self.emit_mangled_name(f.ret, min);
    }

    fn emit_type(&mut self, id: TypeId, min: bool) {
        match &self.1.types[id] {
            Type::Void | Type::Never => write_de!(self, "$void"),
            Type::Int(bits) => write_de!(self, "s{bits}"),
            Type::Uint(bits) => write_de!(self, "u{bits}"),
            Type::CInt(inner) => write_de!(self, "{inner}"),
            Type::CUint(inner) => write_de!(self, "unsigned {inner}"),
            Type::Isize => write_de!(self, "isize"),
            Type::Usize => write_de!(self, "usize"),
            Type::F32 => write_de!(self, "f32"),
            Type::F64 => write_de!(self, "f64"),
            Type::Bool => write_de!(self, "$bool"),
            Type::Char => write_de!(self, "$char"),
            base @ (&Type::Ptr(i) | &Type::MutPtr(i) | &Type::RawPtr(i) | &Type::RawMutPtr(i)) => {
                let is_const = base.is_ptr() || base.is_raw_ptr();
                if i.is_void_like() {
                    write_de!(self, "void");
                } else {
                    self.emit_type(i, min);
                }
                write_if!(is_const, self, " const");
                write_de!(self, "*");
            }
            Type::FnPtr(_) => self.emit_mangled_name(id, min),
            Type::Fn(_) => self.emit_mangled_name(id, min),
            Type::User(ut) => {
                if self.1.scopes.get(ut.id).kind.is_template() {
                    eprintln!("ICE: Template type in emit_type");
                    self.emit_str(self.1.scopes.get(ut.id).name.data);
                    return;
                }

                if let Some(ty) = id.can_omit_tag(&self.1.scopes, &self.1.types) {
                    self.emit_type(ty, min);
                } else {
                    self.emit_type_name(ut, min);
                }
            }
            &Type::Array(_, _) => self.emit_mangled_name(id, min),
            Type::DynPtr(_) => write_de!(self, "DynPtr"),
            Type::DynMutPtr(_) => write_de!(self, "DynMutPtr"),
            Type::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            Type::Unresolved(_, _) => panic!("ICE: TypeId::Unresolved in emit_type"),
        }
    }

    fn emit_type_name(&mut self, ut: &GenericUserType, min: bool) {
        let ty = self.1.scopes.get(ut.id);
        if ty.kind.is_template() {
            eprintln!("ICE: Template type in emit_type_name_ex");
        }
        if ty.name.data == Strings::EMPTY {
            eprintln!("ty is empty");
        }

        if let Some(name) = self.1.scopes.get(ut.id).attrs.link_name {
            return self.emit_str(name);
        }

        if min {
            write_de!(self, "t{}", ut.id);
            for &ty in ut.ty_args.values() {
                self.emit_mangled_name(ty, min);
            }
        } else if ty.kind.is_tuple() {
            write_de!(self, "L");
            for (_name, member) in ty.members.iter() {
                // TODO: tuple names
                let mut data = Buffer::new(self.1);
                data.emit_mangled_name(member.ty.with_templates(&self.1.types, &ut.ty_args), false);
                self.write_len_prefixed(&data.finish());
            }
        } else {
            write_de!(self, "T");
            self.scope_emit_mangled_name(ty.body_scope, &ut.ty_args);
        }
    }

    fn emit_array_struct_name(&mut self, ty: TypeId, size: usize, min: bool) {
        write_de!(self, "A{size}");
        self.emit_mangled_name(ty, min);
    }

    fn emit_fn_name(&mut self, func: &GenericFn, min: bool) {
        let f = &self.1.scopes.get(func.id);
        if let Some(name) = f.attrs.link_name {
            return self.emit_str(name);
        } else if f.is_extern && f.body.is_none() {
            return self.emit_str(f.name.data);
        }

        if min {
            write_de!(self, "p{}", func.id);
            for &ty in func.ty_args.values() {
                self.emit_mangled_name(ty, min);
            }
        } else {
            write_de!(self, "_CTL");
            self.scope_emit_mangled_name(f.body_scope, &func.ty_args);
        }
    }

    fn emit_vtable_prefix(&mut self, tr: UserTypeId, min: bool) {
        self.emit(if min { "v" } else { "$vtable_" });
        if min {
            write_de!(self, "t{tr}");
        } else {
            let ty = self.1.scopes.get(tr);
            self.emit(full_name(self.1, ty.scope, ty.name.data));
        }
        write_de!(self, "_");
    }

    fn emit_vtable_fn_name(&mut self, id: FunctionId, min: bool) {
        if min {
            write_de!(self, "p{id}");
        } else {
            let f = self.1.scopes.get(id);
            self.emit(full_name(self.1, f.scope, f.name.data));
        }
    }

    fn emit_dyn_fn_type(&mut self, func: &GenericFn, min: bool) {
        let func_data = self.1.scopes.get(func.id);
        let types = &self.1.types;
        let ret = func_data.ret.with_templates(types, &func.ty_args);
        if ret.is_void_like() {
            write_de!(self, "void");
        } else {
            self.emit_type(ret, min);
        }
        write_de!(self, "(*)(");
        for (i, param) in func_data.params.iter().enumerate() {
            let param_ty = param.ty.with_templates(types, &func.ty_args);
            if i > 0 {
                write_de!(self, ",");
                self.emit_type(param_ty, min);
            } else if types[param_ty].is_ptr() {
                write_de!(self, "void const*");
            } else {
                write_de!(self, "void*");
            }
        }
        write_de!(self, ")");
    }

    fn scope_emit_mangled_name(&mut self, id: ScopeId, ty_args: &TypeArgs) {
        let mut parts = vec![];
        let mut imp = None;
        for (_id, scope) in self.1.scopes.walk(id) {
            match &scope.kind {
                &ScopeKind::Function(id) => parts.push(self.raw_get_mangled_name(id, ty_args)),
                &ScopeKind::UserType(id) => {
                    let ut = self.1.scopes.get(id);
                    if let Some(imp) = imp.take().and_then(|i| ut.impls.get_checked(i)) {
                        let mut name = Buffer::new(self.1);
                        name.emit_type_name(&imp.with_templates(&self.1.types, ty_args), false);

                        let mut data = Buffer::new(self.1);
                        write_de!(data, "I");
                        data.write_len_prefixed(&name.finish());
                        parts.push(data.finish());
                    } else if let Some(this) =
                        ut.kind.as_trait().and_then(|(this, _, _)| ty_args.get(this))
                    {
                        let mut name = Buffer::new(self.1);
                        name.emit_mangled_name(*this, false);

                        let mut data = Buffer::new(self.1);
                        write_de!(data, "S");
                        data.write_len_prefixed(&name.finish());
                        parts.push(data.finish());
                    }
                    parts.push(self.raw_get_mangled_name(id, ty_args));
                }
                &ScopeKind::Impl(i) => imp = Some(i),
                ScopeKind::Module(name) => {
                    let mut data = Buffer::new(self.1);
                    data.write_len_prefixed(self.1.strings.resolve(&name.data));
                    parts.push(data.finish());
                }
                _ => {}
            }
        }

        for part in parts.iter().rev() {
            self.emit(part);
        }
    }

    fn raw_get_mangled_name<T: ItemId + std::fmt::Display>(
        &self,
        id: T,
        ty_args: &TypeArgs,
    ) -> String
    where
        T::Value: HasTypeParams,
    {
        let mut buffer = Buffer::new(self.1);

        let name = id.name(&self.1.scopes);
        let item = id.get(&self.1.scopes);

        let data = self.1.strings.resolve(&name.data);
        if !data.chars().all(|ch| ch.is_ascii_alphanumeric() || ch == '_') {
            let name = format!("{id}");
            buffer.write_len_prefixed(&name);
        } else if name.data != Strings::TUPLE_NAME {
            buffer.write_len_prefixed(data);
        }

        let mut wrote = false;
        for &ty in item.get_type_params().iter().flat_map(|id| ty_args.get(id)) {
            if !wrote {
                write_de!(buffer, "G");
            }

            let mut data = Buffer::new(self.1);
            data.emit_mangled_name(ty, false);
            buffer.write_len_prefixed(&data.finish());
            wrote = true;
        }

        if wrote {
            write_de!(buffer, "g");
        }
        buffer.finish()
    }

    fn write_len_prefixed(&mut self, data: &str) {
        write_de!(self, "{}{data}", data.len());
    }

    fn finish(self) -> String {
        self.0
    }
}

impl Write for Buffer<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write_str(s)
    }
}

struct StringLiteral<'a>(&'a str);

impl Display for StringLiteral<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "STRLIT(\"")?;
        for byte in self.0.as_bytes() {
            write!(f, "\\x{byte:x}")?;
        }
        write!(f, "\",{})", self.0.len())
    }
}

struct JoiningBuilder<'a> {
    buffer: Buffer<'a>,
    join: &'static str,
    default: &'static str,
}

impl<'a> JoiningBuilder<'a> {
    pub fn new(proj: &'a Project, join: &'static str, default: &'static str) -> Self {
        Self { buffer: Buffer::new(proj), join, default }
    }

    pub fn next(&mut self, f: impl FnOnce(&mut Buffer<'a>)) {
        if !self.buffer.0.is_empty() {
            self.buffer.emit(self.join);
        }
        f(&mut self.buffer);
    }

    pub fn next_str(&mut self, s: impl AsRef<str>) {
        self.next(|buf| buf.emit(s));
    }

    pub fn finish(self) -> String {
        if self.buffer.0.is_empty() { self.default.into() } else { self.buffer.finish() }
    }
}

macro_rules! hoist {
    ($self: expr, $body: expr) => {{
        let buffer = $self.buffer.take();
        let result = $body;
        $self.temporaries.emit(std::mem::replace(&mut $self.buffer, buffer).finish());
        result
    }};
}

macro_rules! never_expr {
    ($self: expr, $body: expr) => {{
        hoist!($self, $body);
        $self.buffer.emit(VOID_INSTANCE);
        $self.emitted_never_in_this_block = true;
    }};
}

macro_rules! tmpbuf {
    ($self: expr, $state: expr, |$tmp: ident| $body: expr) => {{
        hoist!($self, {
            let $tmp = $state.tmpvar();
            $body
        })
    }};
}

macro_rules! tmpbuf_emit {
    ($self: expr, $state: expr, |$tmp: ident| $body: expr) => {{
        let (tmp, result) = tmpbuf!($self, $state, |$tmp| {
            let result = $body;
            ($tmp, result)
        });
        $self.buffer.emit(&tmp);
        result
    }};
}

macro_rules! enter_block {
    ($self: expr, $scope: expr, $ty: expr, |$tmp: ident| $body: expr) => {{
        let ty = $ty;
        let scope = $scope;
        let old = std::mem::replace(&mut $self.cur_block, scope);
        let $tmp = format!("{SCOPE_VAR_PREFIX}{scope}");
        hoist!($self, {
            emit_type!($self, ty);
            write_de!($self.buffer, " {};", $tmp);
            $body;
        });

        $self.cur_block = old;
        $self.buffer.emit($tmp);
    }};
}

macro_rules! enter_loop {
    ($self: expr, $state: expr, $scope: expr, $ty: expr, |$tmp: ident| $body: expr) => {{
        let ty = $ty;
        let scope = $scope;
        let old_block = std::mem::replace(&mut $self.cur_block, scope);
        let old_loop = std::mem::replace(&mut $self.cur_loop, scope);
        let $tmp = format!("{SCOPE_VAR_PREFIX}{}", $self.cur_loop);
        hoist!($self, {
            $self.emit_type(ty);
            write_de!($self.buffer, " {};", $tmp);
            $body;
        });

        $self.cur_block = old_block;
        $self.cur_loop = old_loop;
        $self.buffer.emit($tmp);
    }};
}

macro_rules! hoist_point {
    ($self: expr, $body: expr) => {{
        let old_tmp = $self.temporaries.take();
        let old_buf = $self.buffer.take();
        $body;
        let written = std::mem::replace(&mut $self.buffer, old_buf).finish();
        $self.buffer.emit(std::mem::replace(&mut $self.temporaries, old_tmp).finish());
        $self.buffer.emit(written);
    }};
}

macro_rules! usebuf {
    ($self: expr, $buf: expr, $body: expr) => {{
        std::mem::swap(&mut $self.buffer, $buf);
        $body;
        std::mem::swap(&mut $self.buffer, $buf);
    }};
}

macro_rules! strdata {
    ($self: expr, $key: expr) => {{ $self.proj.strings.resolve(&$key) }};
}

macro_rules! emit_cast {
    ($self: expr, $id: expr) => {{
        let id = $id;
        write_de!($self.buffer, "(");
        $self.tg.add_type(&$self.proj.scopes, &$self.proj.types, id);
        $self.buffer.emit_type(id, $self.flags.minify);
        write_de!($self.buffer, ")");
    }};
}

macro_rules! emit_type {
    ($self: expr, $id: expr) => {{
        let id = $id;
        $self.tg.add_type(&$self.proj.scopes, &$self.proj.types, id);
        $self.buffer.emit_type(id, $self.flags.minify);
    }};
}

#[derive(PartialEq, Eq, Hash)]
struct Vtable {
    tr: GenericTrait,
    ty: TypeId,
    scope: ScopeId,
}

pub struct Codegen<'a> {
    proj: &'a Project,
    buffer: Buffer<'a>,
    temporaries: Buffer<'a>,
    funcs: HashSet<State>,
    statics: HashSet<VariableId>,
    emitted_never_in_this_block: bool,
    cur_block: ScopeId,
    cur_loop: ScopeId,
    flags: CodegenFlags,
    vtables: Buffer<'a>,
    emitted_vtables: HashSet<Vtable>,
    defers: Vec<(ScopeId, Vec<Expr>)>,
    tg: TypeGen,
    str_interp: StrInterp,
    source: CachingSourceProvider,
    ext_cache: ExtensionCache,
    arena: ExprArena,
}

impl<'a> Codegen<'a> {
    pub fn build(proj: &'a Project, arena: ExprArena) -> String {
        let mut this = Codegen {
            arena,
            str_interp: StrInterp::new(proj),
            flags: proj.conf.flags,
            proj,
            funcs: proj
                .scopes
                .functions()
                .filter(|(_, f)| f.is_extern && f.type_params.is_empty() && f.body.is_some())
                .map(|(id, _)| State::from_non_generic(id, &proj.scopes))
                .collect(),
            statics: proj
                .scopes
                .vars()
                .filter(|(_, v)| v.is_extern && v.value.is_some())
                .map(|(id, _)| id)
                .collect(),
            emitted_never_in_this_block: false,
            buffer: Buffer::new(proj),
            temporaries: Buffer::new(proj),
            vtables: Buffer::new(proj),
            cur_block: Default::default(),
            cur_loop: Default::default(),
            emitted_vtables: Default::default(),
            defers: Default::default(),
            tg: Default::default(),
            ext_cache: Default::default(),
            source: CachingSourceProvider::new(),
        };
        let main = this.gen_c_main();
        let mut static_defs = Buffer::new(proj);
        let mut static_init = Buffer::new(proj);
        let mut prototypes = Buffer::new(proj);
        let mut emitted_fns = HashSet::new();
        let mut emitted_statics = HashSet::new();
        let static_state = &mut State::new(
            GenericFn::from_id(&this.proj.scopes, FunctionId::RESERVED),
            ScopeId::ROOT,
        );

        while !this.funcs.is_empty() || !this.statics.is_empty() {
            let diff = this.funcs.difference(&emitted_fns).cloned().collect::<Vec<_>>();
            emitted_fns.extend(this.funcs.drain());

            for mut state in diff {
                this.emit_fn(&mut state, &mut prototypes);
            }

            for var in std::mem::take(&mut this.statics) {
                if emitted_statics.contains(&var) {
                    continue;
                }

                this.proj.static_deps.dfs(var, &mut emitted_statics, |id| {
                    static_state.caller = this.proj.scopes.get(id).scope;
                    usebuf!(this, &mut static_defs, {
                        this.emit_var_decl(id, static_state);
                        write_de!(this.buffer, ";");
                    });

                    if let Some(v) = this.proj.scopes.get(id).value {
                        usebuf!(
                            this,
                            &mut static_init,
                            hoist_point!(this, {
                                this.emit_var_name(id, static_state);
                                write_de!(this.buffer, "=");
                                this.emit_expr_inner(v, static_state);
                                write_de!(this.buffer, ";");
                            })
                        );
                    }
                });
            }
        }

        let functions = this.buffer.take();
        if this.flags.no_bit_int {
            this.buffer.emit("#define CTL_NOBITINT 1\n");
        }
        if this.proj.conf.has_feature(Strings::FEAT_HOSTED) {
            this.buffer.emit("#define CTL_HOSTED 1\n");
        }

        this.buffer.emit("#ifdef __clang__\n");
        let warnings = include_str!("../compile_flags.txt");
        for warning in warnings.split("\n").flat_map(|s| s.strip_prefix("-Wno-")) {
            write_de!(this.buffer, "#pragma clang diagnostic ignored \"-W{warning}\"\n");
        }
        this.buffer.emit("#endif\n");
        this.buffer.emit(include_str!("../ctl.h"));
        if let Some(ut) = &this.str_interp.string {
            this.tg.add_type(
                &this.proj.scopes,
                &this.proj.types,
                this.str_interp.string_ty.unwrap(),
            );
            this.buffer.emit("#define STRLIT(data,n)(");
            this.buffer.emit_type_name(ut, this.flags.minify);
            this.buffer.emit("){.span={.ptr=(u8*)data,.len=(usize)n}}\n");
        }

        this.tg.emit(&mut this.buffer, &this.flags);
        this.buffer.emit(prototypes.finish());
        this.buffer.emit(this.vtables.finish());
        this.buffer.emit(static_defs.finish());
        this.buffer.emit(functions.finish());
        this.buffer.emit("static void $ctl_static_init(void){");
        this.buffer.emit(static_init.finish());
        this.buffer.emit("}static void $ctl_static_deinit(void){}");
        if let Some(main) = main {
            this.buffer.emit(main);
        }

        this.buffer.finish()
    }

    fn emit_vtable(&mut self, vtable: Vtable) {
        if self.emitted_vtables.contains(&vtable) {
            return;
        }

        let mut buffer = Buffer::new(self.proj);
        usebuf!(self, &mut buffer, {
            write_de!(self.buffer, "static const VirtualFn ");
            self.emit_vtable_name(&vtable);
            write_de!(self.buffer, "[");
            self.buffer.emit_vtable_prefix(vtable.tr.id, self.flags.minify);
            write_de!(self.buffer, "{VTABLE_TRAIT_LEN}]={{");
            for tr in self.proj.scopes.walk_super_traits_ex(&self.proj.types, vtable.tr.clone()) {
                for f in vtable_methods(&self.proj.scopes, &self.proj.types, tr.id) {
                    write_de!(self.buffer, "[");
                    self.buffer.emit_vtable_prefix(vtable.tr.id, self.flags.minify);
                    self.buffer.emit_vtable_fn_name(f.id, self.flags.minify);
                    write_de!(self.buffer, "]=(VirtualFn)");
                    let func = self.find_implementation(
                        vtable.ty,
                        &tr,
                        self.proj.scopes.get(f.id).name.data,
                        vtable.scope,
                        |_, _| Default::default(),
                    );
                    self.buffer.emit_fn_name(&func, self.flags.minify);
                    write_de!(self.buffer, ",");
                    self.funcs.insert(State::new(func, vtable.scope));
                }
            }
            write_de!(self.buffer, "}};");
        });

        self.vtables.emit(buffer.finish());
        self.emitted_vtables.insert(vtable);
    }

    fn gen_c_main(&mut self) -> Option<String> {
        self.buffer.emit("int main(int argc, char **argv){CTL_ARGV=argv;CTL_ARGC=argc;");
        if self.proj.conf.has_feature(Strings::FEAT_TEST) {
            self.gen_test_main();
            return Some(self.buffer.take().finish());
        }

        let main = State::from_non_generic(self.proj.main?, &self.proj.scopes);

        let returns = self.proj.types[self.proj.scopes.get(main.func.id).ret].is_integral();
        if returns {
            self.buffer.emit("return ");
        }
        self.buffer.emit_fn_name(&main.func, self.flags.minify);
        if returns {
            self.buffer.emit("();}");
        } else {
            self.buffer.emit("();return 0;}");
        }
        self.funcs.insert(main);
        Some(self.buffer.take().finish())
    }

    fn gen_test_main(&mut self) {
        let dummy_state = &mut State::new(
            GenericFn::from_id(&self.proj.scopes, FunctionId::RESERVED),
            ScopeId::ROOT,
        );

        hoist_point!(self, {
            let test_info_id = self.proj.strings.get("test_info").unwrap();
            let test_info_ty = self.proj.scopes.lang_types.get(&test_info_id).unwrap();
            self.buffer.emit_type_name(
                &GenericUserType::new(*test_info_ty, TypeArgs::default()),
                self.flags.minify,
            );
            self.buffer.emit(" tests[]={");

            let runner = State::from_non_generic(self.proj.test_runner.unwrap(), &self.proj.scopes);
            let mut test_count = 0;
            for (id, func) in self.proj.scopes.functions().filter(|f| f.1.typ.is_test()) {
                let state = State::from_non_generic(id, &self.proj.scopes);
                write_de!(
                    self.buffer,
                    "{{.skip={},.name={},.module={},.test=",
                    func.attrs.test_skip.is_some() as usize,
                    StringLiteral(strdata!(self, func.name.data)),
                    StringLiteral(&full_name_pretty(self.proj, func.scope, true))
                );
                self.buffer.emit_fn_name(&state.func, self.flags.minify);

                write_de!(self.buffer, ",.skip_reason=");
                let skip_reason_ty = self
                    .proj
                    .scopes
                    .get(*test_info_ty)
                    .members
                    .get(&Strings::SKIP_REASON)
                    .unwrap()
                    .ty;
                let opt = if let Some(reason) = func.attrs.test_skip.flatten() {
                    let str_ty = self.str_interp.string_ty.unwrap();
                    let str = self.arena.typed(str_ty, ExprData::String(reason));
                    Expr::option_some(skip_reason_ty, str, &mut self.arena)
                } else {
                    Expr::option_null(skip_reason_ty, &mut self.arena)
                };
                self.emit_expr_inline(opt, dummy_state);
                write_de!(self.buffer, "}},");

                self.funcs.insert(state);
                test_count += 1;
            }
            self.buffer.emit("};");
            self.buffer.emit_fn_name(&runner.func, self.flags.minify);
            write_de!(self.buffer, "(");
            self.emit_cast(self.proj.scopes.get(runner.func.id).params[0].ty);
            write_de!(self.buffer, "{{.ptr=tests,.len={test_count}}});return 0;}}");
            self.funcs.insert(runner);
        });
    }

    fn emit_fn(&mut self, state: &mut State, prototypes: &mut Buffer<'a>) {
        // TODO: emit an error if a function has the c_macro attribute and a body
        let func = self.proj.scopes.get(state.func.id);
        if func.attrs.no_gen {
            return;
        }

        usebuf!(self, prototypes, {
            self.emit_prototype(state, true);
            write_de!(self.buffer, ";");
        });
        let func = self.proj.scopes.get(state.func.id);
        if let Some(body) = func.body {
            let void_return =
                func.ret.with_templates(&self.proj.types, &state.func.ty_args).is_void_like();
            let (unused, thisptr) = self.emit_prototype(state, false);
            write_de!(self.buffer, "{{");
            for id in unused {
                write_de!(self.buffer, "(void)");
                self.emit_var_name(id, state);
                write_de!(self.buffer, ";");
            }

            match thisptr {
                FirstParam::OverriddenLabel { tmp, label, ty } => {
                    self.emit_type(ty);
                    write_de!(self.buffer, " {}={tmp};", strdata!(self, label));
                }
                FirstParam::OverriddenVar { tmp, id } => {
                    self.emit_var_decl(id, state);
                    write_de!(self.buffer, "={tmp};");
                }
                _ => {}
            }

            for param in func.params.iter() {
                let Some(patt) = param
                    .patt
                    .as_checked()
                    .filter(|patt| !matches!(patt.data, PatternData::Variable(_)))
                else {
                    continue;
                };

                let ty = param.ty.with_templates(&self.proj.types, &state.func.ty_args);
                self.emit_pattern_bindings(state, &patt.data, strdata!(self, param.label), ty);
            }

            hoist_point!(self, {
                if void_return {
                    self.emit_expr_stmt(body, state);
                    write_de!(self.buffer, "}}");
                } else {
                    write_de!(self.buffer, "return ");
                    self.emit_expr_inline(body, state);
                    write_de!(self.buffer, ";}}");
                }
            });
        }
    }

    fn emit_expr_stmt(&mut self, expr: Expr, state: &mut State) {
        if self.has_side_effects(&expr) && !expr.ty.is_void_like() {
            self.emit_expr_inline(expr, state);
            write_de!(self.buffer, ";");
        } else {
            write_de!(self.buffer, "(void)(");
            self.emit_expr_inline(expr, state);
            write_de!(self.buffer, ");");
        }
    }

    fn emit_stmt(&mut self, stmt: Stmt, state: &mut State) {
        match stmt {
            Stmt::Expr(expr) => hoist_point!(self, self.emit_expr_stmt(expr, state)),
            Stmt::Let(patt, value) => hoist_point!(self, {
                if let PatternData::Variable(id) = patt.data {
                    if !self.proj.scopes.get(id).unused {
                        let ty = self.emit_var_decl(id, state);
                        if let Some(mut expr) = value {
                            expr.ty = ty;
                            write_de!(self.buffer, "=");
                            self.emit_expr_inner(expr, state);
                        }
                        write_de!(self.buffer, ";");
                    } else if let Some(expr) = value {
                        self.emit_expr_stmt(expr, state);
                    }
                } else if let Some(mut value) = value {
                    let ty = value.ty.with_templates(&self.proj.types, &state.func.ty_args);
                    value.ty = ty;
                    let tmp = self.emit_tmpvar(value, state);
                    self.emit_pattern_bindings(state, &patt.data, &tmp, ty);
                }
            }),
            Stmt::Defer(expr) => self.defers.last_mut().unwrap().1.push(expr),
            Stmt::Guard { cond, body } => hoist_point!(self, {
                write_de!(self.buffer, "if(!(");
                self.emit_expr_inline(cond, state);
                write_de!(self.buffer, ")) {{");
                hoist_point!(self, self.emit_expr_stmt(body, state));
                write_de!(self.buffer, "}}");
            }),
            Stmt::None => {}
        }
    }

    fn emit_expr(&mut self, mut expr: Expr, state: &mut State) {
        expr.ty = expr.ty.with_templates(&self.proj.types, &state.func.ty_args);
        if self.has_side_effects(&expr) {
            self.emit_tmpvar_ident(expr, state);
        } else {
            self.emit_expr_inner(expr, state);
        }
    }

    fn emit_expr_inner(&mut self, expr: Expr, state: &mut State) {
        match self.arena.get(expr.data) {
            &ExprData::Binary(op, lhs, rhs) => self.emit_binary(state, op, expr.ty, lhs, rhs),
            &ExprData::Unary(op, inner) => self.emit_unary(state, op, expr.ty, inner),
            &ExprData::Deref(inner, count) => match &self.proj.types[expr.ty] {
                &Type::Array(_, _) => {
                    let (sz, _) = expr.ty.size_and_align(&self.proj.scopes, &self.proj.types);
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(expr.ty);
                        write_de!(self.buffer, " {tmp};CTL_MEMCPY(&{tmp},({:*<1$}", "", count - 1);
                        self.emit_expr_inline(inner, state);
                        write_de!(self.buffer, "),{sz});");
                    });
                }
                Type::Void => {
                    write_de!(self.buffer, "VOID(");
                    self.emit_expr(inner, state);
                    write_de!(self.buffer, ")");
                }
                _ => {
                    write_de!(self.buffer, "({:*<1$}", "", count);
                    self.emit_expr(inner, state);
                    write_de!(self.buffer, ")");
                }
            },
            &ExprData::DynCoerce(mut inner, scope) => {
                inner.ty = inner.ty.with_templates(&self.proj.types, &state.func.ty_args);

                let to_tr = self
                    .proj
                    .types
                    .get(expr.ty)
                    .as_dyn_pointee()
                    .expect("ICE: DynCoerce to non dyn pointer")
                    .clone();

                self.emit_cast(expr.ty);
                if let &Type::Ptr(from) | &Type::MutPtr(from) = &self.proj.types[inner.ty] {
                    write_de!(self.buffer, "{{.self=");
                    self.emit_expr(inner, state);
                    write_de!(self.buffer, ",.vtable=");
                    let vtable = Vtable { tr: to_tr, ty: from, scope };

                    self.emit_vtable_name(&vtable);
                    self.emit_vtable(vtable);
                    write_de!(self.buffer, "}}");
                } else if let Type::DynPtr(tr) | Type::DynMutPtr(tr) = &self.proj.types[inner.ty] {
                    let from_tr_id = tr.id;
                    let recv = hoist!(self, self.emit_tmpvar(inner, state));
                    if from_tr_id != to_tr.id {
                        write_de!(self.buffer, "{{.self={recv}.self,.vtable=&{recv}.vtable[");
                        self.buffer.emit_vtable_prefix(from_tr_id, self.flags.minify);
                        self.buffer.emit_vtable_prefix(to_tr.id, self.flags.minify);
                        write_de!(self.buffer, "{VTABLE_TRAIT_START}]}}");
                    } else {
                        write_de!(self.buffer, "{{.self={recv}.self,.vtable={recv}.vtable}}");
                    }
                } else {
                    panic!("ICE: DynCoerce from non-pointer");
                };
            }
            ExprData::Call { callee, args, scope, span } => {
                let func = self.proj.types[callee.ty].as_fn().unwrap();
                let args = args.clone();
                if let Some(name) = self.proj.scopes.intrinsic_name(func.id) {
                    let func = func.with_templates(&self.proj.types, &state.func.ty_args);
                    return self.emit_intrinsic(name, expr.ty, &func, args, state, (*scope, *span));
                } else if let Some(id) = self.proj.scopes.get(func.id).constructor {
                    if self.proj.scopes.get(id).kind.is_union() {
                        return self.emit_variant_instance(
                            state,
                            expr.ty,
                            self.proj.scopes.get(func.id).name.data,
                            args,
                        );
                    } else {
                        return self.emit_instance(state, expr.ty, args);
                    }
                }

                if expr.ty.is_void_like() {
                    write_de!(self.buffer, "VOID(");
                }
                self.emit_expr(*callee, state);
                write_de!(self.buffer, "(");
                self.finish_emit_fn_args(state, func.id, args);
                if expr.ty.is_void_like() {
                    write_de!(self.buffer, ")");
                }
            }
            ExprData::CallDyn(func, args) => {
                let mut args = args.clone();
                if expr.ty.is_void_like() {
                    write_de!(self.buffer, "VOID(");
                }
                let (_, recv) = args.shift_remove_index(0).unwrap();
                let tr = self.proj.types[recv.ty].as_dyn_pointee().unwrap().id;

                write_de!(self.buffer, "((");
                self.buffer.emit_dyn_fn_type(func, self.flags.minify);
                let id = func.id;
                let recv = hoist!(self, self.emit_tmpvar(recv, state));
                write_de!(self.buffer, "){recv}.vtable[");
                self.buffer.emit_vtable_prefix(tr, self.flags.minify);
                self.buffer.emit_vtable_fn_name(id, self.flags.minify);
                write_de!(self.buffer, "])({recv}.self");
                if args.is_empty() {
                    write_de!(self.buffer, ")");
                } else {
                    write_de!(self.buffer, ",");
                    self.finish_emit_fn_args(state, id, args);
                }
                if expr.ty.is_void_like() {
                    write_de!(self.buffer, ")");
                }
            }
            ExprData::CallFnPtr(inner, args) => {
                let args = args.clone();
                if expr.ty.is_void_like() {
                    write_de!(self.buffer, "VOID(");
                }
                write_de!(self.buffer, "(");
                self.emit_expr(*inner, state);
                write_de!(self.buffer, ")(");
                for (i, arg) in args.into_iter().enumerate() {
                    if i > 0 {
                        write_de!(self.buffer, ",");
                    }

                    self.emit_expr(arg, state);
                }
                write_de!(self.buffer, ")");
                if expr.ty.is_void_like() {
                    write_de!(self.buffer, ")");
                }
            }
            ExprData::Array(args) => {
                let args = args.clone();
                write_de!(self.buffer, "(");
                self.emit_type(expr.ty);
                write_de!(self.buffer, "){{.{ARRAY_DATA_NAME}={{");
                for expr in args {
                    self.emit_expr(expr, state);
                    write_de!(self.buffer, ",");
                }
                write_de!(self.buffer, "}}}}");
            }
            &ExprData::ArrayWithInit { init, count } => {
                // number chosen arbitrarily
                if count <= 32 {
                    write_de!(self.buffer, "(");
                    self.emit_type(expr.ty);
                    write_de!(self.buffer, "){{.{ARRAY_DATA_NAME}={{");
                    for _ in 0..count {
                        self.emit_expr(init, state);
                        write_de!(self.buffer, ",");
                    }
                    write_de!(self.buffer, "}}}}");
                } else {
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(expr.ty);
                        write_de!(
                            self.buffer,
                            " {tmp}; for(usize i=0;i<{count};i++){{{tmp}.{ARRAY_DATA_NAME}[i]="
                        );
                        self.emit_expr_inline(init, state);
                        write_de!(self.buffer, ";}}");
                    })
                }
            }
            ExprData::Vec(args) => {
                let ut = self.proj.types[expr.ty].as_user().unwrap();
                if args.is_empty() {
                    return self.emit_new(ut);
                }

                let args = args.clone();
                tmpbuf_emit!(self, state, |tmp| {
                    let len = args.len();
                    self.emit_with_capacity(expr.ty, &tmp, ut, len);
                    for (i, expr) in args.into_iter().enumerate() {
                        write_de!(self.buffer, "{tmp}.ptr[{i}]=");
                        self.emit_expr_inline(expr, state);
                        write_de!(self.buffer, ";");
                    }
                    write_de!(self.buffer, "{tmp}.len={len};");
                });
            }
            &ExprData::VecWithInit { init, count } => {
                tmpbuf_emit!(self, state, |tmp| {
                    let ut = self.proj.types[expr.ty].as_user().unwrap();
                    let len = self.emit_tmpvar(count, state);
                    self.emit_with_capacity(expr.ty, &tmp, ut, &len);
                    write_de!(self.buffer, "for(usize i=0;i<{len};i++){{");
                    hoist_point!(self, {
                        write_de!(self.buffer, "((");
                        self.emit_type(ut.first_type_arg().unwrap());
                        write_de!(self.buffer, "*){tmp}.ptr)[i]=");
                        self.emit_expr_inline(init, state);
                        write_de!(self.buffer, ";}}{tmp}.len={len};");
                    });
                });
            }
            &ExprData::Set(ref args, scope) => {
                let ut = self.proj.types[expr.ty].as_user().unwrap();
                if args.is_empty() {
                    return self.emit_new(ut);
                }

                let args = args.clone();
                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_with_capacity(expr.ty, &tmp, ut, args.len());
                    let insert = State::with_inst(
                        GenericFn::from_id(
                            &self.proj.scopes,
                            self.proj
                                .scopes
                                .get(ut.id)
                                .find_associated_fn(&self.proj.scopes, Strings::FN_INSERT)
                                .unwrap(),
                        ),
                        &self.proj.types,
                        expr.ty,
                        scope,
                    );

                    for val in args {
                        self.buffer.emit_fn_name(&insert.func, self.flags.minify);
                        write_de!(self.buffer, "(&{tmp},");
                        self.emit_expr_inline(val, state);
                        write_de!(self.buffer, ");");
                    }
                    self.funcs.insert(insert);
                });
            }
            ExprData::Map(args, scope) => {
                let ut = self.proj.types[expr.ty].as_user().unwrap();
                if args.is_empty() {
                    return self.emit_new(ut);
                }

                let args = args.clone();
                tmpbuf_emit!(self, state, |tmp| {
                    let insert = State::with_inst(
                        GenericFn::from_id(
                            &self.proj.scopes,
                            self.proj
                                .scopes
                                .get(ut.id)
                                .find_associated_fn(&self.proj.scopes, Strings::FN_INSERT)
                                .unwrap(),
                        ),
                        &self.proj.types,
                        expr.ty,
                        *scope,
                    );

                    self.emit_with_capacity(expr.ty, &tmp, ut, args.len());
                    for (key, val) in args {
                        self.buffer.emit_fn_name(&insert.func, self.flags.minify);
                        write_de!(self.buffer, "(&{tmp},");
                        self.emit_expr(key, state);
                        write_de!(self.buffer, ",");
                        self.emit_expr(val, state);
                        write_de!(self.buffer, ");");
                    }
                    self.funcs.insert(insert);
                });
            }
            ExprData::Int(value) => self.emit_literal(value.clone(), expr.ty),
            &ExprData::Float(value) => {
                if expr.ty == TypeId::F32 {
                    let value = value as f32;
                    write_de!(self.buffer, "CTL_IEEE_F32({:#x}u)", value.to_bits());
                } else if expr.ty == TypeId::F64 {
                    write_de!(self.buffer, "CTL_IEEE_F64({:#x}ull)", value.to_bits());
                } else {
                    panic!("ICE: attempt to generate float of type {}", self.proj.fmt_ty(expr.ty))
                }
            }
            &ExprData::String(v) => write_de!(self.buffer, "{}", StringLiteral(strdata!(self, v))),
            ExprData::GeneratedString(v) => write_de!(self.buffer, "{}", StringLiteral(v)),
            ExprData::ByteString(value) => {
                emit_cast!(self, expr.ty);
                write_de!(self.buffer, "\"");
                for byte in value {
                    write_de!(self.buffer, "\\x{byte:x}");
                }
                write_de!(self.buffer, "\"");
            }
            ExprData::Void => self.buffer.emit(VOID_INSTANCE),
            &ExprData::Fn(ref func, scope) => {
                let func = func.with_templates(&self.proj.types, &state.func.ty_args);
                self.buffer.emit_fn_name(&func, self.flags.minify);
                self.funcs.insert(State::new(func, scope));
            }
            ExprData::MemFn(mfn, scope) => self.emit_member_fn(state, mfn.clone(), *scope),
            &ExprData::Var(id) => {
                let var = self.proj.scopes.get(id);
                match var.kind {
                    VariableKind::Static => _ = self.statics.insert(id),
                    VariableKind::Const => {
                        return self.emit_expr(
                            var.value.expect("ICE: emitting const with no value"),
                            state,
                        );
                    }
                    VariableKind::Normal => {}
                }
                self.emit_var_name(id, state);
            }
            ExprData::Instance(members) => self.emit_instance(state, expr.ty, members.clone()),
            ExprData::VariantInstance(name, members) => {
                self.emit_variant_instance(state, expr.ty, *name, members.clone())
            }
            &ExprData::Member { source, member } => {
                let id = self.proj.types[source.ty].as_user().map(|ut| ut.id);
                if let Some(id) = id.filter(|&id| self.proj.scopes.get(id).kind.is_packed_struct())
                {
                    let tmp = tmpbuf!(self, state, |tmp| {
                        let ptr = self.proj.types.insert(Type::Ptr(source.ty));
                        self.emit_type(ptr);
                        write_de!(self.buffer, " {tmp}=&");
                        self.emit_expr_inline(source, state);
                        write_de!(self.buffer, ";");
                        tmp
                    });
                    self.emit_bitfield_read(&format!("(*{tmp})"), id, member, expr.ty);
                } else {
                    self.emit_expr(source, state);
                    write_de!(self.buffer, ".{}", member_name(self.proj, id, member));
                }
            }
            ExprData::Block(block) => enter_block!(self, block.scope, expr.ty, |name| {
                let yields = block.is_yielding(&self.proj.scopes);
                write_nm!(self, "{{");
                self.emit_block(block.clone(), state);
                if !yields {
                    write_de!(self.buffer, "{name}={VOID_INSTANCE};");
                } else {
                    write_de!(self.buffer, "{name}:;");
                }
                write_nm!(self, "}}");
            }),
            &ExprData::If { cond, if_branch, else_branch, dummy_scope } => {
                enter_block!(self, dummy_scope, expr.ty, |name| {
                    write_de!(self.buffer, "if(");
                    self.emit_expr_inline(cond, state);
                    write_de!(self.buffer, "){{");
                    hoist_point!(self, {
                        write_de!(self.buffer, "{name}=");
                        self.emit_expr_inline(if_branch, state);
                    });
                    write_de!(self.buffer, ";}}else{{");
                    hoist_point!(self, {
                        write_de!(self.buffer, "{name}=");
                        self.emit_expr_inline(else_branch, state);
                    });
                    write_de!(self.buffer, ";}}");
                })
            }
            &ExprData::Loop { cond, ref body, do_while, optional } => {
                let scope = body.scope;
                let body = body.clone();
                enter_loop!(self, state, scope, expr.ty, |name| {
                    macro_rules! cond {
                        ($cond: expr) => {
                            hoist_point!(self, {
                                write_de!(self.buffer, "if(!");
                                self.emit_expr_inner($cond, state);
                                write_de!(self.buffer, "){{");
                                self.emit_loop_break(state, expr.ty, optional);
                                write_de!(self.buffer, "}}");
                            });
                        };
                    }

                    write_de!(self.buffer, "for(;;){{");
                    hoist_point!(self, {
                        if let Some(mut cond) = cond {
                            cond.ty = cond.ty.with_templates(&self.proj.types, &state.func.ty_args);
                            if !do_while {
                                cond!(cond);
                                self.emit_block(body, state);
                            } else {
                                self.emit_block(body, state);
                                cond!(cond);
                            }
                        } else {
                            self.emit_block(body, state);
                        }
                    });
                    write_de!(self.buffer, "{LOOP_CONT_PREFIX}{}:;}}", self.cur_loop);
                    if self.proj.scopes[scope].kind.as_loop().unwrap().breaks != LoopBreak::None {
                        write_de!(self.buffer, "{name}:;");
                    }
                });
            }
            &ExprData::Return(mut expr) => never_expr!(self, {
                expr.ty = expr.ty.with_templates(&self.proj.types, &state.func.ty_args);
                let void = expr.ty.is_void_like();
                let tmp = self.emit_tmpvar(expr, state);
                let str = if void {
                    write_de!(self.buffer, "(void){tmp};");
                    "return".into()
                } else {
                    format!("return {tmp}")
                };
                self.leave_scope(state, &str, self.proj.scopes.get(state.func.id).body_scope);
            }),
            &ExprData::Yield(expr, scope) => never_expr!(self, {
                write_de!(self.buffer, "{SCOPE_VAR_PREFIX}{scope}=");
                self.emit_expr_inline(expr, state);
                write_de!(self.buffer, ";");

                // if scope != self.cur_block {
                self.leave_scope(state, &format!("goto {SCOPE_VAR_PREFIX}{scope}"), scope);
                // }
            }),
            &ExprData::Break(expr, scope) => never_expr!(self, {
                write_de!(self.buffer, "{SCOPE_VAR_PREFIX}{scope}=");
                self.emit_expr_inline(expr, state);
                write_de!(self.buffer, ";");
                if self.cur_loop == scope {
                    self.leave_scope(state, "break", scope);
                } else {
                    self.leave_scope(state, &format!("goto {SCOPE_VAR_PREFIX}{scope}"), scope);
                }
            }),
            &ExprData::Continue(scope) => never_expr!(self, {
                if self.cur_loop == scope {
                    self.leave_scope(state, "continue", scope);
                } else {
                    self.leave_scope(state, &format!("goto {LOOP_CONT_PREFIX}{scope}"), scope);
                }
            }),
            &ExprData::Match { mut scrutinee, ref body, dummy_scope } => {
                let body = body.clone();
                enter_block!(self, dummy_scope, expr.ty, |name| {
                    scrutinee.ty =
                        scrutinee.ty.with_templates(&self.proj.types, &state.func.ty_args);

                    let ty = scrutinee.ty;
                    let tmp = self.emit_tmpvar(scrutinee, state);
                    for (i, (patt, expr)) in body.into_iter().enumerate() {
                        if i > 0 {
                            write_de!(self.buffer, "else ");
                        }

                        self.emit_pattern_if_stmt(state, &patt.data, &tmp, ty);

                        hoist_point!(self, {
                            write_de!(self.buffer, "{name}=");
                            self.emit_expr_inline(expr, state);
                            write_de!(self.buffer, ";}}");
                        });
                        self.emitted_never_in_this_block = false;
                    }

                    write_de!(self.buffer, "else{{CTL_UNREACHABLE();}}");
                })
            }
            &ExprData::As(mut inner, _) => {
                inner.ty = inner.ty.with_templates(&self.proj.types, &state.func.ty_args);
                // enum tag cast
                self.emit_cast(expr.ty);
                write_de!(self.buffer, "(");
                if self.proj.types[inner.ty].is_user() {
                    self.emit_expr(inner, state);
                    write_de!(self.buffer, ").{UNION_TAG_NAME}");
                } else {
                    self.emit_expr(inner, state);
                    write_de!(self.buffer, ")");
                }
            }
            &ExprData::Is(mut inner, ref patt) => {
                let patt = patt.clone();
                inner.ty = inner.ty.with_templates(&self.proj.types, &state.func.ty_args);
                let ty = inner.ty;
                let tmp = hoist!(self, self.emit_tmpvar(inner, state));
                let (bindings, conditions) = self.emit_pattern(state, &patt.data, &tmp, ty);
                hoist!(self, self.buffer.emit(bindings.finish()));
                write_de!(self.buffer, "({})", conditions.finish());
            }
            ExprData::Lambda(_) => todo!(),
            &ExprData::NeverCoerce(inner) => {
                if matches!(expr.ty, TypeId::VOID) {
                    self.emit_expr_inline(inner, state);
                } else {
                    write_de!(self.buffer, "COERCE(");
                    self.emit_type(expr.ty);
                    write_de!(self.buffer, ", ");
                    self.emit_expr_inline(inner, state);
                    write_de!(self.buffer, ")");
                }
            }
            &ExprData::SpanMutCoerce(mut inner) => {
                inner.ty = inner.ty.with_templates(&self.proj.types, &state.func.ty_args);
                let tmp = hoist!(self, self.emit_tmpvar(inner, state));
                self.emit_cast(expr.ty);
                write_de!(self.buffer, "{{.ptr={tmp}.ptr,.len={tmp}.len}}");
            }
            &ExprData::StringInterp { ref strings, ref args, scope } => {
                self.emit_string_interp(expr.ty, state, strings.clone(), args.clone(), scope)
            }
            &ExprData::AffixOperator { callee, ref mfn, param, scope, postfix, span } => {
                let mfn = mfn.clone();
                let deref = "*".repeat(callee.ty.strip_references_ex(&self.proj.types).1);
                if !postfix {
                    hoist!(self, {
                        let expr = Expr::member_call(
                            self.proj
                                .scopes
                                .get(mfn.func.id)
                                .ret
                                .with_templates(&self.proj.types, &state.func.ty_args),
                            &self.proj.types,
                            mfn,
                            [(param, callee)].into(),
                            scope,
                            span,
                            &mut self.arena,
                        );
                        self.emit_expr_stmt(expr, state);
                    });
                    write_de!(self.buffer, "({deref}");
                    self.emit_expr(callee, state);
                    write_de!(self.buffer, ")");
                } else {
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(expr.ty);
                        write_de!(self.buffer, " {tmp}={deref}");
                        self.emit_expr_inline(callee, state);
                        write_de!(self.buffer, ";");

                        let expr = Expr::member_call(
                            self.proj
                                .scopes
                                .get(mfn.func.id)
                                .ret
                                .with_templates(&self.proj.types, &state.func.ty_args),
                            &self.proj.types,
                            mfn,
                            [(param, callee)].into(),
                            scope,
                            span,
                            &mut self.arena,
                        );
                        self.emit_expr_stmt(expr, state);
                    });
                }
            }
            &ExprData::Discard(inner) => {
                self.buffer.emit("VOID(");
                self.emit_expr(inner, state);
                self.buffer.emit(")");
            }
            ExprData::Error => panic!("ICE: ExprData::Error in gen_expr"),
        }
    }

    fn emit_expr_inline(&mut self, mut expr: Expr, state: &mut State) {
        expr.ty = expr.ty.with_templates(&self.proj.types, &state.func.ty_args);
        self.emit_expr_inner(expr, state);
    }

    fn emit_member_fn(&mut self, state: &mut State, mut mfn: MemberFn, scope: ScopeId) {
        if let MemberFnType::Trait(mut tr) = mfn.typ {
            let inst = mfn.inst.with_templates(&self.proj.types, &state.func.ty_args);
            tr.fill_templates(&self.proj.types, &state.func.ty_args);
            mfn.func = self.find_implementation(
                inst,
                &tr,
                self.proj.scopes.get(mfn.func.id).name.data,
                state.caller,
                |proj, id| {
                    TypeArgs::in_order(&proj.scopes, id, mfn.func.ty_args.0.iter().map(|kv| *kv.1))
                },
            );
        }

        mfn.func.fill_templates(&self.proj.types, &state.func.ty_args);
        self.buffer.emit_fn_name(&mfn.func, self.flags.minify);
        self.funcs.insert(State::new(mfn.func, scope));
    }

    fn emit_instance(&mut self, state: &mut State, ty: TypeId, members: IndexMap<StrId, Expr>) {
        let ut_id = self.proj.types[ty].as_user().unwrap().id;
        if self.proj.scopes.get(ut_id).kind.is_packed_struct() {
            return tmpbuf_emit!(self, state, |tmp| {
                self.emit_type(ty);
                write_de!(self.buffer, " {tmp} = {{}};");
                for (name, value) in members {
                    let ty = value.ty;
                    let expr = hoist!(self, self.emit_tmpvar(value, state));
                    self.emit_bitfield_write(&tmp, ut_id, name, ty, &expr);
                }
            });
        }

        self.emit_cast(ty);
        write_de!(self.buffer, "{{");
        if members.is_empty() {
            write_de!(self.buffer, "CTL_DUMMY_INIT");
        }

        for (name, mut value) in members {
            value.ty = value.ty.with_templates(&self.proj.types, &state.func.ty_args);
            write_de!(self.buffer, ".{}=", member_name(self.proj, Some(ut_id), name));
            self.emit_expr(value, state);
            write_de!(self.buffer, ",");
        }
        write_de!(self.buffer, "}}");
    }

    fn emit_variant_instance(
        &mut self,
        state: &mut State,
        ty: TypeId,
        variant: StrId,
        mut members: IndexMap<StrId, Expr>,
    ) {
        if ty.can_omit_tag(&self.proj.scopes, &self.proj.types).is_some() {
            if let Some(some) = members.swap_remove(&Strings::TUPLE_ZERO) {
                self.emit_expr(some, state);
            } else {
                self.buffer.emit(NULLPTR);
            }
        } else {
            self.emit_cast(ty);
            let members: Vec<_> = members
                .into_iter()
                .map(|(name, mut expr)| {
                    expr.ty = expr.ty.with_templates(&self.proj.types, &state.func.ty_args);
                    // TODO: dont emit temporaries for expressions that cant have side effects
                    (name, hoist!(self, self.emit_tmpvar(expr, state)))
                })
                .collect();

            let ut_id = self.proj.types[ty].as_user().unwrap().id;
            write_de!(self.buffer, "{{");
            let ut = self.proj.scopes.get(ut_id);
            let union = ut.kind.as_union().unwrap();
            for (name, value) in members.iter().filter(|(name, _)| ut.members.contains_key(name)) {
                write_de!(self.buffer, ".{}={value},", member_name(self.proj, Some(ut_id), *name));
            }

            if union.variants.get(&variant).is_some_and(|v| v.ty.is_some()) {
                write_de!(self.buffer, ".{}={{", member_name(self.proj, Some(ut_id), variant));
                for (name, value) in
                    members.iter().filter(|(name, _)| !ut.members.contains_key(name))
                {
                    write_de!(
                        self.buffer,
                        ".{}={value},",
                        member_name(self.proj, Some(ut_id), *name)
                    );
                }
                write_de!(self.buffer, "}},");
            }

            write_de!(self.buffer, ".{UNION_TAG_NAME}=");
            self.emit_literal(union.discriminant(variant).unwrap().clone(), union.tag);
            write_de!(self.buffer, "}}");
        }
    }

    fn emit_binary(
        &mut self,
        state: &mut State,
        op: BinaryOp,
        ret: TypeId,
        mut lhs: Expr,
        rhs: Expr,
    ) {
        lhs.ty = lhs.ty.with_templates(&self.proj.types, &state.func.ty_args);
        match op {
            BinaryOp::NoneCoalesceAssign => {
                let opt_type = lhs.ty;
                let tag = if opt_type.can_omit_tag(&self.proj.scopes, &self.proj.types).is_some() {
                    ""
                } else {
                    ".Some.$0"
                };
                let mut left = Buffer::new(self.proj);
                usebuf!(self, &mut left, self.emit_expr_inner(lhs, state));
                let left = left.finish();

                hoist!(self, {
                    self.emit_pattern_if_stmt(state, &null_variant(ret), &left, opt_type);
                    hoist_point!(self, {
                        write_de!(self.buffer, "{left}{tag}=");
                        let prev = self.emitted_never_in_this_block;
                        self.emit_expr_inline(rhs, state);
                        self.emitted_never_in_this_block = prev;
                        if opt_type.can_omit_tag(&self.proj.scopes, &self.proj.types).is_none() {
                            write_de!(self.buffer, ";");
                            let tag = self
                                .proj
                                .scopes
                                .get(self.proj.types[opt_type].as_user().unwrap().id)
                                .kind
                                .as_union()
                                .unwrap()
                                .discriminant(Strings::SOME)
                                .unwrap();
                            write_de!(self.buffer, "{left}.{UNION_TAG_NAME}={tag}");
                        }
                        write_de!(self.buffer, ";}}");
                    });
                });

                self.buffer.emit(VOID_INSTANCE);
            }
            BinaryOp::NoneCoalesce => {
                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_type(ret);
                    write_de!(self.buffer, " {tmp};");

                    let opt_type = lhs.ty;
                    let name = hoist!(self, self.emit_tmpvar(lhs, state));
                    self.emit_pattern_if_stmt(state, &null_variant(ret), &name, opt_type);

                    let prev = self.emitted_never_in_this_block;
                    hoist_point!(self, {
                        write_de!(self.buffer, "{tmp}=");
                        self.emit_expr_inline(rhs, state);
                    });
                    self.emitted_never_in_this_block = prev;
                    let tag =
                        if opt_type.can_omit_tag(&self.proj.scopes, &self.proj.types).is_some() {
                            ""
                        } else {
                            ".Some.$0"
                        };
                    write_de!(self.buffer, ";}}else{{{tmp}={name}{tag};}}");
                });
            }
            BinaryOp::Cmp => {
                let mut rhs = rhs;
                rhs.ty = rhs.ty.with_templates(&self.proj.types, &state.func.ty_args);

                let (lhs, rhs) =
                    hoist!(self, (self.emit_tmpvar(lhs, state), self.emit_tmpvar(rhs, state)));
                let union = self
                    .proj
                    .scopes
                    .get(self.proj.types[ret].as_user().unwrap().id)
                    .kind
                    .as_union()
                    .unwrap();
                let tag = union.tag;
                let less = union.discriminant(Strings::VAR_LESS).unwrap().clone();
                let greater = union.discriminant(Strings::VAR_GREATER).unwrap().clone();
                let equal = union.discriminant(Strings::VAR_EQUAL).unwrap().clone();

                write_de!(self.buffer, "({lhs}<{rhs}?");
                self.emit_cast(ret);
                write_de!(self.buffer, "{{.{UNION_TAG_NAME}=");
                self.emit_literal(less, tag);
                write_de!(self.buffer, "}}:({lhs}>{rhs}?");
                self.emit_cast(ret);
                write_de!(self.buffer, "{{.{UNION_TAG_NAME}=");
                self.emit_literal(greater, tag);
                write_de!(self.buffer, "}}:");
                self.emit_cast(ret);
                write_de!(self.buffer, "{{.{UNION_TAG_NAME}=");
                self.emit_literal(equal, tag);
                write_de!(self.buffer, "}}))");
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                tmpbuf_emit!(self, state, |tmp| {
                    let lor = matches!(op, BinaryOp::LogicalOr);
                    let end_label = state.tmpvar();

                    self.emit_type(TypeId::BOOL);
                    write_de!(self.buffer, " {tmp}=");
                    self.emit_expr_inline(lhs, state);
                    write_de!(
                        self.buffer,
                        ";if({}{tmp}){{goto {end_label};}}",
                        if lor { "" } else { "!" }
                    );
                    hoist_point!(self, {
                        write_de!(self.buffer, "{tmp}=");
                        self.emit_expr_inline(rhs, state);
                        write_de!(self.buffer, ";{end_label}:;");
                    });
                });
            }
            _ => {
                if ret == TypeId::BOOL && lhs.ty != TypeId::BOOL {
                    self.emit_cast(ret);
                }
                if op.is_assignment() {
                    if lhs.ty.is_void_like() {
                        hoist!(self, {
                            self.emit_expr(lhs, state);
                            write_de!(self.buffer, ";");
                        });
                        self.emit_expr(rhs, state);
                        return;
                    }

                    if self.proj.types[lhs.ty].is_array()
                        && let &ExprData::Deref(expr, count) = self.arena.get(lhs.data)
                    {
                        return self.emit_array_write_to_deref(state, lhs.ty, expr, count, rhs);
                    } else if let ExprData::Member { source, member, .. } = self.arena.get(lhs.data)
                        && source.ty.is_packed_struct(self.proj)
                    {
                        return self.emit_bitfield_assign(*source, state, *member, lhs.ty, rhs, op);
                    }

                    write_de!(self.buffer, "VOID(");
                }
                write_de!(self.buffer, "(");
                self.emit_expr(lhs, state);
                write_de!(self.buffer, "{op}");
                self.emit_expr(rhs, state);
                write_de!(self.buffer, ")");
                if op.is_assignment() {
                    write_de!(self.buffer, ")");
                }
            }
        }
    }

    fn emit_unary(&mut self, state: &mut State, op: UnaryOp, ret: TypeId, mut lhs: Expr) {
        let ret = ret.with_templates(&self.proj.types, &state.func.ty_args);
        match op {
            // OK for option since lhs has already been coerced
            UnaryOp::Plus | UnaryOp::Option => self.emit_expr(lhs, state),
            UnaryOp::Neg => {
                write_de!(self.buffer, "-");
                self.emit_expr(lhs, state);
            }
            UnaryOp::PostIncrement => {
                self.emit_expr(lhs, state);
                write_de!(self.buffer, "++");
            }
            UnaryOp::PostDecrement => {
                self.emit_expr(lhs, state);
                write_de!(self.buffer, "--");
            }
            UnaryOp::PreIncrement => {
                write_de!(self.buffer, "++");
                self.emit_expr(lhs, state);
            }
            UnaryOp::PreDecrement => {
                write_de!(self.buffer, "--");
                self.emit_expr(lhs, state);
            }
            UnaryOp::Not => {
                if lhs.ty == TypeId::BOOL {
                    self.emit_cast(ret);
                    write_de!(self.buffer, "!(");
                    self.emit_expr(lhs, state);
                    write_de!(self.buffer, ")");
                } else {
                    write_de!(self.buffer, "~");
                    self.emit_expr(lhs, state);
                }
            }
            UnaryOp::Addr | UnaryOp::AddrMut | UnaryOp::AddrRaw | UnaryOp::AddrRawMut => {
                lhs.ty = lhs.ty.with_templates(&self.proj.types, &state.func.ty_args);
                if let &ExprData::Deref(mut inner, count) = self.arena.get(lhs.data) {
                    if count == 1 {
                        inner.ty = inner.ty.with_templates(&self.proj.types, &state.func.ty_args);
                        self.emit_expr_inner(inner, state);
                    } else {
                        let expr = self.arena.typed(lhs.ty, ExprData::Deref(inner, count - 1));
                        self.emit_expr_inner(expr, state);
                    }
                    return;
                }

                let is_lvalue = match self.arena.get(lhs.data) {
                    ExprData::Deref { .. } | ExprData::Fn(_, _) | ExprData::MemFn(_, _) => true,
                    ExprData::Var(id) => !self.proj.scopes.get(*id).kind.is_const(),
                    ExprData::Member { source, .. } => !source.ty.is_packed_struct(self.proj),
                    _ => false,
                };

                write_de!(self.buffer, "&");
                if is_lvalue {
                    self.emit_expr_inner(lhs, state);
                } else {
                    self.emit_tmpvar_ident(lhs, state);
                }
            }
            UnaryOp::Try => {
                tmpbuf_emit!(self, state, |tmp| {
                    lhs.ty = lhs.ty.with_templates(&self.proj.types, &state.func.ty_args);

                    self.emit_type(ret);
                    write_de!(self.buffer, " {tmp};");

                    let inner_ty = lhs.ty;
                    let inner_tmp = self.emit_tmpvar(lhs, state);
                    self.emit_pattern_if_stmt(state, &null_variant(ret), &inner_tmp, inner_ty);
                    hoist_point!(self, {
                        let mut buffer = Buffer::new(self.proj);
                        usebuf!(self, &mut buffer, {
                            write_de!(self.buffer, "return ");
                            let mut ret_type = self.proj.scopes.get(state.func.id).ret;
                            ret_type =
                                ret_type.with_templates(&self.proj.types, &state.func.ty_args);
                            let opt = Expr::option_null(ret_type, &mut self.arena);
                            self.emit_expr_inner(opt, state);
                        });
                        self.leave_scope(
                            state,
                            &buffer.finish(),
                            self.proj.scopes.get(state.func.id).body_scope,
                        );
                    });
                    write_de!(self.buffer, "}}{tmp}=");
                    if inner_ty.can_omit_tag(&self.proj.scopes, &self.proj.types).is_some() {
                        write_de!(self.buffer, "{inner_tmp};");
                    } else {
                        write_de!(self.buffer, "{inner_tmp}.Some.$0;");
                    }
                });
            }
            UnaryOp::Unwrap => panic!("ICE: UnaryOp::Unwrap in gen_expr"),
            UnaryOp::Deref => unreachable!("ICE: Untransformed UnaryOp::Deref in gen_expr"),
        }
    }

    fn finish_emit_fn_args(
        &mut self,
        state: &mut State,
        original_id: FunctionId,
        args: IndexMap<StrId, Expr>,
    ) {
        let mut args: IndexMap<_, _> = args
            .into_iter()
            .map(|(name, mut expr)| {
                expr.ty = expr.ty.with_templates(&self.proj.types, &state.func.ty_args);
                // TODO: dont emit temporaries for expressions that cant have side effects
                (name, hoist!(self, self.emit_tmpvar(expr, state)))
            })
            .collect();

        let mut count = 0;
        macro_rules! arg {
            ($arg: expr) => {{
                if count > 0 {
                    write_de!(self.buffer, ",");
                }

                self.buffer.emit($arg);
                count += 1;
            }};
        }

        self.proj
            .scopes
            .get(original_id)
            .params
            .iter()
            .flat_map(|param| args.shift_remove(&param.label))
            .for_each(|arg| arg!(arg));
        args.into_iter().for_each(|(_, arg)| arg!(arg));
        write_de!(self.buffer, ")");
    }

    fn emit_loop_break(&mut self, state: &mut State, ty: TypeId, optional: bool) {
        if optional {
            write_de!(self.buffer, "{SCOPE_VAR_PREFIX}{}=", self.cur_loop);
            let opt = Expr::option_null(ty, &mut self.arena);
            self.emit_expr_inline(opt, state);
        } else {
            write_de!(self.buffer, "{SCOPE_VAR_PREFIX}{}={VOID_INSTANCE}", self.cur_loop);
        }
        write_de!(self.buffer, ";break;");
    }

    fn leave_scope(&mut self, state: &mut State, exit: &str, scope: ScopeId) {
        let mut emitted = false;
        for i in (0..self.defers.len()).rev() {
            for j in (0..self.defers[i].1.len()).rev() {
                if !emitted {
                    write_nm!(self, "/* begin defers {scope:?} */");
                    emitted = true;
                }

                hoist_point!(self, self.emit_expr_stmt(self.defers[i].1[j], state));
            }

            if self.defers[i].0 == scope {
                break;
            }
        }
        if emitted {
            write_nm!(self, "/* end defers {scope:?} */");
        }
        write_de!(self.buffer, "{exit};");
    }

    fn emit_new(&mut self, ut: &GenericUserType) {
        // FIXME: this should technically use the scope that is creating the literal, but since
        // none of the constructors for literals use trait functions in any way, it doesn't matter
        // right now
        let new_state = State::in_body_scope(
            GenericFn::new(
                self.proj
                    .scopes
                    .get(ut.id)
                    .find_associated_fn(&self.proj.scopes, Strings::NEW)
                    .unwrap(),
                ut.ty_args.clone(),
            ),
            &self.proj.scopes,
        );

        self.buffer.emit_fn_name(&new_state.func, self.flags.minify);
        write_de!(self.buffer, "()");
        self.funcs.insert(new_state);
    }

    fn emit_with_capacity(
        &mut self,
        ty: TypeId,
        tmp: &str,
        ut: &GenericUserType,
        len: impl std::fmt::Display,
    ) {
        self.emit_type(ty);
        write_de!(self.buffer, " {tmp}=");
        // FIXME: this should technically use the scope that is creating the literal, but since
        // none of the constructors for literals use trait functions in any way, it doesn't matter
        // right now
        let state = State::in_body_scope(
            GenericFn::new(
                self.proj
                    .scopes
                    .get(ut.id)
                    .find_associated_fn(&self.proj.scopes, Strings::FN_WITH_CAPACITY)
                    .unwrap(),
                ut.ty_args.clone(),
            ),
            &self.proj.scopes,
        );

        self.buffer.emit_fn_name(&state.func, self.flags.minify);
        write_de!(self.buffer, "({len});");
        self.funcs.insert(state);
    }

    fn emit_intrinsic(
        &mut self,
        name: StrId,
        ret: TypeId,
        func: &GenericFn,
        mut args: IndexMap<StrId, Expr>,
        state: &mut State,
        (scope, span): (ScopeId, Span),
    ) {
        match strdata!(self, name) {
            "numeric_abs" => {
                let (_, mut expr) = args.shift_remove_index(0).unwrap();
                expr.ty = expr.ty.with_templates(&self.proj.types, &state.func.ty_args);
                let tmp = hoist!(self, self.emit_tmpvar(expr, state));
                write_de!(self.buffer, "({tmp}<0?-{tmp}:{tmp})");
            }
            "numeric_cast" => {
                self.emit_cast(ret);
                self.emit_expr(args.shift_remove_index(0).unwrap().1, state);
            }
            "max_value" => {
                self.emit_literal(ret.as_integral(&self.proj.types, true).unwrap().max(), ret)
            }
            "min_value" => {
                self.emit_literal(ret.as_integral(&self.proj.types, true).unwrap().min(), ret)
            }
            "size_of" => {
                write_de!(
                    self.buffer,
                    "(usize){}",
                    func.first_type_arg()
                        .unwrap()
                        .size_and_align(&self.proj.scopes, &self.proj.types)
                        .0
                );
            }
            "align_of" => {
                write_de!(
                    self.buffer,
                    "(usize){}",
                    func.first_type_arg()
                        .unwrap()
                        .size_and_align(&self.proj.scopes, &self.proj.types)
                        .1
                );
            }
            "panic" => {
                let panic = State::from_non_generic(
                    self.proj.panic_handler.expect("a panic handler should exist"),
                    &self.proj.scopes,
                );

                write_de!(self.buffer, "VOID(");
                self.buffer.emit_fn_name(&panic.func, self.flags.minify);
                write_de!(self.buffer, "(");
                for (i, (_, expr)) in args.into_iter().enumerate() {
                    if i > 0 {
                        write_de!(self.buffer, ",");
                    }
                    self.emit_expr(expr, state);
                }
                write_de!(self.buffer, "))");

                self.funcs.insert(panic);
            }
            "unreachable_unchecked" => {
                hoist!(self, write_de!(self.buffer, "CTL_UNREACHABLE();"));
                self.buffer.emit(VOID_INSTANCE);
            }
            "binary_op" => {
                let mut args = args.into_iter();
                let arg0 = args
                    .next()
                    .expect("ICE: binary operator should receive two arguments")
                    .1
                    .auto_deref(&self.proj.types, TypeId::UNKNOWN, &mut self.arena);
                let arg1 = args
                    .next()
                    .expect("ICE: binary operator should receive two arguments")
                    .1
                    .auto_deref(&self.proj.types, TypeId::UNKNOWN, &mut self.arena);
                let op = self.proj.scopes.get(func.id).name.data;
                self.emit_binary(
                    state,
                    match strdata!(self, op) {
                        "cmp" => BinaryOp::Cmp,
                        "gt" => BinaryOp::Gt,
                        "ge" => BinaryOp::GtEqual,
                        "lt" => BinaryOp::Lt,
                        "le" => BinaryOp::LtEqual,
                        "eq" => BinaryOp::Equal,
                        "ne" => BinaryOp::NotEqual,
                        "add" => BinaryOp::Add,
                        "sub" => BinaryOp::Sub,
                        "mul" => BinaryOp::Mul,
                        "div" => BinaryOp::Div,
                        "rem" => BinaryOp::Rem,
                        "bit_and" => BinaryOp::BitAnd,
                        "bit_or" => BinaryOp::BitOr,
                        "xor" => BinaryOp::Xor,
                        "shl" => BinaryOp::Shl,
                        "shr" => BinaryOp::Shr,
                        "add_assign" => BinaryOp::AddAssign,
                        "sub_assign" => BinaryOp::SubAssign,
                        "mul_assign" => BinaryOp::MulAssign,
                        "div_assign" => BinaryOp::DivAssign,
                        "rem_assign" => BinaryOp::RemAssign,
                        "and_assign" => BinaryOp::BitAndAssign,
                        "or_assign" => BinaryOp::BitOrAssign,
                        "xor_assign" => BinaryOp::XorAssign,
                        "shl_assign" => BinaryOp::ShlAssign,
                        "shr_assign" => BinaryOp::ShrAssign,
                        op => panic!("ICE: call to unsupported binary operator '{op}'"),
                    },
                    ret,
                    arg0,
                    arg1,
                );
            }
            "unary_op" => {
                let op = self.proj.scopes.get(func.id).name.data;
                let arg0 = args
                    .into_iter()
                    .next()
                    .expect("ICE: unary operator should receive one argument")
                    .1
                    .auto_deref(&self.proj.types, TypeId::UNKNOWN, &mut self.arena);
                self.emit_unary(
                    state,
                    match strdata!(self, op) {
                        "neg" => UnaryOp::Neg,
                        "not" => UnaryOp::Not,
                        "inc" => UnaryOp::PostIncrement,
                        "dec" => UnaryOp::PostDecrement,
                        op => panic!("ICE: call to unsupported unary operator '{op}'"),
                    },
                    ret,
                    arg0,
                );
            }
            "type_id" => {
                self.emit_cast(ret);
                write_de!(self.buffer, "{{.tag={}}}", func.first_type_arg().unwrap().as_raw());
            }
            "type_name" => {
                let name = self.proj.fmt_ty(func.first_type_arg().unwrap()).to_string();
                write_de!(self.buffer, "{}", StringLiteral(&name))
            }
            "read_volatile" => {
                const COMPLAINT: &str = "ICE: read_volatile should receive one argument";
                write_de!(self.buffer, "*(volatile ");
                self.emit_type(ret);
                write_de!(self.buffer, " const*)(");
                self.emit_expr_inline(args.into_iter().next().expect(COMPLAINT).1, state);
                write_de!(self.buffer, ")");
            }
            "write_volatile" => {
                const COMPLAINT: &str = "ICE: write_volatile should receive two arguments";
                let mut args = args.into_iter();
                let mut arg0 = args.next().expect(COMPLAINT).1;
                arg0.ty = arg0.ty.with_templates(&self.proj.types, &state.func.ty_args);
                write_de!(self.buffer, "VOID(*(volatile ");
                self.emit_type(arg0.ty);
                write_de!(self.buffer, ")(");
                self.emit_expr_inline(arg0, state);
                write_de!(self.buffer, ")=");
                self.emit_expr_inline(args.next().expect(COMPLAINT).1, state);
                write_de!(self.buffer, ")");
            }
            "source_location" => {
                let func =
                    self.proj.scopes.walk(scope).find_map(|s| s.1.kind.as_function().copied());
                let sl_typ = self.proj.types[ret].as_user().unwrap().id;
                let func_strid = self.proj.strings.get("func").unwrap();
                let option_typ = self.proj.scopes.get(sl_typ).members.get(&func_strid).unwrap().ty;

                self.buffer.emit("(");
                self.emit_cast(ret);

                let range = self
                    .source
                    .get_source(self.proj.diag.file_path(span.file), |data| {
                        Diagnostics::get_span_range(data, span, OffsetMode::Utf32)
                    })
                    .ok();

                // TODO: Compiler options like --remap-path-prefix & flag to omit source information
                let path = self.proj.diag.file_path(span.file).to_string_lossy().to_string();
                write_de!(
                    self.buffer,
                    "{{.file={},.line={},.col={},.func=",
                    StringLiteral(&path),
                    range.map(|s| s.start.line).unwrap_or_default() + 1,
                    range.map(|s| s.start.character).unwrap_or_default() + 1,
                );
                let opt = if let Some(func) = func {
                    let str = self.function_name(func, state);
                    Expr::option_some(option_typ, str, &mut self.arena)
                } else {
                    Expr::option_null(option_typ, &mut self.arena)
                };
                self.emit_expr_inline(opt, state);
                self.buffer.emit("})");
            }
            "ptr_add_signed" | "ptr_add_unsigned" | "ptr_sub_signed" | "ptr_sub_unsigned"
            | "ptr_diff" => {
                const COMPLAINT: &str = "ICE: ptr intrinsic should receive two arguments";
                let mut args = args.into_iter();
                let lhs = args.next().expect(COMPLAINT).1;
                let rhs = args.next().expect(COMPLAINT).1;

                self.emit_cast(ret);
                self.buffer.emit("(");
                self.emit_expr(lhs, state);
                let plus = matches!(strdata!(self, name), "ptr_add_signed" | "ptr_add_unsigned");
                self.buffer.emit(["-", "+"][plus as usize]);
                self.emit_expr(rhs, state);
                self.buffer.emit(")");
            }
            name => unreachable!("ICE: Attempt to codegen unsupported intrinsic: '{name}'"),
        }
    }

    fn emit_tmpvar_ident(&mut self, expr: Expr, state: &mut State) {
        tmpbuf_emit!(self, state, |tmp| {
            self.emit_type(expr.ty);
            write_de!(self.buffer, " {tmp}=");
            self.emit_expr_inner(expr, state);
            write_de!(self.buffer, ";");
        });
    }

    fn emit_tmpvar(&mut self, expr: Expr, state: &mut State) -> String {
        let tmp = state.tmpvar();
        self.emit_type(expr.ty);
        write_de!(self.buffer, " {tmp}=");
        self.emit_expr_inner(expr, state);
        write_de!(self.buffer, ";");
        tmp
    }

    fn emit_vtable_name(&mut self, vtable: &Vtable) {
        self.buffer.emit_mangled_name(vtable.ty, self.flags.minify);
        if !self.flags.minify {
            write_de!(self.buffer, "_");
        }
        self.buffer.emit_type_name(&vtable.tr, self.flags.minify);
        self.buffer.emit(if self.flags.minify { "v" } else { "_$vtable" });
        write_de!(self.buffer, "{}", vtable.scope);
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_pattern_inner(
        &mut self,
        state: &mut State,
        pattern: &PatternData,
        src: &str,
        ty: TypeId,
        borrow: bool,
        bindings: &mut Buffer<'a>,
        conditions: &mut JoiningBuilder<'a>,
    ) {
        match pattern {
            PatternData::Int(value) => {
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        write_de!(self.buffer, "{}==", Self::deref(&self.proj.types, src, ty));
                        self.emit_literal(value.clone(), ty.strip_references(&self.proj.types));
                    });
                });
            }
            PatternData::IntRange(RangePattern { inclusive, start, end }) => {
                let (base, ind) = ty.strip_references_ex(&self.proj.types);
                let src = Self::apply_deref(src, ind);
                if let Some(start) = start {
                    conditions.next(|buffer| {
                        usebuf!(self, buffer, {
                            write_de!(self.buffer, "{src}>=");
                            self.emit_literal(start.clone(), base);
                        });
                    });
                }
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        write_de!(self.buffer, "{src}{}", if *inclusive { "<=" } else { "<" });
                        self.emit_literal(end.clone(), base);
                    });
                });
            }
            PatternData::String(value) => {
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        let value = strdata!(self, value);
                        write_de!(
                            self.buffer,
                            "{0}.span.len=={1}&&CTL_MEMCMP({0}.span.ptr,\"",
                            Self::deref(&self.proj.types, src, ty),
                            value.len()
                        );
                        for byte in value.as_bytes() {
                            write_de!(self.buffer, "\\x{byte:x}");
                        }
                        write_de!(self.buffer, "\",{})==0", value.len());
                    });
                });
            }
            &PatternData::Variant { ref pattern, variant, inner, borrows } => {
                let (base, ind) = ty.strip_references_ex(&self.proj.types);
                let src = Self::apply_deref(src, ind);
                if base.can_omit_tag(&self.proj.scopes, &self.proj.types).is_some() {
                    if variant == Strings::SOME {
                        conditions.next_str(format!("{src}!={NULLPTR}"));
                        if let Some((patt, borrows)) =
                            pattern.as_ref().and_then(|patt| patt.data.as_destructure())
                        {
                            self.emit_pattern_inner(
                                state,
                                &patt[0].2.data,
                                &src,
                                patt[0].1,
                                borrow || *borrows,
                                bindings,
                                conditions,
                            );
                        }
                    } else {
                        conditions.next_str(format!("{src}=={NULLPTR}"));
                    }
                } else {
                    let ut_id = self.proj.types.get(base).as_user().map(|ut| ut.id);
                    let (tag, ty) = ut_id
                        .and_then(|id| self.proj.scopes.get(id).kind.as_union())
                        .and_then(|union| union.discriminant(variant).cloned().zip(Some(union.tag)))
                        .unwrap();
                    conditions.next(|buf| {
                        usebuf!(self, buf, {
                            write_de!(self.buffer, "{src}.{UNION_TAG_NAME}==");
                            self.emit_literal(tag, ty);
                        })
                    });

                    if let Some(pattern) = pattern {
                        self.emit_pattern_inner(
                            state,
                            &pattern.data,
                            &format!("{src}.{}", member_name(self.proj, ut_id, variant)),
                            inner,
                            borrow || borrows,
                            bindings,
                            conditions,
                        );
                    }
                }
            }
            PatternData::Span { patterns, rest, inner } => {
                let src = Self::deref(&self.proj.types, src, ty);
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        write_de!(
                            self.buffer,
                            "{src}.len{}{}",
                            if rest.is_some() { ">=" } else { "==" },
                            patterns.len()
                        );
                    });
                });

                let pos = rest.map(|RestPattern { id, pos }| {
                    if let Some(id) = id.filter(|&id| !self.proj.scopes.get(id).unused) {
                        usebuf!(self, bindings, {
                            self.emit_var_decl(id, state);
                            write_de!(
                                self.buffer,
                                "={{.ptr={src}.ptr+{pos},.len={src}.len-{}}};",
                                patterns.len()
                            );
                        });
                    }
                    pos
                });
                let inner = inner.with_templates(&self.proj.types, &state.func.ty_args);
                for (i, patt) in patterns.iter().enumerate() {
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &if pos.is_some_and(|pos| i >= pos) {
                            format!("{src}.ptr[{src}.len-{}+{i}]", patterns.len())
                        } else {
                            format!("{src}.ptr[{i}]")
                        },
                        inner,
                        true,
                        bindings,
                        conditions,
                    );
                }
            }
            PatternData::Destructure { patterns, borrows } => {
                let (ty, ind) = ty.strip_references_ex(&self.proj.types);
                let src = Self::apply_deref(src, ind);
                let ut_id = self.proj.types[ty].as_user().map(|ut| ut.id);
                for (member, inner, patt) in patterns {
                    let inner = inner.with_templates(&self.proj.types, &state.func.ty_args);
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &format!("{src}.{}", member_name(self.proj, ut_id, *member)),
                        inner,
                        borrow || *borrows,
                        bindings,
                        conditions,
                    );
                }
            }
            PatternData::Array { patterns, borrows } => {
                let ArrayPattern { patterns, rest, arr_len, inner } = patterns;
                let is_any_ptr = self.proj.types[ty].is_any_ptr();
                let src = format!("{}.{ARRAY_DATA_NAME}", Self::deref(&self.proj.types, src, ty));
                let rest = rest.map(|RestPattern { id, pos }| {
                    let rest_len = arr_len - patterns.len();
                    if let Some(id) = id.filter(|&id| !self.proj.scopes.get(id).unused) {
                        usebuf!(self, bindings, {
                            if is_any_ptr {
                                // TODO: this is probably technically UB -- we are casting somewhere
                                // in the middle of a `struct Array_T_N` to a `struct Array_T_M`
                                let ty = self.emit_var_decl(id, state);
                                write_de!(self.buffer, "=");
                                self.emit_cast(ty);
                                write_de!(self.buffer, "({src}+{pos});");
                            } else {
                                let var = self.proj.scopes.get(id);
                                let ty =
                                    var.ty.with_templates(&self.proj.types, &state.func.ty_args);
                                let (size, _) =
                                    ty.size_and_align(&self.proj.scopes, &self.proj.types);

                                self.emit_type(ty);
                                write_de!(self.buffer, " ");
                                self.emit_var_name(id, state);
                                write_de!(self.buffer, ";CTL_MEMCPY(&");
                                self.emit_var_name(id, state);
                                write_de!(self.buffer, ",{src}+{pos},{size});");
                            }
                        });
                    }

                    (pos, rest_len)
                });

                let inner = inner.with_templates(&self.proj.types, &state.func.ty_args);
                for (mut i, patt) in patterns.iter().enumerate() {
                    if let Some((_, rest_len)) = rest.filter(|&(pos, _)| i >= pos) {
                        i += rest_len;
                    }
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &format!("{src}[{i}]"),
                        inner,
                        borrow || *borrows,
                        bindings,
                        conditions,
                    );
                }
            }
            &PatternData::Variable(id) => {
                if self.proj.scopes.get(id).unused {
                    return;
                }

                usebuf!(self, bindings, {
                    self.emit_var_decl(id, state);
                    write_de!(self.buffer, "={}{src};", if borrow { "&" } else { "" });
                });
            }
            PatternData::Void => {}
            PatternData::Or(patterns) => {
                let mut conds = JoiningBuilder::new(self.proj, "||", "1");
                let mut binds = Buffer::new(self.proj);
                for pattern in patterns {
                    let mut tmp = JoiningBuilder::new(self.proj, "&&", "1");
                    self.emit_pattern_inner(
                        state,
                        &pattern.data,
                        src,
                        ty,
                        borrow,
                        &mut binds,
                        &mut tmp,
                    );
                    conds.next_str(format!("({})", tmp.finish()));
                }

                if !binds.0.is_empty() {
                    todo!("codegen for or pattern with variable bindings");
                }

                conditions.next_str(format!("({})", conds.finish()));
            }
            PatternData::Error => panic!("ICE: CheckedPatternData::Error in gen_pattern"),
        }
    }

    fn emit_pattern(
        &mut self,
        state: &mut State,
        pattern: &PatternData,
        src: &str,
        ty: TypeId,
    ) -> (Buffer<'a>, JoiningBuilder<'a>) {
        let mut bindings = Buffer::new(self.proj);
        let mut conditions = JoiningBuilder::new(self.proj, "&&", "1");
        self.emit_pattern_inner(state, pattern, src, ty, false, &mut bindings, &mut conditions);
        (bindings, conditions)
    }

    fn emit_pattern_if_stmt(
        &mut self,
        state: &mut State,
        pattern: &PatternData,
        src: &str,
        ty: TypeId,
    ) {
        let (bindings, conditions) = self.emit_pattern(state, pattern, src, ty);
        write_de!(self.buffer, "if({}){{", conditions.finish());
        self.buffer.emit(bindings.finish());
    }

    fn emit_pattern_bindings(
        &mut self,
        state: &mut State,
        pattern: &PatternData,
        src: &str,
        ty: TypeId,
    ) {
        let (bindings, _) = self.emit_pattern(state, pattern, src, ty);
        self.buffer.emit(bindings.finish());
    }

    fn emit_block(&mut self, block: Block, state: &mut State) {
        let old = std::mem::take(&mut self.emitted_never_in_this_block);
        hoist_point!(self, {
            self.defers.push((block.scope, vec![]));
            for stmt in block.body.into_iter() {
                self.emit_stmt(stmt, state);
                if self.emitted_never_in_this_block {
                    break;
                }
            }
        });

        let (id, defers) = self.defers.pop().unwrap();
        if std::mem::replace(&mut self.emitted_never_in_this_block, old) {
            return;
        }

        if !defers.is_empty() {
            write_nm!(self, "/* begin defers {id:?} */");
            for expr in defers.into_iter().rev() {
                hoist_point!(self, self.emit_expr_stmt(expr, state));
            }
            write_nm!(self, "/* end defers {id:?} */");
        }
    }

    fn emit_type(&mut self, id: TypeId) {
        self.tg.add_type(&self.proj.scopes, &self.proj.types, id);
        self.buffer.emit_type(id, self.flags.minify);
    }

    fn emit_cast(&mut self, id: TypeId) {
        write_de!(self.buffer, "(");
        self.emit_type(id);
        write_de!(self.buffer, ")");
    }

    fn emit_prototype(
        &mut self,
        state: &mut State,
        is_prototype: bool,
    ) -> (Vec<VariableId>, FirstParam) {
        let f = self.proj.scopes.get(state.func.id);
        let ret = f.ret.with_templates(&self.proj.types, &state.func.ty_args);
        if f.is_extern {
            write_de!(self.buffer, "extern ");
        } else {
            write_de!(self.buffer, "static ");
            // TODO: inline manually
            match f.attrs.inline {
                Some(FunctionInline::Always) => write_de!(self.buffer, "CTL_FORCEINLINE "),
                Some(FunctionInline::Never) => write_de!(self.buffer, "CTL_NEVERINLINE "),
                Some(FunctionInline::Encouraged) => write_de!(self.buffer, "CTL_INLINE "),
                _ => {}
            }
        }

        if ret == TypeId::NEVER {
            // && real
            write_de!(self.buffer, "CTL_NORETURN ");
        }

        let is_import = f.is_extern && f.body.is_none();
        if ret.is_void_like() {
            write_de!(self.buffer, "void ");
        } else {
            self.emit_type(ret);
            write_de!(self.buffer, " ");
        }
        self.buffer.emit_fn_name(&state.func, self.flags.minify);
        write_de!(self.buffer, "(");

        let mut unused = vec![];
        let mut nonnull = vec![];
        let mut thisptr = FirstParam::Normal;
        for (i, param) in f.params.iter().enumerate() {
            let ty = param.ty.with_templates(&self.proj.types, &state.func.ty_args);
            if i > 0 {
                write_de!(self.buffer, ",");
            }

            if self.proj.types[ty].is_any_ptr() && is_prototype {
                nonnull.push(format!("{}", i + 1));
            }

            let override_with_voidptr = i == 0 && !f.is_extern && self.proj.types[ty].is_safe_ptr();
            if override_with_voidptr {
                if self.proj.types[ty].is_ptr() {
                    self.buffer.emit("void const*");
                } else {
                    self.buffer.emit("void*");
                }
            }

            if is_import || is_prototype {
                if !override_with_voidptr {
                    self.emit_type(ty);
                }
            } else if let ParamPattern::Checked(Pattern {
                data: PatternData::Variable(id), ..
            }) = &param.patt
            {
                if override_with_voidptr {
                    let tmp = state.tmpvar();
                    self.buffer.emit(&tmp);
                    thisptr = FirstParam::OverriddenVar { tmp, id: *id };
                } else {
                    self.emit_var_decl(*id, state);
                    if self.proj.scopes.get(*id).unused {
                        unused.push(*id);
                    }
                }
            } else {
                //
                if override_with_voidptr {
                    let tmp = state.tmpvar();
                    self.buffer.emit(&tmp);
                    thisptr = FirstParam::OverriddenLabel { tmp, label: param.label, ty };
                } else {
                    self.emit_type(ty);
                    write_de!(self.buffer, " {}", strdata!(self, param.label));
                }
            }
        }

        if f.variadic {
            write_de!(self.buffer, "{}...)", [",", ""][f.params.is_empty() as usize]);
        } else if f.params.is_empty() {
            write_de!(self.buffer, "void)");
        } else {
            write_de!(self.buffer, ")");
        }

        if !nonnull.is_empty() {
            write_de!(self.buffer, "CTL_NONNULL({})", nonnull.join(","));
        }

        (unused, thisptr)
    }

    fn emit_var_name(&mut self, id: VariableId, state: &mut State) {
        use std::collections::hash_map::*;

        if self.flags.minify {
            return write_de!(self.buffer, "v{id}");
        }

        let var = self.proj.scopes.get(id);
        if var.kind.is_static() {
            if var.is_extern {
                self.buffer.emit_str(var.attrs.link_name.unwrap_or(var.name.data))
            } else {
                self.buffer.emit(full_name(self.proj, var.scope, var.name.data));
            }
        } else {
            if is_c_reserved_ident(strdata!(self, var.name.data)) {
                write_de!(self.buffer, "$");
            }
            self.buffer.emit_str(var.name.data);
            match state.emitted_names.entry(var.name.data) {
                Entry::Occupied(entry) if *entry.get() == id => {}
                Entry::Occupied(_) => write_de!(self.buffer, "_{id}"),
                Entry::Vacant(entry) => {
                    entry.insert(id);
                }
            }
        }
    }

    fn emit_var_decl(&mut self, id: VariableId, state: &mut State) -> TypeId {
        let var = self.proj.scopes.get(id);
        assert!(!var.kind.is_const());
        if var.is_extern && var.value.is_none() {
            write_de!(self.buffer, "extern ");
        } else if var.kind.is_static() && !var.is_extern {
            write_de!(self.buffer, "static ");
        }

        if var.attrs.thread_local {
            write_de!(self.buffer, "_Thread_local ");
        }

        let ty = var.ty.with_templates(&self.proj.types, &state.func.ty_args);
        self.emit_type(ty);
        if !var.mutable {
            let mutable = var.kind.is_static()
                || self.proj.types[ty]
                    .as_user()
                    .is_some_and(|ut| self.proj.scopes.get(ut.id).interior_mutable);
            self.buffer.emit(if !mutable { " const" } else { "/*const*/" });
        }
        write_de!(self.buffer, " ");
        self.emit_var_name(id, state);

        ty
    }

    fn emit_array_write_to_deref(
        &mut self,
        state: &mut State,
        ty: TypeId,
        mut inner: Expr,
        count: usize,
        rhs: Expr,
    ) {
        hoist!(self, {
            let tmp = if count == 1 {
                inner.ty = inner.ty.with_templates(&self.proj.types, &state.func.ty_args);
                self.emit_tmpvar(inner, state)
            } else {
                let expr = self.arena.typed(ty, ExprData::Deref(inner, count - 1));
                self.emit_tmpvar(expr, state)
            };

            let (size, _) = ty.size_and_align(&self.proj.scopes, &self.proj.types);
            write_de!(self.buffer, "CTL_MEMCPY({tmp},&");
            self.emit_expr_inline(rhs, state);
            write_de!(self.buffer, ",{size});");
        });
        self.buffer.emit(VOID_INSTANCE);
    }

    fn emit_bitfield_assign(
        &mut self,
        source: Expr,
        state: &mut State,
        member: StrId,
        ty: TypeId,
        rhs: Expr,
        op: BinaryOp,
    ) {
        let id = self.proj.types.get(source.ty).as_user().map(|ut| ut.id).unwrap();
        let src = tmpbuf!(self, state, |tmp| {
            let ptr = self.proj.types.insert(Type::MutPtr(source.ty));
            self.emit_type(ptr);
            write_de!(self.buffer, " {tmp}=&");
            self.emit_expr_inline(source, state);
            write_de!(self.buffer, ";");
            format!("(*{tmp})")
        });

        let expr = tmpbuf!(self, state, |tmp| {
            self.emit_type(ty);
            write_de!(self.buffer, " {tmp}=");
            if op != BinaryOp::Assign {
                write_de!(self.buffer, "(");
                self.emit_bitfield_read(&src, id, member, ty);
                write_de!(
                    self.buffer,
                    "){}",
                    match op {
                        BinaryOp::AddAssign => "+",
                        BinaryOp::SubAssign => "-",
                        BinaryOp::MulAssign => "*",
                        BinaryOp::DivAssign => "/",
                        BinaryOp::RemAssign => "%",
                        BinaryOp::BitAndAssign => "&",
                        BinaryOp::XorAssign => "^",
                        BinaryOp::BitOrAssign => "|",
                        BinaryOp::ShlAssign => "<<",
                        BinaryOp::ShrAssign => ">>",
                        _ => unreachable!(),
                    }
                );
            }
            self.emit_expr_inline(rhs, state);

            write_de!(self.buffer, ";");
            tmp
        });
        hoist!(self, self.emit_bitfield_write(&src, id, member, ty, &expr));
        self.buffer.emit(VOID_INSTANCE);
    }

    fn emit_bitfield_read(&mut self, tmp: &str, id: UserTypeId, member: StrId, ty: TypeId) {
        let mut result = JoiningBuilder::new(self.proj, "|", "0");
        self.bitfield_access(id, member, ty, |this, access| {
            let BitfieldAccess {
                word,
                word_offset,
                enum_tag,
                reading,
                offset,
                word_size_bits,
                bits: _,
            } = access;
            let partial = reading != word_size_bits;
            result.next(|buffer| {
                usebuf!(this, buffer, {
                    if enum_tag.is_some() {
                        this.emit_cast(ty);
                        write_de!(this.buffer, "{{.{UNION_TAG_NAME}=");
                    }
                    write_if!(offset != 0, this.buffer, "(");
                    this.emit_cast(enum_tag.unwrap_or(ty));
                    write_if!(partial, this.buffer, "(");
                    write_if!(word_offset != 0, this.buffer, "(");
                    write_de!(this.buffer, "{tmp}.{ARRAY_DATA_NAME}[{word}]");
                    write_if!(word_offset != 0, this.buffer, ">>{word_offset})");
                    write_if!(partial, this.buffer, "&{:#x})", bit_mask(reading));
                    write_if!(offset != 0, this.buffer, "<<{offset})");
                    write_if!(enum_tag.is_some(), this.buffer, "}}");
                })
            });
        });
        self.buffer.emit(result.finish());
    }

    fn emit_bitfield_write(
        &mut self,
        tmp: &str,
        id: UserTypeId,
        member: StrId,
        ty: TypeId,
        expr: &str,
    ) {
        let mut word_type = None;
        self.bitfield_access(id, member, ty, |this, access| {
            let BitfieldAccess {
                word,
                word_offset,
                word_size_bits,
                enum_tag,
                offset,
                reading,
                bits,
            } = access;
            let word_ty =
                word_type.get_or_insert_with(|| this.proj.types.insert(Type::Uint(word_size_bits)));
            let mask = bit_mask(reading);

            write_de!(this.buffer, "{tmp}.{ARRAY_DATA_NAME}[{word}]&=");
            this.emit_cast(*word_ty);
            write_de!(this.buffer, "{:#x};", !(mask << word_offset) & bit_mask(word_size_bits));

            // negative signed bitints contain 1s in the inaccessible bits (ie -1i2 == 0b1111_1111,
            // not 0b0000_0011) so we need the mask even for a non-partial write
            let needs_mask = reading != bits
                || enum_tag.unwrap_or(ty).as_integral(&this.proj.types, true).unwrap().signed;

            write_de!(this.buffer, "{tmp}.{ARRAY_DATA_NAME}[{word}]|=");
            write_if!(word_offset != 0, this.buffer, "(");
            write_if!(needs_mask, this.buffer, "(");
            this.emit_cast(*word_ty);
            write_if!(offset != 0, this.buffer, "(");
            write_de!(this.buffer, "{expr}");
            write_if!(enum_tag.is_some(), this.buffer, ".{UNION_TAG_NAME}");
            write_if!(offset != 0, this.buffer, ">>{offset})");
            write_if!(needs_mask, this.buffer, "&{mask:#x})");
            write_if!(word_offset != 0, this.buffer, "<<{word_offset})");
            write_de!(this.buffer, ";");
        });
    }

    fn bitfield_access(
        &mut self,
        id: UserTypeId,
        member: StrId,
        ty: TypeId,
        mut f: impl FnMut(&mut Self, BitfieldAccess),
    ) {
        let bf = self.proj.scopes.get(id).kind.as_packed_struct().unwrap();
        let word_size_bits = (bf.align * 8) as u32;
        let (bits, enum_tag) = match ty.bit_size(&self.proj.scopes, &self.proj.types) {
            BitSizeResult::Size(n) => (n, None),
            BitSizeResult::Tag(tag, n) => (n, Some(tag)),
            _ => unreachable!(),
        };

        let mut word = bf.bit_offsets[&member] / word_size_bits;
        let mut word_offset = bf.bit_offsets[&member] % word_size_bits;
        let mut needed_bits = bits;
        while needed_bits != 0 {
            let reading = needed_bits.min(word_size_bits - word_offset);
            f(
                self,
                BitfieldAccess {
                    word_size_bits,
                    word,
                    word_offset,
                    enum_tag,
                    reading,
                    offset: bits - needed_bits,
                    bits,
                },
            );
            word += 1;
            word_offset = 0;
            needed_bits -= reading;
        }
    }

    fn emit_literal(&mut self, literal: ComptimeInt, ty: TypeId) {
        let largest_type = Integer::from_cint(CInt::LongLong, false);
        let base = ty.as_integral(&self.proj.types, true).unwrap();
        if base.bits <= largest_type.bits {
            self.emit_cast(ty);
            if base.signed && literal == base.min() {
                return write_de!(self.buffer, "({} - 1)", literal + 1);
            } else {
                return write_de!(self.buffer, "{literal}{}", ["u", ""][base.signed as usize]);
            }
        }

        let mut result = JoiningBuilder::new(self.proj, "|", "0");
        let mut number = |this: &mut Self, number: &ComptimeInt, shift: u32| {
            result.next(|buffer| {
                usebuf!(this, buffer, {
                    write_if!(shift != 0, this.buffer, "(");
                    this.emit_cast(ty);
                    write_de!(this.buffer, "{number:#x}u");
                    write_if!(shift != 0, this.buffer, "<< {shift})");
                });
            });
        };

        let large_mask = (ComptimeInt::new(1) << largest_type.bits) - 1;
        let max_literal = largest_type.max();
        let was_negative = literal.is_negative();
        let mut literal = literal.abs();
        let mut shift = 0;
        while literal > max_literal {
            let value = &literal & &large_mask;
            if value != ComptimeInt::new(0) {
                number(self, &value, shift);
            }
            literal >>= largest_type.bits;
            shift += largest_type.bits;
        }

        number(self, &literal, shift);
        if was_negative {
            write_de!(self.buffer, "(~({}) + 1)", result.finish());
        } else {
            write_de!(self.buffer, "({})", result.finish());
        }
    }

    fn emit_string_interp(
        &mut self,
        ty: TypeId,
        state: &mut State,
        strings: Vec<StrId>,
        args: Vec<(Expr, FormatOpts)>,
        scope: ScopeId,
    ) {
        self.emit_cast(ty);
        write_de!(self.buffer, "{{.parts={{.len={},.ptr=", strings.len());
        tmpbuf_emit!(self, state, |tmp| {
            let Some(string_ut) = &self.str_interp.string else {
                panic!("ICE: StringInterp: Cannot find language type 'string'");
            };

            self.buffer.emit_type_name(string_ut, self.flags.minify);
            write_de!(self.buffer, " {tmp}[{}]={{", strings.len());
            for part in strings.iter() {
                write_de!(self.buffer, "{},", StringLiteral(strdata!(self, part)));
            }
            write_de!(self.buffer, "}};");
        });
        write_de!(self.buffer, "}},.args={{.len={},.ptr=", args.len());
        tmpbuf_emit!(self, state, |tmp| {
            let Some(fmt_arg_ut) = &self.str_interp.fmt_arg else {
                panic!("ICE: StringInterp: Cannot find language type 'fmt_arg'");
            };

            let opts_ty =
                self.proj.scopes.get(fmt_arg_ut.id).members.get(&self.str_interp.opts).unwrap().ty;
            let opts_ut = self.proj.scopes.get(self.proj.types.get(opts_ty).as_user().unwrap().id);
            let typeid_align = opts_ut.members.get(&self.str_interp.align).unwrap().ty;
            let typeid_sign = opts_ut.members.get(&self.str_interp.sign).unwrap().ty;

            self.buffer.emit_type_name(fmt_arg_ut, self.flags.minify);
            write_de!(self.buffer, " {tmp}[{}]={{", args.len());
            for (mut expr, opts) in args {
                expr.ty = expr.ty.with_templates(&self.proj.types, &state.func.ty_args);
                write_de!(self.buffer, "{{.value=&");
                self.emit_tmpvar_ident(expr, state);
                write_de!(self.buffer, ",.format=");
                self.emit_member_fn(state, opts.func, scope);
                write_de!(self.buffer, ",.opts=");
                let inst = ExprData::Instance(
                    [
                        (self.str_interp.width, opts.width),
                        (self.str_interp.prec, opts.prec),
                        (self.str_interp.fill, Expr::from_char(opts.fill, &mut self.arena)),
                        (
                            self.str_interp.align,
                            self.arena.typed(
                                typeid_align,
                                ExprData::VariantInstance(
                                    match opts.align {
                                        None => self.str_interp.align_none,
                                        Some(Alignment::Left) => self.str_interp.align_left,
                                        Some(Alignment::Right) => self.str_interp.align_right,
                                        Some(Alignment::Center) => self.str_interp.align_center,
                                    },
                                    [].into(),
                                ),
                            ),
                        ),
                        (self.str_interp.upper, Expr::from_bool(opts.upper, &mut self.arena)),
                        (self.str_interp.alt, Expr::from_bool(opts.alt, &mut self.arena)),
                        (
                            self.str_interp.sign,
                            self.arena.typed(
                                typeid_sign,
                                ExprData::VariantInstance(
                                    match opts.sign {
                                        Some(Sign::Positive) => self.str_interp.sign_pos,
                                        Some(Sign::Negative) => self.str_interp.sign_neg,
                                        None => self.str_interp.sign_none,
                                    },
                                    [].into(),
                                ),
                            ),
                        ),
                        (self.str_interp.zero, Expr::from_bool(opts.zero, &mut self.arena)),
                    ]
                    .into(),
                );
                let inst = self.arena.typed(opts_ty, inst);
                self.emit_expr_inline(inst, state);
                write_de!(self.buffer, "}},");
            }
            write_de!(self.buffer, "}};");
        });
        write_de!(self.buffer, "}}}}");
    }

    fn has_side_effects(&self, expr: &Expr) -> bool {
        match self.arena.get(expr.data) {
            ExprData::Unary(
                UnaryOp::PostIncrement
                | UnaryOp::PostDecrement
                | UnaryOp::PreIncrement
                | UnaryOp::PreDecrement,
                _,
            ) => true,
            ExprData::Call { .. } | ExprData::CallFnPtr { .. } | ExprData::CallDyn { .. } => true,
            ExprData::Binary(op, _, _) if op.is_assignment() => true,
            _ => false,
        }
    }

    fn find_implementation(
        &mut self,
        inst: TypeId,
        tr: &GenericTrait,
        method: StrId,
        scope: ScopeId,
        finish: impl FnOnce(&Project, FunctionId) -> TypeArgs + Copy,
    ) -> GenericFn {
        let Some(mfn) = self.lookup_trait_fn(inst, tr, method, scope, finish) else {
            panic!(
                "searching from scope: '{}', cannot find implementation for method '{}::{}' for type '{}'",
                full_name_pretty(self.proj, scope, false),
                self.proj.fmt_ut(tr),
                strdata!(self, method),
                self.proj.fmt_ty(inst)
            )
        };

        let picked = self.proj.scopes.get(mfn.func.id);
        if !picked.has_body {
            panic!(
                "searching from scope: '{}', get_member_fn_ex picked invalid function for implementation for method '{}::{}' for type '{}' (picked {}::{})",
                full_name_pretty(self.proj, scope, false),
                self.proj.fmt_ut(tr),
                strdata!(self, method),
                self.proj.fmt_ty(inst),
                full_name_pretty(self.proj, picked.scope, false),
                strdata!(self, picked.name.data),
            )
        }

        mfn.func
    }

    fn deref(types: &Types, src: &str, ty: TypeId) -> String {
        let (_inner, ind) = ty.strip_references_ex(types);
        Self::apply_deref(src, ind)
    }

    fn apply_deref(src: &str, ind: usize) -> String {
        if ind != 0 { format!("({:*<1$}{src})", "", ind) } else { src.into() }
    }

    fn function_name(&mut self, id: FunctionId, state: &State) -> Expr {
        let print_type_params = |result: &mut String, params: &[UserTypeId]| {
            if !params.is_empty() {
                result.push('<');
                for (i, &id) in params.iter().enumerate() {
                    if i != 0 {
                        result.push_str(", ");
                    }

                    if let Some(real) = state.func.ty_args.get(&id) {
                        write_de!(result, "{}", self.proj.fmt_ty(*real));
                    } else {
                        result.push_str(strdata!(self, self.proj.scopes.get(id).name.data));
                    }
                }
                result.push('>');
            }
        };

        let func = self.proj.scopes.get(id);
        let parent = self.proj.scopes.walk(func.scope).find_map(|s| s.1.kind.as_user_type());
        let mut res = String::new();
        if let Some(parent) = parent {
            let parent = self.proj.scopes.get(*parent);
            res += strdata!(self, parent.name.data);
            print_type_params(&mut res, &parent.type_params);
            res += "::";
        }

        res += strdata!(self, func.name.data);
        print_type_params(&mut res, &func.type_params);
        self.arena.typed(self.str_interp.string_ty.unwrap(), ExprData::GeneratedString(res))
    }
}

impl SharedStuff for Codegen<'_> {
    fn resolve_ext_type(&mut self, id: ExtensionId) -> TypeId {
        *self.proj.scopes.get(id).kind.as_extension().unwrap()
    }

    fn do_resolve_impls(&mut self, _: UserTypeId) {}

    fn proj(&self) -> &Project {
        self.proj
    }

    fn extension_cache(&mut self) -> &mut crate::typecheck::ExtensionCache {
        &mut self.ext_cache
    }
}

#[derive(Debug)]
struct BitfieldAccess {
    /// Which word of the bitfield are we accessing
    word: u32,
    /// The bit offset into the current word
    word_offset: u32,
    /// The size in bits of each word in the bitfield (always <= MAX_ALIGN * 8)
    word_size_bits: u32,

    /// If reading a partial enum, this is the underlying integer tag type
    enum_tag: Option<TypeId>,
    /// The bit offset into the integer we are trying to read/write
    offset: u32,
    /// The amount of bits we are reading from this word
    reading: u32,
    /// Bit size of the integer we are trying to read/write
    bits: u32,
}

fn vtable_methods(scopes: &Scopes, types: &Types, tr: UserTypeId) -> Vec<Vis<FunctionId>> {
    scopes
        .get(tr)
        .fns
        .iter()
        .filter(move |f| scopes.get(f.id).is_dyn_compatible(scopes, types, tr))
        .copied()
        .collect()
}

fn member_name(proj: &Project, _id: Option<UserTypeId>, name: StrId) -> String {
    let data = proj.strings.resolve(&name);
    if !is_c_reserved_ident(data) {
        data.into()
    } else {
        format!("${data}")
    }
}

#[rustfmt::skip]
fn is_c_reserved_ident(name: &str) -> bool {
    if name.starts_with("_")
        || name.starts_with(|ch: char| ch.is_ascii_digit())
        || name.starts_with("atomic_")
        || name.starts_with("ATOMIC_")
        || name.starts_with("memory_order")
    {
        return true;
    }

    matches!(name, "alignas" | "alignof" | "auto" | "bool" | "break" | "case" | "char" | "const"
        | "constexpr" | "continue" | "default" | "do" | "double" | "else" | "enum" | "extern"
        | "false" | "float" | "for" | "goto" | "if" | "inline" | "int" | "long" | "nullptr"
        | "register" | "restrict" | "return" | "short" | "signed" | "sizeof" | "static"
        | "static_assert" | "struct" | "switch" | "thread_local" | "true" | "typedef"
        | "typeof" | "typeof_unqual" | "union" | "unsigned" | "void" | "volatile" | "while")
}

fn full_name(proj: &Project, id: ScopeId, ident: StrId) -> String {
    use std::borrow::Cow;

    let mut parts = vec![Cow::Borrowed(proj.strings.resolve(&ident))];
    for (id, scope) in proj.scopes.walk(id) {
        if scope.kind.is_impl() {
            parts.push(Cow::Owned(format!("{id}")));
        }

        let Some(scope_name) = scope.kind.name(&proj.scopes) else {
            continue;
        };

        if let Some(id) = scope.kind.as_function()
            && proj.scopes.get(*id).typ.is_test()
        {
            parts.push(Cow::Owned(format!("test_{id}")));
        } else {
            parts.push(Cow::Borrowed(proj.strings.resolve(&scope_name.data)));
        }
    }
    parts.reverse();
    parts.join("_")
}

fn full_name_pretty(proj: &Project, id: ScopeId, modonly: bool) -> String {
    let mut parts = vec![];
    for (_, scope) in proj.scopes.walk(id) {
        let Some(scope_name) = scope.kind.name(&proj.scopes) else {
            continue;
        };
        if modonly && !scope.kind.is_module() {
            continue;
        }

        parts.push(proj.strings.resolve(&scope_name.data));
    }
    parts.reverse();
    parts.join("::")
}

fn bit_mask(bits: u32) -> u64 {
    if bits < 64 { (1 << bits) - 1 } else { u64::MAX }
}

fn null_variant(typ: TypeId) -> PatternData {
    PatternData::Variant { pattern: None, variant: Strings::NULL, inner: typ, borrows: false }
}

struct StrInterp {
    opts: StrId,
    string: Option<GenericUserType>,
    string_ty: Option<TypeId>,
    fmt_arg: Option<GenericUserType>,
    width: StrId,
    prec: StrId,
    fill: StrId,
    align: StrId,
    upper: StrId,
    alt: StrId,
    sign: StrId,
    zero: StrId,

    align_none: StrId,
    align_left: StrId,
    align_right: StrId,
    align_center: StrId,

    sign_none: StrId,
    sign_pos: StrId,
    sign_neg: StrId,
}

impl StrInterp {
    pub fn new(proj: &Project) -> Self {
        let string = proj
            .scopes
            .lang_types
            .get(&Strings::LANG_STRING)
            .map(|&ut| GenericUserType::from_id(&proj.scopes, &proj.types, ut));
        Self {
            fmt_arg: proj
                .scopes
                .lang_types
                .get(&Strings::LANG_FMT_ARG)
                .map(|&ut| GenericUserType::from_id(&proj.scopes, &proj.types, ut)),
            string_ty: string.as_ref().map(|s| proj.types.insert(Type::User(s.clone()))),
            string,
            opts: proj.strings.get("opts").unwrap(),
            width: proj.strings.get("width").unwrap(),
            prec: proj.strings.get("prec").unwrap(),
            fill: proj.strings.get("fill").unwrap(),
            align: proj.strings.get("align").unwrap(),
            upper: proj.strings.get("upper").unwrap(),
            alt: proj.strings.get("alt").unwrap(),
            sign: proj.strings.get("sign").unwrap(),
            zero: proj.strings.get("zero").unwrap(),
            align_none: proj.strings.get("None").unwrap(),
            align_left: proj.strings.get("Left").unwrap(),
            align_right: proj.strings.get("Right").unwrap(),
            align_center: proj.strings.get("Center").unwrap(),
            sign_none: proj.strings.get("None").unwrap(),
            sign_pos: proj.strings.get("Plus").unwrap(),
            sign_neg: proj.strings.get("Minus").unwrap(),
        }
    }
}

enum FirstParam {
    Normal,
    OverriddenVar { tmp: String, id: VariableId },
    OverriddenLabel { tmp: String, label: StrId, ty: TypeId },
}
