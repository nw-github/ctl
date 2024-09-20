use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use indexmap::IndexMap;

use crate::{
    ast::{checked::*, parsed::RangePattern, BinaryOp, UnaryOp},
    comptime_int::ComptimeInt,
    dgraph::{Dependencies, DependencyGraph},
    error::Diagnostics,
    nearest_pow_of_two,
    project::Project,
    sym::*,
    typecheck::{MemberFn, MemberFnType, TypeChecker},
    typeid::{
        BitSizeResult, CInt, FnPtr, GenericFn, GenericTrait, GenericUserType, Integer, Type,
        TypeArgs, TypeId, Types,
    },
    write_de, writeln_de, CodegenFlags, THIS_PARAM,
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
const ATTR_NOGEN: &str = "c_opaque";
const ATTR_LINKNAME: &str = "c_name";
const NULLPTR: &str = "((void*)0)";

#[derive(Default)]
struct TypeGen(DependencyGraph<TypeId>);

impl TypeGen {
    fn gen_fnptr(
        scopes: &Scopes,
        types: &mut Types,
        buffer: &mut Buffer,
        flags: &CodegenFlags,
        f: &FnPtr,
    ) {
        write_de!(buffer, "typedef ");
        if f.ret.is_void() {
            write_de!(buffer, "void");
        } else {
            buffer.emit_type(scopes, types, f.ret, flags.minify);
        }
        write_de!(buffer, "(*");
        buffer.emit_fnptr_name(scopes, types, f, flags.minify);
        write_de!(buffer, ")(");
        for (i, &param) in f.params.iter().enumerate() {
            if i > 0 {
                write_de!(buffer, ",");
            }

            buffer.emit_type(scopes, types, param, flags.minify);
        }
        write_de!(buffer, ");");
    }

    fn gen_dynptr(
        flags: &CodegenFlags,
        decls: &mut Buffer,
        defs: &mut Buffer,
        scopes: &Scopes,
        types: &mut Types,
        tr: &GenericTrait,
    ) {
        write_de!(decls, "typedef struct ");
        decls.emit_vtable_struct_name(scopes, types, tr, flags.minify);
        write_de!(decls, " ");
        decls.emit_vtable_struct_name(scopes, types, tr, flags.minify);
        write_de!(decls, ";typedef struct ");
        decls.emit_type_name(scopes, types, tr, flags.minify);
        write_de!(decls, "{{void*self;");
        decls.emit_vtable_struct_name(scopes, types, tr, flags.minify);
        write_de!(decls, " const*vtable;}}");
        decls.emit_type_name(scopes, types, tr, flags.minify);
        write_de!(decls, ";");

        write_de!(defs, "struct ");
        defs.emit_vtable_struct_name(scopes, types, tr, flags.minify);
        write_de!(defs, "{{");
        for id in scopes.get_trait_impls(tr.id) {
            for f in vtable_methods(scopes, types, scopes.get(id)) {
                let ret = scopes.get(f.id).ret.with_templates(types, &tr.ty_args);
                if ret.is_void() {
                    write_de!(defs, "void");
                } else {
                    defs.emit_type(scopes, types, ret, flags.minify);
                }
                write_de!(defs, "(*const ");
                defs.emit_fn_name(
                    scopes,
                    types,
                    &GenericFn::new(f.id, tr.ty_args.clone()),
                    flags.minify,
                );
                write_de!(defs, ")(");
                for (i, param) in scopes.get(f.id).params.iter().enumerate() {
                    let ty = param.ty.with_templates(types, &tr.ty_args);
                    if i > 0 {
                        write_de!(defs, ",");
                        defs.emit_type(scopes, types, ty, flags.minify);
                    } else if types[ty].is_ptr() {
                        write_de!(defs, "const void*");
                    } else {
                        write_de!(defs, "void*");
                    }
                }
                write_de!(defs, ");");
            }
        }
        write_de!(defs, "}};");
    }

    fn gen_usertype(
        flags: &CodegenFlags,
        decls: &mut Buffer,
        defs: &mut Buffer,
        scopes: &Scopes,
        types: &mut Types,
        ut: &GenericUserType,
    ) {
        fn emit_member(
            scopes: &Scopes,
            types: &mut Types,
            ut: &GenericUserType,
            name: &str,
            ty: TypeId,
            buffer: &mut Buffer,
            min: bool,
        ) {
            let ty = ty.with_templates(types, &ut.ty_args);
            if ty.size_and_align(scopes, types).0 == 0 {
                write_de!(buffer, "CTL_ZST ");
            }

            buffer.emit_type(scopes, types, ty, min);
            write_de!(buffer, " {};", member_name(scopes, Some(ut.id), name));
        }

        let ut_data = scopes.get(ut.id);
        if ut_data.kind.is_unsafe_union() {
            write_de!(decls, "typedef union ");
        } else {
            write_de!(decls, "typedef struct ");
        }
        decls.emit_type_name(scopes, types, ut, flags.minify);
        write_de!(decls, " ");
        decls.emit_type_name(scopes, types, ut, flags.minify);
        write_de!(decls, ";");
        if scopes.get(ut.id).attrs.has(ATTR_NOGEN) {
            return;
        }

        if ut_data.kind.is_unsafe_union() {
            write_de!(defs, "union ");
        } else {
            write_de!(defs, "struct ");
        }
        defs.emit_type_name(scopes, types, ut, flags.minify);
        write_de!(defs, "{{");

        let members = &scopes.get(ut.id).members;
        match &ut_data.kind {
            UserTypeKind::Union(union) => {
                if !matches!(types[union.tag], Type::Uint(0) | Type::Int(0)) {
                    defs.emit_type(scopes, types, union.tag, flags.minify);
                    write_de!(defs, " {UNION_TAG_NAME};");
                }

                for (name, member) in members {
                    emit_member(scopes, types, ut, name, member.ty, defs, flags.minify);
                }

                write_de!(defs, "union{{");
                for (name, variant) in union.variants.iter() {
                    if let Some(ty) = variant.ty {
                        emit_member(scopes, types, ut, name, ty, defs, flags.minify);
                    }
                }
                write_de!(defs, "}};");
            }
            UserTypeKind::PackedStruct(data) => {
                if members.is_empty() {
                    write_de!(defs, "CTL_DUMMY_MEMBER;");
                }

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

                for (name, member) in members {
                    emit_member(scopes, types, ut, name, member.ty, defs, flags.minify);
                }
            }
        }

        write_de!(defs, "}};");
    }

    fn emit(&self, scopes: &Scopes, types: &mut Types, decls: &mut Buffer, flags: &CodegenFlags) {
        let mut defs = Buffer::default();
        self.0.visit_all(|&id| match &types[id] {
            Type::Fn(f) => {
                let f = f.clone().as_fn_ptr(scopes, types);
                Self::gen_fnptr(scopes, types, &mut defs, flags, &f);
            }
            Type::FnPtr(f) => Self::gen_fnptr(scopes, types, &mut defs, flags, &f.clone()),
            Type::User(ut) => {
                Self::gen_usertype(flags, decls, &mut defs, scopes, types, &ut.clone());
            }
            Type::DynPtr(tr) | Type::DynMutPtr(tr) => {
                Self::gen_dynptr(flags, decls, &mut defs, scopes, types, &tr.clone());
            }
            &Type::Int(bits) | &Type::Uint(bits) if bits != 0 => {
                let signed = types[id].is_int();
                let nearest = nearest_pow_of_two(bits);
                let ch = if signed { 's' } else { 'u' };
                if flags.no_bit_int || nearest == bits as usize {
                    write_de!(
                        decls,
                        "typedef {}int{nearest}_t {ch}{bits};",
                        if signed { "" } else { "u" },
                    );
                } else {
                    write_de!(decls, "typedef {}INT({bits}){ch}{bits};", ch.to_uppercase());
                }
            }
            &Type::Array(ty, len) => {
                write_de!(decls, "typedef struct ");
                decls.emit_array_struct_name(scopes, types, ty, len, flags.minify);
                write_de!(decls, " ");
                decls.emit_array_struct_name(scopes, types, ty, len, flags.minify);
                write_de!(decls, ";");

                write_de!(defs, "struct ");
                defs.emit_array_struct_name(scopes, types, ty, len, flags.minify);
                write_de!(defs, "{{");
                defs.emit_type(scopes, types, ty, flags.minify);
                write_de!(defs, " {ARRAY_DATA_NAME}[{len}];}};");
            }
            _ => {}
        });
        decls.emit(defs.finish());
    }

    fn add_type(&mut self, scopes: &Scopes, types: &mut Types, ty: TypeId) {
        if self.0.contains_key(&ty) {
            return;
        }

        let mut deps = Vec::new();
        macro_rules! dependency {
            ($ty: expr) => {{
                let dep = $ty;
                let mut inner = dep;
                while let Type::Ptr(id) | Type::MutPtr(id) | Type::RawPtr(id) = &types[inner] {
                    inner = *id;
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

        match &types[ty] {
            Type::Int(_) | Type::Uint(_) | Type::DynMutPtr(_) | Type::DynPtr(_) => {}
            Type::FnPtr(f) => {
                let ret = f.ret;
                for param in f.params.clone() {
                    self.add_type(scopes, types, param);
                }
                self.add_type(scopes, types, ret);
            }
            Type::Fn(f) => {
                let f = f.clone().as_fn_ptr(scopes, types);
                for param in f.params {
                    self.add_type(scopes, types, param);
                }
                self.add_type(scopes, types, f.ret);
            }
            &Type::Array(ty, _) => dependency!(ty),
            Type::User(ut) => {
                self.0.insert(ty, Dependencies::Resolving);
                let ut = ut.clone();
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
            Type::Ptr(inner) | Type::MutPtr(inner) | Type::RawPtr(inner) => {
                return self.add_type(scopes, types, *inner);
            }
            _ => return,
        }

        self.0.insert(ty, Dependencies::Resolved(deps));
    }
}

#[derive(Eq, Clone)]
struct State {
    func: GenericFn,
    tmpvar: usize,
    caller: ScopeId,
    emitted_names: HashMap<String, VariableId>,
    renames: HashMap<VariableId, String>,
}

impl State {
    pub fn new(func: GenericFn, caller: ScopeId) -> Self {
        Self {
            func,
            caller,
            tmpvar: 0,
            emitted_names: Default::default(),
            renames: Default::default(),
        }
    }

    pub fn in_body_scope(func: GenericFn, scopes: &Scopes) -> Self {
        let scope = scopes.get(func.id).body_scope;
        Self::new(func, scope)
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

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Buffer(String);

impl Buffer {
    fn emit(&mut self, source: impl AsRef<str>) {
        _ = self.write_str(source.as_ref());
    }

    fn emit_mangled_name(&mut self, scopes: &Scopes, types: &mut Types, id: TypeId, min: bool) {
        match &types[id] {
            Type::Void => write_de!(self, "void"),
            Type::CVoid => write_de!(self, "c_void"),
            Type::Never => write_de!(self, "never"),
            Type::Int(bits) => write_de!(self, "i{bits}"),
            Type::Uint(bits) => write_de!(self, "u{bits}"),
            Type::CInt(inner) => write_de!(self, "c_{inner:#}"),
            Type::CUint(inner) => write_de!(self, "c_u{inner:#}"),
            Type::Isize => write_de!(self, "isize"),
            Type::Usize => write_de!(self, "usize"),
            Type::F32 => write_de!(self, "f32"),
            Type::F64 => write_de!(self, "f64"),
            Type::Bool => write_de!(self, "bool"),
            Type::Char => write_de!(self, "char"),
            &Type::Ptr(inner) => {
                self.emit(if min { "p" } else { "ptr_" });
                self.emit_mangled_name(scopes, types, inner, min);
            }
            &Type::MutPtr(inner) => {
                self.emit(if min { "m" } else { "mutptr_" });
                self.emit_mangled_name(scopes, types, inner, min);
            }
            &Type::RawPtr(inner) => {
                self.emit(if min { "r" } else { "rawptr_" });
                self.emit_mangled_name(scopes, types, inner, min);
            }
            Type::DynPtr(tr) | Type::DynMutPtr(tr) => {
                self.emit_type_name(scopes, types, &tr.clone(), min);
            }
            Type::FnPtr(f) => self.emit_fnptr_name(scopes, types, &f.clone(), min),
            Type::Fn(f) => {
                let fptr = f.clone().as_fn_ptr(scopes, types);
                self.emit_fnptr_name(scopes, types, &fptr, min)
            }
            Type::User(ut) => {
                let ut = ut.clone();
                self.emit_type_name_ex(scopes, types, &ut, min, true);
            }
            &Type::Array(ty, len) => self.emit_array_struct_name(scopes, types, ty, len, min),
            Type::Unknown => {
                write_de!(self, "__Unknown");
                eprintln!("ICE: TypeId::Unknown in emit_generic_mangled_name")
            }
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_generic_mangled_name"),
        }
    }

    fn emit_fnptr_name(&mut self, scopes: &Scopes, types: &mut Types, f: &FnPtr, min: bool) {
        write_de!(self, "fn");
        self.emit_mangled_name(scopes, types, f.ret, min);
        for (i, &param) in f.params.iter().enumerate() {
            if i > 0 && !min {
                write_de!(self, "_");
            }
            self.emit_mangled_name(scopes, types, param, min);
        }
    }

    fn emit_type(&mut self, scopes: &Scopes, types: &mut Types, id: TypeId, min: bool) {
        match &types[id] {
            Type::Void | Type::Never | Type::CVoid => write_de!(self, "$void"),
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
            id @ (&Type::Ptr(inner) | &Type::MutPtr(inner) | &Type::RawPtr(inner)) => {
                let id_is_ptr = id.is_ptr();
                if let &Type::Array(ty, _) = &types[inner] {
                    self.emit_type(scopes, types, ty, min);
                    if !min {
                        write_de!(self, "/*");
                        self.emit_type(scopes, types, inner, min);
                        write_de!(self, "*/");
                    }
                } else if types[inner].is_c_void() {
                    write_de!(self, "void");
                } else {
                    self.emit_type(scopes, types, inner, min);
                }
                if id_is_ptr {
                    write_de!(self, " const*");
                } else {
                    write_de!(self, "*");
                }
            }
            Type::FnPtr(_) => self.emit_mangled_name(scopes, types, id, min),
            Type::Fn(_) => self.emit_mangled_name(scopes, types, id, min),
            Type::User(ut) => {
                if scopes.get(ut.id).kind.is_template() {
                    eprintln!("ICE: Template type in emit_type");
                    self.emit(&scopes.get(ut.id).name.data);
                    return;
                }

                if let Some(ty) = id.can_omit_tag(scopes, types) {
                    self.emit_type(scopes, types, ty, min);
                } else {
                    self.emit_type_name(scopes, types, &ut.clone(), min);
                }
            }
            &Type::Array(_, _) => self.emit_mangled_name(scopes, types, id, min),
            Type::DynPtr(tr) | Type::DynMutPtr(tr) => {
                self.emit_type_name(scopes, types, &tr.clone(), min)
            }
            Type::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_type"),
        }
    }

    fn emit_type_name(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        ut: &GenericUserType,
        min: bool,
    ) {
        self.emit_type_name_ex(scopes, types, ut, min, false)
    }

    fn emit_type_name_ex(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        ut: &GenericUserType,
        min: bool,
        generic: bool,
    ) {
        let ty = scopes.get(ut.id);
        if ty.kind.is_template() {
            eprintln!("ICE: Template type in emit_type_name_ex");
        }
        if ty.name.data.is_empty() {
            eprintln!("ty is empty");
        }

        if !generic {
            if let Some(name) = scopes.get(ut.id).attrs.val(ATTR_LINKNAME) {
                return self.emit(name);
            } else if scopes.get(ut.id).attrs.has(ATTR_NOGEN) {
                return self.emit(&scopes.get(ut.id).name.data);
            }
        }

        if min {
            write_de!(self, "t{}", ut.id);
            for &ty in ut.ty_args.values() {
                self.emit_mangled_name(scopes, types, ty, min);
            }
        } else {
            self.emit(scopes.full_name(ty.scope, &ty.name.data));
            if !ut.ty_args.is_empty() {
                write_de!(self, "$");
                for &ty in ut.ty_args.values() {
                    write_de!(self, "$");
                    self.emit_mangled_name(scopes, types, ty, min);
                }
                write_de!(self, "$$");
            }
        }
    }

    fn emit_array_struct_name(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        ty: TypeId,
        size: usize,
        min: bool,
    ) {
        if min {
            write_de!(self, "a");
            self.emit_mangled_name(scopes, types, ty, min);
            write_de!(self, "{size}");
        } else {
            write_de!(self, "Array_");
            self.emit_mangled_name(scopes, types, ty, min);
            write_de!(self, "_{size}");
        }
    }

    fn emit_fn_name(&mut self, scopes: &Scopes, types: &mut Types, func: &GenericFn, min: bool) {
        let f = scopes.get(func.id);
        if !f.is_extern {
            if min {
                write_de!(self, "p{}", func.id);
                for &ty in func.ty_args.values() {
                    self.emit_mangled_name(scopes, types, ty, min);
                }
            } else {
                self.emit(scopes.full_name(f.scope, &f.name.data));
                if !func.ty_args.is_empty() {
                    write_de!(self, "$");
                    for &ty in func.ty_args.values() {
                        write_de!(self, "$");
                        self.emit_mangled_name(scopes, types, ty, min);
                    }
                    write_de!(self, "$$");
                }
            }
        } else {
            self.emit(f.attrs.val(ATTR_LINKNAME).unwrap_or(&f.name.data));
        }
    }

    fn emit_vtable_struct_name(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        tr: &GenericTrait,
        min: bool,
    ) {
        self.emit(if min { "v" } else { "$vtable_" });
        self.emit_type_name(scopes, types, tr, min);
    }

    fn finish(self) -> String {
        self.0
    }
}

impl Write for Buffer {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write_str(s)
    }
}

struct JoiningBuilder {
    buffer: Buffer,
    join: &'static str,
    default: &'static str,
}

impl JoiningBuilder {
    pub fn new(join: &'static str, default: &'static str) -> Self {
        Self {
            buffer: Default::default(),
            join,
            default,
        }
    }

    pub fn next(&mut self, f: impl FnOnce(&mut Buffer)) {
        if !self.buffer.0.is_empty() {
            self.buffer.emit(self.join);
        }
        f(&mut self.buffer);
    }

    pub fn next_str(&mut self, s: impl AsRef<str>) {
        self.next(|buf| buf.emit(s));
    }

    pub fn finish(self) -> String {
        if self.buffer.0.is_empty() {
            self.default.into()
        } else {
            self.buffer.finish()
        }
    }
}

macro_rules! hoist {
    ($self: expr, $body: expr) => {{
        let buffer = std::mem::take(&mut $self.buffer);
        let result = $body;
        $self
            .temporaries
            .emit(std::mem::replace(&mut $self.buffer, buffer).finish());
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
        let $tmp = scope_var_or_label(scope);
        hoist!($self, {
            $self.emit_type(ty);
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
        let $tmp = scope_var_or_label($self.cur_loop);
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
        let old_tmp = std::mem::take(&mut $self.temporaries);
        let old_buf = std::mem::take(&mut $self.buffer);
        $body;
        let written = std::mem::replace(&mut $self.buffer, old_buf).finish();
        $self
            .buffer
            .emit(std::mem::replace(&mut $self.temporaries, old_tmp).finish());
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

#[derive(PartialEq, Eq, Hash)]
struct Vtable {
    tr: GenericTrait,
    ty: TypeId,
    scope: ScopeId,
}

pub struct Codegen {
    proj: Project,
    buffer: Buffer,
    temporaries: Buffer,
    funcs: HashSet<State>,
    statics: HashSet<VariableId>,
    emitted_never_in_this_block: bool,
    cur_block: ScopeId,
    cur_loop: ScopeId,
    flags: CodegenFlags,
    vtables: Buffer,
    emitted_vtables: HashSet<Vtable>,
    defers: Vec<(ScopeId, Vec<Expr>)>,
    tg: TypeGen,
}

impl Codegen {
    pub fn build(proj: Project, flags: CodegenFlags) -> (String, Diagnostics) {
        let exports = proj
            .scopes
            .functions()
            .filter(|(_, f)| f.is_extern && f.body.is_some())
            .map(|(id, _)| {
                State::in_body_scope(GenericFn::from_id(&proj.scopes, id), &proj.scopes)
            });
        let (funcs, main) = if flags.lib {
            (exports.collect(), None)
        } else {
            let main = State::in_body_scope(
                GenericFn::from_id(&proj.scopes, proj.main.unwrap()),
                &proj.scopes,
            );
            (
                exports.chain(std::iter::once(main.clone())).collect(),
                Some(main),
            )
        };
        let mut this = Self {
            proj,
            funcs,
            flags,
            emitted_never_in_this_block: false,
            buffer: Default::default(),
            temporaries: Default::default(),
            cur_block: Default::default(),
            cur_loop: Default::default(),
            vtables: Default::default(),
            statics: Default::default(),
            emitted_vtables: Default::default(),
            defers: Default::default(),
            tg: Default::default(),
        };
        let main = main.map(|mut main| this.gen_c_main(&mut main));
        let mut static_defs = Buffer::default();
        let mut static_init = Buffer::default();
        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        let mut emitted_statics = HashSet::new();
        let static_state = &mut State::new(
            GenericFn::from_id(&this.proj.scopes, FunctionId::RESERVED),
            ScopeId::ROOT,
        );
        let static_deps = std::mem::take(&mut this.proj.static_deps);
        while !this.funcs.is_empty() || !this.statics.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for mut state in diff {
                this.emit_fn(&mut state, &mut prototypes);
            }

            for var in std::mem::take(&mut this.statics) {
                static_deps.dfs(var, &mut emitted_statics, |id| {
                    static_state.caller = this.proj.scopes.get(id).scope;
                    usebuf!(this, &mut static_defs, {
                        this.emit_var_decl(id, static_state);
                        write_de!(this.buffer, ";");
                    });

                    usebuf!(
                        this,
                        &mut static_init,
                        hoist_point!(this, {
                            this.emit_var_name(id, static_state);
                            write_de!(this.buffer, "=");
                            this.emit_expr_inner(
                                this.proj.scopes.get(id).value.clone().unwrap(),
                                static_state,
                            );
                            write_de!(this.buffer, ";");
                        })
                    );
                });
            }
        }
        let functions = std::mem::take(&mut this.buffer);
        if this.flags.leak {
            writeln_de!(this.buffer, "#define CTL_NOGC");
        }
        if this.flags.no_bit_int {
            writeln_de!(this.buffer, "#define CTL_NOBITINT");
        }
        this.buffer.emit(include_str!("../ctl/ctl.h"));
        this.tg.emit(
            &this.proj.scopes,
            &mut this.proj.types,
            &mut this.buffer,
            &this.flags,
        );
        this.buffer.emit(prototypes.finish());
        this.buffer.emit(this.vtables.finish());
        this.buffer.emit(static_defs.finish());
        this.buffer.emit(functions.finish());
        write_de!(this.buffer, "static void $ctl_static_init(void){{");
        this.buffer.emit(static_init.finish());
        write_de!(this.buffer, "}}static void $ctl_static_deinit(void){{}}");
        if let Some(main) = main {
            this.buffer.emit(main);
        }

        (this.buffer.finish(), this.proj.diag)
    }

    fn emit_vtable(&mut self, vtable: Vtable) {
        if self.emitted_vtables.contains(&vtable) {
            return;
        }

        let mut buffer = Buffer::default();
        usebuf!(self, &mut buffer, {
            write_de!(self.buffer, "static const ");
            self.buffer.emit_vtable_struct_name(
                &self.proj.scopes,
                &mut self.proj.types,
                &vtable.tr,
                self.flags.minify,
            );
            write_de!(self.buffer, " ");
            self.emit_vtable_name(&vtable);
            write_de!(self.buffer, "={{");
            for tr in self
                .proj
                .scopes
                .get_trait_impls_ex(&mut self.proj.types, vtable.tr.clone())
            {
                for f in vtable_methods(
                    &self.proj.scopes,
                    &self.proj.types,
                    self.proj.scopes.get(tr.id),
                ) {
                    let func = GenericFn::new(f.id, tr.ty_args.clone());
                    let func_data = self.proj.scopes.get(f.id);

                    write_de!(self.buffer, ".");
                    self.buffer.emit_fn_name(
                        &self.proj.scopes,
                        &mut self.proj.types,
                        &func,
                        self.flags.minify,
                    );
                    write_de!(self.buffer, "=(");
                    let ret = func_data
                        .ret
                        .with_templates(&mut self.proj.types, &func.ty_args);
                    if ret.is_void() {
                        write_de!(self.buffer, "void");
                    } else {
                        self.buffer.emit_type(
                            &self.proj.scopes,
                            &mut self.proj.types,
                            ret,
                            self.flags.minify,
                        );
                    }
                    write_de!(self.buffer, "(*)(");
                    for (i, param) in func_data.params.iter().enumerate() {
                        let param_ty = param.ty.with_templates(&mut self.proj.types, &func.ty_args);
                        if i > 0 {
                            write_de!(self.buffer, ",");
                            self.buffer.emit_type(
                                &self.proj.scopes,
                                &mut self.proj.types,
                                param_ty,
                                self.flags.minify,
                            );
                        } else if self.proj.types[param_ty].is_ptr() {
                            write_de!(self.buffer, "const void*");
                        } else {
                            write_de!(self.buffer, "void*");
                        }
                    }
                    write_de!(self.buffer, "))");

                    let func = self.find_implementation(
                        vtable.ty,
                        &tr,
                        &self.proj.scopes.get(f.id).name.data.clone(),
                        vtable.scope,
                        |_, _| Default::default(),
                    );
                    self.buffer.emit_fn_name(
                        &self.proj.scopes,
                        &mut self.proj.types,
                        &func,
                        self.flags.minify,
                    );
                    write_de!(self.buffer, ",");
                    self.funcs.insert(State::new(func, vtable.scope));
                }
            }
            write_de!(self.buffer, "}};");
        });

        self.vtables.emit(buffer.finish());
        self.emitted_vtables.insert(vtable);
    }

    fn gen_c_main(&mut self, main: &mut State) -> String {
        write_de!(self.buffer, "int main(int argc, char **argv){{");
        let returns = !self.proj.scopes.get(main.func.id).ret.is_void();
        if let Some(id) = self
            .proj
            .scopes
            .lang_fns
            .get("convert_argv")
            .cloned()
            .filter(|_| self.proj.scopes.get(main.func.id).params.len() == 1)
        {
            let state =
                State::in_body_scope(GenericFn::from_id(&self.proj.scopes, id), &self.proj.scopes);
            if returns {
                write_de!(self.buffer, "return ");
            }
            self.buffer.emit_fn_name(
                &self.proj.scopes,
                &mut self.proj.types,
                &main.func,
                self.flags.minify,
            );
            write_de!(self.buffer, "(");
            self.buffer.emit_fn_name(
                &self.proj.scopes,
                &mut self.proj.types,
                &state.func,
                self.flags.minify,
            );
            if returns {
                write_de!(self.buffer, "(argc,(const char **)argv));}}");
            } else {
                write_de!(self.buffer, "(argc,(const char **)argv));return 0;}}");
            }

            self.funcs.insert(state);
        } else {
            write_de!(self.buffer, "(void)argc;(void)argv;");
            if returns {
                write_de!(self.buffer, "return ");
            }
            self.buffer.emit_fn_name(
                &self.proj.scopes,
                &mut self.proj.types,
                &main.func,
                self.flags.minify,
            );
            if returns {
                write_de!(self.buffer, "();}}");
            } else {
                write_de!(self.buffer, "();return 0;}}");
            }
        }
        std::mem::take(&mut self.buffer).finish()
    }

    fn emit_fn(&mut self, state: &mut State, prototypes: &mut Buffer) {
        // TODO: emit an error if a function has the c_macro attribute and a body
        let func = self.proj.scopes.get(state.func.id);
        if func.attrs.has(ATTR_NOGEN) {
            return;
        }

        usebuf!(self, prototypes, {
            self.emit_prototype(state, true);
            write_de!(self.buffer, ";");
        });
        let func = self.proj.scopes.get(state.func.id);
        if let Some(body) = func.body.clone() {
            let void_return = func
                .ret
                .with_templates(&mut self.proj.types, &state.func.ty_args)
                .is_void();
            let params = func.params.clone();
            let unused = self.emit_prototype(state, false);
            write_de!(self.buffer, "{{");
            for id in unused {
                write_de!(self.buffer, "(void)");
                self.emit_var_name(id, state);
                write_de!(self.buffer, ";");
            }

            for param in params.iter() {
                let Some(patt) = param
                    .patt
                    .as_checked()
                    .filter(|patt| !matches!(patt.data, PatternData::Variable(_)))
                else {
                    continue;
                };

                let ty = param
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                self.emit_pattern_bindings(state, &patt.data, &param.label, ty);
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
        if Self::has_side_effects(&expr) && !expr.ty.is_void() {
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
            Stmt::Expr(expr) => {
                hoist_point!(self, self.emit_expr_stmt(expr, state))
            }
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
                    let ty = value
                        .ty
                        .with_templates(&mut self.proj.types, &state.func.ty_args);
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
        expr.ty = expr
            .ty
            .with_templates(&mut self.proj.types, &state.func.ty_args);
        if Self::has_side_effects(&expr) {
            self.emit_tmpvar_ident(expr, state);
        } else {
            self.emit_expr_inner(expr, state);
        }
    }

    fn emit_expr_inner(&mut self, expr: Expr, state: &mut State) {
        match expr.data {
            ExprData::Binary(op, lhs, rhs) => self.emit_binary(state, op, expr.ty, *lhs, *rhs),
            ExprData::Unary(op, inner) => self.emit_unary(state, op, expr.ty, *inner),
            ExprData::AutoDeref(expr, count) => {
                write_de!(self.buffer, "({:*<1$}", "", count);
                self.emit_expr(*expr, state);
                write_de!(self.buffer, ")");
            }
            ExprData::DynCoerce(mut inner, scope) => {
                inner.ty = inner
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let vtable =
                    if let Type::Ptr(inner) | Type::MutPtr(inner) = &self.proj.types[inner.ty] {
                        Vtable {
                            tr: self
                                .proj
                                .types
                                .get(expr.ty)
                                .as_dyn_pointee()
                                .expect("ICE: DynCoerce to non dyn pointer")
                                .clone(),
                            ty: *inner,
                            scope,
                        }
                    } else {
                        panic!("ICE: DynCoerce from non-pointer");
                    };

                self.emit_cast(expr.ty);
                write_de!(self.buffer, "{{.self=(void*)");
                self.emit_expr(*inner, state);
                write_de!(self.buffer, ",.vtable=&");
                self.emit_vtable_name(&vtable);
                write_de!(self.buffer, "}}");
                self.emit_vtable(vtable);
            }
            ExprData::Call(callee, args) => {
                let func = self.proj.types[callee.ty].as_fn().unwrap();
                if let Some(name) = self.proj.scopes.intrinsic_name(func.id) {
                    let mut func = func.clone();
                    func.fill_templates(&mut self.proj.types, &state.func.ty_args);
                    let name = name.to_string();
                    return self.emit_intrinsic(&name, expr.ty, &func, args, state);
                } else if let Some(id) = self.proj.scopes.get(func.id).constructor {
                    if self.proj.scopes.get(id).kind.is_union() {
                        return self.emit_variant_instance(
                            state,
                            expr.ty,
                            &self.proj.scopes.get(func.id).name.data.clone(),
                            args,
                        );
                    } else {
                        return self.emit_instance(state, expr.ty, args);
                    }
                }

                if expr.ty.is_void() {
                    write_de!(self.buffer, "VOID(");
                }
                let id = func.id;
                self.emit_expr(*callee, state);
                write_de!(self.buffer, "(");
                self.finish_emit_fn_args(state, id, args);
                if expr.ty.is_void() {
                    write_de!(self.buffer, ")");
                }
            }
            ExprData::CallDyn(func, mut args) => {
                if expr.ty.is_void() {
                    write_de!(self.buffer, "VOID(");
                }
                let (_, recv) = args.shift_remove_index(0).unwrap();
                let recv = hoist!(self, self.emit_tmpvar(recv, state));
                write_de!(self.buffer, "{recv}.vtable->");
                self.buffer.emit_fn_name(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    &func,
                    self.flags.minify,
                );
                write_de!(self.buffer, "({recv}.self");
                if args.is_empty() {
                    write_de!(self.buffer, ")");
                } else {
                    write_de!(self.buffer, ",");
                    self.finish_emit_fn_args(state, func.id, args);
                }
                if expr.ty.is_void() {
                    write_de!(self.buffer, ")");
                }
            }
            ExprData::CallFnPtr(inner, args) => {
                if expr.ty.is_void() {
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
                if expr.ty.is_void() {
                    write_de!(self.buffer, ")");
                }
            }
            ExprData::Array(exprs) => {
                write_de!(self.buffer, "(");
                self.emit_type(expr.ty);
                write_de!(self.buffer, "){{.{ARRAY_DATA_NAME}={{");
                for expr in exprs {
                    self.emit_expr(expr, state);
                    write_de!(self.buffer, ",");
                }
                write_de!(self.buffer, "}}}}");
            }
            ExprData::ArrayWithInit { init, count } => {
                // number chosen arbitrarily
                if count <= 32 {
                    write_de!(self.buffer, "(");
                    self.emit_type(expr.ty);
                    write_de!(self.buffer, "){{.{ARRAY_DATA_NAME}={{");
                    for _ in 0..count {
                        self.emit_expr((*init).clone(), state);
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
                        self.emit_expr_inline(*init, state);
                        write_de!(self.buffer, ";}}");
                    })
                }
            }
            ExprData::Vec(exprs) => {
                let ut = self.proj.types[expr.ty].as_user().unwrap().clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    let len = exprs.len();
                    self.emit_with_capacity(expr.ty, &tmp, &ut, len);
                    for (i, expr) in exprs.into_iter().enumerate() {
                        write_de!(self.buffer, "{tmp}.$ptr[{i}]=");
                        self.emit_expr_inline(expr, state);
                        write_de!(self.buffer, ";");
                    }
                    write_de!(self.buffer, "{tmp}.$len={len};");
                });
            }
            ExprData::VecWithInit { init, count } => {
                tmpbuf_emit!(self, state, |tmp| {
                    let ut = self.proj.types[expr.ty].as_user().unwrap().clone();
                    let len = self.emit_tmpvar(*count, state);
                    self.emit_with_capacity(expr.ty, &tmp, &ut, &len);
                    write_de!(self.buffer, "for(usize i=0;i<{len};i++){{");
                    hoist_point!(self, {
                        write_de!(self.buffer, "((");
                        self.emit_type(ut.first_type_arg().unwrap());
                        write_de!(self.buffer, "*){tmp}.$ptr)[i]=");
                        self.emit_expr_inline(*init, state);
                        write_de!(self.buffer, ";}}{tmp}.$len={len};");
                    });
                });
            }
            ExprData::Set(exprs, scope) => {
                let ut = self.proj.types[expr.ty].as_user().unwrap().clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_with_capacity(expr.ty, &tmp, &ut, exprs.len());
                    let insert = State::with_inst(
                        GenericFn::from_id(
                            &self.proj.scopes,
                            self.proj
                                .scopes
                                .get(ut.id)
                                .find_associated_fn(&self.proj.scopes, "insert")
                                .unwrap(),
                        ),
                        &self.proj.types,
                        expr.ty,
                        scope,
                    );

                    for val in exprs {
                        self.buffer.emit_fn_name(
                            &self.proj.scopes,
                            &mut self.proj.types,
                            &insert.func,
                            self.flags.minify,
                        );
                        write_de!(self.buffer, "(&{tmp},");
                        self.emit_expr_inline(val, state);
                        write_de!(self.buffer, ");");
                    }
                    self.funcs.insert(insert);
                });
            }
            ExprData::Map(exprs, scope) => {
                let ut = self.proj.types[expr.ty].as_user().unwrap().clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    let insert = State::with_inst(
                        GenericFn::from_id(
                            &self.proj.scopes,
                            self.proj
                                .scopes
                                .get(ut.id)
                                .find_associated_fn(&self.proj.scopes, "insert")
                                .unwrap(),
                        ),
                        &self.proj.types,
                        expr.ty,
                        scope,
                    );

                    self.emit_with_capacity(expr.ty, &tmp, &ut, exprs.len());
                    for (key, val) in exprs {
                        self.buffer.emit_fn_name(
                            &self.proj.scopes,
                            &mut self.proj.types,
                            &insert.func,
                            self.flags.minify,
                        );
                        write_de!(self.buffer, "(&{tmp},");
                        self.emit_expr(key, state);
                        write_de!(self.buffer, ",");
                        self.emit_expr(val, state);
                        write_de!(self.buffer, ");");
                    }
                    self.funcs.insert(insert);
                });
            }
            ExprData::Int(value) => self.emit_literal(value, expr.ty),
            ExprData::Float(mut value) => {
                self.emit_cast(expr.ty);
                value.retain(|c| c != '_');
                self.buffer.emit(value);
            }
            ExprData::String(value) => self.emit_string_literal(expr.ty, &value),
            ExprData::ByteString(value) => {
                self.emit_cast(expr.ty);
                write_de!(self.buffer, "\"");
                for byte in value {
                    write_de!(self.buffer, "\\x{byte:x}");
                }
                write_de!(self.buffer, "\"");
            }
            ExprData::Void => self.buffer.emit(VOID_INSTANCE),
            ExprData::Fn(mut func, scope) => {
                func.fill_templates(&mut self.proj.types, &state.func.ty_args);
                let state = State::new(func, scope);
                self.buffer.emit_fn_name(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    &state.func,
                    self.flags.minify,
                );
                self.funcs.insert(state);
            }
            ExprData::MemFn(mfn, scope) => self.emit_member_fn(state, mfn, scope),
            ExprData::Var(id) => {
                if self.proj.scopes.get(id).is_static {
                    self.statics.insert(id);
                }
                self.emit_var_name(id, state);
            }
            ExprData::Instance(members) => self.emit_instance(state, expr.ty, members),
            ExprData::VariantInstance(name, members) => {
                self.emit_variant_instance(state, expr.ty, &name, members)
            }
            ExprData::Member { source, member } => {
                let id = self.proj.types[source.ty].as_user().map(|ut| ut.id);
                if let Some(id) = id.filter(|&id| self.proj.scopes.get(id).kind.is_packed_struct())
                {
                    let tmp = tmpbuf!(self, state, |tmp| {
                        let ptr = self.proj.types.insert(Type::Ptr(source.ty));
                        self.emit_type(ptr);
                        writeln_de!(self.buffer, " {tmp}=&");
                        self.emit_expr_inline(*source, state);
                        writeln_de!(self.buffer, ";");
                        tmp
                    });
                    self.emit_bitfield_read(&format!("(*{tmp})"), id, &member, expr.ty);
                } else {
                    self.emit_expr(*source, state);
                    write_de!(
                        self.buffer,
                        ".{}",
                        member_name(&self.proj.scopes, id, &member)
                    );
                }
            }
            ExprData::Block(block) => enter_block!(self, block.scope, expr.ty, |name| {
                let yields = block.is_yielding(&self.proj.scopes);
                write_nm!(self, "{{");
                self.emit_block(block, state);
                if !yields {
                    write_de!(self.buffer, "{name}={VOID_INSTANCE};");
                } else {
                    write_de!(self.buffer, "{name}:;");
                }
                write_nm!(self, "}}");
            }),
            ExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                let dummy = self
                    .proj
                    .scopes
                    .create_scope(ScopeId::ROOT, ScopeKind::None, false);
                enter_block!(self, dummy, expr.ty, |name| {
                    write_de!(self.buffer, "if(");
                    self.emit_expr_inline(*cond, state);
                    write_de!(self.buffer, "){{");
                    hoist_point!(self, {
                        write_de!(self.buffer, "{name}=");
                        self.emit_expr_inline(*if_branch, state);
                    });
                    write_de!(self.buffer, ";}}else{{");
                    if let Some(else_branch) = else_branch {
                        hoist_point!(self, {
                            write_de!(self.buffer, "{name}=");
                            self.emit_expr_inline(*else_branch, state);
                        });
                    } else {
                        write_de!(self.buffer, "{name}={VOID_INSTANCE}");
                    }

                    write_de!(self.buffer, ";}}");
                })
            }
            ExprData::Loop {
                cond,
                body,
                do_while,
                optional,
            } => {
                let scope = body.scope;
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
                            cond.ty = cond
                                .ty
                                .with_templates(&mut self.proj.types, &state.func.ty_args);
                            if !do_while {
                                cond!(*cond);
                                self.emit_block(body, state);
                            } else {
                                self.emit_block(body, state);
                                cond!(*cond);
                            }
                        } else {
                            self.emit_block(body, state);
                        }
                    });
                    write_de!(self.buffer, "{}:;}}", loop_cont_label(self.cur_loop));
                    if self.proj.scopes[scope].kind.as_loop().unwrap().breaks != LoopBreak::None {
                        write_de!(self.buffer, "{name}:;");
                    }
                });
            }
            ExprData::Subscript { callee, arg } => {
                // TODO: bounds check
                if self.proj.types[callee.ty].is_array() {
                    match callee.data {
                        ExprData::Unary(UnaryOp::Deref, expr) => {
                            // we compile pointers to arrays as T *, not Array_T_N *, so in the case
                            // there will be no data member
                            self.emit_expr(*expr, state);
                        }
                        _ => {
                            self.emit_expr(*callee, state);
                            write_de!(self.buffer, ".{ARRAY_DATA_NAME}");
                        }
                    }
                } else {
                    write_de!(
                        self.buffer,
                        "({}",
                        "*".repeat(Self::indirection(&self.proj.types, callee.ty) - 1)
                    );
                    self.emit_expr(*callee, state);
                    write_de!(self.buffer, ")");
                }

                write_de!(self.buffer, "[");
                self.emit_expr(*arg, state);
                write_de!(self.buffer, "]");
            }
            ExprData::SliceArray { callee, arg } => {
                let indirection = Self::indirection(&self.proj.types, callee.ty);
                let src = tmpbuf!(self, state, |tmp| {
                    let len = *callee
                        .ty
                        .strip_references_r(&self.proj.types)
                        .as_array()
                        .unwrap()
                        .1;
                    self.emit_type(expr.ty);
                    write_de!(self.buffer, " {tmp}={{.$ptr=");
                    if indirection != 0 {
                        self.buffer.emit("*".repeat(indirection - 1));
                        self.emit_expr_inline(*callee, state);
                    } else {
                        self.emit_expr_inline(*callee, state);
                        write_de!(self.buffer, ".{ARRAY_DATA_NAME}");
                    }
                    write_de!(self.buffer, ",.$len={len}}};");
                    tmp
                });
                let ut = self.proj.types[expr.ty].as_user().unwrap().clone();
                let mut func = GenericFn::from_type_args(
                    &self.proj.scopes,
                    self.proj
                        .scopes
                        .get(ut.id)
                        .find_associated_fn(&self.proj.scopes, "subspan")
                        .unwrap(),
                    [arg.ty
                        .with_templates(&mut self.proj.types, &state.func.ty_args)],
                );
                func.ty_args.copy_args(&ut.ty_args);
                let new_state = State::in_body_scope(func, &self.proj.scopes);
                self.buffer.emit_fn_name(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    &new_state.func,
                    self.flags.minify,
                );
                write_de!(self.buffer, "({src}, ");
                self.emit_expr_inline(*arg, state);
                write_de!(self.buffer, ")");
                self.funcs.insert(new_state);
            }
            ExprData::Return(mut expr) => never_expr!(self, {
                expr.ty = expr
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let void = expr.ty.is_void();
                let tmp = self.emit_tmpvar(*expr, state);
                let str = if void {
                    write_de!(self.buffer, "(void){tmp};");
                    "return".into()
                } else {
                    format!("return {tmp}")
                };
                self.leave_scope(state, &str, self.proj.scopes.get(state.func.id).body_scope);
            }),
            ExprData::Yield(expr, scope) => never_expr!(self, {
                write_de!(self.buffer, "{}=", scope_var_or_label(scope));
                if let Some(expr) = expr {
                    self.emit_expr_inline(*expr, state);
                } else {
                    self.buffer.emit(VOID_INSTANCE);
                }
                write_de!(self.buffer, ";");

                // if scope != self.cur_block {
                self.leave_scope(state, &format!("goto {}", scope_var_or_label(scope)), scope);
                // }
            }),
            ExprData::Break(expr, scope) => never_expr!(self, {
                write_de!(self.buffer, "{}=", scope_var_or_label(scope));
                if let Some(expr) = expr {
                    self.emit_expr_inline(*expr, state);
                } else {
                    self.buffer.emit(VOID_INSTANCE);
                }
                write_de!(self.buffer, ";");
                if self.cur_loop == scope {
                    self.leave_scope(state, "break", scope);
                } else {
                    self.leave_scope(state, &format!("goto {}", scope_var_or_label(scope)), scope);
                }
            }),
            ExprData::Continue(scope) => never_expr!(self, {
                if self.cur_loop == scope {
                    self.leave_scope(state, "continue", scope);
                } else {
                    self.leave_scope(state, &format!("goto {}", loop_cont_label(scope)), scope);
                }
            }),
            ExprData::Match {
                expr: mut scrutinee,
                body,
            } => {
                let dummy = self
                    .proj
                    .scopes
                    .create_scope(ScopeId::ROOT, ScopeKind::None, false);
                enter_block!(self, dummy, expr.ty, |name| {
                    scrutinee.ty = scrutinee
                        .ty
                        .with_templates(&mut self.proj.types, &state.func.ty_args);

                    let ty = scrutinee.ty;
                    let tmp = self.emit_tmpvar(*scrutinee, state);
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
            ExprData::As(mut inner, _) => {
                inner.ty = inner
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                // enum tag cast
                self.emit_cast(expr.ty);
                write_de!(self.buffer, "(");
                if self.proj.types[inner.ty].is_user() {
                    self.emit_expr(*inner, state);
                    write_de!(self.buffer, ").{UNION_TAG_NAME}");
                } else {
                    self.emit_expr(*inner, state);
                    write_de!(self.buffer, ")");
                }
            }
            ExprData::Is(mut inner, patt) => {
                inner.ty = inner
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let ty = inner.ty;
                let tmp = hoist!(self, self.emit_tmpvar(*inner, state));
                let (bindings, conditions) = self.emit_pattern(state, &patt.data, &tmp, ty);
                hoist!(self, self.buffer.emit(bindings.finish()));
                write_de!(self.buffer, "({})", conditions.finish());
            }
            ExprData::Lambda(_) => todo!(),
            ExprData::NeverCoerce(inner) => {
                if matches!(expr.ty, TypeId::VOID | TypeId::CVOID) {
                    self.emit_expr_inline(*inner, state);
                } else {
                    write_de!(self.buffer, "COERCE(");
                    self.emit_type(expr.ty);
                    write_de!(self.buffer, ", ");
                    self.emit_expr_inline(*inner, state);
                    write_de!(self.buffer, ")");
                }
            }
            ExprData::SpanMutCoerce(mut inner) => {
                inner.ty = inner
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let tmp = hoist!(self, self.emit_tmpvar(*inner, state));
                self.emit_cast(expr.ty);
                write_de!(self.buffer, "{{.$ptr={tmp}.$ptr,.$len={tmp}.$len}}");
            }
            ExprData::StringInterp {
                mut formatter,
                parts,
                scope,
            } => {
                formatter.ty = formatter
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let formatter_ty = formatter.ty;
                let formatter = hoist!(self, {
                    let formatter = self.emit_tmpvar(*formatter, state);
                    for (mfn, mut expr) in parts {
                        hoist_point!(self, {
                            expr.ty = expr
                                .ty
                                .with_templates(&mut self.proj.types, &state.func.ty_args);
                            self.emit_member_fn(state, mfn, scope);
                            write_de!(self.buffer, "(");
                            self.emit_tmpvar_ident(expr, state);
                            write_de!(self.buffer, ",&{formatter});");
                        });
                    }
                    formatter
                });
                let formatter_id = self
                    .proj
                    .scopes
                    .lang_traits
                    .get("formatter")
                    .copied()
                    .unwrap();
                let finish_state = State::with_inst(
                    self.find_implementation(
                        formatter_ty,
                        &GenericTrait::from_type_args(&self.proj.scopes, formatter_id, []),
                        "written",
                        scope,
                        |_, _| Default::default(),
                    ),
                    &self.proj.types,
                    formatter_ty,
                    scope,
                );
                self.buffer.emit_fn_name(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    &finish_state.func,
                    self.flags.minify,
                );
                write_de!(self.buffer, "(&{formatter})");
                self.funcs.insert(finish_state);
            }
            ExprData::AffixOperator {
                callee,
                mfn,
                param,
                scope,
                postfix,
            } => {
                let deref = "*".repeat(Self::indirection(&self.proj.types, callee.ty));
                if !postfix {
                    hoist!(self, {
                        let expr = Expr::new(
                            self.proj
                                .scopes
                                .get(mfn.func.id)
                                .ret
                                .with_templates(&mut self.proj.types, &state.func.ty_args),
                            ExprData::member_call(
                                &mut self.proj.types,
                                mfn,
                                [(param, (*callee).clone())].into(),
                                scope,
                            ),
                        );
                        self.emit_expr_stmt(expr, state);
                    });
                    write_de!(self.buffer, "({deref}");
                    self.emit_expr(*callee, state);
                    write_de!(self.buffer, ")");
                } else {
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(expr.ty);
                        write_de!(self.buffer, " {tmp}={deref}");
                        self.emit_expr_inline((*callee).clone(), state);
                        write_de!(self.buffer, ";");

                        let expr = Expr::new(
                            self.proj
                                .scopes
                                .get(mfn.func.id)
                                .ret
                                .with_templates(&mut self.proj.types, &state.func.ty_args),
                            ExprData::member_call(
                                &mut self.proj.types,
                                mfn,
                                [(param, *callee)].into(),
                                scope,
                            ),
                        );
                        self.emit_expr_stmt(expr, state);
                    });
                }
            }
            ExprData::Error => panic!("ICE: ExprData::Error in gen_expr"),
        }
    }

    #[inline(always)]
    fn emit_expr_inline(&mut self, mut expr: Expr, state: &mut State) {
        expr.ty = expr
            .ty
            .with_templates(&mut self.proj.types, &state.func.ty_args);
        self.emit_expr_inner(expr, state);
    }

    fn emit_member_fn(&mut self, state: &mut State, mut mfn: MemberFn, scope: ScopeId) {
        if let MemberFnType::Trait(mut tr) = mfn.typ {
            let inst = mfn
                .inst
                .with_templates(&mut self.proj.types, &state.func.ty_args);
            tr.fill_templates(&mut self.proj.types, &state.func.ty_args);
            mfn.func = self.find_implementation(
                inst,
                &tr,
                &self.proj.scopes.get(mfn.func.id).name.data.clone(),
                state.caller,
                |tc, id| {
                    TypeArgs::in_order(
                        tc.scopes(),
                        id,
                        mfn.func.ty_args.0.into_iter().map(|kv| kv.1),
                    )
                },
            );
        }

        mfn.func
            .fill_templates(&mut self.proj.types, &state.func.ty_args);
        let state = State::new(mfn.func, scope);
        self.buffer.emit_fn_name(
            &self.proj.scopes,
            &mut self.proj.types,
            &state.func,
            self.flags.minify,
        );
        self.funcs.insert(state);
    }

    fn emit_instance(&mut self, state: &mut State, ty: TypeId, members: IndexMap<String, Expr>) {
        let ut_id = self.proj.types[ty].as_user().unwrap().id;
        if self.proj.scopes.get(ut_id).kind.is_packed_struct() {
            return tmpbuf_emit!(self, state, |tmp| {
                self.emit_type(ty);
                write_de!(self.buffer, " {tmp} = {{}};");
                for (name, value) in members {
                    let ty = value.ty;
                    let expr = hoist!(self, self.emit_tmpvar(value, state));
                    self.emit_bitfield_write(&tmp, ut_id, &name, ty, &expr);
                }
            });
        }

        self.emit_cast(ty);
        write_de!(self.buffer, "{{");
        if members.is_empty() {
            write_de!(self.buffer, "CTL_DUMMY_INIT");
        }

        for (name, mut value) in members {
            value.ty = value
                .ty
                .with_templates(&mut self.proj.types, &state.func.ty_args);
            write_de!(
                self.buffer,
                ".{}=",
                member_name(&self.proj.scopes, Some(ut_id), &name)
            );
            self.emit_expr(value, state);
            write_de!(self.buffer, ",");
        }
        write_de!(self.buffer, "}}");
    }

    fn emit_variant_instance(
        &mut self,
        state: &mut State,
        ty: TypeId,
        variant: &str,
        mut members: IndexMap<String, Expr>,
    ) {
        if ty
            .can_omit_tag(&self.proj.scopes, &self.proj.types)
            .is_some()
        {
            if let Some(some) = members.remove("0") {
                self.emit_expr(some, state);
            } else {
                self.buffer.emit(NULLPTR);
            }
        } else {
            self.emit_cast(ty);
            let members: Vec<_> = members
                .into_iter()
                .map(|(name, mut expr)| {
                    expr.ty = expr
                        .ty
                        .with_templates(&mut self.proj.types, &state.func.ty_args);
                    // TODO: dont emit temporaries for expressions that cant have side effects
                    (name, hoist!(self, self.emit_tmpvar(expr, state)))
                })
                .collect();

            let ut_id = self.proj.types[ty].as_user().unwrap().id;
            write_de!(self.buffer, "{{");
            let ut = self.proj.scopes.get(ut_id);
            let union = ut.kind.as_union().unwrap();
            for (name, value) in members
                .iter()
                .filter(|(name, _)| ut.members.contains_key(name))
            {
                write_de!(
                    self.buffer,
                    ".{}={value},",
                    member_name(&self.proj.scopes, Some(ut_id), name)
                );
            }

            if union.variants.get(variant).is_some_and(|v| v.ty.is_some()) {
                write_de!(self.buffer, ".${variant}={{");
                for (name, value) in members
                    .iter()
                    .filter(|(name, _)| !ut.members.contains_key(name))
                {
                    write_de!(self.buffer, ".${name}={value},");
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
        match op {
            BinaryOp::NoneCoalesceAssign => {
                lhs.ty = lhs
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let opt_type = lhs.ty;
                let tag = if opt_type
                    .can_omit_tag(&self.proj.scopes, &self.proj.types)
                    .is_some()
                {
                    ""
                } else {
                    ".$Some.$0"
                };
                let mut left = Buffer::default();
                usebuf!(self, &mut left, self.emit_expr_inner(lhs, state));
                let left = left.finish();

                hoist!(self, {
                    self.emit_pattern_if_stmt(
                        state,
                        &PatternData::Variant {
                            pattern: None,
                            variant: "None".into(),
                            inner: ret,
                            borrows: false,
                        },
                        &left,
                        opt_type,
                    );

                    hoist_point!(self, {
                        write_de!(self.buffer, "{left}{tag}=");
                        self.emit_expr_inline(rhs, state);
                        if opt_type
                            .can_omit_tag(&self.proj.scopes, &self.proj.types)
                            .is_none()
                        {
                            write_de!(self.buffer, ";");
                            let tag = self
                                .proj
                                .scopes
                                .get(self.proj.types[opt_type].as_user().unwrap().id)
                                .kind
                                .as_union()
                                .unwrap()
                                .discriminant("Some")
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

                    lhs.ty = lhs
                        .ty
                        .with_templates(&mut self.proj.types, &state.func.ty_args);
                    let opt_type = lhs.ty;
                    let name = hoist!(self, self.emit_tmpvar(lhs, state));
                    self.emit_pattern_if_stmt(
                        state,
                        &PatternData::Variant {
                            pattern: None,
                            variant: "None".into(),
                            inner: ret,
                            borrows: false,
                        },
                        &name,
                        opt_type,
                    );
                    hoist_point!(self, {
                        write_de!(self.buffer, "{tmp}=");
                        self.emit_expr_inline(rhs, state);
                    });
                    let tag = if opt_type
                        .can_omit_tag(&self.proj.scopes, &self.proj.types)
                        .is_some()
                    {
                        ""
                    } else {
                        ".$Some.$0"
                    };
                    write_de!(self.buffer, ";}}else{{{tmp}={name}{tag};}}");
                });
            }
            BinaryOp::Cmp => {
                let tmp = tmpbuf!(self, state, |tmp| {
                    let ty = &self.proj.types[lhs.ty];
                    if matches!(ty, Type::RawPtr(_)) {
                        self.emit_type(TypeId::ISIZE);
                    } else if let Some(int) = ty.as_integral(true) {
                        let ty = self
                            .proj
                            .types
                            .insert(Type::Int(int.bits + int.signed as u32));
                        self.emit_type(ty);
                    } else {
                        self.emit_type(lhs.ty);
                    }
                    write_de!(self.buffer, " {tmp}=");
                    self.emit_expr(lhs, state);
                    write_de!(self.buffer, "-");
                    self.emit_expr(rhs, state);
                    write_de!(self.buffer, ";");

                    tmp
                });

                let union = self
                    .proj
                    .scopes
                    .get(self.proj.types[ret].as_user().unwrap().id)
                    .kind
                    .as_union()
                    .unwrap();
                let tag = union.tag;
                let less = union.discriminant("Less").unwrap().clone();
                let greater = union.discriminant("Greater").unwrap().clone();
                let equal = union.discriminant("Equal").unwrap().clone();

                write_de!(self.buffer, "({tmp}<0?");
                self.emit_cast(ret);
                write_de!(self.buffer, "{{.{UNION_TAG_NAME}=");
                self.emit_literal(less, tag);
                write_de!(self.buffer, "}}:({tmp}>0?");
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
                lhs.ty = lhs
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                if ret == TypeId::BOOL && lhs.ty != TypeId::BOOL {
                    self.emit_cast(ret);
                }
                if op.is_assignment() {
                    if matches!(&lhs.data, ExprData::Member { source, .. }
                        if source.ty.is_packed_struct(&self.proj))
                    {
                        let ExprData::Member { source, member } = lhs.data else {
                            unreachable!()
                        };
                        return self.emit_bitfield_assign(*source, state, &member, lhs.ty, rhs, op);
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
        match op {
            UnaryOp::Plus => self.emit_expr(lhs, state),
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
            UnaryOp::Deref => {
                if let &Type::Array(inner, len) = &self.proj.types[ret] {
                    let (sz, _) = inner.size_and_align(&self.proj.scopes, &mut self.proj.types);
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(ret);
                        write_de!(self.buffer, " {tmp};CTL_MEMCPY(&{tmp},");
                        self.emit_expr(lhs, state);
                        write_de!(self.buffer, ",{len}*{sz});");
                    });
                } else {
                    write_de!(self.buffer, "(*");
                    self.emit_expr(lhs, state);
                    write_de!(self.buffer, ")");
                }
            }
            UnaryOp::Addr | UnaryOp::AddrMut | UnaryOp::AddrRaw => {
                lhs.ty = lhs
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);

                let array = self.proj.types[lhs.ty].is_array();
                if !array {
                    write_de!(self.buffer, "&");
                }
                let is_lvalue = match &lhs.data {
                    ExprData::Unary(UnaryOp::Deref, _)
                    | ExprData::AutoDeref { .. }
                    | ExprData::Var(_)
                    | ExprData::Subscript { .. } => true,
                    ExprData::Member { source, .. } => !source.ty.is_packed_struct(&self.proj),
                    _ => false,
                };

                if is_lvalue {
                    self.emit_expr_inner(lhs, state);
                } else {
                    self.emit_tmpvar_ident(lhs, state);
                }

                if array {
                    write_de!(self.buffer, ".{ARRAY_DATA_NAME}");
                }
            }
            UnaryOp::Try => {
                tmpbuf_emit!(self, state, |tmp| {
                    lhs.ty = lhs
                        .ty
                        .with_templates(&mut self.proj.types, &state.func.ty_args);

                    self.emit_type(ret);
                    write_de!(self.buffer, " {tmp};");

                    let inner_ty = lhs.ty;
                    let inner_tmp = self.emit_tmpvar(lhs, state);
                    self.emit_pattern_if_stmt(
                        state,
                        &PatternData::Variant {
                            pattern: None,
                            variant: "None".into(),
                            inner: ret,
                            borrows: false,
                        },
                        &inner_tmp,
                        inner_ty,
                    );
                    hoist_point!(self, {
                        let mut buffer = Buffer::default();
                        usebuf!(self, &mut buffer, {
                            write_de!(self.buffer, "return ");
                            let mut ret_type = self.proj.scopes.get(state.func.id).ret;
                            ret_type =
                                ret_type.with_templates(&mut self.proj.types, &state.func.ty_args);
                            self.emit_expr_inner(Expr::option_null(ret_type), state);
                        });
                        self.leave_scope(
                            state,
                            &buffer.finish(),
                            self.proj.scopes.get(state.func.id).body_scope,
                        );
                    });
                    write_de!(self.buffer, "}}{tmp}=");
                    if inner_ty
                        .can_omit_tag(&self.proj.scopes, &self.proj.types)
                        .is_some()
                    {
                        write_de!(self.buffer, "{inner_tmp};");
                    } else {
                        write_de!(self.buffer, "{inner_tmp}.$Some.$0;");
                    }
                });
            }
            UnaryOp::Unwrap => panic!("ICE: UnaryOp::Unwrap in gen_expr"),
        }
    }

    fn finish_emit_fn_args(
        &mut self,
        state: &mut State,
        original_id: FunctionId,
        args: IndexMap<String, Expr>,
    ) {
        let mut args: IndexMap<_, _> = args
            .into_iter()
            .map(|(name, mut expr)| {
                expr.ty = expr
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
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
            write_de!(self.buffer, "{}=", scope_var_or_label(self.cur_loop));
            self.emit_expr_inline(Expr::option_null(ty), state);
        } else {
            write_de!(
                self.buffer,
                "{}={VOID_INSTANCE}",
                scope_var_or_label(self.cur_loop)
            );
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

                hoist_point!(
                    self,
                    self.emit_expr_stmt(self.defers[i].1[j].clone(), state)
                );
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
                    .find_associated_fn(&self.proj.scopes, "new")
                    .unwrap(),
                ut.ty_args.clone(),
            ),
            &self.proj.scopes,
        );

        self.buffer.emit_fn_name(
            &self.proj.scopes,
            &mut self.proj.types,
            &new_state.func,
            self.flags.minify,
        );
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
                    .find_associated_fn(&self.proj.scopes, "with_capacity")
                    .unwrap(),
                ut.ty_args.clone(),
            ),
            &self.proj.scopes,
        );

        self.buffer.emit_fn_name(
            &self.proj.scopes,
            &mut self.proj.types,
            &state.func,
            self.flags.minify,
        );
        write_de!(self.buffer, "({len});");
        self.funcs.insert(state);
    }

    fn emit_intrinsic(
        &mut self,
        name: &str,
        ret: TypeId,
        func: &GenericFn,
        mut args: IndexMap<String, Expr>,
        state: &mut State,
    ) {
        match name {
            "numeric_abs" => {
                let (_, mut expr) = args.shift_remove_index(0).unwrap();
                expr.ty = expr
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let tmp = hoist!(self, self.emit_tmpvar(expr, state));
                write_de!(self.buffer, "({tmp}<0?-{tmp}:{tmp})");
            }
            "numeric_cast" => {
                let (_, expr) = args.shift_remove_index(0).unwrap();
                self.emit_cast(ret);
                self.emit_expr(expr, state);
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
                        .size_and_align(&self.proj.scopes, &mut self.proj.types)
                        .0
                );
            }
            "align_of" => {
                write_de!(
                    self.buffer,
                    "(usize){}",
                    func.first_type_arg()
                        .unwrap()
                        .size_and_align(&self.proj.scopes, &mut self.proj.types)
                        .1
                );
            }
            "panic" => {
                let panic = State::in_body_scope(
                    GenericFn::from_id(
                        &self.proj.scopes,
                        self.proj
                            .scopes
                            .lang_fns
                            .get("panic_handler")
                            .cloned()
                            .expect("a panic handler should exist"),
                    ),
                    &self.proj.scopes,
                );

                write_de!(self.buffer, "VOID(");
                self.buffer.emit_fn_name(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    &panic.func,
                    self.flags.minify,
                );
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
                    .auto_deref(&mut self.proj.types, TypeId::UNKNOWN);
                let arg1 = args
                    .next()
                    .expect("ICE: binary operator should receive two arguments")
                    .1
                    .auto_deref(&mut self.proj.types, TypeId::UNKNOWN);
                let op = &self.proj.scopes.get(func.id).name.data[..];
                self.emit_binary(
                    state,
                    match op {
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
                        _ => panic!("ICE: call to unsupported binary operator '{op}'"),
                    },
                    ret,
                    arg0,
                    arg1,
                );
            }
            "unary_op" => {
                let op = &self.proj.scopes.get(func.id).name.data[..];
                let arg0 = args
                    .into_iter()
                    .next()
                    .expect("ICE: unary operator should receive one argument")
                    .1
                    .auto_deref(&mut self.proj.types, TypeId::UNKNOWN);
                self.emit_unary(
                    state,
                    match op {
                        "neg" => UnaryOp::Neg,
                        "not" => UnaryOp::Not,
                        "inc" => UnaryOp::PostIncrement,
                        "dec" => UnaryOp::PostDecrement,
                        _ => panic!("ICE: call to unsupported unary operator '{op}'"),
                    },
                    ret,
                    arg0,
                );
            }
            "type_id" => {
                self.emit_cast(ret);
                write_de!(
                    self.buffer,
                    "{{ .$tag = {} }}",
                    func.first_type_arg().unwrap().as_raw()
                );
            }
            "type_name" => {
                let name = func
                    .first_type_arg()
                    .unwrap()
                    .name(&self.proj.scopes, &mut self.proj.types);
                self.emit_string_literal(ret, &name);
            }
            _ => unreachable!(),
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
        self.buffer.emit_mangled_name(
            &self.proj.scopes,
            &mut self.proj.types,
            vtable.ty,
            self.flags.minify,
        );
        if !self.flags.minify {
            write_de!(self.buffer, "_");
        }
        self.buffer.emit_type_name(
            &self.proj.scopes,
            &mut self.proj.types,
            &vtable.tr,
            self.flags.minify,
        );
        self.buffer
            .emit(if self.flags.minify { "v" } else { "_$vtable" });
        write_de!(self.buffer, "{}", vtable.scope.0);
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_pattern_inner(
        &mut self,
        state: &mut State,
        pattern: &PatternData,
        src: &str,
        ty: TypeId,
        borrow: bool,
        bindings: &mut Buffer,
        conditions: &mut JoiningBuilder,
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
            PatternData::IntRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                let src = Self::deref(&self.proj.types, src, ty);
                let base = ty.strip_references(&self.proj.types);
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
                        write_de!(
                            self.buffer,
                            "{0}.$span.$len=={1}&&CTL_MEMCMP({0}.$span.$ptr,\"",
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
            PatternData::Variant {
                pattern,
                variant,
                inner,
                borrows,
            } => {
                let src = Self::deref(&self.proj.types, src, ty);
                let base = ty.strip_references(&self.proj.types);
                if base
                    .can_omit_tag(&self.proj.scopes, &self.proj.types)
                    .is_some()
                {
                    if variant == "Some" {
                        conditions.next_str(format!("{src}!={NULLPTR}"));
                        if let Some((patt, borrows)) =
                            pattern.as_ref().and_then(|patt| patt.data.as_destrucure())
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
                    let (tag, ty) = self
                        .proj
                        .types
                        .get(base)
                        .as_user()
                        .and_then(|ut| self.proj.scopes.get(ut.id).kind.as_union())
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
                            &format!("{src}.${variant}"),
                            *inner,
                            borrow || *borrows,
                            bindings,
                            conditions,
                        );
                    }
                }
            }
            PatternData::Span {
                patterns,
                rest,
                inner,
            } => {
                let src = Self::deref(&self.proj.types, src, ty);
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        write_de!(
                            self.buffer,
                            "{src}.$len{}{}",
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
                                "={{.$ptr={src}.$ptr+{pos},.$len={src}.$len-{}}};",
                                patterns.len()
                            );
                        });
                    }
                    pos
                });
                let inner = inner.with_templates(&mut self.proj.types, &state.func.ty_args);
                for (i, patt) in patterns.iter().enumerate() {
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &if pos.is_some_and(|pos| i >= pos) {
                            format!("{src}.$ptr[{src}.$len-{}+{i}]", patterns.len())
                        } else {
                            format!("{src}.$ptr[{i}]")
                        },
                        inner,
                        true,
                        bindings,
                        conditions,
                    );
                }
            }
            PatternData::Destrucure { patterns, borrows } => {
                let src = Self::deref(&self.proj.types, src, ty);
                let ty = ty.strip_references(&self.proj.types);
                let ut_id = self.proj.types[ty].as_user().map(|ut| ut.id);
                for (member, inner, patt) in patterns {
                    let inner = inner.with_templates(&mut self.proj.types, &state.func.ty_args);
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &format!("{src}.{}", member_name(&self.proj.scopes, ut_id, member)),
                        inner,
                        borrow || *borrows,
                        bindings,
                        conditions,
                    );
                }
            }
            PatternData::Array {
                patterns:
                    ArrayPattern {
                        patterns,
                        rest,
                        arr_len,
                        inner,
                    },
                borrows,
            } => {
                let is_any_ptr = self.proj.types[ty].is_any_ptr();
                let src = Self::deref(&self.proj.types, src, ty);
                let src = if is_any_ptr {
                    src
                } else {
                    format!("{src}.{ARRAY_DATA_NAME}")
                };
                let rest = rest.map(|RestPattern { id, pos }| {
                    let rest_len = arr_len - patterns.len();
                    if let Some(id) = id.filter(|&id| !self.proj.scopes.get(id).unused) {
                        usebuf!(self, bindings, {
                            self.emit_var_decl(id, state);
                            if is_any_ptr {
                                write_de!(self.buffer, "={src}+{pos};");
                            } else {
                                write_de!(self.buffer, "={{.{ARRAY_DATA_NAME}={{");
                                for i in 0..rest_len {
                                    write_de!(self.buffer, "{src}[{}],", pos + i);
                                }
                                write_de!(self.buffer, "}}}};");
                            }
                        });
                    }

                    (pos, rest_len)
                });

                let inner = inner.with_templates(&mut self.proj.types, &state.func.ty_args);
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
                    let id = self.emit_var_decl(id, state);
                    let ty = &self.proj.types[id];
                    if borrow
                        && matches!(ty, Type::Ptr(i) | Type::MutPtr(i) | Type::RawPtr(i)
                        if self.proj.types[*i].is_array())
                    {
                        write_de!(self.buffer, "={src}.{ARRAY_DATA_NAME};");
                    } else {
                        write_de!(self.buffer, "={}{src};", if borrow { "&" } else { "" });
                    }
                });
            }
            PatternData::Void => {}
            PatternData::Or(patterns) => {
                let mut conds = JoiningBuilder::new("||", "1");
                let mut binds = Buffer::default();
                for pattern in patterns {
                    let mut tmp = JoiningBuilder::new("&&", "1");
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
    ) -> (Buffer, JoiningBuilder) {
        let mut bindings = Buffer::default();
        let mut conditions = JoiningBuilder::new("&&", "1");
        self.emit_pattern_inner(
            state,
            pattern,
            src,
            ty,
            false,
            &mut bindings,
            &mut conditions,
        );
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
        self.tg
            .add_type(&self.proj.scopes, &mut self.proj.types, id);
        self.buffer.emit_type(
            &self.proj.scopes,
            &mut self.proj.types,
            id,
            self.flags.minify,
        );
    }

    fn emit_cast(&mut self, id: TypeId) {
        write_de!(self.buffer, "(");
        self.emit_type(id);
        write_de!(self.buffer, ")");
    }

    fn emit_prototype(&mut self, state: &mut State, is_prototype: bool) -> Vec<VariableId> {
        let f = self.proj.scopes.get(state.func.id);
        let ret = f
            .ret
            .with_templates(&mut self.proj.types, &state.func.ty_args);

        if f.is_extern {
            write_de!(self.buffer, "extern ");
        } else {
            write_de!(self.buffer, "static ");
            // TODO: inline manually
            match f.attrs.val("inline") {
                Some("always") => write_de!(self.buffer, "CTL_FORCEINLINE "),
                Some("never") => write_de!(self.buffer, "CTL_NEVERINLINE "),
                _ if f.attrs.has("inline") => write_de!(self.buffer, "CTL_INLINE "),
                _ => {}
            }
        }

        if ret == TypeId::NEVER {
            // && real
            write_de!(self.buffer, "CTL_NORETURN ");
        }

        let variadic = f.variadic;
        let params = f.params.clone();
        let is_import = f.is_extern && f.body.is_none();
        if ret.is_void() {
            write_de!(self.buffer, "void ");
        } else {
            self.emit_type(ret);
            write_de!(self.buffer, " ");
        }
        self.buffer.emit_fn_name(
            &self.proj.scopes,
            &mut self.proj.types,
            &state.func,
            self.flags.minify,
        );
        write_de!(self.buffer, "(");

        let mut unused = vec![];
        let mut nonnull = vec![];
        for (i, param) in params.iter().enumerate() {
            let mut ty = param.ty;
            ty = ty.with_templates(&mut self.proj.types, &state.func.ty_args);
            if i > 0 {
                write_de!(self.buffer, ",");
            }

            if self.proj.types[ty].is_any_ptr() && is_prototype {
                nonnull.push(format!("{}", i + 1));
            }

            if is_import || is_prototype {
                self.emit_type(ty);
            } else if let ParamPattern::Checked(Pattern {
                data: PatternData::Variable(id),
                ..
            }) = &param.patt
            {
                self.emit_var_decl(*id, state);
                if self.proj.scopes.get(*id).unused {
                    unused.push(*id);
                }
                continue;
            } else {
                self.emit_type(ty);
                write_de!(self.buffer, " {}", param.label);
            }
        }

        if variadic {
            write_de!(self.buffer, "{}...)", [",", ""][params.is_empty() as usize]);
        } else if params.is_empty() {
            write_de!(self.buffer, "void)");
        } else {
            write_de!(self.buffer, ")");
        }

        if !nonnull.is_empty() {
            write_de!(self.buffer, "CTL_NONNULL({})", nonnull.join(","));
        }

        unused
    }

    fn emit_var_name(&mut self, id: VariableId, state: &mut State) {
        use std::collections::hash_map::*;

        if self.flags.minify {
            return write_de!(self.buffer, "v{id}");
        }

        let var = self.proj.scopes.get(id);
        if var.is_static {
            self.buffer
                .emit(self.proj.scopes.full_name(var.scope, &var.name.data));
        } else {
            let mut emit = || {
                write_de!(self.buffer, "$");
                if var
                    .name
                    .data
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_digit())
                {
                    write_de!(self.buffer, "p");
                }
                self.buffer.emit(&var.name.data);
            };
            match state.emitted_names.entry(var.name.data.clone()) {
                Entry::Occupied(entry) if *entry.get() == id => {
                    emit();
                }
                Entry::Occupied(_) => {
                    let len = state.renames.len();
                    self.buffer.emit(
                        state
                            .renames
                            .entry(id)
                            .or_insert_with(|| format!("$r{len}")),
                    );
                }
                Entry::Vacant(entry) => {
                    entry.insert(id);
                    emit();
                }
            }
        }
    }

    fn emit_var_decl(&mut self, id: VariableId, state: &mut State) -> TypeId {
        let var = self.proj.scopes.get(id);
        let ty = var
            .ty
            .with_templates(&mut self.proj.types, &state.func.ty_args);
        if var.is_static {
            write_de!(self.buffer, "static ");
        }

        let emit_const = !var.mutable && !var.is_static;
        self.emit_type(ty);
        if emit_const {
            write_de!(self.buffer, " const");
        }
        write_de!(self.buffer, " ");
        self.emit_var_name(id, state);
        ty
    }

    fn emit_bitfield_assign(
        &mut self,
        source: Expr,
        state: &mut State,
        member: &str,
        ty: TypeId,
        rhs: Expr,
        op: BinaryOp,
    ) {
        let id = self
            .proj
            .types
            .get(source.ty)
            .as_user()
            .map(|ut| ut.id)
            .unwrap();
        let src = tmpbuf!(self, state, |tmp| {
            let ptr = self.proj.types.insert(Type::MutPtr(source.ty));
            self.emit_type(ptr);
            writeln_de!(self.buffer, " {tmp}=&");
            self.emit_expr_inline(source, state);
            writeln_de!(self.buffer, ";");
            format!("(*{tmp})")
        });

        let expr = tmpbuf!(self, state, |tmp| {
            self.emit_type(ty);
            writeln_de!(self.buffer, " {tmp}=");
            if op != BinaryOp::Assign {
                writeln_de!(self.buffer, "(");
                self.emit_bitfield_read(&src, id, member, ty);
                writeln_de!(
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

            writeln_de!(self.buffer, ";");
            tmp
        });
        hoist!(self, self.emit_bitfield_write(&src, id, member, ty, &expr));
        self.buffer.emit(VOID_INSTANCE);
    }

    fn emit_bitfield_read(&mut self, tmp: &str, id: UserTypeId, member: &str, ty: TypeId) {
        let mut result = JoiningBuilder::new("|", "0");
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
        member: &str,
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
            write_de!(
                this.buffer,
                "{:#x};",
                !(mask << word_offset) & bit_mask(word_size_bits)
            );

            // negative signed bitints contain 1s in the inaccessible bits (ie -1i2 == 0b1111_1111,
            // not 0b0000_0011) so we need the mask even for a non-partial write
            let needs_mask = reading != bits
                || enum_tag
                    .unwrap_or(ty)
                    .as_integral(&this.proj.types, true)
                    .unwrap()
                    .signed;

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
        member: &str,
        ty: TypeId,
        mut f: impl FnMut(&mut Self, BitfieldAccess),
    ) {
        let bf = self.proj.scopes.get(id).kind.as_packed_struct().unwrap();
        let word_size_bits = (bf.align * 8) as u32;
        let (bits, enum_tag) = match ty.bit_size(&self.proj) {
            BitSizeResult::Size(n) => (n, None),
            BitSizeResult::Tag(tag, n) => (n, Some(tag)),
            _ => unreachable!(),
        };

        let mut word = bf.bit_offsets[member] / word_size_bits;
        let mut word_offset = bf.bit_offsets[member] % word_size_bits;
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

        let mut result = JoiningBuilder::new("|", "0");
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
        // let max_literal = largest_type.max().magnitude();
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

    fn emit_string_literal(&mut self, str: TypeId, value: &str) {
        write_de!(self.buffer, "STRLIT(");
        self.emit_type(str);
        write_de!(self.buffer, ",\"");
        for byte in value.as_bytes() {
            write_de!(self.buffer, "\\x{byte:x}");
        }
        write_de!(self.buffer, "\",{})", value.len());
    }

    fn has_side_effects(expr: &Expr) -> bool {
        match &expr.data {
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
        method: &str,
        scope: ScopeId,
        finish: impl FnOnce(&mut TypeChecker, FunctionId) -> TypeArgs + Clone,
    ) -> GenericFn {
        // TODO: fix this disgusting hack
        let Some(mfn) = TypeChecker::with_project(&mut self.proj, |tc| {
            tc.get_member_fn_ex(inst, Some(tr), method, scope, finish)
        }) else {
            panic!(
                "searching from scope: '{}', cannot find implementation for method '{}::{method}' for type '{}'",
                self.proj.scopes.full_name(scope, ""),
                tr.name(&self.proj.scopes, &mut self.proj.types),
                inst.name(&self.proj.scopes, &mut self.proj.types)
            )
        };

        if !self.proj.scopes.get(mfn.func.id).has_body {
            panic!(
                "searching from scope: '{}', get_member_fn_ex picked invalid function for implementation for method '{}::{method}' for type '{}'",
                self.proj.scopes.full_name(scope, ""),
                tr.name(&self.proj.scopes, &mut self.proj.types),
                inst.name(&self.proj.scopes, &mut self.proj.types)
            )
        }

        mfn.func
    }

    fn deref(types: &Types, src: &str, ty: TypeId) -> String {
        if matches!(types[ty], Type::Ptr(_) | Type::MutPtr(_)) {
            format!(
                "({:*<1$}{src})",
                "",
                Self::indirection(types, ty) - usize::from(ty.strip_references_r(types).is_array())
            )
        } else {
            src.into()
        }
    }

    fn indirection(types: &Types, mut id: TypeId) -> usize {
        let mut count = 0;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = &types[id] {
            id = *inner;
            count += 1;
        }
        count
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

fn vtable_methods(scopes: &Scopes, types: &Types, tr: &UserType) -> Vec<Vis<FunctionId>> {
    let (&this, _) = tr
        .kind
        .as_trait()
        .expect("UserType passed to vtable_methods was not a trait");
    tr.fns
        .iter()
        .filter(move |f| {
            let f = scopes.get(f.id);
            f.type_params.is_empty()
                && f.params.first().is_some_and(|p| p.label == THIS_PARAM)
                && f.params
                    .iter()
                    .all(|p| !types[p.ty].as_user().is_some_and(|ty| ty.id == this))
        })
        .copied()
        .collect()
}

fn member_name(scopes: &Scopes, id: Option<UserTypeId>, name: &str) -> String {
    if id.is_some_and(|id| scopes.get(id).attrs.has(ATTR_NOGEN)) {
        name.into()
    } else {
        format!("${name}")
    }
}

fn loop_cont_label(scope: ScopeId) -> String {
    format!("$c{}", scope.0)
}

fn scope_var_or_label(scope: ScopeId) -> String {
    format!("$sv{}", scope.0)
}

fn bit_mask(bits: u32) -> u64 {
    if bits < 64 {
        (1 << bits) - 1
    } else {
        u64::MAX
    }
}
