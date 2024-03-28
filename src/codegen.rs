use std::collections::{hash_map::Entry, HashMap, HashSet};

use indexmap::IndexMap;

use crate::{
    ast::{
        checked::*,
        parsed::{Linkage, RangePattern},
        BinaryOp, UnaryOp,
    },
    error::{Diagnostics, Error},
    lexer::Span,
    nearest_pow_of_two,
    project::Project,
    sym::*,
    typecheck::TypeChecker,
    typeid::{
        CInt, FnPtr, GenericFunc, GenericTrait, GenericUserType, Type, TypeArgs, TypeId, Types,
    },
    CodegenFlags, THIS_PARAM,
};

const UNION_TAG_NAME: &str = "tag";
const ARRAY_DATA_NAME: &str = "data";
const VOID_INSTANCE: &str = "CTL_VOID";
const ATTR_NOGEN: &str = "c_opaque";
const ATTR_LINKNAME: &str = "c_name";
const NULLPTR: &str = "((void*)0)";

struct TypeGen {
    structs: HashMap<GenericUserType, Vec<GenericUserType>>,
    fnptrs: HashSet<FnPtr>,
    arrays: HashMap<TypeId, HashSet<usize>>,
    integers: HashSet<(u32, bool)>,
    dynptrs: HashSet<GenericTrait>,
}

impl TypeGen {
    fn new() -> Self {
        Self {
            structs: Default::default(),
            fnptrs: Default::default(),
            arrays: Default::default(),
            integers: Default::default(),
            dynptrs: Default::default(),
        }
    }

    fn finish(
        mut self,
        scopes: &Scopes,
        types: &mut Types,
        buffer: &mut Buffer,
        flags: &CodegenFlags,
    ) {
        let mut defs = Buffer::default();
        for (bits, signed) in self.integers {
            if flags.no_bit_int {
                buffer.emit(format!(
                    "typedef {}int{}_t {}{bits};",
                    if signed { "" } else { "u" },
                    nearest_pow_of_two(bits),
                    if signed { 's' } else { 'u' },
                ));
            } else {
                let ch = if signed { 's' } else { 'u' };
                buffer.emit(format!(
                    "typedef {}INT({bits}){ch}{bits};",
                    ch.to_uppercase(),
                ));
            }
        }

        for f in self.fnptrs {
            defs.emit("typedef ");
            defs.emit_type(scopes, types, f.ret, None, flags.minify);
            defs.emit("(*");
            defs.emit_fnptr_name(scopes, types, &f, flags.minify);
            defs.emit(")(");
            for (i, &param) in f.params.iter().enumerate() {
                if i > 0 {
                    defs.emit(",");
                }

                defs.emit_type(scopes, types, param, None, flags.minify);
            }
            defs.emit(");");
        }

        for tr in self.dynptrs {
            buffer.emit("typedef struct ");
            buffer.emit_vtable_struct_name(scopes, types, &tr, flags.minify);
            buffer.emit(" ");
            buffer.emit_vtable_struct_name(scopes, types, &tr, flags.minify);

            buffer.emit(";typedef struct ");
            buffer.emit_type_name(scopes, types, &tr, flags.minify);
            buffer.emit("{void*self;");
            buffer.emit_vtable_struct_name(scopes, types, &tr, flags.minify);
            buffer.emit(" const*vtable;}");
            buffer.emit_type_name(scopes, types, &tr, flags.minify);
            buffer.emit(";");

            defs.emit("struct ");
            defs.emit_vtable_struct_name(scopes, types, &tr, flags.minify);
            defs.emit("{");
            for id in scopes.get_trait_impls(tr.id) {
                for f in vtable_methods(scopes, types, scopes.get(id)) {
                    let ret = scopes.get(f.id).ret.with_templates(types, &tr.ty_args);
                    defs.emit_type(scopes, types, ret, None, flags.minify);
                    defs.emit("(*const ");
                    defs.emit_fn_name(
                        scopes,
                        types,
                        &GenericFunc::new(f.id, tr.ty_args.clone()),
                        flags.minify,
                    );
                    defs.emit(")(");
                    for (i, param) in scopes.get(f.id).params.iter().enumerate() {
                        let ty = param.ty.with_templates(types, &tr.ty_args);
                        if i > 0 {
                            defs.emit(",");
                            defs.emit_type(scopes, types, ty, None, flags.minify);
                        } else if types.get(ty).is_ptr() {
                            defs.emit("const void*");
                        } else {
                            defs.emit("void*");
                        }
                    }
                    defs.emit(");");
                }
            }
            defs.emit("};");
        }

        let mut emitted_arrays = HashSet::new();
        for ut in Self::get_struct_order(&self.structs) {
            let ut_data = scopes.get(ut.id);
            buffer.emit(if ut_data.kind.is_unsafe_union() {
                "typedef union "
            } else {
                "typedef struct "
            });
            buffer.emit_type_name(scopes, types, ut, flags.minify);
            buffer.emit(" ");
            buffer.emit_type_name(scopes, types, ut, flags.minify);
            buffer.emit(";");
            if scopes.get(ut.id).attrs.has(ATTR_NOGEN) {
                continue;
            }

            defs.emit(if ut_data.kind.is_unsafe_union() {
                "union "
            } else {
                "struct "
            });
            defs.emit_type_name(scopes, types, ut, flags.minify);
            defs.emit("{");

            let members = &scopes.get(ut.id).members;
            if let UserTypeKind::Union(union) = &ut_data.kind {
                defs.emit_type(scopes, types, union.tag, None, flags.minify);
                defs.emit(format!(" {UNION_TAG_NAME};"));

                for (name, member) in members {
                    Self::emit_member(scopes, types, ut, name, member.ty, &mut defs, flags.minify);
                }

                defs.emit("union{");
                for (name, variant) in union.variants.iter() {
                    if let Some(ty) = variant.ty {
                        Self::emit_member(scopes, types, ut, name, ty, &mut defs, flags.minify);
                    }
                }
                defs.emit("};");
            } else {
                if members.is_empty() {
                    defs.emit("CTL_DUMMY_MEMBER;");
                }

                for (name, member) in members {
                    Self::emit_member(scopes, types, ut, name, member.ty, &mut defs, flags.minify);
                }
            }

            defs.emit("};");
            let ty = types.insert(Type::User(ut.clone()));
            if let Some(sizes) = self.arrays.remove(&ty) {
                for size in sizes {
                    Self::emit_array(
                        scopes,
                        types,
                        buffer,
                        Some(&mut defs),
                        ty,
                        size,
                        flags.minify,
                    );
                }
                emitted_arrays.insert(ty);
            }
        }

        for (ty, sizes) in self
            .arrays
            .into_iter()
            .filter(|(ty, _)| !emitted_arrays.contains(ty))
        {
            for size in sizes {
                Self::emit_array(scopes, types, buffer, None, ty, size, flags.minify);
            }
        }

        buffer.emit(defs.finish());
    }

    fn add_type(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        diag: &mut Diagnostics,
        ty: TypeId,
        adding: Option<&GenericUserType>,
    ) {
        match types.get(ty) {
            Type::Ptr(inner) | Type::MutPtr(inner) => {
                self.add_type(scopes, types, diag, *inner, adding)
            }
            Type::FnPtr(ptr) => self.add_fnptr(scopes, types, diag, ptr.clone()),
            Type::User(ut) => self.add_user_type(scopes, types, diag, ut.clone(), adding),
            &Type::Array(ty, len) => self.add_array(scopes, types, diag, ty, len, adding),
            Type::Int(bits) => {
                self.integers.insert((*bits, true));
            }
            Type::Uint(bits) => {
                self.integers.insert((*bits, false));
            }
            Type::DynPtr(ut) | Type::DynMutPtr(ut) => {
                self.add_dynptr(ut.clone());
            }
            _ => {}
        }
    }

    fn add_fnptr(&mut self, scopes: &Scopes, types: &mut Types, diag: &mut Diagnostics, f: FnPtr) {
        for &param in f.params.iter() {
            self.add_type(scopes, types, diag, param, None);
        }

        self.add_type(scopes, types, diag, f.ret, None);
        self.fnptrs.insert(f);
    }

    fn add_dynptr(&mut self, tr: GenericTrait) {
        self.dynptrs.insert(tr);
    }

    fn add_array(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        diag: &mut Diagnostics,
        ty: TypeId,
        len: usize,
        adding: Option<&GenericUserType>,
    ) {
        if let Entry::Occupied(mut entry) = self.arrays.entry(ty) {
            entry.get_mut().insert(len);
            return;
        }

        self.add_type(scopes, types, diag, ty, adding);
        self.arrays.insert(ty, [len].into());
    }

    fn add_user_type(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        diag: &mut Diagnostics,
        ut: GenericUserType,
        adding: Option<&GenericUserType>,
    ) {
        if self.structs.contains_key(&ut) {
            return;
        }

        self.structs.insert(ut.clone(), Vec::new());
        let sty = scopes.get(ut.id);

        let mut deps = Vec::new();
        for m in sty.members.values() {
            self.check_member_dep(
                scopes,
                types,
                diag,
                &ut,
                m.ty,
                adding,
                &mut deps,
                sty.name.span,
            );
        }

        if let Some(union) = sty.kind.as_union() {
            self.add_type(scopes, types, diag, union.tag, None);
            for ty in union.variants.values().flat_map(|v| v.ty) {
                self.check_member_dep(
                    scopes,
                    types,
                    diag,
                    &ut,
                    ty,
                    adding,
                    &mut deps,
                    sty.name.span,
                );
            }
        }

        self.structs.insert(ut, deps);
    }

    #[allow(clippy::too_many_arguments)]
    fn check_member_dep(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        diag: &mut Diagnostics,
        ut: &GenericUserType,
        ty: TypeId,
        adding: Option<&GenericUserType>,
        deps: &mut Vec<GenericUserType>,
        span: Span,
    ) {
        let ty = ty.with_templates(types, &ut.ty_args);
        match types.get(ty) {
            Type::User(dep) => {
                let dep = dep.clone();
                if matches!(adding, Some(adding) if adding == &dep) {
                    // ideally get the span of the instantiation that caused this
                    diag.error(Error::cyclic(
                        &dep.name(scopes, types),
                        &ut.name(scopes, types),
                        scopes.get(dep.id).name.span,
                    ));
                    if !matches!(adding, Some(adding) if adding == ut) {
                        diag.error(Error::cyclic(
                            &ut.name(scopes, types),
                            &dep.name(scopes, types),
                            span,
                        ));
                    }
                    return;
                }

                deps.push(dep.clone());
                self.add_user_type(scopes, types, diag, dep, Some(ut));
            }
            &Type::Array(ty, len) => {
                let mut inner = ty;
                while let Type::Array(ty, _) = types.get(inner) {
                    inner = *ty;
                }

                if let Type::User(data) = types.get(inner) {
                    deps.push(data.clone());
                }

                self.add_array(scopes, types, diag, ty, len, Some(ut));
            }
            _ => self.add_type(scopes, types, diag, ty, adding),
        }
    }

    fn get_struct_order(
        structs: &HashMap<GenericUserType, Vec<GenericUserType>>,
    ) -> Vec<&GenericUserType> {
        fn dfs<'b>(
            sid: &'b GenericUserType,
            structs: &'b HashMap<GenericUserType, Vec<GenericUserType>>,
            visited: &mut HashMap<&'b GenericUserType, bool>,
            result: &mut Vec<&'b GenericUserType>,
        ) -> Result<(), ()> {
            visited.insert(sid, true);
            if let Some(deps) = structs.get(sid) {
                for dep in deps.iter() {
                    match visited.get(dep) {
                        Some(true) => return Err(()),
                        None => dfs(dep, structs, visited, result)?,
                        _ => {}
                    }
                }
            }

            *visited.get_mut(sid).unwrap() = false;
            result.push(sid);
            Ok(())
        }

        let mut state = HashMap::new();
        let mut result = Vec::new();
        for sid in structs.keys() {
            if state.contains_key(sid) {
                continue;
            }
            _ = dfs(sid, structs, &mut state, &mut result);
        }

        result
    }

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
            buffer.emit("CTL_ZST ");
        }

        buffer.emit_type(scopes, types, ty, None, min);
        buffer.emit(format!(" {};", member_name(scopes, Some(ut.id), name)));
    }

    fn emit_array(
        scopes: &Scopes,
        types: &mut Types,
        typedef: &mut Buffer,
        defs: Option<&mut Buffer>,
        ty: TypeId,
        size: usize,
        min: bool,
    ) {
        typedef.emit("typedef struct ");
        typedef.emit_array_struct_name(scopes, types, ty, size, min);
        typedef.emit(" ");
        typedef.emit_array_struct_name(scopes, types, ty, size, min);
        typedef.emit(";");

        let defs = defs.unwrap_or(typedef);
        defs.emit("struct ");
        defs.emit_array_struct_name(scopes, types, ty, size, min);
        defs.emit("{");
        defs.emit_type(scopes, types, ty, None, min);
        defs.emit(format!(" {ARRAY_DATA_NAME}[{size}];}};"));
    }
}

#[derive(Eq, Clone)]
struct State {
    func: GenericFunc,
    tmpvar: usize,
    caller: ScopeId,
    emitted_names: HashMap<String, VariableId>,
    renames: HashMap<VariableId, String>,
}

impl State {
    pub fn new(func: GenericFunc, caller: ScopeId) -> Self {
        Self {
            func,
            caller,
            tmpvar: 0,
            emitted_names: Default::default(),
            renames: Default::default(),
        }
    }

    pub fn in_body_scope(func: GenericFunc, scopes: &Scopes) -> Self {
        let scope = scopes.get(func.id).body_scope;
        Self::new(func, scope)
    }

    pub fn with_inst(mut func: GenericFunc, types: &Types, inst: TypeId, caller: ScopeId) -> Self {
        if let Some(ut) = types.get(inst).as_user() {
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
        self.0.push_str(source.as_ref());
    }

    fn emit_info(&mut self, source: impl AsRef<str>, min: bool) {
        if !min {
            self.emit(source);
        }
    }

    fn emit_mangled_name(&mut self, scopes: &Scopes, types: &mut Types, id: TypeId, min: bool) {
        match types.get(id) {
            Type::Void => self.emit("void"),
            Type::CVoid => self.emit("c_void"),
            Type::Never => self.emit("never"),
            Type::Int(bits) => self.emit(format!("i{bits}")),
            Type::Uint(bits) => self.emit(format!("u{bits}")),
            id @ (Type::CInt(ty) | Type::CUint(ty)) => {
                self.emit("c_");
                if matches!(id, Type::CUint(_)) {
                    self.emit("u");
                }
                match ty {
                    CInt::Char => self.emit("char"),
                    CInt::Short => self.emit("short"),
                    CInt::Int => self.emit("int"),
                    CInt::Long => self.emit("long"),
                    CInt::LongLong => self.emit("longlong"),
                }
            }
            Type::Isize => self.emit("isize"),
            Type::Usize => self.emit("usize"),
            Type::F32 => self.emit("f32"),
            Type::F64 => self.emit("f64"),
            Type::Bool => self.emit("bool"),
            Type::Char => self.emit("char"),
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
            Type::Func(f) => {
                let fptr = f.clone().as_fn_ptr(scopes, types);
                self.emit_fnptr_name(scopes, types, &fptr, min)
            }
            Type::User(ut) => {
                let ut = ut.clone();
                self.emit_type_name_ex(scopes, types, &ut, min, true);
            }
            &Type::Array(ty, len) => self.emit_array_struct_name(scopes, types, ty, len, min),
            Type::Unknown => {
                self.emit("__Unknown");
                eprintln!("ICE: TypeId::Unknown in emit_generic_mangled_name")
            }
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_generic_mangled_name"),
        }
    }

    fn emit_fnptr_name(&mut self, scopes: &Scopes, types: &mut Types, f: &FnPtr, min: bool) {
        self.emit("fn");
        self.emit_mangled_name(scopes, types, f.ret, min);
        for (i, &param) in f.params.iter().enumerate() {
            if i > 0 && !min {
                self.emit("_");
            }
            self.emit_mangled_name(scopes, types, param, min);
        }
    }

    fn emit_type(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        id: TypeId,
        tg: Option<(&mut Diagnostics, &mut TypeGen)>,
        min: bool,
    ) {
        match types.get(id) {
            Type::Void | Type::Never | Type::CVoid => self.emit("$void"),
            ty @ (Type::Int(bits) | Type::Uint(bits)) => {
                let ch = if matches!(ty, Type::Int(_)) { 's' } else { 'u' };
                self.emit(format!("{ch}{bits}"));
                if let Some((diag, tg)) = tg {
                    tg.add_type(scopes, types, diag, id, None);
                }
            }
            ty @ (Type::CInt(inner) | Type::CUint(inner)) => {
                if matches!(ty, Type::CUint(_)) {
                    self.emit("unsigned ");
                }

                match inner {
                    CInt::Char => self.emit("char"),
                    CInt::Short => self.emit("short"),
                    CInt::Int => self.emit("int"),
                    CInt::Long => self.emit("long"),
                    CInt::LongLong => self.emit("long long"),
                }
            }
            Type::Isize => self.emit("isize"),
            Type::Usize => self.emit("usize"),
            Type::F32 => self.emit("f32"),
            Type::F64 => self.emit("f64"),
            Type::Bool => self.emit("$bool"),
            Type::Char => self.emit("$char"),
            id @ (&Type::Ptr(inner) | &Type::MutPtr(inner) | &Type::RawPtr(inner)) => {
                let id_is_ptr = id.is_ptr();
                if let &Type::Array(ty, _) = types.get(inner) {
                    self.emit_type(scopes, types, ty, tg, min);
                    if !min {
                        self.emit("/*");
                        self.emit_type(scopes, types, inner, None, min);
                        self.emit("*/");
                    }
                } else if types.get(inner).is_c_void() {
                    self.emit("void");
                } else {
                    self.emit_type(scopes, types, inner, tg, min);
                }
                if id_is_ptr {
                    self.emit(" const*");
                } else {
                    self.emit("*");
                }
            }
            Type::FnPtr(f) => {
                if let Some((diag, tg)) = tg {
                    tg.add_fnptr(scopes, types, diag, f.clone());
                }
                self.emit_mangled_name(scopes, types, id, min);
            }
            Type::Func(f) => {
                let f = f.clone().as_fn_ptr(scopes, types);
                self.emit_mangled_name(scopes, types, id, min);
                if let Some((diag, tg)) = tg {
                    tg.add_fnptr(scopes, types, diag, f);
                }
            }
            Type::User(ut) => {
                if scopes.get(ut.id).kind.is_template() {
                    eprintln!("ICE: Template type in emit_type");
                    self.emit(&scopes.get(ut.id).name.data);
                    return;
                }

                if let Some(ty) = id.can_omit_tag(scopes, types) {
                    self.emit_type(scopes, types, ty, None, min);
                } else {
                    let ut = ut.clone();
                    self.emit_type_name(scopes, types, &ut, min);
                    if let Some((diag, tg)) = tg {
                        tg.add_user_type(scopes, types, diag, ut, None);
                    }
                }
            }
            &Type::Array(ty, len) => {
                self.emit_mangled_name(scopes, types, id, min);
                if let Some((diag, tg)) = tg {
                    tg.add_array(scopes, types, diag, ty, len, None);
                }
            }
            Type::DynPtr(tr) | Type::DynMutPtr(tr) => {
                let tr = tr.clone();
                self.emit_type_name(scopes, types, &tr, min);
                if let Some((_, tg)) = tg {
                    tg.add_dynptr(tr);
                }
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
            self.emit(format!("t{}", ut.id));
            for &ty in ut.ty_args.values() {
                self.emit_mangled_name(scopes, types, ty, min);
            }
        } else {
            self.emit(scopes.full_name(ty.scope, &ty.name.data));
            if !ut.ty_args.is_empty() {
                self.emit("$");
                for &ty in ut.ty_args.values() {
                    self.emit("$");
                    self.emit_mangled_name(scopes, types, ty, min);
                }
                self.emit("$$");
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
            self.emit("a");
            self.emit_mangled_name(scopes, types, ty, min);
            self.emit(format!("{size}"));
        } else {
            self.emit("Array_");
            self.emit_mangled_name(scopes, types, ty, min);
            self.emit(format!("_{size}"));
        }
    }

    fn emit_fn_name(&mut self, scopes: &Scopes, types: &mut Types, f: &GenericFunc, min: bool) {
        self.emit_fn_name_ex(scopes, types, false, f, min)
    }

    fn emit_fn_name_ex(
        &mut self,
        scopes: &Scopes,
        types: &mut Types,
        real: bool,
        func: &GenericFunc,
        min: bool,
    ) {
        let f = scopes.get(func.id);
        let is_macro = f.attrs.has(ATTR_NOGEN);
        if f.linkage == Linkage::Internal {
            if min {
                self.emit(format!("p{}", func.id));
                for &ty in func.ty_args.values() {
                    self.emit_mangled_name(scopes, types, ty, min);
                }
            } else {
                self.emit(scopes.full_name(f.scope, &f.name.data));
                if !func.ty_args.is_empty() {
                    self.emit("$");
                    for &ty in func.ty_args.values() {
                        self.emit("$");
                        self.emit_mangled_name(scopes, types, ty, min);
                    }
                    self.emit("$$");
                }
            }
        } else {
            let name = f.attrs.val(ATTR_LINKNAME).unwrap_or(&f.name.data);
            if !is_macro && !real && needs_fn_wrapper(types, f) {
                if min {
                    return self.emit(format!("f{}", func.id));
                } else {
                    self.emit("$");
                }
            }
            self.emit(name);
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

#[derive(Default)]
struct ConditionBuilder(Buffer);

impl ConditionBuilder {
    pub fn next(&mut self, f: impl FnOnce(&mut Buffer)) {
        if !self.0 .0.is_empty() {
            self.0.emit("&&");
        }
        f(&mut self.0);
    }

    pub fn next_str(&mut self, s: impl AsRef<str>) {
        self.next(|buf| buf.emit(s));
    }

    pub fn finish(self) -> String {
        if self.0 .0.is_empty() {
            "1".into()
        } else {
            self.0.finish()
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
    ($self: expr, $state: expr, $ty: expr, $body: expr) => {{
        let ty = $ty;
        let old = std::mem::replace(&mut $self.cur_block, $state.tmpvar());
        hoist!($self, {
            $self.emit_type(ty);
            $self.buffer.emit(format!(" {};", $self.cur_block));
            $body;
        });

        $self
            .buffer
            .emit(std::mem::replace(&mut $self.cur_block, old));
    }};
}

macro_rules! enter_loop {
    ($self: expr, $state: expr, $scope: expr, $ty: expr, $body: expr) => {{
        let ty = $ty;
        let cur_loop = ($scope, $state.tmpvar());
        let old_block = std::mem::replace(&mut $self.cur_block, cur_loop.1.clone());
        let old_loop = std::mem::replace(&mut $self.cur_loop, cur_loop);
        hoist!($self, {
            $self.emit_type(ty);
            $self.buffer.emit(format!(" {};", $self.cur_loop.1));
            $body;
        });

        $self.cur_block = old_block;
        $self
            .buffer
            .emit(std::mem::replace(&mut $self.cur_loop, old_loop).1);
    }};
}

macro_rules! hoist_point {
    ($self: expr, $body: expr) => {
        let old_tmp = std::mem::take(&mut $self.temporaries);
        let old_buf = std::mem::take(&mut $self.buffer);
        $body;
        let written = std::mem::replace(&mut $self.buffer, old_buf).finish();
        $self
            .buffer
            .emit(std::mem::replace(&mut $self.temporaries, old_tmp).finish());
        $self.buffer.emit(written);
    };
}

macro_rules! usebuf {
    ($self: expr, $buf: expr, $body: expr) => {
        std::mem::swap(&mut $self.buffer, $buf);
        $body;
        std::mem::swap(&mut $self.buffer, $buf);
    };
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
    cur_block: String,
    cur_loop: (ScopeId, String),
    flags: CodegenFlags,
    vtables: Buffer,
    emitted_vtables: HashSet<Vtable>,
    defers: Vec<(ScopeId, Vec<CheckedExpr>)>,
    tg: TypeGen,
}

impl Codegen {
    pub fn build(
        scope: ScopeId,
        mut proj: Project,
        flags: CodegenFlags,
    ) -> Result<(Diagnostics, String), Diagnostics> {
        if proj.diag.has_errors() {
            return Err(proj.diag);
        }

        let exports = proj
            .scopes
            .functions()
            .filter(|(_, f)| f.linkage == Linkage::Export)
            .map(|(id, _)| {
                State::in_body_scope(GenericFunc::from_id(&proj.scopes, id), &proj.scopes)
            });
        let (funcs, main) = if flags.lib {
            (exports.collect(), None)
        } else {
            let Some(main) = proj.scopes[scope].vns.get("main").and_then(|id| id.as_fn()) else {
                proj.diag
                    .error(Error::new("no main function found", Span::default()));
                return Err(proj.diag);
            };
            let main =
                State::in_body_scope(GenericFunc::from_id(&proj.scopes, *main), &proj.scopes);
            (
                exports.chain(std::iter::once(main.clone())).collect(),
                Some(main),
            )
        };
        let mut this = Self {
            proj,
            funcs,
            flags,
            buffer: Default::default(),
            temporaries: Default::default(),
            cur_block: Default::default(),
            cur_loop: Default::default(),
            vtables: Default::default(),
            statics: Default::default(),
            emitted_vtables: Default::default(),
            defers: Default::default(),
            tg: TypeGen::new(),
        };
        let main = main.map(|mut main| this.gen_c_main(&mut main));
        let mut static_defs = Buffer::default();
        let mut static_init = Buffer::default();
        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        let mut emitted_statics = HashSet::new();
        while !this.funcs.is_empty() || !this.statics.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for mut state in diff {
                this.emit_fn(&mut state, &mut prototypes);
            }

            let sdiff = this
                .statics
                .difference(&emitted_statics)
                .cloned()
                .collect::<Vec<_>>();
            emitted_statics.extend(this.statics.drain());
            for id in sdiff {
                let state = &mut State::new(
                    GenericFunc::from_id(&this.proj.scopes, FunctionId::RESERVED),
                    this.proj.scopes.get(id).scope,
                );
                usebuf!(this, &mut static_defs, {
                    this.emit_var_decl(id, state);
                    this.buffer.emit(";");
                });

                usebuf!(
                    this,
                    &mut static_init,
                    hoist_point!(this, {
                        this.emit_var_name(id, state);
                        this.buffer.emit("=");
                        this.emit_expr_inner(
                            this.proj.scopes.get(id).value.clone().unwrap(),
                            state,
                        );
                        this.buffer.emit(";");
                    })
                );
            }
        }
        let functions = std::mem::take(&mut this.buffer);
        if this.flags.leak {
            this.buffer.emit("#define CTL_NOGC\n");
        }
        if this.flags.no_bit_int {
            this.buffer.emit("#define CTL_NOBITINT\n");
        }
        this.buffer.emit(include_str!("../ctl/ctl.h"));
        if this.proj.diag.has_errors() {
            return Err(this.proj.diag);
        }
        this.tg.finish(
            &this.proj.scopes,
            &mut this.proj.types,
            &mut this.buffer,
            &this.flags,
        );
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

        Ok((this.proj.diag, this.buffer.finish()))
    }

    fn emit_vtable(&mut self, vtable: Vtable) {
        if self.emitted_vtables.contains(&vtable) {
            return;
        }

        let mut buffer = Buffer::default();
        usebuf!(self, &mut buffer, {
            self.buffer.emit("static const ");
            self.buffer.emit_vtable_struct_name(
                &self.proj.scopes,
                &mut self.proj.types,
                &vtable.tr,
                self.flags.minify,
            );
            self.buffer.emit(" ");
            self.emit_vtable_name(&vtable);
            self.buffer.emit("={");
            for tr in self.proj.scopes.get_trait_impls(vtable.tr.id) {
                for f in vtable_methods(
                    &self.proj.scopes,
                    &self.proj.types,
                    self.proj.scopes.get(tr),
                ) {
                    let func = GenericFunc::new(f.id, vtable.tr.ty_args.clone());
                    let func_data = self.proj.scopes.get(f.id);

                    self.buffer.emit(".");
                    self.buffer.emit_fn_name(
                        &self.proj.scopes,
                        &mut self.proj.types,
                        &func,
                        self.flags.minify,
                    );
                    self.buffer.emit("=(");
                    let ret = func_data.ret.with_templates(&mut self.proj.types, &func.ty_args);
                    self.buffer.emit_type(
                        &self.proj.scopes,
                        &mut self.proj.types,
                        ret,
                        None,
                        self.flags.minify,
                    );
                    self.buffer.emit("(*)(");
                    for (i, param) in func_data.params.iter().enumerate() {
                        let param_ty = param.ty.with_templates(&mut self.proj.types, &func.ty_args);
                        if i > 0 {
                            self.buffer.emit(",");
                            self.buffer.emit_type(
                                &self.proj.scopes,
                                &mut self.proj.types,
                                param_ty,
                                None,
                                self.flags.minify,
                            );
                        } else if self.proj.types.get(param_ty).is_ptr() {
                            self.buffer.emit("const void*");
                        } else {
                            self.buffer.emit("void*");
                        }
                    }
                    self.buffer.emit("))");

                    let func = self.find_implementation(
                        vtable.ty,
                        tr,
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
                    self.buffer.emit(",");
                    self.funcs.insert(State::new(func, vtable.scope));
                }
            }
            self.buffer.emit("};");
        });

        self.vtables.emit(buffer.finish());
        self.emitted_vtables.insert(vtable);
    }

    fn gen_c_main(&mut self, main: &mut State) -> String {
        self.buffer.emit("int main(int argc, char **argv){");
        let returns = self.proj.scopes.get(main.func.id).ret != TypeId::VOID;
        if let Some(id) = self
            .proj
            .scopes
            .lang_fns
            .get("convert_argv")
            .cloned()
            .filter(|_| self.proj.scopes.get(main.func.id).params.len() == 1)
        {
            let state = State::in_body_scope(
                GenericFunc::from_id(&self.proj.scopes, id),
                &self.proj.scopes,
            );
            if returns {
                self.buffer.emit("return ");
            }
            self.buffer.emit_fn_name(
                &self.proj.scopes,
                &mut self.proj.types,
                &main.func,
                self.flags.minify,
            );
            self.buffer.emit("(");
            self.buffer.emit_fn_name(
                &self.proj.scopes,
                &mut self.proj.types,
                &state.func,
                self.flags.minify,
            );
            if returns {
                self.buffer.emit("(argc,(const char **)argv));}");
            } else {
                self.buffer.emit("(argc,(const char **)argv));return 0;}");
            }

            self.funcs.insert(state);
        } else {
            self.buffer.emit("(void)argc;");
            self.buffer.emit("(void)argv;");
            if returns {
                self.buffer.emit("return ");
            }
            self.buffer.emit_fn_name(
                &self.proj.scopes,
                &mut self.proj.types,
                &main.func,
                self.flags.minify,
            );
            if returns {
                self.buffer.emit("();}");
            } else {
                self.buffer.emit("();return 0;}");
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

        if needs_fn_wrapper(&self.proj.types, func) {
            usebuf!(self, prototypes, {
                let returns_never = func.ret == TypeId::NEVER;
                let params = func.params.len();
                self.emit_prototype(state, false, false);

                self.buffer.emit("{");
                self.emit_prototype(state, true, true);
                self.buffer.emit(";");
                self.buffer.emit_fn_name_ex(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    true,
                    &state.func,
                    self.flags.minify,
                );
                self.buffer.emit("(");
                for i in 0..params {
                    if i > 0 {
                        self.buffer.emit(",");
                    }
                    self.buffer.emit(format!("$p{i}"));
                }

                self.buffer.emit(");");
                if returns_never {
                    self.buffer.emit("CTL_UNREACHABLE();}");
                } else {
                    self.buffer.emit(format!("return {VOID_INSTANCE};}}"));
                }
            });
            return;
        }

        usebuf!(self, prototypes, {
            self.emit_prototype(state, true, false);
            self.buffer.emit(";");
        });
        let func = self.proj.scopes.get(state.func.id);
        if let Some(body) = func.body.clone() {
            let returns_never = func.ret == TypeId::NEVER;
            let params = func.params.clone();
            let unused = self.emit_prototype(state, false, false);
            self.buffer.emit("{");
            for id in unused {
                self.buffer.emit("(void)");
                self.emit_var_name(id, state);
                self.buffer.emit(";");
            }

            for param in params.iter() {
                let Some(patt) = param
                    .patt
                    .as_checked()
                    .filter(|patt| !matches!(patt.data, CheckedPatternData::Variable(_)))
                else {
                    continue;
                };

                self.emit_pattern_bindings(state, &patt.data, &param.label, param.ty);
            }

            hoist_point!(self, {
                if returns_never {
                    self.emit_expr_stmt(body, state);
                    self.buffer.emit("}");
                } else {
                    self.buffer.emit("return ");
                    self.emit_expr_inline(body, state);
                    self.buffer.emit(";}");
                }
            });
        }
    }

    fn emit_expr_stmt(&mut self, expr: CheckedExpr, state: &mut State) {
        if Self::has_side_effects(&expr) {
            self.emit_expr_inline(expr, state);
            self.buffer.emit(";");
        } else {
            self.buffer.emit("(void)(");
            self.emit_expr_inline(expr, state);
            self.buffer.emit(");");
        }
    }

    fn emit_stmt(&mut self, stmt: CheckedStmt, state: &mut State) {
        match stmt {
            CheckedStmt::Expr(expr) => {
                hoist_point!(self, self.emit_expr_stmt(expr, state));
            }
            CheckedStmt::Let(patt, value) => {
                hoist_point!(self, {
                    if let CheckedPatternData::Variable(id) = patt.data {
                        if !self.proj.scopes.get(id).unused {
                            let ty = self.emit_var_decl(id, state);
                            if let Some(mut expr) = value {
                                expr.ty = ty;
                                self.buffer.emit("=");
                                self.emit_expr_inner(expr, state);
                            }
                            self.buffer.emit(";");
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
                });
            }
            CheckedStmt::Defer(expr) => {
                self.defers.last_mut().unwrap().1.push(expr);
            }
            CheckedStmt::None => {}
        }
    }

    fn emit_expr(&mut self, mut expr: CheckedExpr, state: &mut State) {
        expr.ty = expr.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
        if Self::has_side_effects(&expr) {
            self.emit_tmpvar_ident(expr, state);
        } else {
            self.emit_expr_inner(expr, state);
        }
    }

    fn emit_expr_inner(&mut self, expr: CheckedExpr, state: &mut State) {
        match expr.data {
            CheckedExprData::Binary { op, left, right } => {
                self.emit_binary(state, op, expr.ty, *left, *right)
            }
            CheckedExprData::Unary { op, expr: inner } => {
                self.emit_unary(state, op, expr.ty, *inner)
            }
            CheckedExprData::AutoDeref(expr, count) => {
                self.buffer.emit(format!("({:*<1$}", "", count));
                self.emit_expr(*expr, state);
                self.buffer.emit(")");
            }
            CheckedExprData::DynCoerce {
                expr: mut inner,
                scope,
            } => {
                inner.ty = inner
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let vtable =
                    if let Type::Ptr(inner) | Type::MutPtr(inner) = self.proj.types.get(inner.ty) {
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
                self.buffer.emit("{.self=(void*)");
                self.emit_expr(*inner, state);
                self.buffer.emit(",.vtable=&");
                self.emit_vtable_name(&vtable);
                self.buffer.emit("}");
                self.emit_vtable(vtable);
            }
            CheckedExprData::Call(callee, args) => {
                let func = self.proj.types.get(callee.ty).as_func().unwrap();
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

                let id = func.id;
                self.emit_expr(*callee, state);
                self.buffer.emit("(");
                self.finish_emit_fn_args(state, id, args);
            }
            CheckedExprData::CallDyn(func, mut args) => {
                let (_, recv) = args.shift_remove_index(0).unwrap();
                let recv = hoist!(self, self.emit_tmpvar(recv, state));
                self.buffer.emit(format!("{recv}.vtable->"));
                self.buffer.emit_fn_name(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    &func,
                    self.flags.minify,
                );
                self.buffer.emit(format!("({recv}.self"));
                if args.is_empty() {
                    self.buffer.emit(")");
                } else {
                    self.buffer.emit(",");
                    self.finish_emit_fn_args(state, func.id, args);
                }
            }
            CheckedExprData::CallFnPtr(expr, args) => {
                self.buffer.emit("(");
                self.emit_expr(*expr, state);
                self.buffer.emit(")(");
                for (i, arg) in args.into_iter().enumerate() {
                    if i > 0 {
                        self.buffer.emit(",");
                    }

                    self.emit_expr(arg, state);
                }
                self.buffer.emit(")");
            }
            CheckedExprData::Array(exprs) => {
                self.buffer.emit("(");
                self.emit_type(expr.ty);
                self.buffer.emit(format!("){{.{ARRAY_DATA_NAME}={{"));
                for expr in exprs {
                    self.emit_expr(expr, state);
                    self.buffer.emit(",");
                }
                self.buffer.emit("}}");
            }
            CheckedExprData::ArrayWithInit { init, count } => {
                // number chosen arbitrarily
                if count <= 32 {
                    self.buffer.emit("(");
                    self.emit_type(expr.ty);
                    self.buffer.emit(format!("){{.{ARRAY_DATA_NAME}={{"));
                    for _ in 0..count {
                        self.emit_expr((*init).clone(), state);
                        self.buffer.emit(",");
                    }
                    self.buffer.emit("}}");
                } else {
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(expr.ty);
                        self.buffer.emit(format!(" {tmp};"));
                        self.buffer.emit(format!(
                            "for(usize i=0;i<{count};i++){{{tmp}.{ARRAY_DATA_NAME}[i]="
                        ));
                        self.emit_expr_inline(*init, state);
                        self.buffer.emit(";}");
                    })
                }
            }
            CheckedExprData::Vec(exprs) => {
                let ut = self.proj.types.get(expr.ty).as_user().unwrap().clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    let len = exprs.len();
                    self.emit_with_capacity(expr.ty, &tmp, &ut, len);
                    for (i, expr) in exprs.into_iter().enumerate() {
                        self.buffer.emit(format!("{tmp}.$ptr[{i}]="));
                        self.emit_expr_inline(expr, state);
                        self.buffer.emit(";");
                    }
                    self.buffer.emit(format!("{tmp}.$len={len};"));
                });
            }
            CheckedExprData::VecWithInit { init, count } => {
                tmpbuf_emit!(self, state, |tmp| {
                    let ut = self.proj.types.get(expr.ty).as_user().unwrap().clone();
                    let len = self.emit_tmpvar(*count, state);
                    self.emit_with_capacity(expr.ty, &tmp, &ut, &len);
                    self.buffer.emit(format!("for(usize i=0;i<{len};i++){{"));
                    hoist_point!(self, {
                        self.buffer.emit("((");
                        self.emit_type(ut.first_type_arg().unwrap());
                        self.buffer.emit(format!("*){tmp}.$ptr)[i]="));
                        self.emit_expr_inline(*init, state);
                        self.buffer.emit(format!(";}}{tmp}.$len={len};"));
                    });
                });
            }
            CheckedExprData::Set(exprs, scope) => {
                let ut = self.proj.types.get(expr.ty).as_user().unwrap().clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_with_capacity(expr.ty, &tmp, &ut, exprs.len());
                    let insert = State::with_inst(
                        GenericFunc::from_id(
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
                        self.buffer.emit(format!("(&{tmp},"));
                        self.emit_expr_inline(val, state);
                        self.buffer.emit(");");
                    }
                    self.funcs.insert(insert);
                });
            }
            CheckedExprData::Map(exprs, scope) => {
                let ut = self.proj.types.get(expr.ty).as_user().unwrap().clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    let insert = State::with_inst(
                        GenericFunc::from_id(
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
                        self.buffer.emit(format!("(&{tmp},"));
                        self.emit_expr(key, state);
                        self.buffer.emit(",");
                        self.emit_expr(val, state);
                        self.buffer.emit(");");
                    }
                    self.funcs.insert(insert);
                });
            }
            CheckedExprData::Bool(value) => {
                self.emit_cast(expr.ty);
                self.buffer.emit(if value { "1" } else { "0" })
            }
            CheckedExprData::Integer(value) => {
                self.emit_cast(expr.ty);
                if self
                    .proj
                    .types
                    .get(expr.ty)
                    .as_integral()
                    .is_some_and(|ty| ty.signed)
                {
                    self.buffer.emit(format!("{value}ll"));
                } else {
                    self.buffer.emit(format!("{value}ull"));
                }
            }
            CheckedExprData::Float(value) => {
                self.emit_cast(expr.ty);
                self.buffer.emit(value);
            }
            CheckedExprData::String(value) => {
                self.buffer.emit("STRLIT(");
                self.emit_type(expr.ty);
                self.buffer.emit(",\"");
                for byte in value.as_bytes() {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer.emit(format!("\",{})", value.len()));
            }
            CheckedExprData::ByteString(value) => {
                self.emit_cast(expr.ty);
                self.buffer.emit("\"");
                for byte in value {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer.emit("\"");
            }
            CheckedExprData::Char(value) => {
                self.emit_cast(expr.ty);
                self.buffer.emit(format!("0x{:x}", value as u32));
            }
            CheckedExprData::Void => self.buffer.emit(VOID_INSTANCE),
            CheckedExprData::Func(mut func, scope) => {
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
            CheckedExprData::MemFunc(mut mfn, scope) => {
                let parent = &self.proj.scopes[self.proj.scopes.get(mfn.func.id).scope];
                if let Some((trait_id, this)) = parent
                    .kind
                    .as_user_type()
                    .and_then(|&id| Some(id).zip(self.proj.scopes.get(id).kind.as_trait()))
                {
                    let inst = mfn
                        .func
                        .ty_args
                        .get(this)
                        .unwrap()
                        .with_templates(&mut self.proj.types, &state.func.ty_args);
                    mfn.func = self.find_implementation(
                        inst,
                        trait_id,
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
            CheckedExprData::Var(id) => {
                if self.proj.scopes.get(id).is_static {
                    self.statics.insert(id);
                }
                self.emit_var_name(id, state);
            }
            CheckedExprData::Instance(members) => self.emit_instance(state, expr.ty, members),
            CheckedExprData::VariantInstance { members, variant } => {
                self.emit_variant_instance(state, expr.ty, &variant, members)
            }
            CheckedExprData::Member { source, member } => {
                let id = self.proj.types.get(source.ty).as_user().map(|ut| ut.id);
                self.emit_expr(*source, state);
                self.buffer
                    .emit(format!(".{}", member_name(&self.proj.scopes, id, &member)));
            }
            CheckedExprData::Block(block) => {
                enter_block!(self, state, expr.ty, {
                    let scope = block.scope;
                    self.buffer.emit_info("{", self.flags.minify);
                    self.emit_block(block, state);
                    if matches!(self.proj.scopes[scope].kind, ScopeKind::Block(_, false)) {
                        self.buffer
                            .emit(format!("{}={VOID_INSTANCE};", self.cur_block));
                    }
                    self.buffer.emit_info("}", self.flags.minify);
                });
            }
            CheckedExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                enter_block!(self, state, expr.ty, {
                    if let CheckedExprData::Is(mut expr, patt) = cond.data {
                        expr.ty = expr.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                        let ty = expr.ty;
                        let tmp = self.emit_tmpvar(*expr, state);
                        self.emit_pattern_if_stmt(state, &patt.data, &tmp, ty);
                    } else {
                        self.buffer.emit("if(");
                        self.emit_expr_inline(*cond, state);
                        self.buffer.emit("){");
                    }
                    hoist_point!(self, {
                        self.buffer.emit(format!("{}=", self.cur_block));
                        self.emit_expr_inline(*if_branch, state);
                    });

                    self.buffer.emit(";}else{");
                    if let Some(else_branch) = else_branch {
                        hoist_point!(self, {
                            self.buffer.emit(format!("{}=", self.cur_block));
                            self.emit_expr_inline(*else_branch, state);
                        });
                    } else {
                        self.buffer
                            .emit(format!("{}={VOID_INSTANCE}", self.cur_block));
                    }

                    self.buffer.emit(";}");
                })
            }
            CheckedExprData::Loop {
                cond,
                body,
                do_while,
                optional,
            } => {
                enter_loop!(self, state, body.scope, expr.ty, {
                    macro_rules! cond {
                        ($cond: expr) => {
                            hoist_point!(self, {
                                self.buffer.emit("if(!");
                                self.emit_expr_inner($cond, state);
                                self.buffer.emit("){");
                                self.emit_loop_break(state, expr.ty, optional);
                                self.buffer.emit("}");
                            });
                        };
                    }

                    self.buffer.emit("for(;;){");
                    hoist_point!(self, {
                        if let Some(mut cond) = cond {
                            cond.ty = cond.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                            if let CheckedExprData::Is(cond, patt) = cond.data {
                                let ty = cond.ty;
                                let tmp = self.emit_tmpvar(*cond, state);
                                self.emit_pattern_if_stmt(state, &patt.data, &tmp, ty);
                                self.emit_block(body, state);
                                self.buffer.emit("}else{");
                                self.emit_loop_break(state, expr.ty, optional);
                                self.buffer.emit("}");
                            } else if !do_while {
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
                    self.buffer.emit("}");
                });
            }
            CheckedExprData::For {
                mut iter,
                patt,
                body,
                optional,
            } => {
                enter_loop!(self, state, body.scope, expr.ty, {
                    iter.ty = iter.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                    let next = self.find_implementation(
                        iter.ty,
                        self.proj.scopes.lang_traits.get("iter").copied().unwrap(),
                        "next",
                        body.scope,
                        |_, _| Default::default(),
                    );

                    let next_state = State::with_inst(next, &self.proj.types, iter.ty, body.scope);
                    let next_ty = self
                        .proj
                        .scopes
                        .get(next_state.func.id)
                        .ret
                        .with_ut_templates(&mut self.proj.types, iter.ty);
                    let iter_var = self.emit_tmpvar(*iter, state);
                    self.buffer.emit("for(;;){");

                    let item = state.tmpvar();
                    self.emit_type(next_ty);
                    self.buffer.emit(format!(" {item}="));
                    self.buffer.emit_fn_name(
                        &self.proj.scopes,
                        &mut self.proj.types,
                        &next_state.func,
                        self.flags.minify,
                    );
                    self.buffer.emit(format!("(&{iter_var});"));
                    let inner = next_ty
                        .as_option_inner(&self.proj.scopes, &self.proj.types)
                        .unwrap();
                    self.emit_pattern_if_stmt(
                        state,
                        &CheckedPatternData::UnionMember {
                            pattern: Some(
                                CheckedPattern::new(
                                    true,
                                    CheckedPatternData::Destrucure {
                                        patterns: vec![("0".into(), inner, patt)],
                                        borrows: false,
                                    },
                                )
                                .into(),
                            ),
                            variant: "Some".into(),
                            inner,
                            borrows: false,
                        },
                        &item,
                        next_ty,
                    );
                    self.emit_block(body, state);
                    self.buffer.emit("}else{");
                    self.emit_loop_break(state, expr.ty, optional);
                    self.buffer.emit("}}");

                    self.funcs.insert(next_state);
                });
            }
            CheckedExprData::Subscript { callee, arg } => {
                // TODO: bounds check
                if self.proj.types.get(callee.ty).is_array() {
                    match callee.data {
                        CheckedExprData::Unary {
                            op: UnaryOp::Deref,
                            expr,
                        } => {
                            // we compile pointers to arrays as T *, not Array_T_N *, so in the case
                            // there will be no data member
                            self.emit_expr(*expr, state);
                        }
                        _ => {
                            self.emit_expr(*callee, state);
                            self.buffer.emit(format!(".{ARRAY_DATA_NAME}"));
                        }
                    }
                } else {
                    self.buffer.emit(format!(
                        "({}",
                        "*".repeat(Self::indirection(&self.proj.types, callee.ty) - 1)
                    ));
                    self.emit_expr(*callee, state);
                    self.buffer.emit(")");
                }

                self.buffer.emit("[");
                self.emit_expr(*arg, state);
                self.buffer.emit("]");
            }
            CheckedExprData::SliceArray {
                callee,
                arg,
                range_full,
            } => {
                let indirection = Self::indirection(&self.proj.types, callee.ty);
                let src = tmpbuf!(self, state, |tmp| {
                    let len = *callee
                        .ty
                        .strip_references_r(&self.proj.types)
                        .as_array()
                        .unwrap()
                        .1;
                    self.emit_type(expr.ty);
                    self.buffer.emit(format!(" {tmp}={{.$ptr="));
                    if indirection != 0 {
                        self.buffer.emit("*".repeat(indirection - 1));
                        self.emit_expr_inline(*callee, state);
                    } else {
                        self.emit_expr_inline(*callee, state);
                        self.buffer.emit(format!(".{ARRAY_DATA_NAME}"));
                    }
                    self.buffer.emit(format!(",.$len={len}}};"));
                    tmp
                });
                if range_full {
                    self.buffer.emit(src);
                } else {
                    let ut = self.proj.types.get(expr.ty).as_user().unwrap().clone();
                    let mut func = GenericFunc::from_type_args(
                        &self.proj.scopes,
                        self.proj
                            .scopes
                            .get(ut.id)
                            .find_associated_fn(&self.proj.scopes, "subspan")
                            .unwrap(),
                        [arg.ty.with_templates(&mut self.proj.types, &state.func.ty_args)],
                    );
                    func.ty_args.copy_args(&ut.ty_args);
                    let new_state = State::in_body_scope(func, &self.proj.scopes);
                    self.buffer.emit_fn_name(
                        &self.proj.scopes,
                        &mut self.proj.types,
                        &new_state.func,
                        self.flags.minify,
                    );
                    self.buffer.emit(format!("(&{src}, "));
                    self.emit_expr_inline(*arg, state);
                    self.buffer.emit(")");
                    self.funcs.insert(new_state);
                }
            }
            CheckedExprData::Return(mut expr) => {
                hoist!(self, {
                    expr.ty = expr.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                    let tmp = self.emit_tmpvar(*expr, state);
                    self.leave_scope(
                        state,
                        &format!("return {tmp}"),
                        self.proj.scopes.get(state.func.id).body_scope,
                    );
                });
                self.buffer.emit(VOID_INSTANCE);
            }
            CheckedExprData::Yield(expr) => {
                hoist!(self, {
                    // currently, the only way to yield is via tail expression, meaning emit_block
                    // will take care of all defers for us. if we allow something like break to
                    // label, we will need to update this to leave_scope, then goto
                    self.buffer.emit(format!("{}=", self.cur_block));
                    self.emit_expr_inline(*expr, state);
                    self.buffer.emit(";");
                });
                self.buffer.emit(VOID_INSTANCE);
            }
            CheckedExprData::Break(expr) => {
                hoist!(self, {
                    self.buffer.emit(format!("{}=", self.cur_loop.1));
                    if let Some(expr) = expr {
                        self.emit_expr_inline(*expr, state);
                    } else {
                        self.buffer.emit(VOID_INSTANCE);
                    }
                    self.buffer.emit(";");
                    self.leave_scope(state, "break", self.cur_loop.0);
                });
                self.buffer.emit(VOID_INSTANCE);
            }
            CheckedExprData::Continue => {
                hoist!(self, self.leave_scope(state, "continue", self.cur_loop.0));
                self.buffer.emit(VOID_INSTANCE);
            }
            CheckedExprData::Match {
                expr: mut scrutinee,
                body,
            } => {
                enter_block!(self, state, expr.ty, {
                    scrutinee.ty = scrutinee
                        .ty
                        .with_templates(&mut self.proj.types, &state.func.ty_args);
                    let ty = scrutinee.ty;
                    let tmp = self.emit_tmpvar(*scrutinee, state);
                    for (i, (patt, expr)) in body.into_iter().enumerate() {
                        if i > 0 {
                            self.buffer.emit("else ");
                        }

                        self.emit_pattern_if_stmt(state, &patt.data, &tmp, ty);

                        hoist_point!(self, {
                            self.buffer.emit(format!("{}=", self.cur_block));
                            self.emit_expr_inline(expr, state);
                            self.buffer.emit(";}");
                        });
                    }

                    self.buffer.emit("else{CTL_UNREACHABLE();}");
                })
            }
            CheckedExprData::As(mut inner, _) => {
                inner.ty = inner
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                // enum tag cast
                if self.proj.types.get(inner.ty).is_user() {
                    self.buffer.emit("(");
                    self.emit_expr(*inner, state);
                    self.buffer.emit(format!(").{UNION_TAG_NAME}"));
                } else {
                    self.emit_cast(expr.ty);
                    self.buffer.emit("(");
                    self.emit_expr(*inner, state);
                    self.buffer.emit(")");
                }
            }
            CheckedExprData::Is(mut inner, patt) => {
                inner.ty = inner
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let ty = inner.ty;
                let tmp = hoist!(self, self.emit_tmpvar(*inner, state));
                let (_, conditions) = self.emit_pattern(state, &patt.data, &tmp, ty);
                self.buffer.emit(conditions.finish());
            }
            CheckedExprData::Lambda(_) => todo!(),
            CheckedExprData::NeverCoerce(inner) => {
                if matches!(self.proj.types.get(expr.ty), Type::Void | Type::CVoid) {
                    self.emit_expr_inline(*inner, state);
                } else {
                    self.buffer.emit("/*never*/((");
                    self.emit_expr_inline(*inner, state);
                    self.buffer.emit("),*(");
                    self.emit_type(expr.ty);
                    self.buffer.emit(format!("*){NULLPTR})"));
                }
            }
            CheckedExprData::SpanMutCoerce(inner) => {
                let tmp = hoist!(self, self.emit_tmpvar(*inner, state));
                self.emit_cast(expr.ty);
                self.buffer
                    .emit(format!("{{.$ptr={tmp}.$ptr,.$len={tmp}.$len}}"));
            }
            CheckedExprData::StringInterpolation {
                mut formatter,
                parts,
                scope,
            } => {
                formatter.ty = formatter
                    .ty
                    .with_templates(&mut self.proj.types, &state.func.ty_args);
                let formatter_ty = formatter.ty;
                let formatter = hoist!(self, {
                    let format_id = self.proj.scopes.lang_traits.get("format").copied().unwrap();
                    let formatter = self.emit_tmpvar(*formatter, state);
                    for mut expr in parts {
                        hoist_point!(self, {
                            expr.ty = expr.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                            let stripped = expr.ty.strip_references(&self.proj.types);
                            let format_state = State::with_inst(
                                self.find_implementation(
                                    stripped,
                                    format_id,
                                    "format",
                                    scope,
                                    |tc, id| TypeArgs::in_order(tc.scopes(), id, [formatter_ty]),
                                ),
                                &self.proj.types,
                                stripped,
                                scope,
                            );
    
                            self.buffer.emit_fn_name(
                                &self.proj.scopes,
                                &mut self.proj.types,
                                &format_state.func,
                                self.flags.minify,
                            );
                            self.buffer.emit("(");
                            self.emit_tmpvar_ident(expr, state);
                            self.buffer.emit(format!(",&{formatter});"));
                            self.funcs.insert(format_state);
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
                        formatter_id,
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
                self.buffer.emit(format!("(&{formatter})"));
                self.funcs.insert(finish_state);
            }
            CheckedExprData::AffixOperator {
                callee,
                mfn,
                param,
                scope,
                postfix,
            } => {
                let deref = "*".repeat(Self::indirection(&self.proj.types, callee.ty));
                if !postfix {
                    hoist!(self, {
                        let expr = CheckedExpr::new(
                            self.proj
                                .scopes
                                .get(mfn.func.id)
                                .ret
                                .with_templates(&mut self.proj.types, &state.func.ty_args),
                            CheckedExprData::member_call(
                                &mut self.proj.types,
                                mfn,
                                [(param, (*callee).clone())].into(),
                                scope,
                            ),
                        );
                        self.emit_expr_stmt(expr, state);
                    });
                    self.buffer.emit(format!("({deref}"));
                    self.emit_expr(*callee, state);
                    self.buffer.emit(")");
                } else {
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(expr.ty);
                        self.buffer.emit(format!(" {tmp}={deref}"));
                        self.emit_expr_inline((*callee).clone(), state);
                        self.buffer.emit(";");

                        let expr = CheckedExpr::new(
                            self.proj
                                .scopes
                                .get(mfn.func.id)
                                .ret
                                .with_templates(&mut self.proj.types, &state.func.ty_args),
                            CheckedExprData::member_call(
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
            CheckedExprData::Error => panic!("ICE: ExprData::Error in gen_expr"),
        }
    }

    #[inline(always)]
    fn emit_expr_inline(&mut self, mut expr: CheckedExpr, state: &mut State) {
        expr.ty = expr.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
        self.emit_expr_inner(expr, state);
    }

    fn emit_instance(
        &mut self,
        state: &mut State,
        ty: TypeId,
        members: IndexMap<String, CheckedExpr>,
    ) {
        self.emit_cast(ty);
        self.buffer.emit("{");
        let ut_id = self.proj.types.get(ty).as_user().unwrap().id;
        if members.is_empty() {
            self.buffer.emit("CTL_DUMMY_INIT");
        }

        for (name, mut value) in members {
            value.ty = value
                .ty
                .with_templates(&mut self.proj.types, &state.func.ty_args);
            self.buffer.emit(format!(
                ".{}=",
                member_name(&self.proj.scopes, Some(ut_id), &name)
            ));
            self.emit_expr(value, state);
            self.buffer.emit(",");
        }
        self.buffer.emit("}");
    }

    fn emit_variant_instance(
        &mut self,
        state: &mut State,
        ty: TypeId,
        variant: &str,
        mut members: IndexMap<String, CheckedExpr>,
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
                    expr.ty = expr.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                    // TODO: dont emit temporaries for expressions that cant have side effects
                    (name, hoist!(self, self.emit_tmpvar(expr, state)))
                })
                .collect();

            let ut_id = self.proj.types.get(ty).as_user().unwrap().id;
            let ut = self.proj.scopes.get(ut_id);
            let union = ut.kind.as_union().unwrap();
            self.buffer.emit(format!(
                "{{.{UNION_TAG_NAME}={},",
                union.variant_tag(variant).unwrap()
            ));

            for (name, value) in members
                .iter()
                .filter(|(name, _)| ut.members.contains_key(name))
            {
                self.buffer.emit(format!(
                    ".{}={value},",
                    member_name(&self.proj.scopes, Some(ut_id), name)
                ));
            }

            if union.variants.get(variant).is_some_and(|v| v.ty.is_some()) {
                self.buffer.emit(format!(".${variant}={{"));
                for (name, value) in members
                    .iter()
                    .filter(|(name, _)| !ut.members.contains_key(name))
                {
                    self.buffer.emit(format!(".${name}={value},"));
                }
                self.buffer.emit("}");
            }
            self.buffer.emit("}");
        }
    }

    fn emit_binary(
        &mut self,
        state: &mut State,
        op: BinaryOp,
        ret: TypeId,
        mut lhs: CheckedExpr,
        rhs: CheckedExpr,
    ) {
        match op {
            BinaryOp::NoneCoalesceAssign => {
                lhs.ty = lhs.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
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
                        &CheckedPatternData::UnionMember {
                            pattern: None,
                            variant: "None".into(),
                            inner: ret,
                            borrows: false,
                        },
                        &left,
                        opt_type,
                    );

                    hoist_point!(self, {
                        self.buffer.emit(format!("{left}{tag}="));
                        self.emit_expr_inline(rhs, state);
                        if opt_type
                            .can_omit_tag(&self.proj.scopes, &self.proj.types)
                            .is_none()
                        {
                            self.buffer.emit(";");
                            let union = self
                                .proj
                                .scopes
                                .get(self.proj.types.get(opt_type).as_user().unwrap().id)
                                .kind
                                .as_union()
                                .unwrap();
                            self.buffer.emit(format!(
                                "{left}.{UNION_TAG_NAME}={}",
                                union.variant_tag("Some").unwrap()
                            ));
                        }
                        self.buffer.emit(";}");
                    });
                });

                self.buffer.emit(VOID_INSTANCE);
            }
            BinaryOp::NoneCoalesce => {
                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_type(ret);
                    self.buffer.emit(format!(" {tmp};"));

                    lhs.ty = lhs.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                    let opt_type = lhs.ty;
                    let name = hoist!(self, self.emit_tmpvar(lhs, state));
                    self.emit_pattern_if_stmt(
                        state,
                        &CheckedPatternData::UnionMember {
                            pattern: None,
                            variant: "None".into(),
                            inner: ret,
                            borrows: false,
                        },
                        &name,
                        opt_type,
                    );
                    hoist_point!(self, {
                        self.buffer.emit(format!("{tmp}="));
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
                    self.buffer.emit(format!(";}}else{{{tmp}={name}{tag};}}"));
                });
            }
            BinaryOp::Cmp => {
                let tmp = tmpbuf!(self, state, |tmp| {
                    let ty = self.proj.types.get(lhs.ty);
                    if matches!(ty, Type::RawPtr(_)) {
                        self.emit_type(TypeId::ISIZE);
                    } else if let Some(int) = ty.as_integral() {
                        let ty = self
                            .proj
                            .types
                            .insert(Type::Int(int.bits + int.signed as u32));
                        self.emit_type(ty);
                    } else {
                        self.emit_type(lhs.ty);
                    }
                    self.buffer.emit(format!(" {tmp}="));
                    self.emit_expr(lhs, state);
                    self.buffer.emit("-");
                    self.emit_expr(rhs, state);
                    self.buffer.emit(";");

                    tmp
                });

                let union = self
                    .proj
                    .scopes
                    .get(self.proj.types.get(ret).as_user().unwrap().id)
                    .kind
                    .as_union()
                    .unwrap();
                let less = union.variant_tag("Less").unwrap();
                let greater = union.variant_tag("Greater").unwrap();
                let equal = union.variant_tag("Equal").unwrap();

                self.buffer.emit(format!("({tmp}<0?"));
                self.emit_cast(ret);
                self.buffer
                    .emit(format!("{{.{UNION_TAG_NAME}={less}}}:({tmp}>0?",));
                self.emit_cast(ret);
                self.buffer
                    .emit(format!("{{.{UNION_TAG_NAME}={greater}}}:",));
                self.emit_cast(ret);
                self.buffer
                    .emit(format!("{{.{UNION_TAG_NAME}={equal}}}))",));
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                tmpbuf_emit!(self, state, |tmp| {
                    let lor = matches!(op, BinaryOp::LogicalOr);
                    let end_label = state.tmpvar();

                    self.emit_type(TypeId::BOOL);
                    self.buffer.emit(format!(" {tmp}="));
                    self.emit_expr_inline(lhs, state);
                    self.buffer.emit(format!(
                        ";if({}{tmp}){{goto {end_label};}}",
                        if lor { "" } else { "!" }
                    ));
                    hoist_point!(self, {
                        self.buffer.emit(format!("{tmp}="));
                        self.emit_expr_inline(rhs, state);
                        self.buffer.emit(format!(";{end_label}:;"));
                    });
                });
            }
            _ => {
                lhs.ty = lhs.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                if ret == TypeId::BOOL && lhs.ty != TypeId::BOOL {
                    self.emit_cast(ret);
                }
                if op.is_assignment() {
                    self.buffer.emit("(");
                }
                self.buffer.emit("(");
                self.emit_expr(lhs, state);
                self.buffer.emit(format!("{op}"));
                self.emit_expr(rhs, state);
                self.buffer.emit(")");
                if op.is_assignment() {
                    self.buffer.emit(format!(", {VOID_INSTANCE})"));
                }
            }
        }
    }

    fn emit_unary(&mut self, state: &mut State, op: UnaryOp, ret: TypeId, mut lhs: CheckedExpr) {
        match op {
            UnaryOp::Plus => self.emit_expr(lhs, state),
            UnaryOp::Neg => {
                self.buffer.emit("-");
                self.emit_expr(lhs, state);
            }
            UnaryOp::PostIncrement => {
                self.emit_expr(lhs, state);
                self.buffer.emit("++");
            }
            UnaryOp::PostDecrement => {
                self.emit_expr(lhs, state);
                self.buffer.emit("--");
            }
            UnaryOp::PreIncrement => {
                self.buffer.emit("++");
                self.emit_expr(lhs, state);
            }
            UnaryOp::PreDecrement => {
                self.buffer.emit("--");
                self.emit_expr(lhs, state);
            }
            UnaryOp::Not => {
                if lhs.ty == TypeId::BOOL {
                    self.emit_cast(ret);
                    self.buffer.emit("((");
                    self.emit_expr(lhs, state);
                    self.buffer.emit(") ^ 1)");
                } else {
                    self.buffer.emit("~");
                    self.emit_expr(lhs, state);
                }
            }
            UnaryOp::Deref => {
                self.buffer.emit("(*");
                self.emit_expr(lhs, state);
                self.buffer.emit(")");
            }
            UnaryOp::Addr | UnaryOp::AddrMut | UnaryOp::AddrRaw => {
                lhs.ty = lhs.ty.with_templates(&mut self.proj.types, &state.func.ty_args);

                let array = self.proj.types.get(lhs.ty).is_array();
                if !array {
                    self.buffer.emit("&");
                }
                if Self::is_lvalue(&lhs) {
                    self.emit_expr_inner(lhs, state);
                } else {
                    self.emit_tmpvar_ident(lhs, state);
                }

                if array {
                    self.buffer.emit(format!(".{ARRAY_DATA_NAME}"));
                }
            }
            UnaryOp::Try => {
                tmpbuf_emit!(self, state, |tmp| {
                    lhs.ty = lhs.ty.with_templates(&mut self.proj.types, &state.func.ty_args);

                    self.emit_type(ret);
                    self.buffer.emit(format!(" {tmp};"));

                    let inner_ty = lhs.ty;
                    let inner_tmp = self.emit_tmpvar(lhs, state);
                    self.emit_pattern_if_stmt(
                        state,
                        &CheckedPatternData::UnionMember {
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
                            self.buffer.emit("return ");
                            let mut ret_type = self.proj.scopes.get(state.func.id).ret;
                            ret_type =
                                ret_type.with_templates(&mut self.proj.types, &state.func.ty_args);
                            self.emit_expr_inner(CheckedExpr::option_null(ret_type), state);
                        });
                        self.leave_scope(
                            state,
                            &buffer.finish(),
                            self.proj.scopes.get(state.func.id).body_scope,
                        );
                    });
                    self.buffer.emit(format!("}}{tmp}="));
                    if inner_ty
                        .can_omit_tag(&self.proj.scopes, &self.proj.types)
                        .is_some()
                    {
                        self.buffer.emit(format!("{inner_tmp};"));
                    } else {
                        self.buffer.emit(format!("{inner_tmp}.$Some.$0;"));
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
        args: IndexMap<String, CheckedExpr>,
    ) {
        let mut args: IndexMap<_, _> = args
            .into_iter()
            .map(|(name, mut expr)| {
                expr.ty = expr.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                // TODO: dont emit temporaries for expressions that cant have side effects
                (name, hoist!(self, self.emit_tmpvar(expr, state)))
            })
            .collect();

        let mut count = 0;
        macro_rules! arg {
            ($arg: expr) => {{
                if count > 0 {
                    self.buffer.emit(",");
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
        self.buffer.emit(")");
    }

    fn emit_loop_break(&mut self, state: &mut State, ty: TypeId, optional: bool) {
        if optional {
            self.buffer.emit(format!("{}=", self.cur_loop.1));
            self.emit_expr_inline(CheckedExpr::option_null(ty), state);
        } else {
            self.buffer
                .emit(format!("{}={VOID_INSTANCE}", self.cur_loop.1));
        }
        self.buffer.emit(";break;");
    }

    fn leave_scope(&mut self, state: &mut State, exit: &str, scope: ScopeId) {
        let mut emitted = false;
        for i in (0..self.defers.len()).rev() {
            for j in (0..self.defers[i].1.len()).rev() {
                if !emitted {
                    self.buffer
                        .emit_info(format!("/* begin defers {:?} */", scope), self.flags.minify);
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
            self.buffer
                .emit_info(format!("/* end defers {:?} */", scope), self.flags.minify);
        }
        self.buffer.emit(format!("{exit};"));
    }

    fn emit_new(&mut self, ut: &GenericUserType) {
        // FIXME: this should technically use the scope that is creating the literal, but since
        // none of the constructors for literals use trait functions in any way, it doesn't matter
        // right now
        let new_state = State::in_body_scope(
            GenericFunc::new(
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
        self.buffer.emit("()");
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
        self.buffer.emit(format!(" {tmp}="));
        // FIXME: this should technically use the scope that is creating the literal, but since
        // none of the constructors for literals use trait functions in any way, it doesn't matter
        // right now
        let state = State::in_body_scope(
            GenericFunc::new(
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
        self.buffer.emit(format!("({len});"));
        self.funcs.insert(state);
    }

    fn emit_intrinsic(
        &mut self,
        name: &str,
        ret: TypeId,
        func: &GenericFunc,
        mut args: IndexMap<String, CheckedExpr>,
        state: &mut State,
    ) {
        match name {
            "numeric_abs" => {
                let (_, mut expr) = args.shift_remove_index(0).unwrap();
                expr.ty = expr.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
                let tmp = hoist!(self, self.emit_tmpvar(expr, state));
                self.buffer.emit(format!("({tmp}<0?-{tmp}:{tmp})"));
            }
            "numeric_cast" => {
                let (_, expr) = args.shift_remove_index(0).unwrap();
                self.emit_cast(ret);
                self.emit_expr(expr, state);
            }
            "max_value" => {
                self.emit_cast(ret);
                self.buffer.emit(format!(
                    "({})",
                    self.proj.types.get(ret).as_integral().unwrap().max()
                ));
            }
            "min_value" => {
                self.emit_cast(ret);
                self.buffer.emit(format!(
                    "({}-1)",
                    self.proj.types.get(ret).as_integral().unwrap().min() + 1
                ));
            }
            "size_of" => {
                self.buffer.emit(format!(
                    "(usize){}",
                    func.first_type_arg()
                        .unwrap()
                        .size_and_align(&self.proj.scopes, &mut self.proj.types)
                        .0
                ));
            }
            "align_of" => {
                self.buffer.emit(format!(
                    "(usize){}",
                    func.first_type_arg()
                        .unwrap()
                        .size_and_align(&self.proj.scopes, &mut self.proj.types)
                        .1
                ));
            }
            "panic" => {
                let panic = State::in_body_scope(
                    GenericFunc::from_id(
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

                self.buffer.emit_fn_name(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    &panic.func,
                    self.flags.minify,
                );
                self.buffer.emit("(");
                for (i, (_, expr)) in args.into_iter().enumerate() {
                    if i > 0 {
                        self.buffer.emit(",");
                    }
                    self.emit_expr(expr, state);
                }
                self.buffer.emit(")");

                self.funcs.insert(panic);
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
                        "and" => BinaryOp::BitAnd,
                        "or" => BinaryOp::BitOr,
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
            _ => unreachable!(),
        }
    }

    fn emit_tmpvar_ident(&mut self, expr: CheckedExpr, state: &mut State) {
        tmpbuf_emit!(self, state, |tmp| {
            self.emit_type(expr.ty);
            self.buffer.emit(format!(" {tmp}="));
            self.emit_expr_inner(expr, state);
            self.buffer.emit(";");
        });
    }

    fn emit_tmpvar(&mut self, expr: CheckedExpr, state: &mut State) -> String {
        let tmp = state.tmpvar();
        self.emit_type(expr.ty);
        self.buffer.emit(format!(" {tmp}="));
        self.emit_expr_inner(expr, state);
        self.buffer.emit(";");
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
            self.buffer.emit("_");
        }
        self.buffer.emit_type_name(
            &self.proj.scopes,
            &mut self.proj.types,
            &vtable.tr,
            self.flags.minify,
        );
        self.buffer
            .emit(if self.flags.minify { "v" } else { "_$vtable" });
        self.buffer.emit(format!("{}", vtable.scope.0));
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_pattern_inner(
        &mut self,
        state: &mut State,
        pattern: &CheckedPatternData,
        src: &str,
        ty: TypeId,
        borrow: bool,
        bindings: &mut Buffer,
        conditions: &mut ConditionBuilder,
    ) {
        match pattern {
            CheckedPatternData::Int(value) => {
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        self.buffer
                            .emit(format!("{}==", Self::deref(&self.proj.types, src, ty)));
                        self.emit_cast(ty.strip_references(&self.proj.types));
                        self.buffer.emit(format!("{value}"));
                    });
                });
            }
            CheckedPatternData::IntRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                let src = Self::deref(&self.proj.types, src, ty);
                let base = ty.strip_references(&self.proj.types);
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        self.buffer.emit(format!("{src}>="));
                        self.emit_cast(base);
                        self.buffer.emit(format!(
                            "{start}&&{src}{}",
                            if *inclusive { "<=" } else { "<" }
                        ));
                        self.emit_cast(base);
                        self.buffer.emit(format!("{end}"));
                    });
                });
            }
            CheckedPatternData::String(value) => {
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        self.buffer.emit(format!(
                            "{0}.$span.$len=={1}&&CTL_MEMCMP({0}.$span.$ptr,\"",
                            Self::deref(&self.proj.types, src, ty),
                            value.len()
                        ));
                        for byte in value.as_bytes() {
                            self.buffer.emit(format!("\\x{byte:x}"));
                        }
                        self.buffer.emit(format!("\",{})==0", value.len()));
                    });
                });
            }
            CheckedPatternData::UnionMember {
                pattern: patt,
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
                            patt.as_ref().and_then(|patt| patt.data.as_destrucure())
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
                    let tag = self
                        .proj
                        .types
                        .get(base)
                        .as_user()
                        .and_then(|ut| self.proj.scopes.get(ut.id).kind.as_union())
                        .and_then(|union| union.variant_tag(variant))
                        .unwrap();
                    conditions.next_str(format!("{src}.{UNION_TAG_NAME}=={tag}"));

                    if let Some(patt) = patt {
                        self.emit_pattern_inner(
                            state,
                            &patt.data,
                            &format!("{src}.${variant}"),
                            *inner,
                            borrow || *borrows,
                            bindings,
                            conditions,
                        );
                    }
                }
            }
            CheckedPatternData::Span {
                patterns,
                rest,
                inner,
            } => {
                let src = Self::deref(&self.proj.types, src, ty);
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        self.buffer.emit(format!(
                            "{src}.$len{}{}",
                            if rest.is_some() { ">=" } else { "==" },
                            patterns.len()
                        ));
                    });
                });

                let pos = rest.map(|RestPattern { id, pos }| {
                    if let Some(id) = id.filter(|&id| !self.proj.scopes.get(id).unused) {
                        usebuf!(self, bindings, {
                            self.emit_var_decl(id, state);
                            self.buffer.emit(format!(
                                "={{.$ptr={src}.$ptr+{pos},.$len={src}.$len-{}}};",
                                patterns.len()
                            ));
                        });
                    }
                    pos
                });
                for (i, patt) in patterns.iter().enumerate() {
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &if pos.is_some_and(|pos| i >= pos) {
                            format!("{src}.$ptr[{src}.$len-{}+{i}]", patterns.len())
                        } else {
                            format!("{src}.$ptr[{i}]")
                        },
                        *inner,
                        true,
                        bindings,
                        conditions,
                    );
                }
            }
            CheckedPatternData::Destrucure { patterns, borrows } => {
                let src = Self::deref(&self.proj.types, src, ty);
                let ut_id = ty
                    .strip_references_r(&self.proj.types)
                    .as_user()
                    .map(|ut| ut.id);
                for (member, inner, patt) in patterns {
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &format!("{src}.{}", member_name(&self.proj.scopes, ut_id, member)),
                        *inner,
                        borrow || *borrows,
                        bindings,
                        conditions,
                    );
                }
            }
            CheckedPatternData::Array {
                patterns:
                    ArrayPattern {
                        patterns,
                        rest,
                        arr_len,
                        inner,
                    },
                borrows,
            } => {
                let is_any_ptr = self.proj.types.get(ty).is_any_ptr();
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
                                self.buffer.emit(format!("={src}+{pos};"));
                            } else {
                                self.buffer.emit(format!("={{.{ARRAY_DATA_NAME}={{"));
                                for i in 0..rest_len {
                                    self.buffer.emit(format!("{src}[{}],", pos + i));
                                }
                                self.buffer.emit("}};");
                            }
                        });
                    }

                    (pos, rest_len)
                });

                for (mut i, patt) in patterns.iter().enumerate() {
                    if let Some((_, rest_len)) = rest.filter(|&(pos, _)| i >= pos) {
                        i += rest_len;
                    }
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &format!("{src}[{i}]"),
                        *inner,
                        borrow || *borrows,
                        bindings,
                        conditions,
                    );
                }
            }
            &CheckedPatternData::Variable(id) => {
                if self.proj.scopes.get(id).unused {
                    return;
                }

                usebuf!(self, bindings, {
                    let id = self.emit_var_decl(id, state);
                    let ty = self.proj.types.get(id);
                    if borrow
                        && matches!(ty, Type::Ptr(i) | Type::MutPtr(i) | Type::RawPtr(i) if self.proj.types.get(*i).is_array())
                    {
                        self.buffer.emit(format!("={src}.{ARRAY_DATA_NAME};"));
                    } else {
                        self.buffer
                            .emit(format!("={}{src};", if borrow { "&" } else { "" }));
                    }
                });
            }
            CheckedPatternData::Void => conditions.next_str("1"),
            CheckedPatternData::Error => panic!("ICE: CheckedPatternData::Error in gen_pattern"),
        }
    }

    fn emit_pattern(
        &mut self,
        state: &mut State,
        pattern: &CheckedPatternData,
        src: &str,
        ty: TypeId,
    ) -> (Buffer, ConditionBuilder) {
        let mut bindings = Buffer::default();
        let mut conditions = ConditionBuilder::default();
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
        pattern: &CheckedPatternData,
        src: &str,
        ty: TypeId,
    ) {
        let (bindings, conditions) = self.emit_pattern(state, pattern, src, ty);
        self.buffer.emit(format!("if({}){{", conditions.finish()));
        self.buffer.emit(bindings.finish());
    }

    fn emit_pattern_bindings(
        &mut self,
        state: &mut State,
        pattern: &CheckedPatternData,
        src: &str,
        ty: TypeId,
    ) {
        let (bindings, _) = self.emit_pattern(state, pattern, src, ty);
        self.buffer.emit(bindings.finish());
    }

    fn emit_block(&mut self, block: Block, state: &mut State) {
        hoist_point!(self, {
            self.defers.push((block.scope, vec![]));
            for stmt in block.body.into_iter() {
                self.emit_stmt(stmt, state);
            }
        });
        let (id, defers) = self.defers.pop().unwrap();
        if !defers.is_empty() {
            self.buffer
                .emit_info(format!("/* begin defers {:?} */", id), self.flags.minify);

            for expr in defers.into_iter().rev() {
                hoist_point!(self, self.emit_expr_stmt(expr, state));
            }
            self.buffer
                .emit_info(format!("/* end defers {:?} */", id), self.flags.minify);
        }
    }

    fn emit_type(&mut self, id: TypeId) {
        self.buffer.emit_type(
            &self.proj.scopes,
            &mut self.proj.types,
            id,
            Some((&mut self.proj.diag, &mut self.tg)),
            self.flags.minify,
        );
    }

    fn emit_cast(&mut self, id: TypeId) {
        self.buffer.emit("(");
        self.emit_type(id);
        self.buffer.emit(")");
    }

    fn emit_prototype(
        &mut self,
        state: &mut State,
        is_prototype: bool,
        real: bool,
    ) -> Vec<VariableId> {
        let f = self.proj.scopes.get(state.func.id);
        let mut ret = f.ret;
        ret = ret.with_templates(&mut self.proj.types, &state.func.ty_args);

        let needs_wrapper = needs_fn_wrapper(&self.proj.types, f);
        if f.linkage == Linkage::Internal || (needs_wrapper && !real) {
            self.buffer.emit("static ");
            // TODO: inline manually
            if needs_wrapper || f.attrs.val("inline").is_some_and(|v| v == "always") {
                self.buffer.emit("CTL_FORCEINLINE ");
            } else if f.attrs.has("inline") {
                self.buffer.emit("inline ");
            }
        } else {
            self.buffer.emit("extern ");
        }

        if ret == TypeId::NEVER && (!needs_wrapper || real) {
            self.buffer.emit("CTL_NORETURN ");
        }

        let variadic = f.variadic;
        let params = f.params.clone();
        let is_import = f.linkage == Linkage::Import;
        if real && needs_wrapper {
            self.buffer.emit("void ");
        } else {
            self.emit_type(ret);
            self.buffer.emit(" ");
        }
        self.buffer.emit_fn_name_ex(
            &self.proj.scopes,
            &mut self.proj.types,
            real,
            &state.func,
            self.flags.minify,
        );
        self.buffer.emit("(");

        let mut unused = vec![];
        let mut nonnull = vec![];
        for (i, param) in params.iter().enumerate() {
            let mut ty = param.ty;
            ty = ty.with_templates(&mut self.proj.types, &state.func.ty_args);
            if i > 0 {
                self.buffer.emit(",");
            }

            if self.proj.types.get(ty).is_any_ptr() && is_prototype {
                nonnull.push(format!("{}", i + 1));
            }

            if !is_prototype && needs_wrapper {
                self.emit_type(ty);
                self.buffer.emit(format!(" $p{i}"));
            } else if is_import || is_prototype {
                self.emit_type(ty);
            } else if let ParamPattern::Checked(CheckedPattern {
                data: CheckedPatternData::Variable(id),
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
                self.buffer.emit(format!(" {}", param.label));
            }
        }

        if variadic {
            if params.is_empty() {
                self.buffer.emit("...)");
            } else {
                self.buffer.emit(",...)");
            }
        } else if params.is_empty() {
            self.buffer.emit("void)");
        } else {
            self.buffer.emit(")");
        }

        if !nonnull.is_empty() {
            self.buffer
                .emit(format!("CTL_NONNULL({})", nonnull.join(",")));
        }

        unused
    }

    fn emit_var_name(&mut self, id: VariableId, state: &mut State) {
        use std::collections::hash_map::*;

        if self.flags.minify {
            return self.buffer.emit(format!("v{id}"));
        }

        let var = self.proj.scopes.get(id);
        if var.is_static {
            self.buffer
                .emit(self.proj.scopes.full_name(var.scope, &var.name.data));
        } else {
            let mut emit = || {
                self.buffer.emit("$");
                if var
                    .name
                    .data
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_digit())
                {
                    self.buffer.emit("p");
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
        let ty = var.ty.with_templates(&mut self.proj.types, &state.func.ty_args);
        if var.is_static {
            self.buffer.emit("static ");
        }

        let emit_const = !var.mutable && !var.is_static;
        self.emit_type(ty);
        if emit_const {
            self.buffer.emit(" const");
        }
        self.buffer.emit(" ");
        self.emit_var_name(id, state);
        ty
    }

    fn has_side_effects(expr: &CheckedExpr) -> bool {
        match &expr.data {
            CheckedExprData::Unary {
                op:
                    UnaryOp::PostIncrement
                    | UnaryOp::PostDecrement
                    | UnaryOp::PreIncrement
                    | UnaryOp::PreDecrement,
                ..
            } => true,
            CheckedExprData::Call { .. }
            | CheckedExprData::CallFnPtr { .. }
            | CheckedExprData::CallDyn { .. } => true,
            CheckedExprData::Binary { op, .. } if op.is_assignment() => true,
            _ => false,
        }
    }

    fn is_lvalue(expr: &CheckedExpr) -> bool {
        matches!(
            &expr.data,
            CheckedExprData::Unary {
                op: UnaryOp::Deref,
                ..
            } | CheckedExprData::AutoDeref { .. }
                | CheckedExprData::Var(_)
                | CheckedExprData::Member { .. }
                | CheckedExprData::Subscript { .. }
        )
    }

    fn find_implementation(
        &mut self,
        inst: TypeId,
        trait_id: TraitId,
        method: &str,
        scope: ScopeId,
        finish: impl FnOnce(&mut TypeChecker, FunctionId) -> TypeArgs + Clone,
    ) -> GenericFunc {
        // TODO: fix this disgusting hack
        let m = TypeChecker::with_project(&mut self.proj, |tc| {
            tc.get_member_fn_ex(inst, Some(trait_id), method, scope, finish)
        });

        let Some(m) = m else {
            panic!(
                "searching from scope: '{}', cannot find implementation for method '{}::{method}' for type '{}'",
                self.proj.scopes.full_name(scope, ""),
                self.proj.scopes.get(trait_id).name.data,
                inst.name(&self.proj.scopes, &mut self.proj.types)
            )
        };

        if !self.proj.scopes.get(m.func.id).has_body {
            panic!(
                "searching from scope: '{}', get_member_fn_ex picked invalid function for implementation for method '{}::{method}' for type '{}'",
                self.proj.scopes.full_name(scope, ""),
                self.proj.scopes.get(trait_id).name.data,
                inst.name(&self.proj.scopes, &mut self.proj.types)
            )
        }

        m.func
    }

    fn deref(types: &Types, src: &str, ty: TypeId) -> String {
        if matches!(types.get(ty), Type::Ptr(_) | Type::MutPtr(_)) {
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
        while let Type::Ptr(inner) | Type::MutPtr(inner) = types.get(id) {
            id = *inner;
            count += 1;
        }
        count
    }
}

fn needs_fn_wrapper(types: &Types, f: &Function) -> bool {
    f.linkage == Linkage::Import
        && matches!(types.get(f.ret), Type::Void | Type::Never | Type::CVoid)
}

fn vtable_methods(scopes: &Scopes, types: &Types, tr: &UserType) -> Vec<Vis<FunctionId>> {
    let this = *tr
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
                    .all(|p| !types.get(p.ty).as_user().is_some_and(|ty| ty.id == this))
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
