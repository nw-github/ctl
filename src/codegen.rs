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
    sym::*,
    typeid::{CInt, FnPtr, GenericFunc, GenericTrait, GenericUserType, Type, TypeArgs},
    CodegenFlags, THIS_PARAM,
};

const UNION_TAG_NAME: &str = "tag";
const ARRAY_DATA_NAME: &str = "data";
const VOID_INSTANCE: &str = "CTL_VOID";
const ATTR_NOGEN: &str = "c_opaque";
const ATTR_LINKNAME: &str = "c_name";
const NULLPTR: &str = "((void*)0)";

#[derive(Eq, Clone)]
struct State {
    func: GenericFunc,
    tmpvar: usize,
    caller: ScopeId,
    emitted_names: HashMap<String, VariableId>,
    renames: HashMap<VariableId, String>,
}

impl State {
    pub fn new(func: GenericFunc) -> Self {
        Self::new_with_caller(func, ScopeId::ROOT)
    }

    pub fn new_with_caller(func: GenericFunc, caller: ScopeId) -> Self {
        Self {
            func,
            caller,
            tmpvar: 0,
            emitted_names: Default::default(),
            renames: Default::default(),
        }
    }

    pub fn with_inst(func: GenericFunc, inst: &Type) -> Self {
        Self::with_inst_and_scope(func, inst, ScopeId::ROOT)
    }

    pub fn with_inst_and_scope(mut func: GenericFunc, inst: &Type, caller: ScopeId) -> Self {
        if let Some(ut) = inst.as_user() {
            func.ty_args.copy_args(&ut.ty_args);
        }

        Self::new_with_caller(func, caller)
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

struct TypeGen<'a> {
    structs: HashMap<GenericUserType, Vec<GenericUserType>>,
    fnptrs: HashSet<FnPtr>,
    arrays: HashMap<Type, HashSet<usize>>,
    integers: HashSet<(u32, bool)>,
    dynptrs: HashSet<GenericTrait>,
    scopes: &'a Scopes,
}

impl<'a> TypeGen<'a> {
    fn new(scopes: &'a Scopes) -> Self {
        Self {
            scopes,
            structs: Default::default(),
            fnptrs: Default::default(),
            arrays: Default::default(),
            integers: Default::default(),
            dynptrs: Default::default(),
        }
    }

    fn finish(mut self, buffer: &mut Buffer, flags: &CodegenFlags) {
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
            defs.emit_type(self.scopes, &f.ret, None, flags.minify);
            defs.emit("(*");
            defs.emit_fnptr_name(self.scopes, &f, flags.minify);
            defs.emit(")(");
            for (i, param) in f.params.iter().enumerate() {
                if i > 0 {
                    defs.emit(",");
                }

                defs.emit_type(self.scopes, param, None, flags.minify);
            }
            defs.emit(");");
        }

        for tr in self.dynptrs {
            buffer.emit("typedef struct ");
            buffer.emit_vtable_struct_name(self.scopes, &tr, flags.minify);
            buffer.emit(" ");
            buffer.emit_vtable_struct_name(self.scopes, &tr, flags.minify);

            buffer.emit(";typedef struct ");
            buffer.emit_trait_name(self.scopes, &tr, flags.minify);
            buffer.emit("{void*self;");
            buffer.emit_vtable_struct_name(self.scopes, &tr, flags.minify);
            buffer.emit(" const*vtable;}");
            buffer.emit_trait_name(self.scopes, &tr, flags.minify);
            buffer.emit(";");

            defs.emit("struct ");
            defs.emit_vtable_struct_name(self.scopes, &tr, flags.minify);
            defs.emit("{");
            for id in self.scopes.get_trait_impls(tr.id) {
                for f in vtable_methods(self.scopes, self.scopes.get(id)) {
                    defs.emit_type(
                        self.scopes,
                        &self.scopes.get(f.id).ret.with_templates(&tr.ty_args),
                        None,
                        flags.minify,
                    );
                    defs.emit("(*const ");
                    defs.emit_fn_name(
                        self.scopes,
                        &GenericFunc::new(f.id, tr.ty_args.clone()),
                        flags.minify,
                    );
                    defs.emit(")(");
                    for (i, param) in self.scopes.get(f.id).params.iter().enumerate() {
                        if i > 0 {
                            defs.emit(",");
                            defs.emit_type(
                                self.scopes,
                                &param.ty.with_templates(&tr.ty_args),
                                None,
                                flags.minify,
                            );
                        } else if param.ty.is_ptr() {
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
            let ut_data = self.scopes.get(ut.id);
            buffer.emit(if ut_data.data.is_unsafe_union() {
                "typedef union "
            } else {
                "typedef struct "
            });
            buffer.emit_type_name(self.scopes, ut, flags.minify);
            buffer.emit(" ");
            buffer.emit_type_name(self.scopes, ut, flags.minify);
            buffer.emit(";");
            if self.scopes.get(ut.id).attrs.has(ATTR_NOGEN) {
                continue;
            }

            defs.emit(if ut_data.data.is_unsafe_union() {
                "union "
            } else {
                "struct "
            });
            defs.emit_type_name(self.scopes, ut, flags.minify);
            defs.emit("{");

            let members = &self.scopes.get(ut.id).members;
            if let UserTypeData::Union(union) = &ut_data.data {
                defs.emit_type(self.scopes, &union.tag, None, flags.minify);
                defs.emit(format!(" {UNION_TAG_NAME};"));

                for (name, member) in members {
                    Self::emit_member(self.scopes, ut, name, &member.ty, &mut defs, flags.minify);
                }

                defs.emit("union{");
                for (name, (ty, _)) in union.variants.iter() {
                    if let Some(ty) = ty {
                        Self::emit_member(self.scopes, ut, name, ty, &mut defs, flags.minify);
                    }
                }
                defs.emit("};");
            } else {
                if members.is_empty() {
                    defs.emit("CTL_DUMMY_MEMBER;");
                }

                for (name, member) in members {
                    Self::emit_member(self.scopes, ut, name, &member.ty, &mut defs, flags.minify);
                }
            }

            defs.emit("};");

            let ty = Type::User(ut.clone().into());
            if let Some(sizes) = self.arrays.remove(&ty) {
                for size in sizes {
                    Self::emit_array(
                        self.scopes,
                        buffer,
                        Some(&mut defs),
                        &ty,
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
                Self::emit_array(self.scopes, buffer, None, &ty, size, flags.minify);
            }
        }

        buffer.emit(defs.finish());
    }

    fn add_type(&mut self, diag: &mut Diagnostics, ty: &Type, adding: Option<&GenericUserType>) {
        match &ty {
            Type::Ptr(inner) | Type::MutPtr(inner) => self.add_type(diag, inner, adding),
            Type::FnPtr(ptr) => self.add_fnptr(diag, (**ptr).clone()),
            Type::User(ut) => self.add_user_type(diag, (**ut).clone(), adding),
            Type::Array(arr) => self.add_array(diag, arr.0.clone(), arr.1, adding),
            Type::Int(bits) => {
                self.integers.insert((*bits, true));
            }
            Type::Uint(bits) => {
                self.integers.insert((*bits, false));
            }
            Type::DynPtr(ut) | Type::DynMutPtr(ut) => {
                self.add_dynptr(diag, (**ut).clone());
            }
            _ => {}
        }
    }

    fn add_fnptr(&mut self, diag: &mut Diagnostics, f: FnPtr) {
        for param in f.params.iter() {
            self.add_type(diag, param, None);
        }

        self.add_type(diag, &f.ret, None);
        self.fnptrs.insert(f);
    }

    fn add_dynptr(&mut self, _diag: &mut Diagnostics, tr: GenericTrait) {
        self.dynptrs.insert(tr);
    }

    fn add_array(
        &mut self,
        diag: &mut Diagnostics,
        ty: Type,
        len: usize,
        adding: Option<&GenericUserType>,
    ) {
        if let Entry::Occupied(mut entry) = self.arrays.entry(ty.clone()) {
            entry.get_mut().insert(len);
            return;
        }

        self.add_type(diag, &ty, adding);
        self.arrays.insert(ty, [len].into());
    }

    fn add_user_type(
        &mut self,
        diag: &mut Diagnostics,
        ut: GenericUserType,
        adding: Option<&GenericUserType>,
    ) {
        if self.structs.contains_key(&ut) {
            return;
        }

        self.structs.insert(ut.clone(), Vec::new());
        let sty = self.scopes.get(ut.id);

        let mut deps = Vec::new();
        for m in sty.members.values() {
            self.check_member_dep(diag, &ut, &m.ty, adding, &mut deps, sty.name.span);
        }

        if let Some(union) = sty.data.as_union() {
            self.add_type(diag, &union.tag, None);
            for ty in union.variants.values().flat_map(|v| &v.0) {
                self.check_member_dep(diag, &ut, ty, adding, &mut deps, sty.name.span);
            }
        }

        self.structs.insert(ut, deps);
    }

    fn check_member_dep(
        &mut self,
        diag: &mut Diagnostics,
        ut: &GenericUserType,
        ty: &Type,
        adding: Option<&GenericUserType>,
        deps: &mut Vec<GenericUserType>,
        span: Span,
    ) {
        match ty.with_templates(&ut.ty_args) {
            Type::User(dep) => {
                if matches!(adding, Some(adding) if adding == &*dep) {
                    // ideally get the span of the instantiation that caused this
                    diag.error(Error::cyclic(
                        &dep.name(self.scopes),
                        &ut.name(self.scopes),
                        self.scopes.get(dep.id).name.span,
                    ));
                    if !matches!(adding, Some(adding) if adding == ut) {
                        diag.error(Error::cyclic(
                            &ut.name(self.scopes),
                            &dep.name(self.scopes),
                            span,
                        ));
                    }
                    return;
                }

                deps.push((*dep).clone());
                self.add_user_type(diag, *dep, Some(ut));
            }
            Type::Array(arr) => {
                let mut inner = &arr.0;
                while let Type::Array(ty) = inner {
                    inner = &ty.0;
                }

                if let Type::User(data) = inner {
                    deps.push((**data).clone());
                }

                self.add_array(diag, arr.0, arr.1, Some(ut));
            }
            ty => self.add_type(diag, &ty, adding),
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
        ut: &GenericUserType,
        name: &str,
        ty: &Type,
        buffer: &mut Buffer,
        min: bool,
    ) {
        let ty = ty.with_templates(&ut.ty_args);
        if ty.size_and_align(scopes).0 == 0 {
            buffer.emit("CTL_ZST ");
        }

        buffer.emit_type(scopes, &ty, None, min);
        buffer.emit(format!(" {};", member_name(scopes, Some(ut.id), name)));
    }

    fn emit_array(
        scopes: &Scopes,
        typedef: &mut Buffer,
        defs: Option<&mut Buffer>,
        ty: &Type,
        size: usize,
        min: bool,
    ) {
        typedef.emit("typedef struct ");
        typedef.emit_array_struct_name(scopes, ty, size, min);
        typedef.emit(" ");
        typedef.emit_array_struct_name(scopes, ty, size, min);
        typedef.emit(";");

        let defs = defs.unwrap_or(typedef);
        defs.emit("struct ");
        defs.emit_array_struct_name(scopes, ty, size, min);
        defs.emit("{");
        defs.emit_type(scopes, ty, None, min);
        defs.emit(format!(" {ARRAY_DATA_NAME}[{size}];}};"));
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

    fn emit_type(
        &mut self,
        scopes: &Scopes,
        id: &Type,
        tg: Option<(&mut Diagnostics, &mut TypeGen)>,
        min: bool,
    ) {
        match id {
            Type::Void | Type::Never | Type::CVoid => self.emit("$void"),
            Type::Int(bits) | Type::Uint(bits) => {
                if let Some((diag, tg)) = tg {
                    tg.add_type(diag, id, None);
                }
                let ch = if matches!(id, Type::Int(_)) { 's' } else { 'u' };
                self.emit(format!("{ch}{bits}"));
            }
            Type::CInt(ty) | Type::CUint(ty) => {
                if matches!(id, Type::CUint(_)) {
                    self.emit("unsigned ");
                }

                match ty {
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
            Type::Bool => self.emit("CTL_bool"),
            Type::Char => self.emit("CTL_char"),
            Type::Ptr(inner) | Type::MutPtr(inner) | Type::RawPtr(inner) => {
                if let Type::Array(value) = &**inner {
                    self.emit_type(scopes, &value.0, tg, min);
                    // self.emit("/*");
                    // self.emit_type(diag, scopes, inner, tg);
                    // self.emit("*/");
                } else if inner.is_c_void() {
                    self.emit("void");
                } else {
                    self.emit_type(scopes, inner, tg, min);
                }
                if id.is_ptr() {
                    self.emit(" const*");
                } else {
                    self.emit("*");
                }
            }
            Type::FnPtr(f) => {
                self.emit_generic_mangled_name(scopes, id, min);
                if let Some((diag, tg)) = tg {
                    tg.add_fnptr(diag, (**f).clone());
                }
            }
            Type::User(ut) => {
                if scopes.get(ut.id).data.is_template() {
                    eprintln!("ICE: Template type in emit_type");
                    self.emit(&scopes.get(ut.id).name.data);
                    return;
                }

                if let Some(ty) = id.can_omit_tag(scopes) {
                    self.emit_type(scopes, ty, tg, min);
                } else {
                    if let Some((diag, tg)) = tg {
                        tg.add_user_type(diag, (**ut).clone(), None);
                    }
                    self.emit_type_name(scopes, ut, min);
                }
            }
            Type::Array(data) => {
                self.emit_generic_mangled_name(scopes, id, min);
                if let Some((diag, tg)) = tg {
                    tg.add_array(diag, data.0.clone(), data.1, None);
                }
            }
            Type::DynPtr(tr) | Type::DynMutPtr(tr) => {
                self.emit_trait_name(scopes, tr, min);
                if let Some((diag, tg)) = tg {
                    tg.add_dynptr(diag, (**tr).clone());
                }
            }
            Type::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_type"),
        }
    }

    fn emit_generic_mangled_name(&mut self, scopes: &Scopes, id: &Type, min: bool) {
        match id {
            Type::Void => self.emit("void"),
            Type::CVoid => self.emit("c_void"),
            Type::Never => self.emit("never"),
            Type::Int(bits) => self.emit(format!("i{bits}")),
            Type::Uint(bits) => self.emit(format!("u{bits}")),
            Type::CInt(ty) | Type::CUint(ty) => {
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
            Type::Ptr(inner) => {
                self.emit(if min { "p" } else { "ptr_" });
                self.emit_generic_mangled_name(scopes, inner, min);
            }
            Type::MutPtr(inner) => {
                self.emit(if min { "m" } else { "mutptr_" });
                self.emit_generic_mangled_name(scopes, inner, min);
            }
            Type::RawPtr(inner) => {
                self.emit(if min { "r" } else { "rawptr_" });
                self.emit_generic_mangled_name(scopes, inner, min);
            }
            Type::DynPtr(tr) | Type::DynMutPtr(tr) => {
                self.emit_trait_name(scopes, tr, min);
            }
            Type::FnPtr(f) => self.emit_fnptr_name(scopes, f, min),
            Type::User(ut) => {
                self.emit_type_name_ex(scopes, ut, min, true);
            }
            Type::Array(data) => self.emit_array_struct_name(scopes, &data.0, data.1, min),
            Type::Unknown => panic!("ICE: TypeId::Unknown in emit_generic_mangled_name"),
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_generic_mangled_name"),
        }
    }

    fn emit_fnptr_name(&mut self, scopes: &Scopes, f: &FnPtr, min: bool) {
        self.emit("fn");
        self.emit_generic_mangled_name(scopes, &f.ret, min);
        for (i, param) in f.params.iter().enumerate() {
            if i > 0 && !min {
                self.emit("_");
            }
            self.emit_generic_mangled_name(scopes, param, min);
        }
    }

    fn emit_type_name(&mut self, scopes: &Scopes, ut: &GenericUserType, min: bool) {
        self.emit_type_name_ex(scopes, ut, min, false)
    }

    fn emit_type_name_ex(
        &mut self,
        scopes: &Scopes,
        ut: &GenericUserType,
        min: bool,
        generic: bool,
    ) {
        let ty = scopes.get(ut.id);
        if ty.data.is_template() && !cfg!(debug_assertions) {
            eprintln!("ICE: Template type in emit_type");
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
            for ty in ut.ty_args.values() {
                self.emit_generic_mangled_name(scopes, ty, min);
            }
        } else {
            self.emit(scopes.full_name(ty.scope, &ty.name.data));
            if !ut.ty_args.is_empty() {
                self.emit("$");
                for ty in ut.ty_args.values() {
                    self.emit("$");
                    self.emit_generic_mangled_name(scopes, ty, min);
                }
                self.emit("$$");
            }
        }
    }

    fn emit_trait_name(&mut self, scopes: &Scopes, tr: &GenericTrait, min: bool) {
        let ty = scopes.get(tr.id);
        if min {
            self.emit(format!("t{}", tr.id));
            for ty in tr.ty_args.values() {
                self.emit_generic_mangled_name(scopes, ty, min);
            }
        } else {
            self.emit(scopes.full_name(ty.scope, &ty.name.data));
            if !tr.ty_args.is_empty() {
                self.emit("$");
                for ty in tr.ty_args.values() {
                    self.emit("$");
                    self.emit_generic_mangled_name(scopes, ty, min);
                }
                self.emit("$$");
            }
        }
    }

    fn emit_array_struct_name(&mut self, scopes: &Scopes, ty: &Type, size: usize, min: bool) {
        if min {
            self.emit("a");
            self.emit_generic_mangled_name(scopes, ty, min);
            self.emit(format!("{size}"));
        } else {
            self.emit("Array_");
            self.emit_generic_mangled_name(scopes, ty, min);
            self.emit(format!("_{size}"));
        }
    }

    fn emit_fn_name(&mut self, scopes: &Scopes, f: &GenericFunc, min: bool) {
        self.emit_fn_name_ex(false, scopes, f, min)
    }

    fn emit_fn_name_ex(&mut self, real: bool, scopes: &Scopes, func: &GenericFunc, min: bool) {
        let f = scopes.get(func.id);
        let is_macro = f.attrs.has(ATTR_NOGEN);
        if f.linkage == Linkage::Internal {
            if min {
                self.emit(format!("p{}", func.id));
                for ty in func.ty_args.values() {
                    self.emit_generic_mangled_name(scopes, ty, min);
                }
            } else {
                self.emit(scopes.full_name(f.scope, &f.name.data));
                if !func.ty_args.is_empty() {
                    self.emit("$");
                    for ty in func.ty_args.values() {
                        self.emit("$");
                        self.emit_generic_mangled_name(scopes, ty, min);
                    }
                    self.emit("$$");
                }
            }
        } else {
            let name = f.attrs.val(ATTR_LINKNAME).unwrap_or(&f.name.data);
            if !is_macro && !real && needs_fn_wrapper(f) {
                if min {
                    return self.emit(format!("f{}", func.id));
                } else {
                    self.emit("$");
                }
            }
            self.emit(name);
        }
    }

    fn emit_vtable_struct_name(&mut self, scopes: &Scopes, tr: &GenericTrait, min: bool) {
        self.emit(if min { "v" } else { "$vtable_" });
        self.emit_trait_name(scopes, tr, min);
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
    ($self: expr, $state: expr, $body: expr) => {{
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
        hoist!($self, $state, {
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
        hoist!($self, $state, {
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
        hoist!($self, $state, {
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
    ty: Type,
    scope: ScopeId,
}

pub struct Codegen<'a, 'b> {
    scopes: &'a Scopes,
    diag: &'b mut Diagnostics,
    buffer: Buffer,
    temporaries: Buffer,
    type_gen: TypeGen<'a>,
    funcs: HashSet<State>,
    statics: HashSet<VariableId>,
    cur_block: String,
    cur_loop: (ScopeId, String),
    flags: CodegenFlags,
    vtables: Buffer,
    emitted_vtables: HashSet<Vtable>,
    defers: Vec<(ScopeId, Vec<CheckedExpr>)>,
}

impl<'a, 'b> Codegen<'a, 'b> {
    pub fn build(
        diag: &'b mut Diagnostics,
        scope: ScopeId,
        scopes: &'a Scopes,
        flags: CodegenFlags,
    ) -> Result<String, ()> {
        if diag.has_errors() {
            return Err(());
        }

        let exports = scopes
            .functions()
            .filter(|(_, f)| f.linkage == Linkage::Export)
            .map(|(id, _)| State::new(GenericFunc::from_id(scopes, id)));
        let (funcs, main) = if flags.lib {
            (exports.collect(), None)
        } else {
            let Some(main) = scopes[scope].vns.get("main").and_then(|id| id.as_fn()) else {
                diag.error(Error::new("no main function found", Span::default()));
                return Err(());
            };
            let main = State::new(GenericFunc::from_id(scopes, *main));
            (
                exports.chain(std::iter::once(main.clone())).collect(),
                Some(main),
            )
        };
        let mut this = Self {
            scopes,
            diag,
            funcs,
            flags,
            buffer: Default::default(),
            temporaries: Default::default(),
            type_gen: TypeGen::new(scopes),
            cur_block: Default::default(),
            cur_loop: Default::default(),
            vtables: Default::default(),
            statics: Default::default(),
            emitted_vtables: Default::default(),
            defers: Default::default(),
        };
        let main = main.map(|mut main| this.gen_c_main(&mut main));
        let mut static_defs = Buffer::default();
        let mut static_init = Buffer::default();
        let static_state = &mut State::new(GenericFunc::from_id(this.scopes, FunctionId::RESERVED));

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
                usebuf!(this, &mut static_defs, {
                    this.emit_var_decl(id, static_state);
                    this.buffer.emit(";");
                });

                usebuf!(
                    this,
                    &mut static_init,
                    hoist_point!(this, {
                        this.emit_var_name(id, static_state);
                        this.buffer.emit("=");
                        this.emit_expr_inner(
                            this.scopes.get(id).value.clone().unwrap(),
                            static_state,
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
        if this.diag.has_errors() {
            return Err(());
        }
        this.type_gen.finish(&mut this.buffer, &this.flags);
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

        Ok(this.buffer.finish())
    }

    fn emit_vtable(&mut self, vtable: Vtable) {
        if self.emitted_vtables.contains(&vtable) {
            return;
        }

        let mut buffer = Buffer::default();
        usebuf!(self, &mut buffer, {
            self.buffer.emit("static const ");
            self.buffer
                .emit_vtable_struct_name(self.scopes, &vtable.tr, self.flags.minify);
            self.buffer.emit(" ");
            self.emit_vtable_name(&vtable);
            self.buffer.emit("={");
            for tr in self.scopes.get_trait_impls(vtable.tr.id) {
                for f in vtable_methods(self.scopes, self.scopes.get(tr)) {
                    self.buffer.emit(".");

                    let func = GenericFunc::new(f.id, vtable.tr.ty_args.clone());
                    let func_data = self.scopes.get(f.id);

                    self.buffer
                        .emit_fn_name(self.scopes, &func, self.flags.minify);
                    self.buffer.emit("=(");
                    self.buffer.emit_type(
                        self.scopes,
                        &func_data.ret.with_templates(&func.ty_args),
                        None,
                        self.flags.minify,
                    );
                    self.buffer.emit("(*");
                    self.buffer.emit(")(");
                    for (i, param) in func_data.params.iter().enumerate() {
                        if i > 0 {
                            self.buffer.emit(",");
                            self.buffer.emit_type(
                                self.scopes,
                                &param.ty.with_templates(&func.ty_args),
                                None,
                                self.flags.minify,
                            );
                        } else if param.ty.is_ptr() {
                            self.buffer.emit("const void*");
                        } else {
                            self.buffer.emit("void*");
                        }
                    }
                    self.buffer.emit("))");

                    let func = self.find_implementation(
                        &vtable.ty,
                        tr,
                        &self.scopes.get(f.id).name.data,
                        vtable.scope,
                        |id| GenericFunc::new(id, func.ty_args),
                    );
                    self.buffer
                        .emit_fn_name(self.scopes, &func, self.flags.minify);
                    self.buffer.emit(",");
                    self.funcs.insert(State::new(func));
                }
            }
            self.buffer.emit("};");
        });

        self.vtables.emit(buffer.finish());
        self.emitted_vtables.insert(vtable);
    }

    fn gen_c_main(&mut self, main: &mut State) -> String {
        self.buffer.emit("int main(int argc, char **argv){");
        let returns = self.scopes.get(main.func.id).ret != Type::Void;
        if let Some(id) = self
            .scopes
            .lang_fns
            .get("convert_argv")
            .cloned()
            .filter(|_| self.scopes.get(main.func.id).params.len() == 1)
        {
            let state = State::new(GenericFunc::from_id(self.scopes, id));
            if returns {
                self.buffer.emit("return ");
            }
            self.buffer
                .emit_fn_name(self.scopes, &main.func, self.flags.minify);
            self.buffer.emit("(");
            self.buffer
                .emit_fn_name(self.scopes, &state.func, self.flags.minify);
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
            self.buffer
                .emit_fn_name(self.scopes, &main.func, self.flags.minify);
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
        let func = self.scopes.get(state.func.id);
        if func.attrs.has(ATTR_NOGEN) {
            return;
        }

        if needs_fn_wrapper(func) {
            usebuf!(self, prototypes, {
                self.emit_prototype(state, false, false);

                self.buffer.emit("{");
                self.emit_prototype(state, true, true);
                self.buffer.emit(";");
                self.buffer
                    .emit_fn_name_ex(true, self.scopes, &state.func, self.flags.minify);
                self.buffer.emit("(");
                for i in 0..func.params.len() {
                    if i > 0 {
                        self.buffer.emit(",");
                    }
                    self.buffer.emit(format!("$p{i}"));
                }

                self.buffer.emit(");");
                if func.ret.is_never() {
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
        if let Some(body) = func.body.clone() {
            let unused = self.emit_prototype(state, false, false);
            self.buffer.emit("{");
            for id in unused {
                self.buffer.emit("(void)");
                self.emit_var_name(id, state);
                self.buffer.emit(";");
            }

            for param in func.params.iter() {
                let Some(patt) = param
                    .patt
                    .as_checked()
                    .filter(|patt| !matches!(patt.data, CheckedPatternData::Variable(_)))
                else {
                    continue;
                };

                self.emit_pattern_bindings(state, &patt.data, &param.label, &param.ty);
            }

            hoist_point!(self, {
                if !func.ret.is_never() {
                    self.buffer.emit("return ");
                    self.emit_expr_inline(body, state);
                    self.buffer.emit(";}");
                } else {
                    self.emit_expr_stmt(body, state);
                    self.buffer.emit("}");
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
                        if !self.scopes.get(id).unused {
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
                        value.ty.fill_templates(&state.func.ty_args);
                        let ty = value.ty.clone();
                        let tmp = self.emit_tmpvar(value, state);
                        self.emit_pattern_bindings(state, &patt.data, &tmp, &ty);
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
        expr.ty.fill_templates(&state.func.ty_args);
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
            CheckedExprData::Call {
                mut func,
                args,
                scope,
            } => {
                func.fill_templates(&state.func.ty_args);
                if let Some(name) = self.scopes.intrinsic_name(func.id) {
                    return self.emit_intrinsic(name, expr.ty, &func, args, state);
                }

                let next_state = State::new_with_caller(func, scope);
                self.buffer
                    .emit_fn_name(self.scopes, &next_state.func, self.flags.minify);
                self.buffer.emit("(");
                self.finish_emit_fn_args(state, next_state.func.id, args);
                self.funcs.insert(next_state);
            }
            CheckedExprData::MemberCall {
                mut func,
                mut args,
                mut inst,
                trait_id,
                scope,
            } => {
                inst.fill_templates(&state.func.ty_args);
                if let Type::DynPtr(_) | Type::DynMutPtr(_) = &inst {
                    let (_, recv) = args.shift_remove_index(0).unwrap();
                    let recv = hoist!(self, state, self.emit_tmpvar(recv, state));
                    self.buffer.emit(format!("{recv}.vtable->"));
                    self.buffer
                        .emit_fn_name(self.scopes, &func, self.flags.minify);
                    self.buffer.emit(format!("({recv}.self"));
                    if args.is_empty() {
                        self.buffer.emit(")");
                    } else {
                        self.buffer.emit(",");
                        self.finish_emit_fn_args(state, func.id, args);
                    }
                    return;
                }

                let original_id = func.id;
                if let Some(trait_id) = trait_id {
                    func = self.find_implementation(
                        &inst,
                        trait_id,
                        &self.scopes.get(func.id).name.data,
                        state.caller,
                        |id| {
                            GenericFunc::new(
                                id,
                                TypeArgs(
                                    func.ty_args
                                        .0
                                        .into_iter()
                                        .zip(self.scopes.get(id).type_params.iter())
                                        .map(|((_, ty), ut)| (*ut, ty))
                                        .collect(),
                                ),
                            )
                        },
                    );
                }

                func.fill_templates(&state.func.ty_args);
                if let Some(name) = self.scopes.intrinsic_name(func.id) {
                    return self.emit_intrinsic(name, expr.ty, &func, args, state);
                }

                let next_state = State::with_inst_and_scope(func, &inst, scope);
                self.buffer
                    .emit_fn_name(self.scopes, &next_state.func, self.flags.minify);
                self.buffer.emit("(");
                self.finish_emit_fn_args(state, original_id, args);
                self.funcs.insert(next_state);
            }
            CheckedExprData::DynCoerce {
                expr: mut inner,
                scope,
            } => {
                inner.ty.fill_templates(&state.func.ty_args);
                let vtable = if let Type::Ptr(inner) | Type::MutPtr(inner) = &inner.ty {
                    Vtable {
                        tr: expr
                            .ty
                            .as_dyn_pointee()
                            .expect("ICE: DynCoerce to non dyn pointer")
                            .clone(),
                        ty: (**inner).clone(),
                        scope,
                    }
                } else {
                    panic!("ICE: DynCoerce from non-pointer");
                };

                self.emit_cast(&expr.ty);
                self.buffer.emit("{.self=(void*)");
                self.emit_expr(*inner, state);
                self.buffer.emit(",.vtable=&");
                self.emit_vtable_name(&vtable);
                self.buffer.emit("}");
                self.emit_vtable(vtable);
            }
            CheckedExprData::CallFnPtr { expr, args } => {
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
                self.emit_type(&expr.ty);
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
                    self.emit_type(&expr.ty);
                    self.buffer.emit(format!("){{.{ARRAY_DATA_NAME}={{"));
                    for _ in 0..count {
                        self.emit_expr((*init).clone(), state);
                        self.buffer.emit(",");
                    }
                    self.buffer.emit("}}");
                } else {
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(&expr.ty);
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
                let ut = (**expr.ty.as_user().unwrap()).clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    let len = exprs.len();
                    self.emit_with_capacity(&expr.ty, &tmp, &ut, len);
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
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let len = self.emit_tmpvar(*count, state);
                    self.emit_with_capacity(&expr.ty, &tmp, &ut, &len);
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
            CheckedExprData::Set(exprs) => {
                let ut = (**expr.ty.as_user().unwrap()).clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_with_capacity(&expr.ty, &tmp, &ut, exprs.len());
                    let insert = State::with_inst(
                        GenericFunc::from_id(
                            self.scopes,
                            self.scopes
                                .get(ut.id)
                                .find_associated_fn(self.scopes, "insert")
                                .unwrap(),
                        ),
                        &expr.ty,
                    );

                    for val in exprs {
                        self.buffer
                            .emit_fn_name(self.scopes, &insert.func, self.flags.minify);
                        self.buffer.emit(format!("(&{tmp},"));
                        self.emit_expr_inline(val, state);
                        self.buffer.emit(");");
                    }
                    self.funcs.insert(insert);
                });
            }
            CheckedExprData::Map(exprs) => {
                let ut = (**expr.ty.as_user().unwrap()).clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    let insert = State::with_inst(
                        GenericFunc::from_id(
                            self.scopes,
                            self.scopes
                                .get(ut.id)
                                .find_associated_fn(self.scopes, "insert")
                                .unwrap(),
                        ),
                        &expr.ty,
                    );

                    self.emit_with_capacity(&expr.ty, &tmp, &ut, exprs.len());
                    for (key, val) in exprs {
                        self.buffer
                            .emit_fn_name(self.scopes, &insert.func, self.flags.minify);
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
                self.emit_cast(&expr.ty);
                self.buffer.emit(if value { "1" } else { "0" })
            }
            CheckedExprData::Integer(value) => {
                self.emit_cast(&expr.ty);
                if expr.ty.as_integral().is_some_and(|ty| ty.signed) {
                    self.buffer.emit(format!("{value}ll"));
                } else {
                    self.buffer.emit(format!("{value}ull"));
                }
            }
            CheckedExprData::Float(value) => {
                self.emit_cast(&expr.ty);
                self.buffer.emit(value);
            }
            CheckedExprData::String(value) => {
                self.buffer.emit("STRLIT(");
                self.emit_type(&expr.ty);
                self.buffer.emit(",\"");
                for byte in value.as_bytes() {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer.emit(format!("\",{})", value.len()));
            }
            CheckedExprData::ByteString(value) => {
                self.emit_cast(&Type::Ptr(Type::Uint(8).into()));
                self.buffer.emit("\"");
                for byte in value {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer.emit("\"");
            }
            CheckedExprData::Char(value) => {
                self.emit_cast(&expr.ty);
                self.buffer.emit(format!("0x{:x}", value as u32));
            }
            CheckedExprData::Void => self.buffer.emit(VOID_INSTANCE),
            CheckedExprData::Func(func, scope) => {
                let state = State::new_with_caller(func, scope);
                self.buffer
                    .emit_fn_name(self.scopes, &state.func, self.flags.minify);
                self.funcs.insert(state);
            }
            CheckedExprData::Var(id) => {
                if self.scopes.get(id).is_static {
                    self.statics.insert(id);
                }
                self.emit_var_name(id, state);
            }
            CheckedExprData::Instance(members) => {
                self.emit_cast(&expr.ty);
                self.buffer.emit("{");
                let ut_id = expr.ty.as_user().unwrap().id;
                if members.is_empty() {
                    self.buffer.emit("CTL_DUMMY_INIT");
                }

                for (name, mut value) in members {
                    value.ty.fill_templates(&state.func.ty_args);
                    self.buffer.emit(format!(
                        ".{}=",
                        member_name(self.scopes, Some(ut_id), &name)
                    ));
                    self.emit_expr(value, state);
                    self.buffer.emit(",");
                }
                self.buffer.emit("}");
            }
            CheckedExprData::VariantInstance {
                mut members,
                variant,
            } => {
                if expr.ty.can_omit_tag(self.scopes).is_some() {
                    if let Some(some) = members.remove("0") {
                        self.emit_expr(some, state);
                    } else {
                        self.buffer.emit(NULLPTR);
                    }
                } else {
                    self.emit_cast(&expr.ty);
                    self.buffer.emit("{");
                    let ut_id = expr.ty.as_user().unwrap().id;
                    let ut = self.scopes.get(ut_id);
                    let union = ut.data.as_union().unwrap();
                    let members: Vec<_> = members
                        .into_iter()
                        .map(|(name, mut expr)| {
                            expr.ty.fill_templates(&state.func.ty_args);
                            // TODO: dont emit temporaries for expressions that cant have side effects
                            (name, hoist!(self, state, self.emit_tmpvar(expr, state)))
                        })
                        .collect();
                    self.buffer.emit(format!(
                        ".{UNION_TAG_NAME}={},",
                        union.variant_tag(&variant).unwrap()
                    ));

                    for (name, value) in members
                        .iter()
                        .filter(|(name, _)| ut.members.contains_key(name))
                    {
                        self.buffer.emit(format!(
                            ".{}={value},",
                            member_name(self.scopes, Some(ut_id), name)
                        ));
                    }

                    if union.variants.get(&variant).is_some_and(|v| v.0.is_some()) {
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
            CheckedExprData::Member { source, member } => {
                let id = source.ty.as_user().map(|ut| ut.id);
                self.emit_expr(*source, state);
                self.buffer
                    .emit(format!(".{}", member_name(self.scopes, id, &member)));
            }
            CheckedExprData::Block(block) => {
                enter_block!(self, state, &expr.ty, {
                    let scope = block.scope;
                    self.buffer.emit_info("{", self.flags.minify);
                    self.emit_block(block, state);
                    if matches!(self.scopes[scope].kind, ScopeKind::Block(_, false)) {
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
                enter_block!(self, state, &expr.ty, {
                    if let CheckedExprData::Is(mut expr, patt) = cond.data {
                        expr.ty.fill_templates(&state.func.ty_args);
                        let ty = expr.ty.clone();
                        let tmp = self.emit_tmpvar(*expr, state);
                        self.emit_pattern_if_stmt(state, &patt.data, &tmp, &ty);
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
                enter_loop!(self, state, body.scope, &expr.ty, {
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
                            cond.ty.fill_templates(&state.func.ty_args);
                            if let CheckedExprData::Is(cond, patt) = cond.data {
                                let ty = cond.ty.clone();
                                let tmp = self.emit_tmpvar(*cond, state);
                                self.emit_pattern_if_stmt(state, &patt.data, &tmp, &ty);
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
                enter_loop!(self, state, body.scope, &expr.ty, {
                    iter.ty.fill_templates(&state.func.ty_args);
                    let next = self.find_implementation(
                        &iter.ty,
                        self.scopes.lang_traits.get("iter").copied().unwrap(),
                        "next",
                        body.scope,
                        |id| GenericFunc::new(id, Default::default()),
                    );

                    let next_state = State::with_inst_and_scope(next, &iter.ty, body.scope);
                    let mut next_ty = self.scopes.get(next_state.func.id).ret.clone();
                    if let Some(ut) = iter.ty.as_user() {
                        next_ty.fill_templates(&ut.ty_args);
                    }

                    let iter_var = self.emit_tmpvar(*iter, state);
                    self.buffer.emit("for(;;){");

                    let item = state.tmpvar();
                    self.emit_type(&next_ty);
                    self.buffer.emit(format!(" {item}="));
                    self.buffer
                        .emit_fn_name(self.scopes, &next_state.func, self.flags.minify);
                    self.buffer.emit(format!("(&{iter_var});"));
                    let inner = next_ty.as_option_inner(self.scopes).unwrap().clone();
                    self.emit_pattern_if_stmt(
                        state,
                        &CheckedPatternData::UnionMember {
                            pattern: Some(
                                CheckedPattern::new(
                                    true,
                                    CheckedPatternData::Destrucure {
                                        patterns: vec![("0".into(), inner.clone(), patt)],
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
                        &next_ty,
                    );
                    self.emit_block(body, state);
                    self.buffer.emit("}else{");
                    self.emit_loop_break(state, expr.ty, optional);
                    self.buffer.emit("}}");

                    self.funcs.insert(next_state);
                });
            }
            CheckedExprData::Subscript { callee, args } => {
                // TODO: bounds check
                if callee.ty.is_array() {
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
                        "*".repeat(Self::indirection(&callee.ty) - 1)
                    ));
                    self.emit_expr(*callee, state);
                    self.buffer.emit(")");
                }

                self.buffer.emit("[");
                for arg in args {
                    self.emit_expr(arg, state);
                }
                self.buffer.emit("]");
            }
            CheckedExprData::Return(mut expr) => {
                hoist!(self, state, {
                    expr.ty.fill_templates(&state.func.ty_args);
                    let tmp = self.emit_tmpvar(*expr, state);
                    self.leave_scope(
                        state,
                        &format!("return {tmp}"),
                        self.scopes.get(state.func.id).body_scope,
                    );
                });
                self.buffer.emit(VOID_INSTANCE);
            }
            CheckedExprData::Yield(expr) => {
                hoist!(self, state, {
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
                hoist!(self, state, {
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
                hoist!(
                    self,
                    state,
                    self.leave_scope(state, "continue;", self.cur_loop.0)
                );
                self.buffer.emit(VOID_INSTANCE);
            }
            CheckedExprData::Match {
                expr: mut scrutinee,
                body,
            } => {
                enter_block!(self, state, &expr.ty, {
                    scrutinee.ty.fill_templates(&state.func.ty_args);
                    let ty = scrutinee.ty.clone();
                    let tmp = self.emit_tmpvar(*scrutinee, state);
                    for (i, (patt, expr)) in body.into_iter().enumerate() {
                        if i > 0 {
                            self.buffer.emit("else ");
                        }

                        self.emit_pattern_if_stmt(state, &patt.data, &tmp, &ty);

                        hoist_point!(self, {
                            self.buffer.emit(format!("{}=", self.cur_block));
                            self.emit_expr_inline(expr, state);
                            self.buffer.emit(";}");
                        });
                    }

                    self.buffer.emit("else{CTL_UNREACHABLE();}");
                })
            }
            CheckedExprData::As(inner, _) => {
                self.emit_cast(&expr.ty);
                self.buffer.emit("(");
                self.emit_expr(*inner, state);
                self.buffer.emit(")");
            }
            CheckedExprData::Is(mut inner, patt) => {
                inner.ty.fill_templates(&state.func.ty_args);
                let ty = inner.ty.clone();
                let tmp = hoist!(self, state, self.emit_tmpvar(*inner, state));
                let (_, conditions) = self.emit_pattern(state, &patt.data, &tmp, &ty);
                self.buffer.emit(conditions.finish());
            }
            CheckedExprData::Lambda(_) => todo!(),
            CheckedExprData::NeverCoerce(inner) => {
                if matches!(expr.ty, Type::Void | Type::CVoid) {
                    self.emit_expr_inline(*inner, state);
                } else {
                    self.buffer.emit("/*never*/((");
                    self.emit_expr_inline(*inner, state);
                    self.buffer.emit("),*(");
                    self.emit_type(&expr.ty);
                    self.buffer.emit(format!("*){NULLPTR})"));
                }
            }
            CheckedExprData::StringInterpolation {
                mut formatter,
                parts,
                scope,
            } => {
                formatter.ty.fill_templates(&state.func.ty_args);
                let formatter_ty = formatter.ty.clone();
                let formatter = hoist!(self, state, {
                    let format_id = self.scopes.lang_traits.get("format").copied().unwrap();
                    let formatter = self.emit_tmpvar(*formatter, state);
                    for mut expr in parts {
                        expr.ty.fill_templates(&state.func.ty_args);
                        let format_state = State::with_inst_and_scope(
                            self.find_implementation(&expr.ty, format_id, "format", scope, |id| {
                                GenericFunc::from_type_args(self.scopes, id, [formatter_ty.clone()])
                            }),
                            &expr.ty,
                            scope,
                        );

                        self.buffer.emit_fn_name(
                            self.scopes,
                            &format_state.func,
                            self.flags.minify,
                        );
                        self.buffer.emit("(&");
                        self.emit_tmpvar_ident(expr, state);
                        self.buffer.emit(format!(",&{formatter});"));
                        self.funcs.insert(format_state);
                    }
                    formatter
                });
                let formatter_id = self.scopes.lang_traits.get("formatter").copied().unwrap();
                let finish_state = State::with_inst_and_scope(
                    self.find_implementation(&formatter_ty, formatter_id, "written", scope, |id| {
                        GenericFunc::new(id, Default::default())
                    }),
                    &formatter_ty,
                    scope,
                );
                self.buffer
                    .emit_fn_name(self.scopes, &finish_state.func, self.flags.minify);
                self.buffer.emit(format!("(&{formatter})"));
                self.funcs.insert(finish_state);
            }
            CheckedExprData::Error => panic!("ICE: ExprData::Error in gen_expr"),
        }
    }

    #[inline(always)]
    fn emit_expr_inline(&mut self, mut expr: CheckedExpr, state: &mut State) {
        expr.ty.fill_templates(&state.func.ty_args);
        self.emit_expr_inner(expr, state);
    }

    fn emit_binary(
        &mut self,
        state: &mut State,
        op: BinaryOp,
        ret: Type,
        mut lhs: CheckedExpr,
        rhs: CheckedExpr,
    ) {
        match op {
            BinaryOp::NoneCoalesceAssign => {
                lhs.ty.fill_templates(&state.func.ty_args);
                let opt_type = lhs.ty.clone();
                let tag = if opt_type.can_omit_tag(self.scopes).is_some() {
                    ""
                } else {
                    ".$Some.$0"
                };
                let mut left = Buffer::default();
                usebuf!(self, &mut left, self.emit_expr_inner(lhs, state));
                let left = left.finish();

                hoist!(self, state, {
                    self.emit_pattern_if_stmt(
                        state,
                        &CheckedPatternData::UnionMember {
                            pattern: None,
                            variant: "None".into(),
                            inner: ret,
                            borrows: false,
                        },
                        &left,
                        &opt_type,
                    );

                    hoist_point!(self, {
                        self.buffer.emit(format!("{left}{tag}="));
                        self.emit_expr_inline(rhs, state);
                        if opt_type.can_omit_tag(self.scopes).is_none() {
                            self.buffer.emit(";");
                            let union = self
                                .scopes
                                .get(opt_type.as_user().unwrap().id)
                                .data
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

                self.buffer.emit(format!("{left}{tag}"));
            }
            BinaryOp::NoneCoalesce => {
                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_type(&ret);
                    self.buffer.emit(format!(" {tmp};"));

                    lhs.ty.fill_templates(&state.func.ty_args);
                    let opt_type = lhs.ty.clone();
                    let name = hoist!(self, state, self.emit_tmpvar(lhs, state));
                    self.emit_pattern_if_stmt(
                        state,
                        &CheckedPatternData::UnionMember {
                            pattern: None,
                            variant: "None".into(),
                            inner: ret,
                            borrows: false,
                        },
                        &name,
                        &opt_type,
                    );
                    hoist_point!(self, {
                        self.buffer.emit(format!("{tmp}="));
                        self.emit_expr_inline(rhs, state);
                    });
                    let tag = if opt_type.can_omit_tag(self.scopes).is_some() {
                        ""
                    } else {
                        ".$Some.$0"
                    };
                    self.buffer.emit(format!(";}}else{{{tmp}={name}{tag};}}"));
                });
            }
            BinaryOp::Cmp => {
                let tmp = tmpbuf!(self, state, |tmp| {
                    if matches!(lhs.ty, Type::RawPtr(_)) {
                        self.emit_type(&Type::Isize);
                    } else if let Some(int) = lhs.ty.as_integral() {
                        self.emit_type(&Type::Int(int.bits + int.signed as u32));
                    } else {
                        self.emit_type(&lhs.ty);
                    }
                    self.buffer.emit(format!(" {tmp}="));
                    self.emit_expr(lhs, state);
                    self.buffer.emit("-");
                    self.emit_expr(rhs, state);
                    self.buffer.emit(";");

                    tmp
                });

                let union = self
                    .scopes
                    .get(ret.as_user().unwrap().id)
                    .data
                    .as_union()
                    .unwrap();

                self.buffer.emit(format!("({tmp}<0?"));
                self.emit_cast(&ret);
                self.buffer.emit(format!(
                    "{{.{UNION_TAG_NAME}={}}}:({tmp}>0?",
                    union.variant_tag("Less").unwrap()
                ));
                self.emit_cast(&ret);
                self.buffer.emit(format!(
                    "{{.{UNION_TAG_NAME}={}}}:",
                    union.variant_tag("Greater").unwrap()
                ));
                self.emit_cast(&ret);
                self.buffer.emit(format!(
                    "{{.{UNION_TAG_NAME}={}}}))",
                    union.variant_tag("Equal").unwrap()
                ));
            }
            _ => {
                lhs.ty.fill_templates(&state.func.ty_args);
                let b = matches!(ret, Type::Bool) && !lhs.ty.is_bool();
                if b {
                    self.emit_cast(&ret);
                    self.buffer.emit("(");
                }
                self.buffer.emit("(");
                self.emit_expr(lhs, state);
                self.buffer.emit(format!("{op}"));
                self.emit_expr(rhs, state);
                self.buffer.emit(")");
                if b {
                    self.buffer.emit("?1:0)");
                }
            }
        }
    }

    fn emit_unary(&mut self, state: &mut State, op: UnaryOp, ret: Type, mut lhs: CheckedExpr) {
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
                if lhs.ty == Type::Bool {
                    self.emit_cast(&ret);
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
                lhs.ty.fill_templates(&state.func.ty_args);

                let array = lhs.ty.is_array();
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
                    lhs.ty.fill_templates(&state.func.ty_args);

                    self.emit_type(&ret);
                    self.buffer.emit(format!(" {tmp};"));

                    let inner_ty = lhs.ty.clone();
                    let inner_tmp = self.emit_tmpvar(lhs, state);
                    self.emit_pattern_if_stmt(
                        state,
                        &CheckedPatternData::UnionMember {
                            pattern: None,
                            variant: "None".into(),
                            inner: ret.clone(),
                            borrows: false,
                        },
                        &inner_tmp,
                        &inner_ty,
                    );
                    hoist_point!(self, {
                        let mut buffer = Buffer::default();
                        usebuf!(self, &mut buffer, {
                            self.buffer.emit("return ");
                            let mut ret_type = self.scopes.get(state.func.id).ret.clone();
                            ret_type.fill_templates(&state.func.ty_args);
                            self.emit_expr_inner(CheckedExpr::option_null(ret_type), state);
                            self.buffer.emit(";");
                        });
                        self.leave_scope(
                            state,
                            &buffer.finish(),
                            self.scopes.get(state.func.id).body_scope,
                        );
                    });
                    self.buffer.emit(format!("}}{tmp}="));
                    if inner_ty.can_omit_tag(self.scopes).is_some() {
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
                expr.ty.fill_templates(&state.func.ty_args);
                // TODO: dont emit temporaries for expressions that cant have side effects
                (name, hoist!(self, state, self.emit_tmpvar(expr, state)))
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

        self.scopes
            .get(original_id)
            .params
            .iter()
            .flat_map(|param| args.shift_remove(&param.label))
            .for_each(|arg| arg!(arg));
        args.into_iter().for_each(|(_, arg)| arg!(arg));
        self.buffer.emit(")");
    }

    fn emit_loop_break(&mut self, state: &mut State, ty: Type, optional: bool) {
        if optional {
            self.buffer.emit(format!("{}=", self.cur_loop.1));
            self.emit_expr_inline(CheckedExpr::option_null(ty), state);
            self.buffer.emit(";");
        } else {
            self.buffer
                .emit(format!("{}={VOID_INSTANCE};", self.cur_loop.1));
        }
        self.leave_scope(state, "break", self.cur_loop.0);
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
        let new_state = State::new(GenericFunc::new(
            self.scopes
                .get(ut.id)
                .find_associated_fn(self.scopes, "new")
                .unwrap(),
            ut.ty_args.clone(),
        ));

        self.buffer
            .emit_fn_name(self.scopes, &new_state.func, self.flags.minify);
        self.buffer.emit("()");
        self.funcs.insert(new_state);
    }

    fn emit_with_capacity(
        &mut self,
        ty: &Type,
        tmp: &str,
        ut: &GenericUserType,
        len: impl std::fmt::Display,
    ) {
        self.emit_type(ty);
        self.buffer.emit(format!(" {tmp}="));
        let state = State::new(GenericFunc::new(
            self.scopes
                .get(ut.id)
                .find_associated_fn(self.scopes, "with_capacity")
                .unwrap(),
            ut.ty_args.clone(),
        ));

        self.buffer
            .emit_fn_name(self.scopes, &state.func, self.flags.minify);
        self.buffer.emit(format!("({len});"));
        self.funcs.insert(state);
    }

    fn emit_intrinsic(
        &mut self,
        name: &str,
        ret: Type,
        func: &GenericFunc,
        mut args: IndexMap<String, CheckedExpr>,
        state: &mut State,
    ) {
        match name {
            "numeric_abs" => {
                let (_, mut expr) = args.shift_remove_index(0).unwrap();
                expr.ty.fill_templates(&state.func.ty_args);
                let tmp = hoist!(self, state, self.emit_tmpvar(expr, state));
                self.buffer.emit(format!("({tmp}<0?-{tmp}:{tmp})"));
            }
            "numeric_cast" => {
                let (_, expr) = args.shift_remove_index(0).unwrap();
                self.emit_cast(&self.scopes.get(func.id).ret.with_templates(&func.ty_args));
                self.emit_expr(expr, state);
            }
            "size_of" => {
                self.buffer.emit(format!(
                    "(usize){}",
                    func.first_type_arg().unwrap().size_and_align(self.scopes).0
                ));
            }
            "align_of" => {
                self.buffer.emit(format!(
                    "(usize){}",
                    func.first_type_arg().unwrap().size_and_align(self.scopes).1
                ));
            }
            "panic" => {
                let panic = State::new(GenericFunc::from_id(
                    self.scopes,
                    self.scopes
                        .lang_fns
                        .get("panic_handler")
                        .cloned()
                        .expect("a panic handler should exist"),
                ));

                self.buffer
                    .emit_fn_name(self.scopes, &panic.func, self.flags.minify);
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
                let op = &self.scopes.get(func.id).name.data[..];
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
                        "and" => BinaryOp::And,
                        "or" => BinaryOp::Or,
                        "xor" => BinaryOp::Xor,
                        "shl" => BinaryOp::Shl,
                        "shr" => BinaryOp::Shr,
                        _ => panic!("ICE: call to unsupported binary operator '{op}'"),
                    },
                    ret,
                    args.next()
                        .expect("ICE: binary operator should receive two arguments")
                        .1
                        .auto_deref(&Type::Unknown),
                    args.next()
                        .expect("ICE: binary operator should receive two arguments")
                        .1
                        .auto_deref(&Type::Unknown),
                );
            }
            "unary_op" => {
                let op = &self.scopes.get(func.id).name.data[..];
                self.emit_unary(
                    state,
                    match op {
                        "neg" => UnaryOp::Neg,
                        "not" => UnaryOp::Not,
                        "post_inc" => UnaryOp::PostIncrement,
                        "post_dec" => UnaryOp::PostDecrement,
                        "pre_inc" => UnaryOp::PreIncrement,
                        "pre_dec" => UnaryOp::PreDecrement,
                        _ => panic!("ICE: call to unsupported binary operator '{op}'"),
                    },
                    ret,
                    args.into_iter()
                        .next()
                        .expect("ICE: unary operator should receive one argument")
                        .1
                        .auto_deref(&Type::Unknown),
                );
            }
            _ => unreachable!(),
        }
    }

    fn emit_tmpvar_ident(&mut self, expr: CheckedExpr, state: &mut State) {
        tmpbuf_emit!(self, state, |tmp| {
            self.emit_type(&expr.ty);
            self.buffer.emit(format!(" {tmp}="));
            self.emit_expr_inner(expr, state);
            self.buffer.emit(";");
        });
    }

    fn emit_tmpvar(&mut self, expr: CheckedExpr, state: &mut State) -> String {
        let tmp = state.tmpvar();
        self.emit_type(&expr.ty);
        self.buffer.emit(format!(" {tmp}="));
        self.emit_expr_inner(expr, state);
        self.buffer.emit(";");
        tmp
    }

    fn emit_vtable_name(&mut self, vtable: &Vtable) {
        self.buffer
            .emit_generic_mangled_name(self.scopes, &vtable.ty, self.flags.minify);
        if !self.flags.minify {
            self.buffer.emit("_");
        }
        self.buffer
            .emit_trait_name(self.scopes, &vtable.tr, self.flags.minify);
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
        ty: &Type,
        borrow: bool,
        bindings: &mut Buffer,
        conditions: &mut ConditionBuilder,
    ) {
        match pattern {
            CheckedPatternData::Int(value) => {
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        self.buffer.emit(format!("{}==", Self::deref(src, ty)));
                        self.emit_cast(ty.strip_references());
                        self.buffer.emit(format!("{value}"));
                    });
                });
            }
            CheckedPatternData::IntRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                let src = Self::deref(src, ty);
                let base = ty.strip_references();
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
                            Self::deref(src, ty),
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
                let src = Self::deref(src, ty);
                let base = ty.strip_references();
                if base.can_omit_tag(self.scopes).is_some() {
                    if variant == "Some" {
                        conditions.next_str(format!("{src}!={NULLPTR}"));
                        if let Some((patt, borrows)) =
                            patt.as_ref().and_then(|patt| patt.data.as_destrucure())
                        {
                            self.emit_pattern_inner(
                                state,
                                &patt[0].2.data,
                                &src,
                                &patt[0].1,
                                borrow || *borrows,
                                bindings,
                                conditions,
                            );
                        }
                    } else {
                        conditions.next_str(format!("{src}=={NULLPTR}"));
                    }
                } else {
                    let tag = base
                        .as_user()
                        .and_then(|ut| self.scopes.get(ut.id).data.as_union())
                        .and_then(|union| union.variant_tag(variant))
                        .unwrap();
                    conditions.next_str(format!("{src}.{UNION_TAG_NAME}=={tag}"));

                    if let Some(patt) = patt {
                        self.emit_pattern_inner(
                            state,
                            &patt.data,
                            &format!("{src}.${variant}"),
                            inner,
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
                let src = Self::deref(src, ty);
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
                    if let Some(id) = id.filter(|&id| !self.scopes.get(id).unused) {
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
                        inner,
                        true,
                        bindings,
                        conditions,
                    );
                }
            }
            CheckedPatternData::Destrucure { patterns, borrows } => {
                let src = Self::deref(src, ty);
                let ut_id = ty.strip_references().as_user().map(|ut| ut.id);
                for (member, inner, patt) in patterns {
                    self.emit_pattern_inner(
                        state,
                        &patt.data,
                        &format!("{src}.{}", member_name(self.scopes, ut_id, member)),
                        inner,
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
                let src = Self::deref(src, ty);
                let src = if ty.is_any_ptr() {
                    src
                } else {
                    format!("{src}.{ARRAY_DATA_NAME}")
                };
                let rest = rest.map(|RestPattern { id, pos }| {
                    let rest_len = arr_len - patterns.len();
                    if let Some(id) = id.filter(|&id| !self.scopes.get(id).unused) {
                        usebuf!(self, bindings, {
                            self.emit_var_decl(id, state);
                            if ty.is_any_ptr() {
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
                        inner,
                        borrow || *borrows,
                        bindings,
                        conditions,
                    );
                }
            }
            &CheckedPatternData::Variable(id) => {
                if self.scopes.get(id).unused {
                    return;
                }

                usebuf!(self, bindings, {
                    let ty = self.emit_var_decl(id, state);
                    if borrow
                        && matches!(ty, Type::Ptr(i) | Type::MutPtr(i) | Type::RawPtr(i) if i.is_array())
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
        ty: &Type,
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
        ty: &Type,
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
        ty: &Type,
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

    fn emit_type(&mut self, id: &Type) {
        self.buffer.emit_type(
            self.scopes,
            id,
            Some((&mut self.diag, &mut self.type_gen)),
            self.flags.minify,
        );
    }

    fn emit_cast(&mut self, id: &Type) {
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
        let f = self.scopes.get(state.func.id);
        let mut ret = f.ret.clone();
        ret.fill_templates(&state.func.ty_args);

        let needs_wrapper = needs_fn_wrapper(f);
        if f.linkage == Linkage::Internal || (needs_wrapper && !real) {
            self.buffer.emit("static ");
            if needs_wrapper {
                self.buffer.emit("CTL_FORCEINLINE ");
            }
        } else {
            self.buffer.emit("extern ");
        }

        if ret.is_never() && (!needs_wrapper || real) {
            self.buffer.emit("CTL_NORETURN ");
        }

        if real && needs_wrapper {
            self.buffer.emit("void ");
        } else {
            self.emit_type(&ret);
            self.buffer.emit(" ");
        }
        self.buffer
            .emit_fn_name_ex(real, self.scopes, &state.func, self.flags.minify);
        self.buffer.emit("(");

        let mut unused = vec![];
        let mut nonnull = vec![];
        for (i, param) in f.params.iter().enumerate() {
            let mut ty = param.ty.clone();
            ty.fill_templates(&state.func.ty_args);
            if i > 0 {
                self.buffer.emit(",");
            }

            if ty.is_any_ptr() && is_prototype {
                nonnull.push(format!("{}", i + 1));
            }

            if !is_prototype && needs_wrapper {
                self.emit_type(&ty);
                self.buffer.emit(format!(" $p{i}"));
            } else if f.linkage == Linkage::Import || is_prototype {
                self.emit_type(&ty);
            } else if let ParamPattern::Checked(CheckedPattern {
                data: CheckedPatternData::Variable(id),
                ..
            }) = &param.patt
            {
                _ = self.emit_var_decl(*id, state);
                if self.scopes.get(*id).unused {
                    unused.push(*id);
                }
                continue;
            } else {
                self.emit_type(&ty);
                self.buffer.emit(format!(" {}", param.label));
            }
        }

        if f.variadic {
            if f.params.is_empty() {
                self.buffer.emit("...)");
            } else {
                self.buffer.emit(",...)");
            }
        } else if f.params.is_empty() {
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

        let var = self.scopes.get(id);
        if var.is_static {
            self.buffer
                .emit(self.scopes.full_name(var.scope, &var.name.data));
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

    fn emit_var_decl(&mut self, id: VariableId, state: &mut State) -> Type {
        let var = self.scopes.get(id);
        let mut ty = var.ty.clone();
        ty.fill_templates(&state.func.ty_args);
        if var.is_static {
            self.buffer.emit("static ");
        }

        self.emit_type(&ty);
        if !var.mutable && !var.is_static {
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
            | CheckedExprData::MemberCall { .. } => true,
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
            } | CheckedExprData::Var(_)
                | CheckedExprData::Member { .. }
                | CheckedExprData::Subscript { .. }
        )
    }

    fn find_implementation(
        &self,
        this: &Type,
        trait_id: TraitId,
        method: &str,
        scope: ScopeId,
        finish: impl FnOnce(FunctionId) -> GenericFunc,
    ) -> GenericFunc {
        // FIXME: in the very specific case where two extensions exist for the same type that
        // implement the same trait, and they are both used to call the trait in two different
        // places, this function will pick the wrong one for one of them. either fix this lookup or
        // reject duplicate trait implementation in extensions
        let search = |impls: &[TraitImpl]| {
            impls
                .iter()
                .flat_map(|imp| imp.as_checked())
                .find_map(|(imp, scope)| scope.filter(|_| imp.id == trait_id))
                .and_then(|id| self.scopes[id].find_in_vns(method))
                .and_then(|f| f.as_fn().copied())
        };

        if let Some(id) = this
            .as_user()
            .and_then(|ut| search(&self.scopes.get(ut.id).impls))
        {
            return finish(id);
        } else if let Some((id, ext)) = self
            .scopes
            .extensions_in_scope_for(this, scope)
            .iter()
            .find_map(|ext| search(&self.scopes.get(ext.id).impls).zip(Some(ext)))
        {
            let mut f = finish(id);
            f.ty_args.copy_args(&ext.ty_args);
            return f;
        }

        let default = self
            .scopes
            .get(trait_id)
            .fns
            .iter()
            .find(|f| self.scopes.get(f.id).name.data == method)
            .filter(|f| self.scopes.get(f.id).body.is_some())
            .unwrap_or_else(|| {
                eprintln!("searching from scope: {}", self.scopes.full_name(scope, ""));
                panic!(
                    "cannot find implementation for method '{}::{method}' for type '{}'",
                    self.scopes.get(trait_id).name.data,
                    this.name(self.scopes)
                )
            });
        let mut f = finish(default.id);
        f.ty_args
            .insert(self.scopes.get(trait_id).this, this.clone());
        f
    }

    fn deref(src: &str, ty: &Type) -> String {
        if matches!(ty, Type::Ptr(_) | Type::MutPtr(_)) {
            format!(
                "({:*<1$}{src})",
                "",
                Self::indirection(ty) - usize::from(ty.strip_references().is_array())
            )
        } else {
            src.into()
        }
    }

    fn indirection(mut id: &Type) -> usize {
        let mut count = 0;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = id {
            id = inner;
            count += 1;
        }
        count
    }
}

fn needs_fn_wrapper(f: &Function) -> bool {
    f.linkage == Linkage::Import && matches!(f.ret, Type::Void | Type::Never | Type::CVoid)
}

fn vtable_methods<'a>(
    scopes: &'a Scopes,
    tr: &'a Trait,
) -> impl Iterator<Item = &'a Vis<FunctionId>> {
    tr.fns.iter().filter(|f| {
        let f = scopes.get(f.id);
        f.type_params.is_empty()
            && f.params.first().is_some_and(|p| p.label == THIS_PARAM)
            && f.params
                .iter()
                .all(|p| !p.ty.as_user().is_some_and(|ty| ty.id == tr.this))
    })
}

fn member_name(scopes: &Scopes, id: Option<UserTypeId>, name: &str) -> String {
    if id.is_some_and(|id| scopes.get(id).attrs.has(ATTR_NOGEN)) {
        name.into()
    } else {
        format!("${name}")
    }
}
