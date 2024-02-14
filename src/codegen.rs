use std::collections::{hash_map::Entry, HashMap, HashSet};

use indexmap::IndexMap;

use crate::{
    ast::{
        checked::*,
        parsed::{Linkage, RangePattern},
        UnaryOp,
    },
    error::{Diagnostics, Error},
    lexer::Span,
    nearest_pow_of_two,
    sym::*,
    typeid::{CInt, FnPtr, GenericFunc, GenericUserType, Type, TypeArgs},
    CodegenFlags,
};

const UNION_TAG_NAME: &str = "tag";
const ARRAY_DATA_NAME: &str = "data";
const VOID_INSTANCE: &str = "CTL_VOID";
const ATTR_NOGEN: &str = "c_opaque";
const ATTR_LINKNAME: &str = "c_name";

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

    pub fn with_instance(func: GenericFunc, inst: Option<&Type>) -> Self {
        Self::with_instance_and_caller(func, inst, ScopeId::ROOT)
    }

    pub fn with_instance_and_caller(
        mut func: GenericFunc,
        inst: Option<&Type>,
        caller: ScopeId,
    ) -> Self {
        if let Some(ut) = inst.and_then(|inst| inst.as_user()) {
            func.ty_args.copy_args(&ut.ty_args);
        }

        Self::new_with_caller(func, caller)
    }

    pub fn fill_generics(&self, ty: &mut Type) {
        ty.fill_templates(&self.func.ty_args);
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
        }
    }

    fn finish(mut self, buffer: &mut Buffer, flags: &CodegenFlags) {
        let integers = std::mem::take(&mut self.integers);
        let structs = std::mem::take(&mut self.structs);
        let fnptrs = std::mem::take(&mut self.fnptrs);
        let mut arrays = std::mem::take(&mut self.arrays);
        let mut definitions = Buffer::default();
        for (bits, signed) in integers {
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
                    "typedef {}INT({bits}) {ch}{bits};",
                    ch.to_uppercase(),
                ));
            }
        }
        for f in fnptrs {
            definitions.emit("typedef ");
            definitions.emit_type(self.scopes, &f.ret, None, flags.minify);
            definitions.emit("(*");
            definitions.emit_fnptr_name(self.scopes, &f, flags.minify);
            definitions.emit(")(");
            for (i, param) in f.params.iter().enumerate() {
                if i > 0 {
                    definitions.emit(",");
                }

                definitions.emit_type(self.scopes, param, None, flags.minify);
            }
            definitions.emit(");");
        }

        let mut emitted_arrays = HashSet::new();
        for ut in self.get_struct_order(&structs) {
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
            if self
                .scopes
                .get(ut.id)
                .attrs
                .iter()
                .any(|attr| attr.name.data == ATTR_NOGEN)
            {
                continue;
            }

            definitions.emit(if ut_data.data.is_unsafe_union() {
                "union "
            } else {
                "struct "
            });
            definitions.emit_type_name(self.scopes, ut, flags.minify);
            definitions.emit("{");

            let members = &self.scopes.get(ut.id).members;
            if let UserTypeData::Union(union) = &ut_data.data {
                definitions.emit_type(self.scopes, &union.tag_type(), None, flags.minify);
                definitions.emit(format!(" {UNION_TAG_NAME};"));

                for (name, member) in members {
                    self.emit_member(ut, name, &member.ty, &mut definitions, flags.minify);
                }

                definitions.emit(" union{");
                for (name, ty) in union.iter() {
                    if let Some(ty) = ty {
                        self.emit_member(ut, name, ty, &mut definitions, flags.minify);
                    }
                }
                definitions.emit("};");
            } else {
                if members.is_empty() {
                    definitions.emit("CTL_DUMMY_MEMBER;");
                }

                for (name, member) in members {
                    self.emit_member(ut, name, &member.ty, &mut definitions, flags.minify);
                }
            }

            definitions.emit("};");

            let ty = Type::User(ut.clone().into());
            if let Some(sizes) = arrays.remove(&ty) {
                for size in sizes {
                    self.emit_array(buffer, Some(&mut definitions), &ty, size, flags.minify);
                }
                emitted_arrays.insert(ty);
            }
        }

        for (ty, sizes) in arrays
            .into_iter()
            .filter(|(ty, _)| !emitted_arrays.contains(ty))
        {
            for size in sizes {
                self.emit_array(buffer, None, &ty, size, flags.minify);
            }
        }

        buffer.emit(definitions.finish());
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
            self.add_type(diag, &union.tag_type(), None);
            for ty in union.values().flatten() {
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

    fn get_struct_order<'b>(
        &self,
        structs: &'b HashMap<GenericUserType, Vec<GenericUserType>>,
    ) -> Vec<&'b GenericUserType> {
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
        &mut self,
        ut: &GenericUserType,
        name: &str,
        ty: &Type,
        buffer: &mut Buffer,
        min: bool,
    ) {
        let ty = ty.with_templates(&ut.ty_args);
        if ty.size_and_align(self.scopes).0 == 0 {
            buffer.emit("CTL_ZST ");
        }

        buffer.emit_type(self.scopes, &ty, None, min);
        buffer.emit(format!(" ${name};"));
    }

    fn emit_array(
        &mut self,
        typedef: &mut Buffer,
        defs: Option<&mut Buffer>,
        ty: &Type,
        size: usize,
        min: bool,
    ) {
        typedef.emit("typedef struct ");
        typedef.emit_array_struct_name(self.scopes, ty, size, min);
        typedef.emit(" ");
        typedef.emit_array_struct_name(self.scopes, ty, size, min);
        typedef.emit(";");

        let defs = defs.unwrap_or(typedef);
        defs.emit("struct ");
        defs.emit_array_struct_name(self.scopes, ty, size, min);
        defs.emit("{");
        defs.emit_type(self.scopes, ty, None, min);
        defs.emit(format!(" {ARRAY_DATA_NAME}[{size}];}};"));
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Buffer(String);

impl Buffer {
    fn emit(&mut self, source: impl AsRef<str>) {
        self.0.push_str(source.as_ref());
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
            Type::Ptr(inner) | Type::MutPtr(inner) => {
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
                    self.emit(" *");
                }
            }
            Type::FnPtr(f) => {
                self.emit_generic_mangled_name(scopes, id, min);
                if let Some((diag, tg)) = tg {
                    tg.add_fnptr(diag, (**f).clone());
                }
            }
            Type::User(ut) => match &scopes.get(ut.id).data {
                UserTypeData::Template => {
                    if !cfg!(debug_assertions) {
                        panic!("ICE: Template type in emit_type")
                    }

                    self.emit(&scopes.get(ut.id).name.data);
                }
                UserTypeData::Trait => panic!("ICE: Trait type in emit_type"),
                _ => {
                    if let Some(ty) = id.can_omit_tag(scopes) {
                        self.emit_type(scopes, ty, tg, min);
                    } else {
                        if let Some((diag, tg)) = tg {
                            tg.add_user_type(diag, (**ut).clone(), None);
                        }
                        self.emit_type_name(scopes, ut, min);
                    }
                }
            },
            Type::Array(data) => {
                self.emit_generic_mangled_name(scopes, id, min);
                if let Some((diag, tg)) = tg {
                    tg.add_array(diag, data.0.clone(), data.1, None);
                }
            }
            Type::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_type"),
            Type::TraitSelf => panic!("ICE: TypeId::TraitSelf in emit_type"),
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
                self.emit("ptr_");
                self.emit_generic_mangled_name(scopes, inner, min);
            }
            Type::MutPtr(inner) => {
                self.emit("mutptr_");
                self.emit_generic_mangled_name(scopes, inner, min);
            }
            Type::FnPtr(f) => self.emit_fnptr_name(scopes, f, min),
            Type::User(ut) => {
                self.emit_type_name(scopes, ut, min);
            }
            Type::Array(data) => self.emit_array_struct_name(scopes, &data.0, data.1, min),
            Type::Unknown => panic!("ICE: TypeId::Unknown in emit_generic_mangled_name"),
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_generic_mangled_name"),
            Type::TraitSelf => panic!("ICE: TypeId::TraitSelf in emit_generic_mangled_name"),
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
        let ty = scopes.get(ut.id);
        if ty.data.is_template() && !cfg!(debug_assertions) {
            panic!("ICE: Template type in emit_type_name")
        }

        if let Some(name) = scopes
            .get(ut.id)
            .attrs
            .iter()
            .find(|attr| attr.name.data == ATTR_LINKNAME && !attr.props.is_empty())
            .map(|attr| &attr.props[0].name.data)
        {
            self.emit(name);
        } else if min {
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
        if !self.0 .0.is_empty() {
            self.0.emit("&&");
        }
        self.0.emit(s);
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
    ($self: expr, $state: expr, $ty: expr, $body: expr) => {{
        let ty = $ty;
        let cur_loop = $state.tmpvar();
        let old_loop = std::mem::replace(&mut $self.cur_loop, cur_loop.clone());
        let old_block = std::mem::replace(&mut $self.cur_block, cur_loop);
        hoist!($self, $state, {
            $self.emit_type(ty);
            $self.buffer.emit(format!(" {};", $self.cur_loop));
            $body;
        });

        $self.cur_block = old_block;
        $self
            .buffer
            .emit(std::mem::replace(&mut $self.cur_loop, old_loop));
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
        $body
        std::mem::swap(&mut $self.buffer, $buf);
    };
}

pub struct Codegen<'a> {
    scopes: &'a Scopes,
    diag: Diagnostics,
    buffer: Buffer,
    temporaries: Buffer,
    type_gen: TypeGen<'a>,
    funcs: HashSet<State>,
    cur_block: String,
    cur_loop: String,
    yielded: bool,
    flags: CodegenFlags,
}

impl<'a> Codegen<'a> {
    fn new(
        scopes: &'a Scopes,
        diag: Diagnostics,
        funcs: HashSet<State>,
        flags: CodegenFlags,
    ) -> Self {
        Self {
            scopes,
            diag,
            funcs,
            flags,
            buffer: Default::default(),
            temporaries: Default::default(),
            type_gen: TypeGen::new(scopes),
            cur_block: Default::default(),
            cur_loop: Default::default(),
            yielded: Default::default(),
        }
    }

    pub fn build(
        mut diag: Diagnostics,
        scope: ScopeId,
        scopes: &'a Scopes,
        flags: CodegenFlags,
    ) -> Result<(Diagnostics, String), Diagnostics> {
        if diag.has_errors() {
            return Err(diag);
        }

        let exports = scopes
            .functions()
            .filter(|(_, f)| f.linkage == Linkage::Export)
            .map(|(id, _)| State::new(GenericFunc::from_id(scopes, id)));
        let (mut this, main) = if flags.lib {
            (Self::new(scopes, diag, exports.collect(), flags), None)
        } else {
            let Some(main) = scopes[scope].vns.get("main").and_then(|id| id.as_fn()) else {
                diag.error(Error::new("no main function found", Span::default()));
                return Err(diag);
            };
            let main = State::new(GenericFunc::from_id(scopes, *main));
            (
                Self::new(
                    scopes,
                    diag,
                    exports.chain(std::iter::once(main.clone())).collect(),
                    flags,
                ),
                Some(main),
            )
        };

        let main = main.map(|mut main| this.gen_c_main(&mut main));
        let (static_defs, init) = this.gen_ctl_init();

        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        while !this.funcs.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for mut state in diff {
                this.emit_fn(&mut state, &mut prototypes);
            }
        }
        let functions = std::mem::take(&mut this.buffer).finish();

        if this.flags.leak {
            this.buffer.emit("#define CTL_NOGC\n");
        }
        if this.flags.no_bit_int {
            this.buffer.emit("#define CTL_NOBITINT\n");
        }
        this.buffer.emit(include_str!("../ctl/ctl.h"));
        if this.diag.has_errors() {
            return Err(this.diag);
        }
        this.type_gen.finish(&mut this.buffer, &this.flags);
        this.buffer.emit(prototypes.finish());
        this.buffer.emit(static_defs);
        this.buffer.emit(functions);
        this.buffer.emit(init);
        if let Some(main) = main {
            this.buffer.emit(main);
        }

        Ok((this.diag, this.buffer.finish()))
    }

    fn gen_ctl_init(&mut self) -> (String, String) {
        // TODO: temporaries generated by gen_expr should be marked static, so static pointers will
        // not dangle
        let state = &mut State::new(GenericFunc::from_id(self.scopes, FunctionId::RESERVED));
        let mut defs = Buffer::default();

        self.buffer.emit("static void $ctl_static_init(void){");

        hoist_point!(self, {
            for (id, var) in self.scopes.vars().filter(|(_, v)| v.is_static) {
                usebuf!(self, &mut defs, {
                    self.emit_local_decl(id, state);
                    self.buffer.emit(";");
                });

                self.emit_var_name(id, state);
                self.buffer.emit("=");
                self.emit_expr(var.value.clone().unwrap(), state);
                self.buffer.emit(";");
            }
        });

        self.buffer.emit("}static void $ctl_static_deinit(void){}");

        (defs.finish(), std::mem::take(&mut self.buffer).finish())
    }

    fn gen_c_main(&mut self, main: &mut State) -> String {
        self.buffer.emit("int main(int argc, char **argv){");
        let returns = self.scopes.get(main.func.id).ret != Type::Void;
        if let Some(state) = self
            .find_function("std", "convert_argv")
            .filter(|_| self.scopes.get(main.func.id).params.len() == 1)
        {
            if returns {
                self.buffer.emit("return ");
            }
            self.emit_fn_name(main);
            self.buffer.emit("(");
            self.emit_fn_name(&state);
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
            self.emit_fn_name(main);
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
        if func.attrs.iter().any(|attr| attr.name.data == ATTR_NOGEN) {
            return;
        }

        if Self::needs_fn_wrapper(func) {
            usebuf!(self, prototypes, {
                self.emit_prototype(state, false, false);

                self.buffer.emit("{");
                self.emit_prototype(state, true, true);
                self.buffer.emit(";");
                self.emit_fn_name_inner(state, true);
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
            self.emit_prototype(state, false, false);
            self.buffer.emit("{");
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

            for stmt in body.into_iter() {
                self.emit_stmt(stmt, state);
            }
            if !func.returns {
                self.buffer.emit(format!("return {VOID_INSTANCE};"));
            }
            self.buffer.emit("}");
        }
    }

    fn emit_stmt(&mut self, stmt: CheckedStmt, state: &mut State) {
        match stmt {
            CheckedStmt::Expr(mut expr) => {
                hoist_point!(self, {
                    state.fill_generics(&mut expr.ty);
                    self.emit_expr_inner(expr, state);
                    self.buffer.emit(";");
                });
            }
            CheckedStmt::Let(patt, value) => {
                hoist_point!(self, {
                    if let CheckedPatternData::Variable(id) = patt.data {
                        let ty = self.emit_local_decl(id, state);
                        if let Some(mut expr) = value {
                            expr.ty = ty;
                            self.buffer.emit("=");
                            self.emit_expr_inner(expr, state);
                        }

                        self.buffer.emit(";");
                    } else if let Some(mut value) = value {
                        state.fill_generics(&mut value.ty);
                        let ty = value.ty.clone();
                        let tmp = self.emit_tmpvar(value, state);
                        self.emit_pattern_bindings(state, &patt.data, &tmp, &ty);
                    }
                });
            }
            CheckedStmt::None => {}
        }
    }

    fn emit_expr(&mut self, mut expr: CheckedExpr, state: &mut State) {
        state.fill_generics(&mut expr.ty);
        if Self::has_side_effects(&expr) {
            self.emit_tmpvar_ident(expr, state);
        } else {
            self.emit_expr_inner(expr, state);
        }
    }

    fn emit_expr_inner(&mut self, expr: CheckedExpr, state: &mut State) {
        match expr.data {
            CheckedExprData::Binary { op, left, right } => {
                if expr.ty == Type::Bool {
                    self.emit_cast(&expr.ty);
                    self.buffer.emit("(");
                }
                self.buffer.emit("(");
                self.emit_expr(*left, state);
                self.buffer.emit(format!(" {op} "));
                self.emit_expr(*right, state);
                self.buffer.emit(")");

                if expr.ty == Type::Bool {
                    self.buffer.emit(" ? 1 : 0)");
                }
            }
            CheckedExprData::Unary {
                op,
                expr: mut inner,
            } => match op {
                UnaryOp::Plus => {
                    self.emit_expr(*inner, state);
                }
                UnaryOp::Neg => {
                    self.buffer.emit("-");
                    self.emit_expr(*inner, state);
                }
                UnaryOp::PostIncrement => {
                    self.emit_expr(*inner, state);
                    self.buffer.emit("++");
                }
                UnaryOp::PostDecrement => {
                    self.emit_expr(*inner, state);
                    self.buffer.emit("--");
                }
                UnaryOp::PreIncrement => {
                    self.buffer.emit("++");
                    self.emit_expr(*inner, state);
                }
                UnaryOp::PreDecrement => {
                    self.buffer.emit("--");
                    self.emit_expr(*inner, state);
                }
                UnaryOp::Not => {
                    if inner.ty == Type::Bool {
                        self.emit_cast(&expr.ty);
                        self.buffer.emit("(");
                        self.emit_expr(*inner, state);
                        self.buffer.emit(" ^ 1)");
                    } else {
                        self.buffer.emit("~");
                        self.emit_expr(*inner, state);
                    }
                }
                UnaryOp::Deref => {
                    self.buffer.emit("(*");
                    self.emit_expr(*inner, state);
                    self.buffer.emit(")");
                }
                UnaryOp::Addr | UnaryOp::AddrMut => {
                    state.fill_generics(&mut inner.ty);

                    let array = inner.ty.is_array();
                    if !array {
                        self.buffer.emit("&");
                    }
                    if Self::is_lvalue(&inner) {
                        self.emit_expr(*inner, state);
                    } else {
                        self.emit_tmpvar_ident(*inner, state);
                    }

                    if array {
                        self.buffer.emit(format!(".{ARRAY_DATA_NAME}"));
                    }
                }
                UnaryOp::Try => {
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(&expr.ty);
                        self.buffer.emit(format!(" {tmp};"));

                        state.fill_generics(&mut inner.ty);
                        let inner_ty = inner.ty.clone();
                        let inner_tmp = self.emit_tmpvar(*inner, state);
                        self.emit_pattern_if_stmt(
                            state,
                            &CheckedPatternData::UnionMember {
                                pattern: None,
                                variant: "None".into(),
                                inner: expr.ty.clone(),
                                borrows: false,
                            },
                            &inner_tmp,
                            &inner_ty,
                        );
                        self.buffer.emit("return ");
                        let mut ret_type = self.scopes.get(state.func.id).ret.clone();
                        state.fill_generics(&mut ret_type);
                        self.emit_expr_inner(CheckedExpr::option_null(ret_type), state);
                        self.buffer.emit(format!(";}}{tmp}="));
                        if inner_ty.can_omit_tag(self.scopes).is_some() {
                            self.buffer.emit(format!("{inner_tmp};"));
                        } else {
                            self.buffer.emit(format!("{inner_tmp}.$Some.$0;"));
                        }
                    });
                }
                UnaryOp::Unwrap => panic!("ICE: UnaryOp::Unwrap in gen_expr"),
            },
            CheckedExprData::Call {
                mut func,
                args,
                mut inst,
                trait_id,
                scope,
            } => {
                if let Some(inst) = inst.as_mut() {
                    state.fill_generics(inst);
                }

                let original_id = func.id;
                if let Some(trait_id) = trait_id {
                    let f = self.scopes.get(func.id);
                    let inst = inst
                        .as_ref()
                        .expect("generating trait function with no instance");
                    func.id = self
                        .find_implementation(inst, trait_id, &f.name.data, state.caller)
                        .expect(
                            "generating trait fn with no corresponding implementation in instance",
                        );
                    func.ty_args = TypeArgs(
                        func.ty_args
                            .0
                            .into_iter()
                            .zip(self.scopes.get(func.id).type_params.iter())
                            .map(|((_, ty), id)| (*id, ty))
                            .collect(),
                    );
                }

                for ty in func.ty_args.values_mut() {
                    state.fill_generics(ty);
                }

                if let Some(name) = self.scopes.intrinsic_name(func.id) {
                    return self.emit_intrinsic(name, &func, args, state);
                }

                let mut args: IndexMap<_, _> = args
                    .into_iter()
                    .map(|(name, mut expr)| {
                        state.fill_generics(&mut expr.ty);
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

                let next_state = State::with_instance_and_caller(func, inst.as_ref(), scope);
                self.emit_fn_name(&next_state);
                self.buffer.emit("(");
                self.scopes
                    .get(original_id)
                    .params
                    .iter()
                    .flat_map(|param| args.shift_remove(&param.label))
                    .for_each(|arg| arg!(arg));
                args.into_iter().for_each(|(_, arg)| arg!(arg));
                self.buffer.emit(")");
                self.funcs.insert(next_state);
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
                self.buffer.emit("(");
                self.emit_type(&expr.ty);
                self.buffer.emit(format!("){{.{ARRAY_DATA_NAME}={{"));
                for _ in 0..count {
                    self.emit_expr((*init).clone(), state);
                    self.buffer.emit(",");
                }
                self.buffer.emit("}}");
            }
            CheckedExprData::Vec(exprs) => {
                let ut = (**expr.ty.as_user().unwrap()).clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    let arr = state.tmpvar();
                    let len = exprs.len();
                    let inner = ut.first_type_arg().unwrap();
                    self.emit_type(inner);
                    self.buffer.emit(format!(" {arr}[{len}]={{"));
                    for expr in exprs {
                        self.emit_expr(expr, state);
                        self.buffer.emit(",");
                    }
                    self.buffer.emit("};");

                    self.emit_with_capacity(&expr.ty, &tmp, &ut, len);
                    self.buffer.emit(format!(
                        "CTL_MEMCPY((void*){tmp}.$ptr.$addr,(const void*){arr},{len}*{}",
                        inner.size_and_align(self.scopes).0
                    ));
                    self.buffer.emit(format!(");{tmp}.$len={len};"));
                });
            }
            CheckedExprData::VecWithInit { init, count } => {
                tmpbuf_emit!(self, state, |tmp| {
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let len = self.emit_tmpvar(*count, state);
                    self.emit_with_capacity(&expr.ty, &tmp, &ut, &len);
                    self.buffer.emit(format!("for(usize i=0;i<{len};i++){{(("));
                    self.emit_type(ut.first_type_arg().unwrap());
                    self.buffer.emit(format!("*){tmp}.$ptr.$addr)[i]="));
                    self.emit_expr(*init, state);
                    self.buffer.emit(format!(";}}{tmp}.$len={len};"));
                });
            }
            CheckedExprData::Set(exprs) => {
                let ut = (**expr.ty.as_user().unwrap()).clone();
                if exprs.is_empty() {
                    return self.emit_new(&ut);
                }

                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_with_capacity(&expr.ty, &tmp, &ut, exprs.len());
                    let insert = State::with_instance(
                        GenericFunc::from_id(
                            self.scopes,
                            self.scopes
                                .get(ut.id)
                                .find_associated_fn(self.scopes, "insert")
                                .unwrap(),
                        ),
                        Some(&expr.ty),
                    );

                    for val in exprs {
                        self.emit_fn_name(&insert);
                        self.buffer.emit(format!("(&{tmp},"));
                        self.emit_expr(val, state);
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
                    let insert = State::with_instance(
                        GenericFunc::from_id(
                            self.scopes,
                            self.scopes
                                .get(ut.id)
                                .find_associated_fn(self.scopes, "insert")
                                .unwrap(),
                        ),
                        Some(&expr.ty),
                    );

                    self.emit_with_capacity(&expr.ty, &tmp, &ut, exprs.len());
                    for (key, val) in exprs {
                        self.emit_fn_name(&insert);
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
                self.buffer.emit(format!("{value}"));
            }
            CheckedExprData::Float(value) => {
                self.emit_cast(&expr.ty);
                self.buffer.emit(value);
            }
            CheckedExprData::String(value) => {
                self.emit_cast(&expr.ty);
                self.buffer.emit("{.$span={.$ptr=");
                self.emit_cast(&Type::Ptr(Type::Uint(8).into()));
                self.buffer.emit("\"");
                for byte in value.as_bytes() {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer.emit("\",.$len=");
                self.emit_cast(&Type::Usize);
                self.buffer.emit(format!("{}}}}}", value.len()));
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
            CheckedExprData::Symbol(symbol, scope) => match symbol {
                Symbol::Func(func) => {
                    let state = State::new_with_caller(func, scope);
                    self.emit_fn_name(&state);
                    self.funcs.insert(state);
                }
                Symbol::Var(id) => {
                    self.emit_var_name(id, state);
                }
            },
            CheckedExprData::Instance {
                mut members,
                variant,
            } => {
                if expr.ty.can_omit_tag(self.scopes).is_some() {
                    if let Some(some) = members.remove("0") {
                        self.emit_expr(some, state);
                    } else {
                        self.buffer.emit(" NULL");
                    }
                } else {
                    self.emit_cast(&expr.ty);
                    self.buffer.emit("{");
                    let ut = self.scopes.get(expr.ty.as_user().unwrap().id);
                    if let Some((variant, union)) = variant.zip(ut.data.as_union()) {
                        let members: Vec<_> = members
                            .into_iter()
                            .map(|(name, mut expr)| {
                                state.fill_generics(&mut expr.ty);
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
                            self.buffer.emit(format!(".${name}={value},"));
                        }

                        if union.get(&variant).is_some_and(|v| v.is_some()) {
                            self.buffer.emit(format!(".${variant}={{"));
                            for (name, value) in members
                                .iter()
                                .filter(|(name, _)| !ut.members.contains_key(name))
                            {
                                self.buffer.emit(format!(".${name}={value},"));
                            }
                            self.buffer.emit("}");
                        }
                    } else {
                        if members.is_empty() {
                            self.buffer.emit("CTL_DUMMY_INIT");
                        }

                        for (name, mut value) in members {
                            state.fill_generics(&mut value.ty);
                            self.buffer.emit(format!(".${name}="));
                            self.emit_expr(value, state);
                            self.buffer.emit(",");
                        }
                    }
                    self.buffer.emit("}");
                }
            }
            CheckedExprData::Member { source, member } => {
                self.emit_expr(*source, state);
                self.buffer.emit(format!(".${member}"));
            }
            CheckedExprData::Assign {
                target,
                binary,
                value,
            } => {
                self.emit_expr(*target, state);
                if let Some(binary) = binary {
                    self.buffer.emit(format!(" {binary}="));
                } else {
                    self.buffer.emit("=");
                }
                self.emit_expr(*value, state);
            }
            CheckedExprData::Block(block) => {
                enter_block!(self, state, &expr.ty, {
                    self.emit_block(block.body, state);
                    if matches!(self.scopes[block.scope].kind, ScopeKind::Block(_, yields) if !yields)
                    {
                        self.buffer
                            .emit(format!("{}={VOID_INSTANCE};", self.cur_block));
                    }
                });
            }
            CheckedExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                enter_block!(self, state, &expr.ty, {
                    if let CheckedExprData::Is(mut expr, patt) = cond.data {
                        state.fill_generics(&mut expr.ty);
                        let ty = expr.ty.clone();
                        let tmp = self.emit_tmpvar(*expr, state);
                        self.emit_pattern_if_stmt(state, &patt.data, &tmp, &ty);
                    } else {
                        self.buffer.emit("if(");
                        self.emit_expr(*cond, state);
                        self.buffer.emit("){");
                    }
                    hoist_point!(self, {
                        self.buffer.emit(format!("{}=", self.cur_block));
                        self.emit_expr(*if_branch, state);
                    });

                    if let Some(else_branch) = else_branch {
                        self.buffer.emit(";}else{");
                        hoist_point!(self, {
                            self.buffer.emit(format!("{}=", self.cur_block));
                            self.emit_expr(*else_branch, state);
                        });
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
                enter_loop!(self, state, &expr.ty, {
                    macro_rules! cond {
                        ($cond: expr) => {
                            self.buffer.emit("if(!");
                            self.emit_expr($cond, state);
                            self.buffer.emit("){");
                            if optional {
                                self.emit_loop_none(state, expr.ty);
                            }
                            self.buffer.emit("break;}");
                        };
                    }

                    self.buffer.emit("for(;;){");
                    hoist_point!(self, {
                        if let Some(cond) = cond {
                            if let CheckedExprData::Is(mut cond, patt) = cond.data {
                                state.fill_generics(&mut cond.ty);
                                let ty = cond.ty.clone();
                                let tmp = self.emit_tmpvar(*cond, state);
                                self.emit_pattern_if_stmt(state, &patt.data, &tmp, &ty);
                                self.emit_block(body.body, state);
                                self.buffer.emit("}else{");
                                if optional {
                                    self.emit_loop_none(state, expr.ty);
                                }
                                self.buffer.emit("break;}");
                            } else if !do_while {
                                cond!(*cond);
                                self.emit_block(body.body, state);
                            } else {
                                self.emit_block(body.body, state);
                                cond!(*cond);
                            }
                        } else {
                            self.emit_block(body.body, state);
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
                scope,
            } => {
                enter_loop!(self, state, &expr.ty, {
                    state.fill_generics(&mut iter.ty);
                    let next = self
                        .scopes
                        .lang_types
                        .get("iter")
                        .copied()
                        .and_then(|id| self.find_implementation(&iter.ty, id, "next", scope))
                        .expect("for iterator should actually implement Iterator");

                    let next_state = State::with_instance(
                        GenericFunc::from_id(self.scopes, next),
                        Some(&iter.ty),
                    );
                    let mut next_ty = self.scopes.get(next).ret.clone();
                    if let Some(ut) = iter.ty.as_user() {
                        next_ty.fill_templates(&ut.ty_args);
                    }

                    let iter_var = self.emit_tmpvar(*iter, state);
                    self.buffer.emit("for(;;){");

                    let item = state.tmpvar();
                    self.emit_type(&next_ty);
                    self.buffer.emit(format!(" {item}="));
                    self.emit_fn_name(&next_state);
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
                    if optional {
                        self.emit_loop_none(state, expr.ty);
                    }
                    self.buffer.emit("break;}}");

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
                    self.buffer
                        .emit(format!("({}", "*".repeat(callee.ty.indirection() - 1)));
                    self.emit_expr(*callee, state);
                    self.buffer.emit(")");
                }

                self.buffer.emit("[");
                for arg in args {
                    self.emit_expr(arg, state);
                }
                self.buffer.emit("]");
            }
            CheckedExprData::Return(expr) => {
                hoist!(self, state, {
                    self.buffer.emit("return ");
                    self.emit_expr(*expr, state);
                    self.buffer.emit(";");
                });
                self.buffer.emit(VOID_INSTANCE);
                //self.yielded = true;
            }
            CheckedExprData::Yield(expr) => {
                hoist!(self, state, {
                    self.buffer.emit(format!("{}=", self.cur_block));
                    self.emit_expr(*expr, state);
                    self.buffer.emit(";");
                });
                self.buffer.emit(VOID_INSTANCE);
                //self.yielded = true;
            }
            CheckedExprData::Break(expr) => {
                hoist!(self, state, {
                    if let Some(expr) = expr {
                        self.buffer.emit(format!("{}=", self.cur_loop));
                        self.emit_expr(*expr, state);
                        self.buffer.emit(";break;");
                    } else {
                        self.buffer.emit("break;");
                    }
                });
                self.buffer.emit(VOID_INSTANCE);
                //self.yielded = true;
            }
            CheckedExprData::Continue => {
                hoist!(self, state, self.buffer.emit("continue;"));
                self.buffer.emit(VOID_INSTANCE);
                //self.yielded = true;
            }
            CheckedExprData::Match {
                expr: mut scrutinee,
                body,
            } => {
                enter_block!(self, state, &expr.ty, {
                    state.fill_generics(&mut scrutinee.ty);
                    let ty = scrutinee.ty.clone();
                    let tmp = self.emit_tmpvar(*scrutinee, state);
                    for (i, (patt, expr)) in body.into_iter().enumerate() {
                        if i > 0 {
                            self.buffer.emit("else ");
                        }

                        self.emit_pattern_if_stmt(state, &patt.data, &tmp, &ty);

                        hoist_point!(self, {
                            self.buffer.emit(format!("{}=", self.cur_block));
                            self.emit_expr(expr, state);
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
                state.fill_generics(&mut inner.ty);
                let ty = inner.ty.clone();
                let tmp = hoist!(self, state, self.emit_tmpvar(*inner, state));
                let mut bindings = Buffer::default();
                let mut conditions = ConditionBuilder::default();
                self.emit_pattern(
                    state,
                    &patt.data,
                    &tmp,
                    &ty,
                    false,
                    &mut bindings,
                    &mut conditions,
                );
                self.buffer.emit(conditions.finish());
            }
            CheckedExprData::Lambda(_) => todo!(),
            CheckedExprData::NeverCoerce(mut inner) => {
                state.fill_generics(&mut inner.ty);

                self.buffer.emit("/*never*/((");
                self.emit_expr_inner(*inner, state);
                self.buffer.emit("),*(");
                self.emit_type(&expr.ty);
                self.buffer.emit("*)(NULL))");
            }
            CheckedExprData::Error => panic!("ICE: ExprData::Error in gen_expr"),
        }
    }

    fn emit_loop_none(&mut self, state: &mut State, ty: Type) {
        self.buffer.emit(format!("{}=", self.cur_loop));
        self.emit_expr(CheckedExpr::option_null(ty), state);
        self.buffer.emit(";");
    }

    fn emit_new(&mut self, ut: &GenericUserType) {
        let new_state = State::new(GenericFunc::new(
            self.scopes
                .get(ut.id)
                .find_associated_fn(self.scopes, "new")
                .unwrap(),
            ut.ty_args.clone(),
        ));

        self.emit_fn_name(&new_state);
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

        self.emit_fn_name(&state);
        self.buffer.emit(format!("({len});"));
        self.funcs.insert(state);
    }

    fn emit_intrinsic(
        &mut self,
        name: &str,
        func: &GenericFunc,
        args: IndexMap<String, CheckedExpr>,
        state: &mut State,
    ) {
        match name {
            "size_of" => {
                self.buffer.emit(format!(
                    "{}",
                    func.first_type_arg().unwrap().size_and_align(self.scopes).0
                ));
            }
            "panic" => {
                let panic = State::new(GenericFunc::from_id(
                    self.scopes,
                    self.scopes
                        .panic_handler
                        .expect("a panic handler should exist"),
                ));

                self.emit_fn_name(&panic);
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

    #[allow(clippy::too_many_arguments)]
    fn emit_pattern(
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
                        self.buffer.emit(format!("({}==", Self::deref(src, ty)));
                        self.emit_cast(ty.strip_references());
                        self.buffer.emit(format!("{value})"));
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
                        self.buffer.emit(format!("({src}>="));
                        self.emit_cast(base);
                        self.buffer.emit(format!(
                            "{start}&&{src}{}",
                            if *inclusive { "<=" } else { "<" }
                        ));
                        self.emit_cast(base);
                        self.buffer.emit(format!("{end})"));
                    });
                });
            }
            CheckedPatternData::String(value) => {
                conditions.next(|buffer| {
                    usebuf!(self, buffer, {
                        self.buffer.emit(format!(
                            "({0}.$span.$len=={1}&&CTL_MEMCMP({0}.$span.$ptr,\"",
                            Self::deref(src, ty),
                            value.len()
                        ));
                        for byte in value.as_bytes() {
                            self.buffer.emit(format!("\\x{byte:x}"));
                        }
                        self.buffer.emit(format!("\",{})==0)", value.len()));
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
                        conditions.next_str(format!("({src}!=NULL)"));
                        if let Some((patt, borrows)) =
                            patt.as_ref().and_then(|patt| patt.data.as_destrucure())
                        {
                            self.emit_pattern(
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
                        conditions.next_str(format!("({src}==NULL)"));
                    }
                } else {
                    let tag = base
                        .as_user()
                        .and_then(|ut| self.scopes.get(ut.id).data.as_union())
                        .and_then(|union| union.variant_tag(variant))
                        .unwrap();
                    conditions.next_str(format!("({src}.{UNION_TAG_NAME}=={tag})"));

                    if let Some(patt) = patt {
                        self.emit_pattern(
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
                            "({src}.$len{}{})",
                            if rest.is_some() { ">=" } else { "==" },
                            patterns.len()
                        ));
                    });
                });

                let pos = rest.map(|RestPattern { id, pos }| {
                    if let Some(id) = id {
                        usebuf!(self, bindings, {
                            self.emit_local_decl(id, state);
                            self.buffer.emit(format!(
                                "={{.$ptr={src}.$ptr+{pos},.$len={src}.$len-{}}};",
                                patterns.len()
                            ));
                        });
                    }
                    pos
                });
                for (i, patt) in patterns.iter().enumerate() {
                    self.emit_pattern(
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
                for (member, inner, patt) in patterns {
                    self.emit_pattern(
                        state,
                        &patt.data,
                        &format!("{src}.${member}"),
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
                let src = if ty.is_any_ptr() {
                    src.into()
                } else {
                    format!("{src}.{ARRAY_DATA_NAME}")
                };
                let rest = rest.map(|RestPattern { id, pos }| {
                    let rest_len = arr_len - patterns.len();
                    if let Some(id) = id {
                        usebuf!(self, bindings, {
                            self.emit_local_decl(id, state);
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
                    self.emit_pattern(
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
                usebuf!(self, bindings, {
                    self.emit_local_decl(id, state);
                    self.buffer
                        .emit(format!("={}{src};", if borrow { "&" } else { "" }));
                });
            }
            CheckedPatternData::Void => {
                conditions.next(|buffer| buffer.emit("1"));
            }
            CheckedPatternData::Error => panic!("ICE: CheckedPatternData::Error in gen_pattern"),
        }
    }

    fn emit_pattern_if_stmt(
        &mut self,
        state: &mut State,
        pattern: &CheckedPatternData,
        src: &str,
        ty: &Type,
    ) {
        let mut bindings = Buffer::default();
        let mut conditions = ConditionBuilder::default();
        self.emit_pattern(
            state,
            pattern,
            src,
            ty,
            false,
            &mut bindings,
            &mut conditions,
        );
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
        let mut bindings = Buffer::default();
        let mut conditions = ConditionBuilder::default();
        self.emit_pattern(
            state,
            pattern,
            src,
            ty,
            false,
            &mut bindings,
            &mut conditions,
        );
        self.buffer.emit(bindings.finish());
    }

    fn emit_block(&mut self, block: Vec<CheckedStmt>, state: &mut State) {
        let old = std::mem::take(&mut self.yielded);
        self.buffer.emit("{");
        for stmt in block.into_iter() {
            self.emit_stmt(stmt, state);
            if self.yielded {
                break;
            }
        }
        self.buffer.emit("}");
        self.yielded = old;
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

    fn emit_prototype(&mut self, state: &mut State, is_prototype: bool, real: bool) {
        let f = self.scopes.get(state.func.id);
        let mut ret = f.ret.clone();
        state.fill_generics(&mut ret);

        let needs_wrapper = Self::needs_fn_wrapper(f);
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
        self.emit_fn_name_inner(state, real);
        self.buffer.emit("(");
        for (i, param) in f.params.iter().enumerate() {
            let mut ty = param.ty.clone();
            state.fill_generics(&mut ty);
            if i > 0 {
                self.buffer.emit(",");
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
                _ = self.emit_local_decl(*id, state);
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
    }

    fn emit_var_name(&mut self, id: VariableId, state: &mut State) {
        use std::collections::hash_map::*;

        if self.flags.minify {
            return self.buffer.emit(format!("v{id}"));
        }

        let var = self.scopes.get(id);
        if var.is_static {
            self.buffer
                .emit(self.scopes.full_name(var.scope, &var.name));
        } else {
            match state.emitted_names.entry(var.name.clone()) {
                Entry::Occupied(entry) if *entry.get() == id => {
                    self.buffer.emit("$");
                    self.buffer.emit(&var.name);
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
                    self.buffer.emit("$");
                    self.buffer.emit(&var.name);
                }
            }
        }
    }

    fn emit_local_decl(&mut self, id: VariableId, state: &mut State) -> Type {
        let var = self.scopes.get(id);
        let mut ty = var.ty.clone();
        state.fill_generics(&mut ty);
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

    fn emit_fn_name(&mut self, state: &State) {
        self.emit_fn_name_inner(state, false)
    }

    fn emit_fn_name_inner(&mut self, state: &State, real: bool) {
        let f = self.scopes.get(state.func.id);
        let is_macro = f.attrs.iter().any(|attr| attr.name.data == ATTR_NOGEN);
        if f.linkage == Linkage::Internal {
            if self.flags.minify {
                self.buffer.emit(format!("f{}", state.func.id));
                for ty in state.func.ty_args.values() {
                    self.buffer
                        .emit_generic_mangled_name(self.scopes, ty, self.flags.minify);
                }
            } else {
                self.buffer
                    .emit(self.scopes.full_name(f.scope, &f.name.data));
                if !state.func.ty_args.is_empty() {
                    self.buffer.emit("$");
                    for ty in state.func.ty_args.values() {
                        self.buffer.emit("$");
                        self.buffer
                            .emit_generic_mangled_name(self.scopes, ty, self.flags.minify);
                    }
                    self.buffer.emit("$$");
                }
            }
        } else {
            let name = f
                .attrs
                .iter()
                .find(|attr| attr.name.data == ATTR_LINKNAME && !attr.props.is_empty())
                .map(|attr| &attr.props[0].name.data)
                .unwrap_or(&f.name.data);

            if !is_macro && !real && Self::needs_fn_wrapper(f) {
                if self.flags.minify {
                    return self.buffer.emit(format!("f{}", state.func.id));
                } else {
                    self.buffer.emit("$");
                }
            }
            self.buffer.emit(name);
        }
    }

    fn has_side_effects(expr: &CheckedExpr) -> bool {
        matches!(
            &expr.data,
            CheckedExprData::Unary {
                op: UnaryOp::PostIncrement
                    | UnaryOp::PostDecrement
                    | UnaryOp::PreIncrement
                    | UnaryOp::PreDecrement,
                ..
            } | CheckedExprData::Call { .. }
                | CheckedExprData::Assign { .. }
        )
    }

    fn is_lvalue(expr: &CheckedExpr) -> bool {
        matches!(
            &expr.data,
            CheckedExprData::Unary {
                op: UnaryOp::Deref,
                ..
            } | CheckedExprData::Symbol(_, _)
                | CheckedExprData::Member { .. }
                | CheckedExprData::Subscript { .. }
        )
    }

    fn find_implementation(
        &self,
        ty: &Type,
        trait_id: UserTypeId,
        fn_name: &str,
        scope: ScopeId,
    ) -> Option<FunctionId> {
        // FIXME: in the very specific case where two extensions exist for the same type that
        // implement the same trait, and they are both used to call the trait in two different
        // places, this function will pick the wrong one for one of them. either fix this lookup or
        // reject duplicate trait implementation in extensions
        let search = |scope: ScopeId| {
            self.scopes[scope]
                .children
                .iter()
                .find(|&&id| matches!(self.scopes[id].kind, ScopeKind::Impl(id) if id == trait_id))
                .and_then(|&id| self.scopes[id].find_in_vns(fn_name))
                .and_then(|f| f.as_fn().copied())
        };

        if let Some(f) = ty
            .as_user()
            .and_then(|ut| search(self.scopes.get(ut.id).body_scope))
        {
            return Some(f);
        }

        self.scopes
            .extensions_in_scope_for(ty, None, scope)
            .find_map(|(_, ext)| search(ext.body_scope))
    }

    fn needs_fn_wrapper(f: &Function) -> bool {
        f.linkage == Linkage::Import && matches!(f.ret, Type::Void | Type::Never | Type::CVoid)
    }

    fn deref(src: &str, ty: &Type) -> String {
        if ty.is_ptr() || ty.is_mut_ptr() {
            format!(
                "({:*<1$}{src})",
                "",
                ty.indirection() - usize::from(ty.strip_references().is_array())
            )
        } else {
            src.into()
        }
    }

    fn find_function(&self, module: &str, name: &str) -> Option<State> {
        let id = self.scopes[self.scopes[ScopeId::ROOT].find_module(module)?]
            .vns
            .get(name)?
            .id
            .as_fn()?;
        Some(State::new(GenericFunc::from_id(self.scopes, *id)))
    }
}
