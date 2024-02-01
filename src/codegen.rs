use std::collections::{hash_map::Entry, HashMap, HashSet};

use indexmap::IndexMap;

use crate::{
    ast::{
        checked::{
            ArrayPattern, CheckedExpr, CheckedExprData, CheckedPattern, CheckedStmt,
            IrrefutablePattern, RestPattern, Symbol,
        },
        parsed::RangePattern,
        UnaryOp,
    },
    error::{Diagnostics, Error},
    lexer::Span,
    sym::{
        CheckedMember, Function, FunctionId, ParamPattern, ScopeId, ScopeKind, Scopes,
        UserTypeData, UserTypeId, VariableId,
    },
    typeid::{CInt, FnPtr, GenericFunc, GenericUserType, Type},
};

const UNION_TAG_NAME: &str = "$tag";
const ARRAY_DATA_NAME: &str = "$data";

#[derive(PartialEq, Eq, Clone)]
struct State {
    func: GenericFunc,
    inst: Option<Type>,
    tmpvar: usize,
    emitted_names: HashMap<String, VariableId>,
    renames: HashMap<VariableId, String>,
}

impl State {
    pub fn new(func: GenericFunc, inst: Option<Type>) -> Self {
        Self {
            func,
            inst,
            tmpvar: 0,
            emitted_names: Default::default(),
            renames: Default::default(),
        }
    }

    pub fn fill_generics(&self, scopes: &Scopes, ty: &mut Type) {
        ty.fill_func_template(scopes, &self.func);

        if let Some(inst) = self.inst.as_ref().and_then(|inst| inst.as_user()) {
            ty.fill_struct_templates(scopes, inst);
        }
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
        self.inst.hash(state);
    }
}

struct TypeGen<'a> {
    structs: HashMap<GenericUserType, Vec<GenericUserType>>,
    fnptrs: HashSet<FnPtr>,
    arrays: HashMap<Type, HashSet<usize>>,
    scopes: &'a Scopes,
}

impl<'a> TypeGen<'a> {
    fn new(scopes: &'a Scopes) -> Self {
        Self {
            scopes,
            structs: Default::default(),
            fnptrs: Default::default(),
            arrays: Default::default(),
        }
    }

    fn finish(mut self, buffer: &mut Buffer) {
        let structs = std::mem::take(&mut self.structs);
        let fnptrs = std::mem::take(&mut self.fnptrs);
        let mut arrays = std::mem::take(&mut self.arrays);
        let mut definitions = Buffer::default();
        for f in fnptrs {
            definitions.emit("typedef ");
            definitions.emit_type(self.scopes, &f.ret, None);
            definitions.emit("(*");
            definitions.emit_fnptr_name(self.scopes, &f);
            definitions.emit(")(");
            for (i, param) in f.params.iter().enumerate() {
                if i > 0 {
                    definitions.emit(", ");
                }

                definitions.emit_type(self.scopes, param, None);
            }
            definitions.emit(");");
        }

        let mut emitted_arrays = HashSet::new();
        for ut in self.get_struct_order(&structs) {
            let union = self.scopes.get(ut.id).data.as_union();
            let unsafe_union = union.is_some_and(|union| union.is_unsafe);
            if unsafe_union {
                buffer.emit("typedef union ");
                definitions.emit("union ");
            } else {
                buffer.emit("typedef struct ");
                definitions.emit("struct ");
            }

            buffer.emit_type_name(self.scopes, ut);
            buffer.emit(" ");
            buffer.emit_type_name(self.scopes, ut);
            buffer.emit(";");

            definitions.emit_type_name(self.scopes, ut);
            definitions.emit("{");

            let members = self.scopes.get(ut.id).members().unwrap();
            if let Some(union) = union.filter(|_| !unsafe_union) {
                definitions.emit_type(self.scopes, &union.tag_type(), None);
                definitions.emit(format!(" {UNION_TAG_NAME};"));

                for member in members.iter().filter(|m| m.shared) {
                    self.emit_member(ut, member, &mut definitions);
                }

                definitions.emit(" union {");
                for member in members.iter().filter(|m| !m.shared) {
                    self.emit_member(ut, member, &mut definitions);
                }
                definitions.emit("};");
            } else {
                for member in members {
                    self.emit_member(ut, member, &mut definitions);
                }
            }

            definitions.emit("};");

            let ty = Type::User(ut.clone().into());
            if let Some(sizes) = arrays.remove(&ty) {
                for size in sizes {
                    self.emit_array(buffer, Some(&mut definitions), &ty, size);
                }
                emitted_arrays.insert(ty);
            }
        }

        for (ty, sizes) in arrays
            .into_iter()
            .filter(|(ty, _)| !emitted_arrays.contains(ty))
        {
            for size in sizes {
                self.emit_array(buffer, None, &ty, size);
            }
        }

        buffer.emit(definitions.0);
    }

    fn add_type(&mut self, diag: &mut Diagnostics, ty: &Type, adding: Option<&GenericUserType>) {
        match &ty {
            Type::FnPtr(ptr) => self.add_fnptr(diag, (**ptr).clone()),
            Type::User(ut) => self.add_user_type(diag, (**ut).clone(), adding),
            Type::Array(arr) => self.add_array(diag, arr.0.clone(), arr.1, adding),
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

        let mut deps = Vec::new();
        if let Some(members) = self.scopes.get(ut.id).members() {
            for member in members.iter() {
                let mut ty = member.ty.clone();
                ty.fill_struct_templates(self.scopes, &ut);
                match ty {
                    Type::FnPtr(ptr) => self.add_fnptr(diag, *ptr),
                    Type::User(dep) => {
                        if matches!(adding, Some(adding) if adding == &*dep) {
                            // ideally get the span of the instantiation that caused this
                            diag.error(Error::cyclic(
                                &dep.name(self.scopes),
                                &ut.name(self.scopes),
                                self.scopes.get(dep.id).name.span,
                            ));
                            if !matches!(adding, Some(adding) if adding == &ut) {
                                diag.error(Error::cyclic(
                                    &ut.name(self.scopes),
                                    &dep.name(self.scopes),
                                    self.scopes.get(ut.id).name.span,
                                ));
                            }
                            return;
                        }

                        deps.push((*dep).clone());
                        self.add_user_type(diag, *dep, Some(&ut));
                    }
                    Type::Array(arr) => {
                        let mut inner = &arr.0;
                        while let Type::Array(ty) = inner {
                            inner = &ty.0;
                        }

                        if let Type::User(data) = inner {
                            deps.push((**data).clone());
                        }

                        self.add_array(diag, arr.0, arr.1, Some(&ut));
                    }
                    _ => {}
                }
            }
        }

        self.structs.insert(ut, deps);
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

    fn emit_member(&mut self, ut: &GenericUserType, member: &CheckedMember, buffer: &mut Buffer) {
        let mut ty = member.ty.clone();
        ty.fill_struct_templates(self.scopes, ut);
        buffer.emit_type(self.scopes, &ty, None);
        buffer.emit(format!(" {}", member.name));
        buffer.emit(";");
    }

    fn emit_array(
        &mut self,
        typedef: &mut Buffer,
        defs: Option<&mut Buffer>,
        ty: &Type,
        size: usize,
    ) {
        typedef.emit("typedef struct ");
        typedef.emit_array_struct_name(self.scopes, ty, size);
        typedef.emit(" ");
        typedef.emit_array_struct_name(self.scopes, ty, size);
        typedef.emit(";");

        let defs = defs.unwrap_or(typedef);
        defs.emit("struct ");
        defs.emit_array_struct_name(self.scopes, ty, size);
        defs.emit(" { ");
        defs.emit_type(self.scopes, ty, None);
        defs.emit(format!(" {ARRAY_DATA_NAME}[{size}]; }};"));
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
    ) {
        match id {
            Type::Void | Type::Never | Type::CVoid => self.emit("$void"),
            Type::Int(bits) | Type::Uint(bits) => {
                let signed = matches!(id, Type::Int(_));
                self.emit(format!("{}INT({bits})", if signed { "S" } else { "U" }));
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
                    self.emit_type(scopes, &value.0, tg);
                    // self.emit("/*");
                    // self.emit_type(diag, scopes, inner, tg);
                    // self.emit("*/");
                } else if inner.is_c_void() {
                    self.emit("void");
                } else {
                    self.emit_type(scopes, inner, tg);
                }
                if id.is_ptr() {
                    self.emit(" const*");
                } else {
                    self.emit(" *");
                }
            }
            Type::FnPtr(f) => {
                self.emit_generic_mangled_name(scopes, id);
                if let Some((diag, tg)) = tg {
                    tg.add_fnptr(diag, (**f).clone());
                }
            }
            Type::User(ut) => match &scopes.get(ut.id).data {
                UserTypeData::Struct { .. } | UserTypeData::Union(_) => {
                    if is_opt_ptr(scopes, id) {
                        self.emit_type(scopes, &ut.ty_args[0], tg);
                    } else {
                        if let Some((diag, tg)) = tg {
                            tg.add_user_type(diag, (**ut).clone(), None);
                        }
                        self.emit_type_name(scopes, ut);
                    }
                }
                UserTypeData::Enum(backing) => {
                    self.emit_type(scopes, backing, tg);
                }
                UserTypeData::Template(_, _) => {
                    panic!("ICE: Template type in emit_type");
                }
                UserTypeData::Trait => {
                    panic!("ICE: Trait type in emit_type");
                }
            },
            Type::Array(data) => {
                self.emit_generic_mangled_name(scopes, id);
                if let Some((diag, tg)) = tg {
                    tg.add_array(diag, data.0.clone(), data.1, None);
                }
            }
            Type::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_type"),
            Type::TraitSelf => panic!("ICE: TypeId::TraitSelf in emit_type"),
        }
    }

    fn emit_generic_mangled_name(&mut self, scopes: &Scopes, id: &Type) {
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
                self.emit_generic_mangled_name(scopes, inner);
            }
            Type::MutPtr(inner) => {
                self.emit("mutptr_");
                self.emit_generic_mangled_name(scopes, inner);
            }
            Type::FnPtr(f) => self.emit_fnptr_name(scopes, f),
            Type::User(ut) => {
                self.emit_type_name(scopes, ut);
            }
            Type::Array(data) => self.emit_array_struct_name(scopes, &data.0, data.1),
            Type::Unknown => panic!("ICE: TypeId::Unknown in emit_generic_mangled_name"),
            Type::Unresolved(_) => panic!("ICE: TypeId::Unresolved in emit_generic_mangled_name"),
            Type::TraitSelf => panic!("ICE: TypeId::TraitSelf in emit_generic_mangled_name"),
        }
    }

    fn emit_fnptr_name(&mut self, scopes: &Scopes, f: &FnPtr) {
        self.emit("fn");
        self.emit_generic_mangled_name(scopes, &f.ret);
        for (i, param) in f.params.iter().enumerate() {
            if i > 0 {
                self.emit("_");
            }
            self.emit_generic_mangled_name(scopes, param);
        }
    }

    fn emit_type_name(&mut self, scopes: &Scopes, ut: &GenericUserType) {
        let ty = scopes.get(ut.id);
        if ty.data.is_template() {
            panic!("ICE: Template type in emit_type_name");
        }

        self.emit(scopes.full_name(ty.scope, &ty.name.data));
        if !ut.ty_args.is_empty() {
            self.emit("$");
            for ty in ut.ty_args.iter() {
                self.emit("$");
                self.emit_generic_mangled_name(scopes, ty);
            }
            self.emit("$$");
        }
    }

    fn emit_array_struct_name(&mut self, scopes: &Scopes, ty: &Type, size: usize) {
        self.emit("Array_");
        self.emit_generic_mangled_name(scopes, ty);
        self.emit(format!("_{}", size));
    }
}

macro_rules! tmpbuf {
    ($self: expr, $state: expr, |$tmp: ident| $body: block) => {{
        let buffer = std::mem::take(&mut $self.buffer);
        let result = {
            let $tmp = $state.tmpvar();
            $body
        };
        $self
            .temporaries
            .emit(std::mem::replace(&mut $self.buffer, buffer).0);
        result
    }};
}

macro_rules! tmpbuf_emit {
    ($self: expr, $state: expr, |$tmp: ident| $body: block) => {{
        let (tmp, result) = tmpbuf!($self, $state, |$tmp| {
            let result = $body;
            ($tmp, result)
        });
        $self.buffer.emit(&tmp);
        result
    }};
}

macro_rules! enter_block {
    ($self: expr, $state: expr, $ty: expr, |$tmp: ident| $body: block) => {{
        let ty = $ty;
        let old = std::mem::replace(&mut $self.cur_block, $state.tmpvar());
        tmpbuf!($self, $state, |$tmp| {
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
    ($self: expr, $state: expr, $ty: expr, |$tmp: ident| $body: block) => {{
        let ty = $ty;
        let cur_loop = $state.tmpvar();
        let old_loop = std::mem::replace(&mut $self.cur_loop, cur_loop.clone());
        let old_block = std::mem::replace(&mut $self.cur_block, cur_loop);
        tmpbuf!($self, $state, |$tmp| {
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
        let written = std::mem::replace(&mut $self.buffer, old_buf).0;
        $self
            .buffer
            .emit(std::mem::replace(&mut $self.temporaries, old_tmp).0);
        $self.buffer.emit(written);
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
}

impl<'a> Codegen<'a> {
    pub fn build(
        mut diag: Diagnostics,
        scope: ScopeId,
        scopes: &'a Scopes,
        leak: bool,
    ) -> Result<(Diagnostics, String), Diagnostics> {
        if diag.has_errors() {
            return Err(diag);
        }

        let Some(main) = scopes.find_in("main", scope) else {
            diag.error(Error::new("no main function found", Span::default()));
            return Err(diag);
        };
        let main = &mut State::new(GenericFunc::new(*main, Vec::new()), None);
        let mut this = Self {
            scopes,
            diag,
            funcs: [main.clone()].into(),
            buffer: Default::default(),
            temporaries: Default::default(),
            type_gen: TypeGen::new(scopes),
            cur_block: Default::default(),
            cur_loop: Default::default(),
            yielded: Default::default(),
        };

        let conv_argv = (scopes.get(main.func.id).params.len() == 1).then(|| {
            let id = scopes
                .find_in(
                    "convert_argv",
                    *scopes.find_module_in("std", ScopeId::ROOT).unwrap(),
                )
                .unwrap();

            let state = State::new(GenericFunc::new(*id, vec![]), None);
            this.funcs.insert(state.clone());
            state
        });

        for (id, _) in scopes.vars().filter(|(_, v)| v.is_static) {
            this.emit_local_decl(id, main);
            this.buffer.emit(";");
        }
        let static_defs = std::mem::take(&mut this.buffer);

        this.buffer.emit("void $ctl_static_init(void) {");
        hoist_point!(this, {
            for (id, var) in scopes.vars().filter(|(_, v)| v.is_static) {
                this.emit_var_name(id, main);
                this.buffer.emit(" = ");
                this.gen_expr(var.value.clone().unwrap(), main);
                this.buffer.emit(";");
            }
        });
        this.buffer.emit("}");

        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        while !this.funcs.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for mut state in diff {
                this.gen_fn(&mut state, &mut prototypes);
            }
        }

        let functions = std::mem::take(&mut this.buffer);

        this.buffer
            .emit("int main(int argc, char **argv) { $ctl_init(); $ctl_static_init(); ");

        let returns = scopes.get(main.func.id).ret != Type::Void;
        if let Some(state) = conv_argv {
            if returns {
                this.buffer.emit("return ");
            }
            this.emit_fn_name(main);
            this.buffer.emit("(");
            this.emit_fn_name(&state);
            if returns {
                this.buffer.emit("(argc, (const char **)argv)); }");
            } else {
                this.buffer
                    .emit("(argc, (const char **)argv)); return 0; }");
            }
        } else {
            this.buffer.emit("(void)argc;");
            this.buffer.emit("(void)argv;");
            if returns {
                this.buffer.emit("return ");
            }
            this.emit_fn_name(main);
            if returns {
                this.buffer.emit("(); }");
            } else {
                this.buffer.emit("(); return 0; }");
            }
        }
        let main = std::mem::take(&mut this.buffer);

        if leak {
            this.buffer.emit("#define CTL_NOGC\n");
        }
        this.buffer.emit(include_str!("../ctl/ctl.h"));
        if this.diag.has_errors() {
            return Err(this.diag);
        }
        this.type_gen.finish(&mut this.buffer);
        this.buffer.emit(prototypes.0);
        this.buffer.emit(static_defs.0);
        this.buffer.emit(functions.0);
        this.buffer.emit(main.0);

        Ok((this.diag, this.buffer.0))
    }

    fn gen_fn(&mut self, state: &mut State, prototypes: &mut Buffer) {
        // TODO: emit an error if a function has the c_macro attribute and a body
        let func = self.scopes.get(state.func.id);
        if func.attrs.iter().any(|attr| attr.name.data == "c_macro") {
            return;
        }

        if Self::needs_fn_wrapper(func) {
            std::mem::swap(&mut self.buffer, prototypes);
            self.emit_prototype(state, false, false);

            self.buffer.emit("{");
            self.emit_prototype(state, true, true);
            self.buffer.emit(";");
            self.emit_fn_name_2(state, true);
            self.buffer.emit("(");
            for i in 0..func.params.len() {
                if i > 0 {
                    self.buffer.emit(", ");
                }
                self.buffer.emit(format!("$param{i}"));
            }

            self.buffer.emit("); ");
            if func.ret.is_never() {
                self.buffer.emit("UNREACHABLE(); }");
            } else {
                self.buffer.emit("return ($void){}; }");
            }
            std::mem::swap(&mut self.buffer, prototypes);
            return;
        }

        std::mem::swap(&mut self.buffer, prototypes);
        self.emit_prototype(state, true, false);
        self.buffer.emit(";");
        std::mem::swap(&mut self.buffer, prototypes);
        if let Some(body) = func.body.clone() {
            self.emit_prototype(state, false, false);
            self.buffer.emit("{");
            for param in func.params.iter() {
                let Some(patt) = param
                    .patt
                    .as_checked()
                    .filter(|patt| !matches!(patt, IrrefutablePattern::Variable(_)))
                else {
                    continue;
                };

                self.gen_irrefutable_pattern(state, patt, &param.label, &param.ty, false);
            }

            for stmt in body.into_iter() {
                self.gen_stmt(stmt, state);
            }
            if !func.returns {
                self.buffer.emit("return ($void){};");
            }
            self.buffer.emit("}");
        }
    }

    fn gen_stmt(&mut self, stmt: CheckedStmt, state: &mut State) {
        match stmt {
            CheckedStmt::Expr(mut expr) => {
                hoist_point!(self, {
                    state.fill_generics(self.scopes, &mut expr.ty);
                    self.gen_expr_inner(expr, state);
                    self.buffer.emit(";");
                });
            }
            CheckedStmt::Let(id) => {
                hoist_point!(self, {
                    let var = self.scopes.get(id);
                    let ty = self.emit_local_decl(id, state);
                    if let Some(mut expr) = var.value.clone() {
                        expr.ty = ty;
                        self.buffer.emit(" = ");
                        self.gen_expr_inner(expr, state);
                    }

                    self.buffer.emit(";");
                });
            }
            CheckedStmt::LetPattern(pattern, mut value) => {
                hoist_point!(self, {
                    state.fill_generics(self.scopes, &mut value.ty);
                    let ty = value.ty.clone();
                    let tmp = self.gen_tmpvar(value, state);
                    self.gen_irrefutable_pattern(state, &pattern, &tmp, &ty, false);
                });
            }
            CheckedStmt::None => {}
        }
    }

    fn gen_expr_inner(&mut self, expr: CheckedExpr, state: &mut State) {
        match expr.data {
            CheckedExprData::Binary { op, left, right } => {
                if expr.ty == Type::Bool {
                    self.emit_cast(&expr.ty);
                    self.buffer.emit("(");
                }
                self.buffer.emit("(");
                self.gen_expr(*left, state);
                self.buffer.emit(format!(" {op} "));
                self.gen_expr(*right, state);
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
                    self.gen_expr(*inner, state);
                }
                UnaryOp::Neg => {
                    self.buffer.emit("-");
                    self.gen_expr(*inner, state);
                }
                UnaryOp::PostIncrement => {
                    self.gen_expr(*inner, state);
                    self.buffer.emit("++");
                }
                UnaryOp::PostDecrement => {
                    self.gen_expr(*inner, state);
                    self.buffer.emit("--");
                }
                UnaryOp::PreIncrement => {
                    self.buffer.emit("++");
                    self.gen_expr(*inner, state);
                }
                UnaryOp::PreDecrement => {
                    self.buffer.emit("--");
                    self.gen_expr(*inner, state);
                }
                UnaryOp::Not => {
                    if inner.ty == Type::Bool {
                        self.emit_cast(&expr.ty);
                        self.buffer.emit("(");
                        self.gen_expr(*inner, state);
                        self.buffer.emit(" ^ 1)");
                    } else {
                        self.buffer.emit("~");
                        self.gen_expr(*inner, state);
                    }
                }
                UnaryOp::Deref => {
                    self.buffer.emit("(*");
                    self.gen_expr(*inner, state);
                    self.buffer.emit(")");
                }
                UnaryOp::Addr | UnaryOp::AddrMut => {
                    state.fill_generics(self.scopes, &mut inner.ty);

                    let array = inner.ty.is_array();
                    if !array {
                        self.buffer.emit("&");
                    }
                    if Self::is_lvalue(&inner) {
                        self.gen_expr(*inner, state);
                    } else {
                        self.gen_tmpvar_emit(*inner, state);
                    }

                    if array {
                        self.buffer.emit(format!(".{ARRAY_DATA_NAME}"));
                    }
                }
                UnaryOp::Try => {
                    tmpbuf_emit!(self, state, |tmp| {
                        self.emit_type(&expr.ty);
                        self.buffer.emit(format!(" {tmp};"));

                        state.fill_generics(self.scopes, &mut inner.ty);
                        let inner_ty = inner.ty.clone();
                        let inner_tmp = self.gen_tmpvar(*inner, state);
                        self.gen_pattern(
                            state,
                            &CheckedPattern::UnionMember {
                                pattern: None,
                                variant: "None".into(),
                                inner: expr.ty.clone(),
                            },
                            &inner_tmp,
                            &inner_ty,
                        );
                        self.buffer.emit("return ");
                        let mut ret_type = self.scopes.get(state.func.id).ret.clone();
                        state.fill_generics(self.scopes, &mut ret_type);
                        self.gen_expr_inner(CheckedExpr::option_null(ret_type), state);
                        self.buffer.emit(format!("; }} else {{ {tmp} = "));
                        if is_opt_ptr(self.scopes, &inner_ty) {
                            self.buffer.emit(format!("{inner_tmp};"));
                        } else {
                            self.buffer.emit(format!("{inner_tmp}.Some;"));
                        }

                        self.buffer.emit("}");
                    });
                }
                UnaryOp::Unwrap => panic!("ICE: UnaryOp::Unwrap in gen_expr"),
            },
            CheckedExprData::Call {
                mut func,
                args,
                mut inst,
                trait_id,
            } => {
                if let Some(inst) = inst.as_mut() {
                    state.fill_generics(self.scopes, inst);
                }

                let original_id = func.id;
                if let Some(trait_id) = trait_id {
                    let f = self.scopes.get(func.id);
                    let inst = inst
                        .as_ref()
                        .expect("generating trait function with no instance");
                    func.id = Self::find_implementation(inst, self.scopes, trait_id, &f.name.data)
                        .expect(
                            "generating trait fn with no corresponding implementation in instance",
                        );
                }

                for ty in func.ty_args.iter_mut() {
                    state.fill_generics(self.scopes, ty);
                }

                if let Some(name) = self.scopes.intrinsic_name(func.id) {
                    self.gen_intrinsic(name, &func, args, state);
                    return;
                }

                let mut args: IndexMap<_, _> = args
                    .into_iter()
                    .map(|(name, mut expr)| {
                        state.fill_generics(self.scopes, &mut expr.ty);
                        // TODO: dont emit temporaries for expressions that cant have side effects
                        tmpbuf!(self, state, |tmp| {
                            self.emit_type(&expr.ty);
                            self.buffer.emit(format!(" {tmp} = "));
                            self.gen_expr_inner(expr, state);
                            self.buffer.emit(";");
                            (name, tmp)
                        })
                    })
                    .collect();

                let mut count = 0;
                macro_rules! arg {
                    ($arg: expr) => {{
                        if count > 0 {
                            self.buffer.emit(", ");
                        }

                        self.buffer.emit($arg);
                        count += 1;
                    }};
                }

                let next_state = State::new(func, inst);
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
                self.gen_expr(*expr, state);
                self.buffer.emit(")(");
                for (i, arg) in args.into_iter().enumerate() {
                    if i > 0 {
                        self.buffer.emit(", ");
                    }

                    self.gen_expr(arg, state);
                }
                self.buffer.emit(")");
            }
            CheckedExprData::Array(exprs) => {
                self.buffer.emit("(");
                self.emit_type(&expr.ty);
                self.buffer.emit(format!("){{.{ARRAY_DATA_NAME}={{"));
                for expr in exprs {
                    self.gen_expr(expr, state);
                    self.buffer.emit(",");
                }
                self.buffer.emit("}}");
            }
            CheckedExprData::ArrayWithInit { init, count } => {
                self.buffer.emit("(");
                self.emit_type(&expr.ty);
                self.buffer.emit(format!("){{.{ARRAY_DATA_NAME}={{"));
                for _ in 0..count {
                    self.gen_expr((*init).clone(), state);
                    self.buffer.emit(",");
                }
                self.buffer.emit("}}");
            }
            CheckedExprData::Vec(exprs) => {
                tmpbuf_emit!(self, state, |tmp| {
                    let arr = state.tmpvar();
                    let len = exprs.len();
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let inner = &ut.ty_args[0];

                    self.emit_type(inner);
                    self.buffer.emit(format!(" {arr}[{len}] = {{"));
                    for expr in exprs {
                        self.gen_expr(expr, state);
                        self.buffer.emit(",");
                    }
                    self.buffer.emit("};");

                    let wc_state = State::new(
                        GenericFunc::new(
                            *self
                                .scopes
                                .find_in("with_capacity", self.scopes.get(ut.id).body_scope)
                                .unwrap(),
                            vec![inner.clone()],
                        ),
                        state.inst.clone(),
                    );

                    self.emit_type(&expr.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.emit_fn_name(&wc_state);
                    self.buffer.emit(
                        format!(
                            concat!(
                                "({len});",
                                "__builtin_memcpy((void *){tmp}.ptr.addr, (const void *){arr}, {len} * sizeof {arr}[0]);",
                                "{tmp}.len = {len};"
                            ),
                            len = len,
                            tmp = tmp,
                            arr = arr,
                        )
                    );
                    self.funcs.insert(wc_state);
                });
            }
            CheckedExprData::VecWithInit { init, count } => {
                tmpbuf_emit!(self, state, |tmp| {
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let inner = &ut.ty_args[0];
                    let len = self.gen_tmpvar(*count, state);
                    let wc_state = State::new(
                        GenericFunc::new(
                            *self
                                .scopes
                                .find_in("with_capacity", self.scopes.get(ut.id).body_scope)
                                .unwrap(),
                            vec![inner.clone()],
                        ),
                        state.inst.clone(),
                    );

                    self.emit_type(&expr.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.emit_fn_name(&wc_state);
                    self.buffer
                        .emit(format!("({len}); for (usize i = 0; i < {len}; i++) {{ (("));
                    self.emit_type(inner);
                    self.buffer.emit(format!(" *){tmp}.ptr.addr)[i] = "));
                    self.gen_expr(*init, state);
                    self.buffer.emit(format!("; }} {tmp}.len = {len};"));

                    self.funcs.insert(wc_state);
                });
            }
            CheckedExprData::Set(exprs) => {
                tmpbuf_emit!(self, state, |tmp| {
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let body = self.scopes.get(ut.id).body_scope;
                    let wc_state = State::new(
                        GenericFunc::new(
                            *self.scopes.find_in("with_capacity", body).unwrap(),
                            vec![ut.ty_args[0].clone()],
                        ),
                        state.inst.clone(),
                    );

                    self.emit_type(&expr.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.emit_fn_name(&wc_state);
                    self.buffer.emit(format!("({});", exprs.len()));
                    let insert = State::new(
                        GenericFunc::new(*self.scopes.find_in("insert", body).unwrap(), vec![]),
                        Some(expr.ty.clone()),
                    );

                    for val in exprs {
                        self.emit_fn_name(&insert);
                        self.buffer.emit(format!("(&{tmp}, "));
                        self.gen_expr(val, state);
                        self.buffer.emit(");");
                    }
                    self.funcs.insert(wc_state);
                    self.funcs.insert(insert);
                });
            }
            CheckedExprData::Map(exprs) => {
                tmpbuf_emit!(self, state, |tmp| {
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let body = self.scopes.get(ut.id).body_scope;
                    let wc_state = State::new(
                        GenericFunc::new(
                            *self.scopes.find_in("with_capacity", body).unwrap(),
                            vec![ut.ty_args[0].clone(), ut.ty_args[1].clone()],
                        ),
                        state.inst.clone(),
                    );

                    let insert = State::new(
                        GenericFunc::new(*self.scopes.find_in("insert", body).unwrap(), vec![]),
                        Some(expr.ty.clone()),
                    );

                    self.emit_type(&expr.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.emit_fn_name(&wc_state);
                    self.buffer.emit(format!("({});", exprs.len()));

                    for (key, val) in exprs {
                        self.emit_fn_name(&insert);
                        self.buffer.emit(format!("(&{tmp}, "));
                        self.gen_expr(key, state);
                        self.buffer.emit(",");
                        self.gen_expr(val, state);
                        self.buffer.emit(");");
                    }
                    self.funcs.insert(wc_state);
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
                self.buffer.emit("{ .span = { .ptr = ");
                self.emit_cast(&Type::Ptr(Type::Uint(8).into()));
                self.buffer.emit("\"");
                for byte in value.as_bytes() {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer.emit("\", .len = ");
                self.emit_cast(&Type::Usize);
                self.buffer.emit(format!("{} }} }}", value.len()));
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
            CheckedExprData::Void => self.buffer.emit("($void){}"),
            CheckedExprData::Symbol(symbol) => match symbol {
                Symbol::Func(func) => {
                    let state = State::new(func, None);
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
                if is_opt_ptr(self.scopes, &expr.ty) {
                    if let Some(some) = members.remove("Some") {
                        self.gen_expr(some, state);
                    } else {
                        self.buffer.emit(" NULL");
                    }
                } else {
                    self.emit_cast(&expr.ty);
                    self.buffer.emit("{");
                    if let Some((name, union)) = variant.zip(
                        expr.ty
                            .as_user()
                            .and_then(|ut| self.scopes.get(ut.id).data.as_union()),
                    ) {
                        if !union.is_unsafe
                            && union.variants.iter().any(|m| m.name == name && !m.shared)
                        {
                            self.buffer.emit(format!(
                                ".{UNION_TAG_NAME} = {},",
                                union.variant_tag(&name).unwrap()
                            ));
                        }
                    }

                    for (name, mut value) in members {
                        state.fill_generics(self.scopes, &mut value.ty);
                        self.buffer.emit(format!(".{name} = "));
                        self.gen_expr(value, state);
                        self.buffer.emit(", ");
                    }
                    self.buffer.emit("}");
                }
            }
            CheckedExprData::Member { source, member } => {
                self.gen_expr(*source, state);
                self.buffer.emit(format!(".{member}"));
            }
            CheckedExprData::Assign {
                target,
                binary,
                value,
            } => {
                self.gen_expr(*target, state);
                if let Some(binary) = binary {
                    self.buffer.emit(format!(" {binary}= "));
                } else {
                    self.buffer.emit(" = ");
                }
                self.gen_expr(*value, state);
            }
            CheckedExprData::Block(block) => {
                enter_block!(self, state, &expr.ty, |_tmp| {
                    self.emit_block(block.body, state);
                });
            }
            CheckedExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                enter_block!(self, state, &expr.ty, |_tmp| {
                    if let CheckedExprData::Is(mut expr, patt) = cond.data {
                        state.fill_generics(self.scopes, &mut expr.ty);
                        let ty = expr.ty.clone();
                        let tmp = self.gen_tmpvar(*expr, state);
                        self.gen_pattern(state, &patt, &tmp, &ty);
                    } else {
                        self.buffer.emit("if (");
                        self.gen_expr(*cond, state);
                        self.buffer.emit(") {");
                    }
                    hoist_point!(self, {
                        self.buffer.emit(format!("{} = ", self.cur_block));
                        self.gen_expr(*if_branch, state);
                    });

                    if let Some(else_branch) = else_branch {
                        self.buffer.emit("; } else {");
                        hoist_point!(self, {
                            self.buffer.emit(format!("{} = ", self.cur_block));
                            self.gen_expr(*else_branch, state);
                        });
                    }

                    self.buffer.emit("; }");
                })
            }
            CheckedExprData::Loop {
                cond,
                body,
                do_while,
                optional,
            } => {
                enter_loop!(self, state, &expr.ty, |_tmp| {
                    macro_rules! cond {
                        ($cond: expr) => {
                            self.buffer.emit("if (!");
                            self.gen_expr($cond, state);
                            self.buffer.emit(") { ");
                            if optional {
                                self.gen_loop_none(state, expr.ty);
                            }
                            self.buffer.emit("break; }");
                        };
                    }

                    self.buffer.emit("for (;;) {");
                    hoist_point!(self, {
                        if let Some(cond) = cond {
                            if let CheckedExprData::Is(mut cond, patt) = cond.data {
                                state.fill_generics(self.scopes, &mut cond.ty);
                                let ty = cond.ty.clone();
                                let tmp = self.gen_tmpvar(*cond, state);
                                self.gen_pattern(state, &patt, &tmp, &ty);
                                self.emit_block(body.body, state);
                                self.buffer.emit("} else {");
                                if optional {
                                    self.gen_loop_none(state, expr.ty);
                                }
                                self.buffer.emit("break; }");
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
            } => {
                enter_loop!(self, state, &expr.ty, |item| {
                    state.fill_generics(self.scopes, &mut iter.ty);
                    let next = self
                        .scopes
                        .lang_types
                        .get("iter")
                        .copied()
                        .and_then(|id| Self::find_implementation(&iter.ty, self.scopes, id, "next"))
                        .expect("for iterator should actually implement Iterator");

                    let next_state =
                        State::new(GenericFunc::new(next, vec![]), Some(iter.ty.clone()));
                    let mut next_ty = self.scopes.get(next).ret.clone();
                    if let Some(ut) = iter.ty.as_user() {
                        next_ty.fill_struct_templates(self.scopes, ut);
                    }

                    let iter_var = self.gen_tmpvar(*iter, state);
                    self.buffer.emit("for (;;) {");

                    self.emit_type(&next_ty);
                    self.buffer.emit(format!(" {item} = "));
                    self.emit_fn_name(&next_state);
                    self.buffer.emit(format!("(&{iter_var});"));
                    self.gen_pattern(
                        state,
                        &CheckedPattern::UnionMember {
                            pattern: Some(patt),
                            variant: "Some".into(),
                            inner: next_ty.as_option_inner(self.scopes).unwrap().clone(),
                        },
                        &item,
                        &next_ty,
                    );
                    self.emit_block(body, state);
                    self.buffer.emit("} else { ");
                    if optional {
                        self.gen_loop_none(state, expr.ty);
                    }
                    self.buffer.emit(" break; } } ");

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
                            self.gen_expr(*expr, state);
                        }
                        _ => {
                            self.gen_expr(*callee, state);
                            self.buffer.emit(format!(".{ARRAY_DATA_NAME}"));
                        }
                    }
                } else {
                    self.buffer
                        .emit(format!("({}", "*".repeat(callee.ty.indirection() - 1)));
                    self.gen_expr(*callee, state);
                    self.buffer.emit(")");
                }

                self.buffer.emit("[");
                for arg in args {
                    self.gen_expr(arg, state);
                }
                self.buffer.emit("]");
            }
            CheckedExprData::Return(expr) => {
                // TODO: when return is used as anything except a StmtExpr, we will have to change
                // the generated code to accomodate it
                self.buffer.emit("return ");
                self.gen_expr(*expr, state);
                //self.yielded = true;
            }
            CheckedExprData::Yield(expr) => {
                self.buffer.emit(format!("{} = ", self.cur_block));
                self.gen_expr(*expr, state);
                self.buffer.emit(";");
                //self.yielded = true;
            }
            CheckedExprData::Break(expr) => {
                if let Some(expr) = expr {
                    self.buffer.emit(format!("{} = ", self.cur_loop));
                    self.gen_expr(*expr, state);
                    self.buffer.emit("; break;");
                } else {
                    self.buffer.emit("break;");
                }

                //self.yielded = true;
            }
            CheckedExprData::Continue => {
                self.buffer.emit("continue;");
                //self.yielded = true;
            }
            CheckedExprData::Match {
                expr: mut scrutinee,
                body,
            } => {
                enter_block!(self, state, &expr.ty, |_tmp| {
                    // TODO: update to exclude void when literal patterns are implemented
                    state.fill_generics(self.scopes, &mut scrutinee.ty);
                    let ty = scrutinee.ty.clone();
                    let tmp = self.gen_tmpvar(*scrutinee, state);

                    for (i, (pattern, expr)) in body.into_iter().enumerate() {
                        if i > 0 {
                            self.buffer.emit("else ");
                        }

                        self.gen_pattern(state, &pattern, &tmp, &ty);

                        hoist_point!(self, {
                            self.gen_expr(expr, state);
                            self.buffer.emit("; }");
                        });
                    }

                    self.buffer.emit("else { UNREACHABLE(); }");
                })
            }
            CheckedExprData::As(inner, _) => {
                self.emit_cast(&expr.ty);
                self.buffer.emit("(");
                self.gen_expr(*inner, state);
                self.buffer.emit(")");
            }
            CheckedExprData::Is(mut inner, patt) => {
                tmpbuf_emit!(self, state, |tmp| {
                    self.emit_type(&expr.ty);
                    self.buffer.emit(format!(" {tmp};"));

                    state.fill_generics(self.scopes, &mut inner.ty);
                    let ty = inner.ty.clone();
                    let inner_tmp = self.gen_tmpvar(*inner, state);

                    // TODO: pattern condition
                    self.gen_pattern(state, &patt, &inner_tmp, &ty);
                    self.buffer
                        .emit(format!("{tmp} = 1; }} else {{ {tmp} = 0; }}"));
                });
            }
            CheckedExprData::Lambda(_) => todo!(),
            CheckedExprData::Error => panic!("ICE: ExprData::Error in gen_expr"),
        }
    }

    fn gen_loop_none(&mut self, state: &mut State, ty: Type) {
        self.buffer.emit(format!("{} = ", self.cur_loop));
        self.gen_expr(CheckedExpr::option_null(ty), state);
        self.buffer.emit(";");
    }

    fn gen_intrinsic(
        &mut self,
        name: &str,
        func: &GenericFunc,
        args: IndexMap<String, CheckedExpr>,
        state: &mut State,
    ) {
        match name {
            "size_of" => {
                self.buffer.emit("(usize)sizeof");
                self.emit_cast(&func.ty_args[0]);
            }
            "panic" => {
                let id = self
                    .scopes
                    .panic_handler
                    .expect("a panic handler should exist");
                let panic = State::new(GenericFunc::new(id, vec![]), None);

                self.emit_fn_name(&panic);
                self.buffer.emit("(");
                for (i, (_, expr)) in args.into_iter().enumerate() {
                    if i > 0 {
                        self.buffer.emit(", ");
                    }
                    self.gen_expr(expr, state);
                }
                self.buffer.emit(")");

                self.funcs.insert(panic);
            }
            _ => unreachable!(),
        }
    }

    fn gen_expr(&mut self, mut expr: CheckedExpr, state: &mut State) {
        state.fill_generics(self.scopes, &mut expr.ty);
        if Self::has_side_effects(&expr) {
            self.gen_tmpvar_emit(expr, state);
        } else {
            self.gen_expr_inner(expr, state);
        }
    }

    fn gen_tmpvar_emit(&mut self, expr: CheckedExpr, state: &mut State) {
        tmpbuf_emit!(self, state, |tmp| {
            self.emit_type(&expr.ty);
            self.buffer.emit(format!(" {tmp} = "));
            self.gen_expr_inner(expr, state);
            self.buffer.emit(";");
        });
    }

    fn gen_tmpvar(&mut self, expr: CheckedExpr, state: &mut State) -> String {
        let tmp = state.tmpvar();
        self.emit_type(&expr.ty);
        self.buffer.emit(format!(" {tmp} = "));
        self.gen_expr_inner(expr, state);
        self.buffer.emit(";");
        tmp
    }

    fn gen_pattern(&mut self, state: &mut State, pattern: &CheckedPattern, src: &str, ty: &Type) {
        match pattern {
            CheckedPattern::UnionMember {
                pattern,
                variant,
                inner,
            } => {
                let src = deref(src, ty);
                let base = ty.strip_references();
                if let Some(inner) = base
                    .as_option_inner(self.scopes)
                    .filter(|inner| matches!(inner, Type::Ptr(_) | Type::MutPtr(_)))
                {
                    if variant == "Some" {
                        self.buffer.emit(format!("if ({src} != NULL) {{"));
                        if let Some(pattern) = pattern {
                            self.gen_irrefutable_pattern(
                                state,
                                pattern,
                                &src,
                                inner,
                                ty.is_any_ptr(),
                            );
                        }
                    } else {
                        self.buffer.emit(format!("if ({src} == NULL) {{"));
                    }
                } else {
                    let tag = base
                        .as_user()
                        .and_then(|ut| self.scopes.get(ut.id).data.as_union())
                        .and_then(|union| union.variant_tag(variant))
                        .unwrap();
                    self.buffer
                        .emit(format!("if ({src}.{UNION_TAG_NAME} == {tag}) {{"));

                    if let Some(pattern) = pattern {
                        self.gen_irrefutable_pattern(
                            state,
                            pattern,
                            &format!("{src}.{variant}"),
                            inner,
                            ty.is_any_ptr(),
                        );
                    }
                }
            }
            CheckedPattern::Irrefutable(pattern) => {
                self.buffer.emit("if (1) {");
                self.gen_irrefutable_pattern(state, pattern, src, ty, false);
            }
            CheckedPattern::Int(value) => {
                self.buffer.emit(format!("if ({} == ", deref(src, ty)));
                self.emit_cast(ty.strip_references());
                self.buffer.emit(format!("{value}) {{"));
            }
            CheckedPattern::IntRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                let src = deref(src, ty);
                let base = ty.strip_references();
                self.buffer.emit(format!("if ({src} >= "));
                self.emit_cast(base);
                self.buffer.emit(format!(
                    "{start} && {src} {} ",
                    if *inclusive { "<=" } else { "<" }
                ));
                self.emit_cast(base);
                self.buffer.emit(format!("{end}) {{"));
            }
            CheckedPattern::String(value) => {
                let src = deref(src, ty);
                self.buffer.emit(format!(
                    "if ({src}.span.len == {} && __builtin_memcmp({src}.span.ptr, \"",
                    value.len()
                ));
                for byte in value.as_bytes() {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer.emit(format!("\", {}) == 0) {{", value.len()));
            }
            CheckedPattern::Span {
                patterns,
                rest,
                inner,
            } => {
                let src = deref(src, ty);
                self.buffer.emit(format!(
                    "if ({src}.len {} {}) {{",
                    if rest.is_some() { ">=" } else { "==" },
                    patterns.len()
                ));
                let pos = rest.map(|RestPattern { id, pos }| {
                    if let Some(id) = id {
                        self.emit_local_decl(id, state);
                        self.buffer.emit(format!(
                            " = {{ .ptr = {src}.ptr + {pos}, .len = {src}.len - {} }};",
                            patterns.len()
                        ));
                    }
                    pos
                });

                for (i, patt) in patterns.iter().enumerate() {
                    self.gen_irrefutable_pattern(
                        state,
                        patt,
                        &if pos.is_some_and(|pos| i >= pos) {
                            format!("{src}.ptr[{src}.len - {} + {i}]", patterns.len())
                        } else {
                            format!("{src}.ptr[{i}]")
                        },
                        inner,
                        true,
                    );
                }
            }
            CheckedPattern::Destrucure(_) => todo!(),
            CheckedPattern::Array(_) => todo!(),
            CheckedPattern::Error => panic!("ICE: CheckedPattern::Error in gen_pattern"),
        }
    }

    fn gen_irrefutable_pattern(
        &mut self,
        state: &mut State,
        pattern: &IrrefutablePattern,
        src: &str,
        ty: &Type,
        borrow: bool,
    ) {
        match pattern {
            &IrrefutablePattern::Variable(id) => {
                self.emit_local_decl(id, state);
                self.buffer
                    .emit(format!(" = {}{src};", if borrow { "&" } else { "" }));
            }
            IrrefutablePattern::Destrucure(patterns) => {
                let deref = "*".repeat(ty.indirection());
                let borrow = borrow || ty.is_any_ptr();
                for (member, inner, patt) in patterns {
                    self.gen_irrefutable_pattern(
                        state,
                        patt,
                        &format!("({deref}{src}).{member}"),
                        inner,
                        borrow || inner.is_any_ptr(),
                    );
                }
            }
            IrrefutablePattern::Array(ArrayPattern {
                patterns,
                rest,
                arr_len,
                inner,
            }) => {
                let src = if ty.is_any_ptr() {
                    src.into()
                } else {
                    format!("{src}.{ARRAY_DATA_NAME}")
                };
                let borrow = borrow || ty.is_any_ptr();
                let rest = rest.map(|RestPattern { id, pos }| {
                    let rest_len = arr_len - patterns.len();
                    if let Some(id) = id {
                        self.emit_local_decl(id, state);
                        if ty.is_any_ptr() {
                            self.buffer.emit(format!(" = {src} + {pos};"));
                        } else {
                            self.buffer.emit(format!(" = {{ .{ARRAY_DATA_NAME} = {{"));
                            for i in 0..rest_len {
                                self.buffer.emit(format!("{src}[{}],", pos + i));
                            }
                            self.buffer.emit("}};");
                        }
                    }

                    (pos, rest_len)
                });

                for (mut i, patt) in patterns.iter().enumerate() {
                    if let Some((_, rest_len)) = rest.filter(|&(pos, _)| i >= pos) {
                        i += rest_len;
                    }
                    self.gen_irrefutable_pattern(
                        state,
                        patt,
                        &format!("{src}[{i}]"),
                        inner,
                        borrow || inner.is_any_ptr(),
                    );
                }
            }
        }
    }

    fn emit_block(&mut self, block: Vec<CheckedStmt>, state: &mut State) {
        let old = std::mem::take(&mut self.yielded);
        self.buffer.emit("{");
        for stmt in block.into_iter() {
            self.gen_stmt(stmt, state);
            if self.yielded {
                break;
            }
        }
        self.buffer.emit("}");
        self.yielded = old;
    }

    fn emit_type(&mut self, id: &Type) {
        self.buffer
            .emit_type(self.scopes, id, Some((&mut self.diag, &mut self.type_gen)));
    }

    fn emit_cast(&mut self, id: &Type) {
        self.buffer.emit("(");
        self.emit_type(id);
        self.buffer.emit(")");
    }

    fn emit_prototype(&mut self, state: &mut State, is_prototype: bool, real: bool) {
        let f = self.scopes.get(state.func.id);
        let mut ret = f.ret.clone();
        state.fill_generics(self.scopes, &mut ret);

        if f.is_extern {
            self.buffer.emit("extern ");
        } else {
            self.buffer.emit("static ");
        }

        if ret.is_never() {
            self.buffer.emit("_Noreturn ");
        }

        if real && Self::needs_fn_wrapper(f) {
            self.buffer.emit("void ");
        } else {
            self.emit_type(&ret);
            self.buffer.emit(" ");
        }
        self.emit_fn_name_2(state, real);
        self.buffer.emit("(");
        for (i, param) in f.params.iter().enumerate() {
            let mut ty = param.ty.clone();
            state.fill_generics(self.scopes, &mut ty);
            if i > 0 {
                self.buffer.emit(", ");
            }

            if !is_prototype && Self::needs_fn_wrapper(f) {
                self.emit_type(&ty);
                self.buffer.emit(format!(" $param{i}"));
            } else if f.is_extern || is_prototype {
                self.emit_type(&ty);
            } else if let ParamPattern::Checked(IrrefutablePattern::Variable(id)) = &param.patt {
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
                self.buffer.emit(", ...)");
            }
        } else if f.params.is_empty() {
            self.buffer.emit("void)");
        } else {
            self.buffer.emit(")");
        }
    }

    fn emit_var_name(&mut self, id: VariableId, state: &mut State) {
        use std::collections::hash_map::*;

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
        state.fill_generics(self.scopes, &mut ty);
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
        self.emit_fn_name_2(state, false)
    }

    fn emit_fn_name_2(&mut self, state: &State, real: bool) {
        let f = self.scopes.get(state.func.id);
        let is_macro = f.attrs.iter().any(|attr| attr.name.data == "c_macro");
        if !f.is_extern {
            if let Some(inst) = state.inst.as_ref() {
                self.buffer.emit_generic_mangled_name(self.scopes, inst);
                self.buffer.emit("_");
                self.buffer.emit(&f.name.data);
            } else {
                self.buffer
                    .emit(self.scopes.full_name(f.scope, &f.name.data));
            }

            if !state.func.ty_args.is_empty() {
                self.buffer.emit("$");
                for ty in state.func.ty_args.iter() {
                    self.buffer.emit("$");
                    self.buffer.emit_generic_mangled_name(self.scopes, ty);
                }
                self.buffer.emit("$$");
            }
        } else if let Some(attr) = f
            .attrs
            .iter()
            .find(|attr| attr.name.data == "c_name" && !attr.props.is_empty())
        {
            if !is_macro && !real {
                self.buffer.emit("$");
            }
            // TODO: emit error when c_name is placed on a non-extern function
            self.buffer.emit(&attr.props[0].name.data);
        } else {
            if !is_macro && !real {
                self.buffer.emit("$");
            }
            self.buffer.emit(&f.name.data);
        }
    }

    fn has_side_effects(expr: &CheckedExpr) -> bool {
        match &expr.data {
            CheckedExprData::Unary { op, .. } => matches!(
                op,
                UnaryOp::PostIncrement
                    | UnaryOp::PostDecrement
                    | UnaryOp::PreIncrement
                    | UnaryOp::PreDecrement
            ),
            CheckedExprData::Call { .. } => true,
            CheckedExprData::Assign { .. } => true,
            _ => false,
        }
    }

    fn is_lvalue(expr: &CheckedExpr) -> bool {
        matches!(
            &expr.data,
            CheckedExprData::Unary {
                op: UnaryOp::Deref,
                ..
            } | CheckedExprData::Symbol(_)
                | CheckedExprData::Member { .. }
                | CheckedExprData::Subscript { .. }
        )
    }

    fn find_implementation(
        ty: &Type,
        scopes: &Scopes,
        trait_id: UserTypeId,
        fn_name: &str,
    ) -> Option<FunctionId> {
        let search = |scope: ScopeId| {
            scopes[scope]
                .children
                .iter()
                .find(|s| matches!(scopes[s.id].kind, ScopeKind::Impl(id) if id == trait_id))
                .and_then(|scope| scopes.find_in(fn_name, scope.id))
                .map(|f| f.id)
        };

        if let Some(f) = ty
            .as_user()
            .and_then(|ut| search(scopes.get(ut.id).body_scope))
        {
            return Some(f);
        }

        // FIXME: this sucks
        scopes
            .extensions()
            .iter()
            .filter(|ext| ext.matches_type(ty))
            .find_map(|ext| search(ext.body_scope))
    }

    fn needs_fn_wrapper(f: &Function) -> bool {
        f.is_extern && f.body.is_none() && matches!(f.ret, Type::Void | Type::Never | Type::CVoid)
    }
}

fn is_opt_ptr(scopes: &Scopes, ty: &Type) -> bool {
    ty.as_option_inner(scopes)
        .map(|inner| inner.is_ptr() || inner.is_mut_ptr() || inner.is_fn_ptr())
        .unwrap_or(false)
}

fn deref(src: &str, ty: &Type) -> String {
    if ty.is_ptr() || ty.is_mut_ptr() {
        format!(
            "({}{src})",
            "*".repeat(ty.indirection() - usize::from(ty.strip_references().is_array()))
        )
    } else {
        src.into()
    }
}
