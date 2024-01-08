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
    error::Error,
    lexer::Span,
    sym::{
        CheckedMember, FunctionId, ParamPattern, ScopeId, ScopeKind, Scopes, UserTypeData,
        UserTypeId, VariableId,
    },
    typeid::{CInt, FnPtr, GenericFunc, GenericUserType, Type},
};

const UNION_TAG_NAME: &str = "$tag";
const ARRAY_DATA_NAME: &str = "$data";
const PARAM_PREFIX: &str = "$p";

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

struct TypeGen {
    structs: Option<HashSet<GenericUserType>>,
    fnptrs: Option<HashSet<FnPtr>>,
    arrays: HashMap<Type, HashSet<usize>>,
}

impl TypeGen {
    fn finish(&mut self, buffer: &mut Buffer, scopes: &Scopes) -> Result<(), Error> {
        let mut structs = HashMap::new();
        for ut in self.structs.take().unwrap() {
            Self::get_depencencies(scopes, ut, &mut structs);
        }

        let mut definitions = Buffer::default();
        // FIXME: generating structs could cause more function pointers to need to be generated
        for f in self.fnptrs.take().unwrap() {
            definitions.emit("typedef ");
            definitions.emit_type(scopes, &f.ret, self);
            definitions.emit("(*");
            definitions.emit_fnptr_name(scopes, &f);
            definitions.emit(")(");
            for (i, param) in f.params.iter().enumerate() {
                if i > 0 {
                    definitions.emit(", ");
                }

                definitions.emit_type(scopes, param, self);
            }
            definitions.emit(");");
        }

        let mut emitted_arrays = HashSet::new();
        for ut in Self::get_struct_order(scopes, &structs)? {
            let union = scopes.get(ut.id).data.as_union();
            let unsafe_union = union.as_ref().map_or(false, |union| union.is_unsafe);
            if unsafe_union {
                buffer.emit("typedef union ");
                definitions.emit("union ");
            } else {
                buffer.emit("typedef struct ");
                definitions.emit("struct ");
            }

            buffer.emit_type_name(scopes, ut);
            buffer.emit(" ");
            buffer.emit_type_name(scopes, ut);
            buffer.emit(";");

            definitions.emit_type_name(scopes, ut);
            definitions.emit("{");

            let members = scopes.get(ut.id).members().unwrap();
            if let Some(union) = union.filter(|_| !unsafe_union) {
                definitions.emit_type(scopes, &union.tag_type(), self);
                definitions.emit(format!(" {UNION_TAG_NAME};"));

                for member in members.iter().filter(|m| m.shared) {
                    self.emit_member(scopes, ut, member, &mut definitions);
                }

                definitions.emit(" union {");
                for member in members.iter().filter(|m| !m.shared) {
                    self.emit_member(scopes, ut, member, &mut definitions);
                }
                definitions.emit("};");
            } else {
                for member in members {
                    self.emit_member(scopes, ut, member, &mut definitions);
                }
            }

            definitions.emit("};");

            let ty = Type::User(ut.clone().into());
            if let Some(sizes) = self.arrays.remove(&ty) {
                for size in sizes {
                    self.emit_array(scopes, buffer, Some(&mut definitions), &ty, size);
                }
                emitted_arrays.insert(ty);
            }
        }

        for (ty, sizes) in std::mem::take(&mut self.arrays)
            .into_iter()
            .filter(|(ty, _)| !emitted_arrays.contains(ty))
        {
            for size in sizes {
                self.emit_array(scopes, buffer, None, &ty, size);
            }
        }

        buffer.emit(definitions.0);
        Ok(())
    }

    fn get_struct_order<'a>(
        scopes: &Scopes,
        structs: &'a HashMap<GenericUserType, Vec<GenericUserType>>,
    ) -> Result<Vec<&'a GenericUserType>, Error> {
        fn dfs<'a>(
            sid: &'a GenericUserType,
            structs: &'a HashMap<GenericUserType, Vec<GenericUserType>>,
            visited: &mut HashMap<&'a GenericUserType, bool>,
            result: &mut Vec<&'a GenericUserType>,
        ) -> Result<(), (&'a GenericUserType, &'a GenericUserType)> {
            visited.insert(sid, true);
            if let Some(deps) = structs.get(sid) {
                for dep in deps.iter() {
                    match visited.get(dep) {
                        Some(true) => return Err((dep, sid)),
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
            if !state.contains_key(sid) {
                dfs(sid, structs, &mut state, &mut result).map_err(|(a, b)| {
                    // TODO: figure out a real span here
                    Error::new(
                        format!(
                            "cyclic dependency detected between {} and {}.",
                            a.name(scopes),
                            b.name(scopes),
                        ),
                        Span::default(),
                    )
                })?;
            }
        }

        Ok(result)
    }

    fn get_depencencies(
        scopes: &Scopes,
        ut: GenericUserType,
        result: &mut HashMap<GenericUserType, Vec<GenericUserType>>,
    ) {
        if result.contains_key(&ut) {
            return;
        }

        let mut deps = Vec::new();
        for member in scopes.get(ut.id).members().unwrap().iter() {
            let mut ty = member.ty.clone();
            ty.fill_struct_templates(scopes, &ut);

            while let Type::Array(inner) = ty {
                ty = inner.0;
            }

            if let Type::User(data) = ty {
                if !data.ty_args.is_empty() {
                    Self::get_depencencies(scopes, (*data).clone(), result);
                }

                deps.push(*data);
            }
        }

        result.insert(ut, deps);
    }

    fn emit_member(
        &mut self,
        scopes: &Scopes,
        ut: &GenericUserType,
        member: &CheckedMember,
        buffer: &mut Buffer,
    ) {
        let mut ty = member.ty.clone();
        ty.fill_struct_templates(scopes, ut);
        if !ty.is_void_like() {
            buffer.emit_type(scopes, &ty, self);
            buffer.emit(format!(" {}", member.name));
            buffer.emit(";");
        }
    }

    fn emit_array(
        &mut self,
        scopes: &Scopes,
        typedef: &mut Buffer,
        defs: Option<&mut Buffer>,
        ty: &Type,
        size: usize,
    ) {
        typedef.emit("typedef struct ");
        typedef.emit_array_struct_name(scopes, ty, size);
        typedef.emit(" ");
        typedef.emit_array_struct_name(scopes, ty, size);
        typedef.emit(";");

        let defs = defs.unwrap_or(typedef);
        defs.emit("struct ");
        defs.emit_array_struct_name(scopes, ty, size);
        defs.emit(" { ");
        defs.emit_type(scopes, ty, self);
        defs.emit(format!(" {ARRAY_DATA_NAME}[{size}]; }};"));
    }
}

impl Default for TypeGen {
    fn default() -> Self {
        Self {
            structs: Some(HashSet::new()),
            fnptrs: Some(HashSet::new()),
            arrays: HashMap::new(),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Buffer(String);

impl Buffer {
    fn emit(&mut self, source: impl AsRef<str>) {
        self.0.push_str(source.as_ref());
    }

    fn emit_type(&mut self, scopes: &Scopes, id: &Type, tg: &mut TypeGen) {
        match id {
            Type::Void | Type::Never | Type::CVoid => self.emit("void"),
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
                    self.emit("/*");
                    self.emit_type(scopes, inner, tg);
                    self.emit("*/");
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
                if let Some(fnptrs) = tg.fnptrs.as_mut() {
                    fnptrs.insert((**f).clone());
                }
                self.emit_generic_mangled_name(scopes, id);
            }
            Type::User(ut) => match &scopes.get(ut.id).data {
                UserTypeData::Struct { .. } | UserTypeData::Union(_) => {
                    if is_opt_ptr(scopes, id) {
                        self.emit_type(scopes, &ut.ty_args[0], tg);
                    } else {
                        if let Some(structs) = tg.structs.as_mut() {
                            structs.insert((**ut).clone());
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
                match tg.arrays.entry(data.0.clone()) {
                    Entry::Occupied(mut entry) => {
                        entry.get_mut().insert(data.1);
                    }
                    Entry::Vacant(entry) => {
                        entry.insert([data.1].into());
                    }
                }
            }
            Type::Unknown(_) => panic!("ICE: TypeId::Unknown in emit_type"),
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
            Type::Unknown(_) => panic!("ICE: TypeId::Unknown in emit_generic_mangled_name"),
            Type::Array(data) => self.emit_array_struct_name(scopes, &data.0, data.1),
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

    fn emit_fn_name(&mut self, scopes: &Scopes, state: &State) {
        let f = scopes.get(state.func.id);
        if !f.is_extern {
            if let Some(inst) = state.inst.as_ref() {
                self.emit_generic_mangled_name(scopes, inst);
                self.emit("_");
                self.emit(&f.name.data);
            } else {
                self.emit(scopes.full_name(f.scope, &f.name.data));
            }

            if !state.func.ty_args.is_empty() {
                self.emit("$");
                for ty in state.func.ty_args.iter() {
                    self.emit("$");
                    self.emit_generic_mangled_name(scopes, ty);
                }
                self.emit("$$");
            }
        } else if let Some(attr) = f
            .attrs
            .iter()
            .find(|attr| attr.name.data == "c_name" && !attr.props.is_empty())
        {
            // TODO: emit error when c_name is placed on a non-extern function
            self.emit(&attr.props[0].name.data);
        } else {
            self.emit(&f.name.data);
        }
    }

    fn emit_type_name(&mut self, scopes: &Scopes, ut: &GenericUserType) {
        let ty = scopes.get(ut.id);
        if ty.data.is_template() {
            panic!("ICE: Template type in emit_type_name");
        }

        self.emit(scopes.full_name(ty.scope, &ty.name));
        if !ut.ty_args.is_empty() {
            self.emit("$");
            for ty in ut.ty_args.iter() {
                self.emit("$");
                self.emit_generic_mangled_name(scopes, ty);
            }
            self.emit("$$");
        }
    }

    fn emit_prototype(
        &mut self,
        scopes: &Scopes,
        state: &mut State,
        is_prototype: bool,
        tg: &mut TypeGen,
    ) {
        let f = scopes.get(state.func.id);
        let mut ret = f.ret.clone();
        state.fill_generics(scopes, &mut ret);

        if f.is_extern {
            self.emit("extern ");
        } else {
            self.emit("static ");
        }

        if ret.is_never() {
            self.emit("_Noreturn ");
        }

        self.emit_type(scopes, &ret, tg);
        self.emit(" ");
        self.emit_fn_name(scopes, state);
        self.emit("(");
        for (i, param) in f.params.iter().enumerate() {
            let mut ty = param.ty.clone();
            state.fill_generics(scopes, &mut ty);
            if ty.is_void_like() {
                continue;
            }

            if i > 0 {
                self.emit(", ");
            }

            if f.is_extern || is_prototype {
                self.emit_type(scopes, &ty, tg);
            } else if let ParamPattern::Checked(IrrefutablePattern::Variable(id)) = &param.patt {
                _ = self.emit_local_decl(scopes, *id, state, tg);
                continue;
            } else {
                self.emit_type(scopes, &ty, tg);
                self.emit(format!(" {PARAM_PREFIX}{}", param.label));
            }
        }

        if f.variadic {
            if f.params.is_empty() {
                self.emit("...)");
            } else {
                self.emit(", ...)");
            }
        } else if f.params.is_empty() {
            self.emit("void)");
        } else {
            self.emit(")");
        }
    }

    fn emit_var_name(&mut self, scopes: &Scopes, id: VariableId, state: &mut State) {
        use std::collections::hash_map::*;

        let var = scopes.get(id);
        if var.is_static {
            self.emit(scopes.full_name(var.scope, &var.name));
        } else {
            match state.emitted_names.entry(var.name.clone()) {
                Entry::Occupied(entry) if *entry.get() == id => {
                    self.emit("$");
                    self.emit(&var.name);
                }
                Entry::Occupied(_) => {
                    let len = state.renames.len();
                    self.emit(
                        state
                            .renames
                            .entry(id)
                            .or_insert_with(|| format!("$r{len}")),
                    );
                }
                Entry::Vacant(entry) => {
                    entry.insert(id);
                    self.emit("$");
                    self.emit(&var.name);
                }
            }
        }
    }

    fn emit_local_decl(
        &mut self,
        scopes: &Scopes,
        id: VariableId,
        state: &mut State,
        tg: &mut TypeGen,
    ) -> Result<Type, Type> {
        let var = scopes.get(id);
        let mut ty = var.ty.clone();
        state.fill_generics(scopes, &mut ty);
        if !ty.is_void_like() {
            if var.is_static {
                self.emit("static ");
            }

            self.emit_type(scopes, &ty, tg);
            if !var.mutable && !var.is_static {
                self.emit(" const");
            }
            self.emit(" ");
            self.emit_var_name(scopes, id, state);
            Ok(ty)
        } else {
            Err(ty)
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

macro_rules! enter_block {
    ($self: expr, $state: expr, $scopes: expr, $ty: expr, |$tmp: ident| $body: block) => {{
        let ty = $ty;
        let old = std::mem::replace(&mut $self.cur_block, $state.tmpvar());
        tmpbuf!($self, $state, |$tmp| {
            if !ty.is_void_like() {
                $self.emit_type($scopes, ty);
                $self.buffer.emit(format!(" {};", $self.cur_block));
            }
            $body;
        });

        let var = std::mem::replace(&mut $self.cur_block, old);
        if !ty.is_void_like() {
            $self.buffer.emit(var);
        }
    }};
}

macro_rules! stmt {
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

#[derive(Default)]
pub struct Codegen {
    buffer: Buffer,
    temporaries: Buffer,
    type_gen: TypeGen,
    funcs: HashSet<State>,
    cur_block: String,
    cur_loop: String,
    yielded: bool,
}

impl Codegen {
    pub fn build(scope: ScopeId, scopes: &Scopes) -> Result<String, Error> {
        let main = &mut State::new(
            GenericFunc::new(
                *scopes
                    .find_in("main", scope)
                    .ok_or(Error::new("no main function found", Span::default()))?,
                Vec::new(),
            ),
            None,
        );
        let mut this = Self {
            funcs: [main.clone()].into(),
            ..Self::default()
        };

        let conv_argv = (scopes.get(main.func.id).params.len() == 1).then(|| {
            let id = scopes
                .find_in("convert_argv", *scopes.find_module("std").unwrap())
                .unwrap();

            let state = State::new(GenericFunc::new(*id, vec![]), None);
            this.funcs.insert(state.clone());
            state
        });

        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        while !this.funcs.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for mut state in diff {
                this.gen_fn(scopes, &mut state, &mut prototypes);
            }
        }

        let functions = std::mem::take(&mut this.buffer);

        let mut vars = Vec::new();
        for &id in scopes
            .scopes()
            .iter()
            .flat_map(|s| s.vars.iter())
            .filter(|&&v| scopes.get(*v).is_static)
        {
            if this
                .buffer
                .emit_local_decl(scopes, *id, main, &mut this.type_gen)
                .is_ok()
            {
                this.buffer.emit(";");
                vars.push((*id, false));
            } else {
                vars.push((*id, true));
            }
        }
        let statics = std::mem::take(&mut this.buffer);

        this.buffer.emit(include_str!("../ctl/ctl.h"));
        this.type_gen.finish(&mut this.buffer, scopes)?;
        this.buffer.emit(prototypes.0);
        this.buffer.emit(statics.0);
        this.buffer.emit(functions.0);
        this.buffer.emit("int main(int argc, char **argv) {");
        this.buffer.emit("GC_INIT();");
        this.buffer.emit("setlocale(LC_ALL, \"C.UTF-8\");");
        for (id, void) in vars {
            if !void {
                this.buffer.emit_var_name(scopes, id, main);
                this.buffer.emit(" = ");
            }
            this.gen_expr(scopes, scopes.get(id).value.clone().unwrap(), main);
            this.buffer.emit(";");
        }

        let returns = scopes.get(main.func.id).ret != Type::Void;
        if let Some(state) = conv_argv {
            if returns {
                this.buffer.emit("return ");
            }
            this.buffer.emit_fn_name(scopes, main);
            this.buffer.emit("(");
            this.buffer.emit_fn_name(scopes, &state);
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
            this.buffer.emit_fn_name(scopes, main);
            if returns {
                this.buffer.emit("(); }");
            } else {
                this.buffer.emit("(); return 0; }");
            }
        }

        Ok(this.buffer.0)
    }

    fn gen_fn(&mut self, scopes: &Scopes, state: &mut State, prototypes: &mut Buffer) {
        // TODO: emit an error if a function has the c_macro attribute and a body
        let func = scopes.get(state.func.id);
        if func.attrs.iter().any(|attr| attr.name.data == "c_macro") {
            return;
        }

        prototypes.emit_prototype(scopes, state, true, &mut self.type_gen);
        prototypes.emit(";");

        if let Some(body) = func.body.clone() {
            self.buffer
                .emit_prototype(scopes, state, false, &mut self.type_gen);
            self.buffer.emit("{");
            for param in func.params.iter() {
                let Some(patt) = param
                    .patt
                    .as_checked()
                    .filter(|patt| !matches!(patt, IrrefutablePattern::Variable(_)))
                else {
                    continue;
                };
                self.gen_irrefutable_pattern(
                    scopes,
                    state,
                    patt,
                    &format!("{PARAM_PREFIX}{}", param.label),
                    &param.ty,
                    false,
                    false,
                );
            }

            for stmt in body.into_iter() {
                self.gen_stmt(scopes, stmt, state);
            }
            self.buffer.emit("}");
        }
    }

    fn gen_stmt(&mut self, scopes: &Scopes, stmt: CheckedStmt, state: &mut State) {
        match stmt {
            CheckedStmt::Module(block) => {
                for stmt in block.body.into_iter() {
                    self.gen_stmt(scopes, stmt, state);
                }
            }
            CheckedStmt::Expr(mut expr) => {
                stmt!(self, {
                    state.fill_generics(scopes, &mut expr.ty);
                    self.gen_expr_inner(scopes, expr, state);
                    self.buffer.emit(";");
                });
            }
            CheckedStmt::Let(id) => {
                stmt!(self, {
                    let var = scopes.get(id);
                    match self
                        .buffer
                        .emit_local_decl(scopes, id, state, &mut self.type_gen)
                    {
                        Ok(ty) => {
                            if let Some(mut expr) = var.value.clone() {
                                expr.ty = ty;
                                self.buffer.emit(" = ");
                                self.gen_expr_inner(scopes, expr, state);
                            }
                        }
                        Err(ty) => {
                            if let Some(mut expr) = var.value.clone() {
                                expr.ty = ty;
                                self.gen_expr_inner(scopes, expr, state);
                            }
                        }
                    }

                    self.buffer.emit(";");
                });
            }
            CheckedStmt::LetPattern(pattern, value) => {
                stmt!(self, {
                    let tmp = state.tmpvar();
                    let ty = &mut value.ty.clone();
                    state.fill_generics(scopes, ty);

                    self.emit_type(scopes, ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.gen_expr(scopes, value, state);
                    self.buffer.emit(";");
                    self.gen_irrefutable_pattern(scopes, state, &pattern, &tmp, ty, false, false);
                });
            }
            CheckedStmt::None => {}
            CheckedStmt::Error => {
                panic!("ICE: CheckedStmt::Error in gen_stmt");
            }
        }
    }

    fn gen_expr_inner(&mut self, scopes: &Scopes, expr: CheckedExpr, state: &mut State) {
        match expr.data {
            CheckedExprData::Binary { op, left, right } => {
                if expr.ty == Type::Bool {
                    self.emit_cast(scopes, &expr.ty);
                    self.buffer.emit("(");
                }
                self.buffer.emit("(");
                self.gen_expr(scopes, *left, state);
                self.buffer.emit(format!(" {op} "));
                self.gen_expr(scopes, *right, state);
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
                    self.buffer.emit("+");
                    self.gen_expr(scopes, *inner, state);
                }
                UnaryOp::Neg => {
                    self.buffer.emit("-");
                    self.gen_expr(scopes, *inner, state);
                }
                UnaryOp::PostIncrement => {
                    self.gen_expr(scopes, *inner, state);
                    self.buffer.emit("++");
                }
                UnaryOp::PostDecrement => {
                    self.gen_expr(scopes, *inner, state);
                    self.buffer.emit("--");
                }
                UnaryOp::PreIncrement => {
                    self.buffer.emit("++");
                    self.gen_expr(scopes, *inner, state);
                }
                UnaryOp::PreDecrement => {
                    self.buffer.emit("--");
                    self.gen_expr(scopes, *inner, state);
                }
                UnaryOp::Not => {
                    if inner.ty == Type::Bool {
                        self.emit_cast(scopes, &expr.ty);
                        self.buffer.emit("(");
                        self.gen_expr(scopes, *inner, state);
                        self.buffer.emit(" ^ 1)");
                    } else {
                        self.buffer.emit("~");
                        self.gen_expr(scopes, *inner, state);
                    }
                }
                UnaryOp::Deref => {
                    self.buffer.emit("(*");
                    self.gen_expr(scopes, *inner, state);
                    self.buffer.emit(")");
                }
                UnaryOp::Addr | UnaryOp::AddrMut => {
                    // TODO: addr of void
                    state.fill_generics(scopes, &mut inner.ty);

                    let array = inner.ty.is_array();
                    if inner.ty.is_void_like() {
                        self.emit_cast(scopes, &expr.ty);
                        self.buffer.emit("(0xdeadbeef);");
                    } else if !array {
                        self.buffer.emit("&");
                    }

                    match inner.data {
                        CheckedExprData::Unary {
                            op: UnaryOp::Deref, ..
                        } => {
                            self.gen_expr(scopes, *inner, state);
                        }
                        CheckedExprData::Symbol(_)
                        | CheckedExprData::Member { .. }
                        | CheckedExprData::Subscript { .. } => {
                            self.gen_expr(scopes, *inner, state);
                        }
                        _ => {
                            self.gen_to_tmp(scopes, *inner, state);
                        }
                    }

                    if array {
                        self.buffer.emit(format!(".{ARRAY_DATA_NAME}"));
                    }
                }
                UnaryOp::Unwrap => panic!("ICE: UnaryOp::Unwrap in gen_expr"),
                UnaryOp::Try => todo!(),
            },
            CheckedExprData::Call {
                mut func,
                args,
                mut inst,
                trait_id,
            } => {
                if let Some(inst) = inst.as_mut() {
                    state.fill_generics(scopes, inst);
                }

                let original_id = func.id;
                if let Some(trait_id) = trait_id {
                    let f = scopes.get(func.id);
                    let inst = inst
                        .as_ref()
                        .expect("generating trait function with no instance");
                    func.id = Self::find_implementation(inst, scopes, trait_id, &f.name.data)
                        .expect(
                            "generating trait fn with no corresponding implementation in instance",
                        );
                }

                for ty in func.ty_args.iter_mut() {
                    state.fill_generics(scopes, ty);
                }

                if let Some(name) = scopes.intrinsic_name(func.id) {
                    self.gen_intrinsic(scopes, name, &func);
                    return;
                }

                let mut args: IndexMap<_, _> = args
                    .into_iter()
                    .filter_map(|(name, mut expr)| {
                        state.fill_generics(scopes, &mut expr.ty);
                        let valid = !expr.ty.is_void_like();
                        // TODO: dont emit temporaries for expressions that cant have side effects
                        tmpbuf!(self, state, |tmp| {
                            if valid {
                                self.emit_type(scopes, &expr.ty);
                                self.buffer.emit(format!(" {tmp} = "));
                            }
                            self.gen_expr_inner(scopes, expr, state);
                            self.buffer.emit(";");
                            valid.then_some((name, tmp))
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
                self.buffer.emit_fn_name(scopes, &next_state);
                self.buffer.emit("(");
                scopes
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
                self.gen_expr(scopes, *expr, state);
                self.buffer.emit(")(");
                for (i, arg) in args.into_iter().enumerate() {
                    if i > 0 {
                        self.buffer.emit(", ");
                    }

                    self.gen_expr(scopes, arg, state);
                }
                self.buffer.emit(")");
            }
            CheckedExprData::Array(exprs) => {
                self.buffer.emit("(");
                self.emit_type(scopes, &expr.ty);
                self.buffer.emit(format!("){{.{ARRAY_DATA_NAME}={{"));
                for expr in exprs {
                    self.gen_expr(scopes, expr, state);
                    self.buffer.emit(",");
                }
                self.buffer.emit("}}");
            }
            CheckedExprData::ArrayWithInit { init, count } => {
                self.buffer.emit("(");
                self.emit_type(scopes, &expr.ty);
                self.buffer.emit(format!("){{.{ARRAY_DATA_NAME}={{"));
                for _ in 0..count {
                    self.gen_expr(scopes, (*init).clone(), state);
                    self.buffer.emit(",");
                }
                self.buffer.emit("}}");
            }
            CheckedExprData::Vec(exprs) => {
                let tmp = tmpbuf!(self, state, |tmp| {
                    let arr = state.tmpvar();
                    let len = exprs.len();
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let inner = &ut.ty_args[0];

                    self.emit_type(scopes, inner);
                    self.buffer.emit(format!(" {arr}[{len}] = {{"));
                    for expr in exprs {
                        self.gen_expr(scopes, expr, state);
                        self.buffer.emit(",");
                    }
                    self.buffer.emit("};");

                    let next_state = State::new(
                        GenericFunc::new(
                            *scopes
                                .find_in("with_capacity", scopes.get(ut.id).body_scope)
                                .unwrap(),
                            vec![inner.clone()],
                        ),
                        state.inst.clone(),
                    );

                    self.emit_type(scopes, &expr.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.buffer.emit_fn_name(scopes, &next_state);
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
                    self.funcs.insert(next_state);
                    tmp
                });
                self.buffer.emit(tmp);
            }
            CheckedExprData::VecWithInit { init, count } => {
                let tmp = tmpbuf!(self, state, |tmp| {
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let inner = &ut.ty_args[0];
                    let len = state.tmpvar();
                    self.emit_type(scopes, &count.ty);
                    self.buffer.emit(format!(" {len} = "));
                    self.gen_expr(scopes, *count, state);
                    self.buffer.emit(";");

                    let next_state = State::new(
                        GenericFunc::new(
                            *scopes
                                .find_in("with_capacity", scopes.get(ut.id).body_scope)
                                .unwrap(),
                            vec![inner.clone()],
                        ),
                        state.inst.clone(),
                    );

                    self.emit_type(scopes, &expr.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.buffer.emit_fn_name(scopes, &next_state);
                    self.buffer
                        .emit(format!("({len}); for (usize i = 0; i < {len}; i++) {{ (("));
                    self.emit_type(scopes, inner);
                    self.buffer.emit(format!(" *){tmp}.ptr.addr)[i] = "));
                    self.gen_expr(scopes, *init, state);
                    self.buffer.emit(format!("; }} {tmp}.len = {len};"));

                    self.funcs.insert(next_state);
                    tmp
                });
                self.buffer.emit(tmp);
            }
            CheckedExprData::Set(exprs) => {
                let tmp = tmpbuf!(self, state, |tmp| {
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let body = scopes.get(ut.id).body_scope;
                    let with_capacity = State::new(
                        GenericFunc::new(
                            *scopes.find_in("with_capacity", body).unwrap(),
                            vec![ut.ty_args[0].clone()],
                        ),
                        state.inst.clone(),
                    );

                    self.emit_type(scopes, &expr.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.buffer.emit_fn_name(scopes, &with_capacity);
                    self.buffer.emit(format!("({});", exprs.len()));
                    let insert = State::new(
                        GenericFunc::new(*scopes.find_in("insert", body).unwrap(), vec![]),
                        Some(expr.ty.clone()),
                    );

                    for val in exprs {
                        self.buffer.emit_fn_name(scopes, &insert);
                        self.buffer.emit(format!("(&{tmp}, "));
                        self.gen_expr(scopes, val, state);
                        self.buffer.emit(");");
                    }
                    self.funcs.insert(with_capacity);
                    self.funcs.insert(insert);
                    tmp
                });
                self.buffer.emit(tmp);
            }
            CheckedExprData::Map(exprs) => {
                let tmp = tmpbuf!(self, state, |tmp| {
                    let ut = (**expr.ty.as_user().unwrap()).clone();
                    let body = scopes.get(ut.id).body_scope;
                    let with_capacity = State::new(
                        GenericFunc::new(
                            *scopes.find_in("with_capacity", body).unwrap(),
                            vec![ut.ty_args[0].clone(), ut.ty_args[1].clone()],
                        ),
                        state.inst.clone(),
                    );

                    let insert = State::new(
                        GenericFunc::new(*scopes.find_in("insert", body).unwrap(), vec![]),
                        Some(expr.ty.clone()),
                    );

                    self.emit_type(scopes, &expr.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.buffer.emit_fn_name(scopes, &with_capacity);
                    self.buffer.emit(format!("({});", exprs.len()));

                    for (key, val) in exprs {
                        self.buffer.emit_fn_name(scopes, &insert);
                        self.buffer.emit(format!("(&{tmp}, "));
                        self.gen_expr(scopes, key, state);
                        self.buffer.emit(",");
                        self.gen_expr(scopes, val, state);
                        self.buffer.emit(");");
                    }
                    self.funcs.insert(with_capacity);
                    self.funcs.insert(insert);
                    tmp
                });
                self.buffer.emit(tmp);
            }
            CheckedExprData::Bool(value) => {
                self.emit_cast(scopes, &expr.ty);
                self.buffer.emit(if value { "1" } else { "0" })
            }
            CheckedExprData::Integer(value) => {
                self.emit_cast(scopes, &expr.ty);
                self.buffer.emit(format!("{value}"));
            }
            CheckedExprData::Float(value) => {
                self.emit_cast(scopes, &expr.ty);
                self.buffer.emit(value);
            }
            CheckedExprData::String(value) => {
                self.emit_cast(scopes, &expr.ty);
                self.buffer.emit("{ .span = { .ptr = (");
                self.emit_type(scopes, &Type::Ptr(Type::Uint(8).into()));
                self.buffer.emit(")\"");

                for byte in value.as_bytes() {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer
                    .emit(format!("\", .len = (usize){} }} }}", value.len()));
            }
            CheckedExprData::ByteString(value) => {
                self.buffer.emit("(");
                self.emit_type(scopes, &Type::Ptr(Type::Uint(8).into()));
                self.buffer.emit(")\"");
                for byte in value.as_bytes() {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer.emit("\"");
            }
            CheckedExprData::Char(value) => {
                self.emit_cast(scopes, &expr.ty);
                self.buffer.emit(format!("0x{:x}", value as u32));
            }
            CheckedExprData::Void => {}
            CheckedExprData::Symbol(symbol) => match symbol {
                Symbol::Func(func) => {
                    let state = State::new(func, None);
                    self.buffer.emit_fn_name(scopes, &state);
                    self.funcs.insert(state);
                }
                Symbol::Var(id) => {
                    if !scopes.get(id).ty.is_void_like() {
                        self.buffer.emit_var_name(scopes, id, state);
                    }
                }
            },
            CheckedExprData::Instance {
                mut members,
                variant,
            } => {
                if is_opt_ptr(scopes, &expr.ty) {
                    if let Some(some) = members.remove("Some") {
                        self.gen_expr(scopes, some, state);
                    } else {
                        self.buffer.emit(" NULL");
                    }
                } else {
                    self.emit_cast(scopes, &expr.ty);
                    self.buffer.emit("{");
                    if let Some((name, union)) = variant.zip(
                        expr.ty
                            .as_user()
                            .and_then(|ut| scopes.get(ut.id).data.as_union()),
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
                        state.fill_generics(scopes, &mut value.ty);
                        if !value.ty.is_void_like() {
                            self.buffer.emit(format!(".{name} = "));
                            self.gen_expr(scopes, value, state);
                            self.buffer.emit(", ");
                        }
                    }
                    self.buffer.emit("}");
                }
            }
            CheckedExprData::Member { source, member } => {
                if !expr.ty.is_void_like() {
                    self.gen_expr(scopes, *source, state);
                    self.buffer.emit(format!(".{member}"));
                }
            }
            CheckedExprData::Assign {
                target,
                binary,
                value,
            } => {
                if !expr.ty.is_void_like() {
                    self.gen_expr(scopes, *target, state);
                    if let Some(binary) = binary {
                        self.buffer.emit(format!(" {binary}= "));
                    } else {
                        self.buffer.emit(" = ");
                    }
                    self.gen_expr(scopes, *value, state);
                } else {
                    self.gen_expr(scopes, *target, state);
                    self.buffer.emit(";");

                    self.gen_expr(scopes, *value, state);
                    self.buffer.emit(";");
                }
            }
            CheckedExprData::Block(block) => {
                enter_block!(self, state, scopes, &expr.ty, |_tmp| {
                    self.emit_block(scopes, block.body, state);
                });
            }
            CheckedExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                enter_block!(self, state, scopes, &expr.ty, |_tmp| {
                    self.buffer.emit("if (");
                    self.gen_expr(scopes, *cond, state);
                    self.buffer.emit(") {");
                    stmt!(self, {
                        if !expr.ty.is_void_like() {
                            self.buffer.emit(format!("{} = ", self.cur_block));
                        }
                        self.gen_expr(scopes, *if_branch, state);
                    });

                    if let Some(else_branch) = else_branch {
                        self.buffer.emit("; } else {");
                        stmt!(self, {
                            if !expr.ty.is_void_like() {
                                self.buffer.emit(format!("{} = ", self.cur_block));
                            }
                            self.gen_expr(scopes, *else_branch, state);
                        });
                    }

                    self.buffer.emit("; }");
                })
            }
            CheckedExprData::Loop {
                cond,
                body,
                do_while,
            } => {
                let mut emit = false;
                let tmp = tmpbuf!(self, state, |tmp| {
                    let old = std::mem::replace(&mut self.cur_loop, tmp);
                    if !expr.ty.is_void_like() {
                        self.emit_type(scopes, &expr.ty);
                        self.buffer.emit(format!(" {};", self.cur_loop));
                        emit = true;
                    }

                    self.buffer.emit("for (;;) {");

                    macro_rules! cond {
                        () => {
                            if let Some(cond) = cond {
                                stmt! {
                                    self,
                                    {
                                        self.buffer.emit("if (!");
                                        self.gen_expr(scopes, *cond, state);
                                        self.buffer.emit(") { break; }");
                                    }
                                }
                            }
                        };
                    }

                    if !do_while {
                        cond!();
                        self.emit_block(scopes, body.body, state);
                    } else {
                        self.emit_block(scopes, body.body, state);
                        cond!();
                    }

                    self.buffer.emit("}");
                    std::mem::replace(&mut self.cur_loop, old)
                });
                if emit {
                    self.buffer.emit(tmp);
                }
            }
            CheckedExprData::For { iter, patt, body } => {
                let mut emit = false;
                let tmp = tmpbuf!(self, state, |tmp| {
                    let old = std::mem::replace(&mut self.cur_loop, tmp);
                    if !expr.ty.is_void_like() {
                        self.emit_type(scopes, &expr.ty);
                        self.buffer.emit(format!(" {};", self.cur_loop));
                        emit = true;
                    }
                    let mut iter_ty = iter.ty.clone();
                    state.fill_generics(scopes, &mut iter_ty);

                    let next = scopes
                        .lang_types
                        .get("iter")
                        .copied()
                        .and_then(|id| Self::find_implementation(&iter.ty, scopes, id, "next"))
                        .expect("for iterator should actually implement Iterator");
                    let next_state =
                        State::new(GenericFunc::new(next, vec![]), Some(iter_ty.clone()));
                    let mut next_ty = scopes.get(next).ret.clone();
                    if let Some(ut) = iter_ty.as_user() {
                        next_ty.fill_struct_templates(scopes, ut);
                    }

                    let iter_var = tmpbuf!(self, state, |tmp| {
                        self.buffer.emit_type(scopes, &iter_ty, &mut self.type_gen);
                        self.buffer.emit(format!(" {tmp} = "));
                        self.gen_expr(scopes, *iter, state);
                        self.buffer.emit(";");

                        tmp
                    });

                    self.buffer.emit("for (;;) {");

                    let item = state.tmpvar();
                    self.emit_type(scopes, &next_ty);
                    self.buffer.emit(format!(" {item} = "));
                    self.buffer.emit_fn_name(scopes, &next_state);
                    self.buffer.emit(format!("(&{iter_var});"));
                    self.gen_pattern(
                        scopes,
                        state,
                        &CheckedPattern::UnionMember {
                            pattern: Some(patt),
                            variant: "Some".into(),
                            inner: next_ty.as_option_inner(scopes).unwrap().clone(),
                        },
                        &item,
                        &next_ty,
                    );
                    self.emit_block(scopes, body, state);
                    self.buffer.emit("} else { break; } } ");

                    self.funcs.insert(next_state);
                    std::mem::replace(&mut self.cur_loop, old)
                });
                if emit {
                    self.buffer.emit(tmp);
                }
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
                            self.gen_expr(scopes, *expr, state);
                        }
                        _ => {
                            self.gen_expr(scopes, *callee, state);
                            self.buffer.emit(format!(".{ARRAY_DATA_NAME}"));
                        }
                    }
                } else {
                    self.buffer
                        .emit(format!("({}", "*".repeat(callee.ty.indirection() - 1)));
                    self.gen_expr(scopes, *callee, state);
                    self.buffer.emit(")");
                }

                self.buffer.emit("[");
                for arg in args {
                    self.gen_expr(scopes, arg, state);
                }
                self.buffer.emit("]");
            }
            CheckedExprData::Return(expr) => {
                // TODO: when return is used as anything except a StmtExpr, we will have to change
                // the generated code to accomodate it
                self.buffer.emit("return ");
                self.gen_expr(scopes, *expr, state);
                //self.yielded = true;
            }
            CheckedExprData::Yield(expr) => {
                if !expr.ty.is_void_like() {
                    self.buffer.emit(format!("{} = ", self.cur_block));
                }
                self.gen_expr(scopes, *expr, state);
                self.buffer.emit(";");
                //self.yielded = true;
            }
            CheckedExprData::Break(expr) => {
                if !expr.ty.is_void_like() {
                    self.buffer.emit(format!("{} = ", self.cur_loop));
                }
                self.gen_expr(scopes, *expr, state);
                self.buffer.emit("; break;");
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
                enter_block!(self, state, scopes, &expr.ty, |tmp| {
                    // TODO: update to exclude void when literal patterns are implemented
                    state.fill_generics(scopes, &mut scrutinee.ty);
                    let ty = scrutinee.ty.clone();

                    self.emit_type(scopes, &scrutinee.ty);
                    self.buffer.emit(format!(" {tmp} = "));
                    self.gen_expr(scopes, *scrutinee, state);
                    self.buffer.emit(";");

                    for (i, (pattern, expr)) in body.into_iter().enumerate() {
                        if i > 0 {
                            self.buffer.emit("else ");
                        }

                        self.gen_pattern(scopes, state, &pattern, &tmp, &ty);

                        stmt!(self, {
                            self.gen_expr(scopes, expr, state);
                            self.buffer.emit("; }");
                        });
                    }

                    self.buffer.emit("else { UNREACHABLE(); }");
                })
            }
            CheckedExprData::As(inner, _) => {
                self.emit_cast(scopes, &expr.ty);
                self.buffer.emit("(");
                self.gen_expr(scopes, *inner, state);
                self.buffer.emit(")");
            }
            CheckedExprData::Error => panic!("ICE: ExprData::Error in gen_expr"),
            CheckedExprData::Lambda(_) => todo!(),
        }
    }

    fn gen_intrinsic(&mut self, scopes: &Scopes, name: &str, func: &GenericFunc) {
        match name {
            "size_of" => {
                self.buffer.emit("(usize)sizeof");
                self.emit_cast(scopes, &func.ty_args[0]);
            }
            _ => unreachable!(),
        }
    }

    fn gen_expr(&mut self, scopes: &Scopes, mut expr: CheckedExpr, state: &mut State) {
        state.fill_generics(scopes, &mut expr.ty);
        if Self::has_side_effects(&expr) {
            if !expr.ty.is_void_like() {
                self.gen_to_tmp(scopes, expr, state);
            } else {
                self.gen_expr_inner(scopes, expr, state);
                self.buffer.emit(";");
            }
        } else {
            self.gen_expr_inner(scopes, expr, state);
        }
    }

    fn gen_to_tmp(&mut self, scopes: &Scopes, expr: CheckedExpr, state: &mut State) {
        let tmp = tmpbuf!(self, state, |tmp| {
            self.emit_type(scopes, &expr.ty);
            self.buffer.emit(format!(" {tmp} = "));
            self.gen_expr_inner(scopes, expr, state);
            self.buffer.emit(";");
            tmp
        });
        self.buffer.emit(tmp);
    }

    fn gen_pattern(
        &mut self,
        scopes: &Scopes,
        state: &mut State,
        pattern: &CheckedPattern,
        src: &str,
        ty: &Type,
    ) {
        match pattern {
            CheckedPattern::UnionMember {
                pattern,
                variant,
                inner: member_ty,
            } => {
                let src = deref(src, ty);
                let base = ty.strip_references();
                if let Some(inner) = base
                    .as_option_inner(scopes)
                    .filter(|inner| matches!(inner, Type::Ptr(_) | Type::MutPtr(_)))
                {
                    if variant == "Some" {
                        self.buffer.emit(format!("if ({src} != NULL) {{"));
                        if let Some(pattern) = pattern {
                            self.gen_irrefutable_pattern(
                                scopes, state, pattern, &src, inner, false, false,
                            );
                        }
                    } else {
                        self.buffer.emit(format!("if ({src} == NULL) {{"));
                    }
                } else {
                    let tag = base
                        .as_user()
                        .and_then(|ut| scopes.get(ut.id).data.as_union())
                        .and_then(|union| union.variant_tag(variant))
                        .unwrap();
                    self.buffer
                        .emit(format!("if ({src}.{UNION_TAG_NAME} == {tag}) {{"));

                    if let Some(pattern) = pattern {
                        self.gen_irrefutable_pattern(
                            scopes,
                            state,
                            pattern,
                            &format!("{src}.{variant}"),
                            member_ty,
                            ty.is_ptr() || ty.is_mut_ptr(),
                            false,
                        );
                    }
                }
            }
            CheckedPattern::Irrefutable(pattern) => {
                self.buffer.emit("if (1) {");
                self.gen_irrefutable_pattern(scopes, state, pattern, src, ty, false, false);
            }
            CheckedPattern::Int(value) => {
                self.buffer.emit(format!("if ({} == ", deref(src, ty)));
                self.emit_cast(scopes, ty.strip_references());
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
                self.emit_cast(scopes, base);
                self.buffer.emit(format!(
                    "{start} && {src} {} ",
                    if *inclusive { "<=" } else { "<" }
                ));
                self.emit_cast(scopes, base);
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
                if let Some(RestPattern { id, pos }) = *rest {
                    if id
                        .and_then(|id| {
                            self.buffer
                                .emit_local_decl(scopes, id, state, &mut self.type_gen)
                                .ok()
                        })
                        .is_some()
                    {
                        self.buffer.emit(format!(
                            " = {{ .ptr = {src}.ptr + {pos}, .len = {src}.len - {} }};",
                            patterns.len()
                        ));
                    }

                    for (i, patt) in patterns.iter().enumerate() {
                        self.gen_irrefutable_pattern(
                            scopes,
                            state,
                            patt,
                            &if i < pos {
                                format!("{src}.ptr + {i}")
                            } else {
                                format!("{src}.ptr + {src}.len - {} + {i}", patterns.len())
                            },
                            inner,
                            false,
                            false,
                        );
                    }
                } else {
                    for (i, patt) in patterns.iter().enumerate() {
                        self.gen_irrefutable_pattern(
                            scopes,
                            state,
                            patt,
                            &format!("{src}.ptr + {i}"),
                            inner,
                            false,
                            false,
                        );
                    }
                }
            }
            CheckedPattern::Destrucure(_) => todo!(),
            CheckedPattern::Array(_) => todo!(),
            CheckedPattern::Error => panic!("ICE: CheckedPattern::Error in gen_pattern"),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn gen_irrefutable_pattern(
        &mut self,
        scopes: &Scopes,
        state: &mut State,
        pattern: &IrrefutablePattern,
        src: &str,
        ty: &Type,
        borrow: bool,
        recursive: bool,
    ) {
        match pattern {
            &IrrefutablePattern::Variable(id) => {
                if self
                    .buffer
                    .emit_local_decl(scopes, id, state, &mut self.type_gen)
                    .is_ok()
                {
                    self.buffer
                        .emit(format!(" = {}{src};", if borrow { "&" } else { "" }));
                }
            }
            IrrefutablePattern::Destrucure(patterns) => {
                for (member, inner, patt) in patterns {
                    self.gen_irrefutable_pattern(
                        scopes,
                        state,
                        patt,
                        &format!(
                            "({}{src}).{member}",
                            "*".repeat(ty.indirection() - usize::from(borrow))
                        ),
                        inner,
                        ty.is_ptr() || ty.is_mut_ptr(),
                        true,
                    );
                }
            }
            IrrefutablePattern::Array(ArrayPattern {
                patterns,
                rest,
                arr_len,
                inner,
            }) => {
                let from_ptr = ty.is_ptr() || ty.is_mut_ptr();
                // for recursive calls, src will be an actually pointer to the array type, not a
                // pointer to the inner type like we emit for normal array pointers
                let src = if (!recursive || !borrow) && from_ptr {
                    src.into()
                } else {
                    format!("{src}.{ARRAY_DATA_NAME}")
                };
                if let Some(RestPattern { id, pos }) = *rest {
                    let rest_len = arr_len - patterns.len();
                    if id
                        .and_then(|id| {
                            self.buffer
                                .emit_local_decl(scopes, id, state, &mut self.type_gen)
                                .ok()
                        })
                        .is_some()
                    {
                        if from_ptr {
                            self.buffer.emit(format!(" = {src} + {pos};"));
                        } else {
                            self.buffer.emit(format!(" = {{ .{ARRAY_DATA_NAME} = {{"));
                            for i in 0..rest_len {
                                self.buffer.emit(format!("{src}[{}],", pos + i));
                            }
                            self.buffer.emit("}};");
                        }
                    }

                    for (i, patt) in patterns.iter().enumerate() {
                        let i = if i < pos { i } else { rest_len + i };
                        self.gen_irrefutable_pattern(
                            scopes,
                            state,
                            patt,
                            &format!("{src}[{i}]"),
                            inner,
                            from_ptr,
                            true,
                        );
                    }
                } else {
                    for (i, patt) in patterns.iter().enumerate() {
                        self.gen_irrefutable_pattern(
                            scopes,
                            state,
                            patt,
                            &format!("{src}[{i}]"),
                            inner,
                            from_ptr,
                            true,
                        );
                    }
                }
            }
        }
    }

    fn emit_block(&mut self, scopes: &Scopes, block: Vec<CheckedStmt>, state: &mut State) {
        let old = std::mem::take(&mut self.yielded);
        self.buffer.emit("{");
        for stmt in block.into_iter() {
            self.gen_stmt(scopes, stmt, state);
            if self.yielded {
                break;
            }
        }
        self.buffer.emit("}");
        self.yielded = old;
    }

    fn emit_type(&mut self, scopes: &Scopes, id: &Type) {
        self.buffer.emit_type(scopes, id, &mut self.type_gen);
    }

    fn emit_cast(&mut self, scopes: &Scopes, id: &Type) {
        self.buffer.emit("(");
        self.emit_type(scopes, id);
        self.buffer.emit(")");
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
}

fn is_opt_ptr(scopes: &Scopes, ty: &Type) -> bool {
    ty.as_option_inner(scopes)
        .map(|inner| inner.is_ptr() || inner.is_mut_ptr())
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
