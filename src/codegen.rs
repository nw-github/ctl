use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;

use crate::{
    ast::expr::UnaryOp,
    checked_ast::{CheckedExpr, CheckedPattern, CheckedStmt, ExprData},
    lexer::Span,
    typecheck::{
        CheckedParam, GenericFunc, GenericUserType, Member, ScopeId, Scopes, Symbol, TypeId,
        VariableId,
    },
    Error,
};

const UNION_TAG_NAME: &str = "$tag";

#[derive(PartialEq, Eq, Clone)]
struct State {
    func: GenericFunc,
    inst: Option<TypeId>,
    tmpvar: usize,
    emitted_names: HashMap<String, VariableId>,
    renames: HashMap<VariableId, String>,
}

impl State {
    pub fn new(func: GenericFunc, inst: Option<TypeId>) -> Self {
        Self {
            func,
            inst,
            tmpvar: 0,
            emitted_names: Default::default(),
            renames: Default::default(),
        }
    }

    pub fn fill_generics(&self, scopes: &Scopes, ty: &mut TypeId) {
        ty.fill_func_generics(scopes, &self.func);

        if let Some(inst) = self.inst.as_ref().and_then(|inst| inst.as_user_type()) {
            ty.fill_type_generics(scopes, inst);
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

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Buffer(String);

impl Buffer {
    fn emit(&mut self, source: impl AsRef<str>) {
        self.0.push_str(source.as_ref());
    }

    fn emit_type(&mut self, scopes: &Scopes, id: &TypeId) {
        match id {
            TypeId::Void => self.emit("void"),
            TypeId::Never => self.emit("void"),
            TypeId::CVoid => self.emit("void"),
            TypeId::Int(bits) | TypeId::Uint(bits) => {
                let unsigned = matches!(id, TypeId::Uint(_));
                if (8..=64).contains(bits) && bits.is_power_of_two() {
                    if unsigned {
                        self.emit(format!("uint{bits}_t"));
                    } else {
                        self.emit(format!("int{bits}_t"));
                    }
                } else if unsigned {
                    self.emit(format!("CTL_UBITINT({bits})"));
                } else {
                    self.emit(format!("CTL_SBITINT({bits})"));
                }
            }
            TypeId::CInt(ty) | TypeId::CUint(ty) => {
                if matches!(id, TypeId::CUint(_)) {
                    self.emit("unsigned ");
                }

                match ty {
                    crate::typecheck::CInt::Char => self.emit("char"),
                    crate::typecheck::CInt::Short => self.emit("short"),
                    crate::typecheck::CInt::Int => self.emit("int"),
                    crate::typecheck::CInt::Long => self.emit("long"),
                    crate::typecheck::CInt::LongLong => self.emit("long long"),
                }
            }
            TypeId::Isize => self.emit("isize"),
            TypeId::Usize => self.emit("usize"),
            TypeId::F32 => self.emit("f32"),
            TypeId::F64 => self.emit("f64"),
            TypeId::Bool => self.emit("CTL_bool"),
            TypeId::Char => self.emit("CTL_char"),
            TypeId::IntGeneric | TypeId::FloatGeneric => {
                panic!("ICE: Int/FloatGeneric in emit_type");
            }
            TypeId::Ptr(inner) => {
                self.emit_type(scopes, inner);
                self.emit(" const*");
            }
            TypeId::MutPtr(inner) => {
                self.emit_type(scopes, inner);
                self.emit(" *");
            }
            TypeId::Func(_) => todo!(),
            TypeId::UserType(ut) => {
                let tmp = scopes.get_user_type(ut.id);
                if tmp.data.is_func_generic() || tmp.data.is_struct_generic() {
                    panic!("ICE: Template type in emit_type");
                }

                if is_opt_ptr(scopes, id) {
                    self.emit_type(scopes, &ut.generics[0]);
                } else {
                    self.emit_type_name(scopes, ut);
                }
            }
            TypeId::Unknown(_) => panic!("ICE: TypeId::Unknown in emit_type"),
            TypeId::Array(_) => todo!(),
            TypeId::TraitSelf => panic!("ICE: TypeId::TraitSelf in emit_type"),
        }
    }

    fn emit_cast(&mut self, scopes: &Scopes, id: &TypeId) {
        self.emit("(");
        self.emit_type(scopes, id);
        self.emit(")");
    }

    fn emit_generic_mangled_name(&mut self, scopes: &Scopes, id: &TypeId) {
        match id {
            TypeId::Void => self.emit("void"),
            TypeId::CVoid => self.emit("c_void"),
            TypeId::Never => self.emit("never"),
            TypeId::Int(bits) => self.emit(format!("i{bits}")),
            TypeId::Uint(bits) => self.emit(format!("u{bits}")),
            TypeId::CInt(ty) | TypeId::CUint(ty) => {
                if matches!(id, TypeId::CUint(_)) {
                    self.emit("u");
                }
                match ty {
                    crate::typecheck::CInt::Char => self.emit("char"),
                    crate::typecheck::CInt::Short => self.emit("short"),
                    crate::typecheck::CInt::Int => self.emit("int"),
                    crate::typecheck::CInt::Long => self.emit("long"),
                    crate::typecheck::CInt::LongLong => self.emit("longlong"),
                }
            }
            TypeId::Isize => self.emit("isize"),
            TypeId::Usize => self.emit("usize"),
            TypeId::F32 => self.emit("f32"),
            TypeId::F64 => self.emit("f64"),
            TypeId::Bool => self.emit("bool"),
            TypeId::Char => self.emit("char"),
            TypeId::IntGeneric | TypeId::FloatGeneric => {
                panic!("ICE: Int/FloatGeneric in emit_generic_mangled_name");
            }
            TypeId::Ptr(inner) => {
                self.emit("ptr_");
                self.emit_generic_mangled_name(scopes, inner);
            }
            TypeId::MutPtr(inner) => {
                self.emit("mutptr_");
                self.emit_generic_mangled_name(scopes, inner);
            }
            TypeId::Func(_) => todo!(),
            TypeId::UserType(ut) => {
                self.emit_type_name(scopes, ut);
            }
            TypeId::Unknown(_) => panic!("ICE: TypeId::Unknown in emit_generic_mangled_name"),
            TypeId::Array(_) => todo!(),
            TypeId::TraitSelf => panic!("ICE: TypeId::TraitSelf in emit_generic_mangled_name"),
        }
    }

    fn emit_fn_name(&mut self, scopes: &Scopes, state: &State) {
        let f = scopes.get_func(state.func.id);
        if !f.is_extern {
            if let Some(inst) = state.inst.as_ref() {
                self.emit_generic_mangled_name(scopes, inst);
                self.emit("_");
                self.emit(&f.name);
            } else {
                self.emit(scopes.full_name(f.scope, &f.name));
            }

            if !state.func.generics.is_empty() {
                self.emit("$");
                for ty in state.func.generics.iter() {
                    self.emit("$");
                    self.emit_generic_mangled_name(scopes, ty);
                }
                self.emit("$$");
            }
        } else {
            self.emit(&f.name);
        }
    }

    fn emit_type_name(&mut self, scopes: &Scopes, ut: &GenericUserType) {
        let ty = scopes.get_user_type(ut.id);
        if ty.data.is_func_generic() || ty.data.is_struct_generic() {
            panic!("ICE: Template type in emit_type_name");
        }

        self.emit(scopes.full_name(ty.scope, &ty.name));
        if !ut.generics.is_empty() {
            self.emit("$");
            for ty in ut.generics.iter() {
                self.emit("$");
                self.emit_generic_mangled_name(scopes, ty);
            }
            self.emit("$$");
        }
    }

    fn emit_prototype(&mut self, scopes: &Scopes, state: &mut State, is_prototype: bool) {
        let f = scopes.get_func(state.func.id);
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

        self.emit_type(scopes, &ret);
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
                self.emit_type(scopes, &ty);
                if !param.mutable {
                    self.emit(" const");
                }
            } else {
                _ = self.emit_local_decl(
                    scopes,
                    *scopes[f.body_scope]
                        .vars
                        .iter()
                        .find(|&&v| scopes.get_var(v).name == param.name)
                        .unwrap(),
                    state,
                );
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

        let var = scopes.get_var(id);
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
    ) -> Result<TypeId, TypeId> {
        let var = scopes.get_var(id);
        let mut ty = var.ty.clone();
        state.fill_generics(scopes, &mut ty);
        if !ty.is_void_like() {
            if var.is_static {
                self.emit("static ");
            }

            self.emit_type(scopes, &ty);
            if !var.mutable {
                self.emit(" const");
            }
            self.emit(" ");
            self.emit_var_name(scopes, id, state);
            Ok(ty)
        } else {
            Err(ty)
        }
    }
}

macro_rules! tmpbuf {
    ($self: expr, $body: expr) => {{
        let buffer = std::mem::take(&mut $self.buffer);

        $body;

        std::mem::replace(&mut $self.buffer, buffer)
    }};
}

macro_rules! enter_block {
    ($self: expr, $state: expr, $scopes: expr, $ty: expr, $body: expr) => {{
        let ty = $ty;
        let old = std::mem::replace(&mut $self.cur_block, $state.tmpvar());
        let written = tmpbuf! {
            $self,
            {
                if !ty.is_void_like() {
                    $self.buffer.emit_type($scopes, ty);
                    $self.buffer.emit(format!(" {};", $self.cur_block));
                }
                $body;
            }
        };

        $self.temporaries.emit(written.0);
        let var = std::mem::replace(&mut $self.cur_block, old);
        if !ty.is_void_like() {
            $self.buffer.emit(var);
        }
    }};
}

macro_rules! stmt {
    ($self: expr, $body: expr) => {
        let old = std::mem::take(&mut $self.temporaries);
        let written = tmpbuf! { $self, $body };

        $self
            .buffer
            .emit(std::mem::replace(&mut $self.temporaries, old).0);
        $self.buffer.emit(written.0);
    };
}

#[derive(Default)]
pub struct Codegen {
    buffer: Buffer,
    temporaries: Buffer,
    structs: HashSet<GenericUserType>,
    funcs: HashSet<State>,
    cur_block: String,
    cur_loop: String,
    yielded: bool,
}

impl Codegen {
    pub fn build(scope: ScopeId, scopes: &Scopes) -> Result<String, Error> {
        let Some(main) = scopes.find_func_in("main", scope) else {
            return Err(Error::new(
                "no main function found",
                Span::default(),
            ));
        };

        let main = &mut State::new(GenericFunc::new(main, Vec::new()), None);
        let mut this = Self {
            funcs: [main.clone()].into(),
            ..Self::default()
        };

        let conv_argv = (scopes.get_func(main.func.id).params.len() == 1).then(|| {
            let id = scopes
                .find_func_in("convert_argv", scopes.find_module("std").unwrap())
                .unwrap();

            let state = State::new(GenericFunc::new(id, vec![]), None);
            this.funcs.insert(state.clone());
            state
        });

        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        while !this.funcs.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for mut state in diff {
                prototypes.emit_prototype(scopes, &mut state, true);
                prototypes.emit(";");

                if let Some(body) = scopes.get_func(state.func.id).body.clone() {
                    this.buffer.emit_prototype(scopes, &mut state, false);
                    this.emit_block(scopes, body, &mut state);
                }
            }
        }

        let functions = std::mem::take(&mut this.buffer);

        this.buffer.emit(include_str!("../ctl/ctl.h"));
        this.emit_structs(scopes, prototypes.0)?;

        let mut statics = Vec::new();
        for &id in scopes
            .scopes()
            .iter()
            .flat_map(|s| s.vars.iter())
            .filter(|&&v| scopes.get_var(v).is_static)
        {
            if this.buffer.emit_local_decl(scopes, id, main).is_ok() {
                this.buffer.emit(";");
                statics.push((id, false));
            } else {
                statics.push((id, true));
            }
        }

        this.buffer.emit(functions.0);
        this.buffer.emit("int main(int argc, char **argv) {");
        this.buffer.emit("GC_INIT();");
        for (id, void) in statics {
            if !void {
                this.buffer.emit_var_name(scopes, id, main);
                this.buffer.emit(" = ");
            }
            this.gen_expr(scopes, scopes.get_var(id).value.clone().unwrap(), main);
            this.buffer.emit(";");
        }

        if let Some(state) = conv_argv {
            this.buffer.emit("return ");
            this.buffer.emit_fn_name(scopes, main);
            this.buffer.emit("(");
            this.buffer.emit_fn_name(scopes, &state);
            this.buffer.emit("(argc, (const char **)argv)); }");
        } else {
            this.buffer.emit("(void)argc;");
            this.buffer.emit("(void)argv;");
            this.buffer.emit("return ");
            this.buffer.emit_fn_name(scopes, main);
            this.buffer.emit("(); }");
        }

        Ok(this.buffer.0)
    }

    fn gen_stmt(&mut self, scopes: &Scopes, stmt: CheckedStmt, state: &mut State) {
        match stmt {
            CheckedStmt::Module(block) => {
                for stmt in block.body.into_iter() {
                    self.gen_stmt(scopes, stmt, state);
                }
            }
            CheckedStmt::Expr(mut expr) => {
                stmt! {
                    self,
                    {
                        state.fill_generics(scopes, &mut expr.ty);
                        self.gen_expr_inner(scopes, expr, state);
                        self.buffer.emit(";");
                    }
                }
            }
            CheckedStmt::Let(id) => {
                stmt! {
                    self,
                    {
                        let var = scopes.get_var(id);
                        match self.buffer.emit_local_decl(scopes, id, state) {
                            Ok(ty) => if let Some(mut expr) = var.value.clone() {
                                expr.ty = ty;
                                self.buffer.emit(" = ");
                                self.gen_expr_inner(scopes, expr, state);
                            }
                            Err(ty) => if let Some(mut expr) = var.value.clone() {
                                expr.ty = ty;
                                self.gen_expr_inner(scopes, expr, state);
                            }
                        }

                        self.buffer.emit(";");
                    }
                }
            }
            CheckedStmt::None => {}
            CheckedStmt::Error => {
                panic!("ICE: CheckedStmt::Error in gen_stmt");
            }
        }
    }

    fn gen_expr_inner(&mut self, scopes: &Scopes, expr: CheckedExpr, state: &mut State) {
        match expr.data {
            ExprData::Binary { op, left, right } => {
                if expr.ty == TypeId::Bool {
                    self.buffer.emit_cast(scopes, &expr.ty);
                    self.buffer.emit("(");
                }
                self.buffer.emit("(");
                self.gen_expr(scopes, *left, state);
                self.buffer.emit(format!(" {op} "));
                self.gen_expr(scopes, *right, state);
                self.buffer.emit(")");

                if expr.ty == TypeId::Bool {
                    self.buffer.emit(" ? 1 : 0)");
                }
            }
            ExprData::Unary {
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
                    if inner.ty == TypeId::Bool {
                        self.buffer.emit_cast(scopes, &expr.ty);
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
                    self.buffer.emit("&");
                    match inner.data {
                        ExprData::Unary { op, .. } if op == UnaryOp::Deref => {
                            self.gen_expr(scopes, *inner, state);
                        }
                        ExprData::Symbol(_)
                        | ExprData::Member { .. }
                        | ExprData::Subscript { .. } => {
                            self.gen_expr(scopes, *inner, state);
                        }
                        _ => {
                            state.fill_generics(scopes, &mut inner.ty);
                            self.gen_to_tmp(scopes, *inner, state);
                        }
                    }
                }
                UnaryOp::Unwrap => panic!("ICE: UnaryOp::Unwrap in gen_expr"),
                UnaryOp::Try => todo!(),
            },
            ExprData::Call {
                mut func,
                args,
                mut inst,
                trait_fn,
            } => {
                if let Some(inst) = inst.as_mut() {
                    state.fill_generics(scopes, inst);
                }

                let original_id = func.id;
                if trait_fn {
                    let f = scopes.get_func(func.id);
                    if let Some(ut) = inst.as_ref().and_then(|i| i.as_user_type()) {
                        let ut = scopes.get_user_type(ut.id);
                        func.id = scopes.find_func_in(&f.name, ut.body_scope).unwrap();
                    } else {
                        todo!("trait implementations for non-struct types");
                    }
                }

                for ty in func.generics.iter_mut() {
                    state.fill_generics(scopes, ty);
                }

                if scopes.scopes()[0]
                    .children
                    .get("core")
                    .and_then(|&scope| scopes[scope].children.get("mem"))
                    .and_then(|&scope| scopes.find_func_in("size_of", scope))
                    .filter(|&id| id == func.id)
                    .is_some()
                {
                    self.buffer.emit("(usize)sizeof");
                    self.buffer.emit_cast(scopes, &func.generics[0]);
                    if let TypeId::UserType(ty) = &func.generics[0] {
                        self.structs.insert((**ty).clone());
                    }
                    return;
                }

                let args = Self::make_positional(&scopes.get_func(original_id).params, args);

                let next_state = State::new(func, inst);
                self.buffer.emit_fn_name(scopes, &next_state);
                self.funcs.insert(next_state);

                // FIXME: keyword arguments can be specified out of order. this evaluates them in
                // parameter order instead of argument order.
                self.buffer.emit("(");

                for (i, mut arg) in args.into_iter().enumerate() {
                    state.fill_generics(scopes, &mut arg.ty);
                    if arg.ty.is_void_like() {
                        continue;
                    }

                    if i > 0 {
                        self.buffer.emit(", ");
                    }

                    self.gen_expr(scopes, arg, state);
                }
                self.buffer.emit(")");
            }
            ExprData::Array(_) => todo!(),
            ExprData::ArrayWithInit { .. } => todo!(),
            ExprData::Vec(exprs) => {
                let ut = (**expr.ty.as_user_type().unwrap()).clone();

                let inner = &ut.generics[0];
                let tmp = state.tmpvar();
                let len = exprs.len();
                let written = tmpbuf! {
                    self,
                    {
                        let arr = state.tmpvar();

                        self.buffer.emit_type(scopes, inner);
                        self.buffer.emit(format!(" {arr}[{len}] = {{"));
                        for expr in exprs {
                            self.gen_expr(scopes, expr, state);
                            self.buffer.emit(",");
                        }
                        self.buffer.emit("};");

                        let next_state = State::new(
                            GenericFunc::new(
                                scopes
                                    .find_func_in("with_capacity", scopes.get_user_type(ut.id).body_scope)
                                    .unwrap(),
                                vec![inner.clone()],
                            ),
                            state.inst.clone(),
                        );

                        self.buffer.emit_type(scopes, &expr.ty);
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
                    }
                };

                self.temporaries.emit(written.0);
                self.buffer.emit(tmp);

                if let Some(ut) = inner.as_user_type() {
                    self.structs.insert((**ut).clone());
                }
                self.structs.insert(ut);
            }
            ExprData::VecWithInit { init, count } => {
                let ut = (**expr.ty.as_user_type().unwrap()).clone();

                let inner = &ut.generics[0];
                let tmp = state.tmpvar();
                let written = tmpbuf! {
                    self,
                    {
                        let len = state.tmpvar();
                        self.buffer.emit_type(scopes, &count.ty);
                        self.buffer.emit(format!(" {len} = "));
                        self.gen_expr(scopes, *count, state);
                        self.buffer.emit(";");

                        let next_state = State::new(
                            GenericFunc::new(
                                scopes
                                    .find_func_in("with_capacity", scopes.get_user_type(ut.id).body_scope)
                                    .unwrap(),
                                vec![inner.clone()],
                            ),
                            state.inst.clone(),
                        );

                        self.buffer.emit_type(scopes, &expr.ty);
                        self.buffer.emit(format!(" {tmp} = "));
                        self.buffer.emit_fn_name(scopes, &next_state);
                        self.buffer.emit(format!("({len}); for (usize i = 0; i < {len}; i++) {{ (("));
                        self.buffer.emit_type(scopes, inner);
                        self.buffer.emit(format!(" *){tmp}.ptr.addr)[i] = "));
                        self.gen_expr(scopes, *init, state);
                        self.buffer.emit(format!("; }} {tmp}.len = {len};"));

                        self.funcs.insert(next_state);
                    }
                };

                self.temporaries.emit(written.0);
                self.buffer.emit(tmp);

                if let Some(ut) = inner.as_user_type() {
                    self.structs.insert((**ut).clone());
                }
                self.structs.insert(ut);
            }
            ExprData::Map(exprs) => {
                let ut = (**expr.ty.as_user_type().unwrap()).clone();

                let key = &ut.generics[0];
                let val = &ut.generics[1];
                let tmp = state.tmpvar();
                let written = tmpbuf! {
                    self,
                    {
                        let with_capacity = State::new(
                            GenericFunc::new(
                                scopes
                                    .find_func_in("with_capacity", scopes.get_user_type(ut.id).body_scope)
                                    .unwrap(),
                                vec![key.clone(), val.clone()],
                            ),
                            state.inst.clone(),
                        );

                        let insert = State::new(
                            GenericFunc::new(
                                scopes
                                    .find_func_in("insert", scopes.get_user_type(ut.id).body_scope)
                                    .unwrap(),
                                vec![],
                            ),
                            Some(expr.ty.clone()),
                        );

                        self.buffer.emit_type(scopes, &expr.ty);
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
                    }
                };

                self.temporaries.emit(written.0);
                self.buffer.emit(tmp);

                if let Some(ut) = key.as_user_type() {
                    self.structs.insert((**ut).clone());
                }
                if let Some(ut) = val.as_user_type() {
                    self.structs.insert((**ut).clone());
                }
                self.structs.insert(ut);
            }
            ExprData::Bool(value) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit(if value { "1" } else { "0" })
            }
            ExprData::Signed(value) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit(format!("{value}"));
            }
            ExprData::Unsigned(value) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit(format!("{value}"));
            }
            ExprData::Float(value) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit(value);
            }
            ExprData::String(value) => {
                self.structs
                    .insert((**expr.ty.as_user_type().unwrap()).clone());

                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit("{ .span = { .ptr = (uint8_t const*)\"");
                for byte in value.as_bytes() {
                    self.buffer.emit(format!("\\x{byte:x}"));
                }
                self.buffer
                    .emit(format!("\", .len = (usize){} }} }}", value.len()));
            }
            ExprData::Char(value) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                if value as u32 <= 127 {
                    self.buffer.emit(format!("'{value}'"));
                } else {
                    self.buffer.emit(format!("{:#x}", value as u32));
                }
            }
            ExprData::Void => {}
            ExprData::Symbol(symbol) => match symbol {
                Symbol::Func => {
                    let func = expr.ty.into_func().unwrap();
                    self.buffer.emit_fn_name(scopes, &State::new(*func, None))
                }
                Symbol::Var(id) => {
                    if !scopes.get_var(id).ty.is_void_like() {
                        self.buffer.emit_var_name(scopes, id, state);
                    }
                }
            },
            ExprData::Instance {
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
                    self.buffer.emit_cast(scopes, &expr.ty);
                    self.buffer.emit("{");
                    if let Some((name, union)) = variant.zip(
                        expr.ty
                            .as_user_type()
                            .and_then(|ut| scopes.get_user_type(ut.id).data.as_union()),
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

                    if let TypeId::UserType(data) = expr.ty {
                        self.structs.insert((*data).clone());
                    } else {
                        panic!(
                            "ICE: Constructing instance of non-struct type {}!",
                            expr.ty.name(scopes)
                        );
                    }
                }
            }
            ExprData::Member { source, member } => {
                if !expr.ty.is_void_like() {
                    self.gen_expr(scopes, *source, state);
                    self.buffer.emit(format!(".{member}"));
                }
            }
            ExprData::Assign {
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
            ExprData::Block(block) => {
                enter_block! {
                    self, state, scopes, &expr.ty,
                    {
                        self.emit_block(scopes, block.body, state);
                    }
                }
            }
            ExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                enter_block! {
                    self, state, scopes, &expr.ty,
                    {
                        self.buffer.emit("if (");
                        self.gen_expr(scopes, *cond, state);
                        self.buffer.emit(") {");
                        stmt! {
                            self,
                            {
                                if !expr.ty.is_void_like() {
                                    self.buffer.emit(format!("{} = ", self.cur_block));
                                }
                                self.gen_expr(scopes, *if_branch, state);
                            }
                        }

                        if let Some(else_branch) = else_branch {
                            self.buffer.emit("; } else {");
                            stmt! {
                                self,
                                {
                                    if !expr.ty.is_void_like() {
                                        self.buffer.emit(format!("{} = ", self.cur_block));
                                    }
                                    self.gen_expr(scopes, *else_branch, state);
                                }
                            }
                        }

                        self.buffer.emit("; }");
                    }
                }
            }
            ExprData::Loop {
                cond,
                iter,
                body,
                do_while,
            } => {
                let old = std::mem::replace(&mut self.cur_loop, state.tmpvar());
                let written = tmpbuf! {
                    self,
                    {
                        if !expr.ty.is_void_like() {
                            self.buffer.emit_type(scopes, &expr.ty);
                            self.buffer.emit(format!(" {};", self.cur_loop));
                        }

                        if let Some(iter) = iter {
                            let mut expr = scopes.get_var(iter).value.clone().unwrap();
                            let (Ok(ty) | Err(ty)) = self.buffer.emit_local_decl(scopes, iter, state);
                            expr.ty = ty;

                            self.buffer.emit(" = ");
                            self.gen_expr_inner(scopes, expr, state);
                            self.buffer.emit(";");
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
                    }
                };

                self.temporaries.emit(written.0);
                if !expr.ty.is_void_like() {
                    self.buffer.emit(std::mem::replace(&mut self.cur_loop, old));
                }
            }
            ExprData::Subscript { .. } => todo!(),
            ExprData::Return(expr) => {
                // TODO: when return is used as anything except a StmtExpr, we will have to change
                // the generated code to accomodate it
                self.buffer.emit("return ");
                self.gen_expr(scopes, *expr, state);
                //self.yielded = true;
            }
            ExprData::Yield(expr) => {
                if !expr.ty.is_void_like() {
                    self.buffer.emit(format!("{} = ", self.cur_block));
                }
                self.gen_expr(scopes, *expr, state);
                self.buffer.emit(";");
                //self.yielded = true;
            }
            ExprData::Break(expr) => {
                if !expr.ty.is_void_like() {
                    self.buffer.emit(format!("{} = ", self.cur_loop));
                }
                self.gen_expr(scopes, *expr, state);
                self.buffer.emit("; break;");
                //self.yielded = true;
            }
            ExprData::Continue => {
                self.buffer.emit("continue;");
                //self.yielded = true;
            }
            ExprData::Match {
                expr: mut scrutinee,
                body,
            } => {
                enter_block! {
                    self, state, scopes, &expr.ty,
                    {
                        // TODO: update to exclude void when literal patterns are implemented
                        let tmp_name = state.tmpvar();
                        state.fill_generics(scopes, &mut scrutinee.ty);
                        let ty = scrutinee.ty.clone();

                        self.buffer.emit_type(scopes, &scrutinee.ty);
                        self.buffer.emit(format!(" {tmp_name} = "));
                        self.gen_expr(scopes, *scrutinee, state);
                        self.buffer.emit(";");

                        for (i, (pattern, expr)) in body.into_iter().enumerate() {
                            if i > 0 {
                                self.buffer.emit("else ");
                            }

                            self.gen_pattern(
                                scopes,
                                state,
                                &pattern,
                                &tmp_name,
                                &ty,
                            );

                            stmt! {
                                self,
                                {
                                    self.gen_expr(scopes, expr, state);
                                    self.buffer.emit("; }");
                                }
                            }
                        }
                    }
                }
            }
            ExprData::As(inner, _) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit("(");
                self.gen_expr(scopes, *inner, state);
                self.buffer.emit(")");
            }
            ExprData::Error => panic!("ICE: ExprData::Error in gen_expr"),
        }
    }

    fn gen_expr(&mut self, scopes: &Scopes, mut expr: CheckedExpr, state: &mut State) {
        fn has_side_effects(expr: &CheckedExpr) -> bool {
            match &expr.data {
                ExprData::Unary { op, .. } => matches!(
                    op,
                    UnaryOp::PostIncrement
                        | UnaryOp::PostDecrement
                        | UnaryOp::PreIncrement
                        | UnaryOp::PreDecrement
                ),
                ExprData::Call { .. } => true,
                ExprData::Array(_) => todo!(),
                ExprData::ArrayWithInit { .. } => todo!(),
                ExprData::Assign { .. } => true,
                ExprData::Subscript { .. } => todo!(),
                _ => false,
            }
        }

        state.fill_generics(scopes, &mut expr.ty);
        if has_side_effects(&expr) {
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
        let tmp = state.tmpvar();
        let written = tmpbuf! {
            self,
            {
                self.buffer.emit_type(scopes, &expr.ty);
                self.buffer.emit(format!(" {tmp} = "));
                self.gen_expr_inner(scopes, expr, state);
                self.buffer.emit(";");
            }
        };

        self.temporaries.emit(written.0);
        self.buffer.emit(tmp);
    }

    fn gen_pattern(
        &mut self,
        scopes: &Scopes,
        state: &mut State,
        pattern: &CheckedPattern,
        tmp_name: &str,
        mut scrutinee: &TypeId,
    ) {
        match pattern {
            CheckedPattern::UnionMember {
                binding,
                variant,
                ptr,
            } => {
                let mut count = 0;
                while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = scrutinee {
                    scrutinee = inner;
                    count += 1;
                }

                let opt_ptr = is_opt_ptr(scopes, scrutinee.strip_references());
                let tmp_name = format!("({}{tmp_name})", "*".repeat(count));
                if opt_ptr && variant.0 == "Some" {
                    self.buffer.emit(format!("if ({tmp_name} != NULL) {{"));
                    if binding
                        .filter(|&id| self.buffer.emit_local_decl(scopes, id, state).is_ok())
                        .is_some()
                    {
                        self.buffer
                            .emit(format!(" = {}{tmp_name};", if *ptr { "&" } else { "" }));
                    }
                } else if opt_ptr && variant.0 == "None" {
                    self.buffer.emit(format!("if ({tmp_name} == NULL) {{",));
                } else {
                    self.buffer.emit(format!(
                        "if ({tmp_name}.{UNION_TAG_NAME} == {}) {{",
                        variant.1
                    ));

                    if binding
                        .filter(|&id| self.buffer.emit_local_decl(scopes, id, state).is_ok())
                        .is_some()
                    {
                        self.buffer.emit(format!(
                            " = {}{tmp_name}.{};",
                            if *ptr { "&" } else { "" },
                            variant.0,
                        ));
                    }
                }
            }
            CheckedPattern::CatchAll(binding) => {
                self.buffer.emit("if (1) {");
                if self.buffer.emit_local_decl(scopes, *binding, state).is_ok() {
                    self.buffer.emit(format!(" = {tmp_name};"));
                }
            }
            CheckedPattern::Error => panic!("ICE: CheckedPattern::Error in gen_pattern"),
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

    fn emit_member(&mut self, scopes: &Scopes, ut: &GenericUserType, member: &Member) {
        let mut ty = member.ty.clone();
        ty.fill_type_generics(scopes, ut);
        if !ty.is_void_like() {
            self.buffer.emit_type(scopes, &ty);
            self.buffer.emit(format!(" {}", member.name));
            self.buffer.emit(";");
        }
    }

    fn emit_structs(&mut self, scopes: &Scopes, prototypes: String) -> Result<(), Error> {
        let mut structs = HashMap::new();
        for ut in std::mem::take(&mut self.structs) {
            self.get_depencencies(scopes, ut, &mut structs);
        }

        self.buffer.emit(prototypes);
        for ut in Self::get_struct_order(scopes, &structs)? {
            let Some(members) = scopes.get_user_type(ut.id).members() else { continue; };
            if let Some(union) = scopes.get_user_type(ut.id).data.as_union() {
                if union.is_unsafe {
                    self.buffer.emit("union ");
                    self.buffer.emit_type_name(scopes, ut);
                    self.buffer.emit("{");

                    for member in members {
                        self.emit_member(scopes, ut, member);
                    }
                } else {
                    self.buffer.emit("struct ");
                    self.buffer.emit_type_name(scopes, ut);
                    self.buffer.emit("{");

                    self.buffer.emit_type(scopes, &union.tag_type());
                    self.buffer.emit(format!(" {UNION_TAG_NAME};"));

                    for member in members.iter().filter(|m| m.shared) {
                        self.emit_member(scopes, ut, member);
                    }

                    self.buffer.emit(" union {");
                    for member in members.iter().filter(|m| !m.shared) {
                        self.emit_member(scopes, ut, member);
                    }
                    self.buffer.emit("};");
                }
            } else {
                self.buffer.emit("struct ");
                self.buffer.emit_type_name(scopes, ut);
                self.buffer.emit("{");

                for member in members {
                    self.emit_member(scopes, ut, member);
                }
            }

            self.buffer.emit("};");
        }

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
        &mut self,
        scopes: &Scopes,
        ut: GenericUserType,
        result: &mut HashMap<GenericUserType, Vec<GenericUserType>>,
    ) {
        if result.contains_key(&ut) {
            return;
        }

        if scopes
            .get_user_type(ut.id)
            .data
            .as_union()
            .map_or(false, |u| u.is_unsafe)
        {
            self.buffer.emit("typedef union ");
        } else {
            self.buffer.emit("typedef struct ");
        }
        self.buffer.emit_type_name(scopes, &ut);
        self.buffer.emit(" ");
        self.buffer.emit_type_name(scopes, &ut);
        self.buffer.emit(";");

        let mut deps = Vec::new();
        for member in scopes.get_user_type(ut.id).members().unwrap().iter() {
            let mut ty = member.ty.clone();
            ty.fill_type_generics(scopes, &ut);

            while let TypeId::Array(inner) = ty {
                ty = inner.0;
            }

            if let TypeId::UserType(data) = ty {
                if !data.generics.is_empty() {
                    self.get_depencencies(scopes, (*data).clone(), result);
                }

                deps.push(*data);
            }
        }

        result.insert(ut, deps);
    }

    fn make_positional(
        params: &[CheckedParam],
        mut args: IndexMap<String, CheckedExpr>,
    ) -> Vec<CheckedExpr> {
        let mut result = Vec::with_capacity(args.len());
        for param in params {
            result.push(args.shift_remove(&param.name).unwrap());
        }
        result.extend(args.drain(..).map(|(_, arg)| arg));
        result
    }
}

fn is_opt_ptr(scopes: &Scopes, ty: &TypeId) -> bool {
    scopes
        .as_option_inner(ty)
        .map(|inner| inner.is_ptr() || inner.is_mut_ptr())
        .unwrap_or(false)
}
