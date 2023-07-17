use std::collections::{HashMap, HashSet};

use crate::{
    ast::expr::UnaryOp,
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::CheckedStmt,
        Block,
    },
    lexer::Span,
    typecheck::{
        GenericFunc, GenericUserType, Member, ScopeId, Scopes, Symbol, TypeId, Variable, VariableId,
    },
    Error,
};

const RT_STATIC_INIT: &str = "CTL_init_statics";
const UNION_TAG_NAME: &str = "$tag";

#[derive(Hash, PartialEq, Eq, Clone)]
struct State {
    func: GenericFunc,
    inst: Option<GenericUserType>,
}

impl State {
    pub fn fill_generics(&self, scopes: &Scopes, ty: &mut TypeId) {
        ty.fill_func_generics(scopes, &self.func);

        if let Some(inst) = self.inst.as_ref() {
            ty.fill_type_generics(scopes, inst);
        }
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
            TypeId::Isize => self.emit("CTL(isize)"),
            TypeId::Usize => self.emit("CTL(usize)"),
            TypeId::F32 => self.emit("CTL(f32)"),
            TypeId::F64 => self.emit("CTL(f64)"),
            TypeId::Bool => self.emit("CTL(bool)"),
            TypeId::Char => self.emit("CTL(char)"),
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
                self.emit_type_name(scopes, ut);
            }
            TypeId::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            TypeId::Array(_) => todo!(),
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
            TypeId::Unknown => panic!("ICE: TypeId::Unknown in emit_generic_mangled_name"),
            TypeId::Array(_) => todo!(),
        }
    }

    fn emit_fn_name(&mut self, scopes: &Scopes, state: &State) {
        let f = scopes.get_func(state.func.id);
        if !f.proto.is_extern {
            if let Some(inst) = state.inst.as_ref() {
                self.emit_type_name(scopes, inst);
                self.emit("_");
                self.emit(&f.proto.name);
            } else {
                self.emit(scopes.full_name(f.scope, &f.proto.name));
            }

            for ty in state.func.generics.iter() {
                self.emit("$");
                self.emit_generic_mangled_name(scopes, ty);
            }
        } else {
            self.emit(&f.proto.name);
        }
    }

    fn emit_type_name(&mut self, scopes: &Scopes, ut: &GenericUserType) {
        let ty = scopes.get_user_type(ut.id);
        self.emit(scopes.full_name(ty.scope, &ty.name));
        for ty in ut.generics.iter() {
            self.emit("$");
            self.emit_generic_mangled_name(scopes, ty);
        }
    }

    fn emit_var_name(&mut self, scopes: &Scopes, id: VariableId) {
        let var = scopes.get_var(id);
        if var.is_static {
            self.emit(scopes.full_name(var.scope, &var.name));
        } else {
            self.emit(&var.name);
        }
    }

    fn emit_prototype(&mut self, scopes: &Scopes, state: &State) {
        let f = scopes.get_func(state.func.id);
        let mut ret = f.proto.ret.clone();
        state.fill_generics(scopes, &mut ret);

        self.emit_type(scopes, &ret);
        self.emit(" ");
        self.emit_fn_name(scopes, state);
        self.emit("(");
        for (i, param) in f
            .proto
            .params
            .iter()
            .filter(|param| !param.ty.is_void_like())
            .enumerate()
        {
            if i > 0 {
                self.emit(", ");
            }

            let mut ty = param.ty.clone();
            state.fill_generics(scopes, &mut ty);

            self.emit_type(scopes, &ty);
            if !param.mutable {
                self.emit(" const");
            }

            self.emit(format!(" {}", param.name));
        }

        if f.proto.params.is_empty() {
            self.emit("void)");
        } else {
            self.emit(")");
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

macro_rules! tmpvar {
    ($self: expr) => {{
        $self.tmpvar += 1;
        format!("${}", $self.tmpvar)
    }};
}

macro_rules! enter_block {
    ($self: expr, $scopes: expr, $ty: expr, $body: expr) => {{
        let ty = $ty;
        let old = std::mem::replace(
            &mut $self.cur_block,
            BlockInfo {
                label: {
                    $self.tmpblk += 1;
                    format!("blk{}", $self.tmpblk)
                },
                var: tmpvar!($self),
            },
        );
        let written = tmpbuf! {
            $self,
            {
                if !ty.is_void_like() {
                    $self.buffer.emit_type($scopes, ty);
                    $self.buffer.emit(format!(" {};", $self.cur_block.var));
                }
                $body;
                $self.buffer.emit(format!("{}:;", $self.cur_block.label));
            }
        };

        $self.temporaries.emit(written.0);
        let label = std::mem::replace(&mut $self.cur_block, old).var;
        if !ty.is_void_like() {
            $self.buffer.emit(label);
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
pub struct BlockInfo {
    label: String,
    var: String,
}

#[derive(Default)]
pub struct Compiler {
    buffer: Buffer,
    temporaries: Buffer,
    structs: HashSet<GenericUserType>,
    funcs: HashSet<State>,
    tmpvar: usize,
    tmpblk: usize,
    cur_block: BlockInfo,
    cur_loop: String,
}

impl Compiler {
    pub fn compile(scope: ScopeId, scopes: &Scopes) -> Result<String, Error> {
        let Some(main) = scopes.find_func_in("main", scope) else {
            return Err(Error::new(
                "no main function found",
                Span::default(),
            ));
        };

        let main = GenericFunc::new(main, Vec::new());
        let mut this = Self {
            funcs: [State {
                inst: None,
                func: main.clone(),
            }]
            .into(),
            ..Self::default()
        };

        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        while !this.funcs.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for state in diff {
                let f = scopes.get_func(state.func.id);
                if f.proto.is_extern {
                    prototypes.emit("extern ");
                }

                prototypes.emit_prototype(scopes, &state);
                prototypes.emit(";");

                if !f.proto.is_extern {
                    if let Some(body) = f.body.clone() {
                        this.buffer.emit_prototype(scopes, &state);
                        this.emit_block(scopes, body, &state);
                    }
                }
            }
        }

        let functions = std::mem::take(&mut this.buffer);

        this.buffer.emit("#include <runtime/ctl.h>\n");

        this.emit_structs(scopes, prototypes.0)?;

        let mut statics = Vec::new();
        for scope in scopes.scopes().iter() {
            for &id in scope.vars.iter() {
                let var = scopes.get_var(id);
                if var.is_static && !var.ty.is_void_like() {
                    this.buffer.emit("static ");
                    this.buffer.emit_type(scopes, &var.ty);
                    this.buffer.emit(" ");
                    this.buffer.emit_var_name(scopes, id);
                    this.buffer.emit(";");

                    statics.push(id);
                }
            }
        }

        this.buffer.emit(functions.0);

        let static_init = !statics.is_empty();
        if static_init {
            this.buffer.emit(format!("void {RT_STATIC_INIT}() {{"));
            for id in statics {
                this.buffer.emit_var_name(scopes, id);
                this.buffer.emit(" = ");
                //this.compile_expr(scopes, scopes.get_var(id).value.clone().unwrap(), None);
                this.buffer.emit(";");

                todo!("statics")
            }
            this.buffer.emit("}");
        }

        this.buffer.emit("int main(int argc, char **argv) {");
        this.buffer.emit("GC_INIT();");
        this.buffer.emit("(void)argc;");
        this.buffer.emit("(void)argv;");
        if static_init {
            this.buffer.emit(format!("{RT_STATIC_INIT}();"));
        }
        this.buffer.emit("return ");
        this.buffer.emit_fn_name(
            scopes,
            &State {
                func: main,
                inst: None,
            },
        );
        this.buffer.emit("(); }");

        Ok(this.buffer.0)
    }

    fn compile_stmt(&mut self, scopes: &Scopes, stmt: CheckedStmt, state: &State) {
        match stmt {
            CheckedStmt::Module(block) => {
                for stmt in block.body.into_iter() {
                    self.compile_stmt(scopes, stmt, state);
                }
            }
            CheckedStmt::Expr(expr) => {
                stmt! {
                    self,
                    {
                        self.compile_expr_inner(scopes, expr, state);
                        self.buffer.emit(";");
                    }
                }
            }
            CheckedStmt::Let(id) => {
                stmt! {
                    self,
                    {
                        let var = scopes.get_var(id);
                        if !var.ty.is_void_like() {
                            self.emit_local_decl(scopes, var, state);
                            if let Some(value) = &var.value {
                                self.buffer.emit(" = ");
                                self.compile_expr_inner(scopes, value.clone(), state);
                            }

                            self.buffer.emit(";");
                        } else if let Some(value) = &var.value {
                            self.compile_expr_inner(scopes, value.clone(), state);
                            self.buffer.emit(";");
                        }
                    }
                }
            }
            CheckedStmt::None => {}
            CheckedStmt::Error => {
                panic!("ICE: CheckedStmt::Error in compile_stmt");
            }
        }
    }

    fn compile_expr_inner(&mut self, scopes: &Scopes, expr: CheckedExpr, state: &State) {
        match expr.data {
            ExprData::Binary { op, left, right } => {
                if expr.ty == TypeId::Bool {
                    self.buffer.emit_cast(scopes, &expr.ty);
                    self.buffer.emit("(");
                }
                self.buffer.emit("(");
                self.compile_expr(scopes, *left, state);
                self.buffer.emit(format!(" {op} "));
                self.compile_expr(scopes, *right, state);
                self.buffer.emit(")");

                if expr.ty == TypeId::Bool {
                    self.buffer.emit(" ? 1 : 0)");
                }
            }
            ExprData::Unary { op, expr: inner } => match op {
                UnaryOp::Plus => {
                    self.buffer.emit("+");
                    self.compile_expr(scopes, *inner, state);
                }
                UnaryOp::Neg => {
                    self.buffer.emit("-");
                    self.compile_expr(scopes, *inner, state);
                }
                UnaryOp::PostIncrement => {
                    self.compile_expr(scopes, *inner, state);
                    self.buffer.emit("++");
                }
                UnaryOp::PostDecrement => {
                    self.compile_expr(scopes, *inner, state);
                    self.buffer.emit("--");
                }
                UnaryOp::PreIncrement => {
                    self.buffer.emit("++");
                    self.compile_expr(scopes, *inner, state);
                }
                UnaryOp::PreDecrement => {
                    self.buffer.emit("--");
                    self.compile_expr(scopes, *inner, state);
                }
                UnaryOp::Not => {
                    if inner.ty == TypeId::Bool {
                        self.buffer.emit_cast(scopes, &expr.ty);
                        self.buffer.emit("(");
                        self.compile_expr(scopes, *inner, state);
                        self.buffer.emit(" ^ 1)");
                    } else {
                        self.buffer.emit("~");
                        self.compile_expr(scopes, *inner, state);
                    }
                }
                UnaryOp::Deref => {
                    self.buffer.emit("(*");
                    self.compile_expr(scopes, *inner, state);
                    self.buffer.emit(")");
                }
                UnaryOp::Addr | UnaryOp::AddrMut => {
                    // TODO: addr of void
                    self.buffer.emit("(&");
                    self.compile_expr(scopes, *inner, state);
                    self.buffer.emit(")");
                }
                UnaryOp::Unwrap => panic!("ICE: UnaryOp::Unwrap in compile_expr"),
                UnaryOp::Try => todo!(),
            },
            ExprData::Call {
                mut func,
                args,
                mut inst,
            } => {
                for ty in func.generics.iter_mut() {
                    state.fill_generics(scopes, ty);
                }

                if let Some(inst) = inst.as_mut() {
                    for ty in inst.generics.iter_mut() {
                        state.fill_generics(scopes, ty);
                    }
                }

                if scopes.scopes()[0]
                    .children
                    .get("core")
                    .and_then(|&scope| scopes[scope].children.get("mem"))
                    .and_then(|&scope| scopes.find_func_in("size_of", scope))
                    .filter(|&id| id == func.id)
                    .is_some()
                {
                    self.buffer.emit("(CTL(usize))sizeof");
                    self.buffer.emit_cast(scopes, &func.generics[0]);
                    if let TypeId::UserType(ty) = &func.generics[0] {
                        self.structs.insert((**ty).clone());
                    }
                    return;
                }

                let next_state = State { func, inst };
                self.buffer.emit_fn_name(scopes, &next_state);
                self.funcs.insert(next_state);

                // FIXME: keyword arguments can be specified out of order. this evaluates them in
                // parameter order instead of argument order.
                self.buffer.emit("(");
                for (i, arg) in args
                    .into_iter()
                    .filter(|arg| !arg.ty.is_void_like())
                    .enumerate()
                {
                    if i > 0 {
                        self.buffer.emit(", ");
                    }

                    self.compile_expr(scopes, arg, state);
                }
                self.buffer.emit(")");
            }
            ExprData::Array(_) => todo!(),
            ExprData::ArrayWithInit { .. } => todo!(),
            ExprData::Tuple(_) => todo!(),
            ExprData::Map(_) => todo!(),
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
                self.buffer.emit(format!(
                    "{{ .ptr = (uint8_t const*)u8\"{value}\", .len = (CTL(usize)){} }}",
                    value.len()
                ));
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
                    self.buffer.emit_fn_name(
                        scopes,
                        &State {
                            func: *func,
                            inst: None,
                        },
                    )
                }
                Symbol::Var(id) => {
                    if !scopes.get_var(id).ty.is_void_like() {
                        self.buffer.emit_var_name(scopes, id);
                    }
                }
            },
            ExprData::Instance(members) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit("{");
                for (name, value) in members {
                    if let Some(union) = expr
                        .ty
                        .as_user_type()
                        .and_then(|ut| scopes.get_user_type(ut.id).data.as_union())
                    {
                        if union
                            .variants
                            .iter()
                            .find(|n| n.0 == name)
                            .filter(|m| !m.1.shared)
                            .is_some()
                        {
                            self.buffer.emit(format!(
                                ".{UNION_TAG_NAME} = {},",
                                union.variant_tag(&name).unwrap()
                            ));
                        }
                    }

                    if !value.ty.is_void_like() {
                        self.buffer.emit(format!(".{name} = "));
                        self.compile_expr(scopes, value, state);
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
            ExprData::Member { source, member } => {
                if !expr.ty.is_void_like() {
                    self.compile_expr(scopes, *source, state);
                    self.buffer.emit(format!(".{member}"));
                }
            }
            ExprData::Assign {
                target,
                binary,
                value,
            } => {
                if !expr.ty.is_void_like() {
                    self.compile_expr(scopes, *target, state);
                    if let Some(binary) = binary {
                        self.buffer.emit(format!(" {binary}= "));
                    } else {
                        self.buffer.emit(" = ");
                    }
                    self.compile_expr(scopes, *value, state);
                } else {
                    self.compile_expr(scopes, *target, state);
                    self.buffer.emit(";");

                    self.compile_expr(scopes, *value, state);
                    self.buffer.emit(";");
                }
            }
            ExprData::Block(block) => {
                enter_block! {
                    self, scopes, &expr.ty,
                    {
                        self.emit_block(scopes, block, state);
                    }
                }
            }
            ExprData::If {
                cond,
                if_branch,
                else_branch,
                has_default_opt,
            } => {
                enter_block! {
                    self, scopes, &expr.ty,
                    {
                        self.buffer.emit("if (");
                        self.compile_expr(scopes, *cond, state);
                        self.buffer.emit(") {");
                        stmt! {
                            self,
                            {
                                if !expr.ty.is_void_like() {
                                    self.buffer.emit(format!("{} = ", self.cur_block.var));
                                }
                                if has_default_opt {
                                    self.buffer.emit_cast(scopes, &expr.ty);
                                    let tag = scopes
                                        .get_user_type(expr.ty.as_user_type().unwrap().id)
                                        .data
                                        .as_union()
                                        .unwrap()
                                        .variant_tag("Some")
                                        .unwrap();
                                    if !if_branch.ty.is_void_like() {
                                        self.buffer.emit(format!("{{ .$tag = {tag}, .Some = "));
                                        self.compile_expr(scopes, *if_branch, state);
                                        self.buffer.emit(", }");
                                    } else {
                                        self.buffer.emit(format!("{{ .$tag = {tag} }}"));
                                    }
                                } else {
                                    self.compile_expr(scopes, *if_branch, state);
                                }
                            }
                        }

                        if let Some(else_branch) = else_branch {
                            self.buffer.emit("; } else {");
                            stmt! {
                                self,
                                {
                                    if !expr.ty.is_void_like() {
                                        self.buffer.emit(format!("{} = ", self.cur_block.var));
                                    }
                                    self.compile_expr(scopes, *else_branch, state);
                                }
                            }
                        }

                        self.buffer.emit("; }");
                    }
                }
            }
            ExprData::Loop {
                cond,
                body,
                do_while,
            } => {
                let old = std::mem::replace(&mut self.cur_loop, tmpvar!(self));
                let written = tmpbuf! {
                    self,
                    {
                        if !expr.ty.is_void_like() {
                            self.buffer.emit_type(scopes, &expr.ty);
                            self.buffer.emit(format!(" {};", self.cur_loop));
                        }
                        self.buffer.emit("for (;;) {");

                        macro_rules! cond {
                            () => {
                                stmt! {
                                    self,
                                    {
                                        self.buffer.emit("if (!");
                                        self.compile_expr(scopes, *cond, state);
                                        self.buffer.emit(") { break; }");
                                    }
                                }
                            };
                        }

                        if !do_while {
                            cond!();
                            self.emit_block(scopes, body, state);
                        } else {
                            self.emit_block(scopes, body, state);
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
            ExprData::For { .. } => todo!(),
            ExprData::Subscript { .. } => todo!(),
            ExprData::Return(expr) => {
                // TODO: when return is used as anything except a StmtExpr, we will have to change
                // the generated code to accomodate it
                self.buffer.emit("return ");
                self.compile_expr(scopes, *expr, state);
            }
            ExprData::Yield(expr) => {
                if !expr.ty.is_void_like() {
                    self.buffer.emit(format!("{} = ", self.cur_block.var));
                }
                self.compile_expr(scopes, *expr, state);
                self.buffer.emit(format!("; goto {}", self.cur_block.label));
            }
            ExprData::Break(expr) => {
                if !expr.ty.is_void_like() {
                    self.buffer.emit(format!("{} = ", self.cur_loop));
                }
                self.compile_expr(scopes, *expr, state);
                self.buffer.emit("; break;");
            }
            ExprData::Continue => self.buffer.emit("continue;"),
            ExprData::Error => {
                panic!("ICE: ExprData::Error in compile_expr");
            }
            ExprData::Match {
                expr: mut scrutinee,
                body,
            } => {
                enter_block! {
                    self, scopes, &expr.ty,
                    {
                        // TODO: update to exclude void when literal patterns are implemented
                        let tmp_name = tmpvar!(self);
                        state.fill_generics(scopes, &mut scrutinee.ty);

                        self.buffer.emit_type(scopes, &scrutinee.ty);
                        self.buffer.emit(format!(" {tmp_name} = "));
                        self.compile_expr(scopes, *scrutinee, state);
                        self.buffer.emit(";");

                        for (i, (pattern, expr)) in body.into_iter().enumerate() {
                            if i > 0 {
                                self.buffer.emit("else ");
                            }

                            self.buffer.emit(format!(
                                "if ({tmp_name}.{UNION_TAG_NAME} == {}) {{",
                                pattern.variant.1
                            ));

                            if let Some(id) = pattern.binding {
                                if !scopes.get_var(id).ty.is_void_like() {
                                    self.emit_local_decl(scopes, scopes.get_var(id), state);
                                    self.buffer
                                        .emit(format!(" = {tmp_name}.{};", pattern.variant.0));
                                }
                            }

                            stmt! {
                                self,
                                {
                                    self.compile_expr(scopes, expr, state);
                                    self.buffer.emit("; }");
                                }
                            }
                        }
                    }
                }
            }
            ExprData::As(inner) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit("(");
                self.compile_expr(scopes, *inner, state);
                self.buffer.emit(")");
            }
        }
    }

    fn compile_expr(&mut self, scopes: &Scopes, mut expr: CheckedExpr, state: &State) {
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
                ExprData::Tuple(_) => todo!(),
                ExprData::Map(_) => todo!(),
                ExprData::Assign { .. } => true,
                ExprData::For { .. } => todo!(),
                ExprData::Subscript { .. } => todo!(),
                _ => false,
            }
        }

        state.fill_generics(scopes, &mut expr.ty);

        if has_side_effects(&expr) {
            if !expr.ty.is_void_like() {
                let tmp = tmpvar!(self);
                let written = tmpbuf! {
                    self,
                    {
                        self.buffer.emit_type(scopes, &expr.ty);
                        self.buffer.emit(format!(" {tmp} = "));
                        self.compile_expr_inner(scopes, expr, state);
                        self.buffer.emit(";");
                    }
                };

                self.temporaries.emit(written.0);
                self.buffer.emit(tmp);
            } else {
                self.compile_expr_inner(scopes, expr, state);
                self.buffer.emit(";");
            }
        } else {
            self.compile_expr_inner(scopes, expr, state);
        }
    }

    fn emit_block(&mut self, scopes: &Scopes, block: Block, state: &State) {
        self.buffer.emit("{");
        for stmt in block.body.into_iter() {
            self.compile_stmt(scopes, stmt, state);
        }
        self.buffer.emit("}");
    }

    fn emit_member(&mut self, scopes: &Scopes, ut: &GenericUserType, name: &str, member: &Member) {
        let mut ty = member.ty.clone();
        ty.fill_type_generics(scopes, ut);
        if member.ty.is_void_like() {
            return;
        }

        self.buffer.emit_type(scopes, &ty);
        self.buffer.emit(format!(" {name}"));
        self.buffer.emit(";");
        if !member.public {
            self.buffer.emit("/* private */ \n")
        }
    }

    fn emit_structs(&mut self, scopes: &Scopes, prototypes: String) -> Result<(), Error> {
        let mut structs = HashMap::new();
        for ut in std::mem::take(&mut self.structs) {
            self.get_depencencies(scopes, ut, &mut structs);
        }

        self.buffer.emit(prototypes);
        for ut in Self::get_struct_order(scopes, &structs)? {
            self.buffer.emit("struct ");
            self.buffer.emit_type_name(scopes, ut);
            self.buffer.emit("{");

            let members = scopes.get_user_type(ut.id).data.members().unwrap();
            if let Some(union) = scopes.get_user_type(ut.id).data.as_union() {
                self.buffer.emit_type(scopes, &union.tag_type());
                self.buffer.emit(format!(" {UNION_TAG_NAME};"));

                for (name, member) in members {
                    if member.shared {
                        self.emit_member(scopes, ut, name, member);
                    }
                }

                self.buffer.emit(" union {");
                for (name, member) in members {
                    if !member.shared {
                        self.emit_member(scopes, ut, name, member);
                    }
                }
                self.buffer.emit("};");
            } else {
                for (name, member) in members {
                    self.emit_member(scopes, ut, name, member);
                }
            }

            self.buffer.emit("};");
        }

        Ok(())
    }

    fn emit_local_decl(&mut self, scopes: &Scopes, var: &Variable, state: &State) {
        let mut ty = var.ty.clone();
        state.fill_generics(scopes, &mut ty);
        if ty.is_void_like() {
            return;
        }

        self.buffer.emit_type(scopes, &ty);
        if !var.mutable {
            self.buffer.emit(" const");
        }
        self.buffer.emit(format!(" {}", var.name));
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

        self.buffer.emit("typedef struct ");
        self.buffer.emit_type_name(scopes, &ut);
        self.buffer.emit(" ");
        self.buffer.emit_type_name(scopes, &ut);
        self.buffer.emit(";");

        let mut deps = Vec::new();
        for (_, member) in scopes.get_user_type(ut.id).data.members().unwrap().iter() {
            let mut ty = member.ty.clone();
            ty.fill_type_generics(scopes, &ut);

            while matches!(ty, TypeId::Array(_)) {
                while let TypeId::Array(inner) = ty {
                    ty = inner.0;
                }
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
}
