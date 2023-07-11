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
        GenericFunc, GenericUserType, ScopeId, Scopes, Symbol, TypeId, Variable, VariableId,
    },
    Error,
};

const RT_STATIC_INIT: &str = "CTL_init_statics";
const UNION_TAG_NAME: &str = "$tag";

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Buffer(String);

impl Buffer {
    fn emit(&mut self, source: impl AsRef<str>) {
        self.0.push_str(source.as_ref());
    }

    fn emit_type(&mut self, scopes: &Scopes, id: &TypeId) {
        match id {
            TypeId::Void => self.emit("CTL(void)"),
            TypeId::Never => self.emit("void"),
            TypeId::Int(bits) | TypeId::Uint(bits) => {
                let unsigned = matches!(id, TypeId::Uint(_));
                if (8..=64).contains(bits) && bits.is_power_of_two() {
                    if unsigned {
                        self.emit(format!("uint{bits}_t"));
                    } else {
                        self.emit(format!("int{bits}_t"));
                    }
                } else if unsigned {
                    self.emit(format!("unsigned CTL_BITINT({bits})"));
                } else {
                    self.emit(format!("CTL_BITINT({bits})"));
                }
            },
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
            TypeId::Never => self.emit("never"),
            TypeId::Int(bits) => self.emit(format!("i{bits}")),
            TypeId::Uint(bits) => self.emit(format!("u{bits}")),
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

    fn emit_fn_name(
        &mut self,
        scopes: &Scopes,
        func: &GenericFunc,
        inst: Option<&GenericUserType>,
    ) {
        let f = scopes.get_func(func.id);
        if !f.proto.is_extern {
            if let Some(inst) = inst {
                self.emit_type_name(scopes, inst);
                self.emit("_");
                self.emit(&f.proto.name);
            } else {
                self.emit(scopes.full_name(f.scope, &f.proto.name));
            }

            for ty in func.generics.iter() {
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

    fn emit_prototype(
        &mut self,
        scopes: &Scopes,
        func: &GenericFunc,
        inst: Option<&GenericUserType>,
    ) {
        let f = scopes.get_func(func.id);
        let mut ret = f.proto.ret.clone();
        ret.fill_func_generics(scopes, func);
        if let Some(inst) = inst {
            ret.fill_type_generics(scopes, inst);
        }

        self.emit_type(scopes, &ret);
        self.emit(" ");
        self.emit_fn_name(scopes, func, inst);
        self.emit("(");
        for (i, param) in f.proto.params.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }

            let mut ty = param.ty.clone();
            ty.fill_func_generics(scopes, func);
            if let Some(inst) = inst {
                ty.fill_type_generics(scopes, inst);
            }

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

    fn emit_local_decl(&mut self, scopes: &Scopes, inst: Option<&GenericUserType>, var: &Variable) {
        let mut ty = var.ty.clone();
        if let Some(inst) = inst {
            ty.fill_type_generics(scopes, inst);
        }

        self.emit_type(scopes, &ty);
        if !var.mutable {
            self.emit(" const");
        }
        self.emit(format!(" {}", var.name));
    }
}

#[derive(Default)]
pub struct Compiler {
    buffer: Buffer,
    structs: HashSet<GenericUserType>,
    funcs: HashSet<(Option<GenericUserType>, GenericFunc)>,
    tmpnum: usize,
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
            funcs: [(None, main.clone())].into(),
            ..Self::default()
        };

        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        while !this.funcs.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for (inst, func) in diff {
                if scopes.get_func(func.id).proto.is_extern {
                    prototypes.emit("extern ");
                }

                prototypes.emit_prototype(scopes, &func, inst.as_ref());
                prototypes.emit(";");

                if let Some(body) = scopes.get_func(func.id).body.clone() {
                    this.buffer.emit_prototype(scopes, &func, inst.as_ref());
                    this.emit_block(scopes, body, inst.as_ref());
                }
            }
        }

        let functions = std::mem::take(&mut this.buffer);

        this.buffer.emit("#include <runtime/ctl.h>\n");
        this.emit_structs(scopes)?;
        this.buffer.emit(prototypes.0);

        let mut statics = Vec::new();
        for scope in scopes.scopes().iter() {
            for &id in scope.vars.iter() {
                let var = scopes.get_var(id);
                if var.is_static {
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
                this.compile_expr(scopes, scopes.get_var(id).value.clone().unwrap(), None);
                this.buffer.emit(";");
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
        this.buffer.emit_fn_name(scopes, &main, None);
        this.buffer.emit("(); }");

        Ok(this.buffer.0)
    }

    fn compile_stmt(&mut self, scopes: &Scopes, stmt: CheckedStmt, inst: Option<&GenericUserType>) {
        match stmt {
            CheckedStmt::Module(block) => {
                for stmt in block.body.into_iter() {
                    self.compile_stmt(scopes, stmt, inst);
                }
            }
            CheckedStmt::Expr(expr) => {
                self.compile_expr(scopes, expr, inst);
                self.buffer.emit(";");
            }
            CheckedStmt::Let(id) => {
                let var = scopes.get_var(id);
                self.buffer.emit_local_decl(scopes, inst, var);
                if let Some(value) = &var.value {
                    self.buffer.emit(" = ");
                    self.compile_expr(scopes, value.clone(), inst);
                }

                self.buffer.emit(";");
            }
            CheckedStmt::None => {}
            CheckedStmt::Error => {
                panic!("ICE: CheckedStmt::Error in compile_stmt");
            }
        }
    }

    fn compile_expr(
        &mut self,
        scopes: &Scopes,
        mut expr: CheckedExpr,
        inst: Option<&GenericUserType>,
    ) {
        if let Some(inst) = inst {
            expr.ty.fill_type_generics(scopes, inst);
        }

        match expr.data {
            ExprData::Binary { op, left, right } => {
                self.buffer.emit("(");
                self.compile_expr(scopes, *left, inst);
                self.buffer.emit(format!(" {op} "));
                self.compile_expr(scopes, *right, inst);
                self.buffer.emit(")");
            }
            ExprData::Unary { op, expr: inner } => match op {
                UnaryOp::Plus => {
                    self.buffer.emit("+");
                    self.compile_expr(scopes, *inner, inst);
                }
                UnaryOp::Neg => {
                    self.buffer.emit("-");
                    self.compile_expr(scopes, *inner, inst);
                }
                UnaryOp::PostIncrement => {
                    self.compile_expr(scopes, *inner, inst);
                    self.buffer.emit("++");
                }
                UnaryOp::PostDecrement => {
                    self.compile_expr(scopes, *inner, inst);
                    self.buffer.emit("--");
                }
                UnaryOp::PreIncrement => {
                    self.buffer.emit("++");
                    self.compile_expr(scopes, *inner, inst);
                }
                UnaryOp::PreDecrement => {
                    self.buffer.emit("--");
                    self.compile_expr(scopes, *inner, inst);
                }
                UnaryOp::Not => {
                    if inner.ty.is_numeric() {
                        self.buffer.emit("~");
                        self.compile_expr(scopes, *inner, inst);
                    } else {
                        self.buffer.emit("!");
                        self.compile_expr(scopes, *inner, inst);
                    }
                }
                UnaryOp::Deref => {
                    self.buffer.emit("(*");
                    self.compile_expr(scopes, *inner, inst);
                    self.buffer.emit(")");
                }
                UnaryOp::Addr | UnaryOp::AddrMut => {
                    self.buffer.emit_cast(scopes, &expr.ty);
                    self.buffer.emit("(&");
                    self.compile_expr(scopes, *inner, inst);
                    self.buffer.emit(")");
                }
                UnaryOp::Unwrap => panic!("ICE: UnaryOp::Unwrap in compile_expr"),
                UnaryOp::Try => todo!(),
                UnaryOp::Sizeof => todo!(),
            },
            ExprData::Call {
                func,
                args,
                inst: this_inst,
            } => {
                self.buffer.emit_fn_name(scopes, &func, this_inst.as_ref());

                self.funcs.insert((this_inst, func));

                self.buffer.emit("(");
                for (i, arg) in args.into_iter().enumerate() {
                    if i > 0 {
                        self.buffer.emit(", ");
                    }

                    self.compile_expr(scopes, arg, inst);
                }
                self.buffer.emit(")");
            }
            ExprData::Array(_) => todo!(),
            ExprData::ArrayWithInit { .. } => todo!(),
            ExprData::Tuple(_) => todo!(),
            ExprData::Map(_) => todo!(),
            ExprData::Bool(value) => {
                self.buffer
                    .emit(if value { "CTL(true)" } else { "CTL(false)" })
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
                self.structs.insert((**expr.ty.as_user_type().unwrap()).clone());

                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit("{");
                self.buffer.emit(format!(".data = (uint8_t const*)\"{value}\","));
                self.buffer.emit(format!(".len = {},", value.len()));
                self.buffer.emit("}");
            }
            ExprData::Char(value) => {
                self.buffer.emit_cast(scopes, &expr.ty);
                self.buffer.emit(format!("{:#x}", value as u32));
            }
            ExprData::Void => self.buffer.emit("CTL(VOID)"),
            ExprData::Symbol(symbol) => match symbol {
                Symbol::Func(func) => self.buffer.emit_fn_name(scopes, &func, None),
                Symbol::Var(id) => self.buffer.emit_var_name(scopes, id),
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
                        // union instances only have one member
                        self.buffer.emit(format!(
                            ".{UNION_TAG_NAME} = {},",
                            union.variant_tag(&name).unwrap()
                        ));
                    }

                    self.buffer.emit(format!(".{name} = "));
                    self.compile_expr(scopes, value, inst);
                    self.buffer.emit(", ");
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
            ExprData::Assign {
                target,
                binary,
                value,
            } => {
                self.compile_expr(scopes, *target, inst);
                if let Some(binary) = binary {
                    self.buffer.emit(format!(" {binary}= "));
                } else {
                    self.buffer.emit(" = ");
                }
                self.compile_expr(scopes, *value, inst);
            }
            ExprData::Block(block) => {
                self.emit_block(scopes, block, inst);
            }
            ExprData::If { .. } => todo!(),
            ExprData::Loop { .. } => todo!(),
            ExprData::For { .. } => todo!(),
            ExprData::Member { source, member } => {
                self.compile_expr(scopes, *source, inst);
                self.buffer.emit(format!(".{member}"));
            }
            ExprData::Subscript { .. } => todo!(),
            ExprData::Return(expr) => {
                // TODO: when return is used as anything except a StmtExpr, we will have to change
                // the generated code to accomodate it
                self.buffer.emit("return ");
                self.compile_expr(scopes, *expr, inst);
            }
            ExprData::Yield(_) => {
                //                 let block = self.current_block.as_ref().unwrap();
                //                 let assign = format!("{} = ", block.variable);
                //                 let goto = format!("; goto {}", block.label);
                //
                //                 self.buffer.emit(assign);
                //                 self.compile_expr(scopes, *expr, inst);
                //                 self.buffer.emit(goto);
                todo!()
            }
            ExprData::Break(_) => todo!(),
            ExprData::Continue => todo!(),
            ExprData::Error => {
                panic!("ICE: ExprData::Error in compile_expr");
            }
            ExprData::Match { mut expr, body } => {
                let tmp_name = self.get_tmp_name();

                if let Some(inst) = inst {
                    expr.ty.fill_type_generics(scopes, inst);
                }

                self.buffer.emit_type(scopes, &expr.ty);
                self.buffer.emit(format!(" {tmp_name} = "));
                self.compile_expr(scopes, *expr, inst);
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
                        self.buffer
                            .emit_local_decl(scopes, inst, scopes.get_var(id));
                        self.buffer
                            .emit(format!(" = {tmp_name}.{};", pattern.variant.0));
                    }

                    // FIXME: match yields values
                    self.compile_expr(scopes, expr, inst);
                    self.buffer.emit("; }");
                }
            }
        }
    }

    fn emit_block(&mut self, scopes: &Scopes, block: Block, inst: Option<&GenericUserType>) {
        self.buffer.emit("{");
        for stmt in block.body.into_iter() {
            self.compile_stmt(scopes, stmt, inst);
        }
        self.buffer.emit("}");
    }

    fn emit_structs(&mut self, scopes: &Scopes) -> Result<(), Error> {
        let mut structs = HashMap::new();
        for ut in std::mem::take(&mut self.structs) {
            self.get_depencencies(scopes, ut, &mut structs);
        }

        for ut in Self::get_struct_order(scopes, &structs)? {
            self.buffer.emit("struct ");
            self.buffer.emit_type_name(scopes, ut);
            self.buffer.emit("{");

            let union = scopes.get_user_type(ut.id).data.as_union();
            if let Some(union) = union {
                self.buffer.emit_type(scopes, &union.tag_type());
                self.buffer.emit(format!(" {UNION_TAG_NAME}; union {{"));
            }

            for (name, member) in scopes.get_user_type(ut.id).data.members().unwrap() {
                let mut ty = member.ty.clone();
                ty.fill_type_generics(scopes, ut);

                self.buffer.emit_type(scopes, &ty);
                self.buffer.emit(format!(" {}", name));
                self.buffer.emit(";");
                if !member.public {
                    self.buffer.emit("/* private */ \n")
                }
            }

            if union.is_some() {
                self.buffer.emit("};");
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

    fn get_tmp_name(&mut self) -> String {
        self.tmpnum += 1;
        format!("$tmp{}", self.tmpnum)
    }
}
