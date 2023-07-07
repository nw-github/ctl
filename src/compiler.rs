use std::collections::{HashMap, HashSet};

use crate::{
    ast::expr::UnaryOp,
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::CheckedStmt,
        Block,
    },
    lexer::{Location, Span},
    typecheck::{
        CheckedAst, FunctionId, GenericFunc, GenericUserType, Scopes, Symbol, TypeId, UserTypeId,
        VariableId,
    },
    Error,
};

const RT_STATIC_INIT: &str = "CTL_init_statics";

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
            TypeId::Int(bits) => self.emit(format!("CTL(i{bits})")),
            TypeId::Uint(bits) => self.emit(format!("CTL(u{bits})")),
            TypeId::Isize => self.emit("CTL(isize)"),
            TypeId::Usize => self.emit("CTL(usize)"),
            TypeId::F32 => self.emit("CTL(f32)"),
            TypeId::F64 => self.emit("CTL(f64)"),
            TypeId::Bool => self.emit("CTL(bool)"),
            TypeId::String => self.emit("CTL(str)"),
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
            TypeId::Option(inner) => {
                self.emit("CTL(opt_");
                self.emit_type(scopes, inner);
                self.emit(")");
            }
            TypeId::Func(_) => todo!(),
            TypeId::UserType(data) => {
                if scopes.get_user_type(data.id).data.is_struct() {
                    self.emit("struct ");
                    self.emit_type_name(scopes, data.id, &data.generics);
                }
            }
            TypeId::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            TypeId::Array(_) => todo!(),
        }
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
            TypeId::String => self.emit("str"),
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
            TypeId::Option(inner) => {
                self.emit("opt_");
                self.emit_generic_mangled_name(scopes, inner);
            }
            TypeId::Func(_) => todo!(),
            TypeId::UserType(data) => {
                if scopes.get_user_type(data.id).data.is_struct() {
                    self.emit_type_name(scopes, data.id, &data.generics);
                }
            }
            TypeId::Unknown => panic!("ICE: TypeId::Unknown in emit_generic_mangled_name"),
            TypeId::Array(_) => todo!(),
        }
    }

    fn emit_fn_name(&mut self, scopes: &Scopes, id: FunctionId, generics: &[TypeId]) {
        let func = scopes.get_func(id);
        if !func.proto.is_extern {
            self.emit(scopes.full_name(func.scope, &func.proto.name));
            for ty in generics {
                self.emit("$");
                self.emit_generic_mangled_name(scopes, ty);
            }
        } else {
            self.emit(&func.proto.name);
        }
    }

    fn emit_type_name(&mut self, scopes: &Scopes, id: UserTypeId, generics: &[TypeId]) {
        let ty = scopes.get_user_type(id);
        self.emit(scopes.full_name(ty.scope, &ty.name));
        for ty in generics {
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

    fn emit_prototype(&mut self, scopes: &Scopes, id: FunctionId, generics: &[TypeId]) {
        let func = scopes.get_func(id);
        let mut ret = func.proto.ret.clone();
        ret.fill_func_generics(id, scopes, generics);

        self.emit_type(scopes, &ret);
        self.emit(" ");
        self.emit_fn_name(scopes, id, generics);
        self.emit("(");
        for (i, param) in func.proto.params.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }

            let mut ty = param.ty.clone();
            ty.fill_func_generics(id, scopes, generics);

            self.emit_type(scopes, &ty);
            if !param.mutable {
                self.emit(" const");
            }

            self.emit(format!(" {}", param.name));
        }
        self.emit(")");
    }
}

#[derive(Default)]
pub struct Compiler {
    buffer: Buffer,
    structs: HashSet<GenericUserType>,
    funcs: HashSet<GenericFunc>,
}

impl Compiler {
    pub fn compile(ast: CheckedAst) -> Result<String, Error> {
        let main_module = ast.scopes.scopes()[0].children.iter().next().unwrap();
        let Some(main) = ast.scopes.find_func_in("main", *main_module.1) else {
            return Err(Error::new(
                "no main function found",
                Span {
                    loc: Location {
                        row: 0,
                        col: 0,
                        pos: 0,
                    },
                    len: 0,
                },
            ));
        };

        let mut this = Self {
            funcs: [GenericFunc::new(main, Vec::new())].into(),
            ..Self::default()
        };

        let mut prototypes = Buffer::default();
        let mut emitted = HashSet::new();
        while !this.funcs.is_empty() {
            let diff = this.funcs.difference(&emitted).cloned().collect::<Vec<_>>();
            emitted.extend(this.funcs.drain());

            for func in diff {
                if ast.scopes.get_func(func.id).proto.is_extern {
                    prototypes.emit("extern ");
                }

                prototypes.emit_prototype(&ast.scopes, func.id, &func.generics);
                prototypes.emit(";");

                if let Some(body) = ast.scopes.get_func(func.id).body.clone() {
                    this.buffer
                        .emit_prototype(&ast.scopes, func.id, &func.generics);
                    this.emit_block(&ast.scopes, body);
                }
            }
        }

        let functions = std::mem::take(&mut this.buffer);

        this.buffer.emit("#include <runtime/ctl.h>\n");
        this.emit_structs(&ast.scopes)?;
        this.buffer.emit(prototypes.0);

        let mut statics = Vec::new();
        for scope in ast.scopes.scopes().iter() {
            for &id in scope.vars.iter() {
                let var = ast.scopes.get_var(id);
                if var.is_static {
                    this.buffer.emit("static ");
                    this.buffer.emit_type(&ast.scopes, &var.ty);
                    this.buffer.emit(" ");
                    this.buffer.emit_var_name(&ast.scopes, id);
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
                this.buffer.emit_var_name(&ast.scopes, id);
                this.buffer.emit(" = ");
                this.compile_expr(&ast.scopes, ast.scopes.get_var(id).value.clone().unwrap());
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
        this.buffer.emit_fn_name(&ast.scopes, main, &[]);
        this.buffer.emit("(); }");

        Ok(this.buffer.0)
    }

    fn compile_stmt(&mut self, scopes: &Scopes, stmt: CheckedStmt) {
        match stmt {
            CheckedStmt::Module { body, .. } => {
                for stmt in body.body.into_iter() {
                    self.compile_stmt(scopes, stmt);
                }
            }
            CheckedStmt::Expr(expr) => {
                self.compile_expr(scopes, expr);
                self.buffer.emit(";");
            }
            CheckedStmt::Let(name, id) => {
                let var = scopes.get_var(id);
                if let Some(value) = &var.value {
                    self.buffer.emit_type(scopes, &value.ty);
                    if !var.mutable {
                        self.buffer.emit(" const ");
                    }
                    self.buffer.emit(format!(" {name} = "));
                    self.compile_expr(scopes, value.clone());
                } else {
                    self.buffer.emit_type(scopes, &var.ty);
                    if !var.mutable {
                        self.buffer.emit(" const ");
                    }

                    self.buffer.emit(format!(" {name}"));
                }

                self.buffer.emit(";");
            }
            CheckedStmt::None => {}
            CheckedStmt::Error => {
                panic!("ICE: CheckedStmt::Error in compile_stmt");
            }
        }
    }

    fn compile_expr(&mut self, scopes: &Scopes, expr: CheckedExpr) {
        match expr.data {
            ExprData::Binary { op, left, right } => {
                self.buffer.emit("(");
                self.compile_expr(scopes, *left);
                self.buffer.emit(format!(" {op} "));
                self.compile_expr(scopes, *right);
                self.buffer.emit(")");
            }
            ExprData::Unary { op, expr } => match op {
                UnaryOp::Plus => {
                    self.buffer.emit("+");
                    self.compile_expr(scopes, *expr);
                }
                UnaryOp::Neg => {
                    self.buffer.emit("-");
                    self.compile_expr(scopes, *expr);
                }
                UnaryOp::PostIncrement => {
                    self.compile_expr(scopes, *expr);
                    self.buffer.emit("++");
                }
                UnaryOp::PostDecrement => {
                    self.compile_expr(scopes, *expr);
                    self.buffer.emit("--");
                }
                UnaryOp::PreIncrement => {
                    self.buffer.emit("++");
                    self.compile_expr(scopes, *expr);
                }
                UnaryOp::PreDecrement => {
                    self.buffer.emit("--");
                    self.compile_expr(scopes, *expr);
                }
                UnaryOp::Not => {
                    if expr.ty.is_numeric() {
                        self.buffer.emit("~");
                        self.compile_expr(scopes, *expr);
                    } else {
                        self.buffer.emit("!");
                        self.compile_expr(scopes, *expr);
                    }
                }
                UnaryOp::Deref => todo!(),
                UnaryOp::Addr => todo!(),
                UnaryOp::AddrMut => todo!(),
                UnaryOp::Unwrap => todo!(),
                UnaryOp::Try => todo!(),
                UnaryOp::Sizeof => todo!(),
            },
            ExprData::Call { func, args } => {
                self.buffer.emit_fn_name(scopes, func.id, &func.generics);

                self.funcs.insert(func);

                self.buffer.emit("(");
                for (i, arg) in args.into_iter().enumerate() {
                    if i > 0 {
                        self.buffer.emit(", ");
                    }

                    self.compile_expr(scopes, arg);
                }
                self.buffer.emit(")");
            }
            ExprData::MemberCall { func, this, args } => {
                let target = &scopes.get_func(func.id).proto.params[0].ty;

                self.buffer.emit_fn_name(scopes, func.id, &func.generics);
                self.funcs.insert(func);

                self.buffer.emit("(");

                let mut source_ty = &this.ty;
                if !matches!(source_ty, TypeId::Ptr(_) | TypeId::MutPtr(_)) {
                    self.buffer.emit("&");
                } else {
                    while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = source_ty {
                        if source_ty == target {
                            break;
                        }

                        self.buffer.emit("*");
                        source_ty = inner;
                    }
                }

                self.compile_expr(scopes, *this);

                for arg in args {
                    self.buffer.emit(", ");
                    self.compile_expr(scopes, arg);
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
                self.buffer.emit("(");
                self.buffer.emit_type(scopes, &expr.ty);
                self.buffer.emit(")");
                self.buffer.emit(format!("{value}"));
            }
            ExprData::Unsigned(value) => {
                self.buffer.emit("(");
                self.buffer.emit_type(scopes, &expr.ty);
                self.buffer.emit(")");
                self.buffer.emit(format!("{value}"));
            }
            ExprData::Float(value) => {
                self.buffer.emit("(");
                self.buffer.emit_type(scopes, &expr.ty);
                self.buffer.emit(")");
                self.buffer.emit(value);
            }
            ExprData::String(value) => {
                self.buffer.emit("CTL_STR(\"");
                self.buffer.emit(value);
                self.buffer.emit("\")");
            }
            ExprData::Char(value) => {
                self.buffer.emit("(");
                self.buffer.emit_type(scopes, &expr.ty);
                self.buffer.emit(")");
                self.buffer.emit(format!("{:#x}", value as u32));
            }
            ExprData::Symbol(symbol) => match symbol {
                Symbol::Func(data) => self.buffer.emit_fn_name(scopes, data.id, &data.generics),
                Symbol::Var(id) => self.buffer.emit_var_name(scopes, id),
            },
            ExprData::Instance(members) => {
                self.buffer.emit("(");
                self.buffer.emit_type(scopes, &expr.ty);
                self.buffer.emit("){");
                for (name, value) in members {
                    self.buffer.emit(format!(".{name} = "));
                    self.compile_expr(scopes, value);
                    self.buffer.emit(", ");
                }
                self.buffer.emit("}");

                if let TypeId::UserType(data) = expr.ty {
                    self.structs
                        .insert(GenericUserType::new(data.id, data.generics));
                } else {
                    panic!("ICE: Constructing instance of non-struct type!");
                }
            }
            ExprData::None => todo!(),
            ExprData::Assign {
                target,
                binary,
                value,
            } => {
                self.compile_expr(scopes, *target);
                if let Some(binary) = binary {
                    self.buffer.emit(format!(" {binary}= "));
                } else {
                    self.buffer.emit(" = ");
                }
                self.compile_expr(scopes, *value);
            }
            ExprData::Block(block) => {
                self.emit_block(scopes, block);
            }
            ExprData::If { .. } => todo!(),
            ExprData::Loop { .. } => todo!(),
            ExprData::For { .. } => todo!(),
            ExprData::Member { source, member } => {
                self.buffer.emit("(");
                let mut ty = &source.ty;
                while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = ty {
                    self.buffer.emit("*");
                    ty = inner;
                }
                self.compile_expr(scopes, *source);
                self.buffer.emit(format!(").{member}"));
            }
            ExprData::Subscript { .. } => todo!(),
            ExprData::Return(expr) => {
                // TODO: when return is used as anything except a StmtExpr, we will have to change
                // the generated code to accomodate it
                self.buffer.emit("return ");
                self.compile_expr(scopes, *expr);
            }
            ExprData::Yield(_) => {
                //                 let block = self.current_block.as_ref().unwrap();
                //                 let assign = format!("{} = ", block.variable);
                //                 let goto = format!("; goto {}", block.label);
                //
                //                 self.buffer.emit(assign);
                //                 self.compile_expr(scopes, *expr);
                //                 self.buffer.emit(goto);
                todo!()
            }
            ExprData::Break(_) => todo!(),
            ExprData::Continue => todo!(),
            ExprData::Error => {
                panic!("ICE: ExprData::Error in compile_expr");
            }
        }
    }

    fn emit_block(&mut self, scopes: &Scopes, block: Block) {
        self.buffer.emit("{");
        for stmt in block.body.into_iter() {
            self.compile_stmt(scopes, stmt);
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
            self.buffer.emit_type_name(scopes, ut.id, &ut.generics);
            self.buffer.emit("{");
            for (name, member) in scopes.get_user_type(ut.id).data.as_struct().unwrap().iter() {
                let mut ty = member.ty.clone();
                ty.fill_type_generics(scopes, &ut.generics);

                self.buffer.emit_type(scopes, &ty);
                self.buffer.emit(format!(" {}", name));
                self.buffer.emit(";");
                if !member.public {
                    self.buffer.emit("/* private */ \n")
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
                        Span {
                            loc: Location {
                                row: 0,
                                col: 0,
                                pos: 0,
                            },
                            len: 0,
                        },
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

        self.buffer.emit("struct ");
        self.buffer.emit_type_name(scopes, ut.id, &ut.generics);
        self.buffer.emit(";");

        let mut deps = Vec::new();
        for (_, member) in scopes.get_user_type(ut.id).data.as_struct().unwrap().iter() {
            let mut ty = member.ty.clone();
            ty.fill_type_generics(scopes, &ut.generics);

            while matches!(ty, TypeId::Option(_) | TypeId::Array(_)) {
                while let TypeId::Option(inner) = ty {
                    ty = *inner;
                }
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
