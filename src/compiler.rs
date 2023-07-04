use std::collections::HashMap;

use crate::{
    ast::expr::UnaryOp,
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::CheckedStmt,
        Block,
    },
    lexer::{Location, Span},
    typecheck::{
        CheckedAst, CheckedPrototype, Function, FunctionId, Scopes, Symbol, TypeId, UserTypeData,
        UserTypeId, Variable, VariableId,
    },
    Error,
};

const RT_PREFIX: &str = "CTL_RUNTIME_";

pub struct BlockInfo {
    variable: String,
    label: String,
}

pub struct Compiler {
    buffer: String,
    current_block: Option<BlockInfo>,
    block_number: usize,
    main: Option<FunctionId>,
}

impl Compiler {
    pub fn compile(mut ast: CheckedAst) -> Result<String, Error> {
        let mut this = Self {
            buffer: String::new(),
            current_block: None,
            block_number: 0,
            main: None,
        };
        this.emit(concat!(include_str!("../runtime/ctl.h"), "\n\n"));
        this.emit_all_structs(&ast.scopes)?;
        this.emit_all_functions(&mut ast.scopes);
        this.compile_stmt(&mut ast.scopes, &mut ast.stmt);
        if let Some(main) = this.main {
            this.emit("int main(int argc, char **argv) {");
            this.emit("GC_INIT();");
            this.emit("(void)argc;");
            this.emit("(void)argv;");
            this.emit("return ");
            this.emit_fn_name(&ast.scopes, main);
            this.emit("(); }");
        }

        Ok(this.buffer)
    }

    fn emit_all_structs(&mut self, scopes: &Scopes) -> Result<(), Error> {
        fn dfs(
            sid: UserTypeId,
            structs: &HashMap<UserTypeId, Vec<&UserTypeId>>,
            visited: &mut HashMap<UserTypeId, bool>,
            result: &mut Vec<UserTypeId>,
        ) -> Result<(), (UserTypeId, UserTypeId)> {
            visited.insert(sid, true);
            if let Some(deps) = structs.get(&sid) {
                for &&dep in deps {
                    match visited.get(&dep) {
                        Some(true) => return Err((dep, sid)),
                        None => dfs(dep, structs, visited, result)?,
                        _ => {}
                    }
                }
            }
            visited.insert(sid, false);
            result.push(sid);
            Ok(())
        }

        let mut structs = HashMap::new();
        for scope in scopes.scopes().iter() {
            for &id in scope.types.iter() {
                let UserTypeData::Struct(members) = &scopes.get_user_type(id).data else {
                    continue;
                };

                self.emit("struct ");
                self.emit_type_name(scopes, id);
                self.emit(";");

                structs.insert(
                    id,
                    members
                        .iter()
                        .filter_map(|s| {
                            let mut ty = &s.1.ty;
                            while matches!(ty, TypeId::Option(_) | TypeId::Array(_)) {
                                while let TypeId::Option(inner) = ty {
                                    ty = inner.as_ref();
                                }
                                while let TypeId::Array(inner) = ty {
                                    ty = &inner.as_ref().0;
                                }
                            }

                            match ty {
                                TypeId::UserType(id) => Some(id),
                                _ => None,
                            }
                        })
                        .collect::<Vec<_>>(),
                );
            }
        }

        let mut result = Vec::new();
        let mut state = HashMap::new();
        for sid in structs.keys() {
            if !state.contains_key(sid) {
                dfs(*sid, &structs, &mut state, &mut result).map_err(|(a, b)| {
                    // TODO: figure out a real span here
                    Error::new(
                        format!(
                            "recursive dependency detected between {} and {}.",
                            scopes.get_user_type(a).name,
                            scopes.get_user_type(b).name,
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

        for &id in result.iter() {
            self.emit("struct ");
            self.emit_type_name(scopes, id);
            self.emit("{");
            for (name, member) in scopes.get_user_type(id).data.as_struct().unwrap().iter() {
                self.emit_type(scopes, &member.ty);
                self.emit(format!(" {}", name));
                self.emit(";");
                if !member.public {
                    self.emit("/* private */ \n")
                }
            }

            self.emit("};");
        }

        Ok(())
    }

    fn emit_all_functions(&mut self, scopes: &mut Scopes) {
        let mut fns = Vec::new();
        for scope in scopes.scopes().iter() {
            for &id in scope.fns.iter() {
                if scopes.get_func(id).inst || !scopes.get_func(id).proto.type_params.is_empty() {
                    continue;
                }

                self.emit_prototype(scopes, id);
                self.emit(";");
                fns.push(id);
            }
        }

        for id in fns {
            self.emit_prototype(scopes, id);
            // FIXME: this clone hack
            self.emit_block(scopes, &mut scopes.get_func(id).body.clone().unwrap());
        }
    }

    fn compile_stmt(&mut self, scopes: &mut Scopes, stmt: &mut CheckedStmt) {
        match stmt {
            CheckedStmt::Expr(expr) => {
                self.hoist_blocks(scopes, expr);
                self.compile_expr(scopes, expr);
                self.emit(";");
            }
            CheckedStmt::Let {
                name,
                mutable,
                value,
            } => {
                match value {
                    Ok(value) => {
                        self.hoist_blocks(scopes, value);
                        self.emit_type(scopes, &value.ty);
                        if !*mutable {
                            self.emit(" const ");
                        }
                        self.emit(format!(" {name} = "));
                        self.compile_expr(scopes, value);
                    }
                    Err(ty) => {
                        self.emit_type(scopes, ty);
                        if !*mutable {
                            self.emit(" const ");
                        }

                        self.emit(format!(" {name}"));
                    }
                }
                self.emit(";");
            }
            CheckedStmt::Static(id, value) => {
                self.emit("static ");
                self.emit_type(scopes, &value.ty);
                self.emit(" const ");
                self.emit_var_name(scopes, *id);
                self.emit(" = ");
                // FIXME: blocks in statics...
                self.hoist_blocks(scopes, value);
                self.compile_expr(scopes, value);
                self.emit(";");
            }
            CheckedStmt::Module { body, .. } => {
                for stmt in body.body.iter_mut() {
                    self.compile_stmt(scopes, stmt);
                }
            }
            CheckedStmt::None => {}
            CheckedStmt::Error => {
                panic!("ICE: CheckedStmt::Error in compile_stmt");
            }
        }
    }

    fn compile_expr(&mut self, scopes: &mut Scopes, expr: &CheckedExpr) {
        match &expr.data {
            ExprData::Binary { op, left, right } => {
                self.emit("(");
                self.compile_expr(scopes, left);
                self.emit(format!(" {op} "));
                self.compile_expr(scopes, right);
                self.emit(")");
            }
            ExprData::Unary { op, expr } => match op {
                UnaryOp::Plus => {
                    self.emit("+");
                    self.compile_expr(scopes, expr);
                }
                UnaryOp::Neg => {
                    self.emit("-");
                    self.compile_expr(scopes, expr);
                }
                UnaryOp::PostIncrement => {
                    self.compile_expr(scopes, expr);
                    self.emit("++");
                }
                UnaryOp::PostDecrement => {
                    self.compile_expr(scopes, expr);
                    self.emit("--");
                }
                UnaryOp::PreIncrement => {
                    self.emit("++");
                    self.compile_expr(scopes, expr);
                }
                UnaryOp::PreDecrement => {
                    self.emit("--");
                    self.compile_expr(scopes, expr);
                }
                UnaryOp::Not => {
                    if expr.ty.is_numeric() {
                        self.emit("~");
                        self.compile_expr(scopes, expr);
                    } else {
                        self.emit("!");
                        self.compile_expr(scopes, expr);
                    }
                }
                UnaryOp::Deref => todo!(),
                UnaryOp::Addr => todo!(),
                UnaryOp::AddrMut => todo!(),
                UnaryOp::IntoError => todo!(),
                UnaryOp::Try => todo!(),
                UnaryOp::Sizeof => todo!(),
            },
            ExprData::Call { func, args } => {
                self.emit_fn_name(scopes, *func);
                self.emit("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }

                    self.compile_expr(scopes, arg);
                }
                self.emit(")");
            }
            ExprData::MemberCall { func, this, args } => {
                self.emit_fn_name(scopes, *func);
                self.emit("(");

                let mut source_ty = &this.ty;
                let target = &scopes.get_func(*func).proto.params[0].ty;
                if !matches!(source_ty, TypeId::Ptr(_) | TypeId::MutPtr(_)) {
                    self.emit("&");
                } else {
                    while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = source_ty {
                        if source_ty == target {
                            break;
                        }

                        self.emit("*");
                        source_ty = inner;
                    }
                }

                self.compile_expr(scopes, this);

                for arg in args {
                    self.emit(", ");
                    self.compile_expr(scopes, arg);
                }
                self.emit(")");
            }
            ExprData::Array(_) => todo!(),
            ExprData::ArrayWithInit { .. } => todo!(),
            ExprData::Tuple(_) => todo!(),
            ExprData::Map(_) => todo!(),
            ExprData::Bool(value) => {
                if *value {
                    self.emit(format!("{RT_PREFIX}::boolean::TRUE"));
                } else {
                    self.emit(format!("{RT_PREFIX}::boolean::FALSE"));
                }
            }
            ExprData::Signed(value) => self.emit(format!("{value}")),
            ExprData::Unsigned(value) => self.emit(format!("{value}")),
            ExprData::Float(value) => self.emit(value),
            ExprData::String(_) => todo!(),
            ExprData::Symbol(symbol) => match *symbol {
                Symbol::Func(id) => self.emit_fn_name(scopes, id),
                Symbol::Var(id) => self.emit_var_name(scopes, id),
                Symbol::GenericFunc(_) => todo!(),
            },
            ExprData::Instance(members) => {
                self.emit("(");
                self.emit_type(scopes, &expr.ty);
                self.emit("){");
                for (name, value) in members {
                    self.emit(format!(".{name} = "));
                    self.compile_expr(scopes, value);
                    self.emit(", ");
                }
                self.emit("}");
            }
            ExprData::None => todo!(),
            ExprData::Assign {
                target,
                binary,
                value,
            } => {
                self.compile_expr(scopes, target);
                if let Some(binary) = binary {
                    self.emit(format!(" {binary}= "));
                } else {
                    self.emit(" = ");
                }
                self.compile_expr(scopes, value);
            }
            ExprData::Block(_) => panic!("ICE: ExprData::Block in compile_expr"),
            ExprData::If { .. } => todo!(),
            ExprData::Loop { .. } => todo!(),
            ExprData::For { .. } => todo!(),
            ExprData::Member { source, member } => {
                self.emit("(");
                let mut ty = &source.ty;
                while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = ty {
                    self.emit("*");
                    ty = inner;
                }
                self.compile_expr(scopes, source);
                self.emit(format!(").{member}"));
            }
            ExprData::Subscript { .. } => todo!(),
            ExprData::Return(expr) => {
                // TODO: when return is used as anything except a StmtExpr, we will have to change
                // the generated code to accomodate it
                self.emit("return ");
                self.compile_expr(scopes, expr);
            }
            ExprData::Yield(expr) => {
                let block = self.current_block.as_ref().unwrap();
                let assign = format!("{} = ", block.variable);
                let goto = format!("; goto {}", block.label);

                self.emit(assign);
                self.compile_expr(scopes, expr);
                self.emit(goto);
            }
            ExprData::Break(_) => todo!(),
            ExprData::Continue => todo!(),
            ExprData::Error => {
                panic!("ICE: ExprData::Error in compile_expr");
            }
        }
    }

    fn hoist_blocks(&mut self, scopes: &mut Scopes, expr: &mut CheckedExpr) {
        match &mut expr.data {
            ExprData::Binary { op: _, left, right } => {
                self.hoist_blocks(scopes, left);
                self.hoist_blocks(scopes, right);
            }
            ExprData::Unary { op: _, expr } => {
                self.hoist_blocks(scopes, expr);
            }
            ExprData::Call { args, .. } => {
                for arg in args.iter_mut() {
                    self.hoist_blocks(scopes, arg);
                }
            }
            ExprData::Subscript { callee, args }
            | ExprData::MemberCall {
                this: callee, args, ..
            } => {
                self.hoist_blocks(scopes, &mut *callee);
                for arg in args.iter_mut() {
                    self.hoist_blocks(scopes, arg);
                }
            }
            ExprData::Array(args) => {
                for arg in args.iter_mut() {
                    self.hoist_blocks(scopes, arg);
                }
            }
            ExprData::ArrayWithInit { init, count: _ } => {
                self.hoist_blocks(scopes, init);
                //self.hoist_blocks(scopes, count);
            }
            ExprData::Tuple(args) => {
                for arg in args.iter_mut() {
                    self.hoist_blocks(scopes, arg);
                }
            }
            ExprData::Map(args) => {
                for (key, value) in args.iter_mut() {
                    self.hoist_blocks(scopes, key);
                    self.hoist_blocks(scopes, value);
                }
            }
            ExprData::Instance(members) => {
                for (_, value) in members.iter_mut() {
                    self.hoist_blocks(scopes, value);
                }
            }
            ExprData::Assign {
                target,
                binary: _,
                value,
            } => {
                self.hoist_blocks(scopes, target);
                self.hoist_blocks(scopes, value);
            }
            ExprData::If { cond, .. } => {
                self.hoist_blocks(scopes, cond);
            }
            ExprData::Loop { .. } => {
                /*
                    in a situation like:

                loop { if a { return; } else { true } }
                {

                }

                    if we hoist the block here, the condition wont be executed every iteration
                 */

                todo!()
            }
            ExprData::For {
                var: _,
                iter,
                body: _,
            } => {
                self.hoist_blocks(scopes, iter);
            }
            ExprData::Member { source, member: _ } => {
                self.hoist_blocks(scopes, source);
            }
            ExprData::Return(expr) | ExprData::Yield(expr) | ExprData::Break(expr) => {
                self.hoist_blocks(scopes, expr)
            }
            ExprData::Block(block) => {
                let variable = format!("$ctl_block{}", self.block_number);
                let label = format!("$ctl_label{}", self.block_number);
                let old_block = self.current_block.replace(BlockInfo {
                    variable: variable.clone(),
                    label: label.clone(),
                });
                self.block_number += 1;

                self.emit_type(scopes, &expr.ty);
                self.emit(format!(" {variable};"));
                self.emit_block(scopes, block);
                self.emit(format!("{label}:\n"));
                self.current_block = old_block;

                expr.data = ExprData::Symbol(Symbol::Var(scopes.insert_var(Variable {
                    name: variable,
                    ty: TypeId::Unknown,
                    is_static: false,
                    mutable: false,
                    public: false,
                })));
            }
            _ => {}
        }
    }

    fn emit(&mut self, source: impl AsRef<str>) {
        self.buffer.push_str(source.as_ref());
    }

    fn emit_type(&mut self, scopes: &Scopes, id: &TypeId) {
        match id {
            TypeId::Void => self.emit(format!("{RT_PREFIX}void")),
            TypeId::Never => todo!(),
            TypeId::Int(bits) => self.emit(format!("{RT_PREFIX}i{bits}")),
            TypeId::Uint(bits) => self.emit(format!("{RT_PREFIX}u{bits}")),
            TypeId::Isize => self.emit(format!("{RT_PREFIX}isize")),
            TypeId::Usize => self.emit(format!("{RT_PREFIX}usize")),
            TypeId::F32 => self.emit(format!("{RT_PREFIX}f32")),
            TypeId::F64 => self.emit(format!("{RT_PREFIX}f64")),
            TypeId::Bool => self.emit(format!("{RT_PREFIX}bool")),
            TypeId::String => self.emit(format!("{RT_PREFIX}str")),
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
                self.emit(format!("{RT_PREFIX}option_"));
                self.emit_type(scopes, inner);
            }
            TypeId::Func(_) => todo!(),
            TypeId::GenericFunc(_) => todo!(),
            &TypeId::UserType(id) => {
                if scopes.get_user_type(id).data.is_struct() {
                    self.emit("struct ");
                    self.emit_type_name(scopes, id);
                }
            }
            TypeId::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            TypeId::Array(_) => todo!(),
        }
    }

    fn emit_fn_name(&mut self, scopes: &Scopes, id: FunctionId) {
        let func = scopes.get_func(id);
        self.emit(scopes.full_name(func.scope, &func.proto.name));
    }

    fn emit_type_name(&mut self, scopes: &Scopes, id: UserTypeId) {
        let ty = scopes.get_user_type(id);
        self.emit(scopes.full_name(ty.scope, &ty.name));
    }

    fn emit_var_name(&mut self, scopes: &Scopes, id: VariableId) {
        let var = scopes.get_var(id);
        if var.is_static {
            self.emit(scopes.full_name(var.scope, &var.name));
        } else {
            self.emit(&var.name);
        }
    }

    fn emit_prototype(&mut self, scopes: &Scopes, id: FunctionId) {
        let Function {
            proto:
                CheckedPrototype {
                    public: _,
                    name,
                    is_async: _,
                    is_extern: _,
                    type_params: _,
                    params,
                    ret,
                },
            ..
        } = &scopes.get_func(id).item;

        self.emit_type(scopes, ret);
        if name == "main" {
            self.main = Some(id);
        }

        self.emit(" ");
        self.emit_fn_name(scopes, id);
        self.emit("(");
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }

            self.emit_type(scopes, &param.ty);
            if !param.mutable {
                self.emit(" const");
            }

            self.emit(format!(" {}", param.name));
        }
        self.emit(")");
    }

    fn emit_block(&mut self, scopes: &mut Scopes, block: &mut Block) {
        self.emit("{");
        for stmt in block.body.iter_mut() {
            self.compile_stmt(scopes, stmt);
        }
        self.emit("}");
    }
}
