use crate::{
    ast::expr::UnaryOp,
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::{CheckedFnDecl, CheckedStmt, CheckedStruct, CheckedUserType},
        Block,
    },
    typecheck::{CheckedAst, Scope, Type, TypeId},
};

const RT_NAMESPACE: &str = "::ctl_runtime";

pub struct BlockInfo {
    variable: String,
    label: String,
}

pub struct Compiler {
    buffer: String,
    types: Vec<Type>,
    scopes: Vec<Scope>,
    current_block: Option<BlockInfo>,
    block_number: usize,
}

impl Compiler {
    pub fn compile(mut ast: CheckedAst) -> String {
        let mut this = Self {
            buffer: String::new(),
            types: ast.types,
            scopes: ast.scopes,
            current_block: None,
            block_number: 0,
        };
        this.emit("#include <ctl/runtime.hpp>\n\n");
        this.emit(format!("using namespace {RT_NAMESPACE}::literals;\n\n"));
        this.compile_stmt(&mut ast.stmt);
        this.emit(
            "
int main([[maybe_unused]] int argc, [[maybe_unused]] char **argv) {
    auto result = tmpname::$ctl_main();
    return result;
}    
        ",
        );

        this.buffer
    }

    pub fn compile_stmt(&mut self, stmt: &mut CheckedStmt) {
        match stmt {
            CheckedStmt::Expr(expr) => {
                self.hoist_blocks(expr);
                self.compile_expr(expr);
                self.emit(";");
            }
            CheckedStmt::Let {
                name,
                mutable,
                value,
            } => {
                match value {
                    Ok(value) => {
                        self.hoist_blocks(value);
                        if !*mutable {
                            self.emit("const ");
                        }

                        self.emit_type(&value.ty);
                        self.emit(format!(" {name} = "));
                        self.compile_expr(value);
                    }
                    Err(ty) => {
                        if !*mutable {
                            self.emit("const ");
                        }
    
                        self.emit_type(ty);
                        self.emit(format!(" {name}"));
                    }
                }
                self.emit(";");
            }
            CheckedStmt::Fn(f) => {
                self.emit_fn_decl(&f.header, "");
                self.emit_block(&mut f.body);
            }
            CheckedStmt::UserType(data) => match data {
                CheckedUserType::Struct(CheckedStruct {
                    public: _,
                    name,
                    members,
                    functions,
                }) => {
                    self.emit(format!("struct {name} {{"));
                    members.sort_by_key(|member| !member.public);
                    let mut emitted_priv = false;
                    for member in members.iter() {
                        if !emitted_priv && !member.public {
                            self.emit("private: ");
                            emitted_priv = true;
                        }

                        self.emit_type(&member.ty);
                        if let Some(value) = &member.value {
                            self.emit(format!(" {} = ", member.name));
                            // TODO: what if value is a block?
                            // FIXME: instead of filling in default values here, we should do it at
                            // the construction site
                            // struct A { a: i32 = 5, b: 132 = 2 };
                            //   A { a: 0 }   becomes A { a: 0, b: 5 }
                            self.compile_expr(value);
                        } else {
                            self.emit(format!(" {}", member.name));
                        }
                        self.emit(";");
                    }

                    functions.sort_by_key(|function| !function.header.public);
                    emitted_priv = false;
                    self.emit("public: ");
                    for f in functions.iter() {
                        if !emitted_priv && !f.header.public {
                            self.emit("private: ");
                            emitted_priv = true;
                        }

                        self.emit("static ");
                        self.emit_fn_decl(&f.header, "");
                        self.emit(";");
                    }

                    self.emit("};");

                    for f in functions.iter_mut() {
                        self.emit_fn_decl(&f.header, name);
                        self.emit_block(&mut f.body);
                    }
                }
                CheckedUserType::Union { .. } => todo!(),
                CheckedUserType::Interface { .. } => todo!(),
                CheckedUserType::Enum { .. } => todo!(),
            },
            CheckedStmt::Static {
                public: _,
                name,
                value,
            } => {
                self.emit("static const ");
                self.emit_type(&value.ty);
                self.emit(format!(" {name} = "));
                // FIXME: blocks in statics...
                self.hoist_blocks(value);
                self.compile_expr(value);
                self.emit(";");
            }
            CheckedStmt::Module {
                public: _,
                name,
                body,
            } => {
                self.emit(format!("namespace {name} "));
                self.emit_block(body);
            }
            CheckedStmt::Error => {
                panic!("ICE: CheckedStmt::Error in compile_stmt");
            }
        }
    }

    pub fn compile_expr(&mut self, expr: &CheckedExpr) {
        match &expr.data {
            ExprData::Binary { op, left, right } => {
                self.emit("(");
                self.compile_expr(left);
                self.emit(format!(" {op} "));
                self.compile_expr(right);
                self.emit(")");
            }
            ExprData::Unary { op, expr } => match op {
                UnaryOp::Plus => {
                    self.emit("+");
                    self.compile_expr(expr);
                }
                UnaryOp::Neg => {
                    self.emit("-");
                    self.compile_expr(expr);
                }
                UnaryOp::PostIncrement => {
                    self.compile_expr(expr);
                    self.emit("++");
                }
                UnaryOp::PostDecrement => {
                    self.compile_expr(expr);
                    self.emit("--");
                }
                UnaryOp::PreIncrement => {
                    self.emit("++");
                    self.compile_expr(expr);
                }
                UnaryOp::PreDecrement => {
                    self.emit("--");
                    self.compile_expr(expr);
                }
                UnaryOp::Not => {
                    if expr.ty.is_numeric() {
                        self.emit("~");
                        self.compile_expr(expr);
                    } else {
                        self.emit("!");
                        self.compile_expr(expr);
                    }
                }
                UnaryOp::Deref => todo!(),
                UnaryOp::Addr => todo!(),
                UnaryOp::AddrMut => todo!(),
                UnaryOp::IntoError => todo!(),
                UnaryOp::Try => todo!(),
                UnaryOp::Sizeof => todo!(),
            }
            ExprData::Call { callee, args } => {
                self.compile_expr(callee);
                self.emit("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }

                    self.compile_expr(arg);
                }
                self.emit(")");
            }
            ExprData::Array(_) => todo!(),
            ExprData::ArrayWithInit { init, count } => todo!(),
            ExprData::Tuple(_) => todo!(),
            ExprData::Map(_) => todo!(),
            ExprData::Bool(value) => {
                if *value {
                    self.emit(format!("{RT_NAMESPACE}::boolean::TRUE"));
                } else {
                    self.emit(format!("{RT_NAMESPACE}::boolean::FALSE"));
                }
            }
            ExprData::Signed(value) => match expr.ty {
                TypeId::Int(base) => {
                    self.emit(format!("{value}_i{base}"));
                }
                _ => panic!(
                    "ICE: ExprData::Signed with non-int type {:?}",
                    expr.ty
                ),
            }
            ExprData::Unsigned(value) => match expr.ty {
                TypeId::Uint(base) => {
                    self.emit(format!("{value}_u{base}"));
                }
                _ => panic!(
                    "ICE: ExprData::Unsigned with non-uint type {:?}",
                    expr.ty
                ),
            }
            ExprData::Float(value) => {
                // TODO: probably should check the range or something
                match expr.ty {
                    TypeId::F32 => self.emit(format!("{value}_f32")),
                    TypeId::F64 => self.emit(format!("{value}_f64")),
                    _ => panic!(
                        "ICE: ExprData::Float with non-float type {:?}",
                        expr.ty
                    ),
                }
                self.emit(value);
            }
            ExprData::String(_) => todo!(),
            ExprData::Symbol(name) => {
                self.emit(name);
            }
            ExprData::Instance { name, members } => todo!(),
            ExprData::None => todo!(),
            ExprData::Assign {
                target,
                binary,
                value,
            } => {
                self.compile_expr(target);
                if let Some(binary) = binary {
                    self.emit(format!(" {binary}= "));
                } else {
                    self.emit(" = ");
                }
                self.compile_expr(value);
            }
            ExprData::Block(_) => panic!("ICE: ExprData::Block in compile_expr"),
            ExprData::If {
                cond,
                if_branch,
                else_branch,
            } => todo!(),
            ExprData::Loop {
                cond,
                body,
                do_while,
            } => todo!(),
            ExprData::For { var, iter, body } => todo!(),
            ExprData::Member { source, member } => todo!(),
            ExprData::Subscript { callee, args } => todo!(),
            ExprData::Return(expr) => {
                // TODO: when return is used as anything except a StmtExpr, we will have to change
                // the generated code to accomodate it
                self.emit("return ");
                self.compile_expr(expr);
            }
            ExprData::Yield(expr) => {
                let Some(block) = &self.current_block else {
                    panic!("ICE: compiling yield, but there is no hoisted block");
                };

                let assign = format!("{} = ", block.variable);
                let goto = format!("; goto {}", block.label);

                self.emit(assign);
                self.compile_expr(expr);
                self.emit(goto);
            }
            ExprData::Break(_) => todo!(),
            ExprData::Range {
                start,
                end,
                inclusive,
            } => todo!(),
            ExprData::Continue => todo!(),
            ExprData::Error => {
                panic!("ICE: ExprData::Error in compile_expr");
            }
        }
    }

    pub fn hoist_blocks(&mut self, expr: &mut CheckedExpr) {
        match &mut expr.data {
            ExprData::Binary { op: _, left, right } => {
                self.hoist_blocks(left);
                self.hoist_blocks(right);
            }
            ExprData::Unary { op: _, expr } => {
                self.hoist_blocks(expr);
            }
            ExprData::Call { callee, args } | ExprData::Subscript { callee, args } => {
                self.hoist_blocks(&mut *callee);
                for arg in args.iter_mut() {
                    self.hoist_blocks(arg);
                }
            }
            ExprData::Array(args) => {
                for arg in args.iter_mut() {
                    self.hoist_blocks(arg);
                }
            }
            ExprData::ArrayWithInit { init, count } => {
                self.hoist_blocks(init);
                self.hoist_blocks(count);
            }
            ExprData::Tuple(args) => {
                for arg in args.iter_mut() {
                    self.hoist_blocks(arg);
                }
            }
            ExprData::Map(args) => {
                for (key, value) in args.iter_mut() {
                    self.hoist_blocks(key);
                    self.hoist_blocks(value);
                }
            }
            ExprData::Instance { name: _, members } => {
                for (_, value) in members.iter_mut() {
                    self.hoist_blocks(value);
                }
            }
            ExprData::Assign {
                target,
                binary: _,
                value,
            } => {
                self.hoist_blocks(target);
                self.hoist_blocks(value);
            }
            ExprData::If { cond, .. } => {
                self.hoist_blocks(cond);
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
                self.hoist_blocks(iter);
            }
            ExprData::Member { source, member: _ } => {
                self.hoist_blocks(source);
            }
            ExprData::Return(expr) | ExprData::Yield(expr) | ExprData::Break(expr) => {
                self.hoist_blocks(expr)
            }
            ExprData::Range {
                start,
                end,
                inclusive: _,
            } => {
                if let Some(start) = start {
                    self.hoist_blocks(start);
                }

                if let Some(end) = end {
                    self.hoist_blocks(end);
                }
            }
            ExprData::Block(block) => {
                let variable = format!("$ctl_block{}", self.block_number);
                let label = format!("$ctl_label{}", self.block_number);
                let old_block = self.current_block.replace(BlockInfo {
                    variable: variable.clone(),
                    label: label.clone(),
                });
                self.block_number += 1;

                self.emit_type(&expr.ty);
                self.emit(format!(" {variable};"));
                self.emit_block(block);
                self.emit(format!("{label}:\n"));
                self.current_block = old_block;

                expr.data = ExprData::Symbol(variable);
            }
            _ => {}
        }
    }

    pub fn emit(&mut self, source: impl AsRef<str>) {
        self.buffer.push_str(source.as_ref());
    }

    pub fn emit_type(&mut self, id: &TypeId) {
        match id {
            TypeId::Void => self.emit("void"),
            TypeId::Never => todo!(),
            TypeId::Int(bits) => self.emit(format!("{RT_NAMESPACE}::i{bits}")),
            TypeId::Uint(bits) => self.emit(format!("{RT_NAMESPACE}::u{bits}")),
            TypeId::F32 => self.emit(format!("{RT_NAMESPACE}::f32")),
            TypeId::F64 => self.emit(format!("{RT_NAMESPACE}::f64")),
            TypeId::Bool => self.emit(format!("{RT_NAMESPACE}::boolean")),
            TypeId::IntGeneric | TypeId::FloatGeneric => {
                panic!("ICE: Int/FloatGeneric in emit_type");
            }
            TypeId::Ref(_) => todo!(),
            TypeId::RefMut(_) => todo!(),
            TypeId::Type(id) => {
                match &self.types[*id] {
                    Type::Function { params, ret } => todo!(),
                    // TODO: use fully qualified name
                    Type::Struct(base) => self.emit(base.name.clone()),
                }
            }
        }
    }

    pub fn emit_fn_decl(
        &mut self,
        CheckedFnDecl {
            public: _,
            name,
            is_async: _,
            is_extern: _,
            type_params,
            params,
            ret,
        }: &CheckedFnDecl,
        path: &str,
    ) {
        self.emit_type(ret);
        let name = if name == "main" { "$ctl_main" } else { name };
        if path.is_empty() {
            self.emit(format!(" {name}("));
        } else {
            self.emit(format!(" {path}::{name}("));
        }

        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }

            if !param.mutable {
                self.emit("const ");
            }

            self.emit_type(&param.ty);
            self.emit(format!(" {}", param.name));
        }
        self.emit(")");
    }

    pub fn emit_block(&mut self, block: &mut Block) {
        self.emit("{");
        for stmt in block.body.iter_mut() {
            self.compile_stmt(stmt);
        }
        self.emit("}");
    }
}
