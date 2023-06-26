use crate::{
    ast::{
        expr::{Expr, UnaryOp},
        stmt::{FnDecl, Param, Stmt},
    },
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::{CheckedFnDecl, CheckedParam, CheckedStmt},
        Block, TypeId,
    },
    typecheck::{CheckedAst, Scope, Type},
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
                ty,
                mutable,
                value,
            } => {
                if let Some(value) = value {
                    self.hoist_blocks(value);
                    if !*mutable {
                        self.emit("const ");
                    }

                    self.emit_type(*ty);
                    self.emit(format!(" {name} = "));
                    self.compile_expr(value);
                } else {
                    if !*mutable {
                        self.emit("const ");
                    }

                    self.emit_type(*ty);
                    self.emit(format!(" {name}"));
                }
                self.emit(";");
            }
            CheckedStmt::Fn(f) => {
                self.emit_fn_decl(&f.header);
                self.emit_block(&mut f.body);
            }
            CheckedStmt::UserType(_) => todo!(),
            CheckedStmt::Static {
                public: _,
                name,
                ty,
                value,
            } => {
                self.emit("static const ");
                self.emit_type(*ty);
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
                    if self.types[expr.ty].is_numeric() {
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
            },
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
            ExprData::Signed(value) => match &self.types[expr.ty] {
                Type::Int(base) => {
                    self.emit(format!("{value}_i{base}"));
                }
                _ => panic!(
                    "ICE: ExprData::Signed with non-int type {:?}",
                    self.types[expr.ty]
                ),
            },
            ExprData::Unsigned(value) => match &self.types[expr.ty] {
                Type::Uint(base) => {
                    self.emit(format!("{value}_u{base}"));
                }
                _ => panic!(
                    "ICE: ExprData::Unsigned with non-uint type {:?}",
                    self.types[expr.ty]
                ),
            },
            ExprData::Float(value) => {
                // TODO: probably should check the range or something
                match &self.types[expr.ty] {
                    Type::F32 => self.emit(format!("{value}_f32")),
                    Type::F64 => self.emit(format!("{value}_f64")),
                    _ => panic!(
                        "ICE: ExprData::Float with non-float type {:?}",
                        self.types[expr.ty]
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
            } => todo!(),
            ExprData::Block(body) => panic!("ICE: ExprData::Block in compile_expr"),
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

                self.emit_type(expr.ty);
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

    pub fn emit_type(&mut self, id: TypeId) {
        match &self.types[id] {
            Type::Void => self.emit("void"),
            Type::Never => todo!(),
            Type::Int(bits) => self.emit(format!("{RT_NAMESPACE}::i{bits}")),
            Type::Uint(bits) => self.emit(format!("{RT_NAMESPACE}::u{bits}")),
            Type::F32 => self.emit(format!("{RT_NAMESPACE}::f32")),
            Type::F64 => self.emit(format!("{RT_NAMESPACE}::f64")),
            Type::Bool => self.emit(format!("{RT_NAMESPACE}::boolean")),
            Type::IntGeneric | Type::FloatGeneric => {
                panic!("ICE: Int/FloatGeneric in emit_type");
            }
            Type::Struct(_) => todo!(),
            Type::Union { tag, base } => todo!(),
            Type::Enum {} => todo!(),
            Type::Interface {} => todo!(),
            Type::Function { .. } => todo!(),
        }
    }

    pub fn emit_fn_decl(
        &mut self,
        CheckedFnDecl {
            public,
            name,
            is_async,
            is_extern,
            type_params,
            params,
            ret,
        }: &CheckedFnDecl,
    ) {
        self.emit_type(*ret);
        self.emit(format!(" {name}("));
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }

            if !param.mutable {
                self.emit("const ");
            }

            self.emit_type(param.ty);
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
