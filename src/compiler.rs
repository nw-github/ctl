use crate::{
    ast::{
        expr::{Expr, UnaryOp},
        stmt::{FnDecl, Param, Stmt},
    },
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::{CheckedFnDecl, CheckedParam, CheckedStmt},
        TypeId,
    },
    typecheck::{CheckedAst, Scope, Type},
};

const RT_NAMESPACE: &str = "::ctl_runtime";

pub struct Compiler {
    buffer: String,
    types: Vec<Type>,
    scopes: Vec<Scope>,
}

impl Compiler {
    pub fn compile(ast: CheckedAst) -> String {
        let mut this = Self {
            buffer: String::new(),
            types: ast.types,
            scopes: ast.scopes,
        };
        this.emit("#include <ctl/runtime.hpp>\n\n");
        this.emit(format!("using namespace {RT_NAMESPACE}::literals;\n\n"));
        this.compile_stmt(&ast.stmt);
        this.buffer
    }

    pub fn compile_stmt(&mut self, stmt: &CheckedStmt) {
        match stmt {
            CheckedStmt::Expr(expr) => {
                self.compile_expr(expr);
                self.emit(";");
            }
            CheckedStmt::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                if !mutable {
                    self.emit("const ");
                }

                self.emit_type(*ty);
                if let Some(value) = value {
                    self.emit(format!(" {name} = "));
                    self.compile_expr(value);
                } else {
                    self.emit(format!(" {name}"));
                }
                self.emit(";");
            }
            CheckedStmt::Fn(f) => {
                self.emit_fn_decl(&f.header);
                self.emit("{");
                for stmt in f.body.body.iter() {
                    self.compile_stmt(stmt);
                }
                self.emit("}");
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
                self.compile_expr(value);
                self.emit(";");
            }
            CheckedStmt::Module {
                public: _,
                name,
                body,
            } => {
                self.emit(format!("namespace {name} {{"));
                for stmt in body.body.iter() {
                    self.compile_stmt(stmt);
                }
                self.emit("}");
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
            ExprData::Unary { op, expr } => {
                match op {
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
                }
            }
            ExprData::Call { callee, args } => todo!(),
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
            ExprData::Symbol(_) => todo!(),
            ExprData::Instance { name, members } => todo!(),
            ExprData::None => todo!(),
            ExprData::Assign {
                target,
                binary,
                value,
            } => todo!(),
            ExprData::Block(_) => todo!(),
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
            ExprData::Return(_) => todo!(),
            ExprData::Yield(_) => todo!(),
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
            Type::Function {} => todo!(),
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
}
