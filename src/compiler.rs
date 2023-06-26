use crate::{
    ast::{
        expr::Expr,
        stmt::{FnDecl, Param, Stmt},
    },
    checked_ast::{
        expr::CheckedExpr,
        stmt::{CheckedFnDecl, CheckedParam, CheckedStmt},
        TypeId, ResolvedType,
    }, typecheck::{CheckedAst, Scope},
};

const RT_NAMESPACE: &str = "::ctl_runtime";

pub struct Compiler {
    buffer: String,
    types: Vec<ResolvedType>,
    scopes: Vec<Scope>,
}

impl Compiler {
    pub fn compile(ast: CheckedAst) -> String {
        let mut this = Self {
            buffer: String::new(),
            types: ast.types,
            scopes: ast.scopes,
        };
        this.compile_stmt(&ast.stmt);
        this.buffer
    }

    pub fn compile_stmt(&mut self, stmt: &CheckedStmt) {
        match stmt {
            CheckedStmt::Expr(_) => todo!(),
            CheckedStmt::Let {
                name,
                ty,
                mutable,
                value,
            } => todo!(),
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
                public,
                name,
                ty,
                value,
            } => todo!(),
            CheckedStmt::Module { public, name, body } => {
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

    pub fn compile_expr(&mut self, expr: &CheckedExpr) {}

    pub fn emit(&mut self, source: impl AsRef<str>) {
        self.buffer.push_str(source.as_ref());
    }

    pub fn emit_type(&mut self, id: TypeId) {
        match &self.types[id] {
            ResolvedType::Void => self.emit("void"),
            ResolvedType::Never => todo!(),
            ResolvedType::Int(bits) => self.emit(format!("{RT_NAMESPACE}::i{bits}")),
            ResolvedType::Uint(_) => todo!(),
            ResolvedType::F32 => todo!(),
            ResolvedType::F64 => todo!(),
            ResolvedType::IntGeneric => todo!(),
            ResolvedType::FloatGeneric => todo!(),
            ResolvedType::Struct(_) => todo!(),
            ResolvedType::Union { tag, base } => todo!(),
            ResolvedType::Enum {  } => todo!(),
            ResolvedType::Interface {  } => todo!(),
            ResolvedType::Function {  } => todo!(),
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

            if param.mutable {
                self.emit("const ");
            }

            self.emit_type(param.ty);
            self.emit(format!(" {}", param.name));
        }
        self.emit(")");
    }
}
