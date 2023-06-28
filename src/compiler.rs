use crate::{
    ast::expr::UnaryOp,
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::{CheckedFnDecl, CheckedStmt, CheckedStruct, CheckedUserType},
        Block, ScopeId,
    },
    typecheck::{CheckedAst, Scope, Type, TypeId},
};

const RT_PREFIX: &str = "$CTL_RUNTIME_";
const MAIN_NAME: &str = "$CTL_PROGRAM_MAIN";

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
    current_path: String,
}

impl Compiler {
    pub fn compile(mut ast: CheckedAst) -> String {
        let mut this = Self {
            buffer: String::new(),
            types: ast.types,
            scopes: ast.scopes,
            current_block: None,
            block_number: 0,
            current_path: String::new(),
        };
        this.emit("#include <ctl/runtime.h>\n\n");
        this.compile_stmt(&mut ast.stmt);
        this.emit(format!(
            "
int main(int argc, char **argv) {{
    GC_INIT();

    (void)argc;
    (void)argv;

    return {MAIN_NAME}();
}}   
        ",
        ));

        this.buffer
    }

    fn compile_stmt(&mut self, stmt: &mut CheckedStmt) {
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

                        self.emit_type(&value.ty);
                        if !*mutable {
                            self.emit(" const ");
                        }
                        self.emit(format!(" {name} = "));
                        self.compile_expr(value);
                    }
                    Err(ty) => {
                        self.emit_type(ty);
                        if !*mutable {
                            self.emit("const ");
                        }

                        self.emit(format!(" {name}"));
                    }
                }
                self.emit(";");
            }
            CheckedStmt::Fn(f) => {
                self.emit_fn_decl(&f.header);
                self.emit_block(&mut f.body);
            }
            CheckedStmt::UserType(data) => match data {
                CheckedUserType::Struct(CheckedStruct {
                    public: _,
                    name,
                    members,
                    functions,
                }) => {
                    self.emit(format!("struct {}{name} {{", self.current_path));
                    self.add_to_path(name);
                    for member in members.iter() {
                        self.emit_type(&member.ty);
                        self.emit(format!(" {}", member.name));
                        self.emit(";");
                        if !member.public {
                            self.emit("/* private */ \n")
                        }
                    }

                    self.emit("};");

                    for f in functions.iter_mut() {
                        self.emit_fn_decl(&f.header);
                        self.emit_block(&mut f.body);
                    }
                    self.remove_from_path(name);
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
                self.emit("static ");
                self.emit_type(&value.ty);
                self.emit(format!(" const {}{name} = ", self.current_path));
                // FIXME: blocks in statics...
                self.hoist_blocks(value);
                self.compile_expr(value);
                self.emit(";");
            }
            CheckedStmt::Module {
                name,
                body,
            } => {
                self.add_to_path(name);
                for stmt in body.body.iter_mut() {
                    self.compile_stmt(stmt);
                }
                self.remove_from_path(name);
            }
            CheckedStmt::Error => {
                panic!("ICE: CheckedStmt::Error in compile_stmt");
            }
        }
    }

    fn compile_expr(&mut self, expr: &CheckedExpr) {
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
            ExprData::MemberCall {
                source,
                member,
                ty,
                args,
            } => {
                let TypeId::Type(id) = ty else { unreachable!() };
                let Type::Struct(s) = &self.types[*id] else { unreachable!() };

                self.emit(Self::scope_name(s.scope, &self.scopes));
                self.emit(format!("_{member}"));
                self.emit("(");

                let mut source_ty = &source.ty;
                if source_ty == ty {
                    self.emit("&");
                } else {
                    while let TypeId::Ref(inner) | TypeId::RefMut(inner) = source_ty {
                        if source_ty == ty {
                            break;
                        }
    
                        self.emit("*");
                        source_ty = inner;
                    }
                }

                self.compile_expr(source);

                for arg in args {
                    self.emit(", ");
                    self.compile_expr(arg);
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
            ExprData::Symbol { scope, symbol } => {
                if let Some(scope) = scope {
                    self.emit(Self::scope_name(*scope, &self.scopes));
                    self.emit("_");
                }
                self.emit(symbol);
            }
            ExprData::Instance { members } => {
                self.emit("{");
                for (name, value) in members {
                    self.emit(format!(".{name} = "));
                    self.compile_expr(value);
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
                self.compile_expr(target);
                if let Some(binary) = binary {
                    self.emit(format!(" {binary}= "));
                } else {
                    self.emit(" = ");
                }
                self.compile_expr(value);
            }
            ExprData::Block(_) => panic!("ICE: ExprData::Block in compile_expr"),
            ExprData::If { .. } => todo!(),
            ExprData::Loop { .. } => todo!(),
            ExprData::For { .. } => todo!(),
            ExprData::Member { source, member } => {
                self.emit("(");
                let mut ty = &source.ty;
                while let TypeId::Ref(inner) | TypeId::RefMut(inner) = ty {
                    self.emit("*");
                    ty = inner;
                }
                self.compile_expr(source);
                self.emit(format!(").{member}"));
            }
            ExprData::Subscript { .. } => todo!(),
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
            ExprData::Range { .. } => todo!(),
            ExprData::Continue => todo!(),
            ExprData::Error => {
                panic!("ICE: ExprData::Error in compile_expr");
            }
        }
    }

    fn hoist_blocks(&mut self, expr: &mut CheckedExpr) {
        match &mut expr.data {
            ExprData::Binary { op: _, left, right } => {
                self.hoist_blocks(left);
                self.hoist_blocks(right);
            }
            ExprData::Unary { op: _, expr } => {
                self.hoist_blocks(expr);
            }
            ExprData::Call { callee, args } | 
            ExprData::Subscript { callee, args } | 
            ExprData::MemberCall { source: callee, args, .. } => {
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
            ExprData::Instance { members } => {
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

                expr.data = ExprData::Symbol { scope: None, symbol: variable };
            }
            _ => {}
        }
    }

    fn emit(&mut self, source: impl AsRef<str>) {
        self.buffer.push_str(source.as_ref());
    }

    fn emit_type(&mut self, id: &TypeId) {
        match id {
            TypeId::Void => self.emit(format!("{RT_PREFIX}void")),
            TypeId::Never => todo!(),
            TypeId::Int(bits) => self.emit(format!("{RT_PREFIX}i{bits}")),
            TypeId::Uint(bits) => self.emit(format!("{RT_PREFIX}u{bits}")),
            TypeId::F32 => self.emit(format!("{RT_PREFIX}f32")),
            TypeId::F64 => self.emit(format!("{RT_PREFIX}f64")),
            TypeId::Bool => self.emit(format!("{RT_PREFIX}bool")),
            TypeId::IntGeneric | TypeId::FloatGeneric => {
                panic!("ICE: Int/FloatGeneric in emit_type");
            }
            TypeId::Ref(inner) => {
                self.emit_type(inner);
                self.emit(" const*");
            }
            TypeId::RefMut(inner) => {
                self.emit_type(inner);
                self.emit(" *");
            }
            TypeId::Type(id) => {
                match &self.types[*id] {
                    Type::Function { .. } => todo!(),
                    Type::Struct(base) => {
                        let name = Self::scope_name(base.scope, &self.scopes);
                        self.emit(format!("struct {name}"));
                    }
                    Type::Temporary => panic!("ICE: Type::Temporary in emit_type"),
                }
            }
        }
    }

    fn emit_fn_decl(
        &mut self,
        CheckedFnDecl {
            public: _,
            name,
            is_async: _,
            is_extern: _,
            type_params: _,
            params,
            ret,
        }: &CheckedFnDecl,
    ) {
        self.emit_type(ret);
        if name == "main" {
            self.emit(format!(" {MAIN_NAME}("));
        } else {
            self.emit(format!(" {}{name}(", self.current_path));
        }

        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }

            self.emit_type(&param.ty);
            if !param.mutable {
                self.emit("const ");
            }

            self.emit(format!(" {}", param.name));
        }
        self.emit(")");
    }

    fn emit_block(&mut self, block: &mut Block) {
        self.emit("{");
        for stmt in block.body.iter_mut() {
            self.compile_stmt(stmt);
        }
        self.emit("}");
    }

    fn add_to_path(&mut self, name: &str) {
        self.current_path.push_str(&format!("{name}_"));
    }

    fn remove_from_path(&mut self, name: &str) {
        self.current_path.truncate(self.current_path.len() - name.len() - 1);
    }

    fn scope_name(scope: ScopeId, scopes: &[Scope]) -> String {
        let mut scope = Some(scope);
        let mut name = String::new();
        while let Some(id) = scope {
            if let Some(scope_name) = &scopes[id].name {
                for c in scope_name.chars().rev() {
                    name.push(c);
                }
                name.push('_');
            }
    
            scope = scopes[id].parent;
        }
    
        name.chars().rev().skip(1).collect::<String>()
    }
}
