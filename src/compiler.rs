use crate::{
    ast::expr::UnaryOp,
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::{CheckedStmt, CheckedUserType},
        Block,
    },
    scope::{Scopes, Struct, StructDef, CheckedPrototype, FunctionId, Function},
    typecheck::{CheckedAst, TypeId},
};

const RT_PREFIX: &str = "CTL_RUNTIME_";
const MAIN_NAME: &str = "CTL_PROGRAM_MAIN";

pub struct BlockInfo {
    variable: String,
    label: String,
}

pub struct Compiler {
    buffer: String,
    current_block: Option<BlockInfo>,
    block_number: usize,
    current_path: String,
}

impl Compiler {
    pub fn compile(mut ast: CheckedAst) -> String {
        let mut this = Self {
            buffer: String::new(),
            current_block: None,
            block_number: 0,
            current_path: String::new(),
        };
        this.emit(include_str!("../runtime/ctl.h"));
        this.emit("\n\n");
        this.compile_stmt(&mut ast.scopes, &mut ast.stmt);
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
                            self.emit("const ");
                        }

                        self.emit(format!(" {name}"));
                    }
                }
                self.emit(";");
            }
            CheckedStmt::Fn(id) => {
                self.emit_prototype(scopes, id);
                self.add_to_path(&scopes[&*id].proto.name);
                // FIXME: this clone hack
                self.emit_block(scopes, &mut scopes[&*id].body.clone().unwrap());
                self.remove_from_path(&scopes[&*id].proto.name);
            }
            CheckedStmt::UserType(data) => match data {
                CheckedUserType::Struct(id) => {
                    let Struct { 
                        name, 
                        def: Some(StructDef {
                            members,
                            scope
                        }) 
                    } = &scopes[&*id] else {
                        unreachable!()
                    };

                    self.emit(format!("struct {}{name} {{", self.current_path));
                    self.add_to_path(name);
                    for (name, member) in members.iter() {
                        self.emit_type(scopes, &member.ty);
                        self.emit(format!(" {}", name));
                        self.emit(";");
                        if !member.public {
                            self.emit("/* private */ \n")
                        }
                    }

                    self.emit("};");

                    let name = name.clone();
                    for i in 0..scopes[*scope].fns.len() {
                        let id = &FunctionId(id.scope(), i);
                        if scopes[id].inst {
                            continue;
                        }

                        self.emit_prototype(scopes, id);
                        self.add_to_path(&scopes[id].proto.name);
                        self.emit_block(scopes, &mut scopes[id].body.clone().unwrap());
                        self.remove_from_path(&scopes[id].proto.name);
                    }
                    self.remove_from_path(&name);
                }
            },
            CheckedStmt::Static {
                public: _,
                name,
                value,
            } => {
                self.emit("static ");
                self.emit_type(scopes, &value.ty);
                self.emit(format!(" const {}{name} = ", self.current_path));
                // FIXME: blocks in statics...
                self.hoist_blocks(scopes, value);
                self.compile_expr(scopes, value);
                self.emit(";");
            }
            CheckedStmt::Module { name, body } => {
                self.add_to_path(name);
                for stmt in body.body.iter_mut() {
                    self.compile_stmt(scopes, stmt);
                }
                self.remove_from_path(name);
            }
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
            ExprData::Call { callee, args } => {
                self.compile_expr(scopes, callee);
                self.emit("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }

                    self.compile_expr(scopes, arg);
                }
                self.emit(")");
            }
            ExprData::MemberCall {
                source,
                member,
                ty,
                args,
            } => {
                let TypeId::Struct(id) = ty else { unreachable!() };
                let Struct { 
                    name, def: Some(StructDef { 
                        members, 
                        scope 
                    }) 
                } = &scopes[id.as_ref()] else { unreachable!() };

                self.emit(scopes.scope_full_name(*scope));
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

                self.compile_expr(scopes, source);

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
            ExprData::Symbol { scope, symbol } => {
                if let Some(scope) = scope {
                    self.emit(scopes.scope_full_name(*scope));
                    self.emit("_");
                }
                self.emit(symbol);
            }
            ExprData::Instance { members } => {
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
                while let TypeId::Ref(inner) | TypeId::RefMut(inner) = ty {
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
                let Some(block) = &self.current_block else {
                    panic!("ICE: compiling yield, but there is no hoisted block");
                };

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
            ExprData::Call { callee, args }
            | ExprData::Subscript { callee, args }
            | ExprData::MemberCall {
                source: callee,
                args,
                ..
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
            ExprData::Instance { members, .. } => {
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

                expr.data = ExprData::Symbol {
                    scope: None,
                    symbol: variable,
                };
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
            TypeId::Ref(inner) => {
                self.emit_type(scopes, inner);
                self.emit(" const*");
            }
            TypeId::RefMut(inner) => {
                self.emit_type(scopes, inner);
                self.emit(" *");
            }
            TypeId::Option(inner) => {
                self.emit(format!("{RT_PREFIX}option_"));
                self.emit_type(scopes, inner);
            }
            TypeId::Function(_) => todo!(),
            TypeId::Struct(id) => {
                self.emit(format!(
                    "struct {}", 
                    scopes.scope_full_name(scopes[id.as_ref()].def.as_ref().unwrap().scope)
                ));
            }
            TypeId::Unknown => panic!("ICE: TypeId::Unknown in emit_type"),
            TypeId::Array(_) => todo!(),
        }
    }

    fn emit_prototype(
        &mut self,
        scopes: &Scopes,
        id: &FunctionId,
    ) {
        let Function { proto: CheckedPrototype {
            public: _,
            name,
            is_async: _,
            is_extern: _,
            type_params: _,
            params,
            ret,
        }, .. } = &scopes[id];

        self.emit_type(scopes, ret);
        if name == "main" {
            self.emit(format!(" {MAIN_NAME}("));
        } else {
            self.emit(format!(" {}{name}(", self.current_path));
        }

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

    fn add_to_path(&mut self, name: &str) {
        self.current_path.push_str(&format!("{name}_"));
    }

    fn remove_from_path(&mut self, name: &str) {
        self.current_path
            .truncate(self.current_path.len() - name.len() - 1);
    }
}
