use crate::{
    ast::{
        expr::Expr,
        stmt::{Fn, FnDecl, Stmt, Struct},
    },
    lexer::Located,
};

const INDENT: &str = "  ";

macro_rules! print_bool {
    ($id: ident) => {
        if *$id {
            print!("({})", stringify!($id));
        }
    };
}

pub fn dump_ast(stmts: &[Located<Stmt>], indent: usize) {
    for stmt in stmts {
        print_stmt(stmt, indent);
    }
}

fn print_stmt(stmt: &Located<Stmt>, indent: usize) {
    let tabs = INDENT.repeat(indent);
    match &stmt.data {
        Stmt::Expr(expr) => {
            println!("{tabs}StmtExpr");
            print_expr(expr, indent + 1);
        }
        Stmt::Let {
            name,
            ty,
            mutable,
            value,
        } => {
            print!("{tabs}Let[{name}]");
            print_bool!(mutable);
            println!();

            println!("{tabs}Type: {ty:?}");
            if let Some(value) = value {
                print_expr(value, indent + 1);
            }
        }
        Stmt::Fn(f) => print_fn(f, indent),
        Stmt::Struct(base) => print_struct("Struct", base, indent),
        Stmt::Union { tag, base } => {
            if let Some(tag) = tag {
                print_struct(&format!("Union({tag})"), base, indent);
            } else {
                print_struct("Union", base, indent)
            }
        }
        Stmt::Interface {
            name,
            type_params,
            impls,
            functions,
        } => {
            println!("{tabs}Interface[{name}]");

            let plus_1 = INDENT.repeat(indent + 1);
            if !type_params.is_empty() {
                println!("{tabs}Type Params:");
                for param in type_params {
                    println!("{plus_1}{param}");
                }
            }

            if !impls.is_empty() {
                println!("{tabs}Impls: ");
                for i in impls {
                    println!("{plus_1}{i:?}");
                }
            }

            println!("{tabs}Functions:");
            for f in functions {
                print_fn_decl(f, indent + 1);
            }
        }
        Stmt::Enum {
            name,
            impls,
            variants,
            functions,
        } => {
            println!("{tabs}Enum[{name}]");

            let plus_1 = INDENT.repeat(indent + 1);
            if !impls.is_empty() {
                println!("{tabs}Impls: ");
                for i in impls {
                    println!("{plus_1}{i:?}");
                }
            }

            println!("{tabs}Variants:");
            for (name, expr) in variants {
                println!("{plus_1}{name}");
                if let Some(expr) = expr {
                    print_expr(expr, indent + 2);
                }
            }

            for f in functions {
                print_fn(f, indent + 1);
            }
        }
        Stmt::Static { name, ty, value } => {
            println!("{tabs}Static[{name}]");
            if let Some(ty) = ty {
                println!("Type: {ty:?}");
            }

            print_expr(value, indent + 1);
        }
    }
}

fn print_expr(expr: &Located<Expr>, indent: usize) {
    todo!()
}

fn print_fn_decl(
    FnDecl {
        name,
        is_async,
        is_extern,
        type_params,
        params,
        ret,
    }: &FnDecl,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    print!("{tabs}Fn[{name}]");
    print_bool!(is_async);
    print_bool!(is_extern);
    println!();

    let plus_1 = INDENT.repeat(indent + 1);
    let plus_2 = INDENT.repeat(indent + 2);
    if !type_params.is_empty() {
        println!("{plus_1}Type Params:");
        for param in type_params {
            println!("{plus_2}{param}");
        }
    }
    if !params.is_empty() {
        println!("{plus_1}Params:");
        for param in params {
            println!("{plus_2}{param:?}");
        }
    }

    println!("{plus_1}Return Type: {ret:?}");
}

fn print_fn(Fn { header, body }: &Fn, indent: usize) {
    print_fn_decl(header, indent);
    println!("{}Body: ", INDENT.repeat(indent));
    dump_ast(body, indent + 1);
}

fn print_struct(
    type_name: &str,
    Struct {
        name,
        type_params,
        members,
        impls,
        functions,
    }: &Struct,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    println!("{tabs}{type_name}[{name}]");

    let plus_1 = INDENT.repeat(indent + 1);
    if !type_params.is_empty() {
        println!("{tabs}Type Params:");
        for param in type_params {
            println!("{plus_1}{param}");
        }
    }

    if !impls.is_empty() {
        println!("{tabs}Impls: ");
        for i in impls {
            println!("{plus_1}{i:?}");
        }
    }

    if !members.is_empty() {
        println!("{tabs}Members: ");
        for member in members {
            println!("{plus_1}{member:?}");
        }
    }

    println!("{tabs}Functions:");
    for f in functions {
        print_fn(f, indent + 1);
    }
}
