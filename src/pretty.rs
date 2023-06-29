use crate::{
    ast::{
        expr::Expr,
        stmt::{Fn, FnDecl, Stmt, Struct, UserType},
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

pub fn print_stmt(stmt: &Located<Stmt>, indent: usize) {
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
        Stmt::UserType(ty) => match ty {
            UserType::Struct(base) => print_struct("Struct", base, indent),
            UserType::Union { tag, base } => {
                if let Some(tag) = tag {
                    print_struct(&format!("Union({tag})"), base, indent);
                } else {
                    print_struct("Union", base, indent)
                }
            }
            UserType::Interface {
                public,
                name,
                type_params,
                impls,
                functions,
            } => {
                print!("{tabs}Interface[{name}]");
                print_bool!(public);
                println!();

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
            UserType::Enum {
                name,
                impls,
                variants,
                functions,
                public,
            } => {
                print!("{tabs}Enum[{name}]");
                print_bool!(public);
                println!();

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
        },

        Stmt::Static {
            name,
            ty,
            value,
            public,
        } => {
            print!("{tabs}Static[{name}]");
            print_bool!(public);
            println!();

            let tabs = INDENT.repeat(indent + 1);
            if let Some(ty) = ty {
                println!("{tabs}Type: {ty:?}");
            }

            print_expr(value, indent + 1);
        }
        Stmt::Module { name, body, public } => {
            print!("{tabs}Module[{name}]");
            print_bool!(public);
            println!();

            print_stmts(body, indent + 1);
        }
    }
}

pub fn print_expr(expr: &Located<Expr>, indent: usize) {
    let tabs = INDENT.repeat(indent);
    match &expr.data {
        Expr::Binary { op, left, right } => {
            println!("{tabs}Binary({op:?})");
            print_expr(left, indent + 1);
            print_expr(right, indent + 1);
        }
        Expr::Range {
            start,
            end,
            inclusive,
        } => {
            print!("{tabs}Range");
            print_bool!(inclusive);
            println!();

            let tabs = INDENT.repeat(indent + 1);
            if let Some(start) = start {
                println!("{tabs}From:");
                print_expr(start, indent + 2);
            }
            if let Some(end) = end {
                println!("{tabs}To:");
                print_expr(end, indent + 2);
            }
        }
        Expr::Unary { op, expr } => {
            println!("{tabs}Unary({op:?})");
            print_expr(expr, indent + 1);
        }
        Expr::Call { callee, args } => {
            println!("{tabs}Call");
            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Callee: ");
            print_expr(callee, indent + 2);

            if !args.is_empty() {
                println!("{tabs}Args: ");
                for (name, expr) in args {
                    if let Some(name) = name {
                        println!("{tabs}{INDENT}{name}:");
                        print_expr(expr, indent + 3);
                    } else {
                        print_expr(expr, indent + 2);
                    }
                }
            }
        }
        Expr::Array(elements) => {
            println!("{tabs}Array");
            for el in elements {
                print_expr(el, indent + 1);
            }
        }
        Expr::ArrayWithInit { init, count } => {
            println!("{tabs}ArrayWithInit");

            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Init: ");
            print_expr(init, indent + 2);
            println!("{tabs}Count: {count}");
            //print_expr(count, indent + 2);
        }
        Expr::Tuple(elements) => {
            println!("{tabs}Tuple");
            for el in elements {
                print_expr(el, indent + 1);
            }
        }
        Expr::Map(expr) => {
            println!("{tabs}Map");
            let tabs = INDENT.repeat(indent + 1);
            for (key, value) in expr {
                println!("{tabs}Key: ");
                print_expr(key, indent + 2);
                println!("{tabs}Value: ");
                print_expr(value, indent + 2);
            }
        }
        Expr::Integer(base, value) => {
            println!("{tabs}Integer(base {base}) = {value}");
        }
        Expr::Float(value) => {
            println!("{tabs}Float = {value}");
        }
        Expr::String(value) => {
            println!("{tabs}String = \'{value}\'");
        }
        Expr::Symbol(value) => {
            println!("{tabs}Symbol[{value}]");
        }
        Expr::Assign {
            target,
            binary,
            value,
        } => {
            println!("{tabs}Assign({binary:?})");
            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Target: ");
            print_expr(target, indent + 2);
            println!("{tabs}Value: ");
            print_expr(value, indent + 2);
        }
        Expr::Block(expr) => {
            println!("{tabs}Block");
            print_stmts(expr, indent + 1);
        }
        Expr::If {
            cond,
            if_branch,
            else_branch,
        } => {
            println!("{tabs}If");

            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Condition: ");
            print_expr(cond, indent + 2);

            println!("{tabs}Body: ");
            print_expr(if_branch, indent + 2);

            if let Some(else_branch) = else_branch {
                println!("{tabs}Else: ");
                print_expr(else_branch, indent + 2);
            }
        }
        Expr::Loop {
            cond,
            body,
            do_while,
        } => {
            print!("{tabs}Loop");
            print_bool!(do_while);
            println!();

            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Condition: ");
            print_expr(cond, indent + 2);

            println!("{tabs}Body: ");
            print_stmts(body, indent + 2);
        }
        Expr::Member { source, member } => {
            println!("{tabs}Member[{member}]");
            print_expr(source, indent + 1);
        }
        Expr::Subscript { callee, args } => {
            println!("{tabs}Subscript");
            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Callee: ");
            print_expr(callee, indent + 2);

            if !args.is_empty() {
                println!("{tabs}Args: ");
                for expr in args {
                    print_expr(expr, indent + 2);
                }
            }
        }
        Expr::Return(expr) => {
            println!("{tabs}Return");
            print_expr(expr, indent + 1);
        }
        Expr::Yield(expr) => {
            println!("{tabs}Yield");
            print_expr(expr, indent + 1);
        }
        Expr::Break(expr) => {
            println!("{tabs}Break");
            print_expr(expr, indent + 1);
        }
        Expr::Bool(value) => {
            println!("{tabs}Bool = {value}");
        }
        Expr::Continue => {
            println!("{tabs}Continue");
        }
        Expr::None => {
            println!("{tabs}None");
        }
        Expr::For { var, iter, body } => {
            println!("{tabs}For[{var}]");
            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}In: ");
            print_expr(iter, indent + 2);
            println!("{tabs}Body: ");
            print_stmts(body, indent + 2);
        }
        Expr::Instance { name, members } => {
            println!("{tabs}Instance[{name}]");

            let tabs = INDENT.repeat(indent + 1);
            if !members.is_empty() {
                for (name, expr) in members {
                    println!("{tabs}{name}:");
                    print_expr(expr, indent + 2);
                }
            }
        }
        Expr::Void => println!("{tabs}Void")
    }
}

fn print_stmts(stmts: &[Located<Stmt>], indent: usize) {
    for stmt in stmts {
        print_stmt(stmt, indent);
    }
}

fn print_fn_decl(
    FnDecl {
        name,
        is_async,
        is_extern,
        type_params,
        params,
        ret,
        public,
    }: &FnDecl,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    print!("{tabs}Fn[{name}]");
    print_bool!(is_async);
    print_bool!(is_extern);
    print_bool!(public);
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
    print_stmts(body, indent + 1);
}

fn print_struct(
    type_name: &str,
    Struct {
        name,
        type_params,
        members,
        impls,
        functions,
        public,
    }: &Struct,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    println!("{tabs}{type_name}[{name}]");
    print_bool!(public);
    println!();

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
