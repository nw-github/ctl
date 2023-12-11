use crate::ast::{Expr, ExprData, Fn, ImplBlock, ParsedUserType, Stmt, StmtData, Struct};

const INDENT: &str = "  ";

macro_rules! print_bool {
    ($id: ident) => {
        if *$id {
            print!("({})", stringify!($id));
        }
    };
}

pub fn print_stmt(stmt: &Stmt, src: &str, indent: usize) {
    let tabs = INDENT.repeat(indent);
    match &stmt.data {
        StmtData::Expr(expr) => {
            println!("{tabs}StmtExpr");
            print_expr(expr, src, indent + 1);
        }
        StmtData::Let {
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
                print_expr(value, src, indent + 1);
            }
        }
        StmtData::Fn(f) => print_fn(f, src, indent),
        StmtData::UserType(ty) => match ty {
            ParsedUserType::Struct(base) => print_struct("Struct", base, src, indent),
            ParsedUserType::Union {
                tag,
                base,
                is_unsafe,
            } => {
                if let Some(tag) = tag {
                    print_struct(&format!("Union({})", tag.span.text(src)), base, src, indent);
                    print_bool!(is_unsafe);
                } else {
                    print_struct("Union", base, src, indent);
                    print_bool!(is_unsafe);
                }
            }
            ParsedUserType::Trait {
                public,
                name,
                type_params,
                impls,
                functions,
                is_unsafe,
            } => {
                print!("{tabs}Trait[{name}]");
                print_bool!(public);
                print_bool!(is_unsafe);
                println!();

                let plus_1 = INDENT.repeat(indent + 1);
                if !type_params.is_empty() {
                    println!("{tabs}Type Params:");
                    for (name, path) in type_params {
                        println!("{plus_1}{name}: {path:?}");
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
                    print_fn(f, src, indent + 1);
                }
            }
            ParsedUserType::Enum {
                name,
                impls,
                variants,
                functions,
                public,
            } => {
                print!("{tabs}Enum[{}]", name.data);
                print_bool!(public);
                println!();

                print_impls(indent, src, impls);

                let plus_1 = INDENT.repeat(indent + 1);
                println!("{tabs}Variants:");
                for (name, expr) in variants {
                    println!("{plus_1}{name}");
                    if let Some(expr) = expr {
                        print_expr(expr, src, indent + 2);
                    }
                }

                for f in functions {
                    print_fn(f, src, indent + 1);
                }
            }
        },

        StmtData::Static {
            name,
            ty,
            value,
            public,
        } => {
            print!("{tabs}Static[{name}]");
            print_bool!(public);
            println!();

            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Type: {ty:?}");
            print_expr(value, src, indent + 1);
        }
        StmtData::Module { name, body, public } => {
            print!("{tabs}Module[{name}]");
            print_bool!(public);
            println!();

            print_stmts(body, src, indent + 1);
        }
        StmtData::Use { .. } => {
            println!("{tabs}Use[{}]", stmt.span.text(src));
        }
        StmtData::Error => {}
    }
}

pub fn print_expr(expr: &Expr, src: &str, indent: usize) {
    let tabs = INDENT.repeat(indent);
    match &expr.data {
        ExprData::Binary { op, left, right } => {
            println!("{tabs}Binary({op:?})");
            print_expr(left, src, indent + 1);
            print_expr(right, src, indent + 1);
        }
        ExprData::Range {
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
                print_expr(start, src, indent + 2);
            }
            if let Some(end) = end {
                println!("{tabs}To:");
                print_expr(end, src, indent + 2);
            }
        }
        ExprData::Unary { op, expr } => {
            println!("{tabs}Unary({op:?})");
            print_expr(expr, src, indent + 1);
        }
        ExprData::Call { callee, args } => {
            println!("{tabs}Call");
            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Callee: ");
            print_expr(callee, src, indent + 2);

            if !args.is_empty() {
                println!("{tabs}Args: ");
                for (name, expr) in args {
                    if let Some(name) = name {
                        println!("{tabs}{INDENT}{name}:");
                        print_expr(expr, src, indent + 3);
                    } else {
                        print_expr(expr, src, indent + 2);
                    }
                }
            }
        }
        ExprData::Array(elements) | ExprData::Vec(elements) | ExprData::Set(elements) => {
            println!("{tabs}Array|Vector|Set"); // FIXME: correct this name
            for el in elements {
                print_expr(el, src, indent + 1);
            }
        }
        ExprData::ArrayWithInit { init, count } | ExprData::VecWithInit { init, count } => {
            println!("{tabs}ArrayWithInit");

            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Init: ");
            print_expr(init, src, indent + 2);
            println!("{tabs}Count: ");
            print_expr(count, src, indent + 2);
        }
        ExprData::Tuple(elements) => {
            println!("{tabs}Tuple");
            for el in elements {
                print_expr(el, src, indent + 1);
            }
        }
        ExprData::Map(expr) => {
            println!("{tabs}Map");
            let tabs = INDENT.repeat(indent + 1);
            for (key, value) in expr {
                println!("{tabs}Key: ");
                print_expr(key, src, indent + 2);
                println!("{tabs}Value: ");
                print_expr(value, src, indent + 2);
            }
        }
        ExprData::Integer { base, value, width } => {
            println!("{tabs}Integer(base {base}, width {width:?}) = {value}");
        }
        ExprData::Float(value) => {
            println!("{tabs}Float = {value}");
        }
        ExprData::String(value) => {
            println!("{tabs}String = \'{value}\'");
        }
        ExprData::Char(value) => {
            println!("{tabs}Char = \'{value}\'");
        }
        ExprData::ByteString(value) => {
            println!("{tabs}ByteString = \'{value}\'");
        }
        ExprData::ByteChar(value) => {
            println!("{tabs}ByteChar = \'{value}\'");
        }
        ExprData::Path(_) => {
            println!("{tabs}Path[{}]", expr.span.text(src));
        }
        ExprData::Assign {
            target,
            binary,
            value,
        } => {
            println!("{tabs}Assign({binary:?})");
            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Target: ");
            print_expr(target, src, indent + 2);
            println!("{tabs}Value: ");
            print_expr(value, src, indent + 2);
        }
        ExprData::Block(expr) => {
            println!("{tabs}Block");
            print_stmts(expr, src, indent + 1);
        }
        ExprData::If {
            cond,
            if_branch,
            else_branch,
        } => {
            println!("{tabs}If");

            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Condition: ");
            print_expr(cond, src, indent + 2);

            println!("{tabs}Body: ");
            print_expr(if_branch, src, indent + 2);

            if let Some(else_branch) = else_branch {
                println!("{tabs}Else: ");
                print_expr(else_branch, src, indent + 2);
            }
        }
        ExprData::Loop {
            cond,
            body,
            do_while,
        } => {
            print!("{tabs}Loop");
            print_bool!(do_while);
            println!();

            let tabs = INDENT.repeat(indent + 1);
            if let Some(cond) = cond {
                println!("{tabs}Condition: ");
                print_expr(cond, src, indent + 2);
            }

            println!("{tabs}Body: ");
            print_stmts(body, src, indent + 2);
        }
        ExprData::Member {
            source,
            member,
            generics,
        } => {
            println!("{tabs}Member[{member}]");
            if !generics.is_empty() {
                println!("{tabs}Generics:");
                let tabs = INDENT.repeat(indent + 1);
                for ty in generics.iter() {
                    println!("{tabs}{ty:?}")
                }
            }
            print_expr(source, src, indent + 1);
        }
        ExprData::Subscript { callee, args } => {
            println!("{tabs}Subscript");
            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}Callee: ");
            print_expr(callee, src, indent + 2);

            if !args.is_empty() {
                println!("{tabs}Args: ");
                for expr in args {
                    print_expr(expr, src, indent + 2);
                }
            }
        }
        ExprData::Return(expr) => {
            println!("{tabs}Return");
            print_expr(expr, src, indent + 1);
        }
        ExprData::Yield(expr) => {
            println!("{tabs}Yield");
            print_expr(expr, src, indent + 1);
        }
        ExprData::YieldOrReturn(expr) => {
            println!("{tabs}YieldOrReturn");
            print_expr(expr, src, indent + 1);
        }
        ExprData::Break(expr) => {
            println!("{tabs}Break");
            print_expr(expr, src, indent + 1);
        }
        ExprData::Bool(value) => {
            println!("{tabs}Bool = {value}");
        }
        ExprData::Continue => {
            println!("{tabs}Continue");
        }
        ExprData::None => {
            println!("{tabs}None");
        }
        ExprData::For {
            var,
            mutable,
            iter,
            body,
        } => {
            println!("{tabs}For[{var}]");
            print_bool!(mutable);
            let tabs = INDENT.repeat(indent + 1);
            println!("{tabs}In: ");
            print_expr(iter, src, indent + 2);
            println!("{tabs}Body: ");
            print_stmts(body, src, indent + 2);
        }
        ExprData::Void => println!("{tabs}Void"),
        ExprData::Is { expr, pattern } => {
            println!("{tabs}Is ({pattern:?})");
            print_expr(expr, src, indent + 1);
        }
        ExprData::As { expr, ty, throwing } => {
            println!("{tabs}As ({ty:?})");
            print_bool!(throwing);
            print_expr(expr, src, indent + 1);
        }
        ExprData::Match { expr, body } => {
            println!("{tabs}Match");
            print_expr(expr, src, indent + 1);

            let tabs = INDENT.repeat(indent + 1);
            for (i, (patt, expr)) in body.iter().enumerate() {
                println!("{tabs}Case {i} ({patt:?}):");
                print_expr(expr, src, indent + 2);
            }
        }
        ExprData::Error => {}
        ExprData::Lambda { params, ret, body } => {
            println!("{tabs}Lambda");
            println!("{tabs}Return: {ret:?}");
            if !params.is_empty() {
                println!("{tabs}Params:");
                let plus_1 = INDENT.repeat(indent + 1);
                for (name, ty) in params {
                    println!("{plus_1}{}: {ty:?}", name.data);
                }
            }

            println!("{tabs}Body: ");
            print_expr(body, src, indent + 1);
        }
        ExprData::Unsafe(expr) => {
            println!("{tabs}Unsafe");
            print_expr(expr, src, indent + 1);
        }
    }
}

fn print_stmts(stmts: &[Stmt], src: &str, indent: usize) {
    for stmt in stmts {
        print_stmt(stmt, src, indent);
    }
}

fn print_fn(
    Fn {
        name,
        is_async,
        is_extern,
        is_unsafe,
        type_params,
        variadic,
        params,
        ret,
        public,
        body,
    }: &Fn,
    src: &str,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    print!("{tabs}Fn[{}]", name.data);
    print_bool!(is_async);
    print_bool!(is_extern);
    print_bool!(is_unsafe);
    print_bool!(variadic);
    print_bool!(public);
    println!();

    let plus_1 = INDENT.repeat(indent + 1);
    let plus_2 = INDENT.repeat(indent + 2);
    if !type_params.is_empty() {
        println!("{plus_1}Type Params:");
        for (name, path) in type_params {
            println!("{plus_2}{name}: {path:?}");
        }
    }
    if !params.is_empty() {
        println!("{plus_1}Params:");
        for param in params {
            println!("{plus_2}{param:?}");
        }
    }

    println!("{plus_1}Return Type: {ret:?}");
    if let Some(body) = body {
        println!("{}Body: ", INDENT.repeat(indent));
        print_stmts(body, src, indent + 1);
    }
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
    src: &str,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    println!("{tabs}{type_name}[{}]", name.data);
    print_bool!(public);
    println!();

    print_impls(indent, src, impls);

    let plus_1 = INDENT.repeat(indent + 1);
    if !type_params.is_empty() {
        println!("{tabs}Type Params:");
        for (name, impls) in type_params {
            println!("{plus_1}{name}: {impls:?}");
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
        print_fn(f, src, indent + 1);
    }
}

fn print_impls(indent: usize, src: &str, impls: &[ImplBlock]) {
    let tabs = INDENT.repeat(indent);
    let plus_1 = INDENT.repeat(indent + 1);
    if !impls.is_empty() {
        println!("{tabs}New Impls:");
        for imp in impls {
            if !imp.type_params.is_empty() {
                println!("{tabs}Type Params:");
                for (name, impls) in imp.type_params.iter() {
                    println!("{plus_1}{name}: {impls:?}");
                }
            }

            println!("{plus_1}{:?}", imp.path.data);
            for f in imp.functions.iter() {
                print_fn(f, src, indent + 2)
            }
        }
    }
}
