use crate::{ast::parsed::{
    Expr, ExprData, Fn, ImplBlock, IntPattern, OperatorFn, Stmt, StmtData, Struct, UsePath,
    UsePathTail,
}, Located};

const INDENT: &str = "  ";

macro_rules! print_bool {
    ($id: ident) => {
        if *$id {
            eprint!("({})", stringify!($id));
        }
    };
}

pub fn print_stmt(stmt: &Stmt, indent: usize) {
    let tabs = INDENT.repeat(indent);
    match &stmt.data.data {
        StmtData::Expr(expr) => {
            eprintln!("{tabs}ExprStmt");
            print_expr(expr, indent + 1);
        }
        StmtData::Defer(expr) => {
            eprintln!("{tabs}Defer");
            print_expr(expr, indent + 1);
        }
        StmtData::Let { ty, value, patt } => {
            eprint!("{tabs}Let[{patt:?}]");
            eprintln!();

            eprintln!("{tabs}Type: {ty:?}");
            if let Some(value) = value {
                print_expr(value, indent + 1);
            }
        }
        StmtData::Guard { cond, body } => {
            eprintln!("{tabs}Guard");

            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}Condition: ");
            print_expr(cond, indent + 2);

            eprintln!("{tabs}Body: ");
            print_expr(body, indent + 2);
        }
        StmtData::Fn(f) => print_fn(f, indent),
        StmtData::Struct { base, packed } => {
            print_struct(&format!("Struct({})", packed), base, indent)
        }
        StmtData::Union {
            tag,
            base,
            variants,
        } => {
            if let Some(tag) = tag {
                print_struct(&format!("Union({tag:?})"), base, indent);
            } else {
                print_struct("Union", base, indent);
            }
            let plus_1 = INDENT.repeat(indent + 1);
            if !variants.is_empty() {
                eprintln!("{tabs}Members: ");
                for member in variants {
                    eprintln!("{plus_1}{member:?}");
                }
            }
        }
        StmtData::UnsafeUnion(base) => print_struct("Union", base, indent),
        StmtData::Trait {
            public,
            name,
            type_params,
            impls,
            functions,
            is_unsafe,
            sealed,
        } => {
            eprint!("{tabs}Trait[{name}]");
            print_bool!(public);
            print_bool!(sealed);
            print_bool!(is_unsafe);
            eprintln!();

            let plus_1 = INDENT.repeat(indent + 1);
            if !type_params.is_empty() {
                eprintln!("{tabs}Type Params:");
                for (name, path) in type_params {
                    eprintln!("{plus_1}{name}: {path:?}");
                }
            }

            if !impls.is_empty() {
                eprintln!("{tabs}Impls: ");
                for i in impls {
                    eprintln!("{plus_1}{i:?}");
                }
            }

            eprintln!("{tabs}Functions:");
            for f in functions {
                print_fn(&f.data, indent + 1);
            }
        }
        StmtData::Extension {
            public,
            name,
            ty,
            type_params,
            impls,
            functions,
            operators,
        } => {
            eprint!("{tabs}Extension[{name}, for={ty:?}]");
            print_bool!(public);
            eprintln!();

            let plus_1 = INDENT.repeat(indent + 1);
            if !type_params.is_empty() {
                eprintln!("{tabs}Type Params:");
                for (name, path) in type_params {
                    eprintln!("{plus_1}{name}: {path:?}");
                }
            }

            print_impls(indent, impls);

            eprintln!("{tabs}Functions:");
            for f in functions {
                print_fn(&f.data, indent + 1);
            }
            for f in operators {
                print_op_fn(&f.data, indent + 1);
            }
        }
        StmtData::Binding {
            name,
            ty,
            value,
            public,
            constant,
        } => {
            eprint!("{tabs}Static[{name}]");
            print_bool!(public);
            print_bool!(constant);
            eprintln!();

            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}Type: {ty:?}");
            print_expr(value, indent + 1);
        }
        StmtData::Module {
            name,
            body,
            public,
            file: _,
        } => {
            eprint!("{tabs}Module[{name}]");
            print_bool!(public);
            eprintln!();

            print_stmts(body, indent + 1);
        }
        StmtData::Use(UsePath {
            public,
            origin,
            components,
            tail,
        }) => {
            eprintln!("{tabs}Use[From = {origin:?}]");
            print_bool!(public);

            let plus_1 = INDENT.repeat(indent + 1);
            eprintln!(
                "{plus_1}Path = {}::{}",
                components
                    .iter()
                    .map(|x| &x.data[..])
                    .collect::<Vec<_>>()
                    .join("::"),
                if let UsePathTail::Ident(ident) = tail {
                    &ident.data
                } else {
                    "*"
                }
            );
        }
        StmtData::Error => {}
    }
}

pub fn print_expr(expr: &Expr, indent: usize) {
    let tabs = INDENT.repeat(indent);
    match &expr.data {
        ExprData::Binary { op, left, right } => {
            eprintln!("{tabs}Binary({op:?})");
            print_expr(left, indent + 1);
            print_expr(right, indent + 1);
        }
        ExprData::Range {
            start,
            end,
            inclusive,
        } => {
            eprint!("{tabs}Range");
            print_bool!(inclusive);
            eprintln!();

            let tabs = INDENT.repeat(indent + 1);
            if let Some(start) = start {
                eprintln!("{tabs}From:");
                print_expr(start, indent + 2);
            }
            if let Some(end) = end {
                eprintln!("{tabs}To:");
                print_expr(end, indent + 2);
            }
        }
        ExprData::Unary { op, expr } => {
            eprintln!("{tabs}Unary({op:?})");
            print_expr(expr, indent + 1);
        }
        ExprData::Call { callee, args } | ExprData::Subscript { callee, args } => {
            if matches!(expr.data, ExprData::Call { .. }) {
                eprintln!("{tabs}Call");
            } else {
                eprintln!("{tabs}Subscript");
            }
            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}Callee: ");
            print_expr(callee, indent + 2);

            if !args.is_empty() {
                eprintln!("{tabs}Args: ");
                for (name, expr) in args {
                    if let Some(name) = name {
                        eprintln!("{tabs}{INDENT}{name}:");
                        print_expr(expr, indent + 3);
                    } else {
                        print_expr(expr, indent + 2);
                    }
                }
            }
        }
        ExprData::Array(elements) | ExprData::Vec(elements) | ExprData::Set(elements) => {
            eprintln!("{tabs}Array|Vector|Set"); // FIXME: correct this name
            for el in elements {
                print_expr(el, indent + 1);
            }
        }
        ExprData::ArrayWithInit { init, count } | ExprData::VecWithInit { init, count } => {
            eprintln!("{tabs}ArrayWithInit");

            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}Init: ");
            print_expr(init, indent + 2);
            eprintln!("{tabs}Count: ");
            print_expr(count, indent + 2);
        }
        ExprData::Tuple(elements) => {
            eprintln!("{tabs}Tuple");
            for el in elements {
                print_expr(el, indent + 1);
            }
        }
        ExprData::Map(expr) => {
            eprintln!("{tabs}Map");
            let tabs = INDENT.repeat(indent + 1);
            for (key, value) in expr {
                eprintln!("{tabs}Key: ");
                print_expr(key, indent + 2);
                eprintln!("{tabs}Value: ");
                print_expr(value, indent + 2);
            }
        }
        ExprData::Integer(IntPattern {
            negative,
            base,
            value,
            width,
        }) => {
            eprintln!("{tabs}Integer(base {base}, width {width:?}, neg {negative}) = {value}");
        }
        ExprData::Float(value) => {
            eprintln!("{tabs}Float = {value}");
        }
        ExprData::String(value) => {
            eprintln!("{tabs}String = \'{value}\'");
        }
        ExprData::Char(value) => {
            eprintln!("{tabs}Char = \'{value}\'");
        }
        ExprData::ByteString(value) => {
            eprintln!("{tabs}ByteString = \'{value:?}\'");
        }
        ExprData::ByteChar(value) => {
            eprintln!("{tabs}ByteChar = \'{value}\'");
        }
        ExprData::Path(path) => {
            eprintln!("{tabs}Path[{path:?}]");
        }
        ExprData::Block(expr, label) => {
            eprintln!("{tabs}Block({label:?})");
            print_stmts(expr, indent + 1);
        }
        ExprData::If {
            cond,
            if_branch,
            else_branch,
        } => {
            eprintln!("{tabs}If");

            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}Condition: ");
            print_expr(cond, indent + 2);

            eprintln!("{tabs}Body: ");
            print_expr(if_branch, indent + 2);

            if let Some(else_branch) = else_branch {
                eprintln!("{tabs}Else: ");
                print_expr(else_branch, indent + 2);
            }
        }
        ExprData::Loop {
            cond,
            body,
            do_while,
            label,
        } => {
            eprint!("{tabs}Loop({label:?}");
            print_bool!(do_while);
            eprintln!();

            let tabs = INDENT.repeat(indent + 1);
            if let Some(cond) = cond {
                eprintln!("{tabs}Condition: ");
                print_expr(cond, indent + 2);
            }

            eprintln!("{tabs}Body: ");
            print_stmts(body, indent + 2);
        }
        ExprData::Member {
            source,
            member,
            generics,
        } => {
            eprintln!("{tabs}Member[{member}]");
            if !generics.is_empty() {
                eprintln!("{tabs}Generics:");
                let tabs = INDENT.repeat(indent + 1);
                for ty in generics.iter() {
                    eprintln!("{tabs}{ty:?}")
                }
            }
            print_expr(source, indent + 1);
        }
        ExprData::Return(expr) => {
            eprintln!("{tabs}Return");
            print_expr(expr, indent + 1);
        }
        ExprData::Tail(expr) => {
            eprintln!("{tabs}Tail");
            print_expr(expr, indent + 1);
        }
        ExprData::Break(expr, label) => {
            eprintln!("{tabs}Break({label:?})");
            if let Some(expr) = expr {
                print_expr(expr, indent + 1);
            }
        }
        ExprData::Bool(value) => {
            eprintln!("{tabs}Bool = {value}");
        }
        ExprData::Continue(label) => {
            eprintln!("{tabs}Continue({label:?})");
        }
        ExprData::For {
            patt,
            iter,
            body,
            label,
        } => {
            eprintln!("{tabs}For[{patt:?}, {label:?}]");
            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}In: ");
            print_expr(iter, indent + 2);
            eprintln!("{tabs}Body: ");
            print_stmts(body, indent + 2);
        }
        ExprData::Void => eprintln!("{tabs}Void"),
        ExprData::Is { expr, pattern } => {
            eprintln!("{tabs}Is ({pattern:?})");
            print_expr(expr, indent + 1);
        }
        ExprData::As { expr, ty, throwing } => {
            eprintln!("{tabs}As ({ty:?})");
            print_bool!(throwing);
            print_expr(expr, indent + 1);
        }
        ExprData::Match { expr, body } => {
            eprintln!("{tabs}Match");
            print_expr(expr, indent + 1);

            let tabs = INDENT.repeat(indent + 1);
            for (i, (patt, expr)) in body.iter().enumerate() {
                eprintln!("{tabs}Case {i} ({patt:?}):");
                print_expr(expr, indent + 2);
            }
        }
        ExprData::Error => {}
        ExprData::Lambda {
            params,
            ret,
            body,
            moves,
        } => {
            eprintln!("{tabs}Lambda");
            print_bool!(moves);
            eprintln!("\n{tabs}Return: {ret:?}");
            if !params.is_empty() {
                eprintln!("{tabs}Params:");
                let plus_1 = INDENT.repeat(indent + 1);
                for (name, ty) in params {
                    eprintln!("{plus_1}{name}: {ty:?}");
                }
            }

            eprintln!("{tabs}Body: ");
            print_expr(body, indent + 1);
        }
        ExprData::Unsafe(expr) => {
            eprintln!("{tabs}Unsafe");
            print_expr(expr, indent + 1);
        }
        ExprData::StringInterpolation(parts) => {
            eprintln!("{tabs}StringInterpolation");
            for part in parts {
                print_expr(part, indent + 1);
            }
        }
    }
}

fn print_stmts(stmts: &[Stmt], indent: usize) {
    for stmt in stmts {
        print_stmt(stmt, indent);
    }
}

fn print_fn(
    Fn {
        name,
        is_extern,
        is_async,
        is_unsafe,
        type_params,
        variadic,
        params,
        ret,
        public,
        body,
        assign_subscript: _,
        attrs: _,
    }: &Fn,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    eprint!("{tabs}Fn[{name}]");
    print_bool!(is_async);
    print_bool!(is_unsafe);
    print_bool!(is_extern);
    print_bool!(variadic);
    print_bool!(public);
    eprintln!();

    let plus_1 = INDENT.repeat(indent + 1);
    let plus_2 = INDENT.repeat(indent + 2);
    if !type_params.is_empty() {
        eprintln!("{plus_1}Type Params:");
        for (name, path) in type_params {
            eprintln!("{plus_2}{name}: {path:?}");
        }
    }
    if !params.is_empty() {
        eprintln!("{plus_1}Params:");
        for param in params {
            eprintln!("{plus_2}{param:?}");
        }
    }

    eprintln!("{plus_1}Return Type: {ret:?}");
    if let Some(body) = body.as_ref() {
        eprintln!("{plus_1}Body: ");
        print_expr(body, indent);
    }
}

fn print_op_fn(
    OperatorFn {
        name,
        type_params,
        params,
        ret,
        body,
        attrs: _,
    }: &OperatorFn,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    eprintln!("{tabs}OperatorFn[{name}]");

    let plus_1 = INDENT.repeat(indent + 1);
    let plus_2 = INDENT.repeat(indent + 2);
    if !type_params.is_empty() {
        eprintln!("{plus_1}Type Params:");
        for (name, path) in type_params {
            eprintln!("{plus_2}{name}: {path:?}");
        }
    }
    if !params.is_empty() {
        eprintln!("{plus_1}Params:");
        for param in params {
            eprintln!("{plus_2}{param:?}");
        }
    }

    eprintln!("{plus_1}Return Type: {ret:?}");
    if let Some(body) = body.as_ref() {
        eprintln!("{plus_1}Body: ");
        print_expr(body, indent);
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
        operators,
    }: &Struct,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    eprintln!("{tabs}{type_name}[{name}]");
    print_bool!(public);
    eprintln!();

    print_impls(indent, impls);

    let plus_1 = INDENT.repeat(indent + 1);
    if !type_params.is_empty() {
        eprintln!("{tabs}Type Params:");
        for (name, impls) in type_params {
            eprintln!("{plus_1}{name}: {impls:?}");
        }
    }

    if !members.is_empty() {
        eprintln!("{tabs}Members: ");
        for member in members {
            eprintln!("{plus_1}{member:?}");
        }
    }

    eprintln!("{tabs}Functions:");
    for f in functions {
        print_fn(&f.data, indent + 1);
    }
    for f in operators {
        print_op_fn(&f.data, indent + 1);
    }
}

fn print_impls(indent: usize, impls: &[Located<ImplBlock>]) {
    let tabs = INDENT.repeat(indent);
    let plus_1 = INDENT.repeat(indent + 1);
    if !impls.is_empty() {
        eprintln!("{tabs}Impls:");
        for imp in impls {
            if !imp.data.type_params.is_empty() {
                eprintln!("{tabs}Type Params:");
                for (name, impls) in imp.data.type_params.iter() {
                    eprintln!("{plus_1}{name}: {impls:?}");
                }
            }

            eprintln!("{plus_1}{:?}", imp.data.path);
            for f in imp.data.functions.iter() {
                print_fn(&f.data, indent + 2)
            }
        }
    }
}
