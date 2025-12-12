use colored::Colorize;

use crate::{
    Located,
    ast::parsed::{
        Expr, ExprData, Fn, ImplBlock, IntPattern, OperatorFn, Param, Path, Pattern, Stmt,
        StmtData, Struct, TypeHint, UsePath, UsePathTail, Variant,
    },
    intern::{StrId, Strings, THIS_TYPE},
};

const INDENT: &str = "  ";

macro_rules! bool {
    ($id: expr) => {
        if *$id {
            HeaderVar::Unnamed(stringify!($id))
        } else {
            HeaderVar::None
        }
    };
}

macro_rules! str {
    ($id: expr, $strings: expr) => {
        HeaderVar::Named(stringify!($id).into(), $strings.resolve($id).into())
    };
    ($id: expr, $strings: expr, LOCATED) => {
        HeaderVar::Named(stringify!($id).into(), $strings.resolve(&$id.data).into())
    };
}

macro_rules! optstr {
    ($id: expr, $strings: expr) => {
        if let Some(v) = $id {
            HeaderVar::Named(stringify!($id).into(), $strings.resolve(v).into())
        } else {
            HeaderVar::None
        }
    };
    ($id: expr, $strings: expr, LOCATED) => {
        if let Some(v) = $id {
            HeaderVar::Named(stringify!($id).into(), $strings.resolve(&v.data).into())
        } else {
            HeaderVar::None
        }
    };
}

pub fn print_stmt(stmt: &Stmt, strings: &Strings, indent: usize) {
    let tabs = INDENT.repeat(indent);
    match &stmt.data.data {
        StmtData::Expr(expr) => {
            print_header(&tabs, "Stmt::Expr", &[]);
            print_expr(expr, strings, indent + 1);
        }
        StmtData::Defer(expr) => {
            print_header(&tabs, "Stmt::Defer", &[]);
            print_expr(expr, strings, indent + 1);
        }
        StmtData::Let { ty, value, patt } => {
            print_header(&tabs, "Stmt::Let", &[]);

            let tabs = INDENT.repeat(indent + 1);
            eprintln!(
                "{tabs}{}: {}",
                "Pattern".yellow(),
                FmtPatt::new(&patt.data, strings)
            );

            if let Some(ty) = ty {
                eprintln!("{tabs}{}: {}", "Type".yellow(), FmtType::new(ty, strings));
            }
            if let Some(value) = value {
                print_expr(value, strings, indent + 1);
            }
        }
        StmtData::Guard { cond, body } => {
            print_header(&tabs, "Stmt::Guard", &[]);

            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}{}: ", "Condition".yellow());
            print_expr(cond, strings, indent + 2);

            eprintln!("{tabs}{}: ", "Body".yellow());
            print_expr(body, strings, indent + 2);
        }
        StmtData::Fn(f) => print_fn(f, strings, indent),
        StmtData::Struct { base, packed } => print_struct(
            "Stmt::Struct",
            vec![bool!(packed)],
            base,
            None,
            strings,
            indent,
        ),
        StmtData::Union {
            tag,
            base,
            variants,
        } => {
            if let Some(tag) = tag {
                print_struct(
                    "Stmt::Union",
                    vec![HeaderVar::Named(
                        "tag",
                        FmtPath::new(tag, strings).to_string(),
                    )],
                    base,
                    Some(variants),
                    strings,
                    indent,
                )
            } else {
                print_struct("Stmt::Union", vec![], base, Some(variants), strings, indent)
            }
        }
        StmtData::UnsafeUnion(base) => {
            print_struct("StmtData::UnsafeUnion", vec![], base, None, strings, indent)
        }
        StmtData::Trait {
            public,
            name,
            type_params,
            impls,
            functions,
            is_unsafe,
            sealed,
        } => {
            print_header(
                &tabs,
                "Stmt::Trait",
                &[
                    str!(name, strings, LOCATED),
                    bool!(public),
                    bool!(sealed),
                    bool!(is_unsafe),
                ],
            );

            print_type_params(type_params, strings, indent + 1);

            let plus_1 = INDENT.repeat(indent + 1);
            if !impls.is_empty() {
                let plus_2 = INDENT.repeat(indent + 2);
                eprintln!("{plus_1}{}: ", "Impls".yellow());
                for imp in impls {
                    eprintln!("{plus_2}{}", FmtPath::new(imp, strings));
                }
            }

            eprintln!("{plus_1}{}:", "Functions".yellow());
            for f in functions {
                print_fn(&f.data, strings, indent + 2);
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
            print_header(
                &tabs,
                "Stmt::Extension",
                &[str!(name, strings, LOCATED), bool!(public)],
            );

            print_type_params(type_params, strings, indent + 1);

            let plus_1 = INDENT.repeat(indent + 1);
            eprintln!("{plus_1}{}: {}", "For".yellow(), FmtType::new(ty, strings));
            eprintln!("{plus_1}{}: ", "Functions".yellow());
            for f in functions {
                print_fn(&f.data, strings, indent + 2);
            }
            for f in operators {
                print_op_fn(&f.data, strings, indent + 2);
            }
            print_impls(indent, impls, strings);
        }
        StmtData::Binding {
            name,
            ty,
            value,
            public,
            constant,
            is_extern,
            mutable,
        } => {
            print_header(
                &tabs,
                "Stmt::Binding",
                &[
                    str!(name, strings, LOCATED),
                    bool!(public),
                    bool!(constant),
                    bool!(is_extern),
                    bool!(mutable),
                ],
            );
            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}{}: {}", "Type".yellow(), FmtType::new(ty, strings));
            if let Some(value) = value {
                print_expr(value, strings, indent + 1);
            }
        }
        StmtData::Module {
            name,
            body,
            public,
            file: _,
        } => {
            print_header(
                &tabs,
                "Stmt::Module",
                &[str!(name, strings, LOCATED), bool!(public)],
            );
            print_stmts(body, strings, indent + 1);
        }
        StmtData::Use(UsePath {
            public,
            origin,
            components,
            tail,
        }) => {
            print_header(
                &tabs,
                "Stmt::Use",
                &[
                    bool!(public),
                    HeaderVar::Named(
                        "value",
                        format!(
                            "{origin}{}::{}",
                            components
                                .iter()
                                .map(|x| strings.resolve(&x.data))
                                .collect::<Vec<_>>()
                                .join("::"),
                            if let UsePathTail::Ident(ident) = tail {
                                strings.resolve(&ident.data)
                            } else {
                                "*"
                            }
                        ),
                    ),
                ],
            );
        }
        StmtData::Error => {}
    }
}

pub fn print_expr(expr: &Expr, strings: &Strings, indent: usize) {
    let tabs = INDENT.repeat(indent);
    match &expr.data {
        ExprData::Binary { op, left, right } => {
            print_header(
                &tabs,
                "Expr::Binary",
                &[HeaderVar::Named("op", format!("{op:?}"))],
            );
            print_expr(left, strings, indent + 1);
            print_expr(right, strings, indent + 1);
        }
        ExprData::Range {
            start,
            end,
            inclusive,
        } => {
            print_header(&tabs, "Expr::Range", &[bool!(inclusive)]);
            let tabs = INDENT.repeat(indent + 1);
            if let Some(start) = start {
                eprintln!("{tabs}From:");
                print_expr(start, strings, indent + 2);
            }
            if let Some(end) = end {
                eprintln!("{tabs}To:");
                print_expr(end, strings, indent + 2);
            }
        }
        ExprData::Unary { op, expr } => {
            print_header(
                &tabs,
                "Expr::Unary",
                &[HeaderVar::Named("op", format!("{op:?}"))],
            );
            print_expr(expr, strings, indent + 1);
        }
        ExprData::Call { callee, args } | ExprData::Subscript { callee, args } => {
            if matches!(expr.data, ExprData::Call { .. }) {
                print_header(&tabs, "Expr::Call", &[]);
            } else {
                print_header(&tabs, "Expr::Subscript", &[]);
            }
            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}{}: ", "Callee".yellow());
            print_expr(callee, strings, indent + 2);

            if !args.is_empty() {
                eprintln!("{tabs}{}: ", "Args".yellow());
                for (name, expr) in args {
                    if let Some(name) = name {
                        eprintln!("{tabs}{INDENT}{}:", strings.resolve(&name.data));
                        print_expr(expr, strings, indent + 3);
                    } else {
                        print_expr(expr, strings, indent + 2);
                    }
                }
            }
        }
        ExprData::Array(elements) | ExprData::Vec(elements) | ExprData::Set(elements) => {
            let name = match expr.data {
                ExprData::Array(_) => "Expr::Array",
                ExprData::Vec(_) => "Expr::Vec",
                ExprData::Set(_) => "Expr::Set",
                _ => unreachable!(),
            };
            print_header(&tabs, name, &[]);

            for el in elements {
                print_expr(el, strings, indent + 1);
            }
        }
        ExprData::ArrayWithInit { init, count } | ExprData::VecWithInit { init, count } => {
            let arr = matches!(expr.data, ExprData::ArrayWithInit { .. });
            print_header(
                &tabs,
                ["Expr::VecWithInit", "Expr::ArrayWithInit"][arr as usize],
                &[],
            );

            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}{}: ", "Init".yellow());
            print_expr(init, strings, indent + 2);
            eprintln!("{tabs}{}: ", "Count".yellow());
            print_expr(count, strings, indent + 2);
        }
        ExprData::Tuple(elements) => {
            print_header(&tabs, "Expr::Tuple", &[]);
            for el in elements {
                print_expr(el, strings, indent + 1);
            }
        }
        ExprData::Map(expr) => {
            print_header(&tabs, "Expr::Map", &[]);
            let tabs = INDENT.repeat(indent + 1);
            for (key, value) in expr {
                eprintln!("{tabs}{}: ", "Key".yellow());
                print_expr(key, strings, indent + 2);
                eprintln!("{tabs}{}: ", "Value".yellow());
                print_expr(value, strings, indent + 2);
            }
        }
        ExprData::Integer(IntPattern {
            negative,
            value,
            width,
        }) => {
            print_header(
                &tabs,
                "Expr::Integer",
                &[
                    optstr!(width, strings),
                    HeaderVar::Named("value", format!("{}{value}", ["", "-"][*negative as usize])),
                ],
            );
        }
        ExprData::Float(v) => print_header(
            &tabs,
            "Expr::Float",
            &[HeaderVar::Named("value", strings.resolve(v).into())],
        ),
        ExprData::String(v) => print_header(
            &tabs,
            "Expr::String",
            &[HeaderVar::Named("value", strings.resolve(v).into())],
        ),
        ExprData::Char(v) => print_header(
            &tabs,
            "Expr::Char",
            &[HeaderVar::Named("value", format!("{v}"))],
        ),
        ExprData::ByteString(v) => print_header(
            &tabs,
            "Expr::ByteString",
            &[HeaderVar::Named("value", format!("{v:?}"))],
        ),
        ExprData::ByteChar(v) => print_header(
            &tabs,
            "Expr::ByteChar",
            &[HeaderVar::Named("value", format!("{v:#x}"))],
        ),
        ExprData::Bool(v) => print_header(
            &tabs,
            "Expr::Bool",
            &[HeaderVar::Named("value", format!("{v}"))],
        ),
        ExprData::Void => print_header(&tabs, "Expr::Void", &[]),
        ExprData::Path(path) => print_header(
            &tabs,
            "Expr::Path",
            &[HeaderVar::Named(
                "value",
                FmtPath::new(path, strings).to_string(),
            )],
        ),
        ExprData::Block(expr, label) => {
            print_header(&tabs, "Expr::Block", &[optstr!(label, strings)]);
            print_stmts(expr, strings, indent + 1);
        }
        ExprData::If {
            cond,
            if_branch,
            else_branch,
        } => {
            print_header(&tabs, "Expr::If", &[]);

            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}{}: ", "Condition".yellow());
            print_expr(cond, strings, indent + 2);

            eprintln!("{tabs}{}: ", "Body".yellow());
            print_expr(if_branch, strings, indent + 2);

            if let Some(else_branch) = else_branch {
                eprintln!("{tabs}{}: ", "Else".yellow());
                print_expr(else_branch, strings, indent + 2);
            }
        }
        ExprData::Loop {
            cond,
            body,
            do_while,
            label,
        } => {
            print_header(
                &tabs,
                "Expr::Loop",
                &[bool!(do_while), optstr!(label, strings)],
            );

            let tabs = INDENT.repeat(indent + 1);
            if let Some(cond) = cond {
                eprintln!("{tabs}{}: ", "Condition".yellow());
                print_expr(cond, strings, indent + 2);
            }

            print_stmts(body, strings, indent + 1);
        }
        ExprData::Member {
            source,
            member,
            generics,
        } => {
            print_header(&tabs, "Expr::Member", &[str!(member, strings, LOCATED)]);
            if !generics.is_empty() {
                eprintln!("{tabs}{}:", "Type Arguments".yellow());
                let tabs = INDENT.repeat(indent + 1);
                for ty in generics.iter() {
                    eprintln!("{tabs}{}", FmtType::new(ty, strings))
                }
            }
            print_expr(source, strings, indent + 1);
        }
        ExprData::Return(expr) => {
            print_header(&tabs, "Expr::Return", &[]);
            print_expr(expr, strings, indent + 1);
        }
        ExprData::Tail(expr) => {
            print_header(&tabs, "Expr::Tail", &[]);
            print_expr(expr, strings, indent + 1);
        }
        ExprData::Break(expr, label) => {
            print_header(&tabs, "Expr::Break", &[optstr!(label, strings, LOCATED)]);
            if let Some(expr) = expr {
                print_expr(expr, strings, indent + 1);
            }
        }
        ExprData::Continue(label) => {
            print_header(&tabs, "Expr::Continue", &[optstr!(label, strings, LOCATED)]);
        }
        ExprData::For {
            patt,
            iter,
            body,
            label,
        } => {
            print_header(&tabs, "Expr::For", &[optstr!(label, strings)]);
            let tabs = INDENT.repeat(indent + 1);
            eprintln!(
                "{tabs}{}: {}",
                "Pattern".yellow(),
                FmtPatt::new(&patt.data, strings)
            );
            eprintln!("{tabs}{}", "In:".yellow());
            print_expr(iter, strings, indent + 2);
            eprintln!("{tabs}{}", "Body:".yellow());
            print_stmts(body, strings, indent + 2);
        }
        ExprData::Is { expr, pattern } => {
            print_header(&tabs, "Expr::Is", &[]);
            let tabs = INDENT.repeat(indent + 1);
            eprintln!(
                "{tabs}{}: {}",
                "Pattern".yellow(),
                FmtPatt::new(&pattern.data, strings)
            );
            print_expr(expr, strings, indent + 1);
        }
        ExprData::As { expr, ty, throwing } => {
            print_header(&tabs, "Expr::As", &[bool!(throwing)]);
            let tabs = INDENT.repeat(indent + 1);
            eprintln!("{tabs}{}: {}", "Type".yellow(), FmtType::new(ty, strings));
            print_expr(expr, strings, indent + 1);
        }
        ExprData::Match { expr, body } => {
            print_header(&tabs, "Expr::Match", &[]);
            print_expr(expr, strings, indent + 1);

            let tabs = INDENT.repeat(indent + 1);
            for (i, (patt, expr)) in body.iter().enumerate() {
                eprintln!(
                    "{tabs}{} [{}]:",
                    format!("Case {i}").yellow(),
                    FmtPatt::new(&patt.data.data, strings)
                );
                print_expr(expr, strings, indent + 2);
            }
        }
        ExprData::Error => {}
        ExprData::Lambda {
            params,
            ret,
            body,
            moves,
        } => {
            print_header(&tabs, "Expr::Lambda", &[bool!(moves)]);
            let tabs = INDENT.repeat(indent + 1);
            if let Some(ret) = ret {
                eprintln!(
                    "{tabs}{}: {}",
                    "Return".yellow(),
                    FmtType::new(ret, strings)
                );
            }
            if !params.is_empty() {
                eprintln!("{tabs}{}:", "Params".yellow());
                let tabs = INDENT.repeat(indent + 2);
                for (name, ty) in params {
                    if let Some(ty) = ty {
                        eprintln!(
                            "{tabs}{}: {}",
                            strings.resolve(&name.data),
                            FmtType::new(ty, strings)
                        );
                    } else {
                        eprintln!("{tabs}{}", strings.resolve(&name.data));
                    }
                }
            }
            print_expr(body, strings, indent + 1);
        }
        ExprData::Unsafe(expr) => {
            print_header(&tabs, "Expr::Unsafe", &[]);
            print_expr(expr, strings, indent + 1);
        }
        ExprData::StringInterpolation(parts) => {
            print_header(&tabs, "Expr::StringInterpolation", &[]);
            for part in parts {
                print_expr(part, strings, indent + 1);
            }
        }
    }
}

fn print_stmts(stmts: &[Stmt], strings: &Strings, indent: usize) {
    for stmt in stmts {
        print_stmt(stmt, strings, indent);
    }
}

enum HeaderVar {
    Named(&'static str, String),
    Unnamed(&'static str),
    None,
}

fn print_header(tabs: &str, name: &str, vars: &[HeaderVar]) {
    print_header_ex(tabs, name, vars, "");
}

fn print_header_ex(tabs: &str, name: &str, vars: &[HeaderVar], trailing: &str) {
    let mut res = String::new();
    for var in vars {
        let val = match var {
            HeaderVar::Named(name, val) => format!("{}: '{}'", name.red(), val.green()),
            &HeaderVar::Unnamed(val) => format!("{}", val.red()),
            HeaderVar::None => continue,
        };

        if !res.is_empty() {
            res += ", ";
        }

        res += &val;
    }

    match (name.starts_with("Expr"), res.is_empty()) {
        (false, true) => eprintln!("{tabs}{}{trailing}", name.cyan()),
        (false, false) => eprintln!("{tabs}{} [{res}]{trailing}", name.cyan()),
        (true, true) => eprintln!("{tabs}{}{trailing}", name.green()),
        (true, false) => eprintln!("{tabs}{} [{res}]{trailing}", name.green()),
    }
}

fn print_type_params(
    type_params: &[(Located<StrId>, Vec<Path>)],
    strings: &Strings,
    indent: usize,
) {
    if !type_params.is_empty() {
        let plus_1 = INDENT.repeat(indent);
        let plus_2 = INDENT.repeat(indent + 1);

        eprintln!("{plus_1}{}:", "Type Params".yellow());
        for (name, impls) in type_params {
            eprint!("{plus_2}{}", strings.resolve(&name.data));
            for (i, path) in impls.iter().enumerate() {
                eprintln!(
                    "{}{}",
                    [" + ", ": "][(i == 0) as usize],
                    FmtPath::new(path, strings)
                );
            }

            if impls.is_empty() {
                eprintln!();
            }
        }
    }
}

fn do_print_fn(
    type_params: &[(Located<StrId>, Vec<Path>)],
    params: &[Param],
    body: Option<&Expr>,
    strings: &Strings,
    indent: usize,
) {
    let plus_1 = INDENT.repeat(indent + 1);
    let plus_2 = INDENT.repeat(indent + 2);

    print_type_params(type_params, strings, indent + 1);

    if !params.is_empty() {
        eprintln!("{plus_1}{}:", "Params".yellow());
        for param in params {
            eprintln!(
                "{plus_2}{}{}: {}",
                if param.keyword { "kw " } else { "" },
                FmtPatt::new(&param.patt.data, strings),
                FmtType::new(&param.ty, strings)
            );
            if let Some(default) = &param.default {
                print_expr(default, strings, indent + 3);
            }
        }
    }

    if let Some(body) = body.as_ref() {
        print_expr(body, strings, indent + 1);
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
    strings: &Strings,
    indent: usize,
) {
    print_header_ex(
        &INDENT.repeat(indent),
        "Stmt::Fn",
        &[
            str!(name, strings, LOCATED),
            bool!(is_async),
            bool!(is_unsafe),
            bool!(is_extern),
            bool!(variadic),
            bool!(public),
        ],
        &format!(" {} {}", "->".cyan(), FmtType::new(ret, strings)),
    );
    do_print_fn(type_params, params, body.as_ref(), strings, indent);
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
    strings: &Strings,
    indent: usize,
) {
    print_header_ex(
        &INDENT.repeat(indent),
        "Stmt::OperatorFn",
        &[HeaderVar::Named("name", name.data.to_string())],
        &format!(" {} {}", "->".cyan(), FmtType::new(ret, strings)),
    );
    do_print_fn(type_params, params, body.as_ref(), strings, indent);
}

fn print_struct(
    type_name: &str,
    mut headers: Vec<HeaderVar>,
    Struct {
        name,
        type_params,
        members,
        impls,
        functions,
        public,
        operators,
    }: &Struct,
    variants: Option<&[Variant]>,
    strings: &Strings,
    indent: usize,
) {
    let tabs = INDENT.repeat(indent);
    headers.push(HeaderVar::Named("name", strings.resolve(&name.data).into()));
    headers.push(bool!(public));
    print_header(&tabs, type_name, &headers);

    let plus_1 = INDENT.repeat(indent + 1);
    let plus_2 = INDENT.repeat(indent + 2);

    print_type_params(type_params, strings, indent + 1);

    if let Some(variants) = variants
        && !variants.is_empty()
    {
        eprintln!("{plus_1}{}: ", "Variants".yellow());
        for member in variants {
            eprintln!(
                "{plus_2}{}: <TODO: Variant>",
                strings.resolve(&member.name.data),
            );
            if let Some(default) = &member.tag {
                print_expr(default, strings, indent + 3);
            }
        }
    }

    if !members.is_empty() {
        eprintln!("{plus_1}{}: ", "Members".yellow());
        for member in members {
            eprintln!(
                "{plus_2}{}{}: {}",
                if member.public { "pub " } else { "" },
                strings.resolve(&member.name.data),
                FmtType::new(&member.ty, strings)
            );
            if let Some(default) = &member.default {
                print_expr(default, strings, indent + 3);
            }
        }
    }

    eprintln!("{plus_1}{}:", "Functions".yellow());
    for f in functions {
        print_fn(&f.data, strings, indent + 2);
    }
    for f in operators {
        print_op_fn(&f.data, strings, indent + 2);
    }

    print_impls(indent + 1, impls, strings);
}

fn print_impls(indent: usize, impls: &[Located<ImplBlock>], strings: &Strings) {
    let tabs = INDENT.repeat(indent);
    let plus_1 = INDENT.repeat(indent + 1);
    if !impls.is_empty() {
        eprintln!("{tabs}{}:", "Impls".yellow());
        for imp in impls {
            eprintln!(
                "{plus_1}{}: {}",
                "Path".yellow(),
                FmtPath::new(&imp.data.path, strings)
            );

            print_type_params(&imp.data.type_params, strings, indent + 1);

            for f in imp.data.functions.iter() {
                print_fn(&f.data, strings, indent + 2);
            }
        }
    }
}

#[derive(derive_more::Constructor)]
struct FmtType<'a> {
    ty: &'a TypeHint,
    strings: &'a Strings,
}

impl std::fmt::Display for FmtType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty {
            TypeHint::Regular(path) => write!(f, "{}", FmtPath::new(path, self.strings)),
            TypeHint::Array(ty, _) => write!(f, "[{}; <expr>]", FmtType::new(ty, self.strings)),
            TypeHint::Vec(ty) => write!(f, "[{}]", FmtType::new(ty, self.strings)),
            TypeHint::Slice(ty) => write!(f, "[{}..]", FmtType::new(ty, self.strings)),
            TypeHint::SliceMut(ty) => write!(f, "[mut {}..]", FmtType::new(ty, self.strings)),
            TypeHint::Tuple(vals) => {
                write!(f, "(")?;
                for (i, ty) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", FmtType::new(ty, self.strings))?;
                }
                write!(f, ")")
            }
            TypeHint::AnonStruct(vals) => {
                write!(f, "struct {{")?;
                for (i, (name, ty)) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "{}: {}",
                        self.strings.resolve(name),
                        FmtType::new(ty, self.strings)
                    )?;
                }
                write!(f, "}}")
            }
            TypeHint::Set(ty) => write!(f, "#[{}]", FmtType::new(ty, self.strings)),
            TypeHint::Map(kv) => write!(
                f,
                "[{}: {}]",
                FmtType::new(&kv[0], self.strings),
                FmtType::new(&kv[1], self.strings),
            ),
            TypeHint::Option(ty) => write!(f, "?{}", FmtType::new(ty, self.strings)),
            TypeHint::Ptr(ty) => write!(f, "*{}", FmtType::new(ty, self.strings)),
            TypeHint::MutPtr(ty) => write!(f, "*mut {}", FmtType::new(ty, self.strings)),
            TypeHint::RawPtr(ty) => write!(f, "^{}", FmtType::new(ty, self.strings)),
            TypeHint::RawMutPtr(ty) => write!(f, "^mut {}", FmtType::new(ty, self.strings)),
            TypeHint::DynPtr(ty) => write!(f, "*dyn {}", FmtPath::new(ty, self.strings)),
            TypeHint::DynMutPtr(ty) => write!(f, "*dyn mut {}", FmtPath::new(ty, self.strings)),
            TypeHint::Fn {
                is_extern,
                params,
                ret,
            } => {
                write!(f, "{}fn (", if *is_extern { "extern " } else { "" })?;
                for (i, ty) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", FmtType::new(ty, self.strings))?;
                }
                write!(f, "): {}", FmtType::new(ret, self.strings))
            }
            TypeHint::Void => write!(f, "void"),
            TypeHint::This(_) => write!(f, "{THIS_TYPE}"),
            TypeHint::Error => write!(f, "Error"),
        }
    }
}

#[derive(derive_more::Constructor)]
struct FmtPath<'a> {
    path: &'a Path,
    strings: &'a Strings,
}

impl std::fmt::Display for FmtPath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

        let mut tmp = String::new();

        write!(tmp, "{}", self.path.origin)?;
        for (i, (name, ty_args)) in self.path.components.iter().enumerate() {
            if i > 0 {
                write!(tmp, "::")?;
            }

            write!(tmp, "{}", self.strings.resolve(&name.data))?;
            if ty_args.is_empty() {
                continue;
            }

            write!(tmp, "::<")?;
            for (i, generic) in ty_args.iter().enumerate() {
                if i > 0 {
                    write!(tmp, ", ")?;
                }
                write!(tmp, "{}", FmtType::new(generic, self.strings))?;
            }
            write!(tmp, ">")?;
        }

        if tmp.is_empty() {
            write!(f, "<empty>")
        } else {
            write!(f, "{tmp}")
        }
    }
}

#[derive(derive_more::Constructor)]
struct FmtPatt<'a> {
    patt: &'a Pattern,
    strings: &'a Strings,
}

impl std::fmt::Display for FmtPatt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        _ = self.patt;
        _ = self.strings;
        write!(f, "<TODO: Pattern>")
    }
}
