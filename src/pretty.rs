use colored::Colorize;

use crate::{
    Located,
    ast::parsed::{
        Expr, ExprArena, ExprData, Fn, ImplBlock, IntPattern, OperatorFn, Param, Path, Pattern,
        Stmt, StmtData, Struct, TypeHint, Variant,
    },
    format::{FmtHint, FmtPath, FmtPatt, FmtUsePath},
    intern::{StrId, Strings},
};

const INDENT: &str = "  ";

macro_rules! bool {
    ($id: expr) => {
        if *$id { HeaderVar::Unnamed(stringify!($id)) } else { HeaderVar::None }
    };
}

macro_rules! str {
    ($self: expr, $id: expr) => {
        HeaderVar::Named(stringify!($id).into(), $self.strings.resolve($id).into())
    };
    ($self: expr, $id: expr, LOCATED) => {
        HeaderVar::Named(stringify!($id).into(), $self.strings.resolve(&$id.data).into())
    };
}

macro_rules! optstr {
    ($self: expr, $id: expr) => {
        if let Some(v) = $id {
            HeaderVar::Named(stringify!($id).into(), $self.strings.resolve(v).into())
        } else {
            HeaderVar::None
        }
    };
    ($self: expr, $id: expr, LOCATED) => {
        if let Some(v) = $id {
            HeaderVar::Named(stringify!($id).into(), $self.strings.resolve(&v.data).into())
        } else {
            HeaderVar::None
        }
    };
}

pub struct Pretty<'a> {
    strings: &'a Strings,
    arena: &'a ExprArena,
}

impl Pretty<'_> {
    fn print_stmt(&self, stmt: &Stmt, indent: usize) {
        let tabs = INDENT.repeat(indent);
        match &stmt.data.data {
            StmtData::Expr(expr) => {
                self.print_header(&tabs, "Stmt::Expr", &[]);
                self.print_expr(expr, indent + 1);
            }
            StmtData::Defer(expr) => {
                self.print_header(&tabs, "Stmt::Defer", &[]);
                self.print_expr(expr, indent + 1);
            }
            StmtData::Let { ty, value, patt } => {
                self.print_header(&tabs, "Stmt::Let", &[]);

                let tabs = INDENT.repeat(indent + 1);
                eprintln!("{tabs}{}: {}", "Pattern".yellow(), self.patt(&patt.data));

                if let Some(ty) = ty {
                    eprintln!("{tabs}{}: {}", "Type".yellow(), self.typ(*ty));
                }
                if let Some(value) = value {
                    self.print_expr(value, indent + 1);
                }
            }
            StmtData::Guard { cond, body } => {
                self.print_header(&tabs, "Stmt::Guard", &[]);

                let tabs = INDENT.repeat(indent + 1);
                eprintln!("{tabs}{}: ", "Condition".yellow());
                self.print_expr(cond, indent + 2);

                eprintln!("{tabs}{}: ", "Body".yellow());
                self.print_expr(body, indent + 2);
            }
            StmtData::Fn(f) => self.print_fn(f, indent),
            StmtData::Struct { base, packed } => {
                self.print_struct("Stmt::Struct", vec![bool!(packed)], base, None, indent)
            }
            StmtData::Union { tag, base, variants } => {
                if let Some(tag) = tag {
                    self.print_struct(
                        "Stmt::Union",
                        vec![HeaderVar::Named("tag", self.typ(*tag).to_string())],
                        base,
                        Some(variants),
                        indent,
                    )
                } else {
                    self.print_struct("Stmt::Union", vec![], base, Some(variants), indent)
                }
            }
            StmtData::UnsafeUnion(base) => {
                self.print_struct("StmtData::UnsafeUnion", vec![], base, None, indent)
            }
            StmtData::Trait {
                public,
                name,
                type_params,
                impls,
                functions,
                is_unsafe,
                sealed,
                assoc_types,
            } => {
                self.print_header(
                    &tabs,
                    "Stmt::Trait",
                    &[str!(self, name, LOCATED), bool!(public), bool!(sealed), bool!(is_unsafe)],
                );

                self.print_type_params(type_params, indent + 1, None);
                self.print_type_params(assoc_types, indent + 1, Some("Associated Types"));

                let plus_1 = INDENT.repeat(indent + 1);
                if !impls.is_empty() {
                    let plus_2 = INDENT.repeat(indent + 2);
                    eprintln!("{plus_1}{}: ", "Impls".yellow());
                    for imp in impls {
                        eprintln!("{plus_2}{}", self.path(imp));
                    }
                }

                eprintln!("{plus_1}{}:", "Functions".yellow());
                for f in functions {
                    self.print_fn(&f.data, indent + 2);
                }
            }
            StmtData::Extension { public, name, ty, type_params, impls, functions, operators } => {
                self.print_header(
                    &tabs,
                    "Stmt::Extension",
                    &[str!(self, name, LOCATED), bool!(public)],
                );

                self.print_type_params(type_params, indent + 1, None);

                let plus_1 = INDENT.repeat(indent + 1);
                eprintln!("{plus_1}{}: {}", "For".yellow(), self.typ(*ty));
                eprintln!("{plus_1}{}: ", "Functions".yellow());
                for f in functions {
                    self.print_fn(&f.data, indent + 2);
                }
                for f in operators {
                    self.print_op_fn(&f.data, indent + 2);
                }
                self.print_impls(indent, impls);
            }
            StmtData::Binding { name, ty, value, public, constant, is_extern, mutable } => {
                self.print_header(
                    &tabs,
                    "Stmt::Binding",
                    &[
                        str!(self, name, LOCATED),
                        bool!(public),
                        bool!(constant),
                        bool!(is_extern),
                        bool!(mutable),
                    ],
                );
                let tabs = INDENT.repeat(indent + 1);
                eprintln!("{tabs}{}: {}", "Type".yellow(), self.typ(*ty));
                if let Some(value) = value {
                    self.print_expr(value, indent + 1);
                }
            }
            StmtData::Module { name, body, public, file: _ } => {
                self.print_header(
                    &tabs,
                    "Stmt::Module",
                    &[str!(self, name, LOCATED), bool!(public)],
                );
                self.print_stmts(body, indent + 1);
            }
            StmtData::ModuleOOL { public, name, resolved } => {
                self.print_header(
                    &tabs,
                    "Stmt::ModuleOOL",
                    &[str!(self, name, LOCATED), bool!(public), bool!(resolved)],
                );
            }
            StmtData::Use(path) => {
                self.print_header(
                    &tabs,
                    "Stmt::Use",
                    &[HeaderVar::Named(
                        "value",
                        format!("{}", FmtUsePath::new(self.strings, path)),
                    )],
                );
            }
            StmtData::Error => {}
        }
    }

    fn print_expr(&self, expr: &Expr, indent: usize) {
        let tabs = INDENT.repeat(indent);
        match self.arena.get(expr.data) {
            ExprData::Binary { op, left, right } => {
                self.print_header(
                    &tabs,
                    "Expr::Binary",
                    &[HeaderVar::Named("op", format!("{op:?}"))],
                );
                self.print_expr(left, indent + 1);
                self.print_expr(right, indent + 1);
            }
            ExprData::Range { start, end, inclusive } => {
                self.print_header(&tabs, "Expr::Range", &[bool!(inclusive)]);
                let tabs = INDENT.repeat(indent + 1);
                if let Some(start) = start {
                    eprintln!("{tabs}From:");
                    self.print_expr(start, indent + 2);
                }
                if let Some(end) = end {
                    eprintln!("{tabs}To:");
                    self.print_expr(end, indent + 2);
                }
            }
            ExprData::Unary { op, expr } => {
                self.print_header(
                    &tabs,
                    "Expr::Unary",
                    &[HeaderVar::Named("op", format!("{op:?}"))],
                );
                self.print_expr(expr, indent + 1);
            }
            ExprData::Call { callee, args } | ExprData::Subscript { callee, args } => {
                if matches!(self.arena.get(expr.data), ExprData::Call { .. }) {
                    self.print_header(&tabs, "Expr::Call", &[]);
                } else {
                    self.print_header(&tabs, "Expr::Subscript", &[]);
                }
                let tabs = INDENT.repeat(indent + 1);
                self.print_expr(callee, indent + 1);

                if !args.is_empty() {
                    eprintln!("{tabs}{}: ", "Args".yellow());
                    for (name, expr) in args {
                        if let Some(name) = name {
                            eprintln!("{tabs}{INDENT}{}:", self.strings.resolve(&name.data));
                            self.print_expr(expr, indent + 3);
                        } else {
                            self.print_expr(expr, indent + 2);
                        }
                    }
                }
            }
            ExprData::Array(elements) | ExprData::Vec(elements) | ExprData::Set(elements) => {
                let name = match self.arena.get(expr.data) {
                    ExprData::Array(_) => "Expr::Array",
                    ExprData::Vec(_) => "Expr::Vec",
                    ExprData::Set(_) => "Expr::Set",
                    _ => unreachable!(),
                };
                self.print_header(&tabs, name, &[]);

                for el in elements {
                    self.print_expr(el, indent + 1);
                }
            }
            ExprData::ArrayWithInit { init, count } | ExprData::VecWithInit { init, count } => {
                let arr = matches!(self.arena.get(expr.data), ExprData::ArrayWithInit { .. });
                self.print_header(
                    &tabs,
                    ["Expr::VecWithInit", "Expr::ArrayWithInit"][arr as usize],
                    &[],
                );

                let tabs = INDENT.repeat(indent + 1);
                eprintln!("{tabs}{}: ", "Init".yellow());
                self.print_expr(init, indent + 2);
                eprintln!("{tabs}{}: ", "Count".yellow());
                self.print_expr(count, indent + 2);
            }
            ExprData::Tuple(elements) => {
                self.print_header(&tabs, "Expr::Tuple", &[]);
                let tabs = INDENT.repeat(indent + 1);
                for (name, expr) in elements {
                    eprintln!("{tabs}{}:", self.strings.resolve(&name.data));
                    self.print_expr(expr, indent + 2);
                }
            }
            ExprData::Map(expr) => {
                self.print_header(&tabs, "Expr::Map", &[]);
                let tabs = INDENT.repeat(indent + 1);
                for (key, value) in expr {
                    eprintln!("{tabs}{}: ", "Key".yellow());
                    self.print_expr(key, indent + 2);
                    eprintln!("{tabs}{}: ", "Value".yellow());
                    self.print_expr(value, indent + 2);
                }
            }
            ExprData::Integer(IntPattern { negative, value, width }) => {
                self.print_header(
                    &tabs,
                    "Expr::Integer",
                    &[
                        optstr!(self, width),
                        HeaderVar::Named(
                            "value",
                            format!("{}{value}", ["", "-"][*negative as usize]),
                        ),
                    ],
                );
            }
            ExprData::Float(v) => self.print_header(
                &tabs,
                "Expr::Float",
                &[
                    bool!(&v.negative),
                    HeaderVar::Named("value", v.value.to_string()),
                    v.suffix
                        .map(|s| HeaderVar::Named("suffix", self.strings.resolve(&s).into()))
                        .unwrap_or_default(),
                ],
            ),
            ExprData::String(v) => self.print_header(
                &tabs,
                "Expr::String",
                &[HeaderVar::Named("value", self.strings.resolve(v).into())],
            ),
            ExprData::Char(v) => {
                self.print_header(&tabs, "Expr::Char", &[HeaderVar::Named("value", format!("{v}"))])
            }
            ExprData::ByteString(v) => self.print_header(
                &tabs,
                "Expr::ByteString",
                &[HeaderVar::Named("value", format!("{v:?}"))],
            ),
            ExprData::ByteChar(v) => self.print_header(
                &tabs,
                "Expr::ByteChar",
                &[HeaderVar::Named("value", format!("{v:#x}"))],
            ),
            ExprData::Bool(v) => {
                self.print_header(&tabs, "Expr::Bool", &[HeaderVar::Named("value", format!("{v}"))])
            }
            ExprData::Void => self.print_header(&tabs, "Expr::Void", &[]),
            ExprData::Path(path) => self.print_header(
                &tabs,
                "Expr::Path",
                &[HeaderVar::Named("value", self.path(path).to_string())],
            ),
            ExprData::Block(expr, label) => {
                self.print_header(&tabs, "Expr::Block", &[optstr!(self, label)]);
                self.print_stmts(expr, indent + 1);
            }
            ExprData::If { cond, if_branch, else_branch } => {
                self.print_header(&tabs, "Expr::If", &[]);

                let tabs = INDENT.repeat(indent + 1);
                eprintln!("{tabs}{}: ", "Condition".yellow());
                self.print_expr(cond, indent + 2);

                eprintln!("{tabs}{}: ", "Body".yellow());
                self.print_expr(if_branch, indent + 2);

                if let Some(else_branch) = else_branch {
                    eprintln!("{tabs}{}: ", "Else".yellow());
                    self.print_expr(else_branch, indent + 2);
                }
            }
            ExprData::Loop { cond, body, do_while, label } => {
                self.print_header(&tabs, "Expr::Loop", &[bool!(do_while), optstr!(self, label)]);

                let tabs = INDENT.repeat(indent + 1);
                if let Some(cond) = cond {
                    eprintln!("{tabs}{}: ", "Condition".yellow());
                    self.print_expr(cond, indent + 2);
                }

                self.print_stmts(body, indent + 1);
            }
            ExprData::Member { source, member, generics } => {
                self.print_header(&tabs, "Expr::Member", &[str!(self, member, LOCATED)]);
                if !generics.is_empty() {
                    eprintln!("{tabs}{}:", "Type Arguments".yellow());
                    let tabs = INDENT.repeat(indent + 1);
                    for ty in generics.iter() {
                        eprintln!("{tabs}{}", self.typ(*ty))
                    }
                }
                self.print_expr(source, indent + 1);
            }
            ExprData::Return(expr) => {
                self.print_header(&tabs, "Expr::Return", &[]);
                self.print_expr(expr, indent + 1);
            }
            ExprData::Tail(expr) => {
                self.print_header(&tabs, "Expr::Tail", &[]);
                self.print_expr(expr, indent + 1);
            }
            ExprData::Break(expr, label) => {
                self.print_header(&tabs, "Expr::Break", &[optstr!(self, label, LOCATED)]);
                if let Some(expr) = expr {
                    self.print_expr(expr, indent + 1);
                }
            }
            ExprData::Continue(label) => {
                self.print_header(&tabs, "Expr::Continue", &[optstr!(self, label, LOCATED)]);
            }
            ExprData::For { patt, iter, body, label } => {
                self.print_header(&tabs, "Expr::For", &[optstr!(self, label)]);
                let tabs = INDENT.repeat(indent + 1);
                eprintln!("{tabs}{}: {}", "Pattern".yellow(), self.patt(&patt.data));
                eprintln!("{tabs}{}", "In:".yellow());
                self.print_expr(iter, indent + 2);
                eprintln!("{tabs}{}", "Body:".yellow());
                self.print_stmts(body, indent + 2);
            }
            ExprData::Is { expr, pattern } => {
                self.print_header(&tabs, "Expr::Is", &[]);
                let tabs = INDENT.repeat(indent + 1);
                eprintln!("{tabs}{}: {}", "Pattern".yellow(), self.patt(&pattern.data));
                self.print_expr(expr, indent + 1);
            }
            ExprData::As { expr, ty, throwing } => {
                self.print_header(&tabs, "Expr::As", &[bool!(throwing)]);
                let tabs = INDENT.repeat(indent + 1);
                eprintln!("{tabs}{}: {}", "Type".yellow(), self.typ(*ty));
                self.print_expr(expr, indent + 1);
            }
            ExprData::Match { expr, body } => {
                self.print_header(&tabs, "Expr::Match", &[]);
                self.print_expr(expr, indent + 1);

                let tabs = INDENT.repeat(indent + 1);
                for (i, (patt, expr)) in body.iter().enumerate() {
                    eprintln!(
                        "{tabs}{} [{}]:",
                        format!("Case {i}").yellow(),
                        self.patt(&patt.data.data)
                    );
                    self.print_expr(expr, indent + 2);
                }
            }
            ExprData::Error => {}
            ExprData::Lambda { params, ret, body, moves } => {
                self.print_header(&tabs, "Expr::Lambda", &[bool!(moves)]);
                let tabs = INDENT.repeat(indent + 1);
                if let Some(ret) = ret {
                    eprintln!("{tabs}{}: {}", "Return".yellow(), self.typ(*ret));
                }
                if !params.is_empty() {
                    eprintln!("{tabs}{}:", "Params".yellow());
                    let tabs = INDENT.repeat(indent + 2);
                    for (name, ty) in params {
                        if let Some(ty) = ty {
                            eprintln!(
                                "{tabs}{}: {}",
                                self.strings.resolve(&name.data),
                                self.typ(*ty)
                            );
                        } else {
                            eprintln!("{tabs}{}", self.strings.resolve(&name.data));
                        }
                    }
                }
                self.print_expr(body, indent + 1);
            }
            ExprData::Unsafe(expr) => {
                self.print_header(&tabs, "Expr::Unsafe", &[]);
                self.print_expr(expr, indent + 1);
            }
            ExprData::StringInterpolation { strings: parts, args } => {
                self.print_header(&tabs, "Expr::StringInterpolation", &[]);
                let plus_1 = INDENT.repeat(indent + 1);
                let plus_2 = INDENT.repeat(indent + 2);
                let plus_3 = INDENT.repeat(indent + 3);

                eprintln!("{plus_1}{}:", "Strings".yellow());
                for string in parts.iter() {
                    eprintln!("{plus_2}\"{}\"", self.strings.resolve(string));
                }

                eprintln!("{plus_1}{}:", "Args".yellow());
                for (expr, opts) in args {
                    if let Some(opts) = opts {
                        let alt = &opts.alt;
                        let zero = &opts.zero;
                        let vars = [
                            opts.fill
                                .map(|a| HeaderVar::Named("fill", a.to_string()))
                                .unwrap_or_default(),
                            opts.align
                                .map(|a| {
                                    HeaderVar::Named(
                                        "align",
                                        match a {
                                            crate::ast::Alignment::Left => "left".into(),
                                            crate::ast::Alignment::Right => "right".into(),
                                            crate::ast::Alignment::Center => "center".into(),
                                        },
                                    )
                                })
                                .unwrap_or_default(),
                            opts.sign
                                .map(|a| {
                                    HeaderVar::Named(
                                        "sign",
                                        match a {
                                            crate::ast::Sign::Positive => "+".into(),
                                            crate::ast::Sign::Negative => "-".into(),
                                        },
                                    )
                                })
                                .unwrap_or_default(),
                            bool!(alt),
                            bool!(zero),
                            opts.typ
                                .map(|a| {
                                    HeaderVar::Named(
                                        "type",
                                        match a {
                                            crate::ast::parsed::FormatType::Debug => "debug".into(),
                                            crate::ast::parsed::FormatType::Custom(id) => {
                                                self.strings.resolve(&id.data).into()
                                            }
                                        },
                                    )
                                })
                                .unwrap_or_default(),
                        ];

                        eprintln!(
                            "{plus_2}{} [{}]",
                            "Opts".yellow(),
                            self.format_header_vars(&vars)
                        );
                        if let Some(width) = &opts.width {
                            eprint!("{plus_3}{}: ", "Width".yellow());
                            self.print_expr(width, 0);
                        }
                        if let Some(prec) = &opts.prec {
                            eprint!("{plus_3}{}: ", "Prec".yellow());
                            self.print_expr(prec, 0);
                        }
                    }

                    self.print_expr(expr, indent + 2);
                }
            }
        }
    }

    fn print_stmts(&self, stmts: &[Stmt], indent: usize) {
        for stmt in stmts {
            self.print_stmt(stmt, indent);
        }
    }

    fn print_header(&self, tabs: &str, name: &str, vars: &[HeaderVar]) {
        self.print_header_ex(tabs, name, vars, "");
    }

    fn print_header_ex(&self, tabs: &str, name: &str, vars: &[HeaderVar], trailing: &str) {
        let res = self.format_header_vars(vars);
        match (name.starts_with("Expr"), res.is_empty()) {
            (false, true) => eprintln!("{tabs}{}{trailing}", name.cyan()),
            (false, false) => eprintln!("{tabs}{} [{res}]{trailing}", name.cyan()),
            (true, true) => eprintln!("{tabs}{}{trailing}", name.green()),
            (true, false) => eprintln!("{tabs}{} [{res}]{trailing}", name.green()),
        }
    }

    fn format_header_vars(&self, vars: &[HeaderVar]) -> String {
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
        res
    }

    fn print_type_params(
        &self,
        type_params: &[(Located<StrId>, Vec<Path>)],
        indent: usize,
        name: Option<&str>,
    ) {
        if !type_params.is_empty() {
            let tabs = INDENT.repeat(indent);
            let plus_1 = INDENT.repeat(indent + 1);

            eprintln!("{tabs}{}:", name.unwrap_or("Type Params").yellow());
            for (name, impls) in type_params {
                eprint!("{plus_1}{}", self.strings.resolve(&name.data));
                for (i, path) in impls.iter().enumerate() {
                    eprint!("{}{}", [" + ", ": "][(i == 0) as usize], self.path(path));
                }

                eprintln!();
            }
        }
    }

    fn do_print_fn(
        &self,
        type_params: &[(Located<StrId>, Vec<Path>)],
        params: &[Param],
        body: Option<&Expr>,
        indent: usize,
    ) {
        let plus_1 = INDENT.repeat(indent + 1);
        let plus_2 = INDENT.repeat(indent + 2);

        self.print_type_params(type_params, indent + 1, None);

        if !params.is_empty() {
            eprintln!("{plus_1}{}:", "Params".yellow());
            for param in params {
                eprintln!(
                    "{plus_2}{}{}: {}",
                    if param.keyword { "kw " } else { "" },
                    self.patt(&param.patt.data),
                    self.typ(param.ty)
                );
                if let Some(default) = &param.default {
                    self.print_expr(default, indent + 3);
                }
            }
        }

        if let Some(body) = body.as_ref() {
            self.print_expr(body, indent + 1);
        }
    }

    fn print_fn(
        &self,
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
            typ: _,
            attrs: _,
        }: &Fn,
        indent: usize,
    ) {
        self.print_header_ex(
            &INDENT.repeat(indent),
            "Stmt::Fn",
            &[
                str!(self, name, LOCATED),
                bool!(is_async),
                bool!(is_unsafe),
                bool!(is_extern),
                bool!(variadic),
                bool!(public),
            ],
            &ret.as_ref()
                .map(|ret| format!(" {} {}", "->".cyan(), self.typ(*ret)))
                .unwrap_or_default(),
        );
        self.do_print_fn(type_params, params, body.as_ref(), indent);
    }

    fn print_op_fn(
        &self,
        OperatorFn { name, type_params, params, ret, body, attrs: _ }: &OperatorFn,
        indent: usize,
    ) {
        self.print_header_ex(
            &INDENT.repeat(indent),
            "Stmt::OperatorFn",
            &[HeaderVar::Named("name", name.data.to_string())],
            &ret.as_ref()
                .map(|ret| format!(" {} {}", "->".cyan(), self.typ(*ret)))
                .unwrap_or_default(),
        );
        self.do_print_fn(type_params, params, body.as_ref(), indent);
    }

    fn print_struct(
        &self,
        type_name: &str,
        mut headers: Vec<HeaderVar>,
        Struct { name, type_params, members, impls, functions, public, operators }: &Struct,
        variants: Option<&[Variant]>,
        indent: usize,
    ) {
        let tabs = INDENT.repeat(indent);
        headers.push(HeaderVar::Named("name", self.strings.resolve(&name.data).into()));
        headers.push(bool!(public));
        self.print_header(&tabs, type_name, &headers);

        let plus_1 = INDENT.repeat(indent + 1);
        let plus_2 = INDENT.repeat(indent + 2);

        self.print_type_params(type_params, indent + 1, None);

        if let Some(variants) = variants.filter(|v| !v.is_empty()) {
            eprintln!("{plus_1}{}: ", "Variants".yellow());
            for member in variants {
                eprint!("{plus_2}{}", self.strings.resolve(&member.name.data));
                match &member.data {
                    Some((_, hint)) => eprintln!(": {}", self.typ(*hint)),
                    None => eprintln!(),
                }

                if let Some(default) = &member.tag {
                    self.print_expr(default, indent + 3);
                }
            }
        }

        if !members.is_empty() {
            eprintln!("{plus_1}{}: ", "Members".yellow());
            for member in members {
                eprintln!(
                    "{plus_2}{}{}: {}",
                    if member.public { "pub " } else { "" },
                    self.strings.resolve(&member.name.data),
                    self.typ(member.ty)
                );
                if let Some(default) = &member.default {
                    self.print_expr(default, indent + 3);
                }
            }
        }

        eprintln!("{plus_1}{}:", "Functions".yellow());
        for f in functions {
            self.print_fn(&f.data, indent + 2);
        }
        for f in operators {
            self.print_op_fn(&f.data, indent + 2);
        }

        self.print_impls(indent + 1, impls);
    }

    fn print_impls(&self, indent: usize, impls: &[Located<ImplBlock>]) {
        if impls.is_empty() {
            return;
        }

        let tabs = INDENT.repeat(indent);
        let plus_1 = INDENT.repeat(indent + 1);
        let plus_2 = INDENT.repeat(indent + 2);
        eprintln!("{tabs}{}:", "Impls".yellow());
        for imp in impls {
            eprintln!("{plus_1}{}: {}", "Path".yellow(), self.path(&imp.data.path));

            self.print_type_params(&imp.data.type_params, indent + 1, None);

            if !imp.data.assoc_types.is_empty() {
                eprintln!("{plus_1}{}:", "Associated Types".yellow());
                for (name, typ) in imp.data.assoc_types.iter() {
                    eprintln!("{plus_2}{}: {}", self.strings.resolve(&name.data), self.typ(*typ));
                }
            }

            for f in imp.data.functions.iter() {
                self.print_fn(&f.data, indent + 2);
            }
        }
    }
}

impl<'a> Pretty<'a> {
    fn patt(&'a self, patt: &'a Pattern) -> FmtPatt<'a> {
        FmtPatt::new(patt, self.strings, self.arena)
    }

    fn typ(&'a self, ty: TypeHint) -> FmtHint<'a> {
        FmtHint::new(ty, self.strings, self.arena)
    }

    fn path(&'a self, path: &'a Path) -> FmtPath<'a> {
        FmtPath::new(path, self.strings, self.arena)
    }
}

#[derive(Default)]
enum HeaderVar {
    Named(&'static str, String),
    Unnamed(&'static str),
    #[default]
    None,
}

pub fn print_stmt(stmt: &Stmt, strings: &Strings, arena: &ExprArena, indent: usize) {
    let pretty = Pretty { arena, strings };
    pretty.print_stmt(stmt, indent);
}
