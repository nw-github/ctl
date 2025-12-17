use enum_as_inner::EnumAsInner;

use crate::{
    Warning,
    ast::{
        Attributes, BinaryOp, UnaryOp,
        checked::{
            ArrayPattern, Block, Expr as CExpr, ExprData as CExprData, FormatOpts as CFormatSpec,
            Pattern as CPattern, PatternData, RestPattern, Stmt as CStmt,
        },
        declared::{Fn as DFn, ImplBlock as DImplBlock, Stmt as DStmt},
        parsed::{
            Expr as PExpr, ExprData as PExprData, Pattern, Stmt as PStmt, StmtData as PStmtData, *,
        },
    },
    comptime_int::ComptimeInt,
    dgraph::Dependencies,
    error::{Diagnostics, Error},
    hash::{HashMap, HashSet, IndexMap},
    intern::{StrId, Strings, THIS_TYPE},
    lexer::{Located, Span},
    project::{Configuration, Project},
    sym::*,
    typeid::{
        BitSizeResult, CInt, FnPtr, GenericExtension, GenericFn, GenericTrait, GenericUserType,
        Type, TypeArgs, TypeId, Types, WithTypeArgs,
    },
};

macro_rules! resolve_type {
    ($self: expr, $ty: expr) => {{
        let id = match $self.proj.types[$ty] {
            Type::Unresolved(id) => {
                let (hint, scope) = $self.proj.types.take_unresolved(id);
                $self.enter_id_and_resolve(scope, |this| this.resolve_typehint(&hint))
            }
            _ => $ty,
        };
        $ty = id;
        id
    }};
}

macro_rules! resolve_impl {
    ($self: expr, $tr: expr) => {{
        $tr = match std::mem::take(&mut $tr) {
            TraitImpl::Unchecked { scope, data } => {
                $self.enter_id_and_resolve(scope, |this| match data {
                    TraitImplData::Path(path) => match this.resolve_type_path(&path) {
                        ResolvedType::UserType(ut) => {
                            if this.proj.scopes.get(ut.id).kind.is_trait() {
                                TraitImpl::Checked(ut)
                            } else {
                                let name = format!("type '{}'", type_name!(this, ut));
                                this.error(Error::expected_found(
                                    "trait",
                                    &name,
                                    path.final_component_span(),
                                ))
                            }
                        }
                        ResolvedType::Builtin(ty) => {
                            let name = format!("type '{}'", type_name!(this, ty));
                            this.error(Error::expected_found(
                                "trait",
                                &name,
                                path.final_component_span(),
                            ))
                        }
                        ResolvedType::Error => Default::default(),
                    },
                    TraitImplData::Operator { tr, ty_args, span } => {
                        let Some(tr_id) = this.proj.scopes.lang_types.get(&tr).copied() else {
                            let data = strdata!(this, tr);
                            return this.error(Error::no_lang_item(data, span));
                        };

                        this.enter_id_and_resolve(scope, |this| {
                            TraitImpl::Checked(GenericTrait::new(
                                tr_id,
                                this.resolve_type_args(tr_id, &ty_args, true, span),
                            ))
                        })
                    }
                })
            }
            other => other,
        };
    }};
}

macro_rules! check_hover {
    ($self: expr, $span: expr, $item: expr) => {{
        if $span.len > 0
            && let Some(items) = &mut $self.proj.lsp_items
        {
            items.push(($item, $span));
        }
    }};
}

macro_rules! bail {
    ($self: expr, $err: expr) => {{
        $self.proj.diag.report($err);
        return Default::default();
    }};
}

macro_rules! mute_errors {
    ($self: expr, $mute: expr, $body: expr) => {{
        let prev = $self.proj.diag.set_errors_enabled($mute);
        let res = $body;
        $self.proj.diag.set_errors_enabled(prev);
        res
    }};
    ($self: expr, $body: expr) => {
        mute_errors!($self, false, $body)
    };
}

macro_rules! strdata {
    ($self: expr, $key: expr) => {{ $self.proj.strings.resolve(&$key) }};
}

macro_rules! type_mismatch_err {
    ($self: expr, $expected: expr, $received: expr, $span: expr) => {{
        let scopes = &$self.proj.scopes;
        let types = &mut $self.proj.types;
        let strings = &$self.proj.strings;
        Error::type_mismatch(
            &$expected.name(scopes, types, strings),
            &$received.name(scopes, types, strings),
            $span,
        )
    }};
}

macro_rules! type_name {
    ($self: expr, $typ: expr) => {{ $typ.name(&$self.proj.scopes, &mut $self.proj.types, &$self.proj.strings) }};
}

macro_rules! intern {
    ($self: expr, $($args: tt)*) => {{
        $self.proj.strings.get_or_intern(format!($($args)*))
    }};
}

macro_rules! report_error {
    ($self: expr, $span: expr, $($args: tt)*) => {{
        let err = Error::new(format!($($args)*), $span);
        $self.proj.diag.report(err);
        Default::default()
    }};
}

macro_rules! named_error {
    ($self: expr, $err: path, $key: expr, $span: expr) => {{
        let err = $err($self.proj.strings.resolve(&$key), $span);
        $self.proj.diag.report(err);
        Default::default()
    }};
}

macro_rules! check_unsafe {
    ($self: expr, $err: expr) => {{
        if let Safety::Unsafe(v) = &mut $self.safety {
            *v = true;
        } else {
            $self.proj.diag.report($err);
        }
    }};
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum MemberFnType {
    Normal,
    Dynamic,
    Trait(GenericTrait),
}

#[derive(Debug, Clone)]
pub struct MemberFn {
    pub func: GenericFn,
    pub owner: ScopeId,
    pub typ: MemberFnType,
    pub public: bool,
    pub inst: TypeId,
}

#[derive(Default)]
pub enum ResolvedType {
    UserType(GenericUserType),
    Builtin(TypeId),
    #[default]
    Error,
}

#[derive(Default)]
pub enum ResolvedValue {
    UnionConstructor(GenericUserType),
    Fn(GenericFn),
    MemberFn(MemberFn),
    Var(VariableId),
    NotFound(Located<StrId>),
    #[default]
    Error,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum Safety {
    #[default]
    Safe,
    Unsafe(bool),
}

#[derive(Clone, derive_more::From)]
pub enum LspItem {
    Type(UserTypeId),
    Module(ScopeId),
    Literal(CExpr),
    Fn(FunctionId, Option<UserTypeId>),
    Var(VariableId),
    /// This variant only exists so the LSP doesn't apply the `mutable` modifier to the semantic
    /// tokens for parameter labels
    FnParamLabel(VariableId, FunctionId),
    Attribute(StrId),
    Property(Option<TypeId>, UserTypeId, StrId),
    BuiltinType(&'static str),
}

impl From<TypeItem> for LspItem {
    fn from(value: TypeItem) -> Self {
        match value {
            TypeItem::Type(item) => Self::Type(item),
            TypeItem::Module(item) => Self::Module(item),
        }
    }
}

impl From<ValueItem> for LspItem {
    fn from(value: ValueItem) -> Self {
        match value {
            ValueItem::Fn(id) => Self::Fn(id, None),
            ValueItem::Var(id) => Self::Var(id),
            ValueItem::StructConstructor(_, id) => Self::Fn(id, None),
            ValueItem::UnionConstructor(id) => Self::Type(id),
        }
    }
}

impl From<FunctionId> for LspItem {
    fn from(value: FunctionId) -> Self {
        Self::Fn(value, None)
    }
}

#[derive(Default)]
pub struct LspInput {
    pub completion: Option<Span>,
}

impl LspInput {
    pub fn matches(user: Option<Span>, ast: Span) -> bool {
        user.is_some_and(|user| user.file == ast.file && ast.includes(user.pos))
    }
}

pub struct Completions {
    pub items: Vec<LspItem>,
    pub method: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PatternType {
    Regular,
    BodylessFn,
    Fn,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Cast {
    None,
    Unsafe,
    Fallible,
    Infallible,
}

impl Cast {
    fn get(src: &Type, dst: &Type) -> Cast {
        match src {
            Type::Usize | Type::Isize => match dst {
                Type::Ptr(_) | Type::MutPtr(_) | Type::FnPtr(_) => Cast::Unsafe,
                Type::Uint(_) | Type::Int(_) | Type::CInt(_) | Type::CUint(_) => Cast::Fallible,
                Type::Usize | Type::Isize => Cast::Fallible,
                Type::RawPtr(_) | Type::RawMutPtr(_) | Type::F32 | Type::F64 => Cast::Infallible,
                _ => Cast::None,
            },
            Type::CInt(_) | Type::CUint(_) => match dst {
                Type::Uint(_) | Type::Int(_) | Type::CInt(_) | Type::CUint(_) => Cast::Fallible,
                Type::Usize | Type::Isize => Cast::Fallible,
                Type::F32 | Type::F64 => Cast::Infallible,
                _ => Cast::None,
            },
            src if src.as_integral(true).is_some() => {
                let a = src.as_integral(true).unwrap();
                // from can only be Uint(n) | Int(n) | Char | Bool now
                match dst {
                    // we definitely don't support any targets with < 16 bit pointers
                    Type::Usize | Type::Isize if !src.is_bool() && a.bits > 16 => Cast::Fallible,
                    // C types should never be < 8 bits? at least we won't support anything like that
                    Type::CInt(_) | Type::CUint(_) if !src.is_bool() && a.bits > 8 => {
                        Cast::Fallible
                    }
                    Type::F32 | Type::F64 => Cast::Infallible,
                    // d800-e000 is invalid for a char, u15::MAX is 0x7fff
                    Type::Char if matches!(src, Type::Uint(n) if *n <= 15) => Cast::Infallible,
                    Type::Char => Cast::Fallible,
                    _ => {
                        if let Some(b) = dst.as_integral(false) {
                            if (a.signed == b.signed && a.bits <= b.bits)
                                || (a.signed != b.signed && a.bits < b.bits)
                            {
                                Cast::Infallible
                            } else {
                                Cast::Fallible
                            }
                        } else {
                            Cast::None
                        }
                    }
                }
            }
            Type::F32 | Type::F64 => match dst {
                Type::Uint(_)
                | Type::Int(_)
                | Type::CInt(_)
                | Type::CUint(_)
                | Type::Isize
                | Type::Usize
                | Type::F32
                | Type::F64 => Cast::Infallible,
                _ => Cast::None,
            },
            Type::Ptr(_)
            | Type::MutPtr(_)
            | Type::FnPtr(_)
            | Type::RawPtr(_)
            | Type::RawMutPtr(_) => {
                match dst {
                    Type::Ptr(_) | Type::MutPtr(_) | Type::FnPtr(_) => Cast::Unsafe,
                    Type::Usize | Type::Isize | Type::RawPtr(_) | Type::RawMutPtr(_) => {
                        Cast::Infallible
                    } // maybe only *T to ^mut T should be infallible
                    _ => Cast::None,
                }
            }
            _ => Cast::None,
        }
    }
}

struct PatternParams {
    scrutinee: TypeId,
    mutable: bool,
    pattern: Located<Pattern>,
    typ: PatternType,
    has_hint: bool,
}

pub struct TypeChecker {
    safety: Safety,
    current: ScopeId,
    lsp_input: LspInput,
    proj: Project,
    listening_vars: Vec<VariableId>,
    listening_expr: usize,
    current_expr: usize,
    current_static: Option<(VariableId, Vec<VariableId>)>,
    tables: Tables,
}

impl TypeChecker {
    pub fn check(
        project: Vec<PStmt>,
        diag: Diagnostics,
        lsp: Option<LspInput>,
        conf: Configuration,
        mut strings: Strings,
    ) -> Project {
        let mut this = Self {
            tables: Tables::new(&mut strings),
            safety: Safety::Safe,
            current: ScopeId::ROOT,
            proj: Project::new(conf, diag, strings, lsp.is_some()),
            lsp_input: lsp.unwrap_or_default(),
            listening_vars: Vec::new(),
            listening_expr: 1,
            current_expr: 1,
            current_static: None,
        };

        let mut autouse = vec![];
        let mut last_file_id = None;
        let mut last_scope = ScopeId::ROOT;
        for module in project {
            last_file_id = Some(module.data.span.file);

            let stmt = this.declare_stmt(&mut autouse, module);
            for scope in autouse.drain(..) {
                this.enter_id_and_resolve(scope, |_| {});

                for (&name, &item) in this.proj.scopes[scope].tns.clone().iter() {
                    if item.public {
                        this.proj.scopes.autouse_tns.entry(name).or_insert(Vis::new(*item, false));
                    }
                }

                for (&name, &item) in this.proj.scopes[scope].vns.clone().iter() {
                    if item.public {
                        this.proj.scopes.autouse_vns.entry(name).or_insert(Vis::new(*item, false));
                    }
                }
            }

            last_scope = *stmt.as_module().unwrap().0;
            this.check_stmt(stmt);
        }

        this.proj.main =
            this.proj.scopes[last_scope].vns.get(&Strings::MAIN).and_then(|id| id.as_fn()).copied();
        if !this.proj.conf.flags.lib {
            if let Some(id) = this.proj.main {
                let func = this.proj.scopes.get(id);
                if !func.params.is_empty() {
                    this.proj.diag.report(Error::new(
                        "main function must have an empty parameter list",
                        func.name.span,
                    ));
                }

                if !matches!(
                    this.proj.types[func.ret],
                    Type::Void
                        | Type::Never
                        | Type::Int(_)
                        | Type::Uint(_)
                        | Type::CInt(_)
                        | Type::CUint(_)
                        | Type::Isize
                        | Type::Usize
                ) {
                    this.proj.diag.report(Error::new(
                        "main function must return void, never, or an integer type",
                        func.name.span,
                    ));
                }
            } else {
                this.proj.diag.report(Error::new(
                    "no main function found in non-library module",
                    Span { file: last_file_id.unwrap_or_default(), pos: 0, len: 0 },
                ));
            }
        }

        if this.proj.panic_handler.is_none() {
            this.proj.diag.report(Error::new(
                "missing panic handler function",
                Span { file: last_file_id.unwrap_or_default(), pos: 0, len: 0 },
            ));
        }

        for (_, var) in this.proj.scopes.vars() {
            let data = strdata!(this, var.name.data);
            if !var.unused
                || matches!(var.name.data, Strings::THIS_PARAM | Strings::EMPTY)
                || data.starts_with('_')
            {
                continue;
            }

            if this.proj.scopes.walk(var.scope).any(|(id, _)| id == last_scope) {
                this.proj.diag.report(Warning::unused_variable(data, var.name.span));
            }
        }

        this.proj
    }

    pub fn with_project<T>(proj: &mut Project, f: impl FnOnce(&mut TypeChecker) -> T) -> T {
        let mut tc = Self {
            tables: Tables::new(&mut proj.strings),
            safety: Safety::Safe,
            current: ScopeId::ROOT,
            lsp_input: Default::default(),
            proj: std::mem::take(proj),
            listening_vars: Vec::new(),
            listening_expr: 1,
            current_expr: 1,
            current_static: None,
        };
        let res = f(&mut tc);
        std::mem::swap(proj, &mut tc.proj);
        res
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.proj.diag.report(error);
        T::default()
    }

    fn current_function(&self) -> Option<FunctionId> {
        self.proj.scopes.function_of(self.current)
    }

    fn enter<T>(&mut self, kind: ScopeKind, f: impl FnOnce(&mut Self) -> T) -> T {
        let id = self.proj.scopes.create_scope(self.current, kind, false);
        self.enter_id(id, f)
    }

    fn insert<T: ItemId>(&mut self, value: T::Value, public: bool, no_redef: bool) -> T {
        let res = T::insert_in(&mut self.proj.scopes, value, public, self.current);
        self.check_hover(
            res.id.name(&self.proj.scopes).span,
            match res.item {
                InsertedItem::TypeLike(item) => item.into(),
                InsertedItem::ValueLike(item) => item.into(),
            },
        );

        if res.existed && no_redef {
            let name = res.id.name(&self.proj.scopes);
            self.error(Error::redefinition(strdata!(self, name.data), name.span))
        }
        res.id
    }

    fn can_access_privates(&self, scope: ScopeId) -> bool {
        self.proj
            .scopes
            .module_of(scope)
            .map(|target| self.proj.scopes.walk(self.current).any(|(id, _)| id == target))
            .unwrap_or_default()
    }

    fn is_subscript(&self, f: FunctionId) -> bool {
        // TODO: add a field to Function, don't check by name
        strdata!(self, self.proj.scopes.get(f).name.data).starts_with("$sub")
    }

    #[inline(always)]
    pub(crate) fn scopes(&self) -> &Scopes {
        &self.proj.scopes
    }
}

/// LSP routines
impl TypeChecker {
    #[inline]
    fn check_hover(&mut self, span: Span, item: LspItem) {
        check_hover!(self, span, item);
    }

    fn check_dot_completions(&mut self, span: Span, ty: TypeId, method: bool) {
        if self.proj.completions.is_some() || !LspInput::matches(self.lsp_input.completion, span) {
            return;
        }

        if ty == TypeId::UNKNOWN {
            return;
        }

        let mut completions = vec![];
        let mut added = HashSet::new();
        let mut add_methods = |proj: &Project,
                               c: &mut Vec<LspItem>,
                               fns: &[Vis<FunctionId>],
                               cap: bool,
                               m: UserTypeId,
                               dynamic: bool| {
            for func in fns {
                let f = proj.scopes.get(func.id);
                if added.contains(&f.name.data) {
                    continue;
                }

                if dynamic && !f.is_dyn_compatible(&proj.scopes, &proj.types, m) {
                    continue;
                }

                if (f.public || cap)
                    && (!method || f.params.first().is_some_and(|p| p.label == Strings::THIS_PARAM))
                {
                    c.push(LspItem::Fn(func.id, Some(m)));
                    added.insert(f.name.data);
                }
            }
        };

        if let Some(ut_id) = self.proj.types[ty].as_user().map(|ut| ut.id) {
            self.resolve_impls_recursive(ut_id);

            let data = self.proj.scopes.get(ut_id);
            let cap = self.can_access_privates(data.scope);
            if method {
                for (name, _) in data.members.iter().filter(|(_, m)| m.public || cap) {
                    completions.push(LspItem::Property(Some(ty), ut_id, *name))
                }
            }

            for tr in self.proj.scopes.get(ut_id).impls.iter().flat_map(|ut| ut.as_checked()) {
                for tr in self.proj.scopes.get_trait_impls(tr.id) {
                    let data = self.proj.scopes.get(tr);
                    add_methods(&self.proj, &mut completions, &data.fns, cap, tr, false);
                }
            }

            add_methods(&self.proj, &mut completions, &data.fns, cap, ut_id, false);
        } else if let Some(tr) = self.proj.types[ty].as_dyn_pointee() {
            let tr_id = tr.id;
            self.resolve_impls_recursive(tr_id);
            for tr in self.proj.scopes.get_trait_impls(tr_id) {
                add_methods(
                    &self.proj,
                    &mut completions,
                    &self.proj.scopes.get(tr).fns,
                    true,
                    tr,
                    true,
                );
            }
        }

        let extensions = self.extensions_in_scope_for(ty, self.current);
        for ext in extensions.iter() {
            self.resolve_impls_recursive(ext.id);
            for imp in self.proj.scopes.get(ext.id).impls.iter().flat_map(|imp| imp.as_checked()) {
                for tr in self.proj.scopes.get_trait_impls(imp.id) {
                    add_methods(
                        &self.proj,
                        &mut completions,
                        &self.proj.scopes.get(tr).fns,
                        true,
                        tr,
                        false,
                    );
                }
            }
        }

        for ext in extensions.iter() {
            let data = self.proj.scopes.get(ext.id);
            add_methods(
                &self.proj,
                &mut completions,
                &data.fns,
                self.can_access_privates(data.scope),
                ext.id,
                false,
            );
        }

        self.proj.completions = Some(Completions { items: completions, method });
    }

    fn check_cursor_completions(&mut self, span: Span, ty: bool) {
        if self.proj.completions.is_some() || !LspInput::matches(self.lsp_input.completion, span) {
            return;
        }

        let mut completions = vec![];
        let mut emitted_vars = HashSet::new();
        let current_func = self.current_function();
        let mut add_from_vns = |completions: &mut Vec<LspItem>,
                                vns: &HashMap<StrId, Vis<ValueItem>>,
                                utscope: bool| {
            for (_, item) in vns.iter() {
                match item.id {
                    ValueItem::Fn(id) => {
                        if utscope {
                            return;
                        }
                        completions.push(LspItem::Fn(id, None))
                    }
                    ValueItem::Var(id) => {
                        let var = self.proj.scopes.get(id);
                        if !var.is_static && current_func != self.proj.scopes.function_of(var.scope)
                        {
                            return;
                        }
                        if emitted_vars.contains(&var.name.data) {
                            return;
                        }
                        completions.push(LspItem::Var(id));
                        emitted_vars.insert(var.name.data);
                    }
                    ValueItem::StructConstructor(_, id) => completions.push(LspItem::Fn(id, None)),
                    ValueItem::UnionConstructor(_) => {}
                }
            }
        };

        let add_from_tns = |completions: &mut Vec<LspItem>, tns: &HashMap<StrId, Vis<TypeItem>>| {
            for (_, item) in tns.iter() {
                if item.as_type().is_some_and(|&id| {
                    strdata!(self, self.proj.scopes.get(id).name.data).starts_with('$') ||
                        self.proj.scopes.get(id).kind.is_extension()
                }) {
                    continue;
                }

                completions.push(item.id.into());
            }
        };

        let mut saw_root = false;
        for (id, scope) in self.proj.scopes.walk(self.current) {
            if !ty {
                add_from_vns(
                    &mut completions,
                    &scope.vns,
                    matches!(scope.kind, ScopeKind::UserType(_)),
                );
            }

            add_from_tns(&mut completions, &scope.tns);
            if id == ScopeId::ROOT {
                saw_root = true;
            }

            if scope.kind.is_module() {
                break;
            }
        }

        if !ty {
            add_from_vns(&mut completions, &self.proj.scopes.autouse_vns, false);

            if !saw_root {
                add_from_vns(&mut completions, &self.proj.scopes[ScopeId::ROOT].vns, false);
            }
        }

        add_from_tns(&mut completions, &self.proj.scopes.autouse_tns);
        if !saw_root {
            add_from_tns(&mut completions, &self.proj.scopes[ScopeId::ROOT].tns);
        }

        #[rustfmt::skip]
        let builtins = [
            "void", "never", "f32", "f64", "bool", "char", "c_char",
            "c_short", "c_int", "c_long", "c_longlong", "c_uchar", "c_ushort", "c_uint",
            "c_ulong", "c_ulonglong", "int", "uint", "u8", "i8", "u16", "i16", "u32", "i32", "u64",
            "i64", "u128", "i128",
        ];
        completions.extend(builtins.into_iter().map(LspItem::BuiltinType));

        self.proj.completions = Some(Completions { items: completions, method: false });
    }

    fn check_module_completions(&mut self, span: Span, ty: bool, scope: ScopeId) {
        if self.proj.completions.is_some() || !LspInput::matches(self.lsp_input.completion, span) {
            return;
        }

        let mut completions = vec![];
        let cap = self.can_access_privates(scope);
        let scope = &self.proj.scopes[scope];
        if !ty {
            for (_, item) in scope.vns.iter().filter(|item| item.1.public || cap) {
                match item.id {
                    ValueItem::Fn(id) => {
                        if matches!(scope.kind, ScopeKind::UserType(_)) {
                            continue;
                        }
                        completions.push(LspItem::Fn(id, None))
                    }
                    ValueItem::Var(id) => {
                        let var = self.proj.scopes.get(id);
                        if !var.is_static {
                            continue;
                        }
                        completions.push(LspItem::Var(id));
                    }
                    ValueItem::StructConstructor(_, id) => completions.push(LspItem::Fn(id, None)),
                    ValueItem::UnionConstructor(_) => {}
                }
            }
        }

        for (_, item) in scope.tns.iter().filter(|item| item.1.public || cap) {
            if let Some(&id) = item.as_type() {
                let ut = self.proj.scopes.get(id);
                if strdata!(self, ut.name.data).starts_with('$') || ut.kind.is_template() {
                    continue;
                }
            }

            completions.push(item.id.into());
        }

        self.proj.completions = Some(Completions { items: completions, method: false });
    }

    fn check_arg_label_hover(&mut self, span: Span, param: CheckedParam, func: &GenericFn) {
        let f = self.proj.scopes.get(func.id);
        if let Some(owner) = f.constructor {
            let ty = param.ty.with_templates(&mut self.proj.types, &func.ty_args);
            return self.check_hover(span, LspItem::Property(Some(ty), owner, param.label));
        }

        if let ParamPattern::Checked(patt) = param.patt
            && let PatternData::Variable(id) = patt.data
        {
            self.check_hover(span, LspItem::FnParamLabel(id, func.id));
        }
    }
}

/// Forward declaration pass routines
impl TypeChecker {
    fn insert_user_type(&mut self, value: UserType, public: bool) -> UserTypeId {
        let id = self.insert::<UserTypeId>(value, public, true);
        if let Some(name) = self.proj.scopes.get(id).attrs.val(Strings::ATTR_LANG) {
            self.proj.scopes.lang_types.insert(name, id);
        }
        for (&name, m) in self.proj.scopes.get(id).members.iter() {
            check_hover!(self, m.span, LspItem::Property(None, id, name));
        }
        id
    }

    fn declare_struct(&mut self, base: Struct, attrs: Attributes, packed: bool) -> DStmt {
        let pub_constructor = base.public && !base.members.iter().any(|m| !m.public);
        let (ut, init, fns, impls) = self.enter(ScopeKind::None, |this| {
            let init = this.enter(ScopeKind::None, |this| {
                this.declare_fn(Fn {
                    public: pub_constructor,
                    name: base.name,
                    is_async: false,
                    is_extern: false,
                    variadic: false,
                    is_unsafe: false,
                    type_params: vec![],
                    params: base
                        .members
                        .iter()
                        .map(|member| Param {
                            keyword: true,
                            // use default span so hovers ignore this
                            patt: Located::nowhere(Pattern::Path(Path::from(member.name))),
                            ty: member.ty.clone(),
                            default: member.default.clone(),
                        })
                        .collect(),
                    ret: Some(Self::typehint_for_struct(&base.name, &base.type_params)),
                    body: None,
                    attrs: Default::default(),
                    assign_subscript: false,
                })
            });
            let mut members = IndexMap::with_capacity(base.members.len());
            for member in base.members {
                let prev = members.insert(
                    member.name.data,
                    CheckedMember::new(
                        member.public,
                        this.declare_type_hint(member.ty),
                        member.name.span,
                    ),
                );
                if prev.is_some() {
                    let name = strdata!(this, member.name.data);
                    this.error(Error::redefinition_k("member variable", name, member.name.span))
                }
            }

            let (impls, blocks, subscripts) = this.declare_impl_blocks(base.impls, base.operators);
            let mut fns = this.declare_fns(base.functions);
            let kind = if packed {
                UserTypeKind::PackedStruct(PackedStruct::default())
            } else {
                UserTypeKind::Struct
            };
            let ut = this.ut_from_stuff(
                attrs,
                base.name,
                base.public,
                members,
                kind,
                base.type_params,
                &fns,
                impls,
                &blocks,
                &subscripts,
            );

            fns.extend(subscripts);
            (ut, init, fns, blocks)
        });

        let scope = ut.body_scope;
        let id = self.insert_user_type(ut, base.public);
        let prev = self.proj.scopes[self.current].vns.insert(
            base.name.data,
            Vis::new(ValueItem::StructConstructor(id, init.id), pub_constructor),
        );
        if prev.is_some() {
            self.error(Error::redefinition(
                strdata!(self, self.proj.scopes.get(id).name.data),
                base.name.span,
            ))
        }

        self.proj.scopes[scope].kind = ScopeKind::UserType(id);
        self.proj.scopes.get_mut(init.id).constructor = Some(id);

        DStmt::Struct { init, id, impls, fns }
    }

    fn declare_union(
        &mut self,
        tag: Option<Path>,
        base: Struct,
        variants: Vec<Variant>,
        attrs: Attributes,
    ) -> DStmt {
        let (ut, impls, fns, member_cons_len) = self.enter(ScopeKind::None, |this| {
            let mut rvariants = IndexMap::with_capacity(base.members.len());
            let mut members = IndexMap::with_capacity(base.members.len());
            let mut params = Vec::with_capacity(base.members.len());
            let mut fns = Vec::with_capacity(base.members.len());
            for member in base.members {
                if members
                    .insert(
                        member.name.data,
                        CheckedMember::new(
                            member.public,
                            this.declare_type_hint(member.ty.clone()),
                            member.name.span,
                        ),
                    )
                    .is_some()
                {
                    let err = Error::redefinition_k(
                        "member",
                        strdata!(this, member.name.data),
                        member.name.span,
                    );
                    this.error(err)
                }

                params.push(Param {
                    keyword: true,
                    patt: Located::new(member.name.span, Pattern::Path(Path::from(member.name))),
                    ty: member.ty,
                    default: member.default,
                });
            }

            let (impls, blocks, subscripts) = this.declare_impl_blocks(base.impls, base.operators);
            let ret = Self::typehint_for_struct(&base.name, &base.type_params);
            let mut enum_union = true;
            for variant in variants {
                let mut params = params.clone();
                match variant.data {
                    VariantData::Empty => {
                        rvariants.insert(
                            variant.name.data,
                            CheckedVariant {
                                ty: None,
                                span: variant.name.span,
                                discrim: variant
                                    .tag
                                    .map(Discriminant::Unchecked)
                                    .unwrap_or_default(),
                            },
                        );
                    }
                    VariantData::StructLike(smembers) => {
                        enum_union = false;
                        rvariants.insert(
                            variant.name.data,
                            CheckedVariant {
                                ty: Some(
                                    this.declare_type_hint(Located::nowhere(TypeHint::AnonStruct(
                                        smembers
                                            .iter()
                                            .map(|m| (m.name.data, m.ty.clone()))
                                            .collect(),
                                    ))),
                                ),
                                span: variant.name.span,
                                discrim: variant
                                    .tag
                                    .map(Discriminant::Unchecked)
                                    .unwrap_or_default(),
                            },
                        );

                        for member in smembers {
                            if members.contains_key(&member.name.data) {
                                let err = Error::shared_member(
                                    strdata!(this, member.name.data),
                                    member.name.span,
                                );
                                this.error(err)
                            }

                            params.push(Param {
                                keyword: true,
                                // use default span so hovers ignore this
                                patt: Located::nowhere(Pattern::Path(Path::from(member.name))),
                                ty: member.ty,
                                default: member.default,
                            });
                        }
                    }
                    VariantData::TupleLike(members) => {
                        enum_union = false;
                        rvariants.insert(
                            variant.name.data,
                            CheckedVariant {
                                ty: Some(this.declare_type_hint(Located::nowhere(
                                    TypeHint::Tuple(
                                        members.iter().map(|(ty, _)| ty.clone()).collect(),
                                    ),
                                ))),
                                span: variant.name.span,
                                discrim: variant
                                    .tag
                                    .map(Discriminant::Unchecked)
                                    .unwrap_or_default(),
                            },
                        );

                        for (i, (ty, default)) in members.into_iter().enumerate() {
                            params.push(Param {
                                keyword: false,
                                patt: Located::nowhere(Pattern::Path(Path::from(
                                    Located::nowhere(intern!(this, "{i}")),
                                ))),
                                ty,
                                default,
                            });
                        }
                    }
                }

                fns.push(this.declare_fn(Fn {
                    public: base.public,
                    name: Located::new(variant.name.span, variant.name.data),
                    is_extern: false,
                    is_async: false,
                    variadic: false,
                    is_unsafe: false,
                    type_params: vec![],
                    params,
                    ret: Some(ret.clone()),
                    body: None,
                    attrs: Default::default(),
                    assign_subscript: false,
                }));
            }
            let member_cons_len = fns.len();
            fns.extend(this.declare_fns_iter(base.functions));
            let tag = if let Some(tag) = tag {
                this.declare_type_hint(Located::new(tag.span(), TypeHint::Regular(tag)))
            } else {
                TypeId::UNKNOWN
            };
            let ut = this.ut_from_stuff(
                attrs,
                base.name,
                base.public,
                members,
                UserTypeKind::Union(Union { tag, variants: rvariants, enum_union }),
                base.type_params,
                &fns,
                impls,
                &blocks,
                &subscripts,
            );

            fns.extend(subscripts);
            (ut, blocks, fns, member_cons_len)
        });
        let scope = ut.body_scope;
        let id = self.insert_user_type(ut, base.public);
        self.proj.scopes[scope].kind = ScopeKind::UserType(id);
        for c in fns.iter().take(member_cons_len) {
            self.proj.scopes.get_mut(c.id).constructor = Some(id);
        }

        DStmt::Union { id, impls, fns }
    }

    fn declare_unsafe_union(&mut self, mut base: Struct, attrs: Attributes) -> DStmt {
        let (ut, fns, impls) = self.enter(ScopeKind::None, |this| {
            let mut members = IndexMap::with_capacity(base.members.len());
            for member in base.members.iter_mut() {
                let prev = members.insert(
                    member.name.data,
                    CheckedMember::new(
                        true,
                        this.declare_type_hint(std::mem::take(&mut member.ty)),
                        member.name.span,
                    ),
                );
                if prev.is_some() {
                    let name = strdata!(this, member.name.data);
                    this.error(Error::redefinition_k("member variable", name, member.name.span))
                }
            }

            let (impls, blocks, subscripts) = this.declare_impl_blocks(base.impls, base.operators);
            let mut fns = this.declare_fns(base.functions);
            let ut = this.ut_from_stuff(
                attrs,
                base.name,
                base.public,
                members,
                UserTypeKind::UnsafeUnion,
                base.type_params,
                &fns,
                impls,
                &blocks,
                &subscripts,
            );
            fns.extend(subscripts);
            (ut, fns, blocks)
        });

        let scope = ut.body_scope;
        let id = self.insert_user_type(ut, base.public);
        let prev = self.proj.scopes[self.current]
            .vns
            .insert(base.name.data, Vis::new(ValueItem::UnionConstructor(id), base.public));
        if prev.is_some() {
            self.error(Error::redefinition(
                strdata!(self, self.proj.scopes.get(id).name.data),
                base.name.span,
            ))
        }
        self.proj.scopes[scope].kind = ScopeKind::UserType(id);

        DStmt::Union { id, impls, fns }
    }

    fn declare_stmt(&mut self, autouse: &mut Vec<ScopeId>, stmt: PStmt) -> DStmt {
        if self.check_disabled(&stmt.attrs, stmt.data.span) {
            return DStmt::None;
        }

        match stmt.data.data {
            PStmtData::Module { public, name, body, .. } => {
                let parent = self.current;
                self.enter(ScopeKind::Module(name), |this| {
                    this.check_hover(name.span, this.current.into());
                    if this.proj.scopes[parent]
                        .tns
                        .insert(name.data, Vis::new(this.current.into(), public))
                        .is_some()
                    {
                        let data = strdata!(this, name.data);
                        this.error(Error::redefinition(data, name.span))
                    }

                    let autouse_id = this.proj.strings.get_or_intern_static("autouse");
                    let std_id = this.proj.strings.get_or_intern_static("std");
                    if stmt.attrs.iter().any(|attr| attr.name.data == autouse_id) {
                        let std = this.proj.scopes[ScopeId::ROOT]
                            .find_in_tns(std_id)
                            .and_then(|inner| inner.as_module().copied());
                        if this.proj.scopes.walk(this.current).any(|(id, _)| Some(id) == std) {
                            autouse.push(this.current);
                        } else {
                            this.error(Error::new(
                                "autouse modules may only be defined by 'std'",
                                name.span,
                            ))
                        }
                    }

                    DStmt::Module {
                        id: this.current,
                        body: body
                            .into_iter()
                            .map(|stmt| this.declare_stmt(autouse, stmt))
                            .collect(),
                    }
                })
            }
            PStmtData::ModuleOOL { name, resolved, .. } => {
                // TODO: report an error if this is not at the top level of a main.ctl/equivalent
                if !resolved {
                    self.proj.diag.report(Error::new(
                        format!(
                            "couldn't find module '{0}' (couldn't read {0}.ctl or {0}/main.ctl)",
                            strdata!(self, name.data),
                        ),
                        name.span,
                    ));
                    DStmt::None
                } else {
                    DStmt::ModuleOOL { name }
                }
            }
            PStmtData::Struct { base, packed } => self.declare_struct(base, stmt.attrs, packed),
            PStmtData::Union { tag, base, variants } => {
                self.declare_union(tag, base, variants, stmt.attrs)
            }
            PStmtData::UnsafeUnion(base) => self.declare_unsafe_union(base, stmt.attrs),
            PStmtData::Trait {
                public,
                name,
                type_params,
                impls,
                functions,
                sealed,
                is_unsafe: _,
            } => {
                let lang_item = stmt.attrs.val(Strings::ATTR_LANG);
                let (tr, fns, this_id) = self.enter(ScopeKind::None, |this| {
                    let impls: Vec<_> = impls
                        .into_iter()
                        .map(|path| TraitImpl::Unchecked {
                            scope: this.current,
                            data: TraitImplData::Path(path),
                        })
                        .collect();
                    let this_id = this.insert(
                        UserType::template(
                            Located::new(name.span, Strings::THIS_TYPE),
                            this.current,
                            impls.clone(),
                        ),
                        false,
                        false,
                    );
                    let fns = this.declare_fns(functions);
                    let tr = this.ut_from_stuff(
                        stmt.attrs,
                        name,
                        public,
                        Default::default(),
                        UserTypeKind::Trait(this_id, sealed),
                        type_params,
                        &fns,
                        impls,
                        &[],
                        &[],
                    );
                    (tr, fns, this_id)
                });

                let scope = tr.body_scope;
                let id = self.insert::<UserTypeId>(tr, public, true);
                if let Some(name) = lang_item {
                    self.proj.scopes.lang_types.insert(name, id);
                }
                let imp =
                    GenericTrait::from_type_params(&self.proj.scopes, &mut self.proj.types, id);
                self.proj.scopes.get_mut(this_id).impls.push(TraitImpl::Checked(imp));
                self.proj.scopes[scope].kind = ScopeKind::UserType(id);
                DStmt::Trait { id, fns }
            }
            PStmtData::Extension { public, name, ty, type_params, impls, functions, operators } => {
                let (ext, impl_blocks, fns) = self.enter(ScopeKind::None, |this| {
                    let (impls, blocks, subscripts) = this.declare_impl_blocks(impls, operators);
                    let mut fns = this.declare_fns(functions);
                    let ty = this.declare_type_hint(ty);
                    let ext = this.ut_from_stuff(
                        stmt.attrs,
                        name,
                        public,
                        Default::default(),
                        UserTypeKind::Extension(ty),
                        type_params,
                        &fns,
                        impls,
                        &blocks,
                        &subscripts,
                    );
                    fns.extend(subscripts);
                    (ext, blocks, fns)
                });

                let scope = ext.body_scope;
                let id = self.insert::<UserTypeId>(ext, public, true);
                self.proj.scopes[scope].kind = ScopeKind::UserType(id);
                DStmt::Extension { id, impls: impl_blocks, fns }
            }
            PStmtData::Fn(f) => DStmt::Fn(self.declare_fn(f)),
            PStmtData::Binding { public, constant, mut name, mutable, ty, value, is_extern } => {
                let ty = self.declare_type_hint(ty);
                let mut unused = true;
                if name.data == Strings::UNDERSCORE {
                    name.data = Strings::EMPTY;
                    unused = false;
                }

                DStmt::Binding {
                    id: self.insert::<VariableId>(
                        Variable {
                            attrs: stmt.attrs,
                            public,
                            name,
                            ty,
                            unused,
                            is_extern,
                            is_static: true,
                            has_hint: true,
                            mutable: mutable && !constant,
                            ..Default::default()
                        },
                        public,
                        true,
                    ),
                    value,
                    constant,
                }
            }
            PStmtData::Use(stmt) => {
                if matches!(stmt.tail, UsePathTail::All) || self.resolve_use(&stmt, true).is_err() {
                    self.proj.scopes[self.current].use_stmts.push(stmt);
                }
                DStmt::None
            }
            PStmtData::Let { ty, value, patt } => DStmt::Let { ty, value, patt },
            PStmtData::Guard { cond, body } => DStmt::Guard { cond, body },
            PStmtData::Expr(expr) => DStmt::Expr(expr),
            PStmtData::Defer(expr) => DStmt::Defer(expr),
            PStmtData::Error => DStmt::None,
        }
    }

    fn declare_fn(&mut self, f: Fn) -> DFn {
        let span = f.name.span;
        if f.variadic && (!f.is_extern || f.body.is_some()) {
            self.error(Error::new("only imported extern functions may be variadic", span))
        }

        if f.is_extern && !f.type_params.is_empty() && f.body.is_some() {
            self.error(Error::new("generic functions cannot be declared 'extern'", span))
        }

        let id = self.insert::<FunctionId>(
            Function {
                public: f.public,
                attrs: f.attrs,
                name: f.name,
                is_extern: f.is_extern,
                is_async: f.is_async,
                is_unsafe: f.is_unsafe,
                variadic: f.variadic,
                assign_subscript: f.assign_subscript,
                has_body: f.body.is_some(),
                type_params: Vec::new(),
                params: Vec::new(),
                ret: TypeId::UNKNOWN,
                body: None,
                body_scope: ScopeId::ROOT,
                constructor: None,
            },
            f.public,
            true,
        );

        let mut allow_safe_extern = false;
        for attr in self.proj.scopes.get::<FunctionId>(id).attrs.clone().iter() {
            match attr.name.data {
                Strings::ATTR_INSTRINSIC => {
                    allow_safe_extern = true;
                    let (name, span) = if let Some(attr) = attr.props.first() {
                        (attr.name.data, attr.name.span)
                    } else {
                        (self.proj.scopes.get(id).name.data, attr.name.span)
                    };
                    if self.tables.intrinsics.contains(&name) {
                        self.proj.scopes.intrinsics.insert(id, name);
                    } else {
                        let name = strdata!(self, name);
                        self.error(Error::new(format!("intrinsic '{name}' is not supported"), span))
                    }
                }
                Strings::ATTR_SAFE => allow_safe_extern = true,
                _ => {}
            }
        }

        self.enter(ScopeKind::Function(id), |this| {
            if !allow_safe_extern && f.is_extern && f.body.is_none() {
                this.proj.scopes.get_mut(id).is_unsafe = true;
            }

            this.proj.scopes.get_mut(id).body_scope = this.current;
            this.proj.scopes.get_mut(id).type_params = this.declare_type_params(f.type_params);
            this.proj.scopes.get_mut(id).params = f
                .params
                .into_iter()
                .enumerate()
                .map(|(i, param)| CheckedParam {
                    keyword: param.keyword,
                    label: match &param.patt.data {
                        Pattern::MutBinding(name) => Some(*name),
                        Pattern::Path(name) => name.as_identifier(),
                        _ => None,
                    }
                    .unwrap_or_else(|| intern!(this, "$unnamed{i}")),
                    patt: ParamPattern::Unchecked(param.patt),
                    ty: this.declare_type_hint(param.ty),
                    default: param.default.map(|expr| DefaultExpr::Unchecked(this.current, expr)),
                })
                .collect();
            this.proj.scopes.get_mut(id).ret =
                f.ret.map(|ret| this.declare_type_hint(ret)).unwrap_or(TypeId::VOID);

            DFn { id, body: f.body }
        })
    }

    fn declare_fns(&mut self, fns: Vec<Located<Fn>>) -> Vec<DFn> {
        self.declare_fns_iter(fns).collect()
    }

    fn declare_fns_iter(&mut self, fns: Vec<Located<Fn>>) -> impl Iterator<Item = DFn> + use<'_> {
        fns.into_iter().flat_map(|f| {
            if !self.check_disabled(&f.data.attrs, f.span) {
                Some(self.declare_fn(f.data))
            } else {
                None
            }
        })
    }

    fn declare_op_fn(
        &mut self,
        f: Located<OperatorFn>,
        impls: &mut Vec<TraitImpl>,
        blocks: &mut Vec<DImplBlock>,
        subscripts: &mut Vec<DFn>,
    ) {
        if self.check_disabled(&f.data.attrs, f.span) {
            return;
        }

        let f = f.data;

        use OperatorFnType as O;
        let (tr_name, fn_name, ty_args) = match f.name.data {
            O::Plus
            | O::Mul
            | O::Div
            | O::Rem
            | O::BitAnd
            | O::BitOr
            | O::Xor
            | O::Shl
            | O::Shr
            | O::Cmp
            | O::Eq => {
                let op = BinaryOp::try_from(f.name.data).unwrap();
                let (tr_name, fn_name) = self.tables.binary_op_traits.get(&op).copied().unwrap();
                if let Some(p) =
                    f.params.get(1).filter(|_| matches!(f.name.data, O::Cmp | O::Eq)).cloned()
                {
                    if let TypeHint::Ptr(inner) = p.ty.data {
                        (tr_name, fn_name, vec![*inner])
                    } else {
                        // impl check will take care of issuing an error for this case
                        (tr_name, fn_name, vec![p.ty])
                    }
                } else {
                    (
                        tr_name,
                        fn_name,
                        vec![
                            f.params.get(1).map(|p| p.ty.clone()).unwrap_or_default(),
                            f.ret.clone().unwrap_or_else(|| Located::nowhere(TypeHint::Void)),
                        ],
                    )
                }
            }
            O::Minus if f.params.len() > 1 => {
                let op = BinaryOp::try_from(f.name.data).unwrap();
                let (tr_name, fn_name) = self.tables.binary_op_traits.get(&op).copied().unwrap();
                (
                    tr_name,
                    fn_name,
                    vec![
                        f.params.get(1).map(|p| p.ty.clone()).unwrap_or_default(),
                        f.ret.clone().unwrap_or_else(|| Located::nowhere(TypeHint::Void)),
                    ],
                )
            }
            O::Minus | O::Bang => {
                let op = UnaryOp::try_from_postfix_fn(f.name.data).unwrap();
                let (tr_name, fn_name) = self.tables.unary_op_traits.get(&op).copied().unwrap();
                (
                    tr_name,
                    fn_name,
                    vec![f.ret.clone().unwrap_or_else(|| Located::nowhere(TypeHint::Void))],
                )
            }
            O::Increment | O::Decrement => {
                let op = UnaryOp::try_from_postfix_fn(f.name.data).unwrap();
                let (tr_name, fn_name) = self.tables.unary_op_traits.get(&op).copied().unwrap();
                (tr_name, fn_name, vec![])
            }
            O::Subscript | O::SubscriptAssign => {
                let name = intern!(self, "$sub{}", subscripts.len());
                subscripts.push(self.declare_fn(Fn::from_operator_fn(name, f)));
                return;
            }
        };

        let span = f.name.span;
        let mut f = Fn::from_operator_fn(fn_name, f);
        let block = self.enter(ScopeKind::None, |this| DImplBlock {
            type_params: this.declare_type_params(std::mem::take(&mut f.type_params)),
            span: f.name.span,
            scope: this.current,
            fns: vec![this.declare_fn(f)],
        });
        self.proj.scopes[block.scope].kind = ScopeKind::Impl(impls.len());
        impls.push(TraitImpl::Unchecked {
            scope: block.scope,
            data: TraitImplData::Operator { tr: tr_name, ty_args, span },
        });
        blocks.push(block);
    }

    fn declare_type_params(&mut self, vec: TypeParams) -> Vec<UserTypeId> {
        vec.into_iter()
            .map(|(name, impls)| {
                self.insert(
                    UserType::template(
                        name,
                        self.current,
                        impls
                            .into_iter()
                            .map(|path| TraitImpl::Unchecked {
                                scope: self.current,
                                data: TraitImplData::Path(path),
                            })
                            .collect(),
                    ),
                    false,
                    false,
                )
            })
            .collect()
    }

    fn declare_impl_blocks(
        &mut self,
        blocks: Vec<Located<ImplBlock>>,
        operators: Vec<Located<OperatorFn>>,
    ) -> (Vec<TraitImpl>, Vec<DImplBlock>, Vec<DFn>) {
        let mut impls = Vec::new();
        let mut declared_blocks = Vec::new();
        let mut subscripts = Vec::new();
        for block in blocks {
            let ImplBlock { path, functions, type_params, attrs } = block.data;
            if self.check_disabled(&attrs, block.span) {
                continue;
            }

            let block = self.enter(ScopeKind::None, |this| DImplBlock {
                type_params: this.declare_type_params(type_params),
                span: path.final_component_span(),
                scope: this.current,
                fns: this.declare_fns(functions),
            });
            self.proj.scopes[block.scope].kind = ScopeKind::Impl(impls.len());
            impls
                .push(TraitImpl::Unchecked { scope: block.scope, data: TraitImplData::Path(path) });
            declared_blocks.push(block);
        }

        for func in operators {
            self.declare_op_fn(func, &mut impls, &mut declared_blocks, &mut subscripts);
        }

        (impls, declared_blocks, subscripts)
    }

    fn declare_type_hint(&mut self, hint: Located<TypeHint>) -> TypeId {
        self.proj.types.add_unresolved(hint, self.current)
    }

    fn typehint_for_struct(
        name: &Located<StrId>,
        type_params: &[(Located<StrId>, Vec<Path>)],
    ) -> Located<TypeHint> {
        Located::new(
            name.span,
            TypeHint::Regular(Path::new(
                PathOrigin::Normal,
                vec![(
                    *name,
                    type_params
                        .iter()
                        .map(|(n, _)| Located::new(n.span, TypeHint::Regular(Path::from(*n))))
                        .collect(),
                )],
            )),
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn ut_from_stuff(
        &mut self,
        attrs: Attributes,
        name: Located<StrId>,
        public: bool,
        members: IndexMap<StrId, CheckedMember>,
        kind: UserTypeKind,
        type_params: TypeParams,
        fns: &[DFn],
        impls: Vec<TraitImpl>,
        impl_blocks: &[DImplBlock],
        subscripts: &[DFn],
    ) -> UserType {
        UserType {
            attrs,
            name,
            public,
            kind,
            impls,
            impl_blocks: impl_blocks
                .iter()
                .map(|i| ImplBlockData { type_params: i.type_params.clone() })
                .collect(),
            members,
            type_params: self.declare_type_params(type_params),
            body_scope: self.current,
            fns: fns
                .iter()
                .chain(impl_blocks.iter().flat_map(|block| block.fns.iter()))
                .map(|f| Vis::new(f.id, self.proj.scopes.get(f.id).public))
                .collect(),
            subscripts: subscripts.iter().map(|s| s.id).collect(),
            members_resolved: false,
            recursive: false,
        }
    }

    fn check_disabled(&mut self, attrs: &Attributes, span: Span) -> bool {
        if self.proj.conf.is_disabled_by_attrs(attrs) {
            self.proj.diag.add_inactive(span);
            return true;
        }

        false
    }
}

/// Typechecking pass routines
impl TypeChecker {
    fn enter_id<T>(&mut self, id: ScopeId, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.current;
        self.current = id;
        let result = f(self);
        self.current = prev;
        result
    }

    fn enter_id_and_resolve<T>(&mut self, id: ScopeId, f: impl FnOnce(&mut Self) -> T) -> T {
        self.enter_id(id, |this| {
            for id in this.proj.scopes.walk(this.current).map(|(id, _)| id).collect::<Vec<_>>() {
                this.enter_id(id, |this| {
                    for stmt in std::mem::take(&mut this.proj.scopes[this.current].use_stmts) {
                        if let Err(err) = this.resolve_use(&stmt, false) {
                            named_error!(this, Error::no_symbol, err.data, err.span)
                        }
                    }
                });
            }

            f(this)
        })
    }

    fn check_stmt(&mut self, stmt: DStmt) -> CStmt {
        match stmt {
            DStmt::Module { id, body } => {
                self.enter_id_and_resolve(id, |this| {
                    for stmt in body {
                        this.check_stmt(stmt);
                    }
                });
            }
            DStmt::ModuleOOL { name } => {
                let item = self.scopes()[self.current].find_in_tns(name.data);
                if let Some(scope) = item.and_then(|item| item.id.into_module().ok()) {
                    self.check_hover(name.span, LspItem::Module(scope));
                }
            }
            DStmt::Struct { init, id, impls, fns } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.resolve_members(id);
                    this.check_fn(init);
                    let this_ty = Type::User(GenericUserType::from_id(
                        &this.proj.scopes,
                        &mut this.proj.types,
                        id,
                    ));
                    let this_ty = this.proj.types.insert(this_ty);
                    this.resolve_dependencies(id, this_ty, true);
                    this.check_impl_blocks(this_ty, id, impls);

                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DStmt::Union { id, impls, fns } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.resolve_members(id);
                    let this_ty = Type::User(GenericUserType::from_id(
                        &this.proj.scopes,
                        &mut this.proj.types,
                        id,
                    ));
                    let this_ty = this.proj.types.insert(this_ty);
                    this.resolve_dependencies(id, this_ty, true);
                    this.check_impl_blocks(this_ty, id, impls);

                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DStmt::Trait { id, fns } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    // TODO: disable errors so this doesn't cause duplicate errors
                    this.resolve_impls(*this.proj.scopes.get(id).kind.as_trait().unwrap().0);
                    if this.check_trait_dependencies(id) {
                        // FIXME: this error also shows for traits that have a recursive trait in
                        // their impls
                        this.proj
                            .diag
                            .report(Error::recursive_trait(this.proj.scopes.get(id).name.span));
                    }
                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DStmt::Extension { id, impls, fns } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    let ty = resolve_type!(
                        this,
                        *this.proj.scopes.get_mut(id).kind.as_extension_mut().unwrap()
                    );
                    this.resolve_impls(id);
                    this.check_impl_blocks(ty, id, impls);
                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DStmt::Expr(expr) => return CStmt::Expr(self.check_expr(expr, None)),
            DStmt::Let { ty, value, patt } => {
                let span = patt.span;
                if let Some(ty) = ty {
                    let ty = self.resolve_typehint(&ty);
                    if let Some(value) = value {
                        let value = self.type_check(value, ty);
                        let patt = self.check_pattern(PatternParams {
                            scrutinee: ty,
                            mutable: false,
                            pattern: patt,
                            typ: PatternType::Regular,
                            has_hint: true,
                        });
                        if !patt.irrefutable {
                            return self
                                .error(Error::must_be_irrefutable("let binding pattern", span));
                        }
                        return CStmt::Let(patt, Some(value));
                    } else {
                        let patt = self.check_pattern(PatternParams {
                            scrutinee: ty,
                            mutable: false,
                            pattern: patt,
                            typ: PatternType::Regular,
                            has_hint: true,
                        });
                        if !patt.irrefutable {
                            return self
                                .error(Error::must_be_irrefutable("let binding pattern", span));
                        }
                        if !matches!(patt.data, PatternData::Variable(_)) {
                            return self.error(Error::new(
                                "must provide a value with a destructuring assignment",
                                span,
                            ));
                        }
                        return CStmt::Let(patt, None);
                    }
                } else if let Some(value) = value {
                    let span = patt.span;
                    let value = self.check_expr(value, None);
                    let patt = self.check_pattern(PatternParams {
                        scrutinee: value.ty,
                        mutable: false,
                        pattern: patt,
                        typ: PatternType::Regular,
                        has_hint: false,
                    });
                    if !patt.irrefutable {
                        return self.error(Error::must_be_irrefutable("let binding pattern", span));
                    }

                    return CStmt::Let(patt, Some(value));
                } else {
                    return self.error(Error::new("cannot infer type", patt.span));
                }
            }
            DStmt::Defer(expr) => {
                return CStmt::Defer(
                    self.enter(ScopeKind::Defer, |this| this.check_expr(expr, None)),
                );
            }
            DStmt::Guard { cond, body } => {
                let (cond, vars) = self.type_check_with_listen(cond);
                let span = body.span;
                let body = self.check_expr_inner(body, Some(TypeId::NEVER));
                let body = self.type_check_checked(body, TypeId::NEVER, span);
                self.define(&vars);
                return CStmt::Guard { cond, body };
            }
            DStmt::Fn(f) => self.check_fn(f),
            DStmt::Binding { id, value, constant } => {
                // TODO: constants
                let _ = constant;
                // FIXME: detect cycles like static X: usize = X;
                // FIXME: non-const statics should be disallowed
                let ty = resolve_type!(self, self.proj.scopes.get_mut(id).ty);

                self.proj.static_deps.insert(id, Dependencies::Resolving);
                let prev = self.current_static.replace((id, Vec::new()));
                let value = value.map(|value| {
                    self.enter(ScopeKind::Static(id), |this| this.type_check(value, ty))
                });

                let (_, deps) = std::mem::replace(&mut self.current_static, prev).unwrap();
                self.proj.static_deps.insert(id, Dependencies::Resolved(deps));

                let var = self.proj.scopes.get_mut(id);
                if value.is_none() && var.is_static && !var.is_extern {
                    self.proj.diag.report(Error::new(
                        "non-extern static variable must be initialized",
                        var.name.span,
                    ))
                }

                // if value.is_none() && constant {
                //     self.proj
                //         .diag
                //         .error(Error::new("constant must be initialized", var.name.span))
                // }

                var.value = value;
            }
            DStmt::None => {}
        }

        CStmt::None
    }

    fn check_signature_match(
        &mut self,
        tr: Option<(TraitId, TypeId)>,
        has: FunctionId,
        wants: FunctionId,
        ty_args: &TypeArgs,
    ) -> Result<(), String> {
        let hfn = self.proj.scopes.get(has);
        let wfn = self.proj.scopes.get(wants);
        if wfn.is_unsafe && !hfn.is_unsafe {
            return Err(format!(
                "function '{}' must be declared unsafe",
                strdata!(self, hfn.name.data)
            ));
        }

        let mut ty_args = ty_args.clone();
        for (i, &id) in wfn.type_params.iter().enumerate() {
            if let Some(&ty) = hfn.type_params.get(i) {
                let ty = Type::User(GenericUserType::from_id(
                    &self.proj.scopes,
                    &mut self.proj.types,
                    ty,
                ));
                ty_args.insert(id, self.proj.types.insert(ty));
            } else {
                ty_args.insert(id, TypeId::UNKNOWN);
            }
        }

        if let Some((tr, this)) = tr {
            ty_args.insert(*self.proj.scopes.get(tr).kind.as_trait().unwrap().0, this);
        }

        let mut compare_types = |has: TypeId, wants: TypeId| {
            let wants = wants.with_templates(&mut self.proj.types, &ty_args);
            if has != wants {
                Err(format!(
                    "expected '{}', found '{}'",
                    type_name!(self, wants),
                    type_name!(self, has),
                ))
            } else {
                Ok(())
            }
        };

        if let Err(err) = compare_types(hfn.ret, wfn.ret) {
            return Err(format!("return type is incorrect: {err}"));
        }

        for (s, t) in hfn.params.iter().zip(wfn.params.iter().cloned()) {
            if let Err(err) = compare_types(s.ty, t.ty) {
                let label = strdata!(self, t.label);
                return Err(format!("parameter '{label}' is incorrect: {err}"));
            }
        }

        for (&s, &t) in hfn.type_params.iter().zip(wfn.type_params.iter()) {
            let s = self.proj.scopes.get(s);
            let t = self.proj.scopes.get(t);
            let name = t.name.data;
            // TODO: dont enfore impl order
            for (s, t) in s
                .impls
                .iter()
                .flat_map(|tr| tr.as_checked())
                .zip(t.impls.iter().flat_map(|tr| tr.as_checked()))
            {
                for (&s, &t) in s.ty_args.values().zip(t.ty_args.values()) {
                    if let Err(err) = compare_types(s, t) {
                        let data = strdata!(self, name);
                        return Err(format!("type parameter '{data}' is incorrect: {err}"));
                    }
                }
            }

            if s.impls.len() != t.impls.len() {
                return Err(format!("type parameter '{}' is incorrect", strdata!(self, name)));
            }
        }

        if hfn.params.len() != wfn.params.len() {
            return Err(format!(
                "expected {} parameter(s), got {}",
                wfn.params.len(),
                hfn.params.len(),
            ));
        }

        if hfn.type_params.len() != wfn.type_params.len() {
            return Err(format!(
                "expected {} type parameter(s), got {}",
                wfn.type_params.len(),
                hfn.type_params.len(),
            ));
        }

        Ok(())
    }

    fn check_impl_block(&mut self, this: TypeId, tr: &GenericTrait, block: DImplBlock) {
        for id in block.type_params {
            self.resolve_impls(id);
        }

        let tr_ut = self.proj.scopes.get(tr.id);
        let (&this_id, &sealed) = tr_ut.kind.as_trait().unwrap();
        if sealed && !self.can_access_privates(tr_ut.scope) {
            self.error(Error::new(
                format!("cannot implement sealed trait '{}'", strdata!(self, tr_ut.name.data)),
                block.span,
            ))
        }

        for mut dep in
            self.proj.scopes.get(tr.id).impls.clone().into_iter().flat_map(|tr| tr.into_checked())
        {
            for ty_arg in dep.ty_args.values_mut() {
                if self.proj.types[*ty_arg].as_user().is_some_and(|ut| ut.id == this_id) {
                    *ty_arg = this;
                }
            }

            if !self.implements_trait(this, &dep) {
                self.proj.diag.report(Error::new(
                    format!(
                        "trait '{}' requires implementation of trait '{}'",
                        type_name!(self, tr),
                        type_name!(self, dep),
                    ),
                    block.span,
                ));
            }
        }

        let mut required = self.proj.scopes.get(tr.id).fns.clone();
        for f in block.fns {
            let fn_name = self.proj.scopes.get(f.id).name;
            let lhs = f.id;

            self.check_fn(f);
            let Some(pos) =
                required.iter().position(|&id| self.proj.scopes.get(*id).name.data == fn_name.data)
            else {
                self.proj.diag.report(Error::new(
                    format!(
                        "no function '{}' found in trait '{}'",
                        strdata!(self, fn_name.data),
                        strdata!(self, self.proj.scopes.get(tr.id).name.data),
                    ),
                    fn_name.span,
                ));
                continue;
            };

            let rhs = *required.swap_remove(pos);
            self.resolve_proto(rhs);
            let this = Some((tr.id, this));
            if let Err(why) = self.check_signature_match(this, lhs, rhs, &tr.ty_args) {
                let data = strdata!(self, fn_name.data);
                self.error(Error::invalid_impl(data, &why, fn_name.span))
            }
        }

        for id in required {
            if !self.proj.scopes.get(*id).has_body {
                let tr = type_name!(self, tr);
                self.error(Error::new(
                    format!(
                        "must implement '{tr}::{}'",
                        strdata!(self, self.proj.scopes.get(*id).name.data)
                    ),
                    block.span,
                ))
            }
        }
    }

    fn check_fn(&mut self, DFn { id, body, .. }: DFn) {
        // TODO: disallow private type in public interface
        self.enter_id_and_resolve(self.proj.scopes.get(id).body_scope, |this| {
            this.resolve_proto(id);
            for i in 0..this.proj.scopes.get(id).params.len() {
                let Some(patt) = this.proj.scopes.get_mut(id).params[i].patt.as_unchecked().cloned() else {
                    continue;
                };
                let ty = this.proj.scopes.get(id).params[i].ty;
                let span = patt.span;
                let patt = this.check_pattern(PatternParams {
                    scrutinee: ty,
                    mutable: false,
                    pattern: patt,
                    typ: if body.is_none() { PatternType::BodylessFn } else { PatternType::Fn },
                    has_hint: true,
                });
                if !patt.irrefutable {
                    this.error(Error::must_be_irrefutable("parameter patterns", span))
                } else {
                    this.proj.scopes.get_mut(id).params[i].patt = ParamPattern::Checked(patt);
                }
            }

            let func = this.proj.scopes.get(id);
            if func.attrs.has(Strings::ATTR_PANIC_HANDLER) {
                if body.is_none() {
                    this.proj.diag.report(Error::new("panic handler must have a definition", func.name.span));
                }

                if let Some(_old) = this.proj.panic_handler.replace(id) {
                    // TODO: report that it was previously defined at the span of _old
                    this.proj.diag.report(Error::new("a panic handler already exists", func.name.span));
                }

                if let Some((&panic, _)) = this.scopes().intrinsics.iter().find(|(_, v)| strdata!(this, v) == "panic") {
                    let fn_name = func.name;
                    this.resolve_proto(panic);
                    if let Err(why) = this.check_signature_match(None, id, panic, &TypeArgs::default()) {
                        let data = strdata!(this, fn_name.data);
                        this.proj.diag.report(Error::invalid_impl(data, &why, fn_name.span))
                    }
                }
            }

            let func = this.proj.scopes.get(id);
            if let Some(ut_id) = func.constructor {
                let args = func
                    .params
                    .iter()
                    .flat_map(|param| Some((param.label, CExpr::new(
                        param.ty,
                        CExprData::Var(*param.patt.as_checked().and_then(|p| p.data.as_variable())?)
                    ))))
                    .collect();
                let variant = func.name.data;
                let ut = Type::User(GenericUserType::from_id(&this.proj.scopes, &mut this.proj.types, ut_id));
                this.proj.scopes.get_mut(id).body = Some(CExpr::new(
                    this.proj.types.insert(ut),
                    if this.proj.scopes.get(ut_id).kind.is_union() {
                        CExprData::VariantInstance(variant, args)
                    } else {
                        CExprData::Instance(args)
                    },
                ));
                return;
            }

            let Some(body) = body else {
                return;
            };

            let old_safety = std::mem::take(&mut this.safety);
            let ret = this.proj.scopes.get_mut(id).ret;
            let body_span = body.span;
            let body = this.check_expr(body, Some(ret));
            this.proj.scopes.get_mut(id).body = Some(match this.coerce(body, ret) {
                Ok(body) => body,
                Err(body) => {
                    match body.data.is_yielding_block(&this.proj.scopes) {
                        // Yielding blocks already perform a type_check
                        Some(true) => {}
                        Some(false) => {
                            let func = this.proj.scopes.get(id);
                            this.proj.diag.report(Error::new(
                                format!("function '{}' must return a value of type '{}' from all code paths",
                                    strdata!(this, func.name.data),
                                    type_name!(this, func.ret),
                                ),
                                func.name.span,
                            ));
                        }
                        None => {
                            this.proj.diag.report(type_mismatch_err!(this, ret, body.ty, body_span));
                        }
                    }
                    body
                }
            });
            this.safety = old_safety;
        });
    }

    fn check_impl_blocks(&mut self, this_ty: TypeId, id: UserTypeId, impls: Vec<DImplBlock>) {
        let mut seen = HashSet::new();
        for (i, block) in impls.into_iter().enumerate() {
            self.enter_id(block.scope, |this| {
                if let Some(gtr) = this.proj.scopes.get(id).impls[i].as_checked().cloned() {
                    if !seen.insert(gtr.clone()) {
                        this.proj.diag.report(Error::new(
                            format!("duplicate implementation of trait {}", type_name!(this, gtr),),
                            block.span,
                        ))
                    }

                    this.check_impl_block(this_ty, &gtr, block);
                } else {
                    for f in block.fns {
                        this.check_fn(f);
                    }
                }
            });
        }
    }

    fn check_binary(
        &mut self,
        lhs_span: Span,
        lhs: CExpr,
        rhs: PExpr,
        op: BinaryOp,
        span: Span,
    ) -> CExpr {
        let Some(&(trait_name, fn_name)) = self.tables.binary_op_traits.get(&op) else {
            bail!(self, Error::invalid_operator(op, &type_name!(self, lhs.ty), span,));
        };

        let Some(tr_id) = self.proj.scopes.lang_types.get(&trait_name).copied() else {
            return named_error!(self, Error::no_lang_item, trait_name, lhs_span);
        };

        let stripped = lhs.ty.strip_references(&self.proj.types);
        let Some(mut mfn) = self.get_member_fn_legacy(stripped, tr_id, fn_name, self.current)
        else {
            bail!(
                self,
                Error::doesnt_implement(
                    &type_name!(self, stripped),
                    strdata!(self, self.proj.scopes.get(tr_id).name.data),
                    lhs_span,
                )
            );
        };

        let f = self.proj.scopes.get(mfn.func.id);
        let [p0, p1, ..] = &f.params[..] else {
            return Default::default();
        };
        let arg0 = (p0.label, lhs.auto_deref(&mut self.proj.types, p0.ty));
        let p1_ty = p1.ty.with_templates(&mut self.proj.types, &mfn.func.ty_args);
        let ret = f.ret.with_templates(&mut self.proj.types, &mfn.func.ty_args);
        let rhs_span = rhs.span;
        let rhs_name = p1.label;
        let rhs = self.check_expr(rhs, Some(p1_ty.strip_references(&self.proj.types)));
        let rhs = rhs.auto_deref(&mut self.proj.types, p1_ty);
        let arg0val = (rhs_name, self.type_check_checked(rhs, p1_ty, rhs_span));
        self.trait_hack(&mut mfn, p1_ty.strip_references(&self.proj.types));

        CExpr::new(
            ret,
            CExprData::member_call(
                &mut self.proj.types,
                mfn,
                [arg0, arg0val].into(),
                self.current,
                span,
            ),
        )
    }

    fn check_unary(&mut self, expr: CExpr, op: UnaryOp, span: Span) -> CExpr {
        let Some(&(trait_name, fn_name)) = self.tables.unary_op_traits.get(&op) else {
            bail!(self, Error::invalid_operator(op, &type_name!(self, expr.ty), span,));
        };

        let Some(tr_id) = self.proj.scopes.lang_types.get(&trait_name).copied() else {
            return named_error!(self, Error::no_lang_item, trait_name, span);
        };

        let stripped = expr.ty.strip_references(&self.proj.types);
        let Some(mfn) = self.get_member_fn_legacy(stripped, tr_id, fn_name, self.current) else {
            bail!(
                self,
                Error::doesnt_implement(
                    &type_name!(self, stripped),
                    strdata!(self, self.proj.scopes.get(tr_id).name.data),
                    span,
                )
            );
        };

        let f = self.proj.scopes.get(mfn.func.id);
        let [p0, ..] = &f.params[..] else {
            return Default::default();
        };
        if matches!(
            op,
            UnaryOp::PreDecrement
                | UnaryOp::PreIncrement
                | UnaryOp::PostDecrement
                | UnaryOp::PostIncrement
        ) {
            CExpr::new(
                stripped,
                CExprData::AffixOperator {
                    callee: expr.auto_deref(&mut self.proj.types, p0.ty).into(),
                    mfn,
                    param: p0.label,
                    scope: self.current,
                    postfix: matches!(op, UnaryOp::PostDecrement | UnaryOp::PostIncrement),
                    span, // TODO: use the span of the operator itself
                },
            )
        } else {
            let arg0 = expr.auto_deref(&mut self.proj.types, p0.ty);
            CExpr::new(
                f.ret.with_templates(&mut self.proj.types, &mfn.func.ty_args),
                CExprData::member_call(
                    &mut self.proj.types,
                    mfn,
                    [(p0.label, arg0)].into(),
                    self.current,
                    span,
                ),
            )
        }
    }

    fn check_expr_inner(&mut self, expr: PExpr, target: Option<TypeId>) -> CExpr {
        // FIXME: this should just be a parameter to this function
        self.current_expr += 1;
        let span = expr.span;
        match expr.data {
            PExprData::Binary { op, left, right } => {
                let left_span = left.span;
                let assignment = op.is_assignment();
                match op {
                    BinaryOp::Assign => {
                        if let PExprData::Subscript { callee, mut args } = left.data {
                            if args.is_empty() {
                                return self.error(Error::new(
                                    "subscript requires at least one argument",
                                    span,
                                ));
                            }

                            let callee = self.check_expr(*callee, None);
                            let stripped = callee.ty.strip_references(&self.proj.types);
                            if let &Type::Array(inner, _) = &self.proj.types[stripped] {
                                let left = self.check_array_subscript(inner, callee, args);
                                if op.is_assignment()
                                    && !left.is_assignable(&self.proj.scopes, &self.proj.types)
                                {
                                    // TODO: report a better error here
                                    self.error(Error::new(
                                        "expression is not assignable",
                                        left_span,
                                    ))
                                }

                                let right = self.type_check(*right, left.ty);
                                return CExpr::new(
                                    TypeId::VOID,
                                    CExprData::Binary(BinaryOp::Assign, left.into(), right.into()),
                                );
                            } else {
                                args.push((None, *right));
                                return self
                                    .check_subscript(callee, stripped, args, target, true, span);
                            }
                        }
                    }
                    BinaryOp::NoneCoalesce | BinaryOp::NoneCoalesceAssign => {
                        let Some(id) = self.proj.scopes.get_option_id() else {
                            return self.error(Error::no_lang_item("option", span));
                        };

                        let target = if let Some(target) = target {
                            let ut =
                                GenericUserType::from_type_args(&self.proj.scopes, id, [target]);
                            Some(self.proj.types.insert(Type::User(ut)))
                        } else {
                            None
                        };
                        let lhs_span = left.span;
                        let lhs = self.check_expr(*left, target);
                        let Some(target) = lhs.ty.as_option_inner(&self.proj) else {
                            if lhs.ty != TypeId::UNKNOWN {
                                self.proj.diag.report(Error::invalid_operator(
                                    op,
                                    &type_name!(self, lhs.ty),
                                    lhs_span,
                                ));
                            }
                            return Default::default();
                        };
                        if assignment && !lhs.is_assignable(&self.proj.scopes, &self.proj.types) {
                            // TODO: report a better error here
                            self.error(Error::new("expression is not assignable", lhs_span))
                        }

                        let span = right.span;
                        let rhs = self.check_expr_inner(*right, Some(target));
                        let rhs = self.type_check_checked(rhs, target, span);
                        return CExpr::new(
                            if assignment { TypeId::VOID } else { target },
                            CExprData::Binary(op, lhs.into(), rhs.into()),
                        );
                    }
                    BinaryOp::LogicalAnd => {
                        let was_listening = self.listening_expr == self.current_expr;
                        let (left, lvars) = self.type_check_with_listen(*left);
                        let (right, rvars) = self.enter(ScopeKind::None, |this| {
                            this.define(&lvars);
                            this.type_check_with_listen(*right)
                        });
                        if was_listening {
                            self.listening_vars.extend(lvars);
                            self.listening_vars.extend(rvars);
                        }

                        return CExpr::new(
                            TypeId::BOOL,
                            CExprData::Binary(op, left.into(), right.into()),
                        );
                    }
                    _ => {}
                }

                let left = self.check_expr(*left, target);
                if left.ty == TypeId::UNKNOWN {
                    self.check_expr(*right, target);
                    return Default::default();
                }

                if assignment && !left.is_assignable(&self.proj.scopes, &self.proj.types) {
                    // TODO: report a better error here
                    self.error(Error::new("expression is not assignable", left_span))
                }

                if op != BinaryOp::Assign && !left.ty.supports_binary(&self.proj.types, op) {
                    return self.check_binary(left_span, left, *right, op, span);
                }

                match (&self.proj.types[left.ty], op) {
                    (Type::RawPtr(_) | Type::RawMutPtr(_), BinaryOp::Sub) => {
                        let span = right.span;
                        let right = self.check_expr(*right, Some(TypeId::ISIZE));
                        let right = self.try_coerce(right, TypeId::ISIZE);
                        if right.ty == left.ty {
                            CExpr::new(
                                TypeId::ISIZE,
                                CExprData::Binary(op, left.into(), right.into()),
                            )
                        } else if self.proj.types[right.ty].is_integral()
                            || right.ty == TypeId::UNKNOWN
                        {
                            CExpr::new(left.ty, CExprData::Binary(op, left.into(), right.into()))
                        } else {
                            self.proj.diag.report(Error::type_mismatch_s(
                                "{integer}",
                                &type_name!(self, right.ty),
                                span,
                            ));
                            Default::default()
                        }
                    }
                    (
                        Type::RawPtr(_) | Type::RawMutPtr(_),
                        BinaryOp::Add | BinaryOp::AddAssign | BinaryOp::SubAssign,
                    ) => {
                        let span = right.span;
                        let right = self.check_expr(*right, Some(TypeId::USIZE));
                        let right = self.try_coerce(right, TypeId::USIZE);
                        if !self.proj.types[right.ty].is_integral() && right.ty != TypeId::UNKNOWN {
                            self.proj.diag.report(Error::type_mismatch_s(
                                "{integer}",
                                &type_name!(self, right.ty),
                                span,
                            ));
                        }
                        CExpr::new(
                            if assignment { TypeId::VOID } else { left.ty },
                            CExprData::Binary(op, left.into(), right.into()),
                        )
                    }
                    (
                        Type::Int(_)
                        | Type::Uint(_)
                        | Type::CInt(_)
                        | Type::CUint(_)
                        | Type::Isize
                        | Type::Usize,
                        BinaryOp::Shl | BinaryOp::Shr | BinaryOp::ShlAssign | BinaryOp::ShrAssign,
                    ) => {
                        let right = self.check_expr(*right, Some(TypeId::U32));
                        let right = self.try_coerce(right, TypeId::U32);
                        if self.proj.types[right.ty].as_integral(false).is_none_or(|v| v.signed)
                            && right.ty != TypeId::UNKNOWN
                        {
                            self.proj.diag.report(Error::type_mismatch_s(
                                "{unsigned}",
                                &type_name!(self, right.ty),
                                span,
                            ));
                        }
                        CExpr::new(
                            if assignment { TypeId::VOID } else { left.ty },
                            CExprData::Binary(op, left.into(), right.into()),
                        )
                    }
                    _ => {
                        let right = self.type_check(*right, left.ty);
                        CExpr::new(
                            match op {
                                BinaryOp::NoneCoalesce => unreachable!(),
                                BinaryOp::Cmp => self.make_lang_type_by_name("ordering", [], span),
                                BinaryOp::Gt
                                | BinaryOp::GtEqual
                                | BinaryOp::Lt
                                | BinaryOp::LtEqual
                                | BinaryOp::Equal
                                | BinaryOp::NotEqual
                                | BinaryOp::LogicalOr
                                | BinaryOp::LogicalAnd => TypeId::BOOL,
                                op if op.is_assignment() => TypeId::VOID,
                                _ => left.ty,
                            },
                            CExprData::Binary(op, left.into(), right.into()),
                        )
                    }
                }
            }
            PExprData::Unary { op, expr } => {
                let (out_ty, expr) = match op {
                    UnaryOp::Deref => {
                        let expr = if let Some(target) = target {
                            let ty = self.proj.types.insert(Type::Ptr(target));
                            self.check_expr(*expr, Some(ty))
                        } else {
                            self.check_expr(*expr, target)
                        };

                        match self.proj.types[expr.ty] {
                            Type::Ptr(inner) | Type::MutPtr(inner) => (inner, expr),
                            Type::RawPtr(inner) | Type::RawMutPtr(inner) => {
                                check_unsafe!(self, Error::is_unsafe(span));
                                (inner, expr)
                            }
                            Type::Unknown => return Default::default(),
                            _ => {
                                bail!(
                                    self,
                                    Error::invalid_operator(op, &type_name!(self, expr.ty), span,)
                                )
                            }
                        }
                    }
                    UnaryOp::Addr => {
                        let expr = self.check_expr(
                            *expr,
                            target.and_then(|id| id.as_pointee(&self.proj.types)),
                        );
                        match &expr.data {
                            CExprData::Call { callee: inner, .. } => {
                                if matches!(&inner.data, CExprData::Fn(f, _)
                                    if self.is_subscript(f.id))
                                {
                                    self.proj.diag.report(Warning::subscript_addr(span));
                                }
                            }
                            CExprData::Member { source, .. } => {
                                if source.ty.is_packed_struct(&self.proj) {
                                    self.proj.diag.report(Warning::bitfield_addr(span));
                                }
                            }
                            CExprData::Fn(_, _) | CExprData::MemFn(_, _) => {
                                let Type::Fn(f) = &self.proj.types[expr.ty] else { unreachable!() };
                                let f = f.clone();
                                let fptr = Type::FnPtr(
                                    f.as_fn_ptr(&self.proj.scopes, &mut self.proj.types),
                                );
                                return CExpr::new(self.proj.types.insert(fptr), expr.data);
                            }
                            _ => {}
                        }
                        (self.proj.types.insert(Type::Ptr(expr.ty)), expr)
                    }
                    UnaryOp::AddrMut => {
                        let expr = self.check_expr(
                            *expr,
                            target.and_then(|id| id.as_pointee(&self.proj.types)),
                        );
                        if !expr.can_addrmut(&self.proj.scopes, &self.proj.types) {
                            self.error(Error::new(
                                "cannot create mutable pointer to immutable memory location",
                                span,
                            ))
                        }
                        match &expr.data {
                            CExprData::Call { callee: inner, .. } => {
                                if matches!(&inner.data, CExprData::Fn(f, _)
                                    if self.is_subscript(f.id))
                                {
                                    self.proj.diag.report(Warning::subscript_addr(span));
                                }
                            }
                            CExprData::Member { source, .. } => {
                                if source.ty.is_packed_struct(&self.proj) {
                                    self.proj.diag.report(Warning::bitfield_addr(span));
                                }
                            }
                            CExprData::Fn(_, _) | CExprData::MemFn(_, _) => {
                                self.proj.diag.report(Warning::mut_function_ptr(span));

                                let Type::Fn(f) = &self.proj.types[expr.ty] else { unreachable!() };
                                let f = f.clone();
                                let fptr = Type::FnPtr(
                                    f.as_fn_ptr(&self.proj.scopes, &mut self.proj.types),
                                );
                                return CExpr::new(self.proj.types.insert(fptr), expr.data);
                            }
                            _ => {}
                        }
                        (self.proj.types.insert(Type::MutPtr(expr.ty)), expr)
                    }
                    UnaryOp::AddrRaw => {
                        let expr = self.check_expr(
                            *expr,
                            target.and_then(|id| id.as_pointee(&self.proj.types)),
                        );
                        match &expr.data {
                            CExprData::Call { callee: inner, .. } => {
                                if matches!(&inner.data, CExprData::Fn(f, _)
                                    if self.is_subscript(f.id))
                                {
                                    self.proj.diag.report(Warning::subscript_addr(span));
                                }
                            }
                            CExprData::Member { source, .. } => {
                                if source.ty.is_packed_struct(&self.proj) {
                                    self.proj.diag.report(Warning::bitfield_addr(span));
                                }
                            }
                            CExprData::Fn(_, _) | CExprData::MemFn(_, _) => self
                                .error(Error::new("cannot create raw pointer to function", span)),
                            _ => {}
                        }
                        (self.proj.types.insert(Type::RawPtr(expr.ty)), expr)
                    }
                    UnaryOp::AddrRawMut => {
                        let expr = self.check_expr(
                            *expr,
                            target.and_then(|id| id.as_pointee(&self.proj.types)),
                        );
                        if !expr.can_addrmut(&self.proj.scopes, &self.proj.types) {
                            self.error(Error::new(
                                "cannot create mutable pointer to immutable memory location",
                                span,
                            ))
                        }
                        match &expr.data {
                            CExprData::Call { callee: inner, .. } => {
                                if matches!(&inner.data, CExprData::Fn(f, _)
                                    if self.is_subscript(f.id))
                                {
                                    self.proj.diag.report(Warning::subscript_addr(span));
                                }
                            }
                            CExprData::Member { source, .. } => {
                                if source.ty.is_packed_struct(&self.proj) {
                                    self.proj.diag.report(Warning::bitfield_addr(span));
                                }
                            }
                            CExprData::Fn(_, _) | CExprData::MemFn(_, _) => self
                                .error(Error::new("cannot create raw pointer to function", span)),
                            _ => {}
                        }
                        (self.proj.types.insert(Type::RawMutPtr(expr.ty)), expr)
                    }
                    UnaryOp::Try => {
                        let expr = self
                            .check_expr(*expr, target.and_then(|t| t.as_option_inner(&self.proj)));
                        if let Some(inner) = expr.ty.as_option_inner(&self.proj) {
                            // TODO: lambdas
                            if self
                                .current_function()
                                .and_then(|id| {
                                    self.proj.scopes.get(id).ret.as_option_inner(&self.proj)
                                })
                                .is_none()
                            {
                                self.error(Error::new(
                                    "operator '?' is only valid in functions that return Option",
                                    span,
                                ))
                            }

                            (inner, expr)
                        } else if expr.ty == TypeId::UNKNOWN {
                            return Default::default();
                        } else {
                            bail!(
                                self,
                                Error::invalid_operator(op, &type_name!(self, expr.ty), span,)
                            );
                        }
                    }
                    UnaryOp::Option => {
                        let expr = self
                            .check_expr(*expr, target.and_then(|t| t.as_option_inner(&self.proj)));
                        let ty = self.make_lang_type_by_name("option", [expr.ty], span);
                        (ty, self.try_coerce(expr, ty))
                    }
                    _ => {
                        let span = expr.span;
                        let expr = self.check_expr(*expr, target);
                        if !expr.ty.supports_unary(&self.proj.scopes, &self.proj.types, op) {
                            return self.check_unary(expr, op, span);
                        }

                        if matches!(
                            op,
                            UnaryOp::PostIncrement
                                | UnaryOp::PostDecrement
                                | UnaryOp::PreIncrement
                                | UnaryOp::PreDecrement
                        ) && !expr.is_assignable(&self.proj.scopes, &self.proj.types)
                        {
                            self.error(Error::new("expression is not assignable", span))
                        }

                        (expr.ty, expr)
                    }
                };

                CExpr::new(out_ty, CExprData::Unary(op, expr.into()))
            }
            PExprData::Call { callee, args } => self.check_call(target, *callee, args, span),
            PExprData::Array(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let ty = if let Some(Type::Array(ty, _)) = target.map(|t| &self.proj.types[t]) {
                    *ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(expr, None);
                    let ty = expr.ty;
                    checked.push(expr);
                    ty
                } else {
                    return self.error(Error::new("cannot infer type of array literal", expr.span));
                };

                checked.extend(elements.map(|e| self.type_check(e, ty)));
                CExpr::new(
                    self.proj.types.insert(Type::Array(ty, checked.len())),
                    CExprData::Array(checked),
                )
            }
            PExprData::ArrayWithInit { init, count } => {
                let init = if let Some(&Type::Array(ty, _)) = target.map(|t| &self.proj.types[t]) {
                    self.type_check(*init, ty)
                } else {
                    self.check_expr(*init, target)
                };
                if let Some(res) = self.consteval_check(*count, TypeId::USIZE) {
                    let count = res.val.try_into().unwrap();
                    CExpr::new(
                        self.proj.types.insert(Type::Array(init.ty, count)),
                        CExprData::ArrayWithInit { init: init.into(), count },
                    )
                } else {
                    Default::default()
                }
            }
            PExprData::VecWithInit { init, count } => {
                let Some(vec) = self.get_lang_type_or_err("vec", span) else {
                    return Default::default();
                };

                let (init, ty) = if let Some(ty) = target
                    .and_then(|target| self.proj.types[target].as_user())
                    .filter(|ut| ut.id == vec)
                    .and_then(|ut| ut.first_type_arg())
                {
                    (self.type_check(*init, ty), ty)
                } else {
                    let expr = self.check_expr(*init, None);
                    let ty = expr.ty;
                    (expr, ty)
                };

                CExpr::new(
                    self.make_lang_type(vec, [ty], span),
                    CExprData::VecWithInit {
                        init: init.into(),
                        count: self.type_check(*count, TypeId::USIZE).into(),
                    },
                )
            }
            PExprData::Tuple(elements) => {
                let mut result_ty = Vec::with_capacity(elements.len());
                let mut result_elems = IndexMap::with_capacity(elements.len());
                for (i, expr) in elements.into_iter().enumerate() {
                    let result = if let Some(&target) = target
                        .and_then(|t| self.proj.types[t].as_user())
                        .filter(|t| self.proj.scopes.get(t.id).kind.is_tuple())
                        .and_then(|ut| ut.ty_args.get_index(i).map(|(_, v)| v))
                    {
                        self.type_check(expr, target)
                    } else {
                        self.check_expr(expr, None)
                    };

                    result_ty.push(result.ty);
                    result_elems.insert(intern!(self, "{i}"), result);
                }

                CExpr::new(
                    self.proj.scopes.get_tuple(
                        result_ty,
                        &mut self.proj.strings,
                        &mut self.proj.types,
                    ),
                    CExprData::Instance(result_elems),
                )
            }
            PExprData::Vec(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(vec) = self.get_lang_type_or_err("vec", span) else {
                    return Default::default();
                };

                let ty = if let Some(ty) = target
                    .and_then(|target| self.proj.types[target].as_user())
                    .filter(|ut| ut.id == vec)
                    .and_then(|ut| ut.first_type_arg())
                {
                    ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(expr, None);
                    let ty = expr.ty;
                    checked.push(expr);
                    ty
                } else {
                    self.error(Error::new("cannot infer type of vector literal", expr.span))
                };

                checked.extend(elements.map(|e| self.type_check(e, ty)));
                CExpr::new(self.make_lang_type(vec, [ty], span), CExprData::Vec(checked))
            }
            PExprData::Set(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(set) = self.get_lang_type_or_err("set", span) else {
                    return Default::default();
                };

                let ty = if let Some(ty) = target
                    .and_then(|target| self.proj.types[target].as_user())
                    .filter(|ut| ut.id == set)
                    .and_then(|ut| ut.first_type_arg())
                {
                    ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(expr, None);
                    let ty = expr.ty;
                    checked.push(expr);
                    ty
                } else {
                    self.error(Error::new("cannot infer type of set literal", span))
                };

                checked.extend(elements.map(|e| self.type_check(e, ty)));
                CExpr::new(
                    self.make_lang_type(set, [ty], span),
                    CExprData::Set(checked, self.current),
                )
            }
            PExprData::Map(elements) => {
                let Some(map) = self.get_lang_type_or_err("map", expr.span) else {
                    return Default::default();
                };

                let mut result = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let (k, v) = if let Some(ut) = target
                    .and_then(|target| self.proj.types[target].as_user())
                    .filter(|ut| ut.id == map)
                {
                    let mut args = ut.ty_args.values().copied();
                    (args.next().unwrap(), args.next().unwrap())
                } else if let Some((key, val)) = elements.next() {
                    let key = self.check_expr(key, None);
                    let val = self.check_expr(val, None);

                    let k = key.ty;
                    let v = val.ty;
                    result.push((key, val));
                    (k, v)
                } else {
                    self.error(Error::new("cannot infer type of map literal", expr.span))
                };

                result.extend(
                    elements.map(|(key, val)| (self.type_check(key, k), self.type_check(val, v))),
                );
                CExpr::new(
                    self.make_lang_type(map, [k, v], span),
                    CExprData::Map(result, self.current),
                )
            }
            PExprData::Range { start, end, inclusive } => {
                let start_id = self.proj.strings.get_or_intern_static("start");
                let end_id = self.proj.strings.get_or_intern_static("end");

                let (item, ty, inst) = match (start, end) {
                    // this could be skipped by just transforming these expressions to calls
                    (Some(start), Some(end)) => {
                        let start = self.check_expr(*start, None);
                        let end = self.type_check(*end, start.ty);
                        let item = if inclusive { "range_inclusive" } else { "range" };
                        (item, start.ty, [(start_id, start), (end_id, end)].into())
                    }
                    (None, Some(end)) => {
                        let end = self.check_expr(*end, None);
                        let item = if inclusive { "range_to_inclusive" } else { "range_to" };
                        (item, end.ty, [(end_id, end)].into())
                    }
                    (Some(start), None) => {
                        let start = self.check_expr(*start, None);
                        ("range_from", start.ty, [(start_id, start)].into())
                    }
                    (None, None) => {
                        return CExpr::new(
                            self.make_lang_type_by_name("range_full", [], span),
                            CExprData::Instance(Default::default()),
                        );
                    }
                };
                let Some(id) = self.get_lang_type_or_err(item, span) else {
                    return Default::default();
                };
                CExpr::new(self.make_lang_type(id, [ty], span), CExprData::Instance(inst))
            }
            PExprData::String(s) => {
                CExpr::new(self.make_lang_type_by_name("string", [], span), CExprData::String(s))
            }
            PExprData::StringInterpolation { strings, args } => {
                let Some(fmt_id) = self.get_lang_type_or_err("fmt_format", span) else {
                    return Default::default();
                };

                let Some(dbg_id) = self.get_lang_type_or_err("fmt_debug", span) else {
                    return Default::default();
                };

                let mut out = Vec::with_capacity(args.len());
                for (expr, opts) in args {
                    let span = expr.span;
                    let expr = self.check_expr(expr, None);
                    let ty = expr.ty.strip_references(&self.proj.types);
                    if ty == TypeId::UNKNOWN {
                        continue;
                    }

                    let mut target_tr = fmt_id;
                    let mut target_fn = "fmt";
                    let mut upper = false;
                    match opts.as_ref().and_then(|opts| opts.typ) {
                        Some(FormatType::Custom(id)) => {
                            let typ = strdata!(self, id.data);
                            target_fn = match typ {
                                "x" | "X" => "hex",
                                "o" | "O" => "oct",
                                "b" | "B" => "bin",
                                "e" | "E" => "exp",
                                "p" | "P" => "ptr",
                                typ => {
                                    self.proj.diag.report(Error::new(
                                        format!("invalid format type '{typ}'"),
                                        id.span,
                                    ));
                                    "fmt"
                                }
                            };
                            upper = typ.chars().next().is_some_and(|ch| ch.is_ascii_uppercase());
                        }
                        Some(FormatType::Debug) => {
                            target_tr = dbg_id;
                            target_fn = "dbg";
                        }
                        None => {}
                    }

                    let fmt = self.proj.strings.get_or_intern_static(target_fn);
                    let Some(mfn) = self.get_member_fn_ex(
                        ty,
                        Some(&GenericTrait::from_type_args(&self.proj.scopes, target_tr, [])),
                        fmt,
                        self.current,
                        |this, id| TypeArgs::in_order(&this.proj.scopes, id, []),
                    ) else {
                        self.proj.diag.report(Error::doesnt_implement(
                            &type_name!(self, ty),
                            strdata!(self, target_tr.name(&self.proj.scopes).data),
                            span,
                        ));
                        continue;
                    };

                    let mut res = CFormatSpec {
                        width: CExpr::new(TypeId::U16, CExprData::Int(ComptimeInt::Small(0))),
                        prec: CExpr::new(TypeId::U16, CExprData::Int(ComptimeInt::Small(0))),
                        fill: ' ',
                        align: None,
                        sign: None,
                        alt: false,
                        zero: false,
                        upper,
                        func: mfn,
                    };
                    if let Some(opts) = opts {
                        if let Some(width) = opts.width {
                            res.width = self.type_check(width, TypeId::U16);
                        }
                        if let Some(prec) = opts.prec {
                            res.prec = self.type_check(prec, TypeId::U16);
                        }
                        if let Some(fill) = opts.fill {
                            res.fill = fill;
                        }
                        res.sign = opts.sign;
                        res.align = opts.align;
                        res.alt = opts.alt;
                        res.zero = opts.zero;
                    }

                    let ptr_to_unk = self.proj.types.insert(Type::Ptr(TypeId::UNKNOWN));
                    let expr = expr.auto_deref(&mut self.proj.types, ptr_to_unk);
                    out.push((expr, res));
                }

                CExpr::new(
                    self.make_lang_type_by_name("fmt_args", [], span),
                    CExprData::StringInterp { strings, args: out, scope: self.current },
                )
            }
            PExprData::ByteString(s) => {
                let arr = self.proj.types.insert(Type::Array(TypeId::U8, s.len()));
                CExpr::new(self.proj.types.insert(Type::Ptr(arr)), CExprData::ByteString(s))
            }
            PExprData::Char(s) => {
                CExpr::new(TypeId::CHAR, CExprData::Int(ComptimeInt::from(s as u32)))
            }
            PExprData::ByteChar(c) => CExpr::new(TypeId::U8, CExprData::Int(ComptimeInt::from(c))),
            PExprData::Void => CExpr::void(),
            PExprData::Bool(v) => CExpr::from(v),
            PExprData::Integer(integer) => {
                let (ty, value) = self.get_int_type_and_val(target, integer, span);
                CExpr::new(ty, CExprData::Int(value))
            }
            PExprData::Float(float) => {
                let typ = if let Some(suffix) = float.suffix {
                    match strdata!(self, suffix) {
                        "f32" => TypeId::F32,
                        "f64" => TypeId::F64,
                        data => {
                            return self.error(Error::new(
                                format!("invalid float literal type: {data}"),
                                span,
                            ));
                        }
                    }
                } else {
                    target
                        .map(|target| target.strip_options(&self.proj))
                        .filter(|&t| t == TypeId::F32 || t == TypeId::F64)
                        .unwrap_or(TypeId::F64)
                };

                if float.value.is_infinite()
                    || (typ == TypeId::F32 && (float.value as f32).is_infinite())
                {
                    self.proj.diag.report(Error::new(
                        format!(
                            "literal is out of range for type '{0}' (use {0}::inf() if infinity is desired)",
                            type_name!(self, TypeId::F32)
                        ),
                        span,
                    ));
                }

                // TODO: warn for lossy conversion from literal

                CExpr::new(typ, CExprData::Float(float.value))
            }
            PExprData::Path(path) => match self.resolve_value_path(&path, target) {
                ResolvedValue::Var(id) => {
                    let var = self.proj.scopes.get(id);
                    if !var.is_static {
                        if self.current_function() != self.proj.scopes.function_of(var.scope) {
                            self.proj.diag.report(Error::new(
                                "cannot reference local variable of enclosing function",
                                span,
                            ));
                        }
                        if self
                            .current_static
                            .as_ref()
                            .is_some_and(|v| !self.is_var_accessible(v.0, var.scope))
                        {
                            self.proj.diag.report(Error::new(
                                "cannot reference local variable from outside of static initializer",
                                span,
                            ));
                        }
                    } else if let Some((cur, deps)) = &mut self.current_static {
                        deps.push(id);
                        let recursive = match self.proj.static_deps.get(&id) {
                            Some(Dependencies::Resolved(v_deps)) => v_deps.contains(cur),
                            Some(Dependencies::Resolving) => true,
                            _ => false,
                        };
                        if recursive {
                            self.proj.diag.report(Error::new(
                                "static initializer depends directly or indirectly on itself",
                                span,
                            ));
                        }
                    }

                    if var.is_static && var.is_extern {
                        check_unsafe!(
                            self,
                            Error::new("accessing static extern variable is unsafe", span)
                        );
                    } else if var.is_static && var.mutable {
                        check_unsafe!(
                            self,
                            Error::new("accessing static mutable variable is unsafe", span)
                        );
                    }

                    let ty = var.ty;
                    self.proj.scopes.get_mut(id).unused = false;
                    CExpr::new(ty, CExprData::Var(id))
                }
                ResolvedValue::Fn(mut func) => {
                    let unknowns: HashSet<_> = func
                        .ty_args
                        .iter()
                        .filter_map(|(&id, &ty)| (ty == TypeId::UNKNOWN).then_some(id))
                        .collect();
                    if let Some(target) = target {
                        func.infer_type_args(
                            &self.proj.types,
                            self.proj.scopes.get(func.id).ret,
                            target,
                        );
                    }
                    self.check_bounds_filtered(&func, &unknowns, path.final_component_span());

                    if let Some(id) = self.proj.scopes.get(func.id).constructor
                        && self
                            .proj
                            .scopes
                            .get(id)
                            .is_empty_variant(self.proj.scopes.get(func.id).name.data)
                    {
                        return CExpr::new(
                            self.proj
                                .types
                                .insert(Type::User(GenericUserType::new(id, func.ty_args))),
                            CExprData::VariantInstance(
                                self.proj.scopes.get(func.id).name.data,
                                Default::default(),
                            ),
                        );
                    }

                    CExpr::new(
                        self.proj.types.insert(Type::Fn(func.clone())),
                        CExprData::Fn(func, self.current),
                    )
                }
                ResolvedValue::MemberFn(mut mfn) => {
                    let unknowns: HashSet<_> = mfn
                        .func
                        .ty_args
                        .iter()
                        .filter_map(|(&id, &ty)| (ty == TypeId::UNKNOWN).then_some(id))
                        .collect();
                    if let Some(target) = target {
                        mfn.func.infer_type_args(
                            &self.proj.types,
                            self.proj.scopes.get(mfn.func.id).ret,
                            target,
                        );
                    }
                    self.check_bounds_filtered(&mfn.func, &unknowns, path.final_component_span());

                    if let Some(id) = self.proj.scopes.get(mfn.func.id).constructor
                        && self
                            .proj
                            .scopes
                            .get(id)
                            .is_empty_variant(self.proj.scopes.get(mfn.func.id).name.data)
                    {
                        return CExpr::new(
                            self.proj
                                .types
                                .insert(Type::User(GenericUserType::new(id, mfn.func.ty_args))),
                            CExprData::VariantInstance(
                                self.proj.scopes.get(mfn.func.id).name.data,
                                Default::default(),
                            ),
                        );
                    }

                    CExpr::new(
                        self.proj.types.insert(Type::Fn(mfn.func.clone())),
                        CExprData::MemFn(mfn, self.current),
                    )
                }
                ResolvedValue::UnionConstructor(ut) => bail!(
                    self,
                    Error::expected_found(
                        "expression",
                        &format!("type '{}'", type_name!(self, ut)),
                        span,
                    )
                ),
                ResolvedValue::NotFound(err) => {
                    named_error!(self, Error::no_symbol, err.data, err.span)
                }
                ResolvedValue::Error => Default::default(),
            },
            PExprData::Block(body, label) => {
                let block = self.create_block(
                    body,
                    ScopeKind::Block(BlockScopeKind {
                        target,
                        yields: false,
                        label,
                        branches: false,
                    }),
                );
                let data = self.proj.scopes[block.scope].kind.as_block().unwrap();
                let target = match (data.branches, data.yields) {
                    (true, false) => Some(TypeId::NEVER),
                    (_, true) => data.target,
                    _ => None,
                };
                CExpr::new(target.unwrap_or(TypeId::VOID), CExprData::Block(block))
            }
            PExprData::If { cond, if_branch, else_branch } => {
                let (cond, vars) = self.type_check_with_listen(*cond);
                let target = if else_branch.is_none() {
                    target.and_then(|t| t.as_option_inner(&self.proj))
                } else {
                    target
                };

                let if_span = if_branch.span;
                let mut if_branch = self.enter(ScopeKind::None, |this| {
                    this.define(&vars);
                    this.check_expr_inner(*if_branch, target)
                });
                if let Some(target) = target {
                    if_branch = self.type_check_checked(if_branch, target, if_span);
                }

                let mut out_type = if_branch.ty;
                let else_branch = if let Some(expr) = else_branch {
                    if out_type == TypeId::NEVER {
                        let expr = self.check_expr_inner(*expr, None);
                        out_type = expr.ty;
                        if_branch = self.try_coerce(if_branch, expr.ty);
                        expr
                    } else {
                        let span = expr.span;
                        let source = self.check_expr_inner(*expr, target.or(Some(out_type)));
                        if source.ty.as_option_inner(&self.proj).is_some_and(|v| v == out_type) {
                            let Ok(expr) = self.coerce(if_branch, source.ty) else {
                                unreachable!()
                            };
                            if_branch = expr;
                            out_type = source.ty;
                            source
                        } else {
                            self.type_check_checked(source, out_type, span)
                        }
                    }
                } else {
                    // This ensures
                    //                  if false { unreachable() }
                    //                  if false { {} }
                    //                  if false { unreachable(); }
                    // All resolve to type `void` instead of ?never/?void while
                    //                  if false { 10 }
                    // Has type ?int
                    if if_branch.data.is_yielding_block(&self.proj.scopes).unwrap_or(true)
                        && !matches!(out_type, TypeId::NEVER | TypeId::VOID | TypeId::UNKNOWN)
                    {
                        out_type = self.make_lang_type_by_name("option", [out_type], span);
                        if_branch = self.try_coerce(if_branch, out_type);
                        CExpr::option_null(out_type)
                    } else {
                        out_type = TypeId::VOID;
                        CExpr::void()
                    }
                };

                CExpr::new(
                    out_type,
                    CExprData::If {
                        cond: cond.into(),
                        if_branch: if_branch.into(),
                        else_branch: else_branch.into(),
                    },
                )
            }
            PExprData::Loop { cond, body, do_while, label } => {
                let infinite = cond.is_none();
                let target = self.loop_target(target, infinite);
                let kind = LoopScopeKind { target, breaks: LoopBreak::None, infinite, label };
                let (cond, body) = if let Some(cond) = cond {
                    let (cond, vars) = self.type_check_with_listen(*cond);
                    let body = self.create_block_with_init(body, ScopeKind::Loop(kind), |this| {
                        if !do_while {
                            this.define(&vars);
                        }
                    });
                    (Some(cond.into()), body)
                } else {
                    (None, self.create_block(body, ScopeKind::Loop(kind)))
                };
                let (out_type, optional) = self.loop_out_type(
                    self.proj.scopes[body.scope].kind.as_loop().copied().unwrap(),
                    span,
                );
                CExpr::new(out_type, CExprData::Loop { cond, body, do_while, optional })
            }
            PExprData::For { patt, iter, body, label } => {
                self.check_for_expr(target, patt, *iter, body, label)
            }
            PExprData::Member { source, member: name, generics } => {
                if !generics.is_empty() {
                    self.error(Error::new("member variables cannot have type arguments", span))
                }

                let source = self.check_expr(*source, None);
                let id = source.ty.strip_references(&self.proj.types);
                self.check_dot_completions(span, id, true);
                let ut_id = match &self.proj.types[id] {
                    Type::User(data) => data.id,
                    Type::Unknown => return Default::default(),
                    _ => {
                        bail!(
                            self,
                            Error::no_member(
                                &type_name!(self, id),
                                strdata!(self, name.data),
                                name.span
                            )
                        );
                    }
                };
                self.resolve_members(ut_id);
                self.check_hover(name.span, LspItem::Property(Some(source.ty), ut_id, name.data));

                let ut = self.proj.scopes.get(ut_id);
                let Some(member) = ut.members.get(&name.data) else {
                    bail!(
                        self,
                        Error::no_member(
                            &type_name!(self, source.ty),
                            strdata!(self, name.data),
                            name.span
                        )
                    );
                };

                if ut.kind.is_unsafe_union() {
                    check_unsafe!(self, Error::is_unsafe(name.span));
                }

                let ty = member.ty.with_ut_templates(&mut self.proj.types, id);
                if !member.public && !self.can_access_privates(ut.scope) {
                    self.proj.diag.report(Error::private_member(
                        &type_name!(self, id),
                        strdata!(self, name.data),
                        name.span,
                    ));
                }
                CExpr::new(
                    ty,
                    CExprData::Member {
                        source: source.auto_deref(&mut self.proj.types, id).into(),
                        member: name.data,
                    },
                )
            }
            PExprData::Subscript { callee, args } => {
                if args.is_empty() {
                    return self
                        .error(Error::new("subscript requires at least one argument", span));
                }

                let callee = self.check_expr(*callee, None);
                let stripped = callee.ty.strip_references(&self.proj.types);
                if let &Type::Array(target, _) = &self.proj.types[stripped] {
                    self.check_array_subscript(target, callee, args)
                } else {
                    self.check_subscript(callee, stripped, args, target, false, span)
                }
            }
            PExprData::Return(expr) => self.check_return(*expr, span),
            PExprData::Tail(expr) => match &self.proj.scopes[self.current].kind {
                ScopeKind::Function(_) | ScopeKind::Lambda(_, _) => self.check_return(*expr, span),
                ScopeKind::Loop { .. } => self.type_check(*expr, TypeId::VOID),
                ScopeKind::Block(data) => self.check_yield(Some(expr), *data, self.current, span),
                _ => self.error(Error::new("yield outside of block", expr.span)),
            },
            PExprData::Break(expr, label) => {
                if let Some(label) = label {
                    let label_data = Some(&label.data);
                    for (id, scope) in self.proj.scopes.walk(self.current) {
                        if scope.kind.is_defer() {
                            break;
                        }

                        match &scope.kind {
                            ScopeKind::Loop(data) if data.label.as_ref() == label_data => {
                                return self.check_break(expr, *data, id, span);
                            }
                            ScopeKind::Block(data) if data.label.as_ref() == label_data => {
                                return self.check_yield(expr, *data, id, span);
                            }
                            _ => {}
                        }
                    }

                    if let Some(expr) = expr {
                        self.check_expr(*expr, None);
                    }
                    return report_error!(
                        self,
                        label.span,
                        "undefined label '{}'",
                        strdata!(self, label.data)
                    );
                }

                let Some((loop_data, id)) = self.current_loop(&None) else {
                    if let Some(expr) = expr {
                        self.check_expr(*expr, None);
                    }
                    return self.error(Error::new("break outside of loop", span));
                };

                self.check_break(expr, *loop_data, id, span)
            }
            PExprData::Continue(label) => {
                let Some((_, id)) = self.current_loop(&label) else {
                    if let Some(label) = label {
                        let name = strdata!(self, label.data);
                        return self
                            .error(Error::new(format!("undefined label '{name}'"), label.span));
                    } else {
                        return self.error(Error::new("continue outside of loop", span));
                    }
                };

                CExpr::new(TypeId::NEVER, CExprData::Continue(id))
            }
            PExprData::Is { expr, pattern } => self.enter(ScopeKind::None, |this| {
                let mut prev = this.current_expr;
                let expr = this.check_expr(*expr, None);
                std::mem::swap(&mut this.current_expr, &mut prev);
                let patt = this.check_pattern(PatternParams {
                    scrutinee: expr.ty,
                    mutable: false,
                    pattern,
                    typ: PatternType::Regular,
                    has_hint: false,
                });
                std::mem::swap(&mut this.current_expr, &mut prev);
                CExpr::new(TypeId::BOOL, CExprData::Is(expr.into(), patt))
            }),
            PExprData::Match { expr, body } => {
                let scrutinee = self.check_expr(*expr, None);
                let mut has_never = false;
                let mut target = target;
                let mut result: Vec<_> = body
                    .into_iter()
                    .map(|(patt, expr)| {
                        let span = expr.span;
                        let (patt, expr) = self.enter(ScopeKind::None, |this| {
                            (
                                this.check_full_pattern(scrutinee.ty, patt),
                                this.check_expr(expr, target),
                            )
                        });

                        if let Some(target) = target {
                            (patt, self.type_check_checked(expr, target, span))
                        } else {
                            if expr.ty == TypeId::NEVER {
                                has_never = true;
                            } else {
                                target = Some(expr.ty);
                            }

                            (patt, expr)
                        }
                    })
                    .collect();
                let target = target.unwrap_or(if has_never { TypeId::NEVER } else { TypeId::VOID });
                if !matches!(target, TypeId::NEVER | TypeId::VOID) {
                    for (_, e) in result.iter_mut() {
                        *e = self.try_coerce(std::mem::take(e), target);
                    }
                }

                self.check_match_coverage(scrutinee.ty, result.iter().map(|it| &it.0), span);
                CExpr::new(target, CExprData::Match { expr: scrutinee.into(), body: result })
            }
            PExprData::As { expr, ty, throwing } => {
                let to_id = self.resolve_typehint(&ty);
                let expr = self.check_expr(*expr, Some(to_id));
                let expr = match self.coerce(expr, to_id) {
                    Ok(expr) => return expr,
                    Err(expr) => expr,
                };

                let mut from_id = expr.ty;
                if let Type::User(ut) = &self.proj.types[from_id] {
                    let id = ut.id;
                    self.resolve_members(id);
                    if let Some(tag) = self
                        .proj
                        .scopes
                        .get(id)
                        .kind
                        .as_union()
                        .filter(|u| u.enum_union)
                        .map(|u| u.tag)
                    {
                        from_id = tag;
                    }
                }

                match Cast::get(&self.proj.types[from_id], &self.proj.types[to_id]) {
                    Cast::None => self.proj.diag.report(Error::new(
                        format!(
                            "cannot cast expression of type '{}' to '{}'",
                            type_name!(self, from_id),
                            type_name!(self, to_id),
                        ),
                        span,
                    )),
                    Cast::Unsafe => {
                        check_unsafe!(self, Error::is_unsafe(span));
                    }
                    Cast::Fallible if !throwing => self.proj.diag.report(Error::new(
                        format!(
                            "cast of expression of type '{}' to '{}' requires fallible cast",
                            type_name!(self, from_id),
                            type_name!(self, to_id),
                        ),
                        span,
                    )),
                    Cast::Infallible if throwing => {
                        self.proj.diag.report(Warning::unnecessary_fallible_cast(
                            &type_name!(self, from_id),
                            &type_name!(self, to_id),
                            span,
                        ))
                    }
                    _ => {}
                }
                CExpr::new(to_id, CExprData::As(expr.into(), throwing))
            }
            PExprData::Error => CExpr::default(),
            PExprData::Lambda { params, ret, body, moves: _ } => {
                let ty_is_generic = |this: &TypeChecker, ty: TypeId| {
                    !this
                        .proj
                        .types
                        .get(ty)
                        .as_user()
                        .is_some_and(|ut| this.proj.scopes.get(ut.id).kind.is_template())
                };

                let mut lparams = Vec::new();
                let ret = ret.map(|ret| self.resolve_typehint(&ret)).or_else(|| {
                    target
                        .as_ref()
                        .and_then(|&ty| self.proj.types[ty].as_fn_ptr())
                        .and_then(|f| ty_is_generic(self, f.ret).then_some(f.ret))
                });
                // TODO: lambdas should have a unique type
                let (id, body) = self.enter(ScopeKind::Lambda(ret, false), |this| {
                    for (i, (name, hint)) in params.into_iter().enumerate() {
                        let has_hint = hint.is_some();
                        let ty = hint
                            .map(|ty| this.resolve_typehint(&ty))
                            .or_else(|| {
                                target
                                    .as_ref()
                                    .and_then(|&ty| this.proj.types[ty].as_fn_ptr())
                                    .and_then(|f| f.params.get(i))
                                    .filter(|&&ty| ty_is_generic(this, ty))
                                    .copied()
                            })
                            .unwrap_or_else(|| {
                                let data = strdata!(this, name.data);
                                this.error(Error::new(
                                    format!("cannot infer type of parameter '{data}'"),
                                    name.span,
                                ))
                            });

                        lparams.push(ty);
                        this.insert::<VariableId>(
                            Variable { name, ty, unused: true, has_hint, ..Default::default() },
                            false,
                            false,
                        );
                    }

                    let body = if let PExprData::Block(body, _) = body.data {
                        this.check_block(body)
                    } else {
                        vec![CStmt::Expr(
                            this.check_expr(PExpr::new(body.span, PExprData::Return(body)), None),
                        )]
                    };

                    (this.current, body)
                });
                let (target, yields) = self.proj.scopes[id].kind.as_lambda().unwrap();
                let fnptr = Type::FnPtr(FnPtr {
                    is_extern: false,
                    params: lparams,
                    ret: yields.then(|| *target).flatten().unwrap_or(TypeId::VOID),
                });
                CExpr::new(self.proj.types.insert(fnptr), CExprData::Lambda(body))
            }
            PExprData::Unsafe(expr) => {
                let mut span = span;
                span.len = "unsafe".len() as u32;
                let was_unsafe = matches!(self.safety, Safety::Unsafe(_));
                if was_unsafe {
                    self.proj.diag.report(Warning::redundant_unsafe(span));
                }

                // consider the body of the
                self.current_expr -= 1;
                let old_safety = std::mem::replace(&mut self.safety, Safety::Unsafe(false));
                let expr = self.check_expr(*expr, target);
                let old_safety = std::mem::replace(&mut self.safety, old_safety);
                if !was_unsafe && matches!(old_safety, Safety::Unsafe(false)) {
                    self.proj.diag.report(Warning::useless_unsafe(span));
                }

                expr
            }
        }
    }

    fn check_expr(&mut self, expr: PExpr, target: Option<TypeId>) -> CExpr {
        let expr = self.check_expr_inner(expr, target);
        if expr.ty == TypeId::NEVER
            && !matches!(expr.data, CExprData::Yield(_, scope) if scope == self.current)
        {
            // TODO: lambdas
            if let ScopeKind::Block(BlockScopeKind { branches, .. }) =
                &mut self.proj.scopes[self.current].kind
            {
                *branches = true;
            }
        }
        expr
    }

    fn type_check_with_listen(&mut self, expr: PExpr) -> (CExpr, Vec<VariableId>) {
        self.listen_for_vars(self.current_expr + 1, |this| this.type_check(expr, TypeId::BOOL))
    }

    fn listen_for_vars<T>(
        &mut self,
        expr: usize,
        f: impl FnOnce(&mut Self) -> T,
    ) -> (T, Vec<VariableId>) {
        let prev = std::mem::take(&mut self.listening_vars);
        let prev_insp = std::mem::replace(&mut self.listening_expr, expr);
        let res = f(self);
        self.listening_expr = prev_insp;
        (res, std::mem::replace(&mut self.listening_vars, prev))
    }

    fn define(&mut self, vars: &[VariableId]) {
        for &var in vars.iter() {
            let name = self.proj.scopes.get(var).name.data;
            self.proj.scopes[self.current].vns.insert(name, Vis::new(ValueItem::Var(var), false));
        }
    }

    fn current_loop(&self, label: &Option<Located<StrId>>) -> Option<(&LoopScopeKind, ScopeId)> {
        let label = label.as_ref().map(|l| l.data);
        self.proj
            .scopes
            .walk(self.current)
            .take_while(|(_, scope)| !scope.kind.is_defer())
            .find_map(|(id, scope)| {
                scope.kind.as_loop().filter(|l| label.is_none() || l.label == label).zip(Some(id))
            })
    }

    fn check_array_subscript(&mut self, target: TypeId, callee: CExpr, args: CallArgs) -> CExpr {
        fn maybe_span(this: &mut TypeChecker, ty: TypeId, imm: bool) -> Option<UserTypeId> {
            let key = this.proj.strings.get_or_intern_static("range_bounds");
            let id = *this.proj.scopes.lang_types.get(&key)?;
            let bound = GenericTrait::from_type_args(&this.proj.scopes, id, [TypeId::USIZE]);
            if this.implements_trait(ty, &bound) {
                let span_ty = if imm {
                    *this.proj.scopes.lang_types.get(&Strings::LANG_SPAN)?
                } else {
                    *this.proj.scopes.lang_types.get(&Strings::LANG_SPAN_MUT)?
                };
                Some(span_ty)
            } else {
                None
            }
        }

        let mut args = args.into_iter();
        let (name, expr) = args.next().unwrap();
        if let Some(name) = name {
            report_error!(self, expr.span, "unknown parameter: '{}'", strdata!(self, name.data))
        }

        let arg_span = expr.span;
        let arg = self.check_expr(expr, Some(TypeId::USIZE));
        if let Some((_, arg)) = args.next() {
            let last = args.last().map(|(_, arg)| arg.span).unwrap_or(arg.span);
            self.error(Error::new(
                "multidimensional array subscript is not supported",
                arg.span.extended_to(last),
            ))
        }

        match self.coerce(arg, TypeId::USIZE) {
            Ok(expr) => {
                CExpr::new(target, CExprData::Subscript { callee: callee.into(), arg: expr.into() })
            }
            Err(expr) if self.proj.types[expr.ty].is_integral() => {
                CExpr::new(target, CExprData::Subscript { callee: callee.into(), arg: expr.into() })
            }
            Err(expr) => {
                let Some(id) = maybe_span(self, expr.ty, self.immutable_receiver(&callee)) else {
                    bail!(
                        self,
                        Error::expected_found(
                            "array index",
                            &format!("type '{}'", type_name!(self, expr.ty)),
                            arg_span,
                        )
                    );
                };

                CExpr::new(
                    self.proj.types.insert(Type::User(GenericUserType::from_type_args(
                        &self.proj.scopes,
                        id,
                        [target],
                    ))),
                    CExprData::SliceArray { callee: callee.into(), arg: expr.into() },
                )
            }
        }
    }

    fn check_subscript(
        &mut self,
        callee: CExpr,
        ty: TypeId,
        args: CallArgs,
        target: Option<TypeId>,
        assign: bool,
        span: Span,
    ) -> CExpr {
        let imm_receiver = self.immutable_receiver(&callee);
        if let Some(ut) = self.proj.types[ty].as_user().cloned() {
            let mut candidates: Vec<_> = self
                .proj
                .scopes
                .get(ut.id)
                .subscripts
                .iter()
                .cloned()
                .filter(|&f| assign == self.proj.scopes.get(f).assign_subscript)
                .filter(|&f| self.proj.scopes.get(f).params.len() == args.len() + 1)
                .collect();
            candidates.iter().for_each(|&f| self.resolve_proto(f));
            candidates.sort_unstable_by(|&a, &b| {
                let left = self
                    .proj
                    .scopes
                    .get(a)
                    .params
                    .first()
                    .is_some_and(|p| self.proj.types[p.ty].is_mut_ptr());
                let right = self
                    .proj
                    .scopes
                    .get(b)
                    .params
                    .first()
                    .is_some_and(|p| self.proj.types[p.ty].is_mut_ptr());
                right.cmp(&left)
            });

            for f in candidates {
                if imm_receiver
                    && self
                        .proj
                        .scopes
                        .get(f)
                        .params
                        .first()
                        .is_some_and(|p| self.proj.types[p.ty].is_mut_ptr())
                {
                    continue;
                }

                let mut func = GenericFn::from_id(&self.proj.scopes, f);
                func.ty_args.copy_args(&ut.ty_args);

                let args = args.clone();
                let recv = callee
                    .clone()
                    .auto_deref(&mut self.proj.types, self.proj.scopes.get(f).params[0].ty);
                let err_idx = self.proj.diag.capture_errors();
                let (args, ret, failed) =
                    self.check_fn_args(&mut func, Some(recv), args, target, span);
                // TODO: if the arguments have non overload related errors, just stop overload
                // resolution
                if failed || args.iter().any(|arg| matches!(arg.1.data, CExprData::Error)) {
                    self.proj.diag.truncate_errors(err_idx);
                    continue;
                }
                // unsafe doesnt cause check_fn_args to fail, but we mute errors, so check again
                // here
                if self.proj.scopes.get(func.id).is_unsafe {
                    check_unsafe!(self, Error::is_unsafe(span));
                }

                if !assign && let Type::Ptr(inner) | Type::MutPtr(inner) = &self.proj.types[ret] {
                    return CExpr::new(
                        *inner,
                        CExprData::AutoDeref(
                            CExpr::new(
                                ret,
                                CExprData::call(
                                    &mut self.proj.types,
                                    func,
                                    args,
                                    self.current,
                                    span,
                                ),
                            )
                            .into(),
                            1,
                        ),
                    );
                }

                return CExpr::new(
                    ret,
                    CExprData::call(&mut self.proj.types, func, args, self.current, span),
                );
            }
        }

        let args: Vec<_> = args.into_iter().map(|(_, expr)| self.check_expr(expr, None)).collect();
        if ty == TypeId::UNKNOWN || args.iter().any(|expr| expr.ty == TypeId::UNKNOWN) {
            return Default::default();
        }

        bail!(
            self,
            Error::new(
                format!(
                    "type '{}' does not support subscript{} with arguments of type ({})",
                    &type_name!(self, callee.ty),
                    if assign { " assign" } else { "" },
                    args.into_iter()
                        .map(|expr| type_name!(self, expr.ty))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                span,
            )
        )
    }

    fn immutable_receiver(&self, callee: &CExpr) -> bool {
        if !matches!(
            self.proj.types[callee.ty],
            Type::Ptr(_) | Type::MutPtr(_) | Type::DynPtr(_) | Type::DynMutPtr(_)
        ) && !callee.can_addrmut(&self.proj.scopes, &self.proj.types)
        {
            return true;
        }

        let mut ty = &self.proj.types[callee.ty];
        while let Type::MutPtr(inner) = ty {
            ty = &self.proj.types[*inner];
        }

        matches!(ty, Type::Ptr(_) | Type::DynPtr(_))
    }

    fn is_var_accessible(&self, static_var: VariableId, scope: ScopeId) -> bool {
        for (_, scope) in self.proj.scopes.walk(scope) {
            match scope.kind {
                ScopeKind::None | ScopeKind::Defer | ScopeKind::Block(_) | ScopeKind::Loop(_) => {
                    continue;
                }
                ScopeKind::Static(v) if v == static_var => return true,
                _ => break,
            }
        }

        false
    }

    fn check_return(&mut self, expr: PExpr, span: Span) -> CExpr {
        for (id, scope) in self.proj.scopes.walk(self.current) {
            match &scope.kind {
                &ScopeKind::Lambda(target, _) => {
                    let span = expr.span;
                    let mut expr = self.check_expr(expr, target);
                    self.proj.scopes[id].kind = if let Some(target) = target {
                        expr = self.type_check_checked(expr, target, span);
                        ScopeKind::Lambda(Some(target), true)
                    } else {
                        ScopeKind::Lambda(Some(expr.ty), true)
                    };

                    return CExpr::new(TypeId::NEVER, CExprData::Return(expr.into()));
                }
                &ScopeKind::Function(id) => {
                    let target = self.proj.scopes.get(id).ret;
                    return CExpr::new(
                        TypeId::NEVER,
                        CExprData::Return(self.type_check(expr, target).into()),
                    );
                }
                ScopeKind::Defer => {
                    self.proj.diag.report(Error::new("cannot return in defer block", span));
                    return self.check_expr(expr, None);
                }
                _ => {}
            }
        }

        // this should never be possible, but report error instead of crashing for LSP reasons
        self.error(Error::new("return expression outside of function", expr.span))
    }

    fn check_break(
        &mut self,
        expr: Option<Box<PExpr>>,
        mut data: LoopScopeKind,
        id: ScopeId,
        span: Span,
    ) -> CExpr {
        let expr = if let Some(expr) = expr {
            let span = expr.span;
            let expr = if let Some(target) = data.target {
                let expr = self.type_check(*expr, target);
                data = *self.proj.scopes[id].kind.as_loop().unwrap();
                expr
            } else {
                let expr = self.check_expr(*expr, data.target);
                // expr could contain a nested expr that sets the target, ex. `break break 10`
                data = *self.proj.scopes[id].kind.as_loop().unwrap();
                if let Some(target) = data.target {
                    self.type_check_checked(expr, target, span)
                } else {
                    data.target = Some(expr.ty);
                    expr
                }
            };
            data.breaks = LoopBreak::WithValue;

            let (target, opt) = self.loop_out_type(data, span);
            if opt { self.try_coerce(expr, target) } else { expr }
        } else if let Some(target) = data.target {
            self.type_check_checked(CExpr::void(), target, span)
        } else {
            data.target = Some(TypeId::VOID);
            data.breaks = LoopBreak::WithNothing;
            CExpr::void()
        };

        self.proj.scopes[id].kind = ScopeKind::Loop(data);
        CExpr::new(TypeId::NEVER, CExprData::Break(expr.into(), id))
    }

    fn check_yield(
        &mut self,
        expr: Option<Box<PExpr>>,
        mut data: BlockScopeKind,
        id: ScopeId,
        span: Span,
    ) -> CExpr {
        let expr = if let Some(target) = data.target {
            expr.map(|expr| self.type_check(*expr, target))
                .unwrap_or_else(|| self.type_check_checked(CExpr::void(), target, span))
        } else {
            let span = expr.as_ref().map(|e| e.span).unwrap_or(span);
            let expr =
                expr.map(|expr| self.check_expr(*expr, data.target)).unwrap_or_else(CExpr::void);
            // expr could contain a nested expr that sets the target, ex. `@outer: { { break @outer 10 } }`
            data = *self.proj.scopes[id].kind.as_block().unwrap();
            if let Some(target) = data.target {
                self.type_check_checked(expr, target, span)
            } else {
                data.target = Some(expr.ty);
                expr
            }
        };

        data.yields = true;
        self.proj.scopes[id].kind = ScopeKind::Block(data);

        CExpr::new(TypeId::NEVER, CExprData::Yield(expr.into(), id))
    }

    fn check_for_expr(
        &mut self,
        target: Option<TypeId>,
        patt: Located<Pattern>,
        iter: Located<PExprData>,
        body: Vec<PStmt>,
        label: Option<StrId>,
    ) -> CExpr {
        let iter_span = iter.span;
        let iter = self.check_expr(iter, None);
        let Some(iter_tr_id) = self.get_lang_type_or_err("iter", iter_span) else {
            return Default::default();
        };

        let kind = ScopeKind::Block(BlockScopeKind {
            target: None,
            yields: true,
            label: None,
            branches: false,
        });
        self.enter(kind, |this| {
            let key = this.proj.strings.get_or_intern_static("next");
            let Some(mut mfn) = this.get_member_fn_legacy(iter.ty, iter_tr_id, key, this.current)
            else {
                this.check_block(body);
                let name = type_name!(this, iter.ty);
                return this.error(Error::doesnt_implement(&name, "Iterator", iter_span));
            };

            let next_ty = this
                .proj
                .scopes
                .get(mfn.func.id)
                .ret
                .strip_options(&this.proj)
                .with_templates(&mut this.proj.types, &mfn.func.ty_args);
            this.trait_hack(&mut mfn, next_ty);

            let patt_span = patt.span;
            let patt = this.check_pattern(PatternParams {
                scrutinee: next_ty,
                mutable: false,
                pattern: patt,
                typ: PatternType::Regular,
                has_hint: false,
            });
            if !patt.irrefutable {
                this.error(Error::must_be_irrefutable("for patterns", patt_span))
            }

            let body = this.create_block(
                body,
                ScopeKind::Loop(LoopScopeKind {
                    target: this.loop_target(target, false),
                    breaks: LoopBreak::None,
                    infinite: false,
                    label,
                }),
            );
            let (out, optional) = this.loop_out_type(
                this.proj.scopes[body.scope].kind.as_loop().copied().unwrap(),
                iter_span,
            );

            let name = intern!(this, "$iter{}", this.current.0);
            let iter_var = this.insert::<VariableId>(
                Variable {
                    name: Located::nowhere(name),
                    ty: iter.ty,
                    mutable: true,
                    value: None,
                    ..Default::default()
                },
                false,
                false,
            );

            let next_fn_call = {
                let f = this.proj.scopes.get(mfn.func.id);
                let Some(p0) = f.params.first().map(|p| p.label) else {
                    panic!("ICE: Iterator::next() has 0 parameters");
                };
                let arg0 = CExpr::new(
                    this.proj.types.insert(Type::MutPtr(iter.ty)),
                    CExprData::Unary(
                        UnaryOp::AddrMut,
                        CExpr::new(iter.ty, CExprData::Var(iter_var)).into(),
                    ),
                );

                CExpr::new(
                    f.ret.with_templates(&mut this.proj.types, &mfn.func.ty_args),
                    CExprData::member_call(
                        &mut this.proj.types,
                        mfn,
                        [(p0, arg0)].into(),
                        this.current,
                        iter_span,
                    ),
                )
            };

            let cond = CExpr::new(
                TypeId::BOOL,
                CExprData::Is(
                    next_fn_call.into(),
                    CPattern::refutable(PatternData::Variant {
                        pattern: Some(
                            CPattern::irrefutable(PatternData::Destrucure {
                                patterns: vec![(Strings::TUPLE_ZERO, next_ty, patt)],
                                borrows: false,
                            })
                            .into(),
                        ),
                        variant: Strings::SOME,
                        inner: TypeId::UNKNOWN,
                        borrows: false,
                    }),
                ),
            );
            let while_loop = CExpr::new(
                out,
                CExprData::Loop { cond: Some(cond.into()), body, do_while: false, optional },
            );

            CExpr::new(
                out,
                CExprData::Block(Block {
                    body: vec![
                        CStmt::Let(
                            CPattern { irrefutable: true, data: PatternData::Variable(iter_var) },
                            Some(iter),
                        ),
                        CStmt::Expr(CExpr::new(
                            out,
                            CExprData::Yield(while_loop.into(), this.current),
                        )),
                    ],
                    scope: this.current,
                }),
            )
        })
    }

    fn check_unsafe_union_constructor(
        &mut self,
        target: Option<TypeId>,
        mut ut: GenericUserType,
        args: CallArgs,
        span: Span,
    ) -> CExpr {
        self.resolve_members(ut.id);

        if let Some(target) =
            target.and_then(|t| self.proj.types[t].as_user()).filter(|t| t.id == ut.id)
        {
            for (id, ty) in target.ty_args.iter() {
                if ut.ty_args.get(id).is_some_and(|&id| id != TypeId::UNKNOWN) {
                    continue;
                }

                ut.ty_args.insert(*id, *ty);
            }
        }

        let mut members = IndexMap::new();
        for (name, expr) in args {
            let Some(name) = name else {
                self.proj.diag.report(Error::new(
                    "unsafe union constructor expects 0 positional arguments",
                    expr.span,
                ));
                _ = self.check_expr(expr, None);
                continue;
            };

            if !members.is_empty() {
                self.error(Error::new("too many variant arguments", expr.span))
            }

            let Some(ty) = self
                .proj
                .scopes
                .get(ut.id)
                .members
                .get(&name.data)
                .map(|m| m.ty.with_templates(&mut self.proj.types, &ut.ty_args))
            else {
                let _: () = report_error!(
                    self,
                    name.span,
                    "unknown variant '{}'",
                    strdata!(self, name.data)
                );
                _ = self.check_expr(expr, None);
                continue;
            };

            self.check_hover(name.span, LspItem::Property(Some(ty), ut.id, name.data));

            members.insert(name.data, self.check_arg(&mut ut, expr, ty).0);
        }

        if !self.proj.scopes.get(ut.id).members.is_empty() && members.is_empty() {
            return self.error(Error::new("expected 1 variant argument", span));
        }

        for (&id, &ty) in ut.ty_args.iter() {
            if ty == TypeId::UNKNOWN {
                report_error!(
                    self,
                    span,
                    "cannot infer type for type parameter '{}'",
                    strdata!(self, self.proj.scopes.get(id).name.data),
                )
            } else {
                self.check_bounds(&ut.ty_args, ty, self.proj.scopes.get(id).impls.clone(), span);
            }
        }

        CExpr::new(self.proj.types.insert(Type::User(ut)), CExprData::Instance(members))
    }

    fn check_call(
        &mut self,
        target: Option<TypeId>,
        callee: PExpr,
        args: CallArgs,
        span: Span,
    ) -> CExpr {
        match callee.data {
            PExprData::Member { source, member, generics } => {
                let recv = self.check_expr(*source, None);
                let id = recv.ty.strip_references(&self.proj.types);
                if id == TypeId::UNKNOWN {
                    return Default::default();
                }

                // most of the time, the dot span will be inside a non-call member expression.
                // however, if you start editing a function call, it is possible for the span
                // to end up here
                self.check_dot_completions(member.span, id, true);
                let Some(mut mfn) =
                    self.get_member_fn(id, member.data, &generics, span, self.current)
                else {
                    bail!(
                        self,
                        Error::no_method(&type_name!(self, id), strdata!(self, member.data), span)
                    );
                };
                self.check_hover(member.span, LspItem::Fn(mfn.func.id, None));
                if mfn.typ.is_dynamic() && !self.proj.scopes.get(mfn.func.id).type_params.is_empty()
                {
                    self.error(Error::new(
                        "cannot call generic functions through a dynamic pointer",
                        span,
                    ))
                }

                let f = self.proj.scopes.get(mfn.func.id);
                if !mfn.public && !self.can_access_privates(mfn.owner) {
                    report_error!(
                        self,
                        span,
                        "cannot access private method '{}' of type '{}'",
                        strdata!(self, self.proj.scopes.get(mfn.func.id).name.data),
                        type_name!(self, id),
                    )
                }

                let Some(this_param) = f.params.first().filter(|p| p.label == Strings::THIS_PARAM)
                else {
                    return report_error!(
                        self,
                        span,
                        "associated function '{}' cannot be used as a method",
                        strdata!(self, member.data)
                    );
                };

                if mfn.typ.is_dynamic() && !self.proj.types[this_param.ty].is_safe_ptr() {
                    return report_error!(
                        self,
                        span,
                        "cannot call method '{}' which takes this by value through a dynamic pointer",
                        strdata!(self, member.data)
                    );
                }

                let this_param_ty = this_param.ty;
                if self.proj.types[this_param_ty].is_mut_ptr() {
                    if !matches!(
                        self.proj.types[recv.ty],
                        Type::Ptr(_) | Type::MutPtr(_) | Type::DynPtr(_) | Type::DynMutPtr(_)
                    ) && !recv.can_addrmut(&self.proj.scopes, &self.proj.types)
                    {
                        report_error!(
                            self,
                            span,
                            "cannot call method '{}' with immutable receiver",
                            strdata!(self, member.data)
                        )
                    }

                    let mut ty = &self.proj.types[recv.ty];
                    while let Type::MutPtr(inner) = ty {
                        ty = &self.proj.types[*inner];
                    }

                    if matches!(ty, Type::Ptr(_) | Type::DynPtr(_)) {
                        report_error!(
                            self,
                            span,
                            "cannot call method '{}' through an immutable pointer",
                            strdata!(self, member.data)
                        )
                    }

                    if matches!(&recv.data, CExprData::Member { source, .. } if source.ty.is_packed_struct(&self.proj))
                    {
                        self.proj.diag.report(Warning::call_mutating_on_bitfield(span))
                    }
                }

                let recv = recv.auto_deref(&mut self.proj.types, this_param_ty);
                let (args, ret, _) =
                    self.check_fn_args(&mut mfn.func, Some(recv), args, target, span);
                if mfn.typ.is_dynamic() {
                    return CExpr::new(ret, CExprData::CallDyn(mfn.func, args));
                } else {
                    return CExpr::new(
                        ret,
                        CExprData::member_call(&mut self.proj.types, mfn, args, self.current, span),
                    );
                }
            }
            PExprData::Path(ref path) => match self.resolve_value_path(path, target) {
                ResolvedValue::UnionConstructor(ut) => {
                    return self.check_unsafe_union_constructor(target, ut, args, span);
                }
                ResolvedValue::Fn(func) => {
                    let span = path.components.last().map(|c| c.0.span).unwrap_or(span);
                    return self.check_known_fn_call(func, args, target, span);
                }
                ResolvedValue::MemberFn(mut mfn) => {
                    let span = path.components.last().map(|c| c.0.span).unwrap_or(span);
                    let f = self.proj.scopes.get(mfn.func.id);
                    if let Some(id) = f.constructor {
                        let ut = self.proj.scopes.get(id);
                        for &id in ut.type_params.iter() {
                            mfn.func.ty_args.entry(id).or_insert(TypeId::UNKNOWN);
                        }

                        if ut.is_empty_variant(self.proj.scopes.get(mfn.func.id).name.data) {
                            return self.error(Error::expected_found(
                                "function",
                                &format!("union variant '{}'", strdata!(self, f.name.data)),
                                span,
                            ));
                        }
                    }

                    let (args, ret, _) =
                        self.check_fn_args(&mut mfn.func, None, args, target, span);
                    if mfn.typ.is_dynamic() {
                        return CExpr::new(ret, CExprData::CallDyn(mfn.func, args));
                    } else {
                        return CExpr::new(
                            ret,
                            CExprData::member_call(
                                &mut self.proj.types,
                                mfn,
                                args,
                                self.current,
                                span,
                            ),
                        );
                    }
                }
                ResolvedValue::NotFound(err) => {
                    return named_error!(self, Error::no_symbol, err.data, err.span);
                }
                ResolvedValue::Error => return Default::default(),
                _ => {}
            },
            _ => {}
        }

        let span = callee.span;
        let callee = self.check_expr(callee, None);
        match &self.proj.types[callee.ty] {
            Type::Unknown => Default::default(),
            Type::Fn(func) => self.check_known_fn_call(func.clone(), args, target, span),
            Type::FnPtr(f) => {
                let f = f.clone();
                let mut result = vec![];
                for (i, (name, arg)) in args.into_iter().enumerate() {
                    if let Some(&param) = f.params.get(i) {
                        if name.is_some() {
                            self.proj.diag.report(Error::new(
                                "keyword parameters are not allowed here",
                                arg.span,
                            ));
                        }

                        result.push(self.type_check(arg, param));
                    } else {
                        self.proj.diag.report(Error::new("too many positional arguments", span));
                        break;
                    }
                }

                if result.len() < f.params.len() {
                    self.error(Error::new("too few positional arguments", span))
                }

                CExpr::new(f.ret, CExprData::CallFnPtr(callee.into(), result))
            }
            _ => bail!(
                self,
                Error::expected_found(
                    "callable item",
                    &format!("'{}'", &type_name!(self, callee.ty)),
                    span,
                )
            ),
        }
    }

    fn check_known_fn_call(
        &mut self,
        mut func: GenericFn,
        args: CallArgs,
        target: Option<TypeId>,
        span: Span,
    ) -> CExpr {
        let f = self.proj.scopes.get(func.id);
        if let Some(id) = f.constructor {
            let ut = self.proj.scopes.get(id);
            for &id in ut.type_params.iter() {
                func.ty_args.entry(id).or_insert(TypeId::UNKNOWN);
            }

            if ut.is_empty_variant(self.proj.scopes.get(func.id).name.data) {
                let name = strdata!(self, f.name.data);
                return self.error(Error::expected_found(
                    "function",
                    &format!("union variant '{name}'"),
                    span,
                ));
            }
        }

        let (args, ret, _) = self.check_fn_args(&mut func, None, args, target, span);
        CExpr::new(ret, CExprData::call(&mut self.proj.types, func, args, self.current, span))
    }

    fn check_arg<T>(
        &mut self,
        func: &mut WithTypeArgs<T>,
        expr: PExpr,
        ty: TypeId,
    ) -> (CExpr, bool) {
        let mut target = ty.with_templates(&mut self.proj.types, &func.ty_args);
        let span = expr.span;
        let expr = self.check_expr(expr, Some(target));
        if !func.ty_args.is_empty() {
            func.infer_type_args(&self.proj.types, ty, expr.ty);
            target = target.with_templates(&mut self.proj.types, &func.ty_args);
        }

        match self.coerce(expr, target) {
            Ok(expr) => (expr, false),
            Err(expr) => {
                self.proj.diag.report(type_mismatch_err!(self, target, expr.ty, span));
                (Default::default(), true)
            }
        }
    }

    fn check_fn_args(
        &mut self,
        func: &mut GenericFn,
        recv: Option<CExpr>,
        args: CallArgs,
        target: Option<TypeId>,
        span: Span,
    ) -> (IndexMap<StrId, CExpr>, TypeId, bool) {
        self.resolve_proto(func.id);

        let unknowns: HashSet<_> = func
            .ty_args
            .iter()
            .filter_map(|(&id, &ty)| (ty == TypeId::UNKNOWN).then_some(id))
            .collect();
        if let Some(target) = target {
            func.infer_type_args(&self.proj.types, self.proj.scopes.get(func.id).ret, target);
        }

        let mut result = IndexMap::with_capacity(args.len());
        let mut last_pos = 0;
        if let Some(recv) = recv {
            result.insert(Strings::THIS_PARAM, recv);
            last_pos += 1;
        }

        let variadic = self.proj.scopes.get(func.id).variadic;
        let mut num = 0;
        let mut failed = false;
        for (name, expr) in args {
            if let Some(name) = name {
                if result.contains_key(&name.data) {
                    failed = true;
                    report_error!(
                        self,
                        name.span,
                        "duplicate arguments for for parameter '{}'",
                        strdata!(self, name.data)
                    )
                }

                if let Some(param) =
                    self.proj.scopes.get(func.id).params.iter().find(|p| p.label == name.data)
                {
                    let ty = param.ty;
                    self.check_arg_label_hover(name.span, param.clone(), func);

                    let (expr, f) = self.check_arg(func, expr, ty);
                    result.insert(name.data, expr);
                    failed = failed || f;
                } else {
                    failed = true;
                    let _: () = report_error!(
                        self,
                        name.span,
                        "unknown parameter: '{}'",
                        strdata!(self, name.data)
                    );
                    self.check_expr(expr, None);
                }
            } else if let Some((i, param)) = self
                .proj
                .scopes
                .get(func.id)
                .params
                .iter()
                .enumerate()
                .skip(last_pos)
                .find(|(_, param)| !param.keyword && !result.contains_key(&param.label))
            {
                let name = param.label;
                let (expr, f) = self.check_arg(func, expr, param.ty);
                result.insert(name, expr);
                failed = failed || f;
                last_pos = i + 1;
            } else if !variadic {
                failed = true;
                // TODO: a better error here would be nice
                self.error(Error::new("too many positional arguments", expr.span))
            } else {
                num += 1;
                result.insert(intern!(self, "${num}"), self.check_expr(expr, None));
            }
        }

        for param in self
            .proj
            .scopes
            .get(func.id)
            .params
            .iter()
            .filter(|p| !result.contains_key(&p.label))
            .collect::<Vec<_>>()
        {
            if let Some(DefaultExpr::Checked(expr)) = &param.default {
                result.insert(param.label, expr.clone_at(self.current, span));
            }
        }

        if self.proj.scopes.get(func.id).params.len() > result.len() {
            let mut missing = String::new();
            for param in self
                .proj
                .scopes
                .get(func.id)
                .params
                .iter()
                .filter(|p| !result.contains_key(&p.label))
            {
                if !missing.is_empty() {
                    missing.push_str(", ");
                }

                missing.push_str(strdata!(self, param.label));
            }

            failed = true;
            self.error(Error::new(
                format!(
                    "expected {} argument(s), found {} (missing {missing})",
                    self.proj.scopes.get(func.id).params.len(),
                    result.len()
                ),
                span,
            ))
        }

        let f = self.check_bounds_filtered(func, &unknowns, span);
        failed = failed || f;
        if self.proj.scopes.get(func.id).is_unsafe {
            check_unsafe!(self, Error::is_unsafe(span));
        }

        (
            result,
            self.proj.scopes.get(func.id).ret.with_templates(&mut self.proj.types, &func.ty_args),
            failed,
        )
    }

    fn check_bounds_filtered(
        &mut self,
        func: &GenericFn,
        unknowns: &HashSet<UserTypeId>,
        span: Span,
    ) -> bool {
        let mut failed = false;
        for (&id, &ty) in func.ty_args.iter().filter(|(id, _)| unknowns.contains(id)) {
            if ty == TypeId::UNKNOWN {
                failed = true;
                report_error!(
                    self,
                    span,
                    "cannot infer type for type parameter '{}'",
                    strdata!(self, self.proj.scopes.get(id).name.data)
                )
            } else {
                let f = self.check_bounds(
                    &func.ty_args,
                    ty,
                    self.proj.scopes.get(id).impls.clone(),
                    span,
                );
                failed = failed || f;
            }
        }
        failed
    }

    fn check_bounds(
        &mut self,
        ty_args: &TypeArgs,
        ty: TypeId,
        bounds: Vec<TraitImpl>,
        span: Span,
    ) -> bool {
        let mut failed = false;
        for mut bound in bounds.into_iter().flat_map(|bound| bound.into_checked()) {
            self.resolve_impls(bound.id);
            bound.fill_templates(&mut self.proj.types, ty_args);

            if !self.implements_trait(ty, &bound) {
                failed = true;
                self.proj.diag.report(Error::doesnt_implement(
                    &type_name!(self, ty),
                    &type_name!(self, bound),
                    span,
                ));
            }
        }
        failed
    }

    fn check_block(&mut self, body: Vec<PStmt>) -> Vec<CStmt> {
        // TODO: do this in forward decl pass
        let declared: Vec<_> =
            body.into_iter().map(|stmt| self.declare_stmt(&mut vec![], stmt)).collect();

        self.enter_id_and_resolve(self.current, |this| {
            declared.into_iter().map(|stmt| this.check_stmt(stmt)).collect()
        })
    }

    fn create_block(&mut self, body: Vec<PStmt>, kind: ScopeKind) -> Block {
        self.create_block_with_init(body, kind, |_| {})
    }

    fn create_block_with_init(
        &mut self,
        body: Vec<PStmt>,
        kind: ScopeKind,
        init: impl FnOnce(&mut Self),
    ) -> Block {
        self.enter(kind, |this| {
            init(this);
            Block { body: this.check_block(body), scope: this.current }
        })
    }

    fn type_check(&mut self, expr: PExpr, target: TypeId) -> CExpr {
        let span = expr.span;
        let source = self.check_expr(expr, Some(target));
        self.type_check_checked(source, target, span)
    }

    fn type_check_checked(&mut self, source: CExpr, target: TypeId, span: Span) -> CExpr {
        match self.coerce(source, target) {
            Ok(expr) => expr,
            Err(expr) => bail!(self, type_mismatch_err!(self, target, expr.ty, span)),
        }
    }

    fn resolve_lang_type(
        &mut self,
        name: &'static str,
        args: &[Located<TypeHint>],
        span: Span,
    ) -> TypeId {
        let name_id = self.proj.strings.get_or_intern_static(name);
        if let Some(id) = self.proj.scopes.lang_types.get(&name_id).copied() {
            let ty_args = self.resolve_type_args(id, args, true, span);
            self.proj.types.insert(Type::User(GenericUserType::new(id, ty_args)))
        } else {
            self.error(Error::no_lang_item(name, span))
        }
    }

    fn make_lang_type(
        &mut self,
        id: UserTypeId,
        args: impl IntoIterator<Item = TypeId>,
        span: Span,
    ) -> TypeId {
        let ty = GenericUserType::from_type_args(&self.proj.scopes, id, args);
        for (&id, &param) in ty.ty_args.iter() {
            self.resolve_impls(id);
            self.check_bounds(&ty.ty_args, param, self.proj.scopes.get(id).impls.clone(), span);
        }
        self.proj.types.insert(Type::User(ty))
    }

    fn make_lang_type_by_name(
        &mut self,
        name: &'static str,
        args: impl IntoIterator<Item = TypeId>,
        span: Span,
    ) -> TypeId {
        let id = self.proj.strings.get_or_intern_static(name);
        let Some(id) = self.proj.scopes.lang_types.get(&id).copied() else {
            return self.error(Error::no_lang_item(name, span));
        };

        self.proj.types.insert(Type::User(GenericUserType::from_type_args(
            &self.proj.scopes,
            id,
            args,
        )))
    }

    fn get_lang_type_or_err(&mut self, name: &'static str, span: Span) -> Option<UserTypeId> {
        let id = self.proj.strings.get_or_intern_static(name);
        let Some(id) = self.proj.scopes.lang_types.get(&id).copied() else {
            return self.error(Error::no_lang_item(name, span));
        };

        Some(id)
    }

    fn resolve_dyn_ptr(&mut self, path: &Path) -> Option<GenericTrait> {
        match self.resolve_type_path(path) {
            ResolvedType::UserType(ut) => {
                if self.proj.scopes.get(ut.id).kind.is_trait() {
                    Some(ut)
                } else {
                    bail!(
                        self,
                        Error::expected_found(
                            "trait",
                            &format!("type '{}'", type_name!(self, ut)),
                            path.final_component_span(),
                        )
                    )
                }
            }
            ResolvedType::Builtin(ty) => bail!(
                self,
                Error::expected_found(
                    "trait",
                    &format!("type '{}'", type_name!(self, ty)),
                    path.final_component_span(),
                )
            ),
            ResolvedType::Error => None,
        }
    }

    fn resolve_typehint(&mut self, hint: &Located<TypeHint>) -> TypeId {
        let mut create_ptr = |init: fn(TypeId) -> Type, hint| {
            let ty = self.resolve_typehint(hint);
            self.proj.types.insert(init(ty))
        };

        let span = hint.span;
        match &hint.data {
            TypeHint::Regular(path) => match self.resolve_type_path(path) {
                ResolvedType::Builtin(ty) => ty,
                ResolvedType::UserType(ut) => {
                    if !self.proj.scopes.get(ut.id).kind.is_trait() {
                        self.proj.types.insert(Type::User(ut))
                    } else {
                        bail!(
                            self,
                            Error::expected_found(
                                "type",
                                &format!("trait '{}'", type_name!(self, ut)),
                                path.final_component_span(),
                            )
                        )
                    }
                }
                ResolvedType::Error => TypeId::UNKNOWN,
            },
            TypeHint::Void => TypeId::VOID,
            TypeHint::Ptr(ty) => create_ptr(Type::Ptr, ty),
            TypeHint::MutPtr(ty) => create_ptr(Type::MutPtr, ty),
            TypeHint::RawPtr(ty) => create_ptr(Type::RawPtr, ty),
            TypeHint::RawMutPtr(ty) => create_ptr(Type::RawMutPtr, ty),
            TypeHint::DynPtr(path) => self
                .resolve_dyn_ptr(path)
                .map(|tr| self.proj.types.insert(Type::DynPtr(tr)))
                .unwrap_or_default(),
            TypeHint::DynMutPtr(path) => self
                .resolve_dyn_ptr(path)
                .map(|tr| self.proj.types.insert(Type::DynMutPtr(tr)))
                .unwrap_or_default(),
            TypeHint::This => {
                let current = self.current_function();
                for (_, scope) in self.proj.scopes.walk(self.current) {
                    match scope.kind {
                        ScopeKind::UserType(id) => {
                            check_hover!(self, span, id.into());
                            match &self.proj.scopes.get(id).kind {
                                &UserTypeKind::Trait(this, _) => {
                                    let ut = GenericUserType::from_id(
                                        &self.proj.scopes,
                                        &mut self.proj.types,
                                        this,
                                    );
                                    return self.proj.types.insert(Type::User(ut));
                                }
                                UserTypeKind::Extension(ty) => return *ty,
                                _ => {
                                    let ut = GenericUserType::from_id(
                                        &self.proj.scopes,
                                        &mut self.proj.types,
                                        id,
                                    );
                                    return self.proj.types.insert(Type::User(ut));
                                }
                            }
                        }
                        ScopeKind::Function(f) if Some(f) != current => break,
                        _ => {}
                    }
                }
                self.error(Error::new(format!("'{THIS_TYPE}' outside of type"), hint.span))
            }
            TypeHint::Array(ty, count) => {
                let id = self.resolve_typehint(ty);
                let Some(n) = self.consteval_check((**count).clone(), TypeId::USIZE) else {
                    return self.proj.types.insert(Type::Array(id, 0));
                };
                self.proj.types.insert(Type::Array(id, n.val.try_into().unwrap()))
            }
            TypeHint::Option(ty) => {
                self.resolve_lang_type("option", std::slice::from_ref(ty), span)
            }
            TypeHint::Vec(ty) => self.resolve_lang_type("vec", std::slice::from_ref(ty), span),
            TypeHint::Map(kv) => self.resolve_lang_type("map", &kv[..], span),
            TypeHint::Set(ty) => self.resolve_lang_type("set", std::slice::from_ref(ty), span),
            TypeHint::Slice(ty) => self.resolve_lang_type("span", std::slice::from_ref(ty), span),
            TypeHint::SliceMut(ty) => {
                self.resolve_lang_type("span_mut", std::slice::from_ref(ty), span)
            }
            TypeHint::Tuple(params) => {
                let params = params.iter().map(|p| self.resolve_typehint(p)).collect();
                self.proj.scopes.get_tuple(params, &mut self.proj.strings, &mut self.proj.types)
            }
            TypeHint::AnonStruct(params) => {
                let mut types = Vec::with_capacity(params.len());
                let mut names = Vec::with_capacity(params.len());
                for (name, ty) in params {
                    names.push(*name);
                    types.push(self.resolve_typehint(ty));
                }
                self.proj.scopes.get_anon_struct(
                    names,
                    types,
                    &mut self.proj.strings,
                    &mut self.proj.types,
                )
            }
            TypeHint::Fn { is_extern, params, ret } => {
                let fnptr = FnPtr {
                    is_extern: *is_extern,
                    params: params.iter().map(|p| self.resolve_typehint(p)).collect(),
                    ret: ret.as_ref().map(|ret| self.resolve_typehint(ret)).unwrap_or(TypeId::VOID),
                };
                self.proj.types.insert(Type::FnPtr(fnptr))
            }
            TypeHint::Error => TypeId::UNKNOWN,
        }
    }

    fn resolve_members(&mut self, id: UserTypeId) {
        if self.proj.scopes.get(id).members_resolved {
            return;
        }

        for i in 0..self.proj.scopes.get(id).members.len() {
            resolve_type!(self, self.proj.scopes.get_mut(id).members[i].ty);
        }

        if let Some(mut union) = self.proj.scopes.get(id).kind.as_union().cloned() {
            resolve_type!(self, union.tag);
            for variant in union.variants.values_mut().flat_map(|v| &mut v.ty) {
                resolve_type!(self, *variant);
            }

            let mut target = None;
            let mut target_max = None;
            if let Some(stats) = union.tag.as_integral(&self.proj.types, false) {
                target_max = Some(stats.max());
                target = Some(union.tag);
            } else if union.tag != TypeId::UNKNOWN {
                self.error(Error::new(
                    "union tag must be an integer type",
                    self.proj.scopes.get(id).name.span,
                ))
            }

            let mut used = HashSet::new();
            let mut next = ComptimeInt::new(0);
            let mut bits = 0;
            let mut signed = false;
            for (name, variant) in union.variants.iter_mut() {
                let (val, span) = match std::mem::take(&mut variant.discrim) {
                    Discriminant::Unchecked(expr) => {
                        let span = expr.span;
                        if let Some(target) = target {
                            let Some(res) = self.consteval_check(expr, target) else {
                                continue;
                            };
                            (res.val, span)
                        } else {
                            let expr = self.check_expr(expr, None);
                            let Some(res) = self.consteval(&expr, span) else {
                                continue;
                            };

                            // TODO: we need some kind of "compile time int" type so we don't assume
                            // `int` for cases like union { Variant = 1 }, where we should pick the
                            // smallest tag possible as with empty variants
                            target = Some(res.ty);
                            (res.val, span)
                        }
                    }
                    Discriminant::Next => (next, variant.span),
                    _ => unreachable!(),
                };

                if !used.insert(val.clone()) {
                    report_error!(self, span, "duplicate assignment of discriminant '{val}'")
                }
                if target_max.as_ref().is_some_and(|v| &val > v) {
                    report_error!(
                        self,
                        span,
                        "integer overflow attempting to assign discriminant '{val}' for '{}'",
                        strdata!(self, name),
                    )
                }

                variant.discrim = Discriminant::Checked(val.clone());
                signed = signed || val.is_negative();
                bits = bits.max(val.bits());
                next = val + 1;
            }

            if union.tag == TypeId::UNKNOWN {
                union.tag = target.unwrap_or_else(|| {
                    self.proj.types.insert(if signed { Type::Int(bits) } else { Type::Uint(bits) })
                });
            }

            self.proj.scopes.get_mut(id).kind = UserTypeKind::Union(union);
        }

        self.proj.scopes.get_mut(id).members_resolved = true;
    }

    /// Returns `true` if the specified type is recursive
    fn resolve_dependencies(&mut self, id: UserTypeId, this: TypeId, mut canonical: bool) -> bool {
        match self.proj.deps.get(&this) {
            Some(Dependencies::Resolved(_)) => return false,
            Some(_) => return true,
            None => {}
        }

        self.proj.deps.insert(this, Dependencies::Resolving);
        let mut deps = Vec::new();
        let mut failed = false;

        if !canonical {
            let canonical_ty =
                Type::User(GenericUserType::from_id(&self.proj.scopes, &mut self.proj.types, id));
            canonical = this == self.proj.types.insert(canonical_ty);
        }

        macro_rules! check_ty {
            ($ty: expr, $err: expr) => {
                let ty = $ty;
                let ty = ty.with_ut_templates(&mut self.proj.types, this);
                if self.check_member_dep(ty, this, &mut deps) {
                    failed = true;
                    if canonical {
                        self.error($err)
                    }
                }
            };
        }

        for i in 0..self.proj.scopes.get(id).members.len() {
            check_ty!(
                self.proj.scopes.get(id).members[i].ty,
                Error::recursive_type(
                    strdata!(self, self.proj.scopes.get(id).members.get_index(i).unwrap().0),
                    self.proj.scopes.get(id).members[i].span,
                    false,
                )
            );
        }

        if let Some(union) = self.proj.scopes.get(id).kind.as_union().cloned() {
            check_ty!(
                union.tag,
                Error::new(
                    "union tag makes this struct recursive",
                    self.proj.scopes.get(id).name.span
                )
            );
            for (name, var) in union.variants.iter() {
                if let Some(ty) = var.ty {
                    check_ty!(ty, Error::recursive_type(strdata!(self, name), var.span, true));
                }
            }
        }

        if let Some(mut kind) = self.proj.scopes.get(id).kind.as_packed_struct().cloned() {
            let mut bits = 0;
            for (&name, mem) in self.proj.scopes.get(id).members.iter() {
                kind.bit_offsets.insert(name, bits);
                // TODO:
                // - allow ^mut
                // - nested packed structs
                match mem.ty.bit_size(&self.proj.scopes, &self.proj.types) {
                    BitSizeResult::Tag(_, n) | BitSizeResult::Size(n) => bits += n,
                    BitSizeResult::NonEnum => {
                        named_error!(self, Error::bitfield_member, name, mem.span)
                    }
                    BitSizeResult::Bad => {
                        named_error!(self, Error::bitfield_member, name, mem.span)
                    }
                }
            }

            const MAX_ALIGN_BITS: u32 = crate::typeid::MAX_ALIGN as u32 * 8;
            if bits <= MAX_ALIGN_BITS {
                kind.size = crate::nearest_pow_of_two(bits) / 8;
            } else {
                kind.size = ((((bits / MAX_ALIGN_BITS) + 1) * MAX_ALIGN_BITS) / 8) as usize;
            }
            kind.align = kind.size.clamp(1, crate::typeid::MAX_ALIGN);

            self.proj.scopes.get_mut(id).kind = UserTypeKind::PackedStruct(kind);
        }

        if failed {
            self.proj.deps.insert(this, Dependencies::Recursive);
            if canonical {
                self.proj.scopes.get_mut(id).recursive = true;
            }
        } else {
            self.proj.deps.insert(this, Dependencies::Resolved(deps));
        }

        failed
    }

    fn check_member_dep(&mut self, mut this: TypeId, ut: TypeId, deps: &mut Vec<TypeId>) -> bool {
        while let &Type::Array(inner, _) = &self.proj.types[this] {
            this = inner;
        }
        if ut == this {
            return true;
        }

        if let Type::User(dep) = &self.proj.types[this] {
            deps.push(this);

            let dep_id = dep.id;
            self.resolve_members(dep_id);
            if self.resolve_dependencies(dep_id, this, false) {
                return true;
            }
            if let Some(Dependencies::Resolved(member_deps)) = self.proj.deps.get(&this) {
                if member_deps.contains(&ut) {
                    return true;
                }

                deps.extend(member_deps);
            }
        }

        false
    }

    fn check_trait_dependencies(&mut self, id: UserTypeId) -> bool {
        self.resolve_impls(id);

        match self.proj.trait_deps.get(&id) {
            Some(Dependencies::Resolved(_)) => return false,
            Some(_) => return true,
            None => {}
        }

        self.proj.trait_deps.insert(id, Dependencies::Resolving);

        let mut deps = Vec::new();
        let mut failed = false;
        for i in 0..self.proj.scopes.get(id).impls.iter().len() {
            if let Some(checked) = self.proj.scopes.get(id).impls[i].as_checked() {
                deps.push(checked.id);

                let checked_id = checked.id;
                if self.check_trait_dependencies(checked_id) {
                    failed = true;
                }
                if let Some(Dependencies::Resolved(res)) = self.proj.trait_deps.get(&checked_id) {
                    deps.extend(res.iter());
                }
            }
        }

        if failed {
            self.proj.trait_deps.insert(id, Dependencies::Recursive);
            true
        } else {
            self.proj.trait_deps.insert(id, Dependencies::Resolved(deps));
            false
        }
    }

    fn resolve_impls(&mut self, id: UserTypeId) {
        for i in 0..self.proj.scopes.get(id).type_params.len() {
            self.resolve_impls(self.proj.scopes.get(id).type_params[i]);
        }

        for i in 0..self.proj.scopes.get(id).impls.len() {
            resolve_impl!(self, self.proj.scopes.get_mut(id).impls[i]);
        }
    }

    fn resolve_impls_recursive(&mut self, id: UserTypeId) {
        for i in 0..self.proj.scopes.get(id).impls.len() {
            resolve_impl!(self, self.proj.scopes.get_mut(id).impls[i]);
            if let Some(id) = self.proj.scopes.get_mut(id).impls[i].as_checked().map(|tr| tr.id) {
                self.resolve_impls(id);
            }
        }
    }

    fn resolve_proto(&mut self, id: FunctionId) {
        // disable errors to avoid duplicate errors when the struct and the constructor
        // are typechecked
        mute_errors!(self, self.proj.scopes.get(id).constructor.is_none(), {
            for i in 0..self.proj.scopes.get(id).params.len() {
                let target = resolve_type!(self, self.proj.scopes.get_mut(id).params[i].ty);
                match std::mem::take(&mut self.proj.scopes.get_mut(id).params[i].default) {
                    Some(DefaultExpr::Unchecked(scope, expr)) => mute_errors!(self, true, {
                        self.enter_id_and_resolve(scope, |this| {
                            this.proj.scopes.get_mut(id).params[i].default =
                                Some(DefaultExpr::Checked(this.type_check(expr, target)));
                        });
                    }),
                    other => self.proj.scopes.get_mut(id).params[i].default = other,
                }
            }

            resolve_type!(self, self.proj.scopes.get_mut(id).ret);

            for i in 0..self.proj.scopes.get(id).type_params.len() {
                self.resolve_impls(self.proj.scopes.get(id).type_params[i]);
            }
        });
    }

    fn is_impl_usable(
        &mut self,
        ut: UserTypeId,
        idx: usize,
        tr: &mut GenericTrait,
        bound: &GenericTrait,
    ) -> Option<TypeArgs> {
        if let Some(block) = self.proj.scopes.get(ut).impl_blocks.get(idx) {
            let mut ty_args = TypeArgs::unknown(block);
            if ty_args.is_empty() {
                return Some(ty_args);
            } else if bound.id != tr.id {
                return None;
            }

            // trait Trait<T>
            // impl<X> Trait<X>

            // bound:  Trait<T = int>
            // tr:     Trait<T = X>

            // ty_args: [X = int]
            for (arg, val) in tr.ty_args.iter_mut() {
                let Type::User(ut) = &self.proj.types[*val] else {
                    return None;
                };

                ty_args.insert(ut.id, bound.ty_args[arg]);
                *val = bound.ty_args[arg];
            }

            let mut good = true;
            mute_errors!(self, {
                for (&arg, &val) in ty_args.iter() {
                    if val == TypeId::UNKNOWN {
                        continue;
                    }

                    let bounds = self.proj.scopes.get(arg).impls.clone();
                    if self.check_bounds(&ty_args, val, bounds, Span::default()) {
                        good = false;
                        break;
                    }
                }
            });

            good.then_some(ty_args)
        } else {
            Some(Default::default())
        }
    }

    fn has_direct_impl(&mut self, ut: &GenericUserType, bound: &GenericTrait) -> bool {
        self.resolve_impls_recursive(ut.id);
        for i in 0..self.proj.scopes.get(ut.id).impls.len() {
            let Some(mut tr) = self.proj.scopes.get(ut.id).impls[i].as_checked().cloned() else {
                continue;
            };

            if self.is_impl_usable(ut.id, i, &mut tr, bound).is_none() {
                continue;
            }

            for mut tr in self
                .proj
                .scopes
                .get_trait_impls_ex(&mut self.proj.types, tr)
                .into_iter()
                .filter(|tr| tr.id == bound.id)
            {
                tr.fill_templates(&mut self.proj.types, &ut.ty_args);
                if &tr == bound {
                    return true;
                }
            }
        }
        false
    }

    fn implements_trait(&mut self, ty: TypeId, bound: &GenericTrait) -> bool {
        if ty == TypeId::UNKNOWN || self.proj.scopes.has_builtin_impl(&self.proj.types, ty, bound) {
            return true;
        }

        if let Type::User(ut) = &self.proj.types[ty]
            && self.has_direct_impl(&ut.clone(), bound)
        {
            return true;
        }

        for ext in self.extensions_in_scope_for(ty, self.current) {
            if self.has_direct_impl(&ext, bound) {
                return true;
            }
        }

        false
    }

    fn extensions_in_scope_for(&mut self, ty: TypeId, scope: ScopeId) -> Vec<GenericExtension> {
        fn implements_trait(
            this: &mut TypeChecker,
            ty: TypeId,
            bound: &GenericTrait,
            ignore: &HashSet<ExtensionId>,
            exts: &[ExtensionId],
            cache: &mut [HashMap<TypeId, Option<GenericExtension>>],
        ) -> bool {
            if this.proj.scopes.has_builtin_impl(&this.proj.types, ty, bound) {
                return true;
            }

            if let Type::User(ut) = &this.proj.types[ty]
                && this.has_direct_impl(&ut.clone(), bound)
            {
                return true;
            }

            for (i, &ext) in exts.iter().enumerate() {
                if ignore.contains(&ext) {
                    continue;
                }

                let ext = match cache[i].get(&ty) {
                    Some(ext) => ext.as_ref(),
                    None => {
                        let ext = applies_to(this, ty, ext, ignore, exts, cache);
                        cache[i].entry(ty).or_insert(ext).as_ref()
                    }
                };

                if ext.is_some_and(|ext| this.has_direct_impl(ext, bound)) {
                    return true;
                }
            }

            false
        }

        fn applies_to(
            this: &mut TypeChecker,
            ty: TypeId,
            ext: ExtensionId,
            ignore: &HashSet<ExtensionId>,
            exts: &[ExtensionId],
            cache: &mut [HashMap<TypeId, Option<GenericExtension>>],
        ) -> Option<GenericExtension> {
            let ext_ty_id = resolve_type!(
                this,
                *this.proj.scopes.get_mut(ext).kind.as_extension_mut().unwrap()
            );
            this.resolve_impls(ext);

            let mut ext = GenericExtension::from_id_unknown(&this.proj.scopes, ext);
            ext.infer_type_args(&this.proj.types, ext_ty_id, ty);
            if ext_ty_id.with_templates(&mut this.proj.types, &ext.ty_args) != ty {
                return None;
            }

            let mut ignore = ignore.clone();
            ignore.insert(ext.id);
            for (&id, &arg) in ext.ty_args.iter() {
                if arg == TypeId::UNKNOWN {
                    return None;
                }
                let bounds = this.proj.scopes.get(id).impls.clone();
                for mut bound in bounds.into_iter().flat_map(TraitImpl::into_checked) {
                    bound.fill_templates(&mut this.proj.types, &ext.ty_args);
                    if !implements_trait(this, arg, &bound, &ignore, exts, cache) {
                        return None;
                    }
                }
            }

            Some(ext)
        }

        if ty == TypeId::UNKNOWN {
            return vec![];
        }

        fn get_tns_extensions<'a>(
            scopes: &'a Scopes,
            tns: &'a HashMap<StrId, Vis<TypeItem>>,
        ) -> impl Iterator<Item = UserTypeId> + 'a {
            tns.iter().flat_map(|s| {
                s.1.as_type().filter(|&&id| scopes.get(id).kind.is_extension()).cloned()
            })
        }

        let mut exts = vec![];
        for (_, scope) in self.proj.scopes.walk(scope) {
            exts.extend(get_tns_extensions(&self.proj.scopes, &scope.tns));
            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        exts.extend(get_tns_extensions(&self.proj.scopes, &self.proj.scopes.autouse_tns));

        let mut cache = vec![HashMap::new(); exts.len()];
        for (i, &id) in exts.iter().enumerate() {
            // FIXME: by unconditionally calling applies_to we are duplicating work, since we may
            // have already checked it. But we can't just blindly trust the contents of the cache
            // if they are negative due to the `ignores` that prevents infinite recursion

            // in fact, the cache may need to be per-call (defined in this loop) to be
            // 100% correct, but I'm not sure at the moment. That works but has a significant effect
            // on performance. If something stupid is happening with extensions, try here first
            let ext = applies_to(self, ty, id, &HashSet::new(), &exts, &mut cache);
            cache[i].insert(ty, ext);
        }

        cache.into_iter().flat_map(|mut map| map.remove(&ty).flatten()).collect()
    }

    fn search(scopes: &Scopes, id: UserTypeId, method: StrId) -> Option<Vis<FunctionId>> {
        scopes.get(id).fns.iter().find(|&&id| scopes.get(*id).name.data == method).copied()
    }

    pub(crate) fn get_member_fn_ex(
        &mut self,
        inst: TypeId,
        wanted_tr: Option<&GenericTrait>,
        method: StrId,
        scope: ScopeId,
        finish: impl FnOnce(&mut Self, FunctionId) -> TypeArgs + Clone,
    ) -> Option<MemberFn> {
        fn finish_fn(
            this: &mut TypeChecker,
            inst: TypeId,
            wanted_tr: Option<&GenericTrait>,
            f: Vis<FunctionId>,
            ut: &GenericUserType,
            finish: impl FnOnce(&mut TypeChecker, FunctionId) -> TypeArgs + Clone,
        ) -> Option<MemberFn> {
            let scopes = &this.proj.scopes;
            let src_scope = scopes.get(ut.id).scope;

            let scope = scopes.get(f.id).scope;
            let mut impl_block = if let Some((impl_i, impl_ut)) = scopes[scope]
                .kind
                .as_impl()
                .cloned()
                .zip(scopes[scopes[scope].parent?].kind.as_user_type().cloned())
            {
                resolve_impl!(this, this.proj.scopes.get_mut(impl_ut).impls[impl_i]);
                if let Some(mut imp) =
                    this.proj.scopes.get(impl_ut).impls[impl_i].as_checked().cloned()
                {
                    imp.fill_templates(&mut this.proj.types, &ut.ty_args);
                    Some((impl_i, impl_ut, imp))
                } else {
                    None
                }
            } else {
                None
            };
            let imp_ty_args = if let Some(wanted_tr) = wanted_tr {
                let (impl_i, impl_ut, imp) = impl_block.as_mut()?;
                let ty_args = this.is_impl_usable(*impl_ut, *impl_i, imp, wanted_tr)?;
                if imp != wanted_tr {
                    return None;
                }

                Some(ty_args)
            } else {
                None
            };

            let mut func = GenericFn::new(f.id, finish(this, f.id));
            func.ty_args.copy_args(&ut.ty_args);
            if let Some(imp_ty_args) = imp_ty_args {
                func.ty_args.copy_args(&imp_ty_args);
            } else if let Some((i, ut, _)) = impl_block {
                func.ty_args
                    .copy_args(&TypeArgs::unknown(&this.proj.scopes.get(ut).impl_blocks[i]));
            }

            Some(MemberFn {
                func,
                owner: src_scope,
                typ: MemberFnType::Normal,
                public: f.public,
                inst,
            })
        }

        fn search_impls<const IS_EXT: bool>(
            this: &mut TypeChecker,
            inst: TypeId,
            wanted_tr: Option<&GenericTrait>,
            method: StrId,
            ut: &GenericUserType,
            finish: impl FnOnce(&mut TypeChecker, FunctionId) -> TypeArgs + Clone,
        ) -> Option<MemberFn> {
            let src_scope = this.proj.scopes.get(ut.id).scope;
            for tr in this
                .proj
                .scopes
                .get(ut.id)
                .impls
                .clone()
                .into_iter()
                .flat_map(|ut| ut.into_checked())
            {
                for imp in this.proj.scopes.get_trait_impls_ex(&mut this.proj.types, tr) {
                    if wanted_tr.is_some_and(|id| id != &imp) {
                        continue;
                    }

                    let Some(f) = TypeChecker::search(&this.proj.scopes, imp.id, method) else {
                        continue;
                    };

                    let ty_args = imp.ty_args.clone();
                    let mut func = GenericFn::new(f.id, finish(this, f.id));
                    if IS_EXT && let Type::User(ut) = &this.proj.types[inst] {
                        let ut = ut.clone();
                        func.ty_args.copy_args_with(&mut this.proj.types, &ty_args, &ut.ty_args);
                    }
                    func.ty_args.copy_args_with(&mut this.proj.types, &ty_args, &ut.ty_args);
                    func.ty_args
                        .insert(*this.proj.scopes.get(imp.id).kind.as_trait().unwrap().0, inst);
                    return Some(MemberFn {
                        func,
                        owner: src_scope,
                        typ: MemberFnType::Trait(imp),
                        public: f.public,
                        inst,
                    });
                }
            }

            None
        }

        // TODO: trait implement overload ie.
        // impl Eq<f32> { ... } impl Eq<i32> { ... }
        let ut = if let Type::User(ut) = &self.proj.types[inst] {
            let ut = ut.clone();
            if let Some(f) = Self::search(&self.proj.scopes, ut.id, method)
                && let Some(f) = finish_fn(self, inst, wanted_tr, f, &ut, finish.clone())
            {
                return Some(f);
            }
            Some(ut)
        } else if let Some(tr) = self.proj.types[inst].as_dyn_pointee() {
            // TODO: wanted_tr
            let tr = tr.clone();
            self.resolve_impls_recursive(tr.id);
            let data = self.proj.scopes.get(tr.id);
            for imp in self.proj.scopes.get_trait_impls(tr.id) {
                if let Some(f) = Self::search(&self.proj.scopes, imp, method) {
                    let src_scope = data.scope;
                    let mut func = GenericFn::new(f.id, finish(self, f.id));
                    func.ty_args.copy_args(&tr.ty_args);
                    return Some(MemberFn {
                        func,
                        owner: src_scope,
                        typ: MemberFnType::Dynamic,
                        public: f.public,
                        inst,
                    });
                }
            }
            None
        } else {
            None
        };

        let exts = self.extensions_in_scope_for(inst, scope);
        if let Some(f) = exts.iter().find_map(|ext| {
            let f = Self::search(&self.proj.scopes, ext.id, method);
            f.and_then(|f| finish_fn(self, inst, wanted_tr, f, ext, finish.clone()))
        }) {
            return Some(f);
        }

        // look through trait impls AFTER exhausting all concrete functions
        if let Some(ut) = ut {
            self.resolve_impls_recursive(ut.id);
            if let Some(f) =
                search_impls::<false>(self, inst, wanted_tr, method, &ut, finish.clone())
            {
                return Some(f);
            }
        }

        exts.iter().find_map(|ext| {
            search_impls::<true>(self, inst, wanted_tr, method, ext, finish.clone())
        })
    }

    fn get_member_fn(
        &mut self,
        ty: TypeId,
        method: StrId,
        generics: &[Located<TypeHint>],
        span: Span,
        scope: ScopeId,
    ) -> Option<MemberFn> {
        self.get_member_fn_ex(ty, None, method, scope, |this, id| {
            this.resolve_type_args(id, generics, false, span)
        })
        .inspect(|memfn| self.resolve_proto(memfn.func.id))
    }

    fn get_member_fn_legacy(
        &mut self,
        inst: TypeId,
        wanted_tr: TraitId,
        method: StrId,
        scope: ScopeId,
    ) -> Option<MemberFn> {
        fn finish_fn(
            this: &mut TypeChecker,
            inst: TypeId,
            wanted_tr: TraitId,
            f: Vis<FunctionId>,
            ut: &GenericUserType,
        ) -> Option<MemberFn> {
            let scopes = &this.proj.scopes;
            let src_scope = scopes.get(ut.id).scope;

            let scope = scopes.get(f.id).scope;
            let &impl_i = scopes[scope].kind.as_impl()?;
            let &impl_ut = scopes[scopes[scope].parent?].kind.as_user_type()?;
            resolve_impl!(this, this.proj.scopes.get_mut(impl_ut).impls[impl_i]);
            let imp = &this.proj.scopes.get_mut(impl_ut).impls[impl_i];
            if !matches!(imp, TraitImpl::Checked(tr) if tr.id == wanted_tr) {
                return None;
            }

            let mut func = GenericFn::from_id_unknown(&this.proj.scopes, f.id);
            func.ty_args.copy_args(&ut.ty_args);
            func.ty_args
                .copy_args(&TypeArgs::unknown(&this.proj.scopes.get(impl_ut).impl_blocks[impl_i]));
            Some(MemberFn {
                func,
                owner: src_scope,
                typ: MemberFnType::Normal,
                public: f.public,
                inst,
            })
        }

        fn search_impls<const IS_EXT: bool>(
            this: &mut TypeChecker,
            inst: TypeId,
            wanted_tr: TraitId,
            method: StrId,
            ut: &GenericUserType,
        ) -> Option<MemberFn> {
            let src_scope = this.proj.scopes.get(ut.id).scope;
            for tr in this.proj.scopes.get(ut.id).impls.iter().flat_map(|ut| ut.as_checked()) {
                for imp in this.proj.scopes.get_trait_impls(tr.id) {
                    if wanted_tr != imp {
                        continue;
                    }

                    let Some(f) = TypeChecker::search(&this.proj.scopes, imp, method) else {
                        continue;
                    };

                    let ty_args = tr.ty_args.clone();
                    let mut func = GenericFn::from_id_unknown(&this.proj.scopes, f.id);
                    if IS_EXT && let Type::User(ut) = &this.proj.types[inst] {
                        let ut = ut.clone();
                        func.ty_args.copy_args_with(&mut this.proj.types, &ty_args, &ut.ty_args);
                    }
                    func.ty_args.copy_args_with(&mut this.proj.types, &ty_args, &ut.ty_args);
                    func.ty_args
                        .insert(*this.proj.scopes.get(imp).kind.as_trait().unwrap().0, inst);
                    return Some(MemberFn {
                        func,
                        owner: src_scope,
                        typ: MemberFnType::Trait(GenericTrait::from_id_unknown(
                            &this.proj.scopes,
                            imp,
                        )),
                        public: f.public,
                        inst,
                    });
                }
            }

            None
        }

        let ut = if let Type::User(ut) = &self.proj.types[inst] {
            let ut = ut.clone();
            if let Some(f) = Self::search(&self.proj.scopes, ut.id, method)
                && let Some(f) = finish_fn(self, inst, wanted_tr, f, &ut)
            {
                self.resolve_proto(f.func.id);
                return Some(f);
            }
            Some(ut)
        } else {
            // TODO: dyn pointers
            None
        };

        let exts = self.extensions_in_scope_for(inst, scope);
        if let Some(f) = exts.iter().find_map(|ext| {
            let f = Self::search(&self.proj.scopes, ext.id, method);
            f.and_then(|f| finish_fn(self, inst, wanted_tr, f, ext))
        }) {
            self.resolve_proto(f.func.id);
            return Some(f);
        }

        // look through trait impls AFTER exhausting all concrete functions
        if let Some(ut) = ut {
            self.resolve_impls_recursive(ut.id);
            if let Some(f) = search_impls::<false>(self, inst, wanted_tr, method, &ut) {
                self.resolve_proto(f.func.id);
                return Some(f);
            }
        }

        exts.iter().find_map(|ext| search_impls::<true>(self, inst, wanted_tr, method, ext))
    }

    fn get_int_type_and_val(
        &mut self,
        target: Option<TypeId>,
        IntPattern { negative, mut value, width }: IntPattern,
        span: Span,
    ) -> (TypeId, ComptimeInt) {
        let ty = if let Some(width) = width {
            let width = strdata!(self, width);
            if let Some(ty) = Type::from_int_name(width, false) {
                self.proj.types.insert(ty)
            } else {
                return self
                    .error(Error::new(format!("invalid integer literal type: {width}"), span));
            }
        } else {
            target
                .map(|target| target.strip_options(&self.proj))
                .filter(|&target| self.proj.types[target].is_integral())
                .unwrap_or(TypeId::ISIZE)
        };

        let stats = ty.as_integral(&self.proj.types, false).unwrap();
        if negative {
            if !stats.signed {
                self.proj.diag.report(Error::new(
                    format!("cannot negate unsigned integer type '{}'", type_name!(self, ty)),
                    span,
                ));
                return (ty, value);
            }
            value = -value;
        }

        let min = stats.min();
        let max = stats.max();
        if value > max || value < min {
            bail!(
                self,
                Error::new(
                    format!(
                        "integer literal does not fit in range for type '{}' (range is {min}..{max})",
                        type_name!(self, ty),
                    ),
                    span,
                )
            );
        }

        (ty, value)
    }

    fn loop_target(&self, target: Option<TypeId>, infinite: bool) -> Option<TypeId> {
        if infinite {
            target
        } else {
            target
                .and_then(|t| self.proj.types[t].as_user())
                .filter(|t| Some(t.id) == self.proj.scopes.get_option_id())
                .and_then(|target| target.first_type_arg())
        }
    }

    fn loop_out_type(&mut self, kind: LoopScopeKind, span: Span) -> (TypeId, bool) {
        if kind.infinite {
            match kind.breaks {
                LoopBreak::None => (TypeId::NEVER, false),
                _ => (kind.target.unwrap(), false),
            }
        } else {
            match kind.breaks {
                LoopBreak::WithValue => {
                    (self.make_lang_type_by_name("option", [(kind.target).unwrap()], span), true)
                }
                _ => (TypeId::VOID, false),
            }
        }
    }

    #[allow(clippy::result_large_err)]
    fn coerce(&mut self, mut expr: CExpr, target: TypeId) -> Result<CExpr, CExpr> {
        // TODO: This is cacheable by TypeId
        fn may_ptr_coerce(types: &Types, from: &Type, to: &Type) -> bool {
            match (from, to) {
                (Type::MutPtr(s), Type::Ptr(t) | Type::RawPtr(t) | Type::RawMutPtr(t))
                    if s == t =>
                {
                    true
                }
                (Type::Ptr(s), Type::RawPtr(t)) if s == t => true,
                (Type::RawMutPtr(s), Type::RawPtr(t)) if s == t => true,
                (
                    Type::MutPtr(s),
                    Type::RawPtr(t) | Type::RawMutPtr(t) | Type::MutPtr(t) | Type::Ptr(t),
                ) => may_ptr_coerce(types, &types[*s], &types[*t]),
                (Type::Ptr(s), Type::Ptr(t) | Type::RawPtr(t)) => {
                    may_ptr_coerce(types, &types[*s], &types[*t])
                }
                _ => false,
            }
        }

        match (&self.proj.types[expr.ty], &self.proj.types[target]) {
            (Type::Never, Type::Never) => Ok(expr),
            (Type::Never, _) => Ok(CExpr::new(target, CExprData::NeverCoerce(expr.into()))),
            (Type::Unknown, _) | (_, Type::Unknown) => {
                expr.ty = target;
                Ok(expr)
            }
            (Type::DynPtr(lhs), Type::DynPtr(rhs))
            | (Type::DynMutPtr(lhs), Type::DynMutPtr(rhs) | Type::DynPtr(rhs)) => {
                if lhs == rhs || self.has_direct_impl(&lhs.clone(), &rhs.clone()) {
                    Ok(CExpr::new(target, CExprData::DynCoerce(expr.into(), self.current)))
                } else {
                    Err(expr)
                }
            }
            (&Type::Ptr(lhs), Type::DynPtr(rhs)) => {
                if self.implements_trait(lhs, &rhs.clone()) {
                    Ok(CExpr::new(target, CExprData::DynCoerce(expr.into(), self.current)))
                } else {
                    Err(expr)
                }
            }
            (Type::MutPtr(lhs), Type::DynPtr(rhs) | Type::DynMutPtr(rhs)) => {
                if self.implements_trait(*lhs, &rhs.clone()) {
                    Ok(CExpr::new(target, CExprData::DynCoerce(expr.into(), self.current)))
                } else {
                    Err(expr)
                }
            }
            (Type::Fn(lhs), Type::FnPtr(rhs)) => {
                let rhs = rhs.clone();
                let fptr = lhs.clone().as_fn_ptr(&self.proj.scopes, &mut self.proj.types);
                if fptr == rhs {
                    Ok(CExpr::new(self.proj.types.insert(Type::FnPtr(fptr)), expr.data))
                } else {
                    Err(expr)
                }
            }
            (lhs, rhs) if may_ptr_coerce(&self.proj.types, lhs, rhs) => {
                expr.ty = target;
                Ok(expr)
            }
            (lhs, rhs) if lhs == rhs => Ok(expr),
            (_, rhs) => {
                if let Some(inner) = rhs.as_option_inner(&self.proj.scopes) {
                    match self.coerce(expr, inner) {
                        Ok(expr) => Ok(CExpr::new(
                            target,
                            CExprData::VariantInstance(
                                Strings::SOME,
                                [(Strings::TUPLE_ZERO, expr)].into(),
                            ),
                        )),
                        Err(expr) => Err(expr),
                    }
                } else if self.can_span_coerce(expr.ty, target).is_some() {
                    Ok(CExpr::new(target, CExprData::SpanMutCoerce(expr.into())))
                } else {
                    Err(expr)
                }
            }
        }
    }

    fn try_coerce(&mut self, expr: CExpr, target: TypeId) -> CExpr {
        match self.coerce(expr, target) {
            Ok(expr) => expr,
            Err(expr) => expr,
        }
    }

    fn can_span_coerce(&self, lhs: TypeId, rhs: TypeId) -> Option<()> {
        let span = *self.proj.scopes.lang_types.get(&Strings::LANG_SPAN)?;
        let span_mut = *self.proj.scopes.lang_types.get(&Strings::LANG_SPAN_MUT)?;
        let lhs = self.proj.types[lhs].as_user()?;
        let rhs = self.proj.types[rhs].as_user()?;
        if lhs.id == span_mut
            && rhs.id == span
            && lhs.ty_args.get_index(0)?.1 == rhs.ty_args.get_index(0)?.1
        {
            Some(())
        } else {
            None
        }
    }
}

/// Pattern matching routines
impl TypeChecker {
    fn insert_pattern_var(
        &mut self,
        typ: PatternType,
        mut name: Located<StrId>,
        ty: TypeId,
        mutable: bool,
        has_hint: bool,
    ) -> Option<VariableId> {
        let mut good = true;
        let mut no_redef = typ != PatternType::Regular;
        if name.data == Strings::UNDERSCORE {
            name.data = Strings::EMPTY;
            good = false;
            no_redef = false;
        }

        let id = self.insert(
            Variable {
                name,
                ty,
                mutable,
                unused: typ != PatternType::BodylessFn,
                param: typ != PatternType::Regular,
                has_hint,
                ..Default::default()
            },
            false,
            no_redef,
        );
        if self.current_expr == self.listening_expr {
            self.listening_vars.push(id);
        }
        good.then_some(id)
    }

    fn check_match_coverage<'a>(
        &mut self,
        ty: TypeId,
        mut patterns: impl Iterator<Item = &'a CPattern> + Clone,
        span: Span,
    ) {
        let ty = &self.proj.types[ty.strip_references(&self.proj.types)];
        if let Some((mut value, max)) = ty.as_integral(true).map(|int| (int.min(), int.max())) {
            'outer: while value <= max {
                if ty.is_char() && (0xd800u64.into()..=0xe000u64.into()).contains(&value) {
                    value = 0xe000u64.into();
                }

                for patt in patterns.clone() {
                    if patt.irrefutable {
                        return;
                    }

                    match &patt.data {
                        PatternData::Int(i) if i == &value => {
                            value += 1;
                            continue 'outer;
                        }
                        PatternData::IntRange(i) => {
                            if let Some(start) = &i.start {
                                if i.inclusive && &value >= start && value <= i.end {
                                    value = &i.end + 1;
                                    continue 'outer;
                                } else if !i.inclusive && &value >= start && value < i.end {
                                    value.clone_from(&i.end);
                                    continue 'outer;
                                }
                            } else if i.inclusive && value <= i.end {
                                value = &i.end + 1;
                                continue 'outer;
                            } else if !i.inclusive && value < i.end {
                                value.clone_from(&i.end);
                                continue 'outer;
                            }
                        }
                        _ => {}
                    }
                }

                return self.error(Error::match_statement("", span));
            }
        } else if ty.as_user().is_some_and(|ut| {
            Some(&ut.id) == self.proj.scopes.lang_types.get(&Strings::LANG_STRING)
        }) {
            if !patterns.any(|patt| patt.irrefutable) {
                self.error(Error::match_statement("", span))
            }
        } else if ty.as_user().is_some_and(|ut| {
            Some(&ut.id) == self.proj.scopes.lang_types.get(&Strings::LANG_SPAN)
                || Some(&ut.id) == self.proj.scopes.lang_types.get(&Strings::LANG_SPAN_MUT)
        }) {
            if !patterns.any(|patt| {
                patt.irrefutable
                    || match &patt.data {
                        PatternData::Span { patterns, rest, .. } => {
                            patterns.is_empty() && rest.is_some()
                        }
                        _ => false,
                    }
            }) {
                self.error(Error::match_statement("", span))
            }
        } else if let Some(union) =
            ty.as_user().and_then(|ut| self.proj.scopes.get(ut.id).kind.as_union())
        {
            let patt_applies = |patt: &PatternData, name: StrId| {
                patt.as_variant().is_some_and(|(sub, variant, _, _)| {
                    name == *variant && sub.as_ref().is_none_or(|sub| sub.irrefutable)
                })
            };

            let mut missing = vec![];
            'outer: for (&name, _) in union.variants.iter() {
                for patt in patterns.clone() {
                    if patt.irrefutable {
                        return;
                    } else if patt_applies(&patt.data, name)
                        || patt
                            .data
                            .as_or()
                            .is_some_and(|p| p.iter().any(|sub| patt_applies(&sub.data, name)))
                    {
                        continue 'outer;
                    }
                }

                missing.push(strdata!(self, name));
            }

            if !missing.is_empty() {
                self.error(Error::match_statement(
                    &format!("(missing variant(s) {})", missing.join(", ")),
                    span,
                ))
            }
        } else if !patterns.any(|patt| patt.irrefutable) {
            // covers struct/array/void
            self.error(Error::match_statement("", span))
        }
    }

    /// Returns `Ok(_)` if the path is a compatible union variant.
    /// Returns `Err(None)` if the path is a union variant, but of the wrong type.
    /// Returns `Err(Some(_))` if the path is not a union variant.
    fn get_union_variant(
        &mut self,
        scrutinee: TypeId,
        path: ResolvedValue,
        span: Span,
    ) -> Result<(Option<TypeId>, StrId), Option<Error>> {
        let stripped = scrutinee.strip_references(&self.proj.types);
        let Some(ut_id) = self
            .proj
            .types
            .get(stripped)
            .as_user()
            .filter(|ut| self.proj.scopes.get(ut.id).kind.is_union())
            .map(|ut| ut.id)
        else {
            return Err(Some(Error::new(
                format!("cannot use union pattern on type '{}'", type_name!(self, scrutinee)),
                span,
            )));
        };
        self.resolve_members(ut_id);
        let f = match path {
            ResolvedValue::Fn(f) => f,
            ResolvedValue::MemberFn(m) => m.func,
            ResolvedValue::UnionConstructor(ut) => {
                return Err(Some(Error::type_mismatch_s(
                    &type_name!(self, scrutinee),
                    &type_name!(self, ut),
                    span,
                )));
            }
            ResolvedValue::Var(id) => {
                return Err(Some(Error::expected_found(
                    &type_name!(self, scrutinee),
                    &format!("variable '{}'", strdata!(self, self.proj.scopes.get(id).name.data)),
                    span,
                )));
            }
            ResolvedValue::NotFound(err) => {
                return Err(Some(Error::no_symbol(strdata!(self, err.data), err.span)));
            }
            ResolvedValue::Error => return Err(None),
        };

        let f = self.proj.scopes.get(f.id);
        if f.constructor.is_some_and(|id| id == ut_id) {
            let variant = f.name.data;
            Ok((
                (self
                    .proj
                    .scopes
                    .get(ut_id)
                    .kind
                    .as_union()
                    .and_then(|union| union.variants.get(&variant).map(|v| v.ty))
                    .unwrap())
                .map(|ty| {
                    let inner = ty.with_ut_templates(&mut self.proj.types, stripped);
                    scrutinee.matched_inner_type(&mut self.proj.types, inner)
                }),
                variant,
            ))
        } else if f.constructor.is_some_and(|id| self.proj.scopes.get(id).kind.is_union()) {
            self.proj.diag.report(Error::type_mismatch_s(
                &type_name!(self, scrutinee),
                &type_name!(self, f.ret),
                span,
            ));
            Err(Default::default())
        } else if f.constructor.is_some() {
            Err(Some(Error::type_mismatch_s(
                &type_name!(self, scrutinee),
                &type_name!(self, f.ret),
                span,
            )))
        } else {
            Err(Some(Error::expected_found(
                &type_name!(self, scrutinee),
                &format!("function '{}'", strdata!(self, f.name.data)),
                span,
            )))
        }
    }

    fn check_int_pattern(
        &mut self,
        target: TypeId,
        patt: IntPattern,
        span: Span,
    ) -> Option<ComptimeInt> {
        let inner = target.strip_references(&self.proj.types);
        if !self.proj.types[inner].is_integral() {
            let (ty, _) = self.get_int_type_and_val(None, patt, span);
            if ty == TypeId::UNKNOWN {
                return None;
            }

            bail!(self, type_mismatch_err!(self, target, ty, span));
        }

        let (ty, value) = self.get_int_type_and_val(Some(inner), patt, span);
        if ty != inner {
            bail!(self, type_mismatch_err!(self, inner, ty, span));
        }

        Some(value)
    }

    fn check_slice_pattern(
        &mut self,
        inner_ptr: TypeId,
        span_inner: TypeId,
        patterns: Vec<Located<Pattern>>,
        span_id: UserTypeId,
        typ: PatternType,
    ) -> CPattern {
        let mut rest = None;
        let mut result = Vec::new();
        for (i, patt) in patterns.into_iter().enumerate() {
            if let Pattern::Rest(var) = patt.data {
                let id = var.and_then(|(m, name)| {
                    self.insert_pattern_var(typ, name, TypeId::UNKNOWN, m, false)
                });

                if rest.is_some() {
                    self.error(Error::new(
                        "'...' can only be used once in an array pattern",
                        patt.span,
                    ))
                } else {
                    rest = Some(RestPattern { id, pos: i });
                }
            } else {
                result.push(self.check_pattern(PatternParams {
                    scrutinee: inner_ptr,
                    mutable: false,
                    pattern: patt,
                    typ,
                    has_hint: false,
                }));
            }
        }

        if let Some(RestPattern { id: Some(id), .. }) = &rest {
            self.proj.scopes.get_mut(*id).item.ty = self.proj.types.insert(Type::User(
                GenericUserType::from_type_args(&self.proj.scopes, span_id, [span_inner]),
            ));
        }

        CPattern::refutable(PatternData::Span { rest, patterns: result, inner: span_inner })
    }

    fn check_array_pattern(
        &mut self,
        target: TypeId,
        patterns: Vec<Located<Pattern>>,
        span: Span,
        typ: PatternType,
    ) -> CPattern {
        let span_id = self.proj.scopes.lang_types.get(&Strings::LANG_SPAN).copied();
        let span_mut_id = self.proj.scopes.lang_types.get(&Strings::LANG_SPAN_MUT).copied();
        let (real_inner, arr_len) =
            match self.proj.types.get(target.strip_references(&self.proj.types)) {
                &Type::Array(ty, len) => (ty, len),
                Type::User(ut) if Some(ut.id) == span_id => {
                    let inner = ut.first_type_arg().unwrap();
                    let ptr = self.proj.types.insert(Type::Ptr(inner));
                    return self.check_slice_pattern(ptr, inner, patterns, span_id.unwrap(), typ);
                }
                Type::User(ut) if Some(ut.id) == span_mut_id => {
                    let inner = ut.first_type_arg().unwrap();
                    if matches!(self.proj.types[target], Type::Ptr(_) | Type::MutPtr(_)) {
                        let ptr = target.matched_inner_type(&mut self.proj.types, inner);
                        let id = if self.proj.types[ptr].is_ptr() {
                            span_id.unwrap()
                        } else {
                            span_mut_id.unwrap()
                        };
                        return self.check_slice_pattern(ptr, inner, patterns, id, typ);
                    } else {
                        let ptr = self.proj.types.insert(Type::MutPtr(inner));
                        return self.check_slice_pattern(
                            ptr,
                            inner,
                            patterns,
                            span_mut_id.unwrap(),
                            typ,
                        );
                    }
                }
                _ => {
                    bail!(
                        self,
                        Error::new(
                            format!(
                                "array pattern cannot match value of type '{}'",
                                type_name!(self, target)
                            ),
                            span,
                        )
                    )
                }
            };

        let inner = target.matched_inner_type(&mut self.proj.types, real_inner);
        let mut rest = None;
        let mut irrefutable = true;
        let mut result = Vec::new();
        for (i, patt) in patterns.into_iter().enumerate() {
            if let Pattern::Rest(var) = patt.data {
                let id = var.and_then(|(mutable, name)| {
                    self.insert_pattern_var(typ, name, TypeId::UNKNOWN, mutable, false)
                });

                if rest.is_some() {
                    self.error(Error::new(
                        "... can only be used once in an array pattern",
                        patt.span,
                    ))
                } else {
                    rest = Some(RestPattern { id, pos: i });
                }
            } else {
                let patt = self.check_pattern(PatternParams {
                    scrutinee: inner,
                    mutable: false,
                    pattern: patt,
                    typ,
                    has_hint: false,
                });
                if !patt.irrefutable {
                    irrefutable = false;
                }
                result.push(patt);
            }
        }

        if result.len() > arr_len {
            self.error(Error::new(
                format!("expected {} elements, got {}", arr_len, result.len()),
                span,
            ))
        }

        if let Some(RestPattern { id: Some(id), .. }) = &mut rest {
            let arr = self.proj.types.insert(Type::Array(real_inner, arr_len - result.len()));
            self.proj.scopes.get_mut(*id).item.ty =
                target.matched_inner_type(&mut self.proj.types, arr);
        }

        CPattern {
            irrefutable,
            data: PatternData::Array {
                patterns: ArrayPattern { rest, arr_len, inner: real_inner, patterns: result },
                borrows: self.proj.types[target].is_any_ptr(),
            },
        }
    }

    fn check_struct_pattern(
        &mut self,
        scrutinee: TypeId,
        mutable: bool,
        destructures: Vec<Destructure>,
        span: Span,
        typ: PatternType,
    ) -> CPattern {
        let stripped = scrutinee.strip_references(&self.proj.types);
        let Some(ut) = self.proj.types[stripped].as_user().filter(|ut| {
            matches!(
                self.proj.scopes.get(ut.id).kind,
                UserTypeKind::Struct | UserTypeKind::Union(_) | UserTypeKind::AnonStruct
            )
        }) else {
            bail!(self, Error::bad_destructure(&type_name!(self, scrutinee), span,));
        };
        let ut_id = ut.id;
        self.resolve_members(ut_id);

        let cap = self.can_access_privates(self.proj.scopes.get(ut_id).scope);
        let mut irrefutable = true;
        let mut checked = Vec::new();

        for Destructure { name, mutable: pm, pattern } in destructures {
            let Some(member) = self.proj.scopes.get(ut_id).members.get(&name.data) else {
                self.proj.diag.report(Error::no_member(
                    &type_name!(self, scrutinee),
                    strdata!(self, name.data),
                    name.span,
                ));
                continue;
            };

            if !member.public && !cap {
                self.proj.diag.report(Error::private_member(
                    &type_name!(self, scrutinee),
                    strdata!(self, name.data),
                    name.span,
                ));
            }

            // TODO: duplicates
            let inner = member.ty.with_ut_templates(&mut self.proj.types, stripped);
            let scrutinee = scrutinee.matched_inner_type(&mut self.proj.types, inner);
            let patt = self.check_pattern(PatternParams {
                scrutinee,
                mutable: mutable || pm,
                pattern,
                typ,
                has_hint: false,
            });
            if !patt.irrefutable {
                irrefutable = false;
            }
            checked.push((name.data, inner, patt))
        }

        CPattern {
            irrefutable,
            data: PatternData::Destrucure {
                patterns: checked,
                borrows: self.proj.types[scrutinee].is_any_ptr(),
            },
        }
    }

    fn check_tuple_pattern(
        &mut self,
        scrutinee: TypeId,
        mutable: bool,
        subpatterns: Vec<Located<Pattern>>,
        span: Span,
        typ: PatternType,
    ) -> CPattern {
        let stripped = scrutinee.strip_references(&self.proj.types);
        let Some(ut) = self
            .proj
            .types
            .get(stripped)
            .as_user()
            .filter(|ut| self.proj.scopes.get(ut.id).kind.is_tuple())
            .cloned()
        else {
            bail!(
                self,
                Error::expected_found(
                    &type_name!(self, scrutinee),
                    &format!("({})", ["_"].repeat(subpatterns.len()).join(", ")),
                    span,
                )
            );
        };

        if ut.ty_args.len() != subpatterns.len() {
            self.proj.diag.report(Error::expected_found(
                &type_name!(self, scrutinee),
                &format!("({})", ["_"].repeat(subpatterns.len()).join(", ")),
                span,
            ));
        }

        let mut irrefutable = true;
        let mut checked = Vec::new();
        for (i, patt) in subpatterns.into_iter().enumerate() {
            let (inner, ty) = if let Some((_, &ty)) = ut.ty_args.get_index(i) {
                (ty, scrutinee.matched_inner_type(&mut self.proj.types, ty))
            } else {
                (TypeId::UNKNOWN, TypeId::UNKNOWN)
            };

            let patt = self.check_pattern(PatternParams {
                scrutinee: ty,
                mutable,
                pattern: patt,
                typ,
                has_hint: false,
            });
            if !patt.irrefutable {
                irrefutable = false;
            }
            checked.push((intern!(self, "{i}"), inner, patt));
        }

        CPattern {
            irrefutable,
            data: PatternData::Destrucure {
                patterns: checked,
                borrows: self.proj.types[scrutinee].is_any_ptr(),
            },
        }
    }

    fn check_tuple_union_pattern(
        &mut self,
        scrutinee: TypeId,
        mutable: bool,
        resolved: ResolvedValue,
        subpatterns: Vec<Located<Pattern>>,
        span: Span,
        typ: PatternType,
    ) -> CPattern {
        match self.get_union_variant(scrutinee, resolved, span) {
            Ok((Some(scrutinee), variant)) => CPattern::refutable(PatternData::Variant {
                pattern: Some(
                    self.check_tuple_pattern(scrutinee, mutable, subpatterns, span, typ).into(),
                ),
                variant,
                inner: TypeId::UNKNOWN,
                borrows: self.proj.types[scrutinee].is_any_ptr(),
            }),
            Ok((None, _)) => self.error(Error::expected_found(
                "empty variant pattern",
                "tuple variant pattern",
                span,
            )),
            Err(Some(err)) => self.error(err),
            _ => Default::default(),
        }
    }

    fn check_empty_union_pattern(
        &mut self,
        scrutinee: TypeId,
        resolved: ResolvedValue,
        span: Span,
    ) -> CPattern {
        match self.get_union_variant(scrutinee, resolved, span) {
            Ok((Some(_), _)) => {
                self.error(Error::expected_found("empty variant pattern", "variant pattern", span))
            }
            Ok((None, variant)) => CPattern::refutable(PatternData::Variant {
                pattern: None,
                variant,
                inner: TypeId::UNKNOWN,
                borrows: false,
            }),
            Err(Some(err)) => self.error(err),
            Err(None) => Default::default(),
        }
    }

    fn check_or_pattern(
        &mut self,
        scrutinee: TypeId,
        mutable: bool,
        subpatterns: Vec<Located<Pattern>>,
        typ: PatternType,
    ) -> CPattern {
        let mut prev_vars = HashMap::new();
        let mut patterns = vec![];
        let nsubpatterns = subpatterns.len();
        for (i, pattern) in subpatterns.into_iter().enumerate() {
            let patt_span = pattern.span;
            let (res, vars) = self.listen_for_vars(self.current_expr, |this| {
                this.check_pattern(PatternParams {
                    scrutinee,
                    mutable,
                    pattern,
                    typ: if i + 1 != nsubpatterns { PatternType::BodylessFn } else { typ },
                    has_hint: false,
                })
            });
            patterns.push(res);
            if self.current_expr == self.listening_expr {
                self.listening_vars.extend_from_slice(&vars);
            }

            if i == 0 {
                prev_vars = vars
                    .into_iter()
                    .filter(|&v| self.proj.scopes.get(v).name.data != Strings::EMPTY)
                    .map(|v| (self.proj.scopes.get(v).name.data, v))
                    .collect();
                continue;
            }

            let mut prev_vars = prev_vars.clone();
            for &id in vars.iter() {
                let var = self.proj.scopes.get(id);
                if var.name.data == Strings::EMPTY {
                    continue;
                }

                if let Some(old_ty) =
                    prev_vars.remove(&var.name.data).map(|v| self.proj.scopes.get(v).ty)
                {
                    if var.ty != old_ty {
                        let ty_name = type_name!(self, var.ty);
                        let old_ty_name = type_name!(self, old_ty);
                        report_error!(
                            self,
                            var.name.span,
                            "type of variable '{}' ({ty_name}) differs from its original type '{old_ty_name}'",
                            strdata!(self, var.name.data),
                        )
                    }
                } else {
                    report_error!(
                        self,
                        var.name.span,
                        "variable '{}' is not defined in all cases",
                        strdata!(self, var.name.data)
                    )
                }
            }

            for (name, _) in prev_vars {
                report_error!(
                    self,
                    patt_span,
                    "pattern must bind variable '{}'",
                    strdata!(self, name)
                )
            }
        }

        // TODO: the pattern can be irrefutable if it is exhaustive
        CPattern::refutable(PatternData::Or(patterns))
    }

    fn check_pattern(
        &mut self,
        PatternParams { scrutinee, mutable, pattern, typ, has_hint }: PatternParams,
    ) -> CPattern {
        let span = pattern.span;
        match pattern.data {
            Pattern::TupleLike { path, subpatterns } => {
                let value = self.resolve_value_path(&path, Some(scrutinee));
                self.check_tuple_union_pattern(scrutinee, mutable, value, subpatterns, span, typ)
            }
            Pattern::StructLike { path, subpatterns } => {
                let value = self.resolve_value_path(&path, Some(scrutinee));
                match self.get_union_variant(scrutinee, value, span) {
                    Ok((Some(scrutinee), variant)) => CPattern::refutable(PatternData::Variant {
                        pattern: Some(
                            self.check_struct_pattern(scrutinee, mutable, subpatterns, span, typ)
                                .into(),
                        ),
                        variant,
                        inner: TypeId::UNKNOWN,
                        borrows: self.proj.types[scrutinee].is_any_ptr(),
                    }),
                    Ok((None, _)) => self.error(Error::expected_found(
                        "struct variant pattern",
                        "empty variant pattern",
                        span,
                    )),
                    Err(Some(err)) => self.error(err),
                    _ => Default::default(),
                }
            }
            Pattern::Path(path) => {
                let value = self.resolve_value_path(&path, Some(scrutinee));
                if let Some(ident) = path.as_identifier() {
                    match self.get_union_variant(scrutinee, value, span) {
                        Ok((Some(_), _)) => self.error(Error::expected_found(
                            "empty variant pattern",
                            "tuple variant pattern",
                            span,
                        )),
                        Ok((None, variant)) => CPattern::refutable(PatternData::Variant {
                            pattern: None,
                            variant,
                            inner: TypeId::UNKNOWN,
                            borrows: false,
                        }),
                        Err(Some(_)) => {
                            let Some(var) = self.insert_pattern_var(
                                typ,
                                Located::new(span, ident),
                                scrutinee,
                                mutable,
                                has_hint,
                            ) else {
                                return CPattern::irrefutable(PatternData::Void);
                            };
                            CPattern::irrefutable(PatternData::Variable(var))
                        }
                        Err(None) => Default::default(),
                    }
                } else {
                    self.check_empty_union_pattern(scrutinee, value, span)
                }
            }
            Pattern::Option(patt) => {
                let Some(id) = self.proj.scopes.get_option_id() else {
                    return self.error(Error::no_lang_item("option", pattern.span));
                };
                let value = self.resolve_value_path_in(
                    &[(Located::nowhere(Strings::SOME), vec![])],
                    Default::default(),
                    self.proj.scopes.get(id).body_scope,
                    pattern.span,
                );
                self.check_tuple_union_pattern(
                    scrutinee,
                    false,
                    value,
                    vec![Located::new(pattern.span, *patt)],
                    pattern.span,
                    typ,
                )
            }
            Pattern::MutBinding(name) => {
                let Some(var) = self.insert_pattern_var(
                    typ,
                    Located::new(span, name),
                    scrutinee,
                    true,
                    has_hint,
                ) else {
                    return CPattern::irrefutable(PatternData::Void);
                };
                CPattern::irrefutable(PatternData::Variable(var))
            }
            Pattern::Struct(sub) => self.check_struct_pattern(scrutinee, mutable, sub, span, typ),
            Pattern::String(value) => {
                let string = self.make_lang_type_by_name("string", [], span);
                if scrutinee.strip_references(&self.proj.types) != string {
                    bail!(self, type_mismatch_err!(self, scrutinee, string, span));
                }

                CPattern::refutable(PatternData::String(value))
            }
            Pattern::Int(patt) => CPattern::refutable(
                self.check_int_pattern(scrutinee, patt, span)
                    .map(PatternData::Int)
                    .unwrap_or_default(),
            ),
            Pattern::IntRange(RangePattern { inclusive, start, end }) => {
                let start = if let Some(start) = start {
                    let Some(start) = self.check_int_pattern(scrutinee, start, span) else {
                        return Default::default();
                    };
                    Some(start)
                } else {
                    None
                };

                let Some(end) = self.check_int_pattern(scrutinee, end, span) else {
                    return Default::default();
                };

                if start.as_ref().is_some_and(|start| start > &end) {
                    return self.error(Error::new(
                        "range start must be less than or equal to its end",
                        span,
                    ));
                }

                CPattern::refutable(PatternData::IntRange(RangePattern { inclusive, start, end }))
            }
            Pattern::Char(ch) => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::CHAR {
                    bail!(self, type_mismatch_err!(self, scrutinee, TypeId::CHAR, span));
                }

                CPattern::refutable(PatternData::Int(ComptimeInt::from(ch as u32)))
            }
            Pattern::CharRange(RangePattern { inclusive, start, end }) => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::CHAR {
                    bail!(self, type_mismatch_err!(self, scrutinee, TypeId::CHAR, span));
                }

                if start.is_some_and(|start| start > end) {
                    return self.error(Error::new(
                        "range pattern end cannot be greater than its start",
                        span,
                    ));
                }

                CPattern::refutable(PatternData::IntRange(RangePattern {
                    inclusive,
                    start: start.map(|start| ComptimeInt::from(start as u32)),
                    end: ComptimeInt::from(end as u32),
                }))
            }
            Pattern::Rest { .. } => self.error(Error::new(
                "rest patterns are only valid inside array or span patterns",
                span,
            )),
            Pattern::Array(sub) => self.check_array_pattern(scrutinee, sub, span, typ),
            Pattern::Bool(val) => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::BOOL {
                    bail!(self, type_mismatch_err!(self, scrutinee, TypeId::BOOL, span));
                }

                CPattern::refutable(PatternData::Int(ComptimeInt::from(val)))
            }
            Pattern::Tuple(sub) => self.check_tuple_pattern(scrutinee, mutable, sub, span, typ),
            Pattern::Void => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::VOID {
                    bail!(self, type_mismatch_err!(self, scrutinee, TypeId::VOID, span));
                }

                CPattern::irrefutable(PatternData::Void)
            }
            Pattern::Or(sub) => self.check_or_pattern(scrutinee, mutable, sub, typ),
            Pattern::Error => Default::default(),
        }
    }

    fn check_full_pattern(&mut self, scrutinee: TypeId, pattern: Located<FullPattern>) -> CPattern {
        self.check_pattern(PatternParams {
            scrutinee,
            mutable: false,
            pattern: pattern.map(|inner| inner.data),
            typ: PatternType::Regular,
            has_hint: false,
        })
    }
}

/// Path resolution routines
impl TypeChecker {
    fn find_in_tns(&self, name: StrId) -> Option<Vis<TypeItem>> {
        for (id, scope) in self.proj.scopes.walk(self.current) {
            if let Some(item) = self.proj.scopes[id].find_in_tns(name) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        if let Some(item) = self.proj.scopes.autouse_tns.get(&name).copied() {
            return Some(item);
        }

        None
    }

    fn find_in_vns(&self, name: StrId) -> Option<Vis<ValueItem>> {
        for (id, scope) in self.proj.scopes.walk(self.current) {
            if let Some(item) = self.proj.scopes[id].find_in_vns(name) {
                if item.is_fn() && matches!(scope.kind, ScopeKind::UserType(_)) {
                    continue;
                }

                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        if let Some(item) = self.proj.scopes.autouse_vns.get(&name).copied() {
            return Some(item);
        }

        None
    }

    fn get_super(&mut self, span: Span) -> Option<ScopeId> {
        if let Some(module) = self.proj.scopes.module_of(
            self.proj.scopes[self.proj.scopes.module_of(self.current).unwrap()].parent.unwrap(),
        ) {
            Some(module)
        } else {
            self.error(Error::new("cannot use super here", span))
        }
    }

    fn resolve_use(&mut self, path: &UsePath, declaring: bool) -> Result<(), Located<StrId>> {
        enum CheckResult {
            NotFound,
            BadType,
            Next(ScopeId),
        }

        fn check(
            this: &mut TypeChecker,
            val: Option<&TypeItem>,
            span: Span,
            last: bool,
        ) -> CheckResult {
            match val {
                Some(&TypeItem::Module(next)) => {
                    this.check_hover(span, next.into());
                    CheckResult::Next(next)
                }
                Some(&TypeItem::Type(id)) => {
                    this.check_hover(span, id.into());
                    if !last {
                        this.proj.diag.report(Error::new("expected module name", span));
                        CheckResult::BadType
                    } else {
                        CheckResult::Next(this.proj.scopes.get(id).body_scope)
                    }
                }
                _ => CheckResult::NotFound,
            }
        }

        let mut components = &path.components[..];
        let mut scope = match path.origin {
            PathOrigin::Root => ScopeId::ROOT,
            PathOrigin::Super(span) => {
                if let Some(scope) = self.get_super(span) {
                    scope
                } else {
                    return Ok(());
                }
            }
            PathOrigin::Normal => 'out: {
                if let Some((first, rest)) = components.split_first() {
                    let value = self.find_in_tns(first.data).map(|i| i.id);
                    match check(self, value.as_ref(), first.span, rest.is_empty()) {
                        CheckResult::BadType => return Ok(()),
                        CheckResult::NotFound => {}
                        CheckResult::Next(next) => {
                            components = rest;
                            break 'out next;
                        }
                    }
                }
                ScopeId::ROOT
            }
            PathOrigin::Infer => unreachable!("Infer origin in use path"),
        };

        for (i, comp) in components.iter().enumerate() {
            if !declaring {
                self.enter_id_and_resolve(scope, |_| {});
            }

            let val = self.proj.scopes[scope].find_in_tns(comp.data);
            match check(self, val.as_deref(), comp.span, i == components.len() - 1) {
                CheckResult::NotFound => return Err(*comp),
                CheckResult::BadType => return Ok(()),
                CheckResult::Next(next) => scope = next,
            }
        }

        if !declaring {
            self.enter_id_and_resolve(scope, |_| {});
        }

        if let UsePathTail::Ident(tail) = &path.tail {
            // TODO: check privacy
            let mut found = false;
            if let Some(item) = self.proj.scopes[scope].find_in_tns(tail.data) {
                self.check_hover(tail.span, (*item).into());
                if !item.public && !self.can_access_privates(scope) {
                    named_error!(self, Error::private, tail.data, tail.span)
                }

                if self.proj.scopes[self.current]
                    .tns
                    .insert(tail.data, Vis::new(*item, path.public))
                    .is_some()
                {
                    named_error!(self, Error::redefinition, tail.data, tail.span)
                }
                found = true;
            }
            if let Some(item) = self.proj.scopes[scope].find_in_vns(tail.data) {
                self.check_hover(
                    tail.span,
                    match item.id {
                        ValueItem::StructConstructor(id, _) => id.into(),
                        _ => (*item).into(),
                    },
                );

                let mut skip = false;
                if !item.public && !self.can_access_privates(scope) {
                    if !found {
                        named_error!(self, Error::private, tail.data, tail.span)
                    }
                    skip = true;
                }

                if !skip
                    && self.proj.scopes[self.current]
                        .vns
                        .insert(tail.data, Vis::new(*item, path.public))
                        .is_some()
                {
                    named_error!(self, Error::redefinition, tail.data, tail.span)
                }
                found = true;
            }

            if !found {
                return Err(*tail);
            }
        } else {
            let cap = self.can_access_privates(scope);
            if let Some((scope, current)) = self.proj.scopes.borrow_twice(scope, self.current) {
                for (&name, &item) in scope.tns.iter() {
                    if item.public || cap {
                        current.tns.entry(name).or_insert(Vis::new(*item, path.public));
                    }
                }

                for (&name, &item) in scope.vns.iter() {
                    if item.public || cap {
                        current.vns.entry(name).or_insert(Vis::new(*item, path.public));
                    }
                }
            }
        }

        Ok(())
    }

    fn resolve_type_path(&mut self, path: &Path) -> ResolvedType {
        let span = path.span();
        match path.origin {
            PathOrigin::Root => {
                self.resolve_type_path_in(&path.components, Default::default(), ScopeId::ROOT, span)
            }
            PathOrigin::Super(span) => self
                .get_super(span)
                .map(|id| self.resolve_type_path_in(&path.components, Default::default(), id, span))
                .unwrap_or_default(),
            PathOrigin::Normal => {
                let ((name, ty_args), rest) = path.components.split_first().unwrap();
                if rest.is_empty() {
                    self.check_cursor_completions(name.span, true);
                }
                if let Some(builtin) = self.builtin_type_path(name.data) {
                    if let Some((name, _)) = rest.first() {
                        return named_error!(self, Error::no_symbol, name.data, name.span);
                    }
                    return ResolvedType::Builtin(self.proj.types.insert(builtin));
                }

                match self.find_in_tns(name.data).map(|t| t.id) {
                    Some(TypeItem::Type(id)) => {
                        self.check_hover(name.span, id.into());
                        let ty_args = self.resolve_type_args(id, ty_args, true, name.span);
                        if rest.is_empty() {
                            if self.proj.scopes.get(id).kind.is_extension() {
                                return self.error(Error::expected_found(
                                    "type",
                                    &format!(
                                        "extension '{}'",
                                        strdata!(self, self.proj.scopes.get(id).name.data)
                                    ),
                                    name.span,
                                ));
                            }

                            return ResolvedType::UserType(GenericUserType::new(id, ty_args));
                        }

                        self.resolve_type_path_in(
                            rest,
                            ty_args,
                            self.proj.scopes.get(id).body_scope,
                            span,
                        )
                    }
                    Some(TypeItem::Module(id)) if !rest.is_empty() => {
                        self.check_hover(name.span, id.into());
                        if !ty_args.is_empty() {
                            return self.error(Error::new(
                                "modules cannot be parameterized with type arguments",
                                name.span,
                            ));
                        }

                        self.resolve_type_path_in(rest, Default::default(), id, span)
                    }
                    _ => self.resolve_type_path_in(
                        &path.components,
                        Default::default(),
                        ScopeId::ROOT,
                        span,
                    ),
                }
            }
            PathOrigin::Infer => unreachable!("Infer path in type path"),
        }
    }

    fn resolve_type_path_in(
        &mut self,
        data: &[PathComponent],
        mut ty_args: TypeArgs,
        mut scope: ScopeId,
        total_span: Span,
    ) -> ResolvedType {
        for (i, (name, args)) in data.iter().enumerate() {
            self.enter_id_and_resolve(scope, |_| {});
            let done = i + 1 == data.len();
            if done {
                self.check_module_completions(total_span, true, scope);
            }

            let Some(item) = self.proj.scopes[scope].find_in_tns(name.data) else {
                return named_error!(self, Error::no_symbol, name.data, name.span);
            };

            if !item.public && !self.can_access_privates(scope) {
                named_error!(self, Error::private, name.data, name.span)
            }

            match *item {
                TypeItem::Type(id) => {
                    self.check_hover(name.span, id.into());
                    let ty = self.proj.scopes.get(id);
                    scope = ty.body_scope;

                    let args = self.resolve_type_args(id, args, true, name.span);
                    if done {
                        if self.proj.scopes.get(id).kind.is_extension() {
                            return self.error(Error::expected_found(
                                "type",
                                &format!(
                                    "extension '{}'",
                                    strdata!(self, self.proj.scopes.get(id).name.data)
                                ),
                                name.span,
                            ));
                        }

                        return ResolvedType::UserType(GenericUserType::new(id, args));
                    }

                    ty_args.copy_args(&args);
                }
                TypeItem::Module(id) => {
                    self.check_hover(name.span, id.into());
                    if done {
                        return named_error!(self, Error::no_symbol, name.data, name.span);
                    }

                    if !args.is_empty() {
                        return self.error(Error::new(
                            "modules cannot be parameterized with type arguments",
                            name.span,
                        ));
                    }

                    scope = id;
                }
            }
        }

        unreachable!()
    }

    fn resolve_value_path(&mut self, path: &Path, target: Option<TypeId>) -> ResolvedValue {
        let span = path.span();
        match path.origin {
            PathOrigin::Root => self.resolve_value_path_in(
                &path.components,
                Default::default(),
                ScopeId::ROOT,
                span,
            ),
            PathOrigin::Super(span) => self
                .get_super(span)
                .map(|id| {
                    self.resolve_value_path_in(&path.components, Default::default(), id, span)
                })
                .unwrap_or_default(),
            PathOrigin::Normal => {
                let ((name, ty_args), rest) = path.components.split_first().unwrap();
                if let Some(builtin) = self.builtin_type_path(name.data)
                    && !rest.is_empty()
                {
                    let builtin = self.proj.types.insert(builtin);
                    return self.resolve_value_path_from_type(builtin, rest, span);
                }

                if rest.is_empty() {
                    self.check_cursor_completions(name.span, false);
                    match self.find_in_vns(name.data).map(|f| f.id) {
                        Some(ValueItem::Fn(id)) => {
                            self.resolve_proto(id);
                            self.check_hover(name.span, id.into());
                            let mut ty_args = self.resolve_type_args(id, ty_args, false, name.span);
                            if let Some(id) = self.proj.scopes.get(id).constructor {
                                ty_args.copy_args(&TypeArgs::in_order(
                                    &self.proj.scopes,
                                    id,
                                    std::iter::repeat(TypeId::UNKNOWN),
                                ));
                            }

                            ResolvedValue::Fn(GenericFn::new(id, ty_args))
                        }
                        Some(ValueItem::StructConstructor(id, init)) => {
                            self.resolve_proto(init);
                            self.check_hover(name.span, init.into());
                            ResolvedValue::Fn(GenericFn::new(
                                init,
                                self.resolve_type_args(id, ty_args, false, name.span),
                            ))
                        }
                        Some(ValueItem::UnionConstructor(id)) => {
                            self.check_hover(name.span, id.into());
                            ResolvedValue::UnionConstructor(GenericUserType::new(
                                id,
                                self.resolve_type_args(id, ty_args, false, name.span),
                            ))
                        }
                        Some(ValueItem::Var(id)) => {
                            self.check_hover(name.span, id.into());
                            resolve_type!(self, self.proj.scopes.get_mut::<VariableId>(id).ty);
                            if !ty_args.is_empty() {
                                return self.error(Error::new(
                                    "variables cannot have type arguments",
                                    name.span,
                                ));
                            }

                            ResolvedValue::Var(id)
                        }
                        None => self.resolve_value_path_in(
                            &path.components,
                            Default::default(),
                            ScopeId::ROOT,
                            span,
                        ),
                    }
                } else {
                    match self.find_in_tns(name.data).map(|t| t.id) {
                        Some(TypeItem::Type(id)) => {
                            self.check_hover(name.span, id.into());
                            let mut ty_args = self.resolve_type_args(id, ty_args, false, name.span);
                            if let UserTypeKind::Trait(this, _) = self.proj.scopes.get(id).kind {
                                ty_args.insert(this, TypeId::UNKNOWN);
                            } else {
                                let ty = Type::User(GenericUserType::new(id, ty_args));
                                let id = self.proj.types.insert(ty);
                                return self.resolve_value_path_from_type(id, rest, span);
                            }

                            self.resolve_value_path_in(
                                rest,
                                ty_args,
                                self.proj.scopes.get(id).body_scope,
                                span,
                            )
                        }
                        Some(TypeItem::Module(id)) => {
                            self.check_hover(name.span, id.into());
                            if !ty_args.is_empty() {
                                return self.error(Error::new(
                                    "modules cannot be parameterized with type arguments",
                                    name.span,
                                ));
                            }

                            self.resolve_value_path_in(rest, Default::default(), id, span)
                        }
                        None => self.resolve_value_path_in(
                            &path.components,
                            Default::default(),
                            ScopeId::ROOT,
                            span,
                        ),
                    }
                }
            }
            PathOrigin::Infer => {
                let Some(scope) = target
                    .and_then(|t| self.proj.types[t.strip_references(&self.proj.types)].as_user())
                    .map(|ut| self.proj.scopes.get(ut.id))
                    .filter(|ut| ut.kind.is_union())
                    .map(|ut| ut.body_scope)
                else {
                    return self.error(Error::new("cannot infer union type", span));
                };

                let mut res =
                    self.resolve_value_path_in(&path.components, Default::default(), scope, span);
                let func = match &mut res {
                    ResolvedValue::NotFound(_) | ResolvedValue::Error => return res,
                    ResolvedValue::Fn(func) => func,
                    ResolvedValue::MemberFn(mfn) => &mut mfn.func,
                    ResolvedValue::UnionConstructor(_) | ResolvedValue::Var(_) => {
                        self.proj
                            .diag
                            .report(Error::new("infer path must be to union variant", span));
                        return res;
                    }
                };

                let f = self.proj.scopes.get(func.id);
                let mut good = false;
                if let Some(id) = f.constructor {
                    if self.proj.scopes.get(id).kind.is_union() {
                        good = true;
                    }
                    func.ty_args.copy_args(&TypeArgs::in_order(
                        &self.proj.scopes,
                        id,
                        std::iter::repeat(TypeId::UNKNOWN),
                    ));
                }

                if !good {
                    report_error!(
                        self,
                        span,
                        "function '{}' is not a union variant",
                        strdata!(self, f.name.data)
                    )
                }

                res
            }
        }
    }

    fn resolve_value_path_in(
        &mut self,
        data: &[PathComponent],
        mut ty_args: TypeArgs,
        mut scope: ScopeId,
        total_span: Span,
    ) -> ResolvedValue {
        let Some(((last_name, last_args), rest)) = data.split_last() else {
            self.check_module_completions(total_span, false, scope);
            // the only way for this to be empty is for a prior error to have occured
            return ResolvedValue::Error;
        };
        for (i, (name, args)) in rest.iter().enumerate() {
            self.enter_id_and_resolve(scope, |_| {});
            let Some(item) = self.proj.scopes[scope].find_in_tns(name.data) else {
                return ResolvedValue::NotFound(*name);
            };

            if !item.public && !self.can_access_privates(scope) {
                named_error!(self, Error::private, name.data, name.span)
            }

            match *item {
                TypeItem::Type(id) => {
                    self.check_hover(name.span, id.into());
                    ty_args.copy_args(&self.resolve_type_args(id, args, false, name.span));

                    let ty = self.proj.scopes.get(id);
                    scope = ty.body_scope;
                    if let UserTypeKind::Trait(this, _) = ty.kind {
                        ty_args.insert(this, TypeId::UNKNOWN);
                    } else {
                        let ty = Type::User(GenericUserType::new(id, ty_args));
                        let id = self.proj.types.insert(ty);
                        return self.resolve_value_path_from_type(id, &data[i + 1..], total_span);
                    }
                }
                TypeItem::Module(id) => {
                    self.check_hover(name.span, id.into());
                    if !args.is_empty() {
                        return self.error(Error::new(
                            "modules cannot be parameterized with type arguments",
                            name.span,
                        ));
                    }

                    scope = id;
                }
            }
        }

        self.enter_id_and_resolve(scope, |_| {});
        self.check_module_completions(total_span, false, scope);
        let Some(item) = self.proj.scopes[scope].find_in_vns(last_name.data) else {
            return ResolvedValue::NotFound(*last_name);
        };

        if !item.public && !self.can_access_privates(scope) {
            named_error!(self, Error::private, last_name.data, last_name.span)
        }

        let span = last_name.span;
        match *item {
            ValueItem::Fn(id) => {
                self.resolve_proto(id);
                self.check_hover(span, id.into());
                ty_args.copy_args(&self.resolve_type_args(id, last_args, false, span));
                ResolvedValue::Fn(GenericFn::new(id, ty_args))
            }
            ValueItem::StructConstructor(id, init) => {
                self.resolve_proto(init);
                self.check_hover(span, init.into());
                ty_args.copy_args(&self.resolve_type_args(id, last_args, false, span));
                ResolvedValue::Fn(GenericFn::new(init, ty_args))
            }
            ValueItem::UnionConstructor(id) => {
                self.check_hover(span, id.into());
                ty_args.copy_args(&self.resolve_type_args(id, last_args, false, span));
                ResolvedValue::UnionConstructor(GenericUserType::new(id, ty_args))
            }
            ValueItem::Var(id) => {
                self.check_hover(span, id.into());
                resolve_type!(self, self.proj.scopes.get_mut(id).ty);
                if !last_args.is_empty() {
                    self.error(Error::new(
                        "variables cannot be parameterized with type arguments",
                        span,
                    ))
                }

                ResolvedValue::Var(id)
            }
        }
    }

    fn resolve_value_path_from_type(
        &mut self,
        ty: TypeId,
        rest: &[PathComponent],
        total_span: Span,
    ) -> ResolvedValue {
        let ((name, args), rest) = rest.split_first().unwrap();
        self.check_dot_completions(total_span, ty, false);

        let Some(mfn) = self.get_member_fn(ty, name.data, args, name.span, self.current) else {
            return ResolvedValue::NotFound(*name);
        };

        self.check_hover(name.span, mfn.func.id.into());
        if let Some((name, _)) = rest.first() {
            return ResolvedValue::NotFound(*name);
        }

        if !mfn.public && !self.can_access_privates(mfn.owner) {
            report_error!(
                self,
                name.span,
                "cannot access private method '{}' of type '{}'",
                strdata!(self, self.proj.scopes.get(mfn.func.id).name.data),
                type_name!(self, ty)
            )
        }
        ResolvedValue::MemberFn(mfn)
    }

    fn resolve_type_args<T: ItemId>(
        &mut self,
        id: T,
        args: &[Located<TypeHint>],
        typehint: bool,
        span: Span,
    ) -> TypeArgs
    where
        T::Value: HasTypeParams,
    {
        let params = self.proj.scopes.get(id).get_type_params().to_vec();
        if (typehint || !args.is_empty()) && args.len() != params.len() {
            self.error(Error::new(
                format!("expected {} type argument(s), received {}", params.len(), args.len()),
                span,
            ))
        }

        let ty_args = TypeArgs(
            params
                .iter()
                .copied()
                .zip(args.iter().map(|ty| self.resolve_typehint(ty)).take(params.len()).chain(
                    std::iter::repeat_n(
                        TypeId::UNKNOWN,
                        params.len().checked_sub(args.len()).unwrap_or_default(),
                    ),
                ))
                .collect(),
        );
        for (&id, &ty) in ty_args.iter() {
            self.check_bounds(&ty_args, ty, self.proj.scopes.get(id).impls.clone(), span);
        }

        ty_args
    }

    fn builtin_type_path(&self, name: StrId) -> Option<Type> {
        match strdata!(self, name) {
            "void" => Some(Type::Void),
            "never" => Some(Type::Never),
            "f32" => Some(Type::F32),
            "f64" => Some(Type::F64),
            "bool" => Some(Type::Bool),
            "char" => Some(Type::Char),
            "c_char" => Some(Type::CInt(CInt::Char)),
            "c_short" => Some(Type::CInt(CInt::Short)),
            "c_int" => Some(Type::CInt(CInt::Int)),
            "c_long" => Some(Type::CInt(CInt::Long)),
            "c_longlong" => Some(Type::CInt(CInt::LongLong)),
            "c_uchar" => Some(Type::CUint(CInt::Char)),
            "c_ushort" => Some(Type::CUint(CInt::Short)),
            "c_uint" => Some(Type::CUint(CInt::Int)),
            "c_ulong" => Some(Type::CUint(CInt::Long)),
            "c_ulonglong" => Some(Type::CUint(CInt::LongLong)),
            name => Type::from_int_name(name, true),
        }
    }
}

struct ConstValue {
    ty: TypeId,
    val: ComptimeInt,
}

/// CTFE related functions
impl TypeChecker {
    fn consteval_check(&mut self, expr: PExpr, target: TypeId) -> Option<ConstValue> {
        let span = expr.span;
        let expr = self.type_check(expr, target);
        (expr.ty == target).then(|| self.consteval(&expr, span)).flatten()
    }

    fn consteval(&mut self, expr: &CExpr, span: Span) -> Option<ConstValue> {
        match &expr.data {
            CExprData::Int(val) => Some(ConstValue { ty: expr.ty, val: val.clone() }),
            CExprData::Binary(op, lhs, rhs) => {
                let mut lhs = self.consteval(lhs, span)?;
                let rhs = self.consteval(rhs, span)?;
                let int = lhs.ty.as_integral(&self.proj.types, false).unwrap();
                lhs.val = match op {
                    BinaryOp::Add => lhs.val + &rhs.val,
                    BinaryOp::Sub => lhs.val - &rhs.val,
                    BinaryOp::Mul => lhs.val * &rhs.val,
                    BinaryOp::Div => lhs.val / &rhs.val,
                    BinaryOp::Rem => lhs.val % &rhs.val,
                    BinaryOp::BitAnd => lhs.val & &rhs.val,
                    BinaryOp::Xor => lhs.val ^ &rhs.val,
                    BinaryOp::BitOr => lhs.val | &rhs.val,
                    BinaryOp::Shl => {
                        if let Some(rhs) = rhs.val.into_word().filter(|w| (0..int.bits).contains(w))
                        {
                            lhs.val << rhs
                        } else {
                            self.proj.diag.report(Error::no_consteval(span)); // TODO: span of the expression that caused it
                            ComptimeInt::from(0)
                        }
                    }
                    BinaryOp::Shr => {
                        if let Some(rhs) = rhs.val.into_word().filter(|w| (0..int.bits).contains(w))
                        {
                            lhs.val >> rhs
                        } else {
                            self.proj.diag.report(Error::no_consteval(span)); // TODO: span of the expression that caused it
                            ComptimeInt::from(0)
                        }
                    }
                    _ => return self.error(Error::no_consteval(span)),
                };

                if !lhs.val.fits_into(int.bits, int.signed) {
                    lhs.val = ComptimeInt::from(0);
                    self.error(Error::consteval_overflow(span))
                }

                Some(lhs)
            }
            CExprData::Call { callee, .. } => {
                let CExprData::Fn(func, _) = &callee.data else {
                    return self.error(Error::no_consteval(span));
                };

                if !self.proj.scopes.intrinsics.contains_key(&func.id) {
                    return self.error(Error::no_consteval(span));
                }

                match strdata!(self, self.proj.scopes.get(func.id).name.data) {
                    "size_of" => {
                        let ty = func.first_type_arg().unwrap();
                        // TODO: make sure the ty has had resolve_members()
                        // and resolve_dependencies() called on it and doesn't have any template args
                        let (sz, _) = ty.size_and_align(&self.proj.scopes, &mut self.proj.types);
                        Some(ConstValue { ty: TypeId::USIZE, val: ComptimeInt::from(sz) })
                    }
                    "align_of" => {
                        let ty = func.first_type_arg().unwrap();
                        // TODO: make sure the ty has had resolve_members()
                        // and resolve_dependencies() called on it and doesn't have any template args
                        let (_, align) = ty.size_and_align(&self.proj.scopes, &mut self.proj.types);
                        Some(ConstValue { ty: TypeId::USIZE, val: ComptimeInt::from(align) })
                    }
                    _ => self.error(Error::no_consteval(span)),
                }
            }
            CExprData::Error => None,
            _ => self.error(Error::no_consteval(span)),
        }
    }

    fn trait_hack(&self, mfn: &mut MemberFn, ty: TypeId) {
        // FIXME: get rid of this function
        if let MemberFnType::Trait(tr) = &mut mfn.typ {
            for v in tr.ty_args.values_mut() {
                if *v == TypeId::UNKNOWN {
                    *v = ty;
                }
            }
        }
    }
}

struct Tables {
    intrinsics: HashSet<StrId>,
    binary_op_traits: HashMap<BinaryOp, (StrId, StrId)>,
    unary_op_traits: HashMap<UnaryOp, (StrId, StrId)>,
}

impl Tables {
    #[rustfmt::skip]
    pub fn new(strings: &mut Strings) -> Self {
        let op_cmp = strings.get_or_intern_static("op_cmp");
        let op_eq = strings.get_or_intern_static("op_eq");
        Self {
            intrinsics: [
                strings.get_or_intern_static("size_of"),
                strings.get_or_intern_static("align_of"),
                strings.get_or_intern_static("panic"),
                strings.get_or_intern_static("binary_op"),
                strings.get_or_intern_static("unary_op"),
                strings.get_or_intern_static("numeric_cast"),
                strings.get_or_intern_static("numeric_abs"),
                strings.get_or_intern_static("max_value"),
                strings.get_or_intern_static("min_value"),
                strings.get_or_intern_static("unreachable_unchecked"),
                strings.get_or_intern_static("type_id"),
                strings.get_or_intern_static("type_name"),
                strings.get_or_intern_static("read_volatile"),
                strings.get_or_intern_static("write_volatile"),
                strings.get_or_intern_static("source_location"),
            ]
            .into(),
            binary_op_traits: [
                (BinaryOp::Cmp, (op_cmp, strings.get_or_intern_static("cmp"))),
                (BinaryOp::Gt, (op_cmp, strings.get_or_intern_static("gt"))),
                (BinaryOp::GtEqual, (op_cmp, strings.get_or_intern_static("ge"))),
                (BinaryOp::Lt, (op_cmp, strings.get_or_intern_static("lt"))),
                (BinaryOp::LtEqual, (op_cmp, strings.get_or_intern_static("le"))),
                (BinaryOp::Equal, (op_eq, strings.get_or_intern_static("eq"))),
                (BinaryOp::NotEqual, (op_eq, strings.get_or_intern_static("ne"))),
                (BinaryOp::Add, (strings.get_or_intern_static("op_add"), strings.get_or_intern_static("add"))),
                (BinaryOp::Sub, (strings.get_or_intern_static("op_sub"), strings.get_or_intern_static("sub"))),
                (BinaryOp::Mul, (strings.get_or_intern_static("op_mul"), strings.get_or_intern_static("mul"))),
                (BinaryOp::Div, (strings.get_or_intern_static("op_div"), strings.get_or_intern_static("div"))),
                (BinaryOp::Rem, (strings.get_or_intern_static("op_rem"), strings.get_or_intern_static("rem"))),
                (BinaryOp::BitAnd, (strings.get_or_intern_static("op_and"), strings.get_or_intern_static("bit_and"))),
                (BinaryOp::BitOr, (strings.get_or_intern_static("op_or"), strings.get_or_intern_static("bit_or"))),
                (BinaryOp::Xor, (strings.get_or_intern_static("op_xor"), strings.get_or_intern_static("xor"))),
                (BinaryOp::Shl, (strings.get_or_intern_static("op_shl"), strings.get_or_intern_static("shl"))),
                (BinaryOp::Shr, (strings.get_or_intern_static("op_shr"), strings.get_or_intern_static("shr"))),
            ]
            .into(),
            unary_op_traits: [
                (UnaryOp::Neg, (strings.get_or_intern_static("op_neg"), strings.get_or_intern_static("neg"))),
                (UnaryOp::Not, (strings.get_or_intern_static("op_not"), strings.get_or_intern_static("not"))),
                (UnaryOp::Unwrap, (strings.get_or_intern_static("op_unwrap"), strings.get_or_intern_static("unwrap"))),
                (UnaryOp::PostDecrement, (strings.get_or_intern_static("op_dec"), strings.get_or_intern_static("dec"))),
                (UnaryOp::PreDecrement, (strings.get_or_intern_static("op_dec"), strings.get_or_intern_static("dec"))),
                (UnaryOp::PostIncrement, (strings.get_or_intern_static("op_inc"), strings.get_or_intern_static("inc"))),
                (UnaryOp::PreIncrement, (strings.get_or_intern_static("op_inc"), strings.get_or_intern_static("inc"))),
            ]
            .into(),
        }
    }
}
