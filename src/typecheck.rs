use std::marker::PhantomData;

use enum_as_inner::EnumAsInner;

use crate::{
    FeatureSet, Warning,
    ast::{
        Attribute, Attributes, BinaryOp, UnaryOp,
        checked::{
            ArrayPattern, Block, Expr as CExpr, ExprArena, ExprData as CExprData,
            FormatOpts as CFormatSpec, Pattern as CPattern, PatternData, RestPattern,
            Stmt as CStmt,
        },
        declared::{Fn as DFn, ImplBlock as DImplBlock, Stmt as DStmt, UsePath as DUsePath},
        parsed::{
            Expr as PExpr, ExprArena as PExprArena, ExprData as PExprData, Pattern, Stmt as PStmt,
            StmtData as PStmtData, *,
        },
    },
    ds::{ComptimeInt, Dependencies, HashMap, HashSet, IndexMap},
    error::Error,
    intern::{StrId, Strings, THIS_TYPE},
    lexer::{Located, Span},
    project::Project,
    sym::*,
    typeid::{
        BitSizeResult, FnPtr, GenericExtension, GenericFn, GenericTrait, GenericUserType, Type,
        TypeArgs, TypeId, Types,
    },
};

macro_rules! resolve_type {
    ($self: expr, $ty: expr) => {{
        let id = match $self.proj.types[$ty] {
            Type::Unresolved(hint, scope) => {
                $self.enter_id_and_resolve(scope, |this| this.resolve_typehint(hint))
            }
            _ => $ty,
        };
        $ty = id;
        id
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
    ($self: expr, $expected: expr, $received: expr, $span: expr) => {
        Error::type_mismatch($self.proj.fmt_ty($expected), $self.proj.fmt_ty($received), $span)
    };
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

#[derive(Clone, Copy, derive_more::From)]
pub enum LspItem {
    Type(UserTypeId),
    Alias(AliasId),
    Module(ScopeId, bool),
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

impl From<ScopeId> for LspItem {
    fn from(value: ScopeId) -> Self {
        Self::Module(value, false)
    }
}

impl From<TypeItem> for LspItem {
    fn from(value: TypeItem) -> Self {
        match value {
            TypeItem::Type(id) => Self::Type(id),
            TypeItem::Module(id) => Self::Module(id, false),
            TypeItem::Alias(id) => Self::Alias(id),
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
enum Listen {
    No,
    Passthrough,
    Yes,
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
        use Type as T;
        match src {
            T::Usize | T::Isize => match dst {
                T::Ptr(_) | T::MutPtr(_) | T::FnPtr(_) => Cast::Unsafe,
                T::Uint(_) | T::Int(_) => Cast::Fallible,
                T::Usize | T::Isize => Cast::Fallible,
                T::RawPtr(_) | T::RawMutPtr(_) | T::F32 | T::F64 => Cast::Infallible,
                _ => Cast::None,
            },
            src if src.as_integral(true).is_some() => {
                let a = src.as_integral(true).unwrap();
                // from can only be Uint(n) | Int(n) | Char | Bool now
                match dst {
                    // we definitely don't support any targets with < 16 bit pointers
                    T::Usize | T::Isize if !src.is_bool() && a.bits > 16 => Cast::Fallible,
                    T::F32 | T::F64 => Cast::Infallible,
                    // d800-e000 is invalid for a char, u15::MAX is 0x7fff
                    T::Char if matches!(src, T::Uint(n) if *n <= 15) => Cast::Infallible,
                    _ => {
                        if let Some(b) = dst.as_integral(false) {
                            if (a.signed == b.signed && a.bits <= b.bits)
                                || (!a.signed && b.signed && a.bits < b.bits)
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
            T::F32 | T::F64 => match dst {
                T::Int(_) | T::Isize | T::F32 | T::F64 => Cast::Infallible,
                T::Uint(_) | T::Usize => Cast::Fallible,
                _ => Cast::None,
            },
            T::Ptr(_) | T::MutPtr(_) | T::FnPtr(_) | T::RawPtr(_) | T::RawMutPtr(_) => {
                match dst {
                    T::Ptr(_) | T::MutPtr(_) | T::FnPtr(_) => Cast::Unsafe,
                    T::Usize | T::Isize | T::RawPtr(_) | T::RawMutPtr(_) => Cast::Infallible, // maybe only *T to ^mut T should be infallible
                    _ => Cast::None,
                }
            }
            _ => Cast::None,
        }
    }
}

struct PatternParams<'a> {
    scrutinee: TypeId,
    mutable: bool,
    pattern: &'a Located<Pattern>,
    typ: PatternType,
    has_hint: bool,
}

pub type ExtensionCache = HashMap<Vec<UserTypeId>, HashMap<TypeId, Vec<GenericExtension>>>;

pub struct TypeChecker<'a> {
    safety: Safety,
    current: ScopeId,
    lsp_input: LspInput,
    proj: Project,
    listening_vars: Option<Vec<VariableId>>,
    current_static: Option<(VariableId, HashSet<VariableId>)>,
    current_call_ty_args: Option<TypeArgs>,
    cache: ExtensionCache,
    arena: ExprArena,
    parsed: PExprArena,
    _none: PhantomData<&'a ()>,
    feature_set: FeatureSet,
}

impl<'a> TypeChecker<'a> {
    pub fn check(
        modules: Vec<PStmt>,
        feature_sets: Vec<FeatureSet>,
        proj: Project,
        lsp: Option<LspInput>,
        arena: PExprArena,
    ) -> (Project, ExprArena) {
        let mut this = Self {
            safety: Safety::Safe,
            current: ScopeId::ROOT,
            proj,
            lsp_input: lsp.unwrap_or_default(),
            listening_vars: None,
            current_static: None,
            current_call_ty_args: None,
            cache: Default::default(),
            arena: ExprArena::with_capacity(arena.len()),
            parsed: arena,
            _none: Default::default(),
            feature_set: FeatureSet::new(),
        };

        let mut autouse = vec![];
        let mut last_file_id = None;
        let mut last_scope = ScopeId::ROOT;
        for (module, feat) in modules.into_iter().zip(feature_sets.into_iter()) {
            this.feature_set = feat;
            last_file_id = Some(module.data.span.file);

            let stmt = this.declare_stmt(&mut autouse, &module).unwrap();
            for scope in autouse.drain(..) {
                this.enter_id_and_resolve(scope, |_| {});

                for (&name, &item) in this.proj.scopes[scope].tns.iter() {
                    if item.public {
                        this.proj.autouse_tns.entry(name).or_insert(Vis::new(*item, false));
                    }
                }

                for (&name, &item) in this.proj.scopes[scope].vns.iter() {
                    if item.public {
                        this.proj.autouse_vns.entry(name).or_insert(Vis::new(*item, false));
                    }
                }
            }

            let DStmt::Module { id, .. } = stmt else { unreachable!() };
            last_scope = id;
            this.check_stmt(stmt);
        }

        this.proj.main_module = Some(last_scope);
        this.proj.main = this.proj.scopes[last_scope]
            .vns
            .get(&Strings::FN_MAIN)
            .and_then(|id| id.as_fn())
            .copied();
        if !this.proj.conf.is_library && !this.proj.conf.in_test_mode() {
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

        if this.proj.test_runner.is_none() && this.proj.conf.in_test_mode() {
            this.proj.diag.report(Error::new(
                "missing test runner function",
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

        (this.proj, this.arena)
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

    fn check_local(&mut self, id: VariableId, span: Span, mut in_closure: bool) {
        let var = self.proj.scopes.get(id);
        if self.current_function() != self.proj.scopes.function_of(var.scope) {
            self.proj.diag.report(Error::new("cannot reference local of enclosing function", span));
        }

        if self.current_static.as_ref().is_some_and(|v| !self.is_var_accessible(v.0, var.scope)) {
            self.proj.diag.report(Error::new(
                "cannot reference local from outside of static initializer",
                span,
            ));
        }

        // TODO: actually respect the policy, nested closure capture
        let mut closure_policy = None;
        for (sid, scope) in self.proj.scopes.walk(self.current) {
            if sid == var.scope {
                if closure_policy.is_some() {
                    self.proj.diag.report(Error::new(
                        "cannot access non-captured local from inside a closure",
                        span,
                    ));
                }
                return;
            }

            if let ScopeKind::Lambda(_, policy) = scope.kind {
                if !in_closure {
                    closure_policy = Some(policy);
                } else {
                    // Skip the first closure if we are checking closure captures
                    in_closure = false;
                }
            }
        }
    }
}

/// LSP routines
impl TypeChecker<'_> {
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

                if proj.str(f.name.data).contains('$') {
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
            self.resolve_impls(ut_id);

            let data = self.proj.scopes.get(ut_id);
            let cap = self.can_access_privates(data.scope);
            if method {
                for (name, _) in data.members.iter().filter(|(_, m)| m.public || cap) {
                    completions.push(LspItem::Property(Some(ty), ut_id, *name))
                }
            }

            for tr in self.proj.scopes.get(ut_id).impls.iter_checked() {
                for tr in self.proj.scopes.walk_super_traits(tr.id) {
                    let data = self.proj.scopes.get(tr);
                    add_methods(&self.proj, &mut completions, &data.fns, cap, tr, false);
                }
            }

            add_methods(&self.proj, &mut completions, &data.fns, cap, ut_id, false);
        } else if let Some(tr) = self.proj.types[ty].as_dyn_pointee() {
            let tr_id = tr.id;
            self.resolve_impls(tr_id);
            for tr in self.proj.scopes.walk_super_traits(tr_id) {
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

        let extensions = self.extensions_in_scope_for(ty);
        for ext in extensions.iter() {
            self.resolve_impls(ext.id);
            for imp in self.proj.scopes.get(ext.id).impls.iter_checked() {
                for tr in self.proj.scopes.walk_super_traits(imp.id) {
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
                        if var.kind.is_local()
                            && current_func != self.proj.scopes.function_of(var.scope)
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
                    strdata!(self, self.proj.scopes.get(id).name.data).starts_with('$')
                        || self.proj.scopes.get(id).kind.is_extension()
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
            add_from_vns(&mut completions, &self.proj.autouse_vns, false);

            if !saw_root {
                add_from_vns(&mut completions, &self.proj.scopes[ScopeId::ROOT].vns, false);
            }
        }

        add_from_tns(&mut completions, &self.proj.autouse_tns);
        if !saw_root {
            add_from_tns(&mut completions, &self.proj.scopes[ScopeId::ROOT].tns);
        }

        #[rustfmt::skip]
        let builtins = [
            "void", "never", "f32", "f64", "bool", "char", "int", "uint", "u8", "i8", "u16", "i16",
            "u32", "i32", "u64", "i64", "u128", "i128",
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
                        if var.kind.is_local() {
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

    fn check_arg_label_hover(&mut self, span: Span, param: CheckedParam, fid: FunctionId) {
        let Some(ty_args) = &self.current_call_ty_args else {
            return;
        };
        let f = self.proj.scopes.get(fid);
        if let Some(owner) = f.constructor {
            let ty = param.ty.with_templates(&self.proj.types, ty_args);
            return self.check_hover(span, LspItem::Property(Some(ty), owner, param.label));
        }

        if let ParamPattern::Checked(patt) = param.patt
            && let PatternData::Variable(id) = patt.data
        {
            self.check_hover(span, LspItem::FnParamLabel(id, fid));
        }
    }
}

/// Forward declaration pass routines
impl<'a> TypeChecker<'a> {
    fn insert_user_type(&mut self, value: UserType, public: bool) -> UserTypeId {
        let id = self.insert::<UserTypeId>(value, public, true);
        if let Some(name) = self.proj.scopes.get(id).attrs.lang {
            self.proj.scopes.lang_types.insert(name, id);
        }
        for (&name, m) in self.proj.scopes.get(id).members.iter() {
            check_hover!(self, m.span, LspItem::Property(None, id, name));
        }
        id
    }

    fn declare_struct(
        &mut self,
        span: Span,
        base: &Struct,
        attrs: &Attributes,
        packed: bool,
    ) -> DStmt {
        let pub_constructor = base.public && !base.members.iter().any(|m| !m.public);
        let (ut, init, fns, impls) = self.enter(ScopeKind::None, |this| {
            let init = this.enter(ScopeKind::None, |this| {
                this.declare_fn(Located::nowhere(&Fn {
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
                            patt: nowhere(member.name.data, Pattern::Path),
                            ty: member.ty,
                            default: member.default,
                        })
                        .collect(),
                    ret: Some(PExprArena::HINT_THIS),
                    body: None,
                    attrs: Default::default(),
                    typ: FunctionType::Normal,
                }))
            });
            let mut members = IndexMap::with_capacity(base.members.len());
            for member in base.members.iter() {
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

            let (impls, blocks, block_data, subscripts) =
                this.declare_impl_blocks(&base.impls, &base.operators);
            let mut fns = this.declare_fns(&base.functions);
            let ut = this.ut_from_stuff(
                attrs,
                base.name,
                base.public,
                members,
                UserTypeKind::Struct(init.id, packed),
                &base.type_params,
                &fns,
                impls,
                block_data,
                &blocks,
                &subscripts,
                span,
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
        span: Span,
        tag: Option<TypeHint>,
        base: &Struct,
        pvariants: &[Variant],
        attrs: &Attributes,
    ) -> DStmt {
        let (ut, impls, fns, member_cons_len) = self.enter(ScopeKind::None, |this| {
            let mut variants = IndexMap::with_capacity(base.members.len());
            let mut members = IndexMap::with_capacity(base.members.len());
            let mut params = Vec::with_capacity(base.members.len());
            let mut fns = Vec::with_capacity(base.members.len());
            for member in base.members.iter() {
                if members
                    .insert(
                        member.name.data,
                        CheckedMember::new(
                            member.public,
                            this.declare_type_hint(member.ty),
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
                    patt: nowhere(member.name.data, Pattern::Path),
                    ty: member.ty,
                    default: member.default,
                });
            }

            let (impls, blocks, block_data, subscripts) =
                this.declare_impl_blocks(&base.impls, &base.operators);
            let mut enum_union = true;
            for variant in pvariants {
                let mut params = params.clone();
                if let Some((smembers, _)) = &variant.data {
                    enum_union = false;

                    let mut names = HashSet::new();
                    for member in smembers {
                        if members.contains_key(&member.name.data) {
                            this.error(Error::shared_member(
                                strdata!(this, member.name.data),
                                member.name.span,
                            ))
                        }

                        if !names.insert(member.name.data) {
                            // We don't have to report an error, the typehint check will do it for
                            // us
                            continue;
                        }

                        params.push(Param {
                            keyword: !strdata!(this, member.name.data)
                                .starts_with(|ch: char| ch.is_ascii_digit()),
                            patt: nowhere(member.name.data, Pattern::Path),
                            ty: member.ty,
                            default: member.default,
                        });
                    }
                }

                variants.insert(
                    variant.name.data,
                    CheckedVariant {
                        ty: variant.data.as_ref().map(|data| this.declare_type_hint(data.1)),
                        span: variant.name.span,
                        discrim: variant.tag.map(Discriminant::Unchecked).unwrap_or_default(),
                    },
                );

                fns.push(this.declare_fn(Located::nowhere(&Fn {
                    public: base.public,
                    name: Located::new(variant.name.span, variant.name.data),
                    is_extern: false,
                    is_async: false,
                    variadic: false,
                    is_unsafe: false,
                    type_params: vec![],
                    params,
                    ret: Some(PExprArena::HINT_THIS),
                    body: None,
                    attrs: Default::default(),
                    typ: FunctionType::Normal,
                })));
            }
            let member_cons_len = fns.len();
            fns.extend(this.declare_fns_iter(&base.functions));
            let tag = tag.map(|tag| this.declare_type_hint(tag)).unwrap_or_default();
            let ut = this.ut_from_stuff(
                attrs,
                base.name,
                base.public,
                members,
                UserTypeKind::Union(Union { tag, variants, enum_union }),
                &base.type_params,
                &fns,
                impls,
                block_data,
                &blocks,
                &subscripts,
                span,
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

    fn declare_unsafe_union(&mut self, span: Span, base: &Struct, attrs: &Attributes) -> DStmt {
        let (ut, fns, impls) = self.enter(ScopeKind::None, |this| {
            let mut members = IndexMap::with_capacity(base.members.len());
            for member in base.members.iter() {
                let prev = members.insert(
                    member.name.data,
                    CheckedMember::new(true, this.declare_type_hint(member.ty), member.name.span),
                );
                if prev.is_some() {
                    let name = strdata!(this, member.name.data);
                    this.error(Error::redefinition_k("member variable", name, member.name.span))
                }
            }

            let (impls, blocks, block_data, subscripts) =
                this.declare_impl_blocks(&base.impls, &base.operators);
            let mut fns = this.declare_fns(&base.functions);
            let ut = this.ut_from_stuff(
                attrs,
                base.name,
                base.public,
                members,
                UserTypeKind::UnsafeUnion,
                &base.type_params,
                &fns,
                impls,
                block_data,
                &blocks,
                &subscripts,
                span,
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

    fn declare_stmt(&mut self, autouse: &mut Vec<ScopeId>, stmt: &PStmt) -> Option<DStmt> {
        let has_attr_check = matches!(
            stmt.data.data,
            PStmtData::Struct { .. }
                | PStmtData::Union { .. }
                | PStmtData::UnsafeUnion { .. }
                | PStmtData::Trait { .. }
                | PStmtData::Extension { .. }
                | PStmtData::Fn { .. }
                | PStmtData::Module { .. }
                | PStmtData::Binding { .. }
        );

        if self.check_disabled(&stmt.attrs, stmt.data.span, !has_attr_check) {
            return None;
        }

        Some(match &stmt.data.data {
            &PStmtData::Module { public, name, ref body, .. } => {
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
                    if stmt.attrs.iter().any(|attr| attr.name.data.is_str_eq(autouse_id)) {
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
                            .iter()
                            .flat_map(|stmt| this.declare_stmt(autouse, stmt))
                            .collect(),
                    }
                })
            }
            &PStmtData::ModuleOOL { name, resolved, .. } => {
                // TODO: report an error if this is not at the top level of a main.ctl/equivalent
                if !resolved {
                    self.proj.diag.report(Error::new(
                        format!(
                            "couldn't find module '{0}' (couldn't read {0}.ctl or {0}/main.ctl)",
                            strdata!(self, name.data),
                        ),
                        name.span,
                    ));
                    return None;
                } else {
                    DStmt::ModuleOOL { name }
                }
            }
            PStmtData::Struct { base, packed } => {
                self.declare_struct(stmt.data.span, base, &stmt.attrs, *packed)
            }
            PStmtData::Union { tag, base, variants } => {
                self.declare_union(stmt.data.span, *tag, base, variants, &stmt.attrs)
            }
            PStmtData::UnsafeUnion(base) => {
                self.declare_unsafe_union(stmt.data.span, base, &stmt.attrs)
            }
            &PStmtData::Trait {
                public,
                name,
                ref type_params,
                ref impls,
                ref functions,
                sealed,
                is_unsafe: _,
                ref assoc_types,
            } => {
                let (tr, fns, this_id) = self.enter(ScopeKind::None, |this| {
                    let impls = TraitImpls::Unchecked(
                        impls
                            .iter()
                            .map(|path| TraitImplData::Path(this.current, path.clone()))
                            .collect(),
                    );
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
                    let assoc_types = this.declare_associated_types(assoc_types);
                    let tr = this.ut_from_stuff(
                        &stmt.attrs,
                        name,
                        public,
                        Default::default(),
                        UserTypeKind::Trait { this: this_id, sealed, assoc_types },
                        type_params,
                        &fns,
                        impls,
                        vec![],
                        &[],
                        &[],
                        stmt.data.span,
                    );
                    (tr, fns, this_id)
                });

                let scope = tr.body_scope;
                let id = self.insert_user_type(tr, public);
                let imp = GenericTrait::from_type_params(&self.proj.scopes, &self.proj.types, id);
                let impls = self.proj.scopes.get_mut(this_id).impls.as_unchecked_mut().unwrap();
                impls.push(TraitImplData::Checked(imp));
                self.proj.scopes[scope].kind = ScopeKind::UserType(id);
                DStmt::Trait { id, fns }
            }
            PStmtData::Extension { public, name, ty, type_params, impls, functions, operators } => {
                let (ext, impl_blocks, fns) = self.enter(ScopeKind::None, |this| {
                    let (impls, blocks, block_data, subscripts) =
                        this.declare_impl_blocks(impls, operators);
                    let mut fns = this.declare_fns(functions);
                    let ty = this.declare_type_hint(*ty);
                    let ext = this.ut_from_stuff(
                        &stmt.attrs,
                        *name,
                        *public,
                        Default::default(),
                        UserTypeKind::Extension(ty),
                        type_params,
                        &fns,
                        impls,
                        block_data,
                        &blocks,
                        &subscripts,
                        stmt.data.span,
                    );
                    fns.extend(subscripts);
                    (ext, blocks, fns)
                });

                let scope = ext.body_scope;
                let id = self.insert_user_type(ext, *public);
                self.proj.scopes[scope].kind = ScopeKind::UserType(id);
                DStmt::Extension { id, impls: impl_blocks, fns }
            }
            &PStmtData::Alias { public, name, ref type_params, ty } => {
                // TODO: ensure all type params are referenced
                let value = self.enter(ScopeKind::None, |this| Alias {
                    public,
                    name,
                    type_params: this.declare_type_params(type_params),
                    ty: this.declare_type_hint(ty),
                    body_scope: this.current,
                });
                let id = self.insert::<AliasId>(value, public, true);
                DStmt::Alias { id }
            }
            PStmtData::Fn(f) => DStmt::Fn(self.declare_fn(Located::new(stmt.data.span, f))),
            &PStmtData::Binding { public, constant, mut name, mutable, ty, value, is_extern } => {
                let ty = self.declare_type_hint(ty);
                let mut unused = true;
                if name.data == Strings::UNDERSCORE {
                    name.data = Strings::EMPTY;
                    unused = false;
                }

                let attrs = VariableAttrs::relevant(&stmt.attrs, &mut self.proj);
                DStmt::Binding {
                    id: self.insert::<VariableId>(
                        Variable {
                            attrs,
                            public,
                            name,
                            ty,
                            unused,
                            is_extern,
                            kind: [VariableKind::Static, VariableKind::Const][constant as usize],
                            has_hint: true,
                            mutable: mutable && !constant,
                            ..Default::default()
                        },
                        public,
                        true,
                    ),
                    value,
                }
            }
            PStmtData::Use(path) => {
                let scope = match path.origin {
                    UsePathOrigin::Root(_) => Some(ScopeId::ROOT),
                    UsePathOrigin::Super(span) => {
                        if let Some(scope) = self.get_super(span) {
                            Some(scope)
                        } else {
                            return None;
                        }
                    }
                    UsePathOrigin::Here => None,
                };

                self.resolve_use(path.public, scope, &path.component, false, true);
                return None;
            }
            &PStmtData::Let { ty, value, ref patt } => DStmt::Let { ty, value, patt: patt.clone() },
            &PStmtData::Guard { cond, body } => DStmt::Guard { cond, body },
            &PStmtData::Expr(expr) => DStmt::Expr(expr),
            &PStmtData::Defer(expr) => DStmt::Defer(expr),
            PStmtData::Error => return None,
        })
    }

    fn declare_fn(&mut self, f: Located<&Fn>) -> DFn {
        let full_span = f.span;
        let f = f.data;
        if f.variadic && (!f.is_extern || f.body.is_some()) {
            self.error(Error::new("only imported extern functions may be variadic", f.name.span))
        }

        let attrs = FunctionAttrs::relevant(f.name.data, &f.attrs, &mut self.proj);
        let id = self.insert::<FunctionId>(
            Function {
                public: f.public,
                attrs,
                name: f.name,
                is_extern: f.is_extern,
                is_async: f.is_async,
                is_unsafe: f.is_unsafe,
                variadic: f.variadic,
                typ: f.typ,
                has_body: f.body.is_some(),
                type_params: Vec::new(),
                params: Vec::new(),
                ret: TypeId::UNKNOWN,
                body: None,
                body_scope: ScopeId::ROOT,
                constructor: None,
                full_span,
            },
            f.public,
            true,
        );

        let allow_safe_extern = self.proj.scopes.get(id).attrs.safe_extern
            || self.proj.scopes.get(id).attrs.intrinsic.is_some();
        self.enter(ScopeKind::Function(id), |this| {
            if !allow_safe_extern && f.is_extern && f.body.is_none() {
                this.proj.scopes.get_mut(id).is_unsafe = true;
            }

            this.proj.scopes.get_mut(id).body_scope = this.current;
            this.proj.scopes.get_mut(id).type_params = this.declare_type_params(&f.type_params);
            this.proj.scopes.get_mut(id).params = f
                .params
                .iter()
                .enumerate()
                .map(|(i, param)| CheckedParam {
                    keyword: param.keyword,
                    label: match &param.patt.data {
                        Pattern::MutBinding(name) => Some(*name),
                        Pattern::Path(name) => name.as_identifier().map(|name| name.data),
                        _ => None,
                    }
                    .unwrap_or_else(|| intern!(this, "$unnamed{i}")),
                    patt: ParamPattern::Unchecked(param.patt.clone()),
                    ty: this.declare_type_hint(param.ty),
                    default: param.default.map(|expr| DefaultExpr::Unchecked(this.current, expr)),
                })
                .collect();
            this.proj.scopes.get_mut(id).ret =
                f.ret.map(|ret| this.declare_type_hint(ret)).unwrap_or(TypeId::VOID);

            DFn { id, body: f.body }
        })
    }

    fn declare_fns(&mut self, fns: &[Located<Fn>]) -> Vec<DFn> {
        self.declare_fns_iter(fns).collect()
    }

    fn declare_fns_iter<'b>(
        &mut self,
        fns: &'b [Located<Fn>],
    ) -> impl Iterator<Item = DFn> + use<'_, 'a, 'b> {
        fns.iter().flat_map(|f| {
            if !self.check_disabled(&f.data.attrs, f.span, false) {
                Some(self.declare_fn(f.as_ref()))
            } else {
                None
            }
        })
    }

    fn declare_op_fn(
        &mut self,
        f: &Located<OperatorFn>,
        impls: &mut Vec<TraitImplData>,
        blocks: &mut Vec<DImplBlock>,
        data: &mut Vec<ImplBlockData>,
        subscripts: &mut Vec<DFn>,
    ) {
        if self.check_disabled(&f.data.attrs, f.span, false) {
            return;
        }

        let full_span = f.span;
        let f = &f.data;

        use OperatorFnType as O;
        let (tr_name, fn_name, ty_args) =
            match f.name.data {
                O::Minus if f.params.len() > 1 => {
                    let op = BinaryOp::try_from(f.name.data).unwrap();
                    let (tr_name, fn_name) = self.get_binary_op(op).unwrap();
                    (
                        tr_name,
                        fn_name,
                        vec![
                            f.params
                                .get(1)
                                .map(|p| p.ty)
                                .unwrap_or(Located::nowhere(PExprArena::HINT_ERROR)),
                            f.ret.unwrap_or(PExprArena::HINT_VOID),
                        ],
                    )
                }
                O::Minus | O::Bang => {
                    let op = UnaryOp::try_from_postfix_fn(f.name.data).unwrap();
                    let (tr_name, fn_name) = self.get_unary_op(op).unwrap();
                    (tr_name, fn_name, vec![f.ret.unwrap_or(PExprArena::HINT_VOID)])
                }
                O::Increment | O::Decrement => {
                    let op = UnaryOp::try_from_postfix_fn(f.name.data).unwrap();
                    let (tr_name, fn_name) = self.get_unary_op(op).unwrap();
                    (tr_name, fn_name, vec![])
                }
                O::Subscript | O::SubscriptAssign => {
                    let name = intern!(self, "$sub{}", subscripts.len());
                    subscripts.push(self.declare_fn(Located::new(
                        full_span,
                        &Fn::from_operator_fn(name, f.clone()),
                    )));
                    return;
                }
                _ => {
                    let op = BinaryOp::try_from(f.name.data).unwrap();
                    let (tr_name, fn_name) = self.get_binary_op(op).unwrap();
                    if let Some(p) =
                        f.params.get(1).filter(|_| matches!(f.name.data, O::Cmp | O::Eq)).cloned()
                    {
                        if let TypeHintData::Ptr(inner) = self.parsed.hints.get(p.ty.data) {
                            (tr_name, fn_name, vec![*inner])
                        } else {
                            // impl check will take care of issuing an error for this case
                            (tr_name, fn_name, vec![p.ty])
                        }
                    } else {
                        let mut args = vec![
                            f.params
                                .get(1)
                                .map(|p| p.ty)
                                .unwrap_or(Located::nowhere(PExprArena::HINT_ERROR)),
                        ];
                        if !op.is_assignment() {
                            args.push(f.ret.unwrap_or(PExprArena::HINT_VOID));
                        }
                        (tr_name, fn_name, args)
                    }
                }
            };

        let span = f.name.span;
        let mut f = Fn::from_operator_fn(fn_name, f.clone());
        let block = self.enter(ScopeKind::Impl(impls.len()), |this| {
            data.push(ImplBlockData {
                type_params: this.declare_type_params(&std::mem::take(&mut f.type_params)),
                assoc_types: Default::default(),
            });
            let lf = Located::new(full_span, &f);
            DImplBlock { span: f.name.span, scope: this.current, fns: vec![this.declare_fn(lf)] }
        });
        impls.push(TraitImplData::Operator { tr: tr_name, ty_args, span, scope: block.scope });
        blocks.push(block);
    }

    fn declare_type_params(&mut self, vec: &TypeParams) -> Vec<UserTypeId> {
        vec.iter()
            .map(|(name, impls)| {
                self.insert(
                    UserType::template(
                        *name,
                        self.current,
                        TraitImpls::Unchecked(
                            impls
                                .iter()
                                .map(|path| TraitImplData::Path(self.current, path.clone()))
                                .collect(),
                        ),
                    ),
                    false,
                    true,
                )
            })
            .collect()
    }

    fn declare_associated_types(&mut self, vec: &TypeParams) -> HashMap<StrId, UserTypeId> {
        vec.iter()
            .map(|(name, impls)| {
                (
                    name.data,
                    self.insert(
                        UserType::template(
                            *name,
                            self.current,
                            TraitImpls::Unchecked(
                                impls
                                    .iter()
                                    .map(|path| TraitImplData::Path(self.current, path.clone()))
                                    .collect(),
                            ),
                        ),
                        false,
                        true,
                    ),
                )
            })
            .collect()
    }

    fn declare_impl_blocks(
        &mut self,
        blocks: &[Located<ImplBlock>],
        operators: &[Located<OperatorFn>],
    ) -> (TraitImpls, Vec<DImplBlock>, Vec<ImplBlockData>, Vec<DFn>) {
        let mut impls = Vec::new();
        let mut declared_blocks = Vec::new();
        let mut subscripts = Vec::new();
        let mut data = Vec::new();
        for block in blocks {
            let ImplBlock { path, functions, type_params, attrs, assoc_types } = &block.data;
            if self.check_disabled(attrs, block.span, true) {
                continue;
            }

            let block = self.enter(ScopeKind::Impl(impls.len()), |this| {
                let mut atypes = HashMap::with_capacity(assoc_types.len());
                for (name, hint) in assoc_types {
                    let prev = atypes
                        .insert(name.data, Located::new(name.span, this.declare_type_hint(*hint)));
                    if prev.is_some() {
                        let v = strdata!(this, name.data);
                        this.error(Error::redefinition_k("associated type", v, name.span))
                    }
                }

                data.push(ImplBlockData {
                    type_params: this.declare_type_params(type_params),
                    assoc_types: atypes,
                });

                DImplBlock {
                    span: path.final_component_span(),
                    scope: this.current,
                    fns: this.declare_fns(functions),
                }
            });
            impls.push(TraitImplData::Path(block.scope, path.clone()));
            declared_blocks.push(block);
        }

        for func in operators {
            self.declare_op_fn(func, &mut impls, &mut declared_blocks, &mut data, &mut subscripts);
        }

        (TraitImpls::Unchecked(impls), declared_blocks, data, subscripts)
    }

    fn declare_type_hint(&mut self, hint: TypeHint) -> TypeId {
        self.proj.types.insert(Type::Unresolved(hint, self.current))
    }

    #[allow(clippy::too_many_arguments)]
    fn ut_from_stuff(
        &mut self,
        attrs: &Attributes,
        name: Located<StrId>,
        public: bool,
        members: IndexMap<StrId, CheckedMember>,
        kind: UserTypeKind,
        type_params: &TypeParams,
        fns: &[DFn],
        impls: TraitImpls,
        block_data: Vec<ImplBlockData>,
        blocks: &[DImplBlock],
        subscripts: &[DFn],
        full_span: Span,
    ) -> UserType {
        UserType {
            attrs: UserTypeAttrs::relevant(attrs, &mut self.proj),
            name,
            public,
            kind,
            impls,
            impl_blocks: block_data,
            members,
            type_params: self.declare_type_params(type_params),
            body_scope: self.current,
            fns: fns
                .iter()
                .chain(blocks.iter().flat_map(|block| block.fns.iter()))
                .map(|f| Vis::new(f.id, self.proj.scopes.get(f.id).public))
                .collect(),
            subscripts: subscripts.iter().map(|s| s.id).collect(),
            members_resolved: false,
            recursive: false,
            interior_mutable: false,
            full_span,
        }
    }

    fn check_disabled(&mut self, attrs: &Attributes, span: Span, check_attrs: bool) -> bool {
        if self.is_disabled_by_attrs(attrs, check_attrs) {
            self.proj.diag.add_inactive(span);
            return true;
        }

        false
    }
}

/// Typechecking pass routines
impl TypeChecker<'_> {
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
                        this.resolve_use(stmt.public, stmt.scope, &stmt.comp, stmt.in_type, false);
                    }
                });
            }

            f(this)
        })
    }

    fn check_stmt(&mut self, stmt: DStmt) -> Option<CStmt> {
        match stmt {
            DStmt::Module { id, body } => {
                self.enter_id_and_resolve(id, |this| {
                    for stmt in body {
                        this.check_stmt(stmt);
                    }
                });
            }
            DStmt::ModuleOOL { name } => {
                let item = self.proj.scopes[self.current].find_in_tns(name.data);
                if let Some(scope) = item.and_then(|item| item.id.into_module().ok()) {
                    self.check_hover(name.span, LspItem::Module(scope, false));
                }
            }
            DStmt::Struct { init, id, impls, fns } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.resolve_members(id);
                    this.check_fn(init);
                    let this_ty = Type::User(GenericUserType::from_id(
                        &this.proj.scopes,
                        &this.proj.types,
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
                        &this.proj.types,
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
            DStmt::Alias { id } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    this.resolve_alias(this.proj.scopes.get(id).name.span, id);
                });
            }
            DStmt::Expr(expr) => return Some(CStmt::Expr(self.check_expr(expr, None))),
            DStmt::Let { ty, value, ref patt } => {
                let span = patt.span;
                if let Some(ty) = ty {
                    let ty = self.resolve_typehint(ty);
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
                        return Some(CStmt::Let(patt, Some(value)));
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
                        return Some(CStmt::Let(patt, None));
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

                    return Some(CStmt::Let(patt, Some(value)));
                } else {
                    return self.error(Error::new("cannot infer type", patt.span));
                }
            }
            DStmt::Defer(expr) => {
                return Some(CStmt::Defer(
                    self.enter(ScopeKind::Defer, |this| this.check_expr(expr, None)),
                ));
            }
            DStmt::Guard { cond, body } => {
                let (cond, vars) = self.check_condition(cond);
                let span = body.span;
                let body = self.check_expr_no_never_propagation(body, Some(TypeId::NEVER));
                let body = self.type_check_checked(body, TypeId::NEVER, span);
                self.define(&vars);
                return Some(CStmt::Guard { cond, body });
            }
            DStmt::Fn(f) => self.check_fn(f),
            DStmt::Binding { id, value } => {
                // FIXME: reject non-constexpr const/static
                let ty = resolve_type!(self, self.proj.scopes.get_mut(id).ty);

                self.proj.static_deps.insert(id, Dependencies::Resolving);
                let prev = self.current_static.replace((id, HashSet::new()));
                let value = value.map(|value| {
                    self.enter(ScopeKind::Static(id), |this| this.type_check(value, ty))
                });

                let (_, deps) = std::mem::replace(&mut self.current_static, prev).unwrap();
                self.proj.static_deps.insert(id, Dependencies::Resolved(deps));

                let var = self.proj.scopes.get_mut(id);
                if value.is_none() {
                    match var.kind {
                        VariableKind::Static if !var.is_extern => self.proj.diag.report(
                            Error::new("non-extern static must be initialized", var.name.span),
                        ),
                        VariableKind::Const => self
                            .proj
                            .diag
                            .report(Error::new("constant must be initialized", var.name.span)),
                        _ => {}
                    }
                }

                var.value = value;
            }
        }

        None
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
                let ty =
                    Type::User(GenericUserType::from_id(&self.proj.scopes, &self.proj.types, ty));
                ty_args.insert(id, self.proj.types.insert(ty));
            } else {
                ty_args.insert(id, TypeId::UNKNOWN);
            }
        }

        if let Some((tr, this)) = tr {
            ty_args.insert(*self.proj.scopes.get(tr).kind.as_trait().unwrap().0, this);
        }

        let compare_types = |has: TypeId, wants: TypeId| {
            let wants = wants.with_templates(&self.proj.types, &ty_args);
            if has != wants {
                Err(format!(
                    "expected '{}', found '{}'",
                    self.proj.fmt_ty(wants),
                    self.proj.fmt_ty(has),
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
            for (s, t) in s.impls.iter_checked().zip(t.impls.iter_checked()) {
                for (&s, &t) in s.ty_args.values().zip(t.ty_args.values()) {
                    if let Err(err) = compare_types(s, t) {
                        let data = strdata!(self, name);
                        return Err(format!("type parameter '{data}' is incorrect: {err}"));
                    }
                }
            }

            if s.impls
                .as_checked()
                .zip(t.impls.as_checked())
                .is_some_and(|(l, r)| l.len() != r.len())
            {
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

    fn check_impl_block(
        &mut self,
        this: TypeId,
        mut tr: GenericTrait,
        data: &ImplBlockData,
        block: DImplBlock,
    ) {
        let tr_ut = self.proj.scopes.get(tr.id);
        let tr_name = tr_ut.name.data;
        let (&this_id, &sealed, tr_assoc_types) = tr_ut.kind.as_trait().unwrap();
        let tr_assoc_types = tr_assoc_types.clone();
        if sealed && !self.can_access_privates(tr_ut.scope) {
            self.error(Error::new(
                format!("cannot implement sealed trait '{}'", strdata!(self, tr_ut.name.data)),
                block.span,
            ))
        }

        for (&name, typ) in data.assoc_types.iter() {
            let Some(&id) = tr_assoc_types.get(&name) else {
                self.proj.diag.report(Error::new(
                    format!(
                        "trait '{}' has no associated type '{}'",
                        strdata!(self, tr_name),
                        strdata!(self, name)
                    ),
                    typ.span,
                ));
                continue;
            };

            self.check_hover(typ.span, LspItem::Type(id));
            self.check_bounds(&TypeArgs::default(), typ.data, id, typ.span);
            tr.ty_args.insert(id, typ.data);
        }

        self.resolve_impls(tr.id);
        let exts = self.extensions_in_scope(self.current);
        for mut dep in self.proj.scopes.get(tr.id).impls.clone().into_iter_checked() {
            for ty_arg in dep.ty_args.values_mut() {
                if self.proj.types[*ty_arg].as_user().is_some_and(|ut| ut.id == this_id) {
                    *ty_arg = this;
                }
            }

            dep.fill_templates(&self.proj.types, &tr.ty_args);
            if self.find_trait_impl(&exts, dep.id, this).is_none_or(|tr| tr != dep) {
                self.proj.diag.report(Error::new(
                    format!(
                        "trait '{}' requires implementation of trait '{}'",
                        self.proj.fmt_ut(&tr),
                        self.proj.fmt_ut(&dep),
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
                self.error(Error::new(
                    format!(
                        "must implement '{}::{}'",
                        self.proj.fmt_ut(&tr),
                        strdata!(self, self.proj.scopes.get(*id).name.data)
                    ),
                    block.span,
                ))
            }
        }
    }

    fn check_fn(&mut self, DFn { id, body }: DFn) {
        // TODO: disallow private type in public interface
        self.enter_id_and_resolve(self.proj.scopes.get(id).body_scope, |this| {
            this.resolve_proto(id);

            let mut names = vec![];
            for i in 0..this.proj.scopes.get(id).params.len() {
                let Some(patt) = this.proj.scopes.get_mut(id).params[i].patt.as_unchecked().cloned() else {
                    continue;
                };
                let ty = this.proj.scopes.get(id).params[i].ty;
                let span = patt.span;
                let patt = this.check_pattern(PatternParams {
                    scrutinee: ty,
                    mutable: false,
                    pattern: &patt,
                    typ: if body.is_none() { PatternType::BodylessFn } else { PatternType::Fn },
                    has_hint: true,
                });
                if !patt.irrefutable {
                    this.error(Error::must_be_irrefutable("parameter patterns", span))
                } else {
                    this.proj.scopes.get_mut(id).params[i].patt = ParamPattern::Checked(patt);
                }

                names.push(this.proj.strings.get_or_intern(format!("{i}")));
            }

            // XXX: See Codegen::get_tuple
            this.proj.scopes.create_tuple_user_type(names, &this.proj.types);

            let func = this.proj.scopes.get(id);
            if func.attrs.panic_handler {
                if body.is_none() {
                    this.proj.diag.report(Error::new("panic handler must have a definition", func.name.span));
                }

                if let Some(_old) = this.proj.panic_handler.replace(id) {
                    // TODO: report that it was previously defined at the span of _old
                    this.proj.diag.report(Error::new("a panic handler already exists", func.name.span));
                }

                let panic = this.proj.scopes
                    .functions()
                    .find(|(_, f)| f.attrs.intrinsic == Some(Intrinsic::Panic))
                    .map(|p| p.0);
                if let Some(panic) = panic {
                    let fn_name = func.name;
                    this.resolve_proto(panic);
                    if let Err(why) = this.check_signature_match(None, id, panic, &TypeArgs::default()) {
                        let data = strdata!(this, fn_name.data);
                        this.proj.diag.report(Error::invalid_impl(data, &why, fn_name.span))
                    }
                }
            } else if func.attrs.test_runner {
                if body.is_none() {
                    this.proj.diag.report(Error::new("test runner must have a definition", func.name.span));
                }

                // TODO: validate the signature of this method
                if let Some(_old) = this.proj.test_runner.replace(id) {
                    // TODO: report that it was previously defined at the span of _old
                    this.proj.diag.report(Error::new("a test runner already exists", func.name.span));
                }
            }

            let func = this.proj.scopes.get(id);
            if let Some(ut_id) = func.constructor {
                let args = func
                    .params
                    .iter()
                    .flat_map(|param| Some((param.label, this.arena.typed(
                        param.ty,
                        CExprData::Var(*param.patt.as_checked().and_then(|p| p.data.as_variable())?)
                    ))))
                    .collect();
                let variant = func.name.data;
                let ut = Type::User(GenericUserType::from_id(&this.proj.scopes, &this.proj.types, ut_id));
                this.proj.scopes.get_mut(id).body = Some(this.arena.typed(
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
                    match this.arena.get(body.data).is_yielding_block(&this.proj.scopes) {
                        // Yielding blocks already perform a type_check
                        Some(true) => {}
                        Some(false) => {
                            let func = this.proj.scopes.get(id);
                            this.proj.diag.report(Error::new(
                                format!("function '{}' must return a value of type '{}' from all code paths",
                                    strdata!(this, func.name.data),
                                    this.proj.fmt_ty(func.ret),
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
                let mut block_data = this.proj.scopes.get(id).impl_blocks[i].clone();
                for &id in block_data.type_params.iter() {
                    this.resolve_impls(id);
                }

                for (name, typ) in block_data.assoc_types.iter_mut() {
                    typ.data = resolve_type!(
                        this,
                        this.proj.scopes.get_mut(id).impl_blocks[i]
                            .assoc_types
                            .get_mut(name)
                            .unwrap()
                            .data
                    );
                }

                if let Some(gtr) = this.proj.scopes.get(id).impls.get_checked(i).cloned() {
                    if !seen.insert(gtr.clone()) {
                        this.proj.diag.report(Error::new(
                            format!("duplicate implementation of trait {}", this.proj.fmt_ut(&gtr)),
                            block.span,
                        ))
                    }

                    this.check_impl_block(this_ty, gtr, &block_data, block);
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
        let Some((trait_name, fn_name)) = self.get_binary_op(op) else {
            bail!(self, Error::invalid_operator(op, self.proj.fmt_ty(lhs.ty), span));
        };

        let Some(tr_id) = self.proj.scopes.lang_types.get(&trait_name).copied() else {
            return self.error(Error::no_lang_item(trait_name, lhs_span));
        };

        let ty =
            if op.is_assignment() { lhs.ty } else { lhs.ty.strip_references(&self.proj.types) };
        let Some(mfn) = self.lookup_unk_trait_fn(ty, tr_id, fn_name) else {
            bail!(
                self,
                Error::doesnt_implement(
                    self.proj.fmt_ty(ty),
                    strdata!(self, self.proj.scopes.get(tr_id).name.data),
                    lhs_span,
                )
            );
        };

        let f = self.proj.scopes.get(mfn.func.id);
        let [p0, p1, ..] = &f.params[..] else {
            return Default::default();
        };
        let arg0 = (p0.label, lhs.auto_deref_ex(&self.proj.types, p0.ty, &mut self.arena, false));
        let p1_ty = p1.ty.with_templates(&self.proj.types, &mfn.func.ty_args);
        let ret = f.ret.with_templates(&self.proj.types, &mfn.func.ty_args);
        let rhs_span = rhs.span;
        let rhs_name = p1.label;
        let rhs = self.check_expr(rhs, Some(p1_ty.strip_references(&self.proj.types)));
        let rhs = rhs.auto_deref(&self.proj.types, p1_ty, &mut self.arena);
        let arg0val = (rhs_name, self.type_check_checked(rhs, p1_ty, rhs_span));

        CExpr::member_call(
            ret,
            &self.proj.types,
            mfn,
            [arg0, arg0val].into(),
            self.current,
            span,
            &mut self.arena,
        )
    }

    fn check_unary(&mut self, expr: CExpr, op: UnaryOp, span: Span) -> CExpr {
        let Some((trait_name, fn_name)) = self.get_unary_op(op) else {
            bail!(self, Error::invalid_operator(op, self.proj.fmt_ty(expr.ty), span));
        };

        let Some(tr_id) = self.proj.scopes.lang_types.get(&trait_name).copied() else {
            return self.error(Error::no_lang_item(trait_name, span));
        };

        let affix = matches!(
            op,
            UnaryOp::PreDecrement
                | UnaryOp::PreIncrement
                | UnaryOp::PostDecrement
                | UnaryOp::PostIncrement
        );

        let ty = if affix { expr.ty } else { expr.ty.strip_references(&self.proj.types) };
        let Some(mfn) = self.lookup_unk_trait_fn(ty, tr_id, fn_name) else {
            bail!(
                self,
                Error::doesnt_implement(
                    self.proj.fmt_ty(ty),
                    strdata!(self, self.proj.scopes.get(tr_id).name.data),
                    span,
                )
            );
        };

        let f = self.proj.scopes.get(mfn.func.id);
        let [p0, ..] = &f.params[..] else {
            return Default::default();
        };
        if affix {
            let callee = expr.auto_deref(&self.proj.types, p0.ty, &mut self.arena);
            self.arena.typed(
                ty,
                CExprData::AffixOperator {
                    callee,
                    mfn,
                    scope: self.current,
                    postfix: matches!(op, UnaryOp::PostDecrement | UnaryOp::PostIncrement),
                    span, // TODO: use the span of the operator itself
                },
            )
        } else {
            let arg0 = expr.auto_deref(&self.proj.types, p0.ty, &mut self.arena);
            CExpr::member_call(
                f.ret.with_templates(&self.proj.types, &mfn.func.ty_args),
                &self.proj.types,
                mfn,
                [(p0.label, arg0)].into(),
                self.current,
                span,
                &mut self.arena,
            )
        }
    }

    fn check_addr(
        &mut self,
        op: UnaryOp,
        inner: Expr,
        span: Span,
        target: Option<TypeId>,
    ) -> CExpr {
        let target = target.and_then(|id| id.as_pointee(&self.proj.types));
        // allow &raw mut FOO to not be unsafe even if FOO is a static mut
        let inner = if matches!(op, UnaryOp::AddrRaw | UnaryOp::AddrRawMut)
            && matches!(self.parsed.cget(inner.data), ExprData::Path(_))
        {
            self.with_safety(Safety::Unsafe(false), |this| this.check_expr(inner, target)).1
        } else {
            self.check_expr(inner, target)
        };

        if matches!(op, UnaryOp::AddrMut | UnaryOp::AddrRawMut) {
            if matches!(self.arena.get(inner.data), CExprData::Var(id) if self.proj.scopes.get(*id).kind.is_const())
            {
                self.proj.diag.report(Warning::mut_ptr_to_const(span));
            } else if !inner.can_addrmut(&self.proj, &self.arena) {
                self.error(Error::no_mut_ptr(span))
            }
        }

        let mut typ = None;
        match self.arena.get(inner.data) {
            CExprData::Call { callee, .. } => {
                if matches!(self.arena.get(callee.data), CExprData::Fn(f, _)
                    if self.proj.scopes.get(f.id).typ.is_subscript())
                {
                    self.proj.diag.report(Warning::subscript_addr(span));
                }
            }
            CExprData::Member { source, .. } => {
                if source.ty.is_packed_struct(&self.proj) {
                    self.proj.diag.report(Warning::bitfield_addr(span));
                }
            }
            CExprData::Fn(func, _) | CExprData::MemFn(MemberFn { func, .. }, _) => {
                if matches!(op, UnaryOp::AddrRaw | UnaryOp::AddrRawMut) {
                    self.proj.diag.report(Error::new("cannot create raw pointer to function", span))
                } else if matches!(op, UnaryOp::AddrMut) {
                    self.proj
                        .diag
                        .report(Error::new("cannot create mutable pointer to function", span))
                }

                typ = Some(Type::FnPtr(func.as_fn_ptr(&self.proj.scopes, &self.proj.types)));
            }
            _ => {}
        }

        let out_ty = self.proj.types.insert(typ.unwrap_or_else(|| match op {
            UnaryOp::Addr => Type::Ptr(inner.ty),
            UnaryOp::AddrMut => Type::MutPtr(inner.ty),
            UnaryOp::AddrRaw => Type::RawPtr(inner.ty),
            UnaryOp::AddrRawMut => Type::RawMutPtr(inner.ty),
            _ => unreachable!(),
        }));
        self.arena.typed(out_ty, CExprData::Unary(op, inner))
    }

    fn infer_closure_type(&mut self, target: TypeId, i: Option<usize>) -> Option<TypeId> {
        let target = target.strip_references(&self.proj.types);
        match &self.proj.types[target] {
            Type::Fn(func) => {
                let f = self.proj.scopes.get(func.id);
                if let Some(i) = i {
                    f.params.get(i).map(|p| p.ty.with_templates(&self.proj.types, &func.ty_args))
                } else {
                    Some(f.ret.with_templates(&self.proj.types, &func.ty_args))
                }
            }
            Type::FnPtr(fn_ptr) => {
                if let Some(i) = i {
                    fn_ptr.params.get(i).copied()
                } else {
                    Some(fn_ptr.ret)
                }
            }
            Type::User(ut) if self.proj.scopes.get(ut.id).kind.is_template() => {
                let op_fn_tr = self.proj.scopes.lang_types.get(&LangType::OpFn).copied()?;
                let ut_id = ut.id;
                self.resolve_impls(ut_id);

                let mut tr = self
                    .proj
                    .scopes
                    .get(ut_id)
                    .impls
                    .iter_checked()
                    .find(|tr| tr.id == op_fn_tr)
                    .cloned()?;
                if let Some(ty_args) = &self.current_call_ty_args
                    && ty_args.contains_key(&ut_id)
                {
                    tr.fill_templates(&self.proj.types, ty_args);
                }

                // tr = Fn<Tuple, Ret>
                let ty = if let Some(i) = i {
                    let args = self.proj.types[*tr.ty_args.get_index(0)?.1].as_user()?;
                    args.ty_args
                        .get_index(i)
                        .map(|v| v.1.with_templates(&self.proj.types, &tr.ty_args))?
                } else {
                    tr.ty_args.get_index(1)?.1.with_templates(&self.proj.types, &tr.ty_args)
                };

                if let Some(ty_args) = &self.current_call_ty_args
                    && shouldnt_infer_with(&self.proj, ty_args, ty)
                {
                    return None;
                }

                Some(ty)
            }
            _ => None,
        }
    }

    fn check_lambda(
        &mut self,
        span: Span,
        t: Option<TypeId>,
        captures: &[Capture],
        params: &[(Located<Pattern>, Option<TypeHint>)],
        ret: Option<TypeId>,
        body: Expr,
    ) -> CExpr {
        let Some(&fn_tr) = self.proj.scopes.lang_types.get(&LangType::OpFn) else {
            return self.error(Error::no_lang_item(LangType::OpFn, span));
        };

        let closure_ut_scope =
            self.enter_id(ScopeId::ROOT, |this| this.enter(ScopeKind::None, |this| this.current));

        let mut members = IndexMap::new();
        let mut instance = IndexMap::new();
        for capture in captures {
            if let &Capture::New { mutable, ident: name, expr } = capture {
                let expr = self.check_expr(expr, None);
                let var = Variable {
                    name,
                    ty: expr.ty,
                    mutable,
                    unused: true,
                    param: false,
                    has_hint: false,
                    kind: VariableKind::Capture,
                    ..Default::default()
                };
                self.insert::<VariableId>(var, false, true);
                instance.insert(name.data, expr);
                members.insert(name.data, CheckedMember::new(false, expr.ty, Span::nowhere()));
                continue;
            }

            let name = capture.ident();
            let path = Path::from(name);
            let ResolvedValue::Var(o_var_id) = self.resolve_value_path(&path, None) else {
                self.proj.diag.report(Error::no_symbol(self.proj.str(name.data), name.span));
                continue;
            };

            if !self.proj.scopes.get(o_var_id).kind.is_local() {
                self.proj.diag.report(Error::new(
                    format!("invalid capture of non-local variable '{}'", self.proj.str(name.data)),
                    name.span,
                ));
                continue;
            }

            self.check_local(o_var_id, name.span, true);
            self.proj.scopes.get_mut(o_var_id).unused = false;

            let var = self.proj.scopes.get(o_var_id);
            let (ty, mutable) = match capture {
                Capture::ByVal(_) => (var.ty, false),
                Capture::ByValMut(_) => (var.ty, true),
                Capture::ByPtr(_) => (self.proj.types.insert(Type::Ptr(var.ty)), false),
                Capture::ByMutPtr(_) => {
                    if !var.mutable {
                        self.proj.diag.report(Error::no_mut_ptr(name.span));
                    }

                    (self.proj.types.insert(Type::MutPtr(var.ty)), false)
                }
                _ => unreachable!(),
            };

            if matches!(capture, Capture::ByVal(_) | Capture::ByValMut(_)) {
                instance.insert(name.data, self.arena.typed(ty, CExprData::Var(o_var_id)));
            } else {
                let inner = self.arena.typed(var.ty, CExprData::Var(o_var_id));
                // We use AddrMut, but all UnaryOp::Addr* get compiled the same way
                instance.insert(
                    name.data,
                    self.arena.typed(ty, CExprData::Unary(UnaryOp::AddrMut, inner)),
                );
            }

            let var = Variable {
                name,
                ty,
                mutable,
                unused: true,
                param: false,
                has_hint: false,
                kind: VariableKind::Capture,
                ..Default::default()
            };
            self.insert::<VariableId>(var, false, true);
            members.insert(name.data, CheckedMember::new(false, ty, Span::nowhere()));
        }

        let mut names = vec![];
        let mut ty_args = vec![];
        let mut checked_params = vec![];
        for (i, (patt, hint)) in params.iter().enumerate() {
            let ty = if let Some(hint) = hint {
                self.resolve_typehint(*hint)
            } else if let Some(ty) = t.and_then(|target| self.infer_closure_type(target, Some(i))) {
                ty
            } else {
                self.error(Error::new("cannot infer type for this parameter", patt.span))
            };

            let name = self.proj.strings.get_or_intern(format!("{i}"));
            checked_params.push(CheckedParam {
                keyword: false,
                label: name,
                ty,
                patt: ParamPattern::Checked(self.check_pattern(PatternParams {
                    scrutinee: ty,
                    mutable: false,
                    pattern: patt,
                    typ: PatternType::Fn,
                    has_hint: hint.is_some(),
                })),
                default: None,
            });

            names.push(name);
            ty_args.push(ty);
        }

        let span = body.span;
        let (_, body) =
            self.with_safety(Safety::Safe, |this| this.check_expr_no_never_propagation(body, ret));
        let ret = self.proj.scopes[self.current].kind.as_lambda().unwrap().0.unwrap_or(body.ty);
        let body = self.type_check_checked(body, ret, span);

        let args_tuple = self.proj.scopes.get_tuple(names, ty_args, &self.proj.types);
        let fn_tr_inst = GenericTrait::from_type_args(&self.proj.scopes, fn_tr, [args_tuple, ret]);

        let (closure_id, closure_ty) = self.enter_id(closure_ut_scope, |this| {
            let no_captures = members.is_empty();
            let closure_id = this.insert::<UserTypeId>(
                UserType {
                    attrs: Default::default(),
                    public: false,
                    name: Located::nowhere(Strings::CLOSURE_NAME),
                    body_scope: this.current,
                    kind: UserTypeKind::Closure,
                    impls: TraitImpls::Checked(vec![Some(fn_tr_inst)]),
                    impl_blocks: vec![ImplBlockData {
                        type_params: vec![],
                        assoc_types: Default::default(),
                    }],
                    type_params: vec![],
                    fns: vec![],
                    subscripts: vec![],
                    members,
                    members_resolved: true,
                    recursive: false,
                    interior_mutable: true,
                    full_span: Span::nowhere(),
                },
                false,
                false,
            );

            let closure_ty = this
                .proj
                .types
                .insert(Type::User(GenericUserType::new(closure_id, TypeArgs::default())));
            let this_ptr_mut_ty = this.proj.types.insert(Type::MutPtr(closure_ty));
            let do_invoke_id = {
                if !no_captures {
                    checked_params.insert(
                        0,
                        CheckedParam {
                            keyword: false,
                            label: Strings::THIS_PARAM,
                            patt: ParamPattern::Checked(CPattern {
                                irrefutable: true,
                                data: PatternData::Variable(this.insert::<VariableId>(
                                    Variable {
                                        name: Located::nowhere(Strings::THIS_PARAM),
                                        ty: this_ptr_mut_ty,
                                        ..Default::default()
                                    },
                                    false,
                                    false,
                                )),
                            }),
                            ty: this_ptr_mut_ty,
                            default: None,
                        },
                    );
                }
                let do_invoke_scope = this.enter(ScopeKind::None, |this| this.current);
                let func = Function {
                    name: Located::nowhere(Strings::FN_CLOSURE_DO_INVOKE),
                    has_body: true,
                    params: checked_params,
                    ret,
                    body: Some(body),
                    body_scope: do_invoke_scope,
                    ..Default::default()
                };
                let do_invoke_id = this.insert::<FunctionId>(func, false, false);
                this.proj.scopes[do_invoke_scope].kind = ScopeKind::Function(do_invoke_id);
                do_invoke_id
            };

            let fns = this.enter(ScopeKind::Impl(0), |this| {
                let (params, invoke_body, invoke_scope) = this.enter(ScopeKind::None, |this| {
                    let args_var = this.insert::<VariableId>(
                        Variable {
                            name: Located::nowhere(Strings::FN_TR_ARGS_NAME),
                            ty: args_tuple,
                            ..Default::default()
                        },
                        false,
                        false,
                    );

                    let this_ptr_ty = this.proj.types.insert(Type::Ptr(closure_ty));
                    let this_ptr_var = this.insert::<VariableId>(
                        Variable {
                            name: Located::nowhere(Strings::THIS_PARAM),
                            ty: this_ptr_ty,
                            ..Default::default()
                        },
                        false,
                        false,
                    );

                    let mut call_args = IndexMap::new();
                    if !no_captures {
                        let this_ptr_expr =
                            this.arena.typed(this_ptr_ty, CExprData::Var(this_ptr_var));
                        call_args.insert(
                            Strings::THIS_PARAM,
                            this.arena.typed(this_ptr_mut_ty, CExprData::As(this_ptr_expr, false)),
                        );
                    }

                    let source = this.arena.typed(args_tuple, CExprData::Var(args_var));
                    let tpl_ut = this.proj.types.get(args_tuple).as_user().unwrap();
                    for (&member, m) in this.proj.scopes.get(tpl_ut.id).members.iter() {
                        let ty = m.ty.with_templates(&this.proj.types, &tpl_ut.ty_args);
                        call_args.insert(
                            member,
                            this.arena.typed(ty, CExprData::Member { source, member }),
                        );
                    }

                    let body = CExpr::call(
                        ret,
                        &this.proj.types,
                        GenericFn::new(do_invoke_id, TypeArgs::default()),
                        call_args,
                        this.current,
                        Span::nowhere(),
                        &mut this.arena,
                    );

                    let params = vec![
                        CheckedParam {
                            keyword: false,
                            label: Strings::THIS_PARAM,
                            patt: ParamPattern::Checked(CPattern {
                                irrefutable: true,
                                data: PatternData::Variable(this_ptr_var),
                            }),
                            ty: this_ptr_ty,
                            default: None,
                        },
                        CheckedParam {
                            keyword: false,
                            label: Strings::FN_TR_ARGS_NAME,
                            patt: ParamPattern::Checked(CPattern {
                                irrefutable: true,
                                data: PatternData::Variable(args_var),
                            }),
                            ty: args_tuple,
                            default: None,
                        },
                    ];

                    (params, body, this.current)
                });

                let func = Function {
                    public: true,
                    name: Located::nowhere(this.proj.strings.get_or_intern("invoke")),
                    has_body: true,
                    params,
                    ret,
                    body: Some(invoke_body),
                    body_scope: invoke_scope,
                    ..Default::default()
                };
                let invoke_id = this.insert::<FunctionId>(func, false, false);
                this.proj.scopes[invoke_scope].kind = ScopeKind::Function(invoke_id);

                [Vis::new(do_invoke_id, false), Vis::new(invoke_id, true)]
            });

            this.proj.scopes.get_mut(closure_id).fns.extend(fns);
            (closure_id, closure_ty)
        });

        self.proj.scopes[closure_ut_scope].kind = ScopeKind::UserType(closure_id);
        self.arena.typed(closure_ty, CExprData::Instance(instance))
    }

    /// Do not call this function directly
    fn check_expr_inner(&mut self, expr: PExpr, target: Option<TypeId>) -> CExpr {
        let span = expr.span;
        match &self.parsed.cget(expr.data) {
            &PExprData::Binary { op, left, right } => {
                let left_span = left.span;
                let assignment = op.is_assignment();
                match op {
                    BinaryOp::Assign => {
                        if let PExprData::Path(path) = self.parsed.cget(left.data)
                            && let Some(ident) = path.as_identifier()
                            && ident.data == Strings::UNDERSCORE
                        {
                            let right = self.check_expr(right, None);
                            return self.arena.typed(TypeId::VOID, CExprData::Discard(right));
                        } else if let PExprData::Subscript { callee, args } =
                            self.parsed.cget(left.data)
                        {
                            let span = left.span;
                            return self.check_subscript(callee, &args, target, Some(right), span);
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
                        let lhs = self.check_expr(left, target);
                        let Some(target) = lhs.ty.as_option_inner(&self.proj) else {
                            if lhs.ty != TypeId::UNKNOWN {
                                self.proj.diag.report(Error::invalid_operator(
                                    op,
                                    self.proj.fmt_ty(lhs.ty),
                                    lhs_span,
                                ));
                            }
                            return Default::default();
                        };
                        if assignment && !lhs.is_assignable(&self.proj, &self.arena) {
                            // TODO: report a better error here
                            self.error(Error::not_assignable(lhs_span))
                        }

                        let span = right.span;
                        let rhs = self.check_expr_no_never_propagation(right, Some(target));
                        let rhs = self.type_check_checked(rhs, target, span);
                        let ty = if assignment { TypeId::VOID } else { target };
                        return self.arena.typed(ty, CExprData::Binary(op, lhs, rhs));
                    }
                    BinaryOp::LogicalAnd => {
                        let (left, lvars) = self.check_condition(left);
                        let (right, rvars) = self.enter(ScopeKind::None, |this| {
                            this.define(&lvars);
                            this.check_condition(right)
                        });
                        if let Some(listen) = &mut self.listening_vars {
                            listen.extend(lvars);
                            listen.extend(rvars);
                        }

                        return self.arena.typed(TypeId::BOOL, CExprData::Binary(op, left, right));
                    }
                    _ => {}
                }

                let left = self.check_expr(left, target);
                if left.ty == TypeId::UNKNOWN {
                    self.check_expr(right, target);
                    return Default::default();
                }

                if assignment && !left.is_assignable(&self.proj, &self.arena) {
                    // TODO: report a better error here
                    self.error(Error::not_assignable(left_span))
                }

                if op != BinaryOp::Assign && !left.ty.supports_binary(&self.proj.types, op) {
                    return self.check_binary(left_span, left, right, op, span);
                }

                match (&self.proj.types[left.ty], op) {
                    (
                        Type::Int(_) | Type::Uint(_) | Type::Isize | Type::Usize,
                        BinaryOp::Shl | BinaryOp::Shr | BinaryOp::ShlAssign | BinaryOp::ShrAssign,
                    ) => {
                        let right = self.check_expr(right, Some(TypeId::U32));
                        let right = self.try_coerce(right, TypeId::U32);
                        if self.proj.types[right.ty].as_integral(false).is_none_or(|v| v.signed)
                            && right.ty != TypeId::UNKNOWN
                        {
                            self.proj.diag.report(Error::type_mismatch(
                                "{unsigned}",
                                self.proj.fmt_ty(right.ty),
                                span,
                            ));
                        }
                        self.arena.typed(
                            if assignment { TypeId::VOID } else { left.ty },
                            CExprData::Binary(op, left, right),
                        )
                    }
                    _ => {
                        let right = self.type_check(right, left.ty);
                        let ty = match op {
                            BinaryOp::NoneCoalesce => unreachable!(),
                            BinaryOp::Cmp => {
                                self.make_lang_type_by_name(LangType::Ordering, [], span)
                            }
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
                        };
                        self.arena.typed(ty, CExprData::Binary(op, left, right))
                    }
                }
            }
            &PExprData::Unary { op, expr } => {
                let (out_ty, expr) = match op {
                    UnaryOp::Deref => {
                        let expr = if let Some(target) = target {
                            let ty = self.proj.types.insert(Type::Ptr(target));
                            self.check_expr(expr, Some(ty))
                        } else {
                            self.check_expr(expr, target)
                        };

                        let ty = match self.proj.types[expr.ty] {
                            Type::Ptr(inner) | Type::MutPtr(inner) => inner,
                            Type::RawPtr(inner) | Type::RawMutPtr(inner) => {
                                check_unsafe!(self, Error::is_unsafe(span));
                                inner
                            }
                            Type::Unknown => return Default::default(),
                            _ => {
                                bail!(
                                    self,
                                    Error::invalid_operator(op, self.proj.fmt_ty(expr.ty), span)
                                )
                            }
                        };

                        return self.arena.typed(ty, CExprData::Deref(expr, 1));
                    }
                    UnaryOp::Addr | UnaryOp::AddrMut | UnaryOp::AddrRaw | UnaryOp::AddrRawMut => {
                        return self.check_addr(op, expr, span, target);
                    }
                    UnaryOp::Try => self.check_try(expr, span, target),
                    UnaryOp::Option => {
                        let target = target.and_then(|t| t.as_option_inner(&self.proj));
                        let mut expr = self.check_expr(expr, target);
                        if let Some(target) = target {
                            expr = self.try_coerce(expr, target);
                        }

                        let ty = self.make_lang_type_by_name(LangType::Option, [expr.ty], span);
                        (ty, self.try_coerce(expr, ty))
                    }
                    _ => {
                        let span = expr.span;
                        let expr = self.check_expr(expr, target);
                        if !expr.ty.supports_unary(&self.proj.scopes, &self.proj.types, op) {
                            return self.check_unary(expr, op, span);
                        }

                        if matches!(
                            op,
                            UnaryOp::PostIncrement
                                | UnaryOp::PostDecrement
                                | UnaryOp::PreIncrement
                                | UnaryOp::PreDecrement
                        ) && !expr.is_assignable(&self.proj, &self.arena)
                        {
                            self.error(Error::not_assignable(span))
                        }

                        (expr.ty, expr)
                    }
                };

                self.arena.typed(out_ty, CExprData::Unary(op, expr))
            }
            &PExprData::Call { callee, ref args } => self.check_call(target, callee, args, span),
            PExprData::Array(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.iter().copied();
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
                self.arena.typed(
                    self.proj.types.insert(Type::Array(ty, checked.len())),
                    CExprData::Array(checked),
                )
            }
            &PExprData::ArrayWithInit { init, count } => {
                let init = if let Some(&Type::Array(ty, _)) = target.map(|t| &self.proj.types[t]) {
                    self.type_check(init, ty)
                } else {
                    self.check_expr(init, target)
                };
                if let Some(res) = self.consteval_check(count, TypeId::USIZE) {
                    let count = res.val.try_into().unwrap();
                    let ty = self.proj.types.insert(Type::Array(init.ty, count));
                    self.arena.typed(ty, CExprData::ArrayWithInit { init, count })
                } else {
                    Default::default()
                }
            }
            &PExprData::VecWithInit { init, count } => {
                let Some(vec) = self.get_lang_type_or_err(LangType::Vec, span) else {
                    return Default::default();
                };

                let (init, ty) = if let Some(ty) = target
                    .and_then(|target| self.proj.types[target].as_user())
                    .filter(|ut| ut.id == vec)
                    .and_then(|ut| ut.first_type_arg())
                {
                    (self.type_check(init, ty), ty)
                } else {
                    let expr = self.check_expr(init, None);
                    let ty = expr.ty;
                    (expr, ty)
                };

                let count = self.type_check(count, TypeId::USIZE);
                CExpr::new(
                    self.make_lang_type(vec, [ty], span),
                    self.arena.alloc(CExprData::VecWithInit { init, count }),
                )
            }
            PExprData::Tuple(elements) => {
                let mut names = Vec::with_capacity(elements.len());
                let mut types = Vec::with_capacity(elements.len());
                let mut result_elems = IndexMap::with_capacity(elements.len());
                for (label, expr) in elements.iter().copied() {
                    let result = if let Some(target) = target
                        .and_then(|t| self.proj.types[t].as_user())
                        .filter(|t| self.proj.scopes.get(t.id).kind.is_tuple())
                        .and_then(|ut| {
                            let member = self.proj.scopes.get(ut.id).members.get(&label.data)?;
                            Some(member.ty.with_templates(&self.proj.types, &ut.ty_args))
                        }) {
                        self.type_check(expr, target)
                    } else {
                        self.check_expr(expr, None)
                    };

                    if result_elems.insert(label.data, result).is_some() {
                        self.proj.diag.report(Error::redefinition_k(
                            "field",
                            strdata!(self, label.data),
                            label.span,
                        ));
                    } else {
                        names.push(label.data);
                        types.push(result.ty);
                    }
                }

                self.arena.typed(
                    self.proj.scopes.get_tuple(names, types, &self.proj.types),
                    CExprData::Instance(result_elems),
                )
            }
            PExprData::Vec(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.iter().copied();
                let Some(vec) = self.get_lang_type_or_err(LangType::Vec, span) else {
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
                CExpr::new(
                    self.make_lang_type(vec, [ty], span),
                    self.arena.alloc(CExprData::Vec(checked)),
                )
            }
            PExprData::Set(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.iter().copied();
                let Some(set) = self.get_lang_type_or_err(LangType::Set, span) else {
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
                    self.arena.alloc(CExprData::Set(checked, self.current)),
                )
            }
            PExprData::Map(elements) => {
                let Some(map) = self.get_lang_type_or_err(LangType::Map, expr.span) else {
                    return Default::default();
                };

                let mut result = Vec::with_capacity(elements.len());
                let mut elements = elements.iter().copied();
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
                    self.arena.alloc(CExprData::Map(result, self.current)),
                )
            }
            &PExprData::Range { start, end, inclusive: incl } => {
                let start_id = self.proj.strings.get_or_intern_static("start");
                let end_id = self.proj.strings.get_or_intern_static("end");
                let name = match (start.is_some(), end.is_some()) {
                    (false, false) => LangType::RangeFull,
                    (true, false) => LangType::RangeFrom,
                    (false, true) => [LangType::RangeTo, LangType::RangeToInclusive][incl as usize],
                    (true, true) => [LangType::Range, LangType::RangeInclusive][incl as usize],
                };
                let Some(ut_id) = self.get_lang_type_or_err(name, span) else {
                    return Default::default();
                };

                let (&constructor, _) = self.proj.scopes.get(ut_id).kind.as_struct().unwrap();
                // Put them nowhere or else the LSP will use argument label semantic tokens
                let call_args = [
                    start.map(|v| (Some(Located::nowhere(start_id)), v)),
                    end.map(|v| (Some(Located::nowhere(end_id)), v)),
                ]
                .into_iter()
                .flatten()
                .collect();

                let mut func = GenericFn::from_id_unknown(&self.proj.scopes, constructor);
                func.ty_args.copy_args(&TypeArgs::in_order(
                    &self.proj.scopes,
                    ut_id,
                    std::iter::repeat(TypeId::UNKNOWN),
                ));

                let (_, args, ret, _failed) =
                    self.check_fn_args(func, None, &call_args, target, span);
                CExpr::new(ret, self.arena.alloc(CExprData::Instance(args)))
            }
            &PExprData::String(s) => CExpr::new(
                self.make_lang_type_by_name(LangType::String, [], span),
                self.arena.alloc(CExprData::String(s)),
            ),
            PExprData::StringInterpolation { strings, args } => {
                let mut out = Vec::with_capacity(args.len());
                for (expr, opts) in args.iter().copied() {
                    let span = expr.span;
                    let expr = self.check_expr(expr, None);
                    if expr.ty.strip_references(&self.proj.types) == TypeId::UNKNOWN {
                        continue;
                    }

                    let mut target_tr = LangType::Format;
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
                                "p" | "P" => {
                                    target_tr = LangType::Pointer;
                                    "ptr"
                                }
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
                            target_tr = LangType::Debug;
                            target_fn = "dbg";
                        }
                        None => {}
                    }

                    let Some(target_tr) = self.get_lang_type_or_err(target_tr, span) else {
                        continue;
                    };

                    let fmt = self.proj.strings.get_or_intern_static(target_fn);
                    let Some(mfn) = self.lookup_trait_fn(
                        expr.ty,
                        &GenericTrait::from_type_args(&self.proj.scopes, target_tr, []),
                        fmt,
                        self.current,
                        |proj, id| TypeArgs::in_order(&proj.scopes, id, []),
                    ) else {
                        self.proj.diag.report(Error::doesnt_implement(
                            self.proj.fmt_ty(expr.ty),
                            strdata!(self, target_tr.name(&self.proj.scopes).data),
                            span,
                        ));
                        continue;
                    };

                    let mut res = CFormatSpec {
                        width: CExpr::new(TypeId::U16, ExprArena::ZERO),
                        prec: CExpr::new(TypeId::U16, ExprArena::ZERO),
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

                    out.push((expr, res));
                }

                CExpr::new(
                    self.make_lang_type_by_name(LangType::FmtArgs, [], span),
                    self.arena.alloc(CExprData::StringInterp {
                        strings: strings.clone(),
                        args: out,
                        scope: self.current,
                    }),
                )
            }
            &PExprData::ByteString(s) => {
                let data = self.proj.strings.resolve_byte_str(s);
                let arr = self.proj.types.insert(Type::Array(TypeId::U8, data.len()));
                self.arena.typed(self.proj.types.insert(Type::Ptr(arr)), CExprData::ByteString(s))
            }
            &PExprData::Char(s) => CExpr::from_char(s, &mut self.arena),
            &PExprData::ByteChar(c) => CExpr::from_int(TypeId::U8, c.into(), &mut self.arena),
            PExprData::Void => CExpr::VOID,
            &PExprData::Bool(v) => CExpr::from_bool(v, &mut self.arena),
            PExprData::Integer(integer) => {
                let (ty, value) = self.get_int_type_and_val(target, integer.clone(), span);
                CExpr::from_int(ty, value, &mut self.arena)
            }
            &PExprData::Float(float) => {
                let typ = if let Some(suffix) = float.suffix {
                    match strdata!(self, suffix) {
                        "f32" => TypeId::F32,
                        "f64" => TypeId::F64,
                        data => {
                            return self.error(Error::new(
                                format!("invalid float literal type: '{data}'"),
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
                            self.proj.fmt_ty(typ)
                        ),
                        span,
                    ));
                }

                // TODO: warn for lossy conversion from literal
                self.arena.typed(typ, CExprData::Float(float.value))
            }
            PExprData::Path(path) => match self.resolve_value_path(path, target) {
                ResolvedValue::Var(id) => {
                    if self.proj.scopes.get(id).kind.is_local() {
                        self.check_local(id, span, false);
                    } else if let Some((cur, deps)) = &mut self.current_static {
                        deps.insert(id);
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

                    let var = self.proj.scopes.get(id);
                    if var.kind.is_static() && var.is_extern {
                        check_unsafe!(
                            self,
                            Error::new("accessing static extern variable is unsafe", span)
                        );
                    } else if var.kind.is_static() && var.mutable {
                        check_unsafe!(
                            self,
                            Error::new("accessing static mutable variable is unsafe", span)
                        );
                    }

                    let ty = var.ty;
                    self.proj.scopes.get_mut(id).unused = false;
                    self.arena.typed(ty, CExprData::Var(id))
                }
                ResolvedValue::Fn(mut func) => {
                    let unknowns: HashSet<_> = func
                        .ty_args
                        .iter()
                        .filter_map(|(&id, &ty)| (ty == TypeId::UNKNOWN).then_some(id))
                        .collect();
                    if let Some(target) = target {
                        let src = self.proj.scopes.get(func.id).ret;
                        self.infer_type_args(&mut func.ty_args, src, target);
                    }
                    self.check_bounds_filtered(&func, &unknowns, path.final_component_span());

                    if let Some(id) = self.proj.scopes.get(func.id).constructor
                        && self
                            .proj
                            .scopes
                            .get(id)
                            .is_empty_variant(self.proj.scopes.get(func.id).name.data)
                    {
                        return self.arena.typed(
                            self.proj
                                .types
                                .insert(Type::User(GenericUserType::new(id, func.ty_args))),
                            CExprData::VariantInstance(
                                self.proj.scopes.get(func.id).name.data,
                                Default::default(),
                            ),
                        );
                    }

                    self.arena.typed(
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
                        let src = self.proj.scopes.get(mfn.func.id).ret;
                        self.infer_type_args(&mut mfn.func.ty_args, src, target);
                    }
                    self.check_bounds_filtered(&mfn.func, &unknowns, path.final_component_span());

                    if let Some(id) = self.proj.scopes.get(mfn.func.id).constructor
                        && self
                            .proj
                            .scopes
                            .get(id)
                            .is_empty_variant(self.proj.scopes.get(mfn.func.id).name.data)
                    {
                        return self.arena.typed(
                            self.proj
                                .types
                                .insert(Type::User(GenericUserType::new(id, mfn.func.ty_args))),
                            CExprData::VariantInstance(
                                self.proj.scopes.get(mfn.func.id).name.data,
                                Default::default(),
                            ),
                        );
                    }

                    self.arena.typed(
                        self.proj.types.insert(Type::Fn(mfn.func.clone())),
                        CExprData::MemFn(mfn, self.current),
                    )
                }
                ResolvedValue::UnionConstructor(ut) => bail!(
                    self,
                    Error::expected_found(
                        "expression",
                        format_args!("type '{}'", self.proj.fmt_ut(&ut)),
                        span,
                    )
                ),
                ResolvedValue::NotFound(err) => {
                    named_error!(self, Error::no_symbol, err.data, err.span)
                }
                ResolvedValue::Error => Default::default(),
            },
            &PExprData::Block(ref body, label) => {
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
                self.arena.typed(target.unwrap_or(TypeId::VOID), CExprData::Block(block))
            }
            &PExprData::If { cond, if_branch, else_branch } => {
                let (cond, vars) = self.check_condition(cond);
                let target = if else_branch.is_none() {
                    target.and_then(|t| t.as_option_inner(&self.proj))
                } else {
                    target
                };

                let if_span = if_branch.span;
                let mut if_branch = self.enter(ScopeKind::None, |this| {
                    this.define(&vars);
                    this.check_expr_no_never_propagation(if_branch, target)
                });
                if let Some(target) = target {
                    if_branch = self.type_check_checked(if_branch, target, if_span);
                }

                let mut out_type = if_branch.ty;
                let else_branch = if let Some(expr) = else_branch {
                    if out_type == TypeId::NEVER {
                        let expr = self.check_expr_no_never_propagation(expr, None);
                        out_type = expr.ty;
                        if_branch = self.try_coerce(if_branch, expr.ty);
                        expr
                    } else {
                        let span = expr.span;
                        let source =
                            self.check_expr_no_never_propagation(expr, target.or(Some(out_type)));
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
                    if self
                        .arena
                        .get(if_branch.data)
                        .is_yielding_block(&self.proj.scopes)
                        .unwrap_or(true)
                        && !matches!(out_type, TypeId::NEVER | TypeId::VOID | TypeId::UNKNOWN)
                    {
                        out_type = self.make_lang_type_by_name(LangType::Option, [out_type], span);
                        if_branch = self.try_coerce(if_branch, out_type);
                        CExpr::option_null(out_type, &mut self.arena)
                    } else {
                        out_type = TypeId::VOID;
                        CExpr::VOID
                    }
                };

                self.arena.typed(
                    out_type,
                    CExprData::If {
                        cond,
                        if_branch,
                        else_branch,
                        dummy_scope: self.proj.scopes.create_scope(
                            ScopeId::ROOT,
                            ScopeKind::None,
                            false,
                        ),
                    },
                )
            }
            &PExprData::Loop { cond, ref body, do_while, label } => {
                let infinite = cond.is_none();
                let target = self.loop_target(target, infinite);
                let kind = LoopScopeKind { target, breaks: LoopBreak::None, infinite, label };
                let (cond, body) = if let Some(cond) = cond {
                    let (cond, vars) = self.check_condition(cond);
                    let body = self.create_block_with_init(body, ScopeKind::Loop(kind), |this| {
                        if !do_while {
                            this.define(&vars);
                        }
                    });
                    (Some(cond), body)
                } else {
                    (None, self.create_block(body, ScopeKind::Loop(kind)))
                };
                let (out_type, optional) = self.loop_out_type(
                    self.proj.scopes[body.scope].kind.as_loop().copied().unwrap(),
                    span,
                );
                self.arena.typed(out_type, CExprData::Loop { cond, body, do_while, optional })
            }
            &PExprData::For { ref patt, iter, ref body, label } => {
                self.check_for_expr(target, patt, iter, body, label)
            }
            &PExprData::Member { source, member: name, ref generics } => {
                if !generics.is_empty() {
                    self.error(Error::new("member variables cannot have type arguments", span))
                }

                let source = self.check_expr(source, None);
                let id = source.ty.strip_references(&self.proj.types);
                self.check_dot_completions(span, id, true);
                let ut_id = match &self.proj.types[id] {
                    Type::User(data) => data.id,
                    Type::Unknown => return Default::default(),
                    _ => {
                        bail!(
                            self,
                            Error::no_member(
                                self.proj.fmt_ty(id),
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
                            self.proj.fmt_ty(source.ty),
                            strdata!(self, name.data),
                            name.span
                        )
                    );
                };

                if ut.kind.is_unsafe_union() {
                    check_unsafe!(self, Error::is_unsafe(name.span));
                }

                let ty = member.ty.with_ut_templates(&self.proj.types, id);
                if !member.public && !self.can_access_privates(ut.scope) {
                    self.proj.diag.report(Error::private_member(
                        self.proj.fmt_ty(id),
                        strdata!(self, name.data),
                        name.span,
                    ));
                }

                let source = source.auto_deref(&self.proj.types, id, &mut self.arena);
                self.arena.typed(ty, CExprData::Member { source, member: name.data })
            }
            &PExprData::Subscript { callee, ref args } => {
                self.check_subscript(callee, args, target, None, span)
            }
            &PExprData::Return(expr) => self.check_return(expr, span),
            &PExprData::Tail(expr) => match &self.proj.scopes[self.current].kind {
                ScopeKind::Function(_) | ScopeKind::Lambda(_, _) => self.check_return(expr, span),
                ScopeKind::Loop { .. } => self.type_check(expr, TypeId::VOID),
                ScopeKind::Block(data) => self.check_yield(Some(expr), *data, self.current, span),
                _ => self.error(Error::new("yield outside of block", expr.span)),
            },
            &PExprData::Break(expr, label) => {
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
                        self.check_expr(expr, None);
                    }
                    return report_error!(
                        self,
                        label.span,
                        "undefined label '{}'",
                        strdata!(self, label.data)
                    );
                }

                let Some((loop_data, id)) = self.current_loop(None) else {
                    if let Some(expr) = expr {
                        self.check_expr(expr, None);
                    }
                    return self.error(Error::new("break outside of loop", span));
                };

                self.check_break(expr, *loop_data, id, span)
            }
            PExprData::Continue(label) => {
                let Some((_, id)) = self.current_loop(label.map(|l| l.data)) else {
                    if let Some(label) = label {
                        let name = strdata!(self, label.data);
                        return self
                            .error(Error::new(format!("undefined label '{name}'"), label.span));
                    } else {
                        return self.error(Error::new("continue outside of loop", span));
                    }
                };

                self.arena.typed(TypeId::NEVER, CExprData::Continue(id))
            }
            &PExprData::Is { expr, ref pattern } => self.enter(ScopeKind::None, |this| {
                let expr = this.check_expr(expr, None);
                let patt = this.check_pattern(PatternParams {
                    scrutinee: expr.ty,
                    mutable: false,
                    pattern,
                    typ: PatternType::Regular,
                    has_hint: false,
                });
                this.arena.typed(TypeId::BOOL, CExprData::Is(expr, patt))
            }),
            &PExprData::Match { expr, ref body } => {
                let scrutinee = self.check_expr(expr, None);
                let mut has_never = false;
                let mut target = target;
                let mut result: Vec<_> = body
                    .iter()
                    .map(|(patt, expr)| {
                        let span = expr.span;
                        let (patt, expr) = self.enter(ScopeKind::None, |this| {
                            (
                                this.check_full_pattern(scrutinee.ty, patt),
                                this.check_expr(*expr, target),
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
                self.arena.typed(
                    target,
                    CExprData::Match {
                        scrutinee,
                        body: result,
                        dummy_scope: self.proj.scopes.create_scope(
                            ScopeId::ROOT,
                            ScopeKind::None,
                            false,
                        ),
                    },
                )
            }
            &PExprData::As { expr, ty, throwing } => {
                let to_id = self.resolve_typehint(ty);
                let expr = self.check_expr(expr, Some(to_id));
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
                        if from_id == to_id {
                            return self.arena.typed(to_id, CExprData::As(expr, throwing));
                        }
                    }
                }

                match Cast::get(&self.proj.types[from_id], &self.proj.types[to_id]) {
                    Cast::None => self.proj.diag.report(Error::new(
                        format!(
                            "cannot cast expression of type '{}' to '{}'",
                            self.proj.fmt_ty(from_id),
                            self.proj.fmt_ty(to_id),
                        ),
                        span,
                    )),
                    Cast::Unsafe => {
                        check_unsafe!(self, Error::is_unsafe(span));
                    }
                    Cast::Fallible if !throwing => self.proj.diag.report(Error::new(
                        format!(
                            "cast of expression of type '{}' to '{}' requires fallible cast",
                            self.proj.fmt_ty(from_id),
                            self.proj.fmt_ty(to_id),
                        ),
                        span,
                    )),
                    Cast::Infallible if throwing => {
                        self.proj.diag.report(Warning::unnecessary_fallible_cast(
                            self.proj.fmt_ty(from_id),
                            self.proj.fmt_ty(to_id),
                            span,
                        ))
                    }
                    _ => {}
                }
                self.arena.typed(to_id, CExprData::As(expr, throwing))
            }
            PExprData::Error => CExpr::default(),
            &PExprData::Lambda { policy, ref captures, ref params, ret, body } => {
                let ret = ret
                    .map(|ret| self.resolve_typehint(ret))
                    .or_else(|| target.and_then(|t| self.infer_closure_type(t, None)));
                self.enter(ScopeKind::Lambda(ret, policy.unwrap_or_default()), |this| {
                    this.check_lambda(span, target, captures, params, ret, body)
                })
            }
            &PExprData::Unsafe(expr) => {
                let mut span = span;
                span.len = "unsafe".len() as u32;
                let was_unsafe = matches!(self.safety, Safety::Unsafe(_));
                if was_unsafe {
                    self.proj.diag.report(Warning::redundant_unsafe(span));
                }

                let (safety, expr) = self.with_safety(Safety::Unsafe(false), |this| {
                    this.check_expr_ex(expr, target, Listen::Passthrough, true).0
                });
                if !was_unsafe && matches!(safety, Safety::Unsafe(false)) {
                    self.proj.diag.report(Warning::useless_unsafe(span));
                }

                expr
            }
            &PExprData::Grouping(expr) => {
                self.check_expr_ex(expr, target, Listen::Passthrough, true).0
            }
        }
    }

    fn with_safety<T>(&mut self, safety: Safety, f: impl FnOnce(&mut Self) -> T) -> (Safety, T) {
        let old_safety = std::mem::replace(&mut self.safety, safety);
        let res = f(self);
        let old_safety = std::mem::replace(&mut self.safety, old_safety);
        (old_safety, res)
    }

    fn check_expr(&mut self, expr: PExpr, target: Option<TypeId>) -> CExpr {
        self.check_expr_ex(expr, target, Listen::No, true).0
    }

    fn check_expr_ex(
        &mut self,
        expr: PExpr,
        target: Option<TypeId>,
        listen: Listen,
        propagate_never: bool,
    ) -> (CExpr, Option<Vec<VariableId>>) {
        let (expr, res) = if listen != Listen::Passthrough {
            let listen = listen == Listen::Yes;
            let prev = std::mem::replace(&mut self.listening_vars, listen.then(Vec::new));
            let expr = self.check_expr_inner(expr, target);
            let res = std::mem::replace(&mut self.listening_vars, prev);
            (expr, res)
        } else {
            (self.check_expr_inner(expr, target), None)
        };

        if propagate_never
            && expr.ty == TypeId::NEVER
            && !matches!(self.arena.get(expr.data), &CExprData::Yield(_, scope) if scope == self.current)
            && let ScopeKind::Block(BlockScopeKind { branches, .. }) =
                &mut self.proj.scopes[self.current].kind
        {
            *branches = true;
        }
        (expr, res)
    }

    fn check_expr_no_never_propagation(&mut self, expr: PExpr, target: Option<TypeId>) -> CExpr {
        self.check_expr_ex(expr, target, Listen::No, false).0
    }

    fn check_condition(&mut self, expr: PExpr) -> (CExpr, Vec<VariableId>) {
        let (res, vars) = self.check_expr_ex(expr, Some(TypeId::BOOL), Listen::Yes, true);
        (self.type_check_checked(res, TypeId::BOOL, expr.span), vars.unwrap_or_default())
    }

    fn listen_for_vars<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> (T, Vec<VariableId>) {
        let prev = self.listening_vars.replace(Vec::new());
        let expr = f(self);
        let res = std::mem::replace(&mut self.listening_vars, prev);
        (expr, res.unwrap_or_default())
    }

    fn define(&mut self, vars: &[VariableId]) {
        for &var in vars.iter() {
            let name = self.proj.scopes.get(var).name.data;
            self.proj.scopes[self.current].vns.insert(name, Vis::new(ValueItem::Var(var), false));
        }
    }

    fn current_loop(&self, label: Option<StrId>) -> Option<(&LoopScopeKind, ScopeId)> {
        self.proj
            .scopes
            .walk(self.current)
            .take_while(|(_, scope)| !scope.kind.is_defer())
            .find_map(|(id, scope)| {
                scope.kind.as_loop().filter(|l| label.is_none() || l.label == label).zip(Some(id))
            })
    }

    fn check_subscript(
        &mut self,
        callee: Expr,
        args: &CallArgs,
        target: Option<TypeId>,
        assign: Option<Expr>,
        span: Span,
    ) -> CExpr {
        if args.is_empty() {
            return self.error(Error::new("subscript requires at least one argument", span));
        }

        let mut args = args.to_vec();
        let assign = if let Some(assign) = assign {
            args.push((None, assign));
            true
        } else {
            false
        };

        let callee = self.check_expr(callee, None);
        let ty = callee.ty.strip_references(&self.proj.types);

        let imm_receiver = self.immutable_receiver(&callee);
        // if imm_receiver && assign {
        //     args.into_iter().for_each(|(_, expr)| _ = self.check_expr(expr, None));
        //     return self.error(Error::not_assignable(span));
        // }

        let mut candidates = vec![];
        for ut in self.proj.types[ty]
            .as_user()
            .cloned()
            .into_iter()
            .chain(self.extensions_in_scope_for(ty))
        {
            for f in self.proj.scopes.get(ut.id).subscripts.clone() {
                self.resolve_proto(f);
                let data = self.proj.scopes.get(f);
                if assign != data.typ.is_assign_subscript()
                    || data.params.len() != args.len() + 1
                    || (imm_receiver
                        && data.params.first().is_some_and(|p| self.proj.types[p.ty].is_mut_ptr()))
                {
                    continue;
                }

                let mut func = GenericFn::from_id(&self.proj.scopes, f);
                func.ty_args.copy_args(&ut.ty_args);
                candidates.push(func);
            }
        }

        candidates.sort_unstable_by(|a, b| {
            let left = self
                .proj
                .scopes
                .get(a.id)
                .params
                .first()
                .is_some_and(|p| self.proj.types[p.ty].is_mut_ptr());
            let right = self
                .proj
                .scopes
                .get(b.id)
                .params
                .first()
                .is_some_and(|p| self.proj.types[p.ty].is_mut_ptr());
            right.cmp(&left)
        });

        for func in candidates {
            let args = args.clone();
            let recv = callee.auto_deref(
                &self.proj.types,
                self.proj.scopes.get(func.id).params[0].ty,
                &mut self.arena,
            );
            let err_idx = self.proj.diag.capture_errors();
            let (func, args, ret, failed) =
                self.check_fn_args(func, Some(recv), &args, target, span);
            // TODO: if the arguments have non overload related errors, just stop overload
            // resolution
            if failed || args.iter().any(|arg| arg.1.data == ExprArena::ERROR.data) {
                self.proj.diag.truncate_errors(err_idx);
                continue;
            }
            // unsafe doesnt cause check_fn_args to fail, but we mute errors, so check again
            // here
            if self.proj.scopes.get(func.id).is_unsafe {
                check_unsafe!(self, Error::is_unsafe(span));
            }

            let call =
                CExpr::call(ret, &self.proj.types, func, args, self.current, span, &mut self.arena);
            if !assign && let Type::Ptr(inner) | Type::MutPtr(inner) = &self.proj.types[ret] {
                return self.arena.typed(*inner, CExprData::Deref(call, 1));
            }
            return call;
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
                    self.proj.fmt_ty(callee.ty),
                    if assign { " assign" } else { "" },
                    args.into_iter()
                        .map(|expr| self.proj.fmt_ty(expr.ty).to_string())
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
        ) && !callee.can_addrmut(&self.proj, &self.arena)
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

    fn check_try(&mut self, expr: PExpr, span: Span, target: Option<TypeId>) -> (TypeId, CExpr) {
        let expr = self.check_expr(expr, target.and_then(|t| t.as_option_inner(&self.proj)));
        let Some(inner) = expr.ty.as_option_inner(&self.proj) else {
            if expr.ty == TypeId::UNKNOWN {
                return Default::default();
            } else {
                bail!(self, Error::invalid_operator(UnaryOp::Try, self.proj.fmt_ty(expr.ty), span));
            }
        };

        for (_, scope) in self.proj.scopes.walk(self.current) {
            match &scope.kind {
                &ScopeKind::Lambda(target, _) => {
                    if let Some(target) = target
                        && target.as_option_inner(&self.proj).is_none()
                    {
                        self.error(Error::new(
                            "operator '?' is only valid in functions that return Option",
                            span,
                        ))
                    } else if target.is_none() {
                        self.error(Error::new(
                            "closure return value must be known at this point",
                            span,
                        ))
                    }

                    return (inner, expr);
                }
                &ScopeKind::Function(id) => {
                    if self.proj.scopes.get(id).ret.as_option_inner(&self.proj).is_none() {
                        self.error(Error::new(
                            "operator '?' is only valid in functions that return Option",
                            span,
                        ))
                    }

                    return (inner, expr);
                }
                ScopeKind::Defer => {
                    self.proj
                        .diag
                        .report(Error::new("operator '?' cannot be used in a defer block", span));
                    return (inner, expr);
                }
                _ => {}
            }
        }

        self.error(Error::new("'?' expression outside of function", span))
    }

    fn check_return(&mut self, expr: PExpr, span: Span) -> CExpr {
        for (id, scope) in self.proj.scopes.walk(self.current) {
            match &scope.kind {
                &ScopeKind::Lambda(target, policy) => {
                    let span = expr.span;
                    let mut expr = self.check_expr(expr, target);
                    self.proj.scopes[id].kind = if let Some(target) = target {
                        expr = self.type_check_checked(expr, target, span);
                        ScopeKind::Lambda(Some(target), policy)
                    } else {
                        ScopeKind::Lambda(Some(expr.ty), policy)
                    };
                    return self.arena.typed(TypeId::NEVER, CExprData::Return(expr));
                }
                &ScopeKind::Function(id) => {
                    let target = self.proj.scopes.get(id).ret;
                    let expr = self.type_check(expr, target);
                    return self.arena.typed(TypeId::NEVER, CExprData::Return(expr));
                }
                ScopeKind::Defer => {
                    self.proj.diag.report(Error::new("cannot return in defer block", span));
                    return self.check_expr(expr, None);
                }
                _ => {}
            }
        }

        self.error(Error::new("return expression outside of function", expr.span))
    }

    fn check_break(
        &mut self,
        expr: Option<PExpr>,
        mut data: LoopScopeKind,
        id: ScopeId,
        span: Span,
    ) -> CExpr {
        let expr = if let Some(expr) = expr {
            let span = expr.span;
            let expr = if let Some(target) = data.target {
                let expr = self.type_check(expr, target);
                data = *self.proj.scopes[id].kind.as_loop().unwrap();
                expr
            } else {
                let expr = self.check_expr(expr, data.target);
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
            self.type_check_checked(CExpr::VOID, target, span)
        } else {
            data.target = Some(TypeId::VOID);
            data.breaks = LoopBreak::WithNothing;
            CExpr::VOID
        };

        self.proj.scopes[id].kind = ScopeKind::Loop(data);
        self.arena.typed(TypeId::NEVER, CExprData::Break(expr, id))
    }

    fn check_yield(
        &mut self,
        expr: Option<PExpr>,
        mut data: BlockScopeKind,
        id: ScopeId,
        span: Span,
    ) -> CExpr {
        let expr = if let Some(target) = data.target {
            expr.map(|expr| self.type_check(expr, target))
                .unwrap_or_else(|| self.type_check_checked(CExpr::VOID, target, span))
        } else {
            let span = expr.as_ref().map(|e| e.span).unwrap_or(span);
            let expr = expr.map(|expr| self.check_expr(expr, data.target)).unwrap_or(CExpr::VOID);
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

        self.arena.typed(TypeId::NEVER, CExprData::Yield(expr, id))
    }

    fn check_for_expr(
        &mut self,
        target: Option<TypeId>,
        patt: &Located<Pattern>,
        iter: PExpr,
        body: &[PStmt],
        label: Option<StrId>,
    ) -> CExpr {
        let iter_span = iter.span;
        let iter = self.check_expr(iter, None);
        let Some(iter_tr_id) = self.get_lang_type_or_err(LangType::Iterator, iter_span) else {
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
            let Some(mfn) = this.lookup_unk_trait_fn(iter.ty, iter_tr_id, key) else {
                this.check_block(body);
                let name = this.proj.fmt_ty(iter.ty);
                return this.error(Error::doesnt_implement(name, "Iterator", iter_span));
            };

            let next_ty = this
                .proj
                .scopes
                .get(mfn.func.id)
                .ret
                .strip_options(&this.proj)
                .with_templates(&this.proj.types, &mfn.func.ty_args);

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

            let iter_var = this.insert::<VariableId>(
                Variable {
                    name: Located::nowhere(Strings::ITER_VAR_NAME),
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
                let inner = this.arena.typed(iter.ty, CExprData::Var(iter_var));
                let arg0 = this.arena.typed(
                    this.proj.types.insert(Type::MutPtr(iter.ty)),
                    CExprData::Unary(UnaryOp::AddrMut, inner),
                );

                CExpr::member_call(
                    f.ret.with_templates(&this.proj.types, &mfn.func.ty_args),
                    &this.proj.types,
                    mfn,
                    [(p0, arg0)].into(),
                    this.current,
                    iter_span,
                    &mut this.arena,
                )
            };

            let cond = this.arena.typed(
                TypeId::BOOL,
                CExprData::Is(
                    next_fn_call,
                    CPattern::refutable(PatternData::Variant {
                        pattern: Some(
                            CPattern::irrefutable(PatternData::Destructure {
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
            let while_loop = this
                .arena
                .typed(out, CExprData::Loop { cond: Some(cond), body, do_while: false, optional });

            let inner = this.arena.typed(out, CExprData::Yield(while_loop, this.current));
            this.arena.typed(
                out,
                CExprData::Block(Block {
                    body: vec![
                        CStmt::Let(
                            CPattern { irrefutable: true, data: PatternData::Variable(iter_var) },
                            Some(iter),
                        ),
                        CStmt::Expr(inner),
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
        args: &CallArgs,
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
        for (name, expr) in args.iter().copied() {
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
                .map(|m| m.ty.with_templates(&self.proj.types, &ut.ty_args))
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

            let old = self.current_call_ty_args.replace(ut.ty_args);
            members.insert(name.data, self.check_arg(expr, ty).0);
            ut.ty_args = std::mem::replace(&mut self.current_call_ty_args, old).unwrap();
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
                self.check_bounds(&ut.ty_args, ty, id, span);
            }
        }

        self.arena.typed(self.proj.types.insert(Type::User(ut)), CExprData::Instance(members))
    }

    fn check_call(
        &mut self,
        target: Option<TypeId>,
        callee: PExpr,
        args: &CallArgs,
        span: Span,
    ) -> CExpr {
        match self.parsed.cget(callee.data) {
            PExprData::Member { source, member, generics } => {
                let recv = self.check_expr(source, None);
                let id = recv.ty.strip_references(&self.proj.types);
                if id == TypeId::UNKNOWN {
                    return Default::default();
                }

                // most of the time, the dot span will be inside a non-call member expression.
                // however, if you start editing a function call, it is possible for the span
                // to end up here
                self.check_dot_completions(member.span, id, true);
                let Some(mut mfn) = self.lookup_member_fn(id, member.data, &generics, span) else {
                    bail!(
                        self,
                        Error::no_method(self.proj.fmt_ty(id), strdata!(self, member.data), span)
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
                        self.proj.fmt_ty(id),
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
                    ) && !recv.can_addrmut(&self.proj, &self.arena)
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
                }

                let recv = recv.auto_deref(&self.proj.types, this_param_ty, &mut self.arena);
                let (func, args, ret, _) =
                    self.check_fn_args(mfn.func, Some(recv), args, target, span);
                mfn.func = func;
                if mfn.typ.is_dynamic() {
                    return self.arena.typed(ret, CExprData::CallDyn(mfn.func, args));
                } else {
                    return CExpr::member_call(
                        ret,
                        &self.proj.types,
                        mfn,
                        args,
                        self.current,
                        span,
                        &mut self.arena,
                    );
                }
            }
            PExprData::Path(path) => match self.resolve_value_path(&path, target) {
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
                                format_args!("union variant '{}'", strdata!(self, f.name.data)),
                                span,
                            ));
                        }
                    }

                    let (func, args, ret, _) =
                        self.check_fn_args(mfn.func, None, args, target, span);
                    mfn.func = func;
                    return CExpr::member_call(
                        ret,
                        &self.proj.types,
                        mfn,
                        args,
                        self.current,
                        span,
                        &mut self.arena,
                    );
                }
                ResolvedValue::NotFound(err) => {
                    return named_error!(self, Error::no_symbol, err.data, err.span);
                }
                ResolvedValue::Error => return Default::default(),
                _ => {}
            },
            _ => {}
        }

        let callee_span = callee.span;
        let callee = self.check_expr(callee, None);
        match &self.proj.types[callee.ty.strip_references(&self.proj.types)] {
            Type::Fn(func) => {
                return self.check_known_fn_call(func.clone(), args, target, callee_span);
            }
            Type::FnPtr(f) => {
                let f = f.clone();
                let mut result = vec![];
                for (i, (name, arg)) in args.iter().copied().enumerate() {
                    if let Some(&param) = f.params.get(i) {
                        if name.is_some() {
                            self.proj.diag.report(Error::new(
                                "keyword parameters are not allowed here",
                                arg.span,
                            ));
                        }

                        result.push(self.type_check(arg, param));
                    } else {
                        self.proj
                            .diag
                            .report(Error::new("too many positional arguments", callee_span));
                        break;
                    }
                }

                if result.len() < f.params.len() {
                    self.error(Error::new("too few positional arguments", callee_span))
                }

                if f.is_unsafe {
                    check_unsafe!(self, Error::is_unsafe(callee_span));
                }

                let callee = callee.auto_deref(&self.proj.types, TypeId::UNKNOWN, &mut self.arena);
                return self.arena.typed(f.ret, CExprData::CallFnPtr(callee, result));
            }
            _ => {}
        }

        let mut i = 0;
        let rhs = self.parsed.expr(
            span,
            ExprData::Tuple(
                args.iter()
                    .map(|&(label, arg)| {
                        (
                            label.unwrap_or_else(|| {
                                let label = self.proj.strings.get_or_intern(format!("{i}"));
                                i += 1;
                                Located::nowhere(label)
                            }),
                            arg,
                        )
                    })
                    .collect(),
            ),
        );
        self.check_binary(callee_span, callee, rhs, BinaryOp::Call, span)
    }

    fn check_known_fn_call(
        &mut self,
        mut func: GenericFn,
        args: &CallArgs,
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
                    format_args!("union variant '{name}'"),
                    span,
                ));
            }
        }

        let (func, args, ret, _) = self.check_fn_args(func, None, args, target, span);
        CExpr::call(ret, &self.proj.types, func, args, self.current, span, &mut self.arena)
    }

    /// `self.current_call_args` must be non-null
    fn check_arg(&mut self, expr: PExpr, ty: TypeId) -> (CExpr, bool) {
        let ty_args = self.current_call_ty_args.as_mut().unwrap();
        let target = ty.with_templates(&self.proj.types, ty_args);
        let span = expr.span;
        let expr = self.check_expr(expr, Some(target));
        self.do_type_check_checked(expr, ty, span)
    }

    fn check_fn_args(
        &mut self,
        mut func: GenericFn,
        recv: Option<CExpr>,
        args: &CallArgs,
        target: Option<TypeId>,
        span: Span,
    ) -> (GenericFn, IndexMap<StrId, CExpr>, TypeId, bool) {
        self.resolve_proto(func.id);

        let unknowns: HashSet<_> = func
            .ty_args
            .iter()
            .filter_map(|(&id, &ty)| (ty == TypeId::UNKNOWN).then_some(id))
            .collect();
        if let Some(target) = target {
            self.infer_call_type_args(
                &mut func.ty_args,
                self.proj.scopes.get(func.id).ret,
                target,
                true,
            );
        }

        let func_id = func.id;
        let prev_call = self.current_call_ty_args.replace(func.ty_args);

        let mut result = IndexMap::with_capacity(args.len());
        let mut last_pos = 0;
        if let Some(recv) = recv {
            result.insert(Strings::THIS_PARAM, recv);
            last_pos += 1;
        }

        let variadic = self.proj.scopes.get(func_id).variadic;
        let mut num = 0;
        let mut failed = false;
        for (name, expr) in args.iter().copied() {
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
                    self.proj.scopes.get(func_id).params.iter().find(|p| p.label == name.data)
                {
                    let ty = param.ty;
                    self.check_arg_label_hover(name.span, param.clone(), func_id);

                    let (expr, f) = self.check_arg(expr, ty);
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
                .get(func_id)
                .params
                .iter()
                .enumerate()
                .skip(last_pos)
                .find(|(_, param)| !param.keyword && !result.contains_key(&param.label))
            {
                let name = param.label;
                let (expr, f) = self.check_arg(expr, param.ty);
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

        let params = &self.proj.scopes.get(func_id).params;
        for param in params.iter() {
            if result.contains_key(&param.label) {
                continue;
            }

            if let Some(DefaultExpr::Checked(expr)) = &param.default {
                result.insert(param.label, expr.clone_at(self.current, span, &mut self.arena));
            }
        }

        if params.len() > result.len() {
            let mut missing = String::new();
            for param in params.iter().filter(|p| !result.contains_key(&p.label)) {
                if !missing.is_empty() {
                    missing.push_str(", ");
                }

                missing.push_str(strdata!(self, param.label));
            }

            failed = true;
            self.error(Error::new(
                format!(
                    "expected {} argument(s), found {} (missing {missing})",
                    params.len(),
                    result.len()
                ),
                span,
            ))
        }

        let ty_args = std::mem::replace(&mut self.current_call_ty_args, prev_call).unwrap();
        let func = GenericFn::new(func_id, ty_args);
        let f = self.check_bounds_filtered(&func, &unknowns, span);
        failed = failed || f;
        if self.proj.scopes.get(func.id).is_unsafe {
            check_unsafe!(self, Error::is_unsafe(span));
        }

        let ret = self.proj.scopes.get(func.id).ret.with_templates(&self.proj.types, &func.ty_args);
        (func, result, ret, failed)
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
                let f = self.check_bounds(&func.ty_args, ty, id, span);
                failed = failed || f;
            }
        }
        failed
    }

    fn check_bounds(&mut self, ty_args: &TypeArgs, ty: TypeId, id: UserTypeId, span: Span) -> bool {
        let mut failed = false;
        self.resolve_impls(id);
        for mut bound in self.proj.scopes.get(id).impls.clone().into_iter_checked() {
            bound.fill_templates(&self.proj.types, ty_args);
            if !self.implements_trait(ty, &bound) {
                failed = true;
                self.proj.diag.report(Error::doesnt_implement(
                    self.proj.fmt_ty(ty),
                    self.proj.fmt_ut(&bound),
                    span,
                ));
            }
        }
        failed
    }

    fn check_block(&mut self, body: &[PStmt]) -> Vec<CStmt> {
        let declared: Vec<_> =
            body.iter().flat_map(|stmt| self.declare_stmt(&mut vec![], stmt)).collect();

        self.enter_id_and_resolve(self.current, |this| {
            declared.into_iter().flat_map(|stmt| this.check_stmt(stmt)).collect()
        })
    }

    fn create_block(&mut self, body: &[PStmt], kind: ScopeKind) -> Block {
        self.create_block_with_init(body, kind, |_| {})
    }

    fn create_block_with_init(
        &mut self,
        body: &[PStmt],
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
        self.do_type_check_checked(source, target, span).0
    }

    fn do_type_check_checked(
        &mut self,
        expr: CExpr,
        mut target: TypeId,
        span: Span,
    ) -> (CExpr, bool) {
        if let Some(ty_args) = self.current_call_ty_args.as_mut().filter(|args| !args.is_empty()) {
            let mut ty_args = std::mem::take(ty_args);
            self.infer_call_type_args(&mut ty_args, target, expr.ty, false);
            target = target.with_templates(&self.proj.types, &ty_args);
            self.current_call_ty_args = Some(ty_args);
        }

        match self.coerce(expr, target) {
            Ok(expr) => (expr, false),
            Err(expr) => {
                self.proj.diag.report(type_mismatch_err!(self, target, expr.ty, span));
                (Default::default(), true)
            }
        }
    }

    fn resolve_lang_type(&mut self, name: LangType, args: &[TypeHint], span: Span) -> TypeId {
        if let Some(id) = self.proj.scopes.lang_types.get(&name).copied() {
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
            self.check_bounds(&ty.ty_args, param, id, span);
        }
        self.proj.types.insert(Type::User(ty))
    }

    fn make_lang_type_by_name(
        &mut self,
        name: LangType,
        args: impl IntoIterator<Item = TypeId>,
        span: Span,
    ) -> TypeId {
        let Some(id) = self.proj.scopes.lang_types.get(&name).copied() else {
            return self.error(Error::no_lang_item(name, span));
        };

        self.proj.types.insert(Type::User(GenericUserType::from_type_args(
            &self.proj.scopes,
            id,
            args,
        )))
    }

    fn get_lang_type_or_err(&mut self, name: LangType, span: Span) -> Option<UserTypeId> {
        let Some(id) = self.proj.scopes.lang_types.get(&name).copied() else {
            return self.error(Error::no_lang_item(name, span));
        };

        Some(id)
    }

    fn resolve_dyn_ptr(&mut self, path: &Path) -> Option<GenericTrait> {
        match self.resolve_type_path(path) {
            ResolvedType::UserType(ut) => {
                let data = self.proj.scopes.get(ut.id);
                if data.kind.is_trait() {
                    if path.fn_like && data.attrs.lang != Some(LangType::OpFn) {
                        self.error(Error::function_like_tr(path.span()))
                    }

                    Some(ut)
                } else {
                    bail!(
                        self,
                        Error::expected_found(
                            "trait",
                            format_args!("type '{}'", self.proj.fmt_ut(&ut)),
                            path.final_component_span(),
                        )
                    )
                }
            }
            ResolvedType::Builtin(ty) => bail!(
                self,
                Error::expected_found(
                    "trait",
                    format_args!("type '{}'", self.proj.fmt_ty(ty)),
                    path.final_component_span(),
                )
            ),
            ResolvedType::Error => None,
        }
    }

    fn resolve_typehint(&mut self, hint: TypeHint) -> TypeId {
        fn create_ptr(
            this: &mut TypeChecker,
            init: impl FnOnce(TypeId) -> Type,
            hint: TypeHint,
        ) -> TypeId {
            let ty = this.resolve_typehint(hint);
            this.proj.types.insert(init(ty))
        }

        let span = hint.span;
        match &self.parsed.hints.cget(hint.data) {
            TypeHintData::Path(path) => match self.resolve_type_path(path) {
                ResolvedType::Builtin(ty) => ty,
                ResolvedType::UserType(ut) => {
                    if !self.proj.scopes.get(ut.id).kind.is_trait() {
                        self.proj.types.insert(Type::User(ut))
                    } else {
                        bail!(
                            self,
                            Error::expected_found(
                                "type",
                                format_args!("trait '{}'", self.proj.fmt_ut(&ut)),
                                path.final_component_span(),
                            )
                        )
                    }
                }
                ResolvedType::Error => TypeId::UNKNOWN,
            },
            &TypeHintData::Void => TypeId::VOID,
            &TypeHintData::Ptr(ty) => create_ptr(self, Type::Ptr, ty),
            &TypeHintData::MutPtr(ty) => create_ptr(self, Type::MutPtr, ty),
            &TypeHintData::RawPtr(ty) => create_ptr(self, Type::RawPtr, ty),
            &TypeHintData::RawMutPtr(ty) => create_ptr(self, Type::RawMutPtr, ty),
            TypeHintData::DynPtr(path) => self
                .resolve_dyn_ptr(path)
                .map(|tr| self.proj.types.insert(Type::DynPtr(tr)))
                .unwrap_or_default(),
            TypeHintData::DynMutPtr(path) => self
                .resolve_dyn_ptr(path)
                .map(|tr| self.proj.types.insert(Type::DynMutPtr(tr)))
                .unwrap_or_default(),
            &TypeHintData::Array(ty, count) => {
                let id = self.resolve_typehint(ty);
                let Some(n) = self.consteval_check(count, TypeId::USIZE) else {
                    return self.proj.types.insert(Type::Array(id, 0));
                };
                self.proj.types.insert(Type::Array(id, n.val.try_into().unwrap()))
            }
            TypeHintData::Option(ty) => {
                self.resolve_lang_type(LangType::Option, std::slice::from_ref(ty), span)
            }
            TypeHintData::Vec(ty) => {
                self.resolve_lang_type(LangType::Vec, std::slice::from_ref(ty), span)
            }
            TypeHintData::Map(kv) => self.resolve_lang_type(LangType::Map, &kv[..], span),
            TypeHintData::Set(ty) => {
                self.resolve_lang_type(LangType::Set, std::slice::from_ref(ty), span)
            }
            TypeHintData::Slice(ty) => {
                self.resolve_lang_type(LangType::Span, std::slice::from_ref(ty), span)
            }
            TypeHintData::SliceMut(ty) => {
                self.resolve_lang_type(LangType::SpanMut, std::slice::from_ref(ty), span)
            }
            TypeHintData::Tuple(params) => {
                let mut types = Vec::with_capacity(params.len());
                let mut names = Vec::with_capacity(params.len());
                let mut unique = HashSet::new();
                for (name, ty) in params.iter().cloned() {
                    if !unique.insert(name.data) {
                        self.proj.diag.report(Error::redefinition_k(
                            "field",
                            strdata!(self, name.data),
                            name.span,
                        ));
                        continue;
                    }

                    names.push(name.data);
                    types.push(self.resolve_typehint(ty));
                }
                self.proj.scopes.get_tuple(names, types, &self.proj.types)
            }
            TypeHintData::Fn { is_extern, is_unsafe, params, ret } => {
                let fnptr = FnPtr {
                    is_extern: *is_extern,
                    is_unsafe: *is_unsafe,
                    params: params.iter().map(|&p| self.resolve_typehint(p)).collect(),
                    ret: ret.map(|ret| self.resolve_typehint(ret)).unwrap_or(TypeId::VOID),
                };
                self.proj.types.insert(Type::FnPtr(fnptr))
            }
            TypeHintData::Error => TypeId::UNKNOWN,
        }
    }

    fn resolve_members(&mut self, id: UserTypeId) {
        fn set_interior_mutable(this: &mut TypeChecker, id: UserTypeId, ty: TypeId) {
            let is_mutable = |id: UserTypeId| {
                matches!(this.proj.scopes.get(id).attrs.lang, Some(LangType::Mutable))
            };

            if is_mutable(id) || this.proj.types[ty].as_user().is_some_and(|ut| is_mutable(ut.id)) {
                this.proj.scopes.get_mut(id).interior_mutable = true;
            }
        }

        if self.proj.scopes.get(id).members_resolved {
            return;
        }

        for i in 0..self.proj.scopes.get(id).members.len() {
            let ty = resolve_type!(self, self.proj.scopes.get_mut(id).members[i].ty);
            set_interior_mutable(self, id, ty);
            if self.proj.scopes.get(id).kind.is_packed_struct()
                && let BitSizeResult::Bad = ty.bit_size(&self.proj.scopes, &self.proj.types)
            {
                let (&name, mem) = self.proj.scopes.get(id).members.get_index(i).unwrap();
                named_error!(self, Error::bitfield_member, name, mem.span)
            }
        }

        if let Some(mut union) = self.proj.scopes.get(id).kind.as_union().cloned() {
            resolve_type!(self, union.tag);
            for variant in union.variants.values_mut().flat_map(|v| &mut v.ty) {
                let ty = resolve_type!(self, *variant);
                set_interior_mutable(self, id, ty);
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
                            let Some(res) = self.consteval(expr, span) else {
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
        let mut deps = HashSet::new();
        let mut failed = false;

        if !canonical {
            let canonical_ty =
                Type::User(GenericUserType::from_id(&self.proj.scopes, &self.proj.types, id));
            canonical = this == self.proj.types.insert(canonical_ty);
        }

        macro_rules! check_ty {
            ($ty: expr, $err: expr) => {{
                let ty = $ty.with_ut_templates(&self.proj.types, this);
                if self.check_member_dep(ty, this, &mut deps) {
                    failed = true;
                    if canonical {
                        self.error($err)
                    }
                }
                if self.proj.types[ty]
                    .as_user()
                    .is_some_and(|ut| self.proj.scopes.get(ut.id).interior_mutable)
                {
                    self.proj.scopes.get_mut(id).interior_mutable = true;
                }
            }};
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
                    "union tag makes this type recursive",
                    self.proj.scopes.get(id).name.span
                )
            );
            for (name, var) in union.variants.iter() {
                if let Some(ty) = var.ty {
                    check_ty!(ty, Error::recursive_type(strdata!(self, name), var.span, true));
                }
            }
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

    fn check_member_dep(
        &mut self,
        mut this: TypeId,
        ut: TypeId,
        deps: &mut HashSet<TypeId>,
    ) -> bool {
        while let &Type::Array(inner, _) = &self.proj.types[this] {
            this = inner;
        }
        if ut == this {
            return true;
        }

        if let Type::User(dep) = &self.proj.types[this] {
            deps.insert(this);

            let dep_id = dep.id;
            self.resolve_members(dep_id);
            if self.resolve_dependencies(dep_id, this, false) {
                return true;
            }
            if let Some(Dependencies::Resolved(member_deps)) = self.proj.deps.get(&this) {
                if member_deps.contains(&ut) {
                    return true;
                }

                deps.extend(member_deps.iter().cloned());
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

        let mut deps = HashSet::new();
        let mut failed = false;
        let ids: Vec<_> = self.proj.scopes.get(id).impls.iter_checked().map(|i| i.id).collect();
        for id in ids {
            deps.insert(id);
            if self.check_trait_dependencies(id) {
                failed = true;
            }
            if let Some(Dependencies::Resolved(res)) = self.proj.trait_deps.get(&id) {
                deps.extend(res.iter());
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
        fn resolve_impl(this: &mut TypeChecker, data: TraitImplData) -> Option<GenericTrait> {
            match data {
                TraitImplData::Path(scope, path) => {
                    this.enter_id_and_resolve(scope, |this| match this.resolve_type_path(&path) {
                        ResolvedType::UserType(ut) => {
                            let data = this.proj.scopes.get(ut.id);
                            if data.kind.is_trait() {
                                if path.fn_like && data.attrs.lang != Some(LangType::OpFn) {
                                    this.error(Error::function_like_tr(path.span()))
                                }

                                Some(ut)
                            } else {
                                this.error(Error::expected_found(
                                    "trait",
                                    format_args!("type '{}'", this.proj.fmt_ut(&ut)),
                                    path.final_component_span(),
                                ))
                            }
                        }
                        ResolvedType::Builtin(ty) => this.error(Error::expected_found(
                            "trait",
                            format_args!("type '{}'", this.proj.fmt_ty(ty)),
                            path.final_component_span(),
                        )),
                        ResolvedType::Error => Default::default(),
                    })
                }
                TraitImplData::Operator { tr, ty_args, span, scope } => {
                    this.enter_id_and_resolve(scope, |this| {
                        let Some(tr_id) = this.proj.scopes.lang_types.get(&tr).copied() else {
                            return this.error(Error::no_lang_item(tr, span));
                        };

                        Some(GenericTrait::new(
                            tr_id,
                            this.resolve_type_args(tr_id, &ty_args, true, span),
                        ))
                    })
                }
                TraitImplData::Checked(ut) => Some(ut),
            }
        }

        let impls = match std::mem::take(&mut self.proj.scopes.get_mut(id).impls) {
            TraitImpls::Unchecked(impls) => {
                for i in 0..self.proj.scopes.get(id).type_params.len() {
                    self.resolve_impls(self.proj.scopes.get(id).type_params[i]);
                }

                let mut result = Vec::with_capacity(impls.len());
                for data in impls {
                    let res = resolve_impl(self, data);
                    if let Some(res) = &res {
                        self.resolve_impls(res.id);
                    }
                    result.push(res);
                }
                TraitImpls::Checked(result)
            }
            other => other,
        };

        self.proj.scopes.get_mut(id).impls = impls;
    }

    fn resolve_alias(&mut self, span: Span, id: AliasId) {
        self.check_hover(span, id.into());
        for i in 0..self.proj.scopes.get(id).type_params.len() {
            self.resolve_impls(self.proj.scopes.get(id).type_params[i]);
        }
        resolve_type!(self, self.proj.scopes.get_mut(id).ty);
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

    fn search(scopes: &Scopes, id: UserTypeId, method: StrId) -> Option<Vis<FunctionId>> {
        scopes.get(id).fns.iter().find(|&&id| scopes.get(*id).name.data == method).copied()
    }

    fn do_lookup_member_fn(
        &mut self,
        inst: TypeId,
        name: StrId,
        generics: &[TypeHint],
        span: Span,
    ) -> Option<MemberFn> {
        let finish = |this: &mut TypeChecker, id: FunctionId| {
            this.resolve_type_args(id, generics, false, span)
        };

        fn finish_fn(
            this: &mut TypeChecker,
            inst: TypeId,
            f: Vis<FunctionId>,
            ut: &GenericUserType,
            finish: impl FnOnce(&mut TypeChecker, FunctionId) -> TypeArgs + Clone,
        ) -> Option<MemberFn> {
            let scope = this.proj.scopes.get(f.id).scope;
            let mut func = GenericFn::new(f.id, finish(this, f.id));
            func.ty_args.copy_args(&ut.ty_args);
            if let Some(&i) = this.proj.scopes[scope].kind.as_impl() {
                func.ty_args
                    .copy_args(&TypeArgs::unknown(&this.proj.scopes.get(ut.id).impl_blocks[i]));
            }

            Some(MemberFn {
                func,
                owner: this.proj.scopes.get(ut.id).scope,
                typ: MemberFnType::Normal,
                public: f.public,
                inst,
            })
        }

        fn search_impls(
            this: &mut TypeChecker,
            inst: TypeId,
            name: StrId,
            ut: &GenericUserType,
            finish: impl FnOnce(&mut TypeChecker, FunctionId) -> TypeArgs + Clone,
        ) -> Option<MemberFn> {
            for tr in this.proj.scopes.get(ut.id).impls.clone().into_iter_checked() {
                for imp in this.proj.scopes.walk_super_traits_ex(&this.proj.types, tr) {
                    let Some(f) = TypeChecker::search(&this.proj.scopes, imp.id, name) else {
                        continue;
                    };

                    let mut func = GenericFn::new(f.id, finish(this, f.id));
                    func.ty_args.copy_args_with(&this.proj.types, &imp.ty_args, &ut.ty_args);
                    func.ty_args
                        .insert(*this.proj.scopes.get(imp.id).kind.as_trait().unwrap().0, inst);
                    return Some(MemberFn {
                        func,
                        owner: this.proj.scopes.get(ut.id).scope,
                        typ: MemberFnType::Trait(imp),
                        public: f.public,
                        inst,
                    });
                }
            }

            None
        }

        let ut = if let Type::User(ut) = &self.proj.types[inst] {
            let ut = ut.clone();
            self.resolve_impls(ut.id);
            if let Some(f) = Self::search(&self.proj.scopes, ut.id, name)
                .and_then(|f| finish_fn(self, inst, f, &ut, finish))
            {
                return Some(f);
            }
            Some(ut)
        } else if let Some(tr) = self.proj.types[inst].as_dyn_pointee() {
            let tr = tr.clone();
            self.resolve_impls(tr.id);
            let owner = self.proj.scopes.get(tr.id).scope;
            for imp in self.proj.scopes.walk_super_traits_ex(&self.proj.types, tr) {
                if let Some(f) = Self::search(&self.proj.scopes, imp.id, name) {
                    let mut func = GenericFn::new(f.id, finish(self, f.id));
                    func.ty_args.copy_args(&imp.ty_args);
                    return Some(MemberFn {
                        func,
                        owner,
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

        let exts = self.extensions_in_scope_for(inst);
        if let Some(f) = exts.iter().find_map(|ext| {
            let f = Self::search(&self.proj.scopes, ext.id, name);
            f.and_then(|f| finish_fn(self, inst, f, ext, finish))
        }) {
            return Some(f);
        }

        // look through trait impls AFTER exhausting all concrete functions
        ut.into_iter().chain(exts).find_map(|ut| search_impls(self, inst, name, &ut, finish))
    }

    fn lookup_member_fn(
        &mut self,
        inst: TypeId,
        name: StrId,
        generics: &[TypeHint],
        span: Span,
    ) -> Option<MemberFn> {
        self.do_lookup_member_fn(inst, name, generics, span)
            .inspect(|memfn| self.resolve_proto(memfn.func.id))
    }

    fn do_lookup_unk_trait_fn(
        &mut self,
        inst: TypeId,
        wanted_tr: TraitId,
        method: StrId,
    ) -> Option<MemberFn> {
        let search = |proj: &Project, ut: &GenericUserType| -> Option<MemberFn> {
            for f in proj.scopes.get(ut.id).fns.clone() {
                if proj.scopes.get(f.id).name.data == method {
                    let scope = proj.scopes.get(f.id).scope;
                    let Some(&idx) = proj.scopes[scope].kind.as_impl() else {
                        continue;
                    };
                    let imp = proj.scopes.get(ut.id).impls.get_checked(idx)?;
                    if imp.id != wanted_tr {
                        return None;
                    }

                    let mut func = GenericFn::from_id_unknown(&proj.scopes, f.id);
                    func.ty_args.copy_args(&ut.ty_args);
                    func.ty_args
                        .copy_args(&TypeArgs::unknown(&proj.scopes.get(ut.id).impl_blocks[idx]));
                    return Some(MemberFn {
                        func,
                        owner: proj.scopes.get(ut.id).scope,
                        typ: MemberFnType::Normal,
                        public: f.public,
                        inst,
                    });
                }
            }
            None
        };

        let search_impls = |proj: &Project, ut: &GenericUserType| -> Option<MemberFn> {
            for tr in proj.scopes.get(ut.id).impls.iter_checked() {
                for tr in proj.scopes.walk_super_traits_ex(&proj.types, tr.clone()) {
                    if wanted_tr != tr.id {
                        continue;
                    }

                    let Some(f) = TypeChecker::search(&proj.scopes, tr.id, method) else {
                        continue;
                    };

                    let mut func = GenericFn::from_id_unknown(&proj.scopes, f.id);
                    func.ty_args.copy_args_with(&proj.types, &tr.ty_args, &ut.ty_args);
                    func.ty_args.insert(*proj.scopes.get(tr.id).kind.as_trait().unwrap().0, inst);
                    return Some(MemberFn {
                        func,
                        owner: proj.scopes.get(ut.id).scope,
                        typ: MemberFnType::Trait(tr),
                        public: f.public,
                        inst,
                    });
                }
            }

            None
        };

        let ut = if let Type::User(ut) = &self.proj.types[inst] {
            let ut = ut.clone();
            self.resolve_impls(ut.id);

            if let Some(f) = search(&self.proj, &ut) {
                return Some(f);
            }
            Some(ut)
        } else {
            None
        };

        let exts = self.extensions_in_scope_for(inst);
        if let Some(f) = exts.iter().find_map(|ext| search(&self.proj, ext)) {
            return Some(f);
        }

        // look through trait impls AFTER exhausting all concrete functions
        ut.into_iter().chain(exts).find_map(|ut| search_impls(&self.proj, &ut))
    }

    fn lookup_unk_trait_fn(
        &mut self,
        inst: TypeId,
        wanted_tr: TraitId,
        method: StrId,
    ) -> Option<MemberFn> {
        self.do_lookup_unk_trait_fn(inst, wanted_tr, method)
            .inspect(|memfn| self.resolve_proto(memfn.func.id))
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
                    format!("cannot negate unsigned integer type '{}'", self.proj.fmt_ty(ty)),
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
                        self.proj.fmt_ty(ty),
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
                LoopBreak::WithValue => (
                    self.make_lang_type_by_name(LangType::Option, [(kind.target).unwrap()], span),
                    true,
                ),
                _ => (TypeId::VOID, false),
            }
        }
    }

    fn coerce(&mut self, expr: CExpr, target: TypeId) -> Result<CExpr, CExpr> {
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
                (Type::FnPtr(s), Type::FnPtr(t)) => t.is_unsafe_version_of(s),
                _ => false,
            }
        }

        fn can_closure_to_fn_ptr(this: &TypeChecker, ut: &GenericUserType, f: &FnPtr) -> bool {
            let ut_data = this.proj.scopes.get(ut.id);
            if !ut_data.kind.is_closure() || !ut_data.members.is_empty() {
                return false;
            }

            let tr = ut_data
                .impls
                .iter_checked()
                .find(|tr| this.proj.scopes.get(tr.id).attrs.lang == Some(LangType::OpFn))
                .unwrap();

            let args = this.proj.types[tr.ty_args[0]].as_user().unwrap();
            let ret = tr.ty_args[1].with_templates(&this.proj.types, &ut.ty_args);
            if args.ty_args.len() != f.params.len() || ret != f.ret {
                return false;
            }

            args.ty_args
                .values()
                .map(|v| v.with_templates(&this.proj.types, &ut.ty_args))
                .zip(f.params.iter())
                .all(|(l, &r)| l == r)
        }

        match (&self.proj.types[expr.ty], &self.proj.types[target]) {
            (Type::Never, Type::Never) => Ok(expr),
            (Type::Never, _) => Ok(self.arena.typed(target, CExprData::NeverCoerce(expr))),
            (Type::Unknown, _) | (_, Type::Unknown) => Ok(CExpr::new(target, expr.data)),
            (Type::DynPtr(lhs), Type::DynPtr(rhs))
            | (Type::DynMutPtr(lhs), Type::DynMutPtr(rhs) | Type::DynPtr(rhs)) => {
                if lhs == rhs || self.has_direct_impl(&lhs.clone(), &rhs.clone()) {
                    Ok(self.arena.typed(target, CExprData::DynCoerce(expr, self.current)))
                } else {
                    Err(expr)
                }
            }
            (&Type::Ptr(lhs), Type::DynPtr(rhs)) => {
                if self.implements_trait(lhs, &rhs.clone()) {
                    Ok(self.arena.typed(target, CExprData::DynCoerce(expr, self.current)))
                } else {
                    Err(expr)
                }
            }
            (Type::MutPtr(lhs), Type::DynPtr(rhs) | Type::DynMutPtr(rhs)) => {
                if self.implements_trait(*lhs, &rhs.clone()) {
                    Ok(self.arena.typed(target, CExprData::DynCoerce(expr, self.current)))
                } else {
                    Err(expr)
                }
            }
            (Type::Fn(lhs), Type::FnPtr(rhs)) => {
                let fptr = lhs.as_fn_ptr(&self.proj.scopes, &self.proj.types);
                if &fptr == rhs || rhs.is_unsafe_version_of(&fptr) {
                    Ok(CExpr::new(target, expr.data))
                } else {
                    Err(expr)
                }
            }
            (Type::User(lhs), Type::FnPtr(rhs)) if can_closure_to_fn_ptr(self, lhs, rhs) => {
                Ok(self.arena.typed(target, CExprData::ClosureCoerce(expr, self.current)))
            }
            (lhs, rhs) if may_ptr_coerce(&self.proj.types, lhs, rhs) => {
                Ok(CExpr::new(target, expr.data))
            }
            (lhs, rhs) if lhs == rhs => Ok(expr),
            (_, rhs) => {
                if let Some(inner) = rhs.as_option_inner(&self.proj.scopes) {
                    match self.coerce(expr, inner) {
                        Ok(expr) => Ok(CExpr::option_some(target, expr, &mut self.arena)),
                        Err(expr) => Err(expr),
                    }
                } else if self.can_span_coerce(expr.ty, target).is_some() {
                    Ok(self.arena.typed(target, CExprData::SpanMutCoerce(expr)))
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
        let span = *self.proj.scopes.lang_types.get(&LangType::Span)?;
        let span_mut = *self.proj.scopes.lang_types.get(&LangType::SpanMut)?;
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
impl TypeChecker<'_> {
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
        if let Some(listen) = &mut self.listening_vars {
            listen.push(id);
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
            matches!(self.proj.scopes.get(ut.id).attrs.lang, Some(LangType::String))
        }) {
            if !patterns.any(|patt| patt.irrefutable) {
                self.error(Error::match_statement("", span))
            }
        } else if ty.as_user().is_some_and(|ut| {
            matches!(
                self.proj.scopes.get(ut.id).attrs.lang,
                Some(LangType::Span | LangType::SpanMut)
            )
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
                    format_args!("(missing variant(s) {})", missing.join(", ")),
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
                format!("cannot use union pattern on type '{}'", self.proj.fmt_ty(scrutinee)),
                span,
            )));
        };
        self.resolve_members(ut_id);
        let f = match path {
            ResolvedValue::Fn(f) => f,
            ResolvedValue::MemberFn(m) => m.func,
            ResolvedValue::UnionConstructor(ut) => {
                return Err(Some(Error::type_mismatch(
                    self.proj.fmt_ty(scrutinee),
                    self.proj.fmt_ut(&ut),
                    span,
                )));
            }
            ResolvedValue::Var(id) => {
                return Err(Some(Error::expected_found(
                    self.proj.fmt_ty(scrutinee),
                    format_args!(
                        "variable '{}'",
                        strdata!(self, self.proj.scopes.get(id).name.data)
                    ),
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
                    let inner = ty.with_ut_templates(&self.proj.types, stripped);
                    scrutinee.matched_inner_type(&self.proj.types, inner)
                }),
                variant,
            ))
        } else if f.constructor.is_some_and(|id| self.proj.scopes.get(id).kind.is_union()) {
            self.proj.diag.report(Error::type_mismatch(
                self.proj.fmt_ty(scrutinee),
                self.proj.fmt_ty(f.ret),
                span,
            ));
            Err(Default::default())
        } else if f.constructor.is_some() {
            Err(Some(Error::type_mismatch(
                self.proj.fmt_ty(scrutinee),
                self.proj.fmt_ty(f.ret),
                span,
            )))
        } else {
            Err(Some(Error::expected_found(
                self.proj.fmt_ty(scrutinee),
                format_args!("function '{}'", strdata!(self, f.name.data)),
                span,
            )))
        }
    }

    fn check_int_pattern(
        &mut self,
        target: TypeId,
        patt: &IntPattern,
        span: Span,
    ) -> Option<ComptimeInt> {
        let inner = target.strip_references(&self.proj.types);
        let patt = patt.clone();
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
        patterns: &[Located<Pattern>],
        span_id: UserTypeId,
        typ: PatternType,
    ) -> CPattern {
        let mut rest = None;
        let mut result = Vec::new();
        for (i, patt) in patterns.iter().enumerate() {
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
        patterns: &[Located<Pattern>],
        span: Span,
        typ: PatternType,
    ) -> CPattern {
        let span_id = self.proj.scopes.lang_types.get(&LangType::Span).copied();
        let span_mut_id = self.proj.scopes.lang_types.get(&LangType::SpanMut).copied();
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
                        let ptr = target.matched_inner_type(&self.proj.types, inner);
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
                                self.proj.fmt_ty(target)
                            ),
                            span,
                        )
                    )
                }
            };

        let inner = target.matched_inner_type(&self.proj.types, real_inner);
        let mut rest = None;
        let mut irrefutable = true;
        let mut result = Vec::new();
        for (i, patt) in patterns.iter().enumerate() {
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
                target.matched_inner_type(&self.proj.types, arr);
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
        destructures: &[Destructure],
        span: Span,
        typ: PatternType,
    ) -> CPattern {
        let stripped = scrutinee.strip_references(&self.proj.types);
        let Some(ut) = self.proj.types[stripped].as_user().filter(|ut| {
            matches!(
                self.proj.scopes.get(ut.id).kind,
                UserTypeKind::Struct(_, _) | UserTypeKind::Union(_) | UserTypeKind::Tuple
            )
        }) else {
            bail!(self, Error::bad_destructure(self.proj.fmt_ty(scrutinee), span));
        };
        let ut_id = ut.id;
        self.resolve_members(ut_id);

        let cap = self.can_access_privates(self.proj.scopes.get(ut_id).scope);
        let mut irrefutable = true;
        let mut checked = Vec::new();

        for &Destructure { name, mutable: pm, ref pattern } in destructures {
            let Some(member) = self.proj.scopes.get(ut_id).members.get(&name.data) else {
                self.proj.diag.report(Error::no_member(
                    self.proj.fmt_ty(scrutinee),
                    strdata!(self, name.data),
                    name.span,
                ));
                continue;
            };

            if !member.public && !cap {
                self.proj.diag.report(Error::private_member(
                    self.proj.fmt_ty(scrutinee),
                    strdata!(self, name.data),
                    name.span,
                ));
            }

            // TODO: duplicates
            let inner = member.ty.with_ut_templates(&self.proj.types, stripped);
            let scrutinee = scrutinee.matched_inner_type(&self.proj.types, inner);
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
            data: PatternData::Destructure {
                patterns: checked,
                borrows: self.proj.types[scrutinee].is_any_ptr(),
            },
        }
    }

    fn check_tuple_pattern(
        &mut self,
        scrutinee: TypeId,
        mutable: bool,
        subpatterns: &[Located<Pattern>],
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
                    self.proj.fmt_ty(scrutinee),
                    format_args!("({})", ["_"].repeat(subpatterns.len()).join(", ")),
                    span,
                )
            );
        };

        if ut.ty_args.len() != subpatterns.len() {
            self.proj.diag.report(Error::expected_found(
                self.proj.fmt_ty(scrutinee),
                format_args!("({})", ["_"].repeat(subpatterns.len()).join(", ")),
                span,
            ));
        }

        let mut irrefutable = true;
        let mut checked = Vec::new();
        for (i, patt) in subpatterns.iter().enumerate() {
            let member = intern!(self, "{i}");
            let Some((inner, ty)) = self
                .proj
                .scopes
                .get(ut.id)
                .members
                .get(&member)
                .map(|m| m.ty.with_templates(&self.proj.types, &ut.ty_args))
                .map(|ty| (ty, scrutinee.matched_inner_type(&self.proj.types, ty)))
            else {
                self.proj.diag.report(Error::no_member(
                    self.proj.fmt_ty(scrutinee),
                    strdata!(self, member),
                    patt.span,
                ));
                continue;
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
            checked.push((member, inner, patt));
        }

        CPattern {
            irrefutable,
            data: PatternData::Destructure {
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
        subpatterns: &[Located<Pattern>],
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
        subpatterns: &[Located<Pattern>],
        typ: PatternType,
    ) -> CPattern {
        let mut prev_vars = HashMap::new();
        let mut patterns = vec![];
        let nsubpatterns = subpatterns.len();
        for (i, pattern) in subpatterns.iter().enumerate() {
            let patt_span = pattern.span;
            let (res, vars) = self.listen_for_vars(|this| {
                this.check_pattern(PatternParams {
                    scrutinee,
                    mutable,
                    pattern,
                    typ: if i + 1 != nsubpatterns { PatternType::BodylessFn } else { typ },
                    has_hint: false,
                })
            });
            patterns.push(res);
            if let Some(listen) = &mut self.listening_vars {
                listen.extend_from_slice(&vars);
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
                        let ty_name = self.proj.fmt_ty(var.ty);
                        let old_ty_name = self.proj.fmt_ty(old_ty);
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

    fn check_pattern(&mut self, params: PatternParams) -> CPattern {
        let PatternParams { scrutinee, mutable, pattern, typ, has_hint } = params;
        let span = pattern.span;
        match &pattern.data {
            Pattern::TupleLike { path, subpatterns } => {
                let value = self.resolve_value_path(path, Some(scrutinee));
                self.check_tuple_union_pattern(scrutinee, mutable, value, subpatterns, span, typ)
            }
            Pattern::StructLike { path, subpatterns } => {
                let value = self.resolve_value_path(path, Some(scrutinee));
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
                let value = self.resolve_value_path(path, Some(scrutinee));
                if let Some(ident) = path.as_identifier() {
                    match self.get_union_variant(scrutinee, value, ident.span) {
                        Ok((Some(_), _)) => self.error(Error::expected_found(
                            "empty variant pattern",
                            "tuple variant pattern",
                            ident.span,
                        )),
                        Ok((None, variant)) => CPattern::refutable(PatternData::Variant {
                            pattern: None,
                            variant,
                            inner: TypeId::UNKNOWN,
                            borrows: false,
                        }),
                        Err(Some(_)) => {
                            let Some(var) =
                                self.insert_pattern_var(typ, ident, scrutinee, mutable, has_hint)
                            else {
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
                    std::slice::from_ref(patt),
                    pattern.span,
                    typ,
                )
            }
            &Pattern::MutBinding(name) => {
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
            &Pattern::String(value) => {
                let string = self.make_lang_type_by_name(LangType::String, [], span);
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
            &Pattern::IntRange(RangePattern { inclusive, ref start, ref end }) => {
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
            &Pattern::Char(ch) => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::CHAR {
                    bail!(self, type_mismatch_err!(self, scrutinee, TypeId::CHAR, span));
                }

                CPattern::refutable(PatternData::Int(ComptimeInt::from(ch as u32)))
            }
            &Pattern::CharRange(RangePattern { inclusive, start, end }) => {
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
            &Pattern::Bool(val) => {
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

    fn check_full_pattern(&mut self, scrutinee: TypeId, pattern: &FullPattern) -> CPattern {
        self.check_pattern(PatternParams {
            scrutinee,
            mutable: false,
            pattern: &pattern.data,
            typ: PatternType::Regular,
            has_hint: false,
        })
    }
}

/// Path resolution routines
impl TypeChecker<'_> {
    fn find_in_tns(&self, name: StrId) -> Option<Vis<TypeItem>> {
        for (id, scope) in self.proj.scopes.walk(self.current) {
            if let Some(item) = self.proj.scopes[id].find_in_tns(name) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        if let Some(item) = self.proj.autouse_tns.get(&name).copied() {
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

        if let Some(item) = self.proj.autouse_vns.get(&name).copied() {
            return Some(item);
        }

        None
    }

    fn get_super(&mut self, span: Span) -> Option<ScopeId> {
        if let Some(module) = self.proj.scopes.module_of(
            self.proj.scopes[self.proj.scopes.module_of(self.current).unwrap()].parent.unwrap(),
        ) {
            self.check_hover(span, LspItem::Module(module, true));
            Some(module)
        } else {
            self.error(Error::new("cannot use super here", span))
        }
    }

    fn resolve_use(
        &mut self,
        public: bool,
        scope: Option<ScopeId>,
        comp: &UsePathComponent,
        mut in_type: bool,
        declaring: bool,
    ) {
        let can_import_value_item = |this: &Self, item: ValueItem| -> bool {
            !in_type
                || item
                    .as_fn()
                    .and_then(|id| this.proj.scopes.get(*id).constructor)
                    .is_some_and(|ut| this.proj.scopes.get(ut).kind.is_union())
        };

        let resolve_later = |this: &mut Self, ident: Option<Located<StrId>>| {
            if declaring {
                this.proj.scopes[this.current].use_stmts.push(DUsePath {
                    public,
                    in_type,
                    scope,
                    comp: comp.clone(),
                });
            } else if let Some(ident) = ident {
                named_error!(this, Error::no_symbol, ident.data, ident.span)
            }
        };

        let import = |this: &mut Self, name: Located<StrId>, new_name: Located<StrId>| {
            let Some(scope) = scope else {
                return this.error(Error::new("cannot import from the current scope", name.span));
            };

            let check_hover_both = |this: &mut Self, item: LspItem| {
                this.check_hover(name.span, item);
                if name.span != new_name.span {
                    this.check_hover(new_name.span, item);
                }
            };

            let mut found = false;
            if let Some(item) = this.proj.scopes[scope].find_in_tns(name.data) {
                check_hover_both(this, (*item).into());
                if !item.public && !this.can_access_privates(scope) {
                    named_error!(this, Error::private, name.data, name.span)
                }

                if this.proj.scopes[this.current]
                    .tns
                    .insert(new_name.data, Vis::new(*item, public))
                    .is_some()
                {
                    named_error!(this, Error::redefinition, new_name.data, new_name.span)
                }
                found = true;
            }

            if let Some(item) = this.proj.scopes[scope].find_in_vns(name.data)
                && can_import_value_item(this, *item)
            {
                check_hover_both(
                    this,
                    match item.id {
                        ValueItem::StructConstructor(id, _) => id.into(),
                        _ => (*item).into(),
                    },
                );

                let mut skip = false;
                if !item.public && !this.can_access_privates(scope) {
                    if !found {
                        named_error!(this, Error::private, name.data, name.span)
                    }
                    skip = true;
                }

                if !skip
                    && this.proj.scopes[this.current]
                        .vns
                        .insert(new_name.data, Vis::new(*item, public))
                        .is_some()
                {
                    named_error!(this, Error::redefinition, new_name.data, new_name.span)
                }
                found = true;
            }

            if !found {
                resolve_later(this, Some(name));
            }
        };

        if !declaring && let Some(scope) = scope {
            self.enter_id_and_resolve(scope, |_| {});
        }

        match comp {
            UsePathComponent::Multi(comps) => {
                for comp in comps {
                    self.resolve_use(public, scope, comp, in_type, declaring);
                }
            }
            UsePathComponent::Ident { ident, next } => {
                if let Some(next) = next {
                    let value = if let Some(scope) = scope {
                        let item = self.proj.scopes[scope].find_in_tns(ident.data);
                        if let Some(item) = item
                            && !item.public
                            && !self.can_access_privates(scope)
                        {
                            named_error!(self, Error::private, ident.data, ident.span)
                        }
                        item
                    } else if let Some(item) = self.find_in_tns(ident.data) {
                        Some(item)
                    } else {
                        self.proj.scopes[ScopeId::ROOT].find_in_tns(ident.data)
                    };

                    let scope = match value.map(|v| v.id) {
                        Some(TypeItem::Module(next)) => {
                            self.check_hover(ident.span, next.into());
                            next
                        }
                        Some(TypeItem::Type(id)) => {
                            in_type = true;
                            self.check_hover(ident.span, id.into());
                            self.proj.scopes.get(id).body_scope
                        }
                        _ => return resolve_later(self, Some(*ident)),
                    };

                    return self.resolve_use(public, Some(scope), next, in_type, declaring);
                }

                import(self, *ident, *ident);
            }
            &UsePathComponent::Rename { ident, new_name } => import(self, ident, new_name),
            &UsePathComponent::All(span) => {
                let Some(scope) = scope else {
                    return self.error(Error::new("use path cannot start with a '*'", span));
                };

                if declaring {
                    return resolve_later(self, None);
                }

                let cap = self.can_access_privates(scope);
                let mut tns = vec![];
                let mut vns = vec![];
                for (&name, &item) in self.proj.scopes[scope].tns.iter() {
                    if item.public || cap {
                        tns.push((name, Vis::new(*item, public)));
                    }
                }

                for (&name, &item) in self.proj.scopes[scope].vns.iter() {
                    if (item.public || cap) && can_import_value_item(self, *item) {
                        vns.push((name, Vis::new(*item, public)));
                    }
                }

                for (name, item) in tns {
                    self.proj.scopes[self.current].tns.entry(name).or_insert(item);
                }

                for (name, item) in vns {
                    self.proj.scopes[self.current].vns.entry(name).or_insert(item);
                }
            }
            UsePathComponent::Error => {}
        }
    }

    fn resolve_type_path(&mut self, path: &Path) -> ResolvedType {
        let span = path.span();
        match path.origin {
            PathOrigin::Root(_) => {
                self.resolve_type_path_in(&path.components, Default::default(), ScopeId::ROOT, span)
            }
            PathOrigin::Super(span) => self
                .get_super(span)
                .map(|id| self.resolve_type_path_in(&path.components, Default::default(), id, span))
                .unwrap_or_default(),
            PathOrigin::This(span) => {
                let Some(ty) = self.resolve_this_type(span) else {
                    return Default::default();
                };

                if path.components.is_empty() {
                    ResolvedType::Builtin(ty)
                } else if path.components.len() == 1
                    && let Type::User(ut) = &self.proj.types[ty]
                {
                    let (name, args) = &path.components[0];
                    self.find_associated_type(ut.id, *name, args)
                } else {
                    let name = path.components[0].0;
                    named_error!(self, Error::no_symbol, name.data, name.span)
                }
            }
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

                let Some(item) = self.find_in_tns(name.data) else {
                    return self.resolve_type_path_in(
                        &path.components,
                        Default::default(),
                        ScopeId::ROOT,
                        span,
                    );
                };

                match item.id {
                    TypeItem::Type(id) => {
                        self.check_hover(name.span, id.into());
                        let ty_args = self.resolve_type_args(id, ty_args, true, name.span);
                        if rest.is_empty() {
                            if self.proj.scopes.get(id).kind.is_extension() {
                                return self.error(Error::expected_found(
                                    "type",
                                    format_args!(
                                        "extension '{}'",
                                        strdata!(self, self.proj.scopes.get(id).name.data)
                                    ),
                                    name.span,
                                ));
                            }

                            return ResolvedType::UserType(GenericUserType::new(id, ty_args));
                        }

                        if rest.len() == 1 {
                            return self.find_associated_type(id, rest[0].0, &rest[0].1);
                        }

                        self.resolve_type_path_in(
                            rest,
                            ty_args,
                            self.proj.scopes.get(id).body_scope,
                            span,
                        )
                    }
                    TypeItem::Module(id) => {
                        if rest.is_empty() {
                            return named_error!(self, Error::no_symbol, name.data, name.span);
                        }

                        self.check_hover(name.span, id.into());
                        if !ty_args.is_empty() {
                            return self.error(Error::new(
                                "modules cannot be parameterized with type arguments",
                                name.span,
                            ));
                        }

                        self.resolve_type_path_in(rest, Default::default(), id, span)
                    }
                    TypeItem::Alias(id) => {
                        self.resolve_alias(name.span, id);
                        let ty_args = self.resolve_type_args(id, ty_args, true, name.span);
                        let ty =
                            self.proj.scopes.get(id).ty.with_templates(&self.proj.types, &ty_args);
                        if !rest.is_empty() {
                            self.error(Error::new(
                                "this operation is not yet supported with type aliases",
                                span,
                            ))
                        } else if let Some(ut) = self.proj.types[ty].as_user() {
                            ResolvedType::UserType(ut.clone())
                        } else {
                            ResolvedType::Builtin(ty)
                        }
                    }
                }
            }
            PathOrigin::Infer(_) => unreachable!("Infer path in type path"),
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
                                format_args!(
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
                TypeItem::Alias(id) => {
                    self.resolve_alias(name.span, id);
                    let args = self.resolve_type_args(id, args, true, name.span);
                    ty_args.copy_args(&args);

                    let ty = self.proj.scopes.get(id).ty.with_templates(&self.proj.types, &ty_args);
                    if !done {
                        self.error(Error::new(
                            "this operation is not yet supported with type aliases",
                            name.span,
                        ))
                    } else if let Some(ut) = self.proj.types[ty].as_user() {
                        return ResolvedType::UserType(ut.clone());
                    } else {
                        return ResolvedType::Builtin(ty);
                    }
                }
            }
        }

        unreachable!()
    }

    fn resolve_value_path(&mut self, path: &Path, target: Option<TypeId>) -> ResolvedValue {
        let span = path.span();
        match path.origin {
            PathOrigin::Root(_) => self.resolve_value_path_in(
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
                            if let UserTypeKind::Trait { this, .. } = self.proj.scopes.get(id).kind
                            {
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
                        Some(TypeItem::Alias(id)) => {
                            self.resolve_alias(name.span, id);
                            let ty_args = self.resolve_type_args(id, ty_args, false, name.span);
                            let ty = self
                                .proj
                                .scopes
                                .get(id)
                                .ty
                                .with_templates(&self.proj.types, &ty_args);
                            self.resolve_value_path_from_type(ty, rest, span)
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
            PathOrigin::Infer(_) => {
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
            PathOrigin::This(this_span) => {
                let Some(this_ty) = self.resolve_this_type(this_span) else {
                    return Default::default();
                };

                if !path.components.is_empty() {
                    return self.resolve_value_path_from_type(this_ty, &path.components, span);
                }

                if let Type::User(ut) = &self.proj.types[this_ty]
                    && let UserTypeKind::Struct(cons, _) = self.proj.scopes.get(ut.id).kind
                {
                    let ut_id = ut.id;
                    self.resolve_proto(cons);
                    // Struct constructor never has type params of its own
                    return ResolvedValue::Fn(GenericFn::new(
                        cons,
                        TypeArgs::unknown(&self.proj.scopes.get(ut_id).item),
                    ));
                }

                named_error!(self, Error::no_symbol, Strings::THIS_TYPE, this_span)
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
                    if let UserTypeKind::Trait { this, .. } = ty.kind {
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
                TypeItem::Alias(id) => {
                    self.resolve_alias(name.span, id);
                    ty_args.copy_args(&self.resolve_type_args(id, args, false, name.span));
                    let ty = self.proj.scopes.get(id).ty.with_templates(&self.proj.types, &ty_args);
                    return self.resolve_value_path_from_type(ty, &data[i + 1..], total_span);
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

        let Some(mfn) = self.lookup_member_fn(ty, name.data, args, name.span) else {
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
                self.proj.fmt_ty(ty)
            )
        }
        ResolvedValue::MemberFn(mfn)
    }

    fn resolve_type_args<T: ItemId>(
        &mut self,
        id: T,
        args: &[TypeHint],
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

        let mut ty_args = TypeArgs::unknown(&self.proj.scopes.get(id).item);
        let in_scope = &self.extensions_in_scope(self.current);
        for (i, arg) in params.iter().enumerate().take(args.len()) {
            let id = self.resolve_typehint(args[i]);
            self.do_insert_ty_arg(in_scope, &mut ty_args, *arg, id);
        }

        for (&id, &ty) in ty_args.iter() {
            self.check_bounds(&ty_args, ty, id, span);
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
            name => Type::from_int_name(name, true),
        }
    }

    fn find_associated_type(
        &mut self,
        ut: UserTypeId,
        name: Located<StrId>,
        args: &[TypeHint],
    ) -> ResolvedType {
        if !self.proj.scopes.get(ut).kind.is_template() {
            return named_error!(self, Error::no_symbol, name.data, name.span);
        }

        let mut candidates = HashSet::new();
        self.resolve_impls(ut);

        for tr in self.proj.scopes.get(ut).impls.iter_checked() {
            for tr in self.proj.scopes.walk_super_traits(tr.id) {
                let UserTypeKind::Trait { assoc_types, .. } = &self.proj.scopes.get(tr).kind else {
                    unreachable!();
                };

                if let Some(id) = assoc_types.get(&name.data) {
                    candidates.insert(*id);
                }
            }
        }

        if candidates.is_empty() {
            return named_error!(self, Error::no_symbol, name.data, name.span);
        } else if candidates.len() > 1 {
            return self.error(Error::new(
                format!("ambiguous associated type '{}'", strdata!(self, name.data)),
                name.span,
            ));
        }

        let id = candidates.into_iter().next().unwrap();
        self.check_hover(name.span, LspItem::Type(id));
        ResolvedType::UserType(GenericUserType::new(
            id,
            self.resolve_type_args(id, args, true, name.span),
        ))
    }

    fn resolve_this_type(&mut self, span: Span) -> Option<TypeId> {
        let current = self.current_function();
        for (_, scope) in self.proj.scopes.walk(self.current) {
            match scope.kind {
                ScopeKind::UserType(id) => {
                    check_hover!(self, span, id.into());
                    match &self.proj.scopes.get(id).kind {
                        &UserTypeKind::Trait { this: this_ty, .. } => {
                            let ut = GenericUserType::from_id(
                                &self.proj.scopes,
                                &self.proj.types,
                                this_ty,
                            );
                            return Some(self.proj.types.insert(Type::User(ut)));
                        }
                        UserTypeKind::Extension(ty) => return Some(*ty),
                        _ => {
                            let ut =
                                GenericUserType::from_id(&self.proj.scopes, &self.proj.types, id);
                            return Some(self.proj.types.insert(Type::User(ut)));
                        }
                    }
                }
                ScopeKind::Function(f) if Some(f) != current => break,
                _ => {}
            }
        }
        self.error(Error::new(format!("'{THIS_TYPE}' outside of type"), span))
    }
}

struct ConstValue {
    ty: TypeId,
    val: ComptimeInt,
}

/// CTFE related functions
impl TypeChecker<'_> {
    fn consteval_check(&mut self, expr: PExpr, target: TypeId) -> Option<ConstValue> {
        let span = expr.span;
        let expr = self.type_check(expr, target);
        (expr.ty == target).then(|| self.consteval(expr, span)).flatten()
    }

    fn consteval(&mut self, expr: CExpr, span: Span) -> Option<ConstValue> {
        match self.arena.get(expr.data) {
            CExprData::Int(val) => Some(ConstValue { ty: expr.ty, val: val.clone() }),
            &CExprData::Binary(op, lhs, rhs) => {
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
                let CExprData::Fn(func, _) = self.arena.get(callee.data) else {
                    return self.error(Error::no_consteval(span));
                };

                let f = self.proj.scopes.get(func.id);
                match f.attrs.intrinsic {
                    Some(Intrinsic::SizeOf) => {
                        let ty = func.first_type_arg().unwrap();
                        // TODO: make sure the ty has had resolve_members()
                        // and resolve_dependencies() called on it and doesn't have any template args
                        let (sz, _) = ty.size_and_align(&self.proj.scopes, &self.proj.types);
                        Some(ConstValue { ty: TypeId::USIZE, val: ComptimeInt::from(sz) })
                    }
                    Some(Intrinsic::AlignOf) => {
                        let ty = func.first_type_arg().unwrap();
                        // TODO: make sure the ty has had resolve_members()
                        // and resolve_dependencies() called on it and doesn't have any template args
                        let (_, align) = ty.size_and_align(&self.proj.scopes, &self.proj.types);
                        Some(ConstValue { ty: TypeId::USIZE, val: ComptimeInt::from(align) })
                    }
                    _ => self.error(Error::no_consteval(span)),
                }
            }
            &CExprData::Var(id) => {
                let var = self.proj.scopes.get(id);
                if !var.kind.is_const() {
                    return self.error(Error::no_consteval(span));
                }
                self.consteval(var.value?, span)
            }
            CExprData::Error => None,
            _ => self.error(Error::no_consteval(span)),
        }
    }
}

impl TypeChecker<'_> {
    fn extensions_in_scope_for(&mut self, ty: TypeId) -> Vec<GenericExtension> {
        let exts = self.extensions_in_scope(self.current);
        self.do_extensions_in_scope_for(&exts, ty)
    }

    fn implements_trait(&mut self, ty: TypeId, bound: &GenericTrait) -> bool {
        // TODO: removing this causes errors in otherwise valid code
        if ty == TypeId::UNKNOWN {
            return true;
        }

        let exts = self.extensions_in_scope(self.current);
        self.do_implements_trait(&exts, ty, bound)
    }

    fn has_direct_impl(&mut self, ut: &GenericUserType, bound: &GenericTrait) -> bool {
        let exts = self.extensions_in_scope(self.current);
        self.do_has_direct_impl(&exts, ut, bound)
    }

    fn infer_type_args(&mut self, item: &mut TypeArgs, src: TypeId, target: TypeId) {
        let exts = self.extensions_in_scope(self.current);
        self.do_infer_type_args(&exts, item, src, target)
    }

    fn infer_call_type_args(
        &mut self,
        item: &mut TypeArgs,
        src: TypeId,
        target: TypeId,
        use_current_call_args: bool,
    ) {
        if !use_current_call_args || self.current_call_ty_args.is_some() {
            let exts = self.extensions_in_scope(self.current);
            self.do_infer_type_args_ex(&exts, item, src, target, |this, ty_args, target| {
                !shouldnt_infer_with(
                    &this.proj,
                    this.current_call_ty_args.as_ref().unwrap_or(ty_args),
                    target,
                )
            })
        } else {
            self.infer_type_args(item, src, target)
        }
    }
}

impl SharedStuff for TypeChecker<'_> {
    fn do_resolve_impls(&mut self, id: UserTypeId) {
        self.resolve_impls(id);
    }

    fn resolve_ext_type(&mut self, id: ExtensionId) -> TypeId {
        resolve_type!(self, *self.proj.scopes.get_mut(id).kind.as_extension_mut().unwrap())
    }

    fn proj(&self) -> &Project {
        &self.proj
    }

    fn extension_cache(&mut self) -> &mut ExtensionCache {
        &mut self.cache
    }

    fn get_tuple(&mut self, ty_args: Vec<TypeId>) -> TypeId {
        let names = (0..ty_args.len()).map(|i| self.proj.strings.get_or_intern(format!("{i}")));
        self.proj.scopes.get_tuple(names.collect(), ty_args, &self.proj.types)
    }
}

pub trait SharedStuff {
    fn resolve_ext_type(&mut self, id: ExtensionId) -> TypeId;
    fn do_resolve_impls(&mut self, id: UserTypeId);
    fn proj(&self) -> &Project;
    fn extension_cache(&mut self) -> &mut ExtensionCache;
    fn get_tuple(&mut self, ty_args: Vec<TypeId>) -> TypeId;

    // ------

    fn lookup_trait_fn(
        &mut self,
        inst: TypeId,
        wanted_tr: &GenericTrait,
        method: StrId,
        scope: ScopeId,
        finish: impl FnOnce(&Project, FunctionId) -> TypeArgs + Copy,
    ) -> Option<MemberFn> {
        let in_scope = &self.extensions_in_scope(scope);
        let search = |this: &mut Self, ut: &GenericUserType| -> Option<MemberFn> {
            for f in this.proj().scopes.get(ut.id).fns.clone() {
                if this.proj().scopes.get(f.id).name.data == method {
                    let scope = this.proj().scopes.get(f.id).scope;
                    let Some(idx) = this.proj().scopes[scope].kind.as_impl().cloned() else {
                        continue;
                    };
                    let mut imp = this.proj().scopes.get(ut.id).impls.get_checked(idx).cloned()?;
                    imp.fill_templates(&this.proj().types, &ut.ty_args);

                    let imp_ty_args =
                        this.do_is_impl_usable(in_scope, ut.id, idx, &mut imp, wanted_tr)?;
                    if &imp != wanted_tr {
                        continue;
                    }

                    let mut func = GenericFn::new(f.id, finish(this.proj(), f.id));
                    func.ty_args.copy_args(&ut.ty_args);
                    func.ty_args.copy_args(&imp_ty_args);
                    return Some(MemberFn {
                        func,
                        owner: this.proj().scopes.get(ut.id).scope,
                        typ: MemberFnType::Normal,
                        public: f.public,
                        inst,
                    });
                }
            }

            None
        };

        let check_impl = |proj: &Project,
                          mut imp: GenericTrait,
                          ut: Option<&GenericUserType>|
         -> Option<MemberFn> {
            imp.ty_args.copy_args(&wanted_tr.ty_args);
            if wanted_tr != &imp {
                return None;
            }

            let f = TypeChecker::search(&proj.scopes, imp.id, method)?;
            let mut func = GenericFn::new(f.id, finish(proj, f.id));
            if let Some(ut) = ut {
                func.ty_args.copy_args_with(&proj.types, &imp.ty_args, &ut.ty_args);
            } else {
                func.ty_args.copy_args(&imp.ty_args);
            }

            func.ty_args.insert(*proj.scopes.get(imp.id).kind.as_trait().unwrap().0, inst);
            Some(MemberFn {
                func,
                owner: proj.scopes.get(imp.id).scope,
                typ: MemberFnType::Trait(imp),
                public: f.public,
                inst,
            })
        };

        let ut = if let Type::User(ut) = &self.proj().types[inst] {
            let ut = ut.clone();
            self.do_resolve_impls(ut.id);

            if let Some(f) = search(self, &ut) {
                return Some(f);
            }
            Some(ut)
        } else {
            None
        };

        let exts = self.do_extensions_in_scope_for(in_scope, inst);
        if let Some(f) = exts.iter().find_map(|ext| search(self, ext)) {
            return Some(f);
        }

        for imp in self.get_trait_impls(inst).into_iter_checked() {
            if let Some(f) = check_impl(self.proj(), imp, ut.as_ref()) {
                return Some(f);
            }
        }

        for ext in exts {
            for imp in self.proj().scopes.get(ext.id).impls.clone().into_iter_checked() {
                if let Some(f) = check_impl(self.proj(), imp, Some(&ext)) {
                    return Some(f);
                }
            }
        }

        None
    }

    // ------

    fn extensions_in_scope(&self, scope: ScopeId) -> Vec<ExtensionId> {
        fn get_tns_extensions<'a>(
            scopes: &'a Scopes,
            tns: &'a HashMap<StrId, Vis<TypeItem>>,
        ) -> impl Iterator<Item = UserTypeId> + use<'a> {
            tns.iter().flat_map(|s| {
                s.1.as_type().filter(|&&id| scopes.get(id).kind.is_extension()).cloned()
            })
        }

        let mut exts = vec![];
        let scopes = &self.proj().scopes;
        for (_, scope) in scopes.walk(scope) {
            exts.extend(get_tns_extensions(scopes, &scope.tns));
            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        exts.extend(get_tns_extensions(scopes, &self.proj().autouse_tns));
        exts.sort();

        // TODO: this is a hack. ideally the order of extensions shouldn't matter but for now
        // process this extension last so any other Debug impl will be recognized first
        if let Some(dbg_idx) = exts.iter().position(|id| {
            matches!(self.proj().scopes.get(*id).attrs.lang, Some(LangType::FallbackDebug))
        }) {
            let last = exts.len() - 1;
            exts.swap(dbg_idx, last);
        }

        exts
    }

    // ------

    fn do_extensions_in_scope_for(
        &mut self,
        in_scope: &[ExtensionId],
        ty: TypeId,
    ) -> Vec<GenericExtension> {
        if ty == TypeId::UNKNOWN {
            return vec![];
        }

        let map = self.extension_cache().entry(in_scope.to_vec()).or_default();
        if let Some(checked) = map.get(&ty) {
            return checked.clone();
        }

        // TODO: this breaks extensions being able to check impls from other extensions
        //       (like in tests/ext/circular.ctl)
        map.insert(ty, vec![]);

        let res: Vec<_> =
            in_scope.iter().flat_map(|id| self.applies_to(in_scope, ty, *id)).collect();
        self.extension_cache().get_mut(in_scope).unwrap().insert(ty, res.clone());
        res
    }

    fn applies_to(
        &mut self,
        in_scope: &[ExtensionId],
        ty: TypeId,
        ext: ExtensionId,
    ) -> Option<GenericExtension> {
        let ext_ty_id = self.resolve_ext_type(ext);
        self.do_resolve_impls(ext);

        let mut ext = GenericExtension::from_id_unknown(&self.proj().scopes, ext);
        self.do_infer_type_args(in_scope, &mut ext.ty_args, ext_ty_id, ty);
        if ext_ty_id.with_templates(&self.proj().types, &ext.ty_args) != ty {
            return None;
        }

        for (&id, &arg) in ext.ty_args.iter() {
            if arg == TypeId::UNKNOWN {
                return None;
            }
            for mut bound in self.proj().scopes.get(id).impls.clone().into_iter_checked() {
                bound.fill_templates(&self.proj().types, &ext.ty_args);
                if !self.do_implements_trait(in_scope, arg, &bound) {
                    return None;
                }
            }
        }

        Some(ext)
    }

    fn do_implements_trait(
        &mut self,
        in_scope: &[ExtensionId],
        ty: TypeId,
        bound: &GenericTrait,
    ) -> bool {
        for (i, mut tr) in self.get_trait_impls(ty).into_iter_checked_enumerate() {
            if let Type::User(ut) = &self.proj().types[ty]
                && self.do_is_impl_usable(in_scope, ut.id, i, &mut tr, bound).is_none()
            {
                continue;
            }

            self.do_resolve_impls(tr.id);
            for mut tr in self
                .proj()
                .scopes
                .walk_super_traits_ex(&self.proj().types, tr)
                .into_iter()
                .filter(|tr| tr.id == bound.id)
            {
                if let Type::User(ut) = &self.proj().types[ty] {
                    tr.fill_templates(&self.proj().types, &ut.ty_args);
                }
                if &tr == bound {
                    return true;
                }
            }
        }

        for ext in self.do_extensions_in_scope_for(in_scope, ty) {
            if self.do_has_direct_impl(in_scope, &ext, bound) {
                return true;
            }
        }

        false
    }

    fn do_is_impl_usable(
        &mut self,
        in_scope: &[ExtensionId],
        ut: UserTypeId,
        idx: usize,
        tr: &mut GenericTrait,
        bound: &GenericTrait,
    ) -> Option<TypeArgs> {
        if let Some(block) = self.proj().scopes.get(ut).impl_blocks.get(idx) {
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
                let Type::User(ut) = &self.proj().types[*val] else {
                    return None;
                };

                ty_args.insert(ut.id, bound.ty_args[arg]);
                *val = bound.ty_args[arg];
            }

            for (&arg, &val) in ty_args.iter() {
                if val == TypeId::UNKNOWN {
                    continue;
                }

                if !self.satisfies_bounds(in_scope, &ty_args, val, arg) {
                    return None;
                }
            }

            Some(ty_args)
        } else {
            Some(Default::default())
        }
    }

    fn do_has_direct_impl(
        &mut self,
        in_scope: &[ExtensionId],
        ut: &GenericUserType,
        bound: &GenericTrait,
    ) -> bool {
        self.do_resolve_impls(ut.id);
        for (i, mut tr) in self.proj().scopes.get(ut.id).impls.clone().into_iter_checked_enumerate()
        {
            if self.do_is_impl_usable(in_scope, ut.id, i, &mut tr, bound).is_none() {
                continue;
            }

            for mut tr in self
                .proj()
                .scopes
                .walk_super_traits_ex(&self.proj().types, tr)
                .into_iter()
                .filter(|tr| tr.id == bound.id)
            {
                tr.fill_templates(&self.proj().types, &ut.ty_args);
                if &tr == bound {
                    return true;
                }
            }
        }
        false
    }

    fn satisfies_bounds(
        &mut self,
        in_scope: &[ExtensionId],
        ty_args: &TypeArgs,
        ty: TypeId,
        id: UserTypeId,
    ) -> bool {
        self.do_resolve_impls(id);
        for mut bound in self.proj().scopes.get(id).impls.clone().into_iter_checked() {
            bound.fill_templates(&self.proj().types, ty_args);
            if !self.do_implements_trait(in_scope, ty, &bound) {
                return false;
            }
        }
        true
    }

    fn do_infer_type_args(
        &mut self,
        in_scope: &[ExtensionId],
        ty_args: &mut TypeArgs,
        src: TypeId,
        target: TypeId,
    ) {
        self.do_infer_type_args_ex(in_scope, ty_args, src, target, |_, _, _| true);
    }

    fn do_infer_type_args_ex(
        &mut self,
        in_scope: &[ExtensionId],
        ty_args: &mut TypeArgs,
        mut src: TypeId,
        mut target: TypeId,
        mut is_valid: impl FnMut(&Self, &TypeArgs, TypeId) -> bool + Copy,
    ) {
        use indexmap::map::Entry;

        loop {
            match (&self.proj().types[src], &self.proj().types[target]) {
                (
                    Type::Ptr(gi) | Type::MutPtr(gi) | Type::RawPtr(gi) | Type::RawMutPtr(gi),
                    Type::Ptr(ti) | Type::MutPtr(ti) | Type::RawPtr(ti) | Type::RawMutPtr(ti),
                ) => {
                    src = *gi;
                    target = *ti;
                }
                (Type::Array(gi, _), Type::Array(ti, _)) => {
                    src = *gi;
                    target = *ti;
                }
                (Type::FnPtr(src), Type::FnPtr(target)) => {
                    let src = src.clone();
                    let target = target.clone();
                    for (&src, &target) in src.params.iter().zip(target.params.iter()) {
                        self.do_infer_type_args_ex(in_scope, ty_args, src, target, is_valid);
                    }

                    self.do_infer_type_args_ex(in_scope, ty_args, src.ret, target.ret, is_valid);
                    break;
                }
                (
                    Type::DynPtr(src) | Type::DynMutPtr(src),
                    Type::DynPtr(target) | Type::DynMutPtr(target),
                ) => {
                    if src.id != target.id {
                        break;
                    }

                    let src = src.clone();
                    let target = target.clone();
                    for (&src, &target) in src.ty_args.values().zip(target.ty_args.values()) {
                        self.do_infer_type_args_ex(in_scope, ty_args, src, target, is_valid);
                    }
                    break;
                }
                (Type::User(src), target_ty) => {
                    // TODO: T => ?T
                    if let Entry::Occupied(entry) = ty_args.entry(src.id) {
                        if entry.get() != &TypeId::UNKNOWN || !is_valid(self, ty_args, target) {
                            return;
                        }

                        self.do_insert_ty_arg(in_scope, ty_args, src.id, target);
                    } else if let Type::User(target) = target_ty
                        && src.id == target.id
                    {
                        let src = src.clone();
                        let target = target.clone();
                        for (&src, &target) in src.ty_args.values().zip(target.ty_args.values()) {
                            self.do_infer_type_args_ex(in_scope, ty_args, src, target, is_valid);
                        }
                    }

                    break;
                }
                _ => break,
            }
        }
    }

    fn do_insert_ty_arg(
        &mut self,
        in_scope: &[ExtensionId],
        ty_args: &mut TypeArgs,
        key: UserTypeId,
        val: TypeId,
    ) {
        ty_args.insert(key, val);

        self.do_resolve_impls(key);
        for tr in self.proj().scopes.get(key).impls.clone().into_iter_checked() {
            if tr.ty_args.is_empty() {
                continue;
            }

            if let Some(mine) = self.find_trait_impl(in_scope, tr.id, val) {
                for (&src, &target) in tr.ty_args.values().zip(mine.ty_args.values()) {
                    self.do_infer_type_args(in_scope, ty_args, src, target);
                }
            }
        }
    }

    fn get_trait_impls(&mut self, id: TypeId) -> TraitImpls {
        if let Some(ut) = self.proj().types[id].as_user() {
            let id = ut.id;
            self.do_resolve_impls(id);
            return self.proj().scopes.get(id).impls.clone();
        }

        let proj = self.proj();
        let scopes = &proj.scopes;

        let mut trs = vec![];
        let mut add_if = |cond: bool, lang: LangType| {
            if cond && let Some(&tr_id) = scopes.lang_types.get(&lang) {
                trs.push(Some(GenericTrait::new(tr_id, TypeArgs::default())));
            }
        };

        let ty = &proj.types[id];
        add_if(ty.is_numeric(), LangType::Numeric);

        if let Some(value) = ty.as_integral(false) {
            add_if(value.signed, LangType::Signed);
            add_if(!value.signed, LangType::Unsigned);
            add_if(true, LangType::Integral);
        } else if let Type::Array(id, _) = ty
            && let Some(&tr_id) = scopes.lang_types.get(&LangType::Array)
        {
            trs.push(Some(GenericTrait::from_type_args(scopes, tr_id, [*id])));
        } else if let Some(&tr_id) = scopes.lang_types.get(&LangType::FnPtr) {
            match ty {
                Type::Fn(f) => 'out: {
                    let data = scopes.get(f.id);
                    if data.is_extern || data.is_unsafe {
                        break 'out;
                    }

                    let ret = data.ret.with_templates(&self.proj().types, &f.ty_args);
                    let args = data
                        .params
                        .iter()
                        .map(|p| p.ty.with_templates(&proj.types, &f.ty_args))
                        .collect();
                    let args = self.get_tuple(args);
                    trs.push(Some(GenericTrait::from_type_args(
                        &self.proj().scopes,
                        tr_id,
                        [args, ret],
                    )));
                }
                Type::FnPtr(f) if !f.is_extern && !f.is_unsafe => {
                    let ret = f.ret;
                    let args = self.get_tuple(f.params.clone());
                    trs.push(Some(GenericTrait::from_type_args(
                        &self.proj().scopes,
                        tr_id,
                        [args, ret],
                    )));
                }
                _ => {}
            }
        }

        TraitImpls::Checked(trs)
    }

    fn find_trait_impl(
        &mut self,
        in_scope: &[ExtensionId],
        wanted_tr: TraitId,
        ty: TypeId,
    ) -> Option<GenericTrait> {
        let impls = self.get_trait_impls(ty);
        if let Some(tr) = impls.into_iter_checked().find(|tr| tr.id == wanted_tr) {
            if let Some(ut) = self.proj().types[ty].as_user() {
                return Some(tr.with_templates(&self.proj().types, &ut.ty_args));
            }
            return Some(tr);
        }

        // TODO: find multiple?
        for ext in self.do_extensions_in_scope_for(in_scope, ty) {
            self.do_resolve_impls(ext.id);
            let Some(tr) =
                self.proj().scopes.get(ext.id).impls.iter_checked().find(|tr| tr.id == wanted_tr)
            else {
                continue;
            };

            return Some(tr.with_templates(&self.proj().types, &ext.ty_args));
        }

        None
    }
}

impl TypeChecker<'_> {
    #[rustfmt::skip]
    fn get_binary_op(&mut self, op: BinaryOp) -> Option<(LangType, StrId)> {
        let strings = &mut self.proj.strings;
        match op {
            BinaryOp::Cmp => Some((LangType::OpCmp, strings.get_or_intern_static("cmp"))),
            BinaryOp::Gt => Some((LangType::OpCmp, strings.get_or_intern_static("gt"))),
            BinaryOp::GtEqual => Some((LangType::OpCmp, strings.get_or_intern_static("ge"))),
            BinaryOp::Lt => Some((LangType::OpCmp, strings.get_or_intern_static("lt"))),
            BinaryOp::LtEqual => Some((LangType::OpCmp, strings.get_or_intern_static("le"))),
            BinaryOp::Equal => Some((LangType::OpEq, strings.get_or_intern_static("eq"))),
            BinaryOp::NotEqual => Some((LangType::OpEq, strings.get_or_intern_static("ne"))),
            BinaryOp::Add => Some((LangType::OpAdd, strings.get_or_intern_static("add"))),
            BinaryOp::Sub => Some((LangType::OpSub, strings.get_or_intern_static("sub"))),
            BinaryOp::Mul => Some((LangType::OpMul, strings.get_or_intern_static("mul"))),
            BinaryOp::Div => Some((LangType::OpDiv, strings.get_or_intern_static("div"))),
            BinaryOp::Rem => Some((LangType::OpRem, strings.get_or_intern_static("rem"))),
            BinaryOp::BitAnd => Some((LangType::OpAnd, strings.get_or_intern_static("bit_and"))),
            BinaryOp::BitOr => Some((LangType::OpOr, strings.get_or_intern_static("bit_or"))),
            BinaryOp::Xor => Some((LangType::OpXor, strings.get_or_intern_static("xor"))),
            BinaryOp::Shl => Some((LangType::OpShl, strings.get_or_intern_static("shl"))),
            BinaryOp::Shr => Some((LangType::OpShr, strings.get_or_intern_static("shr"))),
            BinaryOp::AddAssign => Some((LangType::OpAddAssign, strings.get_or_intern_static("add_assign"))),
            BinaryOp::SubAssign => Some((LangType::OpSubAssign, strings.get_or_intern_static("sub_assign"))),
            BinaryOp::MulAssign => Some((LangType::OpMulAssign, strings.get_or_intern_static("mul_assign"))),
            BinaryOp::DivAssign => Some((LangType::OpDivAssign, strings.get_or_intern_static("div_assign"))),
            BinaryOp::RemAssign => Some((LangType::OpRemAssign, strings.get_or_intern_static("rem_assign"))),
            BinaryOp::BitAndAssign => Some((LangType::OpAndAssign, strings.get_or_intern_static("and_assign"))),
            BinaryOp::BitOrAssign => Some((LangType::OpOrAssign, strings.get_or_intern_static("or_assign"))),
            BinaryOp::XorAssign => Some((LangType::OpXorAssign, strings.get_or_intern_static("xor_assign"))),
            BinaryOp::ShlAssign => Some((LangType::OpShlAssign, strings.get_or_intern_static("shl_assign"))),
            BinaryOp::ShrAssign => Some((LangType::OpShrAssign, strings.get_or_intern_static("shr_assign"))),
            BinaryOp::Call => Some((LangType::OpFn, strings.get_or_intern_static("invoke"))),
            _ => None,
        }
    }

    fn get_unary_op(&mut self, op: UnaryOp) -> Option<(LangType, StrId)> {
        let strings = &mut self.proj.strings;
        match op {
            UnaryOp::Neg => Some((LangType::OpNeg, strings.get_or_intern_static("neg"))),
            UnaryOp::Not => Some((LangType::OpNot, strings.get_or_intern_static("not"))),
            UnaryOp::Unwrap => Some((LangType::OpUnwrap, strings.get_or_intern_static("unwrap"))),
            UnaryOp::PostDecrement => Some((LangType::OpDec, strings.get_or_intern_static("dec"))),
            UnaryOp::PreDecrement => Some((LangType::OpDec, strings.get_or_intern_static("dec"))),
            UnaryOp::PostIncrement => Some((LangType::OpInc, strings.get_or_intern_static("inc"))),
            UnaryOp::PreIncrement => Some((LangType::OpInc, strings.get_or_intern_static("inc"))),
            _ => None,
        }
    }

    fn is_disabled_by_attrs(&mut self, attrs: &Attributes, check_attrs: bool) -> bool {
        use crate::package::Constraint;

        for attr in attrs.iter() {
            if attr.name.data.is_str_eq(Strings::ATTR_FEATURE) {
                let Some(prop) = attr.props.first() else {
                    self.proj.diag.report(Error::new(
                        format!(
                            "attribute '{}' must include a feature name",
                            self.proj.str(Strings::ATTR_FEATURE)
                        ),
                        attr.name.span,
                    ));
                    continue;
                };

                if !self.is_feature_enabled(prop) {
                    return true;
                }
            } else if attr.name.data.is_str_eq(Strings::ATTR_CFG) {
                let Some((prop, span)) = attr.props.first().and_then(|p| match p.name.data {
                    crate::ast::AttrName::Str(str) => Some((str, p.name.span)),
                    _ => None,
                }) else {
                    self.proj.diag.report(Error::new(
                        format!(
                            "attribute '{}' requires one string argument",
                            self.proj.str(Strings::ATTR_CFG)
                        ),
                        attr.name.span,
                    ));
                    continue;
                };

                if self.proj.str(prop) == "test" {
                    return !self.proj.conf.in_test_mode();
                }

                let constraint = match Constraint::parse(self.proj.str(prop)) {
                    Ok(constraint) => constraint,
                    Err(err) => {
                        self.proj
                            .diag
                            .report(Error::new(format!("invalid constraint: {err}"), span));
                        continue;
                    }
                };

                if !constraint.applies(&self.proj.conf.args) {
                    return true;
                }
            } else if check_attrs {
                self.proj.diag.report(Error::new(
                    match &attr.name.data {
                        &crate::ast::AttrName::Str(id) => {
                            format!("unknown statement attribute '{}'", self.proj.str(id))
                        }
                        crate::ast::AttrName::Int(name) => {
                            format!("unknown statement attribute '{name}'")
                        }
                    },
                    attr.name.span,
                ));
            }
        }

        false
    }

    fn is_feature_enabled(&mut self, attr: &Attribute) -> bool {
        let Some(str) = attr.name.data.as_str() else {
            self.proj.diag.report(Error::new("feature name must be a string", attr.name.span));
            return true;
        };

        match self.proj.str(*str) {
            "not" => {
                let Some(param) = attr.props.first() else {
                    self.proj.diag.report(Error::new("not requires one argument", attr.name.span));
                    return true;
                };

                !self.is_feature_enabled(param)
            }
            // "and" =>
            // "or" =>
            name => {
                let Some(enabled) = self.feature_set.get(name) else {
                    self.proj.diag.report(Error::new(
                        format!("checking undeclared feature '{name}'"),
                        attr.name.span,
                    ));
                    return true;
                };

                *enabled
            }
        }
    }
}

fn nowhere<T>(name: StrId, cons: impl FnOnce(Path) -> T) -> Located<T> {
    // uses default span so hover ignores it
    Located::nowhere(cons(Path::from(Located::nowhere(name))))
}

/// Returns `true` if `ty` is/contains a user type where any parameter is an unresolved
/// (T => TypeId::UNKNOWN) member of `ty_args`
fn shouldnt_infer_with(proj: &Project, ty_args: &TypeArgs, ty: TypeId) -> bool {
    let Some(ut) = proj.types[ty.strip_references(&proj.types)].as_user() else {
        return false;
    };

    if ty_args.get(&ut.id).is_some_and(|ty| ty == &TypeId::UNKNOWN) {
        return true;
    }

    ut.ty_args.values().any(|ty| shouldnt_infer_with(proj, ty_args, *ty))
}
