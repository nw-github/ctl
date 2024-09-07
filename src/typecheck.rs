use std::collections::{HashMap, HashSet};

use indexmap::{map::Entry, IndexMap};
use num_bigint::BigInt;
use num_traits::Num;
use std::sync::LazyLock;

use crate::{
    ast::{checked::*, declared::*, parsed::*, Attributes, BinaryOp, UnaryOp},
    error::{Diagnostics, Error},
    lexer::{Located, Span},
    project::{Dependencies, Project},
    sym::*,
    typeid::{
        CInt, FnPtr, GenericExtension, GenericFn, GenericTrait, GenericUserType, Type, TypeArgs,
        TypeId, Types, WithTypeArgs,
    },
    THIS_PARAM, THIS_TYPE,
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
            TraitImpl::Unchecked { scope, path } => {
                $self.enter_id_and_resolve(scope, |this| match this.resolve_type_path(&path) {
                    ResolvedType::UserType(ut) => {
                        if this.proj.scopes.get(ut.id).kind.is_trait() {
                            TraitImpl::Checked(ut)
                        } else {
                            let name = format!(
                                "type '{}'",
                                ut.name(&this.proj.scopes, &mut this.proj.types)
                            );
                            this.error(Error::expected_found(
                                "trait",
                                &name,
                                path.final_component_span(),
                            ))
                        }
                    }
                    ResolvedType::Builtin(ty) => {
                        let name = format!(
                            "type '{}'",
                            ty.name(&this.proj.scopes, &mut this.proj.types)
                        );
                        this.error(Error::expected_found(
                            "trait",
                            &name,
                            path.final_component_span(),
                        ))
                    }
                    ResolvedType::Error => Default::default(),
                })
            }
            TraitImpl::Known {
                tr,
                ty_args,
                scope,
                span,
            } => {
                let Some(tr_id) = $self.proj.scopes.lang_traits.get(tr).copied() else {
                    return $self.error(Error::no_lang_item(tr, span));
                };

                $self.enter_id_and_resolve(scope, |this| {
                    TraitImpl::Checked(GenericTrait::new(
                        tr_id,
                        this.resolve_type_args(tr_id, &ty_args, true, span),
                    ))
                })
            }
            other => other,
        };
    }};
}

macro_rules! check_hover {
    ($self: expr, $span: expr, $item: expr) => {{
        if LspInput::matches($self.lsp_input.hover, $span) {
            $self.proj.hover = Some($item);
        }
    }};
}

macro_rules! bail {
    ($self: expr, $err: expr) => {{
        $self.proj.diag.error($err);
        return Default::default();
    }};
}

#[derive(Debug, Clone)]
pub struct MemberFn {
    pub func: GenericFn,
    pub owner: ScopeId,
    pub dynamic: bool,
    pub public: bool,
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
    NotFound(Error),
    #[default]
    Error,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum Safety {
    #[default]
    Safe,
    Unsafe,
}

#[derive(Clone, derive_more::From)]
pub enum LspItem {
    Type(UserTypeId),
    Module(ScopeId),
    Literal(CheckedExpr),
    Fn(FunctionId, Option<UserTypeId>),
    Var(VariableId),
    Attribute(String),
    Property(UserTypeId, String),
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
    pub hover: Option<Span>,
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

struct PatternParams {
    binding: bool,
    scrutinee: TypeId,
    mutable: bool,
    pattern: Located<Pattern>,
    typ: PatternType,
    has_hint: bool,
}

pub struct TypeChecker {
    universal: Vec<ScopeId>,
    safety: Safety,
    current: ScopeId,
    lsp_input: LspInput,
    proj: Project,
    listening_vars: Vec<VariableId>,
    listening_expr: usize,
    current_expr: usize,
}

impl TypeChecker {
    pub fn check(project: Vec<Stmt>, diag: Diagnostics, lsp: LspInput) -> Project {
        let mut this = Self {
            universal: Vec::new(),
            safety: Safety::Safe,
            current: ScopeId::ROOT,
            lsp_input: lsp,
            proj: Project::new(diag),
            listening_vars: Vec::new(),
            listening_expr: 0,
            current_expr: 0,
        };

        for module in project {
            let mut autouse = vec![];
            let stmt = this.declare_stmt(&mut autouse, module);
            this.proj.scope = *stmt.as_module().unwrap().0;
            this.check_stmt(stmt);
            this.universal.extend(autouse);
        }

        this.proj.main = this.proj.scopes[this.proj.scope]
            .vns
            .get("main")
            .and_then(|id| id.as_fn())
            .copied();
        for (_, var) in
            this.proj.scopes.vars().filter(|(_, v)| {
                v.unused && !v.name.data.starts_with('_') && v.name.data != THIS_PARAM
            })
        {
            if this
                .proj
                .scopes
                .walk(var.scope)
                .any(|(id, _)| id == this.proj.scope)
            {
                this.proj.diag.warn(Error::new(
                    format!("unused variable: '{}'", var.name.data),
                    var.name.span,
                ));
            }
        }

        this.proj
    }

    pub fn with_project<T>(proj: &mut Project, f: impl FnOnce(&mut TypeChecker) -> T) -> T {
        let mut tc = Self {
            universal: Vec::new(),
            safety: Safety::Safe,
            current: ScopeId::ROOT,
            lsp_input: Default::default(),
            proj: std::mem::take(proj),
            listening_vars: Vec::new(),
            listening_expr: 0,
            current_expr: 0,
        };
        let res = f(&mut tc);
        std::mem::swap(proj, &mut tc.proj);
        res
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.proj.diag.error(error);
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
            self.error(Error::redefinition(&name.data, name.span))
        }
        res.id
    }

    fn can_access_privates(&self, scope: ScopeId) -> bool {
        self.proj
            .scopes
            .module_of(scope)
            .map(|target| {
                self.proj
                    .scopes
                    .walk(self.current)
                    .any(|(id, _)| id == target)
            })
            .unwrap_or_default()
    }

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
        let mut add_methods = |scopes: &Scopes,
                               c: &mut Vec<LspItem>,
                               fns: &[Vis<FunctionId>],
                               cap: bool,
                               m: UserTypeId| {
            for func in fns {
                let f = scopes.get(func.id);
                if added.contains(&f.name.data) {
                    continue;
                }

                if (f.public || cap)
                    && (!method || f.params.first().is_some_and(|p| p.label == THIS_PARAM))
                {
                    c.push(LspItem::Fn(func.id, Some(m)));
                    added.insert(f.name.data.clone());
                }
            }
        };

        if let Some(ut_id) = self.proj.types[ty].as_user().map(|ut| ut.id) {
            self.resolve_impls_recursive(ut_id);

            let data = self.proj.scopes.get(ut_id);
            let cap = self.can_access_privates(data.scope);
            if method {
                for (name, _) in data.members.iter().filter(|(_, m)| m.public || cap) {
                    completions.push(LspItem::Property(ut_id, name.clone()))
                }
            }

            for tr in self
                .proj
                .scopes
                .get(ut_id)
                .impls
                .iter()
                .flat_map(|ut| ut.as_checked())
            {
                for tr in self.proj.scopes.get_trait_impls(tr.id) {
                    let data = self.proj.scopes.get(tr);
                    add_methods(&self.proj.scopes, &mut completions, &data.fns, cap, tr);
                }
            }

            add_methods(&self.proj.scopes, &mut completions, &data.fns, cap, ut_id);
        } else if let Some(tr) = self.proj.types[ty].as_dyn_pointee() {
            let tr_id = tr.id;
            self.resolve_impls_recursive(tr_id);
            for tr in self.proj.scopes.get_trait_impls(tr_id) {
                add_methods(
                    &self.proj.scopes,
                    &mut completions,
                    &self.proj.scopes.get(tr).fns,
                    true,
                    tr,
                );
            }
        }

        let extensions = self.extensions_in_scope_for(ty, self.current);
        for ext in extensions.iter() {
            self.resolve_impls_recursive(ext.id);
            for imp in self
                .proj
                .scopes
                .get(ext.id)
                .impls
                .iter()
                .flat_map(|imp| imp.as_checked())
            {
                for tr in self.proj.scopes.get_trait_impls(imp.id) {
                    add_methods(
                        &self.proj.scopes,
                        &mut completions,
                        &self.proj.scopes.get(tr).fns,
                        true,
                        tr,
                    );
                }
            }
        }

        for ext in extensions.iter() {
            let data = self.proj.scopes.get(ext.id);
            add_methods(
                &self.proj.scopes,
                &mut completions,
                &data.fns,
                self.can_access_privates(data.scope),
                ext.id,
            );
        }

        self.proj.completions = Some(Completions {
            items: completions,
            method,
        });
    }

    fn check_cursor_completions(&mut self, span: Span, ty: bool) {
        if self.proj.completions.is_some() || !LspInput::matches(self.lsp_input.completion, span) {
            return;
        }

        let mut completions = vec![];
        let mut emitted_vars = HashSet::new();
        for (_, scope) in self.proj.scopes.walk(self.current) {
            if !ty {
                for (_, item) in scope.vns.iter() {
                    match item.id {
                        ValueItem::Fn(id) => {
                            if matches!(scope.kind, ScopeKind::UserType(_)) {
                                continue;
                            }
                            completions.push(LspItem::Fn(id, None))
                        }
                        ValueItem::Var(id) => {
                            let var = self.proj.scopes.get(id);
                            if !var.is_static
                                && self.current_function()
                                    != self.proj.scopes.function_of(var.scope)
                            {
                                continue;
                            }
                            if emitted_vars.contains(&var.name.data) {
                                continue;
                            }
                            completions.push(LspItem::Var(id));
                            emitted_vars.insert(var.name.data.clone());
                        }
                        ValueItem::StructConstructor(_, id) => {
                            completions.push(LspItem::Fn(id, None))
                        }
                        ValueItem::UnionConstructor(_) => {}
                    }
                }
            }

            for (_, item) in scope.tns.iter() {
                if item
                    .as_type()
                    .is_some_and(|&id| self.proj.scopes.get(id).name.data.starts_with('$'))
                {
                    continue;
                }

                completions.push(item.id.into());
            }

            if scope.kind.is_module() {
                break;
            }
        }

        #[rustfmt::skip]
        let builtins = [
            "void", "never", "f32", "f64", "bool", "char", "c_void", "c_char",
            "c_short", "c_int", "c_long", "c_longlong", "c_uchar", "c_ushort", "c_uint",
            "c_ulong", "c_ulonglong", "int", "uint", "u8", "i8", "u16", "i16", "u32", "i32", "u64",
            "i64", "u128", "i128",
        ];
        completions.extend(builtins.into_iter().map(LspItem::BuiltinType));

        self.proj.completions = Some(Completions {
            items: completions,
            method: false,
        });
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
                if ut.name.data.starts_with('$') || ut.kind.is_template() {
                    continue;
                }
            }

            completions.push(item.id.into());
        }

        self.proj.completions = Some(Completions {
            items: completions,
            method: false,
        });
    }

    #[inline(always)]
    pub(crate) fn scopes(&self) -> &Scopes {
        &self.proj.scopes
    }
}

/// Forward declaration pass routines
impl TypeChecker {
    fn insert_user_type(&mut self, value: UserType, public: bool) -> UserTypeId {
        let id = self.insert::<UserTypeId>(value, public, true);
        if let Some(name) = self.proj.scopes.get(id).attrs.val("lang") {
            self.proj.scopes.lang_types.insert(name.into(), id);
        }
        for (name, m) in self.proj.scopes.get(id).members.iter() {
            check_hover!(self, m.span, LspItem::Property(id, name.clone()));
        }
        id
    }

    fn declare_struct(&mut self, base: Struct, attrs: Attributes, packed: bool) -> DeclaredStmt {
        let name = base.name.clone();
        let (ut, init, fns, impls) = self.enter(ScopeKind::None, |this| {
            let init = this.enter(ScopeKind::None, |this| {
                this.declare_fn(Fn {
                    public: base.public && !base.members.iter().any(|m| !m.public),
                    name: base.name.clone(),
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
                            patt: Located::new(
                                Span::default(), // use default span so hovers ignore this
                                Pattern::Path(Path::from(member.name.clone())),
                            ),
                            ty: member.ty.clone(),
                            default: member.default.clone(),
                        })
                        .collect(),
                    ret: Self::typehint_for_struct(&base.name, &base.type_params),
                    body: None,
                    attrs: Default::default(),
                    assign_subscript: false,
                })
            });
            let mut members = IndexMap::with_capacity(base.members.len());
            for member in base.members {
                let prev = members.insert(
                    member.name.data.clone(),
                    CheckedMember::new(
                        member.public,
                        this.declare_type_hint(member.ty),
                        member.name.span,
                    ),
                );
                if prev.is_some() {
                    this.error(Error::redefinition_k(
                        "member variable",
                        &member.name.data,
                        member.name.span,
                    ))
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
            name.data,
            Vis::new(ValueItem::StructConstructor(id, init.id), base.public),
        );
        if prev.is_some() {
            self.error(Error::redefinition(
                &self.proj.scopes.get(id).name.data,
                name.span,
            ))
        }

        self.proj.scopes[scope].kind = ScopeKind::UserType(id);
        self.proj.scopes.get_mut(init.id).constructor = Some(id);

        DeclaredStmt::Struct {
            init,
            id,
            impls,
            fns,
        }
    }

    fn declare_union(
        &mut self,
        tag: Option<Path>,
        base: Struct,
        variants: Vec<Variant>,
        attrs: Attributes,
    ) -> DeclaredStmt {
        let (ut, impls, fns, member_cons_len) = self.enter(ScopeKind::None, |this| {
            let mut rvariants = IndexMap::with_capacity(base.members.len());
            let mut members = IndexMap::with_capacity(base.members.len());
            let mut params = Vec::with_capacity(base.members.len());
            let mut fns = Vec::with_capacity(base.members.len());
            for member in base.members {
                if members
                    .insert(
                        member.name.data.clone(),
                        CheckedMember::new(
                            member.public,
                            this.declare_type_hint(member.ty.clone()),
                            member.name.span,
                        ),
                    )
                    .is_some()
                {
                    this.error(Error::redefinition_k(
                        "member",
                        &member.name.data,
                        member.name.span,
                    ))
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
                            variant.name.data.clone(),
                            CheckedVariant {
                                ty: None,
                                span: variant.name.span,
                            },
                        );
                    }
                    VariantData::StructLike(smembers) => {
                        enum_union = false;
                        rvariants.insert(
                            variant.name.data.clone(),
                            CheckedVariant {
                                ty: Some(
                                    this.declare_type_hint(TypeHint::AnonStruct(
                                        smembers
                                            .iter()
                                            .map(|m| (m.name.data.clone(), m.ty.clone()))
                                            .collect(),
                                    )),
                                ),
                                span: variant.name.span,
                            },
                        );

                        for member in smembers {
                            if members.contains_key(&member.name.data) {
                                this.error(Error::shared_member(
                                    &member.name.data,
                                    member.name.span,
                                ))
                            }

                            params.push(Param {
                                keyword: true,
                                patt: Located::new(
                                    Span::default(), // use default span so hovers ignore this
                                    Pattern::Path(Path::from(member.name)),
                                ),
                                ty: member.ty,
                                default: member.default,
                            });
                        }
                    }
                    VariantData::TupleLike(members) => {
                        enum_union = false;
                        rvariants.insert(
                            variant.name.data.clone(),
                            CheckedVariant {
                                ty: Some(this.declare_type_hint(TypeHint::Tuple(
                                    members.iter().map(|(ty, _)| ty.clone()).collect(),
                                ))),
                                span: variant.name.span,
                            },
                        );

                        for (i, (ty, default)) in members.into_iter().enumerate() {
                            params.push(Param {
                                keyword: false,
                                patt: Located::new(
                                    Span::default(),
                                    Pattern::Path(Path::from(Located::new(
                                        Span::default(),
                                        format!("{i}"),
                                    ))),
                                ),
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
                    ret: ret.clone(),
                    body: None,
                    attrs: Default::default(),
                    assign_subscript: false,
                }));
            }
            let member_cons_len = fns.len();
            fns.extend(base.functions.into_iter().map(|f| this.declare_fn(f)));
            let tag = if let Some(tag) = tag {
                this.declare_type_hint(TypeHint::Regular(tag))
            } else {
                this.proj
                    .types
                    .insert(Type::Uint(discriminant_bits(rvariants.len())))
            };
            let ut = this.ut_from_stuff(
                attrs,
                base.name,
                base.public,
                members,
                UserTypeKind::Union(Union {
                    tag,
                    variants: rvariants,
                    enum_union,
                }),
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

        DeclaredStmt::Union { id, impls, fns }
    }

    fn declare_unsafe_union(&mut self, mut base: Struct, attrs: Attributes) -> DeclaredStmt {
        let name = base.name.clone();
        let (ut, fns, impls) = self.enter(ScopeKind::None, |this| {
            let mut members = IndexMap::with_capacity(base.members.len());
            for member in base.members.iter_mut() {
                let prev = members.insert(
                    member.name.data.clone(),
                    CheckedMember::new(
                        true,
                        this.declare_type_hint(std::mem::take(&mut member.ty)),
                        member.name.span,
                    ),
                );
                if prev.is_some() {
                    this.error(Error::redefinition_k(
                        "member variable",
                        &member.name.data,
                        member.name.span,
                    ))
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
        let prev = self.proj.scopes[self.current].vns.insert(
            name.data,
            Vis::new(ValueItem::UnionConstructor(id), base.public),
        );
        if prev.is_some() {
            self.error(Error::redefinition(
                &self.proj.scopes.get(id).name.data,
                name.span,
            ))
        }
        self.proj.scopes[scope].kind = ScopeKind::UserType(id);

        DeclaredStmt::Union { id, impls, fns }
    }

    fn declare_stmt(&mut self, autouse: &mut Vec<ScopeId>, stmt: Stmt) -> DeclaredStmt {
        match stmt.data {
            StmtData::Module {
                public,
                name,
                body,
                file,
            } => {
                if file && name.data == "main" {
                    return DeclaredStmt::Module {
                        id: self.current,
                        body: body
                            .into_iter()
                            .map(|stmt| self.declare_stmt(autouse, stmt))
                            .collect(),
                    };
                }
                let parent = self.current;
                self.enter(ScopeKind::Module(name.clone()), |this| {
                    this.check_hover(name.span, this.current.into());
                    if this.proj.scopes[parent]
                        .tns
                        .insert(name.data.clone(), Vis::new(this.current.into(), public))
                        .is_some()
                    {
                        this.error(Error::redefinition(&name.data, name.span))
                    }

                    let core = this.proj.scopes[ScopeId::ROOT]
                        .find_in_tns("core")
                        .and_then(|inner| inner.as_module().copied());
                    let std = this.proj.scopes[ScopeId::ROOT]
                        .find_in_tns("std")
                        .and_then(|inner| inner.as_module().copied());
                    if stmt.attrs.iter().any(|attr| attr.name.data == "autouse") {
                        if this
                            .proj
                            .scopes
                            .walk(this.current)
                            .any(|(id, _)| Some(id) == core || Some(id) == std)
                        {
                            autouse.push(this.current);
                        } else {
                            this.error(Error::new(
                                "autouse modules may only be defined by 'core' and 'std'",
                                name.span,
                            ))
                        }
                    }

                    DeclaredStmt::Module {
                        id: this.current,
                        body: body
                            .into_iter()
                            .map(|stmt| this.declare_stmt(autouse, stmt))
                            .collect(),
                    }
                })
            }
            StmtData::Struct { base, packed } => self.declare_struct(base, stmt.attrs, packed),
            StmtData::Union {
                tag,
                base,
                variants,
            } => self.declare_union(tag, base, variants, stmt.attrs),
            StmtData::UnsafeUnion(base) => self.declare_unsafe_union(base, stmt.attrs),
            StmtData::Trait {
                public,
                name,
                type_params,
                impls,
                functions,
                is_unsafe: _,
            } => {
                let lang_item = stmt.attrs.val("lang").map(String::from);
                let (tr, fns, this_id) = self.enter(ScopeKind::None, |this| {
                    let impls: Vec<_> = impls
                        .into_iter()
                        .map(|path| TraitImpl::Unchecked {
                            scope: this.current,
                            path,
                        })
                        .collect();
                    let this_id = this.insert(
                        UserType::template(
                            Located::new(name.span, THIS_TYPE.into()),
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
                        UserTypeKind::Trait(this_id),
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
                    self.proj.scopes.lang_traits.insert(name, id);
                }
                let imp =
                    GenericTrait::from_type_params(&self.proj.scopes, &mut self.proj.types, id);
                self.proj
                    .scopes
                    .get_mut(this_id)
                    .impls
                    .push(TraitImpl::Checked(imp));
                self.proj.scopes[scope].kind = ScopeKind::UserType(id);
                DeclaredStmt::Trait { id, fns }
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
                DeclaredStmt::Extension {
                    id,
                    impls: impl_blocks,
                    fns,
                }
            }
            StmtData::Fn(f) => DeclaredStmt::Fn(self.declare_fn(f)),
            StmtData::Binding {
                public,
                constant,
                name,
                ty,
                value,
            } => {
                let ty = self.declare_type_hint(ty);
                DeclaredStmt::Binding {
                    id: self.insert::<VariableId>(
                        Variable {
                            public,
                            name,
                            ty,
                            is_static: true,
                            mutable: false,
                            value: None,
                            unused: true,
                            has_hint: true,
                        },
                        public,
                        true,
                    ),
                    value,
                    constant,
                }
            }
            StmtData::Use(stmt) => {
                if self.resolve_use(&stmt).is_err() {
                    self.proj.scopes[self.current].use_stmts.push(stmt);
                }
                DeclaredStmt::None
            }
            StmtData::Let { ty, value, patt } => DeclaredStmt::Let { ty, value, patt },
            StmtData::Guard { cond, body } => DeclaredStmt::Guard { cond, body },
            StmtData::Expr(expr) => DeclaredStmt::Expr(expr),
            StmtData::Defer(expr) => DeclaredStmt::Defer(expr),
            StmtData::Error => DeclaredStmt::None,
        }
    }

    fn declare_fn(&mut self, f: Fn) -> DeclaredFn {
        let span = f.name.span;
        if f.variadic && (!f.is_extern || f.body.is_some()) {
            self.error(Error::new(
                "only imported extern functions may be variadic",
                span,
            ))
        }

        if f.is_extern && !f.type_params.is_empty() && f.body.is_some() {
            self.error(Error::new(
                "generic functions cannot be declared 'extern'",
                span,
            ))
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

        for attr in self.proj.scopes.get::<FunctionId>(id).attrs.clone().iter() {
            match &attr.name.data[..] {
                "lang" => {
                    let Some(inner) = attr.props.first() else {
                        self.proj
                            .diag
                            .error(Error::new("language item must have name", attr.name.span));
                        continue;
                    };
                    self.proj
                        .scopes
                        .lang_fns
                        .insert(inner.name.data.clone(), id);
                }
                "intrinsic" => {
                    let (name, span) = if let Some(attr) = attr.props.first() {
                        (&attr.name.data[..], attr.name.span)
                    } else {
                        (&self.proj.scopes.get(id).name.data[..], attr.name.span)
                    };
                    match name {
                        "size_of"
                        | "align_of"
                        | "panic"
                        | "binary_op"
                        | "unary_op"
                        | "numeric_cast"
                        | "numeric_abs"
                        | "numeric_lt"
                        | "max_value"
                        | "min_value"
                        | "raw_offset"
                        | "unreachable_unchecked" => {
                            self.proj.scopes.intrinsics.insert(id, name.to_string());
                        }
                        _ => self.error(Error::new(
                            format!("intrinsic '{name}' is not supported"),
                            span,
                        )),
                    }
                }
                _ => {}
            }
        }

        self.enter(ScopeKind::Function(id), |this| {
            this.proj.scopes.get_mut(id).body_scope = this.current;
            this.proj.scopes.get_mut(id).type_params = this.declare_type_params(f.type_params);
            this.proj.scopes.get_mut(id).params = f
                .params
                .into_iter()
                .enumerate()
                .map(|(i, param)| CheckedParam {
                    keyword: param.keyword,
                    label: match &param.patt.data {
                        Pattern::MutBinding(name) => Some(name.clone()),
                        Pattern::Path(name) => name.as_identifier().map(|name| name.into()),
                        _ => None,
                    }
                    .unwrap_or_else(|| format!("$unnamed{i}")),
                    patt: ParamPattern::Unchecked(param.patt),
                    ty: this.declare_type_hint(param.ty),
                    default: param
                        .default
                        .map(|expr| DefaultExpr::Unchecked(this.current, expr)),
                })
                .collect();
            this.proj.scopes.get_mut(id).ret = this.declare_type_hint(f.ret);

            DeclaredFn { id, body: f.body }
        })
    }

    fn declare_fns(&mut self, fns: Vec<Fn>) -> Vec<DeclaredFn> {
        fns.into_iter().map(|f| self.declare_fn(f)).collect()
    }

    fn declare_op_fn(
        &mut self,
        f: OperatorFn,
        impls: &mut Vec<TraitImpl>,
        blocks: &mut Vec<DeclaredImplBlock>,
        subscripts: &mut Vec<DeclaredFn>,
    ) {
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
                let (tr_name, fn_name) = BINARY_OP_TRAITS.get(&op).unwrap();
                if let Some(p) = f
                    .params
                    .get(1)
                    .filter(|_| matches!(f.name.data, O::Cmp | O::Eq))
                    .cloned()
                {
                    if let TypeHint::Ptr(inner) = p.ty {
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
                            f.ret.clone(),
                        ],
                    )
                }
            }
            O::Minus if f.params.len() > 1 => {
                let op = BinaryOp::try_from(f.name.data).unwrap();
                let (tr_name, fn_name) = BINARY_OP_TRAITS.get(&op).unwrap();
                (
                    tr_name,
                    fn_name,
                    vec![
                        f.params.get(1).map(|p| p.ty.clone()).unwrap_or_default(),
                        f.ret.clone(),
                    ],
                )
            }
            O::Minus | O::Bang => {
                let op = UnaryOp::try_from_postfix_fn(f.name.data).unwrap();
                let (tr_name, fn_name) = UNARY_OP_TRAITS.get(&op).unwrap();
                (tr_name, fn_name, vec![f.ret.clone()])
            }
            O::Increment | O::Decrement => {
                let op = UnaryOp::try_from_postfix_fn(f.name.data).unwrap();
                let (tr_name, fn_name) = UNARY_OP_TRAITS.get(&op).unwrap();
                (tr_name, fn_name, vec![])
            }
            O::Subscript | O::SubscriptAssign => {
                subscripts.push(
                    self.declare_fn(Fn::from_operator_fn(format!("$sub{}", subscripts.len()), f)),
                );
                return;
            }
        };

        let span = f.name.span;
        let f = Fn::from_operator_fn(fn_name.to_string(), f);
        let block = self.enter(ScopeKind::None, |this| DeclaredImplBlock {
            span: f.name.span,
            scope: this.current,
            fns: vec![this.declare_fn(f)],
        });
        let unchecked = TraitImpl::Known {
            tr: tr_name,
            ty_args,
            scope: self.current,
            span,
        };
        self.proj.scopes[block.scope].kind = ScopeKind::Impl(unchecked.clone());
        impls.push(unchecked);
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
                                path,
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
        blocks: Vec<ImplBlock>,
        operators: Vec<OperatorFn>,
    ) -> (Vec<TraitImpl>, Vec<DeclaredImplBlock>, Vec<DeclaredFn>) {
        let mut impls = Vec::new();
        let mut declared_blocks = Vec::new();
        let mut subscripts = Vec::new();
        for ImplBlock {
            path, functions, ..
        } in blocks
        {
            let block = self.enter(ScopeKind::None, |this| DeclaredImplBlock {
                span: path.final_component_span(),
                scope: this.current,
                fns: functions.into_iter().map(|f| this.declare_fn(f)).collect(),
            });
            let unchecked = TraitImpl::Unchecked {
                scope: self.current,
                path,
            };
            self.proj.scopes[block.scope].kind = ScopeKind::Impl(unchecked.clone());
            impls.push(unchecked);
            declared_blocks.push(block);
        }

        for func in operators {
            self.declare_op_fn(func, &mut impls, &mut declared_blocks, &mut subscripts);
        }

        (impls, declared_blocks, subscripts)
    }

    fn declare_type_hint(&mut self, hint: TypeHint) -> TypeId {
        self.proj.types.add_unresolved(hint, self.current)
    }

    fn typehint_for_struct(
        name: &Located<String>,
        type_params: &[(Located<String>, Vec<Path>)],
    ) -> TypeHint {
        TypeHint::Regular(Path::new(
            PathOrigin::Normal,
            vec![(
                name.clone(),
                type_params
                    .iter()
                    .map(|(n, _)| TypeHint::Regular(Path::from(n.clone())))
                    .collect(),
            )],
        ))
    }

    #[allow(clippy::too_many_arguments)]
    fn ut_from_stuff(
        &mut self,
        attrs: Attributes,
        name: Located<String>,
        public: bool,
        members: IndexMap<String, CheckedMember>,
        kind: UserTypeKind,
        type_params: TypeParams,
        fns: &[DeclaredFn],
        impls: Vec<TraitImpl>,
        impl_blocks: &[DeclaredImplBlock],
        subscripts: &[DeclaredFn],
    ) -> UserType {
        UserType {
            attrs,
            name,
            public,
            kind,
            impls,
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
            for id in this
                .proj
                .scopes
                .walk(this.current)
                .map(|(id, _)| id)
                .collect::<Vec<_>>()
            {
                this.enter_id(id, |this| {
                    if this.proj.scopes[this.current].kind.is_module() {
                        for scope in this.universal.clone() {
                            this.use_all(scope, false);
                        }
                    }

                    for stmt in std::mem::take(&mut this.proj.scopes[this.current].use_stmts) {
                        if let Err(err) = this.resolve_use(&stmt) {
                            this.error(err)
                        }
                    }
                });
            }

            f(this)
        })
    }

    fn check_stmt(&mut self, stmt: DeclaredStmt) -> CheckedStmt {
        match stmt {
            DeclaredStmt::Module { id, body } => {
                self.enter_id_and_resolve(id, |this| {
                    for stmt in body {
                        this.check_stmt(stmt);
                    }
                });
            }
            DeclaredStmt::Struct {
                init,
                id,
                impls,
                fns,
            } => {
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
            DeclaredStmt::Union { id, impls, fns } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.resolve_members(id);
                    let this_ty = Type::User(GenericUserType::from_id(&this.proj.scopes, &mut this.proj.types, id));
                    let this_ty = this.proj.types.insert(this_ty);
                    this.resolve_dependencies(id, this_ty, true);
                    this.check_impl_blocks(this_ty, id, impls);

                    for f in fns {
                        this.check_fn(f);
                    }

                    let Some(union) = &this.proj.scopes.get(id).kind.as_union() else {
                        return;
                    };
                    if let Some(stats) = union.tag.as_integral(&this.proj.types) {
                        if stats.bits < discriminant_bits(union.variants.len()) {
                            let msg = format!(
                                "type '{}' does not have sufficient range to represent the tag for this type", 
                                union.tag.name(&this.proj.scopes, &mut this.proj.types)
                            );
                            this.error(Error::new(
                                msg,
                                this.proj.scopes.get(id).name.span,
                            ))
                        }
                    } else if union.tag != TypeId::UNKNOWN {
                        this.error(Error::new(
                            "union tag must be an integer type",
                            this.proj.scopes.get(id).name.span,
                        ))
                    }
                });
            }
            DeclaredStmt::Trait { id, fns } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(*this.proj.scopes.get(id).kind.as_trait().unwrap());
                    // TODO: disable errors so this doesn't cause duplicate errors
                    this.resolve_impls(id);
                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Extension { id, impls, fns } => {
                self.enter_id(self.proj.scopes.get(id).body_scope, |this| {
                    let ty = resolve_type!(
                        this,
                        *this
                            .proj
                            .scopes
                            .get_mut(id)
                            .kind
                            .as_extension_mut()
                            .unwrap()
                    );
                    this.resolve_impls(id);
                    this.check_impl_blocks(ty, id, impls);
                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Expr(expr) => return CheckedStmt::Expr(self.check_expr(expr, None)),
            DeclaredStmt::Let { ty, value, patt } => {
                let span = patt.span;
                if let Some(ty) = ty {
                    let ty = self.resolve_typehint(&ty);
                    if let Some(value) = value {
                        let value = self.type_check(value, ty);
                        let patt = self.check_pattern(PatternParams {
                            binding: true,
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
                        return CheckedStmt::Let(patt, Some(value));
                    } else {
                        let patt = self.check_pattern(PatternParams {
                            binding: true,
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
                        if !matches!(patt.data, CheckedPatternData::Variable(_)) {
                            return self.error(Error::new(
                                "must provide a value with a destructuring assignment",
                                span,
                            ));
                        }
                        return CheckedStmt::Let(patt, None);
                    }
                } else if let Some(value) = value {
                    let span = patt.span;
                    let value = self.check_expr(value, None);
                    let patt = self.check_pattern(PatternParams {
                        binding: true,
                        scrutinee: value.ty,
                        mutable: false,
                        pattern: patt,
                        typ: PatternType::Regular,
                        has_hint: false,
                    });
                    if !patt.irrefutable {
                        return self.error(Error::must_be_irrefutable("let binding pattern", span));
                    }

                    return CheckedStmt::Let(patt, Some(value));
                } else {
                    return self.error(Error::new("cannot infer type", patt.span));
                }
            }
            DeclaredStmt::Defer(expr) => {
                return CheckedStmt::Defer(
                    self.enter(ScopeKind::Defer, |this| this.check_expr(expr, None)),
                );
            }
            DeclaredStmt::Guard { cond, body } => {
                let (cond, vars) = self.type_check_with_listen(cond);
                let span = body.span;
                let body = self.check_expr_inner(body, Some(TypeId::NEVER));
                let body = self.type_check_checked(body, TypeId::NEVER, span);
                self.define(&vars);
                return CheckedStmt::Guard { cond, body };
            }
            DeclaredStmt::Fn(f) => self.check_fn(f),
            DeclaredStmt::Binding {
                id,
                value,
                constant,
            } => {
                // FIXME: detect cycles like static X: usize = X;
                // FIXME: non-const statics should be disallowed
                let ty = resolve_type!(self, self.proj.scopes.get_mut(id).ty);
                let value = self.type_check(value, ty);
                let var = self.proj.scopes.get_mut(id);
                var.value = Some(value);
            }
            DeclaredStmt::None => {}
        }

        CheckedStmt::None
    }

    fn signatures_match(
        scopes: &Scopes,
        types: &mut Types,
        tr: TraitId,
        this: TypeId,
        has: FunctionId,
        wants: FunctionId,
        ty_args: &TypeArgs,
    ) -> Result<(), String> {
        let hfn = scopes.get(has);
        let wfn = scopes.get(wants);
        if wfn.is_unsafe && !hfn.is_unsafe {
            return Err(format!(
                "function '{}' must be declared unsafe",
                hfn.name.data
            ));
        }

        let mut ty_args = ty_args.clone();
        for (i, &id) in wfn.type_params.iter().enumerate() {
            if let Some(&ty) = hfn.type_params.get(i) {
                let ty = Type::User(GenericUserType::from_id(scopes, types, ty));
                ty_args.insert(id, types.insert(ty));
            } else {
                ty_args.insert(id, TypeId::UNKNOWN);
            }
        }
        ty_args.insert(*scopes.get(tr).kind.as_trait().unwrap(), this);

        let mut compare_types = |has: TypeId, wants: TypeId| {
            let wants = wants.with_templates(types, &ty_args);
            if has != wants {
                Err(format!(
                    "expected '{}', found '{}'",
                    wants.name(scopes, types),
                    has.name(scopes, types),
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
                return Err(format!("parameter '{}' is incorrect: {err}", t.label));
            }
        }

        for (&s, &t) in hfn.type_params.iter().zip(wfn.type_params.iter()) {
            let s = scopes.get(s);
            let t = scopes.get(t);
            let name = &t.name;
            // TODO: dont enfore impl order
            for (s, t) in s
                .impls
                .iter()
                .flat_map(|tr| tr.as_checked())
                .zip(t.impls.iter().flat_map(|tr| tr.as_checked()))
            {
                for (&s, &t) in s.ty_args.values().zip(t.ty_args.values()) {
                    if let Err(err) = compare_types(s, t) {
                        return Err(format!("type parameter '{name}' is incorrect: {err}"));
                    }
                }
            }

            if s.impls.len() != t.impls.len() {
                return Err(format!("type parameter '{name}' is incorrect"));
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

    fn check_impl_block(&mut self, this: TypeId, tr: &GenericTrait, block: DeclaredImplBlock) {
        let this_id = *self.proj.scopes.get(tr.id).kind.as_trait().unwrap();
        for mut dep in self
            .proj
            .scopes
            .get(tr.id)
            .impls
            .clone()
            .into_iter()
            .flat_map(|tr| tr.into_checked())
        {
            for ty_arg in dep.ty_args.values_mut() {
                if self.proj.types[*ty_arg]
                    .as_user()
                    .is_some_and(|ut| ut.id == this_id)
                {
                    *ty_arg = this;
                }
            }

            if !self.implements_trait(this, &dep) {
                self.proj.diag.error(Error::new(
                    format!(
                        "trait '{}' requires implementation of trait '{}'",
                        tr.name(&self.proj.scopes, &mut self.proj.types),
                        dep.name(&self.proj.scopes, &mut self.proj.types),
                    ),
                    block.span,
                ));
            }
        }

        let mut required = self.proj.scopes.get(tr.id).fns.clone();
        for f in block.fns {
            let Located {
                span: fn_span,
                data: fn_name,
            } = self.proj.scopes.get(f.id).name.clone();
            let lhs = f.id;

            self.check_fn(f);
            let Some(pos) = required
                .iter()
                .position(|&id| self.proj.scopes.get(*id).name.data == fn_name)
            else {
                self.proj.diag.error(Error::new(
                    format!(
                        "no function '{fn_name}' found in trait '{}'",
                        self.proj.scopes.get(tr.id).name
                    ),
                    fn_span,
                ));
                continue;
            };

            let rhs = *required.swap_remove(pos);
            self.resolve_proto(rhs);
            if let Err(err) = Self::signatures_match(
                &self.proj.scopes,
                &mut self.proj.types,
                tr.id,
                this,
                lhs,
                rhs,
                &tr.ty_args,
            ) {
                self.error(Error::new(
                    format!("invalid implementation of function '{fn_name}': {err}"),
                    fn_span,
                ))
            }
        }

        for id in required {
            if !self.proj.scopes.get(*id).has_body {
                let tr = tr.name(&self.proj.scopes, &mut self.proj.types);
                self.error(Error::new(
                    format!(
                        "must implement '{tr}::{}'",
                        self.proj.scopes.get(*id).name.data
                    ),
                    block.span,
                ))
            }
        }
    }

    fn check_fn(&mut self, DeclaredFn { id, body, .. }: DeclaredFn) {
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
                    binding: true,
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
            if let Some(ut_id) = func.constructor {
                let args = func
                    .params
                    .iter()
                    .map(|param| (param.label.clone(), CheckedExpr::new(
                        param.ty,
                        CheckedExprData::Var(*param.patt.as_checked().and_then(|p| p.data.as_variable()).unwrap())
                    )))
                    .collect();
                let variant = func.name.data.clone();
                let ut = Type::User(GenericUserType::from_id(&this.proj.scopes, &mut this.proj.types, ut_id));
                this.proj.scopes.get_mut(id).body = Some(CheckedExpr::new(
                    this.proj.types.insert(ut),
                    if this.proj.scopes.get(ut_id).kind.is_union() {
                        CheckedExprData::VariantInstance {
                            variant,
                            members: args,
                        }
                    } else {
                        CheckedExprData::Instance(args)
                    },
                ));
                return;
            }

            let Some(body) = body else {
                return;
            };

            let old_safety = std::mem::take(&mut this.safety);
            let target = this.proj.scopes.get_mut(id).ret;
            let body = this.check_expr(body, Some(target));
            let body = this.coerce(body, target);
            this.proj.scopes.get_mut(id).body = Some(match body {
                Ok(body) => body,
                Err(body) => {
                    if !body.data.is_yielding_block(&this.proj.scopes) && target != TypeId::VOID && target != TypeId::UNKNOWN {
                        let func = this.proj.scopes.get(id);
                        let err = Error::new(
                            format!("function '{}' must return a value of type '{}' from all code paths",
                                func.name.data,
                                func.ret.name(&this.proj.scopes, &mut this.proj.types),
                            ),
                            func.name.span,
                        );
                        this.error(err)
                    }
                    body
                }
            });
            this.safety = old_safety;
        });
    }

    fn check_impl_blocks(
        &mut self,
        this_ty: TypeId,
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
    ) {
        let mut seen = HashSet::new();
        for (i, block) in impls.into_iter().enumerate() {
            // TODO:
            // - impl type params (impl<T> Trait<T>)
            self.enter_id(block.scope, |this| {
                if let Some(gtr) = this.proj.scopes.get(id).impls[i].as_checked().cloned() {
                    if !seen.insert(gtr.clone()) {
                        this.proj.diag.error(Error::new(
                            format!(
                                "duplicate implementation of trait {}",
                                gtr.name(&this.proj.scopes, &mut this.proj.types)
                            ),
                            block.span,
                        ))
                    }

                    this.check_impl_block(this_ty, &gtr, block);
                    this.proj.scopes[this.current].kind = ScopeKind::Impl(TraitImpl::Checked(gtr));
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
        lhs: CheckedExpr,
        rhs: Expr,
        op: BinaryOp,
        span: Span,
    ) -> CheckedExpr {
        let Some(&(trait_name, fn_name)) = BINARY_OP_TRAITS.get(&op) else {
            bail!(
                self,
                Error::invalid_operator(
                    op,
                    &lhs.ty.name(&self.proj.scopes, &mut self.proj.types),
                    span,
                )
            );
        };

        let Some(tr_id) = self.proj.scopes.lang_traits.get(trait_name).copied() else {
            return self.error(Error::no_lang_item(trait_name, lhs_span));
        };

        let stripped = lhs.ty.strip_references(&self.proj.types);
        let Some(mfn) =
            self.get_member_fn(stripped, Some(tr_id), fn_name, &[], lhs_span, self.current)
        else {
            bail!(
                self,
                Error::doesnt_implement(
                    &stripped.name(&self.proj.scopes, &mut self.proj.types),
                    &self.proj.scopes.get(tr_id).name.data,
                    lhs_span,
                )
            );
        };

        let f = self.proj.scopes.get(mfn.func.id);
        let [p0, p1, ..] = &f.params[..] else {
            return Default::default();
        };
        let arg0 = (
            p0.label.clone(),
            lhs.auto_deref(&mut self.proj.types, p0.ty),
        );
        let p1_ty = p1
            .ty
            .with_templates(&mut self.proj.types, &mfn.func.ty_args);
        let ret = f
            .ret
            .with_templates(&mut self.proj.types, &mfn.func.ty_args);
        let rhs_span = rhs.span;
        let rhs_name = p1.label.clone();
        let rhs = self.check_expr(rhs, Some(p1_ty.strip_references(&self.proj.types)));
        let rhs = rhs.auto_deref(&mut self.proj.types, p1_ty);
        let arg0val = (rhs_name, self.type_check_checked(rhs, p1_ty, rhs_span));
        CheckedExpr::new(
            ret,
            CheckedExprData::member_call(
                &mut self.proj.types,
                mfn,
                [arg0, arg0val].into(),
                self.current,
            ),
        )
    }

    fn check_unary(&mut self, expr: CheckedExpr, op: UnaryOp, span: Span) -> CheckedExpr {
        let Some(&(trait_name, fn_name)) = UNARY_OP_TRAITS.get(&op) else {
            bail!(
                self,
                Error::invalid_operator(
                    op,
                    &expr.ty.name(&self.proj.scopes, &mut self.proj.types),
                    span,
                )
            );
        };

        let Some(tr_id) = self.proj.scopes.lang_traits.get(trait_name).copied() else {
            return self.error(Error::no_lang_item(trait_name, span));
        };

        let stripped = expr.ty.strip_references(&self.proj.types);
        let Some(mfn) = self.get_member_fn(stripped, Some(tr_id), fn_name, &[], span, self.current)
        else {
            bail!(
                self,
                Error::doesnt_implement(
                    &stripped.name(&self.proj.scopes, &mut self.proj.types),
                    &self.proj.scopes.get(tr_id).name.data,
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
            CheckedExpr::new(
                stripped,
                CheckedExprData::AffixOperator {
                    callee: expr.auto_deref(&mut self.proj.types, p0.ty).into(),
                    mfn,
                    param: p0.label.clone(),
                    scope: self.current,
                    postfix: matches!(op, UnaryOp::PostDecrement | UnaryOp::PostIncrement),
                },
            )
        } else {
            let arg0 = expr.auto_deref(&mut self.proj.types, p0.ty);
            CheckedExpr::new(
                f.ret
                    .with_templates(&mut self.proj.types, &mfn.func.ty_args),
                CheckedExprData::member_call(
                    &mut self.proj.types,
                    mfn,
                    [(p0.label.clone(), arg0)].into(),
                    self.current,
                ),
            )
        }
    }

    fn check_expr_inner(&mut self, expr: Expr, target: Option<TypeId>) -> CheckedExpr {
        // FIXME: this should just be a parameter to this function
        self.current_expr += 1;
        let span = expr.span;
        match expr.data {
            ExprData::Binary { op, left, right } => {
                let left_span = left.span;
                let assignment = op.is_assignment();
                match op {
                    BinaryOp::Assign => {
                        if let ExprData::Subscript { callee, mut args } = left.data {
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
                                return CheckedExpr::new(
                                    TypeId::VOID,
                                    CheckedExprData::Binary {
                                        op: BinaryOp::Assign,
                                        left: left.into(),
                                        right: right.into(),
                                    },
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
                        let Some(target) =
                            lhs.ty.as_option_inner(&self.proj.scopes, &self.proj.types)
                        else {
                            if lhs.ty != TypeId::UNKNOWN {
                                self.proj.diag.error(Error::invalid_operator(
                                    BinaryOp::NoneCoalesce,
                                    &lhs.ty.name(&self.proj.scopes, &mut self.proj.types),
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
                        return CheckedExpr::new(
                            if assignment { TypeId::VOID } else { target },
                            CheckedExprData::Binary {
                                op,
                                left: lhs.into(),
                                right: rhs.into(),
                            },
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

                        return CheckedExpr::new(
                            TypeId::BOOL,
                            CheckedExprData::Binary {
                                op,
                                left: left.into(),
                                right: right.into(),
                            },
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
                    (Type::RawPtr(_), BinaryOp::Sub) => {
                        let span = right.span;
                        let right = self.check_expr(*right, Some(TypeId::ISIZE));
                        let right = self.try_coerce(right, TypeId::ISIZE);
                        if right.ty == left.ty {
                            CheckedExpr::new(
                                TypeId::ISIZE,
                                CheckedExprData::Binary {
                                    op,
                                    left: left.into(),
                                    right: right.into(),
                                },
                            )
                        } else if self.proj.types[right.ty].is_integral()
                            || right.ty == TypeId::UNKNOWN
                        {
                            CheckedExpr::new(
                                left.ty,
                                CheckedExprData::Binary {
                                    op,
                                    left: left.into(),
                                    right: right.into(),
                                },
                            )
                        } else {
                            self.proj.diag.error(Error::type_mismatch_s(
                                "{integer}",
                                &right.ty.name(&self.proj.scopes, &mut self.proj.types),
                                span,
                            ));
                            Default::default()
                        }
                    }
                    (
                        Type::RawPtr(_),
                        BinaryOp::Add | BinaryOp::AddAssign | BinaryOp::SubAssign,
                    ) => {
                        let span = right.span;
                        let right = self.check_expr(*right, Some(TypeId::USIZE));
                        let right = self.try_coerce(right, TypeId::USIZE);
                        if !self.proj.types[right.ty].is_integral() && right.ty != TypeId::UNKNOWN {
                            self.proj.diag.error(Error::type_mismatch_s(
                                "{integer}",
                                &right.ty.name(&self.proj.scopes, &mut self.proj.types),
                                span,
                            ));
                        }
                        CheckedExpr::new(
                            if assignment { TypeId::VOID } else { left.ty },
                            CheckedExprData::Binary {
                                op,
                                left: left.into(),
                                right: right.into(),
                            },
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
                        let right = self.type_check(*right, TypeId::U32);
                        CheckedExpr::new(
                            if assignment { TypeId::VOID } else { left.ty },
                            CheckedExprData::Binary {
                                op,
                                left: left.into(),
                                right: right.into(),
                            },
                        )
                    }
                    _ => {
                        let right = self.type_check(*right, left.ty);
                        CheckedExpr::new(
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
                            CheckedExprData::Binary {
                                op,
                                left: left.into(),
                                right: right.into(),
                            },
                        )
                    }
                }
            }
            ExprData::Unary { op, expr } => {
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
                            Type::RawPtr(inner) => {
                                if self.safety != Safety::Unsafe {
                                    self.proj.diag.error(Error::is_unsafe(span));
                                }

                                (inner, expr)
                            }
                            Type::Unknown => return Default::default(),
                            _ => {
                                bail!(
                                    self,
                                    Error::invalid_operator(
                                        op,
                                        &expr.ty.name(&self.proj.scopes, &mut self.proj.types),
                                        span,
                                    )
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
                            CheckedExprData::Call(inner, _) => {
                                // FIXME: don't test by name
                                if matches!(&inner.data, CheckedExprData::Fn(f, _)
                                    if self.proj.scopes.get(f.id).name.data.starts_with("$sub"))
                                {
                                    self.proj.diag.warn(Error::subscript_addr(span));
                                }
                            }
                            CheckedExprData::Member { source, .. } => {
                                if source.ty.is_packed_struct(&self.proj) {
                                    self.proj.diag.warn(Error::bitfield_addr(span));
                                }
                            }
                            CheckedExprData::Fn(_, _) => {
                                let Type::Fn(f) = &self.proj.types[expr.ty] else {
                                    unreachable!()
                                };
                                let f = f.clone();
                                let fptr = Type::FnPtr(
                                    f.as_fn_ptr(&self.proj.scopes, &mut self.proj.types),
                                );
                                return CheckedExpr::new(self.proj.types.insert(fptr), expr.data);
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
                            CheckedExprData::Call(inner, _) => {
                                // FIXME: don't test by name
                                if matches!(&inner.data, CheckedExprData::Fn(f, _)
                                    if self.proj.scopes.get(f.id).name.data.starts_with("$sub"))
                                {
                                    self.proj.diag.warn(Error::subscript_addr(span));
                                }
                            }
                            CheckedExprData::Member { source, .. } => {
                                if source.ty.is_packed_struct(&self.proj) {
                                    self.proj.diag.warn(Error::bitfield_addr(span));
                                }
                            }
                            CheckedExprData::Fn(_, _) => {
                                self.proj.diag.warn(Error::new(
                                    "&mut on function creates immutable function pointer (use &)",
                                    span,
                                ));

                                let Type::Fn(f) = &self.proj.types[expr.ty] else {
                                    unreachable!()
                                };
                                let f = f.clone();
                                let fptr = Type::FnPtr(
                                    f.as_fn_ptr(&self.proj.scopes, &mut self.proj.types),
                                );
                                return CheckedExpr::new(self.proj.types.insert(fptr), expr.data);
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
                            CheckedExprData::Call(inner, _) => {
                                // FIXME: don't test by name
                                if matches!(&inner.data, CheckedExprData::Fn(f, _)
                                    if self.proj.scopes.get(f.id).name.data.starts_with("$sub"))
                                {
                                    self.proj.diag.warn(Error::subscript_addr(span));
                                }
                            }
                            CheckedExprData::Member { source, .. } => {
                                if source.ty.is_packed_struct(&self.proj) {
                                    self.proj.diag.warn(Error::bitfield_addr(span));
                                }
                            }
                            CheckedExprData::Fn(_, _) => self
                                .error(Error::new("cannot create raw pointer to function", span)),
                            _ => {}
                        }
                        (self.proj.types.insert(Type::RawPtr(expr.ty)), expr)
                    }
                    UnaryOp::Try => {
                        let expr = self.check_expr(
                            *expr,
                            target.and_then(|t| {
                                t.as_option_inner(&self.proj.scopes, &self.proj.types)
                            }),
                        );
                        if let Some(inner) =
                            expr.ty.as_option_inner(&self.proj.scopes, &self.proj.types)
                        {
                            // TODO: lambdas
                            if self
                                .current_function()
                                .and_then(|id| {
                                    self.proj
                                        .scopes
                                        .get(id)
                                        .ret
                                        .as_option_inner(&self.proj.scopes, &self.proj.types)
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
                                Error::invalid_operator(
                                    op,
                                    &expr.ty.name(&self.proj.scopes, &mut self.proj.types),
                                    span,
                                )
                            );
                        }
                    }
                    _ => {
                        let span = expr.span;
                        let expr = self.check_expr(*expr, target);
                        if !expr
                            .ty
                            .supports_unary(&self.proj.scopes, &self.proj.types, op)
                        {
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

                CheckedExpr::new(
                    out_ty,
                    CheckedExprData::Unary {
                        op,
                        expr: expr.into(),
                    },
                )
            }
            ExprData::Call { callee, args } => self.check_call(target, *callee, args, span),
            ExprData::Array(elements) => {
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
                CheckedExpr::new(
                    self.proj.types.insert(Type::Array(ty, checked.len())),
                    CheckedExprData::Array(checked),
                )
            }
            ExprData::ArrayWithInit { init, count } => {
                if let Some(&Type::Array(ty, _)) = target.map(|t| &self.proj.types[t]) {
                    let init = self.type_check(*init, ty);
                    match self.consteval(&count, Some(TypeId::USIZE)) {
                        Ok(count) => CheckedExpr::new(
                            self.proj.types.insert(Type::Array(init.ty, count)),
                            CheckedExprData::ArrayWithInit {
                                init: init.into(),
                                count,
                            },
                        ),
                        Err(err) => self.error(err),
                    }
                } else {
                    let init = self.check_expr(*init, target);
                    match self.consteval(&count, Some(TypeId::USIZE)) {
                        Ok(count) => CheckedExpr::new(
                            self.proj.types.insert(Type::Array(init.ty, count)),
                            CheckedExprData::ArrayWithInit {
                                init: init.into(),
                                count,
                            },
                        ),
                        Err(err) => self.error(err),
                    }
                }
            }
            ExprData::VecWithInit { init, count } => {
                let Some(vec) = self.proj.scopes.lang_types.get("vec").copied() else {
                    return self.error(Error::no_lang_item("vec", span));
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

                CheckedExpr::new(
                    self.make_lang_type(vec, [ty], span),
                    CheckedExprData::VecWithInit {
                        init: init.into(),
                        count: self.type_check(*count, TypeId::USIZE).into(),
                    },
                )
            }
            ExprData::Tuple(elements) => {
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
                    result_elems.insert(format!("{i}"), result);
                }

                CheckedExpr::new(
                    self.proj.scopes.get_tuple(result_ty, &mut self.proj.types),
                    CheckedExprData::Instance(result_elems),
                )
            }
            ExprData::Vec(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(vec) = self.proj.scopes.lang_types.get("vec").copied() else {
                    return self.error(Error::no_lang_item("vec", expr.span));
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
                CheckedExpr::new(
                    self.make_lang_type(vec, [ty], span),
                    CheckedExprData::Vec(checked),
                )
            }
            ExprData::Set(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(set) = self.proj.scopes.lang_types.get("set").copied() else {
                    return self.error(Error::no_lang_item("set", span));
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
                CheckedExpr::new(
                    self.make_lang_type(set, [ty], span),
                    CheckedExprData::Set(checked, self.current),
                )
            }
            ExprData::Map(elements) => {
                let Some(map) = self.proj.scopes.lang_types.get("map").copied() else {
                    return self.error(Error::no_lang_item("map", expr.span));
                };

                let mut result = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let (k, v) = if let Some(ut) = target
                    .and_then(|target| self.proj.types[target].as_user())
                    .filter(|ut| ut.id == map)
                {
                    let mut args = ut.ty_args.values().cloned();
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
                CheckedExpr::new(
                    self.make_lang_type(map, [k, v], span),
                    CheckedExprData::Map(result, self.current),
                )
            }
            ExprData::Range {
                start,
                end,
                inclusive,
            } => {
                let (item, ty, inst) = match (start, end) {
                    // this could be skipped by just transforming these expressions to calls
                    (Some(start), Some(end)) => {
                        let start = self.check_expr(*start, None);
                        let end = self.type_check(*end, start.ty);
                        let item = if inclusive {
                            "range_inclusive"
                        } else {
                            "range"
                        };
                        (
                            item,
                            start.ty,
                            [("start".into(), start), ("end".into(), end)].into(),
                        )
                    }
                    (None, Some(end)) => {
                        let end = self.check_expr(*end, None);
                        let item = if inclusive {
                            "range_to_inclusive"
                        } else {
                            "range_to"
                        };
                        (item, end.ty, [("end".into(), end)].into())
                    }
                    (Some(start), None) => {
                        let start = self.check_expr(*start, None);
                        ("range_from", start.ty, [("start".into(), start)].into())
                    }
                    (None, None) => {
                        return CheckedExpr::new(
                            self.make_lang_type_by_name("range_full", [], span),
                            CheckedExprData::Instance(Default::default()),
                        );
                    }
                };
                let Some(id) = self.proj.scopes.lang_types.get(item).copied() else {
                    return self.error(Error::no_lang_item(item, expr.span));
                };
                CheckedExpr::new(
                    self.make_lang_type(id, [ty], span),
                    CheckedExprData::Instance(inst),
                )
            }
            ExprData::String(s) => CheckedExpr::new(
                self.make_lang_type_by_name("string", [], span),
                CheckedExprData::String(s),
            ),
            ExprData::StringInterpolation(parts) => {
                let Some(fmt_id) = self.proj.scopes.lang_traits.get("format").copied() else {
                    return self.error(Error::no_lang_item("Format", span));
                };
                let formatter = self
                    .proj
                    .scopes
                    .lang_types
                    .get("string_formatter")
                    .and_then(|&id| {
                        self.proj.scopes[self.proj.scopes.get(id).body_scope]
                            .find_in_vns("new")
                            .and_then(|f| f.into_fn().ok())
                            .map(|f| {
                                CheckedExpr::new(
                                    self.proj.types.insert(Type::User(GenericUserType::new(
                                        id,
                                        Default::default(),
                                    ))),
                                    CheckedExprData::call(
                                        &mut self.proj.types,
                                        GenericFn::new(f, Default::default()),
                                        Default::default(),
                                        self.current,
                                    ),
                                )
                            })
                    })
                    .unwrap_or_else(|| self.error(Error::no_lang_item("String Formatter", span)));
                let mut out_parts = Vec::with_capacity(parts.len());
                for expr in parts {
                    let span = expr.span;
                    let expr = self.check_expr(expr, None);
                    let ty = expr.ty.strip_references(&self.proj.types);
                    if ty != TypeId::UNKNOWN && self.get_trait_impl(ty, fmt_id).is_none() {
                        self.proj.diag.error(Error::doesnt_implement(
                            &ty.name(&self.proj.scopes, &mut self.proj.types),
                            "Format",
                            span,
                        ));
                    }
                    let ptr_to_unk = self.proj.types.insert(Type::Ptr(TypeId::UNKNOWN));
                    if !matches!(&expr.data, CheckedExprData::String(s) if s.is_empty()) {
                        out_parts.push(expr.auto_deref(&mut self.proj.types, ptr_to_unk));
                    }
                }

                CheckedExpr::new(
                    self.make_lang_type_by_name("string", [], span),
                    CheckedExprData::StringInterpolation {
                        parts: out_parts,
                        formatter: formatter.into(),
                        scope: self.current,
                    },
                )
            }
            ExprData::ByteString(s) => {
                let arr = self.proj.types.insert(Type::Array(TypeId::U8, s.len()));
                CheckedExpr::new(
                    self.proj.types.insert(Type::Ptr(arr)),
                    CheckedExprData::ByteString(s),
                )
            }
            ExprData::Char(s) => CheckedExpr::new(TypeId::CHAR, CheckedExprData::Char(s)),
            ExprData::ByteChar(c) => {
                CheckedExpr::new(TypeId::U8, CheckedExprData::Integer(BigInt::from(c)))
            }
            ExprData::None => {
                if let Some(inner) = target
                    .and_then(|target| target.as_option_inner(&self.proj.scopes, &self.proj.types))
                {
                    CheckedExpr::option_null(self.make_lang_type_by_name("option", [inner], span))
                } else {
                    self.error(Error::new("cannot infer type of option null literal", span))
                }
            }
            ExprData::Void => CheckedExpr::new(TypeId::VOID, CheckedExprData::Void),
            ExprData::Bool(value) => CheckedExpr {
                ty: TypeId::BOOL,
                data: CheckedExprData::Bool(value),
            },
            ExprData::Integer(integer) => {
                let (ty, value) = self.get_int_type_and_val(target, &integer, span);
                CheckedExpr::new(ty, CheckedExprData::Integer(value))
            }
            ExprData::Float(value) => CheckedExpr::new(
                target
                    .map(|target| target.strip_options(&self.proj.scopes, &self.proj.types))
                    .filter(|&t| t == TypeId::F32 || t == TypeId::F64)
                    .unwrap_or(TypeId::F64),
                CheckedExprData::Float(value),
            ),
            ExprData::Path(path) => match self.resolve_value_path(&path) {
                ResolvedValue::Var(id) => {
                    let var = self.proj.scopes.get(id);
                    if !var.is_static
                        && self.current_function() != self.proj.scopes.function_of(var.scope)
                    {
                        self.proj.diag.error(Error::new(
                            "cannot reference local variable of enclosing function",
                            span,
                        ));
                    }

                    let ty = var.ty;
                    self.proj.scopes.get_mut(id).unused = false;
                    CheckedExpr::new(ty, CheckedExprData::Var(id))
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

                    if let Some(id) = self.proj.scopes.get(func.id).constructor {
                        if self
                            .proj
                            .scopes
                            .get(id)
                            .is_empty_variant(&self.proj.scopes.get(func.id).name.data)
                        {
                            return CheckedExpr::new(
                                self.proj
                                    .types
                                    .insert(Type::User(GenericUserType::new(id, func.ty_args))),
                                CheckedExprData::VariantInstance {
                                    members: Default::default(),
                                    variant: self.proj.scopes.get(func.id).name.data.clone(),
                                },
                            );
                        }
                    }

                    CheckedExpr::new(
                        self.proj.types.insert(Type::Fn(func.clone())),
                        CheckedExprData::Fn(func, self.current),
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

                    if let Some(id) = self.proj.scopes.get(mfn.func.id).constructor {
                        if self
                            .proj
                            .scopes
                            .get(id)
                            .is_empty_variant(&self.proj.scopes.get(mfn.func.id).name.data)
                        {
                            return CheckedExpr::new(
                                self.proj
                                    .types
                                    .insert(Type::User(GenericUserType::new(id, mfn.func.ty_args))),
                                CheckedExprData::VariantInstance {
                                    members: Default::default(),
                                    variant: self.proj.scopes.get(mfn.func.id).name.data.clone(),
                                },
                            );
                        }
                    }

                    CheckedExpr::new(
                        self.proj.types.insert(Type::Fn(mfn.func.clone())),
                        CheckedExprData::MemFn(mfn, self.current),
                    )
                }
                ResolvedValue::UnionConstructor(ut) => bail!(
                    self,
                    Error::expected_found(
                        "expression",
                        &format!(
                            "type '{}'",
                            ut.name(&self.proj.scopes, &mut self.proj.types)
                        ),
                        span,
                    )
                ),
                ResolvedValue::NotFound(err) => self.error(err),
                ResolvedValue::Error => Default::default(),
            },
            ExprData::Block(body, label) => {
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
                CheckedExpr::new(
                    target.unwrap_or(TypeId::VOID),
                    CheckedExprData::Block(block),
                )
            }
            ExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                let (cond, vars) = self.type_check_with_listen(*cond);
                let target = if else_branch.is_none() {
                    target.and_then(|t| t.as_option_inner(&self.proj.scopes, &self.proj.types))
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
                let else_branch =
                    if let Some(expr) = else_branch {
                        if out_type == TypeId::NEVER {
                            let expr = self.check_expr_inner(*expr, None);
                            out_type = expr.ty;
                            if_branch = self.try_coerce(if_branch, expr.ty);
                            Some(expr)
                        } else {
                            let span = expr.span;
                            let source = self.check_expr_inner(*expr, target.or(Some(out_type)));
                            Some(self.type_check_checked(source, out_type, span))
                        }
                    } else if if_branch.data.is_yielding_block(&self.proj.scopes) {
                        if out_type == TypeId::NEVER
                            || out_type == TypeId::VOID
                            || out_type == TypeId::UNKNOWN
                        {
                            out_type = TypeId::VOID;
                            Some(CheckedExpr::new(TypeId::VOID, CheckedExprData::Void))
                        } else {
                            out_type = self.make_lang_type_by_name("option", [out_type], span);
                            if_branch = self.try_coerce(if_branch, out_type);
                            Some(self.check_expr_inner(
                                Located::new(span, ExprData::None),
                                Some(out_type),
                            ))
                        }
                    } else {
                        None
                    };

                CheckedExpr::new(
                    out_type,
                    CheckedExprData::If {
                        cond: cond.into(),
                        if_branch: if_branch.into(),
                        else_branch: else_branch.map(|e| e.into()),
                    },
                )
            }
            ExprData::Loop {
                cond,
                body,
                do_while,
                label,
            } => {
                let infinite = cond.is_none();
                let target = self.loop_target(target, infinite);
                let (cond, body) = if let Some(cond) = cond {
                    let (cond, vars) = self.type_check_with_listen(*cond);
                    let body = self.create_block_with_init(
                        body,
                        ScopeKind::Loop(LoopScopeKind {
                            target,
                            breaks: LoopBreak::None,
                            infinite,
                            label,
                        }),
                        |this| {
                            if !do_while {
                                this.define(&vars);
                            }
                        },
                    );
                    (Some(cond.into()), body)
                } else {
                    (
                        None,
                        self.create_block(
                            body,
                            ScopeKind::Loop(LoopScopeKind {
                                target,
                                breaks: LoopBreak::None,
                                infinite,
                                label,
                            }),
                        ),
                    )
                };
                let (out_type, optional) =
                    self.loop_out_type(&self.proj.scopes[body.scope].kind.clone(), span);
                CheckedExpr::new(
                    out_type,
                    CheckedExprData::Loop {
                        cond,
                        body,
                        do_while,
                        optional,
                    },
                )
            }
            ExprData::For {
                patt,
                iter,
                body,
                label,
            } => self.check_for_expr(target, patt, iter, body, label),
            ExprData::Member {
                source,
                member: name,
                generics,
            } => {
                if !generics.is_empty() {
                    self.error(Error::new(
                        "member variables cannot have type arguments",
                        span,
                    ))
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
                                &id.name(&self.proj.scopes, &mut self.proj.types),
                                &name.data,
                                name.span,
                            )
                        );
                    }
                };
                self.resolve_members(ut_id);
                self.check_hover(name.span, LspItem::Property(ut_id, name.data.clone()));

                let ut = self.proj.scopes.get(ut_id);
                let Some(member) = ut.members.get(&name.data) else {
                    bail!(
                        self,
                        Error::no_member(
                            &source.ty.name(&self.proj.scopes, &mut self.proj.types),
                            &name.data,
                            name.span,
                        )
                    );
                };

                if ut.kind.is_unsafe_union() && self.safety != Safety::Unsafe {
                    self.proj.diag.error(Error::is_unsafe(name.span));
                }

                let ty = member.ty.with_ut_templates(&mut self.proj.types, id);
                if !member.public && !self.can_access_privates(ut.scope) {
                    self.proj.diag.error(Error::private_member(
                        &id.name(&self.proj.scopes, &mut self.proj.types),
                        &name.data,
                        name.span,
                    ));
                }
                CheckedExpr::new(
                    ty,
                    CheckedExprData::Member {
                        source: source.auto_deref(&mut self.proj.types, id).into(),
                        member: name.data,
                    },
                )
            }
            ExprData::Subscript { callee, args } => {
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
            ExprData::Return(expr) => self.check_return(*expr, span),
            ExprData::Tail(expr) => match &self.proj.scopes[self.current].kind {
                ScopeKind::Function(_) | ScopeKind::Lambda(_, _) => self.check_return(*expr, span),
                ScopeKind::Loop { .. } => self.type_check(*expr, TypeId::VOID),
                ScopeKind::Block(data) => self.check_yield(Some(expr), data.clone(), self.current),
                _ => self.error(Error::new("yield outside of block", expr.span)),
            },
            ExprData::Break(expr, label) => {
                if let Some(label) = label {
                    let label_data = Some(&label.data);
                    for (id, scope) in self.proj.scopes.walk(self.current) {
                        if scope.kind.is_defer() {
                            break;
                        }

                        match &scope.kind {
                            ScopeKind::Loop(data) if data.label.as_ref() == label_data => {
                                return self.check_break(expr, data.clone(), id);
                            }
                            ScopeKind::Block(data) if data.label.as_ref() == label_data => {
                                return self.check_yield(expr, data.clone(), id);
                            }
                            _ => {}
                        }
                    }

                    if let Some(expr) = expr {
                        self.check_expr(*expr, None);
                    }
                    return self
                        .error(Error::new(format!("undefined label '{label}'"), label.span));
                }

                let Some((loop_data, id)) = self.current_loop(&None) else {
                    if let Some(expr) = expr {
                        self.check_expr(*expr, None);
                    }
                    return self.error(Error::new("break outside of loop", span));
                };

                self.check_break(expr, loop_data.clone(), id)
            }
            ExprData::Continue(label) => {
                let Some((_, id)) = self.current_loop(&label) else {
                    if let Some(label) = label {
                        return self
                            .error(Error::new(format!("undefined label '{label}'"), label.span));
                    } else {
                        return self.error(Error::new("continue outside of loop", span));
                    }
                };

                CheckedExpr::new(TypeId::NEVER, CheckedExprData::Continue(id))
            }
            ExprData::Is { expr, pattern } => self.enter(ScopeKind::None, |this| {
                let mut prev = this.current_expr;
                let expr = this.check_expr(*expr, None);
                std::mem::swap(&mut this.current_expr, &mut prev);
                let patt = this.check_pattern(PatternParams {
                    binding: false,
                    scrutinee: expr.ty,
                    mutable: false,
                    pattern,
                    typ: PatternType::Regular,
                    has_hint: false,
                });
                std::mem::swap(&mut this.current_expr, &mut prev);
                CheckedExpr::new(TypeId::BOOL, CheckedExprData::Is(expr.into(), patt))
            }),
            ExprData::Match { expr, body } => {
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
                let target = target.unwrap_or(if has_never {
                    TypeId::NEVER
                } else {
                    TypeId::VOID
                });
                if !matches!(target, TypeId::NEVER | TypeId::VOID) {
                    for (_, e) in result.iter_mut() {
                        *e = self.try_coerce(std::mem::take(e), target);
                    }
                }

                self.check_match_coverage(scrutinee.ty, result.iter().map(|it| &it.0), span);
                CheckedExpr::new(
                    target,
                    CheckedExprData::Match {
                        expr: scrutinee.into(),
                        body: result,
                    },
                )
            }
            ExprData::As { expr, ty, throwing } => {
                let ty = self.resolve_typehint(&ty);
                let expr = self.check_expr(*expr, Some(ty));
                match self.coerce(expr, ty) {
                    Ok(expr) => expr,
                    Err(expr) => {
                        self.check_cast(expr.ty, ty, throwing, span);
                        CheckedExpr::new(ty, CheckedExprData::As(expr.into(), throwing))
                    }
                }
            }
            ExprData::Error => CheckedExpr::default(),
            ExprData::Lambda {
                params,
                ret,
                body,
                moves: _,
            } => {
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
                                    .cloned()
                            })
                            .unwrap_or_else(|| {
                                this.error(Error::new(
                                    format!("cannot infer type of parameter '{}'", name.data),
                                    name.span,
                                ))
                            });

                        lparams.push(ty);
                        this.insert::<VariableId>(
                            Variable {
                                public: false,
                                name,
                                ty,
                                is_static: false,
                                mutable: false,
                                value: None,
                                unused: true,
                                has_hint,
                            },
                            false,
                            false,
                        );
                    }

                    let body = if let ExprData::Block(body, _) = body.data {
                        this.check_block(body)
                    } else {
                        vec![CheckedStmt::Expr(this.check_expr(
                            Expr::new(body.span, ExprData::Return(body)),
                            None,
                        ))]
                    };

                    (this.current, body)
                });
                let (target, yields) = self.proj.scopes[id].kind.as_lambda().unwrap();
                let fnptr = Type::FnPtr(FnPtr {
                    params: lparams,
                    ret: yields.then(|| *target).flatten().unwrap_or(TypeId::VOID),
                });
                CheckedExpr::new(self.proj.types.insert(fnptr), CheckedExprData::Lambda(body))
            }
            ExprData::Unsafe(expr) => {
                // for unsafe specifically, span is only the keyword
                if self.safety == Safety::Unsafe {
                    self.proj
                        .diag
                        .warn(Error::new("unsafe expression in unsafe context", span))
                }

                let old_safety = std::mem::replace(&mut self.safety, Safety::Unsafe);
                let expr = self.check_expr(*expr, target);
                self.safety = old_safety;
                expr
            }
        }
    }

    fn check_expr(&mut self, expr: Expr, target: Option<TypeId>) -> CheckedExpr {
        let expr = self.check_expr_inner(expr, target);
        if expr.ty == TypeId::NEVER
            && !matches!(expr.data, CheckedExprData::Yield(_, scope) if scope == self.current)
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

    fn type_check_with_listen(&mut self, expr: Expr) -> (CheckedExpr, Vec<VariableId>) {
        let prev = std::mem::take(&mut self.listening_vars);
        let prev_insp = std::mem::replace(&mut self.listening_expr, self.current_expr + 1);
        let expr = self.type_check(expr, TypeId::BOOL);
        self.listening_expr = prev_insp;
        (expr, std::mem::replace(&mut self.listening_vars, prev))
    }

    fn define(&mut self, vars: &[VariableId]) {
        for &var in vars.iter() {
            let name = self.proj.scopes.get(var).name.data.clone();
            self.proj.scopes[self.current]
                .vns
                .insert(name, Vis::new(ValueItem::Var(var), false));
        }
    }

    fn current_loop(&self, label: &Option<Located<String>>) -> Option<(&LoopScopeKind, ScopeId)> {
        let label = label.as_ref().map(|l| &l.data);
        self.proj
            .scopes
            .walk(self.current)
            .take_while(|(_, scope)| !scope.kind.is_defer())
            .find_map(|(id, scope)| {
                scope
                    .kind
                    .as_loop()
                    .filter(|l| label.is_none() || l.label.as_ref() == label)
                    .zip(Some(id))
            })
    }

    fn check_array_subscript(
        &mut self,
        target: TypeId,
        callee: CheckedExpr,
        args: Vec<(Option<String>, Expr)>,
    ) -> CheckedExpr {
        fn maybe_span(this: &mut TypeChecker, ty: TypeId, imm: bool) -> Option<(bool, UserTypeId)> {
            // FIXME: remove this hack when .. implements rangebounds
            let full = this.proj.scopes.lang_types.get("range_full");
            let id = *this.proj.scopes.lang_traits.get("range_bounds")?;
            let bound = GenericTrait::from_type_args(&this.proj.scopes, id, [TypeId::USIZE]);
            let is_range_full = this
                .proj
                .types
                .get(ty)
                .as_user()
                .is_some_and(|ut| Some(&ut.id) == full);
            if is_range_full || this.implements_trait(ty, &bound) {
                let span_ty = if imm {
                    *this.proj.scopes.lang_types.get("span")?
                } else {
                    *this.proj.scopes.lang_types.get("span_mut")?
                };
                Some((is_range_full, span_ty))
            } else {
                None
            }
        }

        let mut args = args.into_iter();
        let (name, expr) = args.next().unwrap();
        if let Some(name) = name {
            self.error(Error::new(
                format!("unknown parameter: '{name}'"),
                expr.span,
            ))
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
            Ok(expr) => CheckedExpr::new(
                target,
                CheckedExprData::Subscript {
                    callee: callee.into(),
                    arg: expr.into(),
                },
            ),
            Err(expr) if self.proj.types[expr.ty].is_integral() => CheckedExpr::new(
                target,
                CheckedExprData::Subscript {
                    callee: callee.into(),
                    arg: expr.into(),
                },
            ),
            Err(expr) => {
                let Some((full, id)) = maybe_span(self, expr.ty, self.immutable_receiver(&callee))
                else {
                    bail!(
                        self,
                        Error::expected_found(
                            "array index",
                            &format!(
                                "type '{}'",
                                expr.ty.name(&self.proj.scopes, &mut self.proj.types)
                            ),
                            arg_span,
                        )
                    );
                };

                CheckedExpr::new(
                    self.proj
                        .types
                        .insert(Type::User(GenericUserType::from_type_args(
                            &self.proj.scopes,
                            id,
                            [target],
                        ))),
                    CheckedExprData::SliceArray {
                        callee: callee.into(),
                        arg: expr.into(),
                        range_full: full,
                    },
                )
            }
        }
    }

    fn check_subscript(
        &mut self,
        callee: CheckedExpr,
        ty: TypeId,
        args: Vec<(Option<String>, Expr)>,
        target: Option<TypeId>,
        assign: bool,
        span: Span,
    ) -> CheckedExpr {
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
                let prev = self.proj.diag.set_errors_enabled(false);
                let (args, ret, failed) =
                    self.check_fn_args(&mut func, Some(recv), args, target, span);
                self.proj.diag.set_errors_enabled(prev);
                // TODO: if the arguments have non overload related errors, just stop overload
                // resolution
                if failed
                    || args
                        .iter()
                        .any(|arg| matches!(arg.1.data, CheckedExprData::Error))
                {
                    continue;
                }
                // unsafe doesnt cause check_fn_args to fail, but we mute errors, so check again
                // here
                if self.proj.scopes.get(func.id).is_unsafe && self.safety != Safety::Unsafe {
                    self.error(Error::is_unsafe(span))
                }

                if !assign {
                    if let Type::Ptr(inner) | Type::MutPtr(inner) = &self.proj.types[ret] {
                        return CheckedExpr::new(
                            *inner,
                            CheckedExprData::AutoDeref(
                                CheckedExpr::new(
                                    ret,
                                    CheckedExprData::call(
                                        &mut self.proj.types,
                                        func,
                                        args,
                                        self.current,
                                    ),
                                )
                                .into(),
                                1,
                            ),
                        );
                    }
                }

                return CheckedExpr::new(
                    ret,
                    CheckedExprData::call(&mut self.proj.types, func, args, self.current),
                );
            }
        }

        let args: Vec<_> = args
            .into_iter()
            .map(|(_, expr)| self.check_expr(expr, None))
            .collect();
        if ty == TypeId::UNKNOWN || args.iter().any(|expr| expr.ty == TypeId::UNKNOWN) {
            return Default::default();
        }

        bail!(
            self,
            Error::new(
                format!(
                    "type '{}' does not support subscript{} with arguments of type ({})",
                    &callee.ty.name(&self.proj.scopes, &mut self.proj.types),
                    if assign { " assign" } else { "" },
                    args.into_iter()
                        .map(|expr| expr.ty.name(&self.proj.scopes, &mut self.proj.types))
                        .collect::<Vec<_>>()
                        .join(", "),
                ),
                span,
            )
        )
    }

    fn immutable_receiver(&self, callee: &CheckedExpr) -> bool {
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

    fn check_cast(&mut self, mut from_id: TypeId, to_id: TypeId, throwing: bool, span: Span) {
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

        let from = &self.proj.types[from_id];
        let to = &self.proj.types[to_id];
        if let Some((a, b)) = from.as_integral().zip(to.as_integral()) {
            if (a.signed == b.signed || (a.signed && !b.signed)) && a.bits <= b.bits {
                return;
            }
            if (!a.signed && b.signed) && a.bits < b.bits {
                return;
            }
        }

        match (from, to) {
            (a, b) if a == b => {}
            (Type::Char, Type::Uint(n)) if *n >= 32 => {}
            (Type::Char, Type::Int(n)) if *n >= 33 => {}
            (Type::F32, Type::F64) => {}
            (Type::Uint(n), Type::F32) if *n <= 24 => {}
            (Type::Int(n), Type::F32) if *n <= 25 => {}
            (Type::Uint(n), Type::F64) if *n <= 53 => {}
            (Type::Int(n), Type::F64) if *n <= 53 => {}
            (Type::Ptr(_) | Type::MutPtr(_) | Type::RawPtr(_), Type::Usize) => {}
            (
                Type::Bool,
                Type::Int(_)
                | Type::Uint(_)
                | Type::CInt(_)
                | Type::CUint(_)
                | Type::Usize
                | Type::Isize,
            ) => {}
            (Type::Usize | Type::MutPtr(_) | Type::Ptr(_) | Type::RawPtr(_), Type::RawPtr(_)) => {}
            (Type::Usize, Type::Ptr(_) | Type::MutPtr(_))
            | (Type::MutPtr(_) | Type::Ptr(_) | Type::RawPtr(_), Type::MutPtr(_) | Type::Ptr(_)) => {
                if self.safety != Safety::Unsafe {
                    self.error(Error::is_unsafe(span))
                }
            }
            (Type::CUint(a), Type::CUint(b)) if a <= b => {}
            (
                Type::Int(_)
                | Type::Uint(_)
                | Type::CInt(_)
                | Type::CUint(_)
                | Type::Usize
                | Type::Isize,
                Type::F32 | Type::F64,
            ) if throwing => {}
            (
                Type::F32 | Type::F64,
                Type::Int(_)
                | Type::Uint(_)
                | Type::CInt(_)
                | Type::CUint(_)
                | Type::Usize
                | Type::Isize,
            ) if throwing => {}
            (
                Type::Int(_)
                | Type::Uint(_)
                | Type::CInt(_)
                | Type::CUint(_)
                | Type::Usize
                | Type::Isize
                | Type::Char,
                Type::Int(_)
                | Type::Uint(_)
                | Type::CInt(_)
                | Type::CUint(_)
                | Type::Usize
                | Type::Isize
                | Type::Char,
            ) if throwing => {}
            (Type::F64, Type::F32) if throwing => {}
            _ => bail!(
                self,
                Error::new(
                    format!(
                        "cannot{}cast expression of type '{}' to '{}'",
                        if !throwing { " infallibly " } else { " " },
                        from_id.name(&self.proj.scopes, &mut self.proj.types),
                        to_id.name(&self.proj.scopes, &mut self.proj.types),
                    ),
                    span,
                )
            ),
        }
    }

    fn check_return(&mut self, expr: Expr, span: Span) -> CheckedExpr {
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

                    return CheckedExpr::new(TypeId::NEVER, CheckedExprData::Return(expr.into()));
                }
                &ScopeKind::Function(id) => {
                    let target = self.proj.scopes.get(id).ret;
                    return CheckedExpr::new(
                        TypeId::NEVER,
                        CheckedExprData::Return(self.type_check(expr, target).into()),
                    );
                }
                ScopeKind::Defer => {
                    self.proj
                        .diag
                        .error(Error::new("cannot return in defer block", span));
                    return self.check_expr(expr, None);
                }
                _ => {}
            }
        }

        // this should never be possible, but report error instead of crashing for LSP reasons
        self.error(Error::new(
            "return expression outside of function",
            expr.span,
        ))
    }

    fn check_break(
        &mut self,
        expr: Option<Box<Expr>>,
        mut data: LoopScopeKind,
        id: ScopeId,
    ) -> CheckedExpr {
        let expr = if let Some(expr) = expr {
            let span = expr.span;
            let expr = if let Some(target) = data.target {
                self.type_check(*expr, target)
            } else {
                let expr = self.check_expr(*expr, data.target);
                data.target = Some(expr.ty);
                expr
            };
            data.breaks = LoopBreak::WithValue;
            self.proj.scopes[id].kind = ScopeKind::Loop(data);

            let (target, opt) = self.loop_out_type(&self.proj.scopes[id].kind.clone(), span);
            if opt {
                Some(self.try_coerce(expr, target).into())
            } else {
                Some(expr.into())
            }
        } else {
            data.target = Some(TypeId::VOID);
            data.breaks = LoopBreak::WithNothing;
            self.proj.scopes[id].kind = ScopeKind::Loop(data);
            None
        };

        CheckedExpr::new(TypeId::NEVER, CheckedExprData::Break(expr, id))
    }

    fn check_yield(
        &mut self,
        expr: Option<Box<Expr>>,
        mut data: BlockScopeKind,
        id: ScopeId,
    ) -> CheckedExpr {
        let expr = if let Some(expr) = expr {
            if let Some(target) = data.target {
                Some(self.type_check(*expr, target).into())
            } else {
                let expr = self.check_expr(*expr, data.target);
                data.target = Some(expr.ty);
                Some(expr.into())
            }
        } else {
            None
        };
        data.yields = true;
        self.proj.scopes[id].kind = ScopeKind::Block(data);

        CheckedExpr::new(TypeId::NEVER, CheckedExprData::Yield(expr, id))
    }

    fn check_for_expr(
        &mut self,
        target: Option<TypeId>,
        patt: Located<Pattern>,
        iter: Box<Located<ExprData>>,
        body: Vec<Stmt>,
        label: Option<String>,
    ) -> CheckedExpr {
        let span = iter.span;
        let iter = self.check_expr(*iter, None);
        let Some(iter_tr_id) = self.proj.scopes.lang_traits.get("iter").copied() else {
            return self.error(Error::no_lang_item("Iterator", span));
        };

        let kind = ScopeKind::Block(BlockScopeKind {
            target: None,
            yields: true,
            label: None,
            branches: false,
        });
        self.enter(kind, |this| {
            let Some(ut) = this.get_trait_impl(iter.ty, iter_tr_id) else {
                this.check_block(body);
                bail!(
                    this,
                    Error::doesnt_implement(
                        &iter.ty.name(&this.proj.scopes, &mut this.proj.types),
                        "Iterator",
                        span,
                    )
                );
            };

            let next_ty = ut
                .first_type_arg()
                .unwrap()
                .with_ut_templates(&mut this.proj.types, iter.ty);
            let patt_span = patt.span;
            let patt = this.check_pattern(PatternParams {
                binding: true,
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
            let (out, optional) =
                this.loop_out_type(&this.proj.scopes[body.scope].kind.clone(), span);

            let iter_var = this.insert::<VariableId>(
                Variable {
                    public: false,
                    name: Located::new(Span::default(), format!("$iter{}", this.current.0)),
                    ty: iter.ty,
                    is_static: false,
                    mutable: true,
                    value: None,
                    unused: false,
                    has_hint: false,
                },
                false,
                false,
            );

            let next_fn_call = {
                let Some(mfn) = this.get_member_fn(
                    iter.ty,
                    Some(iter_tr_id),
                    "next",
                    &[],
                    Span::default(),
                    this.current,
                ) else {
                    panic!("ICE: for loop, can't find next function for iterator type");
                };

                let f = this.proj.scopes.get(mfn.func.id);
                let Some(p0) = f.params.first().map(|p| p.label.clone()) else {
                    panic!("ICE: Iterator::next() has 0 parameters");
                };
                let arg0 = CheckedExpr::new(
                    this.proj.types.insert(Type::MutPtr(iter.ty)),
                    CheckedExprData::Unary {
                        op: UnaryOp::AddrMut,
                        expr: CheckedExpr::new(iter.ty, CheckedExprData::Var(iter_var)).into(),
                    },
                );

                CheckedExpr::new(
                    f.ret
                        .with_templates(&mut this.proj.types, &mfn.func.ty_args),
                    CheckedExprData::member_call(
                        &mut this.proj.types,
                        mfn,
                        [(p0, arg0)].into(),
                        this.current,
                    ),
                )
            };

            let cond = CheckedExpr::new(
                TypeId::BOOL,
                CheckedExprData::Is(
                    next_fn_call.into(),
                    CheckedPattern::refutable(CheckedPatternData::Variant {
                        pattern: Some(
                            CheckedPattern::irrefutable(CheckedPatternData::Destrucure {
                                patterns: vec![("0".into(), next_ty, patt)],
                                borrows: false,
                            })
                            .into(),
                        ),
                        variant: "Some".into(),
                        inner: TypeId::UNKNOWN,
                        borrows: false,
                    }),
                ),
            );
            let while_loop = CheckedExpr::new(
                out,
                CheckedExprData::Loop {
                    cond: Some(cond.into()),
                    body,
                    do_while: false,
                    optional,
                },
            );

            CheckedExpr::new(
                out,
                CheckedExprData::Block(Block {
                    body: vec![
                        CheckedStmt::Let(
                            CheckedPattern {
                                irrefutable: true,
                                data: CheckedPatternData::Variable(iter_var),
                            },
                            Some(iter),
                        ),
                        CheckedStmt::Expr(CheckedExpr::new(
                            out,
                            CheckedExprData::Yield(Some(while_loop.into()), this.current),
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
        args: Vec<(Option<String>, Expr)>,
        span: Span,
    ) -> CheckedExpr {
        self.resolve_members(ut.id);

        if let Some(target) = target
            .and_then(|t| self.proj.types[t].as_user())
            .filter(|t| t.id == ut.id)
        {
            for (id, ty) in target.ty_args.iter() {
                if ut.ty_args.get(id).is_some_and(|&id| id != TypeId::UNKNOWN) {
                    continue;
                }

                ut.ty_args.insert(*id, *ty);
            }
        }

        let mut members = IndexMap::new();
        if !self.proj.scopes.get(ut.id).members.is_empty() {
            let mut args = args.into_iter();
            let Some((name, expr)) = args.next() else {
                return self.error(Error::new("expected 1 variant argument", span));
            };

            let Some(name) = name else {
                return self.error(Error::new("expected 0 positional arguments", span));
            };

            if args.next().is_some() {
                self.error(Error::new("too many variant arguments", span))
            }

            let Some(ty) = self
                .proj
                .scopes
                .get(ut.id)
                .members
                .get(&name)
                .map(|m| m.ty.with_templates(&mut self.proj.types, &ut.ty_args))
            else {
                return self.error(Error::new(format!("unknown variant '{name}'"), span));
            };

            members.insert(name, self.check_arg(&mut ut, expr, ty).0);
        } else if !args.is_empty() {
            self.error(Error::new("expected 0 arguments", span))
        }

        for (&id, &ty) in ut.ty_args.iter() {
            if ty == TypeId::UNKNOWN {
                self.error(Error::new(
                    format!(
                        "cannot infer type for type parameter '{}'",
                        self.proj.scopes.get(id).name
                    ),
                    span,
                ))
            } else {
                self.check_bounds(
                    &ut.ty_args,
                    ty,
                    self.proj.scopes.get(id).impls.clone(),
                    span,
                );
            }
        }

        CheckedExpr::new(
            self.proj.types.insert(Type::User(ut)),
            CheckedExprData::Instance(members),
        )
    }

    fn check_call(
        &mut self,
        target: Option<TypeId>,
        callee: Expr,
        args: Vec<(Option<String>, Expr)>,
        span: Span,
    ) -> CheckedExpr {
        match callee.data {
            ExprData::Member {
                source,
                member,
                generics,
            } => {
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
                    self.get_member_fn(id, None, &member.data, &generics, span, self.current)
                else {
                    bail!(
                        self,
                        Error::no_method(
                            &id.name(&self.proj.scopes, &mut self.proj.types),
                            &member.data,
                            span,
                        )
                    );
                };
                self.check_hover(member.span, LspItem::Fn(mfn.func.id, None));
                if mfn.dynamic && !self.proj.scopes.get(mfn.func.id).type_params.is_empty() {
                    self.error(Error::new(
                        "cannot call generic functions through a dynamic trait pointer",
                        span,
                    ))
                }

                let f = self.proj.scopes.get(mfn.func.id);
                if !mfn.public && !self.can_access_privates(mfn.owner) {
                    self.proj.diag.error(Error::new(
                        format!(
                            "cannot access private method '{}' of type '{}'",
                            self.proj.scopes.get(mfn.func.id).name.data,
                            id.name(&self.proj.scopes, &mut self.proj.types)
                        ),
                        span,
                    ));
                }

                let Some(this_param) = f.params.first().filter(|p| p.label == THIS_PARAM) else {
                    return self.error(Error::new(
                        format!("associated function '{member}' cannot be used as a method"),
                        span,
                    ));
                };

                let this_param_ty = this_param.ty;
                if self.proj.types[this_param_ty].is_mut_ptr() {
                    if !matches!(
                        self.proj.types[recv.ty],
                        Type::Ptr(_) | Type::MutPtr(_) | Type::DynPtr(_) | Type::DynMutPtr(_)
                    ) && !recv.can_addrmut(&self.proj.scopes, &self.proj.types)
                    {
                        self.error(Error::new(
                            format!("cannot call method '{member}' with immutable receiver"),
                            span,
                        ))
                    }

                    let mut ty = &self.proj.types[recv.ty];
                    while let Type::MutPtr(inner) = ty {
                        ty = &self.proj.types[*inner];
                    }

                    if matches!(ty, Type::Ptr(_) | Type::DynPtr(_)) {
                        self.error(Error::new(
                            format!("cannot call method '{member}' through an immutable pointer"),
                            span,
                        ))
                    }
                }

                let recv = recv.auto_deref(&mut self.proj.types, this_param_ty);
                let (args, ret, _) =
                    self.check_fn_args(&mut mfn.func, Some(recv), args, target, span);
                if mfn.dynamic {
                    return CheckedExpr::new(ret, CheckedExprData::CallDyn(mfn.func, args));
                } else {
                    return CheckedExpr::new(
                        ret,
                        CheckedExprData::member_call(&mut self.proj.types, mfn, args, self.current),
                    );
                }
            }
            ExprData::Path(ref path) => match self.resolve_value_path(path) {
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

                        if ut.is_empty_variant(&self.proj.scopes.get(mfn.func.id).name.data) {
                            return self.error(Error::expected_found(
                                "function",
                                &format!("union variant '{}'", f.name.data),
                                span,
                            ));
                        }
                    }

                    let (args, ret, _) =
                        self.check_fn_args(&mut mfn.func, None, args, target, span);
                    if mfn.dynamic {
                        return CheckedExpr::new(ret, CheckedExprData::CallDyn(mfn.func, args));
                    } else {
                        return CheckedExpr::new(
                            ret,
                            CheckedExprData::member_call(
                                &mut self.proj.types,
                                mfn,
                                args,
                                self.current,
                            ),
                        );
                    }
                }
                ResolvedValue::NotFound(err) => return self.error(err),
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
                            self.proj.diag.error(Error::new(
                                "keyword parameters are not allowed here",
                                arg.span,
                            ));
                        }

                        result.push(self.type_check(arg, param));
                    } else {
                        self.proj
                            .diag
                            .error(Error::new("too many positional arguments", span));
                        break;
                    }
                }

                if result.len() < f.params.len() {
                    self.error(Error::new("too few positional arguments", span))
                }

                CheckedExpr::new(f.ret, CheckedExprData::CallFnPtr(callee.into(), result))
            }
            _ => bail!(
                self,
                Error::expected_found(
                    "callable item",
                    &format!(
                        "'{}'",
                        &callee.ty.name(&self.proj.scopes, &mut self.proj.types)
                    ),
                    span,
                )
            ),
        }
    }

    fn check_known_fn_call(
        &mut self,
        mut func: GenericFn,
        args: Vec<(Option<String>, Expr)>,
        target: Option<TypeId>,
        span: Span,
    ) -> CheckedExpr {
        let f = self.proj.scopes.get(func.id);
        if let Some(id) = f.constructor {
            let ut = self.proj.scopes.get(id);
            for &id in ut.type_params.iter() {
                func.ty_args.entry(id).or_insert(TypeId::UNKNOWN);
            }

            if ut.is_empty_variant(&self.proj.scopes.get(func.id).name.data) {
                return self.error(Error::expected_found(
                    "function",
                    &format!("union variant '{}'", f.name.data),
                    span,
                ));
            }
        }

        let (args, ret, _) = self.check_fn_args(&mut func, None, args, target, span);
        CheckedExpr::new(
            ret,
            CheckedExprData::call(&mut self.proj.types, func, args, self.current),
        )
    }

    fn check_arg<T>(
        &mut self,
        func: &mut WithTypeArgs<T>,
        expr: Expr,
        ty: TypeId,
    ) -> (CheckedExpr, bool) {
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
                self.proj.diag.error(Error::type_mismatch(
                    target,
                    expr.ty,
                    &self.proj.scopes,
                    &mut self.proj.types,
                    span,
                ));
                (Default::default(), true)
            }
        }
    }

    fn check_fn_args(
        &mut self,
        func: &mut GenericFn,
        recv: Option<CheckedExpr>,
        args: Vec<(Option<String>, Expr)>,
        target: Option<TypeId>,
        span: Span,
    ) -> (IndexMap<String, CheckedExpr>, TypeId, bool) {
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
            result.insert(THIS_PARAM.into(), recv);
            last_pos += 1;
        }

        let variadic = self.proj.scopes.get(func.id).variadic;
        let mut num = 0;
        let mut failed = false;
        for (name, expr) in args {
            if let Some(name) = name {
                match result.entry(name.clone()) {
                    Entry::Occupied(_) => {
                        failed = true;
                        self.error(Error::new(
                            format!("duplicate arguments for for parameter '{name}'"),
                            expr.span,
                        ))
                    }
                    Entry::Vacant(entry) => {
                        if let Some(param) = self
                            .proj
                            .scopes
                            .get(func.id)
                            .params
                            .iter()
                            .find(|p| p.label == name)
                        {
                            let (expr, f) = self.check_arg(func, expr, param.ty);
                            entry.insert(expr);
                            failed = failed || f;
                        } else {
                            failed = true;
                            self.error(Error::new(
                                format!("unknown parameter: '{name}'"),
                                expr.span,
                            ))
                        }
                    }
                }
            } else if let Some((i, param)) = self
                .proj
                .scopes
                .get(func.id)
                .params
                .iter()
                .enumerate()
                .skip(last_pos)
                .find(|(_, param)| !param.keyword)
            {
                let name = param.label.clone();
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
                result.insert(format!("${num}"), self.check_expr(expr, None));
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
                result.insert(param.label.clone(), expr.clone());
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

                missing.push_str(&param.label);
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
        if self.proj.scopes.get(func.id).is_unsafe && self.safety != Safety::Unsafe {
            self.error(Error::is_unsafe(span))
        }

        (
            result,
            self.proj
                .scopes
                .get(func.id)
                .ret
                .with_templates(&mut self.proj.types, &func.ty_args),
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
                self.error(Error::new(
                    format!(
                        "cannot infer type for type parameter '{}'",
                        self.proj.scopes.get(id).name
                    ),
                    span,
                ))
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
                self.proj.diag.error(Error::doesnt_implement(
                    &ty.name(&self.proj.scopes, &mut self.proj.types),
                    &bound.name(&self.proj.scopes, &mut self.proj.types),
                    span,
                ));
            }
        }
        failed
    }

    fn check_block(&mut self, body: Vec<Stmt>) -> Vec<CheckedStmt> {
        // TODO: do this in forward decl pass
        let declared: Vec<_> = body
            .into_iter()
            .map(|stmt| self.declare_stmt(&mut vec![], stmt))
            .collect();
        declared
            .into_iter()
            .map(|stmt| self.check_stmt(stmt))
            .collect()
    }

    fn create_block(&mut self, body: Vec<Stmt>, kind: ScopeKind) -> Block {
        self.create_block_with_init(body, kind, |_| {})
    }

    fn create_block_with_init(
        &mut self,
        body: Vec<Stmt>,
        kind: ScopeKind,
        init: impl FnOnce(&mut Self),
    ) -> Block {
        self.enter(kind, |this| {
            init(this);
            Block {
                body: this.check_block(body),
                scope: this.current,
            }
        })
    }

    fn type_check(&mut self, expr: Expr, target: TypeId) -> CheckedExpr {
        let span = expr.span;
        let source = self.check_expr(expr, Some(target));
        self.type_check_checked(source, target, span)
    }

    fn type_check_checked(
        &mut self,
        source: CheckedExpr,
        target: TypeId,
        span: Span,
    ) -> CheckedExpr {
        match self.coerce(source, target) {
            Ok(expr) => expr,
            Err(expr) => bail!(
                self,
                Error::type_mismatch(
                    target,
                    expr.ty,
                    &self.proj.scopes,
                    &mut self.proj.types,
                    span,
                )
            ),
        }
    }

    fn resolve_lang_type(&mut self, name: &str, args: &[TypeHint]) -> TypeId {
        let span = Span::default(); // FIXME: be at least somewhat related to the cause
        if let Some(id) = self.proj.scopes.lang_types.get(name).copied() {
            let ty_args = self.resolve_type_args(id, args, true, span);
            self.proj
                .types
                .insert(Type::User(GenericUserType::new(id, ty_args)))
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
            self.check_bounds(
                &ty.ty_args,
                param,
                self.proj.scopes.get(id).impls.clone(),
                span,
            );
        }
        self.proj.types.insert(Type::User(ty))
    }

    fn make_lang_type_by_name(
        &mut self,
        name: &str,
        args: impl IntoIterator<Item = TypeId>,
        span: Span,
    ) -> TypeId {
        let Some(id) = self.proj.scopes.lang_types.get(name).copied() else {
            return self.error(Error::no_lang_item(name, span));
        };

        self.proj
            .types
            .insert(Type::User(GenericUserType::from_type_args(
                &self.proj.scopes,
                id,
                args,
            )))
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
                            &format!(
                                "type '{}'",
                                ut.name(&self.proj.scopes, &mut self.proj.types)
                            ),
                            path.final_component_span(),
                        )
                    )
                }
            }
            ResolvedType::Builtin(ty) => bail!(
                self,
                Error::expected_found(
                    "trait",
                    &format!(
                        "type '{}'",
                        ty.name(&self.proj.scopes, &mut self.proj.types)
                    ),
                    path.final_component_span(),
                )
            ),
            ResolvedType::Error => None,
        }
    }

    fn resolve_typehint(&mut self, hint: &TypeHint) -> TypeId {
        let mut create_ptr = |init: fn(TypeId) -> Type, hint: &TypeHint| {
            let ty = self.resolve_typehint(hint);
            self.proj.types.insert(init(ty))
        };

        match hint {
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
                                &format!(
                                    "trait '{}'",
                                    ut.name(&self.proj.scopes, &mut self.proj.types)
                                ),
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
            TypeHint::DynPtr(path) => self
                .resolve_dyn_ptr(path)
                .map(|tr| self.proj.types.insert(Type::DynPtr(tr)))
                .unwrap_or_default(),
            TypeHint::DynMutPtr(path) => self
                .resolve_dyn_ptr(path)
                .map(|tr| self.proj.types.insert(Type::DynMutPtr(tr)))
                .unwrap_or_default(),
            &TypeHint::This(span) => {
                let current = self.current_function();
                for (_, scope) in self.proj.scopes.walk(self.current) {
                    match scope.kind {
                        ScopeKind::UserType(id) => {
                            check_hover!(self, span, id.into());
                            match &self.proj.scopes.get(id).kind {
                                &UserTypeKind::Trait(this) => {
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
                self.error(Error::new(format!("'{THIS_TYPE}' outside of type"), span))
            }
            TypeHint::Array(ty, count) => {
                let n = match self.consteval(count, Some(TypeId::USIZE)) {
                    Ok(n) => n,
                    Err(err) => return self.error(err),
                };
                let id = self.resolve_typehint(ty);
                self.proj.types.insert(Type::Array(id, n))
            }
            TypeHint::Option(ty) => self.resolve_lang_type("option", std::slice::from_ref(ty)),
            TypeHint::Vec(ty) => self.resolve_lang_type("vec", std::slice::from_ref(ty)),
            TypeHint::Map(key, val) => {
                self.resolve_lang_type("map", &[(**key).clone(), (**val).clone()])
            }
            TypeHint::Set(ty) => self.resolve_lang_type("set", std::slice::from_ref(ty)),
            TypeHint::Slice(ty) => self.resolve_lang_type("span", std::slice::from_ref(ty)),
            TypeHint::SliceMut(ty) => self.resolve_lang_type("span_mut", std::slice::from_ref(ty)),
            TypeHint::Tuple(params) => {
                let params = params.iter().map(|p| self.resolve_typehint(p)).collect();
                self.proj.scopes.get_tuple(params, &mut self.proj.types)
            }
            TypeHint::AnonStruct(params) => {
                let mut types = Vec::with_capacity(params.len());
                let mut names = Vec::with_capacity(params.len());
                for (name, ty) in params {
                    names.push(name.clone());
                    types.push(self.resolve_typehint(ty));
                }
                self.proj
                    .scopes
                    .get_anon_struct(names, types, &mut self.proj.types)
            }
            TypeHint::Fn {
                is_extern: _,
                params,
                ret,
            } => {
                let fnptr = FnPtr {
                    params: params.iter().map(|p| self.resolve_typehint(p)).collect(),
                    ret: self.resolve_typehint(ret),
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
            self.proj.scopes.get_mut(id).kind = UserTypeKind::Union(union);
        }

        let data = &mut self.proj.scopes.get_mut(id).item;
        if let UserTypeKind::PackedStruct(kind) = &mut data.kind {
            let mut bits = 0;
            for (name, mem) in data.members.iter() {
                kind.bit_offsets.insert(name.clone(), bits);
                // TODO: allow *raw and nested packed structs
                match self.proj.types[mem.ty] {
                    Type::Int(n) | Type::Uint(n) => bits += n,
                    Type::CInt(n) | Type::CUint(n) => bits += n.size() as u32 * 8,
                    Type::Bool => bits += 1,
                    Type::Unknown | Type::Unresolved(_) => {}
                    _ => self.proj.diag.error(Error::new(
                        format!("member '{name}' of packed struct must have integer type"),
                        mem.span,
                    )),
                }
            }

            const MAX_ALIGN_BITS: u32 = crate::typeid::MAX_ALIGN as u32 * 8;
            if bits < MAX_ALIGN_BITS {
                kind.size = crate::nearest_pow_of_two(bits) / 8;
            } else {
                kind.size = ((((bits / MAX_ALIGN_BITS) + 1) * MAX_ALIGN_BITS) / 8) as usize;
            }
            kind.align = kind.size.clamp(1, crate::typeid::MAX_ALIGN);
        }

        data.members_resolved = true;
    }

    fn resolve_dependencies(&mut self, ut: UserTypeId, this: TypeId, mut canonical: bool) -> bool {
        match self.proj.deps.get(&this) {
            Some(Dependencies::Resolved(_)) => return false,
            Some(_) => return true,
            None => {}
        }

        self.proj.deps.insert(this, Dependencies::Resolving);
        let mut deps = Vec::new();
        let mut failed = false;

        if !canonical {
            let canonical_ty = Type::User(GenericUserType::from_id(
                &self.proj.scopes,
                &mut self.proj.types,
                ut,
            ));
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

        for i in 0..self.proj.scopes.get(ut).members.len() {
            check_ty!(
                self.proj.scopes.get(ut).members[i].ty,
                Error::recursive_type(
                    self.proj.scopes.get(ut).members.get_index(i).unwrap().0,
                    self.proj.scopes.get(ut).members[i].span,
                    false,
                )
            );
        }

        if let Some(union) = self.proj.scopes.get(ut).kind.as_union().cloned() {
            check_ty!(
                union.tag,
                Error::new(
                    "union tag makes this struct recursive",
                    self.proj.scopes.get(ut).name.span
                )
            );
            for (name, var) in union.variants.iter() {
                if let Some(ty) = var.ty {
                    check_ty!(ty, Error::recursive_type(name, var.span, true));
                }
            }
        }

        if failed {
            self.proj.deps.insert(this, Dependencies::Recursive);
            if canonical {
                self.proj.scopes.get_mut(ut).recursive = true;
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
                if member_deps.iter().any(|&v| v == ut) {
                    return true;
                }

                deps.extend(member_deps);
            }
        }

        false
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
            if let Some(id) = self.proj.scopes.get_mut(id).impls[i]
                .as_checked()
                .map(|tr| tr.id)
            {
                self.resolve_impls(id);
            }
        }
    }

    fn resolve_proto(&mut self, id: FunctionId) {
        // disable errors to avoid duplicate errors when the struct and the constructor
        // are typechecked
        let prev = self
            .proj
            .diag
            .set_errors_enabled(self.proj.scopes.get(id).constructor.is_none());
        for i in 0..self.proj.scopes.get(id).params.len() {
            let target = resolve_type!(self, self.proj.scopes.get_mut(id).params[i].ty);
            match std::mem::take(&mut self.proj.scopes.get_mut(id).params[i].default) {
                Some(DefaultExpr::Unchecked(scope, expr)) => {
                    let prev = self.proj.diag.set_errors_enabled(true);
                    self.enter_id_and_resolve(scope, |this| {
                        this.proj.scopes.get_mut(id).params[i].default =
                            Some(DefaultExpr::Checked(this.type_check(expr, target)));
                    });
                    self.proj.diag.set_errors_enabled(prev);
                }
                other => self.proj.scopes.get_mut(id).params[i].default = other,
            }
        }

        resolve_type!(self, self.proj.scopes.get_mut(id).ret);

        for i in 0..self.proj.scopes.get(id).type_params.len() {
            self.resolve_impls(self.proj.scopes.get(id).type_params[i]);
        }

        self.proj.diag.set_errors_enabled(prev);
    }

    fn has_direct_impl(&mut self, ut: &GenericUserType, bound: &GenericTrait) -> bool {
        for i in 0..self.proj.scopes.get(ut.id).impls.len() {
            resolve_impl!(self, self.proj.scopes.get_mut(ut.id).impls[i]);
            if let Some(mut tr) = self.proj.scopes.get(ut.id).impls[i].as_checked().cloned() {
                tr.fill_templates(&mut self.proj.types, &ut.ty_args);
                if &tr == bound {
                    return true;
                }
            }
        }
        false
    }

    fn implements_trait(&mut self, ty: TypeId, bound: &GenericTrait) -> bool {
        if ty == TypeId::UNKNOWN
            || self
                .proj
                .scopes
                .has_builtin_impl(&self.proj.types, ty, bound)
        {
            return true;
        }

        if let Type::User(ut) = &self.proj.types[ty] {
            if self.has_direct_impl(&ut.clone(), bound) {
                return true;
            }
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
            if this
                .proj
                .scopes
                .has_builtin_impl(&this.proj.types, ty, bound)
            {
                return true;
            }

            if let Type::User(ut) = &this.proj.types[ty] {
                if this.has_direct_impl(&ut.clone(), bound) {
                    return true;
                }
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
                *this
                    .proj
                    .scopes
                    .get_mut(ext)
                    .kind
                    .as_extension_mut()
                    .unwrap()
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

        let exts: Vec<_> = self
            .proj
            .scopes
            .walk(scope)
            .flat_map(|(_, scope)| {
                scope.tns.iter().flat_map(|s| {
                    s.1.as_type()
                        .filter(|&&id| self.proj.scopes.get(id).kind.is_extension())
                        .cloned()
                })
            })
            .collect();
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

        cache
            .into_iter()
            .flat_map(|mut map| map.remove(&ty).flatten())
            .collect()
    }

    fn get_trait_impl(&mut self, ty: TypeId, id: TraitId) -> Option<GenericTrait> {
        fn get_trait_impl_helper(
            this: &mut TypeChecker,
            id: UserTypeId,
            target: TraitId,
        ) -> Option<GenericTrait> {
            for i in 0..this.proj.scopes.get(id).impls.len() {
                resolve_impl!(this, this.proj.scopes.get_mut(id).impls[i]);
                if let Some(tr) = this.proj.scopes.get(id).impls[i]
                    .as_checked()
                    .filter(|tr| tr.id == target)
                    .cloned()
                {
                    return Some(tr);
                }
            }

            None
        }

        if let Type::User(ut) = &self.proj.types[ty] {
            if let Some(ut) = get_trait_impl_helper(self, ut.id, id) {
                return Some(ut);
            }
        }

        for ext in self.extensions_in_scope_for(ty, self.current) {
            if let Some(ut) = get_trait_impl_helper(self, ext.id, id) {
                return Some(ut);
            }
        }

        None
    }

    pub(crate) fn get_member_fn_ex(
        &mut self,
        inst: TypeId,
        wanted_tr: Option<TraitId>,
        method: &str,
        scope: ScopeId,
        finish: impl FnOnce(&mut Self, FunctionId) -> TypeArgs + Clone,
    ) -> Option<MemberFn> {
        fn fn_is_impl(
            this: &mut TypeChecker,
            wanted_tr: Option<TraitId>,
            func: FunctionId,
        ) -> bool {
            let Some(wanted_tr) = wanted_tr else {
                return true;
            };

            let scope = this.proj.scopes.get(func).scope;
            if let Some(mut imp) = this.proj.scopes[scope].kind.as_impl().cloned() {
                let prev = this.proj.diag.set_errors_enabled(false);
                resolve_impl!(this, imp);
                this.proj.diag.set_errors_enabled(prev);
                let res = matches!(&imp, TraitImpl::Checked(tr) if tr.id == wanted_tr);
                this.proj.scopes[scope].kind = ScopeKind::Impl(imp);
                res
            } else {
                false
            }
        }

        fn search(
            scopes: &Scopes,
            funcs: &[Vis<FunctionId>],
            method: &str,
        ) -> Option<Vis<FunctionId>> {
            funcs
                .iter()
                .find(|&&id| (scopes.get(*id).name.data == method))
                .copied()
        }

        fn search_extensions(
            this: &mut TypeChecker,
            inst: TypeId,
            wanted_tr: Option<TraitId>,
            method: &str,
            scope: ScopeId,
            finish: impl FnOnce(&mut TypeChecker, FunctionId) -> TypeArgs + Clone,
        ) -> Option<MemberFn> {
            for ext in this.extensions_in_scope_for(inst, scope) {
                let src_scope = this.proj.scopes.get(ext.id).scope;
                if let Some(f) =
                    search(&this.proj.scopes, &this.proj.scopes.get(ext.id).fns, method)
                {
                    if fn_is_impl(this, wanted_tr, *f) {
                        let mut func = GenericFn::new(f.id, finish(this, f.id));
                        func.ty_args.copy_args(&ext.ty_args);
                        return Some(MemberFn {
                            func,
                            owner: src_scope,
                            dynamic: false,
                            public: f.public,
                        });
                    }
                }

                for tr in this
                    .proj
                    .scopes
                    .get(ext.id)
                    .impls
                    .iter()
                    .flat_map(|ut| ut.as_checked())
                {
                    for imp in this.proj.scopes.get_trait_impls(tr.id) {
                        if wanted_tr.is_some_and(|id| id != imp) {
                            continue;
                        }

                        if let Some(f) =
                            search(&this.proj.scopes, &this.proj.scopes.get(imp).fns, method)
                        {
                            let ty_args = tr.ty_args.clone();
                            let mut func = GenericFn::new(f.id, finish(this, f.id));
                            if let Type::User(ut) = &this.proj.types[inst] {
                                let ut = ut.clone();
                                func.ty_args.copy_args_with(
                                    &mut this.proj.types,
                                    &ty_args,
                                    &ut.ty_args,
                                );
                            }
                            func.ty_args.copy_args_with(
                                &mut this.proj.types,
                                &ty_args,
                                &ext.ty_args,
                            );
                            func.ty_args
                                .insert(*this.proj.scopes.get(imp).kind.as_trait().unwrap(), inst);

                            return Some(MemberFn {
                                func,
                                owner: src_scope,
                                dynamic: false,
                                public: f.public,
                            });
                        }
                    }
                }
            }

            None
        }

        // TODO: trait implement overload ie.
        // impl Eq<f32> { ... } impl Eq<i32> { ... }
        if let Type::User(ut) = &self.proj.types[inst] {
            let ut = ut.clone();
            let src_scope = self.proj.scopes.get(ut.id).scope;
            if let Some(f) = search(&self.proj.scopes, &self.proj.scopes.get(ut.id).fns, method) {
                if fn_is_impl(self, wanted_tr, *f) {
                    let mut func = GenericFn::new(f.id, finish(self, f.id));
                    func.ty_args.copy_args(&ut.ty_args);
                    return Some(MemberFn {
                        func,
                        owner: src_scope,
                        dynamic: false,
                        public: f.public,
                    });
                }
            }

            // TODO: search all concrete impls from all extensions first, then search trait impls
            if let Some(res) =
                search_extensions(self, inst, wanted_tr, method, scope, finish.clone())
            {
                return Some(res);
            }

            self.resolve_impls_recursive(ut.id);
            for tr in self
                .proj
                .scopes
                .get(ut.id)
                .impls
                .iter()
                .flat_map(|ut| ut.as_checked())
            {
                for imp in self.proj.scopes.get_trait_impls(tr.id) {
                    if wanted_tr.is_some_and(|id| id != imp) {
                        continue;
                    }

                    if let Some(f) =
                        search(&self.proj.scopes, &self.proj.scopes.get(imp).fns, method)
                    {
                        let ty_args = tr.ty_args.clone();
                        let mut func = GenericFn::new(f.id, finish(self, f.id));
                        func.ty_args
                            .copy_args_with(&mut self.proj.types, &ty_args, &ut.ty_args);
                        func.ty_args
                            .insert(*self.proj.scopes.get(imp).kind.as_trait().unwrap(), inst);
                        return Some(MemberFn {
                            func,
                            owner: src_scope,
                            dynamic: false,
                            public: f.public,
                        });
                    }
                }
            }

            return None;
        } else if let Some(tr) = self.proj.types[inst].as_dyn_pointee() {
            // TODO: wanted_tr
            let tr = tr.clone();
            self.resolve_impls_recursive(tr.id);
            let data = self.proj.scopes.get(tr.id);
            for imp in self.proj.scopes.get_trait_impls(tr.id) {
                if let Some(f) = search(&self.proj.scopes, &self.proj.scopes.get(imp).fns, method) {
                    let src_scope = data.scope;
                    let mut func = GenericFn::new(f.id, finish(self, f.id));
                    func.ty_args.copy_args(&tr.ty_args);
                    return Some(MemberFn {
                        func,
                        owner: src_scope,
                        dynamic: true,
                        public: f.public,
                    });
                }
            }
        }

        search_extensions(self, inst, wanted_tr, method, scope, finish)
    }

    fn get_member_fn(
        &mut self,
        ty: TypeId,
        wanted_tr: Option<TraitId>,
        method: &str,
        generics: &[TypeHint],
        span: Span,
        scope: ScopeId,
    ) -> Option<MemberFn> {
        self.get_member_fn_ex(ty, wanted_tr, method, scope, |this, id| {
            this.resolve_type_args(id, generics, false, span)
        })
        .inspect(|memfn| self.resolve_proto(memfn.func.id))
    }

    fn get_int_type_and_val(
        &mut self,
        target: Option<TypeId>,
        IntPattern {
            negative,
            base,
            value,
            width,
        }: &IntPattern,
        span: Span,
    ) -> (TypeId, BigInt) {
        let ty = if let Some(width) = width {
            if let Some(ty) = Type::from_int_name(width, false) {
                self.proj.types.insert(ty)
            } else {
                return self.error(Error::new(
                    format!("invalid integer literal type: {width}"),
                    span,
                ));
            }
        } else {
            target
                .map(|target| target.strip_options(&self.proj.scopes, &self.proj.types))
                .filter(|&target| self.proj.types[target].is_integral())
                .unwrap_or(TypeId::ISIZE)
        };

        let stats = self.proj.types[ty].as_integral().unwrap();
        let mut parsable = value.clone();
        parsable.retain(|c| c != '_');
        let mut result = match BigInt::from_str_radix(&parsable, *base as u32) {
            Ok(result) => result,
            Err(e) => {
                return self.error(Error::new(
                    format!("integer literal '{value}' could not be parsed: {e}"),
                    span,
                ));
            }
        };
        if *negative {
            result = -result;
            if !stats.signed {
                self.proj.diag.error(Error::new(
                    format!(
                        "cannot negate unsigned integer type '{}'",
                        ty.name(&self.proj.scopes, &mut self.proj.types)
                    ),
                    span,
                ));
            }
        }

        let min = stats.min();
        let max = stats.max();
        if result > max || result < min {
            bail!(
                self,
                Error::new(
                    format!(
                    "integer literal does not fit in range for type '{}' (range is {min}..{max})",
                    ty.name(&self.proj.scopes, &mut self.proj.types),
                ),
                    span,
                )
            );
        }

        (ty, result)
    }

    fn consteval(&mut self, expr: &Expr, target: Option<TypeId>) -> Result<usize, Error> {
        match &expr.data {
            ExprData::Integer(patt) => {
                let (ty, val) = self.get_int_type_and_val(target, patt, expr.span);
                if let Some(target) = target.filter(|&target| target != ty) {
                    return Err(Error::type_mismatch(
                        target,
                        ty,
                        &self.proj.scopes,
                        &mut self.proj.types,
                        expr.span,
                    ));
                }

                return match val.try_into() {
                    Ok(value) => Ok(value),
                    Err(_) => Err(Error::new("value cannot be converted to uint", expr.span)),
                };
            }
            ExprData::Binary { op, left, right } => {
                let lhs = self.consteval(left, None)?;
                let rhs = self.consteval(right, None)?;
                return Ok(match op {
                    BinaryOp::Add => lhs + rhs,
                    BinaryOp::Sub => lhs - rhs,
                    BinaryOp::Mul => lhs * rhs,
                    BinaryOp::Div => lhs / rhs,
                    BinaryOp::Rem => lhs % rhs,
                    BinaryOp::BitAnd => lhs & rhs,
                    BinaryOp::Xor => lhs ^ rhs,
                    BinaryOp::BitOr => lhs | rhs,
                    BinaryOp::Shl => lhs << rhs,
                    BinaryOp::Shr => lhs >> rhs,
                    op => {
                        return Err(Error::invalid_operator(
                            op,
                            &TypeId::USIZE.name(&self.proj.scopes, &mut self.proj.types),
                            expr.span,
                        ))
                    }
                });
            }
            ExprData::Call { callee, args: _ } => {
                if let ExprData::Path(path) = &callee.data {
                    match self.resolve_value_path(path) {
                        ResolvedValue::Fn(func) => {
                            if self.proj.scopes.intrinsics.contains_key(&func.id)
                                && self.proj.scopes.get(func.id).name.data == "size_of"
                            {
                                // TODO: make sure the first type arg has had resolve_members()
                                // and resolve_dependencies() called on it
                                return Ok(func
                                    .first_type_arg()
                                    .unwrap()
                                    .size_and_align(&self.proj.scopes, &mut self.proj.types)
                                    .0);
                            }
                        }
                        ResolvedValue::NotFound(err) => return Err(err),
                        _ => {}
                    }
                }
            }
            _ => {}
        }

        Err(Error::new(
            "expression is not compile time evaluatable",
            expr.span,
        ))
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

    fn loop_out_type(&mut self, kind: &ScopeKind, span: Span) -> (TypeId, bool) {
        let ScopeKind::Loop(LoopScopeKind {
            target,
            breaks,
            infinite,
            label: _,
        }) = kind
        else {
            panic!("ICE: target of loop changed from loop to something else");
        };

        if *infinite {
            match breaks {
                LoopBreak::None => (TypeId::NEVER, false),
                _ => (target.unwrap(), false),
            }
        } else {
            match breaks {
                LoopBreak::WithValue => (
                    self.make_lang_type_by_name("option", [(*target).unwrap()], span),
                    true,
                ),
                _ => (TypeId::VOID, false),
            }
        }
    }

    pub fn coerce(
        &mut self,
        mut expr: CheckedExpr,
        target: TypeId,
    ) -> Result<CheckedExpr, CheckedExpr> {
        fn may_ptr_coerce(types: &Types, lhs: &Type, rhs: &Type) -> bool {
            match (lhs, rhs) {
                (Type::MutPtr(s), Type::Ptr(t) | Type::RawPtr(t)) if s == t => true,
                (Type::MutPtr(s), Type::RawPtr(t) | Type::MutPtr(t) | Type::Ptr(t)) => {
                    may_ptr_coerce(types, &types[*s], &types[*t])
                }
                (Type::Ptr(s), Type::Ptr(t)) => may_ptr_coerce(types, &types[*s], &types[*t]),
                _ => false,
            }
        }

        match (&self.proj.types[expr.ty], &self.proj.types[target]) {
            (Type::Never, Type::Never) => Ok(expr),
            (Type::Never, _) => Ok(CheckedExpr::new(
                target,
                CheckedExprData::NeverCoerce(expr.into()),
            )),
            (Type::Unknown, _) | (_, Type::Unknown) => {
                expr.ty = target;
                Ok(expr)
            }
            (&Type::Ptr(lhs), Type::DynPtr(rhs)) => {
                if self.implements_trait(lhs, &rhs.clone()) {
                    Ok(CheckedExpr::new(
                        target,
                        CheckedExprData::DynCoerce {
                            expr: expr.into(),
                            scope: self.current,
                        },
                    ))
                } else {
                    Err(expr)
                }
            }
            (Type::MutPtr(lhs), Type::DynPtr(rhs) | Type::DynMutPtr(rhs)) => {
                if self.implements_trait(*lhs, &rhs.clone()) {
                    Ok(CheckedExpr::new(
                        target,
                        CheckedExprData::DynCoerce {
                            expr: expr.into(),
                            scope: self.current,
                        },
                    ))
                } else {
                    Err(expr)
                }
            }
            (Type::Fn(lhs), Type::FnPtr(rhs)) => {
                let rhs = rhs.clone();
                let fptr = lhs
                    .clone()
                    .as_fn_ptr(&self.proj.scopes, &mut self.proj.types);
                if fptr == rhs {
                    Ok(CheckedExpr::new(
                        self.proj.types.insert(Type::FnPtr(fptr)),
                        expr.data,
                    ))
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
                        Ok(expr) => Ok(CheckedExpr::new(
                            target,
                            CheckedExprData::VariantInstance {
                                members: [("0".into(), expr)].into(),
                                variant: "Some".into(),
                            },
                        )),
                        Err(expr) => Err(expr),
                    }
                } else if self.can_span_coerce(expr.ty, target).is_some() {
                    Ok(CheckedExpr::new(
                        target,
                        CheckedExprData::SpanMutCoerce(expr.into()),
                    ))
                } else {
                    Err(expr)
                }
            }
        }
    }

    pub fn try_coerce(&mut self, expr: CheckedExpr, target: TypeId) -> CheckedExpr {
        match self.coerce(expr, target) {
            Ok(expr) => expr,
            Err(expr) => expr,
        }
    }

    fn can_span_coerce(&self, lhs: TypeId, rhs: TypeId) -> Option<()> {
        let span = *self.proj.scopes.lang_types.get("span")?;
        let span_mut = *self.proj.scopes.lang_types.get("span_mut")?;
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
        name: Located<String>,
        ty: TypeId,
        mutable: bool,
        has_hint: bool,
    ) -> VariableId {
        let id = self.insert(
            Variable {
                public: false,
                name,
                ty,
                is_static: false,
                mutable,
                value: None,
                unused: typ != PatternType::BodylessFn,
                has_hint,
            },
            false,
            typ != PatternType::Regular,
        );
        if self.current_expr == self.listening_expr {
            self.listening_vars.push(id);
        }
        id
    }

    fn check_match_coverage<'a>(
        &mut self,
        ty: TypeId,
        mut patterns: impl Iterator<Item = &'a CheckedPattern> + Clone,
        span: Span,
    ) {
        let ty = &self.proj.types[ty.strip_references(&self.proj.types)];
        if let Some((mut value, max)) = ty
            .as_integral()
            .map(|int| (int.min(), int.max()))
            .or_else(|| {
                ty.is_char()
                    .then(|| (BigInt::default(), BigInt::from(char::MAX as u32)))
            })
            .or_else(|| ty.is_bool().then(|| (BigInt::default(), BigInt::from(1))))
        {
            'outer: while value <= max {
                if ty.is_char() && (0xd800.into()..=0xe000.into()).contains(&value) {
                    value = 0xe000.into();
                }

                for patt in patterns.clone() {
                    if patt.irrefutable {
                        return;
                    }

                    match &patt.data {
                        CheckedPatternData::Int(i) if i == &value => {
                            value += 1;
                            continue 'outer;
                        }
                        CheckedPatternData::IntRange(i) => {
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
        } else if ty
            .as_user()
            .is_some_and(|ut| Some(&ut.id) == self.proj.scopes.lang_types.get("string"))
        {
            if !patterns.any(|patt| patt.irrefutable) {
                self.error(Error::match_statement("", span))
            }
        } else if ty.as_user().is_some_and(|ut| {
            Some(&ut.id) == self.proj.scopes.lang_types.get("span")
                || Some(&ut.id) == self.proj.scopes.lang_types.get("span_mut")
        }) {
            if !patterns.any(|patt| {
                patt.irrefutable
                    || match &patt.data {
                        CheckedPatternData::Span { patterns, rest, .. } => {
                            patterns.is_empty() && rest.is_some()
                        }
                        _ => false,
                    }
            }) {
                self.error(Error::match_statement("", span))
            }
        } else if let Some(union) = ty
            .as_user()
            .and_then(|ut| self.proj.scopes.get(ut.id).kind.as_union())
        {
            let mut missing = vec![];
            'outer: for (name, _) in union.variants.iter() {
                for patt in patterns.clone() {
                    if patt.irrefutable {
                        return;
                    } else if patt.data.as_variant().is_some_and(|(sub, variant, _, _)| {
                        name == variant && sub.as_ref().map_or(true, |sub| sub.irrefutable)
                    }) {
                        continue 'outer;
                    }
                }

                missing.push(&name[..]);
            }

            if !missing.is_empty() {
                return self.error(Error::match_statement(
                    &format!("(missing variant(s) {})", missing.join(", ")),
                    span,
                ));
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
    ) -> Result<(Option<TypeId>, String), Option<Error>> {
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
                format!(
                    "cannot use union pattern on type '{}'",
                    scrutinee.name(&self.proj.scopes, &mut self.proj.types)
                ),
                span,
            )));
        };
        self.resolve_members(ut_id);
        let f = match path {
            ResolvedValue::Fn(f) => f,
            ResolvedValue::MemberFn(m) => m.func,
            ResolvedValue::UnionConstructor(ut) => {
                return Err(Some(Error::type_mismatch_s(
                    &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                    &ut.name(&self.proj.scopes, &mut self.proj.types),
                    span,
                )))
            }
            ResolvedValue::Var(id) => {
                return Err(Some(Error::expected_found(
                    &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                    &format!("variable '{}'", self.proj.scopes.get(id).name),
                    span,
                )))
            }
            ResolvedValue::NotFound(err) => return Err(Some(err)),
            ResolvedValue::Error => return Err(None),
        };

        let f = self.proj.scopes.get(f.id);
        if f.constructor.is_some_and(|id| id == ut_id) {
            let variant = f.name.data.clone();
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
        } else if f
            .constructor
            .is_some_and(|id| self.proj.scopes.get(id).kind.is_union())
        {
            self.proj.diag.error(Error::type_mismatch_s(
                &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                &f.ret.name(&self.proj.scopes, &mut self.proj.types),
                span,
            ));
            Err(Default::default())
        } else if f.constructor.is_some() {
            Err(Some(Error::type_mismatch_s(
                &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                &f.ret.name(&self.proj.scopes, &mut self.proj.types),
                span,
            )))
        } else {
            Err(Some(Error::expected_found(
                &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                &format!("function '{}'", f.name.data),
                span,
            )))
        }
    }

    fn check_int_pattern(
        &mut self,
        target: TypeId,
        patt: &IntPattern,
        span: Span,
    ) -> Option<BigInt> {
        let inner = target.strip_references(&self.proj.types);
        if !self.proj.types[inner].is_integral() {
            let (ty, _) = self.get_int_type_and_val(None, patt, span);
            if ty == TypeId::UNKNOWN {
                return None;
            }

            bail!(
                self,
                Error::type_mismatch(target, ty, &self.proj.scopes, &mut self.proj.types, span,)
            );
        }

        let (ty, value) = self.get_int_type_and_val(Some(inner), patt, span);
        if ty != inner {
            bail!(
                self,
                Error::type_mismatch(inner, ty, &self.proj.scopes, &mut self.proj.types, span,)
            );
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
    ) -> CheckedPattern {
        let mut rest = None;
        let mut result = Vec::new();
        for (i, patt) in patterns.into_iter().enumerate() {
            if let Pattern::Rest(var) = patt.data {
                let id = var.map(|(mutable, name)| {
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
                result.push(self.check_pattern(PatternParams {
                    binding: true,
                    scrutinee: inner_ptr,
                    mutable: false,
                    pattern: patt,
                    typ,
                    has_hint: false,
                }));
            }
        }

        if let Some(RestPattern { id: Some(id), .. }) = &rest {
            self.proj.scopes.get_mut(*id).item.ty =
                self.proj
                    .types
                    .insert(Type::User(GenericUserType::from_type_args(
                        &self.proj.scopes,
                        span_id,
                        [span_inner],
                    )));
        }

        CheckedPattern::refutable(CheckedPatternData::Span {
            rest,
            patterns: result,
            inner: span_inner,
        })
    }

    fn check_array_pattern(
        &mut self,
        target: TypeId,
        patterns: Vec<Located<Pattern>>,
        span: Span,
        typ: PatternType,
    ) -> CheckedPattern {
        let span_id = self.proj.scopes.lang_types.get("span").copied();
        let span_mut_id = self.proj.scopes.lang_types.get("span_mut").copied();
        let (real_inner, arr_len) = match self
            .proj
            .types
            .get(target.strip_references(&self.proj.types))
        {
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
                            target.name(&self.proj.scopes, &mut self.proj.types)
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
                let id = var.map(|(mutable, name)| {
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
                    binding: true,
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
            let arr = self
                .proj
                .types
                .insert(Type::Array(real_inner, arr_len - result.len()));
            self.proj.scopes.get_mut(*id).item.ty =
                target.matched_inner_type(&mut self.proj.types, arr);
        }

        CheckedPattern {
            irrefutable,
            data: CheckedPatternData::Array {
                patterns: ArrayPattern {
                    rest,
                    arr_len,
                    inner: real_inner,
                    patterns: result,
                },
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
    ) -> CheckedPattern {
        let stripped = scrutinee.strip_references(&self.proj.types);
        let Some(ut) = self.proj.types[stripped].as_user().filter(|ut| {
            matches!(
                self.proj.scopes.get(ut.id).kind,
                UserTypeKind::Struct | UserTypeKind::Union(_) | UserTypeKind::AnonStruct
            )
        }) else {
            bail!(
                self,
                Error::bad_destructure(
                    &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                    span,
                )
            );
        };
        let ut_id = ut.id;
        self.resolve_members(ut_id);

        let cap = self.can_access_privates(self.proj.scopes.get(ut_id).scope);
        let mut irrefutable = true;
        let mut checked = Vec::new();

        for Destructure {
            name,
            mutable: pm,
            pattern,
        } in destructures
        {
            let Some(member) = self.proj.scopes.get(ut_id).members.get(&name.data) else {
                self.proj.diag.error(Error::no_member(
                    &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                    &name.data,
                    name.span,
                ));
                continue;
            };

            if !member.public && !cap {
                self.proj.diag.error(Error::private_member(
                    &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                    &name.data,
                    name.span,
                ));
            }

            // TODO: duplicates
            let inner = member.ty.with_ut_templates(&mut self.proj.types, stripped);
            let scrutinee = scrutinee.matched_inner_type(&mut self.proj.types, inner);
            let patt = self.check_pattern(PatternParams {
                binding: true,
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

        CheckedPattern {
            irrefutable,
            data: CheckedPatternData::Destrucure {
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
    ) -> CheckedPattern {
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
                    &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
                    &format!("({})", ["_"].repeat(subpatterns.len()).join(", ")),
                    span,
                )
            );
        };

        if ut.ty_args.len() != subpatterns.len() {
            self.proj.diag.error(Error::expected_found(
                &scrutinee.name(&self.proj.scopes, &mut self.proj.types),
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
                binding: true,
                scrutinee: ty,
                mutable,
                pattern: patt,
                typ,
                has_hint: false,
            });
            if !patt.irrefutable {
                irrefutable = false;
            }
            checked.push((format!("{i}"), inner, patt));
        }

        CheckedPattern {
            irrefutable,
            data: CheckedPatternData::Destrucure {
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
    ) -> CheckedPattern {
        match self.get_union_variant(scrutinee, resolved, span) {
            Ok((Some(scrutinee), variant)) => {
                CheckedPattern::refutable(CheckedPatternData::Variant {
                    pattern: Some(
                        self.check_tuple_pattern(scrutinee, mutable, subpatterns, span, typ)
                            .into(),
                    ),
                    variant,
                    inner: TypeId::UNKNOWN,
                    borrows: self.proj.types[scrutinee].is_any_ptr(),
                })
            }
            Ok((None, _)) => self.error(Error::expected_found(
                "tuple variant pattern",
                "empty variant pattern",
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
    ) -> CheckedPattern {
        match self.get_union_variant(scrutinee, resolved, span) {
            Ok((Some(_), _)) => self.error(Error::expected_found(
                "empty variant pattern",
                "variant pattern",
                span,
            )),
            Ok((None, variant)) => CheckedPattern::refutable(CheckedPatternData::Variant {
                pattern: None,
                variant,
                inner: TypeId::UNKNOWN,
                borrows: false,
            }),
            Err(Some(err)) => self.error(err),
            Err(None) => Default::default(),
        }
    }

    fn check_pattern(
        &mut self,
        PatternParams {
            binding,
            scrutinee,
            mutable,
            pattern,
            typ,
            has_hint,
        }: PatternParams,
    ) -> CheckedPattern {
        let span = pattern.span;
        match pattern.data {
            Pattern::TupleLike { path, subpatterns } => {
                let value = self.resolve_value_path(&path);
                self.check_tuple_union_pattern(scrutinee, mutable, value, subpatterns, span, typ)
            }
            Pattern::StructLike { path, subpatterns } => {
                let value = self.resolve_value_path(&path);
                match self.get_union_variant(scrutinee, value, span) {
                    Ok((Some(scrutinee), variant)) => {
                        CheckedPattern::refutable(CheckedPatternData::Variant {
                            pattern: Some(
                                self.check_struct_pattern(
                                    scrutinee,
                                    mutable,
                                    subpatterns,
                                    span,
                                    typ,
                                )
                                .into(),
                            ),
                            variant,
                            inner: TypeId::UNKNOWN,
                            borrows: self.proj.types[scrutinee].is_any_ptr(),
                        })
                    }
                    Ok((None, _)) => self.error(Error::expected_found(
                        "tuple variant pattern",
                        "empty variant pattern",
                        span,
                    )),
                    Err(Some(err)) => self.error(err),
                    _ => Default::default(),
                }
            }
            Pattern::Path(path) => {
                let value = self.resolve_value_path(&path);
                if let Some(ident) = path.as_identifier() {
                    match self.get_union_variant(scrutinee, value, span) {
                        Ok(_) if binding => self.error(Error::new(
                            "cannot create binding that shadows union variant",
                            span,
                        )),
                        Ok((Some(_), _)) => self.error(Error::expected_found(
                            "empty variant pattern",
                            "tuple variant pattern",
                            span,
                        )),
                        Ok((None, variant)) => {
                            CheckedPattern::refutable(CheckedPatternData::Variant {
                                pattern: None,
                                variant,
                                inner: TypeId::UNKNOWN,
                                borrows: false,
                            })
                        }
                        Err(Some(_)) => CheckedPattern::irrefutable(CheckedPatternData::Variable(
                            self.insert_pattern_var(
                                typ,
                                Located::new(span, ident.into()),
                                scrutinee,
                                mutable,
                                has_hint,
                            ),
                        )),
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
                    &[(Located::new(pattern.span, "Some".into()), vec![])],
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
            Pattern::Null => {
                let Some(id) = self.proj.scopes.get_option_id() else {
                    return self.error(Error::no_lang_item("option", pattern.span));
                };
                let value = self.resolve_value_path_in(
                    &[(Located::new(pattern.span, "None".into()), vec![])],
                    Default::default(),
                    self.proj.scopes.get(id).body_scope,
                    pattern.span,
                );
                self.check_empty_union_pattern(scrutinee, value, pattern.span)
            }
            Pattern::MutBinding(name) => CheckedPattern::irrefutable(CheckedPatternData::Variable(
                self.insert_pattern_var(typ, Located::new(span, name), scrutinee, true, has_hint),
            )),
            Pattern::Struct(sub) => self.check_struct_pattern(scrutinee, mutable, sub, span, typ),
            Pattern::String(value) => {
                let string = self.make_lang_type_by_name("string", [], span);
                if scrutinee.strip_references(&self.proj.types) != string {
                    bail!(
                        self,
                        Error::type_mismatch(
                            scrutinee,
                            string,
                            &self.proj.scopes,
                            &mut self.proj.types,
                            span,
                        )
                    );
                }

                CheckedPattern::refutable(CheckedPatternData::String(value))
            }
            Pattern::Int(patt) => CheckedPattern::refutable(
                self.check_int_pattern(scrutinee, &patt, span)
                    .map(CheckedPatternData::Int)
                    .unwrap_or_default(),
            ),
            Pattern::IntRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                let start = if let Some(start) = start {
                    let Some(start) = self.check_int_pattern(scrutinee, &start, span) else {
                        return Default::default();
                    };
                    Some(start)
                } else {
                    None
                };

                let Some(end) = self.check_int_pattern(scrutinee, &end, span) else {
                    return Default::default();
                };

                if start.as_ref().is_some_and(|start| start > &end) {
                    return self.error(Error::new(
                        "range start must be less than or equal to its end",
                        span,
                    ));
                }

                CheckedPattern::refutable(CheckedPatternData::IntRange(RangePattern {
                    inclusive,
                    start,
                    end,
                }))
            }
            Pattern::Char(ch) => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::CHAR {
                    bail!(
                        self,
                        Error::type_mismatch(
                            scrutinee,
                            TypeId::CHAR,
                            &self.proj.scopes,
                            &mut self.proj.types,
                            span,
                        )
                    );
                }

                CheckedPattern::refutable(CheckedPatternData::Int(BigInt::from(ch as u32)))
            }
            Pattern::CharRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::CHAR {
                    bail!(
                        self,
                        Error::type_mismatch(
                            scrutinee,
                            TypeId::CHAR,
                            &self.proj.scopes,
                            &mut self.proj.types,
                            span,
                        )
                    );
                }

                if start.is_some_and(|start| start > end) {
                    return self.error(Error::new(
                        "range pattern end cannot be greater than its start",
                        span,
                    ));
                }

                CheckedPattern::refutable(CheckedPatternData::IntRange(RangePattern {
                    inclusive,
                    start: start.map(|start| BigInt::from(start as u32)),
                    end: BigInt::from(end as u32),
                }))
            }
            Pattern::Rest { .. } => self.error(Error::new(
                "rest patterns are only valid inside array or span patterns",
                span,
            )),
            Pattern::Array(sub) => self.check_array_pattern(scrutinee, sub, span, typ),
            Pattern::Bool(val) => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::BOOL {
                    bail!(
                        self,
                        Error::type_mismatch(
                            scrutinee,
                            TypeId::BOOL,
                            &self.proj.scopes,
                            &mut self.proj.types,
                            span,
                        )
                    );
                }

                CheckedPattern::refutable(CheckedPatternData::Int(BigInt::from(val as u32)))
            }
            Pattern::Tuple(sub) => self.check_tuple_pattern(scrutinee, mutable, sub, span, typ),
            Pattern::Void => {
                if scrutinee.strip_references(&self.proj.types) != TypeId::VOID {
                    bail!(
                        self,
                        Error::type_mismatch(
                            scrutinee,
                            TypeId::VOID,
                            &self.proj.scopes,
                            &mut self.proj.types,
                            span,
                        )
                    );
                }

                CheckedPattern::irrefutable(CheckedPatternData::Void)
            }
            Pattern::Error => Default::default(),
        }
    }

    fn check_full_pattern(
        &mut self,
        scrutinee: TypeId,
        pattern: Located<FullPattern>,
    ) -> CheckedPattern {
        self.check_pattern(PatternParams {
            binding: false,
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
    fn find_in_tns(&self, name: &str) -> Option<Vis<TypeItem>> {
        for (id, scope) in self.proj.scopes.walk(self.current) {
            if let Some(item) = self.proj.scopes[id].find_in_tns(name) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        None
    }

    fn find_in_vns(&self, name: &str) -> Option<Vis<ValueItem>> {
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

        None
    }

    fn get_super(&mut self, span: Span) -> Option<ScopeId> {
        if let Some(module) = self.proj.scopes.module_of(
            self.proj.scopes[self.proj.scopes.module_of(self.current).unwrap()]
                .parent
                .unwrap(),
        ) {
            Some(module)
        } else {
            self.error(Error::new("cannot use super here", span))
        }
    }

    fn resolve_use(
        &mut self,
        UsePath {
            public,
            origin,
            components,
            tail,
        }: &UsePath,
    ) -> Result<(), Error> {
        match origin {
            PathOrigin::Root => self.resolve_use_in(ScopeId::ROOT, *public, components, tail),
            PathOrigin::Super(span) => self
                .get_super(*span)
                .map(|id| self.resolve_use_in(id, *public, components, tail))
                .unwrap_or(Ok(())),
            PathOrigin::Normal => {
                if let Some((first, rest)) = components.split_first() {
                    match self.find_in_tns(&first.data).map(|i| i.id) {
                        Some(TypeItem::Module(next)) => {
                            self.check_hover(first.span, next.into());
                            return self.resolve_use_in(next, *public, rest, tail);
                        }
                        Some(TypeItem::Type(id)) => {
                            self.check_hover(first.span, id.into());
                            if rest.is_empty() {
                                self.resolve_use_union(*public, id, first, tail);
                            } else {
                                self.error(Error::new("expected module name", first.span))
                            }
                            return Ok(());
                        }
                        _ => {}
                    }
                }
                self.resolve_use_in(ScopeId::ROOT, *public, components, tail)
            }
        }
    }

    fn resolve_use_in(
        &mut self,
        mut scope: ScopeId,
        public: bool,
        components: &[Located<String>],
        tail: &UsePathTail,
    ) -> Result<(), Error> {
        for (i, comp) in components.iter().enumerate() {
            match self.proj.scopes[scope].find_in_tns(&comp.data).as_deref() {
                Some(&TypeItem::Module(next)) => {
                    self.check_hover(comp.span, next.into());
                    scope = next;
                }
                Some(&TypeItem::Type(id)) => {
                    self.check_hover(comp.span, id.into());
                    if i != components.len() - 1 {
                        self.error(Error::new("expected module name", comp.span))
                    } else {
                        self.resolve_use_union(public, id, comp, tail);
                    }
                    return Ok(());
                }
                _ => return Err(Error::no_symbol(&comp.data, comp.span)),
            }
        }

        if let UsePathTail::Ident(tail) = tail {
            // TODO: check privacy
            let mut found = false;
            if let Some(item) = self.proj.scopes[scope].find_in_tns(&tail.data) {
                self.check_hover(tail.span, (*item).into());
                if !item.public && !self.can_access_privates(scope) {
                    self.error(Error::private(&tail.data, tail.span))
                }

                if self.proj.scopes[self.current]
                    .tns
                    .insert(tail.data.clone(), Vis::new(*item, public))
                    .is_some()
                {
                    self.error(Error::redefinition(&tail.data, tail.span))
                }
                found = true;
            }
            if let Some(item) = self.proj.scopes[scope].find_in_vns(&tail.data) {
                self.check_hover(
                    tail.span,
                    match item.id {
                        ValueItem::StructConstructor(id, _) => id.into(),
                        _ => (*item).into(),
                    },
                );
                if !item.public && !self.can_access_privates(scope) {
                    self.error(Error::private(&tail.data, tail.span))
                }

                if self.proj.scopes[self.current]
                    .vns
                    .insert(tail.data.clone(), Vis::new(*item, public))
                    .is_some()
                {
                    self.error(Error::redefinition(&tail.data, tail.span))
                }
                found = true;
            }

            if !found {
                return Err(Error::new(
                    format!("no symbol '{}' found in this module", tail.data),
                    tail.span,
                ));
            }
        } else {
            self.use_all(scope, public);
        }

        Ok(())
    }

    fn use_all(&mut self, scope: ScopeId, public: bool) {
        for (name, item) in self.proj.scopes[scope].tns.clone().iter() {
            if item.public || self.can_access_privates(scope) {
                self.proj.scopes[self.current]
                    .tns
                    .entry(name.clone())
                    .or_insert(Vis::new(**item, public));
            }
        }

        for (name, item) in self.proj.scopes[scope].vns.clone().iter() {
            if item.public || self.can_access_privates(scope) {
                self.proj.scopes[self.current]
                    .vns
                    .entry(name.clone())
                    .or_insert(Vis::new(**item, public));
            }
        }
    }

    fn resolve_use_union(
        &mut self,
        public: bool,
        id: UserTypeId,
        comp: &Located<String>,
        tail: &UsePathTail,
    ) {
        let ut = self.proj.scopes.get(id);
        if !ut.kind.is_union() {
            return self.error(Error::expected_found(
                "module or union type",
                &comp.data,
                comp.span,
            ));
        }

        let mut constructors = ut
            .fns
            .iter()
            .filter(|&&id| self.proj.scopes.get(*id).constructor.is_some());
        if let UsePathTail::Ident(tail) = tail {
            let Some(&id) =
                constructors.find(|&&id| self.proj.scopes.get(*id).name.data == tail.data)
            else {
                return self.error(Error::new("expected variant name", comp.span));
            };
            self.check_hover(tail.span, (*id).into());

            if self.proj.scopes[self.current]
                .vns
                .insert(tail.data.clone(), Vis::new((*id).into(), public))
                .is_some()
            {
                self.error(Error::redefinition(&tail.data, tail.span))
            }
        } else {
            for id in constructors.copied().collect::<Vec<_>>() {
                let name = self.proj.scopes.get(*id).name.data.clone();
                self.proj.scopes[self.current]
                    .vns
                    .entry(name)
                    .or_insert(Vis::new((*id).into(), public));
            }
        }
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
                if let Some(builtin) = Self::builtin_type_path(&name.data) {
                    if let Some((name, _)) = rest.first() {
                        return self.error(Error::no_symbol(&name.data, name.span));
                    }
                    return ResolvedType::Builtin(self.proj.types.insert(builtin));
                }

                match self.find_in_tns(&name.data).map(|t| t.id) {
                    Some(TypeItem::Type(id)) => {
                        self.check_hover(name.span, id.into());
                        let ty_args = self.resolve_type_args(id, ty_args, true, name.span);
                        if rest.is_empty() {
                            if self.proj.scopes.get(id).kind.is_extension() {
                                return self.error(Error::expected_found(
                                    "type",
                                    &format!("extension '{}'", self.proj.scopes.get(id).name.data),
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
            let done = i + 1 == data.len();
            if done {
                self.check_module_completions(total_span, true, scope);
            }

            let Some(item) = self.proj.scopes[scope].find_in_tns(&name.data) else {
                return self.error(Error::no_symbol(&name.data, name.span));
            };

            if !item.public && !self.can_access_privates(scope) {
                self.error(Error::private(&name.data, name.span))
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
                                &format!("extension '{}'", self.proj.scopes.get(id).name.data),
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
                        return self.error(Error::no_symbol(&name.data, name.span));
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

    fn resolve_value_path(&mut self, path: &Path) -> ResolvedValue {
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
                if let Some(builtin) = Self::builtin_type_path(&name.data) {
                    if !rest.is_empty() {
                        let builtin = self.proj.types.insert(builtin);
                        return self.resolve_value_path_from_type(builtin, rest, span);
                    }
                }

                if rest.is_empty() {
                    self.check_cursor_completions(name.span, false);
                    match self.find_in_vns(&name.data).map(|f| f.id) {
                        Some(ValueItem::Fn(id)) => {
                            self.resolve_proto(id);
                            self.check_hover(name.span, id.into());
                            ResolvedValue::Fn(GenericFn::new(
                                id,
                                self.resolve_type_args(id, ty_args, false, name.span),
                            ))
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
                    match self.find_in_tns(&name.data).map(|t| t.id) {
                        Some(TypeItem::Type(id)) => {
                            self.check_hover(name.span, id.into());
                            let mut ty_args = self.resolve_type_args(id, ty_args, false, name.span);
                            if let Some(&this) = self.proj.scopes.get(id).kind.as_trait() {
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
            let Some(item) = self.proj.scopes[scope].find_in_tns(&name.data) else {
                return ResolvedValue::NotFound(Error::no_symbol(&name.data, name.span));
            };

            if !item.public && !self.can_access_privates(scope) {
                self.error(Error::private(&name.data, name.span))
            }

            match *item {
                TypeItem::Type(id) => {
                    self.check_hover(name.span, id.into());
                    ty_args.copy_args(&self.resolve_type_args(id, args, false, name.span));

                    let ty = self.proj.scopes.get(id);
                    scope = ty.body_scope;
                    if let Some(&this) = ty.kind.as_trait() {
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

        self.check_module_completions(total_span, false, scope);
        let Some(item) = self.proj.scopes[scope].find_in_vns(&last_name.data) else {
            return ResolvedValue::NotFound(Error::no_symbol(&last_name.data, last_name.span));
        };

        if !item.public && !self.can_access_privates(scope) {
            self.error(Error::private(&last_name.data, last_name.span))
        }

        match *item {
            ValueItem::Fn(id) => {
                self.resolve_proto(id);
                self.check_hover(last_name.span, id.into());
                ty_args.copy_args(&self.resolve_type_args(id, last_args, false, last_name.span));

                ResolvedValue::Fn(GenericFn::new(id, ty_args))
            }
            ValueItem::StructConstructor(id, init) => {
                self.resolve_proto(init);
                self.check_hover(last_name.span, init.into());
                ty_args.copy_args(&self.resolve_type_args(id, last_args, false, last_name.span));
                ResolvedValue::Fn(GenericFn::new(init, ty_args))
            }
            ValueItem::UnionConstructor(id) => {
                self.check_hover(last_name.span, id.into());
                ty_args.copy_args(&self.resolve_type_args(id, last_args, false, last_name.span));
                ResolvedValue::UnionConstructor(GenericUserType::new(id, ty_args))
            }
            ValueItem::Var(id) => {
                self.check_hover(last_name.span, id.into());
                resolve_type!(self, self.proj.scopes.get_mut(id).ty);
                if !last_args.is_empty() {
                    self.error(Error::new(
                        "variables cannot be parameterized with type arguments",
                        last_name.span,
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

        let Some(mfn) = self.get_member_fn(ty, None, &name.data, args, name.span, self.current)
        else {
            return ResolvedValue::NotFound(Error::no_symbol(&name.data, name.span));
        };

        self.check_hover(name.span, mfn.func.id.into());
        if let Some((name, _)) = rest.first() {
            return ResolvedValue::NotFound(Error::no_symbol(&name.data, name.span));
        }

        if !mfn.public && !self.can_access_privates(mfn.owner) {
            self.proj.diag.error(Error::new(
                format!(
                    "cannot access private method '{}' of type '{}'",
                    self.proj.scopes.get(mfn.func.id).name.data,
                    ty.name(&self.proj.scopes, &mut self.proj.types)
                ),
                name.span,
            ));
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
                format!(
                    "expected {} type argument(s), received {}",
                    params.len(),
                    args.len()
                ),
                span,
            ))
        }

        let ty_args = TypeArgs(
            params
                .iter()
                .cloned()
                .zip(
                    args.iter()
                        .map(|ty| self.resolve_typehint(ty))
                        .take(params.len())
                        .chain(
                            std::iter::repeat(TypeId::UNKNOWN)
                                .take(params.len().checked_sub(args.len()).unwrap_or_default()),
                        ),
                )
                .collect(),
        );
        for (&id, &ty) in ty_args.iter() {
            self.check_bounds(&ty_args, ty, self.proj.scopes.get(id).impls.clone(), span);
        }

        ty_args
    }

    fn builtin_type_path(name: &str) -> Option<Type> {
        match name {
            "void" => Some(Type::Void),
            "never" => Some(Type::Never),
            "f32" => Some(Type::F32),
            "f64" => Some(Type::F64),
            "bool" => Some(Type::Bool),
            "char" => Some(Type::Char),
            "c_void" => Some(Type::CVoid),
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

fn discriminant_bits(count: usize) -> u32 {
    (count as f64).log2().ceil() as u32
}

static BINARY_OP_TRAITS: LazyLock<HashMap<BinaryOp, (&str, &str)>> = LazyLock::new(|| {
    [
        (BinaryOp::Cmp, ("op_cmp", "cmp")),
        (BinaryOp::Gt, ("op_cmp", "gt")),
        (BinaryOp::GtEqual, ("op_cmp", "ge")),
        (BinaryOp::Lt, ("op_cmp", "lt")),
        (BinaryOp::LtEqual, ("op_cmp", "le")),
        (BinaryOp::Equal, ("op_eq", "eq")),
        (BinaryOp::NotEqual, ("op_eq", "ne")),
        (BinaryOp::Add, ("op_add", "add")),
        (BinaryOp::Sub, ("op_sub", "sub")),
        (BinaryOp::Mul, ("op_mul", "mul")),
        (BinaryOp::Div, ("op_div", "div")),
        (BinaryOp::Rem, ("op_rem", "rem")),
        (BinaryOp::BitAnd, ("op_and", "bit_and")),
        (BinaryOp::BitOr, ("op_or", "bit_or")),
        (BinaryOp::Xor, ("op_xor", "xor")),
        (BinaryOp::Shl, ("op_shl", "shl")),
        (BinaryOp::Shr, ("op_shr", "shr")),
    ]
    .into()
});

static UNARY_OP_TRAITS: LazyLock<HashMap<UnaryOp, (&str, &str)>> = LazyLock::new(|| {
    [
        (UnaryOp::Neg, ("op_neg", "neg")),
        (UnaryOp::Not, ("op_not", "not")),
        (UnaryOp::Unwrap, ("op_unwrap", "unwrap")),
        (UnaryOp::PostDecrement, ("op_dec", "dec")),
        (UnaryOp::PreDecrement, ("op_dec", "dec")),
        (UnaryOp::PostIncrement, ("op_inc", "inc")),
        (UnaryOp::PreIncrement, ("op_inc", "inc")),
    ]
    .into()
});
