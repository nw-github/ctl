use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use indexmap::{map::Entry, IndexMap};
use num_bigint::BigInt;
use num_traits::Num;

use crate::{
    ast::{checked::*, declared::*, parsed::*, Attributes, BinaryOp, UnaryOp},
    error::{Diagnostics, Error},
    lexer::{Located, Span},
    sym::*,
    typeid::{
        CInt, FnPtr, GenericFunc, GenericTrait, GenericUserType, Type, TypeArgs, WithTypeArgs,
    },
    Compiler, THIS_PARAM, THIS_TYPE,
};

macro_rules! resolve_type {
    ($self: expr, $ty: expr) => {
        $ty = match std::mem::take(&mut $ty) {
            Type::Unresolved(hint) => {
                $self.enter_id_and_resolve(hint.1, |this| this.resolve_typehint(hint.0))
            }
            ty => ty,
        };
    };
}

macro_rules! resolve_impl {
    ($self: expr, $tr: expr) => {
        $tr = match std::mem::take(&mut $tr) {
            TraitImpl::Unchecked { scope, path, block } => {
                $self.enter_id_and_resolve(scope, |this| match this.resolve_type_path(&path) {
                    ResolvedType::Trait(tr) => TraitImpl::Checked(tr, block),
                    ResolvedType::UserType(ut) => this.error(Error::expected_found(
                        "trait",
                        &format!("type '{}'", ut.name(&this.scopes)),
                        path.span,
                    )),
                    ResolvedType::Builtin(ty) => this.error(Error::expected_found(
                        "trait",
                        &format!("type '{}'", ty.name(&this.scopes)),
                        path.span,
                    )),
                    ResolvedType::Error => Default::default(),
                })
            }
            other => other,
        };
    };
}

pub struct Module {
    pub scopes: Scopes,
    pub scope: ScopeId,
}

pub struct MemberFn {
    pub func: GenericFunc,
    pub tr: Option<TraitId>,
}

#[derive(Default)]
pub enum ResolvedType {
    UserType(GenericUserType),
    Trait(GenericTrait),
    Builtin(Type),
    #[default]
    Error,
}

#[derive(Default)]
pub enum ResolvedValue {
    StructConstructor(UserTypeId, GenericFunc),
    UnionConstructor(GenericUserType),
    Func(GenericFunc),
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

pub struct TypeChecker {
    universal: Vec<ScopeId>,
    safety: Safety,
    diag: Diagnostics,
    current: ScopeId,
    scopes: Scopes,
}

impl TypeChecker {
    pub fn check(
        path: &std::path::Path,
        module: Vec<Stmt>,
        libs: Vec<PathBuf>,
        diag: Diagnostics,
    ) -> anyhow::Result<(Module, Diagnostics)> {
        let mut this = Self {
            universal: Vec::new(),
            scopes: Scopes::new(),
            safety: Safety::Safe,
            current: ScopeId::ROOT,
            diag,
        };
        for lib in libs {
            let parsed = Compiler::new(lib, this.diag).parse()?;
            this.diag = parsed.diag;
            this.check_one(&parsed.path, parsed.state.0);
        }

        let scope = this.check_one(path, module);
        for (_, var) in this
            .scopes
            .vars()
            .filter(|(_, v)| v.unused && !v.name.data.starts_with('_') && v.name.data != THIS_PARAM)
        {
            if this.scopes.walk(var.scope).any(|(id, _)| id == scope) {
                this.diag.warn(Error::new(
                    format!("unused variable: '{}'", var.name.data),
                    var.name.span,
                ));
            }
        }

        Ok((
            Module {
                scope,
                scopes: this.scopes,
            },
            this.diag,
        ))
    }

    fn check_one(&mut self, path: &std::path::Path, module: Vec<Stmt>) -> ScopeId {
        let project = crate::derive_module_name(path);
        let is_root_module = |name: &str| (path.is_file() && name == project) || name == "main";

        self.enter_mod(project.clone(), Span::default(), true, |this| {
            let mut autouse = vec![];
            let declared: Vec<_> = module
                .into_iter()
                .map(|ast| {
                    let mut declared = Vec::new();
                    match ast.data {
                        StmtData::Module { name, body, .. } if is_root_module(&name.data) => {
                            declared.extend(
                                body.into_iter()
                                    .map(|stmt| this.declare_stmt(&mut autouse, stmt)),
                            );
                        }
                        _ => declared.push(this.declare_stmt(&mut autouse, ast)),
                    }
                    declared
                })
                .collect();

            for ast in declared {
                for stmt in ast {
                    this.check_stmt(stmt);
                }
            }

            this.universal.extend(autouse);
            this.current
        })
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.diag.error(error);
        T::default()
    }

    fn current_function(&self) -> Option<FunctionId> {
        self.scopes.function_of(self.current)
    }

    fn enter<T>(&mut self, kind: ScopeKind, f: impl FnOnce(&mut Self) -> T) -> T {
        let id = self.scopes.create_scope(self.current, kind);
        self.enter_id(id, f)
    }

    fn enter_mod<T>(
        &mut self,
        name: String,
        span: Span,
        public: bool,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let id = self
            .scopes
            .create_scope(self.current, ScopeKind::Module(name.clone()));
        if self.scopes[self.current]
            .tns
            .insert(name.clone(), Vis::new(id.into(), public))
            .is_some()
        {
            self.error(Error::redefinition(&name, span))
        }
        self.enter_id(id, f)
    }

    fn insert<T: ItemId>(&mut self, value: T::Value, public: bool, no_redef: bool) -> T {
        let (item, redef) = T::insert_in(&mut self.scopes, value, public, self.current);
        if redef && no_redef {
            let name = item.name(&self.scopes);
            self.error(Error::redefinition(&name.data, name.span))
        }
        item
    }

    fn find_in_tns(&self, name: &str) -> Option<Vis<TypeItem>> {
        for (id, scope) in self.scopes.walk(self.current) {
            if let Some(item) = self.scopes[id].find_in_tns(name) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        None
    }

    fn find_in_vns(&self, name: &str) -> Option<Vis<ValueItem>> {
        for (id, scope) in self.scopes.walk(self.current) {
            if let Some(item) = self.scopes[id].find_in_vns(name) {
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

    fn can_access_privates(&self, scope: ScopeId) -> bool {
        self.scopes
            .module_of(scope)
            .map(|target| self.scopes.walk(self.current).any(|(id, _)| id == target))
            .unwrap_or_default()
    }
}

/// Forward declaration pass routines
impl TypeChecker {
    fn declare_struct(
        &mut self,
        autouse: &mut Vec<ScopeId>,
        base: Struct,
        attrs: Attributes,
    ) -> DeclaredStmt {
        let name = base.name.clone();
        let public = base.public;
        let lang_item = attrs.val("lang").map(String::from);
        let (ut, init, fns, impls) = self.enter(ScopeKind::None, |this| {
            let init = this.enter(ScopeKind::None, |this| {
                this.declare_fn(
                    autouse,
                    Fn {
                        public: base.public && !base.members.iter().any(|m| !m.public),
                        name: base.name.clone(),
                        is_async: false,
                        linkage: Linkage::Internal,
                        variadic: false,
                        is_unsafe: false,
                        type_params: vec![],
                        params: base
                            .members
                            .iter()
                            .map(|member| Param {
                                keyword: true,
                                patt: Located::new(
                                    member.name.span,
                                    Pattern::Path(Path::from(member.name.data.clone())),
                                ),
                                ty: member.ty.clone(),
                                default: member.default.clone(),
                            })
                            .collect(),
                        ret: Self::typehint_for_struct(&base.name, &base.type_params),
                        body: None,
                        attrs: Default::default(),
                    },
                )
            });
            let mut members = IndexMap::with_capacity(base.members.len());
            for member in base.members {
                let prev = members.insert(
                    member.name.data.clone(),
                    CheckedMember::new(member.public, this.declare_type_hint(member.ty)),
                );
                if prev.is_some() {
                    this.error(Error::redefinition_k(
                        "member variable",
                        &member.name.data,
                        member.name.span,
                    ))
                }
            }

            let (impls, impl_blocks) = this.declare_impl_blocks(autouse, base.impls);
            let fns: Vec<_> = base
                .functions
                .into_iter()
                .map(|f| this.declare_fn(autouse, f))
                .collect();
            let ut = UserType {
                members,
                body_scope: this.current,
                name: base.name,
                data: UserTypeData::Struct,
                type_params: this.declare_type_params(base.type_params),
                impls,
                attrs,
                fns: fns
                    .iter()
                    .chain(impl_blocks.iter().flat_map(|block| block.fns.iter()))
                    .chain(std::iter::once(&init))
                    .map(|f| Vis::new(f.id, f.public))
                    .collect(),
            };

            (ut, init, fns, impl_blocks)
        });

        let scope = ut.body_scope;
        let id = self.insert::<UserTypeId>(ut, public, true);
        if let Some(name) = lang_item {
            self.scopes.lang_types.insert(name, id);
        }
        let prev = self.scopes[self.current].vns.insert(
            name.data,
            Vis::new(ValueItem::StructConstructor(id, init.id), base.public),
        );
        if prev.is_some() {
            self.error(Error::redefinition(
                &self.scopes.get(id).name.data,
                name.span,
            ))
        }

        self.scopes[scope].kind = ScopeKind::UserType(id);
        self.scopes.get_mut(init.id).constructor = Some(id);

        DeclaredStmt::Struct {
            init,
            id,
            impls,
            fns,
        }
    }

    fn declare_union(
        &mut self,
        autouse: &mut Vec<ScopeId>,
        tag: Option<Located<Path>>,
        base: Struct,
        variants: Vec<Variant>,
        attrs: Attributes,
    ) -> DeclaredStmt {
        let lang_item = attrs.val("lang").map(String::from);
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
                    patt: Located::new(
                        member.name.span,
                        Pattern::Path(Path::from(member.name.data)),
                    ),
                    ty: member.ty,
                    default: member.default,
                });
            }

            let (impls, impl_blocks) = this.declare_impl_blocks(autouse, base.impls);
            let ret = Self::typehint_for_struct(&base.name, &base.type_params);
            for variant in variants {
                let mut params = params.clone();
                match variant.data {
                    VariantData::Empty => {
                        rvariants.insert(variant.name.data.clone(), None);
                    }
                    VariantData::StructLike(smembers) => {
                        rvariants.insert(
                            variant.name.data.clone(),
                            Some(
                                this.declare_type_hint(TypeHint::AnonStruct(
                                    smembers
                                        .iter()
                                        .map(|m| (m.name.data.clone(), m.ty.clone()))
                                        .collect(),
                                )),
                            ),
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
                                    member.name.span,
                                    Pattern::Path(Path::from(member.name.data)),
                                ),
                                ty: member.ty,
                                default: member.default,
                            });
                        }
                    }
                    VariantData::TupleLike(members) => {
                        rvariants.insert(
                            variant.name.data.clone(),
                            Some(this.declare_type_hint(TypeHint::Tuple(
                                members.iter().map(|(ty, _)| ty.clone()).collect(),
                            ))),
                        );

                        for (i, (ty, default)) in members.into_iter().enumerate() {
                            params.push(Param {
                                keyword: false,
                                patt: Located::new(
                                    Span::default(),
                                    Pattern::Path(Path::from(format!("{i}"))),
                                ),
                                ty,
                                default,
                            });
                        }
                    }
                }

                fns.push(this.declare_fn(
                    autouse,
                    Fn {
                        public: base.public,
                        name: Located::new(variant.name.span, variant.name.data),
                        linkage: Linkage::Internal,
                        is_async: false,
                        variadic: false,
                        is_unsafe: false,
                        type_params: vec![],
                        params,
                        ret: ret.clone(),
                        body: None,
                        attrs: Default::default(),
                    },
                ));
            }
            let member_cons_len = fns.len();
            fns.extend(
                base.functions
                    .into_iter()
                    .map(|f| this.declare_fn(autouse, f)),
            );

            let ut = UserType {
                members,
                name: base.name,
                body_scope: this.current,
                type_params: this.declare_type_params(base.type_params),
                data: UserTypeData::Union(Union {
                    tag: tag
                        .map(|tag| this.declare_type_hint(TypeHint::Regular(tag)))
                        .unwrap_or(Type::Uint(discriminant_bits(rvariants.len()))),
                    variants: rvariants,
                }),
                impls,
                attrs,
                fns: fns
                    .iter()
                    .chain(impl_blocks.iter().flat_map(|block| block.fns.iter()))
                    .map(|f| Vis::new(f.id, f.public))
                    .collect(),
            };

            (ut, impl_blocks, fns, member_cons_len)
        });
        let scope = ut.body_scope;
        let id = self.insert::<UserTypeId>(ut, base.public, true);
        if let Some(name) = lang_item {
            self.scopes.lang_types.insert(name, id);
        }
        self.scopes[scope].kind = ScopeKind::UserType(id);
        for c in fns.iter().take(member_cons_len) {
            self.scopes.get_mut(c.id).constructor = Some(id);
        }

        DeclaredStmt::Union { id, impls, fns }
    }

    fn declare_unsafe_union(
        &mut self,
        autouse: &mut Vec<ScopeId>,
        base: Struct,
        attrs: Attributes,
    ) -> DeclaredStmt {
        let lang_item = attrs.val("lang").map(String::from);
        let name = base.name.clone();
        let public = base.public;
        let (ut, fns, impls) = self.enter(ScopeKind::None, |this| {
            let mut members = IndexMap::with_capacity(base.members.len());
            for member in base.members {
                let prev = members.insert(
                    member.name.data.clone(),
                    CheckedMember::new(member.public, this.declare_type_hint(member.ty)),
                );
                if prev.is_some() {
                    this.error(Error::redefinition_k(
                        "member variable",
                        &member.name.data,
                        member.name.span,
                    ))
                }
            }

            let (impls, impl_blocks) = this.declare_impl_blocks(autouse, base.impls);
            let fns: Vec<_> = base
                .functions
                .into_iter()
                .map(|f| this.declare_fn(autouse, f))
                .collect();
            let ut = UserType {
                members,
                body_scope: this.current,
                name: base.name,
                data: UserTypeData::UnsafeUnion,
                type_params: this.declare_type_params(base.type_params),
                impls,
                attrs,
                fns: fns
                    .iter()
                    .chain(impl_blocks.iter().flat_map(|block| block.fns.iter()))
                    .map(|f| Vis::new(f.id, f.public))
                    .collect(),
            };

            (ut, fns, impl_blocks)
        });

        let scope = ut.body_scope;
        let id = self.insert::<UserTypeId>(ut, public, true);
        if let Some(name) = lang_item {
            self.scopes.lang_types.insert(name, id);
        }
        let prev = self.scopes[self.current].vns.insert(
            name.data,
            Vis::new(ValueItem::UnionConstructor(id), base.public),
        );
        if prev.is_some() {
            self.error(Error::redefinition(
                &self.scopes.get(id).name.data,
                name.span,
            ))
        }
        self.scopes[scope].kind = ScopeKind::UserType(id);

        DeclaredStmt::Union { id, impls, fns }
    }

    fn declare_stmt(&mut self, autouse: &mut Vec<ScopeId>, stmt: Stmt) -> DeclaredStmt {
        match stmt.data {
            StmtData::Module { public, name, body } => {
                self.enter_mod(name.data, name.span, public, |this| {
                    let core = this.scopes[ScopeId::ROOT]
                        .find_in_tns("core")
                        .and_then(|inner| inner.as_module().copied());
                    let std = this.scopes[ScopeId::ROOT]
                        .find_in_tns("std")
                        .and_then(|inner| inner.as_module().copied());
                    if stmt.attrs.iter().any(|attr| attr.name.data == "autouse") {
                        if this
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
            StmtData::Struct(base) => self.declare_struct(autouse, base, stmt.attrs),
            StmtData::Union {
                tag,
                base,
                variants,
            } => self.declare_union(autouse, tag, base, variants, stmt.attrs),
            StmtData::UnsafeUnion(base) => self.declare_unsafe_union(autouse, base, stmt.attrs),
            StmtData::Trait {
                public,
                name,
                type_params,
                impls,
                functions,
                is_unsafe: _,
            } => {
                let lang_item = stmt.attrs.val("lang").map(String::from);
                let (tr, fns) = self.enter(ScopeKind::None, |this| {
                    let impls: Vec<_> = impls
                        .into_iter()
                        .map(|path| TraitImpl::Unchecked {
                            scope: this.current,
                            path,
                            block: None,
                        })
                        .collect();
                    let this_ty = this.insert(
                        UserType {
                            name: Located::new(name.span, THIS_TYPE.into()),
                            body_scope: this.current,
                            data: UserTypeData::Template,
                            type_params: Vec::new(),
                            impls: impls.clone(),
                            fns: vec![],
                            attrs: Default::default(),
                            members: Default::default(),
                        },
                        false,
                        false,
                    );
                    let fns: Vec<_> = functions
                        .into_iter()
                        .map(|f| this.declare_fn(autouse, f))
                        .collect();
                    let tr = Trait {
                        name,
                        body_scope: this.current,
                        type_params: this.declare_type_params(type_params),
                        impls,
                        attrs: stmt.attrs,
                        fns: fns.iter().map(|f| Vis::new(f.id, f.public)).collect(),
                        this: this_ty,
                    };
                    (tr, fns)
                });

                let scope = tr.body_scope;
                let id = self.insert::<TraitId>(tr, public, true);
                if let Some(name) = lang_item {
                    self.scopes.lang_traits.insert(name, id);
                }
                let tr = self.scopes.get(id);
                let imp = GenericTrait::from_type_params(&self.scopes, id);
                self.scopes
                    .get_mut(tr.this)
                    .impls
                    .push(TraitImpl::Checked(imp, None));

                self.scopes[scope].kind = ScopeKind::Trait(id);
                DeclaredStmt::Trait { id, fns }
            }
            StmtData::Extension {
                public,
                name,
                ty,
                type_params,
                impls,
                functions,
            } => {
                let (ext, impl_blocks, fns) = self.enter(ScopeKind::None, |this| {
                    let (impls, impl_blocks) = this.declare_impl_blocks(autouse, impls);
                    let fns: Vec<_> = functions
                        .into_iter()
                        .map(|f| this.declare_fn(autouse, f))
                        .collect();
                    let ext = Extension {
                        name,
                        ty: this.declare_type_hint(ty),
                        impls,
                        type_params: this.declare_type_params(type_params),
                        body_scope: this.current,
                        fns: fns
                            .iter()
                            .chain(impl_blocks.iter().flat_map(|block| block.fns.iter()))
                            .map(|f| Vis::new(f.id, f.public))
                            .collect(),
                    };

                    (ext, impl_blocks, fns)
                });

                let scope = ext.body_scope;
                let id = self.insert::<ExtensionId>(ext, public, true);
                self.scopes[scope].kind = ScopeKind::Extension(id);
                DeclaredStmt::Extension {
                    id,
                    impls: impl_blocks,
                    fns,
                }
            }
            StmtData::Fn(f) => DeclaredStmt::Fn(self.declare_fn(autouse, f)),
            StmtData::Static {
                public,
                name,
                ty,
                value,
            } => DeclaredStmt::Static {
                id: self.insert::<VariableId>(
                    Variable {
                        name,
                        ty: self.declare_type_hint(ty),
                        is_static: true,
                        mutable: false,
                        value: None,
                        unused: true,
                    },
                    public,
                    true,
                ),
                value,
            },
            StmtData::Use(stmt) => {
                if self.resolve_use(&stmt).is_err() {
                    self.scopes[self.current].use_stmts.push(stmt);
                }
                DeclaredStmt::None
            }
            StmtData::Let { ty, value, patt } => DeclaredStmt::Let { ty, value, patt },
            StmtData::Expr(expr) => DeclaredStmt::Expr(expr),
            StmtData::Error => DeclaredStmt::None,
        }
    }

    fn declare_fn(&mut self, autouse: &mut Vec<ScopeId>, f: Fn) -> DeclaredFn {
        let span = f.name.span;
        if f.variadic && f.linkage != Linkage::Import {
            self.error(Error::new("only import functions may be variadic", span))
        }

        if f.linkage == Linkage::Export && !f.type_params.is_empty() {
            self.error(Error::new(
                "generic functions cannot be declared 'export'",
                span,
            ))
        }

        let id = self.insert::<FunctionId>(
            Function {
                attrs: f.attrs,
                name: f.name,
                linkage: f.linkage,
                is_async: f.is_async,
                is_unsafe: f.is_unsafe,
                variadic: f.variadic,
                has_body: f.body.is_some(),
                type_params: Vec::new(),
                params: Vec::new(),
                ret: Type::Unknown,
                body: None,
                body_scope: ScopeId::ROOT,
                returns: false,
                constructor: None,
            },
            f.public,
            true,
        );

        for attr in self.scopes.get::<FunctionId>(id).attrs.clone().iter() {
            match &attr.name.data[..] {
                "lang" => {
                    let Some(inner) = attr.props.first() else {
                        self.diag
                            .error(Error::new("language item must have name", attr.name.span));
                        continue;
                    };
                    self.scopes.lang_fns.insert(inner.name.data.clone(), id);
                }
                "intrinsic" => {
                    let (name, span) = if let Some(attr) = attr.props.first() {
                        (&attr.name.data[..], attr.name.span)
                    } else {
                        (&self.scopes.get(id).name.data[..], attr.name.span)
                    };
                    match name {
                        "size_of" | "align_of" | "panic" | "binary_op" | "unary_op"
                        | "numeric_cast" | "numeric_abs" => {
                            self.scopes.intrinsics.insert(id, name.to_string());
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
            this.scopes.get_mut(id).body_scope = this.current;
            this.scopes.get_mut(id).type_params = this.declare_type_params(f.type_params);
            this.scopes.get_mut(id).params = f
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
            this.scopes.get_mut(id).ret = this.declare_type_hint(f.ret);

            DeclaredFn {
                id,
                public: f.public,
                body: f.body.map(|body| {
                    body.into_iter()
                        .map(|stmt| this.declare_stmt(autouse, stmt))
                        .collect()
                }),
            }
        })
    }

    fn declare_type_params(&mut self, vec: Vec<(String, Vec<Located<Path>>)>) -> Vec<UserTypeId> {
        vec.into_iter()
            .map(|(name, impls)| {
                self.insert(
                    UserType {
                        name: Located::new(Span::default(), name),
                        body_scope: self.current,
                        data: UserTypeData::Template,
                        type_params: Vec::new(),
                        impls: impls
                            .into_iter()
                            .map(|path| TraitImpl::Unchecked {
                                scope: self.current,
                                path,
                                block: None,
                            })
                            .collect(),
                        attrs: Default::default(),
                        fns: vec![],
                        members: Default::default(),
                    },
                    false,
                    false,
                )
            })
            .collect()
    }

    fn declare_impl_blocks(
        &mut self,
        autouse: &mut Vec<ScopeId>,
        blocks: Vec<ImplBlock>,
    ) -> (Vec<TraitImpl>, Vec<DeclaredImplBlock>) {
        let mut impls = Vec::new();
        let mut declared_blocks = Vec::new();
        for ImplBlock {
            type_params: _,
            path,
            functions,
        } in blocks
        {
            let block = self.enter(ScopeKind::None, |this| DeclaredImplBlock {
                impl_index: impls.len(),
                span: path.span,
                scope: this.current,
                fns: functions
                    .into_iter()
                    .map(|f| this.declare_fn(autouse, f))
                    .collect(),
            });
            impls.push(TraitImpl::Unchecked {
                scope: self.current,
                path,
                block: Some(block.scope),
            });
            declared_blocks.push(block);
        }

        (impls, declared_blocks)
    }

    fn declare_type_hint(&self, hint: TypeHint) -> Type {
        Type::Unresolved((hint, self.current).into())
    }

    fn typehint_for_struct(
        Located { span, data }: &Located<String>,
        type_params: &[(String, Vec<Located<Path>>)],
    ) -> TypeHint {
        TypeHint::Regular(Located::new(
            *span,
            Path::new(
                PathOrigin::Normal,
                vec![(
                    data.into(),
                    type_params
                        .iter()
                        .map(|(n, _)| TypeHint::Regular(Located::new(*span, Path::from(n.clone()))))
                        .collect(),
                )],
            ),
        ))
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
                .scopes
                .walk(this.current)
                .map(|(id, _)| id)
                .collect::<Vec<_>>()
            {
                this.enter_id(id, |this| {
                    if this.scopes[this.current].kind.is_module() {
                        for scope in this.universal.clone() {
                            this.use_all(scope, false);
                        }
                    }

                    for stmt in std::mem::take(&mut this.scopes[this.current].use_stmts) {
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
                self.enter_id_and_resolve(id, |this| Block {
                    body: body.into_iter().map(|stmt| this.check_stmt(stmt)).collect(),
                    scope: this.current,
                });
            }
            DeclaredStmt::Struct {
                init,
                id,
                impls,
                fns,
            } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.resolve_members(id);
                    this.check_fn(init);
                    this.check_impl_blocks(
                        Type::User(GenericUserType::from_id(&this.scopes, id).into()),
                        id,
                        impls,
                    );

                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Union { id, impls, fns } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.resolve_members(id);
                    this.check_impl_blocks(
                        Type::User(GenericUserType::from_id(&this.scopes, id).into()),
                        id,
                        impls,
                    );

                    for f in fns {
                        this.check_fn(f);
                    }

                    let Some(union) = &this.scopes.get(id).data.as_union() else {
                        return;
                    };
                    if let Some(stats) = union.tag.as_integral() {
                        if stats.bits < discriminant_bits(union.variants.len()) {
                            this.error(Error::new(
                                format!(
                                    "type '{}' does not have sufficient range to represent the tag for this type", 
                                    union.tag.name(&this.scopes)
                                ),
                                this.scopes.get(id).name.span,
                            ))
                        }
                    } else if !union.tag.is_unknown() {
                        this.error(Error::new(
                            "union tag must be an integer type",
                            this.scopes.get(id).name.span,
                        ))
                    }
                });
            }
            DeclaredStmt::Trait { id, fns } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(this.scopes.get(id).this);
                    // TODO: disable errors so this doesn't cause duplicate errors
                    this.resolve_impls(id);
                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Extension { id, impls, fns } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    resolve_type!(this, this.scopes.get_mut(id).ty);
                    this.resolve_impls(id);
                    this.check_impl_blocks(this.scopes.get(id).ty.clone(), id, impls);
                    for f in fns {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Expr(expr) => return CheckedStmt::Expr(self.check_expr(expr, None)),
            DeclaredStmt::Let { ty, value, patt } => {
                let span = patt.span;
                if let Some(ty) = ty {
                    let ty = self.resolve_typehint(ty);
                    if let Some(value) = value {
                        let value = self.type_check(value, &ty);
                        let patt = self.check_pattern(true, &ty, false, patt, false);
                        if !patt.irrefutable {
                            return self
                                .error(Error::must_be_irrefutable("let binding pattern", span));
                        }
                        return CheckedStmt::Let(patt, Some(value));
                    } else {
                        let patt = self.check_pattern(true, &ty, false, patt, false);
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
                    let value = self.check_expr(value, None);
                    let span = patt.span;
                    let patt = self.check_pattern(true, &value.ty, false, patt, false);
                    if !patt.irrefutable {
                        return self.error(Error::must_be_irrefutable("let binding pattern", span));
                    }

                    return CheckedStmt::Let(patt, Some(value));
                } else {
                    return self.error(Error::new("cannot infer type", patt.span));
                }
            }
            DeclaredStmt::Fn(f) => self.check_fn(f),
            DeclaredStmt::Static { id, value } => {
                // FIXME: detect cycles like static X: usize = X;
                // FIXME: non-const statics should be disallowed
                resolve_type!(self, self.scopes.get_mut(id).ty);
                let value = self.type_check(value, &self.scopes.get(id).ty.clone());
                let var = self.scopes.get_mut(id);
                var.value = Some(value);
            }
            DeclaredStmt::None => {}
        }

        CheckedStmt::None
    }

    fn signatures_match(
        scopes: &Scopes,
        tr: TraitId,
        this: &Type,
        has: FunctionId,
        wants: FunctionId,
        ty_args: &TypeArgs,
    ) -> Result<(), String> {
        let hfn = scopes.get(has);
        let wfn = scopes.get(wants);
        if wfn.is_unsafe && !hfn.is_unsafe {
            return Err(format!("function '{}' must be declared unsafe", hfn.name.data));
        }

        let mut ty_args = ty_args.clone();
        for (i, &id) in wfn.type_params.iter().enumerate() {
            ty_args.insert(
                id,
                Type::User(GenericUserType::from_id(scopes, hfn.type_params[i]).into()),
            );
        }
        ty_args.insert(scopes.get(tr).this, this.clone());

        let compare_types = |has: &Type, mut wants: Type| {
            wants.fill_templates(&ty_args);

            if has != &wants {
                Err(format!(
                    "expected '{}', found '{}'",
                    wants.name(scopes),
                    has.name(scopes),
                ))
            } else {
                Ok(())
            }
        };

        if let Err(err) = compare_types(&hfn.ret, wfn.ret.clone()) {
            return Err(format!("return type is incorrect: {err}"));
        }

        for (s, t) in hfn.params.iter().zip(wfn.params.iter().cloned()) {
            if let Err(err) = compare_types(&s.ty, t.ty) {
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
                .flat_map(|tr| tr.as_checked().map(|(a, _)| a))
                .zip(
                    t.impls
                        .iter()
                        .flat_map(|tr| tr.as_checked().map(|(a, _)| a)),
                )
            {
                for (s, t) in s.ty_args.values().zip(t.ty_args.values()) {
                    if let Err(err) = compare_types(s, t.clone()) {
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

    fn check_impl_block(&mut self, this: &Type, tr: &GenericTrait, block: DeclaredImplBlock) {
        for (dep, _) in self
            .scopes
            .get(tr.id)
            .impls
            .clone()
            .iter()
            .flat_map(|tr| tr.as_checked())
        {
            if !self.implements_trait(this, dep) {
                self.diag.error(Error::new(
                    format!(
                        "trait '{}' requires implementation of trait '{}'",
                        tr.name(&self.scopes),
                        dep.name(&self.scopes)
                    ),
                    block.span,
                ));
            }
        }

        let mut required = self.scopes.get(tr.id).fns.clone();
        for f in block.fns {
            let Located {
                span: fn_span,
                data: fn_name,
            } = self.scopes.get(f.id).name.clone();
            let lhs = f.id;

            self.check_fn(f);
            let Some(pos) = required
                .iter()
                .position(|&id| self.scopes.get(*id).name.data == fn_name)
            else {
                self.diag.error(Error::new(
                    format!(
                        "no function '{fn_name}' found in trait '{}'",
                        self.scopes.get(tr.id).name
                    ),
                    fn_span,
                ));
                continue;
            };

            let rhs = *required.swap_remove(pos);
            self.resolve_proto(rhs);
            if let Err(err) =
                Self::signatures_match(&self.scopes, tr.id, this, lhs, rhs, &tr.ty_args)
            {
                self.error(Error::new(
                    format!("invalid implementation of function '{fn_name}': {err}"),
                    fn_span,
                ))
            }
        }

        for id in required {
            if !self.scopes.get(*id).has_body {
                self.error(Error::new(
                    format!(
                        "must implement '{}::{}'",
                        tr.name(&self.scopes),
                        self.scopes.get(*id).name.data
                    ),
                    block.span,
                ))
            }
        }
    }

    fn check_fn(&mut self, DeclaredFn { id, body, .. }: DeclaredFn) {
        // TODO: disallow private type in public interface
        self.enter_id_and_resolve(self.scopes.get(id).body_scope, |this| {
            this.resolve_proto(id);
            for i in 0..this.scopes.get(id).params.len() {
                let Some(patt) = this.scopes.get_mut(id).params[i].patt.as_unchecked().cloned() else {
                    continue;
                };
                let ty = this.scopes.get(id).params[i].ty.clone();
                let span = patt.span;
                let patt = this.check_pattern(true, &ty, false, patt, body.is_none());
                if !patt.irrefutable {
                    this.error(Error::must_be_irrefutable("parameter patterns", span))
                } else {
                    this.scopes.get_mut(id).params[i].patt = ParamPattern::Checked(patt);
                }
            }

            if let Some(body) = body {
                let old_safety = std::mem::take(&mut this.safety);
                let body = body.into_iter().map(|stmt| this.check_stmt(stmt)).collect();
                let func = this.scopes.get_mut(id);
                func.body = Some(body);
                if !func.returns && !func.ret.is_void() {
                    let span = func.name.span;
                    let name = func.name.data.clone();
                    let ret = func.ret.clone().name(&this.scopes);
                    this.error(Error::new(
                        format!("function '{name}' must return a value of type '{ret}' from all code paths"),
                        span,
                    ))
                }

                this.safety = old_safety;
            }
        });
    }

    fn check_impl_blocks<T: ItemId>(&mut self, this_ty: Type, id: T, impls: Vec<DeclaredImplBlock>)
    where
        T::Value: TypeLike,
    {
        for block in impls {
            // TODO:
            // - impl type params (impl<T> Trait<T>)
            // - implement the same trait more than once
            self.enter_id(block.scope, |this| {
                if let Some((gtr, _)) =
                    this.scopes.get(id).get_impls()[block.impl_index].as_checked()
                {
                    let gtr = gtr.clone();
                    this.check_impl_block(&this_ty, &gtr, block);
                    this.scopes[this.current].kind = ScopeKind::Impl(gtr.id);
                } else {
                    for f in block.fns {
                        this.check_fn(f);
                    }
                }
            });
        }
    }

    fn check_null_coalesce(
        &mut self,
        lhs: Expr,
        rhs: Expr,
        op: BinaryOp,
        target: Option<&Type>,
        span: Span,
    ) -> CheckedExpr {
        let Some(id) = self.scopes.get_option_id() else {
            return self.error(Error::no_lang_item("option", span));
        };

        let target = if let Some(target) = target {
            Some(Type::User(
                GenericUserType::from_type_args(&self.scopes, id, [target.clone()]).into(),
            ))
        } else {
            None
        };
        let lhs_span = lhs.span;
        let lhs = self.check_expr(lhs, target.as_ref());
        let Some(target) = lhs.ty.as_option_inner(&self.scopes) else {
            if lhs.ty.is_unknown() {
                return Default::default();
            }

            return self.error(Error::invalid_operator(
                BinaryOp::NoneCoalesce,
                &lhs.ty.name(&self.scopes),
                span,
            ));
        };
        if op.is_assignment() && !lhs.is_assignable(&self.scopes) {
            // TODO: report a better error here
            self.error(Error::new("expression is not assignable", lhs_span))
        }

        let span = rhs.span;
        //
        let rhs = self.check_expr_inner(rhs, Some(target));
        let rhs = self.type_check_checked(rhs, target, span);
        CheckedExpr::new(
            target.clone(),
            CheckedExprData::Binary {
                op,
                left: lhs.into(),
                right: rhs.into(),
            },
        )
    }

    fn check_binary(
        &mut self,
        lhs: CheckedExpr,
        rhs: Expr,
        op: BinaryOp,
        span: Span,
    ) -> CheckedExpr {
        let op_traits: HashMap<BinaryOp, (&str, &str)> = [
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
            (BinaryOp::And, ("op_and", "and")),
            (BinaryOp::Or, ("op_or", "or")),
            (BinaryOp::Xor, ("op_xor", "xor")),
            (BinaryOp::Shl, ("op_shl", "shl")),
            (BinaryOp::Shr, ("op_shr", "shr")),
        ]
        .into();

        let Some(&(trait_name, fn_name)) = op_traits.get(&op) else {
            return self.error(Error::invalid_operator(
                op,
                &lhs.ty.name(&self.scopes),
                span,
            ));
        };

        let Some(tr_id) = self.scopes.lang_traits.get(trait_name).copied() else {
            return self.error(Error::no_lang_item(trait_name, span));
        };

        let stripped = lhs.ty.strip_references();
        if self.get_trait_impl(stripped, tr_id).is_none() {
            return self.error(Error::doesnt_implement(
                &stripped.name(&self.scopes),
                &self.scopes.get(tr_id).name.data,
                span,
            ));
        }
        let Ok(MemberFn { func, tr }) = self.get_member_fn(stripped, fn_name, &[], span) else {
            return Default::default();
        };
        self.resolve_proto(func.id);

        let f = self.scopes.get(func.id);
        let [p0, p1, ..] = &f.params[..] else {
            unreachable!()
        };
        let inst = stripped.clone();
        let arg0 = (p0.label.clone(), lhs.auto_deref(&p0.ty));
        let p1_ty = p1.ty.with_templates(&func.ty_args);
        let ret = f.ret.with_templates(&func.ty_args);
        let rhs_span = rhs.span;
        let rhs_name = p1.label.clone();
        let rhs = self.check_expr(rhs, Some(p1_ty.strip_references()));
        CheckedExpr::new(
            ret,
            CheckedExprData::MemberCall {
                func,
                inst,
                args: [
                    arg0,
                    (
                        rhs_name,
                        self.type_check_checked(rhs.auto_deref(&p1_ty), &p1_ty, rhs_span),
                    ),
                ]
                .into(),
                trait_id: tr,
                scope: self.current,
            },
        )
    }

    fn check_unary(&mut self, expr: CheckedExpr, op: UnaryOp, span: Span) -> CheckedExpr {
        let op_traits: HashMap<UnaryOp, (&str, &str)> = [
            (UnaryOp::Neg, ("op_neg", "neg")),
            (UnaryOp::Not, ("op_not", "not")),
            (UnaryOp::Unwrap, ("op_unwrap", "unwrap")),
            (UnaryOp::PostDecrement, ("op_post_dec", "post_dec")),
            (UnaryOp::PostIncrement, ("op_post_inc", "post_inc")),
            (UnaryOp::PreDecrement, ("op_pre_dec", "pre_dec")),
            (UnaryOp::PreIncrement, ("op_pre_dec", "pre_dec")),
        ]
        .into();

        let Some(&(trait_name, fn_name)) = op_traits.get(&op) else {
            return self.error(Error::invalid_operator(
                op,
                &expr.ty.name(&self.scopes),
                span,
            ));
        };

        let Some(tr_id) = self.scopes.lang_traits.get(trait_name).copied() else {
            return self.error(Error::no_lang_item(trait_name, span));
        };

        let stripped = expr.ty.strip_references();
        if self.get_trait_impl(stripped, tr_id).is_none() {
            return self.error(Error::doesnt_implement(
                &stripped.name(&self.scopes),
                &self.scopes.get(tr_id).name.data,
                span,
            ));
        }
        let Ok(MemberFn { func, tr }) = self.get_member_fn(stripped, fn_name, &[], span) else {
            return Default::default();
        };
        self.resolve_proto(func.id);

        let f = self.scopes.get(func.id);
        CheckedExpr::new(
            f.ret.with_templates(&func.ty_args),
            CheckedExprData::MemberCall {
                func,
                inst: stripped.clone(),
                args: [(f.params[0].label.clone(), expr.auto_deref(&f.params[0].ty))].into(),
                trait_id: tr,
                scope: self.current,
            },
        )
    }

    fn check_expr_inner(&mut self, expr: Expr, target: Option<&Type>) -> CheckedExpr {
        let span = expr.span;
        match expr.data {
            ExprData::Binary { op, left, right } => {
                if matches!(op, BinaryOp::NoneCoalesce | BinaryOp::NoneCoalesceAssign) {
                    return self.check_null_coalesce(*left, *right, op, target, span);
                }

                let left_span = left.span;
                let left = self.check_expr(*left, None);
                if left.ty.is_unknown() {
                    return Default::default();
                }

                if op.is_assignment() && !left.is_assignable(&self.scopes) {
                    // TODO: report a better error here
                    self.error(Error::new("expression is not assignable", left_span))
                }

                if op != BinaryOp::Assign && !left.ty.supports_binary(&self.scopes, op) {
                    return self.check_binary(left, *right, op, span);
                }

                match (&left.ty, op) {
                    (Type::RawPtr(_), BinaryOp::Sub) => {
                        let span = right.span;
                        let right = self.check_expr(*right, Some(&Type::Isize));
                        let right = self.try_coerce(right, &Type::Isize);
                        match &right.ty {
                            ty if &left.ty == ty => CheckedExpr::new(
                                Type::Isize,
                                CheckedExprData::Binary {
                                    op,
                                    left: left.into(),
                                    right: right.into(),
                                },
                            ),
                            ty if ty.is_integral() || ty.is_unknown() => CheckedExpr::new(
                                left.ty.clone(),
                                CheckedExprData::Binary {
                                    op,
                                    left: left.into(),
                                    right: right.into(),
                                },
                            ),
                            _ => self.error(Error::type_mismatch_s(
                                "{integer}",
                                &right.ty.name(&self.scopes),
                                span,
                            )),
                        }
                    }
                    (
                        Type::RawPtr(_),
                        BinaryOp::Add | BinaryOp::AddAssign | BinaryOp::SubAssign,
                    ) => {
                        let span = right.span;
                        let right = self.check_expr(*right, Some(&Type::Isize));
                        let right = self.try_coerce(right, &Type::Isize);
                        if !right.ty.is_integral() && !right.ty.is_unknown() {
                            self.error(Error::type_mismatch_s(
                                "{integer}",
                                &right.ty.name(&self.scopes),
                                span,
                            ))
                        }
                        CheckedExpr::new(
                            left.ty.clone(),
                            CheckedExprData::Binary {
                                op,
                                left: left.into(),
                                right: right.into(),
                            },
                        )
                    }
                    _ => {
                        let right = self.type_check(*right, &left.ty);
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
                                | BinaryOp::LogicalAnd => Type::Bool,
                                _ => left.ty.clone(),
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
                let span = expr.span;
                let (out_ty, expr) = match op {
                    UnaryOp::Deref => {
                        let expr = if let Some(target) = target {
                            self.check_expr(*expr, Some(&Type::Ptr(target.clone().into())))
                        } else {
                            self.check_expr(*expr, target)
                        };

                        match &expr.ty {
                            Type::Ptr(inner) | Type::MutPtr(inner) => ((**inner).clone(), expr),
                            Type::RawPtr(inner) => {
                                if self.safety != Safety::Unsafe {
                                    self.error(Error::is_unsafe(span))
                                }

                                ((**inner).clone(), expr)
                            }
                            Type::Unknown => return Default::default(),
                            ty => {
                                return self.error(Error::invalid_operator(
                                    op,
                                    &ty.name(&self.scopes),
                                    span,
                                ))
                            }
                        }
                    }
                    UnaryOp::Addr => {
                        let expr = self.check_expr(*expr, target.and_then(Type::as_pointee));
                        (Type::Ptr(expr.ty.clone().into()), expr)
                    }
                    UnaryOp::AddrMut => {
                        let expr = self.check_expr(*expr, target.and_then(Type::as_pointee));
                        if !expr.can_addrmut(&self.scopes) {
                            self.error(Error::new(
                                "cannot create mutable pointer to immutable memory location",
                                span,
                            ))
                        }

                        (Type::MutPtr(expr.ty.clone().into()), expr)
                    }
                    UnaryOp::Try => {
                        let expr = self.check_expr(
                            *expr,
                            target.and_then(|t| t.as_option_inner(&self.scopes)),
                        );
                        if let Some(inner) = expr.ty.as_option_inner(&self.scopes) {
                            // TODO: lambdas
                            if self
                                .current_function()
                                .and_then(|id| {
                                    self.scopes.get(id).ret.as_option_inner(&self.scopes)
                                })
                                .is_none()
                            {
                                self.error(Error::new(
                                    "operator '?' is only valid in functions that return Option",
                                    span,
                                ))
                            }

                            (inner.clone(), expr)
                        } else if expr.ty == Type::Unknown {
                            return Default::default();
                        } else {
                            return self.error(Error::invalid_operator(
                                op,
                                &expr.ty.name(&self.scopes),
                                span,
                            ));
                        }
                    }
                    _ => {
                        let span = expr.span;
                        let expr = self.check_expr(*expr, target);
                        if !expr.ty.supports_unary(&self.scopes, op) {
                            return self.check_unary(expr, op, span);
                        }

                        if matches!(
                            op,
                            UnaryOp::PostIncrement
                                | UnaryOp::PostDecrement
                                | UnaryOp::PreIncrement
                                | UnaryOp::PreDecrement
                        ) && !expr.is_assignable(&self.scopes)
                        {
                            self.error(Error::new("expression is not assignable", span))
                        }

                        (expr.ty.clone(), expr)
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
                let ty = if let Some(Type::Array(inner)) = target {
                    inner.0.clone()
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    return self.error(Error::new("cannot infer type of array literal", expr.span));
                };

                checked.extend(elements.map(|e| self.type_check(e, &ty)));
                CheckedExpr::new(
                    Type::Array(Box::new((ty, checked.len()))),
                    CheckedExprData::Array(checked),
                )
            }
            ExprData::Vec(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(vec) = self.scopes.lang_types.get("vec").copied() else {
                    return self.error(Error::no_lang_item("vec", expr.span));
                };

                let ty = if let Some(ty) = target
                    .and_then(|target| target.as_user())
                    .filter(|ut| ut.id == vec)
                    .and_then(|ut| ut.first_type_arg().cloned())
                {
                    ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    self.error(Error::new("cannot infer type of vector literal", expr.span))
                };

                checked.extend(elements.map(|e| self.type_check(e, &ty)));
                CheckedExpr::new(
                    Type::User(self.make_lang_type(vec, [ty], span).into()),
                    CheckedExprData::Vec(checked),
                )
            }
            ExprData::Set(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(set) = self.scopes.lang_types.get("set").copied() else {
                    return self.error(Error::no_lang_item("set", span));
                };

                let ty = if let Some(ty) = target
                    .and_then(|target| target.as_user())
                    .filter(|ut| ut.id == set)
                    .and_then(|ut| ut.first_type_arg().cloned())
                {
                    ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    self.error(Error::new("cannot infer type of set literal", span))
                };

                checked.extend(elements.map(|e| self.type_check(e, &ty)));
                CheckedExpr::new(
                    Type::User(self.make_lang_type(set, [ty], span).into()),
                    CheckedExprData::Set(checked),
                )
            }
            ExprData::ArrayWithInit { init, count } => {
                if let Some(Type::Array(inner)) = target {
                    let init = self.type_check(*init, &inner.0);
                    match self.consteval(&count, Some(&Type::Usize)) {
                        Ok(count) => CheckedExpr::new(
                            Type::Array(Box::new((init.ty.clone(), count))),
                            CheckedExprData::ArrayWithInit {
                                init: init.into(),
                                count,
                            },
                        ),
                        Err(err) => self.error(err),
                    }
                } else {
                    let init = self.check_expr(*init, target);
                    match self.consteval(&count, Some(&Type::Usize)) {
                        Ok(count) => CheckedExpr::new(
                            Type::Array(Box::new((init.ty.clone(), count))),
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
                let Some(vec) = self.scopes.lang_types.get("vec").copied() else {
                    return self.error(Error::no_lang_item("vec", span));
                };

                let (init, ty) = if let Some(ty) = target
                    .and_then(|target| target.as_user())
                    .filter(|ut| ut.id == vec)
                    .and_then(|ut| ut.first_type_arg().cloned())
                {
                    (self.type_check(*init, &ty), ty)
                } else {
                    let expr = self.check_expr(*init, None);
                    let ty = expr.ty.clone();
                    (expr, ty)
                };

                CheckedExpr::new(
                    Type::User(self.make_lang_type(vec, [ty], span).into()),
                    CheckedExprData::VecWithInit {
                        init: init.into(),
                        count: self.type_check(*count, &Type::Usize).into(),
                    },
                )
            }
            ExprData::Tuple(elements) => {
                let mut result_ty = Vec::with_capacity(elements.len());
                let mut result_elems = IndexMap::with_capacity(elements.len());
                for (i, expr) in elements.into_iter().enumerate() {
                    let result = if let Some(target) = target
                        .and_then(|t| t.as_user())
                        .filter(|t| self.scopes.get(t.id).data.is_tuple())
                        .and_then(|ut| ut.ty_args.get_index(i).map(|(_, v)| v))
                    {
                        self.type_check(expr, target)
                    } else {
                        self.check_expr(expr, None)
                    };

                    result_ty.push(result.ty.clone());
                    result_elems.insert(format!("{i}"), result);
                }

                CheckedExpr::new(
                    self.scopes.get_tuple(result_ty),
                    CheckedExprData::Instance {
                        members: result_elems,
                        variant: None,
                    },
                )
            }
            ExprData::Map(elements) => {
                let Some(map) = self.scopes.lang_types.get("map").copied() else {
                    return self.error(Error::no_lang_item("map", expr.span));
                };

                let mut result = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let (k, v) = if let Some(ut) = target
                    .and_then(|target| target.as_user())
                    .filter(|ut| ut.id == map)
                {
                    let mut args = ut.ty_args.values().cloned();
                    (args.next().unwrap(), args.next().unwrap())
                } else if let Some((key, val)) = elements.next() {
                    let key = self.check_expr(key, None);
                    let val = self.check_expr(val, None);

                    let k = key.ty.clone();
                    let v = val.ty.clone();
                    result.push((key, val));
                    (k, v)
                } else {
                    self.error(Error::new("cannot infer type of map literal", expr.span))
                };

                result.extend(
                    elements.map(|(key, val)| (self.type_check(key, &k), self.type_check(val, &v))),
                );
                CheckedExpr::new(
                    Type::User(self.make_lang_type(map, [k, v], span).into()),
                    CheckedExprData::Map(result),
                )
            }
            ExprData::Range {
                start,
                end,
                inclusive,
            } => match (start, end) {
                // this could be skipped by just transforming these expressions to calls
                (Some(start), Some(end)) => {
                    let start = self.check_expr(*start, None);
                    let end = self.type_check(*end, &start.ty);
                    let item = if inclusive {
                        "range_inclusive"
                    } else {
                        "range"
                    };
                    CheckedExpr::new(
                        self.make_lang_type_by_name(item, [start.ty.clone()], span),
                        CheckedExprData::Instance {
                            members: [("start".into(), start), ("end".into(), end)].into(),
                            variant: None,
                        },
                    )
                }
                (None, Some(end)) => {
                    let end = self.check_expr(*end, None);
                    let item = if inclusive {
                        "range_to_inclusive"
                    } else {
                        "range_to"
                    };
                    CheckedExpr::new(
                        self.make_lang_type_by_name(item, [end.ty.clone()], span),
                        CheckedExprData::Instance {
                            members: [("end".into(), end)].into(),
                            variant: None,
                        },
                    )
                }
                (Some(start), None) => {
                    let start = self.check_expr(*start, None);
                    CheckedExpr::new(
                        self.make_lang_type_by_name("range_from", [start.ty.clone()], span),
                        CheckedExprData::Instance {
                            members: [("start".into(), start)].into(),
                            variant: None,
                        },
                    )
                }
                (None, None) => CheckedExpr::new(
                    self.make_lang_type_by_name("range_full", [], span),
                    CheckedExprData::Instance {
                        members: Default::default(),
                        variant: None,
                    },
                ),
            },
            ExprData::String(s) => CheckedExpr::new(
                self.make_lang_type_by_name("string", [], span),
                CheckedExprData::String(s),
            ),
            ExprData::StringInterpolation(parts) => {
                let Some(write_id) = self.scopes.lang_traits.get("format").copied() else {
                    return self.error(Error::no_lang_item("Format", span));
                };
                let formatter = self
                    .scopes
                    .lang_types
                    .get("string_formatter")
                    .and_then(|&id| {
                        self.scopes[self.scopes.get(id).body_scope]
                            .find_in_vns("new")
                            .and_then(|f| f.into_fn().ok())
                            .map(|f| {
                                CheckedExpr::new(
                                    Type::User(GenericUserType::new(id, Default::default()).into()),
                                    CheckedExprData::Call {
                                        func: GenericFunc::new(f, Default::default()),
                                        args: Default::default(),
                                        scope: self.current,
                                    },
                                )
                            })
                    })
                    .unwrap_or_else(|| self.error(Error::no_lang_item("String Formatter", span)));
                let sty = self.make_lang_type_by_name("string", [], span);

                let mut out_parts = Vec::with_capacity(parts.len() * 2 + 1);
                for expr in parts {
                    let span = expr.span;
                    let expr = self.check_expr(expr, target);
                    if !expr.ty.is_unknown() && self.get_trait_impl(&expr.ty, write_id).is_none() {
                        self.diag.error(Error::doesnt_implement(
                            &expr.ty.name(&self.scopes),
                            "Format",
                            span,
                        ));
                    }
                    out_parts.push(expr);
                }

                CheckedExpr::new(
                    sty,
                    CheckedExprData::StringInterpolation {
                        parts: out_parts,
                        formatter: formatter.into(),
                        scope: self.current,
                    },
                )
            }
            ExprData::ByteString(s) => CheckedExpr::new(
                Type::Ptr(Type::Array((Type::Uint(8), s.len()).into()).into()),
                CheckedExprData::ByteString(s),
            ),
            ExprData::Char(s) => CheckedExpr::new(Type::Char, CheckedExprData::Char(s)),
            ExprData::ByteChar(c) => {
                CheckedExpr::new(Type::Uint(8), CheckedExprData::Integer(BigInt::from(c)))
            }
            ExprData::None => {
                if let Some(inner) = target.and_then(|target| target.as_option_inner(&self.scopes))
                {
                    CheckedExpr::option_null(self.make_lang_type_by_name(
                        "option",
                        [inner.clone()],
                        span,
                    ))
                } else {
                    self.error(Error::new("cannot infer type of option null literal", span))
                }
            }
            ExprData::Void => CheckedExpr::new(Type::Void, CheckedExprData::Void),
            ExprData::Bool(value) => CheckedExpr {
                ty: Type::Bool,
                data: CheckedExprData::Bool(value),
            },
            ExprData::Integer { base, value, width } => {
                let (ty, value) = self.get_int_type_and_val(target, base, width, value, span);
                CheckedExpr::new(ty, CheckedExprData::Integer(value))
            }
            ExprData::Float(value) => CheckedExpr::new(
                target
                    .filter(|t| matches!(t.strip_options(&self.scopes), Type::F32 | Type::F64))
                    .cloned()
                    .unwrap_or(Type::F64),
                CheckedExprData::Float(value),
            ),
            ExprData::Path(path) => match self.resolve_value_path(&path, span, true) {
                ResolvedValue::Var(id) => {
                    let var = self.scopes.get(id);
                    if !var.is_static
                        && self.current_function() != self.scopes.function_of(var.scope)
                    {
                        self.diag.error(Error::new(
                            "cannot reference local variable of enclosing function",
                            span,
                        ));
                    }

                    let ty = var.ty.clone();
                    self.scopes.get_mut(id).unused = false;
                    CheckedExpr::new(ty, CheckedExprData::Symbol(Symbol::Var(id), self.current))
                }
                ResolvedValue::Func(func) => CheckedExpr::new(
                    Type::FnPtr(func.as_fn_ptr(&self.scopes).into()),
                    CheckedExprData::Symbol(Symbol::Func(func), self.current),
                ),
                ResolvedValue::StructConstructor(_, func) => CheckedExpr::new(
                    Type::FnPtr(func.as_fn_ptr(&self.scopes).into()),
                    CheckedExprData::Symbol(Symbol::Func(func), self.current),
                ),
                ResolvedValue::UnionConstructor(ut) => self.error(Error::expected_found(
                    "expression",
                    &format!("type '{}'", ut.name(&self.scopes)),
                    span,
                )),
                ResolvedValue::NotFound(err) => self.error(err),
                ResolvedValue::Error => Default::default(),
            },
            ExprData::Block(body) => {
                let block = self.create_block(body, ScopeKind::Block(target.cloned(), false));
                let (target, yields) = self.scopes[block.scope].kind.as_block().unwrap();
                CheckedExpr::new(
                    yields
                        .then(|| target.clone())
                        .flatten()
                        .unwrap_or(Type::Void),
                    CheckedExprData::Block(block),
                )
            }
            ExprData::If {
                cond,
                if_branch,
                else_branch,
            } => {
                let check_if_branch = |this: &mut TypeChecker| {
                    let target = if else_branch.is_none() {
                        target.and_then(|t| t.as_option_inner(&this.scopes))
                    } else {
                        target
                    };

                    let if_span = if_branch.span;
                    let expr = this.check_expr_inner(*if_branch, target);
                    if let Some(target) = target {
                        this.type_check_checked(expr, target, if_span)
                    } else {
                        expr
                    }
                };

                let (cond, mut if_branch) = if let ExprData::Is { expr, pattern } = cond.data {
                    self.check_is_expr(*expr, pattern, |this, cond| (cond, check_if_branch(this)))
                } else {
                    let cond = self.type_check(*cond, &Type::Bool);
                    (cond, check_if_branch(self))
                };

                let mut out_type = if_branch.ty.clone();
                let else_branch = if let Some(expr) = else_branch {
                    if out_type.is_never() {
                        let expr = self.check_expr_inner(*expr, None);
                        out_type = expr.ty.clone();
                        if_branch = self.try_coerce(if_branch, &expr.ty);
                        Some(expr)
                    } else {
                        let span = expr.span;
                        let source = self.check_expr_inner(*expr, Some(&out_type));
                        Some(self.type_check_checked(source, &out_type, span))
                    }
                } else {
                    // this separates these two cases:
                    //   let x /* void? */ = if whatever { yield void; };
                    //   let x /* void */ = if whatever { };
                    if matches!(&if_branch.data, CheckedExprData::Block(b) if
                        matches!(self.scopes[b.scope].kind, ScopeKind::Block(_, yields) if yields))
                    {
                        if out_type.is_never() {
                            out_type = Type::Void;
                            Some(CheckedExpr::new(Type::Void, CheckedExprData::Void))
                        } else {
                            out_type = self.make_lang_type_by_name("option", [out_type], span);
                            if_branch = self.try_coerce(if_branch, &out_type);
                            Some(self.check_expr_inner(
                                Located::new(span, ExprData::None),
                                Some(&out_type),
                            ))
                        }
                    } else {
                        None
                    }
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
            } => {
                let infinite = cond.is_none();
                let target = Self::loop_target(&self.scopes, target, infinite);
                let (cond, body) = if let Some(cond) = cond {
                    match cond.data {
                        ExprData::Is { expr, pattern } if !do_while => {
                            self.check_is_expr(*expr, pattern, |this, cond| {
                                (
                                    Some(cond.into()),
                                    this.create_block(
                                        body,
                                        ScopeKind::Loop {
                                            target: target.cloned(),
                                            breaks: None,
                                            infinite,
                                        },
                                    ),
                                )
                            })
                        }
                        _ => (
                            Some(self.type_check(*cond, &Type::Bool).into()),
                            self.create_block(
                                body,
                                ScopeKind::Loop {
                                    target: target.cloned(),
                                    breaks: None,
                                    infinite,
                                },
                            ),
                        ),
                    }
                } else {
                    (
                        None,
                        self.create_block(
                            body,
                            ScopeKind::Loop {
                                target: target.cloned(),
                                breaks: None,
                                infinite,
                            },
                        ),
                    )
                };

                let (out_type, optional) =
                    self.loop_out_type(&self.scopes[body.scope].kind.clone(), span);
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
            ExprData::For { patt, iter, body } => {
                let span = iter.span;
                let iter = self.check_expr(*iter, None);
                let Some(iter_id) = self.scopes.lang_traits.get("iter").copied() else {
                    return self.error(Error::no_lang_item("Iterator", span));
                };
                let kind = ScopeKind::Loop {
                    target: Self::loop_target(&self.scopes, target, false).cloned(),
                    breaks: None,
                    infinite: false,
                };
                self.enter(kind, |this| {
                    let Some(ut) = this.get_trait_impl(&iter.ty, iter_id) else {
                        for stmt in body {
                            let stmt = this.declare_stmt(&mut vec![], stmt);
                            this.check_stmt(stmt);
                        }

                        return this.error(Error::doesnt_implement(
                            &iter.ty.name(&this.scopes),
                            "Iterator",
                            span,
                        ));
                    };

                    let mut next_ty = ut.first_type_arg().unwrap().clone();
                    if let Some(ut) = iter.ty.as_user() {
                        next_ty.fill_templates(&ut.ty_args);
                    }

                    let patt_span = patt.span;
                    let patt = this.check_pattern(true, &next_ty, false, patt, false);
                    if !patt.irrefutable {
                        this.error(Error::must_be_irrefutable("for patterns", patt_span))
                    }
                    let body = body
                        .into_iter()
                        .map(|stmt| {
                            let stmt = this.declare_stmt(&mut vec![], stmt);
                            this.check_stmt(stmt)
                        })
                        .collect();

                    let (out, optional) =
                        this.loop_out_type(&this.scopes[this.current].kind.clone(), span);
                    CheckedExpr::new(
                        out,
                        CheckedExprData::For {
                            patt,
                            body,
                            optional,
                            iter: iter.into(),
                            scope: this.current,
                        },
                    )
                })
            }
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
                let id = source.ty.strip_references();
                let ut_id = match &id {
                    Type::User(data) => data.id,
                    Type::Unknown => return Default::default(),
                    _ => {
                        return self.error(Error::no_member(&id.name(&self.scopes), &name, span));
                    }
                };
                self.resolve_members(ut_id);

                let ut = self.scopes.get(ut_id);
                let Some(member) = ut.members.get(&name) else {
                    return self.error(Error::no_member(
                        &source.ty.name(&self.scopes),
                        &name,
                        span,
                    ));
                };

                if ut.data.is_unsafe_union() && self.safety != Safety::Unsafe {
                    self.diag.error(Error::is_unsafe(span));
                }

                let mut ty = member.ty.clone();
                if let Some(instance) = id.as_user() {
                    ty.fill_templates(&instance.ty_args);
                }

                if !member.public && !self.can_access_privates(ut.scope) {
                    self.error(Error::private_member(&id.name(&self.scopes), &name, span))
                }

                let id = id.clone();
                CheckedExpr::new(
                    ty,
                    CheckedExprData::Member {
                        source: source.auto_deref(&id).into(),
                        member: name,
                    },
                )
            }
            ExprData::Subscript { callee, args } => {
                if args.len() > 1 {
                    self.error(Error::new(
                        "multidimensional subscript is not supported",
                        args[1].span,
                    ))
                }

                let callee = self.check_expr(*callee, None);
                let arg = self.type_check(args.into_iter().next().unwrap(), &Type::Isize);
                if let Type::Array(target) = callee.ty.strip_references() {
                    CheckedExpr::new(
                        target.0.clone(),
                        CheckedExprData::Subscript {
                            callee: callee.into(),
                            args: vec![arg],
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!(
                            "type {} cannot be subscripted",
                            &callee.ty.name(&self.scopes)
                        ),
                        span,
                    ))
                }
            }
            ExprData::Return(expr) => self.check_return(*expr),
            ExprData::Tail(expr) => match &self.scopes[self.current].kind {
                ScopeKind::Function(_) | ScopeKind::Lambda(_, _) => self.check_return(*expr),
                ScopeKind::Loop { .. } => self.type_check(*expr, &Type::Void),
                _ => self.check_yield(*expr),
            },
            ExprData::Break(expr) => {
                let Some(((target, _, &infinite), id)) = self
                    .scopes
                    .walk(self.current)
                    .find_map(|(id, scope)| scope.kind.as_loop().zip(Some(id)))
                else {
                    if let Some(expr) = expr {
                        self.check_expr(*expr, None);
                    }
                    return self.error(Error::new("break outside of loop", span));
                };
                let target = target.clone();
                let expr = if let Some(expr) = expr {
                    let span = expr.span;
                    let mut expr = self.check_expr(*expr, target.as_ref());
                    if let Some(target) = target {
                        expr = self.type_check_checked(expr, &target, span);
                        self.scopes[id].kind = ScopeKind::Loop {
                            target: Some(target),
                            breaks: Some(true),
                            infinite,
                        };
                    } else {
                        self.scopes[id].kind = ScopeKind::Loop {
                            target: Some(expr.ty.clone()),
                            breaks: Some(true),
                            infinite,
                        };
                    }

                    let (target, opt) = self.loop_out_type(&self.scopes[id].kind.clone(), span);
                    if opt {
                        Some(self.try_coerce(expr, &target).into())
                    } else {
                        Some(expr.into())
                    }
                } else {
                    self.scopes[id].kind = ScopeKind::Loop {
                        target: Some(Type::Void),
                        breaks: Some(false),
                        infinite,
                    };
                    None
                };

                CheckedExpr::new(Type::Never, CheckedExprData::Break(expr))
            }
            ExprData::Continue => {
                if !self
                    .scopes
                    .walk(self.current)
                    .any(|(_, scope)| matches!(scope.kind, ScopeKind::Loop { .. }))
                {
                    return self.error(Error::new("continue outside of loop", span));
                }

                CheckedExpr::new(Type::Never, CheckedExprData::Continue)
            }
            ExprData::Is { expr, pattern } => self.check_is_expr(*expr, pattern, |_, expr| expr),
            ExprData::Match { expr, body } => {
                let scrutinee = self.check_expr(*expr, None);
                let mut target = target.cloned();
                let mut has_never = false;
                let mut result: Vec<_> = body
                    .into_iter()
                    .map(|(patt, expr)| {
                        let span = expr.span;
                        let (patt, expr) = self.enter(ScopeKind::None, |this| {
                            (
                                this.check_full_pattern(&scrutinee.ty, patt),
                                this.check_expr(expr, target.as_ref()),
                            )
                        });

                        if let Some(target) = &target {
                            (patt, self.type_check_checked(expr, target, span))
                        } else {
                            if expr.ty.is_never() {
                                has_never = true;
                            } else {
                                target = Some(expr.ty.clone());
                            }

                            (patt, expr)
                        }
                    })
                    .collect();
                let target = target.unwrap_or(if has_never { Type::Never } else { Type::Void });
                if !matches!(target, Type::Never | Type::Void) {
                    for (_, e) in result.iter_mut() {
                        *e = self.try_coerce(std::mem::take(e), &target);
                    }
                }

                self.check_match_coverage(&scrutinee.ty, result.iter().map(|it| &it.0), span);
                CheckedExpr::new(
                    target,
                    CheckedExprData::Match {
                        expr: scrutinee.into(),
                        body: result,
                    },
                )
            }
            ExprData::As { expr, ty, throwing } => {
                let ty = self.resolve_typehint(ty);
                let expr = self.check_expr(*expr, Some(&ty));
                match self.coerce(expr, &ty) {
                    Ok(expr) => expr,
                    Err(expr) => {
                        self.check_cast(&expr.ty, &ty, throwing, span);
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
                let ty_is_generic = |scopes: &Scopes, ty: &Type| {
                    !ty.as_user()
                        .is_some_and(|ut| scopes.get(ut.id).data.is_template())
                };

                let mut lparams = Vec::new();
                let ret = ret.map(|ret| self.resolve_typehint(ret)).or_else(|| {
                    target
                        .as_ref()
                        .and_then(|ty| ty.as_fn_ptr())
                        .map(|f| &f.ret)
                        .filter(|ty| ty_is_generic(&self.scopes, ty))
                        .cloned()
                });
                // TODO: lambdas should have a unique type
                let (id, body) = self.enter(ScopeKind::Lambda(ret, false), |this| {
                    for (i, param) in params.into_iter().enumerate() {
                        let ty = param
                            .1
                            .map(|ty| this.resolve_typehint(ty))
                            .or_else(|| {
                                target
                                    .as_ref()
                                    .and_then(|ty| ty.as_fn_ptr())
                                    .and_then(|f| f.params.get(i))
                                    .filter(|ty| ty_is_generic(&this.scopes, ty))
                                    .cloned()
                            })
                            .unwrap_or_else(|| {
                                this.error(Error::new(
                                    format!("cannot infer type of parameter '{}'", param.0.data),
                                    param.0.span,
                                ))
                            });

                        lparams.push(ty.clone());
                        this.insert::<VariableId>(
                            Variable {
                                name: param.0,
                                ty,
                                is_static: false,
                                mutable: false,
                                value: None,
                                unused: true,
                            },
                            false,
                            false,
                        );
                    }

                    let body = if let ExprData::Block(body) = body.data {
                        body.into_iter()
                            .map(|stmt| {
                                let stmt = this.declare_stmt(&mut vec![], stmt);
                                this.check_stmt(stmt)
                            })
                            .collect()
                    } else {
                        vec![CheckedStmt::Expr(this.check_expr(
                            Expr::new(body.span, ExprData::Return(body)),
                            None,
                        ))]
                    };

                    (this.current, body)
                });
                let (target, yields) = self.scopes[id].kind.as_lambda().unwrap();
                CheckedExpr::new(
                    Type::FnPtr(
                        FnPtr {
                            params: lparams,
                            ret: yields
                                .then(|| target.clone())
                                .flatten()
                                .unwrap_or(Type::Void),
                        }
                        .into(),
                    ),
                    CheckedExprData::Lambda(body),
                )
            }
            ExprData::Unsafe(expr) => {
                let old_safety = std::mem::replace(&mut self.safety, Safety::Unsafe);
                let expr = self.check_expr(*expr, target);
                self.safety = old_safety;
                expr
            }
        }
    }

    fn check_expr(&mut self, expr: Expr, target: Option<&Type>) -> CheckedExpr {
        let expr = self.check_expr_inner(expr, target);
        if expr.ty.is_never() && !matches!(expr.data, CheckedExprData::Yield(_)) {
            // TODO: lambdas
            match &mut self.scopes[self.current].kind {
                ScopeKind::Block(target, yields @ false) => {
                    *target = Some(Type::Never);
                    *yields = true;
                }
                &mut ScopeKind::Function(id) => {
                    self.scopes.get_mut(id).returns = true;
                }
                _ => {}
            }
        }
        expr
    }

    fn check_cast(&mut self, lhs: &Type, rhs: &Type, throwing: bool, span: Span) {
        if let Some((a, b)) = lhs.as_integral().zip(rhs.as_integral()) {
            if (a.signed == b.signed || (a.signed && !b.signed)) && a.bits <= b.bits {
                return;
            }
            if (!a.signed && b.signed) && a.bits < b.bits {
                return;
            }
        }

        match (lhs, rhs) {
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
            _ => self.error(Error::new(
                format!(
                    "cannot{}cast expression of type '{}' to '{}'",
                    if !throwing { " infallibly " } else { " " },
                    lhs.name(&self.scopes),
                    rhs.name(&self.scopes),
                ),
                span,
            )),
        }
    }

    fn check_is_expr<T>(
        &mut self,
        expr: Expr,
        patt: Located<FullPattern>,
        f: impl FnOnce(&mut Self, CheckedExpr) -> T,
    ) -> T {
        self.enter(ScopeKind::None, |this| {
            let expr = this.check_expr(expr, None);
            let patt = this.check_full_pattern(&expr.ty, patt);
            f(
                this,
                CheckedExpr::new(Type::Bool, CheckedExprData::Is(expr.into(), patt)),
            )
        })
    }

    fn check_yield(&mut self, expr: Expr) -> CheckedExpr {
        let ScopeKind::Block(target, _) = std::mem::take(&mut self.scopes[self.current].kind)
        else {
            return self.error(Error::new("yield outside of block", expr.span));
        };
        let span = expr.span;
        let mut expr = self.check_expr(expr, target.as_ref());
        self.scopes[self.current].kind = if let Some(target) = target {
            expr = self.type_check_checked(expr, &target, span);
            ScopeKind::Block(Some(target), true)
        } else {
            ScopeKind::Block(Some(expr.ty.clone()), true)
        };

        CheckedExpr::new(Type::Never, CheckedExprData::Yield(expr.into()))
    }

    fn check_return(&mut self, expr: Expr) -> CheckedExpr {
        let lambda = self
            .scopes
            .walk(self.current)
            .find_map(|(id, scope)| scope.kind.as_lambda().map(|(t, _)| (id, t.clone())));
        if let Some((id, target)) = lambda {
            let span = expr.span;
            let mut expr = self.check_expr(expr, target.as_ref());
            self.scopes[id].kind = if let Some(target) = &target {
                expr = self.type_check_checked(expr, target, span);
                ScopeKind::Lambda(Some(target.clone()), true)
            } else {
                ScopeKind::Lambda(Some(expr.ty.clone()), true)
            };

            CheckedExpr::new(Type::Never, CheckedExprData::Return(expr.into()))
        } else {
            let target = self
                .current_function()
                .map(|id| self.scopes.get(id).ret.clone())
                .expect("return should only be possible inside functions");
            CheckedExpr::new(
                Type::Never,
                CheckedExprData::Return(self.type_check(expr, &target).into()),
            )
        }
    }

    fn check_unsafe_union_constructor(
        &mut self,
        target: Option<&Type>,
        mut ut: GenericUserType,
        args: Vec<(Option<String>, Expr)>,
        span: Span,
    ) -> CheckedExpr {
        self.resolve_members(ut.id);

        if let Some(target) = target.and_then(|t| t.as_user()).filter(|t| t.id == ut.id) {
            for (id, ty) in target.ty_args.iter() {
                if ut.ty_args.get(id).is_some_and(|id| !id.is_unknown()) {
                    continue;
                }

                ut.ty_args.insert(*id, ty.clone());
            }
        }

        let mut members = IndexMap::new();
        if !self.scopes.get(ut.id).members.is_empty() {
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
                .scopes
                .get(ut.id)
                .members
                .get(&name)
                .map(|m| m.ty.with_templates(&ut.ty_args))
            else {
                return self.error(Error::new(format!("unknown variant '{name}'"), span));
            };

            members.insert(name, self.check_arg(&mut ut, expr, &ty));
        } else if !args.is_empty() {
            self.error(Error::new("expected 0 arguments", span))
        }

        for (id, ty) in ut.ty_args.iter() {
            if ty.is_unknown() {
                self.error(Error::new(
                    format!(
                        "cannot infer type for type parameter '{}'",
                        self.scopes.get(*id).name
                    ),
                    span,
                ))
            } else {
                self.check_bounds(&ut.ty_args, ty, self.scopes.get(*id).impls.clone(), span);
            }
        }

        CheckedExpr::new(
            Type::User(ut.into()),
            CheckedExprData::Instance {
                members,
                variant: None,
            },
        )
    }

    fn check_call(
        &mut self,
        target: Option<&Type>,
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
                let id = recv.ty.strip_references().clone();
                if id.is_unknown() {
                    return Default::default();
                }

                let Ok(MemberFn { mut func, tr }) =
                    self.get_member_fn(&id, &member, &generics, span)
                else {
                    return Default::default();
                };
                self.resolve_proto(func.id);

                let f = self.scopes.get(func.id);
                let Some(this_param) = f.params.first().filter(|p| p.label == THIS_PARAM) else {
                    return self.error(Error::new(
                        format!("associated function '{member}' cannot be used as a method"),
                        span,
                    ));
                };

                if this_param.ty.is_mut_ptr() {
                    let mut ty = &recv.ty;
                    if !matches!(
                        ty,
                        Type::Ptr(_) | Type::MutPtr(_) | Type::DynPtr(_) | Type::DynMutPtr(_)
                    ) && !recv.can_addrmut(&self.scopes)
                    {
                        return self.error(Error::new(
                            format!("cannot call method '{member}' with immutable receiver"),
                            span,
                        ));
                    }

                    while let Type::MutPtr(inner) = ty {
                        ty = inner;
                    }

                    if matches!(ty, Type::Ptr(_) | Type::DynPtr(_)) {
                        return self.error(Error::new(
                            format!("cannot call method '{member}' through an immutable pointer"),
                            span,
                        ));
                    }
                }

                let recv = recv.auto_deref(&this_param.ty);
                let (args, ret) = self.check_fn_args(&mut func, Some(recv), args, target, span);
                return CheckedExpr::new(
                    ret,
                    CheckedExprData::MemberCall {
                        func,
                        inst: id,
                        args,
                        trait_id: tr,
                        scope: self.current,
                    },
                );
            }
            ExprData::Path(ref path) => match self.resolve_value_path(path, callee.span, false) {
                ResolvedValue::UnionConstructor(ut) => {
                    return self.check_unsafe_union_constructor(target, ut, args, span);
                }
                ResolvedValue::StructConstructor(_, mut init) => {
                    let (args, ret) = self.check_fn_args(&mut init, None, args, target, span);
                    return CheckedExpr::new(
                        ret,
                        CheckedExprData::Instance {
                            members: args,
                            variant: None,
                        },
                    );
                }
                ResolvedValue::Func(mut func) => {
                    let f = self.scopes.get(func.id);
                    let constructor = f.constructor;
                    if let Some(id) = constructor {
                        for id in self.scopes.get(id).type_params.iter() {
                            func.ty_args.entry(*id).or_insert(Type::Unknown);
                        }
                    }

                    let variant = constructor.is_some().then(|| f.name.data.clone());
                    let (args, ret) = self.check_fn_args(&mut func, None, args, target, span);
                    return CheckedExpr::new(
                        ret,
                        if constructor.is_some() {
                            CheckedExprData::Instance {
                                members: args,
                                variant,
                            }
                        } else {
                            CheckedExprData::Call {
                                func,
                                args,
                                scope: self.current,
                            }
                        },
                    );
                }
                ResolvedValue::NotFound(err) => return self.error(err),
                ResolvedValue::Error => return Default::default(),
                _ => {}
            },
            _ => {}
        }

        let callee = self.check_expr(callee, None);
        if callee.ty.is_unknown() {
            return Default::default();
        }

        if callee.ty.is_fn_ptr() {
            return self.call_fn_ptr(callee, args, span);
        }

        self.error(Error::expected_found(
            "callable item",
            &format!("'{}'", &callee.ty.name(&self.scopes)),
            span,
        ))
    }

    fn call_fn_ptr(
        &mut self,
        callee: CheckedExpr,
        args: Vec<(Option<String>, Expr)>,
        span: Span,
    ) -> CheckedExpr {
        let f = callee.ty.as_fn_ptr().unwrap();

        let mut result = vec![];
        for (i, (name, arg)) in args.into_iter().enumerate() {
            if let Some(param) = f.params.get(i) {
                if name.is_some() {
                    self.error(Error::new(
                        "keyword parameters are not allowed here",
                        arg.span,
                    ))
                }

                result.push(self.type_check(arg, param));
            } else {
                self.error::<()>(Error::new("too many positional arguments", span));
                break;
            }
        }

        if result.len() < f.params.len() {
            self.error(Error::new("too few positional arguments", span))
        }

        CheckedExpr::new(
            f.ret.clone(),
            CheckedExprData::CallFnPtr {
                expr: callee.into(),
                args: result,
            },
        )
    }

    fn check_arg<T>(&mut self, func: &mut WithTypeArgs<T>, expr: Expr, ty: &Type) -> CheckedExpr {
        let mut target = ty.with_templates(&func.ty_args);
        let span = expr.span;
        let expr = self.check_expr(expr, Some(&target));
        if !func.ty_args.is_empty() {
            func.infer_type_args(ty, &expr.ty);
            target.fill_templates(&func.ty_args);
        }

        self.type_check_checked(expr, &target, span)
    }

    fn check_fn_args(
        &mut self,
        func: &mut GenericFunc,
        recv: Option<CheckedExpr>,
        args: Vec<(Option<String>, Expr)>,
        target: Option<&Type>,
        span: Span,
    ) -> (IndexMap<String, CheckedExpr>, Type) {
        self.resolve_proto(func.id);

        let unknowns: HashSet<_> = func
            .ty_args
            .iter()
            .filter_map(|(&id, ty)| ty.is_unknown().then_some(id))
            .collect();
        if let Some(target) = target {
            func.infer_type_args(&self.scopes.get(func.id).ret, target);
        }

        let mut result = IndexMap::with_capacity(args.len());
        let mut last_pos = 0;
        if let Some(recv) = recv {
            result.insert(THIS_PARAM.into(), recv);
            last_pos += 1;
        }

        let variadic = self.scopes.get(func.id).variadic;
        let mut num = 0;
        for (name, expr) in args {
            if let Some(name) = name {
                match result.entry(name.clone()) {
                    Entry::Occupied(_) => self.error(Error::new(
                        format!("duplicate arguments for for parameter '{name}'"),
                        expr.span,
                    )),
                    Entry::Vacant(entry) => {
                        if let Some(param) = self
                            .scopes
                            .get(func.id)
                            .params
                            .iter()
                            .find(|p| p.label == name)
                        {
                            entry.insert(self.check_arg(func, expr, &param.ty.clone()));
                        } else {
                            self.error(Error::new(
                                format!("unknown parameter: '{name}'"),
                                expr.span,
                            ))
                        }
                    }
                }
            } else if let Some((i, param)) = self
                .scopes
                .get(func.id)
                .params
                .iter()
                .enumerate()
                .skip(last_pos)
                .find(|(_, param)| !param.keyword)
            {
                result.insert(
                    param.label.clone(),
                    self.check_arg(func, expr, &param.ty.clone()),
                );
                last_pos = i + 1;
            } else if !variadic {
                // TODO: a better error here would be nice
                self.error(Error::new("too many positional arguments", expr.span))
            } else {
                num += 1;
                result.insert(format!("${num}"), self.check_expr(expr, None));
            }
        }

        for param in self
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

        if self.scopes.get(func.id).params.len() > result.len() {
            let mut missing = String::new();
            for param in self
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

            self.error(Error::new(
                format!(
                    "expected {} argument(s), found {} (missing {missing})",
                    self.scopes.get(func.id).params.len(),
                    result.len()
                ),
                span,
            ))
        }

        for (id, ty) in func.ty_args.iter().filter(|(id, _)| unknowns.contains(id)) {
            if ty.is_unknown() {
                self.error(Error::new(
                    format!(
                        "cannot infer type for type parameter '{}'",
                        self.scopes.get(*id).name
                    ),
                    span,
                ))
            } else {
                self.check_bounds(&func.ty_args, ty, self.scopes.get(*id).impls.clone(), span);
            }
        }

        if self.scopes.get(func.id).is_unsafe && self.safety != Safety::Unsafe {
            self.error(Error::is_unsafe(span))
        }

        (
            result,
            self.scopes.get(func.id).ret.with_templates(&func.ty_args),
        )
    }

    fn check_bounds(&mut self, ty_args: &TypeArgs, ty: &Type, bounds: Vec<TraitImpl>, span: Span) {
        for (mut bound, _) in bounds.into_iter().flat_map(|bound| bound.into_checked()) {
            self.resolve_impls(bound.id);
            bound.fill_templates(ty_args);

            if !self.implements_trait(ty, &bound) {
                self.error(Error::doesnt_implement(
                    &ty.name(&self.scopes),
                    &bound.name(&self.scopes),
                    span,
                ))
            }
        }
    }

    fn create_block(&mut self, body: Vec<Stmt>, kind: ScopeKind) -> Block {
        self.enter(kind, |this| Block {
            body: body
                .into_iter()
                .map(|stmt| {
                    let stmt = this.declare_stmt(&mut vec![], stmt);
                    this.check_stmt(stmt)
                })
                .collect(),
            scope: this.current,
        })
    }

    fn type_check(&mut self, expr: Expr, target: &Type) -> CheckedExpr {
        let span = expr.span;
        let source = self.check_expr(expr, Some(target));
        self.type_check_checked(source, target, span)
    }

    fn type_check_checked(
        &mut self,
        source: CheckedExpr,
        target: &Type,
        span: Span,
    ) -> CheckedExpr {
        match self.coerce(source, target) {
            Ok(expr) => expr,
            Err(expr) => self.error(Error::type_mismatch(target, &expr.ty, &self.scopes, span)),
        }
    }

    fn resolve_lang_type(&mut self, name: &str, args: &[TypeHint]) -> Type {
        if let Some(id) = self.scopes.lang_types.get(name).copied() {
            Type::User(
                GenericUserType::new(
                    id,
                    self.resolve_type_args(
                        &self.scopes.get(id).type_params.clone(),
                        args,
                        true,
                        Span::default(),
                    ),
                )
                .into(),
            )
        } else {
            self.error(Error::no_lang_item(name, Span::default()))
        }
    }

    fn make_lang_type(
        &mut self,
        id: UserTypeId,
        args: impl IntoIterator<Item = Type>,
        span: Span,
    ) -> GenericUserType {
        let ty = GenericUserType::from_type_args(&self.scopes, id, args);
        for (id, param) in ty.ty_args.iter() {
            self.resolve_impls(*id);
            self.check_bounds(&ty.ty_args, param, self.scopes.get(*id).impls.clone(), span);
        }
        ty
    }

    fn make_lang_type_by_name(
        &mut self,
        name: &str,
        args: impl IntoIterator<Item = Type>,
        span: Span,
    ) -> Type {
        let Some(id) = self.scopes.lang_types.get(name).copied() else {
            return self.error(Error::no_lang_item(name, span));
        };

        Type::User(GenericUserType::from_type_args(&self.scopes, id, args).into())
    }

    fn resolve_dyn_ptr(&mut self, path: Located<Path>) -> Option<GenericTrait> {
        match self.resolve_type_path(&path) {
            ResolvedType::UserType(ut) => self.error(Error::expected_found(
                "trait",
                &format!("type '{}'", ut.name(&self.scopes)),
                path.span,
            )),
            ResolvedType::Builtin(ty) => self.error(Error::expected_found(
                "trait",
                &format!("type '{}'", ty.name(&self.scopes)),
                path.span,
            )),
            ResolvedType::Trait(tr) => Some(tr),
            ResolvedType::Error => None,
        }
    }

    fn resolve_typehint(&mut self, hint: TypeHint) -> Type {
        match hint {
            TypeHint::Regular(path) => match self.resolve_type_path(&path) {
                ResolvedType::Builtin(ty) => ty,
                ResolvedType::UserType(ut) => Type::User(ut.into()),
                ResolvedType::Trait(tr) => self.error(Error::expected_found(
                    "type",
                    &format!("trait '{}'", tr.name(&self.scopes)),
                    path.span,
                )),
                ResolvedType::Error => Type::Unknown,
            },
            TypeHint::Void => Type::Void,
            TypeHint::Ptr(ty) => Type::Ptr(self.resolve_typehint(*ty).into()),
            TypeHint::MutPtr(ty) => Type::MutPtr(self.resolve_typehint(*ty).into()),
            TypeHint::RawPtr(ty) => Type::RawPtr(self.resolve_typehint(*ty).into()),
            TypeHint::DynPtr(path) => {
                if let Some(tr) = self.resolve_dyn_ptr(path) {
                    Type::DynPtr(tr.into())
                } else {
                    Type::Unknown
                }
            }
            TypeHint::DynMutPtr(path) => {
                if let Some(tr) = self.resolve_dyn_ptr(path) {
                    Type::DynMutPtr(tr.into())
                } else {
                    Type::Unknown
                }
            }
            TypeHint::This(span) => {
                let current = self.current_function();
                for (_, scope) in self.scopes.walk(self.current) {
                    match scope.kind {
                        ScopeKind::Trait(id) => {
                            return Type::User(
                                GenericUserType::from_id(&self.scopes, self.scopes.get(id).this)
                                    .into(),
                            )
                        }
                        ScopeKind::UserType(id) => {
                            return Type::User(GenericUserType::from_id(&self.scopes, id).into())
                        }
                        ScopeKind::Extension(id) => return self.scopes.get(id).ty.clone(),
                        ScopeKind::Function(f) if Some(f) != current => break,
                        _ => {}
                    }
                }
                self.error(Error::new(format!("'{THIS_TYPE}' outside of type"), span))
            }
            TypeHint::Array(ty, count) => {
                let n = match self.consteval(&count, Some(&Type::Usize)) {
                    Ok(n) => n,
                    Err(err) => return self.error(err),
                };
                Type::Array((self.resolve_typehint(*ty), n).into())
            }
            TypeHint::Option(ty) => self.resolve_lang_type("option", &[*ty]),
            TypeHint::Vec(ty) => self.resolve_lang_type("vec", &[*ty]),
            TypeHint::Map(key, val) => self.resolve_lang_type("map", &[*key, *val]),
            TypeHint::Set(ty) => self.resolve_lang_type("set", &[*ty]),
            TypeHint::Slice(ty) => self.resolve_lang_type("span", &[*ty]),
            TypeHint::SliceMut(ty) => self.resolve_lang_type("span_mut", &[*ty]),
            TypeHint::Tuple(params) => {
                let params = params
                    .into_iter()
                    .map(|p| self.resolve_typehint(p))
                    .collect();
                self.scopes.get_tuple(params)
            }
            TypeHint::AnonStruct(params) => {
                let mut types = Vec::with_capacity(params.len());
                let mut names = Vec::with_capacity(params.len());
                for (name, ty) in params {
                    names.push(name.clone());
                    types.push(self.resolve_typehint(ty));
                }
                self.scopes.get_anon_struct(names, types)
            }
            TypeHint::Fn {
                is_extern: _,
                params,
                ret,
            } => Type::FnPtr(
                FnPtr {
                    params: params
                        .into_iter()
                        .map(|p| self.resolve_typehint(p))
                        .collect(),
                    ret: self.resolve_typehint(*ret),
                }
                .into(),
            ),
            TypeHint::Error => Type::Unknown,
        }
    }

    fn resolve_members(&mut self, id: UserTypeId) {
        for i in 0..self.scopes.get(id).members.len() {
            resolve_type!(self, self.scopes.get_mut(id).members[i].ty);
        }

        if let Some(mut union) = self.scopes.get(id).data.as_union().cloned() {
            resolve_type!(self, union.tag);
            for variant in union.variants.values_mut().flatten() {
                resolve_type!(self, *variant);
            }
            self.scopes.get_mut(id).data = UserTypeData::Union(union);
        }
    }

    fn resolve_impls<T: ItemId>(&mut self, id: T)
    where
        T::Value: TypeLike,
    {
        for i in 0..self.scopes.get(id).get_type_params().len() {
            self.resolve_impls(self.scopes.get(id).get_type_params()[i]);
        }

        for i in 0..self.scopes.get(id).get_impls().len() {
            resolve_impl!(self, self.scopes.get_mut(id).get_impls_mut()[i]);
            // if let Some(id) = self.scopes.get_mut(id).get_impls_mut()[i].as_checked().map(|tr| tr.id) {
            //     self.resolve_impls(id);
            // }
        }
    }

    fn resolve_impls_recursive<T: ItemId>(&mut self, id: T)
    where
        T::Value: TypeLike,
    {
        for i in 0..self.scopes.get(id).get_impls().len() {
            resolve_impl!(self, self.scopes.get_mut(id).get_impls_mut()[i]);
            if let Some(id) = self.scopes.get_mut(id).get_impls_mut()[i]
                .as_checked()
                .map(|(tr, _)| tr.id)
            {
                self.resolve_impls(id);
            }
        }
    }

    fn resolve_proto(&mut self, id: FunctionId) {
        for i in 0..self.scopes.get(id).params.len() {
            resolve_type!(self, self.scopes.get_mut(id).params[i].ty);
            match std::mem::take(&mut self.scopes.get_mut(id).params[i].default) {
                Some(DefaultExpr::Unchecked(scope, expr)) => {
                    self.enter_id_and_resolve(scope, |this| {
                        let target = this.scopes.get(id).params[i].ty.clone();
                        this.scopes.get_mut(id).params[i].default =
                            Some(DefaultExpr::Checked(this.type_check(expr, &target)));
                    });
                }
                other => self.scopes.get_mut(id).params[i].default = other,
            }
        }

        resolve_type!(self, self.scopes.get_mut(id).ret);

        for i in 0..self.scopes.get(id).type_params.len() {
            self.resolve_impls(self.scopes.get(id).type_params[i]);
        }
    }

    fn implements_trait(&mut self, ty: &Type, bound: &GenericTrait) -> bool {
        fn check(this: Option<&TypeArgs>, tr: &TraitImpl, bound: &GenericTrait) -> bool {
            if let TraitImpl::Checked(tr, _) = tr {
                let mut tr = tr.clone();
                this.inspect(|ty_args| tr.fill_templates(ty_args));
                &tr == bound
            } else {
                false
            }
        }

        if ty.is_unknown() || self.scopes.has_builtin_impl(ty, bound) {
            return true;
        }

        let has_impl = ty.as_user().is_some_and(|this| {
            for i in 0..self.scopes.get(this.id).impls.len() {
                resolve_impl!(self, self.scopes.get_mut(this.id).impls[i]);
                if check(
                    Some(&this.ty_args),
                    &self.scopes.get(this.id).impls[i],
                    bound,
                ) {
                    return true;
                }
            }

            false
        });
        if has_impl {
            return true;
        }

        for ext in self.scopes.extensions_in_scope_for(ty, self.current) {
            for i in 0..self.scopes.get(ext.id).impls.len() {
                resolve_impl!(self, self.scopes.get_mut(ext.id).impls[i]);
                if check(Some(&ext.ty_args), &self.scopes.get(ext.id).impls[i], bound) {
                    return true;
                }
            }
        }

        false
    }

    fn get_trait_impl_helper<Id: ItemId>(&mut self, id: Id, target: TraitId) -> Option<GenericTrait>
    where
        Id::Value: TypeLike,
    {
        for i in 0..self.scopes.get(id).get_impls().len() {
            resolve_impl!(self, self.scopes.get_mut(id).get_impls_mut()[i]);
            if let Some(ut) = self.scopes.get(id).get_impls()[i]
                .as_checked()
                .map(|(ut, _)| ut)
                .filter(|ut| ut.id == target)
                .cloned()
            {
                return Some(ut);
            }
        }

        None
    }

    fn get_trait_impl(&mut self, ty: &Type, id: TraitId) -> Option<GenericTrait> {
        if let Some(ut) = ty
            .as_user()
            .and_then(|ut| self.get_trait_impl_helper(ut.id, id))
        {
            Some(ut)
        } else {
            for ext in self.scopes.extensions_in_scope_for(ty, self.current) {
                if let Some(ut) = self.get_trait_impl_helper(ext.id, id) {
                    return Some(ut);
                }
            }

            None
        }
    }

    fn get_member_fn(
        &mut self,
        ty: &Type,
        method: &str,
        generics: &[TypeHint],
        span: Span,
    ) -> Result<MemberFn, ()> {
        fn search(
            scopes: &Scopes,
            funcs: &[Vis<FunctionId>],
            method: &str,
        ) -> Option<Vis<FunctionId>> {
            // TODO: trait implement overload ie.
            // impl Eq<f32> { ... } impl Eq<i32> { ... }
            funcs
                .iter()
                .find(|&&id| (scopes.get(*id).name.data == method))
                .copied()
        }

        fn search_extensions(
            this: &mut TypeChecker,
            ty: &Type,
            method: &str,
            generics: &[TypeHint],
            span: Span,
        ) -> Result<Option<MemberFn>, ()> {
            for ext in this.scopes.extensions_in_scope_for(ty, this.current) {
                let src_scope = this.scopes.get(ext.id).scope;
                if let Some(func) = search(&this.scopes, &this.scopes.get(ext.id).fns, method) {
                    let mut func = make_func(this, func, ty, generics, src_scope, span)?;
                    func.ty_args.copy_args(&ext.ty_args);
                    return Ok(Some(MemberFn { func, tr: None }));
                }

                this.resolve_impls_recursive(ext.id);
                for (tr, _) in this
                    .scopes
                    .get(ext.id)
                    .impls
                    .iter()
                    .flat_map(|ut| ut.as_checked())
                {
                    let tr_id = tr.id;
                    for imp in this.scopes.get_trait_impls(tr_id) {
                        if let Some(func) = search(&this.scopes, &this.scopes.get(imp).fns, method)
                        {
                            let ty_args = tr.ty_args.clone();
                            let mut func = make_func(this, func, ty, generics, src_scope, span)?;
                            func.ty_args.copy_args(&ext.ty_args);
                            if let Type::User(ut) = ty {
                                func.ty_args.copy_args_with(&ty_args, &ut.ty_args);
                            }
                            func.ty_args.insert(this.scopes.get(tr_id).this, ty.clone());
                            return Ok(Some(MemberFn {
                                func,
                                tr: Some(imp),
                            }));
                        }
                    }
                }
            }

            Ok(None)
        }

        fn make_func(
            this: &mut TypeChecker,
            f: Vis<FunctionId>,
            ty: &Type,
            generics: &[TypeHint],
            scope: ScopeId,
            span: Span,
        ) -> Result<GenericFunc, ()> {
            let func = GenericFunc::new(
                f.id,
                this.resolve_type_args(
                    &this.scopes.get(f.id).type_params.clone(),
                    generics,
                    false,
                    span,
                ),
            );
            if !f.public && !this.can_access_privates(scope) {
                this.diag.error(Error::new(
                    format!(
                        "cannot access private method '{}' of type '{}'",
                        this.scopes.get(f.id).name.data,
                        ty.name(&this.scopes)
                    ),
                    span,
                ));
                return Err(());
            }

            Ok(func)
        }

        if let Type::User(ut) = ty {
            let src_scope = self.scopes.get(ut.id).scope;
            if let Some(func) = search(&self.scopes, &self.scopes.get(ut.id).fns, method) {
                let mut func = make_func(self, func, ty, generics, src_scope, span)?;
                if let Some(ty_args) = ty.as_user().map(|ut| &ut.ty_args) {
                    func.ty_args.copy_args(ty_args);
                }

                return Ok(MemberFn { func, tr: None });
            }

            if let Some(res) = search_extensions(self, ty, method, generics, span)? {
                return Ok(res);
            }

            self.resolve_impls_recursive(ut.id);
            for (tr, _) in self
                .scopes
                .get(ut.id)
                .impls
                .iter()
                .flat_map(|ut| ut.as_checked())
            {
                let tr_id = tr.id;
                for imp in self.scopes.get_trait_impls(tr_id) {
                    if let Some(func) = search(&self.scopes, &self.scopes.get(imp).fns, method) {
                        let ty_args = tr.ty_args.clone();
                        let mut func = make_func(self, func, ty, generics, src_scope, span)?;
                        func.ty_args.copy_args_with(&ty_args, &ut.ty_args);
                        func.ty_args.insert(self.scopes.get(tr_id).this, ty.clone());
                        return Ok(MemberFn {
                            func,
                            tr: self.scopes.get(ut.id).data.is_template().then_some(imp),
                        });
                    }
                }
            }

            self.diag
                .error(Error::no_method(&ty.name(&self.scopes), method, span));
            return Err(());
        } else if let Some(tr) = ty.as_dyn_pointee() {
            let data = self.scopes.get(tr.id);
            let func = search(&self.scopes, &data.fns, method).or_else(|| {
                for (tr, _) in data.impls.iter().flat_map(|ut| ut.as_checked()) {
                    for imp in self.scopes.get_trait_impls(tr.id) {
                        if let Some(func) = search(&self.scopes, &self.scopes.get(imp).fns, method)
                        {
                            return Some(func);
                        }
                    }
                }
                None
            });

            if let Some(func) = func {
                let mut func = make_func(self, func, ty, generics, data.scope, span)?;
                func.ty_args.copy_args(&tr.ty_args);
                if !self.scopes.get(func.id).type_params.is_empty() {
                    self.error(Error::new(
                        "cannot call generic functions through a dynamic trait pointer",
                        span,
                    ))
                }
                return Ok(MemberFn { func, tr: None });
            }
        }

        if let Some(result) = search_extensions(self, ty, method, generics, span)? {
            Ok(result)
        } else {
            self.diag
                .error(Error::no_method(&ty.name(&self.scopes), method, span));
            Err(())
        }
    }

    fn get_int_type_and_val(
        &mut self,
        target: Option<&Type>,
        base: u8,
        width: Option<String>,
        value: String,
        span: Span,
    ) -> (Type, BigInt) {
        let ty = if let Some(width) = width {
            if let Some(ty) = Type::from_int_name(&width, false) {
                ty
            } else {
                return self.error(Error::new(
                    format!("invalid integer literal type: {width}"),
                    span,
                ));
            }
        } else {
            target
                .filter(|target| {
                    matches!(
                        target.strip_options(&self.scopes),
                        Type::Int(_)
                            | Type::Uint(_)
                            | Type::Isize
                            | Type::Usize
                            | Type::CInt(_)
                            | Type::CUint(_),
                    )
                })
                .cloned()
                .unwrap_or(Type::Isize)
        };

        let stats = ty.as_integral().unwrap();
        let result = match BigInt::from_str_radix(&value, base as u32) {
            Ok(result) => result,
            Err(e) => {
                return self.error(Error::new(
                    format!("integer literal '{value}' could not be parsed: {e}"),
                    span,
                ));
            }
        };
        let max = stats.max();
        if result > max {
            return self.error(Error::new(
                format!(
                    "integer literal too large for type '{}' (max is {max})",
                    ty.name(&self.scopes)
                ),
                span,
            ));
        }

        let min = stats.min();
        if result < min {
            return self.error(Error::new(
                format!(
                    "integer literal too small for type '{}' (min is {max})",
                    ty.name(&self.scopes)
                ),
                span,
            ));
        }

        (ty, result)
    }

    fn consteval(&mut self, expr: &Expr, target: Option<&Type>) -> Result<usize, Error> {
        match &expr.data {
            ExprData::Integer { base, value, width } => {
                if let Some(width) = width
                    .as_ref()
                    .and_then(|width| Type::from_int_name(width, false))
                {
                    if let Some(target) = target.filter(|&target| target != &width) {
                        return Err(Error::type_mismatch(
                            target,
                            &width,
                            &self.scopes,
                            expr.span,
                        ));
                    }
                }

                return match usize::from_str_radix(value, *base as u32) {
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
                    BinaryOp::And => lhs & rhs,
                    BinaryOp::Xor => lhs ^ rhs,
                    BinaryOp::Or => lhs | rhs,
                    BinaryOp::Shl => lhs << rhs,
                    BinaryOp::Shr => lhs >> rhs,
                    op => {
                        return Err(Error::invalid_operator(
                            op,
                            &Type::Usize.name(&self.scopes),
                            expr.span,
                        ))
                    }
                });
            }
            ExprData::Call { callee, args: _ } => {
                if let ExprData::Path(path) = &callee.data {
                    match self.resolve_value_path(path, callee.span, false) {
                        ResolvedValue::Func(func) => {
                            if self.scopes.intrinsics.get(&func.id).is_some()
                                && self.scopes.get(func.id).name.data == "size_of"
                            {
                                return Ok(func
                                    .first_type_arg()
                                    .unwrap()
                                    .size_and_align(&self.scopes)
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

    fn loop_target<'a>(
        scopes: &Scopes,
        target: Option<&'a Type>,
        infinite: bool,
    ) -> Option<&'a Type> {
        if infinite {
            target
        } else {
            target
                .and_then(|t| t.as_user())
                .filter(|t| Some(t.id) == scopes.get_option_id())
                .and_then(|target| target.first_type_arg())
        }
    }

    fn loop_out_type(&mut self, kind: &ScopeKind, span: Span) -> (Type, bool) {
        let ScopeKind::Loop {
            target,
            breaks,
            infinite,
        } = kind
        else {
            panic!("ICE: target of loop changed from loop to something else");
        };

        if *infinite {
            match breaks {
                Some(_) => (target.clone().unwrap(), false),
                None => (Type::Never, false),
            }
        } else {
            match breaks {
                Some(true) => (
                    self.make_lang_type_by_name("option", [target.clone().unwrap()], span),
                    true,
                ),
                _ => (Type::Void, false),
            }
        }
    }

    pub fn coerce(
        &mut self,
        mut expr: CheckedExpr,
        target: &Type,
    ) -> Result<CheckedExpr, CheckedExpr> {
        fn may_ptr_coerce(lhs: &Type, rhs: &Type) -> bool {
            match (lhs, rhs) {
                (Type::MutPtr(s), Type::Ptr(t) | Type::RawPtr(t)) if s == t => true,
                (Type::MutPtr(s), Type::RawPtr(t) | Type::MutPtr(t) | Type::Ptr(t)) => {
                    may_ptr_coerce(s, t)
                }
                (Type::Ptr(s), Type::Ptr(t)) => may_ptr_coerce(s, t),
                _ => false,
            }
        }

        match (&expr.ty, target) {
            (Type::Never, Type::Never) => Ok(expr),
            (Type::Never, rhs) => Ok(CheckedExpr::new(
                rhs.clone(),
                CheckedExprData::NeverCoerce(expr.into()),
            )),
            (Type::Unknown, _) | (_, Type::Unknown) => {
                expr.ty = target.clone();
                Ok(expr)
            }
            (Type::Ptr(lhs), Type::DynPtr(rhs)) if self.implements_trait(lhs, rhs) => {
                Ok(CheckedExpr::new(
                    target.clone(),
                    CheckedExprData::DynCoerce {
                        expr: expr.into(),
                        scope: self.current,
                    },
                ))
            }
            (Type::MutPtr(lhs), Type::DynPtr(rhs) | Type::DynMutPtr(rhs))
                if self.implements_trait(lhs, rhs) =>
            {
                Ok(CheckedExpr::new(
                    target.clone(),
                    CheckedExprData::DynCoerce {
                        expr: expr.into(),
                        scope: self.current,
                    },
                ))
            }
            (lhs, rhs) if may_ptr_coerce(lhs, rhs) => {
                expr.ty = target.clone();
                Ok(expr)
            }
            (lhs, rhs) if lhs != rhs => {
                if let Some(inner) = rhs.as_option_inner(&self.scopes) {
                    match self.coerce(expr, inner) {
                        Ok(expr) => Ok(CheckedExpr::new(
                            rhs.clone(),
                            CheckedExprData::Instance {
                                members: [("0".into(), expr)].into(),
                                variant: Some("Some".into()),
                            },
                        )),
                        Err(expr) => Err(expr),
                    }
                } else {
                    Err(expr)
                }
            }
            _ => Ok(expr),
        }
    }

    pub fn try_coerce(&mut self, expr: CheckedExpr, target: &Type) -> CheckedExpr {
        match self.coerce(expr, target) {
            Ok(expr) => expr,
            Err(expr) => expr,
        }
    }
}

/// Pattern matching routines
impl TypeChecker {
    fn check_match_coverage<'a>(
        &mut self,
        ty: &Type,
        mut patterns: impl Iterator<Item = &'a CheckedPattern> + Clone,
        span: Span,
    ) {
        let ty = ty.strip_references();
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
                            if i.inclusive {
                                if value >= i.start && value <= i.end {
                                    value = &i.end + 1;
                                    continue 'outer;
                                }
                            } else if value >= i.start && value < i.end {
                                value = i.end.clone();
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
            .is_some_and(|ut| Some(&ut.id) == self.scopes.lang_types.get("string"))
        {
            if !patterns.any(|patt| patt.irrefutable) {
                self.error(Error::match_statement("", span))
            }
        } else if ty.as_user().is_some_and(|ut| {
            Some(&ut.id) == self.scopes.lang_types.get("span")
                || Some(&ut.id) == self.scopes.lang_types.get("span_mut")
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
            .and_then(|ut| self.scopes.get(ut.id).data.as_union())
        {
            let mut missing = vec![];
            'outer: for (name, _) in union.variants.iter() {
                for patt in patterns.clone() {
                    if patt.irrefutable {
                        return;
                    } else if patt
                        .data
                        .as_union_member()
                        .is_some_and(|(sub, variant, _, _)| {
                            name == variant && sub.as_ref().map_or(true, |sub| sub.irrefutable)
                        })
                    {
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
        scrutinee: &Type,
        path: ResolvedValue,
        span: Span,
    ) -> Result<(Option<Type>, String), Option<Error>> {
        let Some(ut) = scrutinee
            .strip_references()
            .as_user()
            .filter(|ut| self.scopes.get(ut.id).data.is_union())
        else {
            return Err(Some(Error::new(
                format!(
                    "cannot use union pattern on type '{}'",
                    scrutinee.name(&self.scopes)
                ),
                span,
            )));
        };
        self.resolve_members(ut.id);
        match path {
            ResolvedValue::Func(f) => {
                self.resolve_proto(f.id);
                let f = self.scopes.get(f.id);
                if f.constructor.is_some_and(|id| id == ut.id) {
                    let variant = f.name.data.clone();
                    Ok((
                        self.scopes
                            .get(ut.id)
                            .data
                            .as_union()
                            .and_then(|union| union.variants.get(&variant))
                            .unwrap()
                            .clone()
                            .map(|ty| scrutinee.matched_inner_type(ty.with_templates(&ut.ty_args))),
                        variant,
                    ))
                } else if f
                    .constructor
                    .is_some_and(|id| self.scopes.get(id).data.is_union())
                {
                    Err(self.error(Error::type_mismatch_s(
                        &scrutinee.name(&self.scopes),
                        &f.ret.name(&self.scopes),
                        span,
                    )))
                } else {
                    Err(Some(Error::expected_found(
                        &scrutinee.name(&self.scopes),
                        &format!("function '{}'", f.name.data),
                        span,
                    )))
                }
            }
            ResolvedValue::StructConstructor(id, _) => Err(Some(Error::expected_found(
                &scrutinee.name(&self.scopes),
                &format!("type '{}'", self.scopes.get(id).name),
                span,
            ))),
            ResolvedValue::UnionConstructor(ut) => Err(Some(Error::expected_found(
                &scrutinee.name(&self.scopes),
                &format!("type '{}'", self.scopes.get(ut.id).name),
                span,
            ))),
            ResolvedValue::Var(id) => Err(Some(Error::expected_found(
                &scrutinee.name(&self.scopes),
                &format!("variable '{}'", self.scopes.get(id).name),
                span,
            ))),
            ResolvedValue::NotFound(err) => Err(Some(err)),
            ResolvedValue::Error => Err(None),
        }
    }

    fn check_int_pattern(
        &mut self,
        target: &Type,
        IntPattern {
            negative,
            base,
            value,
            width,
        }: IntPattern,
        span: Span,
    ) -> Option<BigInt> {
        let inner = target.strip_references();
        let Some(stats) = inner.as_integral() else {
            let (ty, _) = self.get_int_type_and_val(None, base, width, value, span);
            if ty.is_unknown() {
                return None;
            }

            return self.error(Error::type_mismatch(target, &ty, &self.scopes, span));
        };

        let (ty, value) = self.get_int_type_and_val(Some(inner), base, width, value, span);
        if &ty != inner {
            return self.error(Error::type_mismatch(inner, &ty, &self.scopes, span));
        }

        if !stats.signed && negative {
            return self.error(Error::new(
                format!(
                    "cannot negate unsigned integer type '{}'",
                    ty.name(&self.scopes)
                ),
                span,
            ));
        }

        Some(if negative { -value } else { value })
    }

    fn check_slice_pattern(
        &mut self,
        inner_ptr: Type,
        span_inner: Type,
        patterns: Vec<Located<Pattern>>,
        span_id: UserTypeId,
        param: bool,
    ) -> CheckedPattern {
        let mut rest = None;
        let mut result = Vec::new();
        for (i, patt) in patterns.into_iter().enumerate() {
            if let Pattern::Rest(var) = patt.data {
                let id = var.map(|(mutable, name)| {
                    self.insert(
                        Variable {
                            name,
                            ty: Type::Unknown,
                            is_static: false,
                            mutable,
                            value: None,
                            unused: !param,
                        },
                        false,
                        false,
                    )
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
                result.push(self.check_pattern(true, &inner_ptr, false, patt, param));
            }
        }

        if let Some(RestPattern { id: Some(id), .. }) = &mut rest {
            self.scopes.get_mut(*id).item.ty = Type::User(
                GenericUserType::from_type_args(&self.scopes, span_id, [span_inner.clone()]).into(),
            );
        }

        CheckedPattern::refutable(CheckedPatternData::Span {
            rest,
            patterns: result,
            inner: span_inner,
        })
    }

    fn check_array_pattern(
        &mut self,
        target: &Type,
        patterns: Vec<Located<Pattern>>,
        span: Span,
        param: bool,
    ) -> CheckedPattern {
        let span_id = self.scopes.lang_types.get("span").copied();
        let span_mut_id = self.scopes.lang_types.get("span_mut").copied();
        let (real_inner, arr_len) = match target.strip_references() {
            Type::Array(inner) => (&inner.0, inner.1),
            Type::User(ut) if Some(ut.id) == span_id => {
                let inner = ut.first_type_arg().unwrap().clone();
                return self.check_slice_pattern(
                    Type::Ptr(inner.clone().into()),
                    inner,
                    patterns,
                    span_id.unwrap(),
                    param,
                );
            }
            Type::User(ut) if Some(ut.id) == span_mut_id => {
                let inner = ut.first_type_arg().unwrap().clone();
                if target.is_ptr() || target.is_mut_ptr() {
                    let ptr = target.matched_inner_type(inner.clone());
                    let id = if ptr.is_ptr() {
                        span_id.unwrap()
                    } else {
                        span_mut_id.unwrap()
                    };
                    return self.check_slice_pattern(ptr, inner, patterns, id, param);
                } else {
                    return self.check_slice_pattern(
                        Type::MutPtr(inner.clone().into()),
                        inner,
                        patterns,
                        span_mut_id.unwrap(),
                        param,
                    );
                }
            }
            _ => {
                return self.error(Error::new(
                    format!(
                        "array pattern cannot match value of type '{}'",
                        target.name(&self.scopes)
                    ),
                    span,
                ))
            }
        };

        let inner = target.matched_inner_type(real_inner.clone());

        let mut rest = None;
        let mut irrefutable = true;
        let mut result = Vec::new();
        for (i, patt) in patterns.into_iter().enumerate() {
            if let Pattern::Rest(var) = patt.data {
                let id = var.map(|(mutable, name)| {
                    self.insert(
                        Variable {
                            name,
                            ty: Type::Unknown,
                            is_static: false,
                            mutable,
                            value: None,
                            unused: !param,
                        },
                        false,
                        false,
                    )
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
                let patt = self.check_pattern(true, &inner, false, patt, param);
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
            self.scopes.get_mut(*id).item.ty = target.matched_inner_type(Type::Array(
                (real_inner.clone(), arr_len - result.len()).into(),
            ));
        }

        CheckedPattern {
            irrefutable,
            data: CheckedPatternData::Array {
                patterns: ArrayPattern {
                    rest,
                    arr_len,
                    inner: real_inner.clone(),
                    patterns: result,
                },
                borrows: target.is_any_ptr(),
            },
        }
    }

    fn check_struct_pattern(
        &mut self,
        scrutinee: &Type,
        mutable: bool,
        destructures: Vec<Destructure>,
        span: Span,
        param: bool,
    ) -> CheckedPattern {
        let Some(ut) = scrutinee.strip_references().as_user().filter(|ut| {
            matches!(
                self.scopes.get(ut.id).data,
                UserTypeData::Struct | UserTypeData::Union(_) | UserTypeData::AnonStruct
            )
        }) else {
            return self.error(Error::bad_destructure(&scrutinee.name(&self.scopes), span));
        };
        self.resolve_members(ut.id);

        let cap = self.can_access_privates(self.scopes.get(ut.id).scope);
        let mut irrefutable = true;
        let mut checked = Vec::new();

        for Destructure {
            name,
            mutable: pm,
            pattern,
        } in destructures
        {
            let Some(member) = self.scopes.get(ut.id).members.get(&name.data) else {
                self.diag.error(Error::no_member(
                    &scrutinee.name(&self.scopes),
                    &name.data,
                    name.span,
                ));
                continue;
            };

            if !member.public && !cap {
                self.diag.error(Error::private_member(
                    &scrutinee.name(&self.scopes),
                    &name.data,
                    name.span,
                ));
            }

            // TODO: duplicates
            let inner = member.ty.with_templates(&ut.ty_args);
            let patt = self.check_pattern(
                true,
                &scrutinee.matched_inner_type(inner.clone()),
                mutable || pm,
                pattern,
                param,
            );
            if !patt.irrefutable {
                irrefutable = false;
            }
            checked.push((name.data, inner, patt))
        }

        CheckedPattern {
            irrefutable,
            data: CheckedPatternData::Destrucure {
                patterns: checked,
                borrows: scrutinee.is_any_ptr(),
            },
        }
    }

    fn check_tuple_pattern(
        &mut self,
        scrutinee: &Type,
        mutable: bool,
        subpatterns: Vec<Located<Pattern>>,
        span: Span,
        param: bool,
    ) -> CheckedPattern {
        let Some(ut) = scrutinee
            .strip_references()
            .as_user()
            .filter(|ut| self.scopes.get(ut.id).data.is_tuple())
        else {
            return self.error(Error::expected_found(
                &scrutinee.name(&self.scopes),
                &format!("({})", ["_"].repeat(subpatterns.len()).join(", ")),
                span,
            ));
        };

        if ut.ty_args.len() != subpatterns.len() {
            self.error(Error::expected_found(
                &scrutinee.name(&self.scopes),
                &format!("({})", ["_"].repeat(subpatterns.len()).join(", ")),
                span,
            ))
        }

        let mut irrefutable = true;
        let mut checked = Vec::new();
        for (i, patt) in subpatterns.into_iter().enumerate() {
            let (inner, ty) = if let Some((_, ty)) = ut.ty_args.get_index(i) {
                (ty.clone(), scrutinee.matched_inner_type(ty.clone()))
            } else {
                (Type::Unknown, Type::Unknown)
            };

            let patt = self.check_pattern(true, &ty, mutable, patt, param);
            if !patt.irrefutable {
                irrefutable = false;
            }
            checked.push((format!("{i}"), inner, patt));
        }

        CheckedPattern {
            irrefutable,
            data: CheckedPatternData::Destrucure {
                patterns: checked,
                borrows: scrutinee.is_any_ptr(),
            },
        }
    }

    fn check_tuple_union_pattern(
        &mut self,
        scrutinee: &Type,
        mutable: bool,
        resolved: ResolvedValue,
        subpatterns: Vec<Located<Pattern>>,
        span: Span,
        param: bool,
    ) -> CheckedPattern {
        match self.get_union_variant(scrutinee, resolved, span) {
            Ok((Some(scrutinee), variant)) => {
                CheckedPattern::refutable(CheckedPatternData::UnionMember {
                    pattern: Some(
                        self.check_tuple_pattern(&scrutinee, mutable, subpatterns, span, param)
                            .into(),
                    ),
                    variant,
                    inner: Type::Unknown,
                    borrows: scrutinee.is_any_ptr(),
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
        scrutinee: &Type,
        resolved: ResolvedValue,
        span: Span,
    ) -> CheckedPattern {
        match self.get_union_variant(scrutinee, resolved, span) {
            Ok((Some(_), _)) => self.error(Error::expected_found(
                "empty variant pattern",
                "variant pattern",
                span,
            )),
            Ok((None, variant)) => CheckedPattern::refutable(CheckedPatternData::UnionMember {
                pattern: None,
                variant,
                inner: Type::Unknown,
                borrows: false,
            }),
            Err(Some(err)) => self.error(err),
            Err(None) => Default::default(),
        }
    }

    fn check_pattern(
        &mut self,
        binding: bool,
        scrutinee: &Type,
        mutable: bool,
        pattern: Located<Pattern>,
        param: bool,
    ) -> CheckedPattern {
        let span = pattern.span;
        match pattern.data {
            Pattern::TupleLike { path, subpatterns } => {
                let value = self.resolve_value_path(&path.data, path.span, false);
                self.check_tuple_union_pattern(scrutinee, mutable, value, subpatterns, span, param)
            }
            Pattern::StructLike { path, subpatterns } => {
                let value = self.resolve_value_path(&path.data, path.span, false);
                match self.get_union_variant(scrutinee, value, span) {
                    Ok((Some(scrutinee), variant)) => {
                        CheckedPattern::refutable(CheckedPatternData::UnionMember {
                            pattern: Some(
                                self.check_struct_pattern(
                                    &scrutinee,
                                    mutable,
                                    subpatterns,
                                    span,
                                    param,
                                )
                                .into(),
                            ),
                            variant,
                            inner: Type::Unknown,
                            borrows: scrutinee.is_any_ptr(),
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
                let value = self.resolve_value_path(&path, span, false);
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
                            CheckedPattern::refutable(CheckedPatternData::UnionMember {
                                pattern: None,
                                variant,
                                inner: Type::Unknown,
                                borrows: false,
                            })
                        }
                        Err(Some(_)) => {
                            CheckedPattern::irrefutable(CheckedPatternData::Variable(self.insert(
                                Variable {
                                    name: Located::new(span, ident.into()),
                                    ty: scrutinee.clone(),
                                    is_static: false,
                                    mutable,
                                    value: None,
                                    unused: !param,
                                },
                                false,
                                false,
                            )))
                        }
                        Err(None) => Default::default(),
                    }
                } else {
                    self.check_empty_union_pattern(scrutinee, value, span)
                }
            }
            Pattern::Option(patt) => {
                let Some(id) = self.scopes.get_option_id() else {
                    return self.error(Error::no_lang_item("option", pattern.span));
                };
                let value = self.resolve_value_path_in(
                    &[("Some".into(), vec![])],
                    Default::default(),
                    self.scopes.get(id).body_scope,
                    false,
                    pattern.span,
                );
                self.check_tuple_union_pattern(
                    scrutinee,
                    false,
                    value,
                    vec![Located::new(pattern.span, *patt)],
                    pattern.span,
                    param,
                )
            }
            Pattern::Null => {
                let Some(id) = self.scopes.get_option_id() else {
                    return self.error(Error::no_lang_item("option", pattern.span));
                };
                let value = self.resolve_value_path_in(
                    &[("None".into(), vec![])],
                    Default::default(),
                    self.scopes.get(id).body_scope,
                    false,
                    pattern.span,
                );
                self.check_empty_union_pattern(scrutinee, value, pattern.span)
            }
            Pattern::MutBinding(name) => {
                CheckedPattern::irrefutable(CheckedPatternData::Variable(self.insert(
                    Variable {
                        name: Located::new(span, name),
                        ty: scrutinee.clone(),
                        is_static: false,
                        mutable: true,
                        value: None,
                        unused: !param,
                    },
                    false,
                    false,
                )))
            }
            Pattern::Struct(sub) => self.check_struct_pattern(scrutinee, mutable, sub, span, param),
            Pattern::String(value) => {
                let string = &self.make_lang_type_by_name("string", [], span);
                if scrutinee.strip_references() != string {
                    return self.error(Error::type_mismatch(scrutinee, string, &self.scopes, span));
                }

                CheckedPattern::refutable(CheckedPatternData::String(value))
            }
            Pattern::Int(patt) => CheckedPattern::refutable(
                self.check_int_pattern(scrutinee, patt, span)
                    .map(CheckedPatternData::Int)
                    .unwrap_or_default(),
            ),
            Pattern::IntRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                let Some(start) = self.check_int_pattern(scrutinee, start, span) else {
                    return Default::default();
                };
                let Some(end) = self.check_int_pattern(scrutinee, end, span) else {
                    return Default::default();
                };

                if start > end {
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
                if scrutinee.strip_references() != &Type::Char {
                    return self.error(Error::type_mismatch(
                        scrutinee,
                        &Type::Char,
                        &self.scopes,
                        span,
                    ));
                }

                CheckedPattern::refutable(CheckedPatternData::Int(BigInt::from(ch as u32)))
            }
            Pattern::CharRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                if scrutinee.strip_references() != &Type::Char {
                    return self.error(Error::type_mismatch(
                        scrutinee,
                        &Type::Char,
                        &self.scopes,
                        span,
                    ));
                }

                if start > end {
                    return self.error(Error::new(
                        "range pattern end cannot be greater than its start",
                        span,
                    ));
                }

                CheckedPattern::refutable(CheckedPatternData::IntRange(RangePattern {
                    inclusive,
                    start: BigInt::from(start as u32),
                    end: BigInt::from(end as u32),
                }))
            }
            Pattern::Rest { .. } => self.error(Error::new(
                "rest patterns are only valid inside array or span patterns",
                span,
            )),
            Pattern::Array(sub) => self.check_array_pattern(scrutinee, sub, span, param),
            Pattern::Bool(val) => {
                if scrutinee.strip_references() != &Type::Bool {
                    return self.error(Error::type_mismatch(
                        scrutinee,
                        &Type::Bool,
                        &self.scopes,
                        span,
                    ));
                }

                CheckedPattern::refutable(CheckedPatternData::Int(BigInt::from(val as u32)))
            }
            Pattern::Tuple(sub) => self.check_tuple_pattern(scrutinee, mutable, sub, span, param),
            Pattern::Void => {
                if scrutinee.strip_references() != &Type::Void {
                    return self.error(Error::type_mismatch(
                        scrutinee,
                        &Type::Void,
                        &self.scopes,
                        span,
                    ));
                }

                CheckedPattern::irrefutable(CheckedPatternData::Void)
            }
            Pattern::Error => Default::default(),
        }
    }

    fn check_full_pattern(
        &mut self,
        scrutinee: &Type,
        pattern: Located<FullPattern>,
    ) -> CheckedPattern {
        self.check_pattern(
            false,
            scrutinee,
            false,
            pattern.map(|inner| inner.data),
            false,
        )
    }
}

/// Path resolution routines
impl TypeChecker {
    fn get_super(&mut self, span: Span) -> Option<ScopeId> {
        if let Some(module) = self.scopes.module_of(
            self.scopes[self.scopes.module_of(self.current).unwrap()]
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
                let (first, rest) = components.split_first().unwrap();
                match self.find_in_tns(&first.data).map(|i| i.id) {
                    Some(TypeItem::Module(next)) => self.resolve_use_in(next, *public, rest, tail),
                    Some(TypeItem::Type(id)) => {
                        if rest.is_empty() {
                            self.resolve_use_union(*public, id, first, tail);
                        } else {
                            self.error(Error::new("expected module name", first.span))
                        }
                        Ok(())
                    }
                    _ => self.resolve_use_in(ScopeId::ROOT, *public, components, tail),
                }
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
            match self.scopes[scope].find_in_tns(&comp.data).as_deref() {
                Some(&TypeItem::Module(next)) => {
                    scope = next;
                }
                Some(&TypeItem::Type(id)) => {
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
            if let Some(item) = self.scopes[scope].find_in_tns(&tail.data) {
                if !item.public && !self.can_access_privates(scope) {
                    self.diag
                        .error(Error::new(format!("'{}' is private", tail.data), tail.span));
                }

                if self.scopes[self.current]
                    .tns
                    .insert(tail.data.clone(), Vis::new(*item, public))
                    .is_some()
                {
                    self.error(Error::redefinition(&tail.data, tail.span))
                }
                found = true;
            }
            if let Some(item) = self.scopes[scope].find_in_vns(&tail.data) {
                if !item.public && !self.can_access_privates(scope) {
                    self.diag
                        .error(Error::new(format!("'{}' is private", tail.data), tail.span));
                }

                if self.scopes[self.current]
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
        for (name, item) in self.scopes[scope].tns.clone().iter() {
            if item.public || self.can_access_privates(scope) {
                self.scopes[self.current]
                    .tns
                    .entry(name.clone())
                    .or_insert(Vis::new(**item, public));
            }
        }

        for (name, item) in self.scopes[scope].vns.clone().iter() {
            if item.public || self.can_access_privates(scope) {
                self.scopes[self.current]
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
        let ut = self.scopes.get(id);
        if !ut.data.is_union() {
            return self.error(Error::expected_found(
                "module or union type",
                &comp.data,
                comp.span,
            ));
        }

        let mut constructors = ut
            .fns
            .iter()
            .filter(|&&id| self.scopes.get(*id).constructor.is_some());
        if let UsePathTail::Ident(tail) = tail {
            let Some(&id) = constructors.find(|&&id| self.scopes.get(*id).name.data == tail.data)
            else {
                return self.error(Error::new("expected variant name", comp.span));
            };

            if self.scopes[self.current]
                .vns
                .insert(tail.data.clone(), Vis::new((*id).into(), public))
                .is_some()
            {
                self.error(Error::redefinition(&tail.data, tail.span))
            }
        } else {
            for id in constructors.copied().collect::<Vec<_>>() {
                let name = self.scopes.get(*id).name.data.clone();
                self.scopes[self.current]
                    .vns
                    .entry(name)
                    .or_insert(Vis::new((*id).into(), public));
            }
        }
    }

    fn resolve_type_path(&mut self, path: &Located<Path>) -> ResolvedType {
        let span = path.span;
        let path = &path.data;
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
                if let Some(builtin) = self.builtin_type_path(name) {
                    if rest.is_empty() {
                        return ResolvedType::Builtin(builtin);
                    }
                    return self.error(Error::new("", span));
                }

                match self.find_in_tns(name).map(|t| t.id) {
                    Some(TypeItem::Type(id)) => {
                        let ty_args = self.resolve_type_args(
                            &self.scopes.get(id).type_params.clone(),
                            ty_args,
                            true,
                            span,
                        );
                        if rest.is_empty() {
                            return ResolvedType::UserType(GenericUserType::new(id, ty_args));
                        }

                        self.resolve_type_path_in(
                            rest,
                            ty_args,
                            self.scopes.get(id).body_scope,
                            span,
                        )
                    }
                    Some(TypeItem::Trait(id)) => {
                        let ty_args = self.resolve_type_args(
                            &self.scopes.get(id).type_params.clone(),
                            ty_args,
                            true,
                            span,
                        );
                        if rest.is_empty() {
                            return ResolvedType::Trait(GenericTrait::new(id, ty_args));
                        }

                        self.resolve_type_path_in(
                            rest,
                            ty_args,
                            self.scopes.get(id).body_scope,
                            span,
                        )
                    }
                    Some(TypeItem::Module(id)) if !rest.is_empty() => {
                        if !ty_args.is_empty() {
                            return self.error(Error::new(
                                "modules cannot be parameterized with type arguments",
                                span,
                            ));
                        }

                        self.resolve_type_path_in(rest, Default::default(), id, span)
                    }
                    Some(TypeItem::Extension(id)) => {
                        let ty_args = self.resolve_type_args(
                            &self.scopes.get(id).type_params.clone(),
                            ty_args,
                            true,
                            span,
                        );
                        if rest.is_empty() {
                            return self.error(Error::expected_found(
                                "type",
                                &format!("extension '{}'", self.scopes.get(id).name.data),
                                span,
                            ));
                        }

                        self.resolve_type_path_in(
                            rest,
                            ty_args,
                            self.scopes.get(id).body_scope,
                            span,
                        )
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
        span: Span,
    ) -> ResolvedType {
        for (i, (name, args)) in data.iter().enumerate() {
            let is_end = i + 1 == data.len();
            if let Some(item) = self.scopes[scope].find_in_tns(name) {
                if !item.public && !self.can_access_privates(scope) {
                    self.error(Error::new(format!("'{name}' is private"), span))
                }

                match *item {
                    TypeItem::Type(id) => {
                        let ty = self.scopes.get(id);
                        scope = ty.body_scope;

                        let args =
                            self.resolve_type_args(&ty.type_params.clone(), args, true, span);
                        if is_end {
                            return ResolvedType::UserType(GenericUserType::new(id, args));
                        }

                        ty_args.copy_args(&args);
                        continue;
                    }
                    TypeItem::Extension(id) => {
                        scope = self.scopes.get(id).body_scope;
                        let args = self.resolve_type_args(
                            &self.scopes.get(id).type_params.clone(),
                            args,
                            true,
                            span,
                        );
                        if is_end {
                            return self.error(Error::expected_found(
                                "type",
                                &format!("extension '{}'", self.scopes.get(id).name.data),
                                span,
                            ));
                        }

                        ty_args.copy_args(&args);
                        continue;
                    }
                    TypeItem::Module(id) => {
                        if is_end {
                            return self.error(Error::no_symbol(name, span));
                        }

                        if !args.is_empty() {
                            return self.error(Error::new(
                                "modules cannot be parameterized with type arguments",
                                span,
                            ));
                        }

                        scope = id;
                        continue;
                    }
                    TypeItem::Trait(id) => {
                        let ty = self.scopes.get(id);
                        scope = ty.body_scope;

                        let args =
                            self.resolve_type_args(&ty.type_params.clone(), args, true, span);
                        if is_end {
                            return ResolvedType::Trait(GenericTrait::new(id, args));
                        }

                        ty_args.copy_args(&args);
                        continue;
                    }
                }
            }

            return self.error(Error::no_symbol(name, span));
        }

        unreachable!()
    }

    fn resolve_value_path(&mut self, path: &Path, span: Span, args: bool) -> ResolvedValue {
        match path.origin {
            PathOrigin::Root => self.resolve_value_path_in(
                &path.components,
                Default::default(),
                ScopeId::ROOT,
                args,
                span,
            ),
            PathOrigin::Super(span) => self
                .get_super(span)
                .map(|id| {
                    self.resolve_value_path_in(&path.components, Default::default(), id, args, span)
                })
                .unwrap_or_default(),
            PathOrigin::Normal => {
                let ((name, ty_args), rest) = path.components.split_first().unwrap();
                if rest.is_empty() {
                    match self.find_in_vns(name).map(|f| f.id) {
                        Some(ValueItem::Fn(id)) => ResolvedValue::Func(GenericFunc::new(
                            id,
                            self.resolve_type_args(
                                &self.scopes.get(id).type_params.clone(),
                                ty_args,
                                args,
                                span,
                            ),
                        )),
                        Some(ValueItem::StructConstructor(id, init)) => {
                            ResolvedValue::StructConstructor(
                                id,
                                GenericFunc::new(
                                    init,
                                    self.resolve_type_args(
                                        &self.scopes.get(id).type_params.clone(),
                                        ty_args,
                                        args,
                                        span,
                                    ),
                                ),
                            )
                        }
                        Some(ValueItem::UnionConstructor(id)) => {
                            ResolvedValue::UnionConstructor(GenericUserType::new(
                                id,
                                self.resolve_type_args(
                                    &self.scopes.get(id).type_params.clone(),
                                    ty_args,
                                    args,
                                    span,
                                ),
                            ))
                        }
                        Some(ValueItem::Var(id)) => {
                            resolve_type!(self, self.scopes.get_mut::<VariableId>(id).ty);
                            if !ty_args.is_empty() {
                                return self.error(Error::new(
                                    "variables cannot have type arguments",
                                    span,
                                ));
                            }

                            ResolvedValue::Var(id)
                        }
                        None => self.resolve_value_path_in(
                            &path.components,
                            Default::default(),
                            ScopeId::ROOT,
                            args,
                            span,
                        ),
                    }
                } else {
                    match self.find_in_tns(name).map(|t| t.id) {
                        Some(TypeItem::Type(id)) => {
                            let ty_args = self.resolve_type_args(
                                &self.scopes.get(id).type_params.clone(),
                                ty_args,
                                args,
                                span,
                            );

                            self.resolve_value_path_in(
                                rest,
                                ty_args,
                                self.scopes.get(id).body_scope,
                                args,
                                span,
                            )
                        }
                        Some(TypeItem::Trait(id)) => {
                            let mut ty_args = self.resolve_type_args(
                                &self.scopes.get(id).type_params.clone(),
                                ty_args,
                                args,
                                span,
                            );
                            ty_args.insert(self.scopes.get(id).this, Type::Unknown);

                            self.resolve_value_path_in(
                                rest,
                                ty_args,
                                self.scopes.get(id).body_scope,
                                args,
                                span,
                            )
                        }
                        Some(TypeItem::Extension(id)) => {
                            let ty_args = self.resolve_type_args(
                                &self.scopes.get(id).type_params.clone(),
                                ty_args,
                                args,
                                span,
                            );

                            self.resolve_value_path_in(
                                rest,
                                ty_args,
                                self.scopes.get(id).body_scope,
                                args,
                                span,
                            )
                        }
                        Some(TypeItem::Module(id)) => {
                            if !ty_args.is_empty() {
                                return self.error(Error::new(
                                    "modules cannot be parameterized with type arguments",
                                    span,
                                ));
                            }

                            self.resolve_value_path_in(rest, Default::default(), id, args, span)
                        }
                        None => self.resolve_value_path_in(
                            &path.components,
                            Default::default(),
                            ScopeId::ROOT,
                            args,
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
        typehint: bool,
        span: Span,
    ) -> ResolvedValue {
        let ((last_name, last_args), rest) = data.split_last().unwrap();
        for (name, args) in rest.iter() {
            let Some(item) = self.scopes[scope].find_in_tns(name) else {
                return ResolvedValue::NotFound(Error::no_symbol(name, span));
            };

            if !item.public && !self.can_access_privates(scope) {
                self.error(Error::new(format!("'{name}' is private"), span))
            }

            match *item {
                TypeItem::Type(id) => {
                    let ty = self.scopes.get(id);
                    scope = ty.body_scope;
                    ty_args.copy_args(&self.resolve_type_args(
                        &ty.type_params.clone(),
                        args,
                        typehint,
                        span,
                    ));
                }
                TypeItem::Extension(id) => {
                    let ty = self.scopes.get(id);
                    scope = ty.body_scope;
                    ty_args.copy_args(&self.resolve_type_args(
                        &ty.type_params.clone(),
                        args,
                        typehint,
                        span,
                    ));
                }
                TypeItem::Module(id) => {
                    if !args.is_empty() {
                        return self.error(Error::new(
                            "modules cannot be parameterized with type arguments",
                            span,
                        ));
                    }

                    scope = id;
                }
                TypeItem::Trait(id) => {
                    let ty = self.scopes.get(id);
                    scope = ty.body_scope;
                    ty_args.insert(self.scopes.get(id).this, Type::Unknown);
                    ty_args.copy_args(&self.resolve_type_args(
                        &ty.type_params.clone(),
                        args,
                        typehint,
                        span,
                    ));
                }
            }
        }

        let Some(item) = self.scopes[scope].find_in_vns(last_name) else {
            return ResolvedValue::NotFound(Error::no_symbol(last_name, span));
        };

        if !item.public && !self.can_access_privates(scope) {
            self.error(Error::new(format!("'{last_name}' is private"), span))
        }

        match *item {
            ValueItem::Fn(id) => {
                ty_args.copy_args(&self.resolve_type_args(
                    &self.scopes.get(id).type_params.clone(),
                    last_args,
                    typehint,
                    span,
                ));

                ResolvedValue::Func(GenericFunc::new(id, ty_args))
            }
            ValueItem::StructConstructor(id, init) => {
                ty_args.copy_args(&self.resolve_type_args(
                    &self.scopes.get(id).type_params.clone(),
                    last_args,
                    typehint,
                    span,
                ));

                ResolvedValue::StructConstructor(id, GenericFunc::new(init, ty_args))
            }
            ValueItem::UnionConstructor(id) => {
                ty_args.copy_args(&self.resolve_type_args(
                    &self.scopes.get(id).type_params.clone(),
                    last_args,
                    typehint,
                    span,
                ));
                ResolvedValue::UnionConstructor(GenericUserType::new(id, ty_args))
            }
            ValueItem::Var(id) => {
                resolve_type!(self, self.scopes.get_mut(id).ty);
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

    fn resolve_type_args(
        &mut self,
        params: &[UserTypeId],
        args: &[TypeHint],
        typehint: bool,
        span: Span,
    ) -> TypeArgs {
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
                        .map(|ty| self.resolve_typehint(ty.clone()))
                        .take(params.len())
                        .chain(
                            std::iter::repeat(Type::Unknown)
                                .take(params.len().checked_sub(args.len()).unwrap_or_default()),
                        ),
                )
                .collect(),
        );
        for (id, ty) in ty_args.iter() {
            self.check_bounds(&ty_args, ty, self.scopes.get(*id).impls.clone(), span);
        }

        ty_args
    }

    fn builtin_type_path(&self, name: &str) -> Option<Type> {
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
