use std::path::PathBuf;

use enum_as_inner::EnumAsInner;
use indexmap::{map::Entry, IndexMap};
use num_bigint::BigInt;
use num_traits::Num;

use crate::{
    ast::{checked::*, declared::*, parsed::*, Attribute, BinaryOp, UnaryOp},
    error::{Diagnostics, Error},
    lexer::{Located, Span},
    sym::*,
    typeid::{CInt, FnPtr, GenericFunc, GenericUserType, Type, TypeArgs},
    Pipeline, THIS_PARAM,
};

macro_rules! type_check_bail {
    ($self: expr, $source: expr, $target: expr) => {{
        let source = $source;
        let span = source.span;
        let source = $self.check_expr(source, Some($target));
        match source.coerce_to(&$self.scopes, $target) {
            Ok(expr) => expr,
            Err(expr) => {
                return $self.error(Error::type_mismatch(
                    &$target.name(&$self.scopes),
                    &expr.ty.name(&$self.scopes),
                    span,
                ));
            }
        }
    }};
}

macro_rules! resolve_type {
    ($self: expr, $ty: expr) => {
        let mut ty = std::mem::take(&mut $ty);
        if let Type::Unresolved(hint) = ty {
            ty = $self.enter_id_and_resolve(hint.1, |this| this.resolve_typehint(&hint.0));
        }
        $ty = ty;
    };
}

pub struct Module {
    pub scopes: Scopes,
    pub scope: ScopeId,
}

#[derive(Debug, EnumAsInner)]
pub enum ResolvedPath {
    UserType(GenericUserType),
    Func(GenericFunc),
    Var(VariableId),
    Module(ScopeId),
    Extension(ExtensionId),
    None(Error),
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
            let parsed = Pipeline::new(lib, this.diag).parse()?;
            this.diag = parsed.diag;
            this.check_one(&parsed.path, parsed.state.0);
        }

        Ok((
            Module {
                scope: this.check_one(path, module),
                scopes: this.scopes,
            },
            this.diag,
        ))
    }

    fn check_one(&mut self, path: &std::path::Path, module: Vec<Stmt>) -> ScopeId {
        let project = crate::derive_module_name(path);
        let is_root_module = |name: &str| (path.is_file() && name == project) || name == "main";

        self.enter(
            ScopeKind::Module(project.clone(), Vec::new()),
            true,
            |this| {
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

                this.include_universal();
                for ast in declared {
                    for stmt in ast {
                        this.check_stmt(stmt);
                    }
                }

                this.universal.extend(autouse);
                this.current
            },
        )
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.diag.error(error);
        T::default()
    }

    fn current_function(&self) -> Option<FunctionId> {
        self.scopes.function_of(self.current)
    }

    fn current_this_type(&self) -> Option<Type> {
        let current = self.current_function();
        for (_, scope) in self.scopes.walk(self.current) {
            match scope.kind {
                ScopeKind::UserType(id) => {
                    let ty = self.scopes.get(id);
                    if !ty.data.is_template() {
                        if ty.data.is_trait() {
                            return Some(Type::TraitSelf);
                        }

                        return Some(self.scopes.this_type_of(id));
                    }
                }
                ScopeKind::Extension(id) => {
                    return Some(self.scopes.get(id).ty.clone());
                }
                ScopeKind::Function(f) if Some(f) != current => return None,
                _ => {}
            }
        }

        None
    }

    fn current(&mut self) -> &mut Scope {
        &mut self.scopes[self.current]
    }

    fn enter<T>(&mut self, kind: ScopeKind, public: bool, f: impl FnOnce(&mut Self) -> T) -> T {
        let id = self.scopes.create_scope(self.current, kind);
        self.scopes[self.current]
            .children
            .insert(Vis { id, public });
        self.enter_id(id, f)
    }

    fn find<T: ItemId>(&self, name: &str) -> Option<Vis<T>> {
        T::find(&self.scopes, self.current, name)
    }

    fn insert<T: ItemId>(&mut self, value: T::Value, public: bool) -> T {
        T::insert_in(&mut self.scopes, value, public, self.current)
    }

    fn find_module(&self, name: &str) -> Option<Vis<ScopeId>> {
        for (id, scope) in self.scopes.walk(self.current) {
            if let Some(item) = self.scopes.find_module_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_, _)) {
                break;
            }
        }

        None
    }

    fn find_free_fn(&self, name: &str) -> Option<Vis<FunctionId>> {
        for (id, scope) in self
            .scopes
            .walk(self.current)
            .filter(|(_, s)| !matches!(s.kind, ScopeKind::UserType(_)))
        {
            if let Some(item) = self.scopes.find_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_, _)) {
                break;
            }
        }

        None
    }

    fn can_access_privates(&self, scope: ScopeId) -> bool {
        let target = self
            .scopes
            .module_of(scope)
            .expect("root scope passed to can_access_privates()");
        self.scopes.walk(self.current).any(|(id, _)| id == target)
    }

    fn extensions_in_scope_for<'a, 'b>(
        &'a self,
        ty: &'b Type,
    ) -> impl Iterator<Item = &Scoped<Extension>> + 'b
    where
        'a: 'b,
    {
        self.scopes.walk(self.current).flat_map(|(_, scope)| {
            scope
                .exts
                .iter()
                .map(|ext| self.scopes.get(ext.id))
                .filter(|ext| ext.matches_type(ty))
        })
    }

    fn extension_ids_in_scope_for<'a, 'b>(
        &'a self,
        ty: &'b Type,
    ) -> impl Iterator<Item = ExtensionId> + 'b
    where
        'a: 'b,
    {
        self.scopes.walk(self.current).flat_map(|(_, scope)| {
            scope
                .exts
                .iter()
                .map(|ext| ext.id)
                .filter(|&id| self.scopes.get(id).matches_type(ty))
        })
    }
}

impl TypeChecker {
    fn declare_stmt(&mut self, autouse: &mut Vec<ScopeId>, stmt: Stmt) -> DeclaredStmt {
        match stmt.data {
            StmtData::Module { public, name, body } => {
                if self
                    .scopes
                    .find_module_in(&name.data, self.current)
                    .is_some()
                {
                    self.error(Error::new(
                        format!("redeclaration of module '{name}'"),
                        name.span,
                    ))
                }

                self.enter(ScopeKind::Module(name.data, Vec::new()), public, |this| {
                    let core = this
                        .scopes
                        .find_module_in("core", ScopeId::ROOT)
                        .map(|s| s.id);
                    let std = this
                        .scopes
                        .find_module_in("std", ScopeId::ROOT)
                        .map(|s| s.id);
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
            StmtData::Struct(base) => {
                let (body_scope, init) = self.enter(ScopeKind::None, base.public, |this| {
                    (
                        this.current,
                        this.enter(ScopeKind::None, false, |this| {
                            this.declare_fn(
                                autouse,
                                None,
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
                                                Pattern::Path(TypePath::from(
                                                    member.name.data.clone(),
                                                )),
                                            ),
                                            ty: member.ty.clone(),
                                            default: member.default.clone(),
                                        })
                                        .collect(),
                                    ret: Self::typehint_for_struct(&base.name, &base.type_params),
                                    body: None,
                                },
                                vec![],
                            )
                        }),
                    )
                });
                let id = self.declare_type(
                    UserType {
                        name: base.name,
                        body_scope,
                        data: UserTypeData::Struct {
                            members: Vec::new(),
                            init: init.id,
                        },
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    base.public,
                    stmt.attrs,
                );
                self.enter_id(body_scope, |this| {
                    this.current().kind = ScopeKind::UserType(id);
                    this.scopes.get_mut(init.id).constructor = Some(id);

                    this.scopes.get_mut(id).body_scope = this.current;
                    this.scopes.get_mut(id).type_params =
                        this.declare_type_params(base.type_params);

                    let mut members = Vec::with_capacity(base.members.len());
                    for member in base.members {
                        if members
                            .iter()
                            .any(|m: &CheckedMember| m.name == member.name.data)
                        {
                            this.error(Error::redefinition(
                                "member variable",
                                &member.name.data,
                                member.name.span,
                            ))
                        }

                        members.push(CheckedMember {
                            public: member.public,
                            name: member.name.data,
                            shared: member.shared,
                            ty: this.declare_type_hint(member.ty),
                        });
                    }

                    *this.scopes.get_mut(id).data.as_struct_mut().unwrap().0 = members;

                    let (impls, impl_blocks) = this.declare_impl_blocks(autouse, base.impls);
                    this.scopes.get_mut(id).impls = impls;

                    DeclaredStmt::Struct {
                        init,
                        id,
                        impl_blocks,
                        functions: base
                            .functions
                            .into_iter()
                            .map(|f| this.declare_fn(autouse, None, f, vec![]))
                            .collect(),
                    }
                })
            }
            StmtData::Union {
                tag: _,
                base,
                is_unsafe,
            } => {
                let ret = Self::typehint_for_struct(&base.name, &base.type_params);
                let id = self.declare_type(
                    UserType {
                        name: base.name,
                        body_scope: ScopeId::ROOT,
                        data: UserTypeData::Union(Union {
                            variants: Vec::new(),
                            is_unsafe,
                        }),
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    base.public,
                    stmt.attrs,
                );
                self.enter(ScopeKind::UserType(id), base.public, |this| {
                    this.scopes.get_mut(id).body_scope = this.current;
                    this.scopes.get_mut(id).type_params =
                        this.declare_type_params(base.type_params);

                    let mut variants = Vec::with_capacity(base.members.len());
                    let mut params = Vec::with_capacity(base.members.len());
                    for member in base.members.iter() {
                        if variants
                            .iter()
                            .any(|m: &CheckedMember| m.name == member.name.data)
                        {
                            this.error(Error::redefinition(
                                "member",
                                &member.name.data,
                                member.name.span,
                            ))
                        }

                        variants.push(CheckedMember {
                            public: member.public,
                            name: member.name.data.clone(),
                            shared: member.shared,
                            ty: this.declare_type_hint(member.ty.clone()),
                        });

                        if member.shared && is_unsafe {
                            this.error(Error::new(
                                "cannot have shared members in an unsafe union",
                                member.name.span,
                            ))
                        } else if member.shared {
                            params.push(Param {
                                keyword: true,
                                patt: Located::new(
                                    member.name.span,
                                    Pattern::Path(TypePath::from(member.name.data.clone())),
                                ),
                                ty: member.ty.clone(),
                                default: member.default.clone(),
                            });
                        }
                    }

                    this.scopes
                        .get_mut(id)
                        .data
                        .as_union_mut()
                        .unwrap()
                        .variants = variants;

                    let (impls, impl_blocks) = this.declare_impl_blocks(autouse, base.impls);
                    this.scopes.get_mut(id).impls = impls;
                    let member_cons = base
                        .members
                        .into_iter()
                        .filter(|m| !m.shared)
                        .map(|member| {
                            let mut params = params.clone();
                            if !matches!(member.ty, TypeHint::Void) {
                                params.push(Param {
                                    keyword: false,
                                    patt: Located::new(
                                        member.name.span,
                                        Pattern::Path(TypePath::from(member.name.data.clone())),
                                    ),
                                    ty: member.ty,
                                    default: member.default,
                                });
                            }

                            this.declare_fn(
                                autouse,
                                Some(id),
                                Fn {
                                    public: base.public,
                                    name: Located::new(Span::default(), member.name.data.clone()),
                                    linkage: Linkage::Internal,
                                    is_async: false,
                                    variadic: false,
                                    is_unsafe: false,
                                    type_params: vec![],
                                    params,
                                    ret: ret.clone(),
                                    body: None,
                                },
                                vec![],
                            )
                        })
                        .collect();

                    DeclaredStmt::Union {
                        member_cons,
                        id,
                        impl_blocks,
                        functions: base
                            .functions
                            .into_iter()
                            .map(|f| this.declare_fn(autouse, None, f, vec![]))
                            .collect(),
                    }
                })
            }
            StmtData::Trait {
                public,
                name,
                type_params,
                impls,
                functions,
                is_unsafe: _,
            } => {
                let id = self.declare_type(
                    UserType {
                        name,
                        body_scope: ScopeId::ROOT,
                        data: UserTypeData::Trait,
                        impls: Vec::new(),
                        type_params: Vec::new(),
                    },
                    public,
                    stmt.attrs,
                );
                self.enter(ScopeKind::UserType(id), public, |this| {
                    this.scopes.get_mut(id).body_scope = this.current;
                    this.scopes.get_mut(id).type_params = this.declare_type_params(type_params);
                    this.scopes.get_mut(id).impls = impls
                        .into_iter()
                        .map(|path| this.declare_type_path(path))
                        .collect();

                    DeclaredStmt::Trait {
                        id,
                        functions: functions
                            .into_iter()
                            .map(|f| this.declare_fn(autouse, None, f, vec![]))
                            .collect(),
                    }
                })
            }
            StmtData::Enum {
                public,
                name,
                impls,
                variants,
                functions,
                base_ty: _,
            } => {
                // TODO: should be the largest variant
                let backing = Type::discriminant_for(variants.len());
                let id = self.declare_type(
                    UserType {
                        name,
                        body_scope: ScopeId::ROOT,
                        data: UserTypeData::Enum(backing.clone()),
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    public,
                    stmt.attrs,
                );

                self.enter(ScopeKind::UserType(id), public, |this| {
                    this.scopes.get_mut(id).body_scope = this.current;
                    let mut n_variants: Vec<(VariableId, Option<Located<ExprData>>)> =
                        Vec::with_capacity(variants.len());
                    for (i, (name, expr)) in variants.into_iter().enumerate() {
                        if n_variants
                            .iter()
                            .any(|(id, _)| this.scopes.get(*id).name == name.data)
                        {
                            this.error(Error::redefinition("variant", &name.data, name.span))
                        }

                        n_variants.push((
                            this.insert(
                                Variable {
                                    name: name.data.clone(),
                                    ty: Type::User(
                                        GenericUserType::from_id(&this.scopes, id).into(),
                                    ),
                                    is_static: true,
                                    mutable: false,
                                    value: Some(CheckedExpr::new(
                                        backing.clone(),
                                        CheckedExprData::Integer(BigInt::from(i)),
                                    )),
                                },
                                true,
                            ),
                            expr,
                        ));
                    }

                    let (impls, impl_blocks) = this.declare_impl_blocks(autouse, impls);
                    this.scopes.get_mut(id).impls = impls;
                    DeclaredStmt::Enum {
                        id,
                        variants: n_variants,
                        impl_blocks,
                        functions: functions
                            .into_iter()
                            .map(|f| this.declare_fn(autouse, None, f, vec![]))
                            .collect(),
                    }
                })
            }
            StmtData::Extension {
                public,
                name,
                ty,
                type_params,
                impls,
                functions,
            } => {
                let id = self.insert(
                    Extension {
                        name,
                        ty: Type::Unknown,
                        impls: Vec::new(),
                        type_params: Vec::new(),
                        body_scope: ScopeId::ROOT,
                    },
                    public,
                );
                self.enter(ScopeKind::Extension(id), false, |this| {
                    this.scopes.get_mut(id).body_scope = this.current;
                    this.scopes.get_mut(id).type_params = this.declare_type_params(type_params);
                    this.scopes.get_mut(id).ty = this.declare_type_hint(ty);

                    let (impls, impl_blocks) = this.declare_impl_blocks(autouse, impls);
                    this.scopes.get_mut(id).impls = impls;

                    DeclaredStmt::Extension {
                        id,
                        impl_blocks,
                        functions: functions
                            .into_iter()
                            .map(|f| this.declare_fn(autouse, None, f, vec![]))
                            .collect(),
                    }
                })
            }
            StmtData::Fn(f) => DeclaredStmt::Fn(self.declare_fn(autouse, None, f, stmt.attrs)),
            StmtData::Static {
                public,
                name,
                ty,
                value,
            } => DeclaredStmt::Static {
                id: self.insert(
                    Variable {
                        name,
                        ty: self.declare_type_hint(ty),
                        is_static: true,
                        mutable: false,
                        value: None,
                    },
                    public,
                ),
                value,
            },
            StmtData::Use { path, public, all } => {
                self.current()
                    .use_stmts
                    .push(UnresolvedUse { path, public, all });
                DeclaredStmt::None
            }
            StmtData::Let { ty, value, patt } => DeclaredStmt::Let { ty, value, patt },
            StmtData::Expr(expr) => DeclaredStmt::Expr(expr),
            StmtData::Error => DeclaredStmt::None,
        }
    }

    fn declare_fn(
        &mut self,
        autouse: &mut Vec<ScopeId>,
        constructor: Option<UserTypeId>,
        f: Fn,
        attrs: Vec<Attribute>,
    ) -> DeclaredFn {
        if f.variadic && f.linkage != Linkage::Import {
            self.error(Error::new(
                "only import functions may be variadic",
                f.name.span,
            ))
        }

        if constructor.is_none()
            && self
                .scopes
                .find_in::<FunctionId>(&f.name.data, self.current)
                .is_some()
        {
            self.error(Error::new(
                format!("redeclaration of function '{}'", f.name.data),
                f.name.span,
            ))
        }

        let id = self.insert(
            Function {
                attrs,
                name: f.name,
                linkage: f.linkage,
                is_async: f.is_async,
                is_unsafe: f.is_unsafe,
                variadic: f.variadic,
                type_params: Vec::new(),
                params: Vec::new(),
                ret: Type::Unknown,
                body: None,
                body_scope: ScopeId::ROOT,
                returns: false,
                constructor,
            },
            f.public,
        );

        let attrs = &self.scopes.get::<FunctionId>(id).attrs;
        if attrs.iter().any(|attr| attr.name.data == "panic_handler") {
            self.scopes.panic_handler = Some(id);
            // TODO: verify the signature
        } else if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "intrinsic") {
            if let Some(attr) = attr.props.first() {
                match attr.name.data.as_str() {
                    "size_of" | "panic" => {
                        self.scopes.intrinsics.insert(id, attr.name.data.clone());
                    }
                    _ => self.error(Error::new(
                        format!("intrinsic '{}' is not supported", attr.name.data),
                        attr.name.span,
                    )),
                }
            } else {
                self.error(Error::new(
                    "intrinsic function must have name",
                    attr.name.span,
                ))
            }
        }

        self.enter(ScopeKind::Function(id), false, |this| {
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
                body: f.body.map(|body| {
                    body.into_iter()
                        .map(|stmt| this.declare_stmt(autouse, stmt))
                        .collect()
                }),
            }
        })
    }

    fn declare_type_params(
        &mut self,
        type_params: Vec<(String, Vec<Located<TypePath>>)>,
    ) -> Vec<UserTypeId> {
        type_params
            .into_iter()
            .map(|(name, impls)| {
                self.insert(
                    UserType {
                        name: Located::new(Span::default(), name),
                        body_scope: self.current,
                        data: UserTypeData::Template,
                        type_params: Vec::new(),
                        impls: impls
                            .into_iter()
                            .map(|path| self.declare_type_path(path))
                            .collect(),
                    },
                    false,
                )
            })
            .collect()
    }

    fn declare_impl_blocks(
        &mut self,
        autouse: &mut Vec<ScopeId>,
        blocks: Vec<ImplBlock>,
    ) -> (Vec<Type>, Vec<DeclaredImplBlock>) {
        let mut impls = Vec::new();
        let mut declared_blocks = Vec::new();
        for block in blocks {
            let span = block.path.span;
            let impl_index = impls.len();
            impls.push(self.declare_type_path(block.path));

            declared_blocks.push(self.enter(ScopeKind::None, false, |this| {
                DeclaredImplBlock {
                    impl_index,
                    span,
                    scope: this.current,
                    functions: block
                        .functions
                        .into_iter()
                        .map(|f| this.declare_fn(autouse, None, f, vec![]))
                        .collect(),
                }
            }));
        }

        (impls, declared_blocks)
    }

    fn declare_type(&mut self, ty: UserType, public: bool, attrs: Vec<Attribute>) -> UserTypeId {
        if self
            .scopes
            .find_in::<UserTypeId>(&ty.name.data, self.current)
            .is_some()
        {
            self.error(Error::redefinition("type", &ty.name.data, ty.name.span))
        }

        let id = self.insert(ty, public);
        if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "lang") {
            let Some(name) = attr.props.first() else {
                self.error::<()>(Error::new("language item must have name", attr.name.span));
                return id;
            };

            self.scopes.lang_types.insert(name.name.data.clone(), id);
        }

        id
    }

    fn declare_type_hint(&self, hint: TypeHint) -> Type {
        Type::Unresolved((hint, self.current).into())
    }

    fn declare_type_path(&self, path: Located<TypePath>) -> Type {
        self.declare_type_hint(TypeHint::Regular(path))
    }

    fn typehint_for_struct(
        Located { span, data }: &Located<String>,
        type_params: &[(String, Vec<Located<TypePath>>)],
    ) -> TypeHint {
        TypeHint::Regular(Located::new(
            *span,
            TypePath::Normal(vec![TypePathComponent(
                data.into(),
                type_params
                    .iter()
                    .map(|(n, _)| TypeHint::Regular(Located::new(*span, TypePath::from(n.clone()))))
                    .collect(),
            )]),
        ))
    }
}

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
                        this.include_universal();
                    }

                    for UnresolvedUse { public, path, all } in
                        std::mem::take(&mut this.current().use_stmts)
                    {
                        let span = path.span;
                        if let Some(path) = this.resolve_path(&path.data, span) {
                            this.resolve_use(public, all, path, span);
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
                    this.include_universal();
                    Block {
                        body: body.into_iter().map(|stmt| this.check_stmt(stmt)).collect(),
                        scope: this.current,
                    }
                });
            }
            DeclaredStmt::Struct {
                init,
                id,
                impl_blocks,
                functions,
            } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.resolve_members(id);
                    this.check_fn(init);
                    this.check_impl_blocks(id, impl_blocks);

                    for f in functions {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Union {
                member_cons,
                id,
                impl_blocks,
                functions,
            } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.resolve_members(id);
                    this.check_impl_blocks(id, impl_blocks);

                    for f in member_cons {
                        this.check_fn(f);
                    }

                    for f in functions {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Trait { id, functions } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    for f in functions {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Enum {
                id,
                variants,
                functions,
                impl_blocks,
            } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    this.check_impl_blocks(id, impl_blocks);

                    for (var, expr) in variants {
                        // TODO: these should be constant expressions only
                        if let Some(expr) = expr {
                            this.scopes.get_mut(var).value =
                                Some(this.check_expr(expr, Some(&Type::Usize)));
                        }
                    }

                    for f in functions {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Extension {
                id,
                impl_blocks,
                functions,
            } => {
                self.enter_id(self.scopes.get(id).body_scope, |this| {
                    this.resolve_impls(id);
                    resolve_type!(this, this.scopes.get_mut(id).ty);
                    this.check_impl_blocks_inner(this.scopes.get(id).ty.clone(), id, impl_blocks);
                    for f in functions {
                        this.check_fn(f);
                    }
                });
            }
            DeclaredStmt::Expr(expr) => return CheckedStmt::Expr(self.check_expr(expr, None)),
            DeclaredStmt::Let { ty, value, patt } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_typehint(&ty);
                    if let Some(value) = value {
                        let value = self.type_check(value, &ty);
                        return self.check_var_stmt(&ty, Some(value), patt);
                    } else {
                        return self.check_var_stmt(&ty, None, patt);
                    }
                } else if let Some(value) = value {
                    let value = self.check_expr(value, None);
                    return self.check_var_stmt(&value.ty.clone(), Some(value), patt);
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

    fn check_var_stmt(
        &mut self,
        ty: &Type,
        value: Option<CheckedExpr>,
        patt: Located<Pattern>,
    ) -> CheckedStmt {
        let span = patt.span;
        let patt = self.check_pattern(true, ty, false, patt);
        if !patt.irrefutable {
            return self.error(Error::must_be_irrefutable("variable binding pattern", span));
        } else if value.is_none() && !matches!(patt.data, CheckedPatternData::Variable(_)) {
            return self.error(Error::new(
                "must provide a value with a destructuring assignment",
                span,
            ));
        }

        CheckedStmt::Let(patt, value)
    }

    fn signatures_match(
        scopes: &Scopes,
        this: &Type,
        lhs_id: FunctionId,
        rhs_id: FunctionId,
        rhs_ty: &GenericUserType,
    ) -> Result<(), String> {
        let lhs = scopes.get(lhs_id);
        let rhs = scopes.get(rhs_id);
        let mut ty_args = rhs_ty.ty_args.clone();
        for (i, &id) in rhs.type_params.iter().enumerate() {
            ty_args.insert(
                id,
                Type::User(GenericUserType::from_id(scopes, lhs.type_params[i]).into()),
            );
        }

        let compare_types = |a: &Type, mut b: Type| {
            b.fill_templates(&ty_args);
            b.fill_this(this);

            if a != &b {
                eprintln!("{a:?}\n{b:?}");
                eprintln!("{ty_args:?}");
                Err(format!(
                    "expected '{}', got '{}'",
                    a.name(scopes),
                    b.name(scopes)
                ))
            } else {
                Ok(())
            }
        };

        if let Err(err) = compare_types(&lhs.ret, rhs.ret.clone()) {
            return Err(format!("return type is incorrect: {err}"));
        }

        for (s, t) in lhs.params.iter().zip(rhs.params.iter().cloned()) {
            if let Err(err) = compare_types(&s.ty, t.ty) {
                return Err(format!("parameter '{}' is incorrect: {err}", t.label));
            }
        }

        for (&s, &t) in lhs.type_params.iter().zip(rhs.type_params.iter()) {
            let s = scopes.get(s);
            let t = scopes.get(t);
            let name = &t.name;
            // TODO: dont enfore impl order
            for (s, t) in s
                .impls
                .iter()
                .flat_map(|tr| tr.as_user())
                .zip(t.impls.iter().flat_map(|tr| tr.as_user()))
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

        if lhs.params.len() != rhs.params.len() {
            return Err(format!(
                "expected {} parameter(s), got {}",
                rhs.params.len(),
                lhs.params.len(),
            ));
        }

        if lhs.type_params.len() != rhs.type_params.len() {
            return Err(format!(
                "expected {} type parameter(s), got {}",
                rhs.type_params.len(),
                lhs.type_params.len(),
            ));
        }

        Ok(())
    }

    fn check_impl_block(&mut self, this: &Type, tr_ut: &GenericUserType, block: DeclaredImplBlock) {
        // TODO:
        //  - detect and fail on circular trait dependencies
        //  - default implementations
        let tr = self.scopes.get(tr_ut.id);
        for dep in tr.impls.iter().flat_map(|tr| tr.as_user()) {
            if !self.implements_trait(this, dep) {
                self.diag.error(Error::new(
                    format!(
                        "trait '{}' requires implementation of trait '{}'",
                        tr_ut.name(&self.scopes),
                        dep.name(&self.scopes)
                    ),
                    block.span,
                ));
            }
        }

        let mut functions: Vec<_> = self.scopes[tr.body_scope]
            .fns
            .iter()
            .map(|f| f.id)
            .collect();
        for f in block.functions {
            let Located {
                span: fn_span,
                data: fn_name,
            } = self.scopes.get(f.id).name.clone();
            let f_id = f.id;

            self.check_fn(f);
            let Some(pos) = functions
                .iter()
                .position(|&id| self.scopes.get(id).name.data == fn_name)
            else {
                self.error::<()>(Error::new(
                    format!(
                        "no function '{fn_name}' found in trait '{}'",
                        self.scopes.get(tr_ut.id).name
                    ),
                    fn_span,
                ));
                continue;
            };

            let tr_fn_id = functions.swap_remove(pos);
            self.resolve_proto(tr_fn_id);
            if let Err(err) = Self::signatures_match(&self.scopes, this, f_id, tr_fn_id, tr_ut) {
                self.error(Error::new(
                    format!("invalid implementation of function '{fn_name}': {err}"),
                    fn_span,
                ))
            }
        }

        for id in functions {
            self.error(Error::new(
                format!(
                    "must implement '{}::{}'",
                    tr_ut.name(&self.scopes),
                    self.scopes.get(id).name.data
                ),
                block.span,
            ))
        }
    }

    fn check_fn(&mut self, DeclaredFn { id, body }: DeclaredFn) {
        // TODO: disallow private type in public interface
        self.enter_id_and_resolve(self.scopes.get(id).body_scope, |this| {
            this.resolve_proto(id);
            for i in 0..this.scopes.get(id).params.len() {
                if let ParamPattern::Unchecked(patt) =
                    this.scopes.get_mut(id).params[i].patt.clone()
                {
                    let span = patt.span;
                    let patt = this.check_pattern(
                        true,
                        &this.scopes.get(id).params[i].ty.clone(),
                        false,
                        patt,
                    );
                    if !patt.irrefutable {
                        this.error(Error::must_be_irrefutable("parameter patterns", span))
                    } else {
                        this.scopes.get_mut(id).params[i].patt = ParamPattern::Checked(patt);
                    }
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

    fn check_impl_blocks(&mut self, id: UserTypeId, impls: Vec<DeclaredImplBlock>) {
        self.check_impl_blocks_inner(self.scopes.this_type_of(id), id, impls)
    }

    fn check_impl_blocks_inner<T: ItemId + Copy>(
        &mut self,
        this_ty: Type,
        id: T,
        impls: Vec<DeclaredImplBlock>,
    ) where
        T::Value: HasImplsAndTypeParams,
    {
        for block in impls {
            // TODO:
            // - impl type params (impl<T> Trait<T>)
            // - implement the same trait more than once
            self.enter_id(block.scope, |this| {
                if let Some(gtr) = this.scopes.get(id).get_impls()[block.impl_index].as_user() {
                    let gtr = gtr.clone();
                    this.check_impl_block(&this_ty, &gtr, block);
                    this.current().kind = ScopeKind::Impl(gtr.id);
                } else {
                    for f in block.functions {
                        this.check_fn(f);
                    }
                }
            });
        }
    }

    fn check_expr_inner(&mut self, expr: Expr, target: Option<&Type>) -> CheckedExpr {
        let span = expr.span;
        match expr.data {
            ExprData::Binary { op, left, right } => {
                let left = self.check_expr(*left, target);
                if left.ty.is_unknown() {
                    return Default::default();
                }

                let right = type_check_bail!(self, *right, &left.ty);
                if !left.ty.supports_binop(&self.scopes, op) {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for values of type {} and {}",
                            &left.ty.name(&self.scopes),
                            &right.ty.name(&self.scopes)
                        ),
                        span,
                    ))
                } else {
                    CheckedExpr::new(
                        match op {
                            BinaryOp::NoneCoalesce => todo!(),
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
            ExprData::Unary { op, expr } => self.check_unary(*expr, target, op),
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
                    match Self::consteval(&self.scopes, &count, Some(&Type::Usize)) {
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
                    match Self::consteval(&self.scopes, &count, Some(&Type::Usize)) {
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
            ExprData::Tuple(_) => todo!(),
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
                    let end = type_check_bail!(self, *end, &start.ty);
                    let item = if inclusive {
                        "range_inclusive"
                    } else {
                        "range"
                    };
                    CheckedExpr::new(
                        self.scopes
                            .make_lang_type(item, [start.ty.clone()])
                            .unwrap(),
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
                        self.scopes.make_lang_type(item, [end.ty.clone()]).unwrap(),
                        CheckedExprData::Instance {
                            members: [("end".into(), end)].into(),
                            variant: None,
                        },
                    )
                }
                (Some(start), None) => {
                    let start = self.check_expr(*start, None);
                    CheckedExpr::new(
                        self.scopes
                            .make_lang_type("range_from", [start.ty.clone()])
                            .unwrap(),
                        CheckedExprData::Instance {
                            members: [("start".into(), start)].into(),
                            variant: None,
                        },
                    )
                }
                (None, None) => todo!(),
            },
            ExprData::String(s) => CheckedExpr::new(
                self.scopes.make_lang_type("string", []).unwrap(),
                CheckedExprData::String(s),
            ),
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
                    CheckedExpr::option_null(
                        self.scopes
                            .make_lang_type("option", [inner.clone()])
                            .unwrap(),
                    )
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
                    .map(|target| target.strip_options(&self.scopes))
                    .filter(|target| matches!(target, Type::F32 | Type::F64))
                    .cloned()
                    .unwrap_or(Type::F64),
                CheckedExprData::Float(value),
            ),
            ExprData::Path(path) => match self.resolve_path(&path, span) {
                Some(ResolvedPath::Var(id)) => {
                    let var = self.scopes.get(id);
                    if !var.is_static
                        && self.current_function() != self.scopes.function_of(var.scope)
                    {
                        self.diag.error(Error::new(
                            "cannot reference local variable of enclosing function",
                            span,
                        ));
                    }

                    CheckedExpr::new(var.ty.clone(), CheckedExprData::Symbol(Symbol::Var(id)))
                }
                Some(ResolvedPath::Func(func)) => {
                    let f = self.scopes.get(func.id);
                    CheckedExpr::new(
                        Type::FnPtr(
                            FnPtr {
                                params: f.params.iter().map(|p| p.ty.clone()).collect(),
                                ret: f.ret.clone(),
                            }
                            .into(),
                        ),
                        CheckedExprData::Symbol(Symbol::Func(func)),
                    )
                }
                Some(ResolvedPath::UserType(ut)) => self.error(Error::expected_found(
                    "expression",
                    &format!("type '{}'", ut.name(&self.scopes)),
                    span,
                )),
                Some(ResolvedPath::Extension(id)) => self.error(Error::expected_found(
                    "expression",
                    &format!("extension '{}'", self.scopes.get(id).name),
                    span,
                )),
                Some(ResolvedPath::Module(id)) => self.error(Error::expected_found(
                    "expression",
                    &format!(
                        "module '{}'",
                        self.scopes[id].kind.name(&self.scopes).unwrap()
                    ),
                    span,
                )),
                Some(ResolvedPath::None(err)) => self.error(err),
                None => Default::default(),
            },
            ExprData::Assign {
                target: lhs,
                binary,
                value,
            } => {
                let lhs_span = lhs.span;
                let lhs = self.check_expr(*lhs, None);
                if lhs.ty.is_unknown() {
                    return Default::default();
                }

                if !lhs.is_assignable(&self.scopes) {
                    // TODO: report a better error here
                    return self.error(Error::new("expression is not assignable", lhs_span));
                }

                let rhs = type_check_bail!(self, *value, &lhs.ty);
                if let Some(op) = binary {
                    if !lhs.ty.supports_binop(&self.scopes, op) {
                        self.error::<()>(Error::new(
                            format!(
                                "operator '{op}' is invalid for values of type {} and {}",
                                &lhs.ty.name(&self.scopes),
                                &rhs.ty.name(&self.scopes)
                            ),
                            span,
                        ));
                    }
                }

                CheckedExpr::new(
                    lhs.ty.clone(),
                    CheckedExprData::Assign {
                        target: lhs.into(),
                        binary,
                        value: rhs.into(),
                    },
                )
            }
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
                let (cond, mut if_branch) = if let ExprData::Is { expr, pattern } = cond.data {
                    self.check_is_expr(*expr, pattern, |this, cond| {
                        (
                            cond,
                            this.check_if_branch(*if_branch, target, else_branch.is_some()),
                        )
                    })
                } else {
                    let cond = self.type_check(*cond, &Type::Bool);
                    (
                        cond,
                        self.check_if_branch(*if_branch, target, else_branch.is_some()),
                    )
                };

                let mut out_type = if_branch.ty.clone();
                let else_branch = if let Some(expr) = else_branch {
                    if out_type.is_never() {
                        let expr = self.check_expr_inner(*expr, None);
                        out_type = expr.ty.clone();
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
                            out_type = self.scopes.make_lang_type("option", [out_type]).unwrap();
                            if_branch = if_branch.try_coerce_to(&self.scopes, &out_type);
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
                    Self::loop_out_type(&self.scopes, &self.scopes[body.scope].kind);
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
                let iter_id = self.scopes.lang_types.get("iter").copied().unwrap();
                let kind = ScopeKind::Loop {
                    target: Self::loop_target(&self.scopes, target, false).cloned(),
                    breaks: None,
                    infinite: false,
                };
                self.enter(kind, false, |this| {
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
                    let patt = this.check_pattern(true, &next_ty, false, patt);
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
                        Self::loop_out_type(&this.scopes, &this.scopes[this.current].kind);
                    CheckedExpr::new(
                        out,
                        CheckedExprData::For {
                            patt,
                            body,
                            optional,
                            iter: iter.into(),
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
                    _ => {
                        return self.error(Error::new(
                            format!("cannot get member of type '{}'", id.name(&self.scopes)),
                            span,
                        ));
                    }
                };

                if let Some(ut) = self.scopes.get(ut_id).members() {
                    for i in 0..ut.len() {
                        resolve_type!(
                            self,
                            self.scopes.get_mut(ut_id).members_mut().unwrap()[i].ty
                        );
                    }
                }

                let ty = self.scopes.get(ut_id);
                if let Some(member) = ty
                    .members()
                    .and_then(|members| members.iter().find(|m| m.name == name))
                {
                    if let Some(union) = ty.data.as_union() {
                        if !member.shared && !union.is_unsafe {
                            return self.error(Error::new(
                                "cannot access union variant with '.' (only shared members)",
                                span,
                            ));
                        }

                        if !member.shared && union.is_unsafe && self.safety != Safety::Unsafe {
                            self.diag.error(Error::is_unsafe(span));
                        }
                    }

                    if !member.public && !self.can_access_privates(ty.scope) {
                        self.diag.error(Error::private_member(
                            &id.name(&self.scopes),
                            &member.name,
                            span,
                        ));
                    }

                    let mut ty = member.ty.clone();
                    if let Some(instance) = id.as_user() {
                        ty.fill_templates(&instance.ty_args);
                    }

                    let id = id.clone();
                    return CheckedExpr::new(
                        ty,
                        CheckedExprData::Member {
                            source: source.auto_deref(&id).into(),
                            member: name,
                        },
                    );
                }

                self.error(Error::no_member(&source.ty.name(&self.scopes), &name, span))
            }
            ExprData::Subscript { callee, args } => {
                if args.len() > 1 {
                    self.error::<()>(Error::new(
                        "multidimensional subscript is not supported",
                        args[1].span,
                    ));
                }

                let callee = self.check_expr(*callee, None);
                let arg = type_check_bail!(self, args.into_iter().next().unwrap(), &Type::Isize);
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
            ExprData::Tail(expr) => match &self.current().kind {
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

                    let (target, opt) = Self::loop_out_type(&self.scopes, &self.scopes[id].kind);
                    if opt {
                        Some(expr.try_coerce_to(&self.scopes, &target).into())
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
                let mut result = Vec::new();
                for (pattern, expr) in body.into_iter() {
                    let span = expr.span;
                    let (pattern, mut expr) = self.enter(ScopeKind::None, false, |this| {
                        (
                            this.check_full_pattern(&scrutinee.ty, pattern),
                            this.check_expr(expr, target.as_ref()),
                        )
                    });

                    if let Some(target) = &target {
                        expr = self.type_check_checked(expr, target, span);
                    } else {
                        target = Some(expr.ty.clone());
                    }

                    result.push((
                        pattern,
                        CheckedExpr::new(Type::Never, CheckedExprData::Yield(expr.into())),
                    ));
                }

                self.check_match_coverage(&scrutinee.ty, result.iter().map(|it| &it.0), span);
                CheckedExpr::new(
                    target.unwrap_or(Type::Void),
                    CheckedExprData::Match {
                        expr: scrutinee.into(),
                        body: result,
                    },
                )
            }
            ExprData::As { expr, ty, throwing } => {
                let expr = self.check_expr(*expr, None);
                let ty = self.resolve_typehint(&ty);
                match expr.coerce_to(&self.scopes, &ty) {
                    Ok(expr) => expr,
                    Err(expr) => {
                        self.check_cast(&expr.ty, &ty, throwing, span);
                        CheckedExpr::new(ty, CheckedExprData::As(expr.into(), throwing))
                    }
                }
            }
            ExprData::Error => CheckedExpr::default(),
            ExprData::Lambda { params, ret, body } => {
                let ty_is_generic = |scopes: &Scopes, ty: &Type| {
                    !ty.as_user()
                        .is_some_and(|ut| scopes.get(ut.id).data.is_template())
                };

                let mut lparams = Vec::new();
                let ret = ret.map(|ret| self.resolve_typehint(&ret)).or_else(|| {
                    target
                        .as_ref()
                        .and_then(|ty| ty.as_fn_ptr())
                        .map(|f| &f.ret)
                        .filter(|ty| ty_is_generic(&self.scopes, ty))
                        .cloned()
                });
                // TODO: lambdas should have a unique type
                let (id, body) = self.enter(ScopeKind::Lambda(ret, false), false, |this| {
                    for (i, param) in params.into_iter().enumerate() {
                        let ty = param
                            .1
                            .map(|ty| this.resolve_typehint(&ty))
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
                                name: param.0.data,
                                ty,
                                is_static: false,
                                mutable: false,
                                value: None,
                            },
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
            match &mut self.current().kind {
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
        if let Some((a, b)) = lhs.integer_stats().zip(rhs.integer_stats()) {
            if (a.signed == b.signed || (a.signed && !b.signed)) && a.bits <= b.bits {
                return;
            }
            if (!a.signed && b.signed) && a.bits < b.bits {
                return;
            }
        }

        match (lhs, rhs) {
            (a, b) if a == b => {}
            (Type::Char, Type::Uint(num)) if *num >= 32 => {}
            (Type::Char, Type::Int(num)) if *num >= 33 => {}
            (Type::F32, Type::F64) => {}
            (Type::Ptr(_) | Type::MutPtr(_), Type::Usize) => {}
            (
                Type::Bool,
                Type::Int(_)
                | Type::Uint(_)
                | Type::CInt(_)
                | Type::CUint(_)
                | Type::Usize
                | Type::Isize,
            ) => {}

            (Type::Usize, Type::Ptr(_) | Type::MutPtr(_))
            | (Type::MutPtr(_) | Type::Ptr(_), Type::MutPtr(_) | Type::Ptr(_)) => {
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

    fn check_if_branch(&mut self, expr: Expr, target: Option<&Type>, el: bool) -> CheckedExpr {
        let target = if el {
            target
        } else {
            target
                .and_then(|t| t.as_user())
                .filter(|t| Some(t.id) == self.scopes.get_option_id())
                .and_then(|target| target.first_type_arg())
        };

        let if_span = expr.span;
        let expr = self.check_expr_inner(expr, target);
        if let Some(target) = target {
            self.type_check_checked(expr, target, if_span)
        } else {
            expr
        }
    }

    fn check_is_expr<T>(
        &mut self,
        expr: Expr,
        patt: Located<FullPattern>,
        f: impl FnOnce(&mut Self, CheckedExpr) -> T,
    ) -> T {
        self.enter(ScopeKind::None, false, |this| {
            let expr = this.check_expr(expr, None);
            let patt = this.check_full_pattern(&expr.ty, patt);
            f(
                this,
                CheckedExpr::new(Type::Bool, CheckedExprData::Is(expr.into(), patt)),
            )
        })
    }

    fn check_match_coverage<'a>(
        &mut self,
        ty: &Type,
        mut patterns: impl Iterator<Item = &'a CheckedPattern> + Clone,
        span: Span,
    ) {
        let ty = ty.strip_references();
        if let Some((mut value, max)) = ty
            .integer_stats()
            .map(|int| (int.min(), int.max()))
            .or_else(|| {
                ty.is_char()
                    .then(|| (BigInt::default(), BigInt::from(char::MAX as u32)))
            })
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
        } else if let Some(ut) = ty
            .as_user()
            .and_then(|ut| self.scopes.get(ut.id).data.as_union())
        {
            let mut missing = vec![];
            'outer: for v in ut.variants.iter().filter(|variant| !variant.shared) {
                for patt in patterns.clone() {
                    if patt.irrefutable {
                        return;
                    } else if patt
                        .data
                        .as_union_member()
                        .is_some_and(|(sub, variant, _)| {
                            &v.name == variant && sub.as_ref().map_or(true, |sub| sub.irrefutable)
                        })
                    {
                        continue 'outer;
                    }
                }

                missing.push(&v.name[..]);
            }

            if !missing.is_empty() {
                return self.error(Error::match_statement(
                    &format!("(missing variant(s) {})", missing.join(", ")),
                    span,
                ));
            }
        } else if !patterns.any(|patt| patt.irrefutable) {
            // covers structs and array patterns
            self.error(Error::match_statement("", span))
        }
    }

    fn check_yield(&mut self, expr: Expr) -> CheckedExpr {
        let ScopeKind::Block(target, _) = &self.current().kind else {
            return self.error(Error::new("yield outside of block", expr.span));
        };

        let target = target.clone();
        let span = expr.span;
        let mut expr = self.check_expr(expr, target.as_ref());
        if let Some(target) = &target {
            expr = self.type_check_checked(expr, target, span);
            self.current().kind = ScopeKind::Block(Some(target.clone()), true);
        } else {
            self.current().kind = ScopeKind::Block(Some(expr.ty.clone()), true);
        }

        CheckedExpr::new(Type::Never, CheckedExprData::Yield(expr.into()))
    }

    fn check_return(&mut self, expr: Expr) -> CheckedExpr {
        let lambda = self
            .scopes
            .walk(self.current)
            .find_map(|(id, scope)| match &scope.kind {
                ScopeKind::Lambda(target, _) => Some((id, target.clone())),
                _ => None,
            });
        if let Some((id, target)) = lambda {
            let span = expr.span;
            let mut expr = self.check_expr(expr, target.as_ref());
            if let Some(target) = &target {
                expr = self.type_check_checked(expr, target, span);
                self.scopes[id].kind = ScopeKind::Lambda(Some(target.clone()), true);
            } else {
                self.scopes[id].kind = ScopeKind::Lambda(Some(expr.ty.clone()), true);
            }

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

    fn check_unary(&mut self, expr: Expr, target: Option<&Type>, op: UnaryOp) -> CheckedExpr {
        use UnaryOp::*;

        let span = expr.span;
        macro_rules! invalid {
            ($ty: expr) => {
                if $ty.is_unknown() {
                    Default::default()
                } else {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for value of type {}",
                            $ty.name(&self.scopes)
                        ),
                        span,
                    ))
                }
            };
        }

        let (out_ty, expr) = match op {
            Plus => {
                let expr = self.check_expr(expr, target);
                if !expr.ty.is_numeric() {
                    invalid!(expr.ty)
                }
                (expr.ty.clone(), expr)
            }
            Neg => {
                let expr = self.check_expr(expr, target);
                if !matches!(
                    expr.ty,
                    Type::Int(_) | Type::Isize | Type::F32 | Type::F64 | Type::CInt(_)
                ) {
                    invalid!(expr.ty)
                }
                (expr.ty.clone(), expr)
            }
            PostIncrement | PostDecrement | PreIncrement | PreDecrement => {
                let span = expr.span;
                let expr = self.check_expr(expr, target);
                if expr.ty.is_any_int() {
                    if !expr.is_assignable(&self.scopes) {
                        self.error::<()>(Error::new("expression is not assignable", span));
                    }
                } else {
                    invalid!(expr.ty)
                }

                (expr.ty.clone(), expr)
            }
            Not => {
                let expr = self.check_expr(expr, target);
                if !(expr.ty.is_bool() || expr.ty.is_any_int()) {
                    invalid!(expr.ty)
                }
                (expr.ty.clone(), expr)
            }
            Deref => {
                let expr = if let Some(target) = target {
                    self.check_expr(expr, Some(&Type::Ptr(target.clone().into())))
                } else {
                    self.check_expr(expr, target)
                };

                if let Type::Ptr(inner) | Type::MutPtr(inner) = &expr.ty {
                    ((**inner).clone(), expr)
                } else {
                    (invalid!(expr.ty), expr)
                }
            }
            Addr => {
                let expr = self.check_expr(
                    expr,
                    target.and_then(|t| t.as_mut_ptr().or(t.as_ptr()).map(|t| &**t)),
                );
                (Type::Ptr(expr.ty.clone().into()), expr)
            }
            AddrMut => {
                let expr = self.check_expr(
                    expr,
                    target.and_then(|t| t.as_mut_ptr().or(t.as_ptr()).map(|t| &**t)),
                );
                if !expr.can_addrmut(&self.scopes) {
                    self.error::<()>(Error::new(
                        "cannot create mutable pointer to immutable memory location",
                        span,
                    ));
                }

                (Type::MutPtr(expr.ty.clone().into()), expr)
            }
            Unwrap => {
                let expr =
                    self.check_expr(expr, target.and_then(|t| t.as_option_inner(&self.scopes)));

                if let Some(inner) = expr.ty.as_option_inner(&self.scopes) {
                    let func = self.scopes.find_in(
                        "unwrap",
                        self.scopes
                            .get(self.scopes.get_option_id().unwrap())
                            .body_scope,
                    );

                    return CheckedExpr::new(
                        inner.clone(),
                        CheckedExprData::Call {
                            inst: Some(expr.ty.clone()),
                            args: IndexMap::from([(
                                THIS_PARAM.into(),
                                CheckedExpr::new(
                                    Type::Ptr(expr.ty.clone().into()),
                                    CheckedExprData::Unary {
                                        op: UnaryOp::Addr,
                                        expr: expr.into(),
                                    },
                                ),
                            )]),
                            func: GenericFunc::from_id(&self.scopes, *func.unwrap()),
                            trait_id: None,
                        },
                    );
                }
                (invalid!(expr.ty), expr)
            }
            Try => {
                let expr =
                    self.check_expr(expr, target.and_then(|t| t.as_option_inner(&self.scopes)));

                if let Some(inner) = expr.ty.as_option_inner(&self.scopes) {
                    // TODO: lambdas
                    if self
                        .current_function()
                        .and_then(|id| self.scopes.get(id).ret.as_option_inner(&self.scopes))
                        .is_none()
                    {
                        self.error(Error::new(
                            "operator '?' is only valid in functions that return Option",
                            span,
                        ))
                    }

                    (inner.clone(), expr)
                } else {
                    (invalid!(expr.ty), expr)
                }
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

    /// Returns `Ok(pattern)` if the path is a compatible union variant.
    ///
    /// Returns `Ok(CheckedPattern::Error)` if the path is a union variant, but of the wrong type.
    ///
    /// Returns `Err(err)` if the path is not a union variant.
    fn check_union_pattern(
        &mut self,
        scrutinee: &Type,
        path: ResolvedPath,
        subpatterns: Vec<Located<Pattern>>,
        span: Span,
    ) -> Result<CheckedPattern, Error> {
        let Some(ut) = scrutinee
            .strip_references()
            .as_user()
            .filter(|ut| self.scopes.get(ut.id).data.is_union())
        else {
            return Err(Error::new(
                format!(
                    "cannot use union pattern on type '{}'",
                    scrutinee.name(&self.scopes)
                ),
                span,
            ));
        };
        self.resolve_members(ut.id);

        let mut variant = String::new();
        let Some((union, path_ut)) = path
            .as_func()
            .map(|f| {
                self.resolve_proto(f.id);
                f
            })
            .map(|f| self.scopes.get(f.id))
            .filter(|f| f.constructor.is_some())
            .and_then(|f| {
                variant = f.name.data.clone();
                f.ret.as_user()
            })
            .and_then(|ut| self.scopes.get(ut.id).data.as_union().zip(Some(ut)))
        else {
            let msg = match path {
                ResolvedPath::UserType(ut) => format!(
                    "expected '{}', got type '{}'",
                    scrutinee.name(&self.scopes),
                    self.scopes.get(ut.id).name
                ),
                ResolvedPath::Func(f) => format!(
                    "expected '{}', got function '{}'",
                    scrutinee.name(&self.scopes),
                    self.scopes.get(f.id).name.data,
                ),
                ResolvedPath::Var(id) => format!(
                    "expected '{}', got variable '{}'",
                    scrutinee.name(&self.scopes),
                    self.scopes.get(id).name
                ),
                ResolvedPath::Module(id) => format!(
                    "expected '{}', got module '{}'",
                    scrutinee.name(&self.scopes),
                    self.scopes[id].kind.name(&self.scopes).unwrap()
                ),
                ResolvedPath::Extension(id) => format!(
                    "expected '{}', got extension '{}'",
                    scrutinee.name(&self.scopes),
                    self.scopes.get(id).name
                ),
                ResolvedPath::None(err) => return Err(err),
            };

            return Err(Error::new(msg, span));
        };

        if ut.id != path_ut.id {
            return Ok(self.error(Error::type_mismatch(
                &scrutinee.name(&self.scopes),
                &path_ut.name(&self.scopes),
                span,
            )));
        }

        let mut ty = union
            .variants
            .iter()
            .find(|m| m.name == variant)
            .unwrap()
            .ty
            .clone();
        ty.fill_templates(&ut.ty_args);
        if let Some(patt) = subpatterns.into_iter().next() {
            Ok(CheckedPattern::refutable(CheckedPatternData::UnionMember {
                variant,
                inner: ty.clone(),
                pattern: Some(
                    self.check_pattern(true, &scrutinee.matched_inner_type(ty), false, patt)
                        .into(),
                ),
            }))
        } else if ty.is_void() {
            Ok(CheckedPattern::refutable(CheckedPatternData::UnionMember {
                pattern: None,
                variant,
                inner: Type::Void,
            }))
        } else {
            Ok(self.error(Error::new(
                format!("union variant '{variant}' has data that must be bound"),
                span,
            )))
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
        let Some(stats) = inner.integer_stats() else {
            let (ty, _) = self.get_int_type_and_val(None, base, width, value, span);
            if ty.is_unknown() {
                return None;
            }

            return self.error(Error::type_mismatch(
                &target.name(&self.scopes),
                &ty.name(&self.scopes),
                span,
            ));
        };

        let (ty, value) = self.get_int_type_and_val(Some(inner), base, width, value, span);
        if &ty != inner {
            return self.error(Error::type_mismatch(
                &inner.name(&self.scopes),
                &ty.name(&self.scopes),
                span,
            ));
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
                        },
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
                result.push(self.check_pattern(true, &inner_ptr, false, patt));
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
                    return self.check_slice_pattern(ptr, inner, patterns, id);
                } else {
                    return self.check_slice_pattern(
                        Type::MutPtr(inner.clone().into()),
                        inner,
                        patterns,
                        span_mut_id.unwrap(),
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
                        },
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
                let patt = self.check_pattern(true, &inner, false, patt);
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
            data: CheckedPatternData::Array(ArrayPattern {
                rest,
                arr_len,
                inner: real_inner.clone(),
                patterns: result,
            }),
        }
    }

    fn check_struct_pattern(
        &mut self,
        scrutinee: &Type,
        mutable: bool,
        destructures: Vec<Destructure>,
        span: Span,
    ) -> CheckedPattern {
        let (ut, members) = if let Some(ut) = scrutinee.strip_references().as_user() {
            self.resolve_members(ut.id);
            if let UserTypeData::Struct { members, .. } = &self.scopes.get(ut.id).data {
                (ut, members)
            } else {
                return self.error(Error::bad_destructure(&scrutinee.name(&self.scopes), span));
            }
        } else {
            return self.error(Error::bad_destructure(&scrutinee.name(&self.scopes), span));
        };

        let cap = self.can_access_privates(self.scopes.get(ut.id).scope);
        let mut vars = Vec::new();
        for Destructure {
            name,
            mutable: pm,
            pattern,
        } in destructures
        {
            let Some(member) = members.iter().find(|m| m.name == name.data) else {
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
                    &member.name,
                    name.span,
                ));
                continue;
            }

            // TODO: duplicates
            let mut ty = member.ty.clone();
            ty.fill_templates(&ut.ty_args);
            vars.push((name.data, mutable || pm, ty, pattern));
        }

        let mut irrefutable = true;
        let checked: Vec<_> = vars
            .into_iter()
            .map(|(name, mutable, inner, patt)| {
                let ty = scrutinee.matched_inner_type(inner.clone());
                if let Some(patt) = patt {
                    let patt = self.check_pattern(true, &ty, mutable, patt);
                    if !patt.irrefutable {
                        irrefutable = false;
                    }
                    (name, inner, patt)
                } else {
                    (
                        name.clone(),
                        inner,
                        CheckedPattern::irrefutable(CheckedPatternData::Variable(self.insert(
                            Variable {
                                name,
                                ty,
                                is_static: false,
                                mutable,
                                value: None,
                            },
                            false,
                        ))),
                    )
                }
            })
            .collect();
        CheckedPattern {
            irrefutable,
            data: CheckedPatternData::Destrucure(checked),
        }
    }

    fn check_pattern(
        &mut self,
        binding: bool,
        scrutinee: &Type,
        mutable: bool,
        pattern: Located<Pattern>,
    ) -> CheckedPattern {
        let span = pattern.span;
        match pattern.data {
            Pattern::TupleLike { path, subpatterns } => {
                let Some(resolved) = self.resolve_path(&path.data, path.span) else {
                    return Default::default();
                };

                match self.check_union_pattern(scrutinee, resolved, subpatterns, span) {
                    Ok(pattern) => pattern,
                    Err(err) => self.error(err),
                }
            }
            Pattern::Path(path) => {
                let resolved = self.resolve_path(&path, span);
                if let Some(ident) = path.as_identifier() {
                    match resolved
                        .ok_or(Error::new("", span))
                        .and_then(|p| self.check_union_pattern(scrutinee, p, vec![], span))
                    {
                        Ok(CheckedPattern {
                            data: CheckedPatternData::Error,
                            ..
                        }) => Default::default(),
                        Ok(_) if binding => self.error(Error::new(
                            "cannot create binding that shadows union variant",
                            span,
                        )),
                        Ok(pattern) => pattern,
                        Err(_) => {
                            CheckedPattern::irrefutable(CheckedPatternData::Variable(self.insert(
                                Variable {
                                    name: ident.into(),
                                    ty: scrutinee.clone(),
                                    is_static: false,
                                    mutable,
                                    value: None,
                                },
                                false,
                            )))
                        }
                    }
                } else if let Some(resolved) = resolved {
                    match self.check_union_pattern(scrutinee, resolved, vec![], span) {
                        Ok(patt) => patt,
                        Err(err) => self.error(err),
                    }
                } else {
                    Default::default()
                }
            }
            Pattern::Option(patt) => {
                let Some(path) = self.resolve_path_in(
                    &[TypePathComponent("Some".into(), vec![])],
                    Default::default(),
                    self.scopes
                        .get(self.scopes.get_option_id().unwrap())
                        .body_scope,
                    pattern.span,
                ) else {
                    return self.error(Error::no_lang_item("option", pattern.span));
                };

                match self.check_union_pattern(
                    scrutinee,
                    path,
                    vec![Located::new(pattern.span, *patt)],
                    pattern.span,
                ) {
                    Ok(patt) => patt,
                    Err(err) => self.error(err),
                }
            }
            Pattern::Null => {
                let Some(path) = self.resolve_path_in(
                    &[TypePathComponent("None".into(), vec![])],
                    Default::default(),
                    self.scopes
                        .get(self.scopes.get_option_id().unwrap())
                        .body_scope,
                    pattern.span,
                ) else {
                    return self.error(Error::no_lang_item("option", pattern.span));
                };

                match self.check_union_pattern(scrutinee, path, vec![], pattern.span) {
                    Ok(patt) => patt,
                    Err(err) => self.error(err),
                }
            }
            Pattern::MutBinding(name) => {
                CheckedPattern::irrefutable(CheckedPatternData::Variable(self.insert(
                    Variable {
                        name,
                        ty: scrutinee.clone(),
                        is_static: false,
                        mutable: true,
                        value: None,
                    },
                    false,
                )))
            }
            Pattern::StructDestructure(destructures) => {
                self.check_struct_pattern(scrutinee, mutable, destructures, span)
            }
            Pattern::String(value) => {
                let string = self.scopes.make_lang_type("string", []).unwrap();
                if scrutinee.strip_references() != &string {
                    return self.error(Error::type_mismatch(
                        &scrutinee.name(&self.scopes),
                        &string.name(&self.scopes),
                        span,
                    ));
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
                        &scrutinee.name(&self.scopes),
                        &Type::Char.name(&self.scopes),
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
                        &scrutinee.name(&self.scopes),
                        &Type::Char.name(&self.scopes),
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
            Pattern::Array(subpatterns) => self.check_array_pattern(scrutinee, subpatterns, span),
            Pattern::Error => Default::default(),
        }
    }

    fn check_full_pattern(
        &mut self,
        scrutinee: &Type,
        pattern: Located<FullPattern>,
    ) -> CheckedPattern {
        self.check_pattern(false, scrutinee, false, pattern.map(|inner| inner.data))
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

                let Some((tr, func, ty_scope)) = self.get_member_fn(&id, &member) else {
                    return self.error(Error::new(
                        format!(
                            "no method '{member}' found on type '{}'",
                            id.name(&self.scopes)
                        ),
                        span,
                    ));
                };
                self.resolve_proto(func.id);

                let f = self.scopes.get(*func);
                let Some(this_param) = f.params.first().filter(|p| p.label == THIS_PARAM) else {
                    return self.error(Error::new(
                        format!("associated function '{member}' cannot be used as a method"),
                        span,
                    ));
                };

                if !func.public && !self.can_access_privates(ty_scope) {
                    return self.error(Error::new(
                        format!(
                            "cannot access private method '{member}' of type '{}'",
                            id.name(&self.scopes)
                        ),
                        span,
                    ));
                }

                if this_param.ty.is_mut_ptr() {
                    let mut ty = &recv.ty;
                    if !ty.is_ptr() && !ty.is_mut_ptr() && !recv.can_addrmut(&self.scopes) {
                        return self.error(Error::new(
                            format!("cannot call method '{member}' with immutable receiver"),
                            span,
                        ));
                    } else {
                        while let Type::MutPtr(inner) = ty {
                            ty = inner;
                        }

                        if matches!(ty, Type::Ptr(_)) {
                            return self.error(Error::new(
                                format!(
                                    "cannot call method '{member}' through an immutable pointer"
                                ),
                                span,
                            ));
                        }
                    }
                }

                let recv = if !matches!(recv.ty, Type::Ptr(_) | Type::MutPtr(_)) {
                    if matches!(this_param.ty, Type::Ptr(_)) {
                        CheckedExpr::new(
                            Type::Ptr(recv.ty.clone().into()),
                            CheckedExprData::Unary {
                                op: UnaryOp::Addr,
                                expr: recv.into(),
                            },
                        )
                    } else {
                        CheckedExpr::new(
                            Type::MutPtr(recv.ty.clone().into()),
                            CheckedExprData::Unary {
                                op: UnaryOp::AddrMut,
                                expr: recv.into(),
                            },
                        )
                    }
                } else {
                    recv.auto_deref(&this_param.ty)
                };

                let mut func = GenericFunc::new(
                    *func,
                    self.resolve_type_args(&f.type_params.clone(), &generics, span),
                );
                if let Some(inst) = tr.as_ref().or(id.as_user().map(|ut| &**ut)) {
                    func.ty_args.copy_args(&inst.ty_args);
                }

                let (args, ret) = self.check_fn_args(&mut func, Some(recv), args, target, span);
                return CheckedExpr::new(
                    ret,
                    CheckedExprData::Call {
                        func,
                        inst: Some(id),
                        args,
                        trait_id: tr.map(|ut| ut.id),
                    },
                );
            }
            ExprData::Path(ref path) => {
                match self.resolve_path(path, callee.span) {
                    Some(ResolvedPath::UserType(ty)) => {
                        let ut = self.scopes.get(ty.id);
                        let Some((_, constructor)) = ut.data.as_struct() else {
                            return self.error(Error::new(
                                format!("cannot construct type '{}'", ut.name),
                                span,
                            ));
                        };

                        // TODO: check privacy
                        let (args, ret) = self.check_fn_args(
                            &mut GenericFunc::new(*constructor, ty.ty_args),
                            None,
                            args,
                            target,
                            span,
                        );

                        return CheckedExpr::new(
                            ret,
                            CheckedExprData::Instance {
                                members: args,
                                variant: None,
                            },
                        );
                    }
                    Some(ResolvedPath::Func(mut func)) => {
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
                                    inst: None,
                                    trait_id: None,
                                }
                            },
                        );
                    }
                    Some(ResolvedPath::Var(_)) => {}
                    Some(ResolvedPath::Extension(id)) => {
                        return self.error(Error::new(
                            format!(
                                "expected callable, found extension '{}'",
                                self.scopes.get(id).name
                            ),
                            span,
                        ))
                    }
                    Some(ResolvedPath::Module(scope)) => {
                        return self.error(Error::new(
                            format!(
                                "expected callable, found module '{}'",
                                self.scopes[scope].kind.name(&self.scopes).unwrap()
                            ),
                            span,
                        ))
                    }
                    Some(ResolvedPath::None(err)) => return self.error(err),
                    None => return Default::default(),
                }
            }
            _ => {}
        }

        let callee = self.check_expr(callee, None);
        if callee.ty.is_unknown() {
            return Default::default();
        }

        if callee.ty.is_fn_ptr() {
            return self.call_fn_ptr(callee, args, span);
        }

        self.error(Error::new(
            format!(
                "expected callable item, got '{}'",
                &callee.ty.name(&self.scopes)
            ),
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

    fn check_arg(&mut self, func: &mut GenericFunc, expr: Expr, ty: &Type) -> CheckedExpr {
        let mut target = ty.clone();
        target.fill_templates(&func.ty_args);

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
                    Entry::Occupied(_) => {
                        self.error::<()>(Error::new(
                            format!("parameter '{name}' has already been specified"),
                            expr.span,
                        ));
                    }
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
                            self.error::<()>(Error::new(
                                format!("unknown parameter: '{name}'"),
                                expr.span,
                            ));
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
                self.error::<()>(Error::new("too many positional arguments", expr.span));
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

        let mut ret = self.scopes.get(func.id).ret.clone();
        if !func.ty_args.is_empty() {
            ret.fill_templates(&func.ty_args);
            for (id, ty) in func.ty_args.iter() {
                if ty.is_unknown() {
                    self.error::<()>(Error::new(
                        format!(
                            "cannot infer type for type parameter '{}'",
                            self.scopes.get(*id).name
                        ),
                        span,
                    ));

                    continue;
                }

                self.check_bounds(&func.ty_args, ty, self.scopes.get(*id).impls.clone(), span);
            }
        }

        if self.scopes.get(func.id).is_unsafe && self.safety != Safety::Unsafe {
            self.error(Error::is_unsafe(span))
        }

        (result, ret)
    }

    fn check_bounds(&mut self, ty_args: &TypeArgs, ty: &Type, bounds: Vec<Type>, span: Span) {
        for mut bound in bounds.iter().flat_map(|bound| bound.as_user().cloned()) {
            for bty in bound.ty_args.values_mut() {
                bty.fill_templates(ty_args);
            }

            if !self.implements_trait_and_resolve(ty, &bound) {
                self.error(Error::doesnt_implement(
                    &ty.name(&self.scopes),
                    &bound.name(&self.scopes),
                    span,
                ))
            }
        }
    }

    fn create_block(&mut self, body: Vec<Stmt>, kind: ScopeKind) -> Block {
        self.enter(kind, false, |this| Block {
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
        match source.coerce_to(&self.scopes, target) {
            Ok(expr) => expr,
            Err(expr) => self.error(Error::type_mismatch(
                &target.name(&self.scopes),
                &expr.ty.name(&self.scopes),
                span,
            )),
        }
    }

    fn resolve_lang_type(&mut self, name: &str, ty_args: &[TypeHint]) -> Type {
        if let Some(id) = self.scopes.lang_types.get(name).copied() {
            let args: Vec<_> = ty_args.iter().map(|ty| self.resolve_typehint(ty)).collect();
            Type::User(GenericUserType::from_type_args(&self.scopes, id, args).into())
        } else {
            self.error(Error::no_lang_item(name, Span::default()))
        }
    }

    fn make_lang_type(
        &mut self,
        id: UserTypeId,
        ty_args: impl IntoIterator<Item = Type>,
        span: Span,
    ) -> GenericUserType {
        let ty = GenericUserType::from_type_args(&self.scopes, id, ty_args);
        for (id, param) in ty.ty_args.iter() {
            for j in 0..self.scopes.get(*id).impls.len() {
                resolve_type!(self, self.scopes.get_mut(*id).impls[j]);
            }

            self.check_bounds(&ty.ty_args, param, self.scopes.get(*id).impls.clone(), span);
        }
        ty
    }

    fn resolve_typehint(&mut self, hint: &TypeHint) -> Type {
        match hint {
            TypeHint::Regular(path) => {
                let res = path.data.as_identifier().and_then(|symbol| match symbol {
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
                    _ => Type::from_int_name(symbol),
                });

                if let Some(res) = res {
                    return res;
                }

                match self.resolve_path(&path.data, path.span) {
                    Some(ResolvedPath::UserType(ut)) => Type::User(ut.into()),
                    Some(ResolvedPath::Func(_)) => {
                        self.error(Error::new("expected type, got function", path.span))
                    }
                    Some(ResolvedPath::Var(_)) => {
                        self.error(Error::new("expected type, got variable", path.span))
                    }
                    Some(ResolvedPath::Extension(_)) => {
                        self.error(Error::new("expected type, got extension", path.span))
                    }
                    Some(ResolvedPath::Module(id)) => self.error(Error::new(
                        format!(
                            "expected type, got module '{}'",
                            self.scopes[id].kind.name(&self.scopes).unwrap()
                        ),
                        path.span,
                    )),
                    Some(ResolvedPath::None(err)) => self.error(err),
                    None => Type::Unknown,
                }
            }
            TypeHint::Void => Type::Void,
            TypeHint::Ptr(ty) => Type::Ptr(self.resolve_typehint(ty).into()),
            TypeHint::MutPtr(ty) => Type::MutPtr(self.resolve_typehint(ty).into()),
            TypeHint::This(span) => self
                .current_this_type()
                .unwrap_or_else(|| self.error(Error::new("'This' outside of type", *span))),
            TypeHint::Array(ty, count) => {
                let n = match Self::consteval(&self.scopes, count, Some(&Type::Usize)) {
                    Ok(n) => n,
                    Err(err) => return self.error(err),
                };
                Type::Array((self.resolve_typehint(ty), n).into())
            }
            TypeHint::Option(ty) => self.resolve_lang_type("option", &[(**ty).clone()]),
            TypeHint::Vec(ty) => self.resolve_lang_type("vec", &[(**ty).clone()]),
            TypeHint::Map(key, value) => {
                self.resolve_lang_type("map", &[(**key).clone(), (**value).clone()])
            }
            TypeHint::Set(ty) => self.resolve_lang_type("set", &[(**ty).clone()]),
            TypeHint::Slice(ty) => self.resolve_lang_type("span", &[(**ty).clone()]),
            TypeHint::SliceMut(ty) => self.resolve_lang_type("span_mut", &[(**ty).clone()]),
            TypeHint::Tuple(_) => todo!(),
            TypeHint::Fn {
                is_extern: _,
                params,
                ret,
            } => Type::FnPtr(
                FnPtr {
                    params: params.iter().map(|p| self.resolve_typehint(p)).collect(),
                    ret: self.resolve_typehint(ret),
                }
                .into(),
            ),
            TypeHint::Error => Type::Unknown,
        }
    }

    fn resolve_use(&mut self, public: bool, all: bool, path: ResolvedPath, span: Span) -> bool {
        match path {
            ResolvedPath::UserType(ut) => {
                let name = &self.scopes.get(ut.id).name.data;
                if self
                    .scopes
                    .find_in::<FunctionId>(name, self.current)
                    .is_some()
                    || self
                        .scopes
                        .find_in::<UserTypeId>(name, self.current)
                        .is_some()
                {
                    self.error(Error::redefinition("name", name, span))
                }

                if all {
                    self.error(Error::wildcard_import(span))
                }

                self.current().types.insert(Vis { id: ut.id, public });
            }
            ResolvedPath::Func(func) => {
                let name = &self.scopes.get(func.id).name.data;
                if self
                    .scopes
                    .find_in::<UserTypeId>(name, self.current)
                    .is_some()
                    || self
                        .scopes
                        .find_in::<FunctionId>(name, self.current)
                        .is_some()
                    || self
                        .scopes
                        .find_in::<VariableId>(name, self.current)
                        .is_some()
                {
                    self.error(Error::redefinition("name", name, span))
                }
                if all {
                    self.error(Error::wildcard_import(span))
                }

                self.current().fns.insert(Vis {
                    id: func.id,
                    public,
                });
            }
            ResolvedPath::Var(id) => {
                let name = &self.scopes.get(id).name;
                if !self.scopes.get(id).is_static {
                    self.diag.error(Error::new(
                        format!(
                            "cannot import local variable '{}'",
                            self.scopes.get(id).name
                        ),
                        span,
                    ));
                }

                if self
                    .scopes
                    .find_in::<FunctionId>(name, self.current)
                    .is_some()
                    || self
                        .scopes
                        .find_in::<VariableId>(name, self.current)
                        .is_some()
                {
                    self.error(Error::redefinition("name", name, span))
                }

                if all {
                    self.error(Error::wildcard_import(span))
                }

                self.current().vars.insert(Vis { id, public });
            }
            ResolvedPath::Extension(id) => {
                let name = &self.scopes.get(id).name;
                if self
                    .scopes
                    .find_in::<ExtensionId>(name, self.current)
                    .is_some()
                {
                    self.error(Error::new(
                        format!("redefinition of extension {name}"),
                        span,
                    ))
                }

                if all {
                    self.error(Error::wildcard_import(span))
                }

                self.current().exts.insert(Vis { id, public });
            }
            ResolvedPath::Module(id) => {
                if !all {
                    let name = &self.scopes[id].kind.name(&self.scopes).unwrap();
                    if self.scopes.find_module_in(name, self.current).is_some() {
                        self.error(Error::new(format!("redefinition of module {name}"), span))
                    }

                    self.current().children.insert(Vis { id, public });
                } else {
                    for child in self.scopes[id].children.clone() {
                        if self.scopes[child.id].kind.is_module() && child.public {
                            self.current().children.insert(Vis {
                                id: child.id,
                                public,
                            });
                        }
                    }

                    for f in self.scopes[id].fns.clone() {
                        if f.public {
                            self.current().fns.insert(Vis { id: f.id, public });
                        }
                    }

                    for ut in self.scopes[id].types.clone() {
                        if ut.public {
                            self.current().types.insert(Vis { id: ut.id, public });
                        }
                    }

                    for var in self.scopes[id].vars.clone() {
                        if var.public {
                            self.current().vars.insert(Vis { id: var.id, public });
                        }
                    }

                    for ext in self.scopes[id].exts.clone() {
                        if ext.public {
                            self.current().exts.insert(Vis { id: ext.id, public });
                        }
                    }
                }
            }
            ResolvedPath::None(err) => {
                return self.error(err);
            }
        }

        true
    }

    fn resolve_members(&mut self, id: UserTypeId) {
        for i in 0..self.scopes.get(id).members().unwrap().len() {
            resolve_type!(self, self.scopes.get_mut(id).members_mut().unwrap()[i].ty);
        }
    }

    fn resolve_impls<T: ItemId + Copy>(&mut self, id: T)
    where
        T::Value: HasImplsAndTypeParams,
    {
        for i in 0..self.scopes.get(id).get_type_params().len() {
            self.resolve_impls(self.scopes.get(id).get_type_params()[i]);
        }

        for i in 0..self.scopes.get(id).get_impls().len() {
            resolve_type!(self, self.scopes.get_mut(id).get_impls_mut()[i]);
            let imp = &self.scopes.get(id).get_impls()[i];
            if !imp
                .as_user()
                .is_some_and(|t| self.scopes.get(t.id).data.is_trait())
                && !imp.is_unknown()
            {
                self.error(Error::new("expected trait", Span::default()))
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

    fn resolve_path(&mut self, path: &TypePath, span: Span) -> Option<ResolvedPath> {
        match path {
            TypePath::Root(data) => {
                self.resolve_path_in(data, Default::default(), ScopeId::ROOT, span)
            }
            TypePath::Super(data) => {
                if let Some(module) = self.scopes.module_of(
                    self.scopes[self.scopes.module_of(self.current).unwrap()]
                        .parent
                        .unwrap(),
                ) {
                    self.resolve_path_in(data, Default::default(), module, span)
                } else {
                    self.error(Error::new("cannot use super here", span))
                }
            }
            TypePath::Normal(data) => {
                let TypePathComponent(name, ty_args) = data.first().unwrap();
                let is_end = data.len() == 1;
                if let Some(id) = self.find(name) {
                    resolve_type!(self, self.scopes.get_mut::<VariableId>(*id).ty);
                    if is_end {
                        return Some(ResolvedPath::Var(*id));
                    }

                    if !ty_args.is_empty() {
                        return self
                            .error(Error::new("variables cannot have type arguments", span));
                    }

                    self.error(Error::new(format!("'{name}' is a variable"), span))
                } else if let Some(id) = self.find::<UserTypeId>(name) {
                    let ty_args = self.resolve_type_args(
                        &self.scopes.get(*id).type_params.clone(),
                        ty_args,
                        span,
                    );
                    if is_end {
                        let ut = GenericUserType::new(*id, ty_args);
                        //self.resolve_impls( id);
                        //self.check_bounds( None, &ut, &scopes.get(id).impls, span);
                        return Some(ResolvedPath::UserType(ut));
                    }

                    self.resolve_path_in(&data[1..], ty_args, self.scopes.get(*id).body_scope, span)
                } else if let Some(id) = self.find_free_fn(name) {
                    if is_end {
                        let f = self.scopes.get(*id);
                        return Some(ResolvedPath::Func(GenericFunc::new(
                            *id,
                            self.resolve_type_args(&f.type_params.clone(), ty_args, span),
                        )));
                    }

                    self.error(Error::new(format!("'{name}' is a function"), span))
                } else if let Some(id) = self.find(name) {
                    if is_end {
                        return Some(ResolvedPath::Extension(*id));
                    }

                    self.error(Error::no_symbol(name, span))
                } else if let Some(id) = self.find_module(name) {
                    if is_end {
                        return Some(ResolvedPath::Module(*id));
                    }

                    if !ty_args.is_empty() {
                        return self.error(Error::new(
                            "modules cannot be parameterized with type arguments",
                            span,
                        ));
                    }

                    self.resolve_path_in(&data[1..], Default::default(), *id, span)
                } else {
                    self.resolve_path_in(data, Default::default(), ScopeId::ROOT, span)
                }
            }
        }
    }

    fn resolve_path_in(
        &mut self,
        data: &[TypePathComponent],
        mut ty_args: TypeArgs,
        mut scope: ScopeId,
        span: Span,
    ) -> Option<ResolvedPath> {
        for (i, TypePathComponent(name, args)) in data.iter().enumerate() {
            let is_end = i + 1 == data.len();
            if let Some(id) = self.scopes.find_in::<VariableId>(name, scope) {
                resolve_type!(self, self.scopes.get_mut(*id).ty);
                if !id.public && !self.can_access_privates(scope) {
                    self.error(Error::new(format!("variable '{name}' is private"), span))
                }

                if !args.is_empty() {
                    self.error(Error::new(
                        "variables cannot be parameterized with type arguments",
                        span,
                    ))
                }

                if is_end {
                    return Some(ResolvedPath::Var(*id));
                }

                return self.error(Error::new(format!("'{name}' is a variable"), span));
            } else if let Some(id) = self.scopes.find_in::<UserTypeId>(name, scope) {
                if !id.public && !self.can_access_privates(scope) {
                    self.error(Error::new(format!("type '{name}' is private"), span))
                }

                let ty = self.scopes.get(*id);
                scope = ty.body_scope;

                let args = self.resolve_type_args(&ty.type_params.clone(), args, span);
                if is_end {
                    return Some(ResolvedPath::UserType(GenericUserType::new(*id, args)));
                }

                ty_args.copy_args(&args);
            } else if let Some(id) = self.scopes.find_in::<FunctionId>(name, scope) {
                if !id.public && !self.can_access_privates(scope) {
                    self.error(Error::new(format!("function '{name}' is private"), span))
                }

                ty_args.copy_args(&self.resolve_type_args(
                    &self.scopes.get(*id).type_params.clone(),
                    args,
                    span,
                ));
                if is_end {
                    return Some(ResolvedPath::Func(GenericFunc::new(*id, ty_args)));
                }

                return self.error(Error::new(format!("'{name}' is a function"), span));
            } else if let Some(id) = self.scopes.find_in(name, scope) {
                if !id.public && !self.can_access_privates(scope) {
                    self.error(Error::new(format!("extension '{name}' is private"), span))
                }

                if is_end {
                    return Some(ResolvedPath::Extension(*id));
                }

                return Some(ResolvedPath::None(Error::no_symbol(name, span)));
            } else if let Some(id) = self.scopes.find_module_in(name, scope) {
                if !id.public && !self.can_access_privates(*id) {
                    self.error(Error::new(format!("module '{name}' is private"), span))
                }

                if !args.is_empty() {
                    return self.error(Error::new(
                        "modules cannot be parameterized with type arguments",
                        span,
                    ));
                }

                if is_end {
                    return Some(ResolvedPath::Module(*id));
                }

                scope = *id;
            } else {
                return Some(ResolvedPath::None(Error::no_symbol(name, span)));
            }
        }

        unreachable!()
    }

    fn resolve_type_args(
        &mut self,
        params: &[UserTypeId],
        args: &[TypeHint],
        span: Span,
    ) -> TypeArgs {
        if !args.is_empty() && args.len() != params.len() {
            self.error(Error::new(
                format!(
                    "expected {} type arguments, received {}",
                    params.len(),
                    args.len()
                ),
                span,
            ))
        }

        TypeArgs(
            params
                .iter()
                .cloned()
                .zip(
                    args.iter()
                        .map(|ty| self.resolve_typehint(ty))
                        .take(params.len())
                        .chain(
                            std::iter::repeat(Type::Unknown)
                                .take(params.len().checked_sub(args.len()).unwrap_or_default()),
                        ),
                )
                .collect(),
        )
    }

    fn include_universal(&mut self) {
        for scope in self.universal.clone() {
            self.resolve_use(false, true, ResolvedPath::Module(scope), Span::default());
        }
    }

    fn implements_trait_and_resolve(&mut self, ty: &Type, bound: &GenericUserType) -> bool {
        if ty.is_unknown() {
            return true;
        }

        fn check(this: Option<&GenericUserType>, tr: &Type, bound: &GenericUserType) -> bool {
            let mut tr = tr.clone();
            if let Some(this) = this {
                tr.fill_templates(&this.ty_args);
            }
            tr.as_user().is_some_and(|tr| &**tr == bound)
        }

        let has_impl = ty.as_user().is_some_and(|this| {
            for i in 0..self.scopes.get(this.id).impls.len() {
                resolve_type!(self, self.scopes.get_mut(this.id).impls[i]);
                if check(Some(this), &self.scopes.get(this.id).impls[i], bound) {
                    return true;
                }
            }

            false
        });
        if has_impl {
            return true;
        }

        for id in self.extension_ids_in_scope_for(ty).collect::<Vec<_>>() {
            for i in 0..self.scopes.get(id).impls.len() {
                resolve_type!(self, self.scopes.get_mut(id).impls[i]);
                if check(
                    ty.as_user().map(|ty| &**ty),
                    &self.scopes.get(id).impls[i],
                    bound,
                ) {
                    return true;
                }
            }
        }

        false
    }

    fn implements_trait(&self, ty: &Type, bound: &GenericUserType) -> bool {
        if ty.is_unknown() {
            return true;
        }

        let search = |this: Option<&GenericUserType>, impls: &[Type]| {
            impls.iter().any(|tr| {
                let mut tr = tr.clone();
                if let Some(this) = this {
                    tr.fill_templates(&this.ty_args);
                }
                tr.as_user().is_some_and(|tr| &**tr == bound)
            })
        };

        if ty
            .as_user()
            .is_some_and(|this| search(Some(this), &self.scopes.get(this.id).impls))
        {
            return true;
        }

        self.extensions_in_scope_for(ty)
            .any(|ext| search(ty.as_user().map(|ty| &**ty), &ext.impls))
    }

    fn get_trait_impl_helper<Id: ItemId + Clone + Copy>(
        &mut self,
        id: Id,
        target: UserTypeId,
    ) -> Option<GenericUserType>
    where
        Id::Value: HasImplsAndTypeParams,
    {
        for i in 0..self.scopes.get(id).get_impls().len() {
            resolve_type!(self, self.scopes.get_mut(id).get_impls_mut()[i]);
            if let Some(ut) = self.scopes.get(id).get_impls()[i]
                .as_user()
                .map(|ut| &**ut)
                .filter(|ut| ut.id == target)
                .cloned()
            {
                return Some(ut);
            }
        }

        None
    }

    fn get_trait_impl(&mut self, ty: &Type, id: UserTypeId) -> Option<GenericUserType> {
        if let Some(ut) = ty
            .as_user()
            .and_then(|ut| self.get_trait_impl_helper(ut.id, id))
        {
            Some(ut)
        } else {
            for ext in self.extension_ids_in_scope_for(ty).collect::<Vec<_>>() {
                if let Some(ut) = self.get_trait_impl_helper(ext, id) {
                    return Some(ut);
                }
            }

            None
        }
    }

    fn get_member_fn(
        &self,
        ty: &Type,
        member: &str,
    ) -> Option<(Option<GenericUserType>, Vis<FunctionId>, ScopeId)> {
        let search = |src_scope: ScopeId, scope: ScopeId| {
            // TODO: trait implement overload ie.
            // impl Eq<f32> { ... } impl Eq<i32> { ... }
            std::iter::once(scope)
                .chain(self.scopes[scope].children.iter().map(|s| s.id))
                .find_map(|scope| {
                    self.scopes
                        .find_in(member, scope)
                        .map(|func| (None, func, src_scope))
                })
        };

        if let Some(ut) = ty.as_user().map(|ut| self.scopes.get(ut.id)) {
            let src_scope = ut.scope;
            if ut.data.is_template() {
                for ut in ut.impls.iter().flat_map(|ut| ut.as_user()) {
                    if let Some(func) = self
                        .scopes
                        .find_in(member, self.scopes.get(ut.id).body_scope)
                    {
                        return Some((Some((**ut).clone()), func, src_scope));
                    }
                }

                return None;
            }

            if let Some(result) = search(src_scope, ut.body_scope) {
                return Some(result);
            }
        }

        self.extensions_in_scope_for(ty)
            .find_map(|ext| search(ext.scope, ext.body_scope))
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
            if let Some(ty) = Type::from_int_name(&width) {
                ty
            } else {
                return self.error(Error::new(
                    format!("invalid integer literal type: {width}"),
                    span,
                ));
            }
        } else {
            // FIXME: attempt to promote the literal if its too large for i32
            target
                .map(|target| target.strip_options(&self.scopes))
                .filter(|target| {
                    matches!(
                        target,
                        Type::Int(_)
                            | Type::Uint(_)
                            | Type::Isize
                            | Type::Usize
                            | Type::CInt(_)
                            | Type::CUint(_),
                    )
                })
                .cloned()
                .unwrap_or(Type::Int(32))
        };

        let stats = ty.integer_stats().unwrap();
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
                format!("integer literal is larger than its type allows ({max})"),
                span,
            ));
        }

        let min = stats.min();
        if result < min {
            return self.error(Error::new(
                format!("integer literal is smaller than its type allows ({min})"),
                span,
            ));
        }

        (ty, result)
    }

    fn consteval(scopes: &Scopes, expr: &Expr, target: Option<&Type>) -> Result<usize, Error> {
        match &expr.data {
            ExprData::Integer { base, value, width } => {
                if let Some(width) = width.as_ref().and_then(|width| Type::from_int_name(width)) {
                    if let Some(target) = target.filter(|&target| target != &width) {
                        return Err(Error::type_mismatch(
                            &target.name(scopes),
                            &width.name(scopes),
                            expr.span,
                        ));
                    }
                }

                match usize::from_str_radix(value, *base as u32) {
                    Ok(value) => Ok(value),
                    Err(_) => Err(Error::new("value cannot be converted to usize", expr.span)),
                }
            }
            _ => Err(Error::new(
                "expression is not compile time evaluatable",
                expr.span,
            )),
        }
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

    fn loop_out_type(scopes: &Scopes, kind: &ScopeKind) -> (Type, bool) {
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
                    scopes
                        .make_lang_type("option", [target.clone().unwrap()])
                        .unwrap(),
                    true,
                ),
                _ => (Type::Void, false),
            }
        }
    }
}
