use std::path::PathBuf;

use indexmap::{map::Entry, IndexMap};
use num_bigint::BigInt;
use num_traits::Num;

use crate::{
    ast::{
        checked::{
            ArrayPattern, Block, CheckedExpr, CheckedExprData, CheckedPattern, CheckedStmt,
            IrrefutablePattern, RestPattern, Symbol,
        },
        declared::{DeclaredFn, DeclaredImplBlock, DeclaredStmt, DeclaredStmtData},
        parsed::{
            Destructure, Expr, ExprData, Fn, ImplBlock, IntPattern, Param, Path, Pattern,
            RangePattern, Stmt, StmtData, TypeHint,
        },
        Attribute, BinaryOp, UnaryOp,
    },
    lexer::{Located, Span},
    parser::ParsedFile,
    sym::*,
    typeid::{CInt, FnPtr, GenericFunc, GenericUserType, Type},
    Error, Pipeline, THIS_PARAM, THIS_TYPE,
};

macro_rules! type_check_bail {
    ($self: expr, $scopes: expr, $source: expr, $target: expr) => {{
        let source = $source;
        let span = source.span;
        let source = $self.check_expr($scopes, source, Some($target));
        if !source.ty.coerces_to($scopes, $target) {
            return $self.error(Error::type_mismatch(
                &$target.name($scopes),
                &source.ty.name($scopes),
                span,
            ));
        }

        source.coerce_to($target, $scopes)
    }};
}

macro_rules! resolve_type {
    ($self: expr, $scopes: expr, $ty: expr) => {
        let mut ty = $ty.clone();
        $self.resolve_type($scopes, &mut ty);
        $ty = ty;
    };
}

pub struct Module {
    pub scopes: Scopes,
    pub errors: Vec<Error>,
    pub files: Vec<PathBuf>,
    pub scope: ScopeId,
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum Safety {
    #[default]
    Safe,
    Unsafe,
}

pub struct TypeChecker {
    errors: Vec<Error>,
    universal: Vec<ScopeId>,
    safety: Safety,
    decl: bool,
}

impl TypeChecker {
    pub fn check(
        path: &std::path::Path,
        module: Vec<ParsedFile>,
        libs: Vec<PathBuf>,
    ) -> anyhow::Result<Module> {
        let mut this = Self {
            errors: vec![],
            universal: Vec::new(),
            safety: Safety::Safe,
            decl: true,
        };
        let mut scopes = Scopes::new();
        let mut files: Vec<_> = module.iter().map(|file| file.path.clone()).collect();

        for lib in libs {
            let parsed = Pipeline::new(lib, files.len()).parse()?;
            this.check_one(&mut scopes, &parsed.path, parsed.state.0, &mut files);
        }

        Ok(Module {
            scope: this.check_one(&mut scopes, path, module, &mut vec![]),
            scopes,
            errors: this.errors,
            files,
        })
    }

    fn check_one(
        &mut self,
        scopes: &mut Scopes,
        path: &std::path::Path,
        module: Vec<ParsedFile>,
        paths: &mut Vec<PathBuf>,
    ) -> ScopeId {
        let project = crate::derive_module_name(path);
        scopes.enter(
            ScopeKind::Module(project.clone(), Vec::new()),
            true,
            |scopes| {
                paths.reserve(module.len());
                self.decl = true;
                let declared: Vec<_> = module
                    .into_iter()
                    .map(|file| {
                        self.errors.extend(file.errors);
                        paths.push(file.path);

                        let mut ast = Vec::new();
                        match file.ast.data {
                            StmtData::Module { name, body, .. } if name == project => {
                                self.include_universal(scopes);
                                ast.extend(
                                    body.into_iter().map(|stmt| self.declare_stmt(scopes, stmt)),
                                );
                            }
                            _ => ast.push(self.declare_stmt(scopes, file.ast)),
                        }
                        ast
                    })
                    .collect();
                self.decl = false;
                for ast in declared {
                    for stmt in ast {
                        self.check_stmt(scopes, stmt);
                    }
                }

                scopes.current
            },
        )
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.errors.push(error);
        T::default()
    }

    fn insert_type(
        &mut self,
        scopes: &mut Scopes,
        ty: UserType,
        public: bool,
        attrs: &[Attribute],
        span: Span,
    ) -> UserTypeId {
        if scopes.find::<UserTypeId>(&ty.name).is_some() {
            self.error(Error::name_redef(&ty.name, span))
        }

        let id = scopes.insert(ty, public);
        if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "lang") {
            let Some(name) = attr.props.first() else {
                self.error::<()>(Error::new("language item must have name", attr.name.span));
                return id;
            };

            scopes.lang_types.insert(name.name.data.clone(), id);
        }

        id
    }

    fn declare_stmt(
        &mut self,
        scopes: &mut Scopes,
        Stmt { data, span, attrs }: Stmt,
    ) -> DeclaredStmt {
        let data = match data {
            StmtData::Module { public, name, body } => {
                if scopes.find_module_in(&name, scopes.current).is_some() {
                    self.error(Error::new(
                        format!("redeclaration of module '{name}'"),
                        span,
                    ))
                }

                scopes.enter(ScopeKind::Module(name, Vec::new()), public, |scopes| {
                    // FIXME: only allow core and std to define these
                    if attrs.iter().any(|attr| attr.name.data == "autouse") {
                        self.universal.push(scopes.current);
                    }

                    DeclaredStmtData::Module {
                        id: scopes.current,
                        body: body
                            .into_iter()
                            .map(|stmt| self.declare_stmt(scopes, stmt))
                            .collect(),
                    }
                })
            }
            StmtData::Struct(base) => {
                let init = scopes.enter(ScopeKind::None, false, |scopes| {
                    self.declare_fn(
                        scopes,
                        true,
                        Fn {
                            public: base.public && !base.members.iter().any(|m| !m.public),
                            name: base.name.clone(),
                            is_async: false,
                            is_extern: false,
                            variadic: false,
                            is_unsafe: false,
                            type_params: base.type_params.clone(),
                            params: base
                                .members
                                .iter()
                                .map(|member| Param {
                                    mutable: false,
                                    keyword: true,
                                    name: member.name.clone(),
                                    ty: member.ty.clone(),
                                    default: member.default.clone(),
                                })
                                .collect(),
                            ret: Self::typehint_for_struct(
                                &base.name.data,
                                &base.type_params,
                                span,
                            ),
                            body: None,
                        },
                        &attrs,
                    )
                });
                let id = self.insert_type(
                    scopes,
                    UserType {
                        name: base.name.data,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Struct {
                            members: Vec::new(),
                            init: init.id,
                        },
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    base.public,
                    &attrs,
                    base.name.span,
                );
                scopes.enter(ScopeKind::UserType(id), base.public, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;
                    scopes.get_mut(id).type_params =
                        self.declare_type_params(scopes, TT::Struct, base.type_params);
                    *scopes.get_mut(id).data.as_struct_mut().unwrap().0 = base
                        .members
                        .into_iter()
                        .map(|member| Member {
                            public: member.public,
                            name: member.name,
                            shared: member.shared,
                            ty: self.resolve_typehint(scopes, &member.ty),
                        })
                        .collect();
                    let (impls, impl_blocks) = self.declare_impl_blocks(scopes, base.impls);
                    scopes.get_mut(id).impls = impls;

                    DeclaredStmtData::Struct {
                        init,
                        id,
                        impl_blocks,
                        functions: base
                            .functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
                            .collect(),
                    }
                })
            }
            StmtData::Union {
                tag: _,
                base,
                is_unsafe,
            } => {
                let tp = base.type_params.clone();
                let ret = Self::typehint_for_struct(&base.name.data, &tp, span);
                let id = self.insert_type(
                    scopes,
                    UserType {
                        name: base.name.data,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Union(Union {
                            variants: Vec::new(),
                            is_unsafe,
                        }),
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    base.public,
                    &attrs,
                    base.name.span,
                );
                scopes.enter(ScopeKind::UserType(id), base.public, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;
                    scopes.get_mut(id).type_params =
                        self.declare_type_params(scopes, TT::Struct, base.type_params);

                    let mut variants = Vec::with_capacity(base.members.len());
                    let mut params = Vec::with_capacity(base.members.len());
                    for member in base.members.iter() {
                        variants.push(Member {
                            public: member.public,
                            name: member.name.clone(),
                            shared: member.shared,
                            ty: self.resolve_typehint(scopes, &member.ty),
                        });

                        if member.shared && is_unsafe {
                            // FIXME: span should be related to the member
                            self.error(Error::new(
                                "cannot have shared members in an unsafe union",
                                span,
                            ))
                        } else if member.shared {
                            params.push(Param {
                                mutable: false,
                                keyword: true,
                                name: member.name.clone(),
                                ty: member.ty.clone(),
                                default: member.default.clone(),
                            });
                        }
                    }

                    scopes.get_mut(id).data.as_union_mut().unwrap().variants = variants;

                    let (impls, impl_blocks) = self.declare_impl_blocks(scopes, base.impls);
                    scopes.get_mut(id).impls = impls;
                    let member_cons = base
                        .members
                        .into_iter()
                        .map(|member| {
                            let mut params = params.clone();
                            if !matches!(member.ty, TypeHint::Void) {
                                params.push(Param {
                                    mutable: false,
                                    keyword: false,
                                    name: member.name.clone(),
                                    ty: member.ty,
                                    default: member.default,
                                });
                            }

                            self.declare_fn(
                                scopes,
                                true,
                                Fn {
                                    public: base.public,
                                    name: Located::new(Span::default(), member.name.clone()),
                                    is_async: false,
                                    is_extern: false,
                                    variadic: false,
                                    is_unsafe: false,
                                    type_params: tp.clone(),
                                    params,
                                    ret: ret.clone(),
                                    body: None,
                                },
                                &[],
                            )
                        })
                        .collect();

                    DeclaredStmtData::Union {
                        member_cons,
                        id,
                        impl_blocks,
                        functions: base
                            .functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
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
                let id = self.insert_type(
                    scopes,
                    UserType {
                        name: name.data,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Trait,
                        impls: Vec::new(),
                        type_params: Vec::new(),
                    },
                    public,
                    &attrs,
                    name.span,
                );
                scopes.enter(ScopeKind::UserType(id), public, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;
                    scopes.get_mut(id).type_params =
                        self.declare_type_params(scopes, TT::Struct, type_params);

                    for path in impls.iter() {
                        if let Some(ty) = self.resolve_impl(scopes, path) {
                            scopes.get_mut(id).impls.push(ty);
                        }
                    }

                    DeclaredStmtData::Trait {
                        id,
                        functions: functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
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
            } => {
                // TODO: should be the largest variant
                let backing = Type::discriminant_for(variants.len());
                let id = self.insert_type(
                    scopes,
                    UserType {
                        name: name.data,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Enum(backing.clone()),
                        type_params: Vec::new(),
                        impls: Vec::new(),
                    },
                    public,
                    &attrs,
                    name.span,
                );

                scopes.enter(ScopeKind::UserType(id), public, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;

                    let variants = variants
                        .into_iter()
                        .enumerate()
                        .map(|(i, (name, expr))| {
                            (
                                scopes.insert(
                                    Variable {
                                        name: name.clone(),
                                        ty: Type::User(GenericUserType::new(id, vec![]).into()),
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
                            )
                        })
                        .collect();

                    let (impls, impl_blocks) = self.declare_impl_blocks(scopes, impls);
                    scopes.get_mut(id).impls = impls;
                    DeclaredStmtData::Enum {
                        id,
                        variants,
                        impl_blocks,
                        functions: functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
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
                let id = scopes.insert(
                    Extension {
                        name,
                        ty: Type::Unknown(None),
                        impls: Vec::new(),
                        type_params: Vec::new(),
                        body_scope: ScopeId(0),
                    },
                    public,
                );
                scopes.enter(ScopeKind::Extension(id), false, |scopes| {
                    scopes.get_mut(id).body_scope = scopes.current;
                    scopes.get_mut(id).type_params =
                        self.declare_type_params(scopes, TT::Struct, type_params);
                    scopes.get_mut(id).ty = self.resolve_typehint(scopes, &ty);

                    let (impls, impl_blocks) = self.declare_impl_blocks(scopes, impls);
                    scopes.get_mut(id).impls = impls;

                    DeclaredStmtData::Extension {
                        id,
                        impl_blocks,
                        functions: functions
                            .into_iter()
                            .map(|f| self.declare_fn(scopes, false, f, &[]))
                            .collect(),
                    }
                })
            }
            StmtData::Fn(f) => DeclaredStmtData::Fn(self.declare_fn(scopes, false, f, &attrs)),
            StmtData::Static {
                public,
                name,
                ty,
                value,
            } => {
                let id = scopes.insert(
                    Variable {
                        name,
                        ty: self.resolve_typehint(scopes, &ty),
                        is_static: true,
                        mutable: false,
                        value: None,
                    },
                    public,
                );

                DeclaredStmtData::Static { id, value }
            }
            StmtData::Use { path, public, all } => {
                if let Some(resolved) = self.resolve_path(scopes, &path, span) {
                    if resolved.is_none()
                        && scopes.module_of(scopes.current) == Some(scopes.current)
                    {
                        scopes
                            .unresolved_use_stmts(scopes.current)
                            .unwrap()
                            .push(UnresolvedUse {
                                path: path.clone(),
                                public,
                                all,
                                span,
                            });
                    }
                    if self.resolve_use(scopes, public, all, resolved, span) {
                        return DeclaredStmt {
                            data: DeclaredStmtData::None,
                            span,
                            attrs,
                        };
                    }
                }

                DeclaredStmtData::Use { public, path, all }
            }
            StmtData::Let { ty, value, patt } => DeclaredStmtData::Let { ty, value, patt },
            StmtData::Expr(expr) => DeclaredStmtData::Expr(expr),
            StmtData::Error => DeclaredStmtData::None,
        };

        DeclaredStmt { data, span, attrs }
    }

    fn declare_fn(
        &mut self,
        scopes: &mut Scopes,
        constructor: bool,
        f: Fn,
        attrs: &[Attribute],
    ) -> DeclaredFn {
        if f.variadic && !f.is_extern {
            self.error(Error::new(
                "only extern functions may be variadic",
                f.name.span,
            ))
        }

        if scopes
            .find_in::<FunctionId>(&f.name.data, scopes.current)
            .is_some()
        {
            self.error(Error::new(
                format!("redeclaration of function '{}'", f.name.data),
                f.name.span,
            ))
        }

        let id = scopes.insert(
            Function {
                attrs: attrs.to_vec(),
                name: f.name,
                is_async: f.is_async,
                is_extern: f.is_extern,
                is_unsafe: f.is_unsafe,
                variadic: f.variadic,
                type_params: Vec::new(),
                params: Vec::new(),
                ret: Type::Unknown(None),
                body: None,
                body_scope: ScopeId(0),
                returns: false,
                constructor,
            },
            f.public,
        );

        if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "intrinsic") {
            if let Some(attr) = attr.props.first() {
                match attr.name.data.as_str() {
                    "size_of" => {
                        scopes.intrinsics.insert(id, attr.name.data.clone());
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

        scopes.enter(ScopeKind::Function(id), false, |scopes| {
            scopes.get_mut(id).body_scope = scopes.current;
            scopes.get_mut(id).type_params =
                self.declare_type_params(scopes, TT::Func, f.type_params);
            scopes.get_mut(id).params = f
                .params
                .into_iter()
                .map(|param| CheckedParam {
                    mutable: param.mutable,
                    keyword: param.keyword,
                    name: param.name,
                    ty: self.resolve_typehint(scopes, &param.ty),
                    default: param
                        .default
                        .map(|expr| DefaultExpr::Unchecked(scopes.current, expr)),
                })
                .collect();
            scopes.get_mut(id).ret = self.resolve_typehint(scopes, &f.ret);

            DeclaredFn {
                id,
                body: f.body.map(|body| {
                    body.into_iter()
                        .map(|stmt| self.declare_stmt(scopes, stmt))
                        .collect()
                }),
            }
        })
    }

    fn declare_type_params(
        &mut self,
        scopes: &mut Scopes,
        tt: TT,
        type_params: Vec<(String, Vec<Located<Path>>)>,
    ) -> Vec<UserTypeId> {
        type_params
            .into_iter()
            .enumerate()
            .map(|(i, (name, impls))| {
                scopes.insert(
                    UserType {
                        name,
                        body_scope: scopes.current,
                        data: UserTypeData::Template(tt, i),
                        type_params: Vec::new(),
                        impls: impls
                            .iter()
                            .flat_map(|path| self.resolve_impl(scopes, path))
                            .collect(),
                    },
                    false,
                )
            })
            .collect()
    }

    fn declare_impl_blocks(
        &mut self,
        scopes: &mut Scopes,
        blocks: Vec<ImplBlock>,
    ) -> (Vec<Type>, Vec<DeclaredImplBlock>) {
        let mut impls = Vec::new();
        let mut declared_blocks = Vec::new();
        for block in blocks {
            let ty = self.resolve_impl(scopes, &block.path).unwrap_or_default();
            let impl_index = impls.len();
            impls.push(ty);

            declared_blocks.push(scopes.enter(ScopeKind::None, false, |scopes| {
                DeclaredImplBlock {
                    impl_index,
                    span: block.path.span,
                    scope: scopes.current,
                    functions: block
                        .functions
                        .into_iter()
                        .map(|f| self.declare_fn(scopes, false, f, &[]))
                        .collect(),
                }
            }));
        }

        (impls, declared_blocks)
    }

    fn check_stmt(&mut self, scopes: &mut Scopes, stmt: DeclaredStmt) -> CheckedStmt {
        match stmt.data {
            DeclaredStmtData::Module { id, body } => {
                return CheckedStmt::Module(scopes.enter_id(id, |scopes| {
                    self.include_universal(scopes);
                    Block {
                        body: body
                            .into_iter()
                            .map(|stmt| self.check_stmt(scopes, stmt))
                            .collect(),
                        scope: scopes.current,
                    }
                }))
            }
            DeclaredStmtData::Struct {
                init,
                id,
                impl_blocks,
                functions,
            } => {
                self.check_fn(scopes, init);
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    self.check_impl_blocks(scopes, id, impl_blocks);
                    for i in 0..scopes.get(id).data.as_struct().unwrap().0.len() {
                        resolve_type!(
                            self,
                            scopes,
                            scopes.get_mut(id).data.as_struct_mut().unwrap().0[i].ty
                        );
                    }

                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Union {
                member_cons,
                id,
                impl_blocks,
                functions,
            } => {
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    self.check_impl_blocks(scopes, id, impl_blocks);
                    for (i, f) in member_cons.into_iter().enumerate() {
                        resolve_type!(
                            self,
                            scopes,
                            scopes.get_mut(id).data.as_union_mut().unwrap().variants[i].ty
                        );

                        self.check_fn(scopes, f);
                    }

                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Trait { id, functions } => {
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Enum {
                id,
                variants,
                functions,
                impl_blocks,
            } => {
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    self.check_impl_blocks(scopes, id, impl_blocks);

                    for (var, expr) in variants {
                        // TODO: these should be constant expressions only
                        if let Some(expr) = expr {
                            scopes.get_mut(var).value =
                                Some(self.check_expr(scopes, expr, Some(&Type::Usize)));
                        }
                    }

                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Extension {
                id,
                impl_blocks,
                functions,
            } => {
                scopes.enter_id(scopes.get(id).body_scope, |scopes| {
                    self.resolve_impls(scopes, id);
                    resolve_type!(self, scopes, scopes.get_mut(id).ty);

                    let this = scopes.get_mut(id).ty.clone();
                    self.check_impl_blocks_inner(scopes, this, id, impl_blocks);

                    for f in functions {
                        self.check_fn(scopes, f);
                    }
                });
            }
            DeclaredStmtData::Expr(expr) => {
                return CheckedStmt::Expr(self.check_expr(scopes, expr, None))
            }
            DeclaredStmtData::Let { ty, value, patt } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_typehint(scopes, &ty);
                    if let Some(value) = value {
                        let value = self.type_check(scopes, value, &ty);
                        return self.check_var_stmt(scopes, ty, Some(value), patt);
                    } else {
                        return self.check_var_stmt(scopes, ty, None, patt);
                    }
                } else if let Some(value) = value {
                    let value = self.check_expr(scopes, value, None);
                    return self.check_var_stmt(scopes, value.ty.clone(), Some(value), patt);
                } else {
                    return self.error(Error::new("cannot infer type", stmt.span));
                }
            }
            DeclaredStmtData::Fn(f) => self.check_fn(scopes, f),
            DeclaredStmtData::Static { id, value } => {
                // FIXME: detect cycles like static X: usize = X;
                // FIXME: non-const statics should be disallowed
                let mut ty = scopes.get_mut(id).ty.clone();
                self.resolve_type(scopes, &mut ty);
                (scopes.get_mut(id).ty) = ty.clone();

                let value = self.type_check(scopes, value, &ty);
                let var = scopes.get_mut(id);
                var.value = Some(value);
            }
            DeclaredStmtData::Use { public, path, all } => {
                if let Some(path) = self.resolve_path(scopes, &path, stmt.span) {
                    self.resolve_use(scopes, public, all, path, stmt.span);
                }
            }
            DeclaredStmtData::None => return CheckedStmt::Error,
        }

        CheckedStmt::None
    }

    fn check_var_stmt(
        &mut self,
        scopes: &mut Scopes,
        ty: Type,
        value: Option<CheckedExpr>,
        patt: Located<Pattern>,
    ) -> CheckedStmt {
        let span = patt.span;
        match self.check_pattern(scopes, true, &ty, false, patt) {
            CheckedPattern::Irrefutable(IrrefutablePattern::Variable(id)) => {
                scopes.get_mut(id).value = value;
                CheckedStmt::Let(id)
            }
            CheckedPattern::Irrefutable(pattern) => {
                let Some(value) = value else {
                    return self.error(Error::new(
                        "must provide a value with a destructuring assignment",
                        span,
                    ));
                };

                CheckedStmt::LetPattern(pattern, value)
            }
            _ => self.error(Error::new(
                "variable binding pattern must be irrefutable",
                span,
            )),
        }
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
        let compare_types = |a: &Type, mut b: Type| {
            b.fill_func_template(
                scopes,
                &GenericFunc::new(
                    lhs_id,
                    lhs.type_params
                        .iter()
                        .map(|&id| Type::User(GenericUserType::new(id, vec![]).into()))
                        .collect(),
                ),
            );
            b.fill_struct_templates(scopes, rhs_ty);
            b.fill_this(this);

            if a != &b {
                Err(format!(
                    "expected '{}', got '{}'",
                    rhs.ret.name(scopes),
                    lhs.ret.name(scopes)
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
                return Err(format!("parameter '{}' is incorrect: {err}", t.name));
            }
        }

        for (&s, &t) in lhs.type_params.iter().zip(rhs.type_params.iter()) {
            let s = scopes.get(s);
            let t = scopes.get(t);
            let name = &t.name;
            for (s, t) in s.impls.iter().zip(t.impls.iter()) {
                for (s, t) in s
                    .as_user()
                    .unwrap()
                    .ty_args
                    .iter()
                    .zip(t.as_user().unwrap().ty_args.clone().into_iter())
                {
                    if let Err(err) = compare_types(s, t) {
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

    fn check_impl_block(
        &mut self,
        scopes: &mut Scopes,
        this: &Type,
        tr_ut: &GenericUserType,
        block: DeclaredImplBlock,
    ) {
        // TODO:
        //  - detect and fail on circular trait dependencies
        //  - default implementations
        let tr = scopes.get(tr_ut.id);
        for dep in tr.impls.iter().flat_map(|tr| tr.as_user()) {
            if !this.implements_trait(scopes, dep) {
                self.error(Error::new(
                    format!(
                        "trait '{}' requires implementation of trait '{}'",
                        tr_ut.name(scopes),
                        dep.name(scopes)
                    ),
                    block.span,
                ))
            }
        }

        let mut functions: Vec<_> = scopes[tr.body_scope].fns.iter().map(|f| f.id).collect();
        for f in block.functions {
            let Located {
                span: fn_span,
                data: fn_name,
            } = scopes.get(f.id).name.clone();
            let f_id = f.id;

            self.check_fn(scopes, f);
            let Some(pos) = functions
                .iter()
                .position(|&id| scopes.get(id).name.data == fn_name)
            else {
                self.error::<()>(Error::new(
                    format!(
                        "no function '{fn_name}' found in trait '{}'",
                        scopes.get(tr_ut.id).name
                    ),
                    fn_span,
                ));
                continue;
            };

            let tr_fn_id = functions.swap_remove(pos);
            self.resolve_proto(scopes, tr_fn_id);
            if let Err(err) = Self::signatures_match(scopes, this, f_id, tr_fn_id, tr_ut) {
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
                    tr_ut.name(scopes),
                    scopes.get(id).name.data
                ),
                block.span,
            ))
        }
    }

    fn check_fn(&mut self, scopes: &mut Scopes, DeclaredFn { id, body }: DeclaredFn) {
        // TODO: disallow private type in public interface
        scopes.enter_id(scopes.get(id).body_scope, |scopes| {
            self.resolve_proto(scopes, id);
            for param in scopes
                .get(id)
                .params
                .iter()
                .map(|param| Variable {
                    name: param.name.clone(),
                    ty: param.ty.clone(),
                    is_static: false,
                    mutable: param.mutable,
                    value: None,
                })
                .collect::<Vec<_>>()
            {
                scopes.insert::<VariableId>(param, false);
            }

            if let Some(body) = body {
                let old_safety = std::mem::take(&mut self.safety);
                let body = body
                    .into_iter()
                    .map(|stmt| self.check_stmt(scopes, stmt))
                    .collect();
                let func = scopes.get_mut(id);
                func.body = Some(body);
                if !func.returns && !func.ret.is_void() {
                    let span = func.name.span;
                    let name = func.name.data.clone();
                    let ret = func.ret.clone().name(scopes);
                    self.error(Error::new(
                        format!("function '{name}' must return a value of type '{ret}'"),
                        span,
                    ))
                }

                self.safety = old_safety;
            }
        });
    }

    fn check_impl_blocks(
        &mut self,
        scopes: &mut Scopes,
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
    ) {
        self.check_impl_blocks_inner(scopes, scopes.this_type_of(id), id, impls)
    }

    fn check_impl_blocks_inner<T: ItemId + Copy>(
        &mut self,
        scopes: &mut Scopes,
        this: Type,
        id: T,
        impls: Vec<DeclaredImplBlock>,
    ) where
        T::Value: HasImplsAndTypeParams,
    {
        for block in impls {
            // TODO:
            // - impl type params (impl<T> Trait<T>)
            // - implement the same trait more than once
            scopes.enter_id(block.scope, |scopes| {
                if let Some(gtr) = scopes.get(id).get_impls()[block.impl_index].as_user() {
                    let gtr = gtr.clone();
                    self.check_impl_block(scopes, &this, &gtr, block);
                    scopes.current().kind = ScopeKind::Impl(gtr.id);
                } else {
                    for f in block.functions {
                        self.check_fn(scopes, f);
                    }
                }
            });
        }
    }

    fn check_expr_inner(
        &mut self,
        scopes: &mut Scopes,
        expr: Expr,
        target: Option<&Type>,
    ) -> CheckedExpr {
        macro_rules! l {
            ($e: expr) => {
                Located::new(Span::default(), $e)
            };
        }

        let span = expr.span;
        match expr.data {
            ExprData::Binary { op, left, right } => {
                let left = self.check_expr(scopes, *left, target);
                if left.ty.is_unknown() {
                    return Default::default();
                }

                let right = type_check_bail!(self, scopes, *right, &left.ty);
                if !left.ty.supports_binop(scopes, op) {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for values of type {} and {}",
                            &left.ty.name(scopes),
                            &right.ty.name(scopes)
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
            ExprData::Unary { op, expr } => self.check_unary(scopes, *expr, target, op),
            ExprData::Call { callee, args } => self.check_call(scopes, target, *callee, args, span),
            ExprData::Array(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let ty = if let Some(Type::Array(inner)) = target {
                    inner.0.clone()
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(scopes, expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    return self.error(Error::new("cannot infer type of array literal", expr.span));
                };

                checked.extend(elements.map(|e| self.type_check(scopes, e, &ty)));
                CheckedExpr::new(
                    Type::Array(Box::new((ty, checked.len()))),
                    CheckedExprData::Array(checked),
                )
            }
            ExprData::Vec(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(vec) = scopes.lang_types.get("vec").copied() else {
                    return self.error(Error::no_lang_item("vec", expr.span));
                };

                let ty = if let Some(ty) = target
                    .and_then(|target| target.as_user())
                    .filter(|ut| ut.id == vec)
                    .map(|ut| ut.ty_args[0].clone())
                {
                    ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(scopes, expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    self.error(Error::new("cannot infer type of vector literal", expr.span))
                };

                checked.extend(elements.map(|e| self.type_check(scopes, e, &ty)));
                CheckedExpr::new(
                    Type::User(self.make_lang_type(scopes, vec, vec![ty], span).into()),
                    CheckedExprData::Vec(checked),
                )
            }
            ExprData::Set(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let Some(set) = scopes.lang_types.get("set").copied() else {
                    return self.error(Error::no_lang_item("set", span));
                };

                let ty = if let Some(ty) = target
                    .and_then(|target| target.as_user())
                    .filter(|ut| ut.id == set)
                    .map(|ut| ut.ty_args[0].clone())
                {
                    ty
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(scopes, expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    self.error(Error::new("cannot infer type of set literal", span))
                };

                checked.extend(elements.map(|e| self.type_check(scopes, e, &ty)));
                CheckedExpr::new(
                    Type::User(self.make_lang_type(scopes, set, vec![ty], span).into()),
                    CheckedExprData::Set(checked),
                )
            }
            ExprData::ArrayWithInit { init, count } => {
                if let Some(Type::Array(inner)) = target {
                    let init = self.type_check(scopes, *init, &inner.0);
                    match Self::consteval(scopes, &count, Some(&Type::Usize)) {
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
                    let init = self.check_expr(scopes, *init, target);
                    match Self::consteval(scopes, &count, Some(&Type::Usize)) {
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
                let Some(vec) = scopes.lang_types.get("vec").copied() else {
                    return self.error(Error::no_lang_item("vec", span));
                };

                let (init, ty) = if let Some(ty) = target
                    .and_then(|target| target.as_user())
                    .filter(|ut| ut.id == vec)
                    .map(|ut| ut.ty_args[0].clone())
                {
                    (self.type_check(scopes, *init, &ty), ty)
                } else {
                    let expr = self.check_expr(scopes, *init, None);
                    let ty = expr.ty.clone();
                    (expr, ty)
                };

                CheckedExpr::new(
                    Type::User(self.make_lang_type(scopes, vec, vec![ty], span).into()),
                    CheckedExprData::VecWithInit {
                        init: init.into(),
                        count: self.type_check(scopes, *count, &Type::Usize).into(),
                    },
                )
            }
            ExprData::Tuple(_) => todo!(),
            ExprData::Map(elements) => {
                let Some(map) = scopes.lang_types.get("map").copied() else {
                    return self.error(Error::no_lang_item("map", expr.span));
                };

                let mut result = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let (k, v) = if let Some((k, v)) = target
                    .and_then(|target| target.as_user())
                    .filter(|ut| ut.id == map)
                    .map(|ut| (ut.ty_args[0].clone(), ut.ty_args[1].clone()))
                {
                    (k, v)
                } else if let Some((key, val)) = elements.next() {
                    let key = self.check_expr(scopes, key, None);
                    let val = self.check_expr(scopes, val, None);

                    let k = key.ty.clone();
                    let v = val.ty.clone();
                    result.push((key, val));
                    (k, v)
                } else {
                    self.error(Error::new("cannot infer type of map literal", expr.span))
                };

                result.extend(elements.map(|(key, val)| {
                    (
                        self.type_check(scopes, key, &k),
                        self.type_check(scopes, val, &v),
                    )
                }));
                CheckedExpr::new(
                    Type::User(self.make_lang_type(scopes, map, vec![k, v], span).into()),
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
                    let start = self.check_expr(scopes, *start, None);
                    let end = type_check_bail!(self, scopes, *end, &start.ty);
                    let item = if inclusive {
                        "range_inclusive"
                    } else {
                        "range"
                    };
                    CheckedExpr::new(
                        scopes.make_lang_type(item, vec![start.ty.clone()]).unwrap(),
                        CheckedExprData::Instance {
                            members: [("start".into(), start), ("end".into(), end)].into(),
                            variant: None,
                        },
                    )
                }
                (None, Some(end)) => {
                    let end = self.check_expr(scopes, *end, None);
                    let item = if inclusive {
                        "range_to_inclusive"
                    } else {
                        "range_to"
                    };
                    CheckedExpr::new(
                        scopes.make_lang_type(item, vec![end.ty.clone()]).unwrap(),
                        CheckedExprData::Instance {
                            members: [("end".into(), end)].into(),
                            variant: None,
                        },
                    )
                }
                (Some(start), None) => {
                    let start = self.check_expr(scopes, *start, None);
                    CheckedExpr::new(
                        scopes
                            .make_lang_type("range_from", vec![start.ty.clone()])
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
                scopes.make_lang_type("string", vec![]).unwrap(),
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
                if let Some(inner) = target.and_then(|target| scopes.as_option_inner(target)) {
                    CheckedExpr::new(
                        scopes
                            .make_lang_type("option", vec![inner.clone()])
                            .unwrap(),
                        CheckedExprData::Instance {
                            members: [(
                                "None".into(),
                                self.check_expr(scopes, Located::new(span, ExprData::Void), target),
                            )]
                            .into(),
                            variant: Some("None".into()),
                        },
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
                let (ty, value) =
                    self.get_int_type_and_val(scopes, target, base, width, value, span);
                CheckedExpr::new(ty, CheckedExprData::Integer(value))
            }
            ExprData::Float(value) => CheckedExpr::new(
                target
                    .map(|target| target.strip_options(scopes))
                    .filter(|target| matches!(target, Type::F32 | Type::F64))
                    .cloned()
                    .unwrap_or(Type::F64),
                CheckedExprData::Float(value),
            ),
            ExprData::Path(path) => match self.resolve_path(scopes, &path, span) {
                Some(ResolvedPath::Var(id)) => {
                    let var = scopes.get(id);
                    if !var.is_static && scopes.current_function() != scopes.function_of(var.scope)
                    {
                        self.error(Error::new(
                            "cannot reference local variable of enclosing function",
                            span,
                        ))
                    }

                    CheckedExpr::new(var.ty.clone(), CheckedExprData::Symbol(Symbol::Var(id)))
                }
                Some(ResolvedPath::Func(func)) => {
                    let f = scopes.get(func.id);
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
                Some(ResolvedPath::UserType(ut)) => self.error(Error::new(
                    format!("expected expression, found type '{}'", ut.name(scopes)),
                    span,
                )),
                Some(ResolvedPath::Extension(id)) => self.error(Error::new(
                    format!(
                        "expected expression, found extension '{}'",
                        scopes.get(id).name
                    ),
                    span,
                )),
                Some(ResolvedPath::Module(id)) => self.error(Error::new(
                    format!(
                        "expected expression, found module '{}'",
                        scopes[id].kind.name(scopes).unwrap()
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
                let lhs = self.check_expr(scopes, *lhs, None);
                if lhs.ty.is_unknown() {
                    return Default::default();
                }

                if !lhs.is_assignable(scopes) {
                    // TODO: report a better error here
                    return self.error(Error::new("expression is not assignable", lhs_span));
                }

                let rhs = type_check_bail!(self, scopes, *value, &lhs.ty);
                if let Some(op) = binary {
                    if !lhs.ty.supports_binop(scopes, op) {
                        self.error::<()>(Error::new(
                            format!(
                                "operator '{op}' is invalid for values of type {} and {}",
                                &lhs.ty.name(scopes),
                                &rhs.ty.name(scopes)
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
                let block =
                    self.create_block(scopes, body, ScopeKind::Block(target.cloned(), false));
                let (target, yields) = scopes[block.scope].kind.as_block().unwrap();
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
                let cond = self.type_check(scopes, *cond, &Type::Bool);
                let target = if else_branch.is_some() {
                    target
                } else {
                    target
                        .and_then(|t| t.as_user())
                        .filter(|t| t.id == scopes.get_option_id().unwrap())
                        .map(|target| &target.ty_args[0])
                };

                let if_span = if_branch.span;
                let mut if_branch = self.check_expr_inner(scopes, *if_branch, target);
                if let Some(target) = target {
                    if_branch = self.type_check_checked(scopes, if_branch, target, if_span);
                }

                let mut out_type = if_branch.ty.clone();
                let else_branch = if let Some(expr) = else_branch {
                    if out_type.is_never() {
                        let expr = self.check_expr_inner(scopes, *expr, None);
                        out_type = expr.ty.clone();
                        Some(expr)
                    } else {
                        let span = expr.span;
                        let source = self.check_expr_inner(scopes, *expr, Some(&out_type));
                        Some(self.type_check_checked(scopes, source, &out_type, span))
                    }
                } else {
                    // this separates these two cases:
                    //   let x /* void? */ = if whatever { yield void; };
                    //   let x /* void */ = if whatever { };
                    if matches!(&if_branch.data, CheckedExprData::Block(b) if
                        matches!(scopes[b.scope].kind, ScopeKind::Block(_, yields) if yields))
                    {
                        if out_type.is_never() {
                            out_type = Type::Void;
                            Some(CheckedExpr::new(Type::Void, CheckedExprData::Void))
                        } else {
                            out_type = scopes.make_lang_type("option", vec![out_type]).unwrap();
                            if_branch = if_branch.coerce_to(&out_type, scopes);
                            Some(self.check_expr_inner(
                                scopes,
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
                // if let Some(Expr::Is { expr, pattern }) = cond.map(|cond| cond.data) {
                //
                // }

                let infinite = cond.is_none();
                let cond = cond.map(|cond| self.type_check(scopes, *cond, &Type::Bool));
                let target = Self::loop_target(scopes, target, infinite);
                let body = self.create_block(scopes, body, ScopeKind::Loop(target.cloned(), false));
                CheckedExpr::new(
                    Self::loop_out_type(scopes, &scopes[body.scope].kind, infinite),
                    CheckedExprData::Loop {
                        cond: cond.map(|cond| cond.into()),
                        body,
                        do_while,
                    },
                )
            }
            ExprData::For { patt, iter, body } => {
                let span = iter.span;
                let iter = self.check_expr(scopes, *iter, None);
                let iter_id = scopes.lang_types.get("iter").copied().unwrap();
                let target = Self::loop_target(scopes, target, false);
                let (patt, out_type, body) =
                    scopes.enter(ScopeKind::Loop(target.cloned(), false), false, |scopes| {
                        let Some(ty) = iter.ty.get_trait_impl(scopes, iter_id).cloned() else {
                            for stmt in body {
                                let stmt = self.declare_stmt(scopes, stmt);
                                self.check_stmt(scopes, stmt);
                            }

                            return self.error(Error::doesnt_implement(
                                &iter.ty.name(scopes),
                                "Iterator",
                                span,
                            ));
                        };

                        let mut next_ty = ty.ty_args[0].clone();
                        if let Some(ut) = iter.ty.as_user() {
                            next_ty.fill_struct_templates(scopes, ut);
                        }

                        let patt_span = patt.span;
                        let patt = match self.check_pattern(scopes, true, &next_ty, false, patt) {
                            CheckedPattern::Irrefutable(patt) => Some(patt),
                            _ => {
                                self.error(Error::new("for pattern must be irrefutable", patt_span))
                            }
                        };

                        (
                            patt,
                            Self::loop_out_type(scopes, &scopes[scopes.current].kind, false),
                            body.into_iter()
                                .map(|stmt| {
                                    let stmt = self.declare_stmt(scopes, stmt);
                                    self.check_stmt(scopes, stmt)
                                })
                                .collect(),
                        )
                    });

                patt.map(|patt| {
                    CheckedExpr::new(
                        out_type,
                        CheckedExprData::For {
                            iter: iter.into(),
                            patt,
                            body,
                        },
                    )
                })
                .unwrap_or_default()
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

                let source = self.check_expr(scopes, *source, None);
                let id = source.ty.strip_references();
                let ut_id = match &id {
                    Type::User(data) => data.id,
                    _ => {
                        return self.error(Error::new(
                            format!("cannot get member of type '{}'", id.name(scopes)),
                            span,
                        ));
                    }
                };

                if let Some(ut) = scopes.get(ut_id).members() {
                    for i in 0..ut.len() {
                        resolve_type!(
                            self,
                            scopes,
                            scopes.get_mut(ut_id).members_mut().unwrap()[i].ty
                        );
                    }
                }

                let ty = scopes.get(ut_id);
                if let Some(members) = ty.members() {
                    if let Some(member) = members.iter().find(|m| m.name == name) {
                        if let Some(union) = ty.data.as_union() {
                            if !member.shared && !union.is_unsafe {
                                return self.error(Error::new(
                                    "cannot access union variant with '.' (only shared members)",
                                    span,
                                ));
                            }

                            if !member.shared && union.is_unsafe && self.safety != Safety::Unsafe {
                                self.error(Error::is_unsafe(span))
                            }
                        }

                        if !member.public && !scopes.can_access_privates(ty.scope) {
                            self.error(Error::private_member(&id.name(scopes), &member.name, span))
                        }

                        let mut ty = member.ty.clone();
                        if let Some(instance) = id.as_user() {
                            ty.fill_struct_templates(scopes, instance);
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
                }

                self.error(Error::no_member(&source.ty.name(scopes), &name, span))
            }
            ExprData::Subscript { callee, args } => {
                if args.len() > 1 {
                    self.error::<()>(Error::new(
                        "multidimensional subscript is not supported",
                        args[1].span,
                    ));
                }

                let callee = self.check_expr(scopes, *callee, None);
                let arg =
                    type_check_bail!(self, scopes, args.into_iter().next().unwrap(), &Type::Isize);
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
                        format!("type {} cannot be subscripted", &callee.ty.name(scopes)),
                        span,
                    ))
                }
            }
            ExprData::Return(expr) => self.check_return(scopes, *expr),
            ExprData::Yield(expr) => self.check_yield(scopes, *expr),
            ExprData::Tail(expr) => match &scopes.current().kind {
                ScopeKind::Function(_) | ScopeKind::Lambda(_, _) => {
                    self.check_return(scopes, *expr)
                }
                ScopeKind::Loop(_, _) => self.type_check(scopes, *expr, &Type::Void),
                _ => self.check_yield(scopes, *expr),
            },
            ExprData::Break(expr) => {
                let Some((id, target)) = scopes.iter().find_map(|(id, scope)| match &scope.kind {
                    ScopeKind::Loop(target, _) => Some((id, target.clone())),
                    _ => None,
                }) else {
                    return self.error(Error::new("break outside of loop", span));
                };

                let span = expr.span;
                let mut expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    expr = self.type_check_checked(scopes, expr, target, span);
                    scopes[id].kind = ScopeKind::Loop(Some(target.clone()), true);
                } else {
                    scopes[id].kind = ScopeKind::Loop(Some(expr.ty.clone()), true);
                }

                CheckedExpr::new(Type::Never, CheckedExprData::Break(expr.into()))
            }
            ExprData::Continue => {
                if !scopes
                    .iter()
                    .any(|(_, scope)| matches!(scope.kind, ScopeKind::Loop(_, _)))
                {
                    return self.error(Error::new("continue outside of loop", span));
                }

                CheckedExpr::new(Type::Never, CheckedExprData::Continue)
            }
            ExprData::Is { expr, pattern } => self.check_expr(
                scopes,
                l!(ExprData::Match {
                    expr,
                    body: vec![
                        (pattern, l!(ExprData::Bool(true))),
                        (
                            l!(Pattern::Path(Path::from("_".to_string()))),
                            l!(ExprData::Bool(false))
                        )
                    ],
                }),
                Some(&Type::Bool),
            ),
            ExprData::Match { expr, body } => {
                let scrutinee = self.check_expr(scopes, *expr, None);
                let mut target = target.cloned();
                let mut result = Vec::new();
                for (pattern, expr) in body.into_iter() {
                    let span = expr.span;
                    let (pattern, mut expr) = scopes.enter(ScopeKind::None, false, |scopes| {
                        (
                            self.check_pattern(scopes, false, &scrutinee.ty, false, pattern),
                            self.check_expr(scopes, expr, target.as_ref()),
                        )
                    });

                    if let Some(target) = &target {
                        expr = self.type_check_checked(scopes, expr, target, span);
                    } else {
                        target = Some(expr.ty.clone());
                    }

                    result.push((
                        pattern,
                        CheckedExpr::new(Type::Never, CheckedExprData::Yield(expr.into())),
                    ));
                }

                CheckedExpr::new(
                    target.unwrap_or(Type::Void),
                    CheckedExprData::Match {
                        expr: scrutinee.into(),
                        body: result,
                    },
                )
            }
            ExprData::As { expr, ty, throwing } => {
                let mut expr = self.check_expr(scopes, *expr, None);
                let ty = self.resolve_typehint(scopes, &ty);
                if !expr.ty.coerces_to(scopes, &ty) {
                    match (&expr.ty, &ty) {
                        (a, b) if a == b => {}
                        (Type::Int(a), Type::Int(b) | Type::Uint(b)) if a <= b => {}
                        (Type::Uint(a), Type::Uint(b)) if a <= b => {}
                        (Type::Uint(a), Type::Int(b)) if (a + 1) <= *b => {}
                        (Type::CInt(a), Type::CInt(b) | Type::CUint(b)) if a <= b => {}
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
                        _ => {
                            expr = self.error(Error::new(
                                format!(
                                    "cannot{}cast expression of type '{}' to '{}'",
                                    if !throwing { " infallibly " } else { " " },
                                    expr.ty.name(scopes),
                                    ty.name(scopes)
                                ),
                                span,
                            ));
                        }
                    }

                    CheckedExpr::new(ty, CheckedExprData::As(expr.into(), throwing))
                } else {
                    expr.coerce_to(&ty, scopes)
                }
            }
            ExprData::Error => CheckedExpr::default(),
            ExprData::Lambda { params, ret, body } => {
                let ty_is_generic = |scopes: &Scopes, ty: &Type| {
                    !ty.as_user()
                        .map_or(false, |ut| scopes.get(ut.id).data.is_template())
                };

                let mut lparams = Vec::new();
                let ret = ret
                    .map(|ret| self.resolve_typehint(scopes, &ret))
                    .or_else(|| {
                        target
                            .as_ref()
                            .and_then(|ty| ty.as_fn_ptr())
                            .map(|f| &f.ret)
                            .filter(|ty| ty_is_generic(scopes, ty))
                            .cloned()
                    });
                // TODO: lambdas should have a unique type
                let (id, body) = scopes.enter(ScopeKind::Lambda(ret, false), false, |scopes| {
                    for (i, param) in params.into_iter().enumerate() {
                        let ty = param
                            .1
                            .map(|ty| self.resolve_typehint(scopes, &ty))
                            .or_else(|| {
                                target
                                    .as_ref()
                                    .and_then(|ty| ty.as_fn_ptr())
                                    .and_then(|f| f.params.get(i))
                                    .filter(|ty| ty_is_generic(scopes, ty))
                                    .cloned()
                            })
                            .unwrap_or_else(|| {
                                self.error(Error::new(
                                    format!("cannot infer type of parameter '{}'", param.0.data),
                                    param.0.span,
                                ))
                            });

                        lparams.push(ty.clone());
                        scopes.insert::<VariableId>(
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

                    // yield shouldn't work inside lambdas
                    let body = if let ExprData::Block(body) = body.data {
                        body.into_iter()
                            .map(|stmt| {
                                let stmt = self.declare_stmt(scopes, stmt);
                                self.check_stmt(scopes, stmt)
                            })
                            .collect()
                    } else {
                        vec![CheckedStmt::Expr(self.check_expr(
                            scopes,
                            Expr::new(body.span, ExprData::Return(body)),
                            None,
                        ))]
                    };

                    (scopes.current, body)
                });
                let (target, yields) = scopes[id].kind.as_lambda().unwrap();
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
                let expr = self.check_expr(scopes, *expr, target);
                self.safety = old_safety;
                expr
            }
        }
    }

    fn check_expr(
        &mut self,
        scopes: &mut Scopes,
        expr: Expr,
        target: Option<&Type>,
    ) -> CheckedExpr {
        let expr = self.check_expr_inner(scopes, expr, target);
        if expr.ty.is_never() && !matches!(expr.data, CheckedExprData::Yield(_)) {
            // TODO: lambdas
            match &mut scopes.current().kind {
                ScopeKind::Block(target, yields @ false) => {
                    *target = Some(Type::Never);
                    *yields = true;
                }
                &mut ScopeKind::Function(id) => {
                    scopes.get_mut(id).returns = true;
                }
                _ => {}
            }
        }
        expr
    }

    fn check_yield(&mut self, scopes: &mut Scopes, expr: Expr) -> CheckedExpr {
        let ScopeKind::Block(target, _) = &scopes.current().kind else {
            return self.error(Error::new("yield outside of block", expr.span));
        };

        let target = target.clone();
        let span = expr.span;
        let mut expr = self.check_expr(scopes, expr, target.as_ref());
        if let Some(target) = &target {
            expr = self.type_check_checked(scopes, expr, target, span);
            scopes.current().kind = ScopeKind::Block(Some(target.clone()), true);
        } else {
            scopes.current().kind = ScopeKind::Block(Some(expr.ty.clone()), true);
        }

        CheckedExpr::new(Type::Never, CheckedExprData::Yield(expr.into()))
    }

    fn check_return(&mut self, scopes: &mut Scopes, expr: Expr) -> CheckedExpr {
        let lambda = scopes.iter().find_map(|(id, scope)| match &scope.kind {
            ScopeKind::Lambda(target, _) => Some((id, target.clone())),
            _ => None,
        });
        if let Some((id, target)) = lambda {
            let span = expr.span;
            let mut expr = self.check_expr(scopes, expr, target.as_ref());
            if let Some(target) = &target {
                expr = self.type_check_checked(scopes, expr, target, span);
                scopes[id].kind = ScopeKind::Lambda(Some(target.clone()), true);
            } else {
                scopes[id].kind = ScopeKind::Lambda(Some(expr.ty.clone()), true);
            }

            CheckedExpr::new(Type::Never, CheckedExprData::Return(expr.into()))
        } else {
            let target = scopes
                .current_function()
                .map(|id| scopes.get(id).ret.clone())
                .expect("return should only be possible inside functions");
            CheckedExpr::new(
                Type::Never,
                CheckedExprData::Return(self.type_check(scopes, expr, &target).into()),
            )
        }
    }

    fn check_unary(
        &mut self,
        scopes: &mut Scopes,
        expr: Expr,
        target: Option<&Type>,
        op: UnaryOp,
    ) -> CheckedExpr {
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
                            $ty.name(scopes)
                        ),
                        span,
                    ))
                }
            };
        }

        let (out_ty, expr) = match op {
            Plus => {
                let expr = self.check_expr(scopes, expr, target);
                if !expr.ty.is_numeric() {
                    invalid!(expr.ty)
                }
                (expr.ty.clone(), expr)
            }
            Neg => {
                let expr = self.check_expr(scopes, expr, target);
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
                let expr = self.check_expr(scopes, expr, target);
                if expr.ty.integer_stats().is_some() {
                    if !expr.is_assignable(scopes) {
                        self.error::<()>(Error::new("expression is not assignable", span));
                    }
                } else {
                    invalid!(expr.ty)
                }

                (expr.ty.clone(), expr)
            }
            Not => {
                let expr = self.check_expr(scopes, expr, target);
                if !(expr.ty.is_bool() || expr.ty.integer_stats().is_some()) {
                    invalid!(expr.ty)
                }
                (expr.ty.clone(), expr)
            }
            Deref => {
                let expr = if let Some(target) = target {
                    self.check_expr(scopes, expr, Some(&Type::Ptr(target.clone().into())))
                } else {
                    self.check_expr(scopes, expr, target)
                };

                if let Type::Ptr(inner) | Type::MutPtr(inner) = &expr.ty {
                    ((**inner).clone(), expr)
                } else {
                    (invalid!(expr.ty), expr)
                }
            }
            Addr => {
                let expr = self.check_expr(
                    scopes,
                    expr,
                    target.and_then(|t| t.as_mut_ptr().or(t.as_ptr()).map(|t| &**t)),
                );
                (Type::Ptr(expr.ty.clone().into()), expr)
            }
            AddrMut => {
                let expr = self.check_expr(
                    scopes,
                    expr,
                    target.and_then(|t| t.as_mut_ptr().or(t.as_ptr()).map(|t| &**t)),
                );
                if !expr.can_addrmut(scopes) {
                    self.error::<()>(Error::new(
                        "cannot create mutable pointer to immutable memory location",
                        span,
                    ));
                }

                (Type::MutPtr(expr.ty.clone().into()), expr)
            }
            Unwrap => {
                let expr =
                    self.check_expr(scopes, expr, target.and_then(|t| scopes.as_option_inner(t)));

                if let Some(inner) = scopes.as_option_inner(&expr.ty) {
                    let func = scopes.find_in(
                        "unwrap",
                        scopes.get(scopes.get_option_id().unwrap()).body_scope,
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
                            func: GenericFunc::new(*func.unwrap(), vec![]),
                            trait_id: None,
                        },
                    );
                }
                (invalid!(expr.ty), expr)
            }
            Try => todo!(),
        };

        CheckedExpr::new(
            out_ty,
            CheckedExprData::Unary {
                op,
                expr: expr.into(),
            },
        )
    }

    /// Returns `Some(pattern)` if the path is a compatible union variant.
    ///
    /// Returns `Some(CheckedPattern::Error)` if the path is a union variant, but of the wrong type.
    ///
    /// Returns `None` if the path is not a union variant.
    fn check_union_pattern(
        &mut self,
        scopes: &mut Scopes,
        scrutinee: &Type,
        path: ResolvedPath,
        subpatterns: Vec<Located<Pattern>>,
        span: Span,
    ) -> Option<CheckedPattern> {
        let Some(ut) = scrutinee
            .strip_references()
            .as_user()
            .filter(|ut| scopes.get(ut.id).data.is_union())
        else {
            return None;
        };

        for i in 0..scopes.get_mut(ut.id).members_mut().unwrap().len() {
            resolve_type!(
                self,
                scopes,
                scopes.get_mut(ut.id).members_mut().unwrap()[i].ty
            );
        }

        let mut variant = String::new();
        let Some((union, path_ut)) = path
            .as_func()
            .map(|f| scopes.get(f.id))
            .filter(|f| f.constructor)
            .and_then(|f| {
                variant = f.name.data.clone();
                f.ret.as_user()
            })
            .and_then(|ut| scopes.get(ut.id).data.as_union().zip(Some(ut)))
        else {
            return None;
        };

        if ut.id != path_ut.id {
            return self.error(Error::type_mismatch(
                &scrutinee.name(scopes),
                &path_ut.name(scopes),
                span,
            ));
        }

        let member = union.variants.iter().find(|m| m.name == variant).unwrap();

        let mut ty = member.ty.clone();
        ty.fill_struct_templates(scopes, ut);

        let ptr = scrutinee.is_ptr() || scrutinee.is_mut_ptr();
        if let Some(pattern) = subpatterns.into_iter().next() {
            let span = pattern.span;
            let CheckedPattern::Irrefutable(pattern) = self.check_pattern(
                scopes,
                true,
                &scrutinee.matched_inner_type(ty),
                false,
                pattern,
            ) else {
                return Some(self.error(Error::new("union subpatterns must be irrefutable", span)));
            };

            Some(CheckedPattern::UnionMember {
                pattern: Some(pattern),
                variant,
                ptr,
            })
        } else if ty.is_void() {
            Some(CheckedPattern::UnionMember {
                pattern: None,
                variant,
                ptr,
            })
        } else {
            Some(self.error(Error::new(
                format!("union variant '{variant}' has data that must be bound"),
                span,
            )))
        }
    }

    fn check_int_pattern(
        &mut self,
        scopes: &mut Scopes,
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
            let (ty, _) = self.get_int_type_and_val(scopes, None, base, width, value, span);
            if ty.is_unknown() {
                return None;
            }

            return self.error(Error::type_mismatch(
                &target.name(scopes),
                &ty.name(scopes),
                span,
            ));
        };

        let (ty, value) = self.get_int_type_and_val(scopes, Some(inner), base, width, value, span);
        if &ty != inner {
            return self.error(Error::type_mismatch(
                &inner.name(scopes),
                &ty.name(scopes),
                span,
            ));
        }

        if !stats.signed && negative {
            return self.error(Error::new(
                format!("cannot negate unsigned integer type '{}'", ty.name(scopes)),
                span,
            ));
        }

        Some(if negative { -value } else { value })
    }

    fn check_slice_pattern(
        &mut self,
        scopes: &mut Scopes,
        inner_ptr: Type,
        span_inner: Type,
        patterns: Vec<Located<Pattern>>,
        span_id: UserTypeId,
    ) -> CheckedPattern {
        let mut rest = None;
        let mut result = Vec::new();
        for (i, pattern) in patterns.into_iter().enumerate() {
            if let Pattern::Rest(var) = pattern.data {
                let id = var.map(|(mutable, name)| {
                    scopes.insert(
                        Variable {
                            name,
                            ty: Type::Unknown(None),
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
                        pattern.span,
                    ))
                } else {
                    rest = Some(RestPattern { id, pos: i });
                }
            } else {
                let span = pattern.span;
                let CheckedPattern::Irrefutable(pattern) =
                    self.check_pattern(scopes, true, &inner_ptr, false, pattern)
                else {
                    self.error::<()>(Error::new("span subpatterns must be irrefutable", span));
                    continue;
                };

                result.push(pattern);
            }
        }

        if let Some(RestPattern { id: Some(id), .. }) = &mut rest {
            scopes.get_mut(*id).item.ty =
                Type::User(GenericUserType::new(span_id, vec![span_inner]).into());
        }

        CheckedPattern::Span(result, rest)
    }

    fn check_array_pattern(
        &mut self,
        scopes: &mut Scopes,
        target: &Type,
        patterns: Vec<Located<Pattern>>,
        span: Span,
    ) -> CheckedPattern {
        let span_id = scopes.lang_types.get("span").copied();
        let span_mut_id = scopes.lang_types.get("span_mut").copied();
        let (real_inner, arr_len) = match target.strip_references() {
            Type::Array(inner) => (&inner.0, inner.1),
            Type::User(ut) if Some(ut.id) == span_id => {
                let inner = ut.ty_args[0].clone();
                return self.check_slice_pattern(
                    scopes,
                    Type::Ptr(inner.clone().into()),
                    inner,
                    patterns,
                    span_id.unwrap(),
                );
            }
            Type::User(ut) if Some(ut.id) == span_mut_id => {
                let inner = ut.ty_args[0].clone();
                if target.is_ptr() || target.is_mut_ptr() {
                    let ptr = target.matched_inner_type(inner.clone());
                    let id = if ptr.is_ptr() {
                        span_id.unwrap()
                    } else {
                        span_mut_id.unwrap()
                    };
                    return self.check_slice_pattern(scopes, ptr, inner, patterns, id);
                } else {
                    return self.check_slice_pattern(
                        scopes,
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
                        target.name(scopes)
                    ),
                    span,
                ))
            }
        };

        let inner = target.matched_inner_type(real_inner.clone());

        let mut rest = None;
        let mut had_refutable = false;
        let mut result = Vec::new();
        for (i, pattern) in patterns.into_iter().enumerate() {
            if let Pattern::Rest(var) = pattern.data {
                let id = var.map(|(mutable, name)| {
                    scopes.insert(
                        Variable {
                            name,
                            ty: Type::Unknown(None),
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
                        pattern.span,
                    ))
                } else {
                    rest = Some(RestPattern { id, pos: i });
                }
            } else {
                let patt = self.check_pattern(scopes, true, &inner, false, pattern);
                if !matches!(patt, CheckedPattern::Irrefutable(_)) {
                    had_refutable = true;
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
            scopes.get_mut(*id).item.ty = target.matched_inner_type(Type::Array(
                (real_inner.clone(), arr_len - result.len()).into(),
            ));
        }

        let ptr = target.is_ptr() || target.is_mut_ptr();
        if had_refutable {
            CheckedPattern::Array(ArrayPattern {
                rest,
                patterns: result,
                arr_len,
                ptr,
            })
        } else {
            CheckedPattern::Irrefutable(IrrefutablePattern::Array(ArrayPattern {
                rest,
                patterns: result
                    .into_iter()
                    .map(|patt| patt.into_irrefutable().unwrap())
                    .collect(),
                arr_len,
                ptr,
            }))
        }
    }

    fn check_pattern(
        &mut self,
        scopes: &mut Scopes,
        binding: bool,
        scrutinee: &Type,
        mutable: bool,
        pattern: Located<Pattern>,
    ) -> CheckedPattern {
        let span = pattern.span;
        match pattern.data {
            Pattern::TupleLike { path, subpatterns } => {
                let Some(resolved) = self.resolve_path(scopes, &path.data, path.span) else {
                    return Default::default();
                };

                let Some(result) =
                    self.check_union_pattern(scopes, scrutinee, resolved, subpatterns, span)
                else {
                    return self.error(Error::new(
                        format!("expected '{}'", scrutinee.name(scopes)),
                        span,
                    ));
                };

                result
            }
            Pattern::Path(path) => {
                let resolved = self.resolve_path(scopes, &path, span);
                if let Some(ident) = path.as_identifier() {
                    match resolved
                        .and_then(|p| self.check_union_pattern(scopes, scrutinee, p, vec![], span))
                    {
                        Some(CheckedPattern::Error) => Default::default(),
                        Some(_) if binding => self.error(Error::new(
                            "cannot create binding that shadows union variant",
                            span,
                        )),
                        Some(pattern) => pattern,
                        None => CheckedPattern::Irrefutable(IrrefutablePattern::Variable(
                            scopes.insert(
                                Variable {
                                    name: ident.into(),
                                    ty: scrutinee.clone(),
                                    is_static: false,
                                    mutable,
                                    value: None,
                                },
                                false,
                            ),
                        )),
                    }
                } else if let Some(resolved) = resolved {
                    if let Some(pattern) =
                        self.check_union_pattern(scopes, scrutinee, resolved, vec![], span)
                    {
                        pattern
                    } else {
                        self.error(Error::new(
                            format!("expected '{}'", scrutinee.name(scopes)),
                            span,
                        ))
                    }
                } else {
                    Default::default()
                }
            }
            Pattern::Option(patt) => {
                let path = self.resolve_path_in(
                    scopes,
                    &[("Some".into(), vec![])],
                    scopes.get(scopes.get_option_id().unwrap()).body_scope,
                    pattern.span,
                );

                path.and_then(|path| {
                    self.check_union_pattern(
                        scopes,
                        scrutinee,
                        path,
                        vec![Located::new(pattern.span, *patt)],
                        pattern.span,
                    )
                })
                .unwrap_or_default()
            }
            Pattern::Null => {
                let path = self.resolve_path_in(
                    scopes,
                    &[("None".into(), vec![])],
                    scopes.get(scopes.get_option_id().unwrap()).body_scope,
                    pattern.span,
                );

                path.and_then(|path| {
                    self.check_union_pattern(scopes, scrutinee, path, vec![], pattern.span)
                })
                .unwrap_or_default()
            }
            Pattern::MutBinding(name) => {
                CheckedPattern::Irrefutable(IrrefutablePattern::Variable(scopes.insert(
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
                let Some((ut, (members, _))) = scrutinee
                    .strip_references()
                    .as_user()
                    .and_then(|ut| Some(ut).zip(scopes.get(ut.id).data.as_struct()))
                else {
                    return self.error(Error::new(
                        format!(
                            "cannot destructure value of non-struct type '{}'",
                            scrutinee.name(scopes)
                        ),
                        pattern.span,
                    ));
                };

                let cap = scopes.can_access_privates(scopes.get(ut.id).scope);
                let mut vars = Vec::new();
                for Destructure {
                    name,
                    mutable: pm,
                    pattern,
                } in destructures
                {
                    let Some(member) = members.iter().find(|m| m.name == name.data) else {
                        self.error::<()>(Error::no_member(
                            &scrutinee.name(scopes),
                            &name.data,
                            name.span,
                        ));
                        continue;
                    };

                    if !member.public && !cap {
                        self.error::<()>(Error::private_member(
                            &scrutinee.name(scopes),
                            &member.name,
                            name.span,
                        ));
                        continue;
                    }

                    // TODO: duplicates
                    let mut ty = scrutinee.matched_inner_type(member.ty.clone());
                    ty.fill_struct_templates(scopes, ut);
                    vars.push((name.data, mutable || pm, ty, pattern));
                }

                let mut had_refutable = false;
                let checked: Vec<_> = vars
                    .into_iter()
                    .map(|(name, mutable, ty, patt)| {
                        if let Some(patt) = patt {
                            let patt = self.check_pattern(scopes, true, &ty, mutable, patt);
                            if !matches!(patt, CheckedPattern::Irrefutable(_)) {
                                had_refutable = true;
                            }
                            (name, patt)
                        } else {
                            (
                                name.clone(),
                                CheckedPattern::Irrefutable(IrrefutablePattern::Variable(
                                    scopes.insert(
                                        Variable {
                                            name,
                                            ty,
                                            is_static: false,
                                            mutable,
                                            value: None,
                                        },
                                        false,
                                    ),
                                )),
                            )
                        }
                    })
                    .collect();
                if had_refutable {
                    CheckedPattern::Destrucure(checked)
                } else {
                    CheckedPattern::Irrefutable(IrrefutablePattern::Destrucure(
                        checked
                            .into_iter()
                            .map(|(name, patt)| (name, patt.into_irrefutable().unwrap()))
                            .collect(),
                    ))
                }
            }
            Pattern::String(value) => {
                let string = scopes.make_lang_type("string", vec![]).unwrap();
                if scrutinee.strip_references() != &string {
                    return self.error(Error::type_mismatch(
                        &scrutinee.name(scopes),
                        &string.name(scopes),
                        span,
                    ));
                }

                CheckedPattern::String(value)
            }
            Pattern::Int(patt) => self
                .check_int_pattern(scopes, scrutinee, patt, span)
                .map(CheckedPattern::Int)
                .unwrap_or_default(),
            Pattern::IntRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                let Some(start) = self.check_int_pattern(scopes, scrutinee, start, span) else {
                    return Default::default();
                };
                let Some(end) = self.check_int_pattern(scopes, scrutinee, end, span) else {
                    return Default::default();
                };

                if start > end {
                    return self.error(Error::new(
                        "range start must be less than or equal to its end",
                        span,
                    ));
                }

                CheckedPattern::IntRange(RangePattern {
                    inclusive,
                    start,
                    end,
                })
            }
            Pattern::Char(ch) => {
                if scrutinee.strip_references() != &Type::Char {
                    return self.error(Error::type_mismatch(
                        &scrutinee.name(scopes),
                        &Type::Char.name(scopes),
                        span,
                    ));
                }

                CheckedPattern::Int(BigInt::from(ch as u32))
            }
            Pattern::CharRange(RangePattern {
                inclusive,
                start,
                end,
            }) => {
                if scrutinee.strip_references() != &Type::Char {
                    return self.error(Error::type_mismatch(
                        &scrutinee.name(scopes),
                        &Type::Char.name(scopes),
                        span,
                    ));
                }

                if start > end {
                    return self.error(Error::new(
                        "range pattern end cannot be greater than its start",
                        span,
                    ));
                }

                CheckedPattern::IntRange(RangePattern {
                    inclusive,
                    start: BigInt::from(start as u32),
                    end: BigInt::from(end as u32),
                })
            }
            Pattern::Rest { .. } => self.error(Error::new(
                "rest patterns are only valid inside array or span patterns",
                span,
            )),
            Pattern::Array(subpatterns) => {
                self.check_array_pattern(scopes, scrutinee, subpatterns, span)
            }
            Pattern::Error => Default::default(),
        }
    }

    fn check_call(
        &mut self,
        scopes: &mut Scopes,
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
                let this = self.check_expr(scopes, *source, None);
                let id = this.ty.strip_references().clone();
                if id.is_unknown() {
                    return Default::default();
                }

                let Some((tr, func, ty_scope)) = id.get_member_fn(scopes, &member) else {
                    return self.error(Error::new(
                        format!("no method '{member}' found on type '{}'", id.name(scopes)),
                        span,
                    ));
                };
                let f = scopes.get(*func);
                let Some(this_param) = f.params.first().filter(|p| p.name == THIS_PARAM) else {
                    return self.error(Error::new(
                        format!("associated function '{member}' cannot be used as a method"),
                        span,
                    ));
                };

                if !func.public && !scopes.can_access_privates(ty_scope) {
                    return self.error(Error::new(
                        format!(
                            "cannot access private method '{member}' of type '{}'",
                            id.name(scopes)
                        ),
                        span,
                    ));
                }

                if this_param.ty.is_mut_ptr() {
                    let mut ty = &this.ty;
                    if !ty.is_ptr() && !ty.is_mut_ptr() && !this.can_addrmut(scopes) {
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

                let this = if !matches!(this.ty, Type::Ptr(_) | Type::MutPtr(_)) {
                    if matches!(this_param.ty, Type::Ptr(_)) {
                        CheckedExpr::new(
                            Type::Ptr(this.ty.clone().into()),
                            CheckedExprData::Unary {
                                op: UnaryOp::Addr,
                                expr: this.into(),
                            },
                        )
                    } else {
                        CheckedExpr::new(
                            Type::MutPtr(this.ty.clone().into()),
                            CheckedExprData::Unary {
                                op: UnaryOp::AddrMut,
                                expr: this.into(),
                            },
                        )
                    }
                } else {
                    this.auto_deref(&this_param.ty)
                };

                let mut func = GenericFunc::new(
                    *func,
                    self.resolve_type_args(scopes, f.type_params.len(), &generics, span),
                );
                let (args, ret) = self.check_fn_args(
                    tr.as_ref().or(id.as_user().map(|ut| &**ut)),
                    &mut func,
                    Some(this),
                    args,
                    target,
                    scopes,
                    span,
                );

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
                match self.resolve_path(scopes, path, callee.span) {
                    Some(ResolvedPath::UserType(ty)) => {
                        let ut = scopes.get(ty.id);
                        let Some((_, constructor)) = ut.data.as_struct() else {
                            return self.error(Error::new(
                                format!("cannot construct type '{}'", ut.name),
                                span,
                            ));
                        };

                        // TODO: check privacy
                        let (args, ret) = self.check_fn_args(
                            None,
                            &mut GenericFunc::new(*constructor, ty.ty_args),
                            None,
                            args,
                            target,
                            scopes,
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
                        let f = scopes.get(func.id);
                        let constructor = f.constructor;
                        let variant = constructor.then(|| f.name.data.clone());
                        let (args, ret) =
                            self.check_fn_args(None, &mut func, None, args, target, scopes, span);

                        return CheckedExpr::new(
                            ret,
                            if constructor {
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
                                scopes.get(id).name
                            ),
                            span,
                        ))
                    }
                    Some(ResolvedPath::Module(scope)) => {
                        return self.error(Error::new(
                            format!(
                                "expected callable, found module '{}'",
                                scopes[scope].kind.name(scopes).unwrap()
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

        let callee = self.check_expr(scopes, callee, None);
        if callee.ty.is_unknown() {
            return Default::default();
        }

        if callee.ty.is_fn_ptr() {
            return self.call_fn_ptr(scopes, callee, args, span);
        }

        self.error(Error::new(
            format!("expected callable item, got '{}'", &callee.ty.name(scopes)),
            span,
        ))
    }

    fn call_fn_ptr(
        &mut self,
        scopes: &mut Scopes,
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

                result.push(self.type_check(scopes, arg, param));
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

    fn check_arg(
        &mut self,
        func: &mut GenericFunc,
        scopes: &mut Scopes,
        expr: Expr,
        param: &CheckedParam,
        inst: Option<&GenericUserType>,
    ) -> CheckedExpr {
        let mut target = param.ty.clone();
        target.fill_func_template(scopes, func);

        if let Some(inst) = inst {
            target.fill_struct_templates(scopes, inst);
        }

        let span = expr.span;
        let expr = self.check_expr(scopes, expr, Some(&target));
        if !func.ty_args.is_empty() {
            func.infer_type_args(&param.ty, &expr.ty, scopes);
            target.fill_func_template(scopes, func);
        }

        self.type_check_checked(scopes, expr, &target, span)
    }

    #[allow(clippy::too_many_arguments)]
    fn check_fn_args(
        &mut self,
        inst: Option<&GenericUserType>,
        func: &mut GenericFunc,
        this: Option<CheckedExpr>,
        args: Vec<(Option<String>, Expr)>,
        target: Option<&Type>,
        scopes: &mut Scopes,
        span: Span,
    ) -> (IndexMap<String, CheckedExpr>, Type) {
        self.resolve_proto(scopes, func.id);

        if let Some(target) = target {
            func.infer_type_args(&scopes.get(func.id).ret, target, scopes);
        }

        let mut result = IndexMap::with_capacity(args.len());
        let mut last_pos = 0;
        if let Some(this) = this {
            result.insert(THIS_PARAM.into(), this);
            last_pos += 1;
        }

        let variadic = scopes.get(func.id).variadic;
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
                        if let Some(param) =
                            scopes.get(func.id).params.iter().find(|p| p.name == name)
                        {
                            entry.insert(self.check_arg(func, scopes, expr, &param.clone(), inst));
                        } else {
                            self.error::<()>(Error::new(
                                format!("unknown parameter: '{name}'"),
                                expr.span,
                            ));
                        }
                    }
                }
            } else if let Some((i, param)) = scopes
                .get(func.id)
                .params
                .iter()
                .enumerate()
                .skip(last_pos)
                .find(|(_, param)| !param.keyword)
            {
                result.insert(
                    param.name.clone(),
                    self.check_arg(func, scopes, expr, &param.clone(), inst),
                );
                last_pos = i + 1;
            } else if !variadic {
                // TODO: a better error here would be nice
                self.error::<()>(Error::new("too many positional arguments", expr.span));
            } else {
                num += 1;
                result.insert(format!("${num}"), self.check_expr(scopes, expr, None));
            }
        }

        for param in scopes
            .get(func.id)
            .params
            .iter()
            .filter(|p| !result.contains_key(&p.name))
            .collect::<Vec<_>>()
        {
            if let Some(DefaultExpr::Checked(expr)) = &param.default {
                result.insert(param.name.clone(), expr.clone());
            }
        }

        if scopes.get(func.id).params.len() > result.len() {
            let mut missing = String::new();
            for param in scopes
                .get(func.id)
                .params
                .iter()
                .filter(|p| !result.contains_key(&p.name))
            {
                if !missing.is_empty() {
                    missing.push_str(", ");
                }

                missing.push_str(&param.name);
            }

            self.error::<()>(Error::new(
                format!(
                    "expected {} argument(s), found {} (missing {missing})",
                    scopes.get(func.id).params.len(),
                    result.len()
                ),
                span,
            ));
        }

        let mut ret = scopes.get(func.id).ret.clone();
        if !func.ty_args.is_empty() {
            if let Some(target) = target {
                func.infer_type_args(&ret, target, scopes);
            }

            ret.fill_func_template(scopes, func);
            for (i, ty) in func.ty_args.iter().enumerate() {
                if ty.is_unknown() {
                    self.error::<()>(Error::new(
                        format!(
                            "cannot infer type for type parameter '{}'",
                            scopes.get(scopes.get(func.id).type_params[i]).name
                        ),
                        span,
                    ));

                    continue;
                }

                let f = scopes.get(func.id);
                self.check_bounds(
                    scopes,
                    Some(func),
                    ty,
                    &scopes.get(f.type_params[i]).impls,
                    inst,
                    span,
                );
            }
        }

        if let Some(inst) = inst {
            ret.fill_struct_templates(scopes, inst);
        }

        if scopes.get(func.id).is_unsafe && self.safety != Safety::Unsafe {
            self.error(Error::is_unsafe(span))
        }

        (result, ret)
    }

    fn check_bounds(
        &mut self,
        scopes: &Scopes,
        func: Option<&GenericFunc>,
        ty: &Type,
        bounds: &[Type],
        inst: Option<&GenericUserType>,
        span: Span,
    ) {
        for bound in bounds.iter() {
            let mut bound = bound.as_user().unwrap().clone();
            if let Some(func) = func {
                for bty in bound.ty_args.iter_mut() {
                    bty.fill_func_template(scopes, func);
                }
            }

            if let Some(inst) = inst {
                for bty in bound.ty_args.iter_mut() {
                    bty.fill_struct_templates(scopes, inst);
                }
            }

            if ty.implements_trait(scopes, &bound) {
                continue;
            }

            self.error(Error::doesnt_implement(
                &ty.name(scopes),
                &bound.name(scopes),
                span,
            ))
        }
    }

    fn create_block(&mut self, scopes: &mut Scopes, body: Vec<Stmt>, kind: ScopeKind) -> Block {
        scopes.enter(kind, false, |scopes| Block {
            body: body
                .into_iter()
                .map(|stmt| {
                    let stmt = self.declare_stmt(scopes, stmt);
                    self.check_stmt(scopes, stmt)
                })
                .collect(),
            scope: scopes.current,
        })
    }

    fn type_check(&mut self, scopes: &mut Scopes, expr: Expr, target: &Type) -> CheckedExpr {
        let span = expr.span;
        let source = self.check_expr(scopes, expr, Some(target));
        self.type_check_checked(scopes, source, target, span)
    }

    fn type_check_checked(
        &mut self,
        scopes: &mut Scopes,
        source: CheckedExpr,
        target: &Type,
        span: Span,
    ) -> CheckedExpr {
        if !source.ty.coerces_to(scopes, target) {
            self.error(Error::type_mismatch(
                &target.name(scopes),
                &source.ty.name(scopes),
                span,
            ))
        }

        source.coerce_to(target, scopes)
    }

    fn resolve_lang_type(
        &mut self,
        scopes: &Scopes,
        name: &str,
        ty_args: &[TypeHint],
        fwd: Option<&TypeHint>,
    ) -> Type {
        if let Some(ty) = scopes.lang_types.get(name).copied() {
            Type::User(
                GenericUserType::new(
                    ty,
                    self.resolve_type_args(scopes, ty_args.len(), ty_args, Span::default()),
                )
                .into(),
            )
        } else if let Some(hint) = fwd {
            Type::Unknown(Some((hint.clone(), scopes.current).into()))
        } else {
            self.error(Error::no_lang_item(name, Span::default()))
        }
    }

    fn make_lang_type(
        &mut self,
        scopes: &mut Scopes,
        id: UserTypeId,
        ty_args: Vec<Type>,
        span: Span,
    ) -> GenericUserType {
        let ty = GenericUserType::new(id, ty_args);
        for (i, param) in ty.ty_args.iter().enumerate() {
            for j in 0..scopes.get(scopes.get(id).type_params[i]).impls.len() {
                resolve_type!(
                    self,
                    scopes,
                    scopes.get_mut(scopes.get(id).type_params[i]).impls[j]
                );
            }

            self.check_bounds(
                scopes,
                None,
                param,
                &scopes.get(scopes.get(id).type_params[i]).impls,
                Some(&ty),
                span,
            );
        }
        ty
    }

    fn resolve_typehint(&mut self, scopes: &Scopes, hint: &TypeHint) -> Type {
        match hint {
            TypeHint::Regular(path) => match self.resolve_path(scopes, &path.data, path.span) {
                Some(ResolvedPath::UserType(ut)) => Type::User(ut.into()),
                Some(ResolvedPath::Func(_)) => {
                    if self.decl {
                        Type::Unknown(Some((hint.clone(), scopes.current).into()))
                    } else {
                        self.error(Error::new("expected type, got function", path.span))
                    }
                }
                Some(ResolvedPath::Var(_)) => {
                    if self.decl {
                        Type::Unknown(Some((hint.clone(), scopes.current).into()))
                    } else {
                        self.error(Error::new("expected type, got variable", path.span))
                    }
                }
                Some(ResolvedPath::Extension(_)) => {
                    if self.decl {
                        Type::Unknown(Some((hint.clone(), scopes.current).into()))
                    } else {
                        self.error(Error::new("expected type, got extension", path.span))
                    }
                }
                Some(ResolvedPath::Module(_)) => {
                    if self.decl {
                        Type::Unknown(Some((hint.clone(), scopes.current).into()))
                    } else {
                        self.error(Error::new("expected type, got module", path.span))
                    }
                }
                Some(ResolvedPath::None(err)) => {
                    let res = path.data.as_identifier().and_then(|symbol| match symbol {
                        symbol if symbol == THIS_TYPE => scopes.current_this_type(),
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

                    match res {
                        Some(res) => res,
                        None if self.decl => {
                            Type::Unknown(Some((hint.clone(), scopes.current).into()))
                        }
                        None => self.error(err),
                    }
                }
                None => Type::Unknown(None),
            },
            TypeHint::Void => Type::Void,
            TypeHint::Ptr(ty) => Type::Ptr(self.resolve_typehint(scopes, ty).into()),
            TypeHint::MutPtr(ty) => Type::MutPtr(self.resolve_typehint(scopes, ty).into()),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                scopes
                    .current_this_type()
                    .map(|s| Type::Ptr(s.into()))
                    .expect("ICE: this outside of method")
            }
            TypeHint::MutThis => scopes
                .current_this_type()
                .map(|s| Type::MutPtr(s.into()))
                .expect("ICE: this outside of method"),
            TypeHint::Array(ty, count) => {
                let n = match Self::consteval(scopes, count, Some(&Type::Usize)) {
                    Ok(n) => n,
                    Err(err) => return self.error(err),
                };
                Type::Array((self.resolve_typehint(scopes, ty), n).into())
            }
            TypeHint::Option(ty) => self.resolve_lang_type(
                scopes,
                "option",
                &[(**ty).clone()],
                self.decl.then_some(hint),
            ),
            TypeHint::Vec(ty) => {
                self.resolve_lang_type(scopes, "vec", &[(**ty).clone()], self.decl.then_some(hint))
            }
            TypeHint::Map(key, value) => self.resolve_lang_type(
                scopes,
                "map",
                &[(**key).clone(), (**value).clone()],
                self.decl.then_some(hint),
            ),
            TypeHint::Set(ty) => {
                self.resolve_lang_type(scopes, "set", &[(**ty).clone()], self.decl.then_some(hint))
            }
            TypeHint::Slice(ty) => {
                self.resolve_lang_type(scopes, "span", &[(**ty).clone()], self.decl.then_some(hint))
            }
            TypeHint::SliceMut(ty) => self.resolve_lang_type(
                scopes,
                "span_mut",
                &[(**ty).clone()],
                self.decl.then_some(hint),
            ),
            TypeHint::Tuple(_) => todo!(),
            TypeHint::Fn {
                is_extern: _,
                params,
                ret,
            } => Type::FnPtr(
                FnPtr {
                    params: params
                        .iter()
                        .map(|p| self.resolve_typehint(scopes, p))
                        .collect(),
                    ret: self.resolve_typehint(scopes, ret),
                }
                .into(),
            ),
            TypeHint::Error => Type::Unknown(None),
        }
    }

    fn resolve_type(&mut self, scopes: &mut Scopes, mut ty: &mut Type) {
        loop {
            match ty {
                Type::Array(t) => ty = &mut t.0,
                Type::Ptr(t) | Type::MutPtr(t) => ty = t,
                Type::User(ut) => {
                    for ty in ut.ty_args.iter_mut() {
                        self.resolve_type(scopes, ty);
                    }

                    break;
                }
                Type::Unknown(Some(hint)) => {
                    *ty = scopes.enter_id(hint.1, |scopes| {
                        for UnresolvedUse {
                            public,
                            path,
                            all,
                            span,
                        } in
                            std::mem::take(scopes.unresolved_use_stmts(scopes.current).unwrap())
                        {
                            if let Some(path) = self.resolve_path(scopes, &path, span) {
                                self.resolve_use(scopes, public, all, path, span);
                            }
                        }

                        self.resolve_typehint(scopes, &hint.0)
                    });
                    break;
                }
                Type::FnPtr(f) => {
                    for ty in f.params.iter_mut() {
                        self.resolve_type(scopes, ty);
                    }

                    self.resolve_type(scopes, &mut f.ret);
                    break;
                }
                _ => break,
            }
        }
    }

    fn resolve_use(
        &mut self,
        scopes: &mut Scopes,
        public: bool,
        all: bool,
        path: ResolvedPath,
        span: Span,
    ) -> bool {
        match path {
            ResolvedPath::UserType(ut) => {
                let name = &scopes.get(ut.id).name;
                if scopes.find_in::<FunctionId>(name, scopes.current).is_some()
                    || scopes.find_in::<UserTypeId>(name, scopes.current).is_some()
                {
                    self.error(Error::name_redef(name, span))
                }

                if all {
                    self.error(Error::wildcard_import(span))
                }

                scopes.current().types.insert(Vis { id: ut.id, public });
            }
            ResolvedPath::Func(func) => {
                let name = &scopes.get(func.id).name.data;
                if scopes.find_in::<UserTypeId>(name, scopes.current).is_some()
                    || scopes.find_in::<FunctionId>(name, scopes.current).is_some()
                    || scopes.find_in::<VariableId>(name, scopes.current).is_some()
                {
                    self.error(Error::name_redef(name, span))
                }
                if all {
                    self.error(Error::wildcard_import(span))
                }

                scopes.current().fns.insert(Vis {
                    id: func.id,
                    public,
                });
            }
            ResolvedPath::Var(id) => {
                let name = &scopes.get(id).name;
                if !scopes.get(id).is_static {
                    self.error(Error::new(
                        format!("cannot import local variable '{}'", scopes.get(id).name),
                        span,
                    ))
                }

                if scopes.find_in::<FunctionId>(name, scopes.current).is_some()
                    || scopes.find_in::<VariableId>(name, scopes.current).is_some()
                {
                    self.error(Error::name_redef(name, span))
                }

                if all {
                    self.error(Error::wildcard_import(span))
                }

                scopes.current().vars.insert(Vis { id, public });
            }
            ResolvedPath::Extension(id) => {
                let name = &scopes.get(id).name;
                if scopes
                    .find_in::<ExtensionId>(name, scopes.current)
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

                scopes.current().exts.insert(Vis { id, public });
            }
            ResolvedPath::Module(id) => {
                if !all {
                    let name = &scopes[id].kind.name(scopes).unwrap();
                    if !self.decl && (scopes.find_module_in(name, scopes.current).is_some()) {
                        self.error(Error::new(format!("redefinition of module {name}"), span))
                    }

                    scopes.current().children.insert(Vis { id, public });
                } else {
                    for child in scopes[id].children.clone() {
                        if scopes[child.id].kind.is_module() && child.public {
                            scopes.current().children.insert(Vis {
                                id: child.id,
                                public,
                            });
                        }
                    }

                    for func in scopes[id].fns.clone() {
                        if func.public {
                            scopes.current().fns.insert(Vis {
                                id: func.id,
                                public,
                            });
                        }
                    }

                    for ut in scopes[id].types.clone() {
                        if ut.public {
                            scopes.current().types.insert(Vis { id: ut.id, public });
                        }
                    }

                    for var in scopes[id].vars.clone() {
                        if var.public {
                            scopes.current().vars.insert(Vis { id: var.id, public });
                        }
                    }

                    for ext in scopes[id].exts.clone() {
                        if ext.public {
                            scopes.current().exts.insert(Vis { id: ext.id, public });
                        }
                    }
                }
            }
            ResolvedPath::None(err) => {
                if !self.decl {
                    self.error(err)
                }

                return false;
            }
        }

        true
    }

    fn resolve_impl(&mut self, scopes: &Scopes, path: &Located<Path>) -> Option<Type> {
        match self.resolve_path(scopes, &path.data, path.span)? {
            ResolvedPath::UserType(ty) if scopes.get(ty.id).data.is_trait() => {
                Some(Type::User(ty.into()))
            }
            ResolvedPath::None(err) if !self.decl => self.error(err),
            ResolvedPath::None(_) => Some(Type::Unknown(Some(
                (TypeHint::Regular(path.clone()), scopes.current).into(),
            ))),
            _ => self.error(Error::new("expected trait", path.span)),
        }
    }

    fn resolve_impls<T: ItemId + Copy>(&mut self, scopes: &mut Scopes, id: T)
    where
        T::Value: HasImplsAndTypeParams,
    {
        for i in 0..scopes.get(id).get_type_params().len() {
            self.resolve_impls(scopes, scopes.get(id).get_type_params()[i]);
        }

        let mut removals = Vec::new();
        for i in 0..scopes.get(id).get_impls().len() {
            resolve_type!(self, scopes, scopes.get_mut(id).get_impls_mut()[i]);
            let imp = &scopes.get(id).get_impls()[i];
            if !imp
                .as_user()
                .map_or(false, |t| scopes.get(t.id).data.is_trait())
            {
                if !imp.is_unknown() {
                    self.error(Error::new("expected trait", Span::default()))
                }
                removals.push(i);
            }
        }

        for i in removals.into_iter().rev() {
            scopes.get_mut(id).get_impls_mut().remove(i);
        }
    }

    fn resolve_proto(&mut self, scopes: &mut Scopes, id: FunctionId) {
        for i in 0..scopes.get(id).params.len() {
            resolve_type!(self, scopes, scopes.get_mut(id).params[i].ty);
            match std::mem::take(&mut scopes.get_mut(id).params[i].default) {
                Some(DefaultExpr::Unchecked(scope, expr)) => {
                    scopes.enter_id(scope, |scopes| {
                        let target = scopes.get(id).params[i].ty.clone();
                        scopes.get_mut(id).params[i].default =
                            Some(DefaultExpr::Checked(self.type_check(scopes, expr, &target)));
                    });
                }
                other => scopes.get_mut(id).params[i].default = other,
            }
        }

        resolve_type!(self, scopes, scopes.get_mut(id).ret);

        for i in 0..scopes.get(id).type_params.len() {
            self.resolve_impls(scopes, scopes.get(id).type_params[i]);
        }
    }

    fn resolve_path(&mut self, scopes: &Scopes, path: &Path, span: Span) -> Option<ResolvedPath> {
        match path {
            Path::Root(data) => self.resolve_path_in(scopes, data, ScopeId(0), span),
            Path::Super(data) => {
                if let Some(module) = scopes.module_of(
                    scopes[scopes.module_of(scopes.current).unwrap()]
                        .parent
                        .unwrap(),
                ) {
                    self.resolve_path_in(scopes, data, module, span)
                } else {
                    self.error(Error::new("cannot use super here", span))
                }
            }
            Path::Normal(data) => {
                let (name, ty_args) = data.first().unwrap();
                let is_end = data.len() == 1;
                if let Some(id) = scopes.find(name) {
                    if is_end {
                        return Some(ResolvedPath::Var(*id));
                    }

                    if !ty_args.is_empty() {
                        return self
                            .error(Error::new("variables cannot have type arguments", span));
                    }

                    self.error(Error::new(format!("'{name}' is a variable"), span))
                } else if let Some(id) = scopes.find(name) {
                    if is_end {
                        let ut = GenericUserType::new(
                            *id,
                            self.resolve_type_args(
                                scopes,
                                scopes.get(*id).type_params.len(),
                                ty_args,
                                span,
                            ),
                        );
                        //self.resolve_impls(scopes, id);
                        //self.check_bounds(scopes, None, &ut, &scopes.get(id).impls, span);
                        return Some(ResolvedPath::UserType(ut));
                    }

                    self.resolve_path_in(scopes, &data[1..], scopes.get(*id).body_scope, span)
                } else if let Some(id) = scopes.find_free_fn(name) {
                    if is_end {
                        let f = scopes.get(*id);
                        return Some(ResolvedPath::Func(GenericFunc::new(
                            *id,
                            self.resolve_type_args(scopes, f.type_params.len(), ty_args, span),
                        )));
                    }

                    self.error(Error::new(format!("'{name}' is a function"), span))
                } else if let Some(id) = scopes.find(name) {
                    if is_end {
                        return Some(ResolvedPath::Extension(*id));
                    }

                    self.error(Error::no_symbol(name, span))
                } else if let Some(id) = scopes.find_module(name) {
                    if is_end {
                        return Some(ResolvedPath::Module(*id));
                    }

                    if !ty_args.is_empty() {
                        return self.error(Error::new(
                            "modules cannot be parameterized with type arguments",
                            span,
                        ));
                    }

                    self.resolve_path_in(scopes, &data[1..], *id, span)
                } else {
                    self.resolve_path_in(scopes, data, ScopeId(0), span)
                }
            }
        }
    }

    fn resolve_path_in(
        &mut self,
        scopes: &Scopes,
        data: &[(String, Vec<TypeHint>)],
        mut scope: ScopeId,
        span: Span,
    ) -> Option<ResolvedPath> {
        for (i, (name, ty_args)) in data.iter().enumerate() {
            let is_end = i + 1 == data.len();
            if let Some(id) = scopes.find_in(name, scope) {
                if !id.public && !scopes.can_access_privates(scope) {
                    self.error(Error::new(format!("variable '{name}' is private"), span))
                }

                if !ty_args.is_empty() {
                    return self.error(Error::new(
                        "variables cannot be parameterized with type arguments",
                        span,
                    ));
                }

                if is_end {
                    return Some(ResolvedPath::Var(*id));
                }

                return self.error(Error::new(format!("'{name}' is a variable"), span));
            } else if let Some(id) = scopes.find_in(name, scope) {
                if !id.public && !scopes.can_access_privates(scope) {
                    self.error(Error::new(format!("type '{name}' is private"), span))
                }

                let ty = scopes.get(*id);
                if is_end {
                    return Some(ResolvedPath::UserType(GenericUserType::new(
                        *id,
                        self.resolve_type_args(scopes, ty.type_params.len(), ty_args, span),
                    )));
                }

                scope = ty.body_scope;
            } else if let Some(id) = scopes.find_in(name, scope) {
                if !id.public && !scopes.can_access_privates(scope) {
                    self.error(Error::new(format!("function '{name}' is private"), span))
                }

                if is_end {
                    return Some(ResolvedPath::Func(GenericFunc::new(
                        *id,
                        self.resolve_type_args(
                            scopes,
                            scopes.get(*id).type_params.len(),
                            ty_args,
                            span,
                        ),
                    )));
                }

                return self.error(Error::new(format!("'{name}' is a function"), span));
            } else if let Some(id) = scopes.find_in(name, scope) {
                if !id.public && !scopes.can_access_privates(scope) {
                    self.error(Error::new(format!("extension '{name}' is private"), span))
                }

                if is_end {
                    return Some(ResolvedPath::Extension(*id));
                }

                return Some(ResolvedPath::None(Error::no_symbol(name, span)));
            } else if let Some(id) = scopes.find_module_in(name, scope) {
                if !id.public && !scopes.can_access_privates(*id) {
                    self.error(Error::new(format!("module '{name}' is private"), span))
                }

                if !ty_args.is_empty() {
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
        scopes: &Scopes,
        params: usize,
        args: &[TypeHint],
        span: Span,
    ) -> Vec<Type> {
        if args.is_empty() {
            vec![Type::Unknown(None); params]
        } else if args.len() != params {
            self.error(Error::new(
                format!(
                    "expected {} type arguments, received {}",
                    params,
                    args.len()
                ),
                span,
            ))
        } else {
            args.iter()
                .map(|ty| self.resolve_typehint(scopes, ty))
                .collect()
        }
    }

    fn include_universal(&mut self, scopes: &mut Scopes) {
        for scope in self.universal.clone() {
            self.resolve_use(
                scopes,
                false,
                true,
                ResolvedPath::Module(scope),
                Span::default(),
            );
        }
    }

    fn get_int_type_and_val(
        &mut self,
        scopes: &Scopes,
        target: Option<&Type>,
        base: u8,
        width: Option<String>,
        value: String,
        span: Span,
    ) -> (Type, BigInt) {
        let ty = if let Some(width) = width {
            Type::from_int_name(&width).unwrap_or_else(|| {
                self.error(Error::new(
                    format!("invalid integer literal type: {width}"),
                    span,
                ))
            })
        } else {
            // FIXME: attempt to promote the literal if its too large for i32
            target
                .map(|target| target.strip_options(scopes))
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
        if stats.signed {
            let result = match BigInt::from_str_radix(&value, base as u32) {
                Ok(result) => result,
                Err(e) => {
                    return self.error(Error::new(
                        format!("integer literal '{value}' could not be parsed: {e}"),
                        span,
                    ));
                }
            };

            let max = stats.max_signed();
            if result > max {
                return self.error(Error::new(
                    format!("integer literal is larger than its type allows ({max})"),
                    span,
                ));
            }

            let min = stats.min_signed();
            if result < min {
                return self.error(Error::new(
                    format!("integer literal is smaller than its type allows ({min})"),
                    span,
                ));
            }

            (ty, result)
        } else {
            let result = match BigInt::from_str_radix(&value, base as u32) {
                Ok(result) => result,
                Err(e) => {
                    return self.error(Error::new(
                        format!("integer literal '{value}' could not be parsed: {e}"),
                        span,
                    ));
                }
            };

            let max = stats.max_unsigned();
            if result >= max {
                return self.error(Error::new(
                    format!("integer literal is larger than its type allows ({max})"),
                    span,
                ));
            }

            (ty, result)
        }
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

    fn typehint_for_struct(
        name: &str,
        type_params: &[(String, Vec<Located<Path>>)],
        span: Span,
    ) -> TypeHint {
        TypeHint::Regular(Located::new(
            span,
            Path::Normal(vec![(
                name.into(),
                type_params
                    .iter()
                    .map(|(n, _)| TypeHint::Regular(Located::new(span, Path::from(n.clone()))))
                    .collect(),
            )]),
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
                .filter(|t| t.id == scopes.get_option_id().unwrap())
                .map(|target| &target.ty_args[0])
        }
    }

    fn loop_out_type(scopes: &Scopes, kind: &ScopeKind, infinite: bool) -> Type {
        let ScopeKind::Loop(target, breaks) = kind else {
            panic!("ICE: target of loop changed from loop to something else");
        };

        if infinite {
            breaks
                .then(|| target.clone().unwrap())
                .unwrap_or(Type::Never)
        } else {
            // TODO: coerce the break statements
            breaks
                .then(|| {
                    scopes
                        .make_lang_type("option", vec![target.clone().unwrap()])
                        .unwrap()
                })
                .unwrap_or(Type::Void)
        }
    }
}
