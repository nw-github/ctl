use num_bigint::BigInt;

use crate::{
    ast::{
        checked::{CheckedExpr, CheckedExprData},
        declared::{DeclaredFn, DeclaredImplBlock, DeclaredStmt, DeclaredStmtData},
        parsed::{
            ExprData, Fn, ImplBlock, Param, Pattern, Stmt, StmtData, TypeHint, TypePath,
            TypePathComponent,
        },
        Attribute,
    },
    error::{Diagnostics, Error},
    lexer::{Located, Span},
    sym::{
        CheckedMember, CheckedParam, DefaultExpr, Extension, Function, FunctionId, ParamPattern,
        ScopeId, ScopeKind, Scopes, Union, UnresolvedUse, UserType, UserTypeData, UserTypeId,
        Variable, VariableId, TT,
    },
    typeid::{GenericUserType, Type},
};

pub fn declare_stmt(
    diag: &mut Diagnostics,
    scopes: &mut Scopes,
    autouse: &mut Vec<ScopeId>,
    Stmt { data, span, attrs }: Stmt,
) -> DeclaredStmt {
    let data = match data {
        StmtData::Module { public, name, body } => {
            if scopes.find_module_in(&name, scopes.current).is_some() {
                diag.error(Error::new(
                    format!("redeclaration of module '{name}'"),
                    span,
                ));
            }

            scopes.enter(ScopeKind::Module(name, Vec::new()), public, |scopes| {
                let core = scopes.find_module_in("core", ScopeId(0)).map(|s| s.id);
                let std = scopes.find_module_in("std", ScopeId(0)).map(|s| s.id);
                if attrs.iter().any(|attr| attr.name.data == "autouse") {
                    if scopes
                        .iter()
                        .any(|(id, _)| Some(id) == core || Some(id) == std)
                    {
                        autouse.push(scopes.current);
                    } else {
                        diag.error(Error::new(
                            "autouse modules may only be defined by 'core' and 'std'",
                            span,
                        ));
                    }
                }

                DeclaredStmtData::Module {
                    id: scopes.current,
                    body: body
                        .into_iter()
                        .map(|stmt| declare_stmt(diag, scopes, autouse, stmt))
                        .collect(),
                }
            })
        }
        StmtData::Struct(base) => {
            let init = scopes.enter(ScopeKind::None, false, |scopes| {
                declare_fn(
                    diag,
                    scopes,
                    autouse,
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
                                keyword: true,
                                patt: Located::new(
                                    member.name.span,
                                    Pattern::Path(TypePath::from(member.name.data.clone())),
                                ),
                                ty: member.ty.clone(),
                                default: member.default.clone(),
                            })
                            .collect(),
                        ret: typehint_for_struct(&base.name.data, &base.type_params, span),
                        body: None,
                    },
                    vec![],
                )
            });
            let id = insert_type(
                diag,
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
                attrs,
                base.name.span,
            );
            scopes.enter(ScopeKind::UserType(id), base.public, |scopes| {
                scopes.get_mut(id).body_scope = scopes.current;
                scopes.get_mut(id).type_params =
                    declare_type_params(scopes, TT::Struct, base.type_params);

                let mut members = Vec::with_capacity(base.members.len());
                for member in base.members {
                    if members
                        .iter()
                        .any(|m: &CheckedMember| m.name == member.name.data)
                    {
                        diag.error(Error::redefinition(
                            "member variable",
                            &member.name.data,
                            member.name.span,
                        ))
                    }

                    members.push(CheckedMember {
                        public: member.public,
                        name: member.name.data,
                        shared: member.shared,
                        ty: Type::from_typehint(member.ty, scopes),
                    });
                }

                *scopes.get_mut(id).data.as_struct_mut().unwrap().0 = members;

                let (impls, impl_blocks) = declare_impl_blocks(diag, scopes, autouse, base.impls);
                scopes.get_mut(id).impls = impls;

                DeclaredStmtData::Struct {
                    init,
                    id,
                    impl_blocks,
                    functions: base
                        .functions
                        .into_iter()
                        .map(|f| declare_fn(diag, scopes, autouse, false, f, vec![]))
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
            let ret = typehint_for_struct(&base.name.data, &tp, span);
            let id = insert_type(
                diag,
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
                attrs,
                base.name.span,
            );
            scopes.enter(ScopeKind::UserType(id), base.public, |scopes| {
                scopes.get_mut(id).body_scope = scopes.current;
                scopes.get_mut(id).type_params =
                    declare_type_params(scopes, TT::Struct, base.type_params);

                let mut variants = Vec::with_capacity(base.members.len());
                let mut params = Vec::with_capacity(base.members.len());
                for member in base.members.iter() {
                    if variants
                        .iter()
                        .any(|m: &CheckedMember| m.name == member.name.data)
                    {
                        diag.error(Error::redefinition(
                            "member",
                            &member.name.data,
                            member.name.span,
                        ))
                    }

                    variants.push(CheckedMember {
                        public: member.public,
                        name: member.name.data.clone(),
                        shared: member.shared,
                        ty: Type::from_typehint(member.ty.clone(), scopes),
                    });

                    if member.shared && is_unsafe {
                        // FIXME: span should be related to the member
                        diag.error(Error::new(
                            "cannot have shared members in an unsafe union",
                            span,
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

                scopes.get_mut(id).data.as_union_mut().unwrap().variants = variants;

                let (impls, impl_blocks) = declare_impl_blocks(diag, scopes, autouse, base.impls);
                scopes.get_mut(id).impls = impls;
                let member_cons = base
                    .members
                    .into_iter()
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

                        declare_fn(
                            diag,
                            scopes,
                            autouse,
                            true,
                            Fn {
                                public: base.public,
                                name: Located::new(Span::default(), member.name.data.clone()),
                                is_async: false,
                                is_extern: false,
                                variadic: false,
                                is_unsafe: false,
                                type_params: tp.clone(),
                                params,
                                ret: ret.clone(),
                                body: None,
                            },
                            vec![],
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
                        .map(|f| declare_fn(diag, scopes, autouse, false, f, vec![]))
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
            let id = insert_type(
                diag,
                scopes,
                UserType {
                    name: name.data,
                    body_scope: ScopeId(0),
                    data: UserTypeData::Trait,
                    impls: Vec::new(),
                    type_params: Vec::new(),
                },
                public,
                attrs,
                name.span,
            );
            scopes.enter(ScopeKind::UserType(id), public, |scopes| {
                scopes.get_mut(id).body_scope = scopes.current;
                scopes.get_mut(id).type_params =
                    declare_type_params(scopes, TT::Struct, type_params);
                scopes.get_mut(id).impls = impls
                    .into_iter()
                    .map(|path| Type::from_type_path(path, scopes))
                    .collect();

                DeclaredStmtData::Trait {
                    id,
                    functions: functions
                        .into_iter()
                        .map(|f| declare_fn(diag, scopes, autouse, false, f, vec![]))
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
            let id = insert_type(
                diag,
                scopes,
                UserType {
                    name: name.data,
                    body_scope: ScopeId(0),
                    data: UserTypeData::Enum(backing.clone()),
                    type_params: Vec::new(),
                    impls: Vec::new(),
                },
                public,
                attrs,
                name.span,
            );

            scopes.enter(ScopeKind::UserType(id), public, |scopes| {
                scopes.get_mut(id).body_scope = scopes.current;
                let mut n_variants: Vec<(VariableId, Option<Located<ExprData>>)> =
                    Vec::with_capacity(variants.len());
                for (i, (name, expr)) in variants.into_iter().enumerate() {
                    if n_variants
                        .iter()
                        .any(|(id, _)| scopes.get(*id).name == name.data)
                    {
                        diag.error(Error::redefinition("variant", &name.data, name.span))
                    }

                    n_variants.push((
                        scopes.insert(
                            Variable {
                                name: name.data.clone(),
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
                    ));
                }

                let (impls, impl_blocks) = declare_impl_blocks(diag, scopes, autouse, impls);
                scopes.get_mut(id).impls = impls;
                DeclaredStmtData::Enum {
                    id,
                    variants: n_variants,
                    impl_blocks,
                    functions: functions
                        .into_iter()
                        .map(|f| declare_fn(diag, scopes, autouse, false, f, vec![]))
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
                    ty: Type::Unknown,
                    impls: Vec::new(),
                    type_params: Vec::new(),
                    body_scope: ScopeId(0),
                },
                public,
            );
            scopes.enter(ScopeKind::Extension(id), false, |scopes| {
                scopes.get_mut(id).body_scope = scopes.current;
                scopes.get_mut(id).type_params =
                    declare_type_params(scopes, TT::Struct, type_params);
                scopes.get_mut(id).ty = Type::from_typehint(ty, scopes);

                let (impls, impl_blocks) = declare_impl_blocks(diag, scopes, autouse, impls);
                scopes.get_mut(id).impls = impls;

                DeclaredStmtData::Extension {
                    id,
                    impl_blocks,
                    functions: functions
                        .into_iter()
                        .map(|f| declare_fn(diag, scopes, autouse, false, f, vec![]))
                        .collect(),
                }
            })
        }
        StmtData::Fn(f) => DeclaredStmtData::Fn(declare_fn(diag, scopes, autouse, false, f, attrs)),
        StmtData::Static {
            public,
            name,
            ty,
            value,
        } => {
            let id = scopes.insert(
                Variable {
                    name,
                    ty: Type::from_typehint(ty, scopes),
                    is_static: true,
                    mutable: false,
                    value: None,
                },
                public,
            );

            DeclaredStmtData::Static { id, value }
        }
        StmtData::Use { path, public, all } => {
            scopes.current().use_stmts.push(UnresolvedUse {
                path,
                public,
                all,
                span,
            });
            DeclaredStmtData::None
        }
        StmtData::Let { ty, value, patt } => DeclaredStmtData::Let { ty, value, patt },
        StmtData::Expr(expr) => DeclaredStmtData::Expr(expr),
        StmtData::Error => DeclaredStmtData::None,
    };

    DeclaredStmt { data, span }
}

fn declare_fn(
    diag: &mut Diagnostics,
    scopes: &mut Scopes,
    autouse: &mut Vec<ScopeId>,
    constructor: bool,
    f: Fn,
    attrs: Vec<Attribute>,
) -> DeclaredFn {
    if f.variadic && !f.is_extern {
        diag.error(Error::new(
            "only extern functions may be variadic",
            f.name.span,
        ))
    }

    if !constructor
        && scopes
            .find_in::<FunctionId>(&f.name.data, scopes.current)
            .is_some()
    {
        diag.error(Error::new(
            format!("redeclaration of function '{}'", f.name.data),
            f.name.span,
        ))
    }

    let id = scopes.insert(
        Function {
            attrs,
            name: f.name,
            is_async: f.is_async,
            is_extern: f.is_extern,
            is_unsafe: f.is_unsafe,
            variadic: f.variadic,
            type_params: Vec::new(),
            params: Vec::new(),
            ret: Type::Unknown,
            body: None,
            body_scope: ScopeId(0),
            returns: false,
            constructor,
        },
        f.public,
    );

    let attrs = &scopes.get::<FunctionId>(id).attrs;
    if attrs.iter().any(|attr| attr.name.data == "panic_handler") {
        scopes.panic_handler = Some(id);
        // TODO: verify the signature
    } else if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "intrinsic") {
        if let Some(attr) = attr.props.first() {
            match attr.name.data.as_str() {
                "size_of" | "panic" => {
                    scopes.intrinsics.insert(id, attr.name.data.clone());
                }
                _ => diag.error(Error::new(
                    format!("intrinsic '{}' is not supported", attr.name.data),
                    attr.name.span,
                )),
            }
        } else {
            diag.error(Error::new(
                "intrinsic function must have name",
                attr.name.span,
            ))
        }
    }

    scopes.enter(ScopeKind::Function(id), false, |scopes| {
        scopes.get_mut(id).body_scope = scopes.current;
        scopes.get_mut(id).type_params = declare_type_params(scopes, TT::Func, f.type_params);
        scopes.get_mut(id).params = f
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
                ty: Type::from_typehint(param.ty, scopes),
                default: param
                    .default
                    .map(|expr| DefaultExpr::Unchecked(scopes.current, expr)),
            })
            .collect();
        scopes.get_mut(id).ret = Type::from_typehint(f.ret, scopes);

        DeclaredFn {
            id,
            body: f.body.map(|body| {
                body.into_iter()
                    .map(|stmt| declare_stmt(diag, scopes, autouse, stmt))
                    .collect()
            }),
        }
    })
}

fn declare_type_params(
    scopes: &mut Scopes,
    tt: TT,
    type_params: Vec<(String, Vec<Located<TypePath>>)>,
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
                        .into_iter()
                        .map(|path| Type::from_type_path(path, scopes))
                        .collect(),
                },
                false,
            )
        })
        .collect()
}

fn declare_impl_blocks(
    diag: &mut Diagnostics,
    scopes: &mut Scopes,
    autouse: &mut Vec<ScopeId>,
    blocks: Vec<ImplBlock>,
) -> (Vec<Type>, Vec<DeclaredImplBlock>) {
    let mut impls = Vec::new();
    let mut declared_blocks = Vec::new();
    for block in blocks {
        let span = block.path.span;
        let ty = Type::from_type_path(block.path, scopes);
        let impl_index = impls.len();
        impls.push(ty);

        declared_blocks.push(scopes.enter(ScopeKind::None, false, |scopes| {
            DeclaredImplBlock {
                impl_index,
                span,
                scope: scopes.current,
                functions: block
                    .functions
                    .into_iter()
                    .map(|f| declare_fn(diag, scopes, autouse, false, f, vec![]))
                    .collect(),
            }
        }));
    }

    (impls, declared_blocks)
}

fn insert_type(
    diag: &mut Diagnostics,
    scopes: &mut Scopes,
    ty: UserType,
    public: bool,
    attrs: Vec<Attribute>,
    span: Span,
) -> UserTypeId {
    if scopes
        .find_in::<UserTypeId>(&ty.name, scopes.current)
        .is_some()
    {
        diag.error(Error::redefinition("type", &ty.name, span));
    }

    let id = scopes.insert(ty, public);
    if let Some(attr) = attrs.iter().find(|attr| attr.name.data == "lang") {
        let Some(name) = attr.props.first() else {
            diag.error(Error::new("language item must have name", attr.name.span));
            return id;
        };

        scopes.lang_types.insert(name.name.data.clone(), id);
    }

    id
}

fn typehint_for_struct(
    name: &str,
    type_params: &[(String, Vec<Located<TypePath>>)],
    span: Span,
) -> TypeHint {
    TypeHint::Regular(Located::new(
        span,
        TypePath::Normal(vec![TypePathComponent(
            name.into(),
            type_params
                .iter()
                .map(|(n, _)| TypeHint::Regular(Located::new(span, TypePath::from(n.clone()))))
                .collect(),
        )]),
    ))
}
