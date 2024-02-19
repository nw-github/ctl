use derive_more::{Constructor, Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        checked::{CheckedExpr, CheckedPattern, CheckedStmt},
        parsed::{Expr, Linkage, Path, Pattern, UsePath},
        Attribute,
    },
    lexer::{Located, Span},
    typeid::{GenericTrait, GenericUserType, Type},
    THIS_PARAM,
};

macro_rules! id {
    ($name: ident => $output: ident,
     $vec: ident,
     $namespace: ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(usize);

        impl ItemId for $name {
            type Value = $output;

            fn get(self, scopes: &Scopes) -> &Scoped<Self::Value> {
                &scopes.$vec[self.0]
            }

            fn get_mut(self, scopes: &mut Scopes) -> &mut Scoped<Self::Value> {
                &mut scopes.$vec[self.0]
            }

            fn insert_in(
                scopes: &mut Scopes,
                value: Self::Value,
                public: bool,
                scope: ScopeId,
            ) -> (Self, bool) {
                let index = scopes.$vec.len();
                scopes.$vec.push(Scoped::new(value, scope));
                let id = $name(index);
                let name = (scopes.$vec[id.0].name.data).clone();
                let res = scopes[scope].$namespace.insert(
                    name,
                    Vis {
                        id: id.into(),
                        public,
                    },
                );
                (id, res.is_some())
            }

            fn name<'a>(&self, scopes: &'a Scopes) -> &'a Located<String> {
                &scopes.$vec[self.0].name
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ScopeId(pub usize);

impl ScopeId {
    pub const ROOT: ScopeId = ScopeId(0);
}

id!(FunctionId => Function, fns, vns);
id!(UserTypeId => UserType, types, tns);
id!(TraitId => Trait, traits, tns);
id!(VariableId => Variable, vars, vns);
id!(ExtensionId => Extension, exts, tns);

#[derive(Default, Debug, Clone, EnumAsInner)]
pub enum ScopeKind {
    Block(Option<Type>, bool),
    Loop {
        target: Option<Type>,
        breaks: Option<bool>,
        infinite: bool,
    },
    Lambda(Option<Type>, bool),
    Function(FunctionId),
    UserType(UserTypeId),
    Trait(TraitId),
    Module(String),
    Impl(TraitId),
    Extension(ExtensionId),
    #[default]
    None,
}

impl ScopeKind {
    pub fn name<'a, 'b>(&'a self, scopes: &'b Scopes) -> Option<&'b str>
    where
        'a: 'b,
    {
        match self {
            &ScopeKind::Function(id) => Some(&scopes.get(id).name.data),
            &ScopeKind::UserType(id) => Some(&scopes.get(id).name.data),
            &ScopeKind::Trait(id) => Some(&scopes.get(id).name.data),
            &ScopeKind::Extension(id) => Some(&scopes.get(id).name.data),
            ScopeKind::Module(name) => Some(name),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DefaultExpr {
    Unchecked(ScopeId, Expr),
    Checked(CheckedExpr),
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ParamPattern {
    Unchecked(Located<Pattern>),
    Checked(CheckedPattern),
}

#[derive(Debug, Clone, EnumAsInner, Default)]
pub enum TraitImpl {
    Unchecked {
        scope: ScopeId,
        path: Located<Path>,
        block: Option<ScopeId>,
    },
    Checked(GenericTrait, Option<ScopeId>),
    #[default]
    None,
}

#[derive(Debug, Clone)]
pub struct CheckedParam {
    pub keyword: bool,
    pub label: String,
    pub patt: ParamPattern,
    pub ty: Type,
    pub default: Option<DefaultExpr>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Located<String>,
    pub ty: Type,
    pub is_static: bool,
    pub mutable: bool,
    pub value: Option<CheckedExpr>,
    pub unused: bool,
}

#[derive(Default, Debug)]
pub struct Function {
    pub attrs: Vec<Attribute>,
    pub name: Located<String>,
    pub linkage: Linkage,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    pub type_params: Vec<UserTypeId>,
    pub params: Vec<CheckedParam>,
    pub ret: Type,
    pub body: Option<Vec<CheckedStmt>>,
    pub constructor: Option<UserTypeId>,
    pub body_scope: ScopeId,
    pub returns: bool,
}

impl FunctionId {
    pub const RESERVED: FunctionId = FunctionId(0);
}

#[derive(Debug, Clone, Constructor)]
pub struct CheckedMember {
    pub public: bool,
    pub ty: Type,
}

#[derive(Debug, Clone, Deref, DerefMut)]
pub struct Union(pub IndexMap<String, Option<Type>>);

impl Union {
    pub fn tag_type(&self) -> Type {
        Type::discriminant_for(self.0.len())
    }

    pub fn variant_tag(&self, name: &str) -> Option<usize> {
        self.0.get_index_of(name)
    }
}

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct,
    Union(Union),
    UnsafeUnion,
    Template,
    AnonStruct,
    Tuple,
}

#[derive(Debug)]
pub struct UserType {
    pub name: Located<String>,
    pub body_scope: ScopeId,
    pub data: UserTypeData,
    pub impls: Vec<TraitImpl>,
    pub type_params: Vec<UserTypeId>,
    pub attrs: Vec<Attribute>,
    pub fns: Vec<Vis<FunctionId>>,
    pub members: IndexMap<String, CheckedMember>,
}

impl UserType {
    pub fn find_associated_fn(&self, scopes: &Scopes, name: &str) -> Option<FunctionId> {
        self.fns
            .iter()
            .map(|f| f.id)
            .find(|&id| scopes.get(id).name.data == name)
    }
}

#[derive(Debug)]
pub struct Trait {
    pub name: Located<String>,
    pub body_scope: ScopeId,
    pub impls: Vec<TraitImpl>,
    pub type_params: Vec<UserTypeId>,
    pub attrs: Vec<Attribute>,
    pub fns: Vec<Vis<FunctionId>>,
}

#[derive(Debug)]
pub struct Extension {
    pub ty: Type,
    pub name: Located<String>,
    pub impls: Vec<TraitImpl>,
    pub type_params: Vec<UserTypeId>,
    pub body_scope: ScopeId,
    pub fns: Vec<Vis<FunctionId>>,
}

pub trait HasTypeParams {
    fn get_type_params(&self) -> &[UserTypeId];
}

impl HasTypeParams for UserType {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }
}

impl HasTypeParams for Extension {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }
}

impl HasTypeParams for Function {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }
}

impl HasTypeParams for Trait {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }
}

pub trait TypeLike: HasTypeParams {
    fn get_impls(&self) -> &[TraitImpl];
    fn get_impls_mut(&mut self) -> &mut [TraitImpl];
    fn get_fns(&self) -> &[Vis<FunctionId>];

    fn vtable_methods(&self, scopes: &Scopes) -> impl Iterator<Item = &Vis<FunctionId>> {
        self.get_fns().iter().filter(|f| {
            let f = scopes.get(f.id);
            f.type_params.is_empty() && f.params.first().is_some_and(|p| p.label == THIS_PARAM)
        })
    }
}

impl TypeLike for UserType {
    fn get_impls(&self) -> &[TraitImpl] {
        &self.impls
    }

    fn get_impls_mut(&mut self) -> &mut [TraitImpl] {
        &mut self.impls
    }

    fn get_fns(&self) -> &[Vis<FunctionId>] {
        &self.fns
    }
}

impl TypeLike for Trait {
    fn get_impls(&self) -> &[TraitImpl] {
        &self.impls
    }

    fn get_impls_mut(&mut self) -> &mut [TraitImpl] {
        &mut self.impls
    }

    fn get_fns(&self) -> &[Vis<FunctionId>] {
        &self.fns
    }
}

impl TypeLike for Extension {
    fn get_impls(&self) -> &[TraitImpl] {
        &self.impls
    }

    fn get_impls_mut(&mut self) -> &mut [TraitImpl] {
        &mut self.impls
    }

    fn get_fns(&self) -> &[Vis<FunctionId>] {
        &self.fns
    }
}

#[derive(Deref, DerefMut, Constructor)]
pub struct Scoped<T> {
    #[deref]
    #[deref_mut]
    pub item: T,
    pub scope: ScopeId,
}

#[derive(Debug, Deref, DerefMut, Constructor, Copy, Clone)]
pub struct Vis<T> {
    #[deref]
    #[deref_mut]
    pub id: T,
    pub public: bool,
}

pub trait ItemId: Sized + Copy + Clone {
    type Value;

    fn get(self, scopes: &Scopes) -> &Scoped<Self::Value>;
    fn get_mut(self, scopes: &mut Scopes) -> &mut Scoped<Self::Value>;
    fn name<'a>(&self, scopes: &'a Scopes) -> &'a Located<String>;

    fn insert_in(scopes: &mut Scopes, val: Self::Value, public: bool, id: ScopeId) -> (Self, bool);
}

#[derive(Debug, Clone, Copy, EnumAsInner, From)]
pub enum TypeItem {
    Type(UserTypeId),
    Trait(TraitId),
    Extension(ExtensionId),
    Module(ScopeId),
}

#[derive(Debug, Clone, Copy, EnumAsInner, From)]
pub enum ValueItem {
    Fn(FunctionId),
    Var(VariableId),
    StructConstructor(UserTypeId, FunctionId),
    UnionConstructor(UserTypeId),
}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub tns: HashMap<String, Vis<TypeItem>>,
    pub vns: HashMap<String, Vis<ValueItem>>,
    pub use_stmts: Vec<UsePath>,
}

impl Scope {
    pub fn find_in_tns(&self, name: &str) -> Option<Vis<TypeItem>> {
        self.tns.get(name).copied()
    }

    pub fn find_in_vns(&self, name: &str) -> Option<Vis<ValueItem>> {
        self.vns.get(name).copied()
    }
}

pub struct Scopes {
    scopes: Vec<Scope>,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
    exts: Vec<Scoped<Extension>>,
    traits: Vec<Scoped<Trait>>,
    tuples: HashMap<usize, UserTypeId>,
    structs: HashMap<Vec<String>, UserTypeId>,
    pub lang_types: HashMap<String, UserTypeId>,
    pub lang_traits: HashMap<String, TraitId>,
    pub lang_fns: HashMap<String, FunctionId>,
    pub intrinsics: HashMap<FunctionId, String>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            fns: vec![Scoped::new(Function::default(), ScopeId::ROOT)],
            types: Vec::new(),
            vars: Vec::new(),
            exts: Vec::new(),
            traits: Vec::new(),
            tuples: HashMap::new(),
            structs: HashMap::new(),
            lang_types: HashMap::new(),
            lang_traits: HashMap::new(),
            lang_fns: HashMap::new(),
            intrinsics: HashMap::new(),
        }
    }

    pub fn create_scope(&mut self, parent: ScopeId, kind: ScopeKind) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(Scope {
            parent: Some(parent),
            kind,
            ..Default::default()
        });
        id
    }

    pub fn walk(&self, id: ScopeId) -> impl Iterator<Item = (ScopeId, &Scope)> {
        pub struct ScopeIter<'a> {
            scopes: &'a Scopes,
            next: Option<ScopeId>,
        }

        impl<'a> Iterator for ScopeIter<'a> {
            type Item = (ScopeId, &'a Scope);

            fn next(&mut self) -> Option<Self::Item> {
                self.next.map(|i| {
                    self.next = self.scopes[i].parent;
                    (i, &self.scopes[i])
                })
            }
        }

        ScopeIter {
            scopes: self,
            next: Some(id),
        }
    }

    pub fn full_name(&self, id: ScopeId, ident: &str) -> String {
        let mut name: String = ident.chars().rev().collect();
        for scope_name in self.walk(id).flat_map(|(_, scope)| scope.kind.name(self)) {
            name.reserve(scope_name.len() + 1);
            name.push('_');
            for c in scope_name.chars().rev() {
                name.push(c);
            }
        }

        name.chars().rev().collect::<String>()
    }

    pub fn function_of(&self, scope: ScopeId) -> Option<FunctionId> {
        self.walk(scope)
            .find_map(|(_, scope)| scope.kind.as_function().copied())
    }

    pub fn module_of(&self, id: ScopeId) -> Option<ScopeId> {
        self.walk(id)
            .find(|(_, current)| current.kind.is_module())
            .map(|(id, _)| id)
    }

    pub fn vars(&self) -> impl Iterator<Item = (VariableId, &Scoped<Variable>)> {
        self.vars
            .iter()
            .enumerate()
            .map(|(i, var)| (VariableId(i), var))
    }

    pub fn extensions(&self) -> impl Iterator<Item = (ExtensionId, &Scoped<Extension>)> {
        self.exts
            .iter()
            .enumerate()
            .map(|(i, var)| (ExtensionId(i), var))
    }

    pub fn functions(&self) -> impl Iterator<Item = (FunctionId, &Scoped<Function>)> {
        self.fns
            .iter()
            .enumerate()
            .map(|(i, var)| (FunctionId(i), var))
    }

    pub fn get_option_id(&self) -> Option<UserTypeId> {
        self.lang_types.get("option").copied()
    }

    pub fn intrinsic_name(&self, id: FunctionId) -> Option<&str> {
        self.intrinsics.get(&id).map(|s| s.as_str())
    }

    pub fn get<T: ItemId>(&self, id: T) -> &Scoped<T::Value> {
        id.get(self)
    }

    pub fn get_mut<T: ItemId>(&mut self, id: T) -> &mut Scoped<T::Value> {
        id.get_mut(self)
    }

    pub fn implements_trait(
        &self,
        ty: &Type,
        bound: &GenericTrait,
        excl: Option<ExtensionId>,
        scope: ScopeId,
    ) -> bool {
        if ty.is_unknown() {
            return true;
        }

        let search = |this: Option<&GenericUserType>, impls: &[TraitImpl]| {
            impls.iter().flat_map(|i| i.as_checked()).any(|(tr, _)| {
                let mut tr = tr.clone();
                if let Some(this) = this {
                    for ty in tr.ty_args.values_mut() {
                        ty.fill_templates(&this.ty_args);
                    }
                }
                &tr == bound
            })
        };

        if ty
            .as_user()
            .is_some_and(|this| search(Some(this), &self.get(this.id).impls))
        {
            return true;
        }

        self.extensions_in_scope_for(ty, excl, scope)
            .any(|(_, ext)| search(ty.as_user().map(|ty| &**ty), &ext.impls))
    }

    pub fn extension_applies_to(
        &self,
        id: ExtensionId,
        ext: &Extension,
        rhs: &Type,
        scope: ScopeId,
    ) -> bool {
        match &ext.ty {
            Type::User(ut) if self.get(ut.id).data.is_template() => {
                for bound in self
                    .get(ut.id)
                    .impls
                    .iter()
                    .flat_map(|bound| bound.as_checked())
                {
                    if !self.implements_trait(rhs, bound.0, Some(id), scope) {
                        return false;
                    }
                }
                true
            }
            ty => ty == rhs,
        }
    }

    pub fn extensions_in_scope_for<'a, 'b>(
        &'a self,
        ty: &'b Type,
        excl: Option<ExtensionId>,
        current: ScopeId,
    ) -> impl Iterator<Item = (ExtensionId, &Scoped<Extension>)> + 'b
    where
        'a: 'b,
    {
        self.walk(current).flat_map(move |(_, scope)| {
            // TODO: maybe keep an extensions field to make this lookup faster
            scope
                .tns
                .iter()
                .filter_map(|id| id.1.as_extension())
                .map(|ext| (*ext, self.get(*ext)))
                .filter(move |(id, _)| Some(*id) != excl)
                .filter(move |(id, ext)| self.extension_applies_to(*id, ext, ty, current))
        })
    }

    pub fn get_tuple(&mut self, ty_args: Vec<Type>) -> Type {
        let id = if let Some(id) = self.tuples.get(&ty_args.len()) {
            *id
        } else {
            let type_params: Vec<_> = (0..ty_args.len())
                .map(|_| {
                    UserTypeId::insert_in(
                        self,
                        UserType {
                            name: Default::default(),
                            body_scope: ScopeId::ROOT,
                            data: UserTypeData::Template,
                            impls: Vec::new(),
                            type_params: Vec::new(),
                            attrs: Vec::new(),
                            fns: Vec::new(),
                            members: IndexMap::new(),
                        },
                        false,
                        ScopeId::ROOT,
                    )
                    .0
                })
                .collect();
            let (id, _) = UserTypeId::insert_in(
                self,
                UserType {
                    members: type_params
                        .iter()
                        .enumerate()
                        .map(|(i, id)| {
                            (
                                format!("{i}"),
                                CheckedMember::new(
                                    true,
                                    Type::User(GenericUserType::from_id(self, *id).into()),
                                ),
                            )
                        })
                        .collect(),
                    name: Located::new(Span::default(), "$tuple".into()),
                    body_scope: ScopeId::ROOT,
                    type_params,
                    data: UserTypeData::Tuple,
                    impls: vec![],
                    attrs: vec![],
                    fns: vec![],
                },
                false,
                ScopeId::ROOT,
            );

            self.tuples.insert(ty_args.len(), id);
            id
        };
        Type::User(GenericUserType::from_type_args(self, id, ty_args).into())
    }

    pub fn get_anon_struct(&mut self, names: Vec<String>, types: Vec<Type>) -> Type {
        let id = if let Some(id) = self.structs.get(&names) {
            *id
        } else {
            let type_params: Vec<_> = (0..names.len())
                .map(|_| {
                    UserTypeId::insert_in(
                        self,
                        UserType {
                            name: Default::default(),
                            body_scope: ScopeId::ROOT,
                            data: UserTypeData::Template,
                            impls: Vec::new(),
                            type_params: Vec::new(),
                            attrs: Vec::new(),
                            fns: Vec::new(),
                            members: IndexMap::new(),
                        },
                        false,
                        ScopeId::ROOT,
                    )
                    .0
                })
                .collect();
            let (id, _) = UserTypeId::insert_in(
                self,
                UserType {
                    members: type_params
                        .iter()
                        .enumerate()
                        .map(|(i, id)| {
                            (
                                names[i].clone(),
                                CheckedMember::new(
                                    true,
                                    Type::User(GenericUserType::from_id(self, *id).into()),
                                ),
                            )
                        })
                        .collect(),
                    name: Located::new(Span::default(), "$anonstruct".into()),
                    body_scope: ScopeId::ROOT,
                    data: UserTypeData::AnonStruct,
                    type_params,
                    impls: vec![],
                    attrs: vec![],
                    fns: vec![],
                },
                false,
                ScopeId::ROOT,
            );

            self.structs.insert(names, id);
            id
        };
        Type::User(GenericUserType::from_type_args(self, id, types).into())
    }

    pub fn get_trait_impls(&self, tr: TraitId) -> HashSet<TraitId> {
        fn inner(this: &Scopes, tr: TraitId, results: &mut HashSet<TraitId>) {
            if !results.insert(tr) {
                return;
            }

            for (imp, _) in this.get(tr).impls.iter().flat_map(|tr| tr.as_checked()) {
                inner(this, imp.id, results);
            }
        }

        let mut result = HashSet::new();
        inner(self, tr, &mut result);
        result
    }
}

impl std::ops::Index<ScopeId> for Scopes {
    type Output = Scope;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index.0]
    }
}

impl std::ops::IndexMut<ScopeId> for Scopes {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scopes[index.0]
    }
}

impl Default for Scopes {
    fn default() -> Self {
        Self::new()
    }
}
