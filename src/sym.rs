use derive_more::{Constructor, Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        checked::{CheckedExpr, CheckedPattern},
        parsed::{Expr, Linkage, Path, Pattern, TypeHint, UsePath},
        Attributes,
    },
    lexer::{Located, Span},
    typeid::{GenericTrait, GenericUserType, Type},
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
            ) -> InsertionResult<Self> {
                let index = scopes.$vec.len();
                scopes.$vec.push(Scoped::new(value, scope));
                let id = $name(index);
                let name = (scopes.$vec[id.0].name.data).clone();
                let kind = id.into();
                let prev = scopes[scope]
                    .$namespace
                    .insert(name, Vis { id: kind, public });
                InsertionResult {
                    id,
                    existed: prev.is_some(),
                    item: kind.into(),
                }
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

#[derive(Debug, Clone, Copy, From, EnumAsInner)]
pub enum InsertedItem {
    TypeLike(TypeItem),
    ValueLike(ValueItem),
}

#[derive(Debug, Clone, Copy, From)]
pub struct InsertionResult<T> {
    pub id: T,
    pub item: InsertedItem,
    pub existed: bool,
}

id!(FunctionId => Function, fns, vns);
id!(UserTypeId => UserType, types, tns);
id!(VariableId => Variable, vars, vns);

pub type TraitId = UserTypeId;
pub type ExtensionId = UserTypeId;

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
    Impl(TraitImpl),
    Module(Located<String>),
    Defer,
    #[default]
    None,
}

impl ScopeKind {
    pub fn name<'a, 'b>(&'a self, scopes: &'b Scopes) -> Option<&'b Located<String>>
    where
        'a: 'b,
    {
        match self {
            &ScopeKind::Function(id) => Some(&scopes.get(id).name),
            &ScopeKind::UserType(id) => Some(&scopes.get(id).name),
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
        path: Path,
    },
    Known {
        scope: ScopeId,
        tr: &'static str,
        ty_args: Vec<TypeHint>,
        span: Span,
    },
    Checked(GenericTrait),
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
    pub public: bool,
    pub name: Located<String>,
    pub ty: Type,
    pub is_static: bool,
    pub mutable: bool,
    pub value: Option<CheckedExpr>,
    pub unused: bool,
    pub has_hint: bool,
}

#[derive(Default, Debug, Clone)]
pub struct Function {
    pub public: bool,
    pub attrs: Attributes,
    pub name: Located<String>,
    pub linkage: Linkage,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    /// Is this a trait function with a body
    pub has_body: bool,
    pub assign_subscript: bool,
    pub type_params: Vec<UserTypeId>,
    pub params: Vec<CheckedParam>,
    pub ret: Type,
    pub body: Option<CheckedExpr>,
    pub constructor: Option<UserTypeId>,
    pub body_scope: ScopeId,
}

impl FunctionId {
    pub const RESERVED: FunctionId = FunctionId(0);
}

#[derive(Debug, Clone, Constructor)]
pub struct CheckedMember {
    pub public: bool,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub variants: IndexMap<String, (Option<Type>, Span)>,
    pub tag: Type,
    pub enum_union: bool,
}

impl Union {
    pub fn variant_tag(&self, name: &str) -> Option<usize> {
        self.variants.get_index_of(name)
    }
}

#[derive(Debug, EnumAsInner, Clone)]
pub enum UserTypeKind {
    Struct,
    Union(Union),
    UnsafeUnion,
    Template,
    AnonStruct,
    Tuple,
    Trait(UserTypeId),
    Extension(Type),
}

#[derive(Debug, Clone)]
pub struct UserType {
    pub attrs: Attributes,
    pub public: bool,
    pub name: Located<String>,
    pub body_scope: ScopeId,
    pub kind: UserTypeKind,
    pub impls: Vec<TraitImpl>,
    pub type_params: Vec<UserTypeId>,
    pub fns: Vec<Vis<FunctionId>>,
    pub subscripts: Vec<FunctionId>,
    pub members: IndexMap<String, CheckedMember>,
}

impl UserType {
    pub fn find_associated_fn(&self, scopes: &Scopes, name: &str) -> Option<FunctionId> {
        self.fns
            .iter()
            .map(|f| f.id)
            .find(|&id| scopes.get(id).name.data == name)
    }

    pub fn is_empty_variant(&self, variant: &str) -> bool {
        self.members.is_empty()
            && self
                .kind
                .as_union()
                .and_then(|u| u.variants.get(variant))
                .is_some_and(|u| u.0.is_none())
    }

    pub fn template(name: Located<String>, scope: ScopeId, impls: Vec<TraitImpl>) -> Self {
        Self {
            public: false,
            name,
            body_scope: scope,
            kind: UserTypeKind::Template,
            type_params: Vec::new(),
            impls,
            fns: vec![],
            attrs: Default::default(),
            members: Default::default(),
            subscripts: Vec::new(),
        }
    }
}

pub trait HasTypeParams {
    fn get_type_params(&self) -> &[UserTypeId];
}

impl HasTypeParams for UserType {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }
}

impl HasTypeParams for Function {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }
}

#[derive(Deref, DerefMut, Constructor, Clone)]
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

    fn insert_in(
        scopes: &mut Scopes,
        val: Self::Value,
        public: bool,
        id: ScopeId,
    ) -> InsertionResult<Self>;
}

#[derive(Debug, Clone, Copy, EnumAsInner, From)]
pub enum TypeItem {
    Type(UserTypeId),
    Module(ScopeId),
}

#[derive(Debug, Clone, Copy, EnumAsInner, From)]
pub enum ValueItem {
    Fn(FunctionId),
    Var(VariableId),
    StructConstructor(UserTypeId, FunctionId),
    UnionConstructor(UserTypeId),
}

#[derive(Default, Debug, Clone)]
pub struct Scope {
    pub public: bool,
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

#[derive(Clone)]
pub struct Scopes {
    scopes: Vec<Scope>,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
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
            tuples: HashMap::new(),
            structs: HashMap::new(),
            lang_types: HashMap::new(),
            lang_traits: HashMap::new(),
            lang_fns: HashMap::new(),
            intrinsics: HashMap::new(),
        }
    }

    pub fn create_scope(&mut self, parent: ScopeId, kind: ScopeKind, public: bool) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(Scope {
            parent: Some(parent),
            kind,
            public,
            ..Default::default()
        });
        id
    }

    pub fn walk(&self, id: ScopeId) -> ScopeIter {
        ScopeIter {
            scopes: self,
            next: Some(id),
        }
    }

    pub fn full_name(&self, id: ScopeId, ident: &str) -> String {
        let mut name: String = ident.chars().rev().collect();
        for scope_name in self.walk(id).flat_map(|(_, scope)| scope.kind.name(self)) {
            name.reserve(scope_name.data.len() + 1);
            name.push('_');
            for c in scope_name.data.chars().rev() {
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

    pub fn get_tuple(&mut self, ty_args: Vec<Type>) -> Type {
        let id = if let Some(id) = self.tuples.get(&ty_args.len()) {
            *id
        } else {
            let type_params: Vec<_> = (0..ty_args.len())
                .map(|_| {
                    UserTypeId::insert_in(
                        self,
                        UserType {
                            public: false,
                            name: Default::default(),
                            body_scope: ScopeId::ROOT,
                            kind: UserTypeKind::Template,
                            impls: Vec::new(),
                            type_params: Vec::new(),
                            attrs: Default::default(),
                            fns: Vec::new(),
                            members: IndexMap::new(),
                            subscripts: Vec::new(),
                        },
                        false,
                        ScopeId::ROOT,
                    )
                    .id
                })
                .collect();
            let res = UserTypeId::insert_in(
                self,
                UserType {
                    public: false,
                    members: type_params
                        .iter()
                        .enumerate()
                        .map(|(i, id)| {
                            (
                                format!("{i}"),
                                CheckedMember::new(
                                    true,
                                    Type::User(GenericUserType::from_id(self, *id).into()),
                                    Span::default(),
                                ),
                            )
                        })
                        .collect(),
                    name: Located::new(Span::default(), "$tuple".into()),
                    body_scope: ScopeId::ROOT,
                    type_params,
                    kind: UserTypeKind::Tuple,
                    attrs: Default::default(),
                    impls: vec![],
                    fns: vec![],
                    subscripts: Vec::new(),
                },
                false,
                ScopeId::ROOT,
            );

            self.tuples.insert(ty_args.len(), res.id);
            res.id
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
                            public: false,
                            name: Default::default(),
                            body_scope: ScopeId::ROOT,
                            kind: UserTypeKind::Template,
                            impls: Vec::new(),
                            type_params: Vec::new(),
                            attrs: Default::default(),
                            fns: Vec::new(),
                            members: IndexMap::new(),
                            subscripts: Vec::new(),
                        },
                        false,
                        ScopeId::ROOT,
                    )
                    .id
                })
                .collect();
            let res = UserTypeId::insert_in(
                self,
                UserType {
                    public: false,
                    members: type_params
                        .iter()
                        .enumerate()
                        .map(|(i, id)| {
                            (
                                names[i].clone(),
                                CheckedMember::new(
                                    true,
                                    Type::User(GenericUserType::from_id(self, *id).into()),
                                    Span::default(),
                                ),
                            )
                        })
                        .collect(),
                    name: Located::new(Span::default(), "$anonstruct".into()),
                    body_scope: ScopeId::ROOT,
                    kind: UserTypeKind::AnonStruct,
                    type_params,
                    attrs: Default::default(),
                    impls: vec![],
                    fns: vec![],
                    subscripts: Vec::new(),
                },
                false,
                ScopeId::ROOT,
            );

            self.structs.insert(names, res.id);
            res.id
        };
        Type::User(GenericUserType::from_type_args(self, id, types).into())
    }

    pub fn get_trait_impls(&self, tr: TraitId) -> HashSet<TraitId> {
        fn inner(this: &Scopes, tr: TraitId, results: &mut HashSet<TraitId>) {
            if !results.insert(tr) {
                return;
            }

            for tr in this.get(tr).impls.iter().flat_map(|tr| tr.as_checked()) {
                inner(this, tr.id, results);
            }
        }

        let mut result = HashSet::new();
        inner(self, tr, &mut result);
        result
    }

    pub fn has_builtin_impl(&self, ty: &Type, bound: &GenericTrait) -> bool {
        if ty.is_numeric() && Some(&bound.id) == self.lang_traits.get("numeric") {
            return true;
        }

        if let Some(int) = ty.as_integral() {
            if Some(&bound.id) == self.lang_traits.get("integral") {
                return true;
            }
            if int.signed && Some(&bound.id) == self.lang_traits.get("signed") {
                return true;
            }
            if !int.signed && Some(&bound.id) == self.lang_traits.get("unsigned") {
                return true;
            }
        }

        false
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
