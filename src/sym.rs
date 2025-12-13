use derive_more::{Constructor, Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        Attributes,
        checked::{Expr as CheckedExpr, Pattern as CheckedPattern},
        parsed::{Expr, Path, Pattern, TypeHint, UsePath},
    },
    comptime_int::ComptimeInt,
    hash::{HashMap, HashSet, IndexMap},
    intern::{StrId, Strings},
    lexer::{Located, Span},
    typeid::{GenericTrait, GenericUserType, Type, TypeId, Types},
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
                let name = scopes.$vec[id.0].name.data;
                let kind = id.into();
                let prev = scopes[scope].$namespace.insert(name, Vis { id: kind, public });
                InsertionResult { id, existed: prev.is_some(), item: kind.into() }
            }

            fn name<'a>(&self, scopes: &'a Scopes) -> &'a Located<StrId> {
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LoopBreak {
    None,
    WithValue,
    WithNothing,
}

#[derive(Debug, Clone)]
pub struct LoopScopeKind {
    pub target: Option<TypeId>,
    pub breaks: LoopBreak,
    pub infinite: bool,
    pub label: Option<StrId>,
}

#[derive(Debug, Clone)]
pub struct BlockScopeKind {
    pub target: Option<TypeId>,
    pub yields: bool,
    pub label: Option<StrId>,
    pub branches: bool,
}

#[derive(Default, Debug, Clone, EnumAsInner)]
pub enum ScopeKind {
    Block(BlockScopeKind),
    Loop(LoopScopeKind),
    Lambda(Option<TypeId>, bool),
    Function(FunctionId),
    UserType(UserTypeId),
    Impl(usize),
    Module(Located<StrId>),
    Static(VariableId),
    Defer,
    #[default]
    None,
}

impl ScopeKind {
    pub fn name<'a, 'b>(&'a self, scopes: &'b Scopes) -> Option<&'b Located<StrId>>
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

#[derive(Clone)]
pub enum DefaultExpr {
    Unchecked(ScopeId, Expr),
    Checked(CheckedExpr),
}

#[derive(Clone, EnumAsInner)]
pub enum ParamPattern {
    Unchecked(Located<Pattern>),
    Checked(CheckedPattern),
}

#[derive(Clone)]
pub enum TraitImplData {
    Path(Path),
    Operator { tr: StrId, ty_args: Vec<Located<TypeHint>>, span: Span },
}

#[derive(Clone, EnumAsInner, Default)]
pub enum TraitImpl {
    Unchecked {
        scope: ScopeId,
        data: TraitImplData,
    },
    Checked(GenericTrait),
    #[default]
    None,
}

#[derive(Clone)]
pub struct CheckedParam {
    pub keyword: bool,
    pub label: StrId,
    pub patt: ParamPattern,
    pub ty: TypeId,
    pub default: Option<DefaultExpr>,
}

#[derive(Default)]
pub struct Variable {
    pub attrs: Attributes,
    pub public: bool,
    pub name: Located<StrId>,
    pub ty: TypeId,
    pub is_static: bool,
    pub is_extern: bool,
    pub mutable: bool,
    pub value: Option<CheckedExpr>,
    pub unused: bool,
    pub has_hint: bool,
}

#[derive(Default)]
pub struct Function {
    pub public: bool,
    pub attrs: Attributes,
    pub name: Located<StrId>,
    pub is_extern: bool,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    /// Is this a trait function with a body
    pub has_body: bool,
    pub assign_subscript: bool,
    pub type_params: Vec<UserTypeId>,
    pub params: Vec<CheckedParam>,
    pub ret: TypeId,
    pub body: Option<CheckedExpr>,
    pub constructor: Option<UserTypeId>,
    pub body_scope: ScopeId,
}

impl Function {
    pub fn is_dyn_compatible(&self, scopes: &Scopes, types: &Types, tr: UserTypeId) -> bool {
        let (&this, _) = scopes
            .get(tr)
            .kind
            .as_trait()
            .expect("ICE: Attempt to get dyn compatibility of non-trait function");

        self.type_params.is_empty()
            && self
                .params
                .first()
                .is_some_and(|p| p.label == Strings::THIS_PARAM && types[p.ty].is_safe_ptr())
            && self.params.iter().all(|p| !types[p.ty].as_user().is_some_and(|ty| ty.id == this))
    }
}

impl FunctionId {
    pub const RESERVED: FunctionId = FunctionId(0);
}

#[derive(Constructor)]
pub struct CheckedMember {
    pub public: bool,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Default, Clone, EnumAsInner)]
pub enum Discriminant {
    Unchecked(Expr),
    #[default]
    Next,
    Checked(ComptimeInt),
}

#[derive(Clone)]
pub struct CheckedVariant {
    pub ty: Option<TypeId>,
    pub span: Span,
    pub discrim: Discriminant,
}

#[derive(Clone)]
pub struct Union {
    pub variants: IndexMap<StrId, CheckedVariant>,
    pub tag: TypeId,
    pub enum_union: bool,
}

impl Union {
    pub fn discriminant(&self, name: StrId) -> Option<&ComptimeInt> {
        self.variants.get(&name).and_then(|v| v.discrim.as_checked())
    }
}

#[derive(Debug, Clone, Default)]
pub struct PackedStruct {
    pub bit_offsets: HashMap<StrId, u32>,
    pub size: usize,
    pub align: usize,
}

#[derive(EnumAsInner)]
pub enum UserTypeKind {
    Struct,
    PackedStruct(PackedStruct),
    Union(Union),
    UnsafeUnion,
    Template,
    AnonStruct,
    Tuple,
    Trait(UserTypeId, bool),
    Extension(TypeId),
}

pub struct ImplBlockData {
    pub type_params: Vec<UserTypeId>,
}

impl HasTypeParams for ImplBlockData {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }
}

pub struct UserType {
    pub attrs: Attributes,
    pub public: bool,
    pub name: Located<StrId>,
    pub body_scope: ScopeId,
    pub kind: UserTypeKind,
    pub impls: Vec<TraitImpl>,
    pub impl_blocks: Vec<ImplBlockData>,
    pub type_params: Vec<UserTypeId>,
    pub fns: Vec<Vis<FunctionId>>,
    pub subscripts: Vec<FunctionId>,
    pub members: IndexMap<StrId, CheckedMember>,
    pub members_resolved: bool,
    pub recursive: bool,
}

impl UserType {
    pub fn find_associated_fn(&self, scopes: &Scopes, name: StrId) -> Option<FunctionId> {
        self.fns.iter().map(|f| f.id).find(|&id| scopes.get(id).name.data == name)
    }

    pub fn is_empty_variant(&self, variant: StrId) -> bool {
        self.members.is_empty()
            && self
                .kind
                .as_union()
                .and_then(|u| u.variants.get(&variant))
                .is_some_and(|u| u.ty.is_none())
    }

    pub fn template(name: Located<StrId>, scope: ScopeId, impls: Vec<TraitImpl>) -> Self {
        Self {
            public: false,
            name,
            body_scope: scope,
            kind: UserTypeKind::Template,
            type_params: Vec::new(),
            impls,
            impl_blocks: Vec::new(),
            fns: vec![],
            attrs: Default::default(),
            members: Default::default(),
            subscripts: Vec::new(),
            members_resolved: true,
            recursive: false,
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
    fn name<'a>(&self, scopes: &'a Scopes) -> &'a Located<StrId>;

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

#[derive(Default)]
pub struct Scope {
    pub public: bool,
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub tns: HashMap<StrId, Vis<TypeItem>>,
    pub vns: HashMap<StrId, Vis<ValueItem>>,
    pub use_stmts: Vec<UsePath>,
}

impl Scope {
    pub fn find_in_tns(&self, name: StrId) -> Option<Vis<TypeItem>> {
        self.tns.get(&name).copied()
    }

    pub fn find_in_vns(&self, name: StrId) -> Option<Vis<ValueItem>> {
        self.vns.get(&name).copied()
    }
}

pub struct Scopes {
    scopes: Vec<Scope>,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
    tuples: HashMap<usize, UserTypeId>,
    structs: HashMap<Vec<StrId>, UserTypeId>,
    pub lang_types: HashMap<StrId, UserTypeId>,
    pub intrinsics: HashMap<FunctionId, StrId>,
    pub autouse_tns: HashMap<StrId, Vis<TypeItem>>,
    pub autouse_vns: HashMap<StrId, Vis<ValueItem>>,
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
            intrinsics: HashMap::new(),
            autouse_tns: HashMap::new(),
            autouse_vns: HashMap::new(),
        }
    }

    pub fn create_scope(&mut self, parent: ScopeId, kind: ScopeKind, public: bool) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(Scope { parent: Some(parent), kind, public, ..Default::default() });
        id
    }

    pub fn walk(&self, id: ScopeId) -> ScopeIter<'_> {
        ScopeIter { scopes: self, next: Some(id) }
    }

    pub fn function_of(&self, scope: ScopeId) -> Option<FunctionId> {
        self.walk(scope).find_map(|(_, scope)| scope.kind.as_function().copied())
    }

    pub fn module_of(&self, id: ScopeId) -> Option<ScopeId> {
        self.walk(id).find(|(_, current)| current.kind.is_module()).map(|(id, _)| id)
    }

    pub fn vars(&self) -> impl Iterator<Item = (VariableId, &Scoped<Variable>)> {
        self.vars.iter().enumerate().map(|(i, var)| (VariableId(i), var))
    }

    pub fn functions(&self) -> impl Iterator<Item = (FunctionId, &Scoped<Function>)> {
        self.fns.iter().enumerate().map(|(i, var)| (FunctionId(i), var))
    }

    pub fn get_option_id(&self) -> Option<UserTypeId> {
        self.lang_types.get(&Strings::LANG_OPTION).copied()
    }

    pub fn intrinsic_name(&self, id: FunctionId) -> Option<StrId> {
        self.intrinsics.get(&id).copied()
    }

    pub fn get<T: ItemId>(&self, id: T) -> &Scoped<T::Value> {
        id.get(self)
    }

    pub fn get_mut<T: ItemId>(&mut self, id: T) -> &mut Scoped<T::Value> {
        id.get_mut(self)
    }

    pub fn get_tuple(
        &mut self,
        ty_args: Vec<TypeId>,
        strings: &mut Strings,
        types: &mut Types,
    ) -> TypeId {
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
                            impl_blocks: Vec::new(),
                            type_params: Vec::new(),
                            attrs: Default::default(),
                            fns: Vec::new(),
                            members: IndexMap::new(),
                            subscripts: Vec::new(),
                            members_resolved: true,
                            recursive: false,
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
                            let ty = Type::User(GenericUserType::from_id(self, types, *id));
                            (
                                strings.get_or_intern(format!("{i}")),
                                CheckedMember::new(true, types.insert(ty), Span::default()),
                            )
                        })
                        .collect(),
                    name: Located::nowhere(
                        strings.get_or_intern(format!("$tuple{}", ty_args.len())),
                    ),
                    body_scope: ScopeId::ROOT,
                    type_params,
                    kind: UserTypeKind::Tuple,
                    attrs: Default::default(),
                    impls: Vec::new(),
                    impl_blocks: Vec::new(),
                    fns: Vec::new(),
                    subscripts: Vec::new(),
                    members_resolved: true,
                    recursive: false,
                },
                false,
                ScopeId::ROOT,
            );

            self.tuples.insert(ty_args.len(), res.id);
            res.id
        };
        types.insert(Type::User(GenericUserType::from_type_args(self, id, ty_args)))
    }

    pub fn get_anon_struct(
        &mut self,
        names: Vec<StrId>,
        ty_args: Vec<TypeId>,
        strings: &mut Strings,
        types: &mut Types,
    ) -> TypeId {
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
                            impl_blocks: Vec::new(),
                            type_params: Vec::new(),
                            attrs: Default::default(),
                            fns: Vec::new(),
                            members: IndexMap::new(),
                            subscripts: Vec::new(),
                            members_resolved: true,
                            recursive: false,
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
                            let ty = Type::User(GenericUserType::from_id(self, types, *id));
                            (names[i], CheckedMember::new(true, types.insert(ty), Span::default()))
                        })
                        .collect(),
                    name: Located::nowhere(strings.get_or_intern("$anonstruct")),
                    body_scope: ScopeId::ROOT,
                    kind: UserTypeKind::AnonStruct,
                    type_params,
                    attrs: Default::default(),
                    impls: Vec::new(),
                    impl_blocks: Vec::new(),
                    fns: Vec::new(),
                    subscripts: Vec::new(),
                    members_resolved: true,
                    recursive: false,
                },
                false,
                ScopeId::ROOT,
            );

            self.structs.insert(names, res.id);
            res.id
        };
        types.insert(Type::User(GenericUserType::from_type_args(self, id, ty_args)))
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

    pub fn get_trait_impls_ex(&self, types: &mut Types, tr: GenericTrait) -> HashSet<GenericTrait> {
        fn inner(
            this: &Scopes,
            types: &mut Types,
            tr: GenericTrait,
            results: &mut HashSet<GenericTrait>,
        ) {
            if !results.insert(tr.clone()) {
                return;
            }

            for imp in this.get(tr.id).impls.iter().flat_map(|tr| tr.as_checked()) {
                let mut imp = imp.clone();
                imp.fill_templates(types, &tr.ty_args);
                inner(this, types, imp, results);
            }
        }

        let mut result = HashSet::new();
        inner(self, types, tr, &mut result);
        result
    }

    pub fn has_builtin_impl(&self, types: &Types, id: TypeId, bound: &GenericTrait) -> bool {
        let ty = &types[id];
        if ty.is_numeric() && Some(&bound.id) == self.lang_types.get(&Strings::LANG_NUMERIC) {
            return true;
        }

        if let Some(int) = ty.as_integral(false) {
            if Some(&bound.id) == self.lang_types.get(&Strings::LANG_INTEGRAL) {
                return true;
            }
            if int.signed && Some(&bound.id) == self.lang_types.get(&Strings::LANG_SIGNED) {
                return true;
            }
            if !int.signed && Some(&bound.id) == self.lang_types.get(&Strings::LANG_UNSIGNED) {
                return true;
            }
        }

        false
    }

    pub fn borrow_twice(&mut self, a: ScopeId, b: ScopeId) -> Option<(&mut Scope, &mut Scope)> {
        self.scopes.get_disjoint_mut([a.0, b.0]).ok().map(|[a, b]| (a, b))
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
