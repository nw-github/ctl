use derive_more::{Constructor, Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        DefaultCapturePolicy, FnAbi, Visibility,
        checked::{Expr as CheckedExpr, PatternData as CheckedPatternData},
        declared::UsePath,
        parsed::{Expr, FunctionType, Path, Pattern, TypeHint},
    },
    ds::{ComptimeInt, HashMap, HashSet, IndexMap},
    intern::{StrId, Strings},
    lexer::{Located, Span},
    project::{ImplId, Project},
    typeid::{GenericTrait, TypeId, Types},
};
pub use attrs::*;

#[path = "attrs.rs"]
mod attrs;

macro_rules! id {
    ($name: ident => $output: ident, $vec: ident, $namespace: ident) => {
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
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
                scope: ScopeId,
            ) -> InsertionResult<Self> {
                let index = scopes.$vec.len();
                scopes.$vec.push(Scoped::new(value, scope));
                let id = $name(index);
                let name = scopes.$vec[id.0].name.data;
                let vis = scopes.$vec[id.0].vis;
                let kind = id.into();
                let prev = scopes[scope].$namespace.insert(name, Vis { id: kind, vis });
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

#[derive(Default, Debug, PartialEq, Eq, Clone, Copy, Hash, derive_more::Display)]
pub struct ScopeId(usize);

impl ScopeId {
    pub const ROOT: ScopeId = ScopeId(0);
    pub const DUMMY: ScopeId = ScopeId(1);
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
id!(VariableId => Variable, vars, vns);
id!(UserTypeId => UserType, types, tns);
id!(AliasId => Alias, aliases, tns);
id!(TraitId => Trait, traits, tns);

pub type ExtensionId = UserTypeId;
pub type TypeParamId = UserTypeId;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LoopBreak {
    None,
    WithValue,
    WithNothing,
}

#[derive(Debug, Clone, Copy)]
pub struct LoopScopeKind {
    pub target: Option<TypeId>,
    pub breaks: LoopBreak,
    pub infinite: bool,
    pub label: Option<StrId>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockScopeKind {
    pub target: Option<TypeId>,
    pub yields: bool,
    pub label: Option<StrId>,
    pub branches: bool,
}

#[derive(Default, Debug, Clone, Copy, EnumAsInner)]
pub enum ScopeKind {
    Block(BlockScopeKind),
    Loop(LoopScopeKind),
    Closure(Option<TypeId>, DefaultCapturePolicy),
    Function(FunctionId),
    UserType(UserTypeId),
    Trait(TraitId),
    Impl(ImplId),
    Module(Located<StrId>),
    Static(VariableId),
    Defer,
    #[default]
    None,
}

impl ScopeKind {
    pub fn name(&self, scopes: &Scopes) -> Option<Located<StrId>> {
        match *self {
            ScopeKind::Function(id) => Some(scopes.get(id).name),
            ScopeKind::UserType(id) => Some(scopes.get(id).name),
            ScopeKind::Trait(id) => Some(scopes.get(id).name),
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
    Checked(CheckedPatternData),
}

#[derive(Clone)]
pub struct CheckedParam {
    pub keyword: bool,
    pub label: StrId,
    pub patt: ParamPattern,
    pub ty: TypeId,
    pub default: Option<DefaultExpr>,
}

#[derive(Default, Clone, Copy, EnumAsInner)]
pub enum VariableKind {
    #[default]
    Normal,
    Static,
    Const,
    Capture,
}

impl VariableKind {
    pub fn is_local(&self) -> bool {
        matches!(self, VariableKind::Capture | VariableKind::Normal)
    }
}

#[derive(Default)]
pub struct Variable {
    pub attrs: VariableAttrs,
    pub vis: Visibility,
    pub name: Located<StrId>,
    pub ty: TypeId,
    pub kind: VariableKind,
    pub is_extern: bool,
    pub mutable: bool,
    pub value: Option<CheckedExpr>,
    pub unused: bool,
    pub has_hint: bool,
    pub param: bool,
}

#[derive(Default)]
pub struct Function {
    pub vis: Visibility,
    pub attrs: FunctionAttrs,
    pub name: Located<StrId>,
    pub abi: FnAbi,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    /// Is this a trait function with a body
    pub has_body: bool,
    pub typ: FunctionType,
    pub type_params: Vec<TypeParamId>,
    pub params: Vec<CheckedParam>,
    pub ret: TypeId,
    pub body: Option<CheckedExpr>,
    pub constructor: Option<UserTypeId>,
    pub body_scope: ScopeId,
    pub full_span: Span,
}

impl Function {
    pub fn is_dyn_compatible(&self, scopes: &Scopes, types: &Types, tr: TraitId) -> bool {
        let this = scopes.get(tr).this;
        self.type_params.is_empty()
            && self
                .params
                .first()
                .is_some_and(|p| p.label == Strings::THIS_PARAM && types[p.ty].is_safe_ptr())
            && self.params.iter().all(|p| types[p.ty].as_user().is_none_or(|ty| ty.id != this))
    }
}

impl FunctionId {
    pub const RESERVED: FunctionId = FunctionId(0);
}

#[derive(Constructor)]
pub struct CheckedMember {
    pub vis: Visibility,
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

#[derive(Default, EnumAsInner)]
pub enum TraitImpl {
    Checked(CheckedImpl),
    Unchecked(TypeId, UncheckedImpl),
    Derived(ScopeId, UserTypeId, Path),
    #[default]
    None,
}

#[derive(Clone)]
pub struct CheckedImpl {
    // Some(impl block scope) if this implementation is from an impl block. None if the
    // implementation is a generic bound, or the implementation is synthesized by the compiler.
    pub scope: Option<ScopeId>,
    pub assoc_types: HashMap<TypeParamId, Located<TypeId>>,
    /// The type parameters for the impl block (`T` in `impl<T> Foo`) and the enclosing scope
    pub type_params: Vec<TypeParamId>,
    /// The type args for this trait (`T = i32` in `impl Foo<i32>`)
    pub tr: GenericTrait,
    /// The type this trait is implemented for
    pub ty: TypeId,
    pub span: Span,
    /// This is the automatic impl of a super trait generated by resolve_impls for a type parameter
    pub super_trait: bool,
}

pub enum UncheckedImplTrait {
    Lang(LangTrait, Vec<TypeHint>),
    Path(Path),
    Known(GenericTrait),
}

pub struct UncheckedImpl {
    pub tr: UncheckedImplTrait,
    pub type_params: Vec<TypeParamId>,
    pub assoc_types: HashMap<StrId, Located<TypeId>>,
    pub span: Span,
    /// The scope of the impl block
    pub scope: ScopeId,
    pub is_type_param: Option<TypeParamId>,
    pub is_unsafe: bool,
}

impl UncheckedImpl {
    pub fn type_param(
        tr: UncheckedImplTrait,
        id: TypeParamId,
        scope: ScopeId,
        span: Span,
    ) -> UncheckedImpl {
        UncheckedImpl {
            tr,
            type_params: Default::default(),
            assoc_types: Default::default(),
            span,
            scope,
            is_type_param: Some(id),
            is_unsafe: false,
        }
    }
}

#[derive(EnumAsInner)]
pub enum UserTypeKind {
    Struct(FunctionId, bool),
    Union(Union),
    UnsafeUnion,
    Template,
    Tuple,
    Closure,
    Extension(Option<TypeId>),
}

impl UserTypeKind {
    pub fn is_packed_struct(&self) -> bool {
        matches!(self, Self::Struct(_, true))
    }
}

pub struct UserType {
    pub attrs: UserTypeAttrs,
    pub vis: Visibility,
    pub name: Located<StrId>,
    pub body_scope: ScopeId,
    pub kind: UserTypeKind,
    pub impls: Vec<ImplId>,
    pub type_params: Vec<TypeParamId>,
    pub members: IndexMap<StrId, CheckedMember>,
    pub members_resolved: bool,
    pub recursive: bool,
    pub interior_mutable: bool,
    pub full_span: Span,
}

impl UserType {
    pub fn find_associated_fn(&self, scopes: &Scopes, name: StrId) -> Option<FunctionId> {
        scopes[self.body_scope].vns.get(&name).and_then(|item| item.into_fn().ok())
    }

    pub fn is_empty_variant(&self, variant: StrId) -> bool {
        self.members.is_empty()
            && self
                .kind
                .as_union()
                .and_then(|u| u.variants.get(&variant))
                .is_some_and(|u| u.ty.is_none())
    }

    pub fn type_param(name: Located<StrId>) -> Self {
        Self {
            vis: Visibility::Private,
            name,
            body_scope: ScopeId::DUMMY,
            kind: UserTypeKind::Template,
            type_params: Vec::new(),
            impls: Default::default(),
            attrs: Default::default(),
            members: Default::default(),
            members_resolved: true,
            recursive: false,
            interior_mutable: false,
            full_span: name.span,
        }
    }

    pub fn get_subscripts<'a>(
        &self,
        scopes: &'a Scopes,
    ) -> impl Iterator<Item = FunctionId> + use<'a> {
        scopes[self.body_scope]
            .vns
            .iter()
            .flat_map(|f| f.1.as_fn())
            .filter(|f| {
                matches!(
                    scopes.get(**f).typ,
                    FunctionType::AssignSubscript | FunctionType::Subscript
                )
            })
            .copied()
    }

    pub fn iter_impls<'a>(
        &'a self,
        proj: &'a Project,
        super_traits: bool,
    ) -> impl Iterator<Item = &'a CheckedImpl> {
        self.impls.iter().flat_map(move |imp| {
            proj.impls.get(*imp).as_checked().filter(|imp| super_traits || !imp.super_trait)
        })
    }

    pub fn iter_impls_owned(&self, proj: &Project, super_traits: bool) -> Vec<GenericTrait> {
        self.iter_impls(proj, super_traits).map(|s| s.tr.clone()).collect::<Vec<_>>()
    }
}

pub struct Alias {
    pub vis: Visibility,
    pub name: Located<StrId>,
    pub type_params: Vec<TypeParamId>,
    pub ty: Option<TypeId>,
    pub body_scope: ScopeId,
}

pub enum SuperTraits {
    Checking,
    Unchecked(Vec<Path>),
    Checked(Vec<GenericTrait>),
}

impl SuperTraits {
    pub fn iter_checked(&self) -> impl Iterator<Item = &'_ GenericTrait> {
        if let Self::Checked(checked) = self {
            checked.iter()
        } else {
            panic!("attempt to iter_checked() unchecked SuperTraits")
        }
    }

    pub fn iter_checked_owned(&self) -> impl Iterator<Item = GenericTrait> + use<> {
        if let Self::Checked(checked) = self {
            checked.clone().into_iter()
        } else {
            panic!("attempt to into_iter_checked() unchecked SuperTraits")
        }
    }
}

pub struct Trait {
    pub attrs: TraitAttrs,
    pub vis: Visibility,
    pub name: Located<StrId>,
    pub body_scope: ScopeId,
    pub type_params: Vec<TypeParamId>,
    pub super_traits: SuperTraits,
    pub assoc_types: HashMap<StrId, TypeParamId>,
    /// The template parameter corresponding to the `This` type
    pub this: TypeParamId,
    pub seal: Visibility,
    pub is_unsafe: bool,
    pub implementors: Vec<ImplId>,
    pub full_span: Span,
}

pub trait HasTypeParams {
    fn get_type_params(&self) -> &[TypeParamId];
}

impl HasTypeParams for UserType {
    fn get_type_params(&self) -> &[TypeParamId] {
        &self.type_params
    }
}

impl HasTypeParams for Trait {
    fn get_type_params(&self) -> &[TypeParamId] {
        &self.type_params
    }
}

impl HasTypeParams for Function {
    fn get_type_params(&self) -> &[TypeParamId] {
        &self.type_params
    }
}

impl HasTypeParams for Alias {
    fn get_type_params(&self) -> &[TypeParamId] {
        &self.type_params
    }
}

impl HasTypeParams for [TypeParamId] {
    fn get_type_params(&self) -> &[TypeParamId] {
        self
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
    pub vis: Visibility,
}

pub trait ItemId: Sized + Copy + Clone {
    type Value;

    fn get(self, scopes: &Scopes) -> &Scoped<Self::Value>;
    fn get_mut(self, scopes: &mut Scopes) -> &mut Scoped<Self::Value>;
    fn name<'a>(&self, scopes: &'a Scopes) -> &'a Located<StrId>;
    fn insert_in(scopes: &mut Scopes, val: Self::Value, id: ScopeId) -> InsertionResult<Self>;
}

#[derive(Debug, Clone, Copy, EnumAsInner, From)]
pub enum TypeItem {
    Type(UserTypeId),
    Trait(TraitId),
    Module(ScopeId),
    Alias(AliasId),
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
    pub vis: Visibility,
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

    pub fn find_fn(&self, name: StrId) -> Option<Vis<FunctionId>> {
        self.find_in_vns(name).and_then(|v| v.into_fn().ok().map(|id| Vis::new(id, v.vis)))
    }

    pub fn iter_fns(&self) -> impl Iterator<Item = FunctionId> {
        self.vns.values().flat_map(|v| v.into_fn().ok())
    }
}

pub struct Scopes {
    scopes: Vec<Scope>,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    aliases: Vec<Scoped<Alias>>,
    traits: Vec<Scoped<Trait>>,
    vars: Vec<Scoped<Variable>>,
    pub tuples: HashMap<Vec<StrId>, UserTypeId>,
    pub lang_types: HashMap<LangType, UserTypeId>,
    pub lang_traits: HashMap<LangTrait, TraitId>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default(), Scope::default()],
            fns: vec![Scoped::new(Function::default(), ScopeId::ROOT)],
            types: Vec::new(),
            vars: Vec::new(),
            traits: Vec::new(),
            aliases: Vec::new(),
            tuples: HashMap::new(),
            lang_types: HashMap::new(),
            lang_traits: HashMap::new(),
        }
    }

    pub fn create_scope(&mut self, parent: ScopeId, kind: ScopeKind, vis: Visibility) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(Scope { parent: Some(parent), kind, vis, ..Default::default() });
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
        self.fns.iter().enumerate().map(|(i, func)| (FunctionId(i), func))
    }

    pub fn types(&self) -> impl Iterator<Item = (UserTypeId, &Scoped<UserType>)> {
        self.types.iter().enumerate().map(|(i, ut)| (UserTypeId(i), ut))
    }

    pub fn traits(&self) -> impl Iterator<Item = (TraitId, &Scoped<Trait>)> {
        self.traits.iter().enumerate().map(|(i, ut)| (TraitId(i), ut))
    }

    pub fn is_child_of(&self, child: ScopeId, parent: ScopeId) -> bool {
        self.walk(child).any(|(id, _)| id == parent)
    }

    pub fn get_option_id(&self) -> Option<UserTypeId> {
        self.lang_types.get(&LangType::Option).copied()
    }

    pub fn get<T: ItemId>(&self, id: T) -> &Scoped<T::Value> {
        id.get(self)
    }

    pub fn get_mut<T: ItemId>(&mut self, id: T) -> &mut Scoped<T::Value> {
        id.get_mut(self)
    }

    pub fn find_tuple(&self, names: &[StrId]) -> Option<UserTypeId> {
        self.tuples.get(names).copied()
    }

    pub fn walk_super_trait_ids(&self, tr: TraitId) -> HashSet<TraitId> {
        fn inner(this: &Scopes, tr: TraitId, results: &mut HashSet<TraitId>) {
            if !results.insert(tr) {
                return;
            }

            for tr in this.get(tr).super_traits.iter_checked() {
                inner(this, tr.id, results);
            }
        }

        let mut result = HashSet::new();
        inner(self, tr, &mut result);
        result
    }

    pub fn walk_super_traits(&self, types: &Types, tr: GenericTrait) -> HashSet<GenericTrait> {
        fn inner(
            this: &Scopes,
            types: &Types,
            tr: GenericTrait,
            results: &mut HashSet<GenericTrait>,
        ) {
            if !results.insert(tr.clone()) {
                return;
            }

            for imp in this.get(tr.id).super_traits.iter_checked() {
                inner(this, types, imp.with_templates(types, &tr.ty_args), results);
            }
        }

        let mut result = HashSet::new();
        inner(self, types, tr, &mut result);
        result
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
