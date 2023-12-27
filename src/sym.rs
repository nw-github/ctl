use derive_more::{Constructor, Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use indexmap::IndexSet;
use std::collections::HashMap;

use crate::{
    ast::{
        checked::{CheckedExpr, CheckedStmt},
        parsed::{Expr, Path},
        Attribute,
    },
    lexer::{Located, Span},
    typeid::{GenericFunc, GenericUserType, Type},
    Error,
};

macro_rules! id {
    ($name: ident => $output: ident,
     $vec: ident,
     $($parts:ident).+,
     $suffix: ident) => {
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

            fn find(scopes: &Scopes, name: &str) -> Option<Vis<Self>> {
                for (id, scope) in scopes.iter() {
                    if let Some(item) = Self::find_in(scopes, name, id) {
                        return Some(item);
                    }

                    if matches!(scope.kind, ScopeKind::Module(_, _)) {
                        break;
                    }
                }

                None
            }

            fn find_in(scopes: &Scopes, name: &str, scope: ScopeId) -> Option<Vis<Self>> {
                scopes[scope].$vec
                    .iter()
                    .rev()
                    .find_map(|id| (scopes.$vec[id.0].$($parts).+ == name).then_some(*id))
            }

            fn insert(scopes: &mut Scopes, value: Self::Value, public: bool) -> Self {
                Self::insert_in(scopes, value, public, scopes.current)
            }

            fn insert_in(scopes: &mut Scopes, value: Self::Value, public: bool, scope: ScopeId) -> Self {
                let index = scopes.$vec.len();
                scopes.$vec.push(Scoped::new(value, scope));
                let id = $name(index);
                scopes[scope].$vec.insert(Vis { id, public });
                id
            }
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ScopeId(pub usize);

id!(FunctionId => Function, fns, name.data, func);
id!(UserTypeId => UserType, types, name, user_type);
id!(VariableId => Variable, vars, name, var);
id!(ExtensionId => Extension, exts, name, ext);

#[derive(Debug, Clone)]
pub struct UnresolvedUse {
    pub public: bool,
    pub path: Path,
    pub all: bool,
    pub span: Span,
}

#[derive(Default, Debug, Clone, EnumAsInner)]
pub enum ScopeKind {
    Block(Option<Type>, bool),
    Loop(Option<Type>, bool),
    Lambda(Option<Type>, bool),
    Function(FunctionId),
    UserType(UserTypeId),
    Module(String, Vec<UnresolvedUse>),
    Impl(UserTypeId),
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
            &ScopeKind::UserType(id) => Some(&scopes.get(id).name),
            ScopeKind::Module(name, _) => Some(name),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DefaultExpr {
    Unchecked(ScopeId, Expr),
    Checked(CheckedExpr),
}

#[derive(Debug, Clone)]
pub struct CheckedParam {
    pub mutable: bool,
    pub keyword: bool,
    pub name: String,
    pub ty: Type,
    pub default: Option<DefaultExpr>,
}

#[derive(Default, Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: Type,
    pub is_static: bool,
    pub mutable: bool,
    pub value: Option<CheckedExpr>,
}

#[derive(Debug)]
pub struct Function {
    pub attrs: Vec<Attribute>,
    pub name: Located<String>,
    pub is_async: bool,
    pub is_extern: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    pub type_params: Vec<UserTypeId>,
    pub params: Vec<CheckedParam>,
    pub ret: Type,
    pub body: Option<Vec<CheckedStmt>>,
    pub constructor: bool,
    pub body_scope: ScopeId,
    pub returns: bool,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub name: String,
    pub shared: bool,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub variants: Vec<Member>,
    pub is_unsafe: bool,
}

impl Union {
    pub fn tag_type(&self) -> Type {
        Type::discriminant_for(self.variants.len())
    }

    pub fn variant_tag(&self, name: &str) -> Option<usize> {
        self.variants
            .iter()
            .filter(|m| !m.shared)
            .position(|m| m.name == name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TT {
    Struct,
    Func,
}

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct {
        members: Vec<Member>,
        init: FunctionId,
    },
    Union(Union),
    Enum(Type),
    Template(TT, usize),
    Trait,
}

impl UserTypeData {
    pub fn as_func_template(&self) -> Option<&usize> {
        if let Some((TT::Func, i)) = self.as_template() {
            Some(i)
        } else {
            None
        }
    }

    pub fn as_struct_template(&self) -> Option<&usize> {
        if let Some((TT::Struct, i)) = self.as_template() {
            Some(i)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct UserType {
    pub name: String,
    pub body_scope: ScopeId,
    pub data: UserTypeData,
    pub impls: Vec<Type>,
    pub type_params: Vec<UserTypeId>,
}

impl UserType {
    pub fn members(&self) -> Option<&[Member]> {
        match &self.data {
            UserTypeData::Struct { members, .. } => Some(members),
            UserTypeData::Union(union) => Some(&union.variants),
            _ => None,
        }
    }

    pub fn members_mut(&mut self) -> Option<&mut [Member]> {
        match &mut self.data {
            UserTypeData::Struct { members, .. } => Some(members),
            UserTypeData::Union(union) => Some(&mut union.variants),
            _ => None,
        }
    }
}

pub trait HasImplsAndTypeParams {
    fn get_type_params(&self) -> &[UserTypeId];
    fn get_impls(&self) -> &Vec<Type>;
    fn get_impls_mut(&mut self) -> &mut Vec<Type>;
}

impl HasImplsAndTypeParams for UserType {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }

    fn get_impls(&self) -> &Vec<Type> {
        &self.impls
    }

    fn get_impls_mut(&mut self) -> &mut Vec<Type> {
        &mut self.impls
    }
}

impl HasImplsAndTypeParams for Extension {
    fn get_type_params(&self) -> &[UserTypeId] {
        &self.type_params
    }

    fn get_impls(&self) -> &Vec<Type> {
        &self.impls
    }

    fn get_impls_mut(&mut self) -> &mut Vec<Type> {
        &mut self.impls
    }
}

#[derive(Debug)]
pub struct Extension {
    pub ty: Type,
    pub name: String,
    pub impls: Vec<Type>,
    pub type_params: Vec<UserTypeId>,
    pub body_scope: ScopeId,
}

impl Extension {
    pub fn matches_type(&self, ty: &Type) -> bool {
        // TODO: templates
        &self.ty == ty
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

impl<T: std::hash::Hash> std::hash::Hash for Vis<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Vis<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(other)
    }
}

impl<T: Eq> Eq for Vis<T> {}

pub trait ItemId: Sized {
    type Value;

    fn get(self, scopes: &Scopes) -> &Scoped<Self::Value>;
    fn get_mut(self, scopes: &mut Scopes) -> &mut Scoped<Self::Value>;

    fn find(scopes: &Scopes, name: &str) -> Option<Vis<Self>>;
    fn find_in(scopes: &Scopes, name: &str, scope: ScopeId) -> Option<Vis<Self>>;

    fn insert(scopes: &mut Scopes, value: Self::Value, public: bool) -> Self;
    fn insert_in(scopes: &mut Scopes, value: Self::Value, public: bool, scope: ScopeId) -> Self;
}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub fns: IndexSet<Vis<FunctionId>>,
    pub types: IndexSet<Vis<UserTypeId>>,
    pub vars: IndexSet<Vis<VariableId>>,
    pub exts: IndexSet<Vis<ExtensionId>>,
    pub children: IndexSet<Vis<ScopeId>>,
}

pub struct Scopes {
    scopes: Vec<Scope>,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
    exts: Vec<Scoped<Extension>>,
    pub current: ScopeId,
    pub lang_types: HashMap<String, UserTypeId>,
    pub intrinsics: HashMap<FunctionId, String>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            current: ScopeId(0),
            fns: Vec::new(),
            types: Vec::new(),
            vars: Vec::new(),
            exts: Vec::new(),
            lang_types: HashMap::new(),
            intrinsics: HashMap::new(),
        }
    }

    pub fn iter_from(&self, id: ScopeId) -> impl Iterator<Item = (ScopeId, &Scope)> {
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

    pub fn iter(&self) -> impl Iterator<Item = (ScopeId, &Scope)> {
        self.iter_from(self.current)
    }

    pub fn full_name(&self, id: ScopeId, ident: &str) -> String {
        let mut name: String = ident.chars().rev().collect();
        for scope_name in self
            .iter_from(id)
            .flat_map(|(_, scope)| scope.kind.name(self))
        {
            name.reserve(scope_name.len() + 1);
            name.push('_');
            for c in scope_name.chars().rev() {
                name.push(c);
            }
        }

        name.chars().rev().collect::<String>()
    }

    pub fn current(&mut self) -> &mut Scope {
        let i = self.current;
        &mut self[i]
    }

    pub fn enter<T>(&mut self, kind: ScopeKind, public: bool, f: impl FnOnce(&mut Self) -> T) -> T {
        let id = ScopeId(self.scopes.len());
        self.current().children.insert(Vis { id, public });
        let parent = Some(self.current);
        self.enter_id(id, |this| {
            this.scopes.push(Scope {
                parent,
                kind,
                ..Default::default()
            });

            f(this)
        })
    }

    pub fn enter_id<T>(&mut self, id: ScopeId, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.current;
        self.current = id;
        let result = f(self);
        self.current = prev;
        result
    }

    pub fn this_type_of(&self, id: UserTypeId) -> Type {
        Type::UserType(
            GenericUserType::new(
                id,
                self.get(id)
                    .type_params
                    .iter()
                    .map(|&id| Type::UserType(GenericUserType::new(id, vec![]).into()))
                    .collect(),
            )
            .into(),
        )
    }

    pub fn current_this_type(&self) -> Option<Type> {
        self.iter().find_map(|(_, scope)| {
            match scope.kind {
                ScopeKind::UserType(id) => {
                    let ty = self.get(id);
                    if !ty.data.is_template() {
                        if ty.data.is_trait() {
                            return Some(Type::TraitSelf);
                        }

                        return Some(self.this_type_of(id));
                    }
                }
                ScopeKind::Extension(id) => {
                    return Some(self.get(id).ty.clone());
                }
                _ => {}
            }
            None
        })
    }

    pub fn current_function(&self) -> Option<FunctionId> {
        self.function_of(self.current)
    }

    pub fn function_of(&self, scope: ScopeId) -> Option<FunctionId> {
        self.iter_from(scope)
            .find_map(|(_, scope)| scope.kind.as_function().copied())
    }

    pub fn module_of(&self, id: ScopeId) -> Option<ScopeId> {
        for (id, current) in self.iter_from(id) {
            if matches!(current.kind, ScopeKind::Module(_, _)) {
                return Some(id);
            }
        }

        None
    }

    pub fn unresolved_use_stmts(&mut self, id: ScopeId) -> Option<&mut Vec<UnresolvedUse>> {
        self.module_of(id)
            .and_then(|id| self[id].kind.as_module_mut().map(|m| m.1))
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn extensions(&self) -> &[Scoped<Extension>] {
        &self.exts
    }

    pub fn find_module_in(&self, name: &str, scope: ScopeId) -> Option<Vis<ScopeId>> {
        self[scope]
            .children
            .iter()
            .find(|&&id| matches!(&self[*id].kind, ScopeKind::Module(mn, _) if name == mn))
            .copied()
    }

    pub fn find_module(&self, name: &str) -> Option<Vis<ScopeId>> {
        for (id, scope) in self.iter() {
            if let Some(item) = self.find_module_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_, _)) {
                break;
            }
        }

        None
    }

    pub fn find_free_fn(&self, name: &str) -> Option<Vis<FunctionId>> {
        for (id, scope) in self
            .iter()
            .filter(|(_, s)| !matches!(s.kind, ScopeKind::UserType(_)))
        {
            if let Some(item) = self.find_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_, _)) {
                break;
            }
        }

        None
    }

    pub fn get_option_id(&self) -> Option<UserTypeId> {
        self.lang_types.get("option").copied()
    }

    pub fn as_option_inner<'a>(&self, ty: &'a Type) -> Option<&'a Type> {
        self.get_option_id().and_then(|opt| {
            ty.as_user_type()
                .filter(|ut| ut.id == opt)
                .map(|ut| &ut.ty_args[0])
        })
    }

    pub fn make_lang_type(&self, name: &str, ty_args: Vec<Type>) -> Option<Type> {
        Some(Type::UserType(
            GenericUserType::new(self.lang_types.get(name).copied()?, ty_args).into(),
        ))
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

    pub fn find<T: ItemId>(&self, name: &str) -> Option<Vis<T>> {
        T::find(self, name)
    }

    pub fn find_in<T: ItemId>(&self, name: &str, scope: ScopeId) -> Option<Vis<T>> {
        T::find_in(self, name, scope)
    }

    pub fn insert<T: ItemId>(&mut self, value: T::Value, public: bool) -> T {
        T::insert(self, value, public)
    }

    pub fn extensions_in_scope_for<'a, 'b>(
        &'a self,
        ty: &'b Type,
    ) -> impl Iterator<Item = &Scoped<Extension>> + 'b
    where
        'a: 'b,
    {
        self.iter().flat_map(|(_, scope)| {
            scope
                .exts
                .iter()
                .map(|ext| self.get(ext.id))
                .filter(|ext| ext.matches_type(ty))
        })
    }

    pub fn can_access_privates(&self, scope: ScopeId) -> bool {
        let target = self
            .module_of(scope)
            .expect("root scope passed to can_access_privates()");
        self.iter().any(|(id, _)| id == target)
    }
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
