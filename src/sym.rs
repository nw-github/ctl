use derive_more::{Constructor, Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use std::collections::HashMap;

use crate::{
    ast::{
        checked::{CheckedExpr, CheckedPattern, CheckedStmt},
        parsed::{Expr, Linkage, Pattern, UsePath},
        Attribute,
    },
    lexer::Located,
    typeid::{GenericUserType, Type},
};

macro_rules! id {
    ($name: ident => $output: ident,
     $vec: ident,
     $namespace: ident,
     $($parts:ident).+) => {
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

            fn insert_in(scopes: &mut Scopes, value: Self::Value, public: bool, scope: ScopeId) -> (Self, bool) {
                let index = scopes.$vec.len();
                scopes.$vec.push(Scoped::new(value, scope));
                let id = $name(index);
                let name = (scopes.$vec[id.0].$($parts).+).clone();
                let res = scopes[scope].$namespace.insert(name, Vis { id: id.into(), public });
                (id, res.is_some())
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

id!(FunctionId => Function, fns, vns, name.data);
id!(UserTypeId => UserType, types, tns, name.data);
id!(VariableId => Variable, vars, vns, name);
id!(ExtensionId => Extension, exts, tns, name);

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
    Module(String),
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
            &ScopeKind::UserType(id) => Some(&scopes.get(id).name.data),
            &ScopeKind::Extension(id) => Some(&scopes.get(id).name),
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

#[derive(Debug, Clone)]
pub struct CheckedParam {
    pub keyword: bool,
    pub label: String,
    pub patt: ParamPattern,
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

#[derive(Debug, Clone)]
pub struct CheckedMember {
    pub public: bool,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub variants: IndexMap<String, Type>,
    pub is_unsafe: bool,
}

impl Union {
    pub fn tag_type(&self) -> Type {
        Type::discriminant_for(self.variants.len())
    }

    pub fn variant_tag(&self, name: &str) -> Option<usize> {
        self.variants.get_index_of(name)
    }
}

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct,
    Union(Union),
    Template,
    Trait,
    Tuple,
}

#[derive(Debug)]
pub struct UserType {
    pub name: Located<String>,
    pub body_scope: ScopeId,
    pub data: UserTypeData,
    pub impls: Vec<Type>,
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

pub trait HasImplsAndTypeParams: HasTypeParams {
    fn get_impls(&self) -> &Vec<Type>;
    fn get_impls_mut(&mut self) -> &mut Vec<Type>;
}

impl HasImplsAndTypeParams for UserType {
    fn get_impls(&self) -> &Vec<Type> {
        &self.impls
    }

    fn get_impls_mut(&mut self) -> &mut Vec<Type> {
        &mut self.impls
    }
}

impl HasImplsAndTypeParams for Extension {
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
    pub fns: Vec<Vis<FunctionId>>,
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

pub trait ItemId: Sized {
    type Value;

    fn get(self, scopes: &Scopes) -> &Scoped<Self::Value>;
    fn get_mut(self, scopes: &mut Scopes) -> &mut Scoped<Self::Value>;
    fn insert_in(
        scopes: &mut Scopes,
        value: Self::Value,
        public: bool,
        scope: ScopeId,
    ) -> (Self, bool);
}

#[derive(Debug, Clone, Copy, EnumAsInner, From)]
pub enum TypeItem {
    Type(UserTypeId),
    Extension(ExtensionId),
    Module(ScopeId),
}

#[derive(Debug, Clone, Copy, EnumAsInner, From)]
pub enum ValueItem {
    Fn(FunctionId),
    Var(VariableId),
}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub tns: HashMap<String, Vis<TypeItem>>,
    pub vns: HashMap<String, Vis<ValueItem>>,
    pub children: Vec<ScopeId>,
    pub use_stmts: Vec<UsePath>,
}

impl Scope {
    pub fn find_in_tns(&self, name: &str) -> Option<Vis<TypeItem>> {
        self.tns.get(name).copied()
    }

    pub fn find_in_vns(&self, name: &str) -> Option<Vis<ValueItem>> {
        self.vns.get(name).copied()
    }

    pub fn find_module(&self, name: &str) -> Option<ScopeId> {
        self.find_in_tns(name)
            .and_then(|inner| inner.as_module().copied())
    }
}

pub struct Scopes {
    scopes: Vec<Scope>,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
    exts: Vec<Scoped<Extension>>,
    pub lang_types: HashMap<String, UserTypeId>,
    pub intrinsics: HashMap<FunctionId, String>,
    pub panic_handler: Option<FunctionId>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            fns: vec![Scoped::new(Function::default(), ScopeId::ROOT)],
            types: Vec::new(),
            vars: Vec::new(),
            exts: Vec::new(),
            lang_types: HashMap::new(),
            intrinsics: HashMap::new(),
            panic_handler: None,
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
        bound: &GenericUserType,
        excl: Option<ExtensionId>,
        scope: ScopeId,
    ) -> bool {
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
                    .flat_map(|bound| bound.as_user())
                {
                    if !self.implements_trait(rhs, bound, Some(id), scope) {
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
