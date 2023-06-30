use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use crate::{typecheck::TypeId, checked_ast::expr::CheckedExpr};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ScopeId(usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct FunctionId(ScopeId, usize);

impl FunctionId {
    pub fn scope(&self) -> ScopeId {
        self.0
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct StructId(ScopeId, usize);

impl StructId {
    pub fn scope(&self) -> ScopeId {
        self.0
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Block(Option<TypeId>, bool),
    Loop(Option<TypeId>, bool),
    Function(FunctionId),
    Struct(StructId),
    #[default]
    Module,
}

#[derive(Default, Debug, Clone)]
pub struct Variable {
    pub ty: TypeId,
    pub is_static: bool,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: TypeId,
    pub kw: bool,
    pub default: Option<CheckedExpr>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: TypeId,
    pub inst: Option<StructId>,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub ty: TypeId,
}

#[derive(Debug)]
pub struct DefinedStruct {
    pub name: String,
    pub members: HashMap<String, Member>,
    pub scope: ScopeId,
}

#[derive(Debug)]
pub enum Struct {
    Declared(String),
    Defined(DefinedStruct),
}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    parent: Option<ScopeId>,
    vars: HashMap<String, Variable>,
    fns: Vec<Function>,
    types: Vec<Struct>,
    name: Option<String>,
}

impl Scope {
    pub fn find_fn(&self, name: &str) -> Option<&Function> {
        self.fns.iter().find(|f| f.name == name)
    }

    pub fn insert_fn(&mut self,f: Function) {
        self.fns.push(f);
    } 
}

pub struct Scopes {
    scopes: Vec<Scope>,
    current: ScopeId,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            current: ScopeId(0),
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

    pub fn scope_full_name(&self, id: ScopeId) -> String {
        let mut name = String::new();
        for (_, scope) in self.iter_from(id) {
            if let Some(scope_name) = &scope.name {
                for c in scope_name.chars().rev() {
                    name.push(c);
                }
                name.push('_');
            }
        }

        name.chars().rev().skip(1).collect::<String>()
    }

    pub fn find_struct(&self, target: &str) -> Option<StructId> {
        for (id, scope) in self.iter() {
            if let Some(index) = scope.types.iter().enumerate().find_map(|(i, s)| match s {
                Struct::Defined(d) if d.name == target => Some(i),
                Struct::Declared(name) if name == target => Some(i),
                _ => None,
            }) {
                return Some(StructId(id, index));
            }

            if scope.kind == ScopeKind::Module {
                break;
            }
        }

        None
    }

    pub fn find_var(&self, name: &str) -> Option<(ScopeId, &Variable)> {
        for (id, scope) in self.iter() {
            if let Some(var) = scope.vars.get(name) {
                return Some((id, var));
            }

            if scope.kind == ScopeKind::Module {
                break;
            }
        }

        None
    }

    pub fn find_fn(&self, name: &str) -> Option<FunctionId> {
        for (id, scope) in self.iter() {
            if let Some(var) = scope.fns.iter().position(|f| f.name == name) {
                return Some(FunctionId(id, var));
            }

            if scope.kind == ScopeKind::Module {
                break;
            }
        }

        None
    }

    pub fn is_sub_scope(&self, target: ScopeId) -> bool {
        self.iter().any(|(id, _)| id == target)
    }

    pub fn current(&mut self) -> &mut Scope {
        let i = self.current;
        &mut self[i]
    }

    pub fn current_id(&self) -> ScopeId {
        self.current
    }

    pub fn enter<T>(
        &mut self,
        name: Option<String>,
        kind: ScopeKind,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.scopes.push(Scope {
            parent: Some(self.current),
            kind,
            name,
            ..Default::default()
        });

        let prev = self.current;
        self.current = ScopeId(self.scopes.len() - 1);
        let result = f(self);
        self.current = prev;
        result
    }

    pub fn insert_type(&mut self, ty: Struct) -> StructId {
        let index = self.current().types.len();
        self.current().types.push(ty);
        StructId(self.current_id(), index)
    }

    pub fn insert_fn(&mut self, ty: Function) -> FunctionId {
        let index = self.current().fns.len();
        self.current().fns.push(ty);
        FunctionId(self.current_id(), index)
    }

    pub fn insert_var(&mut self, name: String, var: Variable) {
        self.current().vars.insert(name, var);
    }

    pub fn current_struct(&self) -> Option<TypeId> {
        self
            .iter()
            .find_map(|(_, scope)| {
                if let ScopeKind::Struct(id) = &scope.kind {
                    Some(TypeId::Struct((*id).into()))
                } else {
                    None
                }
            })
    }
}

impl Index<&FunctionId> for Scopes {
    type Output = Function;

    fn index(&self, index: &FunctionId) -> &Self::Output {
        &self[index.0].fns[index.1]
    }
}

impl IndexMut<&FunctionId> for Scopes {
    fn index_mut(&mut self, index: &FunctionId) -> &mut Self::Output {
        &mut self[index.0].fns[index.1]
    }
}

impl Index<&StructId> for Scopes {
    type Output = Struct;

    fn index(&self, index: &StructId) -> &Self::Output {
        &self[index.0].types[index.1]
    }
}

impl IndexMut<&StructId> for Scopes {
    fn index_mut(&mut self, index: &StructId) -> &mut Self::Output {
        &mut self[index.0].types[index.1]
    }
}

impl Index<ScopeId> for Scopes {
    type Output = Scope;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index.0]
    }
}

impl IndexMut<ScopeId> for Scopes {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scopes[index.0]
    }
}

impl Default for Scopes {
    fn default() -> Self {
        Self::new()
    }
}
