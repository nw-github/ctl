use std::collections::HashMap;

use derive_more::{Deref, DerefMut};

use crate::typecheck::TypeId;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Block(Option<TypeId>),
    Function(usize),
    UserType(usize),
    #[default]
    Module,
}

#[derive(Default, Debug, Clone)]
pub struct Variable {
    pub ty: TypeId,
    pub mutable: bool,
}

#[derive(Default, Debug)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub vars: HashMap<String, Variable>,
    pub types: HashMap<String, TypeId>,
    pub kind: ScopeKind,
    pub name: Option<String>,
}

pub type ScopeId = usize;

#[derive(Default, Deref, DerefMut)]
pub struct Scopes {
    #[deref]
    #[deref_mut]
    scopes: Vec<Scope>,
}

impl Scopes {
    pub fn iter_from(&self, id: ScopeId) -> impl Iterator<Item = (ScopeId, &Scope)> {
        pub struct ScopeIter<'a> {
            scope: &'a Scopes,
            next: Option<usize>,
        }

        impl<'a> Iterator for ScopeIter<'a> {
            type Item = (ScopeId, &'a Scope);

            fn next(&mut self) -> Option<Self::Item> {
                self.next.map(|i| {
                    self.next = self.scope.scopes[i].parent;
                    (i, &self.scope.scopes[i])
                })
            }
        }

        ScopeIter {
            scope: self,
            next: Some(id),
        }
    }

    pub fn name(&self, id: ScopeId) -> String {
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

    pub fn find_type(&self, id: ScopeId, name: &str) -> Option<&TypeId> {
        for (_, scope) in self.iter_from(id) {
            if let Some(ty) = scope.types.get(name) {
                return Some(ty);
            }

            if scope.kind == ScopeKind::Module {
                break;
            }
        }

        None
    }

    pub fn find_var(&self, id: ScopeId, name: &str) -> Option<(ScopeId, &Variable)> {
        for (id, scope) in self.iter_from(id) {
            if let Some(var) = scope.vars.get(name) {
                return Some((id, var));
            }

            if scope.kind == ScopeKind::Module {
                break;
            }
        }

        None
    }

    pub fn is_sub_scope(&self, id: ScopeId, target: ScopeId) -> bool {
        self.iter_from(id).any(|(id, _)| id == target)
    }
}
