use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use crate::{
    ast::Path,
    checked_ast::{
        expr::{CheckedExpr, Symbol},
        Block,
    },
    typecheck::TypeId,
};

macro_rules! id {
    ($name: ident => $output: ident,
     $vec: ident,
     $find: ident,
     $($parts:ident).+,
     $insert: ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(pub ScopeId, pub usize);

        impl $name {
            pub fn scope(&self) -> ScopeId {
                self.0
            }
        }

        impl Index<&$name> for Scopes {
            type Output = $output;

            fn index(&self, index: &$name) -> &Self::Output {
                &self[index.0].$vec[index.1]
            }
        }

        impl IndexMut<&$name> for Scopes {
            fn index_mut(&mut self, index: &$name) -> &mut Self::Output {
                &mut self[index.0].$vec[index.1]
            }
        }

        impl Scope {
            pub fn $find(&self, name: &str) -> Option<(usize, &$output)> {
                self.$vec
                    .iter()
                    .enumerate()
                    .find(|(_, item)| item.$($parts).+ == name )
            }
        }

        impl Scopes {
            pub fn $find(&self, name: &str) -> Option<$name> {
                for (id, scope) in self.iter() {
                    if let Some((index, _)) = scope.$find(name) {
                        return Some($name(id, index));
                    }

                    if scope.kind == ScopeKind::Module {
                        break;
                    }
                }

                None
            }

            pub fn $insert(&mut self, item: $output) -> $name {
                let index = self.current().$vec.len();
                self.current().$vec.push(item);
                $name(self.current_id(), index)
            }
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ScopeId(pub usize);

id!(FunctionId => Function, fns, find_fn, proto.name, insert_fn);
id!(StructId => Struct, types, find_struct, name, insert_struct);
id!(VariableId => Variable, vars, find_var, name, insert_var);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Block(Option<TypeId>, bool),
    Loop(Option<TypeId>, bool),
    Function(FunctionId),
    Struct(StructId),
    Module,
    #[default]
    None,
}

#[derive(Debug, Clone)]
pub struct CheckedParam {
    pub mutable: bool,
    pub keyword: bool,
    pub name: String,
    pub ty: TypeId,
    //pub default: Option<CheckedExpr>,
}

#[derive(Debug, Clone)]
pub struct CheckedPrototype {
    pub public: bool,
    pub name: String,
    pub is_async: bool,
    pub is_extern: bool,
    pub type_params: Vec<String>,
    pub params: Vec<CheckedParam>,
    pub ret: TypeId,
}

#[derive(Default, Debug, Clone)]
pub struct Variable {
    pub public: bool,
    pub name: String,
    pub ty: TypeId,
    pub is_static: bool,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct Function {
    pub proto: CheckedPrototype,
    pub body: Option<Block>,
    pub inst: bool,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub ty: TypeId,
}

#[derive(Debug)]
pub struct StructDef {
    pub members: Vec<(String, Member)>,
    pub scope: ScopeId,
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub def: Option<StructDef>,
}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub fns: Vec<Function>,
    pub types: Vec<Struct>,
    parent: Option<ScopeId>,
    vars: Vec<Variable>,
    name: Option<String>,
    children: HashMap<String, ScopeId>,
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

    pub fn full_name(&self, id: ScopeId, ident: &str) -> String {
        let mut name: String = ident.chars().rev().collect();
        for scope_name in self.iter_from(id).flat_map(|scope| scope.1.name.as_ref()) {
            name.reserve(scope_name.len() + 1);
            name.push('_');
            for c in scope_name.chars().rev() {
                name.push(c);
            }
        }

        name.chars().rev().collect::<String>()
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
        let prev = self.current;
        let id = ScopeId(self.scopes.len());
        // blocks are the only unnamed scopes
        self.current()
            .children
            .insert(name.clone().unwrap_or(String::new()), id);
        self.scopes.push(Scope {
            parent: Some(self.current),
            kind,
            name,
            ..Default::default()
        });

        self.current = ScopeId(self.scopes.len() - 1);
        let result = f(self);
        self.current = prev;
        result
    }

    pub fn find_enter<T>(&mut self, name: &str, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.current;
        self.current = *self.current().children.get(name).unwrap();
        let result = f(self);
        self.current = prev;
        result
    }

    pub fn enter_id<T>(&mut self, id: ScopeId, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = self.current;
        self.current = id;
        let result = f(self);
        self.current = prev;
        result
    }

    pub fn current_struct(&self) -> Option<TypeId> {
        self.iter().find_map(|(_, scope)| {
            if let ScopeKind::Struct(id) = &scope.kind {
                Some(TypeId::Struct((*id).into()))
            } else {
                None
            }
        })
    }

    pub fn current_function(&self) -> Option<FunctionId> {
        self.iter().find_map(|(_, scope)| {
            if let ScopeKind::Function(id) = &scope.kind {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn module_of(&self, id: ScopeId) -> Option<ScopeId> {
        for (id, current) in self.iter_from(id) {
            if current.kind == ScopeKind::Module {
                return Some(id);
            }
        }

        None
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn resolve_path(&self, path: &Path) -> Option<Symbol> {
        // TODO: generic params
        if let Some(name) = path.as_symbol() {
            self.find_var(name)
                .map(Symbol::Variable)
                .or_else(|| self.find_fn(name).map(Symbol::Function))
        } else {
            let mut scope = ScopeId(0);
            let mut start = 0;
            if !path.root {
                if path.data[0].0 == "super" {
                    start = 1;
                    if let Some(module) =
                        self.module_of(self[self.module_of(self.current).unwrap()].parent.unwrap())
                    {
                        scope = module;
                    } else {
                        todo!("super at top level")
                    }
                } else {
                    scope = self.module_of(self.current).unwrap();
                }
            }

            for part in path.data[start..path.data.len() - 1].iter() {
                if let Some(found) = self[scope].children.iter().find(|scope| {
                    scope.0 == &part.0
                        && matches!(
                            self[*scope.1].kind,
                            ScopeKind::Module | ScopeKind::Struct(_)
                        )
                }) {
                    scope = *found.1;
                }
            }

            let last = path.data.last().unwrap();
            self[scope]
                .find_var(&last.0)
                .map(|var| Symbol::Variable(VariableId(scope, var.0)))
                .or_else(|| {
                    self[scope]
                        .find_fn(&last.0)
                        .map(|func| Symbol::Function(FunctionId(scope, func.0)))
                })
        }
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
