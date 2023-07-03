use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use concat_idents::concat_idents;
use derive_more::{Constructor, Deref, DerefMut};
use enum_as_inner::EnumAsInner;

use crate::{
    checked_ast::{expr::CheckedExpr, Block},
    typecheck::TypeId,
};

macro_rules! id {
    ($name: ident => $output: ident,
     $vec: ident,
     $($parts:ident).+,
     $suffix: ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        pub struct $name(usize);

        impl Scopes {
            concat_idents!(fn_name = find_, $suffix, _in {
                pub fn fn_name(&self, name: &str, scope: ScopeId) -> Option<$name> {
                    self[scope].$vec
                        .iter()
                        .find_map(|id| (self.$vec[id.0].$($parts).+ == name).then_some(*id))
                }
            });

            concat_idents!(fn_name = find_, $suffix {
                pub fn fn_name(&self, name: &str) -> Option<$name> {
                    for (id, scope) in self.iter() {
                        concat_idents!(fn_name = find_, $suffix, _in {
                            if let Some(item) = self.fn_name(name, id) {
                                return Some(item);
                            }
                        });

                        if matches!(scope.kind, ScopeKind::Module(_)) {
                            break;
                        }
                    }

                    None
                }
            });

            concat_idents!(fn_name = insert_, $suffix, _in {
                pub fn fn_name(&mut self, item: $output, scope: ScopeId) -> $name {
                    let index = self.$vec.len();
                    self.$vec.push(Scoped::new(item, scope));
                    let itemid = $name(index);
                    self[scope].$vec.push(itemid);
                    itemid
                }
            });

            concat_idents!(fn_name = insert_, $suffix {
                pub fn fn_name(&mut self, item: $output) -> $name {
                    concat_idents!(fn_name = insert_, $suffix, _in {
                        self.fn_name(item, self.current_id())
                    })
                }
            });

            concat_idents!(fn_name = get_, $suffix {
                pub fn fn_name(&self, id: $name) -> &Scoped<$output> {
                    &self.$vec[id.0]
                }
            });

            concat_idents!(fn_name = get_, $suffix, _mut {
                #[allow(dead_code)]
                pub fn fn_name(&mut self, id: $name) -> &mut Scoped<$output> {
                    &mut self.$vec[id.0]
                }
            });
        }
    };
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ScopeId(pub usize);

id!(FunctionId => Function, fns, proto.name, func);
id!(UserTypeId => UserType, types, name, user_type);
id!(VariableId => Variable, vars, name, var);

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum ScopeKind {
    Block(Option<TypeId>, bool),
    Loop(Option<TypeId>, bool),
    Function(FunctionId),
    UserType(UserTypeId),
    Module(bool),
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

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct(Vec<(String, Member)>),
    GenericParam,
}

#[derive(Debug)]
pub struct UserType {
    pub public: bool,
    pub name: String,
    pub body_scope: ScopeId,
    pub data: UserTypeData,
}

#[derive(Debug, Clone, Copy)]
pub enum Symbol {
    Function(FunctionId),
    Variable(VariableId),
}

#[derive(Deref, DerefMut, Constructor)]
pub struct Scoped<T> {
    #[deref]
    #[deref_mut]
    pub item: T,
    pub scope: ScopeId,
}

#[derive(Default, Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    pub fns: Vec<FunctionId>,
    pub types: Vec<UserTypeId>,
    pub vars: Vec<VariableId>,
    pub name: Option<String>,
    pub children: HashMap<String, ScopeId>,
}

pub struct Scopes {
    scopes: Vec<Scope>,
    current: ScopeId,
    fns: Vec<Scoped<Function>>,
    types: Vec<Scoped<UserType>>,
    vars: Vec<Scoped<Variable>>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            current: ScopeId(0),
            fns: Vec::new(),
            types: Vec::new(),
            vars: Vec::new(),
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
        let id = ScopeId(self.scopes.len());
        // blocks are the only unnamed scopes
        self.current()
            .children
            .insert(name.clone().unwrap_or(String::new()), id);
        let parent = Some(self.current);
        self.enter_id(id, |this| {
            this.scopes.push(Scope {
                parent,
                kind,
                name,
                ..Default::default()
            });

            f(this)
        })
    }

    pub fn find_enter<T>(&mut self, name: &str, f: impl FnOnce(&mut Self) -> T) -> T {
        let id = *self.current().children.get(name).unwrap();
        self.enter_id(id, f)
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
            if let ScopeKind::UserType(id) = scope.kind {
                if self.get_user_type(id).data.is_struct() {
                    return Some(TypeId::UserType(id));
                }
            }
            None
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
            if matches!(current.kind, ScopeKind::Module(_)) {
                return Some(id);
            }
        }

        None
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
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
