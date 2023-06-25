use std::collections::HashMap;

use crate::{
    ast::{stmt::{Stmt, Struct, Type, UserType}, expr::Expr},
    lexer::Located,
};

pub struct Member {
    pub public: bool,
    pub name: String,
    pub ty: Option<TypeId>,
}

pub struct ResolvedStruct {
    pub members: Vec<Member>,
}

pub enum ResolvedType {
    Void,
    Never,
    Int(u8),
    Uint(u8),
    F32,
    F64,
    IntGeneric,
    FloatGeneric,
    Struct(ResolvedStruct),
    Union {
        tag: Option<Option<TypeId>>,
        base: ResolvedStruct,
    },
    Enum {},
    Interface {},
    Function {},
}

pub type ScopeId = usize;
pub type TypeId = usize;

pub struct Scope {
    parent: Option<ScopeId>,
    name: String,
    public: bool,
    types: HashMap<String, TypeId>,
}

pub struct Resolver {
    types: Vec<ResolvedType>,
    scopes: Vec<Scope>,
    current: ScopeId,
}

impl Resolver {
    pub fn new() -> Self {
        let mut this = Self {
            scopes: vec![Scope {
                parent: None,
                name: "std".into(),
                public: true,
                types: Default::default(),
            }],
            types: vec![],
            // we depend on parser wrapping up the generated code in a Stmt::Module
            current: 0,
        };

        this.insert_type("void".into(), ResolvedType::Void);
        this.insert_type("i32".into(), ResolvedType::Int(32));

        this
    }

    pub fn accept(mut self, stmt: &Located<Stmt>) {
        self.resolve(stmt)
    }

    // 

    fn resolve(&mut self, stmt: &Located<Stmt>) {
        match &stmt.data {
            Stmt::Module { public, name, body } => {
                self.enter_scope(name.clone(), *public, |this| {
                    for stmt in body {
                        this.resolve(stmt);
                    }
                });
            }
            Stmt::UserType(ty) => {
                self.resolve_usertype(ty);
            }
            Stmt::Fn(f) => {
                self.enter_scope(f.header.name.clone(), f.header.public, |this| {
                    for stmt in &f.body {
                        this.resolve(stmt);
                    }
                });
            }
            _ => {}
        }
    }

    fn resolve_usertype(&mut self, ty: &UserType) -> TypeId {
        match ty {
            UserType::Struct(base) => {
                let data = ResolvedType::Struct(self.resolve_struct(base));
                self.insert_type(base.name.clone(), data);
            }
            UserType::Union { tag, base } => {
                let data = ResolvedType::Union {
                    tag: tag.as_ref().map(|ty| self.find_type(ty)),
                    base: self.resolve_struct(base),
                };
                self.insert_type(base.name.clone(), data);
            }
            UserType::Interface {
                public,
                name,
                type_params,
                impls,
                functions,
            } => todo!(),
            UserType::Enum {
                public,
                name,
                impls,
                variants,
                functions,
            } => todo!(),
        }

        todo!()
    }

    fn resolve_struct(&mut self, Struct { members, .. }: &Struct) -> ResolvedStruct {
        ResolvedStruct {
            members: members
                .iter()
                .map(|m| Member {
                    public: m.public,
                    name: m.name.clone(),
                    ty: self.resolve_type(&m.ty),
                })
                .collect(),
        }
    }

    fn resolve_type(&mut self, ty: &Type) -> Option<TypeId> {
        match ty {
            Type::Regular { name, .. } => self.find_type(name),
            Type::Array(_, _) => self.find_type_in_scope("Array", 0),
            Type::Slice(_) => self.find_type_in_scope("Slice", 0),
            Type::Tuple(_) => todo!(),
            Type::Map(_, _) => self.find_type_in_scope("Map", 0),
            Type::Option(_) => self.find_type_in_scope("Option", 0),
            Type::Result(_, _) => self.find_type_in_scope("Result", 0),
            Type::Anon(ty) => Some(self.resolve_usertype(ty)),
            Type::Void => self.find_type_in_scope("void", 0),
            Type::Ref(_) | Type::RefMut(_) | Type::This => unreachable!(),
        }
    }

    // 

    fn find_type(&self, name: &str) -> Option<TypeId> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.types.get(name).copied())
            .next()
    }

    fn find_type_in_scope(&self, name: &str, scope: ScopeId) -> Option<TypeId> {
        self.scopes[scope].types.get(name).copied()
    }

    fn insert_type(&mut self, name: String, data: ResolvedType) -> TypeId {
        let id = self.types.len();
        self.types.push(data);
        self.scopes[self.current].types.insert(name, id);
        id
    }

    fn enter_scope(&mut self, name: String, public: bool, f: impl FnOnce(&mut Self)) {
        self.scopes.push(Scope {
            parent: Some(self.current),
            name,
            public,
            types: Default::default(),
        });

        self.current += 1;
        f(self);
        self.current -= 1;
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Checker {
    resolver: Resolver,
}

impl Checker {
    pub fn check_stmt(&mut self, stmt: &Located<Stmt>) -> TypeId {
        match &stmt.data {
            Stmt::Expr(expr) => self.check_expr(expr),
            Stmt::Let { name, ty, mutable, value } => todo!(),
            Stmt::Fn(_) => todo!(),
            Stmt::UserType(_) => todo!(),
            Stmt::Static { public, name, ty, value } => todo!(),
            Stmt::Module { public, name, body } => todo!(),
        }
    }

    pub fn check_expr(&mut self, expr: &Located<Expr>) -> TypeId {
        match &expr.data {
            Expr::Binary { op, left, right } => todo!(),
            Expr::Unary { op, expr } => todo!(),
            Expr::Call { callee, args } => todo!(),
            Expr::Array(_) => todo!(),
            Expr::ArrayWithInit { init, count } => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::Map(_) => todo!(),
            Expr::Bool(_) => todo!(),
            Expr::Integer(_, _) => todo!(),
            Expr::Float(_) => todo!(),
            Expr::String(_) => todo!(),
            Expr::Symbol(_) => todo!(),
            Expr::Instance { name, members } => todo!(),
            Expr::None => todo!(),
            Expr::Assign { target, binary, value } => todo!(),
            Expr::Block(_) => todo!(),
            Expr::If { cond, if_branch, else_branch } => todo!(),
            Expr::Loop { cond, body, do_while } => todo!(),
            Expr::For { var, iter, body } => todo!(),
            Expr::Member { source, member } => todo!(),
            Expr::Subscript { callee, args } => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Yield(_) => todo!(),
            Expr::Break(_) => todo!(),
            Expr::Range { start, end, inclusive } => todo!(),
            Expr::Continue => todo!(),
        }
    }
}


