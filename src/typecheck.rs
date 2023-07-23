use core::panic;
use std::path::PathBuf;

use concat_idents::concat_idents;
use derive_more::{Constructor, Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use indexmap::{map::Entry, IndexMap, IndexSet};

use crate::{
    ast::{
        expr::{BinaryOp, Expr, UnaryOp},
        stmt::{Fn, Param, ParsedUserType, Prototype, Stmt, TypeHint},
        Path, Pattern,
    },
    checked_ast::{Block, CheckedExpr, CheckedStmt, ExprData, UnionPattern},
    lexer::{Located, Span},
    parser::ParsedFile,
    Error, Pipeline, THIS_PARAM, THIS_TYPE,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash, Constructor)]
pub struct GenericFunc {
    pub id: FunctionId,
    pub generics: Vec<TypeId>,
}

impl GenericFunc {
    fn infer_generics(&mut self, mut src: &TypeId, mut target: &TypeId, scopes: &Scopes) {
        loop {
            match (src, target) {
                (TypeId::Ptr(gi), TypeId::Ptr(ti)) => {
                    src = gi;
                    target = ti;
                }
                (TypeId::MutPtr(gi), TypeId::MutPtr(ti)) => {
                    src = gi;
                    target = ti;
                }
                (TypeId::Array(gi), TypeId::Array(ti)) => {
                    src = &gi.0;
                    target = &ti.0;
                }
                (TypeId::UserType(src), target) => {
                    if let Some(t) = target.as_user_type() {
                        if src.id != t.id {
                            if let Some(inner) = scopes
                                .as_option_inner(target)
                                .and_then(|i| i.as_user_type())
                            {
                                for (src, target) in src.generics.iter().zip(inner.generics.iter())
                                {
                                    self.infer_generics(src, target, scopes);
                                }

                                break;
                            }
                        }

                        if !src.generics.is_empty() && !t.generics.is_empty() {
                            for (src, target) in src.generics.iter().zip(t.generics.iter()) {
                                self.infer_generics(src, target, scopes);
                            }

                            break;
                        }
                    }

                    if let Some(&index) = scopes.get_user_type(src.id).data.as_func_generic() {
                        if self.generics[index].is_unknown() {
                            self.generics[index] = target.clone();
                        }
                    }

                    break;
                }
                _ => break,
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, Constructor)]
pub struct GenericUserType {
    pub id: UserTypeId,
    pub generics: Vec<TypeId>,
}

impl GenericUserType {
    pub fn name(&self, scopes: &Scopes) -> String {
        let mut result = scopes.get_user_type(self.id).name.clone();
        if !self.generics.is_empty() {
            result.push('<');
            for (i, concrete) in self.generics.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&concrete.name(scopes));
            }
            result.push('>');
        }

        result
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum CInt {
    Char,
    Short,
    Int,
    Long,
    LongLong,
}

pub struct IntStats {
    bits: u8,
    signed: bool,
}

#[derive(Debug, Default, PartialEq, Eq, Clone, EnumAsInner, Hash)]
pub enum TypeId {
    #[default]
    Unknown,
    Void,
    Never,
    Int(u8),
    Uint(u8),
    CInt(CInt),
    CUint(CInt),
    CVoid,
    Isize,
    Usize,
    F32,
    F64,
    Bool,
    IntGeneric,
    FloatGeneric,
    Char,
    Func(Box<GenericFunc>),
    UserType(Box<GenericUserType>),
    Ptr(Box<TypeId>),
    MutPtr(Box<TypeId>),
    Array(Box<(TypeId, usize)>),
    TraitSelf,
}

impl TypeId {
    pub fn supports_binop(&self, op: BinaryOp) -> bool {
        match op {
            BinaryOp::Add => matches!(
                self,
                TypeId::Int(_)
                    | TypeId::Isize
                    | TypeId::Uint(_)
                    | TypeId::Usize
                    | TypeId::F32
                    | TypeId::F64
                    | TypeId::CInt(_)
                    | TypeId::CUint(_)
            ),
            BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Rem
            | BinaryOp::Gt
            | BinaryOp::GtEqual
            | BinaryOp::Lt
            | BinaryOp::LtEqual => {
                matches!(
                    self,
                    TypeId::Int(_)
                        | TypeId::Isize
                        | TypeId::Uint(_)
                        | TypeId::Usize
                        | TypeId::F32
                        | TypeId::F64
                        | TypeId::CInt(_)
                        | TypeId::CUint(_)
                )
            }
            BinaryOp::And | BinaryOp::Xor | BinaryOp::Or | BinaryOp::Shl | BinaryOp::Shr => {
                matches!(
                    self,
                    TypeId::Int(_)
                        | TypeId::Uint(_)
                        | TypeId::Isize
                        | TypeId::Usize
                        | TypeId::CInt(_)
                        | TypeId::CUint(_)
                )
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                matches!(
                    self,
                    TypeId::Int(_)
                        | TypeId::Isize
                        | TypeId::Uint(_)
                        | TypeId::Usize
                        | TypeId::F32
                        | TypeId::F64
                        | TypeId::Bool // FIXME: option<T> should be comparable with T without coercion
                        | TypeId::CInt(_)
                        | TypeId::CUint(_)
                        | TypeId::Char
                )
            }
            BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                matches!(self, TypeId::Bool)
            }
            BinaryOp::NoneCoalesce => todo!(),
            BinaryOp::ErrCoalesce => todo!(),
        }
    }

    pub fn strip_references(&self) -> &TypeId {
        let mut id = self;
        while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = id {
            id = inner;
        }
        id
    }

    pub fn fill_type_generics(&mut self, scopes: &Scopes, inst: &GenericUserType) {
        if inst.generics.is_empty() {
            return;
        }

        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    if !ty.generics.is_empty() {
                        for ty in ty.generics.iter_mut() {
                            ty.fill_type_generics(scopes, inst);
                        }
                    } else if let Some(&index) =
                        scopes.get_user_type(ty.id).data.as_struct_generic()
                    {
                        *src = inst.generics[index].clone();
                    }

                    break;
                }
                _ => break,
            }
        }
    }

    pub fn fill_func_generics(&mut self, scopes: &Scopes, func: &GenericFunc) {
        if func.generics.is_empty() {
            return;
        }

        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    if !ty.generics.is_empty() {
                        for ty in ty.generics.iter_mut() {
                            ty.fill_func_generics(scopes, func);
                        }
                    } else if let Some(&index) = scopes.get_user_type(ty.id).data.as_func_generic()
                    {
                        if !func.generics[index].is_unknown() {
                            *src = func.generics[index].clone();
                        }
                    }

                    break;
                }
                _ => break,
            }
        }
    }

    pub fn fill_this(&mut self, this: &TypeId) {
        let mut src = self;
        loop {
            match src {
                TypeId::Array(t) => src = &mut t.0,
                TypeId::Ptr(t) | TypeId::MutPtr(t) => src = t,
                TypeId::UserType(ty) => {
                    for ty in ty.generics.iter_mut() {
                        ty.fill_this(this);
                    }

                    break;
                }
                TypeId::TraitSelf => {
                    *src = this.clone();
                    break;
                }
                _ => break,
            }
        }
    }

    pub fn name(&self, scopes: &Scopes) -> String {
        match self {
            TypeId::Void => "void".into(),
            TypeId::Never => "never".into(),
            TypeId::Int(bits) => format!("i{bits}"),
            TypeId::Uint(bits) => format!("u{bits}"),
            TypeId::Unknown => "{unknown}".into(),
            TypeId::F32 => "f32".into(),
            TypeId::F64 => "f64".into(),
            TypeId::Bool => "bool".into(),
            TypeId::IntGeneric => "{integer}".into(),
            TypeId::FloatGeneric => "{float}".into(),
            TypeId::Char => "char".into(),
            TypeId::Ptr(id) => format!("*{}", id.name(scopes)),
            TypeId::MutPtr(id) => format!("*mut {}", id.name(scopes)),
            TypeId::Func(func) => {
                let f = scopes.get_func(func.id);

                let mut result = format!("fn {}", f.proto.name);
                if !func.generics.is_empty() {
                    result.push('<');
                    for (i, (param, concrete)) in f
                        .proto
                        .type_params
                        .iter()
                        .zip(func.generics.iter())
                        .enumerate()
                    {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(&format!("{param} = {}", concrete.name(scopes)));
                    }
                    result.push('>');
                }

                result.push('(');
                for (i, param) in f.proto.params.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }

                    result.push_str(&format!("{}: {}", param.name, param.ty.name(scopes)));
                }
                format!("{result}) {}", f.proto.ret.name(scopes))
            }
            TypeId::UserType(ty) => ty.name(scopes),
            TypeId::Array(inner) => format!("[{}; {}]", inner.0.name(scopes), inner.1),
            TypeId::Isize => "isize".into(),
            TypeId::Usize => "usize".into(),
            TypeId::CInt(ty) | TypeId::CUint(ty) => {
                let name = match ty {
                    crate::typecheck::CInt::Char => "char",
                    crate::typecheck::CInt::Short => "short",
                    crate::typecheck::CInt::Int => "int",
                    crate::typecheck::CInt::Long => "long",
                    crate::typecheck::CInt::LongLong => "longlong",
                };
                format!(
                    "c_{}{name}",
                    if matches!(self, TypeId::CUint(_)) {
                        "u"
                    } else {
                        ""
                    }
                )
            }
            TypeId::CVoid => "c_void".into(),
            TypeId::TraitSelf => "This".into(),
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            TypeId::Int(_)
                | TypeId::Uint(_)
                | TypeId::F32
                | TypeId::F64
                | TypeId::Isize
                | TypeId::Usize
                | TypeId::CInt(_)
                | TypeId::CUint(_)
        )
    }

    pub fn is_void_like(&self) -> bool {
        match self {
            TypeId::Void | TypeId::Never | TypeId::CVoid => true,
            //TypeId::Array(arr) => arr.1 == 0,
            _ => false,
        }
    }

    pub fn integer_stats(&self) -> Option<IntStats> {
        use std::ffi::*;

        let (bytes, signed) = match self {
            TypeId::Int(bits) | TypeId::Uint(bits) => (*bits / 8, matches!(self, TypeId::Int(_))),
            TypeId::CInt(cint) | TypeId::CUint(cint) => {
                let bits = match cint {
                    CInt::Char => std::mem::size_of::<c_char>(),
                    CInt::Short => std::mem::size_of::<c_short>(),
                    CInt::Int => std::mem::size_of::<c_int>(),
                    CInt::Long => std::mem::size_of::<c_long>(),
                    CInt::LongLong => std::mem::size_of::<c_longlong>(),
                };
                (bits as u8, matches!(self, TypeId::CInt(_)))
            }
            TypeId::Isize => (std::mem::size_of::<isize>() as u8, true),
            TypeId::Usize => (std::mem::size_of::<usize>() as u8, false),
            _ => return None,
        };

        Some(IntStats {
            bits: bytes * 8,
            signed,
        })
    }

    fn implements_trait(&self, scopes: &Scopes, bound: &GenericUserType) -> bool {
        if let Some(this) = self.as_user_type() {
            for mut tr in scopes.get_user_type(this.id).impls.iter().cloned() {
                for ut in tr.generics.iter_mut() {
                    ut.fill_type_generics(scopes, this);
                }

                if &tr == bound {
                    return true;
                }
            }
        }

        false
    }

    fn coerces_to(&self, scopes: &Scopes, target: &TypeId) -> bool {
        match (self, target) {
            (
                TypeId::IntGeneric,
                TypeId::Int(_)
                | TypeId::Uint(_)
                | TypeId::Isize
                | TypeId::Usize
                | TypeId::CInt(_)
                | TypeId::CUint(_),
            ) => true,
            (TypeId::FloatGeneric, TypeId::F32 | TypeId::F64) => true,
            (TypeId::MutPtr(ty), TypeId::Ptr(target)) if ty == target => true,
            (ty, target)
                if scopes
                    .as_option_inner(target)
                    .map_or(false, |inner| ty.coerces_to(scopes, inner)) =>
            {
                true
            }
            (TypeId::Never, _) => true,
            (ty, target) => ty == target,
        }
    }

    fn from_int_name(name: &str) -> Option<TypeId> {
        let mut chars = name.chars();
        let mut i = false;
        let result = match chars.next()? {
            'i' => {
                i = true;
                TypeId::Int
            }
            'u' => TypeId::Uint,
            _ => return None,
        };

        match (
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next().and_then(|c| c.to_digit(10)),
            chars.next(),
        ) {
            (Some(a), None, None, None) => (!i || a > 1).then_some(result(a as u8)),
            (Some(a), Some(b), None, None) => Some(result((a * 10 + b) as u8)),
            (Some(a), Some(b), Some(c), None) => Some(result((a * 100 + b * 10 + c) as u8)),
            _ => match name {
                "usize" => Some(TypeId::Usize),
                "isize" => Some(TypeId::Isize),
                _ => None,
            },
        }
    }
}

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
                        .rev()
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
                    self[scope].$vec.insert(itemid);
                    itemid
                }
            });

            concat_idents!(fn_name = insert_, $suffix {
                #[allow(dead_code)]
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

#[derive(Default, Debug, Clone, PartialEq, Eq, EnumAsInner)]
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
    pub value: Option<CheckedExpr>,
}

#[derive(Debug)]
pub struct Function {
    pub proto: CheckedPrototype,
    pub body: Option<Vec<CheckedStmt>>,
    pub constructor: bool,
    pub body_scope: ScopeId,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub shared: bool,
    pub ty: TypeId,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub variants: Vec<(String, Member)>,
}

impl Union {
    pub fn tag_type(&self) -> TypeId {
        TypeId::Uint(8)
    }

    pub fn variant_tag(&self, name: &str) -> Option<usize> {
        self.variants
            .iter()
            .filter(|m| !m.1.shared)
            .position(|(n, _)| name == n)
    }
}

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct {
        members: Vec<(String, Member)>,
        init: FunctionId,
    },
    Union(Union),
    Enum,
    FuncGeneric(usize),
    StructGeneric(usize),
    Trait,
}

impl UserTypeData {
    pub fn members(&self) -> Option<&[(String, Member)]> {
        match self {
            UserTypeData::Struct { members, .. } => Some(members),
            UserTypeData::Union(union) => Some(&union.variants),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct UserType {
    pub public: bool,
    pub name: String,
    pub body_scope: ScopeId,
    pub data: UserTypeData,
    pub impls: IndexSet<GenericUserType>,
    pub type_params: usize,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Symbol {
    Func,
    Var(VariableId),
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
    pub fns: IndexSet<FunctionId>,
    pub types: IndexSet<UserTypeId>,
    pub vars: IndexSet<VariableId>,
    pub name: Option<String>,
    pub children: IndexMap<String, ScopeId>,
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

    pub fn this_type(&self) -> Option<TypeId> {
        self.iter().find_map(|(_, scope)| {
            if let ScopeKind::UserType(id) = scope.kind {
                let ty = self.get_user_type(id);
                if !ty.data.is_func_generic() && !ty.data.is_struct_generic() {
                    if ty.data.is_trait() {
                        return Some(TypeId::TraitSelf);
                    }

                    return Some(TypeId::UserType(
                        GenericUserType::new(
                            id,
                            self[ty.body_scope]
                                .types
                                .iter()
                                .filter(|&&id| self.get_user_type(id).data.is_struct_generic())
                                .map(|&id| {
                                    TypeId::UserType(
                                        GenericUserType {
                                            id,
                                            generics: vec![],
                                        }
                                        .into(),
                                    )
                                })
                                .collect(),
                        )
                        .into(),
                    ));
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

    pub fn find_module_in(&self, name: &str, scope: ScopeId) -> Option<ScopeId> {
        self[scope]
            .children
            .get(name)
            .filter(|&&id| self[id].kind.is_module())
            .copied()
    }

    pub fn find_module(&self, name: &str) -> Option<ScopeId> {
        for (id, scope) in self.iter() {
            if let Some(item) = self.find_module_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        None
    }

    pub fn find_nonmember_fn(&self, name: &str) -> Option<FunctionId> {
        for (id, scope) in self
            .iter()
            .filter(|(_, s)| !matches!(s.kind, ScopeKind::UserType(_)))
        {
            if let Some(item) = self.find_func_in(name, id) {
                return Some(item);
            }

            if matches!(scope.kind, ScopeKind::Module(_)) {
                break;
            }
        }

        None
    }

    fn find_core_option(&self) -> Option<UserTypeId> {
        let core = self.scopes()[0].children.get("core")?;
        let option = self[*core].children.get("option")?;
        self.find_user_type_in("Option", *option)
    }

    fn find_core_string(&self) -> Option<TypeId> {
        let core = self.scopes[0].children.get("core")?;
        let option = self[*core].children.get("string")?;
        Some(TypeId::UserType(
            GenericUserType {
                id: self.find_user_type_in("str", *option)?,
                generics: vec![],
            }
            .into(),
        ))
    }

    fn as_option_inner<'a>(&self, ty: &'a TypeId) -> Option<&'a TypeId> {
        self.find_core_option().and_then(|opt| {
            ty.as_user_type()
                .filter(|ut| ut.id == opt)
                .map(|ut| &ut.generics[0])
        })
    }

    fn make_option(&self, ty: TypeId) -> Option<TypeId> {
        Some(TypeId::UserType(
            GenericUserType {
                id: self.find_core_option()?,
                generics: vec![ty],
            }
            .into(),
        ))
    }

    fn resolve_type(&self, ty: &TypeHint) -> Result<TypeId, Error> {
        Ok(match ty {
            TypeHint::Regular(path) => {
                return match self.resolve_path(&path.data, path.span)? {
                    ResolvedPath::UserType(ut) => Ok(TypeId::UserType(ut.into())),
                    ResolvedPath::Func(_) => {
                        Err(Error::new("expected type, got function", path.span))
                    }
                    ResolvedPath::Var(_) => {
                        Err(Error::new("expected type, got variable", path.span))
                    }
                    ResolvedPath::Module(_) => {
                        Err(Error::new("expected type, got module", path.span))
                    }
                    ResolvedPath::None(err) => path
                        .data
                        .as_identifier()
                        .and_then(|symbol| match symbol {
                            symbol if symbol == THIS_TYPE => self.this_type(),
                            "void" => Some(TypeId::Void),
                            "never" => Some(TypeId::Never),
                            "f32" => Some(TypeId::F32),
                            "f64" => Some(TypeId::F64),
                            "bool" => Some(TypeId::Bool),
                            "str" => self.find_core_string(),
                            "char" => Some(TypeId::Char),
                            "c_void" => Some(TypeId::CVoid),
                            "c_char" => Some(TypeId::CInt(CInt::Char)),
                            "c_short" => Some(TypeId::CInt(CInt::Short)),
                            "c_int" => Some(TypeId::CInt(CInt::Int)),
                            "c_long" => Some(TypeId::CInt(CInt::Long)),
                            "c_longlong" => Some(TypeId::CInt(CInt::LongLong)),
                            "c_uchar" => Some(TypeId::CUint(CInt::Char)),
                            "c_ushort" => Some(TypeId::CUint(CInt::Short)),
                            "c_uint" => Some(TypeId::CUint(CInt::Int)),
                            "c_ulong" => Some(TypeId::CUint(CInt::Long)),
                            "c_ulonglong" => Some(TypeId::CUint(CInt::LongLong)),
                            _ => TypeId::from_int_name(symbol),
                        })
                        .ok_or_else(|| Error::new(err, path.span)),
                };
            }
            TypeHint::Void => TypeId::Void,
            TypeHint::Ptr(ty) => TypeId::Ptr(self.resolve_type(ty)?.into()),
            TypeHint::MutPtr(ty) => TypeId::MutPtr(self.resolve_type(ty)?.into()),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                self.this_type()
                    .map(|s| TypeId::Ptr(s.into()))
                    .expect("ICE: this outside of method")
            }
            TypeHint::MutThis => self
                .this_type()
                .map(|s| TypeId::MutPtr(s.into()))
                .expect("ICE: this outside of method"),
            TypeHint::Array(ty, count) => {
                let n = TypeChecker::consteval(self, count, Some(&TypeId::Usize))?;
                TypeId::Array((self.resolve_type(ty)?, n).into())
            }
            TypeHint::Option(ty) => self.make_option(self.resolve_type(ty)?).ok_or(Error::new(
                "core::option::Option not found",
                Span::default(),
            ))?,
            _ => todo!(),
        })
    }

    fn resolve_use(&mut self, path: &Path, span: Span) -> Result<(), Error> {
        match self.resolve_path(path, span)? {
            ResolvedPath::UserType(ut) => {
                self.current().types.insert(ut.id);
            }
            ResolvedPath::Func(func) => {
                self.current().fns.insert(func.id);
            }
            ResolvedPath::Var(id) => {
                self.current().vars.insert(id);
            }
            ResolvedPath::Module(id) => {
                let name = self[id].name.as_ref().unwrap().clone();
                self.current().children.insert(name, id);
            }
            ResolvedPath::None(err) => return Err(Error::new(err, span)),
        }

        Ok(())
    }

    fn resolve_impl(&self, path: &Path, span: Span) -> Result<GenericUserType, Error> {
        match self.resolve_path(path, span)? {
            ResolvedPath::UserType(ty) if self.get_user_type(ty.id).data.is_trait() => Ok(ty),
            ResolvedPath::None(err) => Err(Error::new(err, span)),
            _ => Err(Error::new("expected trait", span)),
        }
    }

    fn resolve_path(&self, path: &Path, span: Span) -> Result<ResolvedPath, Error> {
        match path {
            Path::Root(data) => self.resolve_path_from(data, ScopeId(0), span),
            Path::Super(data) => {
                if let Some(module) = self.module_of(
                    self[self.module_of(self.current_id()).unwrap()]
                        .parent
                        .unwrap(),
                ) {
                    self.resolve_path_from(data, module, span)
                } else {
                    Err(Error::new("cannot use super here", span))
                }
            }
            Path::Normal(data) => {
                let (name, generics) = data.first().unwrap();
                let is_end = data.len() == 1;
                if let Some(id) = self.find_user_type(name) {
                    let ty = self.get_user_type(id);
                    if is_end {
                        return Ok(ResolvedPath::UserType(GenericUserType::new(
                            id,
                            self.resolve_generics(ty.type_params, generics, span)?,
                        )));
                    }

                    self.resolve_path_from(&data[1..], ty.body_scope, span)
                } else if let Some(id) = self.find_nonmember_fn(name) {
                    if is_end {
                        let f = self.get_func(id);
                        return Ok(ResolvedPath::Func(GenericFunc::new(
                            id,
                            self.resolve_generics(f.proto.type_params.len(), generics, span)?,
                        )));
                    }

                    Err(Error::new(format!("'{name}' is a function"), span))
                } else if let Some(id) = self.find_var(name) {
                    if is_end {
                        return Ok(ResolvedPath::Var(id));
                    }

                    if !generics.is_empty() {
                        return Err(Error::new(
                            "variables cannot be parameterized with generics",
                            span,
                        ));
                    }

                    Err(Error::new(format!("'{name}' is a variable"), span))
                } else if let Some(id) = self.find_module(name) {
                    if is_end {
                        return Ok(ResolvedPath::Module(id));
                    }

                    if !generics.is_empty() {
                        return Err(Error::new(
                            "modules cannot be parameterized with generics",
                            span,
                        ));
                    }

                    self.resolve_path_from(&data[1..], id, span)
                } else {
                    self.resolve_path_from(data, ScopeId(0), span)
                }
            }
        }
    }

    fn resolve_path_from(
        &self,
        data: &[(String, Vec<TypeHint>)],
        mut scope: ScopeId,
        span: Span,
    ) -> Result<ResolvedPath, Error> {
        for (i, (name, generics)) in data.iter().enumerate() {
            let is_end = i + 1 == data.len();
            if let Some(id) = self.find_user_type_in(name, scope) {
                let ty = self.get_user_type(id);
                if !ty.public && self.module_of(ty.scope) != self.module_of(self.current_id()) {
                    return Err(Error::new(format!("type '{name}' is private"), span));
                }

                if is_end {
                    return Ok(ResolvedPath::UserType(GenericUserType::new(
                        id,
                        self.resolve_generics(ty.type_params, generics, span)?,
                    )));
                }

                scope = ty.body_scope;
            } else if let Some(id) = self.find_func_in(name, scope) {
                let func = self.get_func(id);
                if !func.proto.public {
                    return Err(Error::new(format!("function '{name}' is private"), span));
                }

                if is_end {
                    return Ok(ResolvedPath::Func(GenericFunc::new(
                        id,
                        self.resolve_generics(func.proto.type_params.len(), generics, span)?,
                    )));
                }

                return Err(Error::new(format!("'{name}' is a function"), span));
            } else if let Some(id) = self.find_var_in(name, scope) {
                if !self.get_var(id).public {
                    return Err(Error::new(format!("variable '{name}' is private"), span));
                }

                if !generics.is_empty() {
                    return Err(Error::new(
                        "variables cannot be parameterized with generics",
                        span,
                    ));
                }

                if is_end {
                    return Ok(ResolvedPath::Var(id));
                }

                return Err(Error::new(format!("'{name}' is a variable"), span));
            } else if let Some(id) = self.find_module_in(name, scope) {
                if self.is_private_mod(id, *self[id].kind.as_module().unwrap()) {
                    return Err(Error::new(
                        format!("no symbol '{name}' in this module"),
                        span,
                    ));
                }

                if !generics.is_empty() {
                    return Err(Error::new(
                        "modules cannot be parameterized with generics",
                        span,
                    ));
                }

                if is_end {
                    return Ok(ResolvedPath::Module(id));
                }

                scope = id;
            } else {
                return Ok(ResolvedPath::None(format!(
                    "no symbol '{name}' found in this module"
                )));
            }
        }

        unreachable!()
    }

    fn resolve_generics(
        &self,
        params: usize,
        args: &[TypeHint],
        span: Span,
    ) -> Result<Vec<TypeId>, Error> {
        if args.is_empty() {
            Ok(vec![TypeId::Unknown; params])
        } else if args.len() != params {
            Err(Error::new(
                format!(
                    "expected {} generic arguments, received {}",
                    params,
                    args.len()
                ),
                span,
            ))
        } else {
            let mut generics = Vec::new();
            for ty in args.iter() {
                generics.push(self.resolve_type(ty)?);
            }

            Ok(generics)
        }
    }

    fn is_private_mod(&self, scope: ScopeId, public: bool) -> bool {
        !public && self.module_of(self.current_id()) != self.module_of(self[scope].parent.unwrap())
    }

    fn forward_declare(&mut self, stmt: &Located<Stmt>) {
        match &stmt.data {
            Stmt::Module { public, name, body } => {
                self.enter(Some(name.clone()), ScopeKind::Module(*public), |scopes| {
                    for stmt in body {
                        scopes.forward_declare(stmt)
                    }
                });
            }
            Stmt::UserType(data) => match data {
                ParsedUserType::Struct(base) => {
                    let mut params = Vec::with_capacity(base.members.len());
                    let mut members = Vec::with_capacity(base.members.len());
                    for (name, member) in base.members.iter() {
                        members.push((
                            name.clone(),
                            Member {
                                public: member.public,
                                shared: member.shared,
                                ty: self
                                    .resolve_type(&member.ty)
                                    .unwrap_or(TypeId::Unknown)
                                    .clone(),
                            },
                        ));

                        params.push(Param {
                            mutable: false,
                            keyword: true,
                            name: name.clone(),
                            ty: member.ty.clone(),
                            default: None,
                        });
                    }

                    let id = self.insert_user_type(UserType {
                        name: base.name.data.clone(),
                        public: base.public,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Struct {
                            members,
                            init: FunctionId(0),
                        },
                        type_params: base.type_params.len(),
                        impls: Default::default(),
                    });
                    let parent = self.current_id();
                    self.enter(
                        Some(base.name.data.clone()),
                        ScopeKind::UserType(id),
                        |scopes| {
                            scopes.get_user_type_mut(id).body_scope = scopes.current_id();

                            for (i, (name, impls)) in base.type_params.iter().enumerate() {
                                scopes.insert_user_type(UserType {
                                    public: false,
                                    name: name.clone(),
                                    body_scope: scopes.current_id(),
                                    data: UserTypeData::StructGeneric(i),
                                    type_params: 0,
                                    impls: impls
                                        .iter()
                                        .flat_map(|path| scopes.resolve_impl(&path.data, path.span))
                                        .collect(),
                                });
                            }

                            for path in base.impls.iter() {
                                if let Ok(ty) = scopes.resolve_impl(&path.data, path.span) {
                                    scopes.get_user_type_mut(id).impls.insert(ty);
                                }
                            }

                            *scopes.get_user_type_mut(id).data.as_struct_mut().unwrap().1 = scopes
                                .forward_declare_proto(
                                    parent,
                                    true,
                                    &Prototype {
                                        public: base.public,
                                        name: base.name.data.clone(),
                                        is_async: false,
                                        is_extern: false,
                                        type_params: base.type_params.clone(),
                                        params,
                                        ret: TypeHint::Regular(Located::new(
                                            Path::Normal(vec![(
                                                base.name.data.clone(),
                                                base.type_params
                                                    .iter()
                                                    .map(|(name, _)| {
                                                        TypeHint::Regular(Located::new(
                                                            Path::from(name.clone()),
                                                            stmt.span,
                                                        ))
                                                    })
                                                    .collect(),
                                            )]),
                                            stmt.span,
                                        )),
                                    },
                                );

                            for f in base.functions.iter() {
                                scopes.forward_declare_fn(scopes.current_id(), false, f);
                            }
                        },
                    )
                }
                ParsedUserType::Union { tag: _, base } => {
                    let mut variants = Vec::with_capacity(base.members.len());
                    let mut shared = vec![];
                    for (name, member) in base.members.iter() {
                        variants.push((
                            name.clone(),
                            Member {
                                public: member.public,
                                shared: member.shared,
                                ty: self
                                    .resolve_type(&member.ty)
                                    .unwrap_or(TypeId::Unknown)
                                    .clone(),
                            },
                        ));

                        if member.shared {
                            shared.push(Param {
                                mutable: false,
                                keyword: true,
                                name: name.clone(),
                                ty: member.ty.clone(),
                                default: None,
                            });
                        }
                    }

                    let id = self.insert_user_type(UserType {
                        name: base.name.data.clone(),
                        public: base.public,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Union(Union { variants }),
                        type_params: base.type_params.len(),
                        impls: Default::default(),
                    });
                    self.enter(
                        Some(base.name.data.clone()),
                        ScopeKind::UserType(id),
                        |scopes| {
                            scopes.get_user_type_mut(id).body_scope = scopes.current_id();

                            for (i, (name, impls)) in base.type_params.iter().enumerate() {
                                scopes.insert_user_type(UserType {
                                    public: false,
                                    name: name.clone(),
                                    body_scope: scopes.current_id(),
                                    data: UserTypeData::StructGeneric(i),
                                    type_params: 0,
                                    impls: impls
                                        .iter()
                                        .flat_map(|path| scopes.resolve_impl(&path.data, path.span))
                                        .collect(),
                                });
                            }

                            for path in base.impls.iter() {
                                if let Ok(ty) = scopes.resolve_impl(&path.data, path.span) {
                                    scopes.get_user_type_mut(id).impls.insert(ty);
                                }
                            }

                            for (name, member) in base.members.iter() {
                                let mut params = shared.clone();
                                if !matches!(member.ty, TypeHint::Void) {
                                    params.push(Param {
                                        mutable: false,
                                        keyword: false,
                                        name: name.clone(),
                                        ty: member.ty.clone(),
                                        default: None,
                                    });
                                }
                                // TODO: generic params
                                scopes.forward_declare_fn(
                                    scopes.current_id(),
                                    true,
                                    &Fn {
                                        proto: Prototype {
                                            public: true,
                                            name: name.clone(),
                                            is_async: false,
                                            is_extern: false,
                                            type_params: base.type_params.clone(),
                                            params,
                                            ret: TypeHint::Regular(Located::new(
                                                Path::Normal(vec![(
                                                    base.name.data.clone(),
                                                    base.type_params
                                                        .iter()
                                                        .map(|(name, _)| {
                                                            TypeHint::Regular(Located::new(
                                                                Path::from(name.clone()),
                                                                stmt.span,
                                                            ))
                                                        })
                                                        .collect(),
                                                )]),
                                                stmt.span,
                                            )),
                                        },
                                        body: Vec::new(),
                                    },
                                );
                            }

                            for f in base.functions.iter() {
                                scopes.forward_declare_fn(scopes.current_id(), false, f);
                            }
                        },
                    )
                }
                ParsedUserType::Trait {
                    public,
                    name,
                    type_params,
                    impls,
                    functions,
                } => {
                    let id = self.insert_user_type(UserType {
                        name: name.clone(),
                        public: *public,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Trait,
                        type_params: type_params.len(),
                        impls: Default::default(),
                    });
                    self.enter(Some(name.clone()), ScopeKind::UserType(id), |scopes| {
                        scopes.get_user_type_mut(id).body_scope = scopes.current_id();

                        for (i, (name, impls)) in type_params.iter().enumerate() {
                            scopes.insert_user_type(UserType {
                                public: false,
                                name: name.clone(),
                                body_scope: scopes.current_id(),
                                data: UserTypeData::StructGeneric(i),
                                type_params: 0,
                                impls: impls
                                    .iter()
                                    .flat_map(|path| scopes.resolve_impl(&path.data, path.span))
                                    .collect(),
                            });
                        }

                        for path in impls.iter() {
                            if let Ok(ty) = scopes.resolve_impl(&path.data, path.span) {
                                scopes.get_user_type_mut(id).impls.insert(ty);
                            }
                        }

                        for proto in functions.iter() {
                            scopes.forward_declare_proto(scopes.current_id(), false, proto);
                        }
                    })
                }
                ParsedUserType::Enum {
                    public,
                    name,
                    impls,
                    variants,
                    functions,
                } => {
                    let id = self.insert_user_type(UserType {
                        public: *public,
                        name: name.data.clone(),
                        body_scope: ScopeId(0),
                        data: UserTypeData::Enum,
                        type_params: 0,
                        impls: impls
                            .iter()
                            .flat_map(|path| self.resolve_impl(&path.data, path.span))
                            .collect(),
                    });

                    self.enter(Some(name.data.clone()), ScopeKind::UserType(id), |scopes| {
                        scopes.get_user_type_mut(id).body_scope = scopes.current_id();

                        for (name, _) in variants {
                            scopes.insert_var(Variable {
                                name: name.clone(),
                                public: true,
                                ty: TypeId::UserType(GenericUserType::new(id, vec![]).into()),
                                is_static: true,
                                mutable: false,
                                value: None,
                            });
                        }

                        for f in functions.iter() {
                            scopes.forward_declare_fn(scopes.current_id(), false, f);
                        }
                    });
                }
            },
            Stmt::Fn(f) => self.forward_declare_fn(self.current_id(), false, f),
            Stmt::Static { public, name, .. } => {
                self.insert_var(Variable {
                    name: name.clone(),
                    public: *public,
                    ty: TypeId::Unknown,
                    is_static: true,
                    mutable: false,
                    value: None,
                });
            }
            Stmt::Use(path) => {
                _ = self.resolve_use(path, stmt.span);
            }
            _ => {}
        }
    }

    fn forward_declare_proto(
        &mut self,
        scope: ScopeId,
        constructor: bool,
        proto: &Prototype,
    ) -> FunctionId {
        let checked = Function {
            proto: CheckedPrototype {
                public: proto.public,
                name: proto.name.clone(),
                is_async: proto.is_async,
                is_extern: proto.is_extern,
                type_params: proto.type_params.iter().map(|x| x.0.clone()).collect(),
                params: proto
                    .params
                    .iter()
                    .map(|param| CheckedParam {
                        mutable: param.mutable,
                        keyword: param.keyword,
                        name: param.name.clone(),
                        ty: self.resolve_type(&param.ty).unwrap_or(TypeId::Unknown),
                    })
                    .collect(),
                ret: self.resolve_type(&proto.ret).unwrap_or(TypeId::Unknown),
            },
            body: None,
            body_scope: ScopeId(0),
            constructor,
        };

        let id = self.insert_func_in(checked, scope);
        self.enter(
            Some(proto.name.clone()),
            ScopeKind::Function(id),
            |scopes| {
                scopes.get_func_mut(id).body_scope = scopes.current_id();

                if !proto.type_params.is_empty() {
                    for (i, (name, impls)) in proto.type_params.iter().enumerate() {
                        scopes.insert_user_type(UserType {
                            public: false,
                            name: name.clone(),
                            body_scope: scopes.current_id(),
                            data: UserTypeData::FuncGeneric(i),
                            type_params: 0,
                            impls: impls
                                .iter()
                                .flat_map(|path| scopes.resolve_impl(&path.data, path.span))
                                .collect(),
                        });
                    }

                    for (i, original) in proto.params.iter().enumerate() {
                        scopes.get_func_mut(id).proto.params[i].ty =
                            scopes.resolve_type(&original.ty).unwrap_or(TypeId::Unknown);
                    }

                    scopes.get_func_mut(id).proto.ret =
                        scopes.resolve_type(&proto.ret).unwrap_or(TypeId::Unknown);
                }
            },
        );

        id
    }

    fn forward_declare_fn(&mut self, scope: ScopeId, cons: bool, f: &Fn) {
        let id = self.forward_declare_proto(scope, cons, &f.proto);
        self.enter_id(self.get_func(id).body_scope, |scopes| {
            for stmt in f.body.iter() {
                scopes.forward_declare(stmt);
            }
        });
    }
}

#[derive(Debug, EnumAsInner)]
pub enum ResolvedPath {
    UserType(GenericUserType),
    Func(GenericFunc),
    Var(VariableId),
    Module(ScopeId),
    None(String),
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

macro_rules! type_mismatch {
    ($scopes: expr, $expected: expr, $actual: expr, $span: expr) => {
        Error::new(
            format!(
                "type mismatch: expected type '{}', got '{}'",
                $expected.name($scopes),
                $actual.name($scopes),
            ),
            $span,
        )
    };
}

macro_rules! type_check_bail {
    ($self: expr, $scopes: expr, $source: expr, $target: expr, $span: expr) => {{
        let source = $source;
        if !source.ty.coerces_to($scopes, $target) {
            return $self.error(type_mismatch!($scopes, $target, source.ty, $span));
        }

        Self::coerce_expr(source, $target, $scopes)
    }};
}

macro_rules! type_check {
    ($self: expr, $scopes: expr, $source: expr, $target: expr, $span: expr) => {{
        let source = $source;
        if !source.ty.coerces_to($scopes, $target) {
            $self.error::<()>(type_mismatch!($scopes, $target, source.ty, $span))
        }

        Self::coerce_expr(source, $target, $scopes)
    }};
}

macro_rules! resolve_forward_declare {
    ($self: ident, $scopes: expr, $checked: expr, $unchecked: expr) => {
        if $checked == TypeId::Unknown {
            $checked = $self.resolve_type($scopes, $unchecked);
        }
    };
}

macro_rules! resolve_impls {
    ($self: expr, $ty: expr, $impls: expr, $scopes: expr) => {
        for path in $impls {
            match $scopes.resolve_impl(&path.data, path.span) {
                Ok(ty) => {
                    $ty.impls.insert(ty);
                }
                Err(err) => $self.error(err),
            }
        }
    };
}

pub struct Module {
    pub scopes: Scopes,
    pub errors: Vec<(PathBuf, Vec<Error>)>,
    pub scope: ScopeId,
}

pub struct TypeChecker {
    errors: Vec<Error>,
}

impl TypeChecker {
    pub fn check(
        path: &std::path::Path,
        module: Vec<ParsedFile>,
        libs: Vec<PathBuf>,
    ) -> anyhow::Result<Module> {
        let mut this = Self { errors: vec![] };
        let mut scopes = Scopes::new();
        let mut errors = Vec::new();

        for lib in libs {
            let parsed = Pipeline::new(lib).parse()?;
            this.check_one(&mut scopes, &mut errors, &parsed.path, parsed.state.0);
        }

        Ok(Module {
            scope: this.check_one(&mut scopes, &mut errors, path, module),
            scopes,
            errors,
        })
    }

    fn check_one(
        &mut self,
        scopes: &mut Scopes,
        errors: &mut Vec<(PathBuf, Vec<Error>)>,
        path: &std::path::Path,
        module: Vec<ParsedFile>,
    ) -> ScopeId {
        let project = crate::derive_module_name(path);
        scopes.enter(Some(project.clone()), ScopeKind::Module(true), |scopes| {
            for file in module.iter() {
                match &file.ast.data {
                    Stmt::Module { name, body, .. } if name == &project => {
                        for stmt in body {
                            scopes.forward_declare(stmt);
                        }
                    }
                    _ => scopes.forward_declare(&file.ast),
                }
            }

            for mut file in module {
                match file.ast.data {
                    Stmt::Module { name, body, .. } if name == project => {
                        for stmt in body {
                            self.check_stmt(scopes, stmt);
                        }
                    }
                    _ => {
                        self.check_stmt(scopes, file.ast);
                    }
                }

                file.errors.append(&mut self.errors);
                if !file.errors.is_empty() {
                    errors.push((file.path, std::mem::take(&mut file.errors)));
                }
            }

            scopes.current_id()
        })
    }

    fn check_stmt(&mut self, scopes: &mut Scopes, stmt: Located<Stmt>) -> CheckedStmt {
        match stmt.data {
            Stmt::Module {
                public: _,
                name,
                body,
            } => CheckedStmt::Module(scopes.find_enter(&name, |scopes| {
                Block {
                    body: body
                        .into_iter()
                        .map(|stmt| self.check_stmt(scopes, stmt))
                        .collect(),
                    scope: scopes.current_id(),
                }
            })),
            Stmt::UserType(data) => match data {
                ParsedUserType::Struct(base) => {
                    let id = scopes.find_user_type(&base.name.data).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        for (name, impls) in base.type_params.iter() {
                            let id = scopes.find_user_type(name).unwrap();
                            resolve_impls!(self, scopes.get_user_type_mut(id), impls, scopes);
                        }

                        resolve_impls!(self, scopes.get_user_type_mut(id), base.impls, scopes);

                        for i in 0..base.members.len() {
                            resolve_forward_declare!(
                                self,
                                scopes,
                                scopes.get_user_type_mut(id).data.as_struct_mut().unwrap().0[i]
                                    .1
                                    .ty,
                                &base.members[i].1.ty
                            );
                        }

                        let init = *scopes.get_user_type(id).data.as_struct().unwrap().1;
                        for i in 0..base.members.len() {
                            resolve_forward_declare!(
                                self,
                                scopes,
                                scopes.get_func_mut(init).proto.params[i].ty,
                                &base.members[i].1.ty
                            );
                        }

                        for f in base.functions {
                            self.check_fn(scopes, f);
                        }

                        self.check_impls(scopes, scopes.get_user_type(id), base.name.span);
                    });
                    CheckedStmt::None
                }
                ParsedUserType::Union { tag: _, base } => {
                    let id = scopes.find_user_type(&base.name.data).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        for (name, impls) in base.type_params.iter() {
                            let id = scopes.find_user_type(name).unwrap();
                            resolve_impls!(self, scopes.get_user_type_mut(id), impls, scopes);
                        }

                        resolve_impls!(self, scopes.get_user_type_mut(id), base.impls, scopes);

                        for i in 0..base.members.len() {
                            resolve_forward_declare!(
                                self,
                                scopes,
                                scopes
                                    .get_user_type_mut(id)
                                    .data
                                    .as_union_mut()
                                    .unwrap()
                                    .variants[i]
                                    .1
                                    .ty,
                                &base.members[i].1.ty
                            );
                        }

                        for (name, member) in base.members.iter() {
                            let init = scopes.find_func(name).unwrap();
                            if !scopes.get_func_mut(init).proto.params.is_empty() {
                                resolve_forward_declare!(
                                    self,
                                    scopes,
                                    scopes.get_func_mut(init).proto.params[0].ty,
                                    &member.ty
                                );
                            }
                        }

                        for f in base.functions {
                            self.check_fn(scopes, f);
                        }

                        self.check_impls(scopes, scopes.get_user_type(id), base.name.span);
                    });

                    CheckedStmt::None
                }
                ParsedUserType::Trait {
                    name,
                    type_params,
                    impls,
                    functions,
                    ..
                } => {
                    let id = scopes.find_user_type(&name).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        for (name, impls) in type_params.iter() {
                            let id = scopes.find_user_type(name).unwrap();
                            resolve_impls!(self, scopes.get_user_type_mut(id), impls, scopes);
                        }

                        resolve_impls!(self, scopes.get_user_type_mut(id), impls, scopes);

                        for f in functions {
                            self.check_proto(scopes, f);
                        }
                    });

                    CheckedStmt::None
                }
                ParsedUserType::Enum {
                    name,
                    impls,
                    variants,
                    functions,
                    ..
                } => {
                    let id = scopes.find_user_type(&name.data).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        resolve_impls!(self, scopes.get_user_type_mut(id), impls, scopes);

                        for (name, expr) in variants {
                            scopes.get_var_mut(scopes.find_var(&name).unwrap()).value = expr
                                .map(|expr| self.check_expr(scopes, expr, Some(&TypeId::Usize)));
                        }

                        for f in functions {
                            self.check_fn(scopes, f);
                        }

                        self.check_impls(scopes, scopes.get_user_type(id), name.span);
                    });

                    CheckedStmt::None
                }
            },
            Stmt::Expr(expr) => CheckedStmt::Expr(self.check_expr(scopes, expr, None)),
            Stmt::Let {
                name,
                ty,
                mutable,
                value,
            } => {
                if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty);
                    if let Some(value) = value {
                        let span = value.span;
                        let value = type_check!(
                            self,
                            scopes,
                            self.check_expr(scopes, value, Some(&ty)),
                            &ty,
                            span
                        );

                        CheckedStmt::Let(scopes.insert_var(Variable {
                            public: false,
                            name,
                            ty,
                            is_static: false,
                            mutable,
                            value: Some(value),
                        }))
                    } else {
                        CheckedStmt::Let(scopes.insert_var(Variable {
                            public: false,
                            name,
                            ty,
                            is_static: false,
                            mutable,
                            value: None,
                        }))
                    }
                } else if let Some(value) = value {
                    let value = self.check_expr(scopes, value, None);
                    CheckedStmt::Let(scopes.insert_var(Variable {
                        public: false,
                        name,
                        ty: value.ty.clone(),
                        is_static: false,
                        mutable,
                        value: Some(value),
                    }))
                } else {
                    return self.error(Error::new("cannot infer type", stmt.span));
                }
            }
            Stmt::Fn(f) => {
                self.check_fn(scopes, f);
                CheckedStmt::None
            }
            Stmt::Static {
                name, ty, value, ..
            } => {
                // FIXME: detect cycles like static X: usize = X;
                // FIXME: non-const statics should be disallowed
                let id = scopes.find_var(&name).unwrap();
                let (value, ty) = if let Some(ty) = ty {
                    let ty = self.resolve_type(scopes, &ty);
                    let span = value.span;
                    let value = type_check!(
                        self,
                        scopes,
                        self.check_expr(scopes, value, Some(&ty)),
                        &ty,
                        span
                    );
                    (value, ty)
                } else {
                    let value = self.check_expr(scopes, value, None);
                    let ty = value.ty.clone();
                    (value, ty)
                };

                let var = scopes.get_var_mut(id);
                var.ty = ty;
                var.value = Some(value);
                CheckedStmt::None
            }
            Stmt::Use(path) => {
                if let Err(err) = scopes.resolve_use(&path, stmt.span) {
                    self.error(err)
                } else {
                    CheckedStmt::None
                }
            }
        }
    }

    fn signatures_match(
        scopes: &Scopes,
        this: &TypeId,
        lhs_id: FunctionId,
        rhs_id: FunctionId,
        rhs_ty: &GenericUserType,
    ) -> Result<(), String> {
        let lhs = scopes.get_func(lhs_id);
        let rhs = scopes.get_func(rhs_id);

        let sproto = &lhs.proto;
        let tproto = &rhs.proto;
        if sproto.params.len() != tproto.params.len()
            || sproto.type_params.len() != tproto.type_params.len()
        {
            return Err("type parameters are incorrect".into());
        }

        let compare_types = |a: &TypeId, mut b: TypeId| {
            b.fill_this(this);
            b.fill_type_generics(scopes, rhs_ty);
            b.fill_func_generics(
                scopes,
                &GenericFunc::new(
                    lhs_id,
                    sproto
                        .type_params
                        .iter()
                        .map(|name| {
                            TypeId::UserType(
                                GenericUserType::new(
                                    scopes.find_user_type_in(name, lhs.body_scope).unwrap(),
                                    vec![],
                                )
                                .into(),
                            )
                        })
                        .collect(),
                ),
            );

            a == &b
        };

        if !compare_types(&sproto.ret, tproto.ret.clone()) {
            return Err("return type is incorrect".into());
        }

        for (i, (s, t)) in sproto
            .params
            .iter()
            .zip(tproto.params.iter().cloned())
            .enumerate()
        {
            if !compare_types(&s.ty, t.ty) {
                return Err(format!("parameter {} is incorrect", i + 1));
            }
        }

        for (i, (s, t)) in sproto
            .type_params
            .iter()
            .zip(tproto.type_params.iter())
            .enumerate()
        {
            let s = scopes.get_user_type(scopes.find_user_type_in(s, lhs.body_scope).unwrap());
            let t = scopes.get_user_type(scopes.find_user_type_in(t, rhs.body_scope).unwrap());

            if s.impls.len() != t.impls.len() {
                return Err(format!("generic parameter {} is incorrect", i + 1));
            }

            for (s, t) in s.impls.iter().zip(t.impls.iter().cloned()) {
                for (s, t) in s.generics.iter().zip(t.generics.into_iter()) {
                    if !compare_types(s, t) {
                        return Err(format!("generic parameter {} is incorrect", i + 1));
                    }
                }
            }
        }

        Ok(())
    }

    fn check_impl(
        &mut self,
        scopes: &Scopes,
        this: &TypeId,
        ut: &UserType,
        gtr: &GenericUserType,
        span: Span,
    ) {
        // TODO: detect and fail on circular trait dependencies
        // TODO: default implementations
        let tr = scopes.get_user_type(gtr.id);
        for tr in tr.impls.iter() {
            self.check_impl(scopes, this, ut, tr, span);
        }

        for &rhs in scopes[tr.body_scope].fns.iter() {
            let rhs_func = scopes.get_func(rhs);
            if let Some(lhs) = scopes.find_func_in(&rhs_func.proto.name, ut.body_scope) {
                if let Err(err) = Self::signatures_match(scopes, this, lhs, rhs, gtr) {
                    self.error::<()>(Error::new(
                        format!(
                            "must implement '{}::{}': {err}",
                            gtr.name(scopes),
                            rhs_func.proto.name
                        ),
                        span,
                    ));
                }
            }
        }
    }

    fn check_impls(&mut self, scopes: &Scopes, ut: &UserType, span: Span) {
        for tr in ut.impls.iter() {
            self.check_impl(scopes, &scopes.this_type().unwrap(), ut, tr, span);
        }
    }

    fn check_proto(&mut self, scopes: &mut Scopes, proto: Prototype) -> FunctionId {
        // TODO: disallow private type in public interface
        let id = scopes.find_func(&proto.name).unwrap();
        scopes.enter_id(scopes.get_func(id).body_scope, |scopes| {
            for i in 0..proto.params.len() {
                resolve_forward_declare!(
                    self,
                    scopes,
                    scopes.get_func_mut(id).proto.params[i].ty,
                    &proto.params[i].ty
                );
            }

            resolve_forward_declare!(self, scopes, scopes.get_func_mut(id).proto.ret, &proto.ret);

            for (name, impls) in proto.type_params.iter() {
                let id = scopes.find_user_type(name).unwrap();
                resolve_impls!(self, scopes.get_user_type_mut(id), impls, scopes);
            }

            if proto.is_extern {
                return;
            }

            for param in scopes
                .get_func_mut(id)
                .proto
                .params
                .iter()
                .map(|param| Variable {
                    name: param.name.clone(),
                    ty: param.ty.clone(),
                    is_static: false,
                    public: false,
                    mutable: param.mutable,
                    value: None,
                })
                .collect::<Vec<_>>()
            {
                scopes.insert_var(param);
            }
        });

        id
    }

    fn check_fn(&mut self, scopes: &mut Scopes, f: Fn) -> FunctionId {
        // TODO: disallow private type in public interface
        let id = self.check_proto(scopes, f.proto);
        scopes.enter_id(scopes.get_func(id).body_scope, |scopes| {
            scopes.get_func_mut(id).body = Some(
                f.body
                    .into_iter()
                    .map(|stmt| self.check_stmt(scopes, stmt))
                    .collect(),
            );
        });

        id
    }

    fn check_expr(
        &mut self,
        scopes: &mut Scopes,
        expr: Located<Expr>,
        target: Option<&TypeId>,
    ) -> CheckedExpr {
        let span = expr.span;
        match expr.data {
            Expr::Binary { op, left, right } => {
                let left = self.check_expr(scopes, *left, target);
                let right_span = right.span;
                let right = type_check_bail!(
                    self,
                    scopes,
                    self.check_expr(scopes, *right, Some(&left.ty)),
                    &left.ty,
                    right_span
                );

                if !left.ty.supports_binop(op) {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for values of type {} and {}",
                            &left.ty.name(scopes),
                            &right.ty.name(scopes)
                        ),
                        span,
                    ))
                } else {
                    CheckedExpr::new(
                        match op {
                            BinaryOp::NoneCoalesce => todo!(),
                            BinaryOp::ErrCoalesce => todo!(),
                            BinaryOp::Gt
                            | BinaryOp::GtEqual
                            | BinaryOp::Lt
                            | BinaryOp::LtEqual
                            | BinaryOp::Equal
                            | BinaryOp::NotEqual
                            | BinaryOp::LogicalOr
                            | BinaryOp::LogicalAnd => TypeId::Bool,
                            _ => left.ty.clone(),
                        },
                        ExprData::Binary {
                            op,
                            left: left.into(),
                            right: right.into(),
                        },
                    )
                }
            }
            Expr::Unary { op, expr } => {
                use UnaryOp::*;

                let value_span = expr.span;
                let expr = self.check_expr(scopes, *expr, target);
                let mut out_ty = None;
                let valid = match op {
                    Plus => expr.ty.is_numeric(),
                    Neg => matches!(
                        expr.ty,
                        TypeId::Int(_)
                            | TypeId::Isize
                            | TypeId::F32
                            | TypeId::F64
                            | TypeId::CInt(_)
                    ),
                    PostIncrement | PostDecrement | PreIncrement | PreDecrement => {
                        if matches!(
                            expr.ty,
                            TypeId::Int(_)
                                | TypeId::Isize
                                | TypeId::Uint(_)
                                | TypeId::Usize
                                | TypeId::CInt(_)
                                | TypeId::CUint(_)
                        ) {
                            if !Self::is_assignable(&expr, scopes) {
                                return self
                                    .error(Error::new("expression is not assignable", value_span));
                            }

                            true
                        } else {
                            false
                        }
                    }
                    Not => matches!(
                        expr.ty,
                        TypeId::Int(_)
                            | TypeId::Isize
                            | TypeId::Uint(_)
                            | TypeId::Usize
                            | TypeId::Bool
                            | TypeId::CInt(_)
                            | TypeId::CUint(_)
                    ),
                    Deref => {
                        if let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = &expr.ty {
                            out_ty = Some((**inner).clone());
                            true
                        } else {
                            false
                        }
                    }
                    Addr => {
                        out_ty = Some(TypeId::Ptr(expr.ty.clone().into()));
                        true
                    }
                    AddrMut => {
                        if !Self::can_addrmut(&expr, scopes) {
                            self.error::<()>(Error::new(
                                "cannot create mutable pointer to immutable memory location",
                                span,
                            ));
                        }

                        out_ty = Some(TypeId::MutPtr(expr.ty.clone().into()));
                        true
                    }
                    Unwrap => {
                        if let Some(inner) = scopes.as_option_inner(&expr.ty) {
                            let func = scopes.find_func_in(
                                "unwrap",
                                scopes
                                    .get_user_type(scopes.find_core_option().unwrap())
                                    .body_scope,
                            );

                            return CheckedExpr::new(
                                inner.clone(),
                                ExprData::Call {
                                    inst: Some(expr.ty.clone()),
                                    args: IndexMap::from([(
                                        THIS_PARAM.into(),
                                        CheckedExpr::new(
                                            TypeId::Ptr(expr.ty.clone().into()),
                                            ExprData::Unary {
                                                op: UnaryOp::Addr,
                                                expr: expr.into(),
                                            },
                                        ),
                                    )]),
                                    func: GenericFunc::new(func.unwrap(), vec![]),
                                    trait_fn: false,
                                },
                            );
                        } else {
                            self.error::<()>(Error::new(
                                "unwrap operator is only valid for option types",
                                span,
                            ));
                            false
                        }
                    }
                    Try => todo!(),
                };

                if valid {
                    CheckedExpr::new(
                        out_ty.unwrap_or_else(|| expr.ty.clone()),
                        ExprData::Unary {
                            op,
                            expr: expr.into(),
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!(
                            "operator '{op}' is invalid for value of type {}",
                            &expr.ty.name(scopes)
                        ),
                        span,
                    ))
                }
            }
            Expr::Call { callee, args } => self.check_call(scopes, target, *callee, args, span),
            Expr::Array(elements) => {
                let mut checked = Vec::with_capacity(elements.len());
                let mut elements = elements.into_iter();
                let inner = if let Some(TypeId::Array(inner)) = target {
                    inner.0.clone()
                } else if let Some(expr) = elements.next() {
                    let expr = self.check_expr(scopes, expr, None);
                    let ty = expr.ty.clone();
                    checked.push(expr);
                    ty
                } else {
                    return self.error(Error::new("cannot infer type of array literal", expr.span));
                };

                checked.extend(elements.map(|e| self.check_expr(scopes, e, Some(&inner))));
                CheckedExpr::new(
                    TypeId::Array(Box::new((inner, checked.len()))),
                    ExprData::Array(checked),
                )
            }
            Expr::ArrayWithInit { init, count } => {
                let init = if let Some(TypeId::Array(inner)) = target {
                    let span = init.span;
                    type_check!(
                        self,
                        scopes,
                        self.check_expr(scopes, *init, Some(&inner.0)),
                        &inner.0,
                        span
                    )
                } else {
                    self.check_expr(scopes, *init, None)
                };

                match Self::consteval(scopes, &count, Some(&TypeId::Usize)) {
                    Ok(count) => CheckedExpr::new(
                        TypeId::Array(Box::new((init.ty.clone(), count))),
                        ExprData::ArrayWithInit {
                            init: init.into(),
                            count,
                        },
                    ),
                    Err(err) => self.error(err),
                }
            }
            Expr::Tuple(_) => todo!(),
            Expr::Map(_) => todo!(),
            Expr::Set(_) => todo!(),
            Expr::Range { .. } => todo!(),
            Expr::String(s) => {
                CheckedExpr::new(scopes.find_core_string().unwrap(), ExprData::String(s))
            }
            Expr::Char(s) => CheckedExpr::new(TypeId::Char, ExprData::Char(s)),
            Expr::None => {
                if let Some(inner) = target.and_then(|target| scopes.as_option_inner(target)) {
                    CheckedExpr::new(
                        scopes.make_option(inner.clone()).unwrap(),
                        ExprData::Instance(
                            [(
                                "None".into(),
                                self.check_expr(scopes, Located::new(Expr::Void, span), target),
                            )]
                            .into(),
                        ),
                    )
                } else {
                    self.error(Error::new("cannot infer type of option literal null", span))
                }
            }
            Expr::Void => CheckedExpr::new(TypeId::Void, ExprData::Void),
            Expr::Bool(value) => CheckedExpr {
                ty: TypeId::Bool,
                data: ExprData::Bool(value),
            },
            Expr::Integer { base, value, width } => {
                let ty = if let Some(width) = width {
                    TypeId::from_int_name(&width).unwrap_or_else(|| {
                        self.error(Error::new(
                            format!("invalid integer literal type: {width}"),
                            span,
                        ))
                    })
                } else {
                    // FIXME: attempt to promote the literal if its too large for i32
                    // FIXME: addr of should change the target to remove one pointer
                    target
                        .map(|mut target| {
                            loop {
                                match target {
                                    TypeId::MutPtr(ty) | TypeId::Ptr(ty) => target = ty,
                                    other => {
                                        if let Some(inner) = scopes.as_option_inner(other) {
                                            target = inner;
                                        } else {
                                            break;
                                        }
                                    }
                                }
                            }
                            target
                        })
                        .filter(|target| TypeId::IntGeneric.coerces_to(scopes, target))
                        .cloned()
                        .unwrap_or(TypeId::Int(32))
                };

                let IntStats { signed, bits } = ty.integer_stats().unwrap();
                if signed {
                    let result = match i128::from_str_radix(&value, base as u32) {
                        Ok(result) => result,
                        Err(e) => {
                            return self.error(Error::new(
                                format!("integer literal '{value}' is too large: {e}."),
                                expr.span,
                            ));
                        }
                    };

                    let max = (1 << (bits - 1)) - 1;
                    if result > max {
                        return self.error(Error::new(
                            format!("integer literal is larger than its type allows ({max})"),
                            expr.span,
                        ));
                    }

                    let min = -(1 << (bits - 1));
                    if result < min {
                        return self.error(Error::new(
                            format!("integer literal is smaller than its type allows ({min})"),
                            expr.span,
                        ));
                    }

                    CheckedExpr::new(ty, ExprData::Signed(result))
                } else {
                    let result = match u128::from_str_radix(&value, base as u32) {
                        Ok(result) => result,
                        Err(_) => {
                            return self.error(Error::new(
                                format!("integer literal '{value}' is too large."),
                                expr.span,
                            ));
                        }
                    };

                    let max = (1 << bits) - 1;
                    if result >= max {
                        return self.error(Error::new(
                            format!("integer literal is larger than its type allows ({max})"),
                            expr.span,
                        ));
                    }

                    CheckedExpr::new(ty, ExprData::Unsigned(result))
                }
            }
            Expr::Float(value) => CheckedExpr::new(
                target
                    .map(|mut target| {
                        while let TypeId::MutPtr(ty) | TypeId::Ptr(ty) = target {
                            target = ty;
                        }
                        target
                    })
                    .filter(|target| TypeId::FloatGeneric.coerces_to(scopes, target))
                    .cloned()
                    .unwrap_or(TypeId::F64),
                ExprData::Float(value),
            ),
            Expr::Path(path) => match scopes.resolve_path(&path, span) {
                Ok(ResolvedPath::Var(id)) => CheckedExpr::new(
                    scopes.get_var(id).ty.clone(),
                    ExprData::Symbol(Symbol::Var(id)),
                ),
                Ok(ResolvedPath::Func(id)) => {
                    CheckedExpr::new(TypeId::Func(id.into()), ExprData::Symbol(Symbol::Func))
                }
                Ok(ResolvedPath::UserType(ut)) => {
                    let Some(st) = scopes.get_user_type(ut.id).data.as_struct() else {
                        return self.error(Error::new("expected value", span));
                    };

                    CheckedExpr::new(
                        TypeId::Func(GenericFunc::new(*st.1, ut.generics).into()),
                        ExprData::Symbol(Symbol::Func),
                    )
                }
                Ok(ResolvedPath::Module(_)) => {
                    self.error(Error::new("expected value, found module", span))
                }
                Ok(ResolvedPath::None(err)) => self.error(Error::new(err, span)),
                Err(err) => self.error(err),
            },
            Expr::Assign {
                target: lhs,
                binary,
                value,
            } => {
                let span = lhs.span;
                let lhs = self.check_expr(scopes, *lhs, None);
                if !Self::is_assignable(&lhs, scopes) {
                    // TODO: report a better error here
                    return self.error(Error::new("expression is not assignable", span));
                }

                let rhs = type_check_bail!(
                    self,
                    scopes,
                    self.check_expr(scopes, *value, Some(&lhs.ty)),
                    &lhs.ty,
                    span
                );

                if let Some(op) = binary {
                    if !lhs.ty.supports_binop(op) {
                        self.error::<()>(Error::new(
                            format!(
                                "operator '{op}' is invalid for values of type {} and {}",
                                &lhs.ty.name(scopes),
                                &rhs.ty.name(scopes)
                            ),
                            span,
                        ));
                    }
                }

                CheckedExpr::new(
                    lhs.ty.clone(),
                    ExprData::Assign {
                        target: lhs.into(),
                        binary,
                        value: rhs.into(),
                    },
                )
            }
            Expr::Block(body) => {
                let block =
                    self.create_block(scopes, body, ScopeKind::Block(target.cloned(), false));
                let ScopeKind::Block(target, yields) = &scopes[block.scope].kind else {
                    panic!("ICE: target of block changed from block to something else");
                };
                CheckedExpr::new(
                    yields
                        .then(|| target.clone())
                        .flatten()
                        .unwrap_or(TypeId::Void),
                    ExprData::Block(block),
                )
            }
            Expr::If {
                cond,
                if_branch,
                else_branch,
            } => {
                let cond_span = cond.span;
                let cond = type_check!(
                    self,
                    scopes,
                    self.check_expr(scopes, *cond, Some(&TypeId::Bool)),
                    &TypeId::Bool,
                    cond_span
                );

                let target = if else_branch.is_some() {
                    target
                } else {
                    target
                        .and_then(|t| t.as_user_type())
                        .filter(|t| t.id == scopes.find_core_option().unwrap())
                        .map(|target| &target.generics[0])
                };

                let if_span = if_branch.span;
                let mut if_branch = self.check_expr(scopes, *if_branch, target);
                if let Some(target) = target {
                    if_branch = type_check!(self, scopes, if_branch, target, if_span);
                }

                let mut out_type = if_branch.ty.clone();
                let else_branch = if let Some(e) = else_branch {
                    let span = e.span;
                    Some(type_check!(
                        self,
                        scopes,
                        self.check_expr(scopes, *e, Some(&if_branch.ty)),
                        &out_type,
                        span
                    ))
                } else {
                    // this separates these two cases:
                    //   let x /* void? */ = if whatever { yield void; };
                    //   let x /* void */ = if whatever { };
                    if matches!(&if_branch.data, ExprData::Block(b) if
                        matches!(scopes[b.scope].kind, ScopeKind::Block(_, yields) if yields))
                    {
                        out_type = scopes.make_option(out_type).unwrap();
                        if_branch = Self::coerce_expr(if_branch, &out_type, scopes);

                        Some(self.check_expr(
                            scopes,
                            Located::new(Expr::None, span),
                            Some(&out_type),
                        ))
                    } else {
                        None
                    }
                };

                CheckedExpr::new(
                    out_type,
                    ExprData::If {
                        cond: cond.into(),
                        if_branch: if_branch.into(),
                        else_branch: else_branch.map(|e| e.into()),
                    },
                )
            }
            Expr::Loop {
                cond,
                body,
                do_while,
            } => {
                let span = cond.span;
                let cond = type_check!(
                    self,
                    scopes,
                    self.check_expr(scopes, *cond, Some(&TypeId::Bool)),
                    &TypeId::Bool,
                    span
                );

                let inf = matches!(cond.data, ExprData::Bool(true));
                let target = if inf {
                    target
                } else {
                    target
                        .and_then(|t| t.as_user_type())
                        .filter(|t| t.id == scopes.find_core_option().unwrap())
                        .map(|target| &target.generics[0])
                };

                let body = self.create_block(scopes, body, ScopeKind::Loop(target.cloned(), false));
                let ScopeKind::Loop(target, breaks) = &scopes[body.scope].kind else {
                    panic!("ICE: target of loop changed from loop to something else");
                };

                let out_type = if inf {
                    TypeId::Never
                } else {
                    // TODO: coerce the break statements
                    breaks
                        .then(|| scopes.make_option(target.clone().unwrap()).unwrap())
                        .unwrap_or(TypeId::Void)
                };

                CheckedExpr::new(
                    out_type,
                    ExprData::Loop {
                        cond: cond.into(),
                        body,
                        do_while,
                    },
                )
            }
            Expr::For { .. } => todo!(),
            Expr::Member {
                source,
                member,
                generics,
            } => {
                if !generics.is_empty() {
                    self.error::<()>(Error::new(
                        "member variables cannot be parameterized with generics",
                        span,
                    ));
                }

                let source = self.check_expr(scopes, *source, None);
                let id = source.ty.strip_references();
                let ty = scopes.get_user_type(match &id {
                    TypeId::UserType(data) => data.id,
                    _ => {
                        return self.error(Error::new(
                            format!("cannot get member of type '{}'", id.name(scopes)),
                            span,
                        ));
                    }
                });
                if let Some(members) = ty.data.members() {
                    if let Some((_, var)) = members.iter().find(|m| m.0 == member) {
                        if ty.data.is_union() && !var.shared {
                            return self.error(Error::new(
                                "cannot access union variant with '.' (only shared members)",
                                span,
                            ));
                        }

                        if !var.public && !scopes.is_sub_scope(ty.scope) {
                            return self.error(Error::new(
                                format!(
                                    "cannot access private member '{member}' of type {}",
                                    id.name(scopes)
                                ),
                                span,
                            ));
                        }

                        let mut ty = var.ty.clone();
                        if let Some(instance) = id.as_user_type() {
                            ty.fill_type_generics(scopes, instance);
                        }

                        let id = id.clone();
                        return CheckedExpr::new(
                            ty,
                            ExprData::Member {
                                source: Self::auto_deref(source, &id).into(),
                                member,
                            },
                        );
                    }
                }

                self.error(Error::new(
                    format!("type {} has no member '{member}'", &source.ty.name(scopes)),
                    span,
                ))
            }
            Expr::Subscript { callee, args } => {
                if args.len() > 1 {
                    self.error::<()>(Error::new(
                        "multidimensional subscript is not supported",
                        args[1].span,
                    ));
                }

                let callee = self.check_expr(scopes, *callee, None);
                let arg = args.into_iter().next().unwrap();
                let arg_span = arg.span;
                let arg = type_check_bail!(
                    self,
                    scopes,
                    self.check_expr(scopes, arg, Some(&TypeId::Isize)),
                    &TypeId::Isize,
                    arg_span
                );

                if let TypeId::Array(target) = &callee.ty {
                    CheckedExpr::new(
                        target.0.clone(),
                        ExprData::Subscript {
                            callee: callee.into(),
                            args: vec![arg],
                        },
                    )
                } else {
                    self.error(Error::new(
                        format!("type {} cannot be subscripted", &callee.ty.name(scopes)),
                        span,
                    ))
                }
            }
            Expr::Return(expr) => {
                let Some(target) = scopes.current_function().map(|id| scopes.get_func(id).proto.ret.clone()) else {
                    // the parser ensures return only happens inside functions
                    return self.error(Error::new("return outside of function", span));
                };

                let span = expr.span;
                let expr = type_check!(
                    self,
                    scopes,
                    self.check_expr(scopes, *expr, Some(&target)),
                    &target,
                    span
                );

                CheckedExpr::new(TypeId::Never, ExprData::Return(expr.into()))
            }
            Expr::Yield(expr) => {
                let ScopeKind::Block(target, _) = scopes.current().kind.clone() else {
                    return self.error(Error::new("yield outside of block", span));
                };

                let span = expr.span;
                let mut expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    expr = type_check!(self, scopes, expr, target, span);
                    scopes.current().kind = ScopeKind::Block(Some(target.clone()), true);
                } else {
                    scopes.current().kind = ScopeKind::Block(Some(expr.ty.clone()), true);
                }

                CheckedExpr::new(TypeId::Never, ExprData::Yield(expr.into()))
            }
            Expr::Break(expr) => {
                let Some(scope) = scopes.iter().find_map(|(id, scope)| {
                    matches!(scope.kind, ScopeKind::Loop(_, _)).then_some(id)
                }) else {
                    return self.error(Error::new("break outside of loop", span));
                };

                let ScopeKind::Loop(target, _) = scopes[scope].kind.clone() else {
                    unreachable!()
                };

                let span = expr.span;
                let mut expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    expr = type_check!(self, scopes, expr, target, span);
                    scopes[scope].kind = ScopeKind::Loop(Some(target.clone()), true);
                } else {
                    scopes[scope].kind = ScopeKind::Loop(Some(expr.ty.clone()), true);
                }

                CheckedExpr::new(TypeId::Never, ExprData::Break(expr.into()))
            }
            Expr::Continue => {
                if scopes
                    .iter()
                    .find_map(|(id, scope)| {
                        matches!(scope.kind, ScopeKind::Loop(_, _)).then_some(id)
                    })
                    .is_none()
                {
                    return self.error(Error::new("continue outside of loop", span));
                }

                CheckedExpr::new(TypeId::Never, ExprData::Continue)
            }
            Expr::Is { .. } => todo!(),
            Expr::Match { expr, body } => {
                let scrutinee_span = expr.span;
                let scrutinee = self.check_expr(scopes, *expr, None);
                let mut target = target.cloned();
                let mut result = Vec::new();
                for (pattern, expr) in body.into_iter() {
                    let span = expr.span;
                    let (var, variant) =
                        self.check_pattern(scopes, &scrutinee, scrutinee_span, pattern);
                    let (var, mut expr) = scopes.enter(None, ScopeKind::None, |scopes| {
                        let var = var.map(|var| scopes.insert_var(var));
                        (var, self.check_expr(scopes, expr, target.as_ref()))
                    });

                    if let Some(target) = &target {
                        expr = type_check!(self, scopes, expr, target, span);
                    } else {
                        target = Some(expr.ty.clone());
                    }

                    result.push((
                        UnionPattern {
                            binding: var,
                            variant,
                        },
                        CheckedExpr::new(TypeId::Never, ExprData::Yield(expr.into())),
                    ));
                }

                CheckedExpr::new(
                    target.unwrap_or(TypeId::Void),
                    ExprData::Match {
                        expr: scrutinee.into(),
                        body: result,
                    },
                )
            }
            Expr::As { expr, ty } => {
                let mut expr = self.check_expr(scopes, *expr, None);
                let ty = self.resolve_type(scopes, &ty);
                if !expr.ty.coerces_to(scopes, &ty) {
                    match (&expr.ty, &ty) {
                        (TypeId::Int(a), TypeId::Int(b) | TypeId::Uint(b)) if a <= b => {}
                        (TypeId::Uint(a), TypeId::Uint(b)) if a <= b => {}
                        (TypeId::CInt(a), TypeId::CInt(b) | TypeId::CUint(b)) if a <= b => {}
                        (TypeId::CUint(a), TypeId::CUint(b)) if a <= b => {}
                        // TODO: these should only be allowable with an unsafe pointer type, or
                        // should be unsafe to do
                        (TypeId::Usize, TypeId::Ptr(_) | TypeId::MutPtr(_)) => {}
                        (TypeId::Ptr(_), TypeId::Ptr(_) | TypeId::Usize) => {}
                        (TypeId::MutPtr(_), TypeId::Ptr(_) | TypeId::MutPtr(_) | TypeId::Usize) => {
                        }
                        _ => {
                            expr = self.error(Error::new(
                                format!(
                                    "cannot infallibly cast expression of type '{}' to '{}'",
                                    expr.ty.name(scopes),
                                    ty.name(scopes)
                                ),
                                span,
                            ));
                        }
                    }

                    CheckedExpr::new(ty, ExprData::As(expr.into()))
                } else {
                    Self::coerce_expr(expr, &ty, scopes)
                }
            }
        }
    }

    fn check_pattern(
        &mut self,
        scopes: &Scopes,
        scrutinee: &CheckedExpr,
        span: Span,
        pattern: Pattern,
    ) -> (Option<Variable>, (String, usize)) {
        let Some(ut) = scrutinee.ty
            .as_user_type()
            .filter(|ut| scopes.get_user_type(ut.id).data.is_union()) else {
            return self.error(Error::new("match scrutinee must be a union type", span));
        };

        let span = pattern.path.span;
        let result = match scopes.resolve_path(&pattern.path.data, span) {
            Ok(path) => path,
            Err(err) => return self.error(err),
        };

        let Some(func) = result.as_func() else {
            return self.error(Error::new("pattern does not match the scrutinee", span));
        };

        let func = scopes.get_func(func.id);
        if !func.constructor {
            return self.error(Error::new("pattern does not match the scrutinee", span));
        }

        let Some(union) = func.proto.ret.as_user_type()
            .filter(|ty| ty.id == ut.id)
            .and_then(|ty| scopes.get_user_type(ty.id).data.as_union()) else {
            return self.error(Error::new("pattern does not match the scrutinee", span));
        };

        let name = pattern.path.data.components().last().unwrap().0.clone();
        let (_, member) = union.variants.iter().find(|(m, _)| m == &name).unwrap();

        let mut ty = member.ty.clone();
        ty.fill_type_generics(scopes, ut);
        let tag = union.variant_tag(&name).unwrap();
        if let Some((mutable, binding)) = pattern.binding {
            (
                Some(Variable {
                    public: false,
                    name: binding,
                    ty,
                    is_static: false,
                    mutable,
                    value: None,
                }),
                (name, tag),
            )
        } else if ty.is_void() {
            (None, (name, tag))
        } else {
            self.error(Error::new(
                format!("union variant '{name}' has data that must be bound"),
                span,
            ))
        }
    }

    fn get_member_fn(
        scopes: &Scopes,
        member: &str,
        ut: &Scoped<UserType>,
    ) -> Option<(Option<GenericUserType>, FunctionId)> {
        if let Some(func) = (ut.data.is_struct() || ut.data.is_union() || ut.data.is_enum())
            .then(|| scopes.find_func_in(member, ut.body_scope))
            .flatten()
        {
            return Some((None, func));
        }

        for ut in ut.impls.iter() {
            if let Some(func) = scopes.find_func_in(member, scopes.get_user_type(ut.id).body_scope)
            {
                return Some((Some(ut.clone()), func));
            }
        }

        None
    }

    fn check_call(
        &mut self,
        scopes: &mut Scopes,
        target: Option<&TypeId>,
        callee: Located<Expr>,
        args: Vec<(Option<String>, Located<Expr>)>,
        span: Span,
    ) -> CheckedExpr {
        if let Expr::Member {
            source,
            member,
            generics,
        } = callee.data
        {
            let this = self.check_expr(scopes, *source, None);
            let id = this.ty.strip_references().clone();
            let Some(ut) = id.as_user_type() else {
                return self.error(Error::new(
                    format!("cannot get member of type '{}'", id.name(scopes)),
                    span,
                ));
            };
            let ty = scopes.get_user_type(ut.id);
            if let Some((tr, func)) = Self::get_member_fn(scopes, &member, ty) {
                let f = scopes.get_func(func);
                if !f.proto.public && !scopes.is_sub_scope(ty.scope) {
                    return self.error(Error::new(
                        format!(
                            "cannot access private method '{member}' of type '{}'",
                            id.name(scopes)
                        ),
                        span,
                    ));
                }

                if let Some(this_param) = f.proto.params.get(0).filter(|p| p.name == THIS_PARAM) {
                    if this_param.ty.is_mut_ptr() {
                        let mut ty = &this.ty;
                        if !ty.is_ptr() && !ty.is_mut_ptr() && !Self::can_addrmut(&this, scopes) {
                            return self.error(Error::new(
                                format!("cannot call method '{member}' with immutable receiver"),
                                span,
                            ));
                        } else {
                            while let TypeId::MutPtr(inner) = ty {
                                ty = inner;
                            }

                            if matches!(ty, TypeId::Ptr(_)) {
                                return self.error(Error::new(
                                    format!(
                                        "cannot call method '{member}' through an immutable pointer"
                                    ),
                                    span,
                                ));
                            }
                        }
                    }

                    let mut result = IndexMap::from([(
                        THIS_PARAM.into(),
                        if !matches!(this.ty, TypeId::Ptr(_) | TypeId::MutPtr(_)) {
                            if matches!(this_param.ty, TypeId::Ptr(_)) {
                                CheckedExpr::new(
                                    TypeId::Ptr(this.ty.clone().into()),
                                    ExprData::Unary {
                                        op: UnaryOp::Addr,
                                        expr: this.into(),
                                    },
                                )
                            } else {
                                CheckedExpr::new(
                                    TypeId::MutPtr(this.ty.clone().into()),
                                    ExprData::Unary {
                                        op: UnaryOp::AddrMut,
                                        expr: this.into(),
                                    },
                                )
                            }
                        } else {
                            Self::auto_deref(this, &this_param.ty)
                        },
                    )]);

                    let mut func =
                        match scopes.resolve_generics(f.proto.type_params.len(), &generics, span) {
                            Ok(generics) => GenericFunc::new(func, generics),
                            Err(error) => return self.error(error),
                        };
                    let (args, ret) = self.check_fn_args(
                        tr.as_ref().or(Some(ut)),
                        &mut func,
                        args,
                        &Vec::from(&f.proto.params[1..]),
                        target,
                        scopes,
                        span,
                    );

                    result.extend(args);
                    return CheckedExpr::new(
                        ret,
                        ExprData::Call {
                            func,
                            inst: Some(id),
                            args: result,
                            trait_fn: tr.is_some(),
                        },
                    );
                }
            }

            self.error(Error::new(
                format!("no method '{member}' found on type '{}'", id.name(scopes)),
                span,
            ))
        } else {
            match self.check_expr(scopes, callee, None).ty {
                TypeId::Func(mut func) => {
                    let f = scopes.get_func(func.id);
                    let constructor = f.constructor;
                    if constructor {
                        let ret = f.proto.ret.clone();
                        let ut = scopes.get_user_type(ret.as_user_type().unwrap().id);
                        if let Some(st) = ut.data.as_struct() {
                            if st.0.iter().any(|member| !member.1.public)
                                && !scopes.is_sub_scope(ut.scope)
                            {
                                return CheckedExpr::new(
                                    ret,
                                    self.error(Error::new(
                                        "cannot construct type with private members",
                                        span,
                                    )),
                                );
                            }
                        }
                    }

                    let (args, ret) = self.check_fn_args(
                        None,
                        &mut func,
                        args,
                        &f.proto.params.clone(),
                        target,
                        scopes,
                        span,
                    );

                    CheckedExpr::new(
                        ret,
                        if constructor {
                            ExprData::Instance(args)
                        } else {
                            ExprData::Call {
                                func: *func,
                                args,
                                inst: None,
                                trait_fn: false,
                            }
                        },
                    )
                }
                ty => self.error(Error::new(
                    format!("cannot call value of type '{}'", &ty.name(scopes)),
                    span,
                )),
            }
        }
    }

    fn check_arg(
        &mut self,
        func: &mut GenericFunc,
        scopes: &mut Scopes,
        expr: Located<Expr>,
        param: &CheckedParam,
        inst: Option<&GenericUserType>,
    ) -> CheckedExpr {
        let mut target = param.ty.clone();
        if !func.generics.is_empty() {
            target.fill_func_generics(scopes, func);
        }

        if let Some(inst) = inst {
            target.fill_type_generics(scopes, inst);
        }

        let span = expr.span;
        let expr = self.check_expr(scopes, expr, Some(&target));
        if !func.generics.is_empty() {
            func.infer_generics(&param.ty, &expr.ty, scopes);
            target.fill_func_generics(scopes, func);
        }

        if let Some(inst) = inst {
            target.fill_type_generics(scopes, inst);
        }

        type_check_bail!(self, scopes, expr, &target, span)
    }

    #[allow(clippy::too_many_arguments)]
    fn check_fn_args(
        &mut self,
        inst: Option<&GenericUserType>,
        func: &mut GenericFunc,
        args: Vec<(Option<String>, Located<Expr>)>,
        params: &[CheckedParam],
        target: Option<&TypeId>,
        scopes: &mut Scopes,
        span: Span,
    ) -> (IndexMap<String, CheckedExpr>, TypeId) {
        let mut result = IndexMap::with_capacity(args.len());
        let mut last_pos = 0;
        for (name, expr) in args {
            if let Some(name) = name {
                match result.entry(name.clone()) {
                    Entry::Occupied(_) => {
                        self.error::<()>(Error::new(
                            format!("parameter '{name}' has already been specified"),
                            expr.span,
                        ));
                    }
                    Entry::Vacant(entry) => {
                        if let Some(param) = params.iter().find(|p| p.name == name) {
                            entry.insert(self.check_arg(func, scopes, expr, param, inst));
                        } else {
                            self.error::<()>(Error::new(
                                format!("unknown parameter: '{name}'"),
                                expr.span,
                            ));
                        }
                    }
                }
            } else if let Some((i, param)) = params
                .iter()
                .enumerate()
                .skip(last_pos)
                .find(|(_, param)| !param.keyword)
            {
                result.insert(
                    param.name.clone(),
                    self.check_arg(func, scopes, expr, param, inst),
                );
                last_pos = i + 1;
            } else {
                // TODO: a better error here would be nice
                self.error::<()>(Error::new("too many positional arguments", expr.span));
            }
        }

        // for param in params
        //     .iter()
        //     .filter(|p| !result.contains_key(&p.name))
        //     .collect::<Vec<_>>()
        // {
        //     if let Some(default) = &param.default {
        //         result.insert(param.name.clone(), default.clone());
        //     }
        // }

        if params.len() != result.len() {
            let mut missing = String::new();
            for param in params {
                if !result.contains_key(&param.name) {
                    if !missing.is_empty() {
                        missing.push_str(", ");
                    }

                    missing.push_str(&param.name);
                }
            }

            self.error::<()>(Error::new(
                format!(
                    "expected {} argument(s), found {} (missing {missing})",
                    params.len(),
                    result.len()
                ),
                span,
            ));
        }

        let mut ret = scopes.get_func(func.id).proto.ret.clone();
        if !func.generics.is_empty() {
            if let Some(target) = target {
                func.infer_generics(&ret, target, scopes);
            }

            ret.fill_func_generics(scopes, func);
            for (i, ty) in func.generics.iter_mut().enumerate() {
                if ty.is_unknown() {
                    self.error::<()>(Error::new(
                        format!(
                            "cannot infer type of generic parameter '{}'",
                            scopes.get_func(func.id).proto.type_params[i]
                        ),
                        span,
                    ));
                }

                let f = scopes.get_func(func.id);
                let param = scopes
                    .find_user_type_in(&f.proto.type_params[i], f.body_scope)
                    .unwrap();
                for bound in scopes.get_user_type(param).impls.iter() {
                    if !ty.implements_trait(scopes, bound) {
                        self.error::<()>(Error::new(
                            format!(
                                "type '{}' does not implement '{}'",
                                ty.name(scopes),
                                bound.name(scopes),
                            ),
                            span,
                        ));
                    }
                }
            }
        }

        if let Some(inst) = inst {
            ret.fill_type_generics(scopes, inst);
        }

        (result, ret)
    }

    fn create_block(
        &mut self,
        scopes: &mut Scopes,
        body: Vec<Located<Stmt>>,
        kind: ScopeKind,
    ) -> Block {
        scopes.enter(None, kind, |scopes| Block {
            body: body
                .into_iter()
                .map(|stmt| self.check_stmt(scopes, stmt))
                .collect(),
            scope: scopes.current_id(),
        })
    }

    fn resolve_type(&mut self, scopes: &Scopes, ty: &TypeHint) -> TypeId {
        scopes
            .resolve_type(ty)
            .unwrap_or_else(|err| self.error(err))
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.errors.push(error);
        T::default()
    }

    fn consteval(
        scopes: &Scopes,
        expr: &Located<Expr>,
        target: Option<&TypeId>,
    ) -> Result<usize, Error> {
        match &expr.data {
            Expr::Integer { base, value, width } => {
                if let Some(width) = width
                    .as_ref()
                    .and_then(|width| TypeId::from_int_name(width))
                {
                    if let Some(target) = target {
                        if target != &width {
                            return Err(type_mismatch!(scopes, target, &width, expr.span));
                        }
                    }
                }

                match usize::from_str_radix(value, *base as u32) {
                    Ok(value) => Ok(value),
                    Err(_) => Err(Error::new("value cannot be converted to usize", expr.span)),
                }
            }
            _ => Err(Error::new(
                "expression is not compile time evaluatable",
                expr.span,
            )),
        }
    }

    fn is_assignable(expr: &CheckedExpr, scopes: &Scopes) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, TypeId::MutPtr(_))
            }
            ExprData::Symbol(_) | ExprData::Member { .. } => Self::can_addrmut(expr, scopes),
            ExprData::Subscript { callee, .. } => Self::is_assignable(callee, scopes),
            _ => false,
        }
    }

    fn can_addrmut(expr: &CheckedExpr, scopes: &Scopes) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                !matches!(op, UnaryOp::Deref) || matches!(expr.ty, TypeId::MutPtr(_))
            }
            ExprData::Symbol(symbol) => match symbol {
                Symbol::Func => false,
                Symbol::Var(id) => scopes.get_var(*id).mutable,
            },
            ExprData::Member { source, .. } => {
                matches!(source.ty, TypeId::MutPtr(_)) || Self::can_addrmut(source, scopes)
            }
            ExprData::Subscript { callee, .. } => Self::can_addrmut(callee, scopes),
            _ => true,
        }
    }

    fn coerce_expr(expr: CheckedExpr, target: &TypeId, scopes: &Scopes) -> CheckedExpr {
        match (&expr.ty, target) {
            (
                TypeId::IntGeneric,
                TypeId::Int(_) | TypeId::Uint(_) | TypeId::Isize | TypeId::Usize,
            ) => CheckedExpr::new(target.clone(), expr.data),
            (TypeId::FloatGeneric, TypeId::F32 | TypeId::F64) => {
                CheckedExpr::new(target.clone(), expr.data)
            }
            (TypeId::MutPtr(lhs), TypeId::Ptr(rhs)) if lhs == rhs => {
                CheckedExpr::new(target.clone(), expr.data)
            }
            (ty, target)
                if scopes
                    .as_option_inner(target)
                    .map_or(false, |inner| ty.coerces_to(scopes, inner)) =>
            {
                let inner = scopes.as_option_inner(target).unwrap();
                let expr = Self::coerce_expr(expr, inner, scopes);
                Self::coerce_expr(
                    CheckedExpr::new(
                        scopes.make_option(expr.ty.clone()).unwrap(),
                        ExprData::Instance([("Some".into(), expr)].into()),
                    ),
                    target,
                    scopes,
                )
            }
            (TypeId::Never, _) => CheckedExpr::new(target.clone(), expr.data),
            _ => expr,
        }
    }

    fn auto_deref(mut expr: CheckedExpr, target: &TypeId) -> CheckedExpr {
        #[allow(clippy::redundant_clone)]
        let mut ty = expr.ty.clone();
        while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = &ty {
            match target {
                TypeId::Ptr(t) | TypeId::MutPtr(t) if t == inner => break,
                other if other == &ty => break,
                _ => {}
            }

            expr = CheckedExpr::new(
                (**inner).clone(),
                ExprData::Unary {
                    op: UnaryOp::Deref,
                    expr: expr.into(),
                },
            );
            ty = expr.ty.clone();
        }

        expr
    }
}
