use core::panic;
use std::{
    collections::{hash_map::Entry, HashMap},
    path::PathBuf,
};

use concat_idents::concat_idents;
use derive_more::{Constructor, Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;

use crate::{
    ast::{
        expr::{BinaryOp, Expr, UnaryOp},
        stmt::{Fn, Param, ParsedUserType, Prototype, Stmt, TypeHint},
        Path, Pattern,
    },
    checked_ast::{
        expr::{CheckedExpr, ExprData},
        stmt::CheckedStmt,
        Block, UnionPattern,
    },
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
                    if let Some(target) = target.as_user_type() {
                        if !src.generics.is_empty() && !target.generics.is_empty() {
                            for (src, target) in src.generics.iter().zip(target.generics.iter()) {
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

#[derive(Debug, Default, PartialEq, Eq, Clone, EnumAsInner, Hash)]
pub enum TypeId {
    #[default]
    Unknown,
    Void,
    Never,
    Int(u8),
    Uint(u8),
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
                )
            }
            BinaryOp::And | BinaryOp::Xor | BinaryOp::Or | BinaryOp::Shl | BinaryOp::Shr => {
                matches!(
                    self,
                    TypeId::Int(_) | TypeId::Uint(_) | TypeId::Isize | TypeId::Usize
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
                        | TypeId::Bool // FIXME: option<T> should be comparable with T
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

    pub fn fill_type_generics(&mut self, scopes: &Scopes, instance: &GenericUserType) {
        if instance.generics.is_empty() {
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
                            ty.fill_type_generics(scopes, instance);
                        }
                    } else if let Some(&index) =
                        scopes.get_user_type(ty.id).data.as_struct_generic()
                    {
                        *src = instance.generics[index].clone();
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
        )
    }

    fn coerces_to(&self, scopes: &Scopes, target: &TypeId) -> bool {
        match (self, target) {
            (
                TypeId::IntGeneric,
                TypeId::Int(_) | TypeId::Uint(_) | TypeId::Isize | TypeId::Usize,
            ) => true,
            (TypeId::FloatGeneric, TypeId::F32 | TypeId::F64) => true,
            (TypeId::MutPtr(ty), TypeId::Ptr(target)) if ty == target => true,
            (ty, target)
                if TypeChecker::as_option_inner(scopes, target)
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

#[derive(From)]
enum ResolveError<'a> {
    Path(&'a Located<Path>),
    #[from]
    Error(Error),
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
                    self[scope].$vec.push(itemid);
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
    pub body: Option<Block>,
    pub constructor: bool,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
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
        self.variants.iter().position(|(n, _)| name == n)
    }
}

#[derive(Debug, EnumAsInner)]
pub enum UserTypeData {
    Struct(Vec<(String, Member)>),
    Union(Union),
    Enum,
    FuncGeneric(usize),
    StructGeneric(usize),
}

impl UserTypeData {
    pub fn members(&self) -> Option<&[(String, Member)]> {
        match self {
            UserTypeData::Struct(members) => Some(members),
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
    pub type_params: Vec<String>,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Symbol {
    Func(GenericFunc),
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

    pub fn this_type(&self) -> Option<TypeId> {
        self.iter().find_map(|(_, scope)| {
            if let ScopeKind::UserType(id) = scope.kind {
                let ty = self.get_user_type(id);
                if ty.data.is_struct() || ty.data.is_union() || ty.data.is_enum() {
                    return Some(TypeId::UserType(
                        GenericUserType::new(
                            id,
                            ty.type_params
                                .iter()
                                .map(|name| {
                                    TypeId::UserType(
                                        GenericUserType {
                                            id: self
                                                .find_user_type_in(name, ty.body_scope)
                                                .unwrap(),
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

        Self::coerce_expr($scopes, source, $target)
    }};
}

macro_rules! type_check {
    ($self: expr, $scopes: expr, $source: expr, $target: expr, $span: expr) => {{
        let source = $source;
        if !source.ty.coerces_to($scopes, $target) {
            $self.error::<()>(type_mismatch!($scopes, $target, source.ty, $span))
        }

        Self::coerce_expr($scopes, source, $target)
    }};
}

macro_rules! user_type {
    ($self: expr, $scopes: expr, $id: expr, $span: expr) => {
        $scopes.get_user_type(match &$id {
            TypeId::UserType(data) => data.id,
            _ => {
                return $self.error(Error::new(
                    format!("cannot get member of type '{}'", $id.name($scopes)),
                    $span,
                ));
            }
        })
    };
}

macro_rules! resolve_forward_declare {
    ($self: ident, $scopes: expr, $checked: expr, $unchecked: expr) => {
        if $checked == TypeId::Unknown {
            $checked = $self.resolve_type($scopes, $unchecked);
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
                            self.forward_declare(scopes, stmt);
                        }
                    }
                    _ => {
                        self.forward_declare(scopes, &file.ast);
                    }
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

    fn forward_declare(&mut self, scopes: &mut Scopes, stmt: &Located<Stmt>) {
        match &stmt.data {
            Stmt::Module { public, name, body } => {
                scopes.enter(Some(name.clone()), ScopeKind::Module(*public), |scopes| {
                    for stmt in body {
                        self.forward_declare(scopes, stmt)
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
                                ty: Self::fd_resolve_type(scopes, &member.ty)
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

                    let self_id = scopes.insert_user_type(UserType {
                        name: base.name.clone(),
                        public: base.public,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Struct(members),
                        type_params: base.type_params.clone(),
                    });
                    let parent = scopes.current_id();
                    scopes.enter(
                        Some(base.name.clone()),
                        ScopeKind::UserType(self_id),
                        |scopes| {
                            scopes.get_user_type_mut(self_id).body_scope = scopes.current_id();

                            for (i, param) in base.type_params.iter().enumerate() {
                                scopes.insert_user_type(UserType {
                                    public: false,
                                    name: param.clone(),
                                    body_scope: scopes.current_id(),
                                    data: UserTypeData::StructGeneric(i),
                                    type_params: Vec::new(),
                                });
                            }

                            self.forward_declare_fn(
                                scopes,
                                parent,
                                true,
                                &Fn {
                                    proto: Prototype {
                                        public: base.public,
                                        name: base.name.clone(),
                                        is_async: false,
                                        is_extern: false,
                                        type_params: base.type_params.clone(),
                                        params,
                                        ret: TypeHint::Regular {
                                            is_dyn: false,
                                            path: Located::new(
                                                Path {
                                                    components: vec![(
                                                        base.name.clone(),
                                                        base.type_params
                                                            .iter()
                                                            .map(|name| TypeHint::Regular {
                                                                is_dyn: false,
                                                                path: Located::new(
                                                                    Path::from(name.clone()),
                                                                    stmt.span,
                                                                ),
                                                            })
                                                            .collect(),
                                                    )],
                                                    root: false,
                                                },
                                                stmt.span,
                                            ),
                                        },
                                    },
                                    body: Vec::new(),
                                },
                            );

                            for f in base.functions.iter() {
                                self.forward_declare_fn(scopes, scopes.current_id(), false, f);
                            }
                        },
                    )
                }
                ParsedUserType::Union { tag, base } => {
                    let mut variants = Vec::with_capacity(base.members.len());
                    for (name, member) in base.members.iter() {
                        variants.push((
                            name.clone(),
                            Member {
                                public: member.public,
                                ty: Self::fd_resolve_type(scopes, &member.ty)
                                    .unwrap_or(TypeId::Unknown)
                                    .clone(),
                            },
                        ));
                    }

                    let self_id = scopes.insert_user_type(UserType {
                        name: base.name.clone(),
                        public: base.public,
                        body_scope: ScopeId(0),
                        data: UserTypeData::Union(Union { variants }),
                        type_params: base.type_params.clone(),
                    });
                    scopes.enter(
                        Some(base.name.clone()),
                        ScopeKind::UserType(self_id),
                        |scopes| {
                            scopes.get_user_type_mut(self_id).body_scope = scopes.current_id();

                            for (i, param) in base.type_params.iter().enumerate() {
                                scopes.insert_user_type(UserType {
                                    public: false,
                                    name: param.clone(),
                                    body_scope: scopes.current_id(),
                                    data: UserTypeData::StructGeneric(i),
                                    type_params: Vec::new(),
                                });
                            }

                            for (name, member) in base.members.iter() {
                                // TODO: generic params
                                self.forward_declare_fn(
                                    scopes,
                                    scopes.current_id(),
                                    true,
                                    &Fn {
                                        proto: Prototype {
                                            public: true,
                                            name: name.clone(),
                                            is_async: false,
                                            is_extern: false,
                                            type_params: base.type_params.clone(),
                                            params: vec![Param {
                                                mutable: false,
                                                keyword: false,
                                                name: name.clone(),
                                                ty: member.ty.clone(),
                                                default: None,
                                            }],
                                            ret: TypeHint::Regular {
                                                is_dyn: false,
                                                path: Located::new(
                                                    Path {
                                                        components: vec![(
                                                            base.name.clone(),
                                                            base.type_params
                                                                .iter()
                                                                .map(|name| TypeHint::Regular {
                                                                    is_dyn: false,
                                                                    path: Located::new(
                                                                        Path::from(name.clone()),
                                                                        stmt.span,
                                                                    ),
                                                                })
                                                                .collect(),
                                                        )],
                                                        root: false,
                                                    },
                                                    stmt.span,
                                                ),
                                            },
                                        },
                                        body: Vec::new(),
                                    },
                                );
                            }

                            for f in base.functions.iter() {
                                self.forward_declare_fn(scopes, scopes.current_id(), false, f);
                            }
                        },
                    )
                }
                ParsedUserType::Interface { .. } => todo!(),
                ParsedUserType::Enum {
                    public,
                    name,
                    impls: _,
                    variants,
                    functions,
                } => {
                    let id = scopes.insert_user_type(UserType {
                        public: *public,
                        name: name.clone(),
                        body_scope: ScopeId(0),
                        data: UserTypeData::Enum,
                        type_params: Vec::new(),
                    });

                    scopes.enter(Some(name.clone()), ScopeKind::UserType(id), |scopes| {
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
                            self.forward_declare_fn(scopes, scopes.current_id(), false, f);
                        }
                    });
                }
            },
            Stmt::Fn(f) => self.forward_declare_fn(scopes, scopes.current_id(), false, f),
            Stmt::Static { public, name, .. } => {
                scopes.insert_var(Variable {
                    name: name.clone(),
                    public: *public,
                    ty: TypeId::Unknown,
                    is_static: true,
                    mutable: false,
                    value: None,
                });
            }
            _ => {}
        }
    }

    fn forward_declare_fn(
        &mut self,
        scopes: &mut Scopes,
        scope: ScopeId,
        constructor: bool,
        f: &Fn,
    ) {
        let checked = Function {
            proto: CheckedPrototype {
                public: f.proto.public,
                name: f.proto.name.clone(),
                is_async: f.proto.is_async,
                is_extern: f.proto.is_extern,
                type_params: f.proto.type_params.clone(),
                params: f
                    .proto
                    .params
                    .iter()
                    .map(|param| CheckedParam {
                        mutable: param.mutable,
                        keyword: param.keyword,
                        name: param.name.clone(),
                        ty: Self::fd_resolve_type(scopes, &param.ty).unwrap_or(TypeId::Unknown),
                    })
                    .collect(),
                ret: Self::fd_resolve_type(scopes, &f.proto.ret).unwrap_or(TypeId::Unknown),
            },
            body: None,
            constructor,
        };

        let id = scopes.insert_func_in(checked, scope);
        if f.proto.is_extern {
            return;
        }

        scopes.enter(
            Some(f.proto.name.clone()),
            ScopeKind::Function(id),
            |scopes| {
                if !f.proto.type_params.is_empty() {
                    for (i, param) in f.proto.type_params.iter().enumerate() {
                        scopes.insert_user_type(UserType {
                            public: false,
                            name: param.clone(),
                            body_scope: scopes.current_id(),
                            data: UserTypeData::FuncGeneric(i),
                            type_params: Vec::new(),
                        });
                    }

                    for (i, original) in f.proto.params.iter().enumerate() {
                        scopes.get_func_mut(id).proto.params[i].ty =
                            Self::fd_resolve_type(scopes, &original.ty).unwrap_or(TypeId::Unknown);
                    }

                    scopes.get_func_mut(id).proto.ret =
                        Self::fd_resolve_type(scopes, &f.proto.ret).unwrap_or(TypeId::Unknown);
                }

                for stmt in f.body.iter() {
                    self.forward_declare(scopes, stmt);
                }

                if !constructor {
                    scopes.get_func_mut(id).body = Some(Block {
                        body: Vec::new(),
                        scope: scopes.current_id(),
                    });
                }
            },
        )
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
                    let self_id = scopes.find_user_type(&base.name).unwrap();
                    let parent = scopes.current_id();
                    scopes.enter_id(scopes.get_user_type(self_id).body_scope, |scopes| {
                        for i in 0..base.members.len() {
                            resolve_forward_declare!(
                                self,
                                scopes,
                                scopes
                                    .get_user_type_mut(self_id)
                                    .data
                                    .as_struct_mut()
                                    .unwrap()[i]
                                    .1
                                    .ty,
                                &base.members[i].1.ty
                            );
                        }

                        let init = *scopes[parent]
                            .fns
                            .iter()
                            .find(|&&f| {
                                let f = scopes.get_func(f);
                                f.constructor &&
                                    matches!(&f.proto.ret, TypeId::UserType(ty) if ty.id == self_id)
                            })
                            .unwrap();

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
                    });
                    CheckedStmt::None
                }
                ParsedUserType::Union { tag, base } => {
                    let self_id = scopes.find_user_type(&base.name).unwrap();
                    scopes.enter_id(scopes.get_user_type(self_id).body_scope, |scopes| {
                        for i in 0..base.members.len() {
                            resolve_forward_declare!(
                                self,
                                scopes,
                                scopes
                                    .get_user_type_mut(self_id)
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
                            resolve_forward_declare!(
                                self,
                                scopes,
                                scopes.get_func_mut(init).proto.params[0].ty,
                                &member.ty
                            );
                        }

                        for f in base.functions {
                            self.check_fn(scopes, f);
                        }
                    });

                    CheckedStmt::None
                }
                ParsedUserType::Interface { .. } => todo!(),
                ParsedUserType::Enum {
                    name,
                    impls: _,
                    variants,
                    functions,
                    ..
                } => {
                    let id = scopes.find_user_type(&name).unwrap();
                    scopes.enter_id(scopes.get_user_type(id).body_scope, |scopes| {
                        for (name, expr) in variants {
                            scopes.get_var_mut(scopes.find_var(&name).unwrap()).value = expr
                                .map(|expr| self.check_expr(scopes, expr, Some(&TypeId::Usize)));
                        }

                        for f in functions {
                            self.check_fn(scopes, f);
                        }
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
        }
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
                        TypeId::Int(_) | TypeId::Isize | TypeId::F32 | TypeId::F64
                    ),
                    PostIncrement | PostDecrement | PreIncrement | PreDecrement => {
                        if matches!(
                            expr.ty,
                            TypeId::Int(_) | TypeId::Isize | TypeId::Uint(_) | TypeId::Usize
                        ) {
                            if !Self::is_assignable(scopes, &expr) {
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
                        if !Self::can_addrmut(scopes, &expr) {
                            self.error::<()>(Error::new(
                                "cannot create mutable pointer to immutable memory location",
                                span,
                            ));
                        }

                        out_ty = Some(TypeId::MutPtr(expr.ty.clone().into()));
                        true
                    }
                    Unwrap => {
                        if let Some(inner) = Self::as_option_inner(scopes, &expr.ty) {
                            out_ty = Some(inner.clone());
                            true
                        } else {
                            self.error::<()>(Error::new(
                                "unwrap operator is only valid for option types",
                                span,
                            ));
                            false
                        }
                    }
                    Try => todo!(),
                    Sizeof => todo!(),
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
            Expr::Range { .. } => todo!(),
            Expr::String(s) => {
                CheckedExpr::new(Self::find_core_string(scopes).unwrap(), ExprData::String(s))
            }
            Expr::Char(s) => CheckedExpr::new(TypeId::Char, ExprData::Char(s)),
            Expr::None => {
                if let Some(inner) = target.and_then(|target| Self::as_option_inner(scopes, target))
                {
                    CheckedExpr::new(
                        Self::make_option(scopes, inner.clone()).unwrap(),
                        ExprData::Instance(
                            [(
                                "None".into(),
                                CheckedExpr::new(TypeId::Void, ExprData::Void),
                            )]
                            .into(),
                        ),
                    )
                } else {
                    self.error(Error::new("cannot infer type of option literal none", span))
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
                                        if let Some(inner) = Self::as_option_inner(scopes, other) {
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

                let (signed, bits) = match ty {
                    TypeId::Int(bits) => (true, bits),
                    TypeId::Uint(bits) => (false, bits),
                    TypeId::Isize => (true, std::mem::size_of::<isize>() as u8 * 8),
                    TypeId::Usize => (false, std::mem::size_of::<usize>() as u8 * 8),
                    _ => unreachable!(),
                };

                if signed {
                    let result = match i128::from_str_radix(&value, base as u32) {
                        Ok(result) => result,
                        Err(e) => {
                            return self.error(Error::new(
                                format!("Integer literal '{value}' is too large: {e}."),
                                expr.span,
                            ));
                        }
                    };

                    if result >= 1 << (bits - 1) {
                        return self.error(Error::new(
                            "Integer literal is larger than its type allows",
                            expr.span,
                        ));
                    }
                    if result <= -(1 << (bits - 1)) {
                        return self.error(Error::new(
                            "Integer literal is smaller than its type allows",
                            expr.span,
                        ));
                    }

                    CheckedExpr::new(ty, ExprData::Signed(result))
                } else {
                    let result = match u128::from_str_radix(&value, base as u32) {
                        Ok(result) => result,
                        Err(_) => {
                            return self.error(Error::new(
                                format!("Integer literal '{value}' is too large."),
                                expr.span,
                            ));
                        }
                    };

                    if result >= 1 << bits {
                        return self.error(Error::new(
                            "Integer literal is larger than its type allows",
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
            Expr::Path(path) => match self.resolve_path(scopes, &path, span) {
                Ok(Some(Symbol::Var(id))) => CheckedExpr::new(
                    scopes.get_var(id).ty.clone(),
                    ExprData::Symbol(Symbol::Var(id)),
                ),
                Ok(Some(Symbol::Func(id))) => CheckedExpr::new(
                    TypeId::Func(id.clone().into()),
                    ExprData::Symbol(Symbol::Func(id)),
                ),
                Err(err) => self.error(err),
                Ok(None) => self.error(Error::new("type not found in this scope", span)),
            },
            Expr::Assign {
                target: lhs,
                binary,
                value,
            } => {
                let span = lhs.span;
                let lhs = self.check_expr(scopes, *lhs, None);
                if !Self::is_assignable(scopes, &lhs) {
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
                    self.create_block(scopes, None, body, ScopeKind::Block(target.cloned(), false));
                let ScopeKind::Block(target, _) = &scopes[block.scope].kind else {
                    panic!("ICE: target of block changed from block to something else");
                };
                CheckedExpr::new(
                    target.clone().unwrap_or(TypeId::Void),
                    ExprData::Block(block),
                )
            }
            Expr::If {
                cond,
                if_branch,
                else_branch,
            } => {
                /* FIXME: type inference for cases like this:
                    let foo = 5;
                    let x: ?i64 = if foo {
                        yield 10;
                    };
                */
                let cond_span = cond.span;
                let cond = type_check!(
                    self,
                    scopes,
                    self.check_expr(scopes, *cond, Some(&TypeId::Bool)),
                    &TypeId::Bool,
                    cond_span
                );

                let if_branch = self.check_expr(scopes, *if_branch, None);
                let mut out_type = if_branch.ty.clone();
                let else_branch = if let Some(e) = else_branch {
                    Some(type_check!(
                        self,
                        scopes,
                        self.check_expr(scopes, *e, Some(&if_branch.ty)),
                        &if_branch.ty,
                        span
                    ))
                } else {
                    // this separates these two cases:
                    //   let x /* void? */ = if whatever { yield void; };
                    //   let x /* void */ = if whatever { };
                    if matches!(&if_branch.data, ExprData::Block(b) if
                        matches!(scopes[b.scope].kind, ScopeKind::Block(_, yields) if yields))
                    {
                        out_type = Self::make_option(scopes, out_type).unwrap();
                    }
                    None
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
                /* FIXME: type inference for cases like this:
                    let a = 5;
                    let x: ?i64 = loop 10 < 2 {
                        if a != 2 {
                            break 10;
                        } else {
                            break 11;
                        }
                    };
                */
                let span = cond.span;
                let cond = type_check!(
                    self,
                    scopes,
                    self.check_expr(scopes, *cond, Some(&TypeId::Bool)),
                    &TypeId::Bool,
                    span
                );

                let body = self.create_block(
                    scopes,
                    None,
                    body,
                    ScopeKind::Loop(None, matches!(cond.data, ExprData::Bool(true))),
                );
                let ScopeKind::Loop(target, inf) = &scopes[body.scope].kind else {
                    panic!("ICE: target of loop changed from loop to something else");
                };

                CheckedExpr::new(
                    target
                        .clone()
                        .unwrap_or(if *inf { TypeId::Never } else { TypeId::Void }),
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
                let ty = user_type!(self, scopes, id, span);

                if let Some(members) = ty.data.as_struct() {
                    if let Some((_, var)) = members.iter().find(|m| m.0 == member) {
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
                                source: Self::auto_ref(source, &id).into(),
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

                let ScopeKind::Loop(target, inf) = scopes[scope].kind.clone() else {
                    unreachable!()
                };

                let span = expr.span;
                let mut expr = self.check_expr(scopes, *expr, target.as_ref());
                if let Some(target) = &target {
                    expr = type_check!(self, scopes, expr, target, span);
                } else if inf {
                    scopes[scope].kind = ScopeKind::Loop(Some(expr.ty.clone()), inf);
                } else {
                    scopes[scope].kind = ScopeKind::Loop(
                        Some(Self::make_option(scopes, expr.ty.clone()).unwrap()),
                        inf,
                    );
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
            Expr::Is { expr, pattern } => todo!(),
            Expr::Match { expr, body } => {
                let scrutinee_span = expr.span;
                let scrutinee = self.check_expr(scopes, *expr, None);
                let mut target = target.cloned().unwrap_or(TypeId::Unknown);
                let mut result = Vec::new();
                for (pattern, expr) in body.into_iter() {
                    let span = expr.span;
                    let (var, variant) =
                        self.check_pattern(scopes, &scrutinee, scrutinee_span, pattern);
                    let (var, mut expr) = scopes.enter(None, ScopeKind::None, |scopes| {
                        let var = var.map(|var| scopes.insert_var(var));
                        (var, self.check_expr(scopes, expr, Some(&target)))
                    });

                    if target.is_unknown() {
                        target = expr.ty.clone();
                    } else {
                        expr = type_check!(self, scopes, expr, &target, span);
                    }

                    result.push((
                        UnionPattern {
                            binding: var,
                            variant,
                        },
                        expr,
                    ));
                }

                CheckedExpr::new(
                    target,
                    ExprData::Match {
                        expr: scrutinee.into(),
                        body: result,
                    },
                )
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

        let TypeHint::Regular { is_dyn: _, mut path } = pattern.ty else {
            return self.error(Error::new("invalid pattern, must be a union variant", span));
        };

        if path.data.components.len() < 2 {
            return self.error(Error::new("invalid pattern, must be a union variant", span));
        }

        let scope = match Self::resolve_path_to_end(scopes, &path.data, span) {
            Ok(scope) => scope,
            Err(err) => return self.error(err),
        };

        let Some(union) = scopes[scope].kind
            .as_user_type()
            .filter(|&&id| id == ut.id)
            .and_then(|&id| scopes.get_user_type(id).data.as_union()) else {
            return self.error(Error::new("pattern does not match the scrutinee", span));
        };

        let name = path.data.components.pop().unwrap().0;
        let Some((_, member)) = union.variants
            .iter()
            .find(|(m, _)| m == &name) else {
            return self.error(
                Error::new(
                    format!("type {} has no variant {name}",
                    scrutinee.ty.name(scopes)),
                    span
                )
            );
        };

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
                format!("union variant {name} has data that must be bound"),
                span,
            ))
        }
    }

    fn auto_ref(mut source: CheckedExpr, target: &TypeId) -> CheckedExpr {
        if !matches!(source.ty, TypeId::Ptr(_) | TypeId::MutPtr(_)) {
            source = CheckedExpr::new(
                target.clone(),
                ExprData::Unary {
                    op: if matches!(source.ty, TypeId::Ptr(_)) {
                        UnaryOp::Addr
                    } else {
                        UnaryOp::AddrMut
                    },
                    expr: source.into(),
                },
            )
        } else {
            #[allow(clippy::redundant_clone)]
            let mut ty = source.ty.clone();
            while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = &ty {
                if &ty == target {
                    break;
                }

                source = CheckedExpr::new(
                    (**inner).clone(),
                    ExprData::Unary {
                        op: UnaryOp::Deref,
                        expr: source.into(),
                    },
                );
                ty = source.ty.clone();
            }
        }

        source
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
            let source = self.check_expr(scopes, *source, None);
            let id = source.ty.strip_references().clone();
            let ty = user_type!(self, scopes, id, span);
            if let Some(func) = ty
                .data
                .as_struct()
                .map(|_| ())
                .or_else(|| ty.data.is_enum().then_some(()))
                .and_then(|_| scopes.find_func_in(&member, ty.body_scope))
            {
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

                if let Some(this) = f.proto.params.get(0).filter(|p| p.name == THIS_PARAM) {
                    if let TypeId::MutPtr(inner) = &this.ty {
                        let mut ty = &source.ty;
                        if ty == inner.as_ref() && !Self::can_addrmut(scopes, &source) {
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

                    let mut result = vec![Self::auto_ref(source, &this.ty)];
                    let mut func = match self.resolve_func(scopes, func, &generics, span) {
                        Ok(symbol) => symbol,
                        Err(error) => return self.error(error),
                    };
                    let params = &f.proto.params[1..].to_vec();
                    let (hargs, ret) = self.check_fn_args(
                        id.as_user_type().map(|inner| inner.as_ref()),
                        &mut func,
                        args,
                        params,
                        target,
                        scopes,
                        span,
                    );

                    result.append(&mut Self::make_positional(params, hargs));
                    return CheckedExpr::new(
                        ret,
                        ExprData::Call {
                            func,
                            inst: id.as_user_type().map(|ty| (**ty).clone()),
                            args: result,
                        },
                    );
                }
            }

            self.error(Error::new(
                format!("no method '{member}' found on type '{}'", id.name(scopes)),
                span,
            ))
        } else {
            let callee = self.check_expr(scopes, callee, None);
            match callee.ty {
                TypeId::Func(mut func) => {
                    let f = scopes.get_func(func.id);
                    let constructor = f.constructor;
                    if constructor {
                        let ret = f.proto.ret.clone();
                        let ut = scopes.get_user_type(ret.as_user_type().unwrap().id);
                        if let Some(st) = ut.data.as_struct() {
                            if st.iter().any(|member| !member.1.public)
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

                    let params = &f.proto.params.clone();
                    let (args, ret) =
                        self.check_fn_args(None, &mut func, args, params, target, scopes, span);

                    CheckedExpr::new(
                        ret,
                        if constructor {
                            ExprData::Instance(args)
                        } else {
                            ExprData::Call {
                                func: *func,
                                args: Self::make_positional(params, args),
                                inst: None,
                            }
                        },
                    )
                }
                _ => self.error(Error::new(
                    format!("cannot call value of type '{}'", &callee.ty.name(scopes)),
                    span,
                )),
            }
        }
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

    fn check_fn(&mut self, scopes: &mut Scopes, f: Fn) -> FunctionId {
        // TODO: disallow private type in public interface
        let id = scopes.find_func(&f.proto.name).unwrap();
        for i in 0..f.proto.params.len() {
            resolve_forward_declare!(
                self,
                scopes,
                scopes.get_func_mut(id).proto.params[i].ty,
                &f.proto.params[i].ty
            );
        }

        resolve_forward_declare!(
            self,
            scopes,
            scopes.get_func_mut(id).proto.ret,
            &f.proto.ret
        );

        if f.proto.is_extern {
            return id;
        }

        scopes.get_func_mut(id).body.as_mut().unwrap().body =
            scopes.enter_id(scopes.get_func(id).body.as_ref().unwrap().scope, |scopes| {
                let params: Vec<_> = scopes
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
                    .collect();

                for param in params {
                    scopes.insert_var(param);
                }

                f.body
                    .into_iter()
                    .map(|stmt| self.check_stmt(scopes, stmt))
                    .collect()
            });

        id
    }

    fn check_arg(
        &mut self,
        func: &mut GenericFunc,
        scopes: &mut Scopes,
        expr: Located<Expr>,
        param: &CheckedParam,
        instance: Option<&GenericUserType>,
    ) -> CheckedExpr {
        let mut target = param.ty.clone();
        if !func.generics.is_empty() {
            target.fill_func_generics(scopes, func);
        }

        if let Some(instance) = instance {
            target.fill_type_generics(scopes, instance);
        }

        let span = expr.span;
        let expr = self.check_expr(scopes, expr, Some(&target));
        if !func.generics.is_empty() {
            func.infer_generics(&param.ty, &expr.ty, scopes);
            target.fill_func_generics(scopes, func);
        }

        if let Some(instance) = instance {
            target.fill_type_generics(scopes, instance);
        }

        type_check_bail!(self, scopes, expr, &target, span)
    }

    #[allow(clippy::too_many_arguments)]
    fn check_fn_args(
        &mut self,
        instance: Option<&GenericUserType>,
        func: &mut GenericFunc,
        args: Vec<(Option<String>, Located<Expr>)>,
        params: &[CheckedParam],
        target: Option<&TypeId>,
        scopes: &mut Scopes,
        span: Span,
    ) -> (HashMap<String, CheckedExpr>, TypeId) {
        let mut result = HashMap::with_capacity(args.len());
        let mut last_pos = 0;
        for (name, expr) in args {
            if let Some(name) = name {
                match result.entry(name.clone()) {
                    Entry::Occupied(_) => {
                        self.error::<()>(Error::new(
                            format!("parameter {name} has already been specified"),
                            expr.span,
                        ));
                    }
                    Entry::Vacant(entry) => {
                        if let Some(param) = params.iter().find(|p| p.name == name) {
                            entry.insert(self.check_arg(func, scopes, expr, param, instance));
                        } else {
                            self.error::<()>(Error::new(
                                format!("unknown parameter: {name}"),
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
                    self.check_arg(func, scopes, expr, param, instance),
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
            self.error::<()>(Error::new(
                format!(
                    "expected {} argument(s), found {}",
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
                            "cannot infer type of generic parameter {}",
                            scopes.get_func(func.id).proto.type_params[i]
                        ),
                        span,
                    ));
                }
            }
        }

        if let Some(instance) = instance {
            ret.fill_type_generics(scopes, instance);
        }

        (result, ret)
    }

    fn make_positional(
        params: &[CheckedParam],
        mut args: HashMap<String, CheckedExpr>,
    ) -> Vec<CheckedExpr> {
        if params.len() == args.len() {
            let mut result = Vec::with_capacity(args.len());
            for param in params {
                result.push(args.remove(&param.name).unwrap());
            }
            result
        } else {
            Vec::new()
        }
    }

    fn fd_resolve_type<'b>(scopes: &Scopes, ty: &'b TypeHint) -> Result<TypeId, ResolveError<'b>> {
        Ok(match ty {
            TypeHint::Regular { path, .. } => {
                return Self::resolve_type_path(scopes, &path.data, path.span)?
                    .or_else(|| {
                        path.data.as_identifier().and_then(|symbol| match symbol {
                            symbol if symbol == THIS_TYPE => scopes.this_type(),
                            "void" => Some(TypeId::Void),
                            "never" => Some(TypeId::Never),
                            "f32" => Some(TypeId::F32),
                            "f64" => Some(TypeId::F64),
                            "bool" => Some(TypeId::Bool),
                            "str" => Self::find_core_string(scopes),
                            "char" => Some(TypeId::Char),
                            _ => TypeId::from_int_name(symbol),
                        })
                    })
                    .ok_or(ResolveError::Path(path));
            }
            TypeHint::Void => TypeId::Void,
            TypeHint::Ptr(ty) => TypeId::Ptr(Self::fd_resolve_type(scopes, ty)?.into()),
            TypeHint::MutPtr(ty) => TypeId::MutPtr(Self::fd_resolve_type(scopes, ty)?.into()),
            TypeHint::This => {
                // the parser ensures methods can only appear in structs/enums/etc
                scopes
                    .this_type()
                    .map(|s| TypeId::Ptr(s.into()))
                    .expect("ICE: this outside of method")
            }
            TypeHint::MutThis => scopes
                .this_type()
                .map(|s| TypeId::MutPtr(s.into()))
                .expect("ICE: this outside of method"),
            TypeHint::Array(ty, count) => {
                let n = Self::consteval(scopes, count, Some(&TypeId::Usize))?;
                TypeId::Array((Self::fd_resolve_type(scopes, ty)?, n).into())
            }
            TypeHint::Option(ty) => Self::make_option(scopes, Self::fd_resolve_type(scopes, ty)?)
                .ok_or(ResolveError::Error(Error::new(
                "ICE: core::option::Option not found",
                Span::default(),
            )))?,
            _ => todo!(),
        })
    }

    fn resolve_type(&mut self, scopes: &Scopes, ty: &TypeHint) -> TypeId {
        Self::fd_resolve_type(scopes, ty).unwrap_or_else(|err| match err {
            ResolveError::Error(err) => self.error(err),
            ResolveError::Path(name) => self.error(Error::new("undefined type", name.span)),
        })
    }

    fn find_core_option(scopes: &Scopes) -> Option<UserTypeId> {
        let core = scopes.scopes()[0].children.get("core")?;
        let option = scopes[*core].children.get("option")?;
        scopes.find_user_type_in("Option", *option)
    }

    fn find_core_string(scopes: &Scopes) -> Option<TypeId> {
        let core = scopes.scopes()[0].children.get("core")?;
        let option = scopes[*core].children.get("string")?;
        Some(TypeId::UserType(
            GenericUserType {
                id: scopes.find_user_type_in("str", *option)?,
                generics: vec![],
            }
            .into(),
        ))
    }

    fn make_option(scopes: &Scopes, ty: TypeId) -> Option<TypeId> {
        Some(TypeId::UserType(
            GenericUserType {
                id: Self::find_core_option(scopes).unwrap(),
                generics: vec![ty],
            }
            .into(),
        ))
    }

    fn as_option_inner<'a>(scopes: &Scopes, ty: &'a TypeId) -> Option<&'a TypeId> {
        Self::find_core_option(scopes).and_then(|opt| {
            ty.as_user_type()
                .filter(|ut| ut.id == opt)
                .map(|ut| &ut.generics[0])
        })
    }

    fn create_block(
        &mut self,
        scopes: &mut Scopes,
        name: Option<String>,
        body: Vec<Located<Stmt>>,
        kind: ScopeKind,
    ) -> Block {
        scopes.enter(name, kind, |scopes| Block {
            body: body
                .into_iter()
                .map(|stmt| self.check_stmt(scopes, stmt))
                .collect(),
            scope: scopes.current_id(),
        })
    }

    fn error<T: Default>(&mut self, error: Error) -> T {
        self.errors.push(error);
        T::default()
    }

    fn is_assignable(scopes: &Scopes, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, TypeId::MutPtr(_))
            }
            ExprData::Symbol(_) | ExprData::Member { .. } => Self::can_addrmut(scopes, expr),
            ExprData::Subscript { callee, .. } => Self::is_assignable(scopes, callee),
            _ => false,
        }
    }

    fn can_addrmut(scopes: &Scopes, expr: &CheckedExpr) -> bool {
        match &expr.data {
            ExprData::Unary { op, expr } => {
                !matches!(op, UnaryOp::Deref) || matches!(expr.ty, TypeId::MutPtr(_))
            }
            ExprData::Symbol(symbol) => match symbol {
                Symbol::Func(_) => false,
                Symbol::Var(id) => scopes.get_var(*id).mutable,
            },
            ExprData::Member { source, .. } => {
                matches!(source.ty, TypeId::MutPtr(_)) || Self::can_addrmut(scopes, source)
            }
            ExprData::Subscript { callee, .. } => Self::can_addrmut(scopes, callee),
            _ => true,
        }
    }

    fn resolve_path_to_end(scopes: &Scopes, path: &Path, span: Span) -> Result<ScopeId, Error> {
        let mut scope = ScopeId(0);
        let mut start = 0;
        if !path.root {
            if path.components[0].0 == "super" {
                start = 1;
                if let Some(module) = scopes.module_of(
                    scopes[scopes.module_of(scopes.current_id()).unwrap()]
                        .parent
                        .unwrap(),
                ) {
                    scope = module;
                } else {
                    return Err(Error::new("cannot use super here", span));
                }
            } else {
                scope = scopes.module_of(scopes.current_id()).unwrap();
            }
        }

        'outer: for part in path.components[start..path.components.len() - 1].iter() {
            for (name, &id) in scopes[scope].children.iter() {
                if name == &part.0 {
                    match scopes[id].kind {
                        ScopeKind::Module(public) => {
                            if !public
                                && scopes.module_of(scopes.current_id())
                                    != scopes.module_of(scopes[id].parent.unwrap())
                            {
                                return Err(Error::new(
                                    format!("cannot access private module {name}"),
                                    span,
                                ));
                            }
                        }
                        ScopeKind::UserType(id) => {
                            let ty = scopes.get_user_type(id);
                            if !ty.public
                                && scopes.module_of(ty.scope)
                                    != scopes.module_of(scopes.current_id())
                            {
                                return Err(Error::new(
                                    format!("cannot access members of private struct {name}"),
                                    span,
                                ));
                            }
                        }
                        _ => continue,
                    }

                    scope = id;
                    continue 'outer;
                }
            }

            return Err(Error::new(
                format!("'{}' not found in this scope", part.0),
                span,
            ));
        }

        Ok(scope)
    }

    fn resolve_func(
        &mut self,
        scopes: &Scopes,
        id: FunctionId,
        generics: &[TypeHint],
        span: Span,
    ) -> Result<GenericFunc, Error> {
        let func = scopes.get_func(id);
        if !func.proto.type_params.is_empty() {
            if generics.is_empty() {
                Ok(GenericFunc {
                    id,
                    generics: vec![TypeId::Unknown; func.proto.type_params.len()],
                })
            } else if generics.len() != func.proto.type_params.len() {
                Err(Error::new(
                    format!(
                        "expected {} generic arguments, received {}",
                        func.proto.type_params.len(),
                        generics.len()
                    ),
                    span,
                ))
            } else {
                Ok(GenericFunc::new(
                    id,
                    generics
                        .iter()
                        .map(|ty| self.resolve_type(scopes, ty))
                        .collect(),
                ))
            }
        } else {
            Ok(GenericFunc::new(id, vec![]))
        }
    }

    fn resolve_path(
        &mut self,
        scopes: &Scopes,
        path: &Path,
        span: Span,
    ) -> Result<Option<Symbol>, Error> {
        let last = path.components.last().unwrap();
        let var = if path.components.len() == 1 {
            if let Some(var) = scopes.find_var(&last.0) {
                Some(var)
            } else if let Some(func) = scopes.find_func(&last.0) {
                return Ok(Some(Symbol::Func(
                    self.resolve_func(scopes, func, &last.1, span)?,
                )));
            } else {
                None
            }
        } else {
            let scope = Self::resolve_path_to_end(scopes, path, span)?;
            if let Some(id) = scopes.find_var_in(&last.0, scope) {
                let var = scopes.get_var(id);
                if !var.public {
                    return Err(Error::new(
                        format!("variable '{}' is private", var.name),
                        span,
                    ));
                }

                Some(id)
            } else if let Some(id) = scopes.find_func_in(&last.0, scope) {
                let func = scopes.get_func(id);
                if !func.proto.public {
                    return Err(Error::new(
                        format!("function '{}' is private", func.proto.name),
                        span,
                    ));
                }

                return Ok(Some(Symbol::Func(
                    self.resolve_func(scopes, id, &last.1, span)?,
                )));
            } else {
                None
            }
        };

        if !path.components.last().unwrap().1.is_empty() {
            return Err(Error::new(
                "variables cannot be parameterized with generics",
                span,
            ));
        }

        Ok(var.map(Symbol::Var))
    }

    fn resolve_type_path<'b>(
        scopes: &Scopes,
        path: &'b Path,
        span: Span,
    ) -> Result<Option<TypeId>, ResolveError<'b>> {
        let last = path.components.last().unwrap();
        let ty = if path.components.len() == 1 {
            scopes.find_user_type(&last.0)
        } else {
            // TODO: struct generic params
            let scope = Self::resolve_path_to_end(scopes, path, span)?;
            if let Some(id) = scopes.find_user_type_in(&last.0, scope) {
                let ty = scopes.get_user_type(id);
                if !ty.public {
                    return Err(ResolveError::Error(Error::new(
                        format!("type '{}' is private", ty.name),
                        span,
                    )));
                }

                Some(id)
            } else {
                None
            }
        };

        if let Some(id) = ty {
            let params = &scopes.get_user_type(id).type_params;
            if last.1.len() != params.len() {
                return Err(ResolveError::Error(Error::new(
                    format!(
                        "expected {} generic arguments, received {}",
                        params.len(),
                        last.1.len()
                    ),
                    span,
                )));
            }

            if !last.1.is_empty() {
                let mut generics = Vec::new();
                for ty in last.1.iter() {
                    generics.push(Self::fd_resolve_type(scopes, ty)?);
                }

                Ok(Some(TypeId::UserType(
                    GenericUserType::new(id, generics).into(),
                )))
            } else {
                Ok(Some(TypeId::UserType(
                    GenericUserType::new(id, vec![]).into(),
                )))
            }
        } else {
            Ok(None)
        }
    }

    fn coerce_expr(scopes: &Scopes, expr: CheckedExpr, target: &TypeId) -> CheckedExpr {
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
                if Self::as_option_inner(scopes, target)
                    .map_or(false, |inner| ty.coerces_to(scopes, inner)) =>
            {
                let inner = Self::as_option_inner(scopes, target).unwrap();
                let expr = Self::coerce_expr(scopes, expr, inner);
                Self::coerce_expr(
                    scopes,
                    CheckedExpr::new(
                        Self::make_option(scopes, expr.ty.clone()).unwrap(),
                        ExprData::Instance([("Some".into(), expr)].into()),
                    ),
                    target,
                )
            }
            (TypeId::Never, _) => CheckedExpr::new(target.clone(), expr.data),
            _ => expr,
        }
    }
}
