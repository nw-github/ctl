use crate::{
    lexer::{Located, Span},
    THIS_TYPE,
};

use super::{Attribute, BinaryOp, UnaryOp};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Import,
    Export,
    #[default]
    Internal,
}

#[derive(Debug, Clone, Copy)]
pub enum PathOrigin {
    Root,
    Super(Span),
    Normal,
}

impl std::fmt::Display for PathOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Root => write!(f, "::"),
            Self::Super(_) => write!(f, "super::"),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UsePathTail {
    All,
    Ident(Located<String>),
}

#[derive(Debug, Clone)]
pub struct UsePath {
    pub public: bool,
    pub origin: PathOrigin,
    pub components: Vec<Located<String>>,
    pub tail: UsePathTail,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub data: StmtData,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub enum StmtData {
    Expr(Expr),
    Use(UsePath),
    Let {
        patt: Located<Pattern>,
        ty: Option<TypeHint>,
        value: Option<Expr>,
    },
    Fn(Fn),
    Struct(Struct),
    Union {
        tag: Option<Located<Path>>,
        base: Struct,
        variants: Vec<Variant>,
        is_unsafe: bool,
    },
    Trait {
        public: bool,
        name: Located<String>,
        is_unsafe: bool,
        type_params: Vec<(String, Vec<Located<Path>>)>,
        impls: Vec<Located<Path>>,
        functions: Vec<Fn>,
    },
    Extension {
        public: bool,
        name: Located<String>,
        ty: TypeHint,
        type_params: Vec<(String, Vec<Located<Path>>)>,
        impls: Vec<ImplBlock>,
        functions: Vec<Fn>,
    },
    Static {
        public: bool,
        name: Located<String>,
        ty: TypeHint,
        value: Expr,
    },
    Module {
        public: bool,
        name: Located<String>,
        body: Vec<Stmt>,
    },
    Error,
}

pub type Expr = Located<ExprData>;

#[derive(Debug, Clone)]
pub enum ExprData {
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Is {
        expr: Box<Expr>,
        pattern: Located<FullPattern>,
    },
    As {
        expr: Box<Expr>,
        ty: TypeHint,
        throwing: bool,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<(Option<String>, Expr)>,
    },
    Array(Vec<Expr>),
    Set(Vec<Expr>),
    Vec(Vec<Expr>),
    ArrayWithInit {
        init: Box<Expr>,
        count: Box<Expr>,
    },
    VecWithInit {
        init: Box<Expr>,
        count: Box<Expr>,
    },
    Tuple(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    Bool(bool),
    Integer {
        base: u8,
        value: String,
        width: Option<String>,
    },
    Float(String),
    String(String),
    Char(char),
    ByteString(Vec<u8>),
    ByteChar(u8),
    Path(Path),
    Void,
    None,
    Assign {
        target: Box<Expr>,
        binary: Option<BinaryOp>,
        value: Box<Expr>,
    },
    Block(Vec<Stmt>),
    If {
        cond: Box<Expr>,
        if_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    Loop {
        cond: Option<Box<Expr>>,
        body: Vec<Stmt>,
        do_while: bool,
    },
    For {
        patt: Located<Pattern>,
        iter: Box<Expr>,
        body: Vec<Stmt>,
    },
    Match {
        expr: Box<Expr>,
        body: Vec<(Located<FullPattern>, Expr)>,
    },
    Member {
        source: Box<Expr>,
        generics: Vec<TypeHint>,
        member: String,
    },
    Subscript {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Return(Box<Expr>),
    Tail(Box<Expr>),
    Break(Option<Box<Expr>>),
    Unsafe(Box<Expr>),
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,
    },
    Continue,
    Lambda {
        params: Vec<(Located<String>, Option<TypeHint>)>,
        ret: Option<TypeHint>,
        body: Box<Expr>,
        moves: bool,
    },
    Error,
}

pub type PathComponent = (String, Vec<TypeHint>);

#[derive(Clone, derive_more::Constructor)]
pub struct Path {
    pub origin: PathOrigin,
    pub components: Vec<PathComponent>,
}

impl Path {
    pub fn as_identifier(&self) -> Option<&str> {
        self.components
            .first()
            .filter(|_| matches!(self.origin, PathOrigin::Normal) && self.components.len() == 1)
            .filter(|(_, generics)| generics.is_empty())
            .map(|(path, _)| path.as_str())
    }
}

impl From<String> for Path {
    fn from(value: String) -> Self {
        Self::new(PathOrigin::Normal, vec![(value, Vec::new())])
    }
}

impl std::fmt::Debug for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.origin)?;
        for (i, (zero, one)) in self.components.iter().enumerate() {
            if i > 0 {
                write!(f, "::")?;
            }

            write!(f, "{}", zero)?;
            if one.is_empty() {
                return Ok(());
            }

            write!(f, "::<")?;
            for (i, generic) in one.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{generic:?}")?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Destructure {
    pub name: Located<String>,
    pub mutable: bool,
    pub pattern: Located<Pattern>,
}

#[derive(Debug, Clone)]
pub struct IntPattern {
    pub negative: bool,
    pub base: u8,
    pub value: String,
    pub width: Option<String>,
}

#[derive(Debug, Clone)]
pub struct RangePattern<T> {
    pub inclusive: bool,
    pub start: T,
    pub end: T,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    // x is ::core::opt::Option::Some(mut y)
    TupleLike {
        path: Located<Path>,
        subpatterns: Vec<Located<Pattern>>,
    },
    StructLike {
        path: Located<Path>,
        subpatterns: Vec<Destructure>,
    },
    // x is ::core::opt::Option::None
    // x is y
    Path(Path),
    // x is mut y
    MutBinding(String),
    // x is ?mut y
    Option(Box<Pattern>),
    // x is null
    Null,
    // let {x, y} = z;
    Struct(Vec<Destructure>),
    Tuple(Vec<Located<Pattern>>),
    Int(IntPattern),
    IntRange(RangePattern<IntPattern>),
    String(String),
    Char(char),
    CharRange(RangePattern<char>),
    Array(Vec<Located<Pattern>>),
    Rest(Option<(bool, Located<String>)>),
    Bool(bool),
    Void,
    Error,
}

#[derive(Debug, Clone)]
pub struct FullPattern {
    pub data: Pattern,
    pub if_expr: Option<Box<Expr>>,
}

#[derive(Clone)]
pub enum TypeHint {
    Regular(Located<Path>),
    Array(Box<TypeHint>, Box<Expr>),
    Vec(Box<TypeHint>),
    Slice(Box<TypeHint>),
    SliceMut(Box<TypeHint>),
    Tuple(Vec<TypeHint>),
    AnonStruct(Vec<(String, TypeHint)>),
    Set(Box<TypeHint>),
    Map(Box<TypeHint>, Box<TypeHint>),
    Option(Box<TypeHint>),
    Ptr(Box<TypeHint>),
    MutPtr(Box<TypeHint>),
    RawPtr(Box<TypeHint>),
    Fn {
        is_extern: bool,
        params: Vec<TypeHint>,
        ret: Box<TypeHint>,
    },
    Void,
    This(Span),
    Error,
}

impl std::fmt::Debug for TypeHint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeHint::Regular(path) => write!(f, "{:?}", path.data),
            TypeHint::Array(inner, _) => write!(f, "[{inner:?}; <expr>]"),
            TypeHint::Vec(inner) => write!(f, "[{inner:?}]"),
            TypeHint::Slice(inner) => write!(f, "[{inner:?}..]"),
            TypeHint::SliceMut(inner) => write!(f, "[mut {inner:?}..]"),
            TypeHint::Tuple(vals) => {
                write!(f, "(")?;
                for (i, inner) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{inner:?}")?;
                }
                write!(f, ")")
            }
            TypeHint::AnonStruct(vals) => {
                write!(f, "struct {{")?;
                for (i, (name, hint)) in vals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name}: {hint:?}")?;
                }
                write!(f, "}}")
            }
            TypeHint::Set(inner) => write!(f, "{{{inner:?}}}"),
            TypeHint::Map(key, val) => write!(f, "[{key:?}: {val:?}]"),
            TypeHint::Option(inner) => write!(f, "?{inner:?}"),
            TypeHint::Ptr(inner) => write!(f, "*{inner:?}"),
            TypeHint::MutPtr(inner) => write!(f, "*mut {inner:?}"),
            TypeHint::RawPtr(inner) => write!(f, "*raw {inner:?}"),
            TypeHint::Fn {
                is_extern,
                params,
                ret,
            } => {
                write!(f, "{}fn (", if *is_extern { "extern " } else { "" })?;
                for (i, inner) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{inner:?}")?;
                }
                write!(f, ") {ret:?}")
            }
            TypeHint::Void => write!(f, "void"),
            TypeHint::This(_) => write!(f, "{THIS_TYPE}"),
            TypeHint::Error => write!(f, "Error"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub keyword: bool,
    pub patt: Located<Pattern>,
    pub ty: TypeHint,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub public: bool,
    pub name: Located<String>,
    pub linkage: Linkage,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    pub type_params: Vec<(String, Vec<Located<Path>>)>,
    pub params: Vec<Param>,
    pub ret: TypeHint,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub name: Located<String>,
    pub ty: TypeHint,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum VariantData {
    Empty,
    StructLike(Vec<Member>),
    TupleLike(Vec<(TypeHint, Option<Expr>)>),
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Located<String>,
    pub data: VariantData,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub public: bool,
    pub name: Located<String>,
    pub type_params: Vec<(String, Vec<Located<Path>>)>,
    pub members: Vec<Member>,
    pub impls: Vec<ImplBlock>,
    pub functions: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub type_params: Vec<(String, Vec<Located<Path>>)>,
    pub path: Located<Path>,
    pub functions: Vec<Fn>,
}
