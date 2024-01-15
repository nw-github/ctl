use crate::lexer::{Located, Span};

use super::{Attribute, BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub struct Stmt {
    pub data: StmtData,
    pub span: Span,
    pub attrs: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub enum StmtData {
    Expr(Expr),
    Use {
        public: bool,
        path: TypePath,
        all: bool,
    },
    Let {
        patt: Located<Pattern>,
        ty: Option<TypeHint>,
        value: Option<Expr>,
    },
    Fn(Fn),
    Struct(Struct),
    Union {
        tag: Option<Located<TypePath>>,
        base: Struct,
        is_unsafe: bool,
    },
    Trait {
        public: bool,
        name: Located<String>,
        is_unsafe: bool,
        type_params: Vec<(String, Vec<Located<TypePath>>)>,
        impls: Vec<Located<TypePath>>,
        functions: Vec<Fn>,
    },
    Enum {
        public: bool,
        base_ty: Option<Located<TypePath>>,
        name: Located<String>,
        impls: Vec<ImplBlock>,
        variants: Vec<(Located<String>, Option<Expr>)>,
        functions: Vec<Fn>,
    },
    Extension {
        public: bool,
        name: String,
        ty: TypeHint,
        type_params: Vec<(String, Vec<Located<TypePath>>)>,
        impls: Vec<ImplBlock>,
        functions: Vec<Fn>,
    },
    Static {
        public: bool,
        name: String,
        ty: TypeHint,
        value: Expr,
    },
    Module {
        public: bool,
        name: String,
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
        pattern: Located<Pattern>,
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
    ByteString(String),
    ByteChar(u8),
    Path(TypePath),
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
        body: Vec<(Located<Pattern>, Expr)>,
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
    Yield(Box<Expr>),
    Tail(Box<Expr>),
    Break(Box<Expr>),
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
    },
    Error,
}

pub type TypePath = Path<TypePathComponent>;

impl From<String> for TypePath {
    fn from(value: String) -> Self {
        Self::Normal(vec![TypePathComponent(value, Vec::new())])
    }
}

impl TypePath {
    pub fn as_identifier(&self) -> Option<&str> {
        self.as_normal().and_then(|comps| {
            (comps.len() == 1 && comps[0].1.is_empty()).then_some(comps[0].0.as_str())
        })
    }
}

#[derive(Clone)]
pub struct TypePathComponent(pub String, pub Vec<TypeHint>);

impl std::fmt::Debug for TypePathComponent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;
        if self.1.is_empty() {
            return Ok(());
        }

        write!(f, "::<")?;
        for (i, generic) in self.1.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{generic:?}")?;
        }
        write!(f, ">")
    }
}

#[derive(Clone, enum_as_inner::EnumAsInner)]
pub enum Path<C> {
    Root(Vec<C>),
    Super(Vec<C>),
    Normal(Vec<C>),
}

impl<C: std::fmt::Debug> std::fmt::Debug for Path<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Path::Root(r) => {
                write!(f, "::")?;
                r
            }
            Path::Super(r) => {
                write!(f, "super::")?;
                r
            }
            Path::Normal(r) => r,
        };

        for (i, component) in res.iter().enumerate() {
            if i > 0 {
                write!(f, "::")?;
            }

            write!(f, "{component:?}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Destructure {
    pub name: Located<String>,
    pub mutable: bool,
    pub pattern: Option<Located<Pattern>>,
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
        path: Located<TypePath>,
        subpatterns: Vec<Located<Pattern>>,
    },
    // x is ::core::opt::Option::None
    // x is y
    Path(TypePath),
    // x is mut y
    MutBinding(String),
    // x is ?mut y
    Option(Box<Pattern>),
    // x is null
    Null,
    // let {x, y} = z;
    StructDestructure(Vec<Destructure>),
    Int(IntPattern),
    IntRange(RangePattern<IntPattern>),
    String(String),
    Char(char),
    CharRange(RangePattern<char>),
    Array(Vec<Located<Pattern>>),
    Rest(Option<(bool, String)>),
    Error,
}

#[derive(Clone)]
pub enum TypeHint {
    Regular(Located<TypePath>),
    Array(Box<TypeHint>, Box<Expr>),
    Vec(Box<TypeHint>),
    Slice(Box<TypeHint>),
    SliceMut(Box<TypeHint>),
    Tuple(Vec<TypeHint>),
    Set(Box<TypeHint>),
    Map(Box<TypeHint>, Box<TypeHint>),
    Option(Box<TypeHint>),
    Ptr(Box<TypeHint>),
    MutPtr(Box<TypeHint>),
    Fn {
        is_extern: bool,
        params: Vec<TypeHint>,
        ret: Box<TypeHint>,
    },
    Void,
    This,
    MutThis,
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
            TypeHint::Set(inner) => write!(f, "{{{inner:?}}}"),
            TypeHint::Map(key, val) => write!(f, "[{key:?}: {val:?}]"),
            TypeHint::Option(inner) => write!(f, "?{inner:?}"),
            TypeHint::Ptr(inner) => write!(f, "*{inner:?}"),
            TypeHint::MutPtr(inner) => write!(f, "*mut {inner:?}"),
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
            TypeHint::This => write!(f, "This"),
            TypeHint::MutThis => write!(f, "mut This"),
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
    pub is_async: bool,
    pub is_extern: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    pub type_params: Vec<(String, Vec<Located<TypePath>>)>,
    pub params: Vec<Param>,
    pub ret: TypeHint,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub public: bool,
    pub name: Located<String>,
    pub shared: bool,
    pub ty: TypeHint,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub public: bool,
    pub name: Located<String>,
    pub type_params: Vec<(String, Vec<Located<TypePath>>)>,
    pub members: Vec<Member>,
    pub impls: Vec<ImplBlock>,
    pub functions: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub type_params: Vec<(String, Vec<Located<TypePath>>)>,
    pub path: Located<TypePath>,
    pub functions: Vec<Fn>,
}
