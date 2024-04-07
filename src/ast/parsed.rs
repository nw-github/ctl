use crate::{
    lexer::{Located, Span},
    THIS_TYPE,
};

use super::{Attributes, BinaryOp, UnaryOp};

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
    pub attrs: Attributes,
}

#[derive(Debug, Clone)]
pub enum StmtData {
    Expr(Expr),
    Defer(Expr),
    Use(UsePath),
    Let {
        patt: Located<Pattern>,
        ty: Option<TypeHint>,
        value: Option<Expr>,
    },
    Fn(Fn),
    Struct(Struct),
    Union {
        tag: Option<Path>,
        base: Struct,
        variants: Vec<Variant>,
    },
    UnsafeUnion(Struct),
    Trait {
        public: bool,
        name: Located<String>,
        is_unsafe: bool,
        type_params: TypeParams,
        impls: Vec<Path>,
        functions: Vec<Fn>,
    },
    Extension {
        public: bool,
        name: Located<String>,
        ty: TypeHint,
        type_params: TypeParams,
        impls: Vec<ImplBlock>,
        functions: Vec<Fn>,
        operators: Vec<OperatorFn>,
    },
    Binding {
        public: bool,
        constant: bool,
        name: Located<String>,
        ty: TypeHint,
        value: Expr,
    },
    Module {
        public: bool,
        file: bool,
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
    Subscript {
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
    Integer(IntPattern),
    Float(String),
    String(String),
    Char(char),
    ByteString(Vec<u8>),
    ByteChar(u8),
    Path(Path),
    Void,
    None,
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
        member: Located<String>,
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
    StringInterpolation(Vec<Expr>),
    Error,
}

pub type PathComponent = (Located<String>, Vec<TypeHint>);

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
            .map(|(path, _)| path.data.as_str())
    }

    pub fn span(&self) -> Span {
        let end = self.components.last().unwrap().0.span;
        if let PathOrigin::Super(span) = self.origin {
            span.extended_to(end)
        } else {
            self.components.first().unwrap().0.span.extended_to(end)
        }
    }

    pub fn final_component_span(&self) -> Span {
        self.components.last().unwrap().0.span
    }
}

impl From<Located<String>> for Path {
    fn from(value: Located<String>) -> Self {
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
    pub start: Option<T>,
    pub end: T,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    // x is ::core::opt::Option::Some(mut y)
    TupleLike {
        path: Path,
        subpatterns: Vec<Located<Pattern>>,
    },
    StructLike {
        path: Path,
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

#[derive(Default, Clone)]
pub enum TypeHint {
    Regular(Path),
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
    DynPtr(Path),
    DynMutPtr(Path),
    Fn {
        is_extern: bool,
        params: Vec<TypeHint>,
        ret: Box<TypeHint>,
    },
    Void,
    This(Span),
    #[default]
    Error,
}

impl std::fmt::Debug for TypeHint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeHint::Regular(path) => write!(f, "{path:?}"),
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
            TypeHint::DynPtr(inner) => write!(f, "*dyn {inner:?}"),
            TypeHint::DynMutPtr(inner) => write!(f, "*dyn mut {inner:?}"),
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

#[derive(Debug, Clone, Copy, derive_more::Display)]
pub enum OperatorFnType {
    #[display(fmt = "+")]
    Plus,
    #[display(fmt = "-")]
    Minus,
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "%")]
    Rem,
    #[display(fmt = "&")]
    BitAnd,
    #[display(fmt = "|")]
    BitOr,
    #[display(fmt = "^")]
    Xor,
    #[display(fmt = "<<")]
    Shl,
    #[display(fmt = ">>")]
    Shr,
    #[display(fmt = "==")]
    Eq,
    #[display(fmt = "<=>")]
    Cmp,
    #[display(fmt = "++")]
    Increment,
    #[display(fmt = "--")]
    Decrement,
    #[display(fmt = "!")]
    Bang,
    #[display(fmt = "[]")]
    Subscript,
    #[display(fmt = "[]=")]
    SubscriptAssign,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub attrs: Attributes,
    pub public: bool,
    pub name: Located<String>,
    pub linkage: Linkage,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    pub assign_subscript: bool,
    pub type_params: TypeParams,
    pub params: Vec<Param>,
    pub ret: TypeHint,
    pub body: Option<Expr>,
}

impl Fn {
    pub fn from_operator_fn(name: String, func: OperatorFn) -> Self {
        Self {
            attrs: func.attrs,
            public: true,
            name: Located::new(func.name.span, name),
            linkage: Linkage::Internal,
            is_async: false,
            is_unsafe: false,
            variadic: false,
            type_params: func.type_params,
            params: func.params,
            ret: func.ret,
            body: func.body,
            assign_subscript: matches!(func.name.data, OperatorFnType::SubscriptAssign),
        }
    }
}

#[derive(Debug, Clone)]
pub struct OperatorFn {
    pub attrs: Attributes,
    pub name: Located<OperatorFnType>,
    pub type_params: TypeParams,
    pub params: Vec<Param>,
    pub ret: TypeHint,
    pub body: Option<Expr>,
}

#[derive(Debug, Clone, Default)]
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
    pub tag: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub public: bool,
    pub name: Located<String>,
    pub type_params: TypeParams,
    pub members: Vec<Member>,
    pub impls: Vec<ImplBlock>,
    pub functions: Vec<Fn>,
    pub operators: Vec<OperatorFn>,
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub type_params: TypeParams,
    pub path: Path,
    pub functions: Vec<Fn>,
}

pub type TypeParams = Vec<(Located<String>, Vec<Path>)>;
