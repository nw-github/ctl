use crate::{
    ast::{Alignment, Sign},
    comptime_int::ComptimeInt,
    intern::{StrId, THIS_TYPE},
    lexer::{Located, Span},
};

use super::{Attributes, BinaryOp, UnaryOp};

#[derive(Clone, Copy)]
pub enum PathOrigin {
    Root(Span),
    Super(Span),
    Infer(Span),
    This(Span),
    Normal,
}

impl std::fmt::Display for PathOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Root(_) => write!(f, "::"),
            Self::Infer(_) => write!(f, ":"),
            Self::This(_) => write!(f, "{THIS_TYPE}"),
            Self::Super(_) => write!(f, "super::"),
            _ => Ok(()),
        }
    }
}

#[derive(Clone)]
pub enum UsePathTail {
    All,
    Ident(Located<StrId>),
}

#[derive(Clone)]
pub struct UsePath {
    pub public: bool,
    pub origin: PathOrigin,
    pub components: Vec<Located<StrId>>,
    pub tail: UsePathTail,
}

#[derive(Clone)]
pub struct Stmt {
    pub data: Located<StmtData>,
    pub attrs: Attributes,
}

#[derive(Clone)]
pub enum StmtData {
    Expr(Expr),
    Defer(Expr),
    Guard {
        cond: Expr,
        body: Expr,
    },
    Use(UsePath),
    Let {
        patt: Located<Pattern>,
        ty: Option<Located<TypeHint>>,
        value: Option<Expr>,
    },
    Fn(Fn),
    Struct {
        base: Struct,
        packed: bool,
    },
    Union {
        tag: Option<Path>,
        base: Struct,
        variants: Vec<Variant>,
    },
    UnsafeUnion(Struct),
    Trait {
        public: bool,
        sealed: bool,
        name: Located<StrId>,
        is_unsafe: bool,
        type_params: TypeParams,
        impls: Vec<Path>,
        functions: Vec<Located<Fn>>,
        assoc_types: TypeParams,
    },
    Extension {
        public: bool,
        name: Located<StrId>,
        ty: Located<TypeHint>,
        type_params: TypeParams,
        impls: Vec<Located<ImplBlock>>,
        functions: Vec<Located<Fn>>,
        operators: Vec<Located<OperatorFn>>,
    },
    Binding {
        public: bool,
        constant: bool,
        is_extern: bool,
        mutable: bool,
        name: Located<StrId>,
        ty: Located<TypeHint>,
        value: Option<Expr>,
    },
    Module {
        public: bool,
        file: bool,
        name: Located<StrId>,
        body: Vec<Stmt>,
    },
    ModuleOOL {
        public: bool,
        name: Located<StrId>,
        resolved: bool,
    },
    Error,
}

pub type Expr = Located<ExprData>;

pub type CallArgs = Vec<(Option<Located<StrId>>, Expr)>;

#[derive(Clone)]
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
        ty: Located<TypeHint>,
        throwing: bool,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: CallArgs,
    },
    Subscript {
        callee: Box<Expr>,
        args: CallArgs,
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
    Float(FloatPattern),
    String(StrId),
    Char(char),
    ByteString(Vec<u8>),
    ByteChar(u8),
    Path(Path),
    Void,
    Block(Vec<Stmt>, Option<StrId>),
    If {
        cond: Box<Expr>,
        if_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    Loop {
        cond: Option<Box<Expr>>,
        body: Vec<Stmt>,
        do_while: bool,
        label: Option<StrId>,
    },
    For {
        patt: Located<Pattern>,
        iter: Box<Expr>,
        body: Vec<Stmt>,
        label: Option<StrId>,
    },
    Match {
        expr: Box<Expr>,
        body: Vec<(Located<FullPattern>, Expr)>,
    },
    Member {
        source: Box<Expr>,
        generics: Vec<Located<TypeHint>>,
        member: Located<StrId>,
    },
    Return(Box<Expr>),
    Tail(Box<Expr>),
    Break(Option<Box<Expr>>, Option<Located<StrId>>),
    Unsafe(Box<Expr>),
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,
    },
    Continue(Option<Located<StrId>>),
    Lambda {
        params: Vec<(Located<StrId>, Option<Located<TypeHint>>)>,
        ret: Option<Located<TypeHint>>,
        body: Box<Expr>,
        moves: bool,
    },
    StringInterpolation {
        strings: Vec<StrId>,
        args: Vec<(Expr, Option<FormatOpts>)>,
    },
    Error,
}

pub type PathComponent = (Located<StrId>, Vec<Located<TypeHint>>);

#[derive(Clone, derive_more::Constructor)]
pub struct Path {
    pub origin: PathOrigin,
    pub components: Vec<PathComponent>,
}

impl Path {
    pub fn this_type(span: Span) -> Self {
        Self { origin: PathOrigin::This(span), components: vec![] }
    }

    pub fn as_identifier(&self) -> Option<Located<StrId>> {
        self.components
            .first()
            .filter(|_| matches!(self.origin, PathOrigin::Normal) && self.components.len() == 1)
            .filter(|(_, ty_args)| ty_args.is_empty())
            .map(|(comp, _)| *comp)
    }

    pub fn span(&self) -> Span {
        let start = match self.origin {
            PathOrigin::Root(span) => span,
            PathOrigin::Super(span) => span,
            PathOrigin::Normal => self.components.first().unwrap().0.span,
            PathOrigin::Infer(span) => span,
            PathOrigin::This(span) => span,
        };
        let end = self.components.last().map(|c| c.0.span).unwrap_or(start);
        start.extended_to(end)
    }

    pub fn final_component_span(&self) -> Span {
        self.components.last().unwrap().0.span
    }
}

impl From<Located<StrId>> for Path {
    fn from(value: Located<StrId>) -> Self {
        Self::new(PathOrigin::Normal, vec![(value, Vec::new())])
    }
}

#[derive(Clone)]
pub struct Destructure {
    pub name: Located<StrId>,
    pub mutable: bool,
    pub pattern: Located<Pattern>,
}

#[derive(Clone)]
pub struct IntPattern {
    pub negative: bool,
    pub value: ComptimeInt,
    pub width: Option<StrId>,
}

#[derive(Clone, Copy)]
pub struct FloatPattern {
    pub negative: bool,
    pub value: f64,
    pub suffix: Option<StrId>,
}

#[derive(Clone)]
pub struct RangePattern<T> {
    pub inclusive: bool,
    pub start: Option<T>,
    pub end: T,
}

#[derive(Clone)]
pub enum Pattern {
    // x is ::core::opt::Option::Some(mut y)
    TupleLike { path: Path, subpatterns: Vec<Located<Pattern>> },
    StructLike { path: Path, subpatterns: Vec<Destructure> },
    // x is ::core::opt::Option::None
    // x is y
    Path(Path),
    // x is mut y
    MutBinding(StrId),
    // x is ?mut y
    Option(Box<Pattern>),
    // let {x, y} = z;
    Struct(Vec<Destructure>),
    Tuple(Vec<Located<Pattern>>),
    Int(IntPattern),
    IntRange(RangePattern<IntPattern>),
    String(StrId),
    Char(char),
    CharRange(RangePattern<char>),
    Array(Vec<Located<Pattern>>),
    Rest(Option<(bool, Located<StrId>)>),
    Bool(bool),
    Or(Vec<Located<Pattern>>),
    Void,
    Error,
}

#[derive(Clone)]
pub struct FullPattern {
    pub data: Pattern,
    pub if_expr: Option<Box<Expr>>,
}

#[derive(Default, Clone)]
pub enum TypeHint {
    Path(Path),
    Array(Box<Located<TypeHint>>, Box<Expr>),
    Vec(Box<Located<TypeHint>>),
    Slice(Box<Located<TypeHint>>),
    SliceMut(Box<Located<TypeHint>>),
    Tuple(Vec<Located<TypeHint>>),
    AnonStruct(Vec<(StrId, Located<TypeHint>)>),
    Set(Box<Located<TypeHint>>),
    Map(Box<[Located<TypeHint>; 2]>),
    Option(Box<Located<TypeHint>>),
    Ptr(Box<Located<TypeHint>>),
    MutPtr(Box<Located<TypeHint>>),
    RawPtr(Box<Located<TypeHint>>),
    RawMutPtr(Box<Located<TypeHint>>),
    DynPtr(Path),
    DynMutPtr(Path),
    Fn {
        is_extern: bool,
        params: Vec<Located<TypeHint>>,
        ret: Option<Box<Located<TypeHint>>>,
    },
    Void,
    #[default]
    Error,
}

#[derive(Clone)]
pub struct Param {
    pub keyword: bool,
    pub patt: Located<Pattern>,
    pub ty: Located<TypeHint>,
    pub default: Option<Expr>,
}

#[derive(Clone, Copy, derive_more::Display)]
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
    #[display(fmt = "+=")]
    AddAssign,
    #[display(fmt = "-=")]
    SubAssign,
    #[display(fmt = "*=")]
    MulAssign,
    #[display(fmt = "/=")]
    DivAssign,
    #[display(fmt = "%=")]
    RemAssign,
    #[display(fmt = "&=")]
    BitAndAssign,
    #[display(fmt = "|=")]
    BitOrAssign,
    #[display(fmt = "^=")]
    XorAssign,
    #[display(fmt = "<<=")]
    ShlAssign,
    #[display(fmt = ">>=")]
    ShrAssign,
}

#[derive(Clone)]
pub struct Fn {
    pub attrs: Attributes,
    pub public: bool,
    pub name: Located<StrId>,
    pub is_extern: bool,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub variadic: bool,
    pub assign_subscript: bool,
    pub type_params: TypeParams,
    pub params: Vec<Param>,
    pub ret: Option<Located<TypeHint>>,
    pub body: Option<Expr>,
}

impl Fn {
    pub fn from_operator_fn(name: StrId, func: OperatorFn) -> Self {
        Self {
            attrs: func.attrs,
            public: true,
            name: Located::new(func.name.span, name),
            is_extern: false,
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

#[derive(Clone)]
pub struct OperatorFn {
    pub attrs: Attributes,
    pub name: Located<OperatorFnType>,
    pub type_params: TypeParams,
    pub params: Vec<Param>,
    pub ret: Option<Located<TypeHint>>,
    pub body: Option<Expr>,
}

#[derive(Clone, Default)]
pub struct Member {
    pub public: bool,
    pub name: Located<StrId>,
    pub ty: Located<TypeHint>,
    pub default: Option<Expr>,
}

#[derive(Clone)]
pub enum VariantData {
    Empty,
    StructLike(Vec<Member>),
    TupleLike(Vec<(Located<TypeHint>, Option<Expr>)>),
}

#[derive(Clone)]
pub struct Variant {
    pub name: Located<StrId>,
    pub data: VariantData,
    pub tag: Option<Expr>,
}

#[derive(Clone)]
pub struct Struct {
    pub public: bool,
    pub name: Located<StrId>,
    pub type_params: TypeParams,
    pub members: Vec<Member>,
    pub impls: Vec<Located<ImplBlock>>,
    pub functions: Vec<Located<Fn>>,
    pub operators: Vec<Located<OperatorFn>>,
}

#[derive(Clone)]
pub struct ImplBlock {
    pub attrs: Attributes,
    pub type_params: TypeParams,
    pub path: Path,
    pub assoc_types: Vec<(Located<StrId>, Located<TypeHint>)>,
    pub functions: Vec<Located<Fn>>,
}

pub type TypeParams = Vec<(Located<StrId>, Vec<Path>)>;

#[derive(Debug, Clone, Copy)]
pub enum FormatType {
    Debug,
    Custom(Located<StrId>),
}

#[derive(Clone)]
pub struct FormatOpts {
    pub width: Option<Expr>,
    pub prec: Option<Expr>,
    pub fill: Option<char>,
    pub align: Option<Alignment>,
    pub sign: Option<Sign>,
    pub alt: bool,
    pub zero: bool,
    pub typ: Option<FormatType>,
}
