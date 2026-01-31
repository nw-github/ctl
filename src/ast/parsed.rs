use crate::{
    ast::{Alignment, DefaultCapturePolicy, Sign},
    ds::{
        ComptimeInt,
        arena::{Arena, Id},
    },
    intern::{ByteStrId, StrId, THIS_TYPE},
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
pub enum UsePathOrigin {
    Root(Span),
    Super(Span),
    Here,
}

#[derive(Clone)]
pub enum UsePathComponent {
    Multi(Vec<UsePathComponent>),
    Ident { ident: Located<StrId>, next: Option<Box<UsePathComponent>> },
    Rename { ident: Located<StrId>, new_name: Located<StrId> },
    All(Span),
    Error,
}

#[derive(Clone)]
pub struct UsePath {
    pub public: bool,
    pub origin: UsePathOrigin,
    pub component: UsePathComponent,
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
        ty: Option<TypeHint>,
        value: Option<Expr>,
    },
    Fn(Fn),
    Struct {
        base: Struct,
        packed: bool,
    },
    Union {
        tag: Option<TypeHint>,
        base: Struct,
        variants: Vec<Variant>,
    },
    UnsafeUnion(Struct),
    Trait {
        public: bool,
        is_sealed: bool,
        is_unsafe: bool,
        name: Located<StrId>,
        type_params: TypeParams,
        super_traits: Vec<Path>,
        functions: Vec<Located<Fn>>,
        assoc_types: TypeParams,
    },
    Extension {
        ty: TypeHint,
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
        ty: TypeHint,
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
    Alias {
        public: bool,
        name: Located<StrId>,
        type_params: TypeParams,
        ty: TypeHint,
    },
    Error,
}

#[derive(Clone)]
pub enum ExprData {
    Binary {
        op: BinaryOp,
        left: Expr,
        right: Expr,
    },
    Is {
        expr: Expr,
        pattern: Located<Pattern>,
    },
    As {
        expr: Expr,
        ty: TypeHint,
    },
    Unary {
        op: UnaryOp,
        expr: Expr,
    },
    Call {
        callee: Expr,
        args: CallArgs,
    },
    Subscript {
        callee: Expr,
        args: CallArgs,
    },
    Array(Vec<Expr>),
    Set(Vec<Expr>),
    Vec(Vec<Expr>),
    ArrayWithInit {
        init: Expr,
        count: Expr,
    },
    VecWithInit {
        init: Expr,
        count: Expr,
    },
    Tuple(Vec<(Located<StrId>, Expr)>),
    Map(Vec<(Expr, Expr)>),
    Bool(bool),
    Integer(IntPattern),
    Float(FloatPattern),
    String(StrId),
    Char(char),
    ByteString(ByteStrId),
    ByteChar(u8),
    Path(Path),
    Void,
    Block(Vec<Stmt>, Option<StrId>),
    If {
        cond: Expr,
        if_branch: Expr,
        else_branch: Option<Expr>,
    },
    Loop {
        cond: Option<Expr>,
        body: Vec<Stmt>,
        do_while: bool,
        label: Option<StrId>,
    },
    For {
        patt: Located<Pattern>,
        iter: Expr,
        body: Vec<Stmt>,
        label: Option<StrId>,
    },
    Match {
        expr: Expr,
        body: Vec<(FullPattern, Expr)>,
    },
    Member {
        source: Expr,
        generics: Vec<TypeHint>,
        member: Located<StrId>,
    },
    Return(Expr),
    Tail(Expr),
    Break(Option<Expr>, Option<Located<StrId>>),
    Unsafe(Expr),
    Grouping(Expr),
    Range {
        start: Option<Expr>,
        end: Option<Expr>,
        inclusive: bool,
    },
    Continue(Option<Located<StrId>>),
    Closure {
        policy: Option<DefaultCapturePolicy>,
        captures: Vec<Capture>,
        params: Vec<(Located<Pattern>, Option<TypeHint>)>,
        ret: Option<TypeHint>,
        body: Expr,
    },
    StringInterpolation {
        strings: Vec<StrId>,
        args: Vec<(Expr, Option<FormatOpts>)>,
    },
    Error,
}

pub type PathComponent = (Located<StrId>, Vec<TypeHint>);

#[derive(Clone)]
pub struct Path {
    pub origin: PathOrigin,
    pub components: Vec<PathComponent>,
    pub fn_like: bool,
}

impl Path {
    pub fn new(origin: PathOrigin, components: Vec<PathComponent>) -> Self {
        Self { origin, components, fn_like: false }
    }

    pub fn this_type(span: Span) -> Self {
        Self::new(PathOrigin::This(span), vec![])
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
        self.components.last().map(|c| c.0.span).unwrap_or_else(|| match self.origin {
            PathOrigin::Root(span) => span,
            PathOrigin::Super(span) => span,
            PathOrigin::Normal => self.components.first().unwrap().0.span,
            PathOrigin::Infer(span) => span,
            PathOrigin::This(span) => span,
        })
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

#[derive(Debug, Clone)]
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
    Option(Box<Located<Pattern>>),
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
    pub data: Located<Pattern>,
    pub if_expr: Option<Expr>,
}

#[derive(Clone)]
pub enum TypeHintData {
    Path(Path),
    Array(TypeHint, Expr),
    Vec(TypeHint),
    Slice(TypeHint),
    SliceMut(TypeHint),
    Tuple(Vec<(Located<StrId>, TypeHint)>),
    Set(TypeHint),
    Map([TypeHint; 2]),
    Option(TypeHint),
    Ptr(TypeHint),
    MutPtr(TypeHint),
    RawPtr(TypeHint),
    RawMutPtr(TypeHint),
    DynPtr(Path),
    DynMutPtr(Path),
    Fn { is_extern: bool, is_unsafe: bool, params: Vec<TypeHint>, ret: Option<TypeHint> },
    Void,
    Error,
}

#[derive(Clone)]
pub struct Param {
    pub keyword: bool,
    pub patt: Located<Pattern>,
    pub ty: TypeHint,
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

#[derive(Clone, Copy, Default, enum_as_inner::EnumAsInner)]
pub enum FunctionType {
    #[default]
    Normal,
    Subscript,
    AssignSubscript,
    Test,
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
    pub typ: FunctionType,
    pub type_params: TypeParams,
    pub params: Vec<Param>,
    pub ret: Option<TypeHint>,
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
            typ: match func.name.data {
                OperatorFnType::SubscriptAssign => FunctionType::AssignSubscript,
                OperatorFnType::Subscript => FunctionType::Subscript,
                _ => FunctionType::Normal,
            },
        }
    }
}

#[derive(Clone)]
pub struct OperatorFn {
    pub attrs: Attributes,
    pub name: Located<OperatorFnType>,
    pub type_params: TypeParams,
    pub params: Vec<Param>,
    pub ret: Option<TypeHint>,
    pub body: Option<Expr>,
}

#[derive(Clone)]
pub struct Member {
    pub public: bool,
    pub name: Located<StrId>,
    pub ty: TypeHint,
    pub default: Option<Expr>,
}

#[derive(Clone)]
pub struct VariantData {
    pub name: Located<StrId>,
    pub ty: TypeHint,
    pub default: Option<Expr>,
}

#[derive(Clone)]
pub struct Variant {
    pub name: Located<StrId>,
    pub data: Option<(Vec<VariantData>, TypeHint)>,
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
    pub assoc_types: Vec<(Located<StrId>, TypeHint)>,
    pub functions: Vec<Located<Fn>>,
}

#[derive(Clone, Copy)]
#[allow(clippy::enum_variant_names)]
pub enum Capture {
    ByVal(Located<StrId>),
    ByValMut(Located<StrId>),
    ByPtr(Located<StrId>),
    ByMutPtr(Located<StrId>),
    New { mutable: bool, ident: Located<StrId>, expr: Expr },
}

impl Capture {
    pub fn ident(self) -> Located<StrId> {
        match self {
            Capture::ByVal(id) => id,
            Capture::ByValMut(id) => id,
            Capture::ByPtr(id) => id,
            Capture::ByMutPtr(id) => id,
            Capture::New { ident, .. } => ident,
        }
    }

    pub fn span(self) -> Span {
        self.ident().span
    }
}

pub type TypeParams = Vec<(Located<StrId>, Vec<Path>)>;

#[derive(Debug, Clone, Copy)]
pub enum FormatType {
    Debug,
    Custom(Located<StrId>),
}

#[derive(Clone, Copy)]
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

pub type CallArgs = Vec<(Option<Located<StrId>>, Expr)>;

pub type TypeHint = Located<Id<TypeHintData>>;
pub type Expr = Located<Id<ExprData>>;

#[derive(Default, derive_more::Deref, derive_more::DerefMut)]
pub struct ExprArena {
    #[deref]
    #[deref_mut]
    pub exprs: Arena<ExprData>,
    pub hints: Arena<TypeHintData>,
}

impl ExprArena {
    pub const EXPR_ERROR: Id<ExprData> = Id::new(0);
    pub const HINT_ERROR: Id<TypeHintData> = Id::new(0);
    pub const HINT_VOID: TypeHint = Self::hint_void(Span::nowhere());
    pub const HINT_THIS: TypeHint = TypeHint { span: Span::nowhere(), data: Id::new(2) };

    pub fn new() -> Self {
        let mut this = Self::default();
        this.exprs.alloc(ExprData::Error);
        this.hints.alloc(TypeHintData::Error);
        this.hints.alloc(TypeHintData::Void);
        this.hints.alloc(TypeHintData::Path(Path::this_type(Span::default())));
        this
    }

    pub fn expr(&mut self, span: Span, data: ExprData) -> Expr {
        Located { span, data: self.exprs.alloc(data) }
    }

    pub fn hint(&mut self, span: Span, data: TypeHintData) -> TypeHint {
        Located { span, data: self.hints.alloc(data) }
    }

    pub const fn hint_void(span: Span) -> TypeHint {
        TypeHint { span, data: Id::new(1) }
    }
}
