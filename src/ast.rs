use derive_more::Display;

use crate::lexer::{Located, Span, Token};

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
        path: Path,
        all: bool,
    },
    Let {
        name: String,
        ty: Option<TypeHint>,
        mutable: bool,
        value: Option<Expr>,
    },
    Fn(Fn),
    UserType(ParsedUserType),
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
        pattern: Pattern,
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
        var: String,
        mutable: bool,
        iter: Box<Expr>,
        body: Vec<Stmt>,
    },
    Match {
        expr: Box<Expr>,
        body: Vec<(Pattern, Expr)>,
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
    YieldOrReturn(Box<Expr>),
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

impl ExprData {
    pub fn is_block_expr(&self) -> bool {
        matches!(
            self,
            ExprData::If { .. }
                | ExprData::For { .. }
                | ExprData::Block(_)
                | ExprData::Match { .. }
        ) || matches!(self, ExprData::Loop { do_while, .. } if !do_while )
    }
}

#[derive(Debug, Clone, enum_as_inner::EnumAsInner)]
pub enum Path {
    Root(Vec<(String, Vec<TypeHint>)>),
    Super(Vec<(String, Vec<TypeHint>)>),
    Normal(Vec<(String, Vec<TypeHint>)>),
}

impl From<String> for Path {
    fn from(value: String) -> Self {
        Self::Normal(vec![(value, Vec::new())])
    }
}

impl Path {
    pub fn as_identifier(&self) -> Option<&str> {
        self.as_normal().and_then(|comps| {
            (comps.len() == 1 && comps[0].1.is_empty()).then_some(comps[0].0.as_str())
        })
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    PathWithBindings {
        path: Located<Path>,
        binding: (bool, String),
    },
    Path(Located<Path>),
    MutCatchAll(Located<String>),
    Option(bool, Located<String>),
    Null(Span),
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Located<String>,
    pub props: Vec<Attribute>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum BinaryOp {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
    #[display(fmt = "%")]
    Rem,
    #[display(fmt = "&")]
    And,
    #[display(fmt = "^")]
    Xor,
    #[display(fmt = "|")]
    Or,
    #[display(fmt = "<<")]
    Shl,
    #[display(fmt = ">>")]
    Shr,
    #[display(fmt = "??")]
    NoneCoalesce,
    #[display(fmt = ">")]
    Gt,
    #[display(fmt = ">=")]
    GtEqual,
    #[display(fmt = "<")]
    Lt,
    #[display(fmt = "<=")]
    LtEqual,
    #[display(fmt = "==")]
    Equal,
    #[display(fmt = "!=")]
    NotEqual,
    #[display(fmt = "||")]
    LogicalOr,
    #[display(fmt = "&&")]
    LogicalAnd,
}

impl TryFrom<Token<'_>> for BinaryOp {
    type Error = ();

    fn try_from(value: Token<'_>) -> Result<Self, Self::Error> {
        match value {
            Token::Plus | Token::AddAssign => Ok(BinaryOp::Add),
            Token::Minus | Token::SubAssign => Ok(BinaryOp::Sub),
            Token::Asterisk | Token::MulAssign => Ok(BinaryOp::Mul),
            Token::Div | Token::DivAssign => Ok(BinaryOp::Div),
            Token::Rem | Token::RemAssign => Ok(BinaryOp::Rem),
            Token::Ampersand | Token::AndAssign => Ok(BinaryOp::And),
            Token::Caret | Token::XorAssign => Ok(BinaryOp::Xor),
            Token::Or | Token::OrAssign => Ok(BinaryOp::Or),
            Token::NoneCoalesce | Token::NoneCoalesceAssign => Ok(BinaryOp::NoneCoalesce),
            Token::RAngle => Ok(BinaryOp::Gt),
            Token::GtEqual => Ok(BinaryOp::GtEqual),
            Token::LAngle => Ok(BinaryOp::Lt),
            Token::Shl | Token::ShlAssign => Ok(BinaryOp::Shl),
            Token::Shr | Token::ShrAssign => Ok(BinaryOp::Shr),
            Token::LtEqual => Ok(BinaryOp::LtEqual),
            Token::Equal => Ok(BinaryOp::Equal),
            Token::NotEqual => Ok(BinaryOp::NotEqual),
            Token::LogicalAnd => Ok(BinaryOp::LogicalAnd),
            Token::LogicalOr => Ok(BinaryOp::LogicalOr),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum UnaryOp {
    #[display(fmt = "+")]
    Plus,
    #[display(fmt = "-")]
    Neg,
    #[display(fmt = "++")]
    PostIncrement,
    #[display(fmt = "--")]
    PostDecrement,
    #[display(fmt = "++")]
    PreIncrement,
    #[display(fmt = "--")]
    PreDecrement,
    #[display(fmt = "!")]
    Not,
    #[display(fmt = "*")]
    Deref,
    #[display(fmt = "&")]
    Addr,
    #[display(fmt = "&mut")]
    AddrMut,
    #[display(fmt = "!")]
    Unwrap,
    #[display(fmt = "?")]
    Try,
}

impl UnaryOp {
    pub fn try_from_postfix(value: Token<'_>) -> Option<Self> {
        match value {
            Token::Increment => Some(UnaryOp::PostIncrement),
            Token::Decrement => Some(UnaryOp::PostDecrement),
            Token::Exclamation => Some(UnaryOp::Unwrap),
            Token::Question => Some(UnaryOp::Try),
            _ => None,
        }
    }

    pub fn try_from_prefix(value: Token<'_>) -> Option<Self> {
        match value {
            Token::Plus => Some(UnaryOp::Plus),
            Token::Minus => Some(UnaryOp::Neg),
            Token::Asterisk => Some(UnaryOp::Deref),
            Token::Ampersand => Some(UnaryOp::Addr),
            Token::Increment => Some(UnaryOp::PreIncrement),
            Token::Decrement => Some(UnaryOp::PreDecrement),
            Token::Exclamation => Some(UnaryOp::Not),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeHint {
    Regular(Located<Path>),
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

#[derive(Debug, Clone)]
pub struct Param {
    pub mutable: bool,
    pub keyword: bool,
    pub name: String,
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
    pub type_params: Vec<(String, Vec<Located<Path>>)>,
    pub params: Vec<Param>,
    pub ret: TypeHint,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct MemVar {
    pub public: bool,
    pub name: String,
    pub shared: bool,
    pub ty: TypeHint,
    pub default: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub public: bool,
    pub name: Located<String>,
    pub type_params: Vec<(String, Vec<Located<Path>>)>,
    pub members: Vec<MemVar>,
    pub impls: Vec<ImplBlock>,
    pub functions: Vec<Fn>,
}

#[derive(Debug, Clone)]
pub enum ParsedUserType {
    Struct(Struct),
    Union {
        tag: Option<Located<Path>>,
        base: Struct,
        is_unsafe: bool,
    },
    Trait {
        public: bool,
        name: String,
        is_unsafe: bool,
        type_params: Vec<(String, Vec<Located<Path>>)>,
        impls: Vec<Located<Path>>,
        functions: Vec<Fn>,
    },
    Enum {
        public: bool,
        name: Located<String>,
        impls: Vec<ImplBlock>,
        variants: Vec<(String, Option<Expr>)>,
        functions: Vec<Fn>,
    },
}

#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub type_params: Vec<(String, Vec<Located<Path>>)>,
    pub path: Located<Path>,
    pub functions: Vec<Fn>,
}
