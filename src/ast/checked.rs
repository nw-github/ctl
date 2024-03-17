use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use num_bigint::BigInt;

use crate::{
    ast::{BinaryOp, UnaryOp},
    sym::{ScopeId, ScopeKind, Scopes, VariableId},
    typecheck::MemberFn,
    typeid::{GenericFunc, Type},
};

use super::parsed::RangePattern;

#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<CheckedStmt>,
    pub scope: ScopeId,
}

#[derive(Debug, Clone, Copy)]
pub struct RestPattern {
    pub id: Option<VariableId>,
    pub pos: usize,
}

#[derive(Debug, Clone)]
pub struct ArrayPattern<T> {
    pub patterns: Vec<T>,
    pub rest: Option<RestPattern>,
    pub arr_len: usize,
    pub inner: Type,
}

#[derive(Debug, Clone, Default, EnumAsInner)]
pub enum CheckedPatternData {
    UnionMember {
        pattern: Option<Box<CheckedPattern>>,
        variant: String,
        inner: Type,
        borrows: bool,
    },
    Destrucure {
        patterns: Vec<(String, Type, CheckedPattern)>,
        borrows: bool,
    },
    Array {
        patterns: ArrayPattern<CheckedPattern>,
        borrows: bool,
    },
    Int(BigInt),
    IntRange(RangePattern<BigInt>),
    String(String),
    Span {
        patterns: Vec<CheckedPattern>,
        rest: Option<RestPattern>,
        inner: Type,
    },
    Variable(VariableId),
    Void,
    #[default]
    Error,
}

#[derive(Debug, Clone, derive_more::Constructor)]
pub struct CheckedPattern {
    pub irrefutable: bool,
    pub data: CheckedPatternData,
}

impl CheckedPattern {
    pub fn irrefutable(data: CheckedPatternData) -> Self {
        Self {
            irrefutable: true,
            data,
        }
    }

    pub fn refutable(data: CheckedPatternData) -> Self {
        Self {
            irrefutable: false,
            data,
        }
    }
}

impl Default for CheckedPattern {
    fn default() -> Self {
        Self::irrefutable(Default::default())
    }
}

#[derive(Debug, Default, Clone)]
pub enum CheckedStmt {
    Expr(CheckedExpr),
    Let(CheckedPattern, Option<CheckedExpr>),
    Defer(CheckedExpr),
    #[default]
    None,
}

#[derive(Default, Debug, Clone)]
pub enum CheckedExprData {
    Binary {
        op: BinaryOp,
        left: Box<CheckedExpr>,
        right: Box<CheckedExpr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<CheckedExpr>,
    },
    AutoDeref(Box<CheckedExpr>, usize),
    Call(Box<CheckedExpr>, IndexMap<String, CheckedExpr>),
    CallDyn(GenericFunc, IndexMap<String, CheckedExpr>),
    CallFnPtr(Box<CheckedExpr>, Vec<CheckedExpr>),
    DynCoerce {
        expr: Box<CheckedExpr>,
        scope: ScopeId,
    },
    VariantInstance {
        members: IndexMap<String, CheckedExpr>,
        variant: String,
    },
    Instance(IndexMap<String, CheckedExpr>),
    Array(Vec<CheckedExpr>),
    ArrayWithInit {
        init: Box<CheckedExpr>,
        count: usize,
    },
    Vec(Vec<CheckedExpr>),
    VecWithInit {
        init: Box<CheckedExpr>,
        count: Box<CheckedExpr>,
    },
    Set(Vec<CheckedExpr>, ScopeId),
    Map(Vec<(CheckedExpr, CheckedExpr)>, ScopeId),
    Bool(bool),
    Integer(BigInt),
    Float(String),
    String(String),
    StringInterpolation {
        formatter: Box<CheckedExpr>,
        parts: Vec<CheckedExpr>,
        scope: ScopeId,
    },
    ByteString(Vec<u8>),
    Char(char),
    Void,
    Func(GenericFunc, ScopeId),
    MemFunc(MemberFn, ScopeId),
    Var(VariableId),
    Block(Block),
    If {
        cond: Box<CheckedExpr>,
        if_branch: Box<CheckedExpr>,
        else_branch: Option<Box<CheckedExpr>>,
    },
    Loop {
        cond: Option<Box<CheckedExpr>>,
        body: Block,
        do_while: bool,
        optional: bool,
    },
    For {
        iter: Box<CheckedExpr>,
        patt: CheckedPattern,
        body: Block,
        optional: bool,
    },
    Match {
        expr: Box<CheckedExpr>,
        body: Vec<(CheckedPattern, CheckedExpr)>,
    },
    Member {
        source: Box<CheckedExpr>,
        member: String,
    },
    Subscript {
        callee: Box<CheckedExpr>,
        args: Vec<CheckedExpr>,
    },
    As(Box<CheckedExpr>, bool),
    Is(Box<CheckedExpr>, CheckedPattern),
    Return(Box<CheckedExpr>),
    Yield(Box<CheckedExpr>),
    Break(Option<Box<CheckedExpr>>),
    Lambda(Vec<CheckedStmt>),
    NeverCoerce(Box<CheckedExpr>),
    Continue,
    #[default]
    Error,
}

impl CheckedExprData {
    pub fn is_yielding_block(&self, scopes: &Scopes) -> bool {
        matches!(self, CheckedExprData::Block(block) if
            matches!(scopes[block.scope].kind, ScopeKind::Block(_, true)))
    }

    pub fn member_call(mfn: MemberFn, args: IndexMap<String, CheckedExpr>, scope: ScopeId) -> Self {
        Self::Call(
            Box::new(CheckedExpr::new(
                Type::Func(mfn.func.clone().into()),
                Self::MemFunc(mfn, scope),
            )),
            args,
        )
    }

    pub fn call(func: GenericFunc, args: IndexMap<String, CheckedExpr>, scope: ScopeId) -> Self {
        Self::Call(
            CheckedExpr::new(Type::Func(func.clone().into()), Self::Func(func, scope)).into(),
            args,
        )
    }
}

#[derive(Debug, Default, Clone, derive_more::Constructor)]
pub struct CheckedExpr {
    pub ty: Type,
    pub data: CheckedExprData,
}

impl CheckedExpr {
    pub fn is_assignable(&self, scopes: &Scopes) -> bool {
        match &self.data {
            CheckedExprData::AutoDeref(expr, _) => {
                matches!(expr.ty, Type::MutPtr(_) | Type::RawPtr(_))
            }
            CheckedExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, Type::MutPtr(_) | Type::RawPtr(_))
            }
            CheckedExprData::Var(id) => scopes.get(*id).mutable,
            CheckedExprData::Member { source, .. } => source.is_assignable(scopes),
            CheckedExprData::Subscript { callee, .. } => match &callee.data {
                CheckedExprData::Var(id) => {
                    matches!(callee.ty, Type::MutPtr(_) | Type::RawPtr(_))
                        || scopes.get(*id).mutable
                }
                CheckedExprData::Member { source, .. } => source.is_assignable(scopes),
                _ => true,
            },
            _ => false,
        }
    }

    pub fn can_addrmut(&self, scopes: &Scopes) -> bool {
        match &self.data {
            CheckedExprData::AutoDeref(expr, _) => {
                matches!(expr.ty, Type::MutPtr(_) | Type::RawPtr(_))
            }
            CheckedExprData::Unary { op, expr } => {
                !matches!(op, UnaryOp::Deref)
                    || matches!(expr.ty, Type::MutPtr(_) | Type::RawPtr(_))
            }
            CheckedExprData::Var(id) => scopes.get(*id).mutable,
            CheckedExprData::Member { source, .. } => {
                matches!(source.ty, Type::MutPtr(_)) || source.can_addrmut(scopes)
            }
            CheckedExprData::Subscript { callee, .. } => callee.can_addrmut(scopes),
            _ => true,
        }
    }

    pub fn auto_deref(self, target: &Type) -> CheckedExpr {
        let mut needed = 0;
        let mut current = target;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = current {
            current = inner;
            needed += 1;
        }

        let mut prev = &self.ty;
        let mut ty = &self.ty;
        let mut indirection = 0;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = ty {
            prev = ty;
            ty = inner;
            indirection += 1;
        }

        if let Type::DynMutPtr(_) | Type::DynPtr(_) = ty {
            indirection += 1;
        }

        match indirection.cmp(&needed) {
            std::cmp::Ordering::Less => {
                if matches!(target, Type::Ptr(_)) {
                    CheckedExpr::new(
                        Type::Ptr(self.ty.clone().into()),
                        CheckedExprData::Unary {
                            op: UnaryOp::Addr,
                            expr: self.into(),
                        },
                    )
                } else {
                    CheckedExpr::new(
                        Type::MutPtr(self.ty.clone().into()),
                        CheckedExprData::Unary {
                            op: UnaryOp::AddrMut,
                            expr: self.into(),
                        },
                    )
                }
            }
            std::cmp::Ordering::Equal => self,
            std::cmp::Ordering::Greater => CheckedExpr::new(
                if needed != 0 {
                    prev.clone()
                } else {
                    ty.clone()
                },
                CheckedExprData::AutoDeref(self.into(), indirection - needed),
            ),
        }
    }

    pub fn option_null(opt: Type) -> CheckedExpr {
        CheckedExpr::new(
            opt,
            CheckedExprData::VariantInstance {
                members: Default::default(),
                variant: "None".into(),
            },
        )
    }
}
