use enum_as_inner::EnumAsInner;

use crate::{
    ast::{BinaryOp, UnaryOp},
    comptime_int::ComptimeInt,
    hash::IndexMap,
    intern::{StrId, Strings},
    sym::{ScopeId, ScopeKind, Scopes, VariableId},
    typecheck::MemberFn,
    typeid::{GenericFn, Type, TypeId, Types},
};

use super::parsed::RangePattern;

#[derive(Clone)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub scope: ScopeId,
}

impl Block {
    pub fn is_yielding(&self, scopes: &Scopes) -> bool {
        matches!(&scopes[self.scope].kind, ScopeKind::Block(data) if data.yields)
    }
}

#[derive(Clone, Copy)]
pub struct RestPattern {
    pub id: Option<VariableId>,
    pub pos: usize,
}

#[derive(Clone)]
pub struct ArrayPattern<T> {
    pub patterns: Vec<T>,
    pub rest: Option<RestPattern>,
    pub arr_len: usize,
    pub inner: TypeId,
}

#[derive(Clone, Default, EnumAsInner)]
pub enum PatternData {
    Variant {
        pattern: Option<Box<Pattern>>,
        variant: StrId,
        inner: TypeId,
        borrows: bool,
    },
    Destrucure {
        patterns: Vec<(StrId, TypeId, Pattern)>,
        borrows: bool,
    },
    Array {
        patterns: ArrayPattern<Pattern>,
        borrows: bool,
    },
    Int(ComptimeInt),
    IntRange(RangePattern<ComptimeInt>),
    String(StrId),
    Span {
        patterns: Vec<Pattern>,
        rest: Option<RestPattern>,
        inner: TypeId,
    },
    Variable(VariableId),
    Or(Vec<Pattern>),
    Void,
    #[default]
    Error,
}

#[derive(Clone, derive_more::Constructor)]
pub struct Pattern {
    pub irrefutable: bool,
    pub data: PatternData,
}

impl Pattern {
    pub fn irrefutable(data: PatternData) -> Self {
        Self { irrefutable: true, data }
    }

    pub fn refutable(data: PatternData) -> Self {
        Self { irrefutable: false, data }
    }
}

impl Default for Pattern {
    fn default() -> Self {
        Self::irrefutable(Default::default())
    }
}

#[derive(Default, Clone)]
pub enum Stmt {
    Expr(Expr),
    Let(Pattern, Option<Expr>),
    Defer(Expr),
    Guard {
        cond: Expr,
        body: Expr,
    },
    #[default]
    None,
}

#[derive(Default, Clone)]
pub enum ExprData {
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    AutoDeref(Box<Expr>, usize),
    Call(Box<Expr>, IndexMap<StrId, Expr>),
    CallDyn(GenericFn, IndexMap<StrId, Expr>),
    CallFnPtr(Box<Expr>, Vec<Expr>),
    DynCoerce(Box<Expr>, ScopeId),
    VariantInstance(StrId, IndexMap<StrId, Expr>),
    SpanMutCoerce(Box<Expr>),
    Instance(IndexMap<StrId, Expr>),
    Array(Vec<Expr>),
    ArrayWithInit {
        init: Box<Expr>,
        count: usize,
    },
    Vec(Vec<Expr>),
    VecWithInit {
        init: Box<Expr>,
        count: Box<Expr>,
    },
    Set(Vec<Expr>, ScopeId),
    Map(Vec<(Expr, Expr)>, ScopeId),
    Int(ComptimeInt),
    Float(f64),
    String(StrId),
    StringInterp {
        formatter: Box<Expr>,
        parts: Vec<(MemberFn, Expr)>,
        scope: ScopeId,
    },
    ByteString(Vec<u8>),
    Void,
    Fn(GenericFn, ScopeId),
    MemFn(MemberFn, ScopeId),
    Var(VariableId),
    Block(Block),
    AffixOperator {
        callee: Box<Expr>,
        mfn: MemberFn,
        param: StrId,
        scope: ScopeId,
        postfix: bool,
    },
    If {
        cond: Box<Expr>,
        if_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Loop {
        cond: Option<Box<Expr>>,
        body: Block,
        do_while: bool,
        optional: bool,
    },
    Match {
        expr: Box<Expr>,
        body: Vec<(Pattern, Expr)>,
    },
    Member {
        source: Box<Expr>,
        member: StrId,
    },
    Subscript {
        callee: Box<Expr>,
        arg: Box<Expr>,
    },
    SliceArray {
        callee: Box<Expr>,
        arg: Box<Expr>,
    },
    As(Box<Expr>, bool),
    Is(Box<Expr>, Pattern),
    Return(Box<Expr>),
    Yield(Box<Expr>, ScopeId),
    Break(Box<Expr>, ScopeId),
    Lambda(Vec<Stmt>),
    NeverCoerce(Box<Expr>),
    Continue(ScopeId),
    #[default]
    Error,
}

impl ExprData {
    pub fn is_yielding_block(&self, scopes: &Scopes) -> Option<bool> {
        match self {
            ExprData::Block(block) => Some(block.is_yielding(scopes)),
            _ => None,
        }
    }

    pub fn member_call(
        types: &mut Types,
        mfn: MemberFn,
        args: IndexMap<StrId, Expr>,
        scope: ScopeId,
    ) -> Self {
        Self::Call(
            Box::new(Expr::new(types.insert(Type::Fn(mfn.func.clone())), Self::MemFn(mfn, scope))),
            args,
        )
    }

    pub fn call(
        types: &mut Types,
        func: GenericFn,
        args: IndexMap<StrId, Expr>,
        scope: ScopeId,
    ) -> Self {
        Self::Call(
            Expr::new(types.insert(Type::Fn(func.clone())), Self::Fn(func, scope)).into(),
            args,
        )
    }
}

#[derive(Default, Clone, derive_more::Constructor)]
pub struct Expr {
    pub ty: TypeId,
    pub data: ExprData,
}

impl Expr {
    pub fn is_assignable(&self, scopes: &Scopes, types: &Types) -> bool {
        match &self.data {
            ExprData::AutoDeref(expr, _) => {
                matches!(types[expr.ty], Type::MutPtr(_) | Type::RawMutPtr(_))
            }
            ExprData::Unary(op, expr) => {
                matches!(op, UnaryOp::Deref)
                    && matches!(types[expr.ty], Type::MutPtr(_) | Type::RawMutPtr(_))
            }
            ExprData::Var(id) => scopes.get(*id).mutable,
            ExprData::Member { source, .. } => source.is_assignable(scopes, types),
            ExprData::Subscript { callee, .. } => match &callee.data {
                ExprData::Var(id) => {
                    matches!(types[callee.ty], Type::MutPtr(_) | Type::RawMutPtr(_))
                        || scopes.get(*id).mutable
                }
                ExprData::Member { source, .. } => source.is_assignable(scopes, types),
                _ => true,
            },
            _ => false,
        }
    }

    pub fn can_addrmut(&self, scopes: &Scopes, types: &Types) -> bool {
        match &self.data {
            ExprData::AutoDeref(expr, _) => {
                matches!(types[expr.ty], Type::MutPtr(_) | Type::RawMutPtr(_))
            }
            ExprData::Unary(op, expr) => {
                !matches!(op, UnaryOp::Deref)
                    || matches!(types[expr.ty], Type::MutPtr(_) | Type::RawMutPtr(_))
            }
            ExprData::Var(id) => scopes.get(*id).mutable,
            ExprData::Member { source, .. } => {
                matches!(types[source.ty], Type::MutPtr(_)) || source.can_addrmut(scopes, types)
            }
            ExprData::Subscript { callee, .. } => {
                matches!(types[callee.ty], Type::MutPtr(_) | Type::RawPtr(_))
                    || callee.can_addrmut(scopes, types)
            }
            _ => true,
        }
    }

    pub fn auto_deref(self, types: &mut Types, target: TypeId) -> Expr {
        let mut needed = 0;
        let mut current = target;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = &types[current] {
            current = *inner;
            needed += 1;
        }

        let mut prev = self.ty;
        let mut ty = self.ty;
        let mut indirection = 0;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = &types[ty] {
            prev = ty;
            ty = *inner;
            indirection += 1;
        }

        if let Type::DynMutPtr(_) | Type::DynPtr(_) = types[ty]
            && !matches!(types[target], Type::DynMutPtr(_) | Type::DynPtr(_))
        {
            indirection += 1;
        }

        match indirection.cmp(&needed) {
            std::cmp::Ordering::Less => {
                if matches!(types[target], Type::Ptr(_)) {
                    Expr::new(
                        types.insert(Type::Ptr(self.ty)),
                        ExprData::Unary(UnaryOp::Addr, self.into()),
                    )
                } else {
                    Expr::new(
                        types.insert(Type::MutPtr(self.ty)),
                        ExprData::Unary(UnaryOp::AddrMut, self.into()),
                    )
                }
            }
            std::cmp::Ordering::Equal => self,
            std::cmp::Ordering::Greater => Expr::new(
                if needed != 0 { prev } else { ty },
                ExprData::AutoDeref(self.into(), indirection - needed),
            ),
        }
    }

    pub fn option_null(opt: TypeId) -> Expr {
        Expr::new(opt, ExprData::VariantInstance(Strings::NULL, Default::default()))
    }

    pub fn void() -> Expr {
        Expr::new(TypeId::VOID, ExprData::Void)
    }
}
