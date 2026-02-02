use enum_as_inner::EnumAsInner;

use crate::{
    Span,
    ast::{Alignment, BinaryOp, Sign, UnaryOp},
    ds::{
        ComptimeInt, IndexMap,
        arena::{self, Arena},
    },
    intern::{ByteStrId, StrId, Strings},
    project::Project,
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
    Destructure {
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

#[derive(Clone)]
pub enum Stmt {
    Expr(Expr),
    Let(Pattern, Option<Expr>),
    Defer(Expr),
    Guard { cond: Expr, body: Expr },
}

pub type ExprId = arena::Id<ExprData>;

#[derive(Clone)]
pub enum ExprData {
    Binary(BinaryOp, Expr, Expr),
    Unary(UnaryOp, Expr),
    Deref(Expr, usize),
    Call { callee: Expr, args: IndexMap<StrId, Expr>, scope: ScopeId, span: Span },
    CallDyn(GenericFn, IndexMap<StrId, Expr>),
    CallFnPtr(Expr, Vec<Expr>),
    Instance(IndexMap<StrId, Expr>),
    VariantInstance(StrId, IndexMap<StrId, Expr>),
    DynCoerce(Expr),
    SpanMutCoerce(Expr),
    ClosureToFnPtr(Expr),
    Array(Vec<Expr>),
    ArrayWithInit { init: Expr, count: usize },
    Vec(Vec<Expr>),
    VecWithInit { init: Expr, count: Expr },
    Set(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    Int(ComptimeInt),
    Float(f64),
    String(StrId),
    GeneratedString(String),
    StringInterp { strings: Vec<StrId>, args: Vec<(Expr, FormatOpts)> },
    ByteString(ByteStrId),
    Void,
    Fn(GenericFn),
    MemFn(MemberFn),
    Var(VariableId),
    Block(Block),
    AffixOperator { callee: Expr, mfn: MemberFn, scope: ScopeId, postfix: bool, span: Span },
    If { cond: Expr, if_branch: Expr, else_branch: Expr, dummy_scope: ScopeId },
    Loop { cond: Option<Expr>, body: Block, do_while: bool, optional: bool },
    Match { scrutinee: Expr, body: Vec<(Pattern, Expr)>, dummy_scope: ScopeId },
    Member { source: Expr, member: StrId },
    As(Expr),
    Is(Expr, Pattern),
    Return(Expr),
    Yield(Expr, ScopeId),
    Break(Expr, ScopeId),
    NeverCoerce(Expr),
    Discard(Expr),
    Continue(ScopeId),
    Error,
}

impl ExprData {
    pub fn is_yielding_block(&self, scopes: &Scopes) -> Option<bool> {
        match self {
            ExprData::Block(block) => Some(block.is_yielding(scopes)),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, derive_more::Constructor)]
pub struct Expr {
    pub ty: TypeId,
    pub data: ExprId,
}

impl Expr {
    pub const VOID: Self = ExprArena::VOID;

    pub fn is_assignable(&self, proj: &Project, arena: &ExprArena) -> bool {
        match arena.get(self.data) {
            ExprData::Deref(expr, _) => {
                matches!(proj.types[expr.ty], Type::MutPtr(_) | Type::RawMutPtr(_))
            }
            ExprData::Var(id) => proj.scopes.get(*id).mutable,
            ExprData::Member { source, .. } => source.is_assignable(proj, arena),
            _ => false,
        }
    }

    pub fn can_addrmut(&self, proj: &Project, arena: &ExprArena) -> bool {
        match arena.get(self.data) {
            ExprData::Deref(expr, _) => {
                matches!(proj.types[expr.ty], Type::MutPtr(_) | Type::RawMutPtr(_))
            }
            ExprData::Var(id) => proj.scopes.get(*id).mutable,
            ExprData::Member { source, .. } => {
                matches!(proj.types[source.ty], Type::MutPtr(_)) || source.can_addrmut(proj, arena)
            }
            _ => true,
        }
    }

    pub fn auto_deref_ex(
        self,
        types: &Types,
        target: TypeId,
        arena: &mut ExprArena,
        adjust_dyn: bool,
    ) -> Self {
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

        if adjust_dyn
            && let Type::DynMutPtr(_) | Type::DynPtr(_) = types[ty]
            && !matches!(types[target], Type::DynMutPtr(_) | Type::DynPtr(_))
        {
            indirection += 1;
            prev = ty;
        }

        match indirection.cmp(&needed) {
            std::cmp::Ordering::Less => {
                if matches!(types[target], Type::Ptr(_)) {
                    Expr::new(
                        types.insert(Type::Ptr(self.ty)),
                        arena.alloc(ExprData::Unary(UnaryOp::Addr, self)),
                    )
                } else {
                    Expr::new(
                        types.insert(Type::MutPtr(self.ty)),
                        arena.alloc(ExprData::Unary(UnaryOp::AddrMut, self)),
                    )
                }
            }
            std::cmp::Ordering::Equal => self,
            std::cmp::Ordering::Greater => Expr::new(
                if needed != 0 { prev } else { ty },
                arena.alloc(ExprData::Deref(self, indirection - needed)),
            ),
        }
    }

    pub fn auto_deref(self, types: &Types, target: TypeId, arena: &mut ExprArena) -> Self {
        self.auto_deref_ex(types, target, arena, true)
    }

    pub fn option_some(opt: TypeId, val: Expr, arena: &mut ExprArena) -> Self {
        Expr::new(
            opt,
            arena.alloc(ExprData::VariantInstance(
                Strings::SOME,
                [(Strings::TUPLE_ZERO, val)].into(),
            )),
        )
    }

    pub fn option_null(opt: TypeId, arena: &mut ExprArena) -> Self {
        Expr::new(opt, arena.alloc(ExprData::VariantInstance(Strings::NULL, Default::default())))
    }

    pub fn clone_at(&self, new_scope: ScopeId, new_span: Span, arena: &mut ExprArena) -> Self {
        // TODO: do this recursively
        if let &ExprData::Call { callee, ref args, .. } = arena.get(self.data) {
            arena.typed(
                self.ty,
                ExprData::Call { callee, args: args.clone(), scope: new_scope, span: new_span },
            )
        } else {
            *self
        }
    }

    pub fn from_char(value: char, arena: &mut ExprArena) -> Self {
        Self::from_int(TypeId::CHAR, ComptimeInt::from(value as u32), arena)
    }

    pub fn from_bool(value: bool, arena: &mut ExprArena) -> Self {
        Self::from_int(TypeId::BOOL, ComptimeInt::from(value as u32), arena)
    }

    pub fn from_int(ty: TypeId, value: ComptimeInt, arena: &mut ExprArena) -> Self {
        Expr::new(ty, arena.alloc(ExprData::Int(value)))
    }

    pub fn member_call(
        ret: TypeId,
        types: &Types,
        mfn: MemberFn,
        args: IndexMap<StrId, Expr>,
        scope: ScopeId,
        span: Span,
        arena: &mut ExprArena,
    ) -> Self {
        let func = mfn.func.clone();
        let callee = arena.alloc(ExprData::MemFn(mfn));
        Self {
            ty: ret,
            data: arena.alloc(ExprData::Call {
                callee: Expr::new(types.insert(Type::Fn(func)), callee),
                args,
                scope,
                span,
            }),
        }
    }

    pub fn call(
        ret: TypeId,
        types: &Types,
        func: GenericFn,
        args: IndexMap<StrId, Expr>,
        scope: ScopeId,
        span: Span,
        arena: &mut ExprArena,
    ) -> Self {
        let callee = arena.alloc(ExprData::Fn(func.clone()));
        Expr {
            ty: ret,
            data: arena.alloc(ExprData::Call {
                callee: Expr::new(types.insert(Type::Fn(func)), callee),
                args,
                scope,
                span,
            }),
        }
    }
}

impl Default for Expr {
    fn default() -> Self {
        ExprArena::ERROR
    }
}

#[derive(Clone)]
pub struct FormatOpts {
    pub width: Expr,
    pub prec: Expr,
    pub fill: char,
    pub align: Option<Alignment>,
    pub sign: Option<Sign>,
    pub alt: bool,
    pub zero: bool,
    pub upper: bool,
    pub func: MemberFn,
}

#[derive(derive_more::Deref, derive_more::DerefMut)]
pub struct ExprArena {
    arena: Arena<ExprData>,
}

impl ExprArena {
    pub const ERROR: Expr = Expr { ty: TypeId::UNKNOWN, data: ExprId::new(0) };
    pub const VOID: Expr = Expr { ty: TypeId::VOID, data: ExprId::new(1) };
    pub const ZERO: ExprId = ExprId::new(2);

    pub fn new() -> Self {
        Self::with_capacity(2048)
    }

    pub fn with_capacity(cap: usize) -> Self {
        let mut this = Self { arena: Arena::with_capacity(cap) };
        this.alloc(ExprData::Error);
        this.alloc(ExprData::Void);
        this.alloc(ExprData::Int(ComptimeInt::Small(0)));
        this
    }

    pub fn typed(&mut self, ty: TypeId, data: ExprData) -> Expr {
        Expr { ty, data: self.alloc(data) }
    }
}

impl Default for ExprArena {
    fn default() -> Self {
        Self::new()
    }
}
