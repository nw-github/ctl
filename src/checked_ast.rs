use indexmap::IndexMap;
use num_bigint::{BigInt, BigUint};

use crate::{
    ast::{BinaryOp, UnaryOp},
    typecheck::{GenericFunc, ScopeId, Scopes, Symbol, TypeId, VariableId},
};

#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<CheckedStmt>,
    pub scope: ScopeId,
}

#[derive(Debug, Clone, Default)]
pub enum CheckedPattern {
    UnionMember {
        binding: Option<VariableId>,
        variant: (String, usize),
        ptr: bool,
    },
    CatchAll(VariableId),
    #[default]
    Error,
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
    Call {
        func: GenericFunc,
        args: IndexMap<String, CheckedExpr>,
        inst: Option<TypeId>,
        trait_fn: bool,
    },
    CallFnPtr {
        expr: Box<CheckedExpr>,
        args: Vec<CheckedExpr>, 
    },
    Instance {
        members: IndexMap<String, CheckedExpr>,
        variant: Option<String>,
    },
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
    Set(Vec<CheckedExpr>),
    Map(Vec<(CheckedExpr, CheckedExpr)>),
    Bool(bool),
    Signed(BigInt),
    Unsigned(BigUint),
    Float(String),
    String(String),
    ByteString(String),
    Char(char),
    Void,
    Symbol(Symbol),
    Assign {
        target: Box<CheckedExpr>,
        binary: Option<BinaryOp>,
        value: Box<CheckedExpr>,
    },
    Block(Block),
    If {
        cond: Box<CheckedExpr>,
        if_branch: Box<CheckedExpr>,
        else_branch: Option<Box<CheckedExpr>>,
    },
    Loop {
        cond: Option<Box<CheckedExpr>>,
        iter: Option<VariableId>,
        body: Block,
        do_while: bool,
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
    Return(Box<CheckedExpr>),
    Yield(Box<CheckedExpr>),
    Break(Box<CheckedExpr>),
    Lambda(Vec<CheckedStmt>),
    Continue,
    #[default]
    Error,
}

#[derive(Debug, Default, Clone, derive_more::Constructor)]
pub struct CheckedExpr {
    pub ty: TypeId,
    pub data: CheckedExprData,
}

impl CheckedExpr {
    pub fn is_assignable(&self, scopes: &Scopes) -> bool {
        match &self.data {
            CheckedExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, TypeId::MutPtr(_))
            }
            CheckedExprData::Symbol(_) | CheckedExprData::Member { .. } => self.can_addrmut(scopes),
            CheckedExprData::Subscript { callee, .. } => callee.is_assignable(scopes),
            _ => false,
        }
    }

    pub fn can_addrmut(&self, scopes: &Scopes) -> bool {
        match &self.data {
            CheckedExprData::Unary { op, expr } => {
                !matches!(op, UnaryOp::Deref) || matches!(expr.ty, TypeId::MutPtr(_))
            }
            CheckedExprData::Symbol(symbol) => match symbol {
                Symbol::Func(_) => false,
                Symbol::Var(id) => scopes.get_var(*id).mutable,
            },
            CheckedExprData::Member { source, .. } => {
                matches!(source.ty, TypeId::MutPtr(_)) || source.can_addrmut(scopes)
            }
            CheckedExprData::Subscript { callee, .. } => callee.can_addrmut(scopes),
            _ => true,
        }
    }

    pub fn coerce_to(self, target: &TypeId, scopes: &Scopes) -> CheckedExpr {
        match (&self.ty, target) {
            (
                TypeId::IntGeneric,
                TypeId::Int(_) | TypeId::Uint(_) | TypeId::Isize | TypeId::Usize,
            ) => CheckedExpr::new(target.clone(), self.data),
            (TypeId::FloatGeneric, TypeId::F32 | TypeId::F64) => {
                CheckedExpr::new(target.clone(), self.data)
            }
            (TypeId::MutPtr(lhs), TypeId::Ptr(rhs)) if lhs == rhs => {
                CheckedExpr::new(target.clone(), self.data)
            }
            (ty, target)
                if scopes
                    .as_option_inner(target)
                    .map_or(false, |inner| ty.coerces_to(scopes, inner)) =>
            {
                let inner = scopes.as_option_inner(target).unwrap();
                let expr = self.coerce_to(inner, scopes);
                CheckedExpr::new(
                    scopes
                        .make_lang_type("option", vec![expr.ty.clone()])
                        .unwrap(),
                    CheckedExprData::Instance {
                        members: [("Some".into(), expr)].into(),
                        variant: Some("Some".into()),
                    },
                )
                .coerce_to(target, scopes)
            }
            (TypeId::Never, _) => CheckedExpr::new(target.clone(), self.data),
            _ => self,
        }
    }

    pub fn auto_deref(mut self, mut target: &TypeId) -> CheckedExpr {
        let mut indirection = 0;
        while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = target {
            target = inner;
            indirection += 1;
        }

        let mut ty = &self.ty;
        let mut my_indirection = 0;
        while let TypeId::Ptr(inner) | TypeId::MutPtr(inner) = ty {
            ty = inner;
            my_indirection += 1;
        }

        while my_indirection > indirection {
            my_indirection -= 1;
            let (TypeId::Ptr(inner) | TypeId::MutPtr(inner)) = self.ty.clone() else {
                unreachable!()
            };
            self = CheckedExpr::new(
                (*inner).clone(),
                CheckedExprData::Unary {
                    op: UnaryOp::Deref,
                    expr: self.into(),
                },
            );
        }

        self
    }
}

#[derive(Debug, Default, Clone)]
pub enum CheckedStmt {
    Expr(CheckedExpr),
    Let(VariableId),
    Module(Block),
    None,
    #[default]
    Error,
}
