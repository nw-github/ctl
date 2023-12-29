use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use num_bigint::BigInt;

use crate::{
    ast::{BinaryOp, UnaryOp},
    sym::{ScopeId, Scopes, UserTypeId, VariableId},
    typeid::{GenericFunc, Type},
};

use super::parsed::RangePattern;

#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<CheckedStmt>,
    pub scope: ScopeId,
}

#[derive(Debug, Clone)]
pub enum RestPattern {
    Start(Option<VariableId>),
    Middle(usize, Option<VariableId>),
    End(Option<VariableId>),
}

#[derive(Debug, Clone)]
pub enum IrrefutablePattern {
    Variable(VariableId),
    Destrucure(Vec<(String, IrrefutablePattern)>),
    Array(Option<RestPattern>, Vec<IrrefutablePattern>),
}

#[derive(Debug, Clone, Default, EnumAsInner)]
pub enum CheckedPattern {
    UnionMember {
        pattern: Option<Box<IrrefutablePattern>>,
        variant: (String, usize),
        ptr: bool,
    },
    Irrefutable(IrrefutablePattern),
    Destrucure(Vec<(String, CheckedPattern)>),
    Int(BigInt),
    IntRange(RangePattern<BigInt>),
    String(String),
    Array(Option<RestPattern>, Vec<CheckedPattern>),
    #[default]
    Error,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Symbol {
    Func(GenericFunc),
    Var(VariableId),
}

#[derive(Debug, Default, Clone)]
pub enum CheckedStmt {
    Expr(CheckedExpr),
    Let(VariableId),
    LetPattern(IrrefutablePattern, CheckedExpr),
    Module(Block),
    None,
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
        inst: Option<Type>,
        trait_id: Option<UserTypeId>,
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
    Integer(BigInt),
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
    pub ty: Type,
    pub data: CheckedExprData,
}

impl CheckedExpr {
    pub fn is_assignable(&self, scopes: &Scopes) -> bool {
        match &self.data {
            CheckedExprData::Unary { op, expr } => {
                matches!(op, UnaryOp::Deref) && matches!(expr.ty, Type::MutPtr(_))
            }
            CheckedExprData::Symbol(_) | CheckedExprData::Member { .. } => self.can_addrmut(scopes),
            CheckedExprData::Subscript { callee, .. } => callee.is_assignable(scopes),
            _ => false,
        }
    }

    pub fn can_addrmut(&self, scopes: &Scopes) -> bool {
        match &self.data {
            CheckedExprData::Unary { op, expr } => {
                !matches!(op, UnaryOp::Deref) || matches!(expr.ty, Type::MutPtr(_))
            }
            CheckedExprData::Symbol(symbol) => match symbol {
                Symbol::Func(_) => false,
                Symbol::Var(var) => scopes.get(*var).mutable,
            },
            CheckedExprData::Member { source, .. } => {
                matches!(source.ty, Type::MutPtr(_)) || source.can_addrmut(scopes)
            }
            CheckedExprData::Subscript { callee, .. } => callee.can_addrmut(scopes),
            _ => true,
        }
    }

    pub fn coerce_to(self, target: &Type, scopes: &Scopes) -> CheckedExpr {
        match (&self.ty, target) {
            (Type::MutPtr(lhs), Type::Ptr(rhs)) if lhs == rhs => {
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
            (Type::Never, _) => CheckedExpr::new(target.clone(), self.data),
            _ => self,
        }
    }

    pub fn auto_deref(mut self, mut target: &Type) -> CheckedExpr {
        let mut indirection = 0;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = target {
            target = inner;
            indirection += 1;
        }

        let mut ty = &self.ty;
        let mut my_indirection = 0;
        while let Type::Ptr(inner) | Type::MutPtr(inner) = ty {
            ty = inner;
            my_indirection += 1;
        }

        while my_indirection > indirection {
            my_indirection -= 1;
            let (Type::Ptr(inner) | Type::MutPtr(inner)) = self.ty.clone() else {
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
