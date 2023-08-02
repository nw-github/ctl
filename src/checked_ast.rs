use indexmap::IndexMap;

use crate::{
    ast::{BinaryOp, UnaryOp},
    typecheck::{GenericFunc, ScopeId, Symbol, TypeId, VariableId},
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
    Map(Vec<(CheckedExpr, CheckedExpr)>),
    Bool(bool),
    Signed(i128),
    Unsigned(u128),
    Float(String),
    String(String),
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
    Continue,
    #[default]
    Error,
}

#[derive(Debug, Default, Clone, derive_more::Constructor)]
pub struct CheckedExpr {
    pub ty: TypeId,
    pub data: CheckedExprData,
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
