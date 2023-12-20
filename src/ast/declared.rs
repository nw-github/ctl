use crate::{
    ast::Attribute,
    ast::parsed::{Expr, ImplBlock, Path, TypeHint, Pattern},
    lexer::Span,
    typecheck::{FunctionId, ScopeId, UserTypeId, VariableId},
};

pub struct DeclaredStmt {
    pub data: DeclaredStmtData,
    pub span: Span,
    pub attrs: Vec<Attribute>,
}

pub struct DeclaredFn {
    pub id: FunctionId,
    pub body: Option<Vec<DeclaredStmt>>,
}

pub struct DeclaredStruct {
    pub id: UserTypeId,
    pub impls: Vec<DeclaredImplBlock>,
    pub functions: Vec<DeclaredFn>,
    pub type_params: Vec<UserTypeId>,
}

// pub struct DeclaredImplBlock {
//     // ScopeId?
//     pub functions: Vec<DeclaredFn>,
// }
pub type DeclaredImplBlock = ImplBlock;

pub enum DeclaredStmtData {
    Expr(Expr),
    Use {
        public: bool,
        path: Path,
        all: bool,
    },
    Let {
        patt: Pattern,
        ty: Option<TypeHint>,
        mutable: bool,
        value: Option<Expr>,
    },
    Fn(DeclaredFn),
    Struct {
        init: DeclaredFn,
        base: DeclaredStruct,
    },
    Union {
        member_cons: Vec<DeclaredFn>,
        base: DeclaredStruct,
    },
    Trait {
        id: UserTypeId,
        functions: Vec<DeclaredFn>,
        type_params: Vec<UserTypeId>,
    },
    Enum {
        id: UserTypeId,
        impls: Vec<DeclaredImplBlock>,
        variants: Vec<(String, Option<Expr>)>,
        functions: Vec<DeclaredFn>,
    },
    Static {
        id: VariableId,
        value: Expr,
    },
    Module {
        id: ScopeId,
        body: Vec<DeclaredStmt>,
    },
    Error,
}
