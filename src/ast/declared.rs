use crate::{
    ast::parsed::{Expr, Pattern, TypeHint},
    lexer::{Located, Span},
    sym::{ExtensionId, FunctionId, ScopeId, UserTypeId, VariableId},
};

pub struct DeclaredFn {
    pub public: bool,
    pub id: FunctionId,
    pub body: Option<Vec<DeclaredStmt>>,
}

pub struct DeclaredImplBlock {
    pub impl_index: usize,
    pub span: Span,
    pub scope: ScopeId,
    pub functions: Vec<DeclaredFn>,
}

pub enum DeclaredStmt {
    Expr(Expr),
    Let {
        patt: Located<Pattern>,
        ty: Option<TypeHint>,
        value: Option<Expr>,
    },
    Static {
        id: VariableId,
        value: Expr,
    },
    Fn(DeclaredFn),
    Struct {
        id: UserTypeId,
        impl_blocks: Vec<DeclaredImplBlock>,
        functions: Vec<DeclaredFn>,
        init: DeclaredFn,
    },
    Union {
        id: UserTypeId,
        impl_blocks: Vec<DeclaredImplBlock>,
        functions: Vec<DeclaredFn>,
    },
    Trait {
        id: UserTypeId,
        functions: Vec<DeclaredFn>,
    },
    Extension {
        id: ExtensionId,
        impl_blocks: Vec<DeclaredImplBlock>,
        functions: Vec<DeclaredFn>,
    },
    Module {
        id: ScopeId,
        body: Vec<DeclaredStmt>,
    },
    None,
}
