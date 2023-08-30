use crate::token::{CommandType, ComparatorType, LiteralType, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Def {
        name: String,
        body: Vec<Expr>,
    },
    If {
        condition: Box<Expr>,
        body: Vec<Expr>,
        alternate: Option<Vec<Expr>>,
    },
    While {
        condition: Box<Expr>,
        body: Vec<Expr>,
    },
    Comparator {
        inverse: bool,
        left: Box<Expr>,
        right: Box<Expr>,
        comparator: ComparatorType,
    },
    Conversion {
        ident: Box<Expr>,
        to_type: Type,
    },
    Command {
        command: CommandType,
        args: Vec<Expr>,
    },
    Identifier(String),
    Literal(LiteralType),
    Eof,
}
