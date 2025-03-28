use std::ops::RangeInclusive;
use crate::token::{CommandType, ComparatorType, LiteralType, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct DefExpr {
    pub name: String,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub body: Vec<Expr>,
    pub alternate: Option<Vec<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpr {
    pub condition: Box<Expr>,
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComparatorExpr {
    pub inverse: bool,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub comparator: ComparatorType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConversionExpr {
    pub inverse: bool,
    pub ident: Box<Expr>,
    pub to_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CommandExpr {
    pub command: CommandType,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprType {
    Def(DefExpr),
    If(IfExpr),
    While(WhileExpr),
    Comparator(ComparatorExpr),
    Conversion(ConversionExpr),
    Command(CommandExpr),
    Identifier(String),
    Literal(LiteralType),
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub index_range: RangeInclusive<usize>,
    pub expr_type: ExprType,
}

impl Expr {
    pub fn new(start_index: usize, end_index: usize, expr_type: ExprType) -> Self {
        Self {
            index_range: start_index..=end_index,
            expr_type,
        }
    }
}
