use std::ops::RangeInclusive;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ComparatorType {
    GreaterThan,
    LessThan,
    Equal,
    GreaterOrEqualThan,
    LessOrEqualThan,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum CommandType {
    Move,

    Not,
    And,
    Or,
    Xor,

    Add,
    Sub,
    Mul,
    Div,

    Call,
    Return,
    Break,

    Get,
    Put,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Number(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Type {
    Number,
    String,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Def,
    Enddef,
    If,
    Else,
    Endif,
    While,
    Endwhile,
    Not,
    As,
    Comparator(ComparatorType),
    Identifier(String),
    Command(CommandType),
    Literal(LiteralType),
    Type(Type),
    Newline,
    Comment,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub index_range: RangeInclusive<usize>,
    pub token_type: TokenType,
}

impl Token {
    pub fn new(start_index: usize, end_index: usize, expr_type: TokenType) -> Self {
        Self {
            index_range: start_index..=end_index,
            token_type: expr_type,
        }
    }
}
