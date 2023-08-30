use thiserror::Error;
use crate::expression::Expr;
use crate::lexer::{Lexer, LexerError};
use crate::token::{CommandType, LiteralType, Token};

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("The lexer returned an error: {0}")]
    Lexer(#[from] LexerError),
    #[error("Unexpected EOF while parsing expression")]
    UnexpectedEof { start: usize },
    #[error("Expected an identifier, got: {token:?}")]
    ExpectedIdentifier { token: Token, start: usize, end: usize },
    #[error("Unexpected token: {token:?}")]
    UnexpectedToken { token: Token, start: usize, end: usize },
    #[error("Unexpected expression type: {expr:?}")]
    UnexpectedExpr { expr: Expr, start: usize, end: usize },
    #[error("Expected a comparison or a conversion")]
    InvalidConditionExpr { start: usize, end: usize },
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

pub type ParserResult = Result<Expr, ParserError>;

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer
        }
    }
    
    pub fn next_expression(&mut self) -> ParserResult {
        loop {
            let start = self.lexer.get_index();
            let token = self.lexer.peek()?;

            return match token {
                Token::Def => self.def_expr(start),
                Token::If => self.if_expr(start),
                Token::While => self.while_expr(start),
                Token::Enddef |
                Token::Else |
                Token::Endif |
                Token::Endwhile |
                Token::As |
                Token::Comparator(_) |
                Token::Type(_) => Err(ParserError::UnexpectedToken {
                    token,
                    start,
                    end: self.lexer.get_index(),
                }),
                Token::Not => self.comparator_expr(start, true),
                Token::Identifier(name) => self.identifier_expr(start, name),
                Token::Literal(literal) => self.literal_expr(start, literal),
                Token::Command(command) => self.command_expr(start, command),
                Token::Newline |
                Token::Comment => {
                    self.next_token(false, 0)?;
                    continue;
                },
                Token::Eof => Ok(Expr::Eof),
            }
        }
    }

    fn next_expression_expect<F: FnMut(&Expr) -> bool>(
        &mut self,
        mut condition: F
    ) -> ParserResult {
        let start = self.lexer.get_index();
        let expr = self.next_expression()?;
        let end = self.lexer.get_index();
        if !condition(&expr) {
            Err(ParserError::UnexpectedExpr { expr, start, end })
        } else {
            Ok(expr)
        }
    }

    fn next_token(&mut self, err_when_eof: bool, start: usize) -> Result<Token, ParserError> {
        match self.lexer.next_token()? {
            Token::Eof => {
                if err_when_eof {
                    Err(ParserError::UnexpectedEof { start })
                } else {
                    Ok(Token::Eof)
                }
            },
            token => Ok(token),
        }
    }

    fn next_token_expect<F: FnMut(&Token) -> bool>(
        &mut self,
        err_when_eof: bool,
        mut condition: F
    ) -> Result<Token, ParserError> {
        let start = self.lexer.get_index();
        let token = self.next_token(err_when_eof, start)?;
        let end = self.lexer.get_index();
        if !condition(&token) {
            Err(ParserError::UnexpectedToken { token, start, end })
        } else {
            Ok(token)
        }
    }

    fn next_token_expect_end(&mut self, err_when_eof: bool) -> Result<(), ParserError> {
        self.next_token_expect(err_when_eof, |token| {
            matches!(token, Token::Newline) || matches!(token, Token::Eof)
        }).map(|_| {})
    }

    fn consume_until<F: FnMut(&Token) -> bool>(
        &mut self,
        mut stop: F,
        err_when_eof: bool,
        start: usize,
    ) -> Result<Vec<Expr>, ParserError> {
        let mut exprs = Vec::new();
        loop {
            match self.lexer.peek()? {
                lex_token if stop(&lex_token) => {
                    self.next_token(true, 0)?;
                    break;
                }
                Token::Eof => {
                    if err_when_eof {
                        return Err(ParserError::UnexpectedEof { start });
                    }
                    break;
                },
                Token::Newline |
                Token::Comment => {
                    self.next_token(false, 0)?;
                }
                _ => {
                    exprs.push(self.next_expression()?)
                }
            }
        }
        Ok(exprs)
    }

    fn consume_until_token(
        &mut self,
        token: Token,
        err_when_eof: bool,
        start: usize
    ) -> Result<Vec<Expr>, ParserError> {
        self.consume_until(|tok| tok == &token, err_when_eof, start)
    }
    
    fn def_expr(&mut self, start: usize) -> ParserResult {
        // Consume def keyword
        self.next_token(true, start)?;

        let ident_start = self.lexer.get_index();
        let name = match self.next_token(true, start)? {
            Token::Identifier(ident) => ident,
            token => return Err(ParserError::ExpectedIdentifier {
                token,
                start: ident_start,
                end: self.lexer.get_index(),
            }),
        };

        self.next_token_expect_end(true)?;

        let body = self.consume_until_token(
            Token::Enddef,
            true,
            start)?;
        Ok(Expr::Def { name, body })
    }

    fn if_expr(&mut self, start: usize) -> ParserResult {
        // Consume if keyword
        self.next_token(true, start)?;

        let condition = self.condition_expr()?;

        let mut has_alternate = false;
        let body = self.consume_until(|tok| {
            match tok {
                Token::Else => {
                    has_alternate = true;
                    true
                }
                Token::Endif => true,
                _ => false,
            }
        }, true, start)?;
        let mut alternate = None;
        if has_alternate {
            let alt_opt = self.consume_until_token(
                Token::Endif, true, start)?;
            alternate = Some(alt_opt);
        }
        Ok(Expr::If {
            condition: Box::new(condition),
            body,
            alternate,
        })
    }

    fn while_expr(&mut self, start: usize) -> ParserResult {
        // Consume while keyword
        self.next_token(true, start)?;

        let condition = self.condition_expr()?;

        let body = self.consume_until_token(
            Token::Endwhile, true, start)?;
        Ok(Expr::While {
            condition: Box::new(condition),
            body,
        })
    }

    fn comparator_expr(&mut self, start: usize, inverse: bool) -> ParserResult {
        if inverse {
            // Consume not keyword
            self.next_token(true, start)?;
        }

        let mut tok_start = start;

        let left = match self.next_token(true, start)? {
            Token::Identifier(ident) => Expr::Identifier(ident),
            Token::Literal(lit) => Expr::Literal(lit),
            token => return Err(ParserError::UnexpectedToken {
                token,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };
        tok_start = self.lexer.get_index();

        let comparator = match self.next_token(true, start)? {
            Token::Comparator(comp) => comp,
            token => return Err(ParserError::UnexpectedToken {
                token,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };
        tok_start = self.lexer.get_index();

        let right = match self.next_token(true, start)? {
            Token::Identifier(ident) => Expr::Identifier(ident),
            Token::Literal(lit) => Expr::Literal(lit),
            token => return Err(ParserError::UnexpectedToken {
                token,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };

        Ok(Expr::Comparator {
            inverse,
            left: Box::new(left),
            right: Box::new(right),
            comparator,
        })
    }

    fn conversion_expr(&mut self, start: usize) -> ParserResult {
        let mut tok_start = start;
        let identifier = match self.next_token(true, start)? {
            Token::Identifier(ident) => Expr::Identifier(ident),
            token => return Err(ParserError::UnexpectedToken {
                token,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };

        // Consume as keyword
        tok_start = self.lexer.get_index();
        match self.next_token(true, start)? {
            Token::As => {}
            token => return Err(ParserError::UnexpectedToken {
                token,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        }

        tok_start = self.lexer.get_index();
        let typ = match self.next_token(true, start)? {
            Token::Type(typ) => typ,
            token => return Err(ParserError::UnexpectedToken {
                token,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };

        Ok(Expr::Conversion {
            ident: Box::new(identifier),
            to_type: typ,
        })
    }

    fn condition_expr(&mut self) -> ParserResult {
        let start = self.lexer.get_index();
        let condition_unchecked = self.next_expression()?;
        match condition_unchecked {
            Expr::Comparator { .. } => Ok(condition_unchecked),
            Expr::Conversion { .. } => Ok(condition_unchecked),
            Expr::Literal(LiteralType::Bool(_)) => Ok(condition_unchecked),
            _ => Err(ParserError::InvalidConditionExpr {
                start,
                end: self.lexer.get_index(),
            })
        }
    }

    fn identifier_expr(&mut self, start: usize, name: String) -> ParserResult {
        match self.lexer.peek_distance(1)? {
            Token::Comparator(_) => self.comparator_expr(start, false),
            Token::As => self.conversion_expr(start),
            _ => {
                self.next_token(false, start)?;
                Ok(Expr::Identifier(name))
            }
        }
    }

    fn literal_expr(&mut self, start: usize, literal: LiteralType) -> ParserResult {
        match self.lexer.peek_distance(1)? {
            Token::Comparator(_) => self.comparator_expr(start, false),
            _ => {
                self.next_token(false, start)?;
                Ok(Expr::Literal(literal))
            }
        }
    }

    fn command_expr(&mut self, start: usize, command: CommandType) -> ParserResult {
        // Consume command name
        self.next_token(true, start)?;

        match command {
            CommandType::Move => self.command_binary_expr(CommandType::Move),
            CommandType::Not => self.command_unary_expr(CommandType::Not),
            CommandType::And => self.command_binary_expr(CommandType::And),
            CommandType::Or => self.command_binary_expr(CommandType::And),
            CommandType::Xor => self.command_binary_expr(CommandType::Xor),
            CommandType::Add => self.command_binary_expr(CommandType::Add),
            CommandType::Sub => self.command_binary_expr(CommandType::Sub),
            CommandType::Mul => self.command_binary_expr(CommandType::Mul),
            CommandType::Div => self.command_binary_expr(CommandType::Div),
            CommandType::Call => self.command_unary_expr(CommandType::Call),
            CommandType::Return => self.command_single_expr(CommandType::Return),
            CommandType::Break => self.command_single_expr(CommandType::Break),
            CommandType::Get => self.command_unary_expr(CommandType::Get),
            CommandType::Put => self.command_variadic_expr(CommandType::Put),
        }
    }

    fn command_single_expr(&mut self, command: CommandType) -> ParserResult {
        self.next_token_expect_end(false)?;

        Ok(Expr::Command {
            command,
            args: vec![],
        })
    }

    fn command_unary_expr(&mut self, command: CommandType) -> ParserResult {
        let arg1 = self.next_expression_expect(|expr| {
            matches!(expr, Expr::Identifier(_))
        })?;

        self.next_token_expect_end(false)?;

        Ok(Expr::Command {
            command,
            args: vec![arg1],
        })
    }

    fn command_binary_expr(&mut self, command: CommandType) -> ParserResult {
        let arg1 = self.next_expression_expect(|expr| {
            matches!(expr, Expr::Identifier(_))
        })?;

        let arg2 = self.next_expression_expect(|expr| {
            matches!(expr, Expr::Identifier(_)) || matches!(expr, Expr::Literal(_))
        })?;

        self.next_token_expect_end(false)?;

        Ok(Expr::Command {
            command,
            args: vec![arg1, arg2],
        })
    }

    fn command_variadic_expr(&mut self, command: CommandType) -> ParserResult {
        let mut args = Vec::new();

        loop {
            let start = self.lexer.get_index();
            match self.next_token(false, 0)? {
                Token::Newline |
                Token::Eof => break,
                Token::Identifier(ident) => args.push(Expr::Identifier(ident)),
                Token::Literal(lit) => args.push(Expr::Literal(lit)),
                token => return Err(ParserError::UnexpectedToken {
                    token,
                    start,
                    end: self.lexer.get_index(),
                }),
            }
        }

        Ok(Expr::Command {
            command,
            args,
        })
    }
}
