use thiserror::Error;
use crate::expression::{CommandExpr, ComparatorExpr, ConversionExpr, DefExpr, Expr, ExprType, IfExpr, WhileExpr};
use crate::lexer::{Lexer, LexerError};
use crate::token::{CommandType, LiteralType, Token, TokenType};

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("The lexer returned an error: {0}")]
    Lexer(#[from] LexerError),
    #[error("Unexpected EOF while parsing expression")]
    UnexpectedEof { start: usize },
    #[error("Expected an identifier, got: {token:?}")]
    ExpectedIdentifier { token: Token, start: usize, end: usize },
    #[error("Unexpected token: {expr_type:?}")]
    UnexpectedToken { expr_type: TokenType, start: usize, end: usize },
    #[error("Unexpected expression type: {expr:?}")]
    UnexpectedExpr { expr: Expr, start: usize, end: usize },
    #[error("Expected a comparison or a conversion")]
    InvalidConditionExpr { start: usize, end: usize },
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

pub type ParserResult = Result<Expr, ParserError>;
pub type ParserTypeResult = Result<ExprType, ParserError>;

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer
        }
    }

    pub fn get_index(&self) -> usize {
        self.lexer.get_index()
    }
    
    pub fn next_expression(&mut self) -> ParserResult {
        loop {
            let start = self.get_index();
            let token = self.lexer.peek()?;

            let expr_type = match token.token_type {
                TokenType::Def => self.def_expr(start),
                TokenType::If => self.if_expr(start),
                TokenType::While => self.while_expr(start),
                TokenType::Endwhile |
                TokenType::Else |
                TokenType::As |
                TokenType::Comparator(_) |
                TokenType::Endif |
                TokenType::Enddef |
                TokenType::Type(_) => Err(ParserError::UnexpectedToken {
                    expr_type: token.token_type,
                    start,
                    end: self.lexer.get_index(),
                }),
                TokenType::Not => self.comparator_expr(start, true),
                TokenType::Identifier(name) => self.identifier_expr(start, name),
                TokenType::Literal(literal) => self.literal_expr(start, literal),
                TokenType::Command(command) => self.command_expr(start, command),
                TokenType::Newline |
                TokenType::Comment => {
                    self.next_token(false, 0)?;
                    continue;
                },
                TokenType::Eof => Ok(ExprType::Eof),
            }?;
            
            return Ok(Expr::new(start, self.get_index(), expr_type));
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
            Token { index_range: _, token_type: TokenType::Eof } => {
                if err_when_eof {
                    Err(ParserError::UnexpectedEof { start })
                } else {
                    Ok(Token::new(self.get_index(), self.get_index(), TokenType::Eof))
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
            Err(ParserError::UnexpectedToken { expr_type: token.token_type, start, end })
        } else {
            Ok(token)
        }
    }

    fn next_token_expect_end(&mut self, err_when_eof: bool) -> Result<(), ParserError> {
        self.next_token_expect(err_when_eof, |token| {
            matches!(token, Token { index_range: _, token_type: TokenType::Newline })
                || matches!(token, Token { index_range: _, token_type: TokenType::Eof })
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
                Token { index_range: _, token_type: TokenType::Eof } => {
                    if err_when_eof {
                        return Err(ParserError::UnexpectedEof { start });
                    }
                    break;
                },
                Token { index_range: _, token_type: TokenType::Newline } |
                Token { index_range: _, token_type: TokenType::Comment } => {
                    self.next_token(false, 0)?;
                }
                _ => {
                    exprs.push(self.next_expression()?)
                }
            }
        }
        Ok(exprs)
    }

    fn consume_until_token_type(
        &mut self,
        token_type: TokenType,
        err_when_eof: bool,
        start: usize
    ) -> Result<Vec<Expr>, ParserError> {
        self.consume_until(
            |tok| tok.token_type == token_type,
            err_when_eof,
            start)
    }
    
    fn def_expr(&mut self, start: usize) -> ParserTypeResult {
        // Consume def keyword
        self.next_token(true, start)?;

        let ident_start = self.lexer.get_index();
        let name = match self.next_token(true, start)? {
            Token { index_range: _, token_type: TokenType::Identifier(ident) } => ident,
            token => return Err(ParserError::ExpectedIdentifier {
                token,
                start: ident_start,
                end: self.lexer.get_index(),
            }),
        };

        self.next_token_expect_end(true)?;

        let body = self.consume_until_token_type(
            TokenType::Enddef,
            true,
            start)?;
        Ok(ExprType::Def(DefExpr { 
            name, 
            body 
        }))
    }

    fn if_expr(&mut self, start: usize) -> ParserTypeResult {
        // Consume if keyword
        self.next_token(true, start)?;
        
        let condition = {
            let start = self.get_index();
            let cond_type = self.condition_expr()?;
            let end = self.get_index();
            Expr::new(start, end, cond_type)
        };

        let mut has_alternate = false;
        let body = self.consume_until(|tok| {
            match tok {
                Token { index_range: _, token_type: TokenType::Else } => {
                    has_alternate = true;
                    true
                }
                Token { index_range: _, token_type: TokenType::Endif } => true,
                _ => false,
            }
        }, true, start)?;
        let mut alternate = None;
        if has_alternate {
            let alt_opt = self.consume_until_token_type(
                TokenType::Endif, true, start)?;
            alternate = Some(alt_opt);
        }
        Ok(ExprType::If(IfExpr {
            condition: Box::new(condition),
            body,
            alternate,
        }))
    }

    fn while_expr(&mut self, start: usize) -> ParserTypeResult {
        // Consume while keyword
        self.next_token(true, start)?;

        let condition = {
            let start = self.get_index();
            let cond_type = self.condition_expr()?;
            let end = self.get_index();
            Expr::new(start, end, cond_type)
        };

        let body = self.consume_until_token_type(
            TokenType::Endwhile, true, start)?;
        Ok(ExprType::While(WhileExpr {
            condition: Box::new(condition),
            body,
        }))
    }

    fn comparator_expr(&mut self, start: usize, inverse: bool) -> ParserTypeResult {
        if inverse {
            // Consume not keyword
            self.next_token(true, start)?;
        }

        let mut tok_start = start;

        let left = match self.next_token(true, start)? {
            Token { index_range, token_type: TokenType::Identifier(ident) } =>
                Expr::new(*index_range.start(), *index_range.end(), ExprType::Identifier(ident)),
            Token { index_range, token_type: TokenType::Literal(lit) } =>
                Expr::new(*index_range.start(), *index_range.end(), ExprType::Literal(lit)),
            token => return Err(ParserError::UnexpectedToken {
                expr_type: token.token_type,
                start: tok_start,
                end: self.get_index(),
            })
        };
        tok_start = self.lexer.get_index();

        let comparator = match self.next_token(true, start)?.token_type {
            TokenType::Comparator(comp) => comp,
            expr_type => return Err(ParserError::UnexpectedToken {
                expr_type,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };
        tok_start = self.lexer.get_index();

        let right = match self.next_token(true, start)? {
            Token { index_range, token_type: TokenType::Identifier(ident) } =>
                Expr::new(*index_range.start(), *index_range.end(), ExprType::Identifier(ident)),
            Token { index_range, token_type: TokenType::Literal(lit) } =>
                Expr::new(*index_range.start(), *index_range.end(), ExprType::Literal(lit)),
            token => return Err(ParserError::UnexpectedToken {
                expr_type: token.token_type,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };

        Ok(ExprType::Comparator(ComparatorExpr {
            inverse,
            left: Box::new(left),
            right: Box::new(right),
            comparator,
        }))
    }

    fn conversion_expr(&mut self, start: usize) -> ParserTypeResult {
        let mut tok_start = start;
        let identifier = match self.next_token(true, start)? {
            Token { index_range, token_type: TokenType::Identifier(ident) } =>
                Expr::new(*index_range.start(), *index_range.end(), ExprType::Identifier(ident)),
            token => return Err(ParserError::UnexpectedToken {
                expr_type: token.token_type,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };

        // Consume as keyword
        tok_start = self.lexer.get_index();
        match self.next_token(true, start)?.token_type {
            TokenType::As => {}
            expr_type => return Err(ParserError::UnexpectedToken {
                expr_type,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        }

        tok_start = self.lexer.get_index();
        let typ = match self.next_token(true, start)?.token_type {
            TokenType::Type(typ) => typ,
            expr_type => return Err(ParserError::UnexpectedToken {
                expr_type,
                start: tok_start,
                end: self.lexer.get_index(),
            })
        };

        Ok(ExprType::Conversion(ConversionExpr {
            ident: Box::new(identifier),
            to_type: typ,
        }))
    }

    fn condition_expr(&mut self) -> ParserTypeResult {
        let start = self.lexer.get_index();
        let condition_unchecked = self.next_expression()?.expr_type;
        match condition_unchecked {
            ExprType::Comparator { .. } => Ok(condition_unchecked),
            ExprType::Conversion { .. } => Ok(condition_unchecked),
            ExprType::Literal(LiteralType::Bool(_)) => Ok(condition_unchecked),
            _ => Err(ParserError::InvalidConditionExpr {
                start,
                end: self.lexer.get_index(),
            })
        }
    }

    fn identifier_expr(&mut self, start: usize, name: String) -> ParserTypeResult {
        match self.lexer.peek_distance(1)?.token_type {
            TokenType::Comparator(_) => self.comparator_expr(start, false),
            TokenType::As => self.conversion_expr(start),
            _ => {
                self.next_token(false, start)?;
                Ok(ExprType::Identifier(name))
            }
        }
    }

    fn literal_expr(&mut self, start: usize, literal: LiteralType) -> ParserTypeResult {
        match self.lexer.peek_distance(1)?.token_type {
            TokenType::Comparator(_) => self.comparator_expr(start, false),
            _ => {
                self.next_token(false, start)?;
                Ok(ExprType::Literal(literal))
            }
        }
    }

    fn command_expr(&mut self, start: usize, command: CommandType) -> ParserTypeResult {
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

    fn command_single_expr(&mut self, command: CommandType) -> ParserTypeResult {
        self.next_token_expect_end(false)?;

        Ok(ExprType::Command(CommandExpr {
            command,
            args: vec![],
        }))
    }

    fn command_unary_expr(&mut self, command: CommandType) -> ParserTypeResult {
        let arg1 = self.next_expression_expect(|expr| {
            matches!(expr, Expr { expr_type: ExprType::Identifier(_), .. })
        })?;

        self.next_token_expect_end(false)?;

        Ok(ExprType::Command(CommandExpr {
            command,
            args: vec![arg1],
        }))
    }

    fn command_binary_expr(&mut self, command: CommandType) -> ParserTypeResult {
        let arg1 = self.next_expression_expect(|expr| {
            matches!(expr, Expr { expr_type: ExprType::Identifier(_), .. })
        })?;

        let arg2 = self.next_expression_expect(|expr| {
            matches!(expr, Expr { expr_type: ExprType::Identifier(_), .. }) ||
                matches!(expr, Expr { expr_type: ExprType::Literal(_), .. })
        })?;

        self.next_token_expect_end(false)?;

        Ok(ExprType::Command(CommandExpr {
            command,
            args: vec![arg1, arg2],
        }))
    }

    fn command_variadic_expr(&mut self, command: CommandType) -> ParserTypeResult {
        let mut args = Vec::new();

        loop {
            let start = self.lexer.get_index();
            let token = self.next_token(false, 0)?;
            let token_start = *token.index_range.start();
            let token_end = *token.index_range.end();
            match token.token_type {
                TokenType::Newline |
                TokenType::Eof => break,
                TokenType::Identifier(ident) => 
                    args.push(Expr::new(token_start, token_end, ExprType::Identifier(ident))),
                TokenType::Literal(lit) => 
                    args.push(Expr::new(token_start, token_end, ExprType::Literal(lit))),
                token => return Err(ParserError::UnexpectedToken {
                    expr_type: token,
                    start,
                    end: self.lexer.get_index(),
                }),
            }
        }

        Ok(ExprType::Command(CommandExpr {
            command,
            args,
        }))
    }
}
