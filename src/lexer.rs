use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;
use crate::token::{CommandType, ComparatorType, LiteralType, Token, Type};

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Malformed number literal: {num:?}")]
    MalformedNumberLiteral { num: String, start: usize, end: usize },
    #[error("Unknown comparator: {comp:?}")]
    UnknownComparator { comp: String, start: usize, end: usize },
    #[error("Unexpected EOF")]
    UnexpectedEof { start: usize },
}

type LexerResult = Result<Token, LexerError>;

pub struct Lexer<'a> {
    index: usize,
    // peek: bool,
    // read_chars_since_peek: usize,
    peeked_tokens: Vec<Token>,
    chars: Peekable<Chars<'a>>,
    last_token: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            index: 0,
            // peek: false,
            // read_chars_since_peek: 0,
            peeked_tokens: vec![],
            chars: input.chars().peekable(),
            last_token: None,
        }
    }

    pub fn get_index(&self) -> usize {
        self.index
    }

    fn next_char(&mut self) -> Option<char> {
        self.index += 1;
        // if self.peek {
        //     self.read_chars_since_peek += 1;
        //     let mut peeked = None;
        //     for _ in 0..self.read_chars_since_peek {
        //         peeked = self.chars.peek();
        //     }
        //     peeked.map(|&c| c)
        // } else {
            self.chars.next()
        // }
    }

    fn consume_until<F: Fn(char) -> bool>(&mut self, stop: F) -> (String, bool) {
        let mut string = String::new();
        loop {
            let ch = self.chars.peek();
            match ch {
                Some(&ch) => {
                    if stop(ch) { break }
                    self.next_char();
                    string.push(ch);
                }
                None => return (string, true),
            }
        }
        (string, false)
    }

    fn consume_word(&mut self) -> String {
        self.consume_until(|ch| {
            match ch {
                'a'..='z' |
                'A'..='Z' |
                '0'..='9' |
                '_' | '-' | '?' | '$' => false,
                c if c.is_whitespace() => true,
                _ => true,
            }
        }).0
    }

    fn consume_number(&mut self) -> String {
        self.consume_until(|ch| {
            match ch {
                '0'..='9' |
                '_' | '.' | '-' => false,
                _ => true,
            }
        }).0
    }

    fn consume_comparator(&mut self) -> String {
        self.consume_until(|ch| {
            match ch {
                '>' | '<' | '=' => false,
                _ => true,
            }
        }).0
    }

    // pub fn peek(&mut self) -> LexerResult {
    //     self.peek = true;
    //     let last_token = self.last_token.clone();
    //     let token = self.next_token();
    //     if let Ok(token) = &token {
    //         self.last_token = Some(token.clone());
    //     }
    //     self.peek = false;
    //     self.last_token = last_token;
    //     self.index -= self.read_chars_since_peek;
    //     self.read_chars_since_peek = 0;
    //     token
    // }

    pub fn peek(&mut self) -> LexerResult {
        if let Some(token) = self.peeked_tokens.first() {
            return Ok(token.clone());
        }
        let token = self._next_token()?;
        self.peeked_tokens.push(token.clone());
        Ok(token)
    }

    pub fn peek_distance(&mut self, dist: usize) -> LexerResult {
        if let Some(token) = self.peeked_tokens.get(dist) {
            return Ok(token.clone());
        }

        let offset = dist - self.peeked_tokens.len();

        for _ in 0..=offset {
            let token = self._next_token()?;
            self.peeked_tokens.push(token);
        }

        Ok(self.peeked_tokens.last().unwrap().clone())
    }

    pub fn next_token(&mut self) -> LexerResult {
        if self.peeked_tokens.len() > 0 {
            let token = self.peeked_tokens[0].clone();
            self.peeked_tokens.remove(0);
            Ok(token)
        } else {
            self._next_token()
        }
    }

    pub fn _next_token(&mut self) -> LexerResult {
        let token = self.__next_token();
        if let Ok(token) = &token {
            self.last_token = Some(token.clone());
        }
        token
    }

    fn __next_token(&mut self) -> LexerResult {
        loop {
            let c = match self.chars.peek() {
                Some(c) => c,
                None => return Ok(Token::Eof),
            };

            return match c {
                ';' => Ok(self.comment()),
                'a'..='z' |
                'A'..='Z' => {
                    let word = self.consume_word();
                    Ok(self.get_token_from_word(word.as_str()))
                }
                '0'..='9' | '-' | '.' => self.num_literal(),
                '"' => self.str_literal(),
                '=' | '<' | '>' => self.comparator(),
                '\n' => Ok(self.newline()),
                c if c.is_whitespace() => {
                    self.next_char();
                    continue;
                },
                _ => {
                    let identifier = self.consume_word();
                    Ok(Token::Identifier(identifier))
                }
            }
        }
    }

    fn comment(&mut self) -> Token {
        self.consume_until(|ch| ch == '\n');
        Token::Comment
    }

    fn num_literal(&mut self) -> LexerResult {
        let start = self.index;
        let num = self.consume_number();
        let end = self.index;
        if let Ok(float) = num.parse::<f64>() {
            Ok(Token::Literal(LiteralType::Number(float)))
        } else {
            Err(LexerError::MalformedNumberLiteral { num, start, end })
        }
    }

    fn str_literal(&mut self) -> LexerResult {
        let start = self.index;
        self.next_char();
        let (string, eof) = self.consume_until(|ch| ch == '"');
        if eof {
            return Err(LexerError::UnexpectedEof { start });
        }
        self.next_char();
        Ok(Token::Literal(LiteralType::String(string)))
    }

    fn comparator(&mut self) -> LexerResult {
        let start = self.index;
        let comparator = self.consume_comparator();
        let end = self.index;
        match comparator.as_str() {
            ">" => Ok(Token::Comparator(ComparatorType::GreaterThan)),
            "<" => Ok(Token::Comparator(ComparatorType::LessThan)),
            "==" => Ok(Token::Comparator(ComparatorType::Equal)),
            ">=" => Ok(Token::Comparator(ComparatorType::GreaterOrEqualThan)),
            "<=" => Ok(Token::Comparator(ComparatorType::LessOrEqualThan)),
            c => Err(LexerError::UnknownComparator { comp: String::from(c), start, end }),
        }
    }

    fn newline(&mut self) -> Token {
        self.next_char();
        Token::Newline
    }

    fn get_token_from_word(&self, word: &str) -> Token {
        match word {
            // Keywords
            "def" => Token::Def,
            "enddef" => Token::Enddef,

            "if" => Token::If,
            "else" => Token::Else,
            "endif" => Token::Endif,

            "while" => Token::While,
            "endwhile" => Token::Endwhile,

            "not" if self.last_token == Some(Token::If) => Token::Not,
            "as" => Token::As,
            // Commands
            "move" => Token::Command(CommandType::Move),

            "not" => Token::Command(CommandType::Not),
            "and" => Token::Command(CommandType::And),
            "or" => Token::Command(CommandType::Or),
            "xor" => Token::Command(CommandType::Xor),

            "add" => Token::Command(CommandType::Add),
            "sub" => Token::Command(CommandType::Sub),
            "mul" => Token::Command(CommandType::Mul),
            "div" => Token::Command(CommandType::Div),

            "call" => Token::Command(CommandType::Call),
            "return" => Token::Command(CommandType::Return),
            "break" => Token::Command(CommandType::Break),

            "get" => Token::Command(CommandType::Get),
            "put" => Token::Command(CommandType::Put),
            // Types
            "number" => Token::Type(Type::Number),
            "string" => Token::Type(Type::String),
            "bool" => Token::Type(Type::Bool),
            // Literals
            "true" => Token::Literal(LiteralType::Bool(true)),
            "false" => Token::Literal(LiteralType::Bool(false)),
            // Identifier
            w => Token::Identifier(w.into())
        }
    }
}
