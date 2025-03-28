use std::collections::HashMap;
use std::num::ParseFloatError;
use text_io::read;
use thiserror::Error;
use crate::expression::{CommandExpr, ComparatorExpr, ConversionExpr, DefExpr, Expr, ExprType, IfExpr, WhileExpr};
use crate::parser::{Parser, ParserError};
use crate::token::{CommandType, ComparatorType, LiteralType, Type};

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("The parser returned an error: {0}")]
    ParserError(#[from] ParserError),
    #[error("Cannot execute expressions in a consumed Interpreter")]
    InterpreterConsumed,
    #[error("Unexpected EOF")]
    UnexpectedEof,
    #[error("Expected type {expected:?}, got {got:?}")]
    ExpectedType { expected: Vec<InterpreterType>, got: InterpreterType, start: usize, end: usize },
    #[error("Unexpected Unit when a value was expected")]
    UnexpectedUnit,
    #[error("No variable named {name} found in global scope")]
    NoSuchVar { name: String },
    #[error("No definition named {name} found in global scope")]
    NoSuchDef { name: String },
    #[error("Expected identifier at {start}-{end} ")]
    ExpectedIdentifier { start: usize, end: usize },
    #[error("Invalid format for float: {0}")]
    ConversionError(#[from] ParseFloatError),
    #[error("Incorrect number of arguments for command {command:?}, expected: {expected}, got: {got}")]
    WrongNumOfArgs { command: CommandType, expected: usize, got: usize },
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum InterpreterStatus {
    Consumable,
    SkipUntilEndOfExpressions { initiator: CommandType },
    Consumed,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum InterpreterType {
    Number,
    String,
    Bool,
    Unit,
    Ident,
}

impl From<InterpreterValue> for InterpreterType {
    fn from(value: InterpreterValue) -> Self {
        match value {
            InterpreterValue::Number(_) => InterpreterType::Number,
            InterpreterValue::String(_) => InterpreterType::String,
            InterpreterValue::Bool(_) => InterpreterType::Bool,
            InterpreterValue::Unit => InterpreterType::Unit,
            InterpreterValue::Ident { .. } => InterpreterType::Ident,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterValue {
    Number(f64),
    String(String),
    Bool(bool),
    Unit,
    Ident { name: String, value: Option<Box<InterpreterValue>> }
}

pub struct Interpreter<'a> {
    parser: Parser<'a>,
    variables: HashMap<String, InterpreterValue>,
    definitions: Vec<DefExpr>,
    status: InterpreterStatus,
}

type InterpreterResult = Result<InterpreterValue, InterpreterError>;

impl<'a> Interpreter<'a> {
    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            parser,
            variables: HashMap::new(),
            definitions: vec![],
            status: InterpreterStatus::Consumable,
        }
    }

    pub fn get_index(&self) -> usize {
        self.parser.get_index()
    }

    pub fn execute_next(&mut self) -> InterpreterResult {
        if self.status == InterpreterStatus::Consumed {
            return Err(InterpreterError::InterpreterConsumed);
        }

        match self.parser.next_expression()? {
            expr if expr.expr_type == ExprType::Eof => {
                self.status = InterpreterStatus::Consumed;
                Ok(InterpreterValue::Unit)
            }
            expr => self.execute_expression(&expr),
        }
    }

    pub fn execute_expressions(&mut self, exprs: &Vec<Expr>) -> InterpreterResult {
        for expr in exprs {
            match self.status {
                InterpreterStatus::SkipUntilEndOfExpressions { .. } => break,
                _ => {},
            }
            self.execute_expression(expr)?;
        }
        Ok(InterpreterValue::Unit)
    }

    pub fn execute_expression(&mut self, expr: &Expr) -> InterpreterResult {
        let start = *expr.index_range.start();
        let end = *expr.index_range.end();
        match &expr.expr_type {
            ExprType::Def(def_expr) => self.execute_def(def_expr),
            ExprType::If(if_expr) => self.execute_if(if_expr),
            ExprType::While(while_expr) => self.execute_while(while_expr),
            ExprType::Comparator(comp_expr) => self.execute_comparator(comp_expr, start, end),
            ExprType::Conversion(conv_expr) => self.execute_conversion(conv_expr),
            ExprType::Command(comm_expr) => self.execute_command(comm_expr, start, end),
            ExprType::Identifier(ident) => self.execute_identifier(ident),
            ExprType::Literal(lit) => self.execute_literal(lit),
            ExprType::Eof => Err(InterpreterError::UnexpectedEof),
        }
    }

    fn get_identifier(&self, expr: &Expr) -> Result<String, InterpreterError> {
        match &expr.expr_type {
            ExprType::Identifier(ident) => Ok(ident.clone()),
            _ => Err(InterpreterError::ExpectedIdentifier { 
                start: *expr.index_range.start(), 
                end: *expr.index_range.end() 
            }),
        }
    }

    fn get_variable(&self, identifier: String) -> InterpreterResult {
        if let Some(&ref value) = self.variables.get(&identifier) {
            Ok(value.clone())
        } else {
            Err(InterpreterError::NoSuchVar {
                name: String::from(identifier),
            })
        }
    }

    fn set_variable(&mut self, identifier: String, value: InterpreterValue) {
        self.variables.insert(identifier, value);
    }

    fn get_def(&self, identifier: &str) -> Result<&DefExpr, InterpreterError> {
        if let Some(value) = self.definitions.iter().find(|e| e.name == identifier) {
            Ok(value)
        } else {
            Err(InterpreterError::NoSuchDef {
                name: String::from(identifier),
            })
        }
    }

    fn get_ident(
        &self,
        value: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<(String, Option<Box<InterpreterValue>>), InterpreterError> {
        if let InterpreterValue::Ident { name, value} = value {
            Ok((name, value))
        } else {
            Err(InterpreterError::ExpectedType {
                expected: vec![InterpreterType::Ident],
                got: value.into(),
                start,
                end,
            })
        }
    }

    fn get_bool(
        &self,
        value: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<bool, InterpreterError> {
        match value {
            InterpreterValue::Bool(b) => Ok(b),
            v => Err(InterpreterError::ExpectedType {
                expected: vec![InterpreterType::Bool],
                got: v.into(),
                start,
                end,
            })
        }
    }

    fn get_number(
        &self,
        value: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<f64, InterpreterError> {
        match value {
            InterpreterValue::Number(n) => Ok(n),
            v => Err(InterpreterError::ExpectedType {
                expected: vec![InterpreterType::Number],
                got: v.into(),
                start,
                end,
            })
        }
    }

    fn get_primitive_from_value(
        &self,
        value: InterpreterValue
    ) -> Result<InterpreterValue, InterpreterError> {
        if let InterpreterValue::Ident { name, value } = &value {
            if let Some(value) = value {
                Ok(*value.clone())
            } else {
                Err(InterpreterError::NoSuchVar {
                    name: name.clone(),
                })
            }
        } else {
            Ok(value)
        }
    }

    fn execute_identifier(&self, ident: &String) -> InterpreterResult {
        Ok(InterpreterValue::Ident {
            name: ident.clone(),
            value: self.get_variable(ident.clone()).ok().map(Box::new),
        })
    }

    fn execute_literal(&self, lit: &LiteralType) -> InterpreterResult {
        Ok(match lit {
            LiteralType::Number(n) => InterpreterValue::Number(*n),
            LiteralType::String(s) => InterpreterValue::String(s.clone()),
            LiteralType::Bool(b) => InterpreterValue::Bool(*b),
        })
    }

    fn execute_def(&mut self, def_expr: &DefExpr) -> InterpreterResult {
        self.definitions.push(def_expr.clone());
        Ok(InterpreterValue::Unit)
    }

    fn execute_if(&mut self, if_expr: &IfExpr) -> InterpreterResult {
        let condition = match self.execute_expression(if_expr.condition.as_ref())? {
            InterpreterValue::Bool(cond) => cond,
            int_type => return Err(InterpreterError::ExpectedType {
                expected: vec![InterpreterType::Bool],
                got: int_type.into(),
                start: *if_expr.condition.as_ref().index_range.start(),
                end: *if_expr.condition.as_ref().index_range.end(),
            })
        };

        if condition {
            self.execute_expressions(&if_expr.body)
        } else if let Some(alternate) = &if_expr.alternate {
            self.execute_expressions(alternate)
        } else {
            Ok(InterpreterValue::Unit)
        }
    }

    fn execute_while(&mut self, while_expr: &WhileExpr) -> InterpreterResult {
        loop {
            match self.execute_expression(while_expr.condition.as_ref())? {
                InterpreterValue::Bool(true) => {},
                InterpreterValue::Bool(false) => break,
                int_type => return Err(InterpreterError::ExpectedType {
                    expected: vec![InterpreterType::Bool],
                    got: int_type.into(),
                    start: *while_expr.condition.index_range.start(),
                    end: *while_expr.condition.index_range.end(),
                })
            }

            self.execute_expressions(&while_expr.body)?;

            match self.status {
                InterpreterStatus::SkipUntilEndOfExpressions { initiator: CommandType::Return } => break,
                InterpreterStatus::SkipUntilEndOfExpressions { initiator: CommandType::Break } => {
                    self.status = InterpreterStatus::Consumable;
                    break;
                },
                InterpreterStatus::SkipUntilEndOfExpressions { initiator: CommandType::Continue } => {
                    // Do nothing, this is the same thing as continue
                    self.status = InterpreterStatus::Consumable;
                }
                _ => {},
            }
        }

        Ok(InterpreterValue::Unit)
    }

    fn execute_comparator(
        &mut self, 
        comp_expr: &ComparatorExpr, 
        start: usize, 
        end: usize
    ) -> InterpreterResult {
        let left_val = self.execute_expression(comp_expr.left.as_ref())?;
        let left = self.get_primitive_from_value(left_val)?;
        let right_val = self.execute_expression(comp_expr.right.as_ref())?;
        let right = self.get_primitive_from_value(right_val)?;

        match comp_expr.comparator {
            ComparatorType::GreaterThan => self.compare_numbers(comp_expr.inverse, left, right, PartialOrd::gt, start, end),
            ComparatorType::LessThan => self.compare_numbers(comp_expr.inverse, left, right, PartialOrd::lt, start, end),
            ComparatorType::Equal => self.is_equal(comp_expr.inverse, left, right, start, end),
            ComparatorType::GreaterOrEqualThan => self.compare_numbers(comp_expr.inverse, left, right, PartialOrd::ge, start, end),
            ComparatorType::LessOrEqualThan => self.compare_numbers(comp_expr.inverse, left, right, PartialOrd::le, start, end),
        }
    }

    fn compare_numbers(
        &self,
        inverse: bool,
        left: InterpreterValue,
        right: InterpreterValue,
        comp: impl Fn(&f64, &f64) -> bool,
        start: usize,
        end: usize,
    ) -> InterpreterResult {
        match (left, right) {
            (InterpreterValue::Number(left), InterpreterValue::Number(right)) => {
                Ok(InterpreterValue::Bool(comp(&left, &right) ^ inverse))
            },
            (typ, _) => Err(InterpreterError::ExpectedType {
                expected: vec![InterpreterType::Number],
                got: typ.into(),
                start,
                end,
            })
        }
    }

    fn is_equal(
        &self,
        inverse: bool,
        left: InterpreterValue,
        right: InterpreterValue,
        start: usize,
        end: usize,
    ) -> InterpreterResult {
        match (left, right) {
            (InterpreterValue::Number(left), InterpreterValue::Number(right)) => {
                Ok(InterpreterValue::Bool((left == right) ^ inverse))
            },
            (InterpreterValue::String(left), InterpreterValue::String(right)) => {
                Ok(InterpreterValue::Bool((left == right) ^ inverse))
            }
            (typ, _) => Err(InterpreterError::ExpectedType {
                expected: vec![InterpreterType::Number],
                got: typ.into(),
                start,
                end,
            })
        }
    }

    fn execute_conversion(&mut self, conv_expr: &ConversionExpr) -> InterpreterResult {
        let identifier = self.get_identifier(conv_expr.ident.as_ref())?;
        let value = self.get_variable(identifier.clone())?;
        match (value, conv_expr.to_type) {
            (InterpreterValue::String(_), Type::String) => {}
            (InterpreterValue::Number(_), Type::Number) => {}
            (InterpreterValue::Bool(_), Type::Bool) => {}
            (InterpreterValue::Ident {..}, _) => {}

            (InterpreterValue::String(string), Type::Bool) => {
                self.set_variable(
                    identifier,
                    InterpreterValue::Bool(string.is_empty())
                );
            }
            (InterpreterValue::String(string), Type::Number) => {
                match string.parse::<f64>() {
                    Ok(num) => self.set_variable(
                        identifier,
                        InterpreterValue::Number(num)
                    ),
                    Err(_) => return Ok(InterpreterValue::Bool(conv_expr.inverse)),
                }
            }

            (InterpreterValue::Number(num), Type::String) => {
                self.set_variable(
                    identifier,
                    InterpreterValue::String(num.to_string())
                );
            }
            (InterpreterValue::Number(num), Type::Bool) => {
                self.set_variable(
                    identifier,
                    InterpreterValue::Bool(num == 0.0)
                );
            }

            (InterpreterValue::Bool(b), Type::String) => {
                self.set_variable(
                    identifier,
                    InterpreterValue::String(b.to_string())
                );
            }
            (InterpreterValue::Bool(b), Type::Number) => {
                self.set_variable(
                    identifier,
                    InterpreterValue::Number(b.into())
                );
            }

            (InterpreterValue::Unit, _) => {
                return Err(InterpreterError::UnexpectedUnit);
            }
        }

        Ok(InterpreterValue::Bool(!conv_expr.inverse))
    }

    fn execute_command(&mut self, comm_expr: &CommandExpr, start: usize, end: usize) -> InterpreterResult {
        let command = &comm_expr.command;
        let args = &comm_expr.args;
        match command {
            CommandType::Move => {
                let (dest, source) = self.command_two_args(*command, args)?;
                let (dest_ident, _) = self.get_ident(dest, start, end)?;
                let source_value = self.get_primitive_from_value(source)?;
                self.set_variable(dest_ident, source_value);
            }
            CommandType::Not => {
                let dest = self.command_single_arg(*command, args)?;
                let (dest_ident, value) = self.get_ident(dest, start, end)?;
                if let Some(value) = value {
                    self.command_not(dest_ident, *value, start, end)?;
                } else {
                    return Err(InterpreterError::NoSuchVar {
                        name: dest_ident,
                    })
                }
            }
            CommandType::And => {
                let (dest, source) = self.command_two_args(*command, args)?;
                let (dest_ident, dest_value) = self.get_ident(dest, start, end)?;
                let source_value = self.get_primitive_from_value(source)?;

                if let Some(dest_value) = dest_value {
                    self.command_and(dest_ident, *dest_value, source_value, start, end)?;
                } else {
                    return Err(InterpreterError::NoSuchVar {
                        name: dest_ident,
                    })
                }
            }
            CommandType::Or => {
                let (dest, source) = self.command_two_args(*command, args)?;
                let (dest_ident, dest_value) = self.get_ident(dest, start, end)?;
                let source_value = self.get_primitive_from_value(source)?;

                if let Some(dest_value) = dest_value {
                    self.command_or(dest_ident, *dest_value, source_value, start, end)?;
                } else {
                    return Err(InterpreterError::NoSuchVar {
                        name: dest_ident,
                    })
                }
            }
            CommandType::Xor => {
                let (dest, source) = self.command_two_args(*command, args)?;
                let (dest_ident, dest_value) = self.get_ident(dest, start, end)?;
                let source_value = self.get_primitive_from_value(source)?;

                if let Some(dest_value) = dest_value {
                    self.command_xor(dest_ident, *dest_value, source_value, start, end)?;
                } else {
                    return Err(InterpreterError::NoSuchVar {
                        name: dest_ident,
                    })
                }
            }
            CommandType::Add => {
                let (dest, source) = self.command_two_args(*command, args)?;
                let (dest_ident, dest_value) = self.get_ident(dest, start, end)?;
                let source_value = self.get_primitive_from_value(source)?;

                if let Some(dest_value) = dest_value {
                    self.command_add(dest_ident, *dest_value, source_value, start, end)?;
                } else {
                    return Err(InterpreterError::NoSuchVar {
                        name: dest_ident,
                    })
                }
            }
            CommandType::Sub => {
                let (dest, source) = self.command_two_args(*command, args)?;
                let (dest_ident, dest_value) = self.get_ident(dest, start, end)?;
                let source_value = self.get_primitive_from_value(source)?;

                if let Some(dest_value) = dest_value {
                    self.command_sub(dest_ident, *dest_value, source_value, start, end)?;
                } else {
                    return Err(InterpreterError::NoSuchVar {
                        name: dest_ident,
                    })
                }
            }
            CommandType::Mul => {
                let (dest, source) = self.command_two_args(*command, args)?;
                let (dest_ident, dest_value) = self.get_ident(dest, start, end)?;
                let source_value = self.get_primitive_from_value(source)?;

                if let Some(dest_value) = dest_value {
                    self.command_mul(dest_ident, *dest_value, source_value, start, end)?;
                } else {
                    return Err(InterpreterError::NoSuchVar {
                        name: dest_ident,
                    })
                }
            }
            CommandType::Div => {
                let (dest, source) = self.command_two_args(*command, args)?;
                let (dest_ident, dest_value) = self.get_ident(dest, start, end)?;
                let source_value = self.get_primitive_from_value(source)?;

                if let Some(dest_value) = dest_value {
                    self.command_div(dest_ident, *dest_value, source_value, start, end)?;
                } else {
                    return Err(InterpreterError::NoSuchVar {
                        name: dest_ident,
                    })
                }
            }
            CommandType::Call => {
                let def = self.command_single_arg(*command, args)?;
                let (def_ident, _) = self.get_ident(def, start, end)?;
                let def_expr = self.get_def(def_ident.as_str())?.clone();
                self.execute_expressions(&def_expr.body)?;
                self.status = InterpreterStatus::Consumable;
            }
            CommandType::Return => {
                self.command_no_args(*command, args)?;
                self.status = InterpreterStatus::SkipUntilEndOfExpressions { 
                    initiator: CommandType::Return 
                };
            }
            CommandType::Break => {
                self.command_no_args(*command, args)?;
                self.status = InterpreterStatus::SkipUntilEndOfExpressions { 
                    initiator: CommandType::Break 
                };
            }
            CommandType::Continue => {
                self.command_no_args(*command, args)?;
                self.status = InterpreterStatus::SkipUntilEndOfExpressions { 
                    initiator: CommandType::Continue
                }
            }
            CommandType::Get => {
                let dest = self.command_single_arg(*command, args)?;
                let (dest_ident, _) = self.get_ident(dest, start, end)?;

                let input: String = read!("{}\n");
                self.set_variable(dest_ident, InterpreterValue::String(input));
            }
            CommandType::Put => {
                let sources = self.command_multiple_args(args)?;

                for source in sources {
                    let value = self.get_primitive_from_value(source)?;
                    let print_string = match value {
                        InterpreterValue::Number(n) => n.to_string(),
                        InterpreterValue::String(s) => s,
                        InterpreterValue::Bool(b) => b.to_string(),
                        InterpreterValue::Unit => String::from("()"),
                        InterpreterValue::Ident { .. } =>
                            panic!("Cannot return an InterpreterValue::Ident from Interpreter::get_primitive_from_value")
                    };
                    print!("{print_string}");
                }

                println!();
            }
        }

        Ok(InterpreterValue::Unit)
    }

    fn command_no_args(
        &self,
        command: CommandType,
        args: &Vec<Expr>
    ) -> Result<(), InterpreterError> {
        if args.is_empty() {
            Ok(())
        } else {
            Err(InterpreterError::WrongNumOfArgs {
                command,
                expected: 0,
                got: args.len(),
            })
        }
    }

    fn command_single_arg(
        &mut self,
        command: CommandType,
        args: &Vec<Expr>
    ) -> Result<InterpreterValue, InterpreterError> {
        if args.len() == 1 {
            Ok(self.execute_expression(&args[0])?)
        } else {
            Err(InterpreterError::WrongNumOfArgs {
                command,
                expected: 1,
                got: args.len(),
            })
        }
    }

    fn command_two_args(
        &mut self,
        command: CommandType,
        args: &Vec<Expr>
    ) -> Result<(InterpreterValue, InterpreterValue), InterpreterError> {
        if args.len() == 2 {
            let arg1 = self.execute_expression(&args[0])?;
            let arg2 = self.execute_expression(&args[1])?;

            Ok((arg1, arg2))
        } else {
            Err(InterpreterError::WrongNumOfArgs {
                command,
                expected: 2,
                got: args.len(),
            })
        }
    }

    fn command_multiple_args(
        &mut self,
        args: &Vec<Expr>
    ) -> Result<Vec<InterpreterValue>, InterpreterError> {
        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.execute_expression(arg)?)
        }
        Ok(arg_values)
    }

    fn command_not(
        &mut self, 
        ident: String, 
        value: InterpreterValue, 
        start: usize, 
        end: usize
    ) -> Result<(), InterpreterError> {
        let b = self.get_bool(value, start, end)?;
        self.set_variable(ident, InterpreterValue::Bool(!b));

        Ok(())
    }

    fn command_and(
        &mut self, 
        ident: String, 
        value_a: InterpreterValue, 
        value_b: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<(), InterpreterError> {
        let a = self.get_bool(value_a, start, end)?;
        let b = self.get_bool(value_b, start, end)?;

        self.set_variable(ident, InterpreterValue::Bool(a && b));

        Ok(())
    }

    fn command_or(
        &mut self,
        ident: String,
        value_a: InterpreterValue,
        value_b: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<(), InterpreterError> {
        let a = self.get_bool(value_a, start, end)?;
        let b = self.get_bool(value_b, start, end)?;

        self.set_variable(ident, InterpreterValue::Bool(a || b));

        Ok(())
    }

    fn command_xor(
        &mut self, 
        ident: String, 
        value_a: InterpreterValue,
        value_b: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<(), InterpreterError> {
        let a = self.get_bool(value_a, start, end)?;
        let b = self.get_bool(value_b, start, end)?;

        self.set_variable(ident, InterpreterValue::Bool(a ^ b));

        Ok(())
    }

    fn command_add(
        &mut self,
        ident: String, 
        value_a: InterpreterValue,
        value_b: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<(), InterpreterError> {
        let a = self.get_number(value_a, start, end)?;
        let b = self.get_number(value_b, start, end)?;

        self.set_variable(ident, InterpreterValue::Number(a + b));

        Ok(())
    }

    fn command_sub(
        &mut self, 
        ident: String,
        value_a: InterpreterValue, 
        value_b: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<(), InterpreterError> {
        let a = self.get_number(value_a, start, end)?;
        let b = self.get_number(value_b, start, end)?;

        self.set_variable(ident, InterpreterValue::Number(a - b));

        Ok(())
    }

    fn command_mul(
        &mut self, 
        ident: String, 
        value_a: InterpreterValue,
        value_b: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<(), InterpreterError> {
        let a = self.get_number(value_a, start, end)?;
        let b = self.get_number(value_b, start, end)?;

        self.set_variable(ident, InterpreterValue::Number(a * b));

        Ok(())
    }

    fn command_div(
        &mut self, 
        ident: String,
        value_a: InterpreterValue, 
        value_b: InterpreterValue,
        start: usize,
        end: usize,
    ) -> Result<(), InterpreterError> {
        let a = self.get_number(value_a, start, end)?;
        let b = self.get_number(value_b, start, end)?;

        self.set_variable(ident, InterpreterValue::Number(a / b));

        Ok(())
    }
}
