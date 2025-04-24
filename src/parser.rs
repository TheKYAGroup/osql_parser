use std::{backtrace::Backtrace, collections::HashMap, iter::Map, string::ParseError};

use derive_more::Display;
use thiserror::Error;

use crate::{
    ast::{
        Columns, Expression, IdentExpression, InfixOperator, Join, JoinType, Named, Program,
        SelectExpression, Statement,
    },
    lexer::Lexer,
    token::Token,
};

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Range,
    Sum,
    Product,
    Prefix,
    Access,
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        match value {
            Token::Eq => Precedence::Equals,
            Token::Period => Precedence::Access,
            Token::In => Precedence::Range,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("No prefix parse function for token: {0}")]
    NoPrefixParseFn(Token),

    #[error("Partial select expression")]
    PartialSelectExpr,

    #[error("Unexpected end of file")]
    UnexpectedEOF,

    #[error("Missing FROM clause in SELECT statement")]
    MissingFromInSelect,

    #[error("Missing join token")]
    MissingJoinToken,

    #[error("Unsupported infix operator: {0}")]
    UnsupportedInfix(Token),

    #[error("Peek failed for token: {0}")]
    PeekFailed(Token),

    #[error("Expected current token to be: {0}")]
    ExpectedCurrent(Token),
}

#[derive(Debug, Error)]
#[error("{}", .inner)]
pub struct ParserErrorWithBacktrace {
    #[from]
    #[source]
    inner: ParserError,
    #[backtrace]
    backtrace: Backtrace,
}

#[derive(derive_more::Debug)]
pub struct Parser {
    #[debug(skip)]
    lex: Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,

    #[debug(skip)]
    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    #[debug(skip)]
    infix_parse_fns: HashMap<Token, InfixParseFn>,
}

type Result<T> = core::result::Result<T, ParserErrorWithBacktrace>;

type PrefixParseFn = fn(&mut Parser) -> Result<Expression>;
type InfixParseFn = fn(&mut Parser, left: Expression) -> Result<Expression>;

impl Parser {
    pub fn new(lex: Lexer) -> Self {
        let mut out = Parser {
            lex,
            cur_token: None,
            peek_token: None,

            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        out.register_prefix(Token::Select, Self::parse_select);
        out.register_prefix(Token::ident(""), Self::parse_ident);
        out.register_prefix(Token::string(""), Self::parse_ident);
        out.register_prefix(Token::LParen, Self::parse_grouped);

        out.register_infix(Token::Period, Self::parse_infix);
        out.register_infix(Token::Eq, Self::parse_infix);
        out.register_infix(Token::In, Self::parse_infix);

        out.next_token();
        out.next_token();

        out
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lex.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program { statements: vec![] };

        while self.cur_token.is_some() {
            match self.cur_token {
                Some(Token::Comment(_)) => {
                    self.next_token();
                }
                Some(_) => {
                    let stmt = Statement::Expression(self.parse_expression(Precedence::Lowest)?);
                    program.statements.push(stmt);
                    self.next_token();
                }
                None => panic!("Got none for cur_token after it was detected as some"),
            }
        }

        return Ok(program);
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        assert!(
            self.cur_token.is_some(),
            "Called parse_expression with cur_token being None"
        );

        let Some(prefix) = self.prefix_parse_fns.get(self.cur_token.as_ref().unwrap()) else {
            return Err(ParserError::NoPrefixParseFn(
                self.cur_token.clone().unwrap(),
            ))?;
        };

        let mut left_exp = prefix(self)?;

        while !self.peek_token_is(Token::Semicolon)
            && precedence < self.peek_precedence()
            && self.peek_token.is_some()
        {
            let infix = {
                let Some(cur_token) = &self.peek_token else {
                    return Ok(left_exp);
                };
                let Some(infix) = self.infix_parse_fns.get(cur_token) else {
                    return Ok(left_exp);
                };

                infix.clone()
            };

            self.next_token();

            left_exp = infix(self, left_exp)?;
        }

        return Ok(left_exp);
    }

    fn peek_precedence(&self) -> Precedence {
        match &self.peek_token {
            None => Precedence::Lowest,
            Some(tok) => tok.into(),
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match &self.cur_token {
            None => Precedence::Lowest,
            Some(tok) => tok.into(),
        }
    }

    fn parse_select(&mut self) -> Result<Expression> {
        assert_eq!(
            self.cur_token,
            Some(Token::Select),
            "Called parse_select when the cur_token is not select",
        );

        self.next_token();

        let columns = match self.cur_token {
            None => return Err(ParserError::PartialSelectExpr)?,
            Some(Token::Asterisk) => Columns::All,
            _ => {
                let mut columns = vec![self.parse_column()?];

                self.next_token();

                while self.cur_token == Some(Token::Comma) {
                    self.next_token();
                    columns.push(self.parse_column()?);
                    if self.peek_token_is(Token::Comma) {
                        self.next_token();
                    }
                }

                Columns::Individual(columns)
            }
        };

        self.expect_peek(Token::From)?;
        self.next_token();

        let from = self.parse_named()?;

        let join = if self.peek_token_in(&[Token::Inner]) {
            self.next_token();
            Some(self.parse_join()?)
        } else {
            None
        };

        let where_expr = if self.peek_token_is(Token::Where) {
            self.next_token();
            self.next_token();

            let expr = self.parse_expression(Precedence::Lowest)?;
            Some(Box::new(expr))
        } else {
            None
        };

        Ok(Expression::Select(SelectExpression {
            columns,
            from: Box::new(from),
            where_expr,
            join,
        }))
    }

    fn parse_join(&mut self) -> Result<Join> {
        let join_type = match self.cur_token {
            Some(Token::Inner) => JoinType::Inner,
            _ => panic!("Unsupported join type: {:?}", self.cur_token),
        };

        self.expect_peek(Token::Join)?;

        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        let on = if self.peek_token_is(Token::On) {
            self.next_token();
            self.next_token();
            let out = Some(Box::new(self.parse_expression(Precedence::Lowest)?));
            out
        } else {
            None
        };

        Ok(Join {
            join_type,
            expr: Box::new(expr),
            on,
        })
    }

    fn parse_named(&mut self) -> Result<Named> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        let name = if self.peek_token_in(&[Token::ident(""), Token::string("")]) {
            self.next_token();
            Some(self.parse_ident_unwrap()?)
        } else {
            None
        };

        Ok(Named { expr, name })
    }

    fn parse_ident_unwrap(&mut self) -> Result<IdentExpression> {
        let Expression::Ident(ident) = self.parse_ident()? else {
            panic!("parse_ident didn't return ident");
        };
        Ok(ident)
    }

    fn parse_column(&mut self) -> Result<Named> {
        self.parse_named()
    }

    fn register_prefix(&mut self, token: Token, fun: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, fun);
    }

    fn register_infix(&mut self, token: Token, fun: InfixParseFn) {
        self.infix_parse_fns.insert(token, fun);
    }

    fn parse_ident(&mut self) -> Result<Expression> {
        let ident = match &self.cur_token {
            Some(Token::String(str)) => format!("\"{}\"", str),
            Some(Token::Ident(str)) => str.clone(),
            None => Err(ParserError::UnexpectedEOF)?,
            _ => panic!(
                "Called parse_ident with unsupported token type: {:?}",
                self.cur_token
            ),
        };

        Ok(Expression::Ident(IdentExpression { ident }))
    }

    fn cur_token_is(&self, token: Token) -> bool {
        self.cur_token == Some(token)
    }

    fn cur_token_in(&self, tokens: &[Token]) -> bool {
        let Some(cur_tok) = &self.cur_token else {
            return false;
        };

        tokens.contains(cur_tok)
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == Some(token)
    }

    fn peek_token_in(&self, tokens: &[Token]) -> bool {
        let Some(peek_tok) = &self.peek_token else {
            return false;
        };

        tokens.contains(peek_tok)
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression> {
        let op = match self.cur_token {
            None => panic!("Called parse_infix with cur_token = None"),
            Some(Token::Period) => InfixOperator::Period,
            Some(Token::Eq) => InfixOperator::Eq,
            Some(Token::In) => InfixOperator::In,
            _ => Err(ParserError::UnsupportedInfix(
                self.cur_token.clone().unwrap(),
            ))?,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(crate::ast::InfixExpression {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }))
    }

    fn parse_grouped(&mut self) -> Result<Expression> {
        assert_eq!(self.cur_token, Some(Token::LParen));

        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen)?;

        let name = if self.peek_token_in(&[Token::ident(""), Token::string("")]) {
            self.next_token();

            Some(self.parse_ident_unwrap()?)
        } else {
            None
        };

        Ok(Expression::Grouped(crate::ast::GroupedExpression {
            inner: Box::new(exp),
            name,
        }))
    }

    fn expect_peek(&mut self, token: Token) -> Result<()> {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            Ok(())
        } else {
            eprintln!("Expect peek: {:?}", self);
            Err(ParserError::PeekFailed(token))?
        }
    }

    fn expect_cur(&mut self, token: Token) -> Result<()> {
        if self.cur_token_is(token.clone()) {
            self.next_token();
            Ok(())
        } else {
            eprintln!("Expect cur: {:?}", self);
            Err(ParserError::ExpectedCurrent(token))?
        }
    }
}

#[cfg(test)]
mod tests {
    use ntest::timeout;

    use crate::ast::{GroupedExpression, IdentExpression, InfixExpression, SelectExpression};

    use super::*;

    fn unwrap_backtrace(program: Result<Program>) -> Program {
        match program {
            Ok(program) => program,
            Err(err) => panic!("Err({}):\n {}", err.inner, err.backtrace),
        }
    }

    #[test]
    #[timeout(100)]
    fn simple_select() {
        let input = "SELECT * FROM Hello";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = unwrap_backtrace(parser.parse_program());

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(Expression::Select(select)) = &program.statements[0] else {
            panic!()
        };

        assert_eq!(
            &SelectExpression {
                columns: crate::ast::Columns::All,
                from: Box::new(Named {
                    expr: Expression::Ident(IdentExpression {
                        ident: "Hello".to_string()
                    }),
                    name: None,
                }),
                where_expr: None,
                join: None
            },
            select
        );
    }

    #[test]
    #[timeout(100)]
    fn grouped() {
        let input = "(SELECT * FROM Hello) M";

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = unwrap_backtrace(parser.parse_program());

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(Expression::Grouped(select)) = &program.statements[0] else {
            panic!()
        };

        assert_eq!(
            &GroupedExpression {
                inner: Box::new(Expression::Select(SelectExpression {
                    columns: crate::ast::Columns::All,
                    from: Box::new(Named {
                        expr: Expression::Ident(IdentExpression {
                            ident: "Hello".to_string()
                        }),
                        name: None,
                    }),
                    where_expr: None,
                    join: None
                },)),
                name: Some(IdentExpression {
                    ident: "M".to_string()
                })
            },
            select
        );
    }

    #[test]
    #[timeout(1000)]
    fn inner_join() {
        let input = r#"SELECT * FROM Hello h INNER JOIN SELECT * FROM Other o
            ON h.first = o.first
            "#;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = unwrap_backtrace(parser.parse_program());

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(Expression::Select(select)) = &program.statements[0] else {
            panic!()
        };

        let expected = SelectExpression {
            columns: crate::ast::Columns::All,
            from: Box::new(Named {
                expr: Expression::Ident(IdentExpression {
                    ident: "Hello".to_string(),
                }),
                name: Some(IdentExpression {
                    ident: "h".to_string(),
                }),
            }),
            where_expr: None,
            join: Some(crate::ast::Join {
                join_type: JoinType::Inner,
                expr: Box::new(Expression::Select(SelectExpression {
                    columns: crate::ast::Columns::All,
                    from: Box::new(Named {
                        expr: Expression::Ident(IdentExpression {
                            ident: "Other".to_string(),
                        }),
                        name: Some(IdentExpression {
                            ident: "o".to_string(),
                        }),
                    }),
                    where_expr: None,
                    join: None,
                })),
                on: Some(Box::new(Expression::Infix(crate::ast::InfixExpression {
                    left: Box::new(Expression::Infix(crate::ast::InfixExpression {
                        left: Box::new(Expression::ident("h")),
                        op: InfixOperator::Period,
                        right: Box::new(Expression::ident("first")),
                    })),
                    op: InfixOperator::Eq,
                    right: Box::new(Expression::Infix(crate::ast::InfixExpression {
                        left: Box::new(Expression::ident("o")),
                        op: InfixOperator::Period,
                        right: Box::new(Expression::ident("first")),
                    })),
                }))),
            }),
        };

        println!("{}", expected);
        println!("{}", select);

        test_select(&expected, select);
    }

    fn test_expression(expected: &Expression, got: &Expression) {
        match (expected, got) {
            (Expression::Select(expected), Expression::Select(got)) => test_select(expected, got),
            (Expression::Infix(expected), Expression::Infix(got)) => test_infix(expected, got),
            (Expression::Ident(expected), Expression::Ident(got)) => assert_eq!(expected, got),
            _ => panic!("{:?}, {:?}", expected, got),
        }
    }

    fn test_select(expected: &SelectExpression, got: &SelectExpression) {
        assert_eq!(expected.columns, got.columns);
        assert_eq!(expected.from, got.from);
        assert_eq!(expected.where_expr, got.where_expr);
        test_join(&expected.join, &got.join);
    }

    fn test_infix(expected: &InfixExpression, got: &InfixExpression) {
        assert_eq!(expected.op, got.op);
        test_expression(&expected.left, &got.left);
        test_expression(&expected.right, &got.right);
    }

    fn test_join(expected: &Option<Join>, got: &Option<Join>) {
        let (Some(expected), Some(got)) = (expected, got) else {
            assert_eq!(expected, got);
            return;
        };

        assert_eq!(expected.join_type, got.join_type);

        test_expression(&expected.expr, &got.expr);

        test_optional_expr(&expected.on, &got.on);
    }

    fn test_optional_expr(expected: &Option<Box<Expression>>, got: &Option<Box<Expression>>) {
        match (expected, got) {
            (Some(expected), Some(got)) => test_expression(expected, got),
            _ => assert_eq!(expected, got),
        }
    }

    #[test]
    #[timeout(1000)]
    fn test_large() {
        let input = include_str!("./test.sql");
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let _output = match parser.parse_program() {
            Ok(out) => out,
            Err(err) => panic!("Err({}):\n {}", err.inner, err.backtrace),
        };
    }
}
