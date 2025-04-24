use std::{backtrace::Backtrace, collections::HashMap};

use thiserror::Error;

use crate::{
    ast::{
        Columns, ExpressionInner, IdentExpression, InfixOperator, Join, JoinType, Named, Program,
        SelectExpression, Statement,
    },
    lexer::Lexer,
    token::{GetKind, Token, TokenKind},
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

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::Eq => Precedence::Equals,
            TokenKind::Period => Precedence::Access,
            TokenKind::In => Precedence::Range,
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
    UnsupportedInfix(TokenKind),

    #[error("Peek failed for token: {0}")]
    PeekFailed(TokenKind),

    #[error("Expected current token to be: {0}")]
    ExpectedCurrent(TokenKind),
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
    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn>,
    #[debug(skip)]
    infix_parse_fns: HashMap<TokenKind, InfixParseFn>,
}

type Result<T> = core::result::Result<T, ParserErrorWithBacktrace>;

type PrefixParseFn = fn(&mut Parser) -> Result<ExpressionInner>;
type InfixParseFn = fn(&mut Parser, left: ExpressionInner) -> Result<ExpressionInner>;

impl Parser {
    pub fn new(lex: Lexer) -> Self {
        let mut out = Parser {
            lex,
            cur_token: None,
            peek_token: None,

            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        out.register_prefix(TokenKind::Select, Self::parse_select);
        out.register_prefix(TokenKind::ident(""), Self::parse_ident);
        out.register_prefix(TokenKind::string(""), Self::parse_ident);
        out.register_prefix(TokenKind::LParen, Self::parse_grouped);

        out.register_infix(TokenKind::Period, Self::parse_infix);
        out.register_infix(TokenKind::Eq, Self::parse_infix);
        out.register_infix(TokenKind::In, Self::parse_infix);

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
                Some(Token {
                    kind: TokenKind::Comment(_),
                    ..
                }) => {
                    self.next_token();
                }
                Some(_) => {
                    let stmt =
                        Statement::Expression(self.parse_expression(Precedence::Lowest)?.into());
                    program.statements.push(stmt);
                    self.next_token();
                }
                None => panic!("Got none for cur_token after it was detected as some"),
            }
        }

        Ok(program)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ExpressionInner> {
        assert!(
            self.cur_token.is_some(),
            "Called parse_expression with cur_token being None"
        );

        let Some(prefix) = self
            .prefix_parse_fns
            .get(&self.cur_token.as_ref().unwrap().kind)
        else {
            return Err(ParserError::NoPrefixParseFn(
                self.cur_token.clone().unwrap(),
            ))?;
        };

        let mut left_exp = prefix(self)?;

        while !self.peek_token_is(TokenKind::Semicolon)
            && precedence < self.peek_precedence()
            && self.peek_token.is_some()
        {
            let infix = {
                let Some(cur_token) = &self.peek_token else {
                    return Ok(left_exp);
                };
                let Some(infix) = self.infix_parse_fns.get(&cur_token.kind) else {
                    return Ok(left_exp);
                };

                *infix
            };

            self.next_token();

            left_exp = infix(self, left_exp)?;
        }

        Ok(left_exp)
    }

    fn peek_precedence(&self) -> Precedence {
        match &self.peek_token {
            None => Precedence::Lowest,
            Some(tok) => Into::into(&tok.kind),
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match &self.cur_token {
            None => Precedence::Lowest,
            Some(tok) => Into::into(&tok.kind),
        }
    }

    fn parse_select(&mut self) -> Result<ExpressionInner> {
        assert_eq!(
            self.cur_token.get_kind(),
            Some(&TokenKind::Select),
            "Called parse_select when the cur_token is not select",
        );

        self.next_token();

        let columns = match self.cur_token.get_kind() {
            None => return Err(ParserError::PartialSelectExpr)?,
            Some(TokenKind::Asterisk) => Columns::All,
            _ => {
                let mut columns = vec![self.parse_column()?];

                self.next_token();

                while self.cur_token.get_kind() == Some(&TokenKind::Comma) {
                    self.next_token();
                    columns.push(self.parse_column()?);
                    if self.peek_token_is(TokenKind::Comma) {
                        self.next_token();
                    }
                }

                Columns::Individual(columns)
            }
        };

        self.expect_peek(TokenKind::From)?;
        self.next_token();

        let from = self.parse_named()?;

        let join = if self.peek_token_in(&[TokenKind::Inner]) {
            self.next_token();
            Some(self.parse_join()?)
        } else {
            None
        };

        let where_expr = if self.peek_token_is(TokenKind::Where) {
            self.next_token();
            self.next_token();

            let expr = self.parse_expression(Precedence::Lowest)?;
            Some(Box::new(expr))
        } else {
            None
        }
        .map(Into::into);

        Ok(ExpressionInner::Select(SelectExpression {
            columns,
            from: Box::new(from),
            where_expr,
            join,
        }))
    }

    fn parse_join(&mut self) -> Result<Join> {
        let join_type = match self.cur_token.get_kind() {
            Some(TokenKind::Inner) => JoinType::Inner,
            _ => panic!("Unsupported join type: {:?}", self.cur_token),
        };

        self.expect_peek(TokenKind::Join)?;

        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        let on = if self.peek_token_is(TokenKind::On) {
            self.next_token();
            self.next_token();

            Some(Box::new(self.parse_expression(Precedence::Lowest)?))
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

        let name = if self.peek_token_in(&[TokenKind::ident(""), TokenKind::string("")]) {
            self.next_token();
            Some(self.parse_ident_unwrap()?)
        } else {
            None
        };

        Ok(Named { expr, name })
    }

    fn parse_ident_unwrap(&mut self) -> Result<IdentExpression> {
        let ExpressionInner::Ident(ident) = self.parse_ident()? else {
            panic!("parse_ident didn't return ident");
        };
        Ok(ident)
    }

    fn parse_column(&mut self) -> Result<Named> {
        self.parse_named()
    }

    fn register_prefix(&mut self, token: TokenKind, fun: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, fun);
    }

    fn register_infix(&mut self, token: TokenKind, fun: InfixParseFn) {
        self.infix_parse_fns.insert(token, fun);
    }

    fn parse_ident(&mut self) -> Result<ExpressionInner> {
        let ident = match &self.cur_token.get_kind() {
            Some(TokenKind::String(str)) => format!("\"{}\"", str),
            Some(TokenKind::Ident(str)) => str.clone(),
            None => Err(ParserError::UnexpectedEOF)?,
            _ => panic!(
                "Called parse_ident with unsupported token type: {:?}",
                self.cur_token
            ),
        };

        Ok(ExpressionInner::Ident(IdentExpression { ident }))
    }

    fn peek_token_is(&self, token: TokenKind) -> bool {
        self.peek_token.get_kind() == Some(&token)
    }

    fn peek_token_in(&self, tokens: &[TokenKind]) -> bool {
        let Some(peek_tok) = self.peek_token.get_kind() else {
            return false;
        };

        tokens.contains(peek_tok)
    }

    fn parse_infix(&mut self, left: ExpressionInner) -> Result<ExpressionInner> {
        let op = match self.cur_token.get_kind() {
            None => panic!("Called parse_infix with cur_token = None"),
            Some(TokenKind::Period) => InfixOperator::Period,
            Some(TokenKind::Eq) => InfixOperator::Eq,
            Some(TokenKind::In) => InfixOperator::In,
            _ => Err(ParserError::UnsupportedInfix(
                self.cur_token.as_ref().get_kind().unwrap().clone(),
            ))?,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(ExpressionInner::Infix(crate::ast::InfixExpression {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }))
    }

    fn parse_grouped(&mut self) -> Result<ExpressionInner> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::LParen));

        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::RParen)?;

        let name = if self.peek_token_in(&[TokenKind::ident(""), TokenKind::string("")]) {
            self.next_token();

            Some(self.parse_ident_unwrap()?)
        } else {
            None
        };

        Ok(ExpressionInner::Grouped(crate::ast::GroupedExpression {
            inner: Box::new(exp.into()),
            name,
        }))
    }

    fn expect_peek(&mut self, token: TokenKind) -> Result<()> {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            Ok(())
        } else {
            eprintln!("Expect peek: {:?}", self);
            Err(ParserError::PeekFailed(token))?
        }
    }
}

#[cfg(test)]
mod tests {
    use ntest::timeout;

    use crate::ast::{
        Expression, GroupedExpression, IdentExpression, InfixExpression, SelectExpression,
    };

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

        let Statement::Expression(Expression {
            inner: ExpressionInner::Select(select),
            ..
        }) = &program.statements[0]
        else {
            panic!()
        };

        assert_eq!(
            &SelectExpression {
                columns: crate::ast::Columns::All,
                from: Box::new(Named {
                    expr: ExpressionInner::Ident(IdentExpression {
                        ident: "Hello".to_string()
                    })
                    .into(),
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

        let Statement::Expression(ExpressionInner::Grouped(select)) = &program.statements[0] else {
            panic!()
        };

        assert_eq!(
            &GroupedExpression {
                inner: Box::new(
                    ExpressionInner::Select(SelectExpression {
                        columns: crate::ast::Columns::All,
                        from: Box::new(Named {
                            expr: ExpressionInner::Ident(IdentExpression {
                                ident: "Hello".to_string()
                            })
                            .into(),
                            name: None,
                        }),
                        where_expr: None,
                        join: None
                    },)
                    .into()
                ),
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

        let Statement::Expression(Expression {
            inner: ExpressionInner::Select(select),
            ..
        }) = &program.statements[0]
        else {
            panic!()
        };

        let expected = SelectExpression {
            columns: crate::ast::Columns::All,
            from: Box::new(Named {
                expr: ExpressionInner::Ident(IdentExpression {
                    ident: "Hello".to_string(),
                })
                .into(),
                name: Some(IdentExpression {
                    ident: "h".to_string(),
                }),
            }),
            where_expr: None,
            join: Some(crate::ast::Join {
                join_type: JoinType::Inner,
                expr: Box::new(
                    ExpressionInner::Select(SelectExpression {
                        columns: crate::ast::Columns::All,
                        from: Box::new(Named {
                            expr: ExpressionInner::Ident(IdentExpression {
                                ident: "Other".to_string(),
                            })
                            .into(),
                            name: Some(IdentExpression {
                                ident: "o".to_string(),
                            }),
                        }),
                        where_expr: None,
                        join: None,
                    })
                    .into(),
                ),
                on: Some(Box::new(
                    ExpressionInner::Infix(crate::ast::InfixExpression {
                        left: Box::new(
                            ExpressionInner::Infix(crate::ast::InfixExpression {
                                left: Box::new(ExpressionInner::ident("h").into()),
                                op: InfixOperator::Period,
                                right: Box::new(ExpressionInner::ident("first").into()),
                            })
                            .into(),
                        ),
                        op: InfixOperator::Eq,
                        right: Box::new(
                            ExpressionInner::Infix(crate::ast::InfixExpression {
                                left: Box::new(ExpressionInner::ident("o").into()),
                                op: InfixOperator::Period,
                                right: Box::new(ExpressionInner::ident("first").into()),
                            })
                            .into(),
                        ),
                    })
                    .into(),
                )),
            }),
        };

        println!("{}", expected);
        println!("{}", select);

        test_select(&expected, select);
    }

    fn test_expression(expected: &Expression, got: &Expression) {
        match (&expected.inner, &got.inner) {
            (ExpressionInner::Select(expected), ExpressionInner::Select(got)) => {
                test_select(expected, got)
            }
            (ExpressionInner::Infix(expected), ExpressionInner::Infix(got)) => {
                test_infix(expected, got)
            }
            (ExpressionInner::Ident(expected), ExpressionInner::Ident(got)) => {
                assert_eq!(expected, got)
            }
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
