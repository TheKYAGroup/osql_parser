use std::{backtrace::Backtrace, collections::HashMap, num::ParseIntError};

use thiserror::Error;

use crate::{
    ast::{
        Columns, Expression, ExpressionInner, IdentExpression, InfixOperator, IntExpression, Join,
        JoinType, Named, PrefixOperator, Program, SelectExpression, Statement, When,
    },
    lexer::Lexer,
    token::{GetKind, Token, TokenKind},
};

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    As,
    Equals,
    LessGreater,
    Range,
    And,
    Or,
    Sum,
    Product,
    Prefix,
    Access,
    Call,
}

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::As => Precedence::As,
            TokenKind::Eq => Precedence::Equals,
            TokenKind::Period => Precedence::Access,
            TokenKind::In => Precedence::Range,
            TokenKind::Sub => Precedence::Sum,
            TokenKind::Asterisk => Precedence::Product,
            TokenKind::Slash => Precedence::Product,
            TokenKind::LParen => Precedence::Call,
            TokenKind::LT | TokenKind::GT | TokenKind::LTEq | TokenKind::GTEq => {
                Precedence::LessGreater
            }
            TokenKind::And => Precedence::And,
            TokenKind::Or => Precedence::Or,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("No prefix parse function for token: {0:?}")]
    NoPrefixParseFn(Token),

    #[error("Partial select expression")]
    PartialSelectExpr,

    #[error("Unexpected end of file")]
    UnexpectedEOF,

    #[error("Missing FROM clause in SELECT statement")]
    MissingFromInSelect,

    #[error("Missing join token")]
    MissingJoinToken,

    #[error("Unsupported infix operator: {0:?}")]
    UnsupportedInfix(TokenKind),

    #[error("Unsupported prefix operator: {0:?}")]
    UnsupportedPrefix(TokenKind),

    #[error("Peek failed for token: {expected}")]
    PeekFailed {
        expected: TokenKind,
        got: Option<Token>,
    },

    #[error("Expected current token to be: {0}")]
    ExpectedCurrent(TokenKind),

    #[error("Failed to parse int: {0}")]
    ParseIntError(#[from] ParseIntError),
}

#[derive(Debug, Error)]
#[error("{}", .inner)]
pub struct ParserErrorWithBacktrace {
    #[from]
    #[source]
    pub inner: ParserError,
    #[backtrace]
    pub backtrace: Backtrace,
}

#[derive(derive_more::Debug)]
pub struct Parser {
    #[debug(skip)]
    lex: Lexer,
    cur_token: Box<Option<Token>>,
    peek_token: Box<Option<Token>>,

    #[debug(skip)]
    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn>,
    #[debug(skip)]
    infix_parse_fns: HashMap<TokenKind, InfixParseFn>,

    expr_depth: usize,
}

type Result<T> = core::result::Result<T, ParserErrorWithBacktrace>;

type PrefixParseFn = fn(&mut Parser) -> Result<ExpressionInner>;
type InfixParseFn = fn(&mut Parser, left: Box<ExpressionInner>) -> Result<ExpressionInner>;

impl Parser {
    pub fn new(lex: Lexer) -> Self {
        let mut out = Parser {
            lex,
            cur_token: Box::new(None),
            peek_token: Box::new(None),

            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),

            expr_depth: 0,
        };

        out.register_prefix(TokenKind::Select, Self::parse_select);
        out.register_prefix(TokenKind::ident(""), Self::parse_ident);
        out.register_prefix(TokenKind::string(""), Self::parse_ident);
        out.register_prefix(TokenKind::LParen, Self::parse_grouped);
        out.register_prefix(TokenKind::Case, Self::parse_case);
        out.register_prefix(TokenKind::Integer("".to_string()), Self::parse_int);
        out.register_prefix(TokenKind::Sub, Self::parse_prefix);
        out.register_prefix(TokenKind::Asterisk, Self::parse_prefix);

        out.register_infix(TokenKind::Period, Self::parse_infix);
        out.register_infix(TokenKind::Eq, Self::parse_infix);
        out.register_infix(TokenKind::In, Self::parse_in);
        out.register_infix(TokenKind::Sub, Self::parse_infix);
        out.register_infix(TokenKind::Slash, Self::parse_infix);
        out.register_infix(TokenKind::Asterisk, Self::parse_infix);
        out.register_infix(TokenKind::LT, Self::parse_infix);
        out.register_infix(TokenKind::GT, Self::parse_infix);
        out.register_infix(TokenKind::LTEq, Self::parse_infix);
        out.register_infix(TokenKind::GTEq, Self::parse_infix);
        out.register_infix(TokenKind::LParen, Self::parse_call);
        out.register_infix(TokenKind::And, Self::parse_infix);
        out.register_infix(TokenKind::Or, Self::parse_infix);
        out.register_infix(TokenKind::As, Self::parse_as);

        out.next_token();
        out.next_token();

        out
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = Box::new(self.lex.next_token());
        loop {
            match *self.peek_token {
                Some(Token {
                    kind: TokenKind::Comment(_),
                    ..
                }) => {
                    self.peek_token = Box::new(self.lex.next_token());
                }
                _ => break,
            }
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program { statements: vec![] };

        while self.cur_token.is_some() {
            match *self.cur_token {
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

        self.expr_depth += 1;

        eprintln!("Expr depth: {}", self.expr_depth);

        let Some(prefix) = self
            .prefix_parse_fns
            .get(&self.cur_token.clone().unwrap().kind)
        else {
            return Err(ParserError::NoPrefixParseFn(
                self.cur_token.clone().unwrap(),
            ))?;
        };

        let mut left_exp = prefix(self)?;

        eprintln!("Got prefix");

        while !self.peek_token_is(TokenKind::Semicolon)
            && precedence < self.peek_precedence()
            && self.peek_token.is_some()
        {
            let infix = {
                let Some(cur_token) = &*self.peek_token else {
                    return Ok(left_exp);
                };
                let Some(infix) = self.infix_parse_fns.get(&cur_token.kind) else {
                    return Ok(left_exp);
                };

                *infix
            };

            self.next_token();

            left_exp = infix(self, Box::new(left_exp))?;
        }

        self.expr_depth -= 1;

        eprintln!("Exited depth: {}", self.expr_depth);

        Ok(left_exp)
    }

    fn peek_precedence(&self) -> Precedence {
        match &*self.peek_token {
            None => Precedence::Lowest,
            Some(tok) => Into::into(&tok.kind),
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match &*self.cur_token {
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

        let mut join = vec![];

        let join_things = &[TokenKind::Inner, TokenKind::Left];

        while self.peek_token_in(join_things) {
            self.next_token();
            println!("Parsing join: {:?}\n", self);
            let out = self.parse_join()?;
            println!("Parsed join: {:?}\n", self);
            join.push(out);
        }

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
            Some(TokenKind::Left) => JoinType::Left,
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
            expr: Box::new(expr.into()),
            on: on.map(Into::into),
        })
    }

    fn parse_as(&mut self, left: Box<ExpressionInner>) -> Result<ExpressionInner> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::As));

        self.next_token();

        let right = self.parse_ident()?;
        let ExpressionInner::Ident(ident) = right else {
            unreachable!()
        };

        Ok(ExpressionInner::Named(Named {
            expr: left.into(),
            name: Some(ident),
        }))
    }

    fn parse_named(&mut self) -> Result<Named> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        let name = if self.peek_token_in(&[TokenKind::ident(""), TokenKind::string("")]) {
            self.next_token();
            Some(self.parse_ident_unwrap()?)
        } else if self.peek_token_is(TokenKind::As) {
            self.next_token();
            self.next_token();
            Some(self.parse_ident_unwrap()?)
        } else {
            None
        };

        Ok(Named {
            expr: Box::new(expr.into()),
            name,
        })
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

    fn parse_int(&mut self) -> Result<ExpressionInner> {
        let int = match &self.cur_token.get_kind() {
            Some(TokenKind::Integer(str)) => str.parse().map_err(|err| ParserError::from(err))?,
            None => Err(ParserError::UnexpectedEOF)?,
            _ => panic!(
                "Called parse_ident with unsupported token type: {:?}",
                self.cur_token
            ),
        };

        Ok(ExpressionInner::Int(IntExpression { int }))
    }

    fn peek_token_is(&self, token: TokenKind) -> bool {
        self.peek_token.get_kind() == Some(&token)
    }

    fn cur_token_is(&self, token: TokenKind) -> bool {
        self.cur_token.get_kind() == Some(&token)
    }

    fn peek_token_in(&self, tokens: &[TokenKind]) -> bool {
        let Some(peek_tok) = self.peek_token.get_kind() else {
            return false;
        };

        tokens.contains(peek_tok)
    }

    fn parse_in(&mut self, left: Box<ExpressionInner>) -> Result<ExpressionInner> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::In));

        self.next_token();

        let arr = self.parse_array()?;

        let right = ExpressionInner::Array(crate::ast::Array { arr });

        Ok(ExpressionInner::Infix(crate::ast::InfixExpression {
            left: left.into(),
            op: InfixOperator::In,
            right: Box::new(right.into()),
        }))
    }

    fn parse_infix(&mut self, left: Box<ExpressionInner>) -> Result<ExpressionInner> {
        let op = match self.cur_token.get_kind() {
            None => panic!("Called parse_infix with cur_token = None"),
            Some(TokenKind::Period) => InfixOperator::Period,
            Some(TokenKind::Eq) => InfixOperator::Eq,
            Some(TokenKind::Sub) => InfixOperator::Sub,
            Some(TokenKind::Slash) => InfixOperator::Div,
            Some(TokenKind::Asterisk) => InfixOperator::Mul,
            Some(TokenKind::LT) => InfixOperator::LT,
            Some(TokenKind::GT) => InfixOperator::GT,
            Some(TokenKind::LTEq) => InfixOperator::LTEq,
            Some(TokenKind::GTEq) => InfixOperator::GTEq,
            Some(TokenKind::And) => InfixOperator::And,
            Some(TokenKind::Or) => InfixOperator::Or,
            _ => Err(ParserError::UnsupportedInfix(
                self.cur_token.as_ref().get_kind().unwrap().clone(),
            ))?,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(ExpressionInner::Infix(crate::ast::InfixExpression {
            left: left.into(),
            op,
            right: Box::new(right.into()),
        }))
    }

    fn parse_call(&mut self, left: Box<ExpressionInner>) -> Result<ExpressionInner> {
        let func = left.into();

        let args = self.parse_array()?;

        Ok(ExpressionInner::FunctionCall(crate::ast::FunctionCall {
            func,
            args,
        }))
    }

    fn parse_array(&mut self) -> Result<Vec<Expression>> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::LParen));
        self.next_token();
        if self.cur_token_is(TokenKind::RParen) {
            return Ok(vec![]);
        }
        eprintln!("Parsing array element : {:?}", self);
        let mut out = vec![self.parse_expression(Precedence::Lowest)?.into()];
        eprintln!("Parsed element and then: {:?}", self);

        while self.peek_token_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();
            out.push(self.parse_expression(Precedence::Lowest)?.into());

            eprintln!("Parsed element and then: {:?}", self);
        }

        self.expect_peek(TokenKind::RParen)?;

        Ok(out)
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

    fn parse_prefix(&mut self) -> Result<ExpressionInner> {
        let op = match self.cur_token.get_kind() {
            Some(TokenKind::Sub) => PrefixOperator::Sub,
            Some(TokenKind::Asterisk) => {
                return Ok(ExpressionInner::All);
            }
            _ => Err(ParserError::UnsupportedPrefix(
                self.cur_token.as_ref().get_kind().unwrap().clone(),
            ))?,
        };

        self.next_token();

        let right = Box::new(self.parse_expression(Precedence::Lowest)?.into());

        Ok(ExpressionInner::Prefix(crate::ast::PrefixExpression {
            op,
            right,
        }))
    }

    fn parse_case(&mut self) -> Result<ExpressionInner> {
        assert_eq!(self.cur_token.get_kind(), Some(TokenKind::Case).as_ref());

        let mut expr = None;

        if !self.peek_token_is(TokenKind::When) && !self.peek_token_is(TokenKind::Else) {
            self.next_token();
            expr = Some(Box::new(self.parse_expression(Precedence::Lowest)?.into()));
        }

        let mut when_exprs = Vec::new();

        while self.peek_token_is(TokenKind::When) {
            self.next_token();
            when_exprs.push(self.parse_when()?);
        }

        self.expect_peek(TokenKind::Else)?;
        self.next_token();

        let else_expr = Box::new(self.parse_expression(Precedence::Lowest)?.into());

        self.expect_peek(TokenKind::End)?;

        Ok(ExpressionInner::Case(crate::ast::CaseExpression {
            expr,
            when_exprs,
            else_expr,
        }))
    }

    fn parse_when(&mut self) -> Result<When> {
        assert_eq!(self.cur_token.get_kind(), Some(TokenKind::When).as_ref());
        self.next_token();

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?.into());
        self.expect_peek(TokenKind::Then)?;
        self.next_token();
        let result = Box::new(self.parse_expression(Precedence::Lowest)?.into());

        Ok(When { condition, result })
    }

    fn expect_peek(&mut self, token: TokenKind) -> Result<()> {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            Ok(())
        } else {
            eprintln!("Expected: {:?}", token);
            eprintln!("Expect peek: {:?}", self);
            Err(ParserError::PeekFailed {
                expected: token,
                got: *self.peek_token.clone(),
            })?
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
    #[timeout(1000)]
    fn call() {
        let input = "SELECT * FROM Hello WHERE ISNULL(Thing)";

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

        let Expression {
            inner: ExpressionInner::FunctionCall(call),
            ..
        } = &**select.where_expr.as_ref().unwrap()
        else {
            unreachable!()
        };

        let Expression {
            inner: ExpressionInner::Ident(func),
            ..
        } = &*call.func
        else {
            unreachable!()
        };

        assert_eq!(func.ident, "ISNULL");

        assert_eq!(call.args.len(), 1);

        let Expression {
            inner: ExpressionInner::Ident(arg),
            ..
        } = &call.args[0]
        else {
            unreachable!()
        };

        assert_eq!(arg.ident, "Thing");
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
                    expr: Box::new(
                        ExpressionInner::Ident(IdentExpression {
                            ident: "Hello".to_string()
                        })
                        .into()
                    ),
                    name: None,
                }),
                where_expr: None,
                join: vec![]
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

        let Statement::Expression(Expression {
            inner: ExpressionInner::Grouped(select),
            ..
        }) = &program.statements[0]
        else {
            panic!()
        };

        assert_eq!(
            &GroupedExpression {
                inner: Box::new(
                    ExpressionInner::Select(SelectExpression {
                        columns: crate::ast::Columns::All,
                        from: Box::new(Named {
                            expr: Box::new(
                                ExpressionInner::Ident(IdentExpression {
                                    ident: "Hello".to_string()
                                })
                                .into()
                            ),
                            name: None,
                        }),
                        where_expr: None,
                        join: vec![]
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
                expr: Box::new(
                    ExpressionInner::Ident(IdentExpression {
                        ident: "Hello".to_string(),
                    })
                    .into(),
                ),
                name: Some(IdentExpression {
                    ident: "h".to_string(),
                }),
            }),
            where_expr: None,
            join: vec![crate::ast::Join {
                join_type: JoinType::Inner,
                expr: Box::new(
                    ExpressionInner::Select(SelectExpression {
                        columns: crate::ast::Columns::All,
                        from: Box::new(Named {
                            expr: Box::new(
                                ExpressionInner::Ident(IdentExpression {
                                    ident: "Other".to_string(),
                                })
                                .into(),
                            ),
                            name: Some(IdentExpression {
                                ident: "o".to_string(),
                            }),
                        }),
                        where_expr: None,
                        join: vec![],
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
            }],
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

    fn test_join(expected: &Vec<Join>, got: &Vec<Join>) {
        assert_eq!(expected.len(), got.len());
        for (expected, got) in std::iter::zip(expected.iter(), got.iter()) {
            assert_eq!(expected.join_type, got.join_type);

            test_expression(&expected.expr, &got.expr);

            test_optional_expr(&expected.on, &got.on);
        }
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
            Err(err) => match err.inner {
                ParserError::PeekFailed { expected, got } => {
                    eprintln!("Backtrace: {}", err.backtrace);
                    eprintln!("Failed to parse expected: {}", expected);
                    eprintln!("Got: {:?}", got);
                    match got {
                        None => {}
                        Some(tok) => {
                            eprint!("{}", &input[0..tok.start.idx]);
                            eprint!("{{|{}|}}", &input[tok.start.idx..tok.end.idx]);
                            eprintln!("{}", &input[tok.end.idx..]);
                        }
                    }
                    panic!()
                }
                _ => {
                    panic!("Err({}):\n {}", err.inner, err.backtrace)
                }
            },
        };
    }
}
