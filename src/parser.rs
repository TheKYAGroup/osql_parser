use std::{backtrace::Backtrace, collections::HashMap, num::ParseIntError};

use thiserror::Error;

use crate::{
    ast::{
        Between, Columns, Expression, ExpressionIdx, ExpressionInner, ExpressionStore, Fetch,
        FetchType, GroupBy, IdentExpression, InfixOperator, IntExpression, Join, JoinType, Named,
        NotInfixOperator, PrefixOperator, Program, Rows, SelectExpression, Statement, Union,
        UnionType, When,
    },
    lexer::Lexer,
    oir::Span,
    token::{GetKind, Loc, Token, TokenKind},
};

///
///
/// ```rust
/// use osql_parser::parser::Precedence;
/// assert!(Precedence::And < Precedence::Range)
/// ```
#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    As,
    Or,
    And,
    Not,
    Range,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Access,
    Call,
}

impl From<&TokenKind> for Precedence {
    fn from(value: &TokenKind) -> Self {
        match value {
            TokenKind::Not => Precedence::Not,
            TokenKind::Date => Precedence::Prefix,
            TokenKind::As => Precedence::As,
            TokenKind::Eq => Precedence::Equals,
            TokenKind::UnEq => Precedence::Equals,
            TokenKind::NotEq => Precedence::Equals,
            TokenKind::Period => Precedence::Access,
            TokenKind::In => Precedence::Range,
            TokenKind::Like => Precedence::Range,
            TokenKind::Is => Precedence::Range,
            TokenKind::Using => Precedence::Range,
            TokenKind::Between => Precedence::Range,
            TokenKind::Sub => Precedence::Sum,
            TokenKind::Plus => Precedence::Sum,
            TokenKind::JoinStrings => Precedence::Sum,
            TokenKind::Asterisk => Precedence::Product,
            TokenKind::Slash => Precedence::Product,
            TokenKind::LParen => Precedence::Call,
            TokenKind::By => Precedence::Range,
            TokenKind::LT | TokenKind::GT | TokenKind::LTEq | TokenKind::GTEq => {
                Precedence::LessGreater
            }
            TokenKind::And => Precedence::And,
            TokenKind::Or => Precedence::Or,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug, Error, Clone)]
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

    #[error("Unsupported not operator: {0:?}")]
    UnsupportedNot(TokenKind),

    #[error("Unsupported prefix operator: {0:?}")]
    UnsupportedPrefix(TokenKind),

    #[error("Peek failed for token: {expected}")]
    PeekFailed {
        expected: TokenKind,
        got: Option<Token>,
    },

    #[error("Peek failed for token: {expected:?}")]
    MultiPeekFailed {
        expected: Vec<TokenKind>,
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
    pub backtrace: Box<Backtrace>,
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

    #[debug(skip)]
    expr_store: ExpressionStore,
}

impl std::hash::Hash for Parser {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.lex.hash(state);
        self.cur_token.hash(state);
        self.peek_token.hash(state);
        self.expr_store.hash(state);
    }
}

pub type Result<T> = core::result::Result<T, ParserErrorWithBacktrace>;

type PrefixParseFn = fn(&mut Parser) -> Result<ExpressionIdx>;
type InfixParseFn = fn(&mut Parser, left: ExpressionIdx) -> Result<ExpressionIdx>;

impl Parser {
    pub fn new(lex: Lexer) -> Self {
        let mut out = Parser {
            lex,
            cur_token: None,
            peek_token: None,

            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),

            expr_store: ExpressionStore::new(),
        };

        out.register_prefix(TokenKind::Select, Self::parse_select);
        out.register_prefix(TokenKind::ident(""), Self::parse_ident);
        out.register_prefix(TokenKind::string(""), Self::parse_ident);
        out.register_prefix(TokenKind::LParen, Self::parse_grouped);
        out.register_prefix(TokenKind::Case, Self::parse_case);
        out.register_prefix(TokenKind::Integer("".into()), Self::parse_int);
        out.register_prefix(TokenKind::Sub, Self::parse_prefix);
        out.register_prefix(TokenKind::Asterisk, Self::parse_prefix);
        out.register_prefix(TokenKind::At, Self::parse_null_or);
        out.register_prefix(TokenKind::Null, Self::parse_null);
        out.register_prefix(TokenKind::Not, Self::parse_prefix);
        out.register_prefix(TokenKind::Date, Self::parse_prefix);

        out.register_infix(TokenKind::Period, Self::parse_infix);
        out.register_infix(TokenKind::Eq, Self::parse_infix);
        out.register_infix(TokenKind::In, Self::parse_not_infix);
        out.register_infix(TokenKind::Sub, Self::parse_infix);
        out.register_infix(TokenKind::Plus, Self::parse_infix);
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
        out.register_infix(TokenKind::Is, Self::parse_infix);
        out.register_infix(TokenKind::UnEq, Self::parse_infix);
        out.register_infix(TokenKind::NotEq, Self::parse_infix);
        out.register_infix(TokenKind::Using, Self::parse_infix);
        out.register_infix(TokenKind::Like, Self::parse_not_infix);
        out.register_infix(TokenKind::By, Self::parse_infix);
        out.register_infix(TokenKind::JoinStrings, Self::parse_infix);
        out.register_infix(TokenKind::Between, Self::parse_between);
        out.register_infix(TokenKind::Not, Self::parse_not_infix);

        out.next_token();
        out.next_token();

        out
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lex.next_token();
        while let Some(Token {
            kind: TokenKind::Comment(_),
            ..
        }) = self.peek_token
        {
            self.peek_token = self.lex.next_token();
        }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut stmts = vec![];

        while self.cur_token.is_some() {
            match self.cur_token {
                Some(Token {
                    kind: TokenKind::Comment(_),
                    ..
                }) => {
                    self.next_token();
                }
                Some(_) => {
                    let stmt = Statement::Expression(self.parse_expression(Precedence::Lowest)?);
                    stmts.push(stmt);
                    self.next_token();
                }
                None => panic!("Got none for cur_token after it was detected as some"),
            }
        }

        Ok(Program {
            store: self.expr_store.clone(),
            statements: stmts,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ExpressionIdx> {
        assert!(
            self.cur_token.is_some(),
            "Called parse_expression with cur_token being None"
        );

        let Some(prefix) = self
            .prefix_parse_fns
            .get(&self.cur_token.clone().unwrap().kind)
        else {
            return Err(ParserError::NoPrefixParseFn(
                self.cur_token.clone().unwrap(),
            ))?;
        };

        let mut left_exp = Box::new(prefix(self)?);

        while !self.peek_token_is(TokenKind::Semicolon)
            && precedence < self.peek_precedence()
            && self.peek_token.is_some()
        {
            let infix = {
                let Some(cur_token) = &self.peek_token else {
                    return Ok(*left_exp);
                };
                let Some(infix) = self.infix_parse_fns.get(&cur_token.kind) else {
                    return Ok(*left_exp);
                };

                *infix
            };

            self.next_token();

            left_exp = Box::new(infix(self, *left_exp)?);
        }

        Ok(*left_exp)
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

    fn parse_select(&mut self) -> Result<ExpressionIdx> {
        assert_eq!(
            self.cur_token.get_kind(),
            Some(&TokenKind::Select),
            "Called parse_select when the cur_token is not select",
        );

        let start = self.cur_token.as_ref().unwrap().start;

        self.next_token();

        let distinct = match self.cur_token.get_kind() {
            Some(TokenKind::Distinct) => {
                self.next_token();
                true
            }
            _ => false,
        };

        let columns = match self.cur_token.get_kind() {
            None => return Err(ParserError::PartialSelectExpr)?,
            Some(TokenKind::Asterisk) => Columns::All,
            _ => {
                let mut columns = vec![self.parse_column()?];

                if self.peek_token_is(TokenKind::Comma) {
                    self.next_token();
                }

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

        let join_things = &[TokenKind::Inner, TokenKind::Left, TokenKind::Full];

        while self.peek_token_in(join_things) {
            self.next_token();
            let out = self.parse_join()?;
            join.push(out);
        }

        let where_expr = if self.peek_token_is(TokenKind::Where) {
            self.next_token();
            self.next_token();

            Some(self.parse_expression(Precedence::Lowest)?)
        } else {
            None
        };

        let group = if self.peek_token_is(TokenKind::Group) {
            self.next_token();
            Some(self.parse_group_by()?)
        } else {
            None
        };

        let mut union = vec![];

        while self.peek_token_is(TokenKind::Union) {
            self.next_token();
            let out = self.parse_union()?;
            union.push(out);
        }

        let fetch = if self.peek_token_is(TokenKind::Fetch) {
            self.next_token();
            Some(self.parse_fetch()?)
        } else {
            None
        };

        let end = self.cur_token.as_ref().unwrap().end;

        Ok(self.expr_store.add(Expression {
            inner: ExpressionInner::Select(SelectExpression {
                distinct,
                columns,
                from,
                where_expr,
                join,
                group,
                union,
                fetch,
            }),
            start,
            end,
        }))
    }

    fn parse_join(&mut self) -> Result<Join> {
        let join_type = match self.cur_token.get_kind() {
            Some(TokenKind::Inner) => JoinType::Inner,
            Some(TokenKind::Left) => match self.peek_token.get_kind() {
                Some(TokenKind::Outer) => JoinType::Outer(crate::ast::OuterJoinDirection::Left),
                _ => JoinType::Left,
            },
            Some(TokenKind::Full) if self.peek_token.get_kind() == Some(&TokenKind::Outer) => {
                JoinType::Outer(crate::ast::OuterJoinDirection::Full)
            }
            _ => panic!("Unsupported join type: {:?}", self.cur_token),
        };

        if let JoinType::Outer(
            crate::ast::OuterJoinDirection::Left | crate::ast::OuterJoinDirection::Full,
        ) = join_type
        {
            self.next_token()
        };

        self.expect_peek(TokenKind::Join)?;

        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        let on = if self.peek_token_is(TokenKind::On) {
            self.next_token();
            self.next_token();

            Some(self.parse_expression(Precedence::Lowest)?)
        } else {
            None
        };

        Ok(Join {
            join_type,
            expr,
            on,
        })
    }

    fn parse_union(&mut self) -> Result<Union> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::Union));
        let union_type = match self.peek_token.get_kind() {
            Some(TokenKind::All) => UnionType::All,
            Some(_) => UnionType::None,
            _ => return Err(ParserError::UnexpectedEOF.into()),
        };
        self.next_token();
        if union_type != UnionType::None {
            self.next_token();
        }

        let expr = self.parse_expression(Precedence::Lowest)?;

        Ok(Union { union_type, expr })
    }

    fn parse_as(&mut self, left: ExpressionIdx) -> Result<ExpressionIdx> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::As));

        let start = self.get_expr_start(&left);

        self.next_token();

        let right = match &self.cur_token {
            Some(Token {
                kind: TokenKind::Date,
                start,
                end,
            }) => self.new_expr_idx(Expression {
                inner: ExpressionInner::Ident(IdentExpression {
                    ident: "date".into(),
                }),
                start: *start,
                end: *end,
            }),
            _ => self.parse_ident()?,
        };
        let ExpressionInner::Ident(ident) = self.expr_store.remove(right).unwrap().inner else {
            unreachable!()
        };

        let end = self.get_end().unwrap();

        let inner = ExpressionInner::Named(Named {
            expr: left,
            name: Some(ident),
            span: Span { start, end },
        });

        Ok(self.new_expr_idx(Expression { start, inner, end }))
    }

    fn parse_named(&mut self) -> Result<Named> {
        let start = self.get_start().unwrap();
        let expr = self.parse_expression(Precedence::Lowest)?;

        if let Expression {
            inner: ExpressionInner::Named(named),
            ..
        } = self.expr_store.get_ref(&expr).unwrap()
        {
            let out = named.clone();
            _ = self.expr_store.remove(expr);
            return Ok(out);
        };
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

        let end = self.get_end().unwrap();

        Ok(Named {
            expr,
            name,
            span: Span { start, end },
        })
    }

    fn parse_ident_unwrap(&mut self) -> Result<IdentExpression> {
        let ident = match &self.cur_token.get_kind() {
            Some(TokenKind::String(str)) => ecow::eco_format!("\"{}\"", str),
            Some(TokenKind::Ident(str)) => str.clone(),
            None => Err(ParserError::UnexpectedEOF)?,
            _ => {
                panic!(
                    "Called parse_ident with unsupported token type: {:?}",
                    self.cur_token
                )
            }
        };
        Ok(IdentExpression { ident })
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

    fn parse_ident(&mut self) -> Result<ExpressionIdx> {
        let ident = self.parse_ident_unwrap()?;

        let Token { start, end, .. } = self.cur_token.clone().unwrap();

        Ok(self.new_expr_idx(Expression {
            inner: ExpressionInner::Ident(ident),
            start,
            end,
        }))
    }

    fn parse_null(&mut self) -> Result<ExpressionIdx> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::Null));

        Ok(self.new_expr_idx(Expression {
            inner: ExpressionInner::Null(crate::ast::Null),
            start: self.get_start().unwrap(),
            end: self.get_end().unwrap(),
        }))
    }

    fn new_expr_idx(&mut self, expr: Expression) -> ExpressionIdx {
        self.expr_store.add(expr)
    }

    fn parse_int(&mut self) -> Result<ExpressionIdx> {
        let int = match &self.cur_token.get_kind() {
            Some(TokenKind::Integer(str)) => str.parse().map_err(ParserError::from)?,
            None => Err(ParserError::UnexpectedEOF)?,
            _ => panic!(
                "Called parse_int with unsupported token type: {:?}",
                self.cur_token
            ),
        };
        let start = self.get_start().unwrap();
        let end = self.get_end().unwrap();

        let inner = ExpressionInner::Int(IntExpression { int });

        Ok(self.new_expr_idx(Expression { inner, start, end }))
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

    fn parse_infix(&mut self, left: ExpressionIdx) -> Result<ExpressionIdx> {
        let start = self.get_expr_start(&left);
        let op = match self.cur_token.get_kind() {
            None => panic!("Called parse_infix with cur_token = None"),
            Some(TokenKind::Period) => InfixOperator::Period,
            Some(TokenKind::Eq) => InfixOperator::Eq,
            Some(TokenKind::Sub) => InfixOperator::Sub,
            Some(TokenKind::Plus) => InfixOperator::Add,
            Some(TokenKind::Slash) => InfixOperator::Div,
            Some(TokenKind::Asterisk) => InfixOperator::Mul,
            Some(TokenKind::LT) => InfixOperator::LT,
            Some(TokenKind::GT) => InfixOperator::GT,
            Some(TokenKind::LTEq) => InfixOperator::LTEq,
            Some(TokenKind::GTEq) => InfixOperator::GTEq,
            Some(TokenKind::And) => InfixOperator::And,
            Some(TokenKind::Or) => InfixOperator::Or,
            Some(TokenKind::Is) => InfixOperator::Is,
            Some(TokenKind::UnEq) => InfixOperator::UnEq,
            Some(TokenKind::NotEq) => InfixOperator::NotEq,
            Some(TokenKind::Not) => InfixOperator::NotEq,
            Some(TokenKind::Using) => InfixOperator::Using,
            Some(TokenKind::By) => InfixOperator::By,
            Some(TokenKind::JoinStrings) => InfixOperator::JoinStrings,
            _ => Err(ParserError::UnsupportedInfix(
                self.cur_token.as_ref().get_kind().unwrap().clone(),
            ))?,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;

        let end = self.get_end().unwrap();

        let inner = ExpressionInner::Infix(crate::ast::InfixExpression { left, op, right });

        Ok(self.new_expr_idx(Expression { inner, start, end }))
    }

    fn get_expr_start(&self, expr: &ExpressionIdx) -> Loc {
        self.expr_store.get_ref(expr).unwrap().start
    }

    fn parse_call(&mut self, left: ExpressionIdx) -> Result<ExpressionIdx> {
        let start = self.get_expr_start(&left);

        let args = self.parse_array()?;

        let inner = ExpressionInner::FunctionCall(crate::ast::FunctionCall { func: left, args });

        let end = self.get_end().unwrap();

        Ok(self.new_expr_idx(Expression { inner, start, end }))
    }

    fn parse_array_infix(&mut self, left: ExpressionIdx) -> Result<ExpressionIdx> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::Comma));
        let start = self.get_expr_start(&left);
        let mut arr = vec![left];
        self.next_token();
        arr.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();
            arr.push(self.parse_expression(Precedence::Lowest)?);
        }

        let inner = ExpressionInner::Array(crate::ast::Array { arr });

        let end = self.get_end().unwrap();

        Ok(self.new_expr_idx(Expression { inner, start, end }))
    }

    fn parse_array(&mut self) -> Result<Vec<ExpressionIdx>> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::LParen));
        self.next_token();
        if self.cur_token_is(TokenKind::RParen) {
            return Ok(vec![]);
        }
        let mut out = vec![self.parse_expression(Precedence::Lowest)?];

        while self.peek_token_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();
            out.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(TokenKind::RParen)?;

        Ok(out)
    }

    fn get_start(&self) -> Option<Loc> {
        self.cur_token.as_ref().map(|tok| tok.start)
    }

    fn get_end(&self) -> Option<Loc> {
        self.cur_token.as_ref().map(|tok| tok.end)
    }

    fn parse_grouped(&mut self) -> Result<ExpressionIdx> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::LParen));

        let start = self.get_start().unwrap();

        self.next_token();

        let mut exp = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token.get_kind() == Some(&TokenKind::Comma) {
            self.next_token();
            exp = self.parse_array_infix(exp)?;
        }

        self.expect_peek(TokenKind::RParen)?;

        let name = if self.peek_token_in(&[TokenKind::ident(""), TokenKind::string("")]) {
            self.next_token();

            Some(self.parse_ident_unwrap()?)
        } else {
            None
        };

        let end = self.get_end().unwrap();

        Ok(self.new_expr_idx(Expression {
            start,
            end,
            inner: ExpressionInner::Grouped(crate::ast::GroupedExpression { inner: exp, name }),
        }))
    }

    fn parse_null_or(&mut self) -> Result<ExpressionIdx> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::At));
        let start = self.get_start().unwrap();

        self.expect_peek(TokenKind::LBracket)?;

        self.next_token();

        let expected = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RBracket)?;
        self.expect_peek(TokenKind::LBracket)?;
        self.next_token();
        let alternative = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RBracket)?;

        let end = self.get_end().unwrap();

        let inner = ExpressionInner::NullOr(crate::ast::NullOr {
            expected,
            alternative,
        });

        Ok(self.new_expr_idx(Expression { inner, start, end }))
    }

    fn parse_prefix(&mut self) -> Result<ExpressionIdx> {
        let start = self.get_start().unwrap();
        let op = match self.cur_token.get_kind() {
            Some(TokenKind::Sub) => PrefixOperator::Sub,
            Some(TokenKind::Not) => PrefixOperator::Not,
            Some(TokenKind::Asterisk) => {
                let end = self.get_end().unwrap();
                return Ok(self.new_expr_idx(Expression {
                    inner: ExpressionInner::All(crate::ast::All),
                    start,
                    end,
                }));
            }
            Some(TokenKind::Date) => PrefixOperator::Date,
            _ => Err(ParserError::UnsupportedPrefix(
                self.cur_token.as_ref().get_kind().unwrap().clone(),
            ))?,
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        let end = self.get_end().unwrap();

        let inner = ExpressionInner::Prefix(crate::ast::PrefixExpression { op, right });

        Ok(self.new_expr_idx(Expression { inner, start, end }))
    }

    fn parse_case(&mut self) -> Result<ExpressionIdx> {
        assert_eq!(self.cur_token.get_kind(), Some(TokenKind::Case).as_ref());

        let start = self.get_start().unwrap();

        let mut expr = None;

        if !self.peek_token_is(TokenKind::When) && !self.peek_token_is(TokenKind::Else) {
            self.next_token();
            expr = Some(self.parse_expression(Precedence::Lowest)?);
        }

        let mut when_exprs = Vec::new();

        while self.peek_token_is(TokenKind::When) {
            self.next_token();
            when_exprs.push(self.parse_when()?);
        }

        self.expect_peek(TokenKind::Else)?;
        self.next_token();

        let else_expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::End)?;

        let end = self.get_end().unwrap();

        let inner = ExpressionInner::Case(crate::ast::CaseExpression {
            expr,
            when_exprs,
            else_expr,
        });

        Ok(self.new_expr_idx(Expression { inner, start, end }))
    }

    fn parse_when(&mut self) -> Result<When> {
        assert_eq!(self.cur_token.get_kind(), Some(TokenKind::When).as_ref());
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::Then)?;
        self.next_token();
        let result = self.parse_expression(Precedence::Lowest)?;

        Ok(When { condition, result })
    }

    fn expect_peek(&mut self, token: TokenKind) -> Result<()> {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::PeekFailed {
                expected: token,
                got: self.peek_token.clone(),
            })?
        }
    }

    fn expect_peek_in(&mut self, token: &[TokenKind]) -> Result<()> {
        if self.peek_token_in(token) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::MultiPeekFailed {
                expected: token.to_vec(),
                got: self.peek_token.clone(),
            })?
        }
    }

    fn parse_group_by(&mut self) -> Result<GroupBy> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::Group));
        self.expect_peek(TokenKind::By)?;

        self.next_token();

        let mut by = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(TokenKind::Comma) {
            self.next_token();
            by = self.parse_array_infix(by)?;
        }

        Ok(GroupBy { by })
    }

    fn parse_between(&mut self, left: ExpressionIdx) -> Result<ExpressionIdx> {
        assert_eq!(self.cur_token.get_kind(), Some(&TokenKind::Between));

        let start = self.get_expr_start(&left);

        self.next_token();

        let lower = self.parse_expression(Precedence::Range)?;
        self.expect_peek(TokenKind::And)?;
        self.next_token();
        let upper = self.parse_expression(Precedence::Lowest)?;

        let end = self.get_end().unwrap();

        let inner = ExpressionInner::Between(Between { left, lower, upper });

        Ok(self.new_expr_idx(Expression { start, inner, end }))
    }

    fn parse_not_infix(&mut self, left: ExpressionIdx) -> Result<ExpressionIdx> {
        let not = matches!(self.cur_token.get_kind(), Some(TokenKind::Not));

        let start = self.get_expr_start(&left);

        if not {
            self.next_token();
        }

        let op = match self.cur_token.get_kind() {
            None => panic!("Called parse_not_infix with cur_token = None"),
            Some(TokenKind::Like) => NotInfixOperator::Like,
            Some(TokenKind::In) => NotInfixOperator::In,
            _ => Err(ParserError::UnsupportedInfix(
                self.cur_token.as_ref().get_kind().unwrap().clone(),
            ))?,
        };

        self.next_token();

        let right = self.parse_expression(Precedence::Range)?;

        let end = self.get_end().unwrap();

        Ok(self.new_expr_idx(Expression {
            inner: ExpressionInner::NotInfix(crate::ast::NotInfixExpression {
                left,
                not,
                right,
                op,
            }),
            start,
            end,
        }))
    }

    fn parse_fetch(&mut self) -> Result<Fetch> {
        assert_eq!(
            self.cur_token.get_kind(),
            Some(&TokenKind::Fetch),
            "Called parse_select when the cur_token is not FETCH",
        );

        self.expect_peek_in(&[TokenKind::First, TokenKind::Next])?;
        let fetch_type = match self.cur_token.get_kind() {
            Some(TokenKind::First) => FetchType::First,
            Some(TokenKind::Next) => FetchType::Next,
            _ => {
                panic!("Got unexpected token for fetch_type: {:?}", self.peek_token)
            }
        };

        self.next_token();

        let amount = if self.peek_token_is(TokenKind::Row) {
            None
        } else {
            Some(self.parse_int()?)
        };

        self.expect_peek_in(&[TokenKind::Rows, TokenKind::Row])?;

        let rows = match self.cur_token.get_kind() {
            Some(TokenKind::Rows) => Rows::Rows,
            Some(TokenKind::Row) => Rows::Row,
            _ => panic!("Got unexpected rows kind"),
        };

        self.next_token();

        Ok(Fetch {
            fetch_type,
            amount,
            rows,
        })
    }
}

#[cfg(test)]
mod tests {
    use ntest::timeout;

    use crate::ast::{
        Expression, GroupedExpression, IdentExpression, InfixExpression, PrintExpression,
        SelectExpression,
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

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = unwrap_backtrace(parser.parse_program());

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(expr) = &program.statements[0];

        let store = &program.store;

        let ExpressionInner::Select(ref select) = store.get_ref(expr).unwrap().inner else {
            panic!()
        };

        let Expression {
            inner: ExpressionInner::FunctionCall(call),
            ..
        } = store.get_ref(select.where_expr.as_ref().unwrap()).unwrap()
        else {
            unreachable!()
        };

        let Expression {
            inner: ExpressionInner::Ident(func),
            ..
        } = store.get_ref(&call.func).unwrap()
        else {
            unreachable!()
        };

        assert_eq!(func.ident, "ISNULL");

        assert_eq!(call.args.len(), 1);

        let Expression {
            inner: ExpressionInner::Ident(arg),
            ..
        } = store.get_ref(&call.args[0]).unwrap()
        else {
            unreachable!()
        };

        assert_eq!(arg.ident, "Thing");
    }

    #[test]
    #[timeout(100)]
    fn simple_select() {
        let input = "SELECT * FROM Hello";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = unwrap_backtrace(parser.parse_program());

        let expected = {
            let store = &mut program.store;

            let expr = store.add(
                ExpressionInner::Ident(IdentExpression {
                    ident: "Hello".into(),
                })
                .into(),
            );

            SelectExpression {
                columns: crate::ast::Columns::All,
                from: Named {
                    expr,
                    name: None,
                    ..Default::default()
                },
                where_expr: None,
                join: vec![],
                group: None,
                union: vec![],
                distinct: false,
                ..Default::default()
            }
        };

        let store = &program.store;

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(stmt) = &program.statements[0];

        let ExpressionInner::Select(select) = &store.get_ref(stmt).unwrap().inner else {
            unreachable!()
        };

        test_select(store, &expected, select);
    }

    #[test]
    #[timeout(100)]
    fn grouped() {
        let input = "(SELECT * FROM Hello) M";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = unwrap_backtrace(parser.parse_program());

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(expr) = &program.statements[0];

        let expected = {
            let store = &mut program.store;

            let expr = store.add(
                ExpressionInner::Ident(IdentExpression {
                    ident: "Hello".into(),
                })
                .into(),
            );

            GroupedExpression {
                inner: store.add(
                    ExpressionInner::Select(SelectExpression {
                        columns: crate::ast::Columns::All,
                        from: Named {
                            expr,
                            name: None,
                            ..Default::default()
                        },
                        where_expr: None,
                        join: vec![],
                        group: None,
                        union: vec![],
                        distinct: false,
                        ..Default::default()
                    })
                    .into(),
                ),
                name: Some(IdentExpression { ident: "M".into() }),
            }
        };

        let ExpressionInner::Grouped(ref select) = program.store.get_ref(expr).unwrap().inner
        else {
            unreachable!()
        };

        test_grouped(&program.store, &expected, select);
    }

    #[test]
    #[timeout(1000)]
    fn inner_join() {
        let input = r#"SELECT * FROM Hello h INNER JOIN SELECT * FROM Other o
            ON h.thing = o.thing
            "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = unwrap_backtrace(parser.parse_program());

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(expr) = &program.statements[0];

        let expected = {
            let store = &mut program.store;

            SelectExpression {
                group: None,
                union: vec![],
                distinct: false,
                columns: crate::ast::Columns::All,
                from: Named {
                    expr: store.add(
                        ExpressionInner::Ident(IdentExpression {
                            ident: "Hello".into(),
                        })
                        .into(),
                    ),
                    name: Some(IdentExpression { ident: "h".into() }),
                    ..Default::default()
                },
                where_expr: None,
                join: vec![crate::ast::Join {
                    join_type: JoinType::Inner,
                    expr: {
                        let expr = {
                            store.add(
                                ExpressionInner::Ident(IdentExpression {
                                    ident: "Other".into(),
                                })
                                .into(),
                            )
                        };
                        store.add(
                            ExpressionInner::Select(SelectExpression {
                                group: None,
                                columns: crate::ast::Columns::All,
                                from: Named {
                                    expr,
                                    name: Some(IdentExpression { ident: "o".into() }),
                                    ..Default::default()
                                },
                                where_expr: None,
                                join: vec![],
                                union: vec![],
                                distinct: false,
                                ..Default::default()
                            })
                            .into(),
                        )
                    },
                    on: {
                        let left = {
                            let left = store.add(ExpressionInner::ident("h").into());
                            let right = store.add(ExpressionInner::ident("thing").into());
                            store.add(
                                ExpressionInner::Infix(crate::ast::InfixExpression {
                                    left,
                                    op: InfixOperator::Period,
                                    right,
                                })
                                .into(),
                            )
                        };
                        Some({
                            let right = {
                                let left = store.add(ExpressionInner::ident("o").into());
                                let right = store.add(ExpressionInner::ident("thing").into());
                                store.add(
                                    ExpressionInner::Infix(crate::ast::InfixExpression {
                                        left,
                                        op: InfixOperator::Period,
                                        right,
                                    })
                                    .into(),
                                )
                            };
                            store.add(
                                ExpressionInner::Infix(crate::ast::InfixExpression {
                                    left,
                                    op: InfixOperator::Eq,
                                    right,
                                })
                                .into(),
                            )
                        })
                    },
                }],
                ..Default::default()
            }
        };

        let ExpressionInner::Select(ref select) = program.store.get_ref(expr).unwrap().inner else {
            panic!()
        };

        println!(
            "{}",
            PrintExpression::new(
                &Into::<Expression>::into(ExpressionInner::Select(expected.clone())),
                &program.store
            )
        );
        println!("{}", &program);

        test_select(&program.store, &expected, select);
    }

    fn test_expression(store: &ExpressionStore, expected: &ExpressionIdx, got: &ExpressionIdx) {
        match (
            &store.get_ref(expected).unwrap().inner,
            &store.get_ref(got).unwrap().inner,
        ) {
            (ExpressionInner::Select(expected), ExpressionInner::Select(got)) => {
                test_select(store, expected, got)
            }
            (ExpressionInner::Infix(expected), ExpressionInner::Infix(got)) => {
                test_infix(store, expected, got)
            }
            (ExpressionInner::Ident(expected), ExpressionInner::Ident(got)) => {
                assert_eq!(expected, got)
            }
            (ExpressionInner::Prefix(expected), ExpressionInner::Prefix(got)) => {
                test_prefix(store, expected, got)
            }
            (expected, got) => assert_eq!(expected, got),
        }
    }

    fn test_prefix(
        store: &ExpressionStore,
        expected: &crate::ast::PrefixExpression,
        got: &crate::ast::PrefixExpression,
    ) {
        assert_eq!(expected.op, got.op);
        test_expression(store, &expected.right, &got.right);
    }

    fn test_select(store: &ExpressionStore, expected: &SelectExpression, got: &SelectExpression) {
        assert_eq!(expected.columns, got.columns);
        test_named(store, &expected.from, &got.from);
        test_optional_expr(store, &expected.where_expr, &got.where_expr);
        test_join(store, &expected.join, &got.join);
    }

    fn test_named(store: &ExpressionStore, expected: &Named, got: &Named) {
        assert_eq!(expected.name, got.name);

        test_expression(store, &expected.expr, &got.expr);
    }

    fn test_infix(store: &ExpressionStore, expected: &InfixExpression, got: &InfixExpression) {
        assert_eq!(expected.op, got.op);
        test_expression(store, &expected.left, &got.left);
        test_expression(store, &expected.right, &got.right);
    }

    fn test_join(store: &ExpressionStore, expected: &[Join], got: &[Join]) {
        assert_eq!(expected.len(), got.len());
        for (expected, got) in std::iter::zip(expected.iter(), got.iter()) {
            assert_eq!(expected.join_type, got.join_type);

            test_expression(store, &expected.expr, &got.expr);

            test_optional_expr(store, &expected.on, &got.on);
        }
    }

    fn test_grouped(
        store: &ExpressionStore,
        expected: &GroupedExpression,
        got: &GroupedExpression,
    ) {
        assert_eq!(expected.name, got.name);
        test_expression(store, &expected.inner, &got.inner);
    }

    fn test_optional_expr(
        store: &ExpressionStore,
        expected: &Option<ExpressionIdx>,
        got: &Option<ExpressionIdx>,
    ) {
        match (expected, got) {
            (Some(expected), Some(got)) => test_expression(store, expected, got),
            _ => assert_eq!(expected, got),
        }
    }

    #[test]
    #[timeout(1000)]
    fn test_large() {
        let input = include_str!("./test.sql");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let _output = match parser.parse_program() {
            Ok(out) => out,
            Err(err) => match err.inner {
                ParserError::PeekFailed { expected, got } => {
                    eprintln!("Backtrace: {}", err.backtrace);
                    eprintln!("Failed to parse expected: {expected}");
                    eprintln!("Got: {got:?}");
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

    #[test]
    fn test_precedence() {
        let input = "SELECT * FROM A WHERE 1 + 2 * -3 < 5";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let mut program = parser.parse_program().unwrap();

        let expected = {
            let store = &mut program.store;

            let expr =
                store.add(ExpressionInner::Ident(IdentExpression { ident: "A".into() }).into());

            let where_expr = {
                let left = {
                    let right = {
                        let left = { store.add(ExpressionInner::Int(2.into()).into()) };
                        let right = {
                            let right = store.add(ExpressionInner::Int(3.into()).into());

                            store.add(
                                ExpressionInner::Prefix(crate::ast::PrefixExpression {
                                    op: PrefixOperator::Sub,
                                    right,
                                })
                                .into(),
                            )
                        };
                        store.add(
                            ExpressionInner::Infix(InfixExpression {
                                left,
                                op: InfixOperator::Mul,
                                right,
                            })
                            .into(),
                        )
                    };
                    let left = { store.add(ExpressionInner::Int(1.into()).into()) };
                    store.add(
                        ExpressionInner::Infix(InfixExpression {
                            left,
                            op: InfixOperator::Add,
                            right,
                        })
                        .into(),
                    )
                };
                let right = { store.add(ExpressionInner::Int(IntExpression { int: 5 }).into()) };
                store.add(Expression::from(ExpressionInner::Infix(InfixExpression {
                    left,
                    op: InfixOperator::LT,
                    right,
                })))
            };

            SelectExpression {
                columns: crate::ast::Columns::All,
                from: Named {
                    expr,
                    name: None,
                    ..Default::default()
                },
                where_expr: Some(where_expr),
                join: vec![],
                group: None,
                union: vec![],
                distinct: false,
                ..Default::default()
            }
        };

        println!("Program: {program}");

        let store = &program.store;

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(stmt) = &program.statements[0];

        let ExpressionInner::Select(select) = &store.get_ref(stmt).unwrap().inner else {
            unreachable!()
        };

        test_select(store, &expected, select);
    }
}
