use std::fmt::Display;

use derive_more::Display;

use crate::token::Loc;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            writeln!(f, "{};", stmt)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum Statement {
    Expression(Expression),
}

#[derive(Debug, Clone, Display)]
#[display("{inner}")]
pub struct Expression {
    pub inner: ExpressionInner,
    pub start: Loc,
    pub end: Loc,
}

pub struct ExpressionIdx(u32);

pub struct ExpressionStore {
    inner: Vec<Expression>,
}

impl ExpressionStore {
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn add_expr(&mut self, expr: Expression) -> ExpressionIdx {
        self.inner.push(expr);
        ExpressionIdx((self.inner.len() - 1) as u32)
    }

    pub fn get_expr<'a>(&'a self, idx: ExpressionIdx) -> &'a Expression {
        &self.inner[idx.0 as usize]
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum ExpressionInner {
    Grouped(GroupedExpression),
    Select(SelectExpression),
    Infix(InfixExpression),
    Ident(IdentExpression),
    Int(IntExpression),
    Case(CaseExpression),
    Prefix(PrefixExpression),
    FunctionCall(FunctionCall),
    #[display("*")]
    All,
    Array(Array),
    Named(Named),
}

impl Into<Expression> for ExpressionInner {
    fn into(self) -> Expression {
        Expression {
            inner: self,
            start: Default::default(),
            end: Default::default(),
        }
    }
}

impl Into<Box<Expression>> for Box<ExpressionInner> {
    fn into(self) -> Box<Expression> {
        Box::new((*self).into())
    }
}

impl ExpressionInner {
    #[cfg(test)]
    /// A helper function for writing tests
    pub(crate) fn ident(str: &str) -> Self {
        ExpressionInner::Ident(IdentExpression {
            ident: str.to_string(),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedExpression {
    pub inner: Box<Expression>,
    pub name: Option<IdentExpression>,
}

impl Display for GroupedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.inner,)?;

        if let Some(name) = &self.name {
            write!(f, " {}", name)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectExpression {
    pub columns: Columns,
    pub from: Box<Named>,
    pub where_expr: Option<Box<Expression>>,
    pub join: Vec<Join>,
}

impl Display for SelectExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SELECT {} FROM {}", self.columns, self.from)?;

        if let Some(w_expr) = &self.where_expr {
            write!(f, " WHERE {}", w_expr)?;
        }

        for join in &self.join {
            write!(f, " {}", join)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
#[display("WHEN {condition} THEN {result}")]
pub struct When {
    pub condition: Box<Expression>,
    pub result: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseExpression {
    pub expr: Option<Box<Expression>>,
    pub when_exprs: Vec<When>,
    pub else_expr: Box<Expression>,
}

impl Display for CaseExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CASE")?;

        if let Some(expr) = &self.expr {
            write!(f, " {}", expr)?;
        }

        for when in &self.when_exprs {
            write!(f, " {}", when)?;
        }

        write!(f, " ELSE {}", self.else_expr)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Columns {
    All,
    Individual(Vec<Named>),
}

impl Display for Columns {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Columns::All => write!(f, "*"),
            Columns::Individual(nameds) => {
                write!(
                    f,
                    "{}",
                    nameds
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Join {
    pub join_type: JoinType,
    pub expr: Box<Expression>,
    pub on: Option<Box<Expression>>,
}

impl Display for Join {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} JOIN ", self.join_type)?;
        write!(f, "{}", self.expr)?;
        if let Some(on) = &self.on {
            write!(f, " ON {}", on)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum JoinType {
    #[display("INNER")]
    Inner,
    #[display("LEFT")]
    Left,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Named {
    pub expr: Box<Expression>,
    pub name: Option<IdentExpression>,
}

impl Display for Named {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)?;

        if let Some(name) = &self.name {
            write!(f, " {}", name)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum InfixOperator {
    #[display(".")]
    Period,
    #[display(" = ")]
    Eq,
    #[display(" IN ")]
    In,
    #[display(" - ")]
    Sub,
    #[display(" / ")]
    Div,
    #[display(" * ")]
    Mul,
    #[display(" < ")]
    LT,
    #[display(" > ")]
    GT,
    #[display(" <= ")]
    LTEq,
    #[display(" >= ")]
    GTEq,
    #[display(" AND ")]
    And,
    #[display(" OR ")]
    Or,
}

#[derive(Debug, Clone, PartialEq, Display)]
#[display("({left}{op}{right})")]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub op: InfixOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub func: Box<Expression>,
    pub args: Vec<Expression>,
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| arg.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}({})", self.func, args)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum PrefixOperator {
    #[display(" - ")]
    Sub,
}

#[derive(Debug, Clone, PartialEq, Display)]
#[display("({op}{right})")]
pub struct PrefixExpression {
    pub op: PrefixOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Display)]
pub struct IdentExpression {
    pub ident: String,
}

#[derive(Debug, Clone, PartialEq, Display)]
pub struct IntExpression {
    pub int: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub arr: Vec<Expression>,
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let thing = self
            .arr
            .iter()
            .map(|expr| expr.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "({})", thing)
    }
}
