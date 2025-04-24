use std::fmt::Display;

use derive_more::Display;

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

#[derive(Debug, Clone, PartialEq, Display)]
pub enum Expression {
    Grouped(GroupedExpression),
    Select(SelectExpression),
    Infix(InfixExpression),
    Ident(IdentExpression),
}

impl Expression {
    #[cfg(test)]
    /// A helper function for writing tests
    pub(crate) fn ident(str: &str) -> Self {
        Self::Ident(IdentExpression {
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
    pub join: Option<Join>,
}

impl Display for SelectExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SELECT {} FROM {}", self.columns, self.from)?;

        if let Some(w_expr) = &self.where_expr {
            write!(f, " WHERE {}", w_expr)?;
        }

        if let Some(join) = &self.join {
            write!(f, " {}", join)?;
        }

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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Named {
    pub expr: Expression,
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
}

#[derive(Debug, Clone, PartialEq, Display)]
#[display("({left}{op}{right})")]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub op: InfixOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Display)]
pub struct IdentExpression {
    pub ident: String,
}
