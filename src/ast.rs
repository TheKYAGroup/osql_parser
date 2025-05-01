use std::fmt::{Debug, Display};

use ambassador::{delegatable_trait, Delegate};
use derive_more::Display;
use uuid::Uuid;

use crate::token::Loc;

macro_rules! write_store {
    ($dst:expr, $store:expr, $value:expr) => {
        FmtWithStore::fmt_with_store(&$value, $dst, $store)
    };
}

#[derive(Clone)]
pub struct Program {
    pub store: ExpressionStore,
    pub statements: Vec<Statement>,
}

impl Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.statements)
    }
}

impl PartialEq for Program {
    fn eq(&self, other: &Self) -> bool {
        self.statements == other.statements
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.statements {
            match stmt {
                Statement::Expression(expression_idx) => {
                    let pexp = PrintExpression {
                        idx: expression_idx,
                        store: &self.store,
                    };
                    writeln!(f, "{};", pexp)?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(ExpressionIdx),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub inner: ExpressionInner,
    pub start: Loc,
    pub end: Loc,
}

impl FmtWithStore for Expression {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        FmtWithStore::fmt_with_store(&self.inner, f, store)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ExpressionIdx {
    uuid: Uuid,
    idx: u32,
}

impl FmtWithStore for ExpressionIdx {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        let Some(expr) = store.get_ref(self) else {
            unreachable!()
        };
        FmtWithStore::fmt_with_store(expr, f, store)
    }
}

#[derive(Clone)]
struct ExpressionWithUuid {
    uuid: Uuid,
    expr: Expression,
}

#[derive(Clone)]
pub struct ExpressionStore {
    inner: Vec<ExpressionWithUuid>,
    unused: Vec<ExpressionIdx>,
}

pub struct PrintExpression<'a> {
    idx: &'a dyn FmtWithStore,
    store: &'a ExpressionStore,
}

impl<'a> PrintExpression<'a> {
    pub fn new(inner: &'a dyn FmtWithStore, store: &'a ExpressionStore) -> Self {
        Self { idx: inner, store }
    }
}

impl Display for PrintExpression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        FmtWithStore::fmt_with_store(self.idx, f, self.store)
    }
}

impl Default for ExpressionStore {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionStore {
    pub fn new() -> Self {
        Self {
            inner: vec![],
            unused: vec![],
        }
    }

    pub fn add(&mut self, expr: Expression) -> ExpressionIdx {
        let uuid = Uuid::new_v4();

        if let Some(id) = self.unused.pop() {
            *self.inner.get_mut(id.idx as usize).unwrap() = ExpressionWithUuid { expr, uuid };
            return ExpressionIdx { uuid, idx: id.idx };
        }

        self.inner.push(ExpressionWithUuid { uuid, expr });
        ExpressionIdx {
            uuid,
            idx: (self.inner.len() - 1) as u32,
        }
    }

    pub fn get_ref<'a>(&'a self, idx: &ExpressionIdx) -> Option<&'a Expression> {
        let thing = self.inner.get(idx.idx as usize)?;
        if thing.uuid == idx.uuid {
            Some(&thing.expr)
        } else {
            None
        }
    }

    pub fn remove(&mut self, idx: ExpressionIdx) -> Option<Expression> {
        let expr = self.inner.get_mut(idx.idx as usize)?;

        expr.uuid = Uuid::new_v4();
        self.unused.push(idx);

        Some(expr.expr.clone())
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

#[derive(Debug, Clone, Delegate, PartialEq)]
#[delegate(FmtWithStore)]
pub enum ExpressionInner {
    Grouped(GroupedExpression),
    Select(SelectExpression),
    Infix(InfixExpression),
    Ident(IdentExpression),
    Int(IntExpression),
    Case(CaseExpression),
    Prefix(PrefixExpression),
    FunctionCall(FunctionCall),
    All(All),
    Array(Array),
    Named(Named),
    NullOr(NullOr),
    Null(Null),
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct All;

impl FmtWithStore for All {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "*")
    }
}

impl From<ExpressionInner> for Expression {
    fn from(val: ExpressionInner) -> Self {
        Expression {
            inner: val,
            start: Default::default(),
            end: Default::default(),
        }
    }
}

impl From<Box<ExpressionInner>> for Box<Expression> {
    fn from(val: Box<ExpressionInner>) -> Self {
        Box::new((*val).into())
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
    pub inner: ExpressionIdx,
    pub name: Option<IdentExpression>,
}

impl FmtWithStore for GroupedExpression {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "(")?;
        self.inner.fmt_with_store(f, store)?;
        write!(f, ")")?;

        if let Some(name) = &self.name {
            write!(f, " {}", name)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SelectExpression {
    pub columns: Columns,
    pub from: Named,
    pub where_expr: Option<ExpressionIdx>,
    pub join: Vec<Join>,
    pub group: Option<GroupBy>,
}

impl FmtWithStore for SelectExpression {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "SELECT ")?;
        write_store!(f, store, self.columns)?;
        write!(f, " FROM ")?;
        write_store!(f, store, self.from)?;

        if let Some(w_expr) = &self.where_expr {
            write!(f, " WHERE: {}", PrintExpression { store, idx: w_expr })?;
        }

        for join in &self.join {
            join.fmt_with_store(f, store)?;
        }

        if let Some(group) = &self.group {
            group.fmt_with_store(f, store)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupBy {
    pub by: ExpressionIdx,
}

impl FmtWithStore for GroupBy {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "GROUP BY ")?;
        self.by.fmt_with_store(f, store)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct When {
    pub condition: ExpressionIdx,
    pub result: ExpressionIdx,
}

impl FmtWithStore for When {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "WHEN ")?;
        self.condition.fmt_with_store(f, store)?;
        write!(f, "THEN ")?;
        self.result.fmt_with_store(f, store)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseExpression {
    pub expr: Option<ExpressionIdx>,
    pub when_exprs: Vec<When>,
    pub else_expr: ExpressionIdx,
}

impl FmtWithStore for CaseExpression {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "CASE")?;

        if let Some(expr) = &self.expr {
            expr.fmt_with_store(f, store)?;
        }

        for when in &self.when_exprs {
            when.fmt_with_store(f, store)?;
        }

        write!(f, " ELSE ")?;

        self.else_expr.fmt_with_store(f, store)
    }
}

#[delegatable_trait]
pub trait FmtWithStore {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result;
}

impl<T> FmtWithStore for T
where
    T: Display,
{
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _store: &ExpressionStore,
    ) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Columns {
    All,
    Individual(Vec<Named>),
}

impl FmtWithStore for Columns {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        match self {
            Columns::All => write!(f, "*"),
            Columns::Individual(nameds) => {
                write!(
                    f,
                    "{}",
                    nameds
                        .iter()
                        .map(|named| { PrintExpression { idx: named, store }.to_string() })
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
    pub expr: ExpressionIdx,
    pub on: Option<ExpressionIdx>,
}

impl FmtWithStore for Join {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "{} JOIN ", self.join_type)?;
        self.expr.fmt_with_store(f, store)?;
        if let Some(on) = &self.on {
            write!(f, " ON ")?;
            on.fmt_with_store(f, store)?;
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
    pub expr: ExpressionIdx,
    pub name: Option<IdentExpression>,
}

impl FmtWithStore for Named {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write_store!(f, store, self.expr)?;

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
    #[display(" IS ")]
    Is,
    #[display(" USING ")]
    Using,
    #[display(" LIKE ")]
    Like,
    #[display(" <> ")]
    NotEq,
    #[display(" BY ")]
    By,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixExpression {
    pub left: ExpressionIdx,
    pub op: InfixOperator,
    pub right: ExpressionIdx,
}

impl FmtWithStore for InfixExpression {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        self.left.fmt_with_store(f, store)?;
        write!(f, "{}", self.op)?;
        self.right.fmt_with_store(f, store)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub func: ExpressionIdx,
    pub args: Vec<ExpressionIdx>,
}

impl FmtWithStore for FunctionCall {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| PrintExpression { idx: arg, store }.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        self.func.fmt_with_store(f, store)?;
        write!(f, "({})", args)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
pub enum PrefixOperator {
    #[display(" - ")]
    Sub,
    #[display(" NOT ")]
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixExpression {
    pub op: PrefixOperator,
    pub right: ExpressionIdx,
}

impl FmtWithStore for PrefixExpression {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "{}", self.op)?;
        self.right.fmt_with_store(f, store)
    }
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
    pub arr: Vec<ExpressionIdx>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NullOr {
    pub expected: ExpressionIdx,
    pub alternative: ExpressionIdx,
}

impl FmtWithStore for NullOr {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        write!(f, "@{{")?;

        self.expected.fmt_with_store(f, store)?;

        write!(f, "}}{{")?;

        self.alternative.fmt_with_store(f, store)?;

        write!(f, "}}")
    }
}

impl FmtWithStore for Array {
    fn fmt_with_store(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        store: &ExpressionStore,
    ) -> std::fmt::Result {
        let thing = self
            .arr
            .iter()
            .map(|expr| PrintExpression { store, idx: expr }.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "({})", thing)
    }
}

#[derive(Debug, Clone, PartialEq, Display)]
#[display("NULL")]
pub struct Null;
