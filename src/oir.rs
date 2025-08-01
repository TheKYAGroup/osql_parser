use std::{backtrace::Backtrace, collections::HashMap};

use ecow::EcoString;
use thiserror::Error;

use crate::{
    ast::{
        self, Expression, ExpressionIdx, ExpressionStore, GroupedExpression, IdentExpression,
        InfixExpression, Program, SelectExpression, Statement,
    },
    token::Loc,
};

#[derive(Debug)]
pub enum InnerOir {
    BaseTable(EcoString),
    Select(Select),
    Named(Named),
    Ident(EcoString),
    BinaryOP(BinaryOp),
    FunctionCall(FunctionCall),
}

#[derive(Debug)]
pub struct FunctionCall {
    name: EcoString,
    args: Vec<Oir>,
}

#[derive(Debug)]
pub enum BinaryOpKind {
    Equality,
    Access,
}

#[derive(Debug)]
pub struct BinaryOp {
    left: Box<Oir>,
    right: Box<Oir>,
    kind: BinaryOpKind,
}

#[derive(Debug)]
pub struct Named {
    name: EcoString,
    inner: Box<Oir>,
}

#[derive(Debug)]
pub struct Select {
    pub columns: HashMap<EcoString, Oir>,
}

#[derive(Debug)]
pub struct Oir {
    pub inner: InnerOir,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

pub struct OirCompiler<'a> {
    store: &'a ExpressionStore,
    column_store: Option<&'a Vec<ObjectColumns>>,
}

#[derive(Debug, Error, PartialEq)]
pub enum ErrorType {
    #[error("Unkown Column")]
    UnkownColumn,
    #[error("Function is not an ident")]
    FucntionNotIdent,
}

#[derive(Debug, Error)]
#[error("{inner}")]
pub struct Error {
    #[source]
    pub inner: ErrorType,
    pub span: Span,
    #[backtrace]
    pub backtrace: Box<Backtrace>,
}

impl Error {
    fn new(inner: ErrorType, span: Span) -> Self {
        Self {
            inner,
            span,
            backtrace: Box::new(Backtrace::capture()),
        }
    }
}

type CompilerResult = Result<Oir, Error>;

impl<'a> OirCompiler<'a> {
    pub fn new(store: &'a ExpressionStore) -> Self {
        Self {
            store,
            column_store: None,
        }
    }

    fn extend<'b>(&self, column_store: &'a Vec<ObjectColumns>) -> Self {
        Self {
            store: &self.store,
            column_store: Some(column_store),
        }
    }

    pub fn compile_program(program: &'a Program) -> Vec<CompilerResult> {
        let comp = Self::new(&program.store);
        program
            .statements
            .iter()
            .map(|s| comp.compile_statement(s))
            .collect()
    }

    fn compile_statement(&self, stmt: &Statement) -> CompilerResult {
        match stmt {
            Statement::Expression(expr) => self.compile_expr_idx(expr),
        }
    }

    fn compile_expr_idx(&self, idx: &ExpressionIdx) -> CompilerResult {
        let expr = self.store.get_ref(idx).expect("Cannot get expr from store");
        self.compile_expr(expr)
    }

    fn compile_expr(&self, expr: &Expression) -> CompilerResult {
        match &expr.inner {
            crate::ast::ExpressionInner::Grouped(grouped_expression) => {
                self.compile_grouped(grouped_expression, expr.span())
            }
            crate::ast::ExpressionInner::Select(select_expression) => {
                self.compile_select(select_expression, expr.span())
            }
            crate::ast::ExpressionInner::Infix(infix_expression) => self.compile_infix(
                infix_expression,
                Span {
                    start: expr.start,
                    end: expr.end,
                },
            ),
            crate::ast::ExpressionInner::Ident(ident_expression) => {
                Ok(self.compile_ident(ident_expression, expr.span()))
            }
            crate::ast::ExpressionInner::Int(_int_expression) => todo!(),
            crate::ast::ExpressionInner::Case(_case_expression) => todo!(),
            crate::ast::ExpressionInner::Prefix(_prefix_expression) => todo!(),
            crate::ast::ExpressionInner::FunctionCall(function_call) => {
                self.compile_function_call(function_call, expr.span())
            }
            crate::ast::ExpressionInner::All(_all) => todo!(),
            crate::ast::ExpressionInner::Array(_array) => todo!(),
            crate::ast::ExpressionInner::Named(_named) => todo!(),
            crate::ast::ExpressionInner::NullOr(_null_or) => todo!(),
            crate::ast::ExpressionInner::Null(_null) => todo!(),
            crate::ast::ExpressionInner::Between(_between) => todo!(),
            crate::ast::ExpressionInner::NotInfix(_not_infix_expression) => todo!(),
        }
    }

    fn compile_function_call(
        &self,
        functioncall: &ast::FunctionCall,
        span: Span,
    ) -> CompilerResult {
        let name = self.compile_expr_idx(&functioncall.func)?;
        let InnerOir::Ident(ident) = &name.inner else {
            return Err(Error::new(ErrorType::FucntionNotIdent, name.span));
        };

        let mut args = Vec::new();

        for arg in &functioncall.args {
            args.push(self.compile_expr_idx(arg)?);
        }

        Ok(Oir {
            inner: InnerOir::FunctionCall(FunctionCall {
                name: ident.clone(),
                args,
            }),
            span,
        })
    }

    fn compile_grouped(&self, grouped: &GroupedExpression, span: Span) -> CompilerResult {
        let inner = self.compile_expr_idx(&grouped.inner)?;
        if let Some(name) = &grouped.name {
            Ok(Oir {
                span,
                inner: InnerOir::Named(Named {
                    name: name.ident.clone(),
                    inner: Box::new(inner),
                }),
            })
        } else {
            Ok(inner)
        }
    }

    fn compile_ident(&self, ident: &IdentExpression, span: Span) -> Oir {
        let out = Oir {
            span,
            inner: InnerOir::Ident(ident.ident.clone()),
        };

        out
    }

    fn compile_infix(&self, infix: &InfixExpression, span: Span) -> CompilerResult {
        let left = Box::new(self.compile_expr_idx(&infix.left)?);
        let right = Box::new(self.compile_expr_idx(&infix.right)?);

        match infix.op {
            ast::InfixOperator::Period => {
                let out = Oir {
                    inner: InnerOir::BinaryOP(BinaryOp {
                        left: left,
                        right: right,
                        kind: BinaryOpKind::Access,
                    }),
                    span,
                };

                let mut found = false;
                for column in self
                    .column_store
                    .ok_or(Error::new(ErrorType::UnkownColumn, out.span.clone()))?
                {
                    println!("Checking in: {:?}", column);
                    if column.is_one(&out) {
                        println!("Found: {out:?} in column: {column:?}");
                        found = true;
                    }
                    if found {
                        break;
                    }
                }

                if !found {
                    return Err(Error::new(ErrorType::UnkownColumn, out.span));
                }

                Ok(out)
            }
            ast::InfixOperator::Eq => Ok(Oir {
                inner: InnerOir::BinaryOP(BinaryOp {
                    left,
                    right,
                    kind: BinaryOpKind::Equality,
                }),
                span,
            }),
            ast::InfixOperator::Sub => todo!(),
            ast::InfixOperator::Div => todo!(),
            ast::InfixOperator::Mul => todo!(),
            ast::InfixOperator::Add => todo!(),
            ast::InfixOperator::LT => todo!(),
            ast::InfixOperator::GT => todo!(),
            ast::InfixOperator::LTEq => todo!(),
            ast::InfixOperator::GTEq => todo!(),
            ast::InfixOperator::And => todo!(),
            ast::InfixOperator::Or => todo!(),
            ast::InfixOperator::Is => todo!(),
            ast::InfixOperator::Using => todo!(),
            ast::InfixOperator::UnEq => todo!(),
            ast::InfixOperator::NotEq => todo!(),
            ast::InfixOperator::By => todo!(),
            ast::InfixOperator::JoinStrings => todo!(),
        }
    }

    fn compile_select(&self, select: &SelectExpression, span: Span) -> CompilerResult {
        let from = self.compile_named(&select.from)?;

        println!("Getting cols from: {:?}", from);
        let mut check_columns = vec![from.get_object_columns().unwrap()];

        for join in &select.join {
            let expr = self.compile_expr_idx(&join.expr)?;
            check_columns.push(expr.get_object_columns().unwrap());
        }

        let new_comp = self.extend(&check_columns);

        for join in &select.join {
            if let Some(on) = &join.on {
                new_comp.compile_expr_idx(&on)?;
            }
        }

        let mut columns = Vec::new();

        match &select.columns {
            ast::Columns::All => todo!(),
            ast::Columns::Individual(nameds) => {
                for named in nameds {
                    let expr = new_comp.compile_expr_idx(&named.expr)?;

                    let name = match named.name.as_ref().map(|i| i.ident.clone()) {
                        Some(val) => val,
                        None => {
                            let mut found = false;
                            for column_store in &check_columns {
                                if column_store.is_one(&expr) {
                                    found = true;
                                }
                                if found {
                                    break;
                                }
                            }

                            if !found {
                                return Err(Error::new(
                                    ErrorType::UnkownColumn,
                                    named.span.clone(),
                                ));
                            }

                            let name = expr.get_end_name().unwrap();

                            name
                        }
                    };

                    columns.push((name, expr));
                }
            }
        }

        let columns: HashMap<EcoString, Oir> = HashMap::from_iter(columns);

        Ok(Oir {
            inner: InnerOir::Select(Select { columns }),
            span,
        })
    }

    fn compile_named(&self, named: &ast::Named) -> CompilerResult {
        let inner = self.compile_expr_idx(&named.expr)?;

        Ok(if let Some(name) = &named.name {
            Oir {
                span: named.span.clone(),
                inner: InnerOir::Named(Named {
                    name: name.ident.clone(),
                    inner: Box::new(inner),
                }),
            }
        } else {
            inner
        })
    }
}

#[derive(Debug, PartialEq)]
enum ColumnTypes {
    Base,
    Normal(Vec<EcoString>),
}

#[derive(Debug)]
struct ObjectColumns {
    name: Option<EcoString>,
    columns: ColumnTypes,
}

impl ObjectColumns {
    fn is_one(&self, oir: &Oir) -> bool {
        println!("Checking: {:?}", oir);
        match &oir.inner {
            InnerOir::BaseTable(_eco_string) => todo!(),
            InnerOir::Select(_select) => todo!(),
            InnerOir::Named(_named) => todo!(),
            InnerOir::Ident(eco_string) => {
                if self.name.is_some() {
                    return false;
                }

                self.check_cols(eco_string)
            }
            InnerOir::BinaryOP(BinaryOp { left, right, kind }) => match kind {
                BinaryOpKind::Access => {
                    if self.columns == ColumnTypes::Base && self.name.is_none() {
                        return true;
                    }
                    let InnerOir::Ident(left) = &left.inner else {
                        if self.columns == ColumnTypes::Base {
                            return self.is_one(left.as_ref());
                        }
                        return false;
                    };

                    let InnerOir::Ident(right) = &right.inner else {
                        return false;
                    };

                    let Some(name) = &self.name else {
                        return false;
                    };

                    if name != left {
                        return false;
                    }

                    self.check_cols(right)
                }
                BinaryOpKind::Equality => todo!(),
            },
            InnerOir::FunctionCall(_) => todo!(),
        }
    }

    fn check_cols(&self, to_check: &EcoString) -> bool {
        match &self.columns {
            ColumnTypes::Base => true,
            ColumnTypes::Normal(eco_strings) => {
                for col in eco_strings {
                    if col == to_check {
                        return true;
                    }
                }
                false
            }
        }
    }
}

impl Oir {
    fn get_object_columns(&self) -> Option<ObjectColumns> {
        match &self.inner {
            InnerOir::BaseTable(_) => Some(ObjectColumns {
                name: None,
                columns: ColumnTypes::Base,
            }),
            InnerOir::Select(select) => Some(ObjectColumns {
                name: None,
                columns: ColumnTypes::Normal(select.columns.keys().map(Clone::clone).collect()),
            }),
            InnerOir::Named(named) => {
                let ObjectColumns { columns, .. } = named.inner.get_object_columns()?;
                Some(ObjectColumns {
                    name: Some(named.name.clone()),
                    columns,
                })
            }
            InnerOir::Ident(_) => Some(ObjectColumns {
                name: None,
                columns: ColumnTypes::Base,
            }),
            _ => None,
        }
    }

    fn get_end_name(&self) -> Option<EcoString> {
        match &self.inner {
            InnerOir::BaseTable(_) => None,
            InnerOir::Select(_) => None,
            InnerOir::Named(_) => None,
            InnerOir::Ident(eco_string) => Some(eco_string.clone()),
            InnerOir::BinaryOP(BinaryOp { right, kind, .. }) => match kind {
                BinaryOpKind::Equality => None,
                BinaryOpKind::Access => right.get_end_name(),
            },
            InnerOir::FunctionCall(_) => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_select() {
        let input = "SELECT A, B, C D FROM Table";
        let program = crate::parse(input.into()).unwrap();

        let out = OirCompiler::compile_program(&program);

        let oir = out[0].as_ref().unwrap();

        let InnerOir::Select(select) = &oir.inner else {
            panic!("Not select: {:?}", oir);
        };

        select.columns.get("A").unwrap();
        select.columns.get("B").unwrap();
        select.columns.get("D").unwrap();
        assert!(select.columns.get("c").is_none());
    }

    #[test]
    fn named_select() {
        let input = "SELECT T.A, T.B, T.C D FROM Table T";
        let program = crate::parse(input.into()).unwrap();

        let out = OirCompiler::compile_program(&program);

        let oir = out[0].as_ref().unwrap();

        let InnerOir::Select(select) = &oir.inner else {
            panic!("Not select: {:?}", oir);
        };

        select.columns.get("A").unwrap();
        select.columns.get("B").unwrap();
        select.columns.get("D").unwrap();
        assert!(select.columns.get("c").is_none());
    }

    #[test]
    fn named_select_error() {
        let input = "SELECT A, T.B, T.C D FROM Table T";
        let program = crate::parse(input.into()).unwrap();

        let span_start = "SELECT ".len();
        let span_end = span_start + 1;
        let span = Span {
            start: Loc {
                line: 0,
                col: span_start,
                idx: span_start,
            },
            end: Loc {
                line: 0,
                col: span_end,
                idx: span_end,
            },
        };

        assert_eq!(&input[span_start..span_end], "A");

        let out = OirCompiler::compile_program(&program);

        let err = out[0].as_ref().unwrap_err();

        assert_eq!(ErrorType::UnkownColumn, err.inner);

        assert_eq!(span, err.span);
    }

    #[test]
    fn inner_select() {
        let input = "SELECT A, B, C D FROM (
            SELECT A, B, C FROM Table
        )";
        let program = crate::parse(input.into()).unwrap();

        let out = OirCompiler::compile_program(&program);

        let oir = out[0].as_ref().unwrap();

        let InnerOir::Select(select) = &oir.inner else {
            panic!("Not select: {:?}", oir);
        };

        select.columns.get("A").unwrap();
        select.columns.get("B").unwrap();
        select.columns.get("D").unwrap();
        assert!(select.columns.get("c").is_none());
    }

    #[test]
    fn inner_select_error() {
        let input = "SELECT T.A, T.B, T.C D FROM (
            SELECT B, C FROM Table
        ) T";
        let program = crate::parse(input.into()).unwrap();

        let span_start = "SELECT ".len();
        let span_end = span_start + 3;
        let span = Span {
            start: Loc {
                line: 0,
                col: span_start,
                idx: span_start,
            },
            end: Loc {
                line: 0,
                col: span_end,
                idx: span_end,
            },
        };

        assert_eq!(&input[span_start..span_end], "T.A");

        let out = OirCompiler::compile_program(&program);

        let err = out[0].as_ref().unwrap_err();

        assert_eq!(ErrorType::UnkownColumn, err.inner);

        assert_eq!(span, err.span);
    }

    #[test]
    fn layered_select() {
        let input = "SELECT T.A.E, T.B, T.C D FROM Table T";
        let program = crate::parse(input.into()).unwrap();

        let out = OirCompiler::compile_program(&program);

        let oir = out[0].as_ref().unwrap();

        let InnerOir::Select(select) = &oir.inner else {
            panic!("Not select: {:?}", oir);
        };

        select.columns.get("E").unwrap();
        select.columns.get("B").unwrap();
        select.columns.get("D").unwrap();
        assert!(select.columns.get("C").is_none());
    }

    #[test]
    fn join_select() {
        let input = "SELECT T.A, T.B, T2.Col1 D, T2.Col2 FROM Table T
            INNER JOIN (
                SELECT Col1, Col2, Col3 FROM Table2
            ) T2 ON T.A = T2.Col1
            ";
        let program = crate::parse(input.into()).unwrap();

        let out = OirCompiler::compile_program(&program);

        let oir = out[0].as_ref().unwrap();

        let InnerOir::Select(select) = &oir.inner else {
            panic!("Not select: {:?}", oir);
        };

        select.columns.get("A").unwrap();
        select.columns.get("B").unwrap();
        select.columns.get("D").unwrap();
        select.columns.get("Col2").unwrap();
        assert!(select.columns.get("c").is_none());
    }

    #[test]
    fn on_select_error() {
        let input = "SELECT T.A, T.B, T.C D FROM Table T
            INNER JOIN (
                SELECT Col1 FROM Table2
            ) T2 ON T.A = T2.Col2";
        let program = crate::parse(input.into()).unwrap();

        let err_place = "T2.Col2";
        let col_place = "            ) T2 ON T.A = ".len();
        let span_start = input.find(err_place).unwrap();
        let span_end = span_start + err_place.len();
        let col_end = col_place + err_place.len();
        let span = Span {
            start: Loc {
                line: 3,
                col: col_place,
                idx: span_start,
            },
            end: Loc {
                line: 3,
                col: col_end,
                idx: span_end,
            },
        };

        assert_eq!(&input[span_start..span_end], err_place);

        let out = OirCompiler::compile_program(&program);

        let err = out[0].as_ref().unwrap_err();

        assert_eq!(ErrorType::UnkownColumn, err.inner);

        assert_eq!(span, err.span);
    }

    #[test]
    fn sum_select() {
        let input = "SELECT A, B, SUM(C) D FROM (
            SELECT A, B, C FROM Table
        )";
        let program = crate::parse(input.into()).unwrap();

        let out = OirCompiler::compile_program(&program);

        let oir = out[0].as_ref().unwrap();

        let InnerOir::Select(select) = &oir.inner else {
            panic!("Not select: {:?}", oir);
        };

        select.columns.get("A").unwrap();
        select.columns.get("B").unwrap();
        select.columns.get("D").unwrap();
        assert!(select.columns.get("c").is_none());
    }

    #[test]
    fn full_test() {
        let input = include_str!("test.sql");

        let program = crate::parse(input.into()).unwrap();

        let out = OirCompiler::compile_program(&program);

        match out[0].as_ref() {
            Ok(_) => {}
            Err(err) => {
                print!("Failed with {:?} at {:?}", err.inner, err.span);
                panic!("backtrace: {}", err.backtrace);
            }
        }
    }
}
