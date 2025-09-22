use ecow::EcoString;
use log::{error, info, warn};
use osql_parser::{
    Loc, Span,
    ast::{
        Columns, Expression, ExpressionIdx, ExpressionInner, ExpressionStore, GroupedExpression,
        InfixExpression, InfixOperator, Named, Program, SelectExpression, Statement,
    },
};

use crate::lsp::Position;

struct DefintionGetter<'a> {
    store: &'a ExpressionStore,
}

impl<'a> DefintionGetter<'a> {
    fn get_from_program(program: &'a Program, position: &Position) -> Option<Loc> {
        let out = Self {
            store: &program.store,
        };

        for stmt in &program.statements {
            if let Some(loc) = out.get_statement(stmt, position) {
                return Some(loc);
            }
        }

        None
    }

    fn get_statement(&self, stmt: &Statement, position: &Position) -> Option<Loc> {
        match stmt {
            Statement::Expression(expression_idx) => self.get_expression(expression_idx, position),
        }
    }

    fn get_expression(&self, expr_idx: &ExpressionIdx, position: &Position) -> Option<Loc> {
        let expr = self.store.get_ref(expr_idx)?;
        info!("Cecking expr: {}", expr.inner.as_ref());

        if !within(&expr.span(), position) {
            info!("Not withing span: {}", expr.inner.as_ref());
            return None;
        }

        match &expr.inner {
            ExpressionInner::Grouped(grouped_expression) => {
                if let Some(name) = &grouped_expression.name {
                    info!("In group: {}", name.element.ident);
                    if within(&name.span, position) {
                        return Some(name.span.start);
                    }
                }

                self.get_expression(&grouped_expression.inner, position)
            }
            ExpressionInner::Select(select_expression) => {
                if within(&select_expression.from.span, position) {
                    info!("In from");
                    return self.get_expression(&select_expression.from.expr, position);
                }

                let mut got_col = None;
                if let Columns::Individual(cols) = &select_expression.columns {
                    for col in cols {
                        if got_col.is_some() {
                            break;
                        }
                        if within(&col.span, position) {
                            got_col = self.get_col(&col.expr, position);
                            info!("Got col: {got_col:?}");
                        }
                    }
                }

                if got_col.is_none()
                    && let Some((expr_idx, span)) =
                        &select_expression.where_expr.as_ref().and_then(|expr| {
                            self.store
                                .get_ref(expr)
                                .map(|expr_in| (expr, expr_in.span()))
                        })
                    && within(span, position)
                {
                    got_col = self.get_col(expr_idx, position)
                }

                if got_col.is_none()
                    && let Some((expr_idx, span)) =
                        &select_expression.group.as_ref().and_then(|expr| {
                            self.store
                                .get_ref(&expr.by)
                                .map(|expr_in| (&expr.by, expr_in.span()))
                        })
                    && within(span, position)
                {
                    got_col = self.get_col(expr_idx, position)
                }

                info!("Out: {got_col:?}");

                let out = got_col.and_then(|v| self.handle_select_col(select_expression, v));

                if out.is_some() {
                    return out;
                }

                for join in &select_expression.join {
                    let join = self.get_expression(&join.expr, position);
                    if join.is_some() {
                        return join;
                    }
                }

                for union in &select_expression.union {
                    let union = self.get_expression(&union.expr, position);
                    if union.is_some() {
                        return union;
                    }
                }

                if within(&select_expression.from.span, position) {
                    self.get_expression(&select_expression.from.expr, position)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn handle_select_col(&self, select: &SelectExpression, col: GetCol) -> Option<Loc> {
        info!("Handeling select for col: {col:?}");
        if let (GetCol::Base(base), Columns::Individual(cols)) = (&col, &select.columns) {
            for col in cols {
                if self.is_col(col, base) {
                    return Some(col.span.end);
                }
            }
        }

        match (&col, &select.from) {
            (
                GetCol::Base(base),
                Named {
                    name: Some(name), ..
                },
            ) if base == &name.element.ident => return Some(name.span.end),
            (
                GetCol::Base(base),
                Named {
                    expr, name: None, ..
                },
            ) => match self.store.get_ref(expr) {
                Some(Expression {
                    inner: ExpressionInner::Ident(_),
                    start,
                    ..
                }) => return Some(*start),
                Some(Expression {
                    inner: ExpressionInner::Select(select),
                    start,
                    ..
                }) => match &select.columns {
                    Columns::All => return Some(*start),
                    Columns::Individual(cols) => {
                        info!("Searching cols");
                        for col in cols {
                            if self.is_col(col, base) {
                                return Some(col.span.end);
                            }
                        }
                    }
                },
                _ => {}
            },
            (
                GetCol::Rec {
                    name: col_name,
                    right: inner,
                },
                Named {
                    expr,
                    name: Some(name),
                    ..
                },
            ) if col_name == &name.element.ident => match self.store.get_ref(expr) {
                Some(Expression {
                    inner: ExpressionInner::Ident(_),
                    start,
                    ..
                })
                | Some(Expression {
                    inner:
                        ExpressionInner::Select(SelectExpression {
                            columns: Columns::All,
                            ..
                        }),
                    start,
                    ..
                }) => {
                    return Some(*start);
                }
                Some(Expression {
                    inner:
                        ExpressionInner::Select(SelectExpression {
                            columns: Columns::Individual(cols),
                            ..
                        }),
                    ..
                }) => {
                    if let GetCol::Base(col_name) = inner.as_ref() {
                        for col in cols {
                            if self.is_col(col, col_name) {
                                return Some(col.span.end);
                            }
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }

        for join in &select.join {
            let expr = self.store.get_ref(&join.expr)?;

            info!("Checking join: {:?}", (&col, expr));
            match (&col, expr) {
                (
                    GetCol::Base(base),
                    Expression {
                        inner:
                            ExpressionInner::Named(Named {
                                name: Some(name), ..
                            }),
                        ..
                    },
                )
                | (
                    GetCol::Base(base),
                    Expression {
                        inner:
                            ExpressionInner::Grouped(GroupedExpression {
                                name: Some(name), ..
                            }),
                        ..
                    },
                ) if base == &name.element.ident => {
                    return Some(name.span.start);
                }
                (
                    GetCol::Rec {
                        name: outer_name,
                        right,
                    },
                    Expression {
                        inner:
                            ExpressionInner::Named(Named {
                                name: Some(name),
                                expr,
                                ..
                            }),
                        ..
                    },
                )
                | (
                    GetCol::Rec {
                        name: outer_name,
                        right,
                    },
                    Expression {
                        inner:
                            ExpressionInner::Grouped(GroupedExpression {
                                name: Some(name),
                                inner: expr,
                                ..
                            }),
                        ..
                    },
                ) if outer_name == &name.element.ident => {
                    return self.handle_expr(expr, *right.clone());
                }
                _ => {
                    warn!("Failed to find join: {expr:?}")
                }
            }
        }

        None
    }

    fn handle_expr(&self, expr_idx: &ExpressionIdx, col: GetCol) -> Option<Loc> {
        let expr = self.store.get_ref(expr_idx)?;
        match &expr.inner {
            ExpressionInner::Select(select_expression) => {
                self.handle_select_col(select_expression, col)
            }
            _ => None,
        }
    }

    fn is_col(&self, named: &Named, col_name: &EcoString) -> bool {
        if let Some(name) = &named.name
            && name.element.ident == *col_name
        {
            return true;
        }

        self.is_col_expr(&named.expr, col_name)
    }

    fn is_col_expr(&self, expr_idx: &ExpressionIdx, _col_name: &EcoString) -> bool {
        let Some(expr) = self.store.get_ref(expr_idx) else {
            return false;
        };

        if let Expression {
            inner: ExpressionInner::Infix(InfixExpression { right, .. }),
            ..
        } = expr
        {
            self.is_col_expr(right, _col_name)
        } else {
            false
        }
    }

    fn get_col(&self, expr_idx: &ExpressionIdx, position: &Position) -> Option<GetCol> {
        let expr = self.store.get_ref(expr_idx)?;

        info!("Getting col for: {}", expr.inner.as_ref());
        if !within(&expr.span(), position) {
            return None;
        }

        match &expr.inner {
            ExpressionInner::Grouped(grouped_expression) => {
                self.get_col(&grouped_expression.inner, position)
            }
            ExpressionInner::Infix(infix_expression) => {
                if infix_expression.op == InfixOperator::Period {
                    info!("getting left");
                    let left = self.get_col(&infix_expression.left, position);
                    if left.is_some() {
                        return left;
                    }
                    let right = self.get_col(&infix_expression.right, position)?;
                    let GetCol::Base(left) = self.collect_col(&infix_expression.left)? else {
                        error!("Left is not base");
                        return None;
                    };
                    return Some(GetCol::Rec {
                        name: left,
                        right: right.into(),
                    });
                }

                if let Some(left) = self.get_col(&infix_expression.left, position) {
                    return Some(left);
                }

                self.get_col(&infix_expression.right, position)
            }
            ExpressionInner::Ident(ident_expression) => {
                info!("Got ident: {ident_expression:?}");
                Some(GetCol::Base(ident_expression.ident.clone()))
            }
            ExpressionInner::Int(_) => None,
            ExpressionInner::Case(case_expression) => {
                if let Some(expr) = &case_expression.expr {
                    let expr_cols = self.get_col(expr, position);
                    if expr_cols.is_some() {
                        return expr_cols;
                    }
                }

                let else_expr = self.get_col(&case_expression.else_expr, position);
                if else_expr.is_some() {
                    return else_expr;
                }

                for when in &case_expression.when_exprs {
                    let condition = self.get_col(&when.condition, position);
                    if condition.is_some() {
                        return condition;
                    }
                    let result = self.get_col(&when.result, position);
                    if result.is_some() {
                        return result;
                    }
                }

                None
            }
            ExpressionInner::Prefix(prefix_expression) => {
                self.get_col(&prefix_expression.right, position)
            }
            ExpressionInner::FunctionCall(function_call) => {
                let func = self.get_col(&function_call.func, position);
                if func.is_some() {
                    return func;
                }

                for arg in &function_call.args {
                    let arg = self.get_col(arg, position);
                    if arg.is_some() {
                        return arg;
                    }
                }

                None
            }
            ExpressionInner::Array(array) => {
                for item in &array.arr {
                    let item = self.get_col(item, position);
                    if item.is_some() {
                        return item;
                    }
                }
                None
            }
            ExpressionInner::Named(named) => self.get_col(&named.expr, position),
            ExpressionInner::NullOr(null_or) => {
                let expected = self.get_col(&null_or.expected, position);
                if expected.is_some() {
                    return expected;
                }
                self.get_col(&null_or.alternative, position)
            }
            ExpressionInner::Between(between) => {
                let left = self.get_col(&between.left, position);
                if left.is_some() {
                    return left;
                }
                let lower = self.get_col(&between.lower, position);
                if lower.is_some() {
                    return left;
                }
                self.get_col(&between.upper, position)
            }
            ExpressionInner::NotInfix(not_infix_expression) => {
                let left = self.get_col(&not_infix_expression.left, position);
                if left.is_some() {
                    return left;
                }
                self.get_col(&not_infix_expression.right, position)
            }
            _ => None,
        }
    }

    fn collect_col(&self, expr_idx: &ExpressionIdx) -> Option<GetCol> {
        let expr = self.store.get_ref(expr_idx)?;

        match &expr.inner {
            ExpressionInner::Infix(InfixExpression { left, op, right })
                if op == &InfixOperator::Period =>
            {
                let GetCol::Base(left) = self.collect_col(left)? else {
                    error!("Left is not base");
                    return None;
                };
                let right = self.collect_col(right)?;
                Some(GetCol::Rec {
                    name: left,
                    right: Box::new(right),
                })
            }
            ExpressionInner::Ident(ident_expression) => {
                Some(GetCol::Base(ident_expression.ident.clone()))
            }
            _ => None,
        }
    }
}

pub fn get_definition(program: &Program, position: &Position) -> Option<Loc> {
    DefintionGetter::get_from_program(program, position)
}

fn within(span: &Span, position: &Position) -> bool {
    if span.start.line > position.line {
        return false;
    }
    if span.end.line < position.line {
        return false;
    }
    if span.start.line == position.line && span.start.col > position.character {
        return false;
    }
    if span.end.line == position.line && span.end.col < position.character {
        return false;
    }
    true
}

#[derive(Debug, PartialEq, Clone)]
enum GetCol {
    Base(EcoString),
    Rec { name: EcoString, right: Box<GetCol> },
}

use core::fmt::Debug;
