//! Walk a syntax tree without writing the recursion yourself.
//!
//! Implement [`Visitor`] and override the `visit_*` methods for the nodes you care about.
//! Each of them defaults to the matching `walk_*` function, which visits the children of
//! the node in source order. Call that function from your override to continue into the
//! children, before or after your own work, or leave it out to skip them.
//!
//! ```
//! use vbscript::parser::Parser;
//! use vbscript::parser::ast::{Expr, ExprKind};
//! use vbscript::parser::visit::{Visitor, walk_expr, walk_items};
//!
//! /// Collects the names of everything that is called like `name(...)`.
//! #[derive(Default)]
//! struct Calls<'ast>(Vec<&'ast str>);
//!
//! impl<'ast> Visitor<'ast> for Calls<'ast> {
//!     fn visit_expr(&mut self, expr: &'ast Expr) {
//!         if let ExprKind::FnApplication { callee, .. } = &expr.node
//!             && let ExprKind::Ident(name) = &callee.node
//!         {
//!             self.0.push(name);
//!         }
//!         walk_expr(self, expr);
//!     }
//! }
//!
//! let items = Parser::new("x = Foo(Bar(1), 2)").file().unwrap();
//! let mut calls = Calls::default();
//! walk_items(&mut calls, &items);
//! assert_eq!(calls.0, ["Foo", "Bar"]);
//! ```

use crate::parser::ast::{
    Case, DoLoopCheck, DoLoopCondition, Expr, ExprKind, Item, ItemKind, MemberAccess, SetRhs,
    Spanned, Stmt, StmtKind,
};

/// Visits the nodes of a syntax tree, see the [module documentation](self).
///
/// The `'ast` lifetime is the one of the tree, so a visitor can keep references into it.
pub trait Visitor<'ast> {
    fn visit_item(&mut self, item: &'ast Item) {
        walk_item(self, item);
    }

    /// A `Property Get`, `Let` or `Set` of a class.
    fn visit_member_access(&mut self, member_access: &'ast Spanned<MemberAccess>) {
        walk_member_access(self, member_access);
    }

    /// A `Case` of a `Select Case`, with the values it is for and its statements. The
    /// statements of a `Case Else` are visited as statements of the `Select Case`.
    fn visit_case(&mut self, case: &'ast Spanned<Case>) {
        walk_case(self, case);
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr);
    }
}

/// Visits the items of a script, which is what [`Parser::file`](crate::parser::Parser::file)
/// returns.
pub fn walk_items<'ast, V: Visitor<'ast> + ?Sized>(visitor: &mut V, items: &'ast [Item]) {
    for item in items {
        visitor.visit_item(item);
    }
}

/// Visits the statement of an item, or the properties and the methods of a class.
///
/// The properties of a class are visited before its methods, the tree does not keep the
/// order between those.
pub fn walk_item<'ast, V: Visitor<'ast> + ?Sized>(visitor: &mut V, item: &'ast Item) {
    match &item.node {
        ItemKind::Statement(stmt) => visitor.visit_stmt(stmt),
        ItemKind::Class {
            member_accessors,
            methods,
            ..
        } => {
            for member_access in member_accessors {
                visitor.visit_member_access(member_access);
            }
            walk_stmts(visitor, methods);
        }
        ItemKind::OptionExplicit | ItemKind::Const { .. } | ItemKind::Variable { .. } => {}
    }
}

/// Visits the statements of the body of a property.
pub fn walk_member_access<'ast, V: Visitor<'ast> + ?Sized>(
    visitor: &mut V,
    member_access: &'ast Spanned<MemberAccess>,
) {
    walk_stmts(visitor, &member_access.body);
}

/// Visits the values a case is for, then its statements.
pub fn walk_case<'ast, V: Visitor<'ast> + ?Sized>(visitor: &mut V, case: &'ast Spanned<Case>) {
    walk_exprs(visitor, &case.tests);
    walk_stmts(visitor, &case.body);
}

pub fn walk_stmts<'ast, V: Visitor<'ast> + ?Sized>(visitor: &mut V, stmts: &'ast [Stmt]) {
    for stmt in stmts {
        visitor.visit_stmt(stmt);
    }
}

/// Visits the expressions and the nested statements of a statement, in source order.
pub fn walk_stmt<'ast, V: Visitor<'ast> + ?Sized>(visitor: &mut V, stmt: &'ast Stmt) {
    match &stmt.node {
        StmtKind::ReDim { vars, .. } => {
            for var in vars {
                walk_exprs(visitor, &var.bounds);
            }
        }
        StmtKind::Set { var, rhs } => {
            visitor.visit_expr(&var.0);
            match rhs {
                SetRhs::Expr(expr) => visitor.visit_expr(expr),
                SetRhs::Nothing => {}
            }
        }
        StmtKind::Assignment { full_ident, value } => {
            visitor.visit_expr(&full_ident.0);
            visitor.visit_expr(value);
        }
        StmtKind::IfStmt {
            condition,
            body,
            elseif_statements,
            else_stmt,
        } => {
            visitor.visit_expr(condition);
            walk_stmts(visitor, body);
            for (condition, body) in elseif_statements {
                visitor.visit_expr(condition);
                walk_stmts(visitor, body);
            }
            if let Some(body) = else_stmt {
                walk_stmts(visitor, body);
            }
        }
        StmtKind::WhileStmt { condition, body } => {
            visitor.visit_expr(condition);
            walk_stmts(visitor, body);
        }
        StmtKind::ForStmt {
            start,
            end,
            step,
            body,
            ..
        } => {
            visitor.visit_expr(start);
            visitor.visit_expr(end);
            if let Some(step) = step {
                visitor.visit_expr(step);
            }
            walk_stmts(visitor, body);
        }
        StmtKind::ForEachStmt { group, body, .. } => {
            visitor.visit_expr(group);
            walk_stmts(visitor, body);
        }
        StmtKind::DoLoop { check, body } => {
            let (DoLoopCondition::While(condition) | DoLoopCondition::Until(condition)) =
                match check {
                    DoLoopCheck::Pre(condition) | DoLoopCheck::Post(condition) => condition,
                    DoLoopCheck::None => return walk_stmts(visitor, body),
                };
            // the condition of `Do ... Loop While x` comes after the body
            if matches!(check, DoLoopCheck::Pre(_)) {
                visitor.visit_expr(condition);
                walk_stmts(visitor, body);
            } else {
                walk_stmts(visitor, body);
                visitor.visit_expr(condition);
            }
        }
        StmtKind::SelectCase {
            test_expr,
            cases,
            else_stmt,
        } => {
            visitor.visit_expr(test_expr);
            for case in cases {
                visitor.visit_case(case);
            }
            if let Some(body) = else_stmt {
                walk_stmts(visitor, body);
            }
        }
        StmtKind::SubCall { fn_name, args } => {
            visitor.visit_expr(&fn_name.0);
            walk_exprs(visitor, args.iter().flatten());
        }
        StmtKind::Call(ident) => visitor.visit_expr(&ident.0),
        StmtKind::With { object, body } => {
            visitor.visit_expr(&object.0);
            walk_stmts(visitor, body);
        }
        StmtKind::Sub { body, .. } | StmtKind::Function { body, .. } => {
            walk_stmts(visitor, body);
        }
        StmtKind::Dim { .. }
        | StmtKind::Const(_)
        | StmtKind::Stop
        | StmtKind::ExitDo
        | StmtKind::ExitFor
        | StmtKind::ExitFunction
        | StmtKind::ExitProperty
        | StmtKind::ExitSub
        | StmtKind::OnError { .. } => {}
    }
}

/// Visits the expressions an expression is made of, in source order.
pub fn walk_expr<'ast, V: Visitor<'ast> + ?Sized>(visitor: &mut V, expr: &'ast Expr) {
    match &expr.node {
        ExprKind::PrefixOp { expr, .. } | ExprKind::Paren(expr) => visitor.visit_expr(expr),
        ExprKind::InfixOp { lhs, rhs, .. } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        ExprKind::FnApplication { callee, args } => {
            visitor.visit_expr(callee);
            walk_exprs(visitor, args.iter().flatten());
        }
        ExprKind::MemberExpression { base, .. } => visitor.visit_expr(base),
        ExprKind::Literal(_) | ExprKind::Ident(_) | ExprKind::New(_) | ExprKind::WithScoped => {}
    }
}

fn walk_exprs<'ast, V: Visitor<'ast> + ?Sized>(
    visitor: &mut V,
    exprs: impl IntoIterator<Item = &'ast Expr>,
) {
    for expr in exprs {
        visitor.visit_expr(expr);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Parser;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    /// The identifiers of a script, in the order they are visited.
    #[derive(Default)]
    struct Idents<'ast>(Vec<&'ast str>);

    impl<'ast> Visitor<'ast> for Idents<'ast> {
        fn visit_expr(&mut self, expr: &'ast Expr) {
            if let ExprKind::Ident(name) = &expr.node {
                self.0.push(name);
            }
            walk_expr(self, expr);
        }
    }

    fn idents(input: &str) -> Vec<String> {
        let items = Parser::new(input).file().unwrap();
        let mut idents = Idents::default();
        walk_items(&mut idents, &items);
        idents.0.into_iter().map(String::from).collect()
    }

    #[test]
    fn visits_every_kind_of_statement_in_source_order() {
        let input = indoc! {"
            Dim a(1)
            ReDim c(b2)
            Set d = e
            f = (g + -h).i(j, , k)
            If l Then
                m
            ElseIf n Then
                o
            Else
                p
            End If
            While q
                r
            Wend
            For s = t To u Step v
                w
            Next
            For Each x In y
                z
            Next
            Do While aa
                ab
            Loop
            Do
                ac
            Loop Until ad
            Select Case ae
                Case af, ag
                    ah
                Case Else
                    ai
            End Select
            aj ak, al
            Call am(an)
            With ao
                .ap = aq
            End With
            Sub ar()
                Stop
                at
            End Sub
        "};
        let expected = "b2 d e f g h j k l m n o p q r t u v w y z aa ab ac ad ae af ag ah ai \
                        aj ak al am an ao aq at";
        assert_eq!(idents(input).join(" "), expected);
    }

    #[test]
    fn visits_the_properties_and_the_methods_of_a_class() {
        let input = indoc! {"
            Class Foo
                Public Property Get Bar()
                    Bar = inGet
                End Property
                Public Property Let Bar(value)
                    inLet = value
                End Property
                Public Sub Baz()
                    inSub
                End Sub
            End Class
        "};
        assert_eq!(idents(input), ["Bar", "inGet", "inLet", "value", "inSub"]);
    }

    #[test]
    fn work_before_and_after_the_children() {
        /// The procedure each call is made from.
        #[derive(Default)]
        struct Calls<'ast> {
            procedures: Vec<&'ast str>,
            calls: Vec<String>,
        }

        impl<'ast> Visitor<'ast> for Calls<'ast> {
            fn visit_stmt(&mut self, stmt: &'ast Stmt) {
                match &stmt.node {
                    StmtKind::Sub { name, .. } | StmtKind::Function { name, .. } => {
                        self.procedures.push(name);
                        walk_stmt(self, stmt);
                        self.procedures.pop();
                    }
                    StmtKind::SubCall { fn_name, .. } => {
                        let procedure = self.procedures.last().copied().unwrap_or("<script>");
                        self.calls.push(format!("{procedure}: {fn_name}"));
                        walk_stmt(self, stmt);
                    }
                    _ => walk_stmt(self, stmt),
                }
            }
        }

        let input = indoc! {"
            One
            Sub Outer()
                Two
                If x Then Three
            End Sub
            Four
        "};
        let items = Parser::new(input).file().unwrap();
        let mut calls = Calls::default();
        walk_items(&mut calls, &items);
        assert_eq!(
            calls.calls,
            [
                "<script>: One",
                "Outer: Two",
                "Outer: Three",
                "<script>: Four"
            ]
        );
    }

    #[test]
    fn visits_the_cases_of_a_select_case() {
        /// How many values each case is for.
        #[derive(Default)]
        struct Cases(Vec<usize>);

        impl<'ast> Visitor<'ast> for Cases {
            fn visit_case(&mut self, case: &'ast Spanned<Case>) {
                self.0.push(case.tests.len());
                walk_case(self, case);
            }
        }

        let input = "Select Case x\nCase 1, 2\nCase 3\nSelect Case y\nCase 4, 5, 6\nEnd Select\nCase Else\nEnd Select\n";
        let items = Parser::new(input).file().unwrap();
        let mut cases = Cases::default();
        walk_items(&mut cases, &items);
        // the case of the nested select case is visited as part of the second case
        assert_eq!(cases.0, [2, 1, 3]);
    }

    #[test]
    fn children_are_skipped_without_the_walk() {
        struct TopLevelOnly(usize);

        impl<'ast> Visitor<'ast> for TopLevelOnly {
            fn visit_stmt(&mut self, _stmt: &'ast Stmt) {
                self.0 += 1;
            }
        }

        let items = Parser::new("If a Then\nb\nc\nEnd If\nd\n").file().unwrap();
        let mut count = TopLevelOnly(0);
        walk_items(&mut count, &items);
        assert_eq!(count.0, 2);
    }
}
