//! Lexes and parses whatever the fuzzer comes up with. A parse error is expected, a panic
//! or a stack overflow is a bug, and so is a tree that does not fit its source:
//!
//! - the text of the tokens of the lexer put together is the input again
//! - the span of every node lies within the source and within its parent
//! - the source under the span of a name is that name
//!
//! Run it with the scripts of the test corpus as a start. The first directory is where the
//! fuzzer keeps what it finds, which has to exist:
//! `mkdir -p fuzz/corpus/parse`
//! `cargo +nightly fuzz run parse fuzz/corpus/parse testscripts/wine-vbscript`
#![no_main]

use libfuzzer_sys::fuzz_target;
use vbscript::lexer::{Lexer, Span};
use vbscript::parser::Parser;
use vbscript::parser::ast::{
    Argument, Expr, ExprKind, Item, ItemKind, MemberAccess, Name, Stmt, StmtKind,
};
use vbscript::parser::visit::{
    Visitor, walk_expr, walk_item, walk_items, walk_member_access, walk_stmt,
};

struct Check<'a> {
    input: &'a str,
    parents: Vec<Span>,
}

impl Check<'_> {
    fn within_parent(&self, span: Span) {
        let parent = *self.parents.last().unwrap();
        assert!(span.start <= span.end, "{span:?} ends before it starts");
        assert!(
            span.start >= parent.start && span.end <= parent.end,
            "{span:?} outside of its parent {parent:?}"
        );
    }

    fn name(&self, name: &Name) {
        self.within_parent(name.span);
        let text = &self.input[std::ops::Range::<usize>::from(name.span)];
        assert_eq!(text, name.node, "the span of a name is not that name");
    }
}

impl<'ast> Visitor<'ast> for Check<'_> {
    fn visit_item(&mut self, item: &'ast Item) {
        self.within_parent(item.span);
        self.parents.push(item.span);
        match &item.node {
            ItemKind::Class { name, .. } => self.name(name),
            ItemKind::Const { values, .. } => values.iter().for_each(|(name, _)| self.name(name)),
            ItemKind::Variable { vars, .. } => vars.iter().for_each(|var| self.name(&var.name)),
            ItemKind::OptionExplicit | ItemKind::Statement(_) => {}
        }
        walk_item(self, item);
        self.parents.pop();
    }

    fn visit_member_access(&mut self, member_access: &'ast MemberAccess) {
        self.name(&member_access.name);
        walk_member_access(self, member_access);
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        self.within_parent(stmt.span);
        self.parents.push(stmt.span);
        match &stmt.node {
            StmtKind::Sub {
                name, parameters, ..
            }
            | StmtKind::Function {
                name, parameters, ..
            } => {
                self.name(name);
                for Argument::ByVal(name) | Argument::ByRef(name) in parameters {
                    self.name(name);
                }
            }
            StmtKind::ForStmt { counter: name, .. }
            | StmtKind::ForEachStmt { element: name, .. } => self.name(name),
            _ => {}
        }
        walk_stmt(self, stmt);
        self.parents.pop();
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        self.within_parent(expr.span);
        self.parents.push(expr.span);
        match &expr.node {
            ExprKind::Ident(name)
            | ExprKind::New(name)
            | ExprKind::MemberExpression { property: name, .. } => self.name(name),
            _ => {}
        }
        walk_expr(self, expr);
        self.parents.pop();
    }
}

fuzz_target!(|data: &[u8]| {
    // the lexer and the parser take a str
    let Ok(input) = std::str::from_utf8(data) else {
        return;
    };

    let text: String = Lexer::new(input).map(|token| token.text(input)).collect();
    assert_eq!(text, input, "the tokens are not the input");

    if let Ok(items) = Parser::new(input).file() {
        let whole = Span {
            start: 0,
            end: input.len() as u32,
        };
        let mut check = Check {
            input,
            parents: vec![whole],
        };
        walk_items(&mut check, &items);
    }
});
