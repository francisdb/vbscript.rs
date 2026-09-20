//! A lexer and a parser for VBScript, the language of Windows Script Host and classic ASP,
//! and of the table scripts of Visual Pinball.
//!
//! What it accepts is checked against `cscript` on Windows, not only against the
//! documentation, and against a corpus of over 1500 real scripts.
//!
//! # Parsing
//!
//! [`Parser`](parser::Parser) turns a script into a tree of [`Item`](parser::ast::Item)s:
//! classes, the declarations of the script and its statements, which hold
//! [`Stmt`](parser::ast::Stmt)s and [`Expr`](parser::ast::Expr)s. Each of those is the kind of
//! node in a [`Spanned`](parser::ast::Spanned), so matching goes through its `node`.
//!
//! ```
//! use vbscript::parser::Parser;
//! use vbscript::parser::ast::{Argument, ItemKind, StmtKind};
//!
//! let input = "Sub Greet(ByVal name)\r\n    MsgBox \"Hello \" & name\r\nEnd Sub\r\n";
//! let items = Parser::new(input).file().unwrap();
//!
//! let ItemKind::Statement(stmt) = &items[0].node else { panic!() };
//! let StmtKind::Sub { name, parameters, body, .. } = &stmt.node else { panic!() };
//! assert_eq!(*name, "Greet");
//! assert_eq!(parameters, &[Argument::ByVal("name".into())]);
//! // the expressions print with the parentheses that show how they were read
//! let StmtKind::SubCall { fn_name, args } = &body[0].node else { panic!() };
//! assert_eq!(fn_name.to_string(), "MsgBox");
//! assert_eq!(args[0].as_ref().unwrap().to_string(), "(\"Hello \" & name)");
//! ```
//!
//! A script that does not parse gives a [`ParseError`](parser::ParseError) with the position,
//! it does not panic, also not on deeply nested input.
//!
//! ```
//! use vbscript::parser::Parser;
//!
//! let error = Parser::new("Dim total\ntotal = 1 + )\n").file().unwrap_err();
//! assert_eq!((error.line(), error.column()), (2, 13));
//! assert_eq!(error.to_string(), "line 2, column 13: Unknown start of expression: )");
//! ```
//!
//! # From a node back to the source
//!
//! Every item, statement, expression and name has a [`Span`](lexer::Span), the range of
//! bytes it was parsed from. Slice the source with it to get what was written, which the
//! tree does not always keep: `&HFF` is the number 255 in the tree. A
//! [`LineIndex`](lexer::LineIndex) gives the line and column for a person to look at.
//!
//! Spans are left out when nodes are compared or printed with `{:?}`: two trees are equal
//! if they have the same structure, however the source was laid out.
//!
//! ```
//! use vbscript::lexer::LineIndex;
//! use vbscript::parser::Parser;
//! use vbscript::parser::ast::ItemKind;
//!
//! let input = "' colors\nConst Red = &HFF\n";
//! let items = Parser::new(input).file().unwrap();
//! let ItemKind::Const { values, .. } = &items[0].node else { panic!() };
//! let (name, value) = &values[0];
//!
//! assert_eq!(value.to_string(), "255");
//! let index = LineIndex::new(input);
//! assert_eq!(index.line_column(name.span.start), (2, 7));
//! // the item is the statement without the comment before it or the line end after it
//! assert_eq!(&input[std::ops::Range::from(items[0].span)], "Const Red = &HFF");
//! ```
//!
//! # Walking the tree
//!
//! [`parser::visit`] has a [`Visitor`](parser::visit::Visitor) that does the recursion over
//! all kinds of statements and expressions, so a tool only writes what it is about.
//!
//! ```
//! use vbscript::parser::Parser;
//! use vbscript::parser::ast::{Expr, ExprKind};
//! use vbscript::parser::visit::{Visitor, walk_expr, walk_items};
//!
//! /// The classes a script makes objects of.
//! #[derive(Default)]
//! struct Created(Vec<String>);
//!
//! impl<'ast> Visitor<'ast> for Created {
//!     fn visit_expr(&mut self, expr: &'ast Expr) {
//!         if let ExprKind::New(class) = &expr.node {
//!             self.0.push(class.to_string());
//!         }
//!         walk_expr(self, expr);
//!     }
//! }
//!
//! let input = "Sub Init\n    Set timer = New cvpmTimer\n    If big Then Set a = New Big\nEnd Sub\n";
//! let items = Parser::new(input).file().unwrap();
//! let mut created = Created::default();
//! walk_items(&mut created, &items);
//! assert_eq!(created.0, ["cvpmTimer", "Big"]);
//! ```
//!
//! # Lexing
//!
//! The [`Lexer`](lexer::Lexer) can be used on its own. It keeps everything, whitespace and
//! comments included, so the text of its tokens put together is the input again. The parser
//! leaves those two out.
//!
//! ```
//! use vbscript::lexer::{Lexer, TokenKind};
//!
//! let input = "x = &HFF ' hex";
//! let tokens: Vec<_> = Lexer::new(input).collect();
//! let text: String = tokens.iter().map(|token| token.text(input)).collect();
//! assert_eq!(text, input);
//!
//! let comment = tokens.iter().find(|token| token.kind == TokenKind::Comment).unwrap();
//! assert_eq!(comment.text(input), "' hex");
//! assert_eq!((comment.line, comment.column), (1, 10));
//! ```

pub mod lexer;
pub mod parser;
