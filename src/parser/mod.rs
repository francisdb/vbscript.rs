use crate::{lexer::*, T};
use std::iter::Peekable;

pub mod ast;
mod expressions;
mod hierarchy;

pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
}

impl<'input> Parser<'input, TokenIter<'input>> {
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
        }
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    /// Get the source text of a token.
    pub fn text(&self, token: &Token) -> &'input str {
        token.text(self.input)
    }

    /// Look-ahead one token and see what kind of token it is.
    pub(crate) fn peek(&mut self) -> TokenKind {
        self.tokens
            .peek()
            .map(|token| token.kind)
            .unwrap_or(T![EOF])
    }

    pub(crate) fn peek_full(&mut self) -> &Token {
        self.tokens
            .peek()
            .unwrap_or_else(|| panic!("Expected a token, but found EOF"))
    }

    /// Check if the next token is some `kind` of token.
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Get the next token.
    pub(crate) fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    /// Move forward one token in the input and check
    /// that we pass the kind of token we expect.
    pub(crate) fn consume(&mut self, expected: TokenKind) -> Token {
        let token = self.next().unwrap_or_else(|| {
            panic!(
                "Expected to consume `{}`, but there was no next token",
                expected
            )
        });
        if token.kind != expected {
            panic!(
                "{}:{} Expected to consume `{}`, but found `{}`",
                token.line, token.column, expected, token.kind
            );
        }
        token
    }

    pub(crate) fn consume_line_delimiter(&mut self) {
        let peek = self.peek_full();
        match peek.kind {
            T![EOF] => {}
            T![end] => {}
            T![nl] => {
                self.consume(T![nl]);
            }
            T![:] => {
                self.consume(T![:]);
                // if there is a newline directly after the colon, consume it
                if self.at(T![nl]) {
                    self.consume(T![nl]);
                }
            }
            other => panic!(
                "{}:{} Unexpected token Expected newline or colon, but found {}",
                peek.line, peek.column, other
            ),
        }
    }

    fn consume_optional_line_delimiter(&mut self) {
        if matches!(self.peek(), T![nl] | T![:]) {
            self.consume_line_delimiter();
        }
    }

    pub(crate) fn at_new_line_or_eof(&mut self) -> bool {
        matches!(self.peek(), T![nl] | T![:] | T![EOF])
    }
}

/// Iterator over the tokens of the lexer, filtering out whitespace, empty lines and comments.
pub struct TokenIter<'input> {
    lexer: Lexer<'input>,
    prev_token_kind_including_ws: TokenKind,
    prev_token_kind: TokenKind,
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Lexer::new(input),
            prev_token_kind: T![nl],
            prev_token_kind_including_ws: T![nl],
        }
    }
}

impl<'input> Iterator for TokenIter<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let current_token = self.lexer.next()?;
            // translate [non-whitepace, .] to [non-whitepace, _.]
            // without lookahead/back on the lexer we can't do this kind of check
            // TODO would it not be better to do a positive check here checking for valid cases?
            if !matches!(
                self.prev_token_kind_including_ws,
                T![nl] | T![ws] | T![:] | T!['('] | T![-] | T![,] | T![&]
            ) && matches!(current_token.kind, T![.])
            {
                let repacement_token = Token {
                    kind: T![_.],
                    span: current_token.span,
                    line: current_token.line,
                    column: current_token.column,
                };
                self.prev_token_kind_including_ws = repacement_token.kind;
                self.prev_token_kind = repacement_token.kind;
                return Some(repacement_token);
            }

            // ignore whitespace
            if matches!(current_token.kind, T![ws]) {
                self.prev_token_kind_including_ws = current_token.kind;
                continue;
            }
            if matches!(current_token.kind, T![line_continuation]) {
                // the lexer already consumes the newline
                self.prev_token_kind_including_ws = current_token.kind;
                continue;
            }
            // skip empty lines
            if matches!(self.prev_token_kind, T![nl]) && matches!(current_token.kind, T![nl]) {
                self.prev_token_kind = current_token.kind;
                self.prev_token_kind_including_ws = current_token.kind;
                continue;
            }
            // skip empty inline lines
            if matches!(self.prev_token_kind, T![:] | T![nl]) && matches!(current_token.kind, T![:])
            {
                self.prev_token_kind = current_token.kind;
                self.prev_token_kind_including_ws = current_token.kind;
                continue;
            }
            // skip single line comments that are preceded by a newline
            if matches!(self.prev_token_kind, T![nl]) && matches!(current_token.kind, T![comment]) {
                // hacky way to not keep the comment newline
                self.prev_token_kind = T![nl];
                self.prev_token_kind_including_ws = T![nl];
                continue;
            }
            // if we find a REM that is not preceded by a dot we discard until the end of the line
            if matches!(current_token.kind, T![rem])
                && !matches!(self.prev_token_kind_including_ws, T![_.])
            {
                let mut cur = current_token;
                // this will also discard any error tokens which are expected
                while !matches!(cur.kind, T![nl]) {
                    cur = self.lexer.next()?;
                }
                self.prev_token_kind = cur.kind;
                self.prev_token_kind_including_ws = cur.kind;
                continue;
            }
            self.prev_token_kind = current_token.kind;
            self.prev_token_kind_including_ws = current_token.kind;
            if !matches!(current_token.kind, T![comment]) {
                return Some(current_token);
            } // else continue
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::ast::ErrorClause::{Goto0, ResumeNext};
    use crate::parser::ast::Expr::{InfixOp, WithScoped};
    use crate::parser::ast::Stmt::OnError;
    use crate::parser::ast::{
        Argument, ArgumentType, Case, DoLoopCheck, DoLoopCondition, Expr, FullIdent, Item, Lit,
        MemberAccess, MemberDefinitions, PropertyType, PropertyVisibility, SetRhs, Stmt,
        Visibility,
    };
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn parse(input: &str) -> Expr {
        let mut parser = Parser::new(input);
        parser.expression()
    }

    #[test]
    fn test_line_continuations() {
        // mind the trailing spaces
        let input = indoc! {r#"
            Dim x _
             _
                , y
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        assert_eq!(
            file,
            vec![Item::Statement(Stmt::Dim {
                vars: vec![("x".to_string(), vec![]), ("y".to_string(), vec![]),],
            }),]
        );
    }

    #[test]
    fn test_token_iter_newlines_with_comments() {
        let input = indoc! {r#"
Const a = 1			' some info
					' info continued
					' info further continued
        "#};
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .iter()
            .map(|t| t.kind)
            .filter(|t| t != &T![ws])
            .collect();
        assert_eq!(
            tokens,
            [
                T![const],
                T![ident],
                T![=],
                T![integer_literal],
                T![comment],
                T![nl],
                T![comment],
                T![nl],
                T![comment],
                T![nl],
                T![EOF],
            ]
        );

        let token_iter = TokenIter::new(input);
        let tokens: Vec<TokenKind> = token_iter.map(|t| t.kind).collect();
        assert_eq!(
            tokens,
            [
                T![const],
                T![ident],
                T![=],
                T![integer_literal],
                T![nl],
                T![EOF],
            ]
        );
    }

    #[test]
    fn parse_expression() {
        // Weird spaces are to test that whitespace gets filtered out
        let expr = parse("42");
        assert_eq!(expr, Expr::int(42));
        let expr = parse("  2.7768");
        assert_eq!(expr, Expr::Literal(Lit::Float(2.7768)));
        let expr = parse(r#""I am a String!""#);
        assert_eq!(expr, Expr::str("I am a String!".to_string()));
        let expr = parse("foo");
        assert_eq!(expr, ast::Expr::ident("foo"));
        let expr = parse("bar (  x, 2)");
        assert_eq!(
            expr,
            Expr::FnApplication {
                callee: Box::new(Expr::ident("bar")),
                args: vec![Some(Expr::ident("x")), Some(Expr::int(2))],
            }
        );
        let expr = parse("Not is_visible");
        assert_eq!(
            expr,
            Expr::PrefixOp {
                op: T![not],
                expr: Box::new(Expr::ident("is_visible")),
            }
        );
        let expr = parse("(-13)");
        assert_eq!(
            expr,
            Expr::PrefixOp {
                op: T![-],
                expr: Box::new(Expr::int(13)),
            }
        );
    }

    #[test]
    fn parse_binary_expressions() {
        let expr = parse("4 + 2 * 3");
        assert_eq!(expr.to_string(), "(4 + (2 * 3))"); // passes

        let expr = parse("4 * 2 + 3");
        assert_eq!(expr.to_string(), "((4 * 2) + 3)"); // fails

        let expr = parse("4 - 2 - 3");
        assert_eq!(expr.to_string(), "((4 - 2) - 3)"); // fails

        let expr = parse("4 ^ 2 ^ 3");
        assert_eq!(expr.to_string(), "(4 ^ (2 ^ 3))"); // passes

        let expr = parse(r#"45.7 + 3 + 5 * 4^8^9 / 6 > 4 and test - 7 / 4 = "Hallo""#);
        assert_eq!(
            expr.to_string(),
            r#"((((45.7 + 3) + ((5 * (4 ^ (8 ^ 9))) / 6)) > 4) and ((test - (7 / 4)) = "Hallo"))"#
        );

        let expr = parse("2.0 / ((3.0 + 4.0) * (5.0 - 6.0)) * 7.0");
        assert_eq!(expr.to_string(), "((2 / ((3 + 4) * (5 - 6))) * 7)");

        let expr = parse("min ( test + 4 , sin(2*PI ))");
        assert_eq!(
            expr,
            Expr::FnApplication {
                callee: Box::new(Expr::ident("min")),
                args: vec![
                    Some(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("test")),
                        rhs: Box::new(Expr::int(4)),
                    }),
                    Some(Expr::FnApplication {
                        callee: Box::new(Expr::ident("sin")),
                        args: vec![Some(Expr::InfixOp {
                            op: T![*],
                            lhs: Box::new(Expr::int(2)),
                            rhs: Box::new(Expr::ident("PI")),
                        })],
                    }),
                ],
            }
        );
        assert_eq!(expr.to_string(), "min((test + 4), sin((2 * PI)))");
    }

    #[test]
    fn parse_simple_if_stmt() {
        let input = indoc! {r#"
            if x > 2 then
                x = 4
            end if
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::int(2)),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::int(4)),
                }],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn parse_simple_while() {
        let input = indoc! {r#"
            While x < 5
                x=x+1
            Wend
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::WhileStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![<],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::int(5)),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(1)),
                    }),
                },],
            }
        );
    }

    #[test]
    fn test_while_single_line() {
        let input = r#"while skipIdx = skipIdx2:skipIdx2=Int(Rnd*6):wend"#;
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::WhileStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![=],
                    lhs: Box::new(Expr::ident("skipIdx")),
                    rhs: Box::new(Expr::ident("skipIdx2")),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("skipIdx2"),
                    value: Box::new(Expr::FnApplication {
                        callee: Box::new(Expr::ident("Int")),
                        args: vec![Some(Expr::InfixOp {
                            op: T![*],
                            lhs: Box::new(Expr::ident("Rnd")),
                            rhs: Box::new(Expr::int(6)),
                        })],
                    }),
                },],
            }),]
        );
    }

    #[test]
    fn parse_simple_for_next_loop() {
        let input = indoc! {r#"
            For i = 1 to 10
                x = x + i
            Next
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForStmt {
                counter: "i".to_string(),
                start: Box::new(Expr::int(1)),
                end: Box::new(Expr::int(10)),
                step: None,
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::ident("i")),
                    }),
                }],
            }
        );
    }

    #[test]
    fn parse_simple_for_next_loop_with_exit() {
        let input = indoc! {r#"
            For i = 1 to 10
                x = x * i
                If x > 10 Then Exit For
            Next
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForStmt {
                counter: "i".to_string(),
                start: Box::new(Expr::int(1)),
                end: Box::new(Expr::int(10)),
                step: None,
                body: vec![
                    Stmt::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(Expr::InfixOp {
                            op: T![*],
                            lhs: Box::new(Expr::ident("x")),
                            rhs: Box::new(Expr::ident("i")),
                        }),
                    },
                    Stmt::IfStmt {
                        condition: Box::new(Expr::InfixOp {
                            op: T![>],
                            lhs: Box::new(Expr::ident("x")),
                            rhs: Box::new(Expr::int(10)),
                        }),
                        body: vec![Stmt::ExitFor],
                        elseif_statements: vec![],
                        else_stmt: None,
                    },
                ],
            }
        );
    }

    #[test]
    fn parse_for_step() {
        let input = indoc! {r#"
            For i = For_nr to Next_nr step Bdir
                ' do nothing
            Next
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        assert_eq!(
            file,
            vec![Item::Statement(Stmt::ForStmt {
                counter: "i".to_string(),
                start: Box::new(Expr::ident("For_nr")),
                end: Box::new(Expr::ident("Next_nr")),
                step: Some(Box::new(Expr::ident("Bdir"))),
                body: vec![],
            }),]
        );
    }

    #[test]
    fn parse_for_inline() {
        let input = "For x = 1 To PlayerMode(currentplayer)+1 : Blink(x,1)=1 : Next";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        #[rustfmt::skip]
        assert_eq!(
            stmt,
            Stmt::ForStmt {
                counter: "x".to_string(),
                start: Box::new(Expr::int(1)),
                end: Box::new(Expr::InfixOp {
                    op: T![+],
                    lhs: Box::new(Expr::fn_application(
                        Expr::ident("PlayerMode"),
                        vec![Expr::ident("currentplayer")])
                    ),
                    rhs: Box::new(Expr::int(1)),
                }),
                step: None,
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::new(Expr::fn_application(
                        Expr::ident("Blink"),
                        vec![Expr::ident("x"), Expr::int(1)],
                    )),
                    value: Box::new(Expr::int(1)),
                },],
            }
        );
    }

    #[test]
    fn parse_foreach_multiline() {
        let input = indoc! {r#"
            For each dog in dogs
                dog.visible = true
            Next
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForEachStmt {
                element: "dog".to_string(),
                group: Box::new(Expr::ident("dogs")),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent(Box::new(Expr::MemberExpression {
                        base: Box::new(Expr::ident("dog")),
                        property: "visible".to_string(),
                    })),
                    value: Box::new(Expr::Literal(Lit::Bool(true))),
                }],
            }
        );
    }

    #[test]
    fn parse_foreach_inline() {
        let input = indoc! {r#"
            For each dog in dogs : dog.volume = 0 : dog.visible = true : Next
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ForEachStmt {
                element: "dog".to_string(),
                group: Box::new(Expr::ident("dogs")),
                body: vec![
                    Stmt::Assignment {
                        full_ident: FullIdent::new(Expr::member(Expr::ident("dog"), "volume"),),
                        value: Box::new(Expr::int(0)),
                    },
                    Stmt::Assignment {
                        full_ident: FullIdent::new(Expr::member(Expr::ident("dog"), "visible"),),
                        value: Box::new(Expr::Literal(Lit::Bool(true))),
                    },
                ],
            }
        );
    }

    #[test]
    fn parse_simple_function_declaration() {
        let input = indoc! {r#"
            Function add (a, b)
                add = a + b
            End Function
        "#};
        let mut parser = Parser::new(input);
        let item = parser.item();
        assert_eq!(
            item,
            Item::Statement(Stmt::Function {
                visibility: Visibility::Default,
                name: "add".to_string(),
                parameters: vec![
                    Argument::ByVal("a".to_string()),
                    Argument::ByVal("b".to_string())
                ],
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("add"),
                    value: Box::new(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("a")),
                        rhs: Box::new(Expr::ident("b")),
                    }),
                }],
            })
        );
    }

    #[test]
    fn parse_sub_declaration() {
        let input = indoc! {r#"
            Sub log (a, b)
                'print a
                'print b
            End Sub
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Sub {
                visibility: Visibility::Default,
                name: "log".to_string(),
                parameters: vec![
                    Argument::ByVal("a".to_string()),
                    Argument::ByVal("b".to_string())
                ],
                body: vec![],
            }
        );
    }

    #[test]
    fn parse_empty_sub_without_args() {
        let input = indoc! {r#"
            private Sub log
            End Sub
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Sub {
                visibility: Visibility::Private,
                name: "log".to_string(),
                parameters: vec![],
                body: vec![],
            }
        );
    }

    #[test]
    fn parse_inline_sub_declaration() {
        let input = "Sub Trigger003_hit : RampWireRight.x = 0.1 : Light030.state = 0 : End Sub";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        #[rustfmt::skip]
        assert_eq!(
            stmt,
            Stmt::Sub {
                visibility: Visibility::Default,
                name: "Trigger003_hit".to_string(),
                parameters: vec![],
                body: vec![
                    Stmt::Assignment {
                        full_ident: FullIdent::new(
                            Expr::member(Expr::ident("RampWireRight"), "x"),
                        ),
                        value: Box::new(Expr::Literal(Lit::Float(0.1))),
                    },
                    Stmt::Assignment {
                        full_ident: FullIdent::new(
                            Expr::member(Expr::ident("Light030"), "state"),
                        ),
                        value: Box::new(Expr::int(0)),
                    },
                ],
            }
        );
    }

    #[test]
    fn parse_nested_sub_declaration() {
        // Nested subs are available globally (hoisted)
        // You can't define a Sub procedure inside any other procedure (e.g. Function, Sub or Property Get).
        let input = indoc! {r#"
            If true Then
                Sub inner
                    ' do something
                End Sub
            End If
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        assert_eq!(
            file,
            vec![Item::Statement(Stmt::IfStmt {
                condition: Box::new(Expr::Literal(Lit::Bool(true))),
                body: vec![Stmt::Sub {
                    visibility: Visibility::Default,
                    name: "inner".to_string(),
                    parameters: vec![],
                    body: vec![],
                }],
                elseif_statements: vec![],
                else_stmt: None,
            })],
        );
    }

    #[test]
    fn parse_sub_without_newline() {
        let input = indoc! {r#"
            Sub Trigger1_Hit() If BIP=1 Then
                BIP=0
                End If
            End Sub
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Sub {
                visibility: Visibility::Default,
                name: "Trigger1_Hit".to_string(),
                parameters: vec![],
                body: vec![Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![=],
                        lhs: Box::new(Expr::ident("BIP")),
                        rhs: Box::new(Expr::int(1)),
                    }),
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("BIP"),
                        value: Box::new(Expr::int(0)),
                    }],
                    elseif_statements: vec![],
                    else_stmt: None,
                },],
            }
        );
    }

    #[test]
    fn parse_sub_with_array_argument() {
        let input = indoc! {r#"
            Sub test (byref a())
                'print a
            End Sub
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Sub {
                visibility: Visibility::Default,
                name: "test".to_string(),
                parameters: vec![Argument::ByRef("a".to_string())],
                body: vec![],
            }
        );
    }

    #[test]
    fn parse_byval_byref() {
        let input = indoc! {r#"
            Sub test (ByRef a)
                'print a
            End Sub
            Function test2 (ByVal a)
                test2 = a
            End Function
        "#};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Statement(Stmt::Sub {
                    visibility: Visibility::Default,
                    name: "test".to_string(),
                    parameters: vec![Argument::ByRef("a".to_string())],
                    body: vec![],
                }),
                Item::Statement(Stmt::Function {
                    visibility: Visibility::Default,
                    name: "test2".to_string(),
                    parameters: vec![Argument::ByVal("a".to_string())],
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("test2"),
                        value: Box::new(Expr::ident("a")),
                    }],
                }),
            ]
        );
    }

    #[test]
    fn test_parse_file_empty() {
        let input = "";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_newlines() {
        let input = "\r\n\n\n\r\n";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_colons() {
        let input = ":: ::";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_colon_and_newline() {
        let input = ":\r\n";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_comments() {
        let input = indoc! {"
            ' This is a comment

            REM This is a rem comment

            ' This is another comment without newline"};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_statement_with_trailing_comment() {
        let input = indoc! {"
            Option Explicit ' Force explicit variable declaration.
            ' This is another comment"};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(all, vec![Item::OptionExplicit,]);
    }

    #[test]
    fn parse_option_and_randomize_on_single_line() {
        let input = "Option Explicit : Randomize";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::OptionExplicit,
                Item::Statement(Stmt::SubCall {
                    fn_name: FullIdent::ident("Randomize"),
                    args: vec![],
                }),
            ]
        );
    }

    #[test]
    fn test_parse_no_arg_sub_call() {
        let input = "SayHello";
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![Item::Statement(Stmt::SubCall {
                fn_name: FullIdent::ident("SayHello"),
                args: vec![],
            }),]
        );
    }

    #[test]
    fn test_parse_sub_call() {
        let input = indoc! {"
            test
            test 1
            test 1, 2"
        };
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Statement(Stmt::SubCall {
                    fn_name: FullIdent::ident("test"),
                    args: vec![],
                }),
                Item::Statement(Stmt::SubCall {
                    fn_name: FullIdent::ident("test"),
                    args: vec![Some(Expr::int(1))],
                }),
                Item::Statement(Stmt::SubCall {
                    fn_name: FullIdent::ident("test"),
                    args: vec![Some(Expr::int(1)), Some(Expr::int(2))],
                }),
            ]
        );
    }

    #[test]
    fn test_parse_error_handling() {
        let input = indoc! {"
            On Error Resume Next
            On Error GoTo 0
        "};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Statement(OnError {
                    error_clause: ResumeNext
                }),
                Item::Statement(OnError {
                    error_clause: Goto0
                }),
            ]
        );
    }

    #[test]
    fn test_single_line_if() {
        let input = r#"If Err Then MsgBox "Oh noes""#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::ident("Err")),
                body: vec![Stmt::SubCall {
                    fn_name: FullIdent::ident("MsgBox"),
                    args: vec![Some(Expr::str("Oh noes"))],
                }],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn test_single_line_if_multi_statement() {
        let input = r#"If Err Then MsgBox "Oh noes": MsgBox "Crash""#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::ident("Err")),
                body: vec![
                    Stmt::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Some(Expr::str("Oh noes"))],
                    },
                    Stmt::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Some(Expr::str("Crash"))],
                    }
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn test_singe_line_if_with_end_if() {
        let input = r#"if VRRoom > 0 Then bbs006.state = x2 Else controller.B2SSetData 50,x2 : controller.B2SSetData 53,x2 : End If"#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("VRRoom")),
                    rhs: Box::new(Expr::int(0)),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::new(Expr::member(Expr::ident("bbs006"), "state"),),
                    value: Box::new(Expr::ident("x2")),
                }],
                elseif_statements: vec![],
                else_stmt: Some(vec![
                    Stmt::SubCall {
                        fn_name: FullIdent::new(Expr::member(
                            Expr::ident("controller"),
                            "B2SSetData"
                        ),),
                        args: vec![Some(Expr::int(50)), Some(Expr::ident("x2")),],
                    },
                    Stmt::SubCall {
                        fn_name: FullIdent::new(Expr::member(
                            Expr::ident("controller"),
                            "B2SSetData"
                        ),),
                        args: vec![Some(Expr::int(53)), Some(Expr::ident("x2")),],
                    },
                ]),
            }
        );
    }

    #[test]
    fn parse_if_end_if_single_line_no_colons() {
        let input = r#"if a=1 then DoSomething() end if"#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![=],
                    lhs: Box::new(Expr::ident("a")),
                    rhs: Box::new(Expr::int(1)),
                }),
                body: vec![Stmt::SubCall {
                    fn_name: FullIdent::new(Expr::fn_application(
                        Expr::ident("DoSomething"),
                        vec![]
                    )),
                    args: vec![],
                }],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn test_single_line_if_with_many_colons() {
        let input =
            r#"If x < 0 Or Err Then : DoSomething obj : Else : DoSomethingElse obj : End If"#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![or],
                    lhs: Box::new(Expr::InfixOp {
                        op: T![<],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(0)),
                    }),
                    rhs: Box::new(Expr::ident("Err")),
                }),
                body: vec![Stmt::SubCall {
                    fn_name: FullIdent::ident("DoSomething"),
                    args: vec![Some(Expr::ident("obj"))],
                },],
                elseif_statements: vec![],
                else_stmt: Some(vec![Stmt::SubCall {
                    fn_name: FullIdent::ident("DoSomethingElse"),
                    args: vec![Some(Expr::ident("obj"))],
                },]),
            }
        );
    }

    #[test]
    fn test_parse_if_two_single_lines() {
        let input = r#"
            If(x <> "") Then LutValue = CDbl(x) Else LutValue = 1
	        If LutValue < 1 Then LutValue = 1
        "#;
        let mut parser = Parser::new(input);
        let file = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                Item::Statement(Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![<>],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::Literal(Lit::Str("".to_string()))),
                    }),
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::fn_application(
                            Expr::ident("CDbl"),
                            vec![Expr::ident("x")])
                        ),
                    }],
                    elseif_statements: vec![],
                    else_stmt: Some(vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::int(1)),
                    }]),
                }),
                Item::Statement(Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![<],
                        lhs: Box::new(Expr::ident("LutValue")),
                        rhs: Box::new(Expr::int(1)),
                    }),
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::int(1)),
                    }],
                    elseif_statements: vec![],
                    else_stmt: None,
                }),
            ]
        );
    }

    #[test]
    fn test_parse_if_single_line_with_else() {
        let input = r#"If x > 2 Then y = 3 Else y = 4"#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::int(2)),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("y"),
                    value: Box::new(Expr::int(3)),
                }],
                elseif_statements: vec![],
                else_stmt: Some(vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("y"),
                    value: Box::new(Expr::int(4)),
                }]),
            }
        );
    }

    #[test]
    fn test_if_nested_single_line() {
        let input = "If key = 1 Then foo.fire: If bar=1 then DoSomething() else DoSomethingElse()";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![=],
                    lhs: Box::new(Expr::ident("key")),
                    rhs: Box::new(Expr::int(1)),
                }),
                body: vec![
                    Stmt::SubCall {
                        fn_name: FullIdent::new(Expr::member(Expr::ident("foo"), "fire"),),
                        args: vec![],
                    },
                    Stmt::IfStmt {
                        condition: Box::new(Expr::InfixOp {
                            op: T![=],
                            lhs: Box::new(Expr::ident("bar")),
                            rhs: Box::new(Expr::int(1)),
                        }),
                        body: vec![Stmt::SubCall {
                            fn_name: FullIdent::new(Expr::fn_application(
                                Expr::ident("DoSomething"),
                                vec![],
                            )),
                            args: vec![],
                        }],
                        elseif_statements: vec![],
                        else_stmt: Some(vec![Stmt::SubCall {
                            fn_name: FullIdent::new(Expr::fn_application(
                                Expr::ident("DoSomethingElse"),
                                vec![],
                            )),
                            args: vec![],
                        }]),
                    }
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn test_if_nested_mixed() {
        let input = indoc! {r#"
        If x > 2 Then
            If This Or That Then DoSomething 'weird comment
        End If
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.file();
        assert_eq!(
            stmt,
            vec![Item::Statement(Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::int(2)),
                }),
                body: vec![Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![or],
                        lhs: Box::new(Expr::ident("This")),
                        rhs: Box::new(Expr::ident("That")),
                    }),
                    body: vec![Stmt::SubCall {
                        fn_name: FullIdent::ident("DoSomething"),
                        args: vec![],
                    }],
                    elseif_statements: vec![],
                    else_stmt: None,
                }],
                elseif_statements: vec![],
                else_stmt: None,
            }),]
        );
    }

    #[test]
    fn parse_if_mixed() {
        let input = indoc! {r#"
            If a = 3 Then
                b = 0
            Else If	a = 2 Then
                b = 2
                End If
            End If
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::InfixOp {
                    op: T![=],
                    lhs: Box::new(Expr::ident("a")),
                    rhs: Box::new(Expr::int(3)),
                }),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("b"),
                    value: Box::new(Expr::int(0)),
                }],
                elseif_statements: vec![],
                else_stmt: Some(vec![Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![=],
                        lhs: Box::new(Expr::ident("a")),
                        rhs: Box::new(Expr::int(2)),
                    }),
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("b"),
                        value: Box::new(Expr::int(2)),
                    }],
                    elseif_statements: vec![],
                    else_stmt: None,
                }]),
            }
        );
    }

    #[test]
    fn parse_dim() {
        let input = "Dim x";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(stmt, Stmt::dim("x"));
    }

    #[test]
    fn test_dim_array() {
        let input = "Dim x(1, 2)";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Dim {
                vars: vec![("x".to_string(), vec![Expr::int(1), Expr::int(2)])],
            }
        );
    }

    #[test]
    fn test_dim_array_with_space() {
        let input = "Dim PlayerMode (2)";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Dim {
                vars: vec![("PlayerMode".to_string(), vec![Expr::int(2)])],
            }
        );
    }

    #[test]
    fn parse_dim_multiple() {
        let input = "Dim x,y, z(1 + 3)";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Dim {
                vars: vec![
                    ("x".to_string(), vec![]),
                    ("y".to_string(), vec![]),
                    (
                        "z".to_string(),
                        vec![Expr::InfixOp {
                            op: T![+],
                            lhs: Box::new(Expr::int(1)),
                            rhs: Box::new(Expr::int(3)),
                        }]
                    ),
                ],
            }
        );
    }

    #[test]
    fn parse_const() {
        let input = "Const x = 42";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(stmt, Stmt::const_("x", Lit::int(42)));
    }

    #[test]
    fn parse_const_negative() {
        let input = "Const x = -1";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(stmt, Stmt::const_("x", Lit::int(-1)));
    }

    #[test]
    fn parse_const_multi() {
        let input = r#"const x = 42, txt = "Hello""#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Const(vec![
                ("x".to_string(), Lit::int(42)),
                ("txt".to_string(), Lit::str("Hello".to_string())),
            ])
        );
    }

    #[test]
    fn parse_two_consts_with_comments() {
        let input = indoc! {"
            Const x = 42 ' The answer to everything
            Const y = 13 ' An unlucky number
        "};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Const {
                    visibility: Visibility::Public,
                    values: vec![("x".to_string(), Lit::int(42))],
                },
                Item::Const {
                    visibility: Visibility::Public,
                    values: vec![("y".to_string(), Lit::int(13))],
                }
            ]
        );
    }

    #[test]
    fn parse_true_false() {
        let input = indoc! {r#"
            Const Test = False
            Const Test2 = True
        "#};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Const {
                    visibility: Visibility::Public,
                    values: vec![("Test".to_string(), Lit::Bool(false))],
                },
                Item::Const {
                    visibility: Visibility::Public,
                    values: vec![("Test2".to_string(), Lit::Bool(true))],
                }
            ]
        );
    }

    #[test]
    fn parse_const_private_negative_int() {
        let input = indoc! {r#"
            Private Const Test = -1
        "#};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![Item::Const {
                visibility: Visibility::Private,
                values: vec![("Test".to_string(), Lit::int(-1))],
            }]
        );
    }

    #[test]
    fn parse_variables() {
        let input = indoc! {r#"
            Public x, y(1,2)
            Private z, a(1), b()
        "#};
        let mut parser = Parser::new(input);
        let all = parser.file();
        assert_eq!(
            all,
            vec![
                Item::Variable {
                    visibility: Visibility::Public,
                    vars: vec![("x".to_string(), None), ("y".to_string(), Some(vec![1, 2])),],
                },
                Item::Variable {
                    visibility: Visibility::Private,
                    vars: vec![
                        ("z".to_string(), None),
                        ("a".to_string(), Some(vec![1])),
                        ("b".to_string(), Some(vec![])),
                    ],
                },
            ]
        );
    }

    #[test]
    #[should_panic = "2:12 Expected `sub` or `function` after visibility, but found `const`"]
    fn parse_const_private_nested_fail() {
        // not allowed on windows
        let input = indoc! {r#"
            Sub Test
                Private Const Test = 1
            End Sub
        "#};
        Parser::new(input).file();
    }

    #[test]
    fn test_set() {
        let input = indoc! {r#"
            Set foo = bar
            Set Obj(x) = NullFader
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                Item::Statement(Stmt::Set {
                    var: FullIdent::ident("foo"),
                    rhs: SetRhs::ident("bar"),
                }),
                Item::Statement(Stmt::Set {
                    var: FullIdent::new(
                        Expr::fn_application(
                            Expr::ident("Obj"),
                            vec![Expr::ident("x")]
                        )
                    ),
                    rhs: SetRhs::ident("NullFader"),
                }),
            ]
        );
    }

    #[test]
    fn test_set_using_new() {
        let input = "Set foo = New Bar";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Set {
                var: FullIdent::ident("foo"),
                rhs: SetRhs::Expr(Box::new(Expr::new("Bar"))),
            })]
        );
    }

    #[test]
    fn test_set_using_new_parentheses() {
        // This creates a new class, calls a default function that returns Me
        let input = "Set DT1 = (new DropTarget)(1, 0, False)";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Set {
                var: FullIdent::ident("DT1"),
                rhs: SetRhs::Expr(Box::new(Expr::fn_application(
                    Expr::new("DropTarget"),
                    vec![Expr::int(1), Expr::int(0), Expr::Literal(Lit::Bool(false)),]
                ))),
            })]
        );
    }

    #[test]
    fn test_set_using_nothing() {
        let input = "Set foo = Nothing";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Set {
                var: FullIdent::ident("foo"),
                rhs: SetRhs::Nothing,
            })]
        );
    }

    #[test]
    fn parse_set_deep() {
        let input = "Set lampz.obj(x) = 1";
        let mut parser = Parser::new(input);
        let items = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![
                Item::Statement(
                    Stmt::Set {
                        var: FullIdent::new(
                            Expr::fn_application(
                                Expr::member(Expr::ident("lampz"), "obj"),
                                vec![Expr::ident("x")]
                            )
                        ),
                        rhs: SetRhs::expr(Expr::int(1)),
                    }
                )
            ]
        );
    }

    #[test]
    fn test_redim() {
        let input = "Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::ReDim {
                preserve: true,
                var_bounds: vec![(
                    "tmp".to_string(),
                    vec![Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::InfixOp {
                            op: T![+],
                            lhs: Box::new(Expr::FnApplication {
                                callee: Box::new(Expr::ident("uBound")),
                                args: vec![Some(Expr::ident("aArray"))],
                            }),
                            rhs: Box::new(Expr::FnApplication {
                                callee: Box::new(Expr::ident("uBound")),
                                args: vec![Some(Expr::ident("aInput"))],
                            }),
                        }),
                        rhs: Box::new(Expr::int(1)),
                    }]
                )]
            })]
        );
    }

    #[test]
    fn parse_redim_multi() {
        let input = "Redim a(length), b(2, 3)";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::ReDim {
                preserve: false,
                var_bounds: vec![
                    ("a".to_string(), vec![Expr::ident("length")]),
                    ("b".to_string(), vec![Expr::int(2), Expr::int(3)]),
                ],
            }
        );
    }

    #[test]
    fn parse_block_with_colons() {
        let input = indoc! {r#"
            Dim test
            If RenderingMode = 2 Then
                test = 1 : startcontroller
            elseif RenderingMode = 3 Then
                test = 2 : startcontroller
            else
                test = 0
            End If
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![
                Item::Statement(Stmt::dim("test")),
                Item::Statement(Stmt::IfStmt {
                    condition: Box::new(Expr::InfixOp {
                        op: T![=],
                        lhs: Box::new(Expr::ident("RenderingMode")),
                        rhs: Box::new(Expr::int(2)),
                    }),
                    body: vec![
                        Stmt::Assignment {
                            full_ident: FullIdent::ident("test"),
                            value: Box::new(Expr::int(1)),
                        },
                        Stmt::SubCall {
                            fn_name: FullIdent::ident("startcontroller"),
                            args: vec![],
                        }
                    ],
                    elseif_statements: vec![(
                        Box::new(Expr::InfixOp {
                            op: T![=],
                            lhs: Box::new(Expr::ident("RenderingMode")),
                            rhs: Box::new(Expr::int(3)),
                        }),
                        vec![
                            Stmt::Assignment {
                                full_ident: FullIdent::ident("test"),
                                value: Box::new(Expr::int(2)),
                            },
                            Stmt::SubCall {
                                fn_name: FullIdent::ident("startcontroller"),
                                args: vec![],
                            }
                        ]
                    )],
                    else_stmt: Some(vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("test"),
                        value: Box::new(Expr::int(0)),
                    }]),
                }),
            ]
        );
    }

    #[test]
    fn test_assignment_with_negative_int() {
        let input = "x = -1";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Assignment {
                full_ident: FullIdent::ident("x"),
                value: Box::new(Expr::PrefixOp {
                    op: T![-],
                    expr: Box::new(Expr::int(1)),
                }),
            })]
        );
    }

    #[test]
    fn test_assignment_to_object_in_array_property() {
        let input = r#"objectArray(i).image = "test""#;
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Assignment {
                full_ident: FullIdent::new(Expr::member(
                    Expr::FnApplication {
                        callee: Box::new(Expr::ident("objectArray")),
                        args: vec![Some(Expr::ident("i"))],
                    },
                    "image"
                )),
                value: Box::new(Expr::str("test")),
            })]
        );
    }

    #[test]
    fn test_assignment_with_property_in_expression() {
        let input = "foo.a = foo.b-120.5";
        let mut parser = Parser::new(input);
        let items = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::assignment (
                FullIdent::new(
                    Expr::member(Expr::ident("foo"), "a")
                ),
                Expr::InfixOp {
                    op: T![-],
                    lhs: Box::new(Expr::member(Expr::ident("foo"), "b")),
                    rhs: Box::new(Expr::Literal(Lit::Float(120.5))),
                }
            )
            )]
        );
    }

    #[test]
    fn test_parse_select() {
        let input = indoc! {r#"
            Select Case x
                Case 1, 2
                    y = 2
                Case 3
                    y = 3
                Case Else
                    y = 4
            End Select
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::SelectCase {
                test_expr: Box::new(Expr::ident("x")),
                cases: vec![
                    Case {
                        tests: vec![Expr::int(1), Expr::int(2),],
                        body: vec![Stmt::Assignment {
                            full_ident: FullIdent::ident("y"),
                            value: Box::new(Expr::int(2)),
                        }]
                    },
                    Case {
                        tests: vec![Expr::int(3),],
                        body: vec![Stmt::Assignment {
                            full_ident: FullIdent::ident("y"),
                            value: Box::new(Expr::int(3)),
                        }]
                    },
                ],
                else_stmt: Some(vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("y"),
                    value: Box::new(Expr::int(4)),
                }]),
            })]
        );
    }

    #[test]
    fn test_parse_select_expr() {
        let input = indoc! {r#"
            Select Case true
                Case x=1 AND y=2
                    z = 2
                case x=1, y=2
                    z = 3
            End Select
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::SelectCase {
                test_expr: Box::new(Expr::Literal(Lit::Bool(true))),
                cases: vec![
                    Case {
                        tests: vec![Expr::InfixOp {
                            op: T![and],
                            lhs: Box::new(Expr::InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(1)),
                            }),
                            rhs: Box::new(Expr::InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("y")),
                                rhs: Box::new(Expr::int(2)),
                            }),
                        },],
                        body: vec![Stmt::Assignment {
                            full_ident: FullIdent::ident("z"),
                            value: Box::new(Expr::int(2)),
                        }]
                    },
                    Case {
                        tests: vec![
                            Expr::InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(1)),
                            },
                            Expr::InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("y")),
                                rhs: Box::new(Expr::int(2)),
                            },
                        ],
                        body: vec![Stmt::Assignment {
                            full_ident: FullIdent::ident("z"),
                            value: Box::new(Expr::int(3)),
                        }]
                    }
                ],
                else_stmt: None,
            })]
        );
    }

    #[test]
    fn test_parse_select_inline_cases() {
        let input = indoc! {r#"
            Select Case x
                Case 1, 2:y = 2
                Case Else: y = 4
            End Select
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::SelectCase {
                test_expr: Box::new(Expr::ident("x")),
                cases: vec![Case {
                    tests: vec![Expr::int(1), Expr::int(2),],
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("y"),
                        value: Box::new(Expr::int(2)),
                    }]
                },],
                else_stmt: Some(vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("y"),
                    value: Box::new(Expr::int(4)),
                }]),
            })]
        );
    }

    #[test]
    fn test_select_inline_cases_without_colon() {
        let input = indoc! {r#"
            Select Case keycode
                Case keyA    MySub True : OtherSub 1
                Case 82      .Switch(swCPUDiag)     = t
			    Case Else    DoNothing
            End Select
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::SelectCase {
                test_expr: Box::new(Expr::ident("keycode")),
                cases: vec![
                    Case {
                        tests: vec![Expr::ident("keyA")],
                        body: vec![
                            Stmt::SubCall {
                                fn_name: FullIdent::ident("MySub"),
                                args: vec![Some(Expr::Literal(Lit::Bool(true)))],
                            },
                            Stmt::SubCall {
                                fn_name: FullIdent::ident("OtherSub"),
                                args: vec![Some(Expr::int(1))],
                            }
                        ],
                    },
                    Case {
                        tests: vec![Expr::int(82),],
                        body: vec![Stmt::Assignment {
                            full_ident: FullIdent::new(Expr::fn_application(
                                Expr::member(WithScoped, "Switch"),
                                vec![Expr::ident("swCPUDiag")]
                            )),
                            value: Box::new(Expr::ident("t")),
                        }]
                    },
                ],
                else_stmt: Some(vec![Stmt::SubCall {
                    fn_name: FullIdent::ident("DoNothing"),
                    args: vec![],
                }]),
            })]
        );
    }

    #[test]
    fn parse_select_with_colon() {
        let input = indoc! {r#"
            Select case serviceLevel:
                case kMenuTop, kMenuNone:
                    bInService=False
            End Select
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::SelectCase {
                test_expr: Box::new(Expr::ident("serviceLevel")),
                cases: vec![Case {
                    tests: vec![Expr::ident("kMenuTop"), Expr::ident("kMenuNone"),],
                    body: vec![Stmt::Assignment {
                        full_ident: FullIdent::ident("bInService"),
                        value: Box::new(Expr::Literal(Lit::Bool(false))),
                    }]
                },],
                else_stmt: None,
            })]
        );
    }

    #[test]
    fn test_parse_is() {
        let input = indoc! {r#"
            If Not Controller Is Nothing Then ' Controller might no be there
                Controller.Run
            End If
        "#};
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        #[rustfmt::skip]
        assert_eq!(
            stmt,
            Stmt::IfStmt {
                condition: Box::new(Expr::PrefixOp {
                    op: T![not],
                    expr: Box::new(Expr::InfixOp {
                        op: T![is],
                        lhs: Box::new(Expr::ident("Controller")),
                        rhs: Box::new(Expr::Literal(Lit::Nothing)),
                    }),
                }),
                body: vec![Stmt::SubCall {
                    fn_name: FullIdent::new(
                        Expr::member(
                            Expr::ident("Controller"),
                            "Run"
                        ),
                    ),
                    args: vec![],
                },],
                elseif_statements: vec![],
                else_stmt: None,
            }
        );
    }

    #[test]
    fn test_sub_call_with_empty_args() {
        let input = r#"DoSomething 1,,"test""#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                args: vec![Some(Expr::int(1)), None, Some(Expr::str("test")),],
            }
        );
    }

    #[test]
    fn inline_class() {
        let input = indoc! {r#"
            Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Class {
                name: "NullFadingObject".to_string(),
                members: vec![],
                dims: vec![],
                member_accessors: vec![MemberAccess {
                    name: "IntensityScale".to_string(),
                    visibility: PropertyVisibility::Public { default: false },
                    property_type: PropertyType::Let,
                    args: vec![("input".to_string(), ArgumentType::ByVal),],
                    body: vec![],
                }],
                methods: vec![],
            }]
        );
    }

    #[test]
    fn class_with_only_members() {
        let input = indoc! {r#"
            Class MyClass
                Public Foo, Bar(9,0) 
                Private Qux(1), Baz
            End Class
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Class {
                name: "MyClass".to_string(),
                members: vec![
                    MemberDefinitions {
                        visibility: Visibility::Public,
                        properties: vec![
                            ("Foo".to_string(), None),
                            ("Bar".to_string(), Some(vec![9, 0])),
                        ],
                    },
                    MemberDefinitions {
                        visibility: Visibility::Private,
                        properties: vec![
                            ("Qux".to_string(), Some(vec![1])),
                            ("Baz".to_string(), None),
                        ],
                    },
                ],
                dims: vec![],
                member_accessors: vec![],
                methods: vec![],
            }]
        );
    }

    #[test]
    fn class_with_only_dims() {
        let input = indoc! {r#"
            Class MyClass
                Dim Foo, Bar(9,0) 
                Dim Qux(1), Baz
            End Class
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Class {
                name: "MyClass".to_string(),
                members: vec![],
                dims: vec![
                    vec![
                        ("Foo".to_string(), None),
                        ("Bar".to_string(), Some(vec![9, 0])),
                    ],
                    vec![
                        ("Qux".to_string(), Some(vec![1])),
                        ("Baz".to_string(), None),
                    ]
                ],
                member_accessors: vec![],
                methods: vec![],
            }]
        );
    }

    #[test]
    fn class_with_subs_mixed() {
        let input = indoc! {r#"
            Class MyClass
                Public Enabled
                Public Sub Class_Initialize
                    Enabled = True
                End Sub
                Private Sub Class_Terminate()
                    'Termination code goes here
                End Sub
            End Class
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Class {
                name: "MyClass".to_string(),
                members: vec![MemberDefinitions {
                    visibility: Visibility::Public,
                    properties: vec![("Enabled".to_string(), None)],
                },],
                dims: vec![],
                member_accessors: vec![],
                methods: vec![
                    Stmt::Sub {
                        visibility: Visibility::Public,
                        name: "Class_Initialize".to_string(),
                        parameters: vec![],
                        body: vec![Stmt::Assignment {
                            full_ident: FullIdent::ident("Enabled"),
                            value: Box::new(Expr::Literal(Lit::Bool(true))),
                        }],
                    },
                    Stmt::Sub {
                        visibility: Visibility::Private,
                        name: "Class_Terminate".to_string(),
                        parameters: vec![],
                        body: vec![],
                    }
                ],
            },]
        );
    }

    #[test]
    fn test_parse_with() {
        let input = indoc! {r#"
            With foo.obj
                .bar = 1
                .baz.z = x
                x = .qux
            End With
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::With {
                object: FullIdent::new(
                    Expr::member(
                        Expr::ident("foo"),
                        "obj"
                    )
                ),
                body: vec![
                    Stmt::Assignment {
                        full_ident: FullIdent::new(
                            Expr::member(
                                Expr::WithScoped,
                                "bar"
                            )
                        ),
                        value: Box::new(Expr::int(1)),
                    },
                    Stmt::Assignment {
                        full_ident: FullIdent::new(
                            Expr::member(
                                Expr::member(
                                    Expr::WithScoped,
                                    "baz"
                                ),
                                "z"
                            )
                        ),
                        value: Box::new(Expr::ident("x")),
                    },
                    Stmt::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(Expr::member(
                            Expr::WithScoped,
                            "qux"
                        )),
                    },
                ],
            })]
        );
    }

    #[test]
    fn test_parse_colon_without_remainder() {
        let input = indoc! { r#"
            Dim LutToggleSoundLevel : 
            LutToggleSoundLevel = 0.5
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![
                Item::Statement(Stmt::Dim {
                    vars: vec![("LutToggleSoundLevel".to_string(), vec![])],
                }),
                Item::Statement(Stmt::Assignment {
                    full_ident: FullIdent::ident("LutToggleSoundLevel"),
                    value: Box::new(Expr::Literal(Lit::Float(0.5))),
                }),
            ]
        );
    }

    #[test]
    fn test_parse_double_array_access() {
        let input = "foo(1)(2) = 3";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Assignment {
                full_ident: FullIdent::new(Expr::fn_application(
                    Expr::fn_application(Expr::ident("foo"), vec![Expr::int(1)]),
                    vec![Expr::int(2)]
                )),
                value: Box::new(Expr::int(3)),
            }
        );
    }

    #[test]
    fn test_parse_call() {
        let input = indoc! { r#"
            Call MyFunction
            Call MyOtherFunction(1, 2)
            Call mQue3(ii)(mQue2(ii))
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![
                Item::Statement(Stmt::Call(FullIdent::ident("MyFunction"))),
                Item::Statement(Stmt::Call(FullIdent::new(
                    Expr::fn_application(
                        Expr::ident("MyOtherFunction"),
                        vec![Expr::int(1), Expr::int(2)]
                    )
                ))),
                Item::Statement(Stmt::Call(FullIdent::new(
                    Expr::fn_application(
                        Expr::fn_application(
                            Expr::ident("mQue3"),
                            vec![Expr::ident("ii")]
                        ),
                        vec![Expr::fn_application(
                            Expr::ident("mQue2"),
                            vec![Expr::ident("ii")]
                        )]
                    )
                ))),
            ]
        );
    }
    #[test]
    fn test_statement_ambiguity_accepted() {
        // These just check what is normally valid and should not panic

        // sub call for object element in multi-dimensional array
        let input = "arr(0,0).s";
        Parser::new(input).statement(true);

        // sub call with single argument being parenthesized expression
        let input = "DoSomething(x)";
        Parser::new(input).statement(true);

        // assignment to multi-dimensional array element
        let input = "arr(0,0) = 1";
        Parser::new(input).statement(true);

        // this is accepted on windows, why?
        let input = "something(0)(0)";
        Parser::new(input).statement(true);

        // deep sub call from within a multidimensional array
        let input = "arr(0,0).DoSomething(x)";
        Parser::new(input).statement(true);

        // sub call with first arg expression
        let input = "DoSomething (x + y) * z";
        Parser::new(input).statement(true);

        // sub call with comma after first arg in brackets
        let input = "DoSomething (x + 1), 2";
        Parser::new(input).statement(true);
    }

    #[test]
    #[should_panic = "Expected end of statement"]
    fn test_statement_expected_end() {
        // Windows: compilation error: Expected end of statement
        // TODO make these tests pass
        let input = "DoSomething(),0";
        Parser::new(input).statement(true);
    }

    #[test]
    fn test_statement_do_not_expect_end() {
        let input = "DoSomething(1),0";
        let stmt = Parser::new(input).statement(true);
        assert_eq!(
            stmt,
            Stmt::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                args: vec![Some(Expr::int(1)), Some(Expr::int(0))],
            }
        );
    }

    #[test]
    #[should_panic = "compilation error: Cannot use parentheses when calling a Sub"]
    fn test_statement_no_parenthesis_when_calling_sub() {
        // compilation error: Cannot use parentheses when calling a Sub
        let input = "DoSomething(0,0)";
        Parser::new(input).statement(true);
    }

    #[test]
    #[should_panic = "compilation error: Cannot use parentheses when calling a Sub"]
    fn test_statement_no_parenthesis_when_calling_deep_sub() {
        // compilation error: Cannot use parentheses when calling a Sub
        let input = "SomeArray(1,2).DoSomething(0,0,3)";
        Parser::new(input).statement(true);
    }

    #[test]
    #[should_panic = "compilation error: Cannot use parentheses when calling a Sub"]
    fn test_statement_no_parenthesis_when_calling_sub_and_invalid_statement() {
        // compilation error: Cannot use parentheses when calling a Sub
        let input = "something(0,0) + 1";
        Parser::new(input).statement(true);
    }

    #[test]
    fn test_parse_statement_sub_call_with_parens() {
        // This is tricky because the parens are used for both function calls, array access
        // but here they are part of the sub call first argument.
        // Would the space in front of the parens make a difference?
        let input = "DoSomething (x + y) * z";

        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                args: vec![Some(Expr::InfixOp {
                    op: T![*],
                    lhs: Box::new(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::ident("y")),
                    }),
                    rhs: Box::new(Expr::ident("z"))
                })],
            },
        );
    }

    #[test]
    fn test_parse_statement_sub_call_with_parens2() {
        // This is tricky because the parens are used for both function calls, array access
        // but here they are part of the sub call first argument.
        // Would the space in front of the parens make a difference?
        let input = "DoSomething z * (x + y)";

        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                args: vec![Some(Expr::InfixOp {
                    op: T![*],
                    lhs: Box::new(Expr::ident("z")),
                    rhs: Box::new(Expr::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::ident("y")),
                    }),
                })],
            },
        );
    }

    #[test]
    fn test_parse_function_or_array_call_with_parens() {
        // This is tricky because the parens are used for both function calls, array access
        let input = "x = AddScore(x + y) * z";

        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::Assignment {
                full_ident: FullIdent::ident("x"),
                value: Box::new(Expr::InfixOp {
                    op: T![*],
                    lhs: Box::new(Expr::FnApplication {
                        callee: Box::new(Expr::ident("AddScore")),
                        args: vec![Some(Expr::InfixOp {
                            op: T![+],
                            lhs: Box::new(Expr::ident("x")),
                            rhs: Box::new(Expr::ident("y")),
                        })],
                    }),
                    rhs: Box::new(Expr::ident("z")),
                }),
            },
        );
    }

    #[test]
    fn parse_dim_call_with_nested_empty_args() {
        let input = r#"MySub MyFn(0, , -1)"#;
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::SubCall {
                fn_name: FullIdent::ident("MySub"),
                args: vec![Some(Expr::FnApplication {
                    callee: Box::new(Expr::ident("MyFn")),
                    args: vec![
                        Some(Expr::int(0)),
                        None,
                        Some(Expr::PrefixOp {
                            op: T![-],
                            expr: Box::new(Expr::int(1)),
                        })
                    ],
                })]
            }
        );
    }

    #[test]
    fn test_parse_function_single_line_with_colons() {
        let input = "Function NullFunction(a) : End Function";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Function {
                visibility: Visibility::Default,
                name: "NullFunction".to_string(),
                parameters: vec![Argument::ByVal("a".to_string())],
                body: vec![],
            })]
        );
    }

    #[test]
    fn test_parse_function_single_line_without_colons() {
        let input = "Function NullFunction(a) End Function";
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::Function {
                visibility: Visibility::Default,
                name: "NullFunction".to_string(),
                parameters: vec![Argument::ByVal("a".to_string())],
                body: vec![],
            })]
        );
    }

    #[test]
    fn test_parse_do_while_loop() {
        let input = indoc! {r#"
            Do While x > 0
                x = x - 1
            Loop
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::DoLoop {
                check: DoLoopCheck::Pre(DoLoopCondition::While(Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::int(0)),
                }))),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::InfixOp {
                        op: T![-],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(1)),
                    }),
                }],
            })]
        );
    }

    #[test]
    fn test_parse_do_until_loop() {
        let input = indoc! {r#"
            Do Until x = 0
                x = x - 1
            Loop
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::DoLoop {
                check: DoLoopCheck::Pre(DoLoopCondition::Until(Box::new(Expr::InfixOp {
                    op: T![=],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::int(0)),
                }))),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::InfixOp {
                        op: T![-],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(1)),
                    }),
                }],
            })]
        );
    }

    #[test]
    fn test_parse_do_loop_while() {
        let input = indoc! {r#"
            Do
                x = x - 1
            Loop While x > 0
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::DoLoop {
                check: DoLoopCheck::Post(DoLoopCondition::While(Box::new(Expr::InfixOp {
                    op: T![>],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::int(0)),
                }))),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::InfixOp {
                        op: T![-],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(1)),
                    }),
                }],
            })]
        );
    }

    #[test]
    fn test_parse_do_loop_until() {
        let input = indoc! {r#"
            Do
                x = x - 1
            Loop Until x = 0
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::DoLoop {
                check: DoLoopCheck::Post(DoLoopCondition::Until(Box::new(Expr::InfixOp {
                    op: T![=],
                    lhs: Box::new(Expr::ident("x")),
                    rhs: Box::new(Expr::int(0)),
                }))),
                body: vec![Stmt::Assignment {
                    full_ident: FullIdent::ident("x"),
                    value: Box::new(Expr::InfixOp {
                        op: T![-],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(1)),
                    }),
                }],
            })]
        );
    }

    #[test]
    fn test_parse_do_loop_exit() {
        let input = indoc! {r#"
            Do
                x = x - 1
                If x = 0 Then Exit Do
            Loop
        "#};
        let mut parser = Parser::new(input);
        let items = parser.file();
        assert_eq!(
            items,
            vec![Item::Statement(Stmt::DoLoop {
                check: DoLoopCheck::None,
                body: vec![
                    Stmt::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(Expr::InfixOp {
                            op: T![-],
                            lhs: Box::new(Expr::ident("x")),
                            rhs: Box::new(Expr::int(1)),
                        }),
                    },
                    Stmt::IfStmt {
                        condition: Box::new(Expr::InfixOp {
                            op: T![=],
                            lhs: Box::new(Expr::ident("x")),
                            rhs: Box::new(Expr::int(0)),
                        }),
                        body: vec![Stmt::ExitDo],
                        elseif_statements: vec![],
                        else_stmt: None,
                    },
                ],
            })]
        );
    }

    #[test]
    fn test_property_access_in_args_without_space() {
        let input = "Foo 1,.enabled";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::SubCall {
                fn_name: FullIdent::ident("Foo"),
                args: vec![
                    Some(Expr::int(1)),
                    Some(Expr::member(Expr::WithScoped, "enabled")),
                ],
            }
        );
    }

    #[test]
    fn test_step_as_identifier() {
        let input = "x=y.Step";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        #[rustfmt::skip]
        assert_eq!(
            stmt,
            Stmt::assignment (
                FullIdent::ident("x"),
                Expr::member(
                    Expr::ident("y"), 
                    "Step"
                )
            )
        );
    }

    #[test]
    fn test_error_as_identifier() {
        let input = "Dim Error: Error = 42";
        let mut parser = Parser::new(input);
        let file = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                Item::Statement(Stmt::Dim {
                    vars: vec![("Error".to_string(), vec![])],
                }),
                Item::Statement(Stmt::Assignment {
                    full_ident: FullIdent::ident("Error"),
                    value: Box::new(Expr::int(42)),
                }),
            ]
        );
    }

    #[test]
    fn test_end_sub_where_end_if_optional() {
        let input = indoc! {r#"
            Sub MySub
                if true Then x = 1:
            end Sub
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                Item::Statement(
                    Stmt::Sub {
                        visibility: Visibility::Default,
                        name: "MySub".to_string(),
                        parameters: vec![],
                        body: vec![
                            Stmt::IfStmt {
                                condition: Box::new(Expr::bool(true)),
                                body: vec![
                                    Stmt::Assignment {
                                        full_ident: FullIdent::ident("x"),
                                        value: Box::new(Expr::int(1)),
                                    },
                                ],
                                elseif_statements: vec![],
                                else_stmt: None,
                            },
                        ],
                    }
                )
            ]
        );
    }

    #[test]
    #[should_panic = "2:27 Expected to consume `if`, but found `sub`"]
    fn parse_fail_when_end_sub_not_on_newline() {
        // Windows: compilation error: Expected 'If'
        let input = indoc! {r#"
            Sub MySub
                if true Then x = 1:end Sub
        "#};
        Parser::new(input).file();
    }

    #[test]
    fn parse_class_named_property() {
        let input = indoc! {r#"
            Class Property
                Sub Property(byref property)
                End Sub
            End Class
            Class Property2
                Function Property()
                End Function
            End Class
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                Item::Class{
                    name: "Property".to_string(),
                    members: vec![],
                    dims: vec![],
                    member_accessors: vec![],
                    methods: vec![
                        Stmt::Sub {
                            visibility: Visibility::Default,
                            name: "Property".to_string(),
                            parameters: vec![Argument::ByRef("property".to_string())],
                            body: vec![],
                        },
                    ],
                },
                Item::Class{
                    name: "Property2".to_string(),
                    members: vec![],
                    dims: vec![],
                    member_accessors: vec![],
                    methods: vec![
                        Stmt::Function {
                            visibility: Visibility::Default,
                            name: "Property".to_string(),
                            parameters: vec![],
                            body: vec![],
                        },
                    ],
                },
            ]
        );
    }

    #[test]
    #[should_panic = "Name redefined"]
    fn parse_fail_duplicate() {
        let input = indoc! {r#"
            Class Property
                Dim dUpE
                Sub dupe
                End Sub
                Function Dupe
                End Function
            End Class
        "#};
        Parser::new(input).file();
    }

    #[test]
    fn parse_allowed_keyword_identifiers() {
        let input = indoc! {r#"
            Dim default
            default = 1
            Dim error
            error = 2
            Dim explicit
            explicit = 3
            Dim step
            step = 4
            Dim property
            property = 5
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                Item::Statement(Stmt::Dim {
                    vars: vec![("default".to_string(), vec![])],
                }),
                Item::Statement(Stmt::Assignment {
                    full_ident: FullIdent::ident("default"),
                    value: Box::new(Expr::int(1)),
                }),
                Item::Statement(Stmt::Dim {
                    vars: vec![("error".to_string(), vec![])],
                }),
                Item::Statement(Stmt::Assignment {
                    full_ident: FullIdent::ident("error"),
                    value: Box::new(Expr::int(2)),
                }),
                Item::Statement(Stmt::Dim {
                    vars: vec![("explicit".to_string(), vec![])],
                }),
                Item::Statement(Stmt::Assignment {
                    full_ident: FullIdent::ident("explicit"),
                    value: Box::new(Expr::int(3)),
                }),
                Item::Statement(Stmt::Dim {
                    vars: vec![("step".to_string(), vec![])],
                }),
                Item::Statement(Stmt::Assignment {
                    full_ident: FullIdent::ident("step"),
                    value: Box::new(Expr::int(4)),
                }),
                Item::Statement(Stmt::Dim {
                    vars: vec![("property".to_string(), vec![])],
                }),
                Item::Statement(Stmt::Assignment {
                    full_ident: FullIdent::ident("property"),
                    value: Box::new(Expr::int(5)),
                }),
            ]
        );
    }

    #[test]
    fn parse_call_with_keyword_identifier() {
        let input = indoc! {r#"
            Call ok(error = "xx", "error = " & error & " expected ""xx""")
        "#};
        let mut parser = Parser::new(input);
        let file = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                Item::Statement(Stmt::Call(FullIdent::new(
                    Expr::fn_application(
                        Expr::ident("ok"),
                        vec![
                            Expr::InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("error")),
                                rhs: Box::new(Expr::str("xx")),
                            },
                            Expr::InfixOp {
                                op: T![&],
                                lhs: Box::new(Expr::InfixOp {
                                    op: T![&],
                                    lhs: Box::new(Expr::str("error = ")),
                                    rhs: Box::new(Expr::ident("error")),
                                }),
                                rhs: Box::new(Expr::str(" expected \"xx\"")),
                            },
                        ]
                    )
                ))),
            ]
        );
    }

    #[test]
    fn parse_member_access_with_rem() {
        // this is a special case because `rem` is normally used for comments
        let input = indoc! {r#"
            rem this is a comment!
            obj.rem = 10
        "#};
        let mut parser = Parser::new(input);
        // read all tokens seen from the parser
        let mut tokens = vec![];
        while let Some(next) = parser.next() {
            tokens.push(next.kind);
        }
        assert_eq!(
            tokens,
            vec![
                T![ident],
                T![_.],
                T![rem],
                T![=],
                T![integer_literal],
                T![nl],
                T![EOF],
            ]
        );
    }

    #[test]
    fn parse_member_access_with_keywords() {
        // If an object is created outside vbscript and passed in,
        // it can have all kinds of properties and methods that are not
        // valid in vbscript. But these are still accessible using dot notation.
        let input = indoc! {r#"
            obj.rem = 10
            obj.true = 10
            obj.false = 10
            obj.not = 10
            obj.and = 10
            obj.or = 10
            obj.xor = 10
            obj.eqv = 10
            obj.imp = 10
            obj.is = 10
            obj.mod = 10
            obj.call = 10
            obj.dim = 10
            obj.sub = 10
            obj.function = 10
            obj.get = 10
            obj.let = 10
            obj.const = 10
            obj.if = 10
            obj.else = 10
            obj.elseif = 10
            obj.end = 10
            obj.then = 10
            obj.exit = 10
            obj.while = 10
            obj.wend = 10
            obj.do = 10
            obj.loop = 10
            obj.until = 10
            obj.for = 10
            obj.to = 10
            obj.each = 10
            obj.in = 10
            obj.select = 10
            obj.case = 10
            obj.byref = 10
            obj.byval = 10
            obj.option = 10
            obj.nothing = 10
            obj.empty = 10
            obj.null = 10
            obj.class = 10
            obj.set = 10
            obj.new = 10
            obj.public = 10
            obj.private = 10
            obj.next = 10
            obj.on = 10
            obj.resume = 10
            obj.goto = 10
            obj.with = 10
            obj.redim = 10
            obj.preserve = 10
            obj.property = 10
            obj.me = 10
            obj.stop = 10
            obj.step = 10
        "#};

        fn member_assign(name: &str) -> Item {
            Item::Statement(Stmt::Assignment {
                full_ident: FullIdent::new(Expr::member(Expr::ident("obj"), name)),
                value: Box::new(Expr::int(10)),
            })
        }

        let mut parser = Parser::new(input);
        let file = parser.file();
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                member_assign("rem"),
                member_assign("true"),
                member_assign("false"),
                member_assign("not"),
                member_assign("and"),
                member_assign("or"),
                member_assign("xor"),
                member_assign("eqv"),
                member_assign("imp"),
                member_assign("is"),
                member_assign("mod"),
                member_assign("call"),
                member_assign("dim"),
                member_assign("sub"),
                member_assign("function"),
                member_assign("get"),
                member_assign("let"),
                member_assign("const"),
                member_assign("if"),
                member_assign("else"),
                member_assign("elseif"),
                member_assign("end"),
                member_assign("then"),
                member_assign("exit"),
                member_assign("while"),
                member_assign("wend"),
                member_assign("do"),
                member_assign("loop"),
                member_assign("until"),
                member_assign("for"),
                member_assign("to"),
                member_assign("each"),
                member_assign("in"),
                member_assign("select"),
                member_assign("case"),
                member_assign("byref"),
                member_assign("byval"),
                member_assign("option"),
                member_assign("nothing"),
                member_assign("empty"),
                member_assign("null"),
                member_assign("class"),
                member_assign("set"),
                member_assign("new"),
                member_assign("public"),
                member_assign("private"),
                member_assign("next"),
                member_assign("on"),
                member_assign("resume"),
                member_assign("goto"),
                member_assign("with"),
                member_assign("redim"),
                member_assign("preserve"),
                member_assign("property"),
                member_assign("me"),
                member_assign("stop"),
                member_assign("step"),
            ]
        );
    }

    #[test]
    fn sub_with_member_check() {
        let input = "MySub \"test\"&.prop";
        let mut parser = Parser::new(input);
        let stmt = parser.statement(true);
        assert_eq!(
            stmt,
            Stmt::SubCall {
                fn_name: FullIdent::ident("MySub"),
                args: vec![Some(InfixOp {
                    op: T![&],
                    lhs: Box::new(Expr::str("test")),
                    rhs: Box::new(Expr::member(Expr::WithScoped, "prop")),
                })],
            }
        );
    }
}
