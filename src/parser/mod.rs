use crate::parser::ast::Spanned;
use crate::{T, lexer::*};
use std::fmt::{Debug, Display};
use std::iter::Peekable;

pub mod ast;
mod expressions;
mod hierarchy;
pub mod visit;

/// Why a script could not be parsed, and where.
///
/// The line and the column start at 1, 0 stands for unknown. The column counts bytes, like
/// the one of a [`Token`].
#[derive(Clone, PartialEq, Eq)]
pub struct ParseError {
    message: String,
    line: usize,
    column: usize,
}

impl ParseError {
    pub fn new<S: Into<String>>(message: S, line: usize, column: usize) -> Self {
        Self {
            message: message.into(),
            line,
            column,
        }
    }

    /// Description of what went wrong, without the position.
    pub fn message(&self) -> &str {
        &self.message
    }

    /// 1-indexed, 0 means unknown
    pub fn line(&self) -> usize {
        self.line
    }

    /// 1-indexed, 0 means unknown
    pub fn column(&self) -> usize {
        self.column
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "line {}, column {}: {}",
            self.line, self.column, self.message
        )
    }
}

impl std::error::Error for ParseError {}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ParseError at line {}, column {}: {}",
            self.line, self.column, self.message
        )
    }
}

/// Maximum nesting depth of expressions and statements.
///
/// The parser is recursive, so without a limit a deeply nested input like `((((...))))`
/// overflows the stack, which aborts the process instead of returning an error.
///
/// A level of nested `If` blocks takes about 12 KiB of stack in a debug build, so this
/// fits the 2 MiB default stack of a Rust thread. Real scripts stay far below it: the
/// deepest nesting in the test corpus is 26.
const MAX_NESTING_DEPTH: usize = 128;

/// A parser for VBScript, see the [documentation of the crate](crate) for an example.
///
/// [`file`](Parser::file) parses a whole script. The other public functions parse a part of
/// one, like a single [`statement`](Parser::statement) or
/// [`expression`](Parser::expression), from where the parser is in the input.
pub struct Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    input: &'input str,
    tokens: Peekable<I>,
    /// Current nesting depth of expressions and statements, see [`MAX_NESTING_DEPTH`].
    depth: usize,
    /// Line and column of the last token that was consumed, (0, 0) if there was none yet.
    last_position: (usize, usize),
    /// Offset of the end of the last token that was consumed.
    last_end: u32,
    /// How many subs, functions or properties we are in the body of.
    procedure_depth: usize,
}

impl<'input> Parser<'input, TokenIter<'input>> {
    /// A parser for a script, or for a part of one.
    pub fn new(input: &'input str) -> Parser<'input, TokenIter<'input>> {
        Parser {
            input,
            tokens: TokenIter::new(input).peekable(),
            depth: 0,
            last_position: (0, 0),
            last_end: 0,
            procedure_depth: 0,
        }
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    /// Register one more level of nesting, fails if the input is nested too deep.
    pub(crate) fn enter_nested(&mut self) -> Result<(), ParseError> {
        if self.depth >= MAX_NESTING_DEPTH {
            let (line, column) = self
                .tokens
                .peek()
                .map_or((0, 0), |token| (token.line, token.column));
            return Err(ParseError::new(
                format!("Nesting deeper than {MAX_NESTING_DEPTH} levels is not supported"),
                line,
                column,
            ));
        }
        self.depth += 1;
        Ok(())
    }

    pub(crate) fn leave_nested(&mut self) {
        self.depth -= 1;
    }

    /// The text of a token as a name, with the span of the token.
    pub(crate) fn name(&self, token: &Token) -> ast::Name {
        Spanned::with_span(self.text(token).to_string(), token.span)
    }

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

    pub(crate) fn peek_full(&mut self) -> Result<&Token, ParseError> {
        let (line, column) = self.last_position;
        match self.tokens.peek() {
            Some(token) => Ok(token),
            None => Err(ParseError::new(
                "Expected a token, but found EOF",
                line,
                column,
            )),
        }
    }

    /// Check if the next token is some `kind` of token.
    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.peek() == kind
    }

    /// Get the next token.
    pub(crate) fn next(&mut self) -> Option<Token> {
        let token = self.tokens.next();
        if let Some(token) = &token {
            self.last_position = (token.line, token.column);
            // a node does not include the delimiter that follows it
            if !matches!(token.kind, T![nl] | T![:]) {
                self.last_end = token.span.end;
            }
        }
        token
    }

    /// The offset where the next token starts, to be used as the start of the span of the
    /// node we are about to parse.
    pub(crate) fn start(&mut self) -> u32 {
        self.tokens
            .peek()
            .map_or(self.last_end, |token| token.span.start)
    }

    /// A node that spans from `start` to the end of the last token that was consumed.
    pub(crate) fn spanned<T>(&self, node: T, start: u32) -> Spanned<T> {
        let span = Span {
            start,
            end: self.last_end.max(start),
        };
        Spanned::with_span(node, span)
    }

    /// An error for when there are no tokens left, positioned at the last token we have seen
    /// as that is the end of the input.
    pub(crate) fn end_of_input_error(&self, message: impl Into<String>) -> ParseError {
        let (line, column) = self.last_position;
        ParseError::new(message, line, column)
    }

    /// Move forward one token in the input and check
    /// that we pass the kind of token we expect.
    pub(crate) fn consume(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        let token = match self.next() {
            Some(token) => token,
            None => {
                return Err(self.end_of_input_error(format!(
                    "Expected to consume `{expected}`, but there was no next token"
                )));
            }
        };
        if token.kind != expected {
            return Err(ParseError::new(
                format!(
                    "Expected to consume `{}`, but found `{}`",
                    expected, token.kind
                ),
                token.line,
                token.column,
            ));
        }
        Ok(token)
    }

    pub(crate) fn consume_line_delimiter(&mut self) -> Result<(), ParseError> {
        let peek = self.peek_full()?;
        match peek.kind {
            T![EOF] => {}
            T![end] => {}
            T![nl] => {
                self.consume(T![nl])?;
            }
            T![:] => {
                self.consume(T![:])?;
                // if there is a newline directly after the colon, consume it
                if self.at(T![nl]) {
                    self.consume(T![nl])?;
                }
            }
            other => {
                return Err(ParseError::new(
                    format!("Unexpected token Expected newline or colon, but found {other}"),
                    peek.line,
                    peek.column,
                ));
            }
        };
        Ok(())
    }

    fn consume_optional_line_delimiter(&mut self) -> Result<(), ParseError> {
        if matches!(self.peek(), T![nl] | T![:]) {
            self.consume_line_delimiter()
        } else {
            Ok(())
        }
    }

    pub(crate) fn at_new_line_or_eof(&mut self) -> bool {
        matches!(self.peek(), T![nl] | T![:] | T![EOF])
    }
}

/// Iterator over the tokens of the lexer, filtering out whitespace, empty lines and comments.
pub struct TokenIter<'input> {
    lexer: Lexer<'input>,
    prev_token_kind: TokenKind,
}

impl<'input> TokenIter<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            lexer: Lexer::new(input),
            prev_token_kind: T![nl],
        }
    }
}

impl Iterator for TokenIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let current_token = self.lexer.next()?;

            // ignore whitespace
            if matches!(current_token.kind, T![ws]) {
                continue;
            }
            if matches!(current_token.kind, T![line_continuation]) {
                // the lexer already consumes the newline
                continue;
            }
            // skip empty lines
            if matches!(self.prev_token_kind, T![nl]) && matches!(current_token.kind, T![nl]) {
                self.prev_token_kind = current_token.kind;
                continue;
            }
            // skip empty inline lines
            if matches!(self.prev_token_kind, T![:] | T![nl]) && matches!(current_token.kind, T![:])
            {
                self.prev_token_kind = current_token.kind;
                continue;
            }
            // skip single line comments that are preceded by a newline
            if matches!(self.prev_token_kind, T![nl]) && matches!(current_token.kind, T![comment]) {
                // hacky way to not keep the comment newline
                self.prev_token_kind = T![nl];
                continue;
            }
            self.prev_token_kind = current_token.kind;
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
    use crate::parser::ast::ExprKind::{InfixOp, WithScoped};
    use crate::parser::ast::StmtKind::OnError;
    use crate::parser::ast::{
        Argument, ArgumentType, Case, DoLoopCheck, DoLoopCondition, Expr, ExprKind, FullIdent,
        Item, ItemKind, Lit, MemberAccess, MemberDefinitions, PropertyType, PropertyVisibility,
        ReDimVar, SetRhs, Stmt, StmtKind, VarDecl, Visibility,
    };
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn parse(input: &str) -> Expr {
        let mut parser = Parser::new(input);
        parser.expression().unwrap()
    }

    fn parse_file(input: &str) -> Vec<Item> {
        let mut parser = Parser::new(input);
        parser.file().unwrap()
    }

    fn parse_stmt(input: &str, consume_line_delimiter: bool) -> Stmt {
        let mut parser = Parser::new(input);
        parser.statement(consume_line_delimiter).unwrap()
    }

    #[test]
    fn test_line_continuations() {
        // mind the trailing spaces
        let input = indoc! {r#"
            Dim x _
             _
                , y
        "#};
        let file = parse_file(input);
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(
                    StmtKind::Dim {
                        vars: vec![VarDecl::new("x"), VarDecl::new("y"),],
                    }
                    .into()
                )
                .into(),
            ]
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
        assert_eq!(expr, ExprKind::Literal(Lit::Float(2.7768)).into());
        let expr = parse(r#""I am a String!""#);
        assert_eq!(expr, Expr::str("I am a String!".to_string()));
        let expr = parse("foo");
        assert_eq!(expr, Expr::ident("foo"));
        let expr = parse("bar (  x, 2)");
        assert_eq!(
            expr,
            ExprKind::FnApplication {
                callee: Box::new(Expr::ident("bar")),
                args: vec![Some(Expr::ident("x")), Some(Expr::int(2))],
            }
            .into()
        );
        let expr = parse("Not is_visible");
        assert_eq!(
            expr,
            ExprKind::PrefixOp {
                op: T![not],
                expr: Box::new(Expr::ident("is_visible")),
            }
            .into()
        );
        let expr = parse("(-13)");
        assert_eq!(
            expr,
            Expr::paren(
                ExprKind::PrefixOp {
                    op: T![-],
                    expr: Box::new(Expr::int(13)),
                }
                .into()
            )
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
            ExprKind::FnApplication {
                callee: Box::new(Expr::ident("min")),
                args: vec![
                    Some(
                        InfixOp {
                            op: T![+],
                            lhs: Box::new(Expr::ident("test")),
                            rhs: Box::new(Expr::int(4)),
                        }
                        .into()
                    ),
                    Some(
                        ExprKind::FnApplication {
                            callee: Box::new(Expr::ident("sin")),
                            args: vec![Some(
                                InfixOp {
                                    op: T![*],
                                    lhs: Box::new(Expr::int(2)),
                                    rhs: Box::new(Expr::ident("PI")),
                                }
                                .into()
                            )],
                        }
                        .into()
                    ),
                ],
            }
            .into()
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
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(
                    InfixOp {
                        op: T![>],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(2)),
                    }
                    .into()
                ),
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(Expr::int(4)),
                    }
                    .into()
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
            .into()
        );
    }

    #[test]
    fn parse_simple_while() {
        let input = indoc! {r#"
            While x < 5
                x=x+1
            Wend
        "#};
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::WhileStmt {
                condition: Box::new(
                    InfixOp {
                        op: T![<],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(5)),
                    }
                    .into()
                ),
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(
                            InfixOp {
                                op: T![+],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(1)),
                            }
                            .into()
                        ),
                    }
                    .into(),
                ],
            }
            .into()
        );
    }

    #[test]
    fn test_while_single_line() {
        let input = r#"while skipIdx = skipIdx2:skipIdx2=Int(Rnd*6):wend"#;
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::WhileStmt {
                        condition: Box::new(
                            InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("skipIdx")),
                                rhs: Box::new(Expr::ident("skipIdx2")),
                            }
                            .into()
                        ),
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("skipIdx2"),
                                value: Box::new(
                                    ExprKind::FnApplication {
                                        callee: Box::new(Expr::ident("Int")),
                                        args: vec![Some(
                                            InfixOp {
                                                op: T![*],
                                                lhs: Box::new(Expr::ident("Rnd")),
                                                rhs: Box::new(Expr::int(6)),
                                            }
                                            .into()
                                        )],
                                    }
                                    .into()
                                ),
                            }
                            .into(),
                        ],
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn parse_simple_for_next_loop() {
        let input = indoc! {r#"
            For i = 1 to 10
                x = x + i
            Next
        "#};
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::ForStmt {
                counter: "i".into(),
                start: Box::new(Expr::int(1)),
                end: Box::new(Expr::int(10)),
                step: None,
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(
                            InfixOp {
                                op: T![+],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::ident("i")),
                            }
                            .into()
                        ),
                    }
                    .into()
                ],
            }
            .into()
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
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::ForStmt {
                counter: "i".into(),
                start: Box::new(Expr::int(1)),
                end: Box::new(Expr::int(10)),
                step: None,
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(
                            InfixOp {
                                op: T![*],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::ident("i")),
                            }
                            .into()
                        ),
                    }
                    .into(),
                    StmtKind::IfStmt {
                        condition: Box::new(
                            InfixOp {
                                op: T![>],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(10)),
                            }
                            .into()
                        ),
                        body: vec![StmtKind::ExitFor.into()],
                        elseif_statements: vec![],
                        else_stmt: None,
                    }
                    .into(),
                ],
            }
            .into()
        );
    }

    #[test]
    fn parse_for_step() {
        let input = indoc! {r#"
            For i = For_nr to Next_nr step Bdir
                ' do nothing
            Next
        "#};
        let file = parse_file(input);
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(
                    StmtKind::ForStmt {
                        counter: "i".into(),
                        start: Box::new(Expr::ident("For_nr")),
                        end: Box::new(Expr::ident("Next_nr")),
                        step: Some(Box::new(Expr::ident("Bdir"))),
                        body: vec![],
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn parse_for_inline() {
        let input = "For x = 1 To PlayerMode(currentplayer)+1 : Blink(x,1)=1 : Next";
        let stmt = parse_stmt(input, true);
        #[rustfmt::skip]
        assert_eq!(
            stmt,
            StmtKind::ForStmt {
                counter: "x".into(),
                start: Box::new(Expr::int(1)),
                end: Box::new(InfixOp {
                    op: T![+],
                    lhs: Box::new(Expr::fn_application(
                        Expr::ident("PlayerMode"),
                        vec![Expr::ident("currentplayer")])
                    ),
                    rhs: Box::new(Expr::int(1)),
                }.into()),
                step: None,
                body: vec![StmtKind::Assignment {
                    full_ident: FullIdent::new(Expr::fn_application(
                        Expr::ident("Blink"),
                        vec![Expr::ident("x"), Expr::int(1)],
                    )),
                    value: Box::new(Expr::int(1)),
                }.into(),],
            }.into()
        );
    }

    #[test]
    fn parse_foreach_multiline() {
        let input = indoc! {r#"
            For each dog in dogs
                dog.visible = true
            Next
        "#};
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::ForEachStmt {
                element: "dog".into(),
                group: Box::new(Expr::ident("dogs")),
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent(Box::new(
                            ExprKind::MemberExpression {
                                base: Box::new(Expr::ident("dog")),
                                property: "visible".into(),
                            }
                            .into()
                        )),
                        value: Box::new(ExprKind::Literal(Lit::Bool(true)).into()),
                    }
                    .into()
                ],
            }
            .into()
        );
    }

    #[test]
    fn parse_foreach_inline() {
        let input = indoc! {r#"
            For each dog in dogs : dog.volume = 0 : dog.visible = true : Next
        "#};
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::ForEachStmt {
                element: "dog".into(),
                group: Box::new(Expr::ident("dogs")),
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::new(Expr::member(Expr::ident("dog"), "volume"),),
                        value: Box::new(Expr::int(0)),
                    }
                    .into(),
                    StmtKind::Assignment {
                        full_ident: FullIdent::new(Expr::member(Expr::ident("dog"), "visible"),),
                        value: Box::new(ExprKind::Literal(Lit::Bool(true)).into()),
                    }
                    .into(),
                ],
            }
            .into()
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
        let item = parser.item().unwrap();
        assert_eq!(
            item,
            ItemKind::Statement(
                StmtKind::Function {
                    visibility: Visibility::Default,
                    name: "add".into(),
                    parameters: vec![Argument::ByRef("a".into()), Argument::ByRef("b".into())],
                    body: vec![
                        StmtKind::Assignment {
                            full_ident: FullIdent::ident("add"),
                            value: Box::new(
                                InfixOp {
                                    op: T![+],
                                    lhs: Box::new(Expr::ident("a")),
                                    rhs: Box::new(Expr::ident("b")),
                                }
                                .into()
                            ),
                        }
                        .into()
                    ],
                }
                .into()
            )
            .into()
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
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Sub {
                visibility: Visibility::Default,
                name: "log".into(),
                parameters: vec![Argument::ByRef("a".into()), Argument::ByRef("b".into())],
                body: vec![],
            }
            .into()
        );
    }

    /// A syntax error for `cscript` on Windows.
    #[test]
    fn test_procedure_in_a_procedure() {
        for input in [
            "Sub Outer\nSub Inner()\nEnd Sub\nEnd Sub",
            "Sub Outer\nFunction Inner()\nEnd Function\nEnd Sub",
            "Function Outer()\nSub Inner()\nEnd Sub\nEnd Function",
            "Sub Outer\nIf a Then\nPublic Sub Inner()\nEnd Sub\nEnd If\nEnd Sub",
            "Class C\nSub Outer\nSub Inner()\nEnd Sub\nEnd Sub\nEnd Class",
            "Class C\nProperty Get P\nFunction Inner()\nEnd Function\nEnd Property\nEnd Class",
        ] {
            let error = Parser::new(input).file().unwrap_err();
            assert!(
                error
                    .message()
                    .contains("can not be declared in a procedure"),
                "{input}: {error}"
            );
        }
        // at script level a sub can be declared in a block
        let result =
            Parser::new("If a Then\nSub Inner()\nEnd Sub\nEnd If\nSub Next1()\nEnd Sub").file();
        assert!(result.is_ok(), "{result:?}");
    }

    #[test]
    fn parse_empty_sub_without_args() {
        let input = indoc! {r#"
            private Sub log
            End Sub
        "#};
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Sub {
                visibility: Visibility::Private,
                name: "log".into(),
                parameters: vec![],
                body: vec![],
            }
            .into()
        );
    }

    #[test]
    fn parse_inline_sub_declaration() {
        let input = "Sub Trigger003_hit : RampWireRight.x = 0.1 : Light030.state = 0 : End Sub";
        let stmt = parse_stmt(input, true);
        #[rustfmt::skip]
        assert_eq!(
            stmt,
            StmtKind::Sub {
                visibility: Visibility::Default,
                name: "Trigger003_hit".into(),
                parameters: vec![],
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::new(
                            Expr::member(Expr::ident("RampWireRight"), "x"),
                        ),
                        value: Box::new(ExprKind::Literal(Lit::Float(0.1)).into()),
                    }.into(),
                    StmtKind::Assignment {
                        full_ident: FullIdent::new(
                            Expr::member(Expr::ident("Light030"), "state"),
                        ),
                        value: Box::new(Expr::int(0)),
                    }.into(),
                ],
            }.into()
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
        let file = parse_file(input);
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(
                    StmtKind::IfStmt {
                        condition: Box::new(ExprKind::Literal(Lit::Bool(true)).into()),
                        body: vec![
                            StmtKind::Sub {
                                visibility: Visibility::Default,
                                name: "inner".into(),
                                parameters: vec![],
                                body: vec![],
                            }
                            .into()
                        ],
                        elseif_statements: vec![],
                        else_stmt: None,
                    }
                    .into()
                )
                .into()
            ],
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
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Sub {
                visibility: Visibility::Default,
                name: "Trigger1_Hit".into(),
                parameters: vec![],
                body: vec![
                    StmtKind::IfStmt {
                        condition: Box::new(
                            InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("BIP")),
                                rhs: Box::new(Expr::int(1)),
                            }
                            .into()
                        ),
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("BIP"),
                                value: Box::new(Expr::int(0)),
                            }
                            .into()
                        ],
                        elseif_statements: vec![],
                        else_stmt: None,
                    }
                    .into(),
                ],
            }
            .into()
        );
    }

    #[test]
    fn parse_sub_with_array_argument() {
        let input = indoc! {r#"
            Sub test (byref a())
                'print a
            End Sub
        "#};
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Sub {
                visibility: Visibility::Default,
                name: "test".into(),
                parameters: vec![Argument::ByRef("a".into())],
                body: vec![],
            }
            .into()
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
        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::Statement(
                    StmtKind::Sub {
                        visibility: Visibility::Default,
                        name: "test".into(),
                        parameters: vec![Argument::ByRef("a".into())],
                        body: vec![],
                    }
                    .into()
                )
                .into(),
                ItemKind::Statement(
                    StmtKind::Function {
                        visibility: Visibility::Default,
                        name: "test2".into(),
                        parameters: vec![Argument::ByVal("a".into())],
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("test2"),
                                value: Box::new(Expr::ident("a")),
                            }
                            .into()
                        ],
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn test_parse_file_empty() {
        let input = "";
        let all = parse_file(input);
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_newlines() {
        let input = "\r\n\n\n\r\n";
        let all = parse_file(input);
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_colons() {
        let input = ":: ::";
        let all = parse_file(input);
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_colon_and_newline() {
        let input = ":\r\n";
        let all = parse_file(input);
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_empty_with_comments() {
        let input = indoc! {"
            ' This is a comment

            REM This is a rem comment

            ' This is another comment without newline"};
        let all = parse_file(input);
        assert_eq!(all, vec![]);
    }

    #[test]
    fn test_parse_file_statement_with_trailing_comment() {
        let input = indoc! {"
            Option Explicit ' Force explicit variable declaration.
            ' This is another comment"};
        let all = parse_file(input);
        assert_eq!(all, vec![ItemKind::OptionExplicit.into(),]);
    }

    #[test]
    fn parse_option_and_randomize_on_single_line() {
        let input = "Option Explicit : Randomize";
        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::OptionExplicit.into(),
                ItemKind::Statement(
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("Randomize"),
                        args: vec![],
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn test_parse_no_arg_sub_call() {
        let input = "SayHello";
        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::Statement(
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("SayHello"),
                        args: vec![],
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn test_parse_sub_call() {
        let input = indoc! {"
            test
            test 1
            test 1, 2"
        };
        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::Statement(
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("test"),
                        args: vec![],
                    }
                    .into()
                )
                .into(),
                ItemKind::Statement(
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("test"),
                        args: vec![Some(Expr::int(1))],
                    }
                    .into()
                )
                .into(),
                ItemKind::Statement(
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("test"),
                        args: vec![Some(Expr::int(1)), Some(Expr::int(2))],
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn test_parse_error_handling() {
        let input = indoc! {"
            On Error Resume Next
            On Error GoTo 0
        "};

        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::Statement(
                    OnError {
                        error_clause: ResumeNext
                    }
                    .into()
                )
                .into(),
                ItemKind::Statement(
                    OnError {
                        error_clause: Goto0
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn test_invalid_input_is_an_error_instead_of_a_panic() {
        for (input, expected) in [
            ("Option Foo", "Expected `explicit` after `option`"),
            ("On Error Foo", "Expected `resume next` or `goto 0`"),
            // more than 32 bits is a syntax error on Windows
            ("x = &HFFFFFFFFFFFFFFFFFF", "Invalid hex integer literal"),
            (
                "x = &O7777777777777777777777777",
                "Invalid octal integer literal",
            ),
            (
                "Class C\nPublic a\nPublic Sub A()\nEnd Sub\nEnd Class",
                "Name redefined 'A'",
            ),
            (
                "Class C\nPublic a(99999999999999999999999)\nEnd Class",
                "Expected integer literal as bound",
            ),
        ] {
            let error = Parser::new(input).file().unwrap_err();
            assert!(
                format!("{error:?}").contains(expected),
                "{input}: {error:?}"
            );
        }
    }

    #[test]
    fn test_nesting_too_deep_is_an_error_instead_of_a_stack_overflow() {
        let inputs = [
            format!("x = {}1{}", "(".repeat(100_000), ")".repeat(100_000)),
            format!("x = {}1", "Not ".repeat(100_000)),
            format!("{}x = 1\n", "If a Then\n".repeat(100_000)),
        ];
        for input in inputs {
            let error = Parser::new(&input).file().unwrap_err();
            assert!(format!("{error:?}").contains("Nesting deeper than"));
        }
    }

    #[test]
    fn test_nesting_below_the_limit() {
        let input = format!("x = {}1{}", "(".repeat(100), ")".repeat(100));
        assert!(Parser::new(&input).file().is_ok());
        let input = format!(
            "{}x = 1\n{}",
            "If a Then\n".repeat(100),
            "End If\n".repeat(100)
        );
        assert!(Parser::new(&input).file().is_ok());
    }

    /// Expected values are the output of `cscript` on Windows.
    #[test]
    fn test_hex_and_octal_literal_values() {
        for (input, expected) in [
            ("&H7FFF", 32767),
            ("&H8000", -32768),
            ("&HFFFF", -1),
            ("&HFFFF&", 65535),
            ("&H8000&", 32768),
            ("&H10000", 65536),
            ("&H7FFFFFFF", 2147483647),
            ("&H80000000", -2147483648),
            ("&HFFFFFFFF", -1),
            ("&HFFFFFFFF&", -1),
            ("&H0FFFFFFFF", -1),
            ("&H00000000F", 15),
            ("&O77777", 32767),
            ("&O100000", -32768),
            ("&O177777", -1),
            ("&O177777&", 65535),
            ("&O200000", 65536),
            ("&O17777777777", 2147483647),
            ("&O20000000000", -2147483648),
            ("&O37777777777", -1),
            ("&O037777777777", -1),
            ("&177777", -1),
        ] {
            assert_eq!(parse(input), Expr::int(expected), "{input}");
        }
        for input in ["x = &H100000000", "x = &O40000000000"] {
            assert!(Parser::new(input).file().is_err(), "{input}");
        }
    }

    /// What `cscript` on Windows takes as a date literal, and what it does not.
    #[test]
    fn test_date_literal_forms() {
        for (literal, text) in [
            ("#1/2/2020#", "1/2/2020"),
            ("# 1/2/2020 #", " 1/2/2020 "),
            ("#2020-01-02#", "2020-01-02"),
            ("#1/2/20#", "1/2/20"),
            ("#12-25#", "12-25"),
            ("#Jan 2, 2020#", "Jan 2, 2020"),
            ("#2 Jan 2020#", "2 Jan 2020"),
            ("#10:30#", "10:30"),
            ("#10:30:15 PM#", "10:30:15 PM"),
            ("#1/2/2020 10:30:00 AM#", "1/2/2020 10:30:00 AM"),
        ] {
            assert_eq!(
                parse(literal),
                ExprKind::Literal(Lit::DateTime(text.to_string())).into(),
                "{literal}"
            );
        }
        for literal in ["#1.2.2020#", "##", "#hello#", "#1/2/2020"] {
            let result = Parser::new(&format!("x = {literal}\n")).file();
            assert!(result.is_err(), "{literal}: {result:?}");
        }
    }

    #[test]
    fn test_error_at_end_of_input_has_a_position() {
        for (input, line, column) in [
            ("x = 1\nFor Each", 2, 9),
            ("x = 1\nFor", 2, 4),
            ("x = 1\nOption", 2, 7),
        ] {
            let error = Parser::new(input).file().unwrap_err();
            assert_eq!((error.line(), error.column()), (line, column), "{error}");
        }
    }

    #[test]
    fn test_error_position_for_exit_and_select_case() {
        let error = Parser::new("x = 1\nExit While").file().unwrap_err();
        assert_eq!((error.line(), error.column()), (2, 6), "{error}");

        let input = "Select Case x\nCase Else\ny = 1\nCase 2\ny = 2\nEnd Select";
        let error = Parser::new(input).file().unwrap_err();
        assert_eq!((error.line(), error.column()), (4, 1), "{error}");
    }

    #[test]
    fn test_keywords_as_member_names() {
        for word in ["default", "error", "end", "rem", "next", "type"] {
            let input = format!(
                "o.{word} = 1\nx = o.{word}.{word}(1)\nWith o\n.{word} = 1\nx = .{word}\nEnd With\n"
            );
            let result = Parser::new(&input).file();
            assert!(result.is_ok(), "{word}: {result:?}");
        }
    }

    /// The source text of a node.
    fn text<'a, T>(input: &'a str, node: &Spanned<T>) -> &'a str {
        &input[std::ops::Range::<usize>::from(node.span)]
    }

    #[test]
    fn test_span_of_expressions() {
        let input = "x = (a + b) * -c.d(1, e) & \"s\"";
        let items = parse_file(input);
        let ItemKind::Statement(stmt) = &items[0].node else {
            panic!("expected a statement")
        };
        let StmtKind::Assignment { full_ident, value } = &stmt.node else {
            panic!("expected an assignment")
        };
        assert_eq!(text(input, &full_ident.0), "x");
        assert_eq!(text(input, value), "(a + b) * -c.d(1, e) & \"s\"");
        let ExprKind::InfixOp { lhs, rhs, .. } = &value.node else {
            panic!("expected an infix operator")
        };
        assert_eq!(text(input, rhs), "\"s\"");
        assert_eq!(text(input, lhs), "(a + b) * -c.d(1, e)");
        let ExprKind::InfixOp { lhs, rhs, .. } = &lhs.node else {
            panic!("expected an infix operator")
        };
        // the node for the parentheses, which holds the expression without them
        assert_eq!(text(input, lhs), "(a + b)");
        let ExprKind::Paren(inner) = &lhs.node else {
            panic!("expected a parenthesized expression")
        };
        assert_eq!(text(input, inner), "a + b");
        assert_eq!(text(input, rhs), "-c.d(1, e)");
        let ExprKind::PrefixOp { expr, .. } = &rhs.node else {
            panic!("expected a prefix operator")
        };
        assert_eq!(text(input, expr), "c.d(1, e)");
        let ExprKind::FnApplication { callee, args } = &expr.node else {
            panic!("expected a function application")
        };
        assert_eq!(text(input, callee), "c.d");
        assert_eq!(text(input, args[1].as_ref().unwrap()), "e");
    }

    #[test]
    fn test_span_of_statements_excludes_delimiters_and_comments() {
        let input = indoc! {r#"
            ' leading comment
            Dim a : a = 1 ' trailing comment

            Public Sub Foo(b)
                If b Then
                    .prop = 2
                End If
            End Sub   ' done
        "#};
        let items = parse_file(input);
        let texts: Vec<_> = items.iter().map(|item| text(input, item)).collect();
        assert_eq!(
            texts,
            [
                "Dim a",
                "a = 1",
                "Public Sub Foo(b)\n    If b Then\n        .prop = 2\n    End If\nEnd Sub"
            ]
        );
        let ItemKind::Statement(sub) = &items[2].node else {
            panic!("expected a statement")
        };
        // the statement of an item has the same span, visibility included
        assert_eq!(sub.span, items[2].span);
        let StmtKind::Sub { body, .. } = &sub.node else {
            panic!("expected a sub")
        };
        assert_eq!(
            text(input, &body[0]),
            "If b Then\n        .prop = 2\n    End If"
        );
        let StmtKind::IfStmt { body, .. } = &body[0].node else {
            panic!("expected an if")
        };
        assert_eq!(text(input, &body[0]), ".prop = 2");
        let StmtKind::Assignment { full_ident, .. } = &body[0].node else {
            panic!("expected an assignment")
        };
        let ExprKind::MemberExpression { base, .. } = &full_ident.0.node else {
            panic!("expected a member expression")
        };
        // the object of the with block is implied
        assert_eq!(base.node, WithScoped);
        assert_eq!(text(input, base), "");
    }

    #[test]
    fn test_span_of_expression_lifted_out_of_sub_call_parentheses() {
        // `(a \ 10)` looks like the arguments of the call but is the start of an expression
        let input = "Foo (a \\ 10) + 1";
        let items = parse_file(input);
        let ItemKind::Statement(stmt) = &items[0].node else {
            panic!("expected a statement")
        };
        let StmtKind::SubCall { fn_name, args } = &stmt.node else {
            panic!("expected a sub call")
        };
        assert_eq!(text(input, &fn_name.0), "Foo");
        let arg = args[0].as_ref().unwrap();
        assert_eq!(text(input, arg), "(a \\ 10) + 1");
        let ExprKind::InfixOp { lhs, .. } = &arg.node else {
            panic!("expected an infix operator")
        };
        assert_eq!(text(input, lhs), "(a \\ 10)");
    }

    #[test]
    fn test_spans_are_ignored_when_comparing() {
        let compact = parse_file("x=a+1");
        let spaced = parse_file("x  =  a   +   1");
        assert_eq!(compact, spaced);
        assert_ne!(compact[0].span, spaced[0].span);
        // a tree that is built by hand is equal to the parsed one
        let built = Item::from(ItemKind::Statement(Stmt::assignment(
            FullIdent::ident("x"),
            ExprKind::InfixOp {
                op: T![+],
                lhs: Box::new(Expr::ident("a")),
                rhs: Box::new(Expr::int(1)),
            }
            .into(),
        )));
        assert_eq!(compact, vec![built]);
    }

    #[test]
    fn test_sub_call_with_parenthesized_argument_before_else_or_end() {
        let call: Stmt = StmtKind::SubCall {
            fn_name: FullIdent::ident("Foo"),
            args: vec![Some(Expr::paren(Expr::ident("a")))],
        }
        .into();
        let other: Stmt = StmtKind::SubCall {
            fn_name: FullIdent::ident("Bar"),
            args: vec![],
        }
        .into();
        assert_eq!(
            parse_file("If x Then Foo (a) Else Bar"),
            vec![
                ItemKind::Statement(
                    StmtKind::IfStmt {
                        condition: Box::new(Expr::ident("x")),
                        body: vec![call.clone()],
                        elseif_statements: vec![],
                        else_stmt: Some(vec![other]),
                    }
                    .into()
                )
                .into()
            ]
        );
        assert_eq!(
            parse_file("If x Then Foo (a) End If"),
            vec![
                ItemKind::Statement(
                    StmtKind::IfStmt {
                        condition: Box::new(Expr::ident("x")),
                        body: vec![call],
                        elseif_statements: vec![],
                        else_stmt: None,
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    /// `cscript` on Windows accepts these keywords wherever a name is declared.
    #[test]
    fn test_keywords_as_declared_names() {
        for word in ["default", "error", "property", "step"] {
            let input = format!(
                indoc! {"
                    Public {w}
                    Sub {w}()
                        Const {w} = 1
                        ReDim {w}(2), other(3)
                        ReDim Preserve {w}(2)
                    End Sub
                    Public Function {w}(a)
                    End Function
                    Class K
                        Dim {w}, other
                        Sub Bar({w})
                        End Sub
                        Public Property Let Foo({w})
                        End Property
                    End Class
                    Class L
                        Sub {w}()
                        End Sub
                    End Class
                "},
                w = word
            );
            let result = Parser::new(&input).file();
            assert!(result.is_ok(), "{word}: {result:?}");
        }
    }

    #[test]
    fn test_with_new_object() {
        let input = indoc! {"
            With New Foo
                .bar = 1
            End With
        "};
        let items = parse_file(input);
        let ItemKind::Statement(stmt) = &items[0].node else {
            panic!("expected a statement")
        };
        let StmtKind::With { object, .. } = &stmt.node else {
            panic!("expected a with statement")
        };
        assert_eq!(*object.0, Expr::new("Foo"));
        assert_eq!(text(input, &object.0), "New Foo");
    }

    /// Parentheses around the only argument of a sub call pass it by value. Verified with
    /// `cscript` on Windows: `Inc x` and `Call Inc(x)` change `x` for a `ByRef` parameter,
    /// `Inc (x)`, `Inc(x)` and `Call Inc((x))` do not.
    #[test]
    fn test_sub_call_argument_in_parentheses_is_kept() {
        let by_reference: Stmt = StmtKind::SubCall {
            fn_name: FullIdent::ident("Inc"),
            args: vec![Some(Expr::ident("x"))],
        }
        .into();
        let by_value: Stmt = StmtKind::SubCall {
            fn_name: FullIdent::ident("Inc"),
            args: vec![Some(Expr::paren(Expr::ident("x")))],
        }
        .into();
        assert_ne!(by_reference, by_value);
        assert_eq!(parse_stmt("Inc x", false), by_reference);
        for input in ["Inc (x)", "Inc(x)"] {
            assert_eq!(parse_stmt(input, false), by_value, "{input}");
        }

        // the same, whatever follows the statement
        let items = parse_file("Inc (x) : Inc (x)\nIf a Then Inc (x) Else Inc (x)\n");
        let item = |stmt: &Stmt| Item::from(ItemKind::Statement(stmt.clone()));
        assert_eq!(items[0], item(&by_value));
        assert_eq!(items[1], item(&by_value));
        let expected_if: Stmt = StmtKind::IfStmt {
            condition: Box::new(Expr::ident("a")),
            body: vec![by_value.clone()],
            elseif_statements: vec![],
            else_stmt: Some(vec![by_value.clone()]),
        }
        .into();
        assert_eq!(items[2], item(&expected_if));

        // the parentheses of a call are not an expression
        let call = parse_stmt("Call Inc(x)", false);
        assert_eq!(
            call,
            StmtKind::Call(FullIdent::new(Expr::fn_application(
                Expr::ident("Inc"),
                vec![Expr::ident("x")]
            )))
            .into()
        );
        let call = parse_stmt("Call Inc((x))", false);
        assert_eq!(
            call,
            StmtKind::Call(FullIdent::new(Expr::fn_application(
                Expr::ident("Inc"),
                vec![Expr::paren(Expr::ident("x"))]
            )))
            .into()
        );
        // and neither are empty ones
        assert_eq!(
            parse_stmt("Inc()", false),
            StmtKind::SubCall {
                fn_name: FullIdent::new(Expr::fn_application(Expr::ident("Inc"), vec![])),
                args: vec![],
            }
            .into()
        );
    }

    #[test]
    fn test_span_of_sub_call_argument_in_parentheses() {
        let input = "Inc (x)";
        let stmt = parse_stmt(input, false);
        let StmtKind::SubCall { fn_name, args } = &stmt.node else {
            panic!("expected a sub call")
        };
        assert_eq!(text(input, &fn_name.0), "Inc");
        let arg = args[0].as_ref().unwrap();
        assert_eq!(text(input, arg), "(x)");
        let ExprKind::Paren(inner) = &arg.node else {
            panic!("expected a parenthesized expression")
        };
        assert_eq!(text(input, inner), "x");
    }

    #[test]
    fn test_display_of_parenthesized_expression() {
        // operators are written with parentheses, those are not doubled
        assert_eq!(parse("(a + b) * c").to_string(), "((a + b) * c)");
        assert_eq!(parse("(-a)").to_string(), "(- a)");
        assert_eq!(parse("(a)").to_string(), "(a)");
        assert_eq!(parse("f((a), b)").to_string(), "f((a), b)");
    }

    #[test]
    fn test_stop_statement() {
        let input = indoc! {"
            Stop
            Sub Foo()
                If a Then
                    Stop
                End If
                o.Stop
            End Sub
        "};
        let items = parse_file(input);
        assert_eq!(
            items[0],
            Item::from(ItemKind::Statement(StmtKind::Stop.into()))
        );
        assert_eq!(text(input, &items[0]), "Stop");
        let ItemKind::Statement(sub) = &items[1].node else {
            panic!("expected a statement")
        };
        let StmtKind::Sub { body, .. } = &sub.node else {
            panic!("expected a sub")
        };
        let StmtKind::IfStmt { body: then, .. } = &body[0].node else {
            panic!("expected an if")
        };
        assert_eq!(then, &vec![Stmt::from(StmtKind::Stop)]);
        // a member can be called stop, as with any keyword
        assert_eq!(
            body[1],
            StmtKind::SubCall {
                fn_name: FullIdent::new(Expr::member(Expr::ident("o"), "Stop")),
                args: vec![],
            }
            .into()
        );
    }

    /// All of these are an error for `cscript` on Windows.
    #[test]
    fn test_stop_option_and_set_are_no_identifiers() {
        for input in [
            "Stop 1",
            "Stop = 1",
            "x = Stop",
            "Call Stop",
            "Dim stop",
            "Option = 1",
            "x = Option",
            "Dim option",
            "Set = 1",
            "x = Set",
            "Dim set",
        ] {
            let result = Parser::new(input).file();
            assert!(result.is_err(), "{input}: {result:?}");
        }
    }

    /// `Me` is an expression: `Me = 1` compiles on Windows and fails when it runs. It can
    /// not be declared as a name.
    #[test]
    fn test_me_is_an_expression_but_no_name() {
        for input in [
            "Me.prop = 1",
            "x = Me.prop",
            "Set x = Me",
            "Foo Me",
            "Me = 1",
        ] {
            let result = Parser::new(input).file();
            assert!(result.is_ok(), "{input}: {result:?}");
        }
        for input in [
            "Dim Me",
            "Sub Foo(Me)\nEnd Sub",
            "Sub Me()\nEnd Sub",
            "Const Me = 1",
        ] {
            let result = Parser::new(input).file();
            assert!(result.is_err(), "{input}: {result:?}");
        }
    }

    /// `cscript` on Windows fails with "Expected integer constant" for these, the bounds of
    /// a `Dim` are integer literals. Only `ReDim` takes expressions, and needs them.
    #[test]
    fn test_invalid_array_bounds() {
        for input in [
            "Dim a(n)",
            "Dim a(1 + 1)",
            "Dim a(-1)",
            "Dim a(1.5)",
            "Public a(n)",
            "ReDim a",
            "ReDim a()",
            "ReDim a(1), b",
        ] {
            let result = Parser::new(input).file();
            assert!(result.is_err(), "{input}: {result:?}");
        }
    }

    #[test]
    fn test_span_of_names() {
        let input = indoc! {"
            Public Function  Foo (ByVal first, ByRef second)
                Dim local, arr(2)
                For counter = 1 To 2
                    ReDim other(counter)
                Next
                Set x = New Bar
                x. Prop = 1
            End Function
            Class Klass
                Public Property Let Value(v)
                End Property
            End Class
        "};
        let items = parse_file(input);
        let ItemKind::Statement(function) = &items[0].node else {
            panic!("expected a statement")
        };
        let StmtKind::Function {
            name,
            parameters,
            body,
            ..
        } = &function.node
        else {
            panic!("expected a function")
        };
        // a name compares with a str and points at just the name
        assert_eq!(*name, "Foo");
        assert_eq!(text(input, name), "Foo");
        assert!(text(input, function).starts_with("Public Function"));
        let parameters: Vec<_> = parameters
            .iter()
            .map(|(Argument::ByVal(name) | Argument::ByRef(name))| text(input, name))
            .collect();
        assert_eq!(parameters, ["first", "second"]);

        let StmtKind::Dim { vars } = &body[0].node else {
            panic!("expected a dim")
        };
        let vars: Vec<_> = vars.iter().map(|var| text(input, &var.name)).collect();
        assert_eq!(vars, ["local", "arr"]);

        let StmtKind::ForStmt {
            counter,
            body: for_body,
            ..
        } = &body[1].node
        else {
            panic!("expected a for")
        };
        assert_eq!(text(input, counter), "counter");
        let StmtKind::ReDim { vars, .. } = &for_body[0].node else {
            panic!("expected a redim")
        };
        assert_eq!(text(input, &vars[0].name), "other");

        let StmtKind::Set {
            rhs: SetRhs::Expr(new),
            ..
        } = &body[2].node
        else {
            panic!("expected a set")
        };
        let ExprKind::New(class) = &new.node else {
            panic!("expected a new")
        };
        assert_eq!(text(input, new), "New Bar");
        assert_eq!(text(input, class), "Bar");

        let StmtKind::Assignment { full_ident, .. } = &body[3].node else {
            panic!("expected an assignment")
        };
        let ExprKind::MemberExpression { property, .. } = &full_ident.0.node else {
            panic!("expected a member expression")
        };
        // there can be whitespace after the dot, not before it
        assert_eq!(text(input, &full_ident.0), "x. Prop");
        assert_eq!(text(input, property), "Prop");

        let ItemKind::Class {
            name,
            member_accessors,
            ..
        } = &items[1].node
        else {
            panic!("expected a class")
        };
        assert_eq!(text(input, name), "Klass");
        assert_eq!(text(input, &member_accessors[0].name), "Value");
        assert_eq!(text(input, &member_accessors[0].args[0].0), "v");
    }

    /// These are reserved for `cscript` on Windows but can be the name of a member.
    #[test]
    fn test_endif_and_enum_are_reserved() {
        for word in ["endif", "enum", "EndIf", "Enum"] {
            for input in [
                format!("Dim {word}"),
                format!("Sub {word}()\nEnd Sub"),
                format!("x = {word}"),
            ] {
                let result = Parser::new(&input).file();
                assert!(result.is_err(), "{input}: {result:?}");
            }
            let input = format!("o.{word} = 1\nx = o.{word}");
            let result = Parser::new(&input).file();
            assert!(result.is_ok(), "{input}: {result:?}");
        }
    }

    #[test]
    fn test_dim_without_variables() {
        for input in ["Dim", "Dim ' nothing", "Dim rem", "Sub Foo()\nDim\nEnd Sub"] {
            let result = Parser::new(input).file();
            assert!(result.is_err(), "{input}: {result:?}");
        }
    }

    /// A parameter is passed by reference unless it says `ByVal`. Verified with `cscript` on
    /// Windows: `Sub Inc(n) : n = n + 1 : End Sub` changes the variable of the caller.
    #[test]
    fn test_parameters_are_by_reference_by_default() {
        let input = indoc! {"
            Sub Foo(plain, ByRef reference, ByVal value)
            End Sub
            Class Bar
                Public Property Let Baz(plain, ByRef reference, ByVal value)
                End Property
            End Class
        "};
        let items = parse_file(input);
        let ItemKind::Statement(sub) = &items[0].node else {
            panic!("expected a statement")
        };
        let StmtKind::Sub { parameters, .. } = &sub.node else {
            panic!("expected a sub")
        };
        assert_eq!(
            parameters,
            &vec![
                Argument::ByRef("plain".into()),
                Argument::ByRef("reference".into()),
                Argument::ByVal("value".into()),
            ]
        );
        let ItemKind::Class {
            member_accessors, ..
        } = &items[1].node
        else {
            panic!("expected a class")
        };
        assert_eq!(
            member_accessors[0].args,
            vec![
                ("plain".into(), ArgumentType::ByRef),
                ("reference".into(), ArgumentType::ByRef),
                ("value".into(), ArgumentType::ByVal),
            ]
        );
    }

    #[test]
    fn test_error_at_a_line_end_is_on_that_line() {
        // the expression is cut off by the end of line 2
        let error = Parser::new("Dim total\ntotal = 1 +\nMsgBox total\n")
            .file()
            .unwrap_err();
        assert_eq!((error.line(), error.column()), (2, 12), "{error}");
    }

    /// Valid for `cscript` on Windows: the dot of a with block right after an operator or a
    /// keyword, without whitespace.
    #[test]
    fn test_with_dot_without_whitespace_before_it() {
        for line in [
            "x =.p",
            "x = 1 +.p",
            "x = 1 *.p",
            "x = a <.p",
            "x = a =.p",
            "x = Not.p",
            "x = a And.p",
            "x = a Or.p",
            "x = a Mod.p",
            "x = a&.p",
            "x = -.p",
            "x = (.p)",
            "x = f(1,.p)",
            "If a Then.p = 1",
            "Select Case.p\nEnd Select",
            "Do While.p\nLoop",
        ] {
            let input = format!("With o\n{line}\nEnd With\n");
            let result = Parser::new(&input).file();
            assert!(result.is_ok(), "{line}: {result:?}");
        }
    }

    #[test]
    fn test_member_dot_and_with_dot() {
        // right after a name, a `)`, `Me` or a string it is a member access
        for (input, expected) in [
            ("x = a.p", "a.p"),
            ("x = f(1).p", "f(1).p"),
            ("x = Me.p", "Me.p"),
            ("x = a.b.c", "a.b.c"),
            // also over a line continuation
            ("x = a _\n  .p", "a.p"),
            // after an operator it is the one of a with block
            ("x = a +.p", "(a + ..p)"),
        ] {
            let items = parse_file(input);
            let ItemKind::Statement(stmt) = &items[0].node else {
                panic!("expected a statement")
            };
            let StmtKind::Assignment { value, .. } = &stmt.node else {
                panic!("expected an assignment")
            };
            assert_eq!(value.to_string(), expected, "{input}");
        }
        // whitespace before the dot makes it the argument of a call
        let items = parse_file("Foo .p");
        let ItemKind::Statement(stmt) = &items[0].node else {
            panic!("expected a statement")
        };
        let StmtKind::SubCall { fn_name, args } = &stmt.node else {
            panic!("expected a sub call")
        };
        assert_eq!(fn_name.to_string(), "Foo");
        assert_eq!(args[0].as_ref().unwrap().to_string(), "..p");
    }

    #[test]
    fn test_single_line_if() {
        let input = r#"If Err Then MsgBox "Oh noes""#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(Expr::ident("Err")),
                body: vec![
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Some(Expr::str("Oh noes"))],
                    }
                    .into()
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
            .into()
        );
    }

    #[test]
    fn test_single_line_if_multi_statement() {
        let input = r#"If Err Then MsgBox "Oh noes": MsgBox "Crash""#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(Expr::ident("Err")),
                body: vec![
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Some(Expr::str("Oh noes"))],
                    }
                    .into(),
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("MsgBox"),
                        args: vec![Some(Expr::str("Crash"))],
                    }
                    .into()
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
            .into()
        );
    }

    #[test]
    fn test_singe_line_if_with_end_if() {
        let input = r#"if VRRoom > 0 Then bbs006.state = x2 Else controller.B2SSetData 50,x2 : controller.B2SSetData 53,x2 : End If"#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(
                    InfixOp {
                        op: T![>],
                        lhs: Box::new(Expr::ident("VRRoom")),
                        rhs: Box::new(Expr::int(0)),
                    }
                    .into()
                ),
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::new(Expr::member(Expr::ident("bbs006"), "state"),),
                        value: Box::new(Expr::ident("x2")),
                    }
                    .into()
                ],
                elseif_statements: vec![],
                else_stmt: Some(vec![
                    StmtKind::SubCall {
                        fn_name: FullIdent::new(Expr::member(
                            Expr::ident("controller"),
                            "B2SSetData"
                        ),),
                        args: vec![Some(Expr::int(50)), Some(Expr::ident("x2")),],
                    }
                    .into(),
                    StmtKind::SubCall {
                        fn_name: FullIdent::new(Expr::member(
                            Expr::ident("controller"),
                            "B2SSetData"
                        ),),
                        args: vec![Some(Expr::int(53)), Some(Expr::ident("x2")),],
                    }
                    .into(),
                ]),
            }
            .into()
        );
    }

    #[test]
    fn parse_if_end_if_single_line_no_colons() {
        let input = r#"if a=1 then DoSomething() end if"#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(
                    InfixOp {
                        op: T![=],
                        lhs: Box::new(Expr::ident("a")),
                        rhs: Box::new(Expr::int(1)),
                    }
                    .into()
                ),
                body: vec![
                    StmtKind::SubCall {
                        fn_name: FullIdent::new(Expr::fn_application(
                            Expr::ident("DoSomething"),
                            vec![]
                        )),
                        args: vec![],
                    }
                    .into()
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
            .into()
        );
    }

    #[test]
    fn test_single_line_if_with_many_colons() {
        let input =
            r#"If x < 0 Or Err Then : DoSomething obj : Else : DoSomethingElse obj : End If"#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(
                    InfixOp {
                        op: T![or],
                        lhs: Box::new(
                            InfixOp {
                                op: T![<],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(0)),
                            }
                            .into()
                        ),
                        rhs: Box::new(Expr::ident("Err")),
                    }
                    .into()
                ),
                body: vec![
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("DoSomething"),
                        args: vec![Some(Expr::ident("obj"))],
                    }
                    .into(),
                ],
                elseif_statements: vec![],
                else_stmt: Some(vec![
                    StmtKind::SubCall {
                        fn_name: FullIdent::ident("DoSomethingElse"),
                        args: vec![Some(Expr::ident("obj"))],
                    }
                    .into(),
                ]),
            }
            .into()
        );
    }

    #[test]
    fn test_parse_if_two_single_lines() {
        let input = r#"
            If(x <> "") Then LutValue = CDbl(x) Else LutValue = 1
	        If LutValue < 1 Then LutValue = 1
        "#;
        let file = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(StmtKind::IfStmt {
                    condition: Box::new(Expr::paren(InfixOp {
                        op: T![<>],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(ExprKind::Literal(Lit::Str("".to_string())).into()),
                    }.into())),
                    body: vec![StmtKind::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::fn_application(
                            Expr::ident("CDbl"),
                            vec![Expr::ident("x")])
                        ),
                    }.into()],
                    elseif_statements: vec![],
                    else_stmt: Some(vec![StmtKind::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::int(1)),
                    }.into()]),
                }.into()).into(),
                ItemKind::Statement(StmtKind::IfStmt {
                    condition: Box::new(InfixOp {
                        op: T![<],
                        lhs: Box::new(Expr::ident("LutValue")),
                        rhs: Box::new(Expr::int(1)),
                    }.into()),
                    body: vec![StmtKind::Assignment {
                        full_ident: FullIdent::ident("LutValue"),
                        value: Box::new(Expr::int(1)),
                    }.into()],
                    elseif_statements: vec![],
                    else_stmt: None,
                }.into()).into(),
            ]
        );
    }

    #[test]
    fn test_parse_if_single_line_with_else() {
        let input = r#"If x > 2 Then y = 3 Else y = 4"#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(
                    InfixOp {
                        op: T![>],
                        lhs: Box::new(Expr::ident("x")),
                        rhs: Box::new(Expr::int(2)),
                    }
                    .into()
                ),
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("y"),
                        value: Box::new(Expr::int(3)),
                    }
                    .into()
                ],
                elseif_statements: vec![],
                else_stmt: Some(vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("y"),
                        value: Box::new(Expr::int(4)),
                    }
                    .into()
                ]),
            }
            .into()
        );
    }

    #[test]
    fn test_if_nested_single_line() {
        let input = "If key = 1 Then foo.fire: If bar=1 then DoSomething() else DoSomethingElse()";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(
                    InfixOp {
                        op: T![=],
                        lhs: Box::new(Expr::ident("key")),
                        rhs: Box::new(Expr::int(1)),
                    }
                    .into()
                ),
                body: vec![
                    StmtKind::SubCall {
                        fn_name: FullIdent::new(Expr::member(Expr::ident("foo"), "fire"),),
                        args: vec![],
                    }
                    .into(),
                    StmtKind::IfStmt {
                        condition: Box::new(
                            InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("bar")),
                                rhs: Box::new(Expr::int(1)),
                            }
                            .into()
                        ),
                        body: vec![
                            StmtKind::SubCall {
                                fn_name: FullIdent::new(Expr::fn_application(
                                    Expr::ident("DoSomething"),
                                    vec![],
                                )),
                                args: vec![],
                            }
                            .into()
                        ],
                        elseif_statements: vec![],
                        else_stmt: Some(vec![
                            StmtKind::SubCall {
                                fn_name: FullIdent::new(Expr::fn_application(
                                    Expr::ident("DoSomethingElse"),
                                    vec![],
                                )),
                                args: vec![],
                            }
                            .into()
                        ]),
                    }
                    .into()
                ],
                elseif_statements: vec![],
                else_stmt: None,
            }
            .into()
        );
    }

    #[test]
    fn test_if_nested_mixed() {
        let input = indoc! {r#"
        If x > 2 Then
            If This Or That Then DoSomething 'weird comment
        End If
        "#};
        let stmt = parse_file(input);
        assert_eq!(
            stmt,
            vec![
                ItemKind::Statement(
                    StmtKind::IfStmt {
                        condition: Box::new(
                            InfixOp {
                                op: T![>],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(2)),
                            }
                            .into()
                        ),
                        body: vec![
                            StmtKind::IfStmt {
                                condition: Box::new(
                                    InfixOp {
                                        op: T![or],
                                        lhs: Box::new(Expr::ident("This")),
                                        rhs: Box::new(Expr::ident("That")),
                                    }
                                    .into()
                                ),
                                body: vec![
                                    StmtKind::SubCall {
                                        fn_name: FullIdent::ident("DoSomething"),
                                        args: vec![],
                                    }
                                    .into()
                                ],
                                elseif_statements: vec![],
                                else_stmt: None,
                            }
                            .into()
                        ],
                        elseif_statements: vec![],
                        else_stmt: None,
                    }
                    .into()
                )
                .into(),
            ]
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
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(
                    InfixOp {
                        op: T![=],
                        lhs: Box::new(Expr::ident("a")),
                        rhs: Box::new(Expr::int(3)),
                    }
                    .into()
                ),
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("b"),
                        value: Box::new(Expr::int(0)),
                    }
                    .into()
                ],
                elseif_statements: vec![],
                else_stmt: Some(vec![
                    StmtKind::IfStmt {
                        condition: Box::new(
                            InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("a")),
                                rhs: Box::new(Expr::int(2)),
                            }
                            .into()
                        ),
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("b"),
                                value: Box::new(Expr::int(2)),
                            }
                            .into()
                        ],
                        elseif_statements: vec![],
                        else_stmt: None,
                    }
                    .into()
                ]),
            }
            .into()
        );
    }

    #[test]
    fn parse_dim() {
        let input = "Dim x";
        let stmt = parse_stmt(input, true);
        assert_eq!(stmt, Stmt::dim("x"));
    }

    #[test]
    fn test_dim_array() {
        let input = "Dim x(1, 2)";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Dim {
                vars: vec![VarDecl::array("x", vec![1, 2])],
            }
            .into()
        );
    }

    #[test]
    fn test_dim_array_with_space() {
        let input = "Dim PlayerMode (2)";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Dim {
                vars: vec![VarDecl::array("PlayerMode", vec![2])],
            }
            .into()
        );
    }

    #[test]
    fn parse_dim_multiple() {
        let input = "Dim x,y, z(1, 3), dynamic(), h(&H10)";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Dim {
                vars: vec![
                    VarDecl::new("x"),
                    VarDecl::new("y"),
                    VarDecl::array("z", vec![1, 3]),
                    // not the same as `Dim dynamic`
                    VarDecl::array("dynamic", vec![]),
                    VarDecl::array("h", vec![16]),
                ],
            }
            .into()
        );
    }

    #[test]
    fn parse_assign_very_big_number() {
        let input = "x = 1111111111111111111111111111111111111111111111111111111111111";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Assignment {
                full_ident: FullIdent::ident("x"),
                value: Box::new(Expr::int_str(
                    "1111111111111111111111111111111111111111111111111111111111111"
                ))
            }
            .into()
        );
    }

    #[test]
    fn parse_const() {
        let input = "Const x = 42";
        let stmt = parse_stmt(input, true);
        assert_eq!(stmt, Stmt::const_("x", Lit::int(42)));
    }

    #[test]
    fn parse_const_negative() {
        let input = "Const x = -1";
        let stmt = parse_stmt(input, true);
        assert_eq!(stmt, Stmt::const_("x", Lit::int(-1)));
    }

    #[test]
    fn parse_const_multi() {
        let input = r#"const x = 42, txt = "Hello""#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Const(vec![
                ("x".into(), Lit::int(42)),
                ("txt".into(), Lit::str("Hello".to_string())),
            ])
            .into()
        );
    }

    #[test]
    fn parse_two_consts_with_comments() {
        let input = indoc! {"
            Const x = 42 ' The answer to everything
            Const y = 13 ' An unlucky number
        "};
        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::Const {
                    visibility: Visibility::Public,
                    values: vec![("x".into(), Lit::int(42))],
                }
                .into(),
                ItemKind::Const {
                    visibility: Visibility::Public,
                    values: vec![("y".into(), Lit::int(13))],
                }
                .into()
            ]
        );
    }

    #[test]
    fn parse_true_false() {
        let input = indoc! {r#"
            Const Test = False
            Const Test2 = True
        "#};
        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::Const {
                    visibility: Visibility::Public,
                    values: vec![("Test".into(), Lit::Bool(false))],
                }
                .into(),
                ItemKind::Const {
                    visibility: Visibility::Public,
                    values: vec![("Test2".into(), Lit::Bool(true))],
                }
                .into()
            ]
        );
    }

    #[test]
    fn parse_const_private_negative_int() {
        let input = indoc! {r#"
            Private Const Test = -1
        "#};
        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::Const {
                    visibility: Visibility::Private,
                    values: vec![("Test".into(), Lit::int(-1))],
                }
                .into()
            ]
        );
    }

    #[test]
    fn parse_variables() {
        let input = indoc! {r#"
            Public x, y(1,2)
            Private z, a(1), b()
        "#};
        let all = parse_file(input);
        assert_eq!(
            all,
            vec![
                ItemKind::Variable {
                    visibility: Visibility::Public,
                    vars: vec![VarDecl::new("x"), VarDecl::array("y", vec![1, 2]),],
                }
                .into(),
                ItemKind::Variable {
                    visibility: Visibility::Private,
                    vars: vec![
                        VarDecl::new("z"),
                        VarDecl::array("a", vec![1]),
                        VarDecl::array("b", vec![]),
                    ],
                }
                .into(),
            ]
        );
    }

    #[test]
    #[should_panic = "ParseError at line 2, column 13: Expected `sub` or `function` after visibility, but found `const`"]
    fn parse_const_private_nested_fail() {
        // not allowed on windows
        let input = indoc! {r#"
            Sub Test
                Private Const Test = 1
            End Sub
        "#};
        Parser::new(input).file().unwrap();
    }

    #[test]
    fn test_set() {
        let input = indoc! {r#"
            Set foo = bar
            Set Obj(x) = NullFader
        "#};
        let file = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(StmtKind::Set {
                    var: FullIdent::ident("foo"),
                    rhs: SetRhs::ident("bar"),
                }.into()).into(),
                ItemKind::Statement(StmtKind::Set {
                    var: FullIdent::new(
                        Expr::fn_application(
                            Expr::ident("Obj"),
                            vec![Expr::ident("x")]
                        )
                    ),
                    rhs: SetRhs::ident("NullFader"),
                }.into()).into(),
            ]
        );
    }

    #[test]
    fn test_set_using_new() {
        let input = "Set foo = New Bar";
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Set {
                        var: FullIdent::ident("foo"),
                        rhs: SetRhs::Expr(Box::new(Expr::new("Bar"))),
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_set_using_new_parentheses() {
        // This creates a new class, calls a default function that returns Me
        let input = "Set DT1 = (new DropTarget)(1, 0, False)";
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Set {
                        var: FullIdent::ident("DT1"),
                        rhs: SetRhs::Expr(Box::new(Expr::fn_application(
                            Expr::paren(Expr::new("DropTarget")),
                            vec![
                                Expr::int(1),
                                Expr::int(0),
                                ExprKind::Literal(Lit::Bool(false)).into(),
                            ]
                        ))),
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_set_using_nothing() {
        let input = "Set foo = Nothing";
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Set {
                        var: FullIdent::ident("foo"),
                        rhs: SetRhs::Nothing,
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn parse_set_deep() {
        let input = "Set lampz.obj(x) = 1";
        let items = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Set {
                        var: FullIdent::new(
                            Expr::fn_application(
                                Expr::member(Expr::ident("lampz"), "obj"),
                                vec![Expr::ident("x")]
                            )
                        ),
                        rhs: SetRhs::expr(Expr::int(1)),
                    }.into()
                ).into()
            ]
        );
    }

    #[test]
    fn test_redim() {
        let input = "Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)";
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::ReDim {
                        preserve: true,
                        vars: vec![ReDimVar {
                            name: "tmp".into(),
                            bounds: vec![
                                InfixOp {
                                    op: T![+],
                                    lhs: Box::new(
                                        InfixOp {
                                            op: T![+],
                                            lhs: Box::new(
                                                ExprKind::FnApplication {
                                                    callee: Box::new(Expr::ident("uBound")),
                                                    args: vec![Some(Expr::ident("aArray"))],
                                                }
                                                .into()
                                            ),
                                            rhs: Box::new(
                                                ExprKind::FnApplication {
                                                    callee: Box::new(Expr::ident("uBound")),
                                                    args: vec![Some(Expr::ident("aInput"))],
                                                }
                                                .into()
                                            ),
                                        }
                                        .into()
                                    ),
                                    rhs: Box::new(Expr::int(1)),
                                }
                                .into()
                            ],
                        }],
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn parse_redim_multi() {
        let input = "Redim a(length), b(2, 3)";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::ReDim {
                preserve: false,
                vars: vec![
                    ReDimVar {
                        name: "a".into(),
                        bounds: vec![Expr::ident("length")],
                    },
                    ReDimVar {
                        name: "b".into(),
                        bounds: vec![Expr::int(2), Expr::int(3)],
                    },
                ],
            }
            .into()
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(Stmt::dim("test")).into(),
                ItemKind::Statement(
                    StmtKind::IfStmt {
                        condition: Box::new(
                            InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("RenderingMode")),
                                rhs: Box::new(Expr::int(2)),
                            }
                            .into()
                        ),
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("test"),
                                value: Box::new(Expr::int(1)),
                            }
                            .into(),
                            StmtKind::SubCall {
                                fn_name: FullIdent::ident("startcontroller"),
                                args: vec![],
                            }
                            .into()
                        ],
                        elseif_statements: vec![(
                            Box::new(
                                InfixOp {
                                    op: T![=],
                                    lhs: Box::new(Expr::ident("RenderingMode")),
                                    rhs: Box::new(Expr::int(3)),
                                }
                                .into()
                            ),
                            vec![
                                StmtKind::Assignment {
                                    full_ident: FullIdent::ident("test"),
                                    value: Box::new(Expr::int(2)),
                                }
                                .into(),
                                StmtKind::SubCall {
                                    fn_name: FullIdent::ident("startcontroller"),
                                    args: vec![],
                                }
                                .into()
                            ]
                        )],
                        else_stmt: Some(vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("test"),
                                value: Box::new(Expr::int(0)),
                            }
                            .into()
                        ]),
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn test_assignment_with_negative_int() {
        let input = "x = -1";
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(
                            ExprKind::PrefixOp {
                                op: T![-],
                                expr: Box::new(Expr::int(1)),
                            }
                            .into()
                        ),
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_assignment_to_object_in_array_property() {
        let input = r#"objectArray(i).image = "test""#;
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Assignment {
                        full_ident: FullIdent::new(Expr::member(
                            ExprKind::FnApplication {
                                callee: Box::new(Expr::ident("objectArray")),
                                args: vec![Some(Expr::ident("i"))],
                            }
                            .into(),
                            "image"
                        )),
                        value: Box::new(Expr::str("test")),
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_assignment_with_property_in_expression() {
        let input = "foo.a = foo.b-120.5";
        let items = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![ItemKind::Statement(Stmt::assignment (
                FullIdent::new(
                    Expr::member(Expr::ident("foo"), "a")
                ),
                InfixOp {
                    op: T![-],
                    lhs: Box::new(Expr::member(Expr::ident("foo"), "b")),
                    rhs: Box::new(ExprKind::Literal(Lit::Float(120.5)).into()),
                }.into()
            )
            ).into()]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::SelectCase {
                        test_expr: Box::new(Expr::ident("x")),
                        cases: vec![
                            Case {
                                tests: vec![Expr::int(1), Expr::int(2),],
                                body: vec![
                                    StmtKind::Assignment {
                                        full_ident: FullIdent::ident("y"),
                                        value: Box::new(Expr::int(2)),
                                    }
                                    .into()
                                ]
                            },
                            Case {
                                tests: vec![Expr::int(3),],
                                body: vec![
                                    StmtKind::Assignment {
                                        full_ident: FullIdent::ident("y"),
                                        value: Box::new(Expr::int(3)),
                                    }
                                    .into()
                                ]
                            },
                        ],
                        else_stmt: Some(vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("y"),
                                value: Box::new(Expr::int(4)),
                            }
                            .into()
                        ]),
                    }
                    .into()
                )
                .into()
            ]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::SelectCase {
                        test_expr: Box::new(ExprKind::Literal(Lit::Bool(true)).into()),
                        cases: vec![
                            Case {
                                tests: vec![
                                    InfixOp {
                                        op: T![and],
                                        lhs: Box::new(
                                            InfixOp {
                                                op: T![=],
                                                lhs: Box::new(Expr::ident("x")),
                                                rhs: Box::new(Expr::int(1)),
                                            }
                                            .into()
                                        ),
                                        rhs: Box::new(
                                            InfixOp {
                                                op: T![=],
                                                lhs: Box::new(Expr::ident("y")),
                                                rhs: Box::new(Expr::int(2)),
                                            }
                                            .into()
                                        ),
                                    }
                                    .into(),
                                ],
                                body: vec![
                                    StmtKind::Assignment {
                                        full_ident: FullIdent::ident("z"),
                                        value: Box::new(Expr::int(2)),
                                    }
                                    .into()
                                ]
                            },
                            Case {
                                tests: vec![
                                    InfixOp {
                                        op: T![=],
                                        lhs: Box::new(Expr::ident("x")),
                                        rhs: Box::new(Expr::int(1)),
                                    }
                                    .into(),
                                    InfixOp {
                                        op: T![=],
                                        lhs: Box::new(Expr::ident("y")),
                                        rhs: Box::new(Expr::int(2)),
                                    }
                                    .into(),
                                ],
                                body: vec![
                                    StmtKind::Assignment {
                                        full_ident: FullIdent::ident("z"),
                                        value: Box::new(Expr::int(3)),
                                    }
                                    .into()
                                ]
                            }
                        ],
                        else_stmt: None,
                    }
                    .into()
                )
                .into()
            ]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::SelectCase {
                        test_expr: Box::new(Expr::ident("x")),
                        cases: vec![Case {
                            tests: vec![Expr::int(1), Expr::int(2),],
                            body: vec![
                                StmtKind::Assignment {
                                    full_ident: FullIdent::ident("y"),
                                    value: Box::new(Expr::int(2)),
                                }
                                .into()
                            ]
                        },],
                        else_stmt: Some(vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("y"),
                                value: Box::new(Expr::int(4)),
                            }
                            .into()
                        ]),
                    }
                    .into()
                )
                .into()
            ]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::SelectCase {
                        test_expr: Box::new(Expr::ident("keycode")),
                        cases: vec![
                            Case {
                                tests: vec![Expr::ident("keyA")],
                                body: vec![
                                    StmtKind::SubCall {
                                        fn_name: FullIdent::ident("MySub"),
                                        args: vec![Some(ExprKind::Literal(Lit::Bool(true)).into())],
                                    }
                                    .into(),
                                    StmtKind::SubCall {
                                        fn_name: FullIdent::ident("OtherSub"),
                                        args: vec![Some(Expr::int(1))],
                                    }
                                    .into()
                                ],
                            },
                            Case {
                                tests: vec![Expr::int(82),],
                                body: vec![
                                    StmtKind::Assignment {
                                        full_ident: FullIdent::new(Expr::fn_application(
                                            Expr::member(WithScoped.into(), "Switch"),
                                            vec![Expr::ident("swCPUDiag")]
                                        )),
                                        value: Box::new(Expr::ident("t")),
                                    }
                                    .into()
                                ]
                            },
                        ],
                        else_stmt: Some(vec![
                            StmtKind::SubCall {
                                fn_name: FullIdent::ident("DoNothing"),
                                args: vec![],
                            }
                            .into()
                        ]),
                    }
                    .into()
                )
                .into()
            ]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::SelectCase {
                        test_expr: Box::new(Expr::ident("serviceLevel")),
                        cases: vec![Case {
                            tests: vec![Expr::ident("kMenuTop"), Expr::ident("kMenuNone"),],
                            body: vec![
                                StmtKind::Assignment {
                                    full_ident: FullIdent::ident("bInService"),
                                    value: Box::new(ExprKind::Literal(Lit::Bool(false)).into()),
                                }
                                .into()
                            ]
                        },],
                        else_stmt: None,
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_parse_is() {
        let input = indoc! {r#"
            If Not Controller Is Nothing Then ' Controller might no be there
                Controller.Run
            End If
        "#};
        let stmt = parse_stmt(input, true);
        #[rustfmt::skip]
        assert_eq!(
            stmt,
            StmtKind::IfStmt {
                condition: Box::new(ExprKind::PrefixOp {
                    op: T![not],
                    expr: Box::new(InfixOp {
                        op: T![is],
                        lhs: Box::new(Expr::ident("Controller")),
                        rhs: Box::new(ExprKind::Literal(Lit::Nothing).into()),
                    }.into()),
                }.into()),
                body: vec![StmtKind::SubCall {
                    fn_name: FullIdent::new(
                        Expr::member(
                            Expr::ident("Controller"),
                            "Run"
                        ),
                    ),
                    args: vec![],
                }.into(),],
                elseif_statements: vec![],
                else_stmt: None,
            }.into()
        );
    }

    #[test]
    fn test_sub_call_with_empty_args() {
        let input = r#"DoSomething 1,,"test""#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                args: vec![Some(Expr::int(1)), None, Some(Expr::str("test")),],
            }
            .into()
        );
    }

    #[test]
    fn inline_class() {
        let input = indoc! {r#"
            Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class
        "#};
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Class {
                    name: "NullFadingObject".into(),
                    members: vec![],
                    dims: vec![],
                    member_accessors: vec![MemberAccess {
                        name: "IntensityScale".into(),
                        visibility: PropertyVisibility::Public { default: false },
                        property_type: PropertyType::Let,
                        args: vec![("input".into(), ArgumentType::ByRef),],
                        body: vec![],
                    }],
                    methods: vec![],
                }
                .into()
            ]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Class {
                    name: "MyClass".into(),
                    members: vec![
                        MemberDefinitions {
                            visibility: Visibility::Public,
                            properties: vec![
                                VarDecl::new("Foo"),
                                VarDecl::array("Bar", vec![9, 0]),
                            ],
                        },
                        MemberDefinitions {
                            visibility: Visibility::Private,
                            properties: vec![VarDecl::array("Qux", vec![1]), VarDecl::new("Baz"),],
                        },
                    ],
                    dims: vec![],
                    member_accessors: vec![],
                    methods: vec![],
                }
                .into()
            ]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Class {
                    name: "MyClass".into(),
                    members: vec![],
                    dims: vec![
                        vec![VarDecl::new("Foo"), VarDecl::array("Bar", vec![9, 0]),],
                        vec![VarDecl::array("Qux", vec![1]), VarDecl::new("Baz"),]
                    ],
                    member_accessors: vec![],
                    methods: vec![],
                }
                .into()
            ]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Class {
                    name: "MyClass".into(),
                    members: vec![MemberDefinitions {
                        visibility: Visibility::Public,
                        properties: vec![VarDecl::new("Enabled")],
                    },],
                    dims: vec![],
                    member_accessors: vec![],
                    methods: vec![
                        StmtKind::Sub {
                            visibility: Visibility::Public,
                            name: "Class_Initialize".into(),
                            parameters: vec![],
                            body: vec![
                                StmtKind::Assignment {
                                    full_ident: FullIdent::ident("Enabled"),
                                    value: Box::new(ExprKind::Literal(Lit::Bool(true)).into()),
                                }
                                .into()
                            ],
                        }
                        .into(),
                        StmtKind::Sub {
                            visibility: Visibility::Private,
                            name: "Class_Terminate".into(),
                            parameters: vec![],
                            body: vec![],
                        }
                        .into()
                    ],
                }
                .into(),
            ]
        );
    }

    #[test]
    fn test_parse_with_dot_after_then_and_else() {
        // https://github.com/francisdb/vbscript.rs/issues/37
        // no space is required between `Then`/`Else` and a with-statement dot
        let input = indoc! {r#"
            With Controller
                If usePUP = False Then.PuPHide = 1 Else.PuPHide = 2
                if Mute = 1 then.Games(cGameName).Settings.Value("sound") = 0
            End With
        "#};
        let items = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![ItemKind::Statement(StmtKind::With {
                object: FullIdent::ident("Controller"),
                body: vec![
                    StmtKind::IfStmt {
                        condition: Box::new(InfixOp {
                            op: T![=],
                            lhs: Box::new(Expr::ident("usePUP")),
                            rhs: Box::new(Expr::bool(false)),
                        }.into()),
                        body: vec![StmtKind::Assignment {
                            full_ident: FullIdent::new(Expr::member(WithScoped.into(), "PuPHide")),
                            value: Box::new(Expr::int(1)),
                        }.into()],
                        elseif_statements: vec![],
                        else_stmt: Some(vec![StmtKind::Assignment {
                            full_ident: FullIdent::new(Expr::member(WithScoped.into(), "PuPHide")),
                            value: Box::new(Expr::int(2)),
                        }.into()]),
                    }.into(),
                    StmtKind::IfStmt {
                        condition: Box::new(InfixOp {
                            op: T![=],
                            lhs: Box::new(Expr::ident("Mute")),
                            rhs: Box::new(Expr::int(1)),
                        }.into()),
                        body: vec![StmtKind::Assignment {
                            full_ident: FullIdent::new(Expr::fn_application(
                                Expr::member(
                                    Expr::member(
                                        Expr::fn_application(
                                            Expr::member(WithScoped.into(), "Games"),
                                            vec![Expr::ident("cGameName")],
                                        ),
                                        "Settings",
                                    ),
                                    "Value",
                                ),
                                vec![Expr::str("sound")],
                            )),
                            value: Box::new(Expr::int(0)),
                        }.into()],
                        elseif_statements: vec![],
                        else_stmt: None,
                    }.into(),
                ],
            }.into()).into()]
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
        let items = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![ItemKind::Statement(StmtKind::With {
                object: FullIdent::new(
                    Expr::member(
                        Expr::ident("foo"),
                        "obj"
                    )
                ),
                body: vec![
                    StmtKind::Assignment {
                        full_ident: FullIdent::new(
                            Expr::member(
                                WithScoped.into(),
                                "bar"
                            )
                        ),
                        value: Box::new(Expr::int(1)),
                    }.into(),
                    StmtKind::Assignment {
                        full_ident: FullIdent::new(
                            Expr::member(
                                Expr::member(
                                    WithScoped.into(),
                                    "baz"
                                ),
                                "z"
                            )
                        ),
                        value: Box::new(Expr::ident("x")),
                    }.into(),
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("x"),
                        value: Box::new(Expr::member(
                            WithScoped.into(),
                            "qux"
                        )),
                    }.into(),
                ],
            }.into()).into()]
        );
    }

    #[test]
    fn test_parse_colon_without_remainder() {
        let input = indoc! { r#"
            Dim LutToggleSoundLevel : 
            LutToggleSoundLevel = 0.5
        "#};
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Dim {
                        vars: vec![VarDecl::new("LutToggleSoundLevel")],
                    }
                    .into()
                )
                .into(),
                ItemKind::Statement(
                    StmtKind::Assignment {
                        full_ident: FullIdent::ident("LutToggleSoundLevel"),
                        value: Box::new(ExprKind::Literal(Lit::Float(0.5)).into()),
                    }
                    .into()
                )
                .into(),
            ]
        );
    }

    #[test]
    fn test_parse_double_array_access() {
        let input = "foo(1)(2) = 3";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Assignment {
                full_ident: FullIdent::new(Expr::fn_application(
                    Expr::fn_application(Expr::ident("foo"), vec![Expr::int(1)]),
                    vec![Expr::int(2)]
                )),
                value: Box::new(Expr::int(3)),
            }
            .into()
        );
    }

    #[test]
    fn test_parse_call() {
        let input = indoc! { r#"
            Call MyFunction
            Call MyOtherFunction(1, 2)
            Call mQue3(ii)(mQue2(ii))
        "#};
        let items = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(StmtKind::Call(FullIdent::ident("MyFunction")).into()).into(),
                ItemKind::Statement(StmtKind::Call(FullIdent::new(
                    Expr::fn_application(
                        Expr::ident("MyOtherFunction"),
                        vec![Expr::int(1), Expr::int(2)]
                    )
                )).into()).into(),
                ItemKind::Statement(StmtKind::Call(FullIdent::new(
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
                )).into()).into(),
            ]
        );
    }
    #[test]
    fn test_statement_ambiguity_accepted() {
        // These just check what is normally valid and should not panic

        // sub call for object element in multi-dimensional array
        let input = "arr(0,0).s";
        parse_stmt(input, true);

        // sub call with single argument being parenthesized expression
        let input = "DoSomething(x)";
        parse_stmt(input, true);

        // assignment to multi-dimensional array element
        let input = "arr(0,0) = 1";
        parse_stmt(input, true);

        // this is accepted on windows, why?
        let input = "something(0)(0)";
        parse_stmt(input, true);

        // deep sub call from within a multidimensional array
        let input = "arr(0,0).DoSomething(x)";
        parse_stmt(input, true);

        // sub call with first arg expression
        let input = "DoSomething (x + y) * z";
        parse_stmt(input, true);

        // sub call with comma after first arg in brackets
        let input = "DoSomething (x + 1), 2";
        parse_stmt(input, true);
    }

    #[test]
    #[should_panic = "Expected end of statement"]
    fn test_statement_expected_end() {
        // Windows: compilation error: Expected end of statement
        // TODO make these tests pass
        let input = "DoSomething(),0";
        parse_stmt(input, true);
    }

    #[test]
    fn test_statement_do_not_expect_end() {
        let input = "DoSomething(1),0";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                // the first argument is passed by value
                args: vec![Some(Expr::paren(Expr::int(1))), Some(Expr::int(0))],
            }
            .into()
        );
    }

    #[test]
    #[should_panic = "compilation error: Cannot use parentheses when calling a Sub"]
    fn test_statement_no_parenthesis_when_calling_sub() {
        // compilation error: Cannot use parentheses when calling a Sub
        let input = "DoSomething(0,0)";
        parse_stmt(input, true);
    }

    #[test]
    #[should_panic = "compilation error: Cannot use parentheses when calling a Sub"]
    fn test_statement_no_parenthesis_when_calling_deep_sub() {
        // compilation error: Cannot use parentheses when calling a Sub
        let input = "SomeArray(1,2).DoSomething(0,0,3)";
        parse_stmt(input, true);
    }

    #[test]
    #[should_panic = "compilation error: Cannot use parentheses when calling a Sub"]
    fn test_statement_no_parenthesis_when_calling_sub_and_invalid_statement() {
        // compilation error: Cannot use parentheses when calling a Sub
        let input = "something(0,0) + 1";
        parse_stmt(input, true);
    }

    #[test]
    fn test_parse_statement_sub_call_with_parens() {
        // This is tricky because the parens are used for both function calls, array access
        // but here they are part of the sub call first argument.
        // Would the space in front of the parens make a difference?
        let input = "DoSomething (x + y) * z";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                args: vec![Some(
                    ExprKind::InfixOp {
                        op: T![*],
                        lhs: Box::new(Expr::paren(
                            ExprKind::InfixOp {
                                op: T![+],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::ident("y")),
                            }
                            .into()
                        )),
                        rhs: Box::new(Expr::ident("z"))
                    }
                    .into()
                )],
            }
            .into(),
        );
    }

    #[test]
    fn test_parse_statement_sub_call_with_parens2() {
        // This is tricky because the parens are used for both function calls, array access
        // but here they are part of the sub call first argument.
        // Would the space in front of the parens make a difference?
        let input = "DoSomething z * (x + y)";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::SubCall {
                fn_name: FullIdent::ident("DoSomething"),
                args: vec![Some(
                    ExprKind::InfixOp {
                        op: T![*],
                        lhs: Box::new(Expr::ident("z")),
                        rhs: Box::new(Expr::paren(
                            ExprKind::InfixOp {
                                op: T![+],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::ident("y")),
                            }
                            .into()
                        )),
                    }
                    .into()
                )],
            }
            .into(),
        );
    }

    #[test]
    fn test_parse_function_or_array_call_with_parens() {
        // This is tricky because the parens are used for both function calls, array access
        let input = "x = AddScore(x + y) * z";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::Assignment {
                full_ident: FullIdent::ident("x"),
                value: Box::new(
                    ExprKind::InfixOp {
                        op: T![*],
                        lhs: Box::new(
                            ExprKind::FnApplication {
                                callee: Box::new(Expr::ident("AddScore")),
                                args: vec![Some(
                                    ExprKind::InfixOp {
                                        op: T![+],
                                        lhs: Box::new(Expr::ident("x")),
                                        rhs: Box::new(Expr::ident("y")),
                                    }
                                    .into()
                                )],
                            }
                            .into()
                        ),
                        rhs: Box::new(Expr::ident("z")),
                    }
                    .into()
                ),
            }
            .into(),
        );
    }

    #[test]
    fn parse_dim_call_with_nested_empty_args() {
        let input = r#"MySub MyFn(0, , -1)"#;
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::SubCall {
                fn_name: FullIdent::ident("MySub"),
                args: vec![Some(
                    ExprKind::FnApplication {
                        callee: Box::new(Expr::ident("MyFn")),
                        args: vec![
                            Some(Expr::int(0)),
                            None,
                            Some(
                                ExprKind::PrefixOp {
                                    op: T![-],
                                    expr: Box::new(Expr::int(1)),
                                }
                                .into()
                            )
                        ],
                    }
                    .into()
                )]
            }
            .into()
        );
    }

    #[test]
    fn test_parse_function_single_line_with_colons() {
        let input = "Function NullFunction(a) : End Function";
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Function {
                        visibility: Visibility::Default,
                        name: "NullFunction".into(),
                        parameters: vec![Argument::ByRef("a".into())],
                        body: vec![],
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_parse_function_single_line_without_colons() {
        let input = "Function NullFunction(a) End Function";
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::Function {
                        visibility: Visibility::Default,
                        name: "NullFunction".into(),
                        parameters: vec![Argument::ByRef("a".into())],
                        body: vec![],
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_parse_do_while_loop() {
        let input = indoc! {r#"
            Do While x > 0
                x = x - 1
            Loop
        "#};
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::DoLoop {
                        check: DoLoopCheck::Pre(DoLoopCondition::While(Box::new(
                            InfixOp {
                                op: T![>],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(0)),
                            }
                            .into()
                        ))),
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("x"),
                                value: Box::new(
                                    InfixOp {
                                        op: T![-],
                                        lhs: Box::new(Expr::ident("x")),
                                        rhs: Box::new(Expr::int(1)),
                                    }
                                    .into()
                                ),
                            }
                            .into()
                        ],
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_parse_do_until_loop() {
        let input = indoc! {r#"
            Do Until x = 0
                x = x - 1
            Loop
        "#};

        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::DoLoop {
                        check: DoLoopCheck::Pre(DoLoopCondition::Until(Box::new(
                            InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(0)),
                            }
                            .into()
                        ))),
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("x"),
                                value: Box::new(
                                    InfixOp {
                                        op: T![-],
                                        lhs: Box::new(Expr::ident("x")),
                                        rhs: Box::new(Expr::int(1)),
                                    }
                                    .into()
                                ),
                            }
                            .into()
                        ],
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_parse_do_loop_while() {
        let input = indoc! {r#"
            Do
                x = x - 1
            Loop While x > 0
        "#};
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::DoLoop {
                        check: DoLoopCheck::Post(DoLoopCondition::While(Box::new(
                            InfixOp {
                                op: T![>],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(0)),
                            }
                            .into()
                        ))),
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("x"),
                                value: Box::new(
                                    InfixOp {
                                        op: T![-],
                                        lhs: Box::new(Expr::ident("x")),
                                        rhs: Box::new(Expr::int(1)),
                                    }
                                    .into()
                                ),
                            }
                            .into()
                        ],
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_parse_do_loop_until() {
        let input = indoc! {r#"
            Do
                x = x - 1
            Loop Until x = 0
        "#};
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::DoLoop {
                        check: DoLoopCheck::Post(DoLoopCondition::Until(Box::new(
                            InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("x")),
                                rhs: Box::new(Expr::int(0)),
                            }
                            .into()
                        ))),
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("x"),
                                value: Box::new(
                                    InfixOp {
                                        op: T![-],
                                        lhs: Box::new(Expr::ident("x")),
                                        rhs: Box::new(Expr::int(1)),
                                    }
                                    .into()
                                ),
                            }
                            .into()
                        ],
                    }
                    .into()
                )
                .into()
            ]
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
        let items = parse_file(input);
        assert_eq!(
            items,
            vec![
                ItemKind::Statement(
                    StmtKind::DoLoop {
                        check: DoLoopCheck::None,
                        body: vec![
                            StmtKind::Assignment {
                                full_ident: FullIdent::ident("x"),
                                value: Box::new(
                                    InfixOp {
                                        op: T![-],
                                        lhs: Box::new(Expr::ident("x")),
                                        rhs: Box::new(Expr::int(1)),
                                    }
                                    .into()
                                ),
                            }
                            .into(),
                            StmtKind::IfStmt {
                                condition: Box::new(
                                    InfixOp {
                                        op: T![=],
                                        lhs: Box::new(Expr::ident("x")),
                                        rhs: Box::new(Expr::int(0)),
                                    }
                                    .into()
                                ),
                                body: vec![StmtKind::ExitDo.into()],
                                elseif_statements: vec![],
                                else_stmt: None,
                            }
                            .into(),
                        ],
                    }
                    .into()
                )
                .into()
            ]
        );
    }

    #[test]
    fn test_property_access_in_args_without_space() {
        let input = "Foo 1,.enabled";
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::SubCall {
                fn_name: FullIdent::ident("Foo"),
                args: vec![
                    Some(Expr::int(1)),
                    Some(Expr::member(WithScoped.into(), "enabled")),
                ],
            }
            .into()
        );
    }

    #[test]
    fn test_step_as_identifier() {
        let input = "x=y.Step";
        let stmt = parse_stmt(input, true);
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
        let file = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(StmtKind::Dim {
                    vars: vec![VarDecl::new("Error")],
                }.into()).into(),
                ItemKind::Statement(StmtKind::Assignment {
                    full_ident: FullIdent::ident("Error"),
                    value: Box::new(Expr::int(42)),
                }.into()).into(),
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
        let file = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(
                    StmtKind::Sub {
                        visibility: Visibility::Default,
                        name: "MySub".into(),
                        parameters: vec![],
                        body: vec![
                            StmtKind::IfStmt {
                                condition: Box::new(Expr::bool(true)),
                                body: vec![
                                    StmtKind::Assignment {
                                        full_ident: FullIdent::ident("x"),
                                        value: Box::new(Expr::int(1)),
                                    }.into(),
                                ],
                                elseif_statements: vec![],
                                else_stmt: None,
                            }.into(),
                        ],
                    }.into()
                ).into()
            ]
        );
    }

    #[test]
    #[should_panic = "ParseError at line 2, column 28: Expected to consume `if`, but found `sub`"]
    fn parse_fail_when_end_sub_not_on_newline() {
        // Windows: compilation error: Expected 'If'
        let input = indoc! {r#"
            Sub MySub
                if true Then x = 1:end Sub
        "#};
        parse_file(input);
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
        let file = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                ItemKind::Class{
                    name: "Property".into(),
                    members: vec![],
                    dims: vec![],
                    member_accessors: vec![],
                    methods: vec![
                        StmtKind::Sub {
                            visibility: Visibility::Default,
                            name: "Property".into(),
                            parameters: vec![Argument::ByRef("property".into())],
                            body: vec![],
                        }.into(),
                    ],
                }.into(),
                ItemKind::Class{
                    name: "Property2".into(),
                    members: vec![],
                    dims: vec![],
                    member_accessors: vec![],
                    methods: vec![
                        StmtKind::Function {
                            visibility: Visibility::Default,
                            name: "Property".into(),
                            parameters: vec![],
                            body: vec![],
                        }.into(),
                    ],
                }.into(),
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
        parse_file(input);
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
        let file = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(StmtKind::Dim {
                    vars: vec![VarDecl::new("default")],
                }.into()).into(),
                ItemKind::Statement(StmtKind::Assignment {
                    full_ident: FullIdent::ident("default"),
                    value: Box::new(Expr::int(1)),
                }.into()).into(),
                ItemKind::Statement(StmtKind::Dim {
                    vars: vec![VarDecl::new("error")],
                }.into()).into(),
                ItemKind::Statement(StmtKind::Assignment {
                    full_ident: FullIdent::ident("error"),
                    value: Box::new(Expr::int(2)),
                }.into()).into(),
                ItemKind::Statement(StmtKind::Dim {
                    vars: vec![VarDecl::new("explicit")],
                }.into()).into(),
                ItemKind::Statement(StmtKind::Assignment {
                    full_ident: FullIdent::ident("explicit"),
                    value: Box::new(Expr::int(3)),
                }.into()).into(),
                ItemKind::Statement(StmtKind::Dim {
                    vars: vec![VarDecl::new("step")],
                }.into()).into(),
                ItemKind::Statement(StmtKind::Assignment {
                    full_ident: FullIdent::ident("step"),
                    value: Box::new(Expr::int(4)),
                }.into()).into(),
                ItemKind::Statement(StmtKind::Dim {
                    vars: vec![VarDecl::new("property")],
                }.into()).into(),
                ItemKind::Statement(StmtKind::Assignment {
                    full_ident: FullIdent::ident("property"),
                    value: Box::new(Expr::int(5)),
                }.into()).into(),
            ]
        );
    }

    #[test]
    fn parse_call_with_keyword_identifier() {
        let input = indoc! {r#"
            Call ok(error = "xx", "error = " & error & " expected ""xx""")
        "#};
        let file = parse_file(input);
        #[rustfmt::skip]
        assert_eq!(
            file,
            vec![
                ItemKind::Statement(StmtKind::Call(FullIdent::new(
                    Expr::fn_application(
                        Expr::ident("ok"),
                        vec![
                            InfixOp {
                                op: T![=],
                                lhs: Box::new(Expr::ident("error")),
                                rhs: Box::new(Expr::str("xx")),
                            }.into(),
                            InfixOp {
                                op: T![&],
                                lhs: Box::new(InfixOp {
                                    op: T![&],
                                    lhs: Box::new(Expr::str("error = ")),
                                    rhs: Box::new(Expr::ident("error")),
                                }.into()),
                                rhs: Box::new(Expr::str(" expected \"xx\"")),
                            }.into(),
                        ]
                    )
                )).into()).into(),
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
                T![ident],
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
            ItemKind::Statement(
                StmtKind::Assignment {
                    full_ident: FullIdent::new(Expr::member(Expr::ident("obj"), name)),
                    value: Box::new(Expr::int(10)),
                }
                .into(),
            )
            .into()
        }
        let file = parse_file(input);
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
        let stmt = parse_stmt(input, true);
        assert_eq!(
            stmt,
            StmtKind::SubCall {
                fn_name: FullIdent::ident("MySub"),
                args: vec![Some(
                    InfixOp {
                        op: T![&],
                        lhs: Box::new(Expr::str("test")),
                        rhs: Box::new(Expr::member(WithScoped.into(), "prop")),
                    }
                    .into()
                )],
            }
            .into()
        );
    }

    #[test]
    fn test_parse_error_api() {
        let error = Parser::new("x = 1\ny = )").file().unwrap_err();
        assert_eq!(error.line(), 2);
        assert_eq!(error.column(), 5);
        assert_eq!(error.message(), "Unknown start of expression: )");
        assert_eq!(
            error.to_string(),
            "line 2, column 5: Unknown start of expression: )"
        );
        // usable with `?` in functions returning a boxed error
        let boxed: Box<dyn std::error::Error + Send + Sync> = error.clone().into();
        assert_eq!(boxed.to_string(), error.to_string());
    }
}
