use crate::lexer::generated::LogosToken;
pub use line_index::LineIndex;
use logos::Logos;
pub use token::{Span, Token, TokenKind};

use crate::T;

mod generated;
mod keyword;
mod line_index;
mod token;

//pub type Lexer<'input> = CustomLexer<'input>;
/// A lexer for the VBA language.
/// This splits the input into tokens.
///
/// The end of the input is marked by a `TokenKind::EOF` token.
/// Any token that is not recognized is returned as a `TokenKind::ParseError`.
pub type Lexer<'input> = LogosLexer<'input>;

pub struct LogosLexer<'input> {
    generated: logos::SpannedIter<'input, LogosToken>,
    eof: bool,
    prev_token: Token,
    /// The last token was a dot, not counting whitespace and line continuations.
    after_dot: bool,
    queued_token: Option<Token>,
}

impl<'input> LogosLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            generated: LogosToken::lexer(input).spanned(),
            eof: false,
            prev_token: Token {
                kind: T![nl],
                span: (0..0).into(),
                line: 1,
                column: 1,
            },
            after_dot: false,
            queued_token: None,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }

    /// The 1-indexed line and column of an offset on the line the lexer is currently at.
    ///
    /// Used for tokens that do not carry their own position.
    fn line_column(&self, offset: usize) -> (usize, usize) {
        let (line, line_start) = self.generated.extras;
        (line + 1, offset - line_start + 1)
    }
}

impl Iterator for LogosLexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.queued_token.take() {
            self.queued_token = None;
            return Some(token);
        }
        match self.generated.next() {
            Some((token_result, span)) => match token_result {
                Ok(token) => {
                    //println!("{:?} {:?}", token, span);
                    let mut current_token = token;

                    // keywords are not known to the generated lexer, they arrive as identifiers
                    let word = match current_token {
                        LogosToken::Ident(_) => Some(&self.generated.source()[span.clone()]),
                        _ => None,
                    };
                    // A word that follows a dot is a member name, also if it is a keyword:
                    // `x.end`, `x.rem` and `.default` in a with block are all valid.
                    let after_dot = std::mem::take(&mut self.after_dot);
                    let word = word.filter(|_| !after_dot);
                    let keyword = word.and_then(keyword::keyword_kind);
                    // We can't handle this one as a comment in the generated lexer because
                    // x.rem is a valid member access
                    let is_rem = word.is_some_and(|word| word.eq_ignore_ascii_case("rem"));

                    // if we find a REM we see it as a comment until the end of the line
                    if is_rem {
                        let (rem_line, rem_column) = current_token.line_column();
                        let rem_span = span.clone();
                        let mut current_span = span.clone();
                        // this will also consume any error tokens which are expected
                        while !matches!(current_token, LogosToken::NewLine(_)) {
                            match self.generated.next() {
                                Some((token_result, span)) => {
                                    match token_result {
                                        Ok(token) => {
                                            //println!("consuming rem {:?} {:?}", token, span);
                                            current_token = token;
                                            current_span = span;
                                        }
                                        Err(_) => {
                                            // we also consume anything that could not be tokenized
                                            // current_token = token;
                                            current_span = span;
                                        }
                                    }
                                }
                                None => {
                                    // return the comment, the eof will be handled in the next iteration
                                    let comment_span = rem_span.start..current_span.end;
                                    return Some(Token {
                                        kind: T![comment],
                                        span: comment_span.into(),
                                        line: rem_line,
                                        column: rem_column,
                                    });
                                }
                            }
                        }
                        // queue the newline token, which has its own position
                        let (nl_line, nl_column) = current_token.line_column();
                        self.queued_token = Some(Token {
                            kind: T![nl],
                            span: (current_span.start..current_span.end).into(),
                            line: nl_line,
                            column: nl_column,
                        });
                        // return the comment
                        let comment_span = rem_span.start..current_span.start;
                        return Some(Token {
                            kind: T![comment],
                            span: comment_span.into(),
                            line: rem_line,
                            column: rem_column,
                        });
                    }

                    let current_kind = keyword.unwrap_or_else(|| current_token.kind());
                    let (line, column) = match current_token {
                        LogosToken::WS | LogosToken::Comment => self.line_column(span.start),
                        _ => current_token.line_column(),
                    };

                    // translate [non-whitepace, .] to [non-whitepace, _.]
                    // without lookahead/back on the lexer we can't do this kind of check
                    // TODO would it not be better to do a positive check here checking for valid cases?
                    // A dot directly after `Then` or `Else` (`If x Then.prop = 1`) is a
                    // with-statement dot, not a member access on the keyword.
                    if !matches!(
                        self.prev_token.kind,
                        T![nl]
                            | T![ws]
                            | T![:]
                            | T!['(']
                            | T![-]
                            | T![,]
                            | T![&]
                            | T![then]
                            | T![else]
                    ) && matches!(current_kind, T![.])
                    {
                        let replacement_token = Token {
                            kind: T![_.],
                            span: span.into(),
                            line,
                            column,
                        };
                        self.prev_token = replacement_token;
                        self.after_dot = true;
                        return Some(replacement_token);
                    }
                    let token = Token {
                        kind: current_kind,
                        span: span.into(),
                        line,
                        column,
                    };
                    self.prev_token = token;
                    self.after_dot = match current_kind {
                        T![.] => true,
                        // there can be whitespace between the dot and the member name
                        T![ws] | T![line_continuation] => after_dot,
                        _ => false,
                    };
                    Some(token)
                }
                Err(_) => {
                    let (line, column) = self.line_column(span.start);
                    Some(Token {
                        kind: TokenKind::ParseError,
                        span: span.into(),
                        line,
                        column,
                    })
                }
            },
            None if self.eof => None,
            None => {
                self.eof = true;
                let end = self.generated.source().len();
                let (line, column) = self.line_column(end);
                Some(Token {
                    kind: T![EOF],
                    span: (end..end).into(),
                    line,
                    column,
                })
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::T;
    use crate::lexer::{Lexer, Span, Token, TokenKind};
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn reconstruct(input: &&str, tokens: Vec<Token>) -> String {
        tokens.iter().map(|t| &input[t.span]).collect::<String>()
    }

    #[test]
    fn parse_error() {
        let input = "$";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![parse_error], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    /// Digits are ASCII only: `cscript` on Windows rejects any other Unicode digit
    /// with error 1032 "Invalid character", both in numbers and in identifiers.
    #[test]
    fn non_ascii_digit_is_parse_error() {
        // ARABIC-INDIC DIGIT THREE, FULLWIDTH DIGIT THREE, DEVANAGARI DIGIT THREE
        for digit in ['\u{0663}', '\u{FF13}', '\u{0969}'] {
            let input = format!("1{digit}");
            let tokens = Lexer::new(&input).tokenize();
            let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
            assert_eq!(
                token_kinds,
                [T![integer_literal], T![parse_error], T![EOF]],
                "{input}"
            );

            let input = format!("a{digit}");
            let tokens = Lexer::new(&input).tokenize();
            let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
            assert_eq!(
                token_kinds,
                [T![ident], T![parse_error], T![EOF]],
                "{input}"
            );
        }
    }

    #[test]
    fn position_of_tokens_without_own_position() {
        let input = "x = 1\n  y = 2 $ ' note\n  ";
        let tokens = Lexer::new(input).tokenize();
        let positions = |kind: TokenKind| -> Vec<(usize, usize)> {
            tokens
                .iter()
                .filter(|t| t.kind == kind)
                .map(|t| (t.line, t.column))
                .collect()
        };
        assert_eq!(positions(T![parse_error]), [(2, 9)]);
        assert_eq!(positions(T![comment]), [(2, 11)]);
        assert_eq!(
            positions(T![ws]),
            [
                (1, 2),
                (1, 4),
                (2, 1),
                (2, 4),
                (2, 6),
                (2, 8),
                (2, 10),
                (3, 1)
            ]
        );
        assert_eq!(positions(T![EOF]), [(3, 3)]);
    }

    #[test]
    fn position_of_eof_after_newline() {
        let tokens = Lexer::new("x\r\n").tokenize();
        let eof = tokens.last().unwrap();
        assert_eq!((eof.kind, eof.line, eof.column), (T![EOF], 2, 1));
    }

    /// These are reserved on Windows, `cscript` fails with error 1010 "Expected identifier"
    /// when they are used as a variable name.
    #[test]
    fn reserved_words_without_own_token_kind() {
        for word in [
            "currency",
            "endif",
            "enum",
            "event",
            "implements",
            "like",
            "lset",
            "optional",
            "paramarray",
            "raiseevent",
            "rset",
            "shared",
            "single",
            "static",
            "typeof",
            "variant",
            "as",
            "byte",
            "boolean",
            "double",
            "integer",
            "long",
            "type",
        ] {
            let tokens = Lexer::new(word).tokenize();
            let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
            assert_eq!(token_kinds, [T![unused], T![EOF]], "{word}");
        }
    }

    /// `cscript` on Windows accepts every keyword as a member name, the wine lexer does not
    /// look for keywords after a dot either.
    #[test]
    fn word_after_dot_is_an_identifier() {
        let kinds = |input: &str| -> Vec<_> {
            Lexer::new(input)
                .map(|t| t.kind)
                .filter(|kind| !matches!(kind, T![ws] | T![line_continuation]))
                .collect()
        };
        for word in ["end", "rem", "default", "error", "true", "type", "x"] {
            let expected = [
                T![ident],
                T![_.],
                T![ident],
                T![=],
                T![integer_literal],
                T![EOF],
            ];
            assert_eq!(kinds(&format!("o.{word} = 1")), expected, "{word}");
            // there can be whitespace or a line continuation after the dot
            assert_eq!(kinds(&format!("o. {word} = 1")), expected, "{word}");
            assert_eq!(kinds(&format!("o. _\n {word} = 1")), expected, "{word}");
            // the dot of a with block
            assert_eq!(
                kinds(&format!(".{word} = 1")),
                [T![.], T![ident], T![=], T![integer_literal], T![EOF]],
                "{word}"
            );
        }
        // only the word right after the dot
        assert_eq!(
            kinds("o.end end"),
            [T![ident], T![_.], T![ident], T![end], T![EOF]]
        );
    }

    /// A line end is on the line that it ends, at the column where it starts.
    #[test]
    fn position_of_line_ends() {
        let input = "ab\r\nx = 1 + _\n  2 Rem note\n";
        let tokens = Lexer::new(input).tokenize();
        let line_ends: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(t.kind, T![nl] | T![line_continuation]))
            .map(|t| (t.kind, t.line, t.column))
            .collect();
        assert_eq!(
            line_ends,
            [
                (T![nl], 1, 3),
                (T![line_continuation], 2, 9),
                // after a comment that starts with `rem`
                (T![nl], 3, 13),
            ]
        );
    }

    #[test]
    fn string_literal() {
        let input = r#""hello world""#;
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![string_literal], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn test_lexer_options() {
        let input = indoc! {r#"
        Option Explicit
    "#};
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [T![option], T![ws], T![ident], T![nl], T![EOF],]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn comment_with_pipe() {
        let input = "' |\n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![comment], T![nl], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn rem_comment_with_newline() {
        let input = "REM comment here\n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![comment], T![nl], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn rem_comment_without_newline() {
        let input = "REM comment here";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![comment], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn rem_comment_trailing() {
        let input = "dim x:REM comment here";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [T![dim], T![ws], T![ident], T![:], T![comment], T![EOF],]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn ident_not_rem() {
        // this used to lexed as a comment
        let input = "Private Sub RemoveBall(aBall)";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens
            .iter()
            .filter(|t| t.kind != T![ws])
            .map(|t| t.kind)
            .collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![private],
                T![sub],
                T![ident],
                T!['('],
                T![ident],
                T![')'],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn string_with_escaped_quotes() {
        let input = r#"
        str = "hello ""world"""
    "#
        .trim();
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens
            .iter()
            .filter(|t| t.kind != T![ws])
            .map(|t| t.kind)
            .collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [T![ident], T![=], T![string_literal], T![EOF],]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn tokenize_all_operators() {
        // we can't make a difference between unary negation and subtraction at this level
        let input = "^ * / \\ Mod + - & = <> < > <= =< >= => Is Not And Or Xor Eqv Imp";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).filter(|tk| tk != &T![ws]).collect();
        assert_eq!(
            tokens,
            vec![
                T![^],
                T![*],
                T![/],
                T!['\\'],
                T![mod],
                T![+],
                T![-],
                T![&],
                T![=],
                T![<>],
                T![<],
                T![>],
                T![<=],
                T![<=],
                T![>=],
                T![>=],
                T![is],
                T![not],
                T![and],
                T![or],
                T![xor],
                T![eqv],
                T![imp],
                T![EOF],
            ]
        );
    }

    #[test]
    fn hex_integer_literal() {
        let input = "&H10 &h80040000&";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(
            tokens,
            vec![
                T![hex_integer_literal],
                T![ws],
                T![hex_integer_literal],
                T![EOF],
            ]
        );
    }

    #[test]
    fn octal_integer_literal() {
        // octal literals: the `O` after `&` is optional and a `&` Long suffix is allowed
        let input = "&010+&o17&";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(
            tokens,
            vec![
                T![octal_integer_literal],
                T![+],
                T![octal_integer_literal],
                T![EOF],
            ]
        );
    }

    #[test]
    fn double_science_notation() {
        let input = "1.401298E-45";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let tokens_kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(tokens_kinds, vec![T![real_literal], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn keyword_part_of_identifier() {
        let input = indoc! {r#"
        ' Stop comment
        Sub stop_sequencer()
            StopSound("metalrolling")
        End Sub"#};
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let token_kinds: Vec<_> = tokens
            .iter()
            .map(|t| t.kind)
            .filter(|t| t != &T![ws])
            .collect();
        assert_eq!(
            token_kinds,
            [
                T![comment],
                T![nl],
                T![sub],
                T![ident],
                T!['('],
                T![')'],
                T![nl],
                T![ident],
                T!['('],
                T![string_literal],
                T![')'],
                T![nl],
                T![end],
                T![sub],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn error_handling() {
        let input = indoc! {r#"
        on error resume next
        On Error Goto 0
        "#};
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let token_kinds: Vec<_> = tokens
            .iter()
            .map(|t| t.kind)
            .filter(|tk| tk != &T![ws])
            .collect();
        assert_eq!(
            token_kinds,
            vec![
                T![on],
                T![error],
                T![resume],
                T![next],
                T![nl],
                T![on],
                T![error],
                T![goto],
                T![integer_literal],
                T![nl],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn test_lexer_continuation_character() {
        let input = indoc! {r#"
        a = 1 _
            + 2 _
            + 3
        "#};
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens
            .iter()
            .map(|t| t.kind)
            .filter(|tk| tk != &T![ws])
            .collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![ident],
                T![=],
                T![integer_literal],
                T![line_continuation],
                T![+],
                T![integer_literal],
                T![line_continuation],
                T![+],
                T![integer_literal],
                T![nl],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn parse_two_consts_with_comments() {
        let input = indoc! {"
            Const x = 42 ' The answer to everything
            Const y = 13 ' An unlucky number
        "};
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let token_kinds: Vec<_> = tokens
            .iter()
            .map(|t| t.kind)
            .filter(|t| t != &T![ws])
            .collect();
        assert_eq!(
            token_kinds,
            [
                T![const],
                T![ident],
                T![=],
                T![integer_literal],
                T![comment],
                T![nl],
                T![const],
                T![ident],
                T![=],
                T![integer_literal],
                T![comment],
                T![nl],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn test_line_continuations_crlf() {
        let input = "x = txt & _\r\ntxt2";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let token_kinds: Vec<_> = tokens
            .iter()
            .map(|t| t.kind)
            .filter(|t| t != &T![ws])
            .collect();
        assert_eq!(
            token_kinds,
            [
                T![ident],
                T![=],
                T![ident],
                T![&],
                T![line_continuation],
                T![ident],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn test_line_continuations() {
        let input = " _ \t \r\n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![ws], T![line_continuation], T![EOF],]);
        let input = " _  \n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![ws], T![line_continuation], T![EOF],]);
        let input = " _\r";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![ws], T![line_continuation], T![EOF],]);
        let input = "this &_\r\nthat";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let token_kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            token_kinds,
            [
                T![ident],
                T![ws],
                T![&],
                T![line_continuation],
                T![ident],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn test_newlines() {
        // CRLF should get processed as a single newline
        let input = "\r\n\n\r";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let token_kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, [T![nl], T![nl], T![nl], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    fn token(kind: TokenKind, span: std::ops::Range<u32>, line: usize, column: usize) -> Token {
        Token {
            kind,
            span: Span {
                start: span.start,
                end: span.end,
            },
            line,
            column,
        }
    }

    #[test]
    fn test_identifier_cant_start_with_underscore() {
        let input = "_x";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_eq!(
            tokens,
            [
                token(T![parse_error], 0..1, 1, 1),
                token(T![ident], 1..2, 1, 2),
                token(T![EOF], 2..2, 1, 3),
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn test_identifier_can_end_with_underscore() {
        let input = "x_";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let token_kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(token_kinds, [T![ident], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn float_literal_without_fraction() {
        let input = "1.";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![real_literal], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn float_literal_without_integral() {
        let input = ".1";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![real_literal], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn tokenize_space_dot_float() {
        let input = " .3";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![ws], T![real_literal], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn float_literal_without_fraction_with_exponent() {
        let input = "2e400";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![real_literal], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    /// `cscript` on Windows evaluates `12e3` to 12000 and `123E-2` to 1.23.
    #[test]
    fn float_literal_without_fraction_with_multi_digit_integral() {
        for input in ["12e3", "123E-2", "10e+2"] {
            let tokens = Lexer::new(input).tokenize();
            let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
            assert_eq!(token_kinds, [T![real_literal], T![EOF]], "{input}");
        }
        // an integer directly followed by a keyword starting with `e` stays an integer
        let tokens = Lexer::new("12else").tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![integer_literal], T![else], T![EOF]]);
    }

    #[test]
    fn tokenize_member_access() {
        // for member access there can't be a space between the object and the dot
        let input = "o.s";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![ident], T![_.], T![ident], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn tokenize_with_dot_after_then_and_else() {
        // a dot directly after `Then` or `Else` is a with-statement dot
        let input = "If a Then.b = 1 Else.c = 2";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens
            .iter()
            .filter(|t| t.kind != T![ws])
            .map(|t| t.kind)
            .collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![if],
                T![ident],
                T![then],
                T![.],
                T![ident],
                T![=],
                T![integer_literal],
                T![else],
                T![.],
                T![ident],
                T![=],
                T![integer_literal],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn tokenize_member_access_2() {
        // whitespace behind the dot and the member is allowed
        let input = "o. s";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [T![ident], T![_.], T![ws], T![ident], T![EOF],]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn tokenize_spaced_member_access() {
        // This should even work with _ between the . and the property name
        let input = "o. _\n s";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![ident],
                T![_.],
                T![ws],
                T![line_continuation],
                T![ws],
                T![ident],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn tokenize_member_access_invalid() {
        // Fails on windows with: runtime error: Invalid or unqualified reference
        let input = "o .s";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![ident], T![ws], T![.], T![ident], T![EOF],]);
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn multi_level_member_access() {
        let input = "obj.prop1.prop2.prop3";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let token_types: Vec<_> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            token_types,
            vec![
                T![ident],
                T![_.],
                T![ident],
                T![_.],
                T![ident],
                T![_.],
                T![ident],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }

    #[test]
    fn tokenize_date_literals() {
        // Fails on windows with: runtime error: Invalid or unqualified reference
        // `#1/15/2024#` has a day (15) whose second digit is not 0-2
        let input = "#1/1/2000# #12/31/2000# #1/15/2024# #1899-12-31# #112-31# # 1/1/2011 #";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![date_time_literal],
                T![ws],
                T![date_time_literal],
                T![ws],
                T![date_time_literal],
                T![ws],
                T![date_time_literal],
                T![ws],
                T![date_time_literal],
                T![ws],
                T![date_time_literal],
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }
}
