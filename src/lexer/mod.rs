use crate::lexer::generated::LogosToken;
use logos::Logos;
pub use token::{Token, TokenKind};

use crate::T;

mod generated;
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
    prev_token_kind: TokenKind,
    queued_token: Option<Token>,
}

impl<'input> LogosLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            generated: LogosToken::lexer(input).spanned(),
            eof: false,
            prev_token_kind: TokenKind::Newline,
            queued_token: None,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }
}

impl<'input> Iterator for LogosLexer<'input> {
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

                    // Some tokens are transformed to identifiers when they are following member access
                    // TODO look at the last non-whitespace character
                    // TODO add more cases
                    if matches!(self.prev_token_kind, T![_.]) {
                        match current_token {
                            LogosToken::KwTrue(_)
                            | LogosToken::KwFalse(_)
                            | LogosToken::KwRem(_) => {
                                let (line, column) = current_token.line_column();
                                let rem_ident = Token {
                                    kind: T![ident],
                                    span: span.into(),
                                    line,
                                    column,
                                };
                                self.prev_token_kind = rem_ident.kind;
                                return Some(rem_ident);
                            }
                            _ => {}
                        }
                    }

                    // if we find a REM we see it as a comment until the end of the line
                    if let LogosToken::KwRem(_) = current_token {
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
                        // queue the newline token
                        self.queued_token = Some(Token {
                            kind: T![nl],
                            span: (current_span.start..current_span.end).into(),
                            line: rem_line,
                            column: rem_column,
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

                    let current_kind = current_token.kind();
                    let (line, column) = current_token.line_column();

                    // translate [non-whitepace, .] to [non-whitepace, _.]
                    // without lookahead/back on the lexer we can't do this kind of check
                    // TODO would it not be better to do a positive check here checking for valid cases?
                    if !matches!(
                        self.prev_token_kind,
                        T![nl] | T![ws] | T![:] | T!['('] | T![-] | T![,] | T![&]
                    ) && matches!(current_kind, T![.])
                    {
                        let replacement_token = Token {
                            kind: T![_.],
                            span: span.into(),
                            line,
                            column,
                        };
                        self.prev_token_kind = replacement_token.kind;
                        return Some(replacement_token);
                    }

                    self.prev_token_kind = current_kind;

                    Some(Token {
                        kind: current_kind,
                        span: span.into(),
                        line,
                        column,
                    })
                }
                Err(_) => {
                    // TODO can we provide more information here?
                    //   can we get the line and column number?
                    Some(Token {
                        kind: TokenKind::ParseError,
                        span: span.into(),
                        line: 0,
                        column: 0,
                    })
                }
            },
            None if self.eof => None,
            None => {
                self.eof = true;
                // TODO can we provide more information here?
                //   can we get the line and column number?
                Some(Token {
                    kind: T![EOF],
                    span: (0..0).into(),
                    line: 0,
                    column: 0,
                })
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{Lexer, Token};
    use crate::T;
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
        // 0 prefix not mandatory
        let input = "&010+&10";
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

    #[test]
    fn test_identifier_cant_start_with_underscore() {
        let input = "_x";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        assert_eq!(
            tokens,
            [
                Token::error(0..1),
                Token::ident(1..2, 0, 1),
                Token::eof(0..0)
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
        let input = "#1/1/2000# #12/31/2000# #1899-12-31# #112-31# # 1/1/2011 #";
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
                T![EOF],
            ]
        );
        let reconstructed = reconstruct(&input, tokens);
        assert_eq!(reconstructed, input);
    }
}
