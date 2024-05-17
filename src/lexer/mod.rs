use crate::lexer::generated::LogosToken;
use logos::Logos;
pub use token::{Token, TokenKind};

use crate::lexer::TokenKind::ParseError;
use crate::T;

mod generated;
mod token;

//pub type Lexer<'input> = CustomLexer<'input>;
pub type Lexer<'input> = LogosLexer<'input>;

pub struct LogosLexer<'input> {
    generated: logos::SpannedIter<'input, LogosToken>,
    eof: bool,
}

impl<'input> LogosLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            generated: LogosToken::lexer(input).spanned(),
            eof: false,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }
}

impl<'input> Iterator for LogosLexer<'input> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.generated.next() {
            Some((token_result, span)) => match token_result {
                Ok(token) => {
                    let (line, column) = token.line_column();
                    //println!("{}: {}:{}", token.kind(), line, column);
                    Some(Token {
                        kind: token.kind(),
                        span: span.into(),
                        line,
                        column,
                    })
                }
                Err(_) => {
                    // TODO can we provide more information here?
                    //   can we get the line and column number?
                    Some(Token {
                        kind: ParseError,
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

    #[test]
    fn test_string_literal() {
        let input = r#""hello world""#;
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![string_literal], T![EOF],]);
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
    }

    #[test]
    fn test_lexer_comment_with_pipe() {
        let input = "' |\n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .into_iter()
            .filter(|t| t.kind != T![ws])
            .collect();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![comment], T![nl], T![EOF],]);
    }

    #[test]
    fn test_lexer_rem_comment() {
        let input = "REM comment here\n";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .into_iter()
            .filter(|t| t.kind != T![ws])
            .collect();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![comment], T![nl], T![EOF],]);
    }

    #[test]
    fn test_lexer_not_rem() {
        // this used to lexed as a comment
        let input = "Private Sub RemoveBall(aBall)";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .into_iter()
            .filter(|t| t.kind != T![ws])
            .collect();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
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
    }

    #[test]
    fn test_lexer_string_with_escaped_quotes() {
        let input = r#"
        str = "hello ""world"""
    "#
        .trim();
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer
            .tokenize()
            .into_iter()
            .filter(|t| t.kind != T![ws])
            .collect();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [T![ident], T![=], T![string_literal], T![EOF],]
        );
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
        let input = "&H10";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(tokens, vec![T![hex_integer_literal], T![EOF],]);
    }

    #[test]
    fn octal_integer_literal() {
        let input = "&O10";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(tokens, vec![T![octal_integer_literal], T![EOF],]);
    }

    #[test]
    fn double_science_notation() {
        let input = "1.401298E-45";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(tokens, vec![T![real_literal], T![EOF],]);
    }

    #[test]
    fn multi_level_property_access() {
        let input = "obj.prop1.prop2.prop3";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).collect();
        assert_eq!(
            tokens,
            vec![
                T![ident],
                T![.],
                T![ident],
                T![.],
                T![ident],
                T![.],
                T![ident],
                T![EOF],
            ]
        );
    }

    #[test]
    fn keyword_part_of_identifier() {
        let input = indoc! {r#"
        ' Stop comment
        Sub stop_sequencer()
            StopSound("metalrolling")
        End Sub"#};
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).filter(|t| t != &T![ws]).collect();
        assert_eq!(
            tokens,
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
    }

    #[test]
    fn error_handling() {
        let input = indoc! {r#"
        on error resume next
        On Error Goto 0
        "#};
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.map(|t| t.kind).filter(|tk| tk != &T![ws]).collect();
        assert_eq!(
            tokens,
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
    }

    #[test]
    fn parse_two_consts_with_comments() {
        let input = indoc! {"
            Const x = 42 ' The answer to everything
            Const y = 13 ' An unlucky number
        "};
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
                T![const],
                T![ident],
                T![=],
                T![integer_literal],
                T![comment],
                T![nl],
                T![EOF],
            ]
        );
    }

    #[test]
    fn test_line_continuations_crlf() {
        let input = "x = txt & _\r\ntxt2";
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
                T![ident],
                T![=],
                T![ident],
                T![&],
                T![line_continuation],
                T![ident],
                T![EOF],
            ]
        );
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
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(
            tokens,
            [
                T![ident],
                T![ws],
                T![&],
                T![line_continuation],
                T![ident],
                T![EOF],
            ]
        );
    }

    #[test]
    fn test_newlines() {
        // CRLF should get processed as a single newline
        let input = "\r\n\n\r";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![nl], T![nl], T![nl], T![EOF],]);
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
    }

    #[test]
    fn test_identifier_can_end_with_underscore() {
        let input = "x_";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize().iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [T![ident], T![EOF],]);
    }

    #[test]
    fn float_literal_without_fraction() {
        let input = "1.";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![real_literal], T![EOF],]);
    }

    #[test]
    fn float_literal_without_integral() {
        let input = ".1";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![real_literal], T![EOF],]);
    }

    #[test]
    fn tokenize_spaced_property_access() {
        // This should even work with _ between the . and the property name
        let input = "o. _\n s";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                T![ident],
                T![.],
                T![ws],
                T![line_continuation],
                T![ws],
                T![ident],
                T![EOF],
            ]
        );
    }

    #[test]
    fn tokenize_spaced_property_access_invalid() {
        // Fails on windows with: runtime error: Invalid or unqualified reference
        let input = "o .s";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.tokenize();
        let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(token_kinds, [T![ident], T![ws], T![.], T![ident], T![EOF],]);
    }
}
