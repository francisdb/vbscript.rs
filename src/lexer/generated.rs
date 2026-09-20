use super::TokenKind;
use crate::T;
use logos::{Lexer, Logos};

/* ANCHOR: callbacks */
/// Update the line count and the char index.
///
/// Gives the position of the line end itself, which is on the line that it ends.
fn newline_callback(lex: &mut Lexer<LogosToken>) -> (usize, usize) {
    let position = word_callback(lex);
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
    lex.extras.2 = 0;
    position
}

/// Compute the line and column position for the current word.
///
/// The column counts characters. The offsets are in bytes, so what the line so far has
/// more in bytes than in characters is taken off.
fn word_callback(lex: &mut Lexer<LogosToken>) -> (usize, usize) {
    let line = lex.extras.0;
    let column = lex.span().start - lex.extras.1 - lex.extras.2;

    (line, column)
}

/// The position of a token that can have characters of more than one byte: a string, a
/// comment or a name in brackets. All other rules only match ASCII.
fn wide_callback(lex: &mut Lexer<LogosToken>) -> (usize, usize) {
    let position = word_callback(lex);
    count_wide_characters(lex);
    position
}

/// Keeps track of what the line has more in bytes than in characters.
pub(super) fn count_wide_characters(lex: &mut Lexer<LogosToken>) {
    let text = lex.slice();
    lex.extras.2 += text.len() - text.chars().count();
}
/* ANCHOR_END: callbacks */

#[derive(Logos, Debug, PartialEq, Eq)]
// the line, the offset of its start, and how many bytes it has more than characters so far
#[logos(extras = (usize, usize, usize))]
pub(super) enum LogosToken {
    // A property access can not have whitespace between the name and the dot, but it can
    // after the dot. Which of the two a dot is depends on the token before it, which the
    // lexer that wraps this one looks at: it turns this `Dot` into a `_.` for a property
    // access and leaves it as the dot of a with block otherwise.
    #[token(".", word_callback)]
    Dot((usize, usize)),
    #[token(":", word_callback)]
    Colon((usize, usize)),
    #[token(",", word_callback)]
    Comma((usize, usize)),
    #[token(";", word_callback)]
    Semi((usize, usize)),
    #[token("+", word_callback)]
    Plus((usize, usize)),
    #[token("-", word_callback)]
    Minus((usize, usize)),
    #[token("*", word_callback)]
    Times((usize, usize)),
    #[token("/", word_callback)]
    Slash((usize, usize)),
    #[token("\\", word_callback)]
    BackSlash((usize, usize)),
    #[token("^", word_callback)]
    Pow((usize, usize)),
    #[token("=", word_callback)]
    Eq((usize, usize)),
    #[regex(r"<>|><", word_callback)]
    Neq((usize, usize)),
    #[regex(r#"<=|=<"#, word_callback)]
    Leq((usize, usize)),
    #[regex(r#">=|=>"#, word_callback)]
    Geq((usize, usize)),
    #[token("&", word_callback)]
    Ampersand((usize, usize)),
    // Brackets
    #[token("<", word_callback)]
    LAngle((usize, usize)),
    #[token(">", word_callback)]
    RAngle((usize, usize)),
    #[token("(", word_callback)]
    LParen((usize, usize)),
    #[token(")", word_callback)]
    RParen((usize, usize)),
    // Constructs
    #[regex(r#""([^"]|"")*""#, wide_callback)]
    String((usize, usize)),
    #[regex(r#"[0-9]+"#, word_callback, priority = 6)]
    Int((usize, usize)),
    // A & suffix means that the number is of the type Long (32 bit integer).
    #[regex(r#"&[Hh][0-9A-Fa-f]+&?"#, word_callback, priority = 6)]
    HexInt((usize, usize)),
    // Octal literals: an optional `O` after the `&`, octal digits, and an optional
    // `&` Long suffix, e.g. `&O17`, `&17` or `&O17&`. The `O` really is optional in
    // VBScript: `cscript` on Windows evaluates `&10000000` to octal 2097152 (note that
    // the wine vbscript engine wrongly rejects the bare `&<digits>` form).
    #[regex(r#"&[Oo]?[0-7]+&?"#, word_callback, priority = 6)]
    OctalInt((usize, usize)),
    #[regex(
        r#"(([0-9]+\.[0-9]*)|(\.[0-9]+))([Ee](\+|-)?[0-9]+)?|[0-9]+([Ee](\+|-)?[0-9]+)"#,
        word_callback,
        priority = 100
    )]
    Float((usize, usize)),
    // A date, a time or both, in one of the many forms that `cscript` on Windows takes:
    // `#1/2/2020#`, `#2020-01-02#`, `#1/2/20#`, `#12-25#`, `#Jan 2, 2020#`, `#2 Jan 2020#`,
    // `#10:30#`, `#10:30:15 PM#`, `#1/2/2020 10:30:00 AM#`, with spaces around it. It fails
    // for `#1.2.2020#`, for `##` and for `#hello#`, so there has to be a digit and there is
    // no dot. It also fails for a date that does not exist like `#13/13/2020#` and for
    // `#123-45#`, which needs a calendar to tell; those are a date literal here.
    #[regex(r"#[ 0-9A-Za-z/:,\-]*[0-9][ 0-9A-Za-z/:,\-]*#", word_callback)]
    DateTime((usize, usize)),

    // Keywords also arrive as `Ident`, there are no rules for them on purpose. The lexer
    // that wraps this one resolves them with `keyword::keyword_kind`.
    //
    // As `#[token("dim", ignore(case))]` rules they made up most of the generated state
    // machine: about 80 case-insensitive words that all overlap with the identifier rule.
    // Without them a release build of this crate takes a third of the time and the lexer
    // is half the machine code, for the same lexing speed.
    //
    // It also fits the language better: whether a word is a keyword depends on the
    // context, `end` is a member name in `x.end`, and only the wrapping lexer knows that.
    #[regex(r#"([A-Za-z])([A-Za-z]|_|[0-9])*"#, word_callback)]
    // Escaped/bracketed identifier, e.g. `[L178 Side Flasher]`, which may contain
    // spaces and other characters that are not valid in a bare identifier.
    #[regex(r#"\[[^\]]*\]"#, wide_callback)]
    Ident((usize, usize)),

    // Misc
    #[regex(r"[ \t\f]+")]
    WS,
    #[regex(r"(\r\n?|\n)", newline_callback)]
    NewLine((usize, usize)),
    // A line with only a `_` is a line continuation as well, there is nothing to do for
    // that: the whitespace before it is its own token.
    // (\r\n?|\n) matches \r\n, \r, and \n
    #[regex(r"_[ \t\f]*(\r\n?|\n)", newline_callback)]
    LineContinuation((usize, usize)),

    // comments using '
    #[regex(r"(?i)'[^\r\n]*", wide_callback, allow_greedy = true)]
    Comment((usize, usize)),
}

impl LogosToken {
    pub fn line_column(&self) -> (usize, usize) {
        use LogosToken::*;
        let line_col = match self {
            Dot((line, column)) => (*line, *column),
            NewLine((line, column)) => (*line, *column),
            Ampersand((line, column)) => (*line, *column),
            Colon((line, column)) => (*line, *column),
            Comma((line, column)) => (*line, *column),
            Ident((line, column)) => (*line, *column),
            String((line, column)) => (*line, *column),
            Int((line, column)) => (*line, *column),
            HexInt((line, column)) => (*line, *column),
            OctalInt((line, column)) => (*line, *column),
            Float((line, column)) => (*line, *column),
            DateTime((line, column)) => (*line, *column),
            Plus((line, column)) => (*line, *column),
            Minus((line, column)) => (*line, *column),
            Times((line, column)) => (*line, *column),
            Slash((line, column)) => (*line, *column),
            BackSlash((line, column)) => (*line, *column),
            Pow((line, column)) => (*line, *column),
            Eq((line, column)) => (*line, *column),
            Neq((line, column)) => (*line, *column),
            Leq((line, column)) => (*line, *column),
            Geq((line, column)) => (*line, *column),
            LAngle((line, column)) => (*line, *column),
            RAngle((line, column)) => (*line, *column),
            LParen((line, column)) => (*line, *column),
            RParen((line, column)) => (*line, *column),
            Semi((line, column)) => (*line, *column),
            WS => (0, 0),
            Comment((line, column)) => (*line, *column),
            LineContinuation((line, column)) => (*line, *column),
        };
        // line and column are 0-indexed
        // further down lines and columns are 1-indexed
        (line_col.0 + 1, line_col.1 + 1)
    }

    #[rustfmt::skip]
    pub fn kind(&self) -> TokenKind {
        use LogosToken::*;
        match self {
            Dot(_)          => T![.],
            Colon(_)        => T![:],
            Comma(_)        => T![,],
            Semi(_)         => T![;],
            Plus(_)         => T![+],
            Minus(_)        => T![-],
            Times(_)        => T![*],
            Slash(_)        => T![/],
            BackSlash(_)    => T!['\\'],
            Pow(_)          => T![^],
            Eq(_)           => T![=],
            Neq(_)          => T![<>],
            Leq(_)          => T![<=],
            Geq(_)          => T![>=],
            LAngle(_)       => T![<],
            RAngle(_)       => T![>],
            Ampersand(_) => T![&],
            LParen(_)       => T!['('],
            RParen(_)       => T![')'],
            String(_)    => T![string_literal],
            Int(_)       => T![integer_literal],
            HexInt(_)    => T![hex_integer_literal],
            OctalInt(_)  => T![octal_integer_literal],
            Float(_)     => T![real_literal],
            DateTime(_)  => T![date_time_literal],
            Ident(_)     => T![ident],
            WS           => T![ws],
            Comment(_)   => T![comment],
            NewLine(_)   => T![nl],
            LineContinuation(_) => T![line_continuation],
        }
    }
}

#[cfg(test)]
mod test {
    use super::LogosToken;
    use crate::T;
    use logos::Logos;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_lexer() {
        let input = "Dim x:x = 42\n";
        let lexer = LogosToken::lexer(input);
        let token_kinds = lexer
            .spanned()
            .map(|(res, span)| match res {
                Ok(t) => t.kind(),
                Err(_e) => {
                    let section = &input[span.start..span.end];
                    panic!(
                        "Some error occurred between char {} and {}: {}",
                        span.start, span.end, section
                    )
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(
            token_kinds,
            [
                // `Dim`: keywords are resolved by the lexer that wraps this one, see `Ident`
                T![ident],
                T![ws],
                T![ident],
                T![:],
                T![ident],
                T![ws],
                T![=],
                T![ws],
                T![integer_literal],
                T![nl]
            ]
        );
    }
}
