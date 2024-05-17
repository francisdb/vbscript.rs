use super::TokenKind;
use crate::T;
use logos::{Lexer, Logos};

/* ANCHOR: callbacks */
/// Update the line count and the char index.
fn newline_callback(lex: &mut Lexer<LogosToken>) -> (usize, usize) {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
    (lex.extras.0, lex.extras.1)
}

/// Compute the line and column position for the current word.
fn word_callback(lex: &mut Lexer<LogosToken>) -> (usize, usize) {
    let line = lex.extras.0;
    let column = lex.span().start - lex.extras.1;

    (line, column)
}
/* ANCHOR_END: callbacks */

#[derive(Logos, Debug, PartialEq, Eq)]
#[logos(extras = (usize, usize))]
pub(super) enum LogosToken {
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
    #[regex(r#""([^"]|"")*""#, word_callback)]
    String((usize, usize)),
    #[regex(r#"\d+"#, word_callback, priority = 2)]
    Int((usize, usize)),
    #[regex(r#"&[Hh][0-9A-Fa-f]+"#, word_callback, priority = 2)]
    HexInt((usize, usize)),
    #[regex(r#"&O[0-7]+"#, word_callback, priority = 2)]
    OctalInt((usize, usize)),
    #[regex(
        r#"((\d+(\.\d*)?)|(\.\d+))([Ee](\+|-)?\d+)?"#,
        word_callback,
        priority = 1
    )]
    Float((usize, usize)),
    #[regex(r#"([A-Za-z])([A-Za-z]|_|\d)*"#, word_callback, priority = 3)]
    Ident((usize, usize)),

    // Keywords
    #[token("and", word_callback, ignore(ascii_case))]
    KwAnd((usize, usize)),
    #[token("byref", word_callback, ignore(ascii_case))]
    KwByRef((usize, usize)),
    #[token("byval", word_callback, ignore(ascii_case))]
    KwByVal((usize, usize)),
    #[token("call", word_callback, ignore(ascii_case))]
    KwCall((usize, usize)),
    #[token("case", word_callback, ignore(ascii_case))]
    KwCase((usize, usize)),
    #[token("class", word_callback, ignore(ascii_case))]
    KwClass((usize, usize)),
    #[token("const", word_callback, ignore(ascii_case))]
    KwConst((usize, usize)),
    #[token("currency", word_callback, ignore(ascii_case))]
    KwCurrency((usize, usize)),
    #[token("default", word_callback, ignore(ascii_case))]
    KwDefault((usize, usize)),
    // #[token("debug", ignore(ascii_case))]
    // KwDebug,
    #[token("dim", word_callback, ignore(ascii_case))]
    KwDim((usize, usize)),
    #[token("do", word_callback, ignore(ascii_case))]
    KwDo((usize, usize)),
    #[token("each", word_callback, ignore(ascii_case))]
    KwEach((usize, usize)),
    #[token("else", word_callback, ignore(ascii_case))]
    KwElse((usize, usize)),
    #[token("elseif", word_callback, ignore(ascii_case))]
    KwElseIf((usize, usize)),
    #[token("empty", word_callback, ignore(ascii_case))]
    KwEmpty((usize, usize)),
    #[token("end", word_callback, ignore(ascii_case))]
    KwEnd((usize, usize)),
    #[token("eqv", word_callback, ignore(ascii_case))]
    KwEqv((usize, usize)),
    #[token("error", word_callback, ignore(ascii_case))]
    KwError((usize, usize)),
    #[token("event", word_callback, ignore(ascii_case))]
    KwEvent((usize, usize)),
    #[token("exit", word_callback, ignore(ascii_case))]
    KwExit((usize, usize)),
    #[token("false", word_callback, ignore(ascii_case))]
    KwFalse((usize, usize)),
    #[token("for", word_callback, ignore(ascii_case))]
    KwFor((usize, usize)),
    #[token("function", word_callback, ignore(ascii_case))]
    KwFunction((usize, usize)),
    #[token("get", word_callback, ignore(ascii_case))]
    KwGet((usize, usize)),
    #[token("goto", word_callback, ignore(ascii_case))]
    KwGoTo((usize, usize)),
    #[token("if", word_callback, ignore(ascii_case))]
    KwIf((usize, usize)),
    #[token("imp", word_callback, ignore(ascii_case))]
    KwImp((usize, usize)),
    #[token("implements", word_callback, ignore(ascii_case))]
    KwImplements((usize, usize)),
    #[token("in", word_callback, ignore(ascii_case))]
    KwIn((usize, usize)),
    #[token("is", word_callback, ignore(ascii_case))]
    KwIs((usize, usize)),
    #[token("let", word_callback, ignore(ascii_case))]
    KwLet((usize, usize)),
    #[token("like", word_callback, ignore(ascii_case))]
    KwLike((usize, usize)),
    #[token("loop", word_callback, ignore(ascii_case))]
    KwLoop((usize, usize)),
    #[token("lset", word_callback, ignore(ascii_case))]
    KwLSet((usize, usize)),
    #[token("me", word_callback, ignore(ascii_case))]
    KwMe((usize, usize)),
    #[token("mod", word_callback, ignore(ascii_case))]
    KwMod((usize, usize)),
    #[token("new", word_callback, ignore(ascii_case))]
    KwNew((usize, usize)),
    #[token("next", word_callback, ignore(ascii_case))]
    KwNext((usize, usize)),
    #[token("not", word_callback, ignore(ascii_case))]
    KwNot((usize, usize)),
    #[token("nothing", word_callback, ignore(ascii_case))]
    KwNothing((usize, usize)),
    #[token("null", word_callback, ignore(ascii_case))]
    KwNull((usize, usize)),
    #[token("on", word_callback, ignore(ascii_case))]
    KwOn((usize, usize)),
    #[token("option", word_callback, ignore(ascii_case))]
    KwOption((usize, usize)),
    #[token("optional", word_callback, ignore(ascii_case))]
    KwOptional((usize, usize)),
    #[token("or", word_callback, ignore(ascii_case))]
    KwOr((usize, usize)),
    #[token("paramarray", word_callback, ignore(ascii_case))]
    KwParamArray((usize, usize)),
    #[token("preserve", word_callback, ignore(ascii_case))]
    KwPreserve((usize, usize)),
    #[token("private", word_callback, ignore(ascii_case))]
    KwPrivate((usize, usize)),
    #[token("property", word_callback, ignore(ascii_case))]
    KwProperty((usize, usize)),
    #[token("public", word_callback, ignore(ascii_case))]
    KwPublic((usize, usize)),
    #[token("raiseevent", word_callback, ignore(ascii_case))]
    KwRaiseEvent((usize, usize)),
    #[token("redim", word_callback, ignore(ascii_case))]
    KwReDim((usize, usize)),
    #[token("resume", word_callback, ignore(ascii_case))]
    KwResume((usize, usize)),
    #[token("rset", word_callback, ignore(ascii_case))]
    KwRSet((usize, usize)),
    #[token("select", word_callback, ignore(ascii_case))]
    KwSelect((usize, usize)),
    #[token("set", word_callback, ignore(ascii_case))]
    KwSet((usize, usize)),
    #[token("shared", word_callback, ignore(ascii_case))]
    KwShared((usize, usize)),
    #[token("single", word_callback, ignore(ascii_case))]
    KwSingle((usize, usize)),
    #[token("static", word_callback, ignore(ascii_case))]
    KwStatic((usize, usize)),
    // In the listing I found 'step' was missing as keyword so I wonder if this
    // should be handled in a different way.
    #[token("step", word_callback, ignore(ascii_case))]
    KwStep((usize, usize)),
    #[token("sub", word_callback, ignore(ascii_case))]
    KwSub((usize, usize)),
    #[token("then", word_callback, ignore(ascii_case))]
    KwThen((usize, usize)),
    #[token("to", word_callback, ignore(ascii_case))]
    KwTo((usize, usize)),
    #[token("true", word_callback, ignore(ascii_case))]
    KwTrue((usize, usize)),
    #[token("type", word_callback, ignore(ascii_case))]
    KwType((usize, usize)),
    #[token("typeof", word_callback, ignore(ascii_case))]
    KwTypeOf((usize, usize)),
    #[token("until", word_callback, ignore(ascii_case))]
    KwUntil((usize, usize)),
    #[token("variant", word_callback, ignore(ascii_case))]
    KwVariant((usize, usize)),
    #[token("wend", word_callback, ignore(ascii_case))]
    KwWend((usize, usize)),
    #[token("while", word_callback, ignore(ascii_case))]
    KwWhile((usize, usize)),
    #[token("with", word_callback, ignore(ascii_case))]
    KwWith((usize, usize)),
    #[token("xor", word_callback, ignore(ascii_case))]
    KwXor((usize, usize)),
    #[token("stop", word_callback, ignore(ascii_case))]
    KwStop((usize, usize)),
    /// Represents reserved keywords but that are not actually in use
    /// https://isvbscriptdead.com/reserved-keywords/
    // As, Byte, Boolean, Double, Integer, Long, Single, Stop, Variant
    #[regex(
        r"(?i)as|byte|boolean|double|integer|long|single|variant",
        word_callback
    )]
    KwUnused((usize, usize)),

    // Misc
    #[regex(r"[ \t\f]+")]
    WS,
    #[regex(r"(\r\n?|\n)", newline_callback)]
    NewLine((usize, usize)),
    // TODO to be 100% correct we should also capture a line
    //   with only _ (and trailing whitespace + newline) as a line continuation
    // (\r\n?|\n) matches \r\n, \r, and \n
    #[regex(r"_[ \t\f]*(\r\n?|\n)", newline_callback)]
    LineContinuation((usize, usize)),

    // comments using ' or REM
    #[regex(r"(?i)'([^\r\n]*)|rem ([^\r\n]*)")]
    Comment,
}

impl LogosToken {
    pub fn line_column(&self) -> (usize, usize) {
        use LogosToken::*;
        let mut line_col = match self {
            Dot((line, column)) => (*line, *column),
            NewLine((line, _)) => (*line, 0),
            Ampersand((line, column)) => (*line, *column),
            Colon((line, column)) => (*line, *column),
            Comma((line, column)) => (*line, *column),
            Ident((line, column)) => (*line, *column),
            String((line, column)) => (*line, *column),
            Int((line, column)) => (*line, *column),
            HexInt((line, column)) => (*line, *column),
            OctalInt((line, column)) => (*line, *column),
            Float((line, column)) => (*line, *column),
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
            KwAnd((line, column)) => (*line, *column),
            KwByRef((line, column)) => (*line, *column),
            KwByVal((line, column)) => (*line, *column),
            KwCall((line, column)) => (*line, *column),
            KwCase((line, column)) => (*line, *column),
            KwClass((line, column)) => (*line, *column),
            KwConst((line, column)) => (*line, *column),
            KwCurrency((line, column)) => (*line, *column),
            KwDefault((line, column)) => (*line, *column),
            KwDim((line, column)) => (*line, *column),
            KwDo((line, column)) => (*line, *column),
            KwEach((line, column)) => (*line, *column),
            KwElse((line, column)) => (*line, *column),
            KwElseIf((line, column)) => (*line, *column),
            KwEmpty((line, column)) => (*line, *column),
            KwEnd((line, column)) => (*line, *column),
            KwEqv((line, column)) => (*line, *column),
            KwError((line, column)) => (*line, *column),
            KwEvent((line, column)) => (*line, *column),
            KwExit((line, column)) => (*line, *column),
            KwFalse((line, column)) => (*line, *column),
            KwFor((line, column)) => (*line, *column),
            KwFunction((line, column)) => (*line, *column),
            KwGet((line, column)) => (*line, *column),
            KwGoTo((line, column)) => (*line, *column),
            KwIf((line, column)) => (*line, *column),
            KwImp((line, column)) => (*line, *column),
            KwImplements((line, column)) => (*line, *column),
            KwIn((line, column)) => (*line, *column),
            KwIs((line, column)) => (*line, *column),
            KwLet((line, column)) => (*line, *column),
            KwLike((line, column)) => (*line, *column),
            KwLoop((line, column)) => (*line, *column),
            KwLSet((line, column)) => (*line, *column),
            KwMe((line, column)) => (*line, *column),
            KwMod((line, column)) => (*line, *column),
            KwNew((line, column)) => (*line, *column),
            KwNext((line, column)) => (*line, *column),
            KwNot((line, column)) => (*line, *column),
            KwNothing((line, column)) => (*line, *column),
            KwNull((line, column)) => (*line, *column),
            KwOn((line, column)) => (*line, *column),
            KwOption((line, column)) => (*line, *column),
            KwOptional((line, column)) => (*line, *column),
            KwOr((line, column)) => (*line, *column),
            KwParamArray((line, column)) => (*line, *column),
            KwPreserve((line, column)) => (*line, *column),
            KwPrivate((line, column)) => (*line, *column),
            KwProperty((line, column)) => (*line, *column),
            KwPublic((line, column)) => (*line, *column),
            KwRaiseEvent((line, column)) => (*line, *column),
            KwReDim((line, column)) => (*line, *column),
            KwResume((line, column)) => (*line, *column),
            KwRSet((line, column)) => (*line, *column),
            KwSelect((line, column)) => (*line, *column),
            KwSet((line, column)) => (*line, *column),
            KwShared((line, column)) => (*line, *column),
            KwSingle((line, column)) => (*line, *column),
            KwStatic((line, column)) => (*line, *column),
            KwStep((line, column)) => (*line, *column),
            KwStop((line, column)) => (*line, *column),
            KwSub((line, column)) => (*line, *column),
            KwThen((line, column)) => (*line, *column),
            KwTo((line, column)) => (*line, *column),
            KwTrue((line, column)) => (*line, *column),
            KwType((line, column)) => (*line, *column),
            KwTypeOf((line, column)) => (*line, *column),
            KwUntil((line, column)) => (*line, *column),
            KwVariant((line, column)) => (*line, *column),
            KwWend((line, column)) => (*line, *column),
            KwWhile((line, column)) => (*line, *column),
            KwWith((line, column)) => (*line, *column),
            KwXor((line, column)) => (*line, *column),
            KwUnused((line, column)) => (*line, *column),
            WS => (0, 0),
            Comment => (0, 0),
            LineContinuation((line, column)) => (*line, *column),
        };
        // further down lines are 1-indexed
        if line_col.0 > 0 {
            line_col.0 += 1;
        }
        line_col
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
            Ident(_)     => T![ident],
            KwAnd(_)        => T![and],
            KwByRef(_)      => T![byref],
            KwByVal(_)      => T![byval],
            KwCall(_)       => T![call],
            KwCase(_)       => T![case],
            KwClass(_)      => T![class],
            KwConst(_)      => T![const],
            KwCurrency(_)   => unimplemented!("KwCurrency"),
            KwDefault(_) => T![default],
            KwDim(_)        => T![dim],
            KwDo(_)         => T![do],
            KwEach(_)       => T![each],
            KwElse(_)       => T![else],
            KwElseIf(_)     => T![elseif],
            KwEmpty(_)   => T![empty],
            KwEnd(_)     => T![end],
            KwEqv(_)        => T![eqv],
            KwError(_)      => T![error],
            KwEvent(_)      => unimplemented!("KwEvent"),
            KwExit(_)       => T![exit],
            KwFalse(_)      => T![false],
            KwFor(_)        => T![for],
            KwFunction(_)   => T![function],
            KwGet(_)        => T![get],
            KwGoTo(_)       => T![goto],
            KwIf(_)      => T![if],
            KwImp(_)        => T![imp],
            KwImplements(_) => unimplemented!("KwImplements"),
            KwIn(_)         => T![in],
            KwIs(_)      => T![is],
            KwLet(_)     => T![let],
            KwLike(_)       => unimplemented!( "KwLike"),
            KwLoop(_)       => T![loop],
            KwLSet(_)       => unimplemented!( "KwLSet"),
            KwMe(_)      => T![me],
            KwMod(_)     => T![mod],
            KwNew(_)     => T![new],
            KwNext(_)       => T![next],
            KwNot(_)        => T![not],
            KwNothing(_)    => T![nothing],
            KwNull(_)       => T![null],
            KwOn(_)         => T![on],
            KwOption(_)     => T![option],
            KwOptional(_)   => unimplemented!(),
            KwOr(_)         => T![or],
            KwParamArray(_) => unimplemented!( "KwParamArray"),
            KwPreserve(_)   => T![preserve],
            KwPrivate(_) => T![private],
            KwProperty(_)=> T![property],
            KwPublic(_)  => T![public],
            KwRaiseEvent(_) => unimplemented!( "KwRaiseEvent"),
            KwReDim(_)      => T![redim],
            KwResume(_)     => T![resume],
            KwRSet(_)       => unimplemented!( "KwRSet"),
            KwSelect(_)  => T![select],
            KwSet(_)        => T![set],
            KwShared(_)     => unimplemented!( "KwShared"),
            KwSingle(_)     => unimplemented!( "KwSingle"),
            KwStatic(_)     => unimplemented!( "KwStatic"),
            KwStep(_)    => T![step],
            KwStop(_)    => T![stop],
            KwSub(_)     => T![sub],
            KwThen(_)       => T![then],
            KwTo(_)         => T![to],
            KwTrue(_)       => T![true],
            KwType(_)       => unimplemented!("KwType"),
            KwTypeOf(_)     => unimplemented!( "KwTypeOf"),
            KwUntil(_)      => T![until],
            KwVariant(_)    => unimplemented!( "KwVariant"),
            KwWend(_)       => T![wend],
            KwWhile(_)      => T![while],
            KwWith(_)       => T![with],
            KwXor(_)        => T![xor],
            KwUnused(_)     => T![unused],
            WS           => T![ws],
            Comment      => T![comment],
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
                T![dim],
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
