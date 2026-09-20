use super::TokenKind;
use crate::T;

/// The longest keyword is this many bytes.
const MAX_KEYWORD_LEN: usize = 10;

/// The kind of token for a word if it is a keyword, `None` if it is a regular identifier.
///
/// Keywords are case-insensitive. They are resolved here instead of being separate rules of
/// the generated lexer: that many case-insensitive rules which all overlap with the
/// identifier rule blow up its state machine, and with that the compile time and code size.
///
/// `rem` is missing on purpose, the lexer turns it into a comment.
pub(super) fn keyword_kind(word: &str) -> Option<TokenKind> {
    if word.len() > MAX_KEYWORD_LEN {
        return None;
    }
    let mut buffer = [0u8; MAX_KEYWORD_LEN];
    let lowercase = &mut buffer[..word.len()];
    lowercase.copy_from_slice(word.as_bytes());
    lowercase.make_ascii_lowercase();
    let kind = match &*lowercase {
        b"and" => T![and],
        b"byref" => T![byref],
        b"byval" => T![byval],
        b"call" => T![call],
        b"case" => T![case],
        b"class" => T![class],
        b"const" => T![const],
        b"default" => T![default],
        b"dim" => T![dim],
        b"do" => T![do],
        b"each" => T![each],
        b"else" => T![else],
        b"elseif" => T![elseif],
        b"empty" => T![empty],
        b"end" => T![end],
        b"eqv" => T![eqv],
        b"error" => T![error],
        b"exit" => T![exit],
        b"false" => T![false],
        b"for" => T![for],
        b"function" => T![function],
        b"get" => T![get],
        b"goto" => T![goto],
        b"if" => T![if],
        b"imp" => T![imp],
        b"in" => T![in],
        b"is" => T![is],
        b"let" => T![let],
        b"loop" => T![loop],
        b"me" => T![me],
        b"mod" => T![mod],
        b"new" => T![new],
        b"next" => T![next],
        b"not" => T![not],
        b"nothing" => T![nothing],
        b"null" => T![null],
        b"on" => T![on],
        b"option" => T![option],
        b"or" => T![or],
        b"preserve" => T![preserve],
        b"private" => T![private],
        b"property" => T![property],
        b"public" => T![public],
        b"redim" => T![redim],
        b"resume" => T![resume],
        b"select" => T![select],
        b"set" => T![set],
        b"step" => T![step],
        b"stop" => T![stop],
        b"sub" => T![sub],
        b"then" => T![then],
        b"to" => T![to],
        b"true" => T![true],
        b"until" => T![until],
        b"wend" => T![wend],
        b"while" => T![while],
        b"with" => T![with],
        b"xor" => T![xor],
        // Reserved words that have no meaning in VBScript, `cscript` on Windows fails with
        // error 1010 "Expected identifier" when they are used as a variable name.
        // https://isvbscriptdead.com/reserved-keywords/
        b"as" | b"boolean" | b"byte" | b"currency" | b"double" | b"event" | b"implements"
        | b"integer" | b"like" | b"long" | b"lset" | b"optional" | b"paramarray"
        | b"raiseevent" | b"rset" | b"shared" | b"single" | b"static" | b"type" | b"typeof"
        | b"variant" => T![unused],
        _ => return None,
    };
    Some(kind)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn keywords_are_case_insensitive() {
        for word in ["paramarray", "ParamArray", "PARAMARRAY"] {
            assert_eq!(keyword_kind(word), Some(T![unused]), "{word}");
        }
        for word in ["elseif", "ElseIf", "ELSEIF"] {
            assert_eq!(keyword_kind(word), Some(T![elseif]), "{word}");
        }
    }

    #[test]
    fn identifiers_are_not_keywords() {
        // `rem` is handled by the lexer, `debug` is a regular identifier on Windows
        for word in [
            "x",
            "rem",
            "debug",
            "ends",
            "en",
            "paramarrays",
            "[end]",
            "",
        ] {
            assert_eq!(keyword_kind(word), None, "{word}");
        }
    }
}
