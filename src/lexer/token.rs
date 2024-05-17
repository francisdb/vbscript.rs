use std::fmt;
use std::ops::{Index, Range};

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    // 1-indexed, 0 means unknown
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn error(p0: Range<i32>) -> Token {
        Token {
            kind: TokenKind::ParseError,
            span: Span {
                start: p0.start as u32,
                end: p0.end as u32,
            },
            line: 0,
            column: 0,
        }
    }
    pub fn eof(p0: Range<i32>) -> Token {
        Token {
            kind: TokenKind::Eof,
            span: Span {
                start: p0.start as u32,
                end: p0.end as u32,
            },
            line: 0,
            column: 0,
        }
    }

    pub fn ident(p1: Range<i32>, line: usize, column: usize) -> Token {
        Token {
            kind: TokenKind::Identifier,
            span: Span {
                start: p1.start as u32,
                end: p1.end as u32,
            },
            line,
            column,
        }
    }
}

impl Token {
    pub fn is_empty(&self) -> bool {
        self.kind == TokenKind::Eof
    }

    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} - line {}, col {} <{} - {}>",
            self.kind, self.line, self.column, self.span.start, self.span.end
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Hash, Default, Debug)]
pub struct Span {
    /// inclusive
    pub start: u32,
    /// exclusive
    pub end: u32,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TokenKind {
    // Single characters
    Plus,
    Minus,
    Times,
    /// Regular division
    Slash,
    /// Integer division
    Backslash,
    Pow,
    Eq,
    Dot,
    Comma,
    Bang,
    Ampersand,
    Colon,
    SemiColon,
    // Brackets
    LAngle,
    RAngle,
    LParen,
    RParen,
    // Multiple characters
    Option,
    Comment,
    Identifier,
    // Literals
    // see
    Integer,
    HexInteger,
    OctalInteger,
    Real,
    String,
    // Keywords
    KeywordMod,
    KeywordConst,
    KeywordDim,
    KeywordRedim,
    KeywordSub,
    KeywordFunction,
    KeywordIf,
    KeywordThen,
    KeywordElse,
    KeywordElseIf,
    KeywordSet,
    KeywordEnd,
    KeywordClass,
    KeywordProperty,
    KeywordPublic,
    KeywordPrivate,
    KeywordPreserve,
    KeywordDefault,
    KeywordGet,
    KeywordLet,
    KeywordAs,
    KeywordByVal,
    KeywordByRef,
    KeywordNew,
    KeywordReturn,
    KeywordFor,
    KeywordEach,
    KeywordIn,
    KeywordTo,
    KeywordStep,
    KeywordNext,
    KeywordWhile,
    KeywordWend,
    KeywordDo,
    KeywordLoop,
    KeywordUntil,
    KeywordWith,
    KeywordSelect,
    KeywordCase,
    KeywordCall,
    KeywordExit,
    KeywordMe,
    /// https://www.vbsedit.com/html/3ff21ea0-54e5-4f95-9c77-7f2d02977463.asp
    /// The Stop statement suspends execution, similar to setting a breakpoint in the code.
    KeywordStop,
    /// Any keywords that are reserved but not used, like "As"
    /// see https://docs.microsoft.com/en-us/dotnet/visual-basic/language-reference/keywords/reserved-keywords
    KeywordUnused,
    //Special values
    Empty,
    Null,
    Nothing,
    True,
    False,
    // Logical operators
    Not,
    And,
    Or,
    Xor,
    Eqv,
    Imp,
    // Comparison operators
    // = is shared with assignment
    Neq,
    Geq,
    Leq,
    Is,
    // Error handling
    On,
    Error,
    Resume,
    // Next is already defined as a keyword
    Goto,

    // Misc,
    Whitespace,
    /// CRLF, LF (or CR)
    Newline,
    LineContinuation,
    Eof,
    /// We found something that we can't tokenize
    ParseError,
}

impl TokenKind {
    pub(crate) fn is_ending_expression(&self) -> bool {
        matches!(
            self,
            Self::Eof
                | Self::KeywordEnd
                | Self::KeywordWend
                | Self::KeywordLoop
                | Self::KeywordNext
                | Self::KeywordUntil
                | Self::KeywordThen
                | Self::KeywordTo
                | Self::KeywordStep
                | Self::KeywordElse
                | Self::Newline
                | Self::Colon
                | Self::Comment
        )
    }
}

#[macro_export]
macro_rules! T {
    // Single characters
    [.] => {
        $crate::lexer::TokenKind::Dot
    };
    [+] => {
        $crate::lexer::TokenKind::Plus
    };
    [-] => {
        $crate::lexer::TokenKind::Minus
    };
    [*] => {
        $crate::lexer::TokenKind::Times
    };
    [/] => {
        $crate::lexer::TokenKind::Slash
    };
    ['\\'] => {
        $crate::lexer::TokenKind::Backslash
    };
    [mod] => {
        $crate::lexer::TokenKind::KeywordMod
    };
    [^] => {
        $crate::lexer::TokenKind::Pow
    };
    [=] => {
        $crate::lexer::TokenKind::Eq
    };
    [,] => {
        $crate::lexer::TokenKind::Comma
    };
    [!] => {
        $crate::lexer::TokenKind::Bang
    };
    [&] => {
        $crate::lexer::TokenKind::Ampersand
    };
    [:] => {
        $crate::lexer::TokenKind::Colon
    };
    [;] => {
        $crate::lexer::TokenKind::SemiColon
    };
    [<] => {
        $crate::lexer::TokenKind::LAngle
    };
    [>] => {
        $crate::lexer::TokenKind::RAngle
    };
    ['['] => {
        $crate::lexer::TokenKind::LSquare
    };
    [']'] => {
        $crate::lexer::TokenKind::RSquare
    };
    ['('] => {
        $crate::lexer::TokenKind::LParen
    };
    [')'] => {
        $crate::lexer::TokenKind::RParen
    };
    [option] => {
        $crate::lexer::TokenKind::Option
    };
    [comment] => {
        $crate::lexer::TokenKind::Comment
    };
    // literals
    [integer_literal] => {
        $crate::lexer::TokenKind::Integer
    };
    [hex_integer_literal] => {
        $crate::lexer::TokenKind::HexInteger
    };
    [octal_integer_literal] => {
        $crate::lexer::TokenKind::OctalInteger
    };
    [real_literal] => {
        $crate::lexer::TokenKind::Real
    };
    [string_literal] => {
        $crate::lexer::TokenKind::String
    };
    // Keywords
    [ident] => {
        $crate::lexer::TokenKind::Identifier
    };
    [const] => {
        $crate::lexer::TokenKind::KeywordConst
    };
    [dim] => {
        $crate::lexer::TokenKind::KeywordDim
    };
    [redim] => {
        $crate::lexer::TokenKind::KeywordRedim
    };
    [set] => {
        $crate::lexer::TokenKind::KeywordSet
    };
    [let] => {
        $crate::lexer::TokenKind::KeywordLet
    };
    [get] => {
        $crate::lexer::TokenKind::KeywordGet
    };
    [sub] => {
        $crate::lexer::TokenKind::KeywordSub
    };
    [function] => {
        $crate::lexer::TokenKind::KeywordFunction
    };
    [byval] => {
        $crate::lexer::TokenKind::KeywordByVal
    };
    [byref] => {
        $crate::lexer::TokenKind::KeywordByRef
    };
    [call] => {
        $crate::lexer::TokenKind::KeywordCall
    };
    [class] => {
        $crate::lexer::TokenKind::KeywordClass
    };
    [property] => {
        $crate::lexer::TokenKind::KeywordProperty
    };
    [public] => {
        $crate::lexer::TokenKind::KeywordPublic
    };
    [private] => {
        $crate::lexer::TokenKind::KeywordPrivate
    };
    [preserve] => {
        $crate::lexer::TokenKind::KeywordPreserve
    };
    [default] => {
        $crate::lexer::TokenKind::KeywordDefault
    };
    [as] => {
        $crate::lexer::TokenKind::KeywordAs
    };
    [new] => {
        $crate::lexer::TokenKind::KeywordNew
    };
    [return] => {
        $crate::lexer::TokenKind::KeywordReturn
    };
    [for] => {
        $crate::lexer::TokenKind::KeywordFor
    };
    [each] => {
        $crate::lexer::TokenKind::KeywordEach
    };
    [in] => {
        $crate::lexer::TokenKind::KeywordIn
    };
    [to] => {
        $crate::lexer::TokenKind::KeywordTo
    };
    [step] => {
        $crate::lexer::TokenKind::KeywordStep
    };
    [next] => {
        $crate::lexer::TokenKind::KeywordNext
    };
    [while] => {
        $crate::lexer::TokenKind::KeywordWhile
    };
    [wend] => {
        $crate::lexer::TokenKind::KeywordWend
    };
    [do] => {
        $crate::lexer::TokenKind::KeywordDo
    };
    [loop] => {
        $crate::lexer::TokenKind::KeywordLoop
    };
    [until] => {
        $crate::lexer::TokenKind::KeywordUntil
    };
    [with] => {
        $crate::lexer::TokenKind::KeywordWith
    };
    [select] => {
        $crate::lexer::TokenKind::KeywordSelect
    };
    [case] => {
        $crate::lexer::TokenKind::KeywordCase
    };
    [if] => {
        $crate::lexer::TokenKind::KeywordIf
    };
    [then] => {
        $crate::lexer::TokenKind::KeywordThen
    };
    [else] => {
        $crate::lexer::TokenKind::KeywordElse
    };
    [elseif] => {
        $crate::lexer::TokenKind::KeywordElseIf
    };
    [end] => {
        $crate::lexer::TokenKind::KeywordEnd
    };
    [exit] => {
        $crate::lexer::TokenKind::KeywordExit
    };
    [me] => {
        $crate::lexer::TokenKind::KeywordMe
    };
    [unused] => {
        $crate::lexer::TokenKind::KeywordUnused
    };
    // Special values
    [empty] => {
        $crate::lexer::TokenKind::Empty
    };
    [null] => {
        $crate::lexer::TokenKind::Null
    };
    [nothing] => {
        $crate::lexer::TokenKind::Nothing
    };
    [true] => {
        $crate::lexer::TokenKind::True
    };
    [false] => {
        $crate::lexer::TokenKind::False
    };
    // Logical operators
    [not] => {
        $crate::lexer::TokenKind::Not
    };
    [and] => {
        $crate::lexer::TokenKind::And
    };
    [or] => {
        $crate::lexer::TokenKind::Or
    };
    [xor] => {
        $crate::lexer::TokenKind::Xor
    };
    [eqv] => {
        $crate::lexer::TokenKind::Eqv
    };
    [imp] => {
        $crate::lexer::TokenKind::Imp
    };
    // Comparison operators
    [<>] => {
        $crate::lexer::TokenKind::Neq
    };
    [>=] => {
        $crate::lexer::TokenKind::Geq
    };
    [<=] => {
        $crate::lexer::TokenKind::Leq
    };
    [is] => {
        $crate::lexer::TokenKind::Is
    };
    // Error handling
    [error] => {
        $crate::lexer::TokenKind::Error
    };
    [resume] => {
        $crate::lexer::TokenKind::Resume
    };
    [goto] => {
        $crate::lexer::TokenKind::Goto
    };
    [on] => {
        $crate::lexer::TokenKind::On
    };
    // Misc
    [stop] => {
        $crate::lexer::TokenKind::KeywordStop
    };
    [ws] => {
        $crate::lexer::TokenKind::Whitespace
    };
    [nl] => {
        $crate::lexer::TokenKind::Newline
    };
    [line_continuation] => {
        $crate::lexer::TokenKind::LineContinuation
    };
    [EOF] => {
        $crate::lexer::TokenKind::Eof
    };
    [parse_error] => {
        $crate::lexer::TokenKind::ParseError
    };
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                // Single characters
                T![.] => ".",
                T![+] => "+",
                T![-] => "-",
                T![*] => "*",
                T![/] => "/",
                T!['\\'] => "\\",
                T![^] => "^",
                T![=] => "=",
                T![,] => ",",
                T![!] => "!",
                T![&] => "&",
                T![:] => ":",
                T![;] => ";",
                // Brackets
                T![<] => "<",
                T![>] => ">",
                T!['('] => "(",
                T![')'] => ")",
                // Multiple characters
                T![mod] => "mod",
                T![me] => "me",
                T![option] => "option",
                T![comment] => "' comment",
                // literals
                T![integer_literal] => "integer_literal",
                T![hex_integer_literal] => "hex_integer_literal",
                T![octal_integer_literal] => "octal_integer_literal",
                T![real_literal] => "real_literal",
                T![string_literal] => "string_literal",
                // Keywords
                T![ident] => "identifier",
                T![const] => "const",
                T![dim] => "dim",
                T![redim] => "redim",
                T![set] => "set",
                T![let] => "let",
                T![get] => "get",
                T![sub] => "sub",
                T![function] => "function",
                T![byval] => "byval",
                T![byref] => "byref",
                T![class] => "class",
                T![property] => "property",
                T![public] => "public",
                T![private] => "private",
                T![preserve] => "preserve",
                T![default] => "default",
                T![call] => "call",
                T![as] => "as",
                T![new] => "new",
                T![return] => "return",
                T![for] => "for",
                T![each] => "each",
                T![in] => "in",
                T![to] => "to",
                T![step] => "step",
                T![next] => "next",
                T![while] => "while",
                T![wend] => "wend",
                T![do] => "do",
                T![loop] => "loop",
                T![until] => "until",
                T![with] => "with",
                T![select] => "select",
                T![case] => "case",
                T![if] => "if",
                T![then] => "then",
                T![elseif] => "elseif",
                T![else] => "else",
                T![end] => "end",
                T![exit] => "exit",
                // Special values
                T![empty] => "empty",
                T![null] => "null",
                T![nothing] => "nothing",
                T![true] => "true",
                T![false] => "false",
                // Logical operators
                T![not] => "not",
                T![and] => "and",
                T![or] => "or",
                T![xor] => "xor",
                T![eqv] => "eqv",
                T![imp] => "imp",
                // Comparison operators
                T![<>] => "<>",
                T![>=] => ">=",
                T![<=] => "<=",
                T![is] => "is",
                // Error handling
                T![on] => "on",
                T![error] => "error",
                T![resume] => "resume",
                T![goto] => "goto",
                // Misc
                T![stop] => "stop",
                T![ws] => "<WS>",
                T![nl] => "<NL>",
                T![line_continuation] => "<_>",
                T![EOF] => "<EOF>",
                T![parse_error] => "<?>",
                T![unused] => "<unused_keyword>",
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    #[test]
    fn token_kind_display() {
        assert_eq!(T![+].to_string(), "+");
        assert_eq!(T![<=].to_string(), "<=");
        assert_eq!(T![dim].to_string(), "dim");
        assert_eq!(T![parse_error].to_string(), "<?>");
        assert_eq!(T![comment].to_string(), "' comment");
    }
}
