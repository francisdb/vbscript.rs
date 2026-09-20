use super::Span;

/// Finds the line and column of an offset in a source, like the start of a [`Span`].
///
/// A span is a range of bytes, which is what slicing the source needs. To point a person at
/// it you need a line and a column. The index looks at the source once, after that a lookup
/// is a binary search, so it suits many lookups in a large script.
///
/// Lines and columns start at 1. Lines end with `\r\n`, `\r` or `\n`, like for the lexer.
/// The column counts characters, not bytes: it is what an editor shows, and what the column
/// of a [`Token`](super::Token) and of a parse error count.
///
/// ```
/// use vbscript::lexer::LineIndex;
/// use vbscript::parser::Parser;
/// use vbscript::parser::ast::{ItemKind, StmtKind};
///
/// let input = "' héllo\r\nSub Foo()\r\nEnd Sub\r\n";
/// let items = Parser::new(input).file().unwrap();
/// let ItemKind::Statement(stmt) = &items[0].node else { panic!() };
/// let StmtKind::Sub { name, .. } = &stmt.node else { panic!() };
///
/// let index = LineIndex::new(input);
/// assert_eq!(index.line_column(name.span.start), (2, 5));
/// ```
pub struct LineIndex<'input> {
    input: &'input str,
    /// The offset of the first byte of every line.
    line_starts: Vec<u32>,
}

impl<'input> LineIndex<'input> {
    pub fn new(input: &'input str) -> Self {
        let bytes = input.as_bytes();
        let mut line_starts = vec![0];
        for (offset, byte) in bytes.iter().enumerate() {
            let ends_line = match byte {
                b'\n' => true,
                // the `\n` of a `\r\n` ends the line
                b'\r' => bytes.get(offset + 1) != Some(&b'\n'),
                _ => false,
            };
            if ends_line {
                line_starts.push(offset as u32 + 1);
            }
        }
        LineIndex { input, line_starts }
    }

    /// The line and column of an offset, both from 1, the column in characters.
    ///
    /// An offset past the end of the source gives the position of the end, one in the
    /// middle of a character the position of that character.
    pub fn line_column(&self, offset: u32) -> (usize, usize) {
        let offset = (offset as usize).min(self.input.len());
        // the last line that starts at or before the offset
        let line = self
            .line_starts
            .partition_point(|&start| start as usize <= offset)
            - 1;
        let line_start = self.line_starts[line] as usize;
        let column = self.input[line_start..]
            .char_indices()
            .take_while(|(index, _)| line_start + index < offset)
            .count();
        (line + 1, column + 1)
    }

    /// The number of lines, which is one more than the number of line ends.
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}

impl Span {
    /// The line and column of the start of the span, see [`LineIndex::line_column`].
    ///
    /// This looks at the source from its start, use a [`LineIndex`] for more than a few.
    pub fn line_column(&self, input: &str) -> (usize, usize) {
        LineIndex::new(input).line_column(self.start)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn line_ends() {
        // every kind of line end, and an empty line
        let input = "a\nbc\r\nd\re\n\nf";
        let index = LineIndex::new(input);
        let positions: Vec<_> = input
            .char_indices()
            .filter(|(_, c)| c.is_alphabetic())
            .map(|(offset, c)| (c, index.line_column(offset as u32)))
            .collect();
        assert_eq!(
            positions,
            [
                ('a', (1, 1)),
                ('b', (2, 1)),
                ('c', (2, 2)),
                ('d', (3, 1)),
                ('e', (4, 1)),
                ('f', (6, 1)),
            ]
        );
        assert_eq!(index.line_count(), 6);
        // the line end itself is still on its line
        assert_eq!(index.line_column(1), (1, 2));
        assert_eq!(index.line_column(4), (2, 3));
        assert_eq!(index.line_column(5), (2, 4));
    }

    #[test]
    fn column_counts_characters() {
        let input = "x = \"é€😀\" : y = 1";
        let index = LineIndex::new(input);
        let y = input.find('y').unwrap() as u32;
        // the 9 bytes in the string are 3 characters
        assert_eq!(y, 18);
        assert_eq!(index.line_column(y), (1, 13));
        // in the middle of a character is the position of that character
        let euro = input.find('€').unwrap() as u32;
        assert_eq!(index.line_column(euro), (1, 7));
        assert_eq!(index.line_column(euro + 1), (1, 8));
    }

    #[test]
    fn ends_and_empty_source() {
        assert_eq!(LineIndex::new("").line_column(0), (1, 1));
        assert_eq!(LineIndex::new("").line_column(10), (1, 1));
        assert_eq!(LineIndex::new("").line_count(), 1);
        let index = LineIndex::new("ab\n");
        assert_eq!(index.line_column(2), (1, 3));
        assert_eq!(index.line_column(3), (2, 1));
        assert_eq!(index.line_column(99), (2, 1));
    }

    #[test]
    fn span_line_column() {
        let input = "a\r\n  bc";
        let span = Span { start: 5, end: 7 };
        assert_eq!(span.line_column(input), (2, 3));
    }
}
