// In parser/expressions.rs

use crate::T;
use crate::lexer::{Token, TokenKind};
use crate::parser::ast::{Expr, ExprKind, Lit};
use crate::parser::{ParseError, Parser};

impl<I> Parser<'_, I>
where
    I: Iterator<Item = Token>,
{
    pub fn expression_with_prefix(
        &mut self,
        first_expression_part: Option<Expr>,
    ) -> Result<Expr, ParseError> {
        self.parse_expression_with_prefix(0, first_expression_part)
    }

    pub fn expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_expression(0)
    }

    pub fn parse_expression(&mut self, binding_power: u8) -> Result<Expr, ParseError> {
        self.parse_expression_with_prefix(binding_power, None)
    }

    pub fn parse_expression_with_prefix(
        &mut self,
        binding_power: u8,
        first_expression_part: Option<Expr>,
    ) -> Result<Expr, ParseError> {
        self.enter_nested()?;
        let result = self.parse_expression_inner(binding_power, first_expression_part);
        self.leave_nested();
        result
    }

    fn parse_expression_inner(
        &mut self,
        binding_power: u8,
        first_expression_part: Option<Expr>,
    ) -> Result<Expr, ParseError> {
        let mut lhs = match first_expression_part {
            Some(expr) => expr,
            None => self.parse_expression_lhs()?,
        };
        let start = lhs.span.start;
        loop {
            let op = match self.peek() {
                op @ T![+]
                | op @ T![-]
                | op @ T![*]
                | op @ T![/]
                | op @ T!['\\']
                | op @ T![mod]
                | op @ T![^]
                | op @ T![=]
                | op @ T![<>]
                | op @ T![is]
                | op @ T![and]
                | op @ T![or]
                | op @ T![xor]
                | op @ T![eqv]
                | op @ T![imp]
                | op @ T![<]
                | op @ T![<=]
                | op @ T![>]
                | op @ T![>=]
                | op @ T![not]
                | op @ T!['(']
                | op @ T![_.]
                | op @ T![&] => op,
                T![EOF] => break,
                T![')'] | T![,] => break,
                ending if ending.is_ending_expression() => break,
                _kind => {
                    //let token = *self.peek_full();
                    //let span = self.text(&token);
                    // panic!(
                    //     "Unknown operator `{kind}` in expression at line {}, column {}: {span}",
                    //     token.line, token.column
                    // )
                    // println!(
                    //     "Stopping expression parsing at line {}, column {} ({kind})",
                    //     token.line, token.column
                    // );
                    break;
                }
            };

            // highest binding power
            if op == T!['('] {
                let args = self.parenthesized_optional_arguments()?;
                let application = ExprKind::FnApplication {
                    callee: Box::new(lhs),
                    args,
                };
                lhs = self.spanned(application, start);
                continue;
            }

            // highest binding power
            // _. is the same as . but disallows whitespace between the dot and the base.
            // The property itself is allowed to be prefixed with whitespace.
            if op == T![_.] {
                self.consume(T![_.])?;
                let property = self.member_identifier()?;
                let member = ExprKind::MemberExpression {
                    base: Box::new(lhs),
                    property,
                };
                lhs = self.spanned(member, start);
                continue;
            }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power than
                    // new one --> end of expression
                    break;
                }

                self.consume(op)?;
                let rhs = self.parse_expression(right_binding_power)?;
                let infix = ExprKind::InfixOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                lhs = self.spanned(infix, start);
                // parsed an operator --> go round the loop again
                continue;
            } else {
                // break; // Not an operator --> end of expression
                let token = *self.peek_full()?;
                let span = self.text(&token);
                return Err(ParseError::new(
                    format!("No binding power for operator `{op}` in expression: {span}"),
                    token.line,
                    token.column,
                ));
            }
        }

        Ok(lhs)
    }

    // expressions for constants are very limited, no math
    pub fn parse_const_literal(&mut self) -> Result<Lit, ParseError> {
        let lit = match self.peek() {
            sign @ T![+] | sign @ T![-] => {
                self.consume(sign)?;
                self.literal()?.map(|lit| match lit {
                    Lit::Int(i) => Ok(Lit::Int(format!("-{}", i))),
                    Lit::Float(f) => Ok(Lit::Float(-f)),
                    _ => {
                        let peek = self.peek_full()?;
                        Err(ParseError::new(
                            "Expected integer or float literal for unary minus",
                            peek.line,
                            peek.column,
                        ))
                    }
                })
            }
            _ => self.literal()?.map(Ok),
        };
        match lit {
            Some(lit) => Ok(lit?),
            None => {
                let token = self.peek_full()?;
                Err(ParseError::new(
                    "Expected literal for constant",
                    token.line,
                    token.column,
                ))
            }
        }
    }

    /// Parse a literal if there is one next, fails if it is not a valid one.
    pub(crate) fn literal(&mut self) -> Result<Option<Lit>, ParseError> {
        let literal = match self.peek() {
            lit @ T![integer_literal]
            | lit @ T![hex_integer_literal]
            | lit @ T![octal_integer_literal]
            | lit @ T![real_literal]
            | lit @ T![string_literal]
            | lit @ T![date_time_literal]
            | lit @ T![true]
            | lit @ T![false]
            | lit @ T![nothing]
            | lit @ T![empty]
            | lit @ T![null] => {
                let literal_token = *self.peek_full()?;
                self.consume(lit)?;
                let literal_text = self.text(&literal_token);
                let invalid = |kind: &str| {
                    ParseError::new(
                        format!("Invalid {kind} literal: `{literal_text}`"),
                        literal_token.line,
                        literal_token.column,
                    )
                };
                // We are using parse here which is for parsing rust literals, we might have to
                // implement our own parser for VBScript literals
                let literal = match lit {
                    T![integer_literal] => Lit::Int(literal_text.to_string()),
                    T![hex_integer_literal] => {
                        // trim the &H prefix
                        // trim possible & suffix (for long hex literals)
                        let trimmed = if literal_text.ends_with('&') {
                            &literal_text[2..(literal_text.len() - 1)]
                        } else {
                            &literal_text[2..]
                        };
                        let long = literal_text.ends_with('&');
                        Lit::int(
                            radix_literal_value(trimmed, 16, long)
                                .ok_or_else(|| invalid("hex integer"))?,
                        )
                    }
                    T![octal_integer_literal] => {
                        // trim the `&` prefix, an optional `O`, and a possible `&` Long suffix
                        let digits = literal_text
                            .trim_start_matches('&')
                            .trim_start_matches(['O', 'o'])
                            .trim_end_matches('&');
                        let long = literal_text.ends_with('&');
                        Lit::int(
                            radix_literal_value(digits, 8, long)
                                .ok_or_else(|| invalid("octal integer"))?,
                        )
                    }
                    T![real_literal] => Lit::Float(
                        literal_text
                            .parse()
                            .map_err(|_| invalid("floating point"))?,
                    ),
                    T![string_literal] => Lit::Str(
                        // trim the quotation marks
                        // replace double quotes with single quotes
                        literal_text[1..(literal_text.len() - 1)]
                            .to_string()
                            .replace("\"\"", "\""),
                    ),
                    T![date_time_literal] => {
                        // drop the # prefix and suffix
                        let trimmed = &literal_text[1..(literal_text.len() - 1)];
                        Lit::DateTime(trimmed.to_string())
                    }
                    T![true] => Lit::Bool(true),
                    T![false] => Lit::Bool(false),
                    T![nothing] => Lit::Nothing,
                    T![empty] => Lit::Empty,
                    T![null] => Lit::Null,
                    _ => unreachable!(),
                };
                Some(literal)
            }
            _ => None,
        };
        Ok(literal)
    }

    fn parse_expression_lhs(&mut self) -> Result<Expr, ParseError> {
        let start = self.start();
        if let Some(literal) = self.literal()? {
            return Ok(self.spanned(ExprKind::Literal(literal), start));
        }

        if let Some(ident) = self.identifier_opt()? {
            return Ok(self.spanned(ExprKind::Ident(ident), start));
        }

        let kind = match self.peek() {
            T![.] => {
                // the object of the with block is implied, it has an empty span
                let base = self.spanned(ExprKind::WithScoped, start);
                self.consume(T![.])?;
                let ident = self.consume(T![ident])?;
                let property = self.text(&ident).to_string();
                ExprKind::MemberExpression {
                    base: Box::new(base),
                    property,
                }
            }
            T![new] => {
                self.consume(T![new])?;
                let ident = self.consume(T![ident])?;
                let class_name = self.text(&ident);
                ExprKind::New(class_name.to_string())
            }
            T!['('] => {
                // There is no AST node for grouped expressions.
                // Parentheses just influence the tree structure.
                self.consume(T!['('])?;
                let expr = self.parse_expression(0)?;
                self.consume(T![')'])?;
                // the parentheses are part of the expression they group
                return Ok(self.spanned(expr.node, start));
            }
            op @ T![+] | op @ T![-] | op @ T![not] => {
                self.consume(op)?;
                let ((), right_binding_power) = op.prefix_binding_power();
                // NEW!
                let expr = self.parse_expression(right_binding_power)?;
                ExprKind::PrefixOp {
                    op,
                    expr: Box::new(expr),
                }
            }
            kind => {
                let token = self.peek_full()?;
                return Err(ParseError::new(
                    format!("Unknown start of expression: {kind}"),
                    token.line,
                    token.column,
                ));
            }
        };
        Ok(self.spanned(kind, start))
    }

    pub(crate) fn parenthesized_arguments(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut arguments = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['('])?;
            while !self.at(T![')']) {
                let expr = self.expression()?;
                arguments.push(expr);
                if self.at(T![,]) {
                    self.consume(T![,])?;
                }
            }
            self.consume(T![')'])?;
        };
        Ok(arguments)
    }

    pub(crate) fn parenthesized_optional_arguments(
        &mut self,
    ) -> Result<Vec<Option<Expr>>, ParseError> {
        let mut arguments = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['('])?;
            while !self.at(T![')']) {
                // empty args are allowed
                let expr = if self.at(T![,]) {
                    None
                } else {
                    Some(self.expression()?)
                };
                arguments.push(expr);
                if self.at(T![,]) {
                    self.consume(T![,])?;
                }
            }
            self.consume(T![')'])?;
        };
        Ok(arguments)
    }
}

trait Operator {
    /// Prefix operators bind their operand to the right.
    fn prefix_binding_power(&self) -> ((), u8);

    /// Infix operators bind two operands, lhs and rhs.
    fn infix_binding_power(&self) -> Option<(u8, u8)>;

    // /// Postfix operators bind their operand to the left.
    // fn postfix_binding_power(&self) -> Option<(u8, ())>;
}

impl Operator for TokenKind {
    fn prefix_binding_power(&self) -> ((), u8) {
        match self {
            T![+] | T![-] => ((), 51),
            T![not] => ((), 4),
            // Prefixes are the only operators we have already seen
            // when we call this, so we know the token must be
            // one of the above
            _ => unreachable!("Not a prefix operator: {:?}", self),
        }
    }

    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let result = match self {
            T![or] => (1, 2),
            T![xor] => (3, 4),
            T![and] | T![eqv] | T![imp] => (5, 6),
            T![=] | T![<>] | T![is] => (7, 8),
            T![<] | T![>] | T![<=] | T![>=] => (9, 10),
            T![+] | T![-] | T![&] => (11, 12),
            T![*] | T![/] | T!['\\'] | T![mod] => (13, 14),
            T![^] => (22, 21), // <- This binds stronger to the left!
            _ => return None,
        };
        Some(result)
    }

    // fn postfix_binding_power(&self) -> Option<(u8, ())> {
    //     let result = match self {
    //         T![!] => (101, ()),
    //         _ => return None,
    //     };
    //     Some(result)
    // }
}

/// The value of a hex or octal literal, `None` if it does not fit in 32 bits.
///
/// These literals are the two's complement bit pattern of the value. Without the `&` Long
/// suffix anything up to `&HFFFF` is a 16 bit Integer, so `&HFFFF` is -1 while `&HFFFF&` is
/// 65535. Everything else is a 32 bit Long, so `&HFFFFFFFF` is -1. Validated with `cscript`
/// on Windows, which reports a syntax error for more than 32 bits.
fn radix_literal_value(digits: &str, radix: u32, long: bool) -> Option<isize> {
    let bits = u32::from_str_radix(digits, radix).ok()?;
    let value = match u16::try_from(bits) {
        Ok(bits) if !long => i32::from(bits as i16),
        _ => bits as i32,
    };
    Some(value as isize)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::ast::ExprKind::{Literal, MemberExpression};
    use pretty_assertions::assert_eq;

    fn parse_expression(input: &str) -> Expr {
        let mut parser = Parser::new(input);
        parser.expression().unwrap()
    }

    #[test]
    fn test_expression_operator_priority() {
        let input = "1 + 2 * 3";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![+],
                lhs: Box::new(Expr::int(1)),
                rhs: Box::new(
                    ExprKind::InfixOp {
                        op: T![*],
                        lhs: Box::new(Expr::int(2)),
                        rhs: Box::new(Expr::int(3)),
                    }
                    .into()
                ),
            }
            .into()
        );
    }

    #[test]
    fn test_expression_with_parentheses() {
        let input = "(1 + 2) * 3";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![*],
                lhs: Box::new(
                    ExprKind::InfixOp {
                        op: T![+],
                        lhs: Box::new(Expr::int(1)),
                        rhs: Box::new(Expr::int(2)),
                    }
                    .into()
                ),
                rhs: Box::new(Expr::int(3)),
            }
            .into()
        );
    }

    #[test]
    fn test_expression_with_hex_leteral() {
        let input = "col And &HFF";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![and],
                lhs: Box::new(Expr::ident("col")),
                rhs: Box::new(Expr::int(0xFF)),
            }
            .into()
        );
    }

    #[test]
    fn test_expression_with_octal_literal() {
        // `&O17` is octal 17 == 15, the trailing `&` Long suffix must be ignored
        let input = "col And &O17&";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![and],
                lhs: Box::new(Expr::ident("col")),
                rhs: Box::new(Expr::int(0o17)),
            }
            .into()
        );
    }

    #[test]
    fn test_expression_with_bare_octal_literal() {
        // The `O` is optional: Windows cscript evaluates `&10000000` to octal 2097152
        let input = "col And &10000000";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![and],
                lhs: Box::new(Expr::ident("col")),
                rhs: Box::new(Expr::int(0o10000000)),
            }
            .into()
        );
    }

    #[test]
    fn test_bare_octal_rejects_non_octal_digit() {
        // `&19` is a syntax error on Windows cscript: 9 is not an octal digit, so it
        // lexes as `&1` followed by a stray `9` and must fail to parse.
        let mut parser = Parser::new("x = &19");
        assert!(
            parser.file().is_err(),
            "`&19` should not parse: 9 is not an octal digit"
        );
    }

    #[test]
    fn test_expression_is_nothing() {
        let input = "varValue Is Nothing";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![is],
                lhs: Box::new(Expr::ident("varValue")),
                rhs: Box::new(Literal(Lit::Nothing).into()),
            }
            .into()
        );
    }

    #[test]
    fn test_expression_not_ident_is_nothing() {
        let input = "Not varValue Is Nothing";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::PrefixOp {
                op: T![not],
                expr: Box::new(
                    ExprKind::InfixOp {
                        op: T![is],
                        lhs: Box::new(Expr::ident("varValue")),
                        rhs: Box::new(Literal(Lit::Nothing).into()),
                    }
                    .into()
                ),
            }
            .into()
        );
    }

    #[test]
    fn test_expression_equals() {
        let input = "varValue = varValue2";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![=],
                lhs: Box::new(Expr::ident("varValue")),
                rhs: Box::new(Expr::ident("varValue2")),
            }
            .into()
        );
    }

    #[test]
    fn test_expression_string_concatenation() {
        let input = r#""Hello" & " " & name"#;
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![&],
                lhs: Box::new(
                    ExprKind::InfixOp {
                        op: T![&],
                        lhs: Box::new(Literal(Lit::str("Hello")).into()),
                        rhs: Box::new(Literal(Lit::str(" ")).into()),
                    }
                    .into()
                ),
                rhs: Box::new(Expr::ident("name")),
            }
            .into()
        );
    }

    #[test]
    fn test_me_property_assignment() {
        let input = "Me.Name = \"John\"";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![=],
                lhs: Box::new(
                    MemberExpression {
                        base: Box::new(Expr::ident("Me")),
                        property: "Name".to_string(),
                    }
                    .into()
                ),
                rhs: Box::new(Literal(Lit::str("John")).into()),
            }
            .into()
        );
    }

    #[test]
    fn test_multiline_string() {
        let input = "test &_\r\n  \"Hello\"";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![&],
                lhs: Box::new(Expr::ident("test")),
                rhs: Box::new(Literal(Lit::str("Hello")).into()),
            }
            .into()
        );
    }

    #[test]
    fn test_default_function_call() {
        // call the default function of a class with argument 1
        let input = "(new Foo)(1)";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::fn_application(Expr::new("Foo"), vec![Expr::int(1)])
        );
    }

    #[test]
    fn test_parenthesized_property_access_check() {
        // eg `if (wheelchange).enabled=false then`
        let input = "(foo).enabled=false";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            ExprKind::InfixOp {
                op: T![=],
                lhs: Box::new(
                    MemberExpression {
                        base: Box::new(Expr::ident("foo")),
                        property: "enabled".to_string(),
                    }
                    .into()
                ),
                rhs: Box::new(Literal(Lit::Bool(false)).into()),
            }
            .into()
        );
    }
}
