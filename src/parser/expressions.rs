// In parser/expressions.rs

use crate::lexer::{Token, TokenKind};
use crate::parser::ast::{Expr, Lit};
use crate::parser::{ParseError, Parser};
use crate::T;

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn expression_with_prefix(&mut self, first_expression_part: Option<Expr>) -> Result<Expr, ParseError> {
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
        let mut lhs = first_expression_part.unwrap_or_else(|| self.parse_expression_lhs()?);
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
                lhs = Expr::FnApplication {
                    callee: Box::new(lhs),
                    args,
                };
                continue;
            }

            // highest binding power
            // _. is the same as . but disallows whitespace between the dot and the base.
            // The property itself is allowed to be prefixed with whitespace.
            if op == T![_.] {
                self.consume(T![_.])?;
                let property = self.member_identifier()?;
                lhs = Expr::MemberExpression {
                    base: Box::new(lhs),
                    property,
                };
                continue;
            }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power than
                    // new one --> end of expression
                    break;
                }

                self.consume(op);
                let rhs = self.parse_expression(right_binding_power)?;
                lhs = Expr::InfixOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                // parsed an operator --> go round the loop again
                continue;
            } else {
                // break; // Not an operator --> end of expression
                let token = *self.peek_full()?;
                let span = self.text(&token);
                return Err(ParseError::new(format!(
                    "No binding power for operator `{op}` in expression: {span}"),
                    token.line, token.column
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
                self.parse_literal().map(|lit| match lit {
                    Lit::Int(i) => Ok(Lit::Int(-i)),
                    Lit::Float(f) => Ok(Lit::Float(-f)),
                    _ => {
                        let peek = self.peek_full()?;
                        return Err(ParseError::new(
                            "Expected integer or float literal for unary minus",
                            peek.line,
                            peek.column
                        ))
                    }
                })
            }
            _ => self.parse_literal().map(Ok),
        }?;
        match lit {
            Some(lit) => Ok(lit),
            None => {
                let token = self.peek_full()?;
                return Err(ParseError::new(
                    "Expected literal for constant",
                    token.line,
                    token.column,
                ));
            }
        }
    }

    pub fn parse_literal(&mut self) -> Option<Lit> {
        match self.peek() {
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
                let literal_text = {
                    // the calls on `self` need to be split, because `next` takes
                    // `&mut self` if `peek` is not `T![EOF]`, then there must be
                    // a next token
                    let literal_token = self.next().unwrap();
                    self.text(&literal_token)
                };
                // We are using parse here which is for parsing rust literals, we might have to
                // implement our own parser for VBScript literals
                let literal = match lit {
                    T![integer_literal] => {
                        Lit::Int(literal_text.parse().unwrap_or_else(|_| {
                            panic!("invalid integer literal: `{literal_text}`")
                        }))
                    }
                    T![hex_integer_literal] => {
                        // trim the &H prefix
                        // trim possible & suffix (for long hex literals)
                        let trimmed = if literal_text.ends_with('&') {
                            &literal_text[2..(literal_text.len() - 1)]
                        } else {
                            &literal_text[2..]
                        };
                        Lit::Int(isize::from_str_radix(trimmed, 16).unwrap_or_else(|_| {
                            panic!("invalid hex integer literal: `{literal_text}`")
                        }))
                    }
                    T![octal_integer_literal] => {
                        Lit::Int(isize::from_str_radix(&literal_text[2..], 8).unwrap_or_else(
                            |_| panic!("invalid octal integer literal: `{literal_text}`"),
                        ))
                    }
                    T![real_literal] => Lit::Float(literal_text.parse().unwrap_or_else(|_| {
                        panic!("invalid floating point literal: `{literal_text}`")
                    })),
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
        }
    }

    fn parse_expression_lhs(&mut self) -> Result<Expr, ParseError> {
        if let Some(literal) = self.parse_literal() {
            return Ok(Expr::Literal(literal));
        }

        if let Some(ident) = self.identifier_opt() {
            return Ok(Expr::ident(ident));
        }

        let res = match self.peek() {
            T![.] => {
                self.consume(T![.])?;
                let ident = self.consume(T![ident])?;
                let property = self.text(&ident).to_string();
                Expr::MemberExpression {
                    base: Box::new(Expr::WithScoped),
                    property,
                }
            }
            T![new] => {
                self.consume(T![new]);
                let ident = self.consume(T![ident])?;
                let class_name = self.text(&ident);
                Expr::new(class_name)
            }
            T!['('] => {
                // There is no AST node for grouped expressions.
                // Parentheses just influence the tree structure.
                self.consume(T!['('])?;
                let expr = self.parse_expression(0)?;
                self.consume(T![')'])?;
                expr
            }
            op @ T![+] | op @ T![-] | op @ T![not] => {
                self.consume(op);
                let ((), right_binding_power) = op.prefix_binding_power();
                // NEW!
                let expr = self.parse_expression(right_binding_power)?;
                Expr::PrefixOp {
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
        Ok(res)
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

    pub(crate) fn parenthesized_optional_arguments(&mut self) -> Result<Vec<Option<Expr>>, ParseError> {
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::ast::Expr::{Literal, MemberExpression};
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
            Expr::InfixOp {
                op: T![+],
                lhs: Box::new(Expr::int(1)),
                rhs: Box::new(Expr::InfixOp {
                    op: T![*],
                    lhs: Box::new(Expr::int(2)),
                    rhs: Box::new(Expr::int(3)),
                }),
            }
        );
    }

    #[test]
    fn test_expression_with_parentheses() {
        let input = "(1 + 2) * 3";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![*],
                lhs: Box::new(Expr::InfixOp {
                    op: T![+],
                    lhs: Box::new(Expr::int(1)),
                    rhs: Box::new(Expr::int(2)),
                }),
                rhs: Box::new(Expr::int(3)),
            }
        );
    }

    #[test]
    fn test_expression_with_hex_leteral() {
        let input = "col And &HFF";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![and],
                lhs: Box::new(Expr::ident("col")),
                rhs: Box::new(Expr::int(0xFF)),
            }
        );
    }

    #[test]
    fn test_expression_is_nothing() {
        let input = "varValue Is Nothing";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![is],
                lhs: Box::new(Expr::ident("varValue")),
                rhs: Box::new(Literal(Lit::Nothing)),
            }
        );
    }

    #[test]
    fn test_expression_not_ident_is_nothing() {
        let input = "Not varValue Is Nothing";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::PrefixOp {
                op: T![not],
                expr: Box::new(Expr::InfixOp {
                    op: T![is],
                    lhs: Box::new(Expr::ident("varValue")),
                    rhs: Box::new(Literal(Lit::Nothing)),
                }),
            }
        );
    }

    #[test]
    fn test_expression_equals() {
        let input = "varValue = varValue2";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![=],
                lhs: Box::new(Expr::ident("varValue")),
                rhs: Box::new(Expr::ident("varValue2")),
            }
        );
    }

    #[test]
    fn test_expression_string_concatenation() {
        let input = r#""Hello" & " " & name"#;
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![&],
                lhs: Box::new(Expr::InfixOp {
                    op: T![&],
                    lhs: Box::new(Literal(Lit::str("Hello"))),
                    rhs: Box::new(Literal(Lit::str(" "))),
                }),
                rhs: Box::new(Expr::ident("name")),
            }
        );
    }

    #[test]
    fn test_me_property_assignment() {
        let input = "Me.Name = \"John\"";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![=],
                lhs: Box::new(MemberExpression {
                    base: Box::new(Expr::ident("Me")),
                    property: "Name".to_string(),
                }),
                rhs: Box::new(Literal(Lit::str("John"))),
            }
        );
    }

    #[test]
    fn test_multiline_string() {
        let input = "test &_\r\n  \"Hello\"";
        let expr = parse_expression(input);
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![&],
                lhs: Box::new(Expr::ident("test")),
                rhs: Box::new(Literal(Lit::str("Hello"))),
            }
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
            Expr::InfixOp {
                op: T![=],
                lhs: Box::new(Expr::MemberExpression {
                    base: Box::new(Expr::ident("foo")),
                    property: "enabled".to_string(),
                }),
                rhs: Box::new(Literal(Lit::Bool(false))),
            }
        );
    }
}
