// In parser/expressions.rs

use crate::lexer::{Token, TokenKind};
use crate::parser::ast::{Expr, Lit};
use crate::parser::Parser;
use crate::T;

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn expression_with_prefix(&mut self, first_expression_part: Option<Expr>) -> Expr {
        self.parse_expression_with_prefix(0, first_expression_part)
    }

    pub fn expression(&mut self) -> Expr {
        self.parse_expression(0)
    }

    pub fn parse_expression(&mut self, binding_power: u8) -> Expr {
        self.parse_expression_with_prefix(binding_power, None)
    }

    pub fn parse_expression_with_prefix(
        &mut self,
        binding_power: u8,
        first_expression_part: Option<Expr>,
    ) -> Expr {
        let mut lhs = first_expression_part.unwrap_or_else(|| self.parse_expression_lhs());
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
                | op @ T![<]
                | op @ T![<=]
                | op @ T![>]
                | op @ T![>=]
                | op @ T![not]
                | op @ T!['(']
                // | op @ T![.]
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
                let args = self.parenthesized_arguments();
                lhs = Expr::FnCall {
                    callee: Box::new(lhs),
                    args,
                };
                continue;
            }

            // highest binding power
            // This does not work because we need to be sure there is no space between the base and the dot.
            // The property itself is allowed to be prefixed with whitespace.
            // if op == T![.] {
            //     self.consume(T![.]);
            //     let ident = self.consume(T![ident]);
            //     let property = self.text(&ident).to_string();
            //     lhs = Expr::PropertyAccess {
            //         base: Box::new(lhs),
            //         property,
            //     };
            //     continue;
            // }

            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    // previous operator has higher binding power than
                    // new one --> end of expression
                    break;
                }

                self.consume(op);
                let rhs = self.parse_expression(right_binding_power);
                lhs = Expr::InfixOp {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                // parsed an operator --> go round the loop again
                continue;
            } else {
                // break; // Not an operator --> end of expression
                let token = *self.peek_full();
                let span = self.text(&token);
                panic!(
                    "No binding power for operator `{op}` in expression at line {}, column {}: {span}",
                    token.line, token.column
                )
            }
        }

        lhs
    }

    // expressions for constants are very limited, no math
    pub fn parse_const_literal(&mut self) -> Lit {
        let lit = match self.peek() {
            sign @ T![+] | sign @ T![-] => {
                self.consume(sign);
                self.parse_literal().map(|lit| match lit {
                    Lit::Int(i) => Lit::Int(-i),
                    Lit::Float(f) => Lit::Float(-f),
                    _ => {
                        let peek = self.peek_full();
                        panic!(
                            "Expected literal for constant at line {}, column {}",
                            peek.line, peek.column
                        )
                    }
                })
            }
            _ => self.parse_literal(),
        };
        match lit {
            Some(lit) => lit,
            None => {
                let token = self.peek_full();
                panic!(
                    "Expected literal for constant at line {}, column {}",
                    token.line, token.column
                )
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
                    T![hex_integer_literal] => Lit::Int(
                        isize::from_str_radix(&literal_text[2..], 16).unwrap_or_else(|_| {
                            panic!("invalid hex integer literal: `{literal_text}`")
                        }),
                    ),
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
                        literal_text[1..(literal_text.len() - 1)].to_string(),
                    ),
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

    fn parse_expression_lhs(&mut self) -> Expr {
        if let Some(literal) = self.parse_literal() {
            return Expr::Literal(literal);
        }

        match self.peek() {
            // TODO deduplicate this list with identifier()
            //   we have seen these tokens being used as identifiers
            T![ident]
            | T![me]
            | T![.]
            | T![property]
            | T![stop]
            | T![option]
            | T![step]
            | T![default]
            | T![set] => {
                let full_ident = self.ident_deep();
                Expr::IdentFnSubCall(full_ident)
            }
            T![new] => {
                self.consume(T![new]);
                let ident = self.consume(T![ident]);
                let class_name = self.text(&ident);
                Expr::new(class_name)
            }
            T!['('] => {
                // There is no AST node for grouped expressions.
                // Parentheses just influence the tree structure.
                self.consume(T!['(']);
                let expr = self.parse_expression(0);
                self.consume(T![')']);
                expr
            }
            op @ T![+] | op @ T![-] | op @ T![not] => {
                self.consume(op);
                let ((), right_binding_power) = op.prefix_binding_power();
                // NEW!
                let expr = self.parse_expression(right_binding_power);
                Expr::PrefixOp {
                    op,
                    expr: Box::new(expr),
                }
            }
            kind => {
                let token = self.peek_full();
                panic!(
                    "{}:{} Unknown start of expression: {kind}",
                    token.line, token.column
                )
            }
        }
    }

    pub(crate) fn parenthesized_arguments(&mut self) -> Vec<Expr> {
        let mut arguments = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                let expr = self.expression();
                arguments.push(expr);
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        };
        arguments
    }

    pub(crate) fn parenthesized_optional_arguments(&mut self) -> Vec<Option<Expr>> {
        let mut arguments = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                let expr = if self.at(T![,]) {
                    None
                } else {
                    Some(self.expression())
                };
                arguments.push(expr);
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        };
        arguments
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
            T![and] => (5, 6),
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
    use crate::parser::ast::Expr::Literal;
    use crate::parser::ast::{FullIdent, IdentBase, IdentPart};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_expression_operator_priority() {
        let input = "1 + 2 * 3";
        let mut parser = Parser::new(input);
        let expr = parser.expression();
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
        let mut parser = Parser::new(input);
        let expr = parser.expression();
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
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![and],
                lhs: Box::new(Expr::IdentFnSubCall(FullIdent::ident("col"))),
                rhs: Box::new(Expr::int(0xFF)),
            }
        );
    }

    #[test]
    fn test_expression_is_nothing() {
        let input = "varValue Is Nothing";
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![is],
                lhs: Box::new(Expr::IdentFnSubCall(FullIdent {
                    base: IdentBase::ident("varValue"),
                    property_accesses: Vec::new(),
                })),
                rhs: Box::new(Literal(Lit::Nothing)),
            }
        );
    }

    #[test]
    fn test_expression_not_ident_is_nothing() {
        let input = "Not varValue Is Nothing";
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::PrefixOp {
                op: T![not],
                expr: Box::new(Expr::InfixOp {
                    op: T![is],
                    lhs: Box::new(Expr::IdentFnSubCall(FullIdent {
                        base: IdentBase::ident("varValue"),
                        property_accesses: Vec::new(),
                    })),
                    rhs: Box::new(Literal(Lit::Nothing)),
                }),
            }
        );
    }

    #[test]
    fn test_expression_equals() {
        let input = "varValue = varValue2";
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![=],
                lhs: Box::new(Expr::IdentFnSubCall(FullIdent {
                    base: IdentBase::ident("varValue"),
                    property_accesses: Vec::new(),
                })),
                rhs: Box::new(Expr::IdentFnSubCall(FullIdent {
                    base: IdentBase::ident("varValue2"),
                    property_accesses: Vec::new(),
                })),
            }
        );
    }

    #[test]
    fn test_expression_string_concatenation() {
        let input = r#""Hello" & " " & name"#;
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![&],
                lhs: Box::new(Expr::InfixOp {
                    op: T![&],
                    lhs: Box::new(Literal(Lit::str("Hello"))),
                    rhs: Box::new(Literal(Lit::str(" "))),
                }),
                rhs: Box::new(Expr::IdentFnSubCall(FullIdent::ident("name"))),
            }
        );
    }

    #[test]
    fn test_me_property_assignment() {
        let input = "Me.Name = \"John\"";
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![=],
                lhs: Box::new(Expr::IdentFnSubCall(FullIdent {
                    base: IdentBase::me(),
                    property_accesses: vec![IdentPart::ident("Name")],
                })),
                rhs: Box::new(Literal(Lit::str("John"))),
            }
        );
    }

    #[test]
    fn test_multiline_string() {
        let input = "test &_\r\n  \"Hello\"";
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![&],
                lhs: Box::new(Expr::IdentFnSubCall(FullIdent::ident("test"))),
                rhs: Box::new(Literal(Lit::str("Hello"))),
            }
        );
    }

    #[test]
    fn test_default_function_call() {
        // call the default function of a class with argument 1
        let input = "(new Foo)(1)";
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::FnCall {
                callee: Box::new(Expr::new("Foo")),
                args: vec![Expr::int(1)]
            }
        );
    }

    #[test]
    fn test_parenthesized_property_access_check() {
        let input = "(foo).enabled=false";
        let mut parser = Parser::new(input);
        let expr = parser.expression();
        assert_eq!(
            expr,
            Expr::InfixOp {
                op: T![=],
                lhs: Box::new(Expr::PropertyAccess {
                    base: Box::new(Expr::IdentFnSubCall(FullIdent::ident("foo"))),
                    property: "enabled".to_string(),
                }),
                rhs: Box::new(Literal(Lit::Bool(false))),
            }
        );
    }
}
