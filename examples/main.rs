use indoc::indoc;
use vbscript::lexer::TokenKind;

/// Run this example with `cargo run --example main`
fn main() {
    let input = indoc! {r#"
        ' This is a comment
        Sub Main()
            MsgBox "Hello, world!"
        End Sub
    "#};

    lex_and_print_strings(input);

    parse_and_print_ast_tree(input);
}

fn parse_and_print_ast_tree(input: &str) {
    let mut parser = vbscript::parser::Parser::new(input);
    let ast = parser.file();
    // print the tree
    println!("{:#?}", ast);
}

fn lex_and_print_strings(input: &str) {
    let lexer = vbscript::lexer::Lexer::new(input);
    let tokens = lexer.collect::<Vec<_>>();
    // print all strings
    for token in tokens {
        if let vbscript::lexer::Token {
            kind: TokenKind::String,
            ..
        } = token
        {
            println!(
                "Found a string at line {}, row {}: {}",
                token.line, token.column, &input[token.span]
            );
        }
    }
}
