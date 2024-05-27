use std::ops::Range;
use std::path::PathBuf;

use indoc::indoc;
use pretty_assertions::assert_eq;

use vbscript::parser::Parser;
use vbscript::{lexer::*, T};

/// walks `$tokens` and compares them to the given kinds.
macro_rules! assert_tokens {
    ($tokens:ident, [$($kind:expr,)*]) => {
        {
            let mut it = $tokens.iter();
            $(
                let token = it.next().expect("not enough tokens");
            assert_eq!(token.kind, $kind);
        )*
        }
    };
}

#[test]
fn single_char_tokens() {
    let input = "+-():";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_tokens!(tokens, [T![+], T![-], T!['('], T![')'], T![:], T![EOF],]);
}

#[test]
fn unknown_input() {
    let input = "{$$+";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().iter().map(|t| t.kind).collect::<Vec<_>>();

    // check lexer is instance of custom lexer

    // for the custom lexer
    // assert_eq!(tokens, [T![error], T![+], T![EOF],]);
    // for the logos lexer
    assert_eq!(
        tokens,
        [
            T![parse_error],
            T![parse_error],
            T![parse_error],
            T![+],
            T![EOF],
        ]
    );
}

#[test]
fn token_spans() {
    {
        let input = "+-():";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().iter().map(|t| t.kind).collect::<Vec<_>>();
        assert_eq!(tokens, [T![+], T![-], T!['('], T![')'], T![:], T![EOF],]);
    }
    {
        let input = "{$$$$$$$+";
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let error = tokens[0];
        assert_eq!(error.kind, T![parse_error]);
        // for the custom lexer
        //assert_eq!(error.span, (0..8).into())
        // for the logos lexer
        assert_eq!(error.span, (0..1).into())
    }
}

#[test]
fn single_char_tokens_with_whitespace() {
    let input = "   + -  ( ): ";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().iter().map(|t| t.kind).collect::<Vec<_>>();
    assert_eq!(
        tokens,
        [
            T![ws],
            T![+],
            T![ws],
            T![-],
            T![ws],
            T!['('],
            T![ws],
            T![')'],
            T![:],
            T![ws],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_only_whitespace() {
    let input = "  \t ";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_tokens!(tokens, [T![ws], T![EOF],]);
}

#[test]
fn maybe_multiple_char_tokens() {
    let input = "and= <=<>or";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    assert_tokens!(
        tokens,
        [T![and], T![=], T![ws], T![<=], T![<>], T![or], T![EOF],]
    );
}

#[test]
fn keywords() {
    let input = "if dim = function else sub";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [
            T![if],
            T![dim],
            T![=],
            T![function],
            T![else],
            T![sub],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_function() {
    let input = indoc! { r#"
        'tests stuff
        Function add(a, b)
	        test = a + b
        End Function
    "#};
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
            // comment
            T![comment],
            T![nl],
            // function signature
            T![function],
            T![ident],
            T!['('],
            T![ident],
            T![,],
            T![ident],
            T![')'],
            T![nl],
            // function result assignment
            T![ident],
            T![=],
            T![ident],
            T![+],
            T![ident],
            T![nl],
            // end function
            T![end],
            T![function],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_sub() {
    let input = indoc! {r#"
        Sub sw35_Hit() 'Drain
            UpdateTrough
            Controller.Switch(35) = 1
            RandomSoundDrain sw35
        End Sub
    "#};
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
    assert_eq!(
        //assert_tokens!(
        token_kinds,
        [
            // sub
            T![sub],
            //T![ws],
            T![ident],
            T!['('],
            T![')'],
            T![comment],
            T![nl],
            // UpdateTrough
            T![ident],
            T![nl],
            T![ident],
            T![_.],
            T![ident],
            T!['('],
            T![integer_literal],
            T![')'],
            T![=],
            T![integer_literal],
            T![nl],
            // RandomSoundDrain
            T![ident],
            T![ident],
            T![nl],
            // end sub
            T![end],
            T![sub],
            T![nl],
            T![EOF],
        ]
    );
    let update_through = tokens[6];
    assert_eq!("UpdateTrough", &input[update_through.span]);
}

#[test]
fn test_lexer_if_else() {
    let input = indoc! {r#"
        If (a = b) Then
            a = 1
        ElseIf (a = c) Then
            a = -1    
        Else
            a = 2
        End If
    "#};
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
            T![if],
            T!['('],
            T![ident],
            T![=],
            T![ident],
            T![')'],
            T![then],
            T![nl],
            T![ident],
            T![=],
            T![integer_literal],
            T![nl],
            T![elseif],
            T!['('],
            T![ident],
            T![=],
            T![ident],
            T![')'],
            T![then],
            T![nl],
            T![ident],
            T![=],
            T![-],
            T![integer_literal],
            T![nl],
            T![else],
            T![nl],
            T![ident],
            T![=],
            T![integer_literal],
            T![nl],
            T![end],
            T![if],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_string_literal() {
    let input = r#""Hello, World!""#;
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(tokens, [T![string_literal], T![EOF],]);
}

#[test]
fn test_lexer_array_declaration() {
    let input = "Dim a(1, 2, 3)";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [
            T![dim],
            T![ident],
            T!['('],
            T![integer_literal],
            T![,],
            T![integer_literal],
            T![,],
            T![integer_literal],
            T![')'],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_colon_separator() {
    let input = r#"Dim a: a = "Hello, World!""#;
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [
            T![dim],
            T![ident],
            T![:],
            T![ident],
            T![=],
            T![string_literal],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_class() {
    let input = indoc! {r#"
        Class MyClass
            Dim a
            Sub MySub()
                a = 1
            End Sub
        End Class
    "#};
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
            T![class],
            T![ident],
            T![nl],
            T![dim],
            T![ident],
            T![nl],
            T![sub],
            T![ident],
            T!['('],
            T![')'],
            T![nl],
            T![ident],
            T![=],
            T![integer_literal],
            T![nl],
            T![end],
            T![sub],
            T![nl],
            T![end],
            T![class],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_single_line_multi_const() {
    let input = "Const a = 1, b = 2, c = 3";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer
        .tokenize()
        .into_iter()
        .filter(|t| t.kind != T![ws])
        .collect();
    assert_tokens!(
        tokens,
        [
            T![const],
            T![ident],
            T![=],
            T![integer_literal],
            T![,],
            T![ident],
            T![=],
            T![integer_literal],
            T![,],
            T![ident],
            T![=],
            T![integer_literal],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_for() {
    let input = indoc! {r#"
        For i = 1 To 10 Step 2
            Debug.Print i
        Next i
    "#};
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
            T![for],
            T![ident],
            T![=],
            T![integer_literal],
            T![to],
            T![integer_literal],
            T![step],
            T![integer_literal],
            T![nl],
            T![ident],
            T![_.],
            T![ident],
            T![ident],
            T![nl],
            T![next],
            T![ident],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_while() {
    let input = indoc! {r#"
        Dim x:x=1
        Do While x<5
            document.write("Welcome.")
            x=x+1
        Loop
    "#};
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
            T![dim],
            T![ident],
            T![:],
            T![ident],
            T![=],
            T![integer_literal],
            T![nl],
            T![do],
            T![while],
            T![ident],
            T![<],
            T![integer_literal],
            T![nl],
            T![ident],
            T![_.],
            T![ident],
            T!['('],
            T![string_literal],
            T![')'],
            T![nl],
            T![ident],
            T![=],
            T![ident],
            T![+],
            T![integer_literal],
            T![nl],
            T![loop],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_select_case() {
    let input = indoc! {r#"
        select case strPerson
           case "Alex"
              WScript.Echo "We found Alex"
           case "Jasper"
              WScript.Echo "We found Jasper"
           case else
              WScript.Echo "We found someone else"   
        end select 
    "#};
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
            T![select],
            T![case],
            T![ident],
            T![nl],
            T![case],
            T![string_literal],
            T![nl],
            T![ident],
            T![_.],
            T![ident],
            T![string_literal],
            T![nl],
            T![case],
            T![string_literal],
            T![nl],
            T![ident],
            T![_.],
            T![ident],
            T![string_literal],
            T![nl],
            T![case],
            T![else],
            T![nl],
            T![ident],
            T![_.],
            T![ident],
            T![string_literal],
            T![nl],
            T![end],
            T![select],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_string_concatenation() {
    let input = r#"a = "Hello" & "World""#;
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer.tokenize();
    let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();
    assert_eq!(
        token_kinds,
        [
            T![ident],
            T![ws],
            T![=],
            T![ws],
            T![string_literal],
            T![ws],
            T![&],
            T![ws],
            T![string_literal],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_with() {
    let input = indoc! {r#"
        With obj
            .height = 5 \ x
        End With
    "#};
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
            T![with],
            T![ident],
            T![nl],
            T![.],
            T![ident],
            T![=],
            T![integer_literal],
            T!['\\'],
            T![ident],
            T![nl],
            T![end],
            T![with],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_comments_with_different_line_endings() {
    let input = "' comment with a CRLF\r\n' comment with a CR\r' comment with a LF\n";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer.tokenize();
    let token_kinds = tokens.iter().map(|t| t.kind).collect::<Vec<_>>();

    // print all tokens with their string
    // for token in tokens.iter() {
    //     println!("{:?} {:?}", token.kind, &input[token.span]);
    // }

    assert_eq!(
        token_kinds,
        [
            T![comment],
            T![nl],
            T![comment],
            T![nl],
            T![comment],
            T![nl],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_string_with_backslash() {
    let input =
        r#"check.RegRead ("HKLM\Software\Microsoft\Windows NT\CurrentVersion\CurrentVersion")"#;
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
            T![ident],
            T![_.],
            T![ident],
            T!['('],
            T![string_literal],
            T![')'],
            T![EOF],
        ]
    );
}

#[test]
fn test_lexer_full_class() {
    let input = indoc! {r#"
   Class Comp
   
       Private modStrType
       Private OS

       'Instantation of the Object
       Set objectname = New classname

       Private Sub Class_Initialize(  )
           'Initalization code goes here
       End Sub

       'When Object is Set to Nothing
       Private Sub Class_Terminate(  )
           'Termination code goes here
       End Sub
     
       Public Property Let ComputerType(strType)
           modStrType = strType
       End Property
     
       Public Property Get ComputerType()
           ComputerType = modStrType
       End Property
     
       Public Property Set OperatingSystem(oObj)
           Set OS = oObj
       End Property
     
       Public Property Get OperatingSystem()
           Set OperatingSystem = OS
       End Property

       Public Function Start()
           Debug.Print "Starting the computer"
       End Function
     
    End Class
    "#};
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer.tokenize();
    let token_kinds = tokens
        .iter()
        .filter(|t| t.kind != T![ws])
        .map(|t| t.kind)
        .collect::<Vec<_>>();
    assert_eq!(
        token_kinds,
        [
            T![class],
            T![ident],
            T![nl],
            T!(nl),
            T![private],
            T![ident],
            T![nl],
            T![private],
            T![ident],
            T![nl],
            T![nl],
            T![comment],
            T![nl],
            T![set],
            T![ident],
            T![=],
            T![new],
            T![ident],
            T![nl],
            T![nl],
            T![private],
            T![sub],
            T![ident],
            T!['('],
            T![')'],
            T![nl],
            T![comment],
            T![nl],
            T![end],
            T![sub],
            T![nl],
            T![nl],
            T![comment],
            T![nl],
            T![private],
            T![sub],
            T![ident],
            T!['('],
            T![')'],
            T![nl],
            T![comment],
            T![nl],
            T![end],
            T![sub],
            T![nl],
            T![nl],
            T![public],
            T![property],
            T![let],
            T![ident],
            T!['('],
            T![ident],
            T![')'],
            T![nl],
            T![ident],
            T![=],
            T![ident],
            T![nl],
            T![end],
            T![property],
            T![nl],
            T![nl],
            T![public],
            T![property],
            T![get],
            T![ident],
            T!['('],
            T![')'],
            T![nl],
            T![ident],
            T![=],
            T![ident],
            T![nl],
            T![end],
            T![property],
            T![nl],
            T![nl],
            T![public],
            T![property],
            T![set],
            T![ident],
            T!['('],
            T![ident],
            T![')'],
            T![nl],
            T![set],
            T![ident],
            T![=],
            T![ident],
            T![nl],
            T![end],
            T![property],
            T![nl],
            T![nl],
            T![public],
            T![property],
            T![get],
            T![ident],
            T!['('],
            T![')'],
            T![nl],
            T![set],
            T![ident],
            T![=],
            T![ident],
            T![nl],
            T![end],
            T![property],
            T![nl],
            T![nl],
            T![public],
            T![function],
            T![ident],
            T!['('],
            T![')'],
            T![nl],
            T![ident],
            T![_.],
            T![ident],
            T![string_literal],
            T![nl],
            T![end],
            T![function],
            T![nl],
            T![nl],
            T![end],
            T![class],
            T![nl],
            T![EOF],
        ]
    );
}
//
// #[test]
// fn parse_statements() {
//     fn parse(input: &str) -> ast::Stmt {
//         let mut parser = Parser::new(input);
//         parser.statement()
//     }
//
//     let stmt = parse(indoc! {r#"
//         {
//             let x = 7 + sin(y)
//             {
//                 x = 3
//                 if (bar < 3) then
//                     x = x + 1
//                     y = 3 * x
//                 else if (bar < 2) {
//                     let i = 2!
//                     x = x + i
//                 else
//                     x = 1
//                 end if
//             }
//         }
//     "#});
//
//     let stmts = match stmt {
//         ast::Stmt::Block { stmts } => stmts,
//         _ => unreachable!(),
//     };
//     assert_eq!(stmts.len(), 2);
//
//     let let_stmt = &stmts[0];
//     match let_stmt {
//         ast::Stmt::Set { var_name, .. } => assert_eq!(var_name, "x"),
//         _ => unreachable!(),
//     }
//
//     let stmts = match &stmts[1] {
//         ast::Stmt::Block { stmts } => stmts,
//         _ => unreachable!(),
//     };
//     assert_eq!(stmts.len(), 2);
//
//     let assignment_stmt = &stmts[0];
//     match assignment_stmt {
//         ast::Stmt::Assignment { var_name, .. } => {
//             assert_eq!(var_name, "x");
//         }
//         _ => unreachable!(),
//     }
//
//     let if_stmt = &stmts[1];
//     match if_stmt {
//         ast::Stmt::IfStmt {
//             condition,
//             body,
//             else_stmt,
//         } => {
//             assert!(matches!(
//                 &**condition,
//                 ast::Expr::InfixOp {
//                     op: T![<],
//                     lhs: _lhs,
//                     rhs: _rhs,
//                 }
//             ));
//             assert_eq!(body.len(), 2);
//             let x_assignment = &body[0];
//             match x_assignment {
//                 ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "x"),
//                 _ => unreachable!(),
//             }
//             let y_assignment = &body[1];
//             match y_assignment {
//                 ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "y"),
//                 _ => unreachable!(),
//             }
//
//             let else_stmt = match else_stmt {
//                 Some(stmt) => &**stmt,
//                 None => unreachable!(),
//             };
//
//             match else_stmt {
//                 ast::Stmt::IfStmt {
//                     condition,
//                     body,
//                     else_stmt,
//                 } => {
//                     assert!(matches!(
//                         &**condition,
//                         ast::Expr::InfixOp {
//                             op: T![<],
//                             lhs: _lhs,
//                             rhs: _rhs,
//                         }
//                     ));
//                     assert_eq!(body.len(), 2);
//                     let let_i = &body[0];
//                     match let_i {
//                         ast::Stmt::Set { var_name, .. } => assert_eq!(var_name, "i"),
//                         _ => unreachable!(),
//                     }
//                     let x_assignment = &body[1];
//                     match x_assignment {
//                         ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "x"),
//                         _ => unreachable!(),
//                     }
//
//                     let else_stmt = match else_stmt {
//                         Some(stmt) => &**stmt,
//                         None => unreachable!(),
//                     };
//
//                     let stmts = match else_stmt {
//                         ast::Stmt::Block { stmts } => stmts,
//                         _ => unreachable!(),
//                     };
//                     assert_eq!(stmts.len(), 1);
//
//                     let x_assignment = &stmts[0];
//                     match x_assignment {
//                         ast::Stmt::Assignment { var_name, .. } => assert_eq!(var_name, "x"),
//                         _ => unreachable!(),
//                     }
//                 }
//                 _ => unreachable!(),
//             };
//         }
//         _ => unreachable!(),
//     }
// }

// TODO make sure we can remove these exclusions
static EXCLUDED_FILES: &[&str] = &[
    // all below for Error { kind: InvalidData, message: "stream did not contain valid UTF-8" }
    "sverrewl-vpxtable-scripts/Aztec (Williams 1976) 1.3 Mod Citedor JPJ-ARNGRIM-CED Team PP.vbs",
    "sverrewl-vpxtable-scripts/Cactus Canyon (Bally 1998) TTNZ v2.0.vbs",
    "sverrewl-vpxtable-scripts/Check Mate (Taito - 1977) 1.0.0.vbs",
    "sverrewl-vpxtable-scripts/Demolition Man (Knorr-Kiwi) 1.3.1.vbs",
    "sverrewl-vpxtable-scripts/Freddy A Nightmare On Elm Street (Gottlieb 1994) 1.1b JPJ - Team PP - Hauntfreaks Mod.vbs",
    "sverrewl-vpxtable-scripts/Lucky Luke (Gottlieb 1975 - FastDrawMod) v0.91.vbs",
    "sverrewl-vpxtable-scripts/Star Gazer(Stern 1980) v1.2.vbs",
    "sverrewl-vpxtable-scripts/Star Gazer(Stern 1980)Siggis Mod 1.0.vbs",
    "sverrewl-vpxtable-scripts/The Six Million Dollar Man (Bally 1978) v1.0.vbs",
    "sverrewl-vpxtable-scripts/Theatre of magic VPX NZ-TT 1.0.vbs",
    // Uses `If ... End If` without Then TODO check if this is valid
    "sverrewl-vpxtable-scripts/Bally Roller Derby 2.0.vbs",
];

/// It tries to tokenize all `.vbs` files going one level lower from the root of the project.
/// We suggest to make sure you have https://github.com/jsm174/vpx-standalone-scripts cloned
/// in ./testsctipts
///
/// Run this test with `cargo test --release -- --nocapture --ignored try_tokenizing_all_vbs_files`
#[test]
fn try_tokenizing_all_vbs_files() {
    let paths = test_scripts();
    for path in paths {
        println!("Tokenizing file: {:?}", path);
        let input = std::fs::read_to_string(&path).unwrap();
        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokenize();

        // print path and the last 10 tokens before the error if there is an error
        // and fail the test
        if let Some(token) = tokens.iter().find(|t| t.kind == T![parse_error]) {
            let idx = tokens.iter().position(|t| t == token).unwrap();
            let start = if idx > 10 { idx - 10 } else { 0 };
            let end = idx + 1;
            println!("Error in file: {:?}", path);
            for token in &tokens[start..end] {
                let range: Range<usize> = token.span.into();
                println!("  {:?} {:?}", token.kind, &input[range]);
            }
            panic!("Error in file: {:?}", path);
        }
    }
}

fn test_scripts() -> impl Iterator<Item = PathBuf> {
    let paths = glob::glob("./testscripts/**/*.vbs")
        .unwrap()
        .filter_map(Result::ok)
        .filter(|p| {
            !EXCLUDED_FILES
                .iter()
                .any(|f| p.to_str().unwrap().contains(f))
        });
    paths
}

/// This test tries to parse all `.vbs` files going one level lower from the root of the project.
/// We suggest to make sure you have https://github.com/jsm174/vpx-standalone-scripts cloned
/// in ./testsctipts
///
/// Run this test with `cargo test --release -- --nocapture --ignored try_parsing_all_vbs_files`
#[test]
fn try_parsing_all_vbs_files() {
    let paths = test_scripts();
    for path in paths {
        println!("Parsing file: {}", path.display());
        let input = std::fs::read_to_string(&path).unwrap();
        let mut parser = Parser::new(&input);
        let items = parser.file();

        assert!(!items.is_empty())
    }
}
