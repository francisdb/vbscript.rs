//! "Name redefined": every case was compiled with `cscript` on Windows, as a script of its
//! own. The positions are the ones that Windows reports.

use vbscript::parser::Parser;

/// What Windows rejects, with the line and column of the error.
const REDEFINED: &[(&str, &str, (usize, usize))] = &[
    (
        "dim twice one stmt",
        "Sub S\n Dim a, b, A\nEnd Sub",
        (2, 12),
    ),
    (
        "dim twice two stmts",
        "Sub S\n Dim a\n Dim a\nEnd Sub",
        (3, 6),
    ),
    ("dim of a parameter", "Sub S(a)\n Dim a\nEnd Sub", (2, 6)),
    (
        "const twice",
        "Sub S\n Const a = 1\n Const a = 2\nEnd Sub",
        (3, 8),
    ),
    (
        "const twice one stmt",
        "Sub S\n Const a = 1, a = 2\nEnd Sub",
        (2, 15),
    ),
    (
        "dim then const",
        "Sub S\n Dim a\n Const a = 1\nEnd Sub",
        (3, 8),
    ),
    (
        "const then dim",
        "Sub S\n Const a = 1\n Dim a\nEnd Sub",
        (3, 6),
    ),
    (
        "const of a parameter",
        "Sub S(a)\n Const a = 1\nEnd Sub",
        (2, 8),
    ),
    (
        "dim twice in blocks",
        "Sub S\n If True Then\n  Dim a\n Else\n  Dim a\n End If\nEnd Sub",
        (5, 7),
    ),
    (
        "dim in loop and after",
        "Sub S\n Do\n  Dim a\n Loop\n Dim a\nEnd Sub",
        (5, 6),
    ),
    (
        "dim array then scalar",
        "Sub S\n Dim a(1), a\nEnd Sub",
        (2, 12),
    ),
    (
        "redim then dim",
        "Sub S\n ReDim a(2)\n Dim a\nEnd Sub",
        (3, 6),
    ),
    (
        "redim then const",
        "Sub S\n ReDim a(2)\n Const a = 1\nEnd Sub",
        (3, 8),
    ),
    (
        "function: dim own name",
        "Function F\n Dim F\nEnd Function",
        (2, 6),
    ),
    (
        "function: const own name",
        "Function F\n Const F = 1\nEnd Function",
        (2, 8),
    ),
    ("dim twice", "Dim a\nDim a", (2, 5)),
    ("dim twice one stmt", "Dim a, A", (1, 8)),
    ("dim then const", "Dim a\nConst a = 1", (2, 7)),
    ("const twice", "Const a = 1\nConst a = 2", (2, 7)),
    ("dim then sub", "Dim a\nSub a\nEnd Sub", (2, 5)),
    ("sub then dim", "Sub a\nEnd Sub\nDim a", (3, 5)),
    (
        "dim then function",
        "Dim a\nFunction a\nEnd Function",
        (2, 10),
    ),
    ("const then sub", "Const a = 1\nSub a\nEnd Sub", (2, 5)),
    (
        "class twice",
        "Class a\nEnd Class\nClass a\nEnd Class",
        (3, 7),
    ),
    ("class then dim", "Class a\nEnd Class\nDim a", (3, 5)),
    ("dim then class", "Dim a\nClass a\nEnd Class", (2, 7)),
    (
        "class then sub",
        "Class a\nEnd Class\nSub a\nEnd Sub",
        (3, 5),
    ),
    (
        "sub then class",
        "Sub a\nEnd Sub\nClass a\nEnd Class",
        (3, 7),
    ),
    (
        "class then const",
        "Class a\nEnd Class\nConst a = 1",
        (3, 7),
    ),
    ("public then private", "Public a\nPrivate a", (2, 9)),
    ("public then dim", "Public a\nDim a", (2, 5)),
    (
        "dim in if blocks",
        "If True Then\n Dim a\nElse\n Dim a\nEnd If",
        (4, 6),
    ),
    ("redim then dim", "ReDim a(2)\nDim a", (2, 5)),
    ("public const then dim", "Public Const a = 1\nDim a", (2, 5)),
    ("redim then sub", "ReDim a(2)\nSub a\nEnd Sub", (2, 5)),
    ("redim then const", "ReDim a(2)\nConst a = 1", (2, 7)),
    (
        "public twice",
        "Class C\n Public a\n Public a\nEnd Class",
        (3, 9),
    ),
    (
        "public then dim",
        "Class C\n Public a\n Dim a\nEnd Class",
        (3, 6),
    ),
    (
        "public then sub",
        "Class C\n Public a\n Sub a\n End Sub\nEnd Class",
        (3, 6),
    ),
    (
        "sub twice",
        "Class C\n Sub a\n End Sub\n Sub a\n End Sub\nEnd Class",
        (4, 6),
    ),
    (
        "sub then function",
        "Class C\n Sub a\n End Sub\n Function a\n End Function\nEnd Class",
        (4, 11),
    ),
    (
        "public then property get",
        "Class C\n Public a\n Property Get a\n End Property\nEnd Class",
        (3, 15),
    ),
    (
        "property get then sub",
        "Class C\n Property Get a\n End Property\n Sub a\n End Sub\nEnd Class",
        (4, 6),
    ),
    (
        "property get twice",
        "Class C\n Property Get a\n End Property\n Property Get a\n End Property\nEnd Class",
        (4, 15),
    ),
    (
        "property let twice",
        "Class C\n Property Let a(v)\n End Property\n Property Let a(v)\n End Property\nEnd Class",
        (4, 15),
    ),
    (
        "property then public",
        "Class C\n Property Get a\n End Property\n Public a\nEnd Class",
        (4, 9),
    ),
    (
        "property: dim own name",
        "Class C\n Property Get P\n  Dim P\n End Property\nEnd Class",
        (3, 7),
    ),
    (
        "property let: dim own",
        "Class C\n Property Let P(v)\n  Dim P\n End Property\nEnd Class",
        (3, 7),
    ),
    (
        "property set: dim own",
        "Class C\n Property Set P(v)\n  Dim P\n End Property\nEnd Class",
        (3, 7),
    ),
    (
        "class function: dim own",
        "Class C\n Function F\n  Dim F\n End Function\nEnd Class",
        (3, 7),
    ),
    (
        "sub: redim a global then dim",
        "Dim g\nSub S\n ReDim g(2)\n Dim g\nEnd Sub",
        (4, 6),
    ),
    ("sub then redim", "Sub a\nEnd Sub\nReDim a(2)", (3, 7)),
    ("class then redim", "Class a\nEnd Class\nReDim a(2)", (3, 7)),
    ("redim then class", "ReDim a(2)\nClass a\nEnd Class", (2, 7)),
    (
        "redim preserve then dim",
        "Sub S\n ReDim Preserve a(2)\n Dim a\nEnd Sub",
        (3, 6),
    ),
    ("dim, case differs", "Sub S(abc)\n Dim ABC\nEnd Sub", (2, 6)),
    (
        "function then const",
        "Function a\nEnd Function\nConst a = 1",
        (3, 7),
    ),
    ("private then sub", "Private a\nSub a\nEnd Sub", (2, 5)),
    (
        "global dim in if, then sub",
        "If True Then\n Dim a\nEnd If\nSub a\nEnd Sub",
        (4, 5),
    ),
    (
        "class: dim twice one stmt",
        "Class C\n Dim a, a\nEnd Class",
        (2, 9),
    ),
    (
        "class: dim then dim",
        "Class C\n Dim a\n Dim a\nEnd Class",
        (3, 6),
    ),
    (
        "class: private then get",
        "Class C\n Private a\n Public Property Get a\n End Property\nEnd Class",
        (3, 22),
    ),
    (
        "class: public then let",
        "Class C\n Public a\n Property Let a(v)\n End Property\nEnd Class",
        (3, 15),
    ),
    (
        "class: let then function",
        "Class C\n Property Let a(v)\n End Property\n Function a\n End Function\nEnd Class",
        (4, 11),
    ),
    (
        "class: sub then public",
        "Class C\n Sub a\n End Sub\n Public a\nEnd Class",
        (4, 9),
    ),
    (
        "class: set twice",
        "Class C\n Property Set a(v)\n End Property\n Property Set a(v)\n End Property\nEnd Class",
        (4, 15),
    ),
    (
        "class: sub, get, sub",
        "Class C\n Sub a\n End Sub\n Property Get a\n End Property\n Sub a\n End Sub\nEnd Class",
        (6, 6),
    ),
    (
        "class: sub, get, get",
        "Class C\n Sub a\n End Sub\n Property Get a\n End Property\n Property Get a\n End Property\nEnd Class",
        (6, 15),
    ),
    (
        "class: dim then sub",
        "Class C\n Dim a\n Sub a\n End Sub\nEnd Class",
        (3, 6),
    ),
    (
        "class: sub then dim",
        "Class C\n Sub a\n End Sub\n Dim a\nEnd Class",
        (4, 6),
    ),
    (
        "class: get then dim",
        "Class C\n Property Get a\n End Property\n Dim a\nEnd Class",
        (4, 6),
    ),
    (
        "class: public array twice",
        "Class C\n Public a(2)\n Private a\nEnd Class",
        (3, 10),
    ),
    (
        "property param like other",
        "Class C\n Property Let a(b, B)\n End Property\nEnd Class",
        (2, 20),
    ),
    (
        "sub: redim twice then const",
        "Sub S\n ReDim a(1)\n ReDim a(2)\n Const a = 1\nEnd Sub",
        (4, 8),
    ),
    (
        "sub: dim in select case",
        "Sub S\n Select Case 1\n Case 1\n  Dim a\n Case 2\n  Dim a\n End Select\nEnd Sub",
        (6, 7),
    ),
    (
        "class method: const twice",
        "Class C\n Sub S\n  Const a = 1\n  Const a = 2\n End Sub\nEnd Class",
        (4, 9),
    ),
    (
        "global: sub, dim in between",
        "Sub a\nEnd Sub\nSub b\nEnd Sub\nDim b",
        (5, 5),
    ),
    (
        "redim two, then dim",
        "Sub S\n ReDim a(2), b(3)\n Dim b\nEnd Sub",
        (3, 6),
    ),
    (
        "method: redim member, dim",
        "Class C\n Public a\n Sub S\n  ReDim a(2)\n  Dim a\n End Sub\nEnd Class",
        (5, 7),
    ),
    ("dim three on one line", "Dim a : Dim b : Dim a", (1, 21)),
    (
        "default get then get",
        "Class C\n Public Default Property Get a\n End Property\n Property Get A\n End Property\nEnd Class",
        (4, 15),
    ),
    (
        "class_initialize twice",
        "Class C\n Private Sub Class_Initialize\n End Sub\n Private Sub Class_Initialize\n End Sub\nEnd Class",
        (4, 14),
    ),
];

/// What Windows accepts.
const VALID: &[(&str, &str)] = &[
    ("dim then redim", "Sub S\n Dim a()\n ReDim a(2)\nEnd Sub"),
    ("redim twice", "Sub S\n ReDim a(2)\n ReDim a(3)\nEnd Sub"),
    ("redim of a parameter", "Sub S(a)\n ReDim a(2)\nEnd Sub"),
    (
        "function: redim own name",
        "Function F\n ReDim F(2)\nEnd Function",
    ),
    ("sub: dim own name", "Sub S\n Dim S\nEnd Sub"),
    ("sub: const own name", "Sub S\n Const S = 1\nEnd Sub"),
    (
        "dim other sub's name",
        "Sub S1\nEnd Sub\nSub S2\n Dim S1\nEnd Sub",
    ),
    ("dim a global's name", "Dim g\nSub S\n Dim g\nEnd Sub"),
    (
        "dim a class name",
        "Class C\nEnd Class\nSub S\n Dim C\nEnd Sub",
    ),
    (
        "same dim in two subs",
        "Sub S1\n Dim a\nEnd Sub\nSub S2\n Dim a\nEnd Sub",
    ),
    (
        "for each var then dim",
        "Sub S\n For Each a In b\n Next\n Dim a\nEnd Sub",
    ),
    ("sub twice", "Sub a\nEnd Sub\nSub a\nEnd Sub"),
    (
        "sub then function",
        "Sub a\nEnd Sub\nFunction a\nEnd Function",
    ),
    ("dim then redim", "Dim a()\nReDim a(2)"),
    ("redim twice", "ReDim a(2)\nReDim a(3)"),
    (
        "sub then property get",
        "Class C\n Sub a\n End Sub\n Property Get a\n End Property\nEnd Class",
    ),
    (
        "get let set",
        "Class C\n Property Get a\n End Property\n Property Let a(v)\n End Property\n Property Set a(v)\n End Property\nEnd Class",
    ),
    ("member named like class", "Class C\n Public C\nEnd Class"),
    (
        "method named like class",
        "Class C\n Sub C\n End Sub\nEnd Class",
    ),
    (
        "method dim like member",
        "Class C\n Public a\n Sub S\n  Dim a\n End Sub\nEnd Class",
    ),
    (
        "class sub: dim own name",
        "Class C\n Sub S\n  Dim S\n End Sub\nEnd Class",
    ),
    (
        "same member in 2 classes",
        "Class C1\n Public a\nEnd Class\nClass C2\n Public a\nEnd Class",
    ),
    (
        "member like a global sub",
        "Sub a\nEnd Sub\nClass C\n Public a\nEnd Class",
    ),
    (
        "function twice",
        "Function a\nEnd Function\nFunction a\nEnd Function",
    ),
    (
        "class: function then get",
        "Class C\n Function a\n End Function\n Property Get a\n End Property\nEnd Class",
    ),
    (
        "class: sub then let",
        "Class C\n Sub a\n End Sub\n Property Let a(v)\n End Property\nEnd Class",
    ),
    (
        "class: get, sub, let",
        "Class C\n Property Get a\n End Property\n Sub b\n End Sub\n Property Let a(v)\n End Property\nEnd Class",
    ),
    (
        "function: redim own, dim",
        "Function F\n ReDim F(2)\nEnd Function\nSub S\n Dim F\nEnd Sub",
    ),
    (
        "global const in sub scope",
        "Const a = 1\nSub S\n Const a = 2\n Dim b\nEnd Sub",
    ),
    (
        "global dim after use in sub",
        "Sub S\n x = 1\nEnd Sub\nDim x",
    ),
    ("for var then dim global", "For i = 1 To 2\nNext\nDim i"),
    ("const then redim", "Const a = 1\nReDim a(2)"),
    (
        "sub: const then redim",
        "Sub S\n Const a = 1\n ReDim a(2)\nEnd Sub",
    ),
    ("sub: redim own name", "Sub S\n ReDim S(2)\nEnd Sub"),
    ("redim twice one stmt", "Sub S\n ReDim a(2), a(3)\nEnd Sub"),
    (
        "function, names of others",
        "Function F(x)\n F = 1\n Dim x2, F2\nEnd Function\nSub F2\nEnd Sub",
    ),
];

#[test]
fn name_redefined() {
    for (what, input, position) in REDEFINED {
        match Parser::new(input).file() {
            Ok(_) => panic!("{what}: expected an error for {input:?}"),
            Err(error) => {
                assert!(
                    error.message().starts_with("Name redefined"),
                    "{what}: {error}"
                );
                assert_eq!((error.line(), error.column()), *position, "{what}: {error}");
            }
        }
    }
}

#[test]
fn name_not_redefined() {
    for (what, input) in VALID {
        let result = Parser::new(input).file();
        assert!(result.is_ok(), "{what}: {input:?}: {result:?}");
    }
}
