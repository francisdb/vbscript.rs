use crate::lexer::TokenKind;
use std::fmt;
use std::fmt::{Display, Formatter};

/*

From https://www.vbsedit.com/html/9233ea93-1f8d-4ac5-9ad9-d27ecff00da4.asp

Dim varname[([subscripts])][, varname[([subscripts])]] . . .

ReDim [Preserve] varname(subscripts) [, varname(subscripts)] . . .

Set objectvar = {objectexpression | New classname | Nothing}
' or
Set object.eventname = GetRef(procname)

Do [{While | Until} condition]
   [statements]
   [Exit Do]
   [statements]
Loop               ' or use this syntax
Do
   [statements]
   [Exit Do]
   [statements]
Loop [{While | Until} condition]

For counter = start To end [Step step]
    [statements]
    [Exit For]
    [statements]
Next

For Each element In group
   [statements]
   [Exit For]
   [statements]
Next [element]

While condition
   [statements]
Wend

If condition Then statements [Else elsestatements ]
' Or, you can use the block form syntax:
If condition Then
   [statements]
[ElseIf condition-n Then
   [elseifstatements]] . . .
[Else
   [elsestatements]]
End If

Select Case testexpression
   [Case expressionlist-n
      [statements-n]] . . .
   [Case Else
      [elsestatements-n]]
End Select

[Call] name [argumentlist]



With object
      statements
End With

[Public [Default] | Private] Sub name [(arglist)]
   [statements]
   [Exit Sub]
   [statements]
End Sub

[Public [Default] | Private] Function name [(arglist)]
   [statements]
   [name = expression]
   [Exit Function]
   [statements]
   [name = expression]
End Function

Class name
      statements
End Class

[Public | Private] Property Let name ([arglist,] value)
   [statements]
   [Exit Property]
   [statements]
End Property

[Public | Private] Property Set name([arglist,] reference)
   [statements]
   [Exit Property]
   [statements]
End Property

[Public [Default] | Private] Property Get name [(arglist)]
   [statements]
   [[Set] name = expression]
   [Exit Property]
   [statements]
   [[Set] name = expression]
End Property

*/

#[derive(Debug, Clone, PartialEq)]
pub struct IdentPart {
    pub name: String,
    // there might be multiple array indices/function calls
    // eg a(1,2)(2)
    pub array_indices: Vec<Vec<Option<Expr>>>,
}

impl IdentPart {
    pub fn ident(name: impl Into<String>) -> Self {
        IdentPart {
            name: name.into(),
            array_indices: Vec::new(),
        }
    }
}

impl Display for IdentPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        fmt_indices(f, &self.array_indices)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IdentBase {
    /// When used in a `with` statement, eg `.property`
    Partial(IdentPart),
    /// When used as a standalone identifier, eg `variable`
    Complete(IdentPart),
    Me {
        array_indices: Vec<Vec<Option<Expr>>>,
    },
}

fn fmt_indices(f: &mut Formatter, array_indices: &Vec<Vec<Option<Expr>>>) -> fmt::Result {
    for indices in array_indices {
        write!(f, "(")?;
        for (i, index) in indices.iter().enumerate() {
            match index {
                Some(index) => write!(f, "{}", index)?,
                None => write!(f, "")?,
            }
            if i < indices.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;
    }
    Ok(())
}

impl IdentBase {
    pub fn ident(name: impl Into<String>) -> Self {
        IdentBase::Complete(IdentPart::ident(name))
    }

    pub fn partial(name: impl Into<String>) -> Self {
        IdentBase::Partial(IdentPart::ident(name))
    }

    pub fn me() -> Self {
        IdentBase::Me {
            array_indices: Vec::new(),
        }
    }

    pub fn array_indices(&self) -> &Vec<Vec<Option<Expr>>> {
        match &self {
            IdentBase::Complete(part) => &part.array_indices,
            IdentBase::Partial(part) => &part.array_indices,
            IdentBase::Me { array_indices } => array_indices,
        }
    }

    pub fn set_array_indices(&mut self, array_indices: Vec<Vec<Option<Expr>>>) {
        match self {
            IdentBase::Complete(part) => part.array_indices = array_indices,
            IdentBase::Partial(part) => part.array_indices = array_indices,
            IdentBase::Me {
                array_indices: me_array_indices,
            } => *me_array_indices = array_indices,
        }
    }
}

impl Display for IdentBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IdentBase::Partial(part) => write!(f, ".{}", part),
            IdentBase::Complete(part) => write!(f, "{}", part),
            IdentBase::Me { array_indices } => {
                write!(f, "Me")?;
                fmt_indices(f, array_indices)?;
                Ok(())
            }
        }
    }
}

/// An identifier with optional property accesses
/// eg `a.b.c`, `a.b(1).c`, `a.b(1)(2).c` or `a.b(1,2).c(3)`
///
/// note: Contructed like this because at least one identifier is required
#[derive(Debug, Clone, PartialEq)]
pub struct FullIdent {
    pub base: IdentBase,
    pub property_accesses: Vec<IdentPart>,
}

impl FullIdent {
    pub fn ident(name: impl Into<String>) -> Self {
        FullIdent {
            base: IdentBase::ident(name),
            property_accesses: Vec::new(),
        }
    }

    pub fn partial(name: impl Into<String>) -> Self {
        FullIdent {
            base: IdentBase::partial(name),
            property_accesses: Vec::new(),
        }
    }
}

impl Display for FullIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.base)?;
        for prop in &self.property_accesses {
            write!(f, ".{}", prop)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Lit),
    Ident(String),
    /// An identifier, identifier with array access, sub or function call
    /// This grammar is ambiguous, so will need to be resolved at runtime
    /// TODO we can probably make a different type for
    ///   * Ident without array access or SubCall without args
    ///   * Ident with array access or FnCall with args or
    ///   * FnCall without args
    ///   * SubCall with args
    IdentFnSubCall(FullIdent),
    // FnCall {
    //     fn_name: FullIdent,
    //     args: Vec<Expr>,
    // },
    // SubCall {
    //     fn_name: String,
    //     args: Vec<Expr>,
    // },
    PrefixOp {
        op: TokenKind,
        expr: Box<Expr>,
    },
    InfixOp {
        op: TokenKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    // PostfixOp {
    //     op: TokenKind,
    //     expr: Box<Expr>,
    // }
    New(String),
    FnApplication {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    PropertyAccess {
        base: Box<Expr>,
        property: String,
    },
}

impl Expr {
    pub fn ident2(name: impl Into<String>) -> Self {
        Expr::IdentFnSubCall(FullIdent::ident(name))
    }

    pub fn ident(name: impl Into<String>) -> Self {
        Expr::Ident(name.into())
    }

    pub fn int(i: isize) -> Self {
        Expr::Literal(Lit::Int(i))
    }

    pub fn str(s: impl Into<String>) -> Self {
        Expr::Literal(Lit::Str(s.into()))
    }

    pub fn new(name: impl Into<String>) -> Self {
        Expr::New(name.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(isize),
    Float(f64),
    Str(String),
    Bool(bool),
    Nothing,
    Empty,
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorClause {
    ResumeNext,
    Goto0,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SetRhs {
    Expr(Box<Expr>),
    Nothing,
}

impl SetRhs {
    pub fn ident(name: impl Into<String>) -> Self {
        SetRhs::Expr(Box::new(Expr::ident(name)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DoLoopCondition {
    While(Box<Expr>),
    Until(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DoLoopCheck {
    Pre(DoLoopCondition),
    Post(DoLoopCondition),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub tests: Vec<Expr>,
    pub body: Vec<Stmt>,
}

// Statements
// https://learn.microsoft.com/en-us/previous-versions/7aw9cadb(v=vs.85)
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Dim {
        vars: Vec<(String, Vec<Expr>)>,
    },
    ReDim {
        preserve: bool,
        var_bounds: Vec<(String, Vec<Expr>)>,
    },
    Const(Vec<(String, Lit)>),
    Set {
        var: FullIdent,
        rhs: SetRhs,
    },
    Assignment {
        full_ident: FullIdent,
        value: Box<Expr>,
    },
    IfStmt {
        condition: Box<Expr>,
        body: Vec<Stmt>,
        elseif_statements: Vec<(Box<Expr>, Vec<Stmt>)>,
        else_stmt: Option<Vec<Stmt>>,
    },
    WhileStmt {
        condition: Box<Expr>,
        body: Vec<Stmt>,
    },
    ForStmt {
        counter: String,
        start: Box<Expr>,
        end: Box<Expr>,
        step: Option<Box<Expr>>,
        body: Vec<Stmt>,
    },
    ForEachStmt {
        element: String,
        group: Box<Expr>,
        body: Vec<Stmt>,
    },
    DoLoop {
        check: DoLoopCheck,
        body: Vec<Stmt>,
    },
    // https://learn.microsoft.com/en-us/previous-versions/6ef9w614(v=vs.85)
    SelectCase {
        test_expr: Box<Expr>,
        cases: Vec<Case>,
        else_stmt: Option<Vec<Stmt>>,
    },
    SubCall {
        fn_name: FullIdent,
        /// Empty arguments are allowed, eg 'MySub 1,,2'
        args: Vec<Option<Expr>>,
    },
    /// Call statement
    /// You are not required to use the Call keyword when calling a procedure. However,
    /// if you use the Call keyword to call a procedure that requires arguments, argumentlist
    /// must be enclosed in parentheses. If you omit the Call keyword, you also must omit
    /// the parentheses around argumentlist. If you use either Call syntax to call any intrinsic
    /// or user-defined function, the function's return value is discarded.
    Call(FullIdent),
    With {
        object: FullIdent,
        body: Vec<Stmt>,
    },
    // https://learn.microsoft.com/en-us/previous-versions//tt223ahx(v=vs.85)
    // There are restrictions as to where these can be defined
    // TODO apply these restrictions, also to function
    // You can't define a Sub procedure inside any other procedure (e.g. Function, Sub or Property Get).
    Sub {
        visibility: Visibility,
        name: String,
        // TODO handle ByVal and ByRef
        parameters: Vec<Argument>,
        body: Vec<Stmt>,
    },
    // https://learn.microsoft.com/en-us/previous-versions//x7hbf8fa(v=vs.85)
    Function {
        visibility: Visibility,
        name: String,
        parameters: Vec<Argument>,
        body: Vec<Stmt>,
    },
    ExitDo,
    ExitFor,
    ExitFunction,
    ExitProperty,
    ExitSub,
    OnError {
        error_clause: ErrorClause,
    },
}

// Byval and ByRef
// https://docs.microsoft.com/en-us/dotnet/visual-basic/programming-guide/language-features/procedures/argument-passing-mechanisms
#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
    ByVal(String),
    ByRef(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyVisibility {
    Public { default: bool },
    Private,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PropertyType {
    Let,
    Set,
    Get,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgumentType {
    ByVal,
    ByRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccess {
    pub visibility: PropertyVisibility,
    pub name: String,
    pub property_type: PropertyType,
    pub args: Vec<(String, ArgumentType)>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Default,
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberDefinitions {
    pub visibility: Visibility,
    pub properties: Vec<(String, Option<Vec<usize>>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    // https://learn.microsoft.com/en-us/previous-versions/windows/internet-explorer/ie-developer/scripting-articles/bw9t3484%28v%3Dvs.84%29
    OptionExplicit,
    // https://learn.microsoft.com/en-us/previous-versions//4ah5852c(v=vs.85)
    Class {
        name: String,
        members: Vec<MemberDefinitions>,
        dims: Vec<Vec<(String, Option<Vec<usize>>)>>,
        member_accessors: Vec<MemberAccess>,
        methods: Vec<Stmt>, // expect only functions and subs
    },
    /// This is a script-level const that has visibility
    /// Consts in procedures are handled by Stmt::Const
    Const {
        visibility: Visibility,
        values: Vec<(String, Lit)>,
    },
    /// This is a script-level variable that has visibility
    /// e.g. `Public a, b, c` or `Private a, b, c`
    /// note: `Public a()` is a dynamic array and not the same as `Public a`
    /// https://stackoverflow.com/a/23911728/42198
    Variable {
        visibility: Visibility,
        vars: Vec<(String, Option<Vec<usize>>)>,
    },
    Statement(Stmt),
}

impl Stmt {
    pub fn dim(var_name: impl Into<String>) -> Self {
        Stmt::Dim {
            vars: vec![(var_name.into(), Vec::new())],
        }
    }

    pub fn const_(var_name: impl Into<String>, value: Lit) -> Self {
        Stmt::Const(vec![(var_name.into(), value)])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub name: String,
    pub generics: Vec<Type>,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(lit) => write!(f, "{}", lit),
            Expr::Ident(ident) => write!(f, "{}", ident),
            Expr::IdentFnSubCall(ident) => {
                write!(f, "{}", ident)
            }
            // Expr::FnCall { fn_name, args } => {
            //     write!(f, "{}(", fn_name)?;
            //     for arg in args {
            //         write!(f, "{},", arg)?;
            //     }
            //     write!(f, ")")
            // }
            // Expr::SubCall { fn_name, args } => {
            //     write!(f, "{}", fn_name)?;
            //     for arg in args {
            //         write!(f, "{},", arg)?;
            //     }
            //     write!(f, "")
            // }
            Expr::PrefixOp { op, expr } => write!(f, "({} {})", op, expr),
            Expr::InfixOp { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            // Expr::PostfixOp { op, expr } =>
            //     write!(f, "({} {})", expr, op),
            Expr::New(name) => write!(f, "New {}", name),
            Expr::FnApplication { callee, args } => {
                write!(f, "{}(", callee)?;
                let len = args.len();
                for (i, arg) in args.iter().enumerate() {
                    if i != len - 1 {
                        write!(f, "{}, ", arg)?;
                    } else {
                        write!(f, "{}", arg)?;
                    }
                }
                write!(f, ")")
            }
            Expr::PropertyAccess { base, property } => write!(f, "{}.{}", base, property),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Float(fl) => write!(f, "{}", fl),
            Lit::Str(s) => write!(f, r#""{}""#, s),
            Lit::Bool(b) => {
                if *b {
                    write!(f, "True")
                } else {
                    write!(f, "False")
                }
            }
            Lit::Nothing => write!(f, "Nothing"),
            Lit::Empty => write!(f, "Empty"),
            Lit::Null => write!(f, "Null"),
        }
    }
}

impl Lit {
    pub fn str(s: impl Into<String>) -> Self {
        Lit::Str(s.into())
    }

    pub fn int(i: isize) -> Self {
        Lit::Int(i)
    }
}
