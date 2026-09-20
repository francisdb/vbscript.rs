use crate::lexer::{Span, TokenKind};
use std::fmt;
use std::fmt::{Debug, Display};
use std::ops::Deref;

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

/// A node of the syntax tree together with the part of the source it was parsed from.
///
/// The span is where a node came from, it is not part of what the node means. That is why
/// it is ignored when comparing nodes and left out of the `Debug` output: two trees parsed
/// from differently formatted sources are equal if they have the same structure, and a
/// tree that is built by hand, with [`Span::default`] everywhere, is equal to the parsed
/// one. Compare the `span` fields if the position matters.
#[derive(Clone)]
pub struct Spanned<T> {
    pub node: T,
    /// Byte range in the source, empty for nodes that were not parsed from a source.
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn with_span(node: T, span: Span) -> Self {
        Spanned { node, span }
    }
}

/// A node that was not parsed from a source, it has an empty span.
impl<T> From<T> for Spanned<T> {
    fn from(node: T) -> Self {
        Spanned {
            node,
            span: Span::default(),
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.fmt(f)
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.fmt(f)
    }
}

/// A name as it is written in the source, with the span of just that name.
///
/// It dereferences to the `String` and compares with a `&str`, the span is there to point
/// at the name: the one of a statement like a `Sub` covers all of it.
pub type Name = Spanned<String>;

impl From<&str> for Name {
    fn from(name: &str) -> Self {
        Spanned::from(name.to_string())
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        self.node == other
    }
}

impl PartialEq<&str> for Name {
    fn eq(&self, other: &&str) -> bool {
        self.node == *other
    }
}

/// An expression, see [`ExprKind`].
pub type Expr = Spanned<ExprKind>;

/// A statement, see [`StmtKind`].
pub type Stmt = Spanned<StmtKind>;

/// A top-level item of a script, see [`ItemKind`].
pub type Item = Spanned<ItemKind>;

/// An identifier with optional property accesses
/// eg `a.b.c`, `a.b(1).c`, `a.b(1)(2).c` or `a.b(1,2).c(3)`
///
/// Contains a restricted expression that does not allow all operators.
#[derive(Debug, Clone, PartialEq)]
pub struct FullIdent(pub Box<Expr>);

impl FullIdent {
    pub fn ident(name: impl Into<String>) -> Self {
        FullIdent(Box::new(Expr::ident(name)))
    }

    pub fn new(expr: Expr) -> Self {
        FullIdent(Box::new(expr))
    }
}

impl Display for FullIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Lit),
    /// The span of the name is the one of the expression.
    Ident(Name),
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
    New(Name),
    FnApplication {
        callee: Box<Expr>,
        args: Vec<Option<Expr>>,
    },
    WithScoped,
    /// An expression in parentheses: `(a + b)`.
    ///
    /// The parentheses are more than grouping when they are around the single argument of
    /// a sub call: `Foo (x)` passes `x` by value where `Foo x` passes it by reference.
    Paren(Box<Expr>),
    MemberExpression {
        base: Box<Expr>,
        property: Name,
    },
}

impl Expr {
    pub fn ident(name: impl Into<String>) -> Self {
        ExprKind::Ident(Name::from(name.into())).into()
    }

    pub fn int(i: i32) -> Self {
        ExprKind::Literal(Lit::Int(i.to_string())).into()
    }

    pub fn int_str(i: impl Into<String>) -> Self {
        ExprKind::Literal(Lit::Int(i.into())).into()
    }

    pub fn bool(b: bool) -> Self {
        ExprKind::Literal(Lit::Bool(b)).into()
    }

    pub fn str(s: impl Into<String>) -> Self {
        ExprKind::Literal(Lit::Str(s.into())).into()
    }

    pub fn new(name: impl Into<String>) -> Self {
        ExprKind::New(Name::from(name.into())).into()
    }

    pub fn paren(expr: Expr) -> Self {
        ExprKind::Paren(Box::new(expr)).into()
    }

    pub fn member(base: Expr, property: impl Into<String>) -> Self {
        ExprKind::MemberExpression {
            base: Box::new(base),
            property: Name::from(property.into()),
        }
        .into()
    }

    pub fn fn_application(callee: Expr, args: Vec<Expr>) -> Self {
        let args = args.into_iter().map(Some).collect();
        ExprKind::FnApplication {
            callee: Box::new(callee),
            args,
        }
        .into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    /// Integral literal, can be negative, can be very large and at runtime seen as Double with loss of precision
    Int(String),
    /// Floating point literal, can be negative, at runtime seen as Float or Double
    Float(f64),
    Str(String),
    Bool(bool),
    DateTime(String),
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
    pub fn expr(expr: Expr) -> Self {
        SetRhs::Expr(Box::new(expr))
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
pub enum StmtKind {
    Dim {
        vars: Vec<VarDecl>,
    },
    ReDim {
        preserve: bool,
        vars: Vec<ReDimVar>,
    },
    Const(Vec<(Name, Lit)>),
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
        counter: Name,
        start: Box<Expr>,
        end: Box<Expr>,
        step: Option<Box<Expr>>,
        body: Vec<Stmt>,
    },
    ForEachStmt {
        element: Name,
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
        name: Name,
        parameters: Vec<Argument>,
        body: Vec<Stmt>,
    },
    // https://learn.microsoft.com/en-us/previous-versions//x7hbf8fa(v=vs.85)
    Function {
        visibility: Visibility,
        name: Name,
        parameters: Vec<Argument>,
        body: Vec<Stmt>,
    },
    /// Suspends execution when a debugger is attached, does nothing otherwise.
    Stop,
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
    ByVal(Name),
    ByRef(Name),
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
    pub name: Name,
    pub property_type: PropertyType,
    pub args: Vec<(Name, ArgumentType)>,
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
    pub properties: Vec<VarDecl>,
}

/// An array that is given new bounds by a `ReDim` statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ReDimVar {
    pub name: Name,
    /// The new upper bounds, at least one. Unlike the ones of a `Dim` these are expressions.
    pub bounds: Vec<Expr>,
}

/// A variable that is declared with `Dim`, in a class or, with a visibility, at script
/// level.
#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: Name,
    /// `None` for a plain variable like `a`, the constant upper bounds for an array:
    /// `a(1, 2)`, which are empty for a dynamic array `a()`.
    pub bounds: Option<Vec<usize>>,
}

impl VarDecl {
    /// A plain variable.
    pub fn new(name: impl Into<String>) -> Self {
        VarDecl {
            name: Name::from(name.into()),
            bounds: None,
        }
    }

    /// An array, dynamic if there are no bounds.
    pub fn array(name: impl Into<String>, bounds: Vec<usize>) -> Self {
        VarDecl {
            name: Name::from(name.into()),
            bounds: Some(bounds),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    // https://learn.microsoft.com/en-us/previous-versions/windows/internet-explorer/ie-developer/scripting-articles/bw9t3484%28v%3Dvs.84%29
    OptionExplicit,
    // https://learn.microsoft.com/en-us/previous-versions//4ah5852c(v=vs.85)
    Class {
        name: Name,
        members: Vec<MemberDefinitions>,
        /// The variables of each `Dim` in the class
        dims: Vec<Vec<VarDecl>>,
        member_accessors: Vec<MemberAccess>,
        methods: Vec<Stmt>, // expect only functions and subs
    },
    /// This is a script-level const that has visibility
    /// Consts in procedures are handled by Stmt::Const
    Const {
        visibility: Visibility,
        values: Vec<(Name, Lit)>,
    },
    /// This is a script-level variable that has visibility
    /// e.g. `Public a, b, c` or `Private a, b, c`
    /// note: `Public a()` is a dynamic array and not the same as `Public a`
    /// <https://stackoverflow.com/a/23911728/42198>
    Variable {
        visibility: Visibility,
        vars: Vec<VarDecl>,
    },
    Statement(Stmt),
}

impl Stmt {
    pub fn dim(var_name: impl Into<String>) -> Self {
        StmtKind::Dim {
            vars: vec![VarDecl::new(var_name)],
        }
        .into()
    }

    pub fn const_(var_name: impl Into<String>, value: Lit) -> Self {
        StmtKind::Const(vec![(Name::from(var_name.into()), value)]).into()
    }

    pub fn assignment(ident: FullIdent, value: Expr) -> Self {
        StmtKind::Assignment {
            full_ident: ident,
            value: Box::new(value),
        }
        .into()
    }
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprKind::Literal(lit) => write!(f, "{lit}"),
            ExprKind::Ident(ident) => write!(f, "{ident}"),
            ExprKind::WithScoped => write!(f, "."),
            ExprKind::PrefixOp { op, expr } => write!(f, "({op} {expr})"),
            ExprKind::InfixOp { op, lhs, rhs } => write!(f, "({lhs} {op} {rhs})"),
            // ExprKind::PostfixOp { op, expr } =>
            //     write!(f, "({} {})", expr, op),
            ExprKind::New(name) => write!(f, "New {name}"),
            ExprKind::FnApplication { callee, args } => {
                write!(f, "{callee}(")?;
                let len = args.len();
                for (i, arg) in args.iter().enumerate() {
                    if let Some(arg) = arg {
                        write!(f, "{arg}")?;
                    }
                    if i != len - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            // prefix and infix operators are already written with parentheses
            ExprKind::Paren(expr)
                if matches!(
                    expr.node,
                    ExprKind::PrefixOp { .. } | ExprKind::InfixOp { .. }
                ) =>
            {
                write!(f, "{expr}")
            }
            ExprKind::Paren(expr) => write!(f, "({expr})"),
            ExprKind::MemberExpression { base, property } => write!(f, "{base}.{property}"),
        }
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{i}"),
            Lit::Float(fl) => write!(f, "{fl}"),
            Lit::Str(s) => write!(f, r#""{}""#, s.replace('"', "\"\"")),
            Lit::DateTime(dt) => write!(f, "#{dt}#"),
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

    pub fn int(i: i32) -> Self {
        Lit::Int(i.to_string())
    }
}
