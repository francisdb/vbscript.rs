use crate::lexer::{Token, TokenKind};
use crate::parser::ast::{
    Argument, ArgumentType, Case, DoLoopCheck, DoLoopCondition, ErrorClause, Expr, FullIdent,
    IdentBase, IdentPart, Item, MemberAccess, MemberDefinitions, PropertyType, PropertyVisibility,
    SetRhs, Stmt, Visibility,
};
use crate::parser::{ast, Parser};
use crate::T;

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub fn file(&mut self) -> Vec<ast::Item> {
        let mut items = Vec::new();
        while !self.at(T![EOF]) {
            let item = self.item();
            items.push(item);
        }
        items
    }

    pub fn item(&mut self) -> ast::Item {
        match self.peek() {
            T![option] => self.item_option(),
            vis @ T![public] | vis @ T![private] => {
                let visibility = match self.consume(vis).kind {
                    T![public] => Visibility::Public,
                    T![private] => Visibility::Private,
                    _ => unreachable!(),
                };

                match self.peek() {
                    T![function] => {
                        let item = Item::Statement(self.statement_function(visibility));
                        self.consume_line_delimiter();
                        item
                    }
                    T![sub] => {
                        let item = Item::Statement(self.statement_sub(visibility));
                        self.consume_line_delimiter();
                        item
                    }
                    T![const] => self.item_const(visibility),
                    T![ident] => self.item_variable(visibility),
                    _ => {
                        let peek = self.peek_full();
                        panic!(
                            "Expected `function`, `sub` or `const` after visibility at line {}, column {} but found `{}`",
                            peek.line, peek.column, peek.kind
                        )
                    }
                }
            }
            T![class] => self.item_class(),
            T![const] => self.item_const(Visibility::Public),
            _ => {
                // this must be a statement
                let stmt = self.statement(true);
                Item::Statement(stmt)
            }
        }
    }

    fn item_const(&mut self, visibility: Visibility) -> Item {
        self.consume(T![const]);
        let mut values = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let name = self.identifier("const name");
            self.consume(T![=]);
            let literal = self.parse_const_literal();
            values.push((name, literal));
            if self.at(T![,]) {
                self.consume(T![,]);
            } else {
                break;
            }
        }
        self.consume_line_delimiter();
        Item::Const { visibility, values }
    }

    fn item_variable(&mut self, visibility: Visibility) -> Item {
        let mut vars = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let ident = self.consume(T![ident]);
            let name = self.text(&ident).to_string();
            let bounds = self.const_bounds();
            vars.push((name, bounds));
            if self.at(T![,]) {
                self.consume(T![,]);
            } else {
                break;
            }
        }
        self.consume_line_delimiter();
        Item::Variable { visibility, vars }
    }

    fn item_class(&mut self) -> Item {
        self.consume(T![class]);
        let ident = self.consume(T![ident]);
        let name = self.text(&ident).to_string();
        self.consume_line_delimiter();
        let mut members = Vec::new();
        let mut member_accessors = Vec::new();
        let mut methods = Vec::new();
        let mut dims = Vec::new();
        while !self.at(T![end]) {
            let mut default = None;
            let visibility = if self.at(T![public]) {
                self.consume(T![public]);
                if self.at(T![default]) {
                    self.consume(T![default]);
                    default = Some(true);
                }
                Visibility::Public
            } else if self.at(T![private]) {
                self.consume(T![private]);
                Visibility::Private
            } else {
                Visibility::Default
            };

            match self.peek() {
                T![property] => member_accessors.push(self.class_property(default, visibility)),
                T![function] => methods.push(self.class_function(visibility)),
                T![sub] => methods.push(self.class_sub(visibility)),
                T![dim] => {
                    // visibility can not be set for dims
                    if visibility != Visibility::Default {
                        let peek = self.peek_full();
                        panic!(
                            "Class dim does not support visibility at line {}, column {} but found '{}'",
                            peek.line, peek.column, peek.kind
                        )
                    }
                    dims.push(self.class_dim());
                }
                _ => {
                    members.push(self.class_member(visibility));
                }
            }
        }
        self.consume(T![end]);
        self.consume(T![class]);
        self.consume_if_not_eof(T![nl]);
        Item::Class {
            name,
            members,
            dims,
            member_accessors,
            methods,
        }
    }

    fn class_member(&mut self, visibility: Visibility) -> MemberDefinitions {
        // properties
        if visibility == Visibility::Default {
            let peek = self.peek_full();
            panic!(
                "Expected visibility for class member at line {}, column {} but found '{}'",
                peek.line, peek.column, peek.kind
            )
        };
        // like a dim we can have multiple properties in one line of which some can be arrays
        let mut properties = Vec::new();
        while {
            let name = self.identifier("class member");
            let bounds = self.const_bounds();
            properties.push((name, bounds));
            self.at(T![,])
        } {
            self.consume(T![,]);
        }
        let member_definitions = MemberDefinitions {
            visibility,
            properties,
        };
        self.consume_line_delimiter();
        member_definitions
    }

    fn class_dim(&mut self) -> Vec<(String, Option<Vec<usize>>)> {
        self.consume(T![dim]);
        let mut vars = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let ident = self.consume(T![ident]);
            let name = self.text(&ident).to_string();
            let bounds = self.const_bounds();
            vars.push((name, bounds));
            if self.at(T![,]) {
                self.consume(T![,]);
            } else {
                break;
            }
        }
        self.consume_line_delimiter();
        vars
    }

    fn class_sub(&mut self, visibility: Visibility) -> Stmt {
        self.consume(T![sub]);
        let ident = self.consume(T![ident]);
        let method_name = self.text(&ident).to_string();
        let parameters = self.optional_declaration_parameter_list("Sub");
        self.consume_line_delimiter();
        let body = self.block(true, &[T![end]]);
        self.consume(T![end]);
        self.consume(T![sub]);
        self.consume_line_delimiter();
        Stmt::Sub {
            visibility,
            name: method_name.clone(),
            parameters,
            body,
        }
    }

    fn class_function(&mut self, visibility: Visibility) -> Stmt {
        self.consume(T![function]);
        let ident = self.consume(T![ident]);
        let method_name = self.text(&ident).to_string();
        let parameters = self.optional_declaration_parameter_list("Function");
        self.consume_line_delimiter();
        let body = self.block(true, &[T![end]]);
        self.consume(T![end]);
        self.consume(T![function]);
        self.consume_if_not_eof(T![nl]);
        Stmt::Function {
            visibility,
            name: method_name.clone(),
            parameters,
            body,
        }
    }

    fn class_property(&mut self, default: Option<bool>, visibility: Visibility) -> MemberAccess {
        let property_visibility = match visibility {
            Visibility::Public => PropertyVisibility::Public {
                default: default.unwrap_or(false),
            },
            Visibility::Private => PropertyVisibility::Private,
            Visibility::Default => PropertyVisibility::Public {
                default: default.unwrap_or(false),
            },
        };

        self.consume(T![property]);
        // let (Variant), get or set (Object)
        let property_type = match self.peek() {
            T![let] => {
                self.consume(T![let]);
                PropertyType::Let
            }
            T![set] => {
                self.consume(T![set]);
                PropertyType::Set
            }
            T![get] => {
                self.consume(T![get]);
                PropertyType::Get
            }
            other => {
                let peek = self.peek_full();
                panic!(
                    "Expected `let`, `set` or `get` in class property definition at line {}, column {}, got `{}`",
                    peek.line, peek.column, other
                );
            }
        };

        let ident = self.consume(T![ident]);
        let name = self.text(&ident).to_string();
        let property_arguments = self.optional_parenthesized_property_arguments();

        let property_body = self.block(true, &[T![end]]);
        self.consume(T![end]);
        self.consume(T![property]);
        self.consume_line_delimiter();
        MemberAccess {
            visibility: property_visibility,
            name,
            property_type,
            args: property_arguments,
            body: property_body,
        }
    }

    fn item_option(&mut self) -> Item {
        self.consume(T![option]);
        let explicit = self.next().expect("Expected identifier after `option`");
        match explicit.kind {
            T![ident] => {
                assert_eq!(
                    self.text(&explicit).to_ascii_lowercase(),
                    "explicit",
                    "Expected `explicit` after `option`"
                );
            }
            _ => panic!("Expected `explicit` after `option`"),
        }
        self.consume_if_not_eof(T![nl]);
        Item::OptionExplicit
    }

    fn statement_sub(&mut self, visibility: Visibility) -> Stmt {
        self.consume(T![sub]);

        let ident = self
            .next()
            .expect("Tried to parse sub name, but there were no more tokens");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier as sub name, but found `{}`",
            ident.kind
        );
        let name = self.text(&ident).to_string();
        let parameters = self.optional_declaration_parameter_list("Sub");
        self.consume_line_delimiter();
        let body = self.block(true, &[T![end]]);

        self.consume(T![end]);
        self.consume(T![sub]);

        Stmt::Sub {
            visibility,
            name,
            parameters,
            body,
        }
    }

    fn statement_function(&mut self, visibility: Visibility) -> Stmt {
        self.consume(T![function]);

        let ident = self
            .next()
            .expect("Tried to parse function name, but there were no more tokens");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier as function name, but found `{}`",
            ident.kind
        );
        let name = self.text(&ident).to_string();

        let parameters = self.optional_declaration_parameter_list("Function");

        self.consume_line_delimiter();
        // do we need to do something special with the returned value?
        let body = self.block(true, &[T![end]]);

        self.consume(T![end]);
        self.consume(T![function]);

        Stmt::Function {
            visibility,
            name,
            parameters,
            body,
        }
    }

    fn const_bounds(&mut self) -> Option<Vec<usize>> {
        let mut bounds = None;
        if self.at(T!['(']) {
            self.consume(T!['(']);
            bounds = Some(vec![]);
            while !self.at(T![')']) {
                let dim = self.consume(T![integer_literal]);
                let dim: usize = match self.text(&dim).parse() {
                    Ok(dim) => dim,
                    Err(_) => panic!(
                        "Expected integer literal as bound at line {}, row {}",
                        dim.line, dim.column
                    ),
                };
                bounds.as_mut().unwrap().push(dim);
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        }
        bounds
    }

    /// Parse a list of parameters for a function or sub declaration.
    fn optional_declaration_parameter_list(&mut self, item_type: &str) -> Vec<Argument> {
        let mut parameters: Vec<Argument> = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                // optional modifier
                let modifier = if self.at(T![byval]) {
                    self.consume(T![byval]);
                    Argument::ByVal
                } else if self.at(T![byref]) {
                    self.consume(T![byref]);
                    Argument::ByRef
                } else {
                    Argument::ByVal
                };

                let parameter_name = self.identifier(item_type);
                parameters.push(modifier(parameter_name));
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        }
        parameters
    }

    pub(crate) fn identifier(&mut self, item_type: &str) -> String {
        let ident = self.next().unwrap_or_else(|| {
            let peek_full = self.peek_full();
            panic!(
                "{}:{} Tried to parse {}, but there were no more tokens",
                peek_full.line, peek_full.column, item_type
            )
        });
        let name = match ident.kind {
            // We might have to add more here.
            // For now we have only encountered keywords `property`, `option` and `stop`
            // Probably the `unused` keyword will also be added here
            T![ident] | T![property] | T![stop] | T![option] | T![step] | T![default] | T![set] => {
                self.text(&ident).to_string()
            }
            _ => panic!(
                "{}:{} Expected identifier as {}, but found `{}`",
                ident.line, ident.column, item_type, ident.kind
            ),
        };
        name
    }

    /// Parse a block of statements until we reach an `end` token.
    pub fn block(&mut self, multi_line: bool, end_tokens: &[TokenKind]) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !end_tokens.contains(&self.peek()) {
            if !self.at(T![nl]) && !self.at(T![:]) {
                let stmt = self.statement(false);
                stmts.push(stmt);
            }
            if end_tokens.contains(&self.peek()) {
                break;
            } else if self.at(T![nl]) {
                if multi_line {
                    self.consume(T![nl]);
                } else {
                    break;
                }
            } else if self.at(T![:]) {
                self.consume(T![:]);
            }
        }
        stmts
    }

    pub fn statement(&mut self, consume_delimiter: bool) -> Stmt {
        let stmt = match self.peek() {
            T![dim] => self.statement_dim(),
            T![redim] => self.statement_redim(),
            T![const] => self.statement_const(),
            T![set] => self.statement_set(),
            T![if] => self.statement_if(),
            T![while] => self.statement_while(),
            T![for] => self.statement_for(),
            T![do] => self.statement_do(),
            T![select] => self.statement_select(),
            T![on] => self.statement_on(),
            T![exit] => self.statement_exit(),
            T![with] => self.statement_with(),
            T![call] => self.statement_call(),
            T![sub] => self.statement_sub(Visibility::Default),
            T![function] => self.statement_function(Visibility::Default),
            T![private] | T![public] => {
                let visibility = match self.peek() {
                    T![public] => {
                        self.consume(T![public]);
                        Visibility::Public
                    }
                    T![private] => {
                        self.consume(T![private]);
                        Visibility::Private
                    }
                    _ => unreachable!(),
                };
                match self.peek() {
                    T![sub] => self.statement_sub(visibility),
                    T![function] => self.statement_function(visibility),
                    _ => {
                        let full = self.peek_full();
                        panic!(
                            "{}:{} Expected `sub` or `function` after visibility, but found `{}`",
                            full.line, full.column, full.kind
                        )
                    }
                }
            }
            // property, stop, option, step was added here because it can also be used as identifier
            // TODO find a better way to handle this without copy pasting
            //   see `identifier()`
            T![ident] | T![me] | T![.] | T![property] | T![stop] | T![option] | T![step] => {
                // multiple options here
                // 1. assignment
                // 2. sub call without args
                // 3. sub call with args
                //    there is some ambiguity here, because a sub call can argument might contain
                //    parentheses, which are also used for array access
                // 4. function call discarding the return value
                //    In this case the function call is treated as a sub call and should not have parentheses around the arguments,
                //    or you will get a 'compilation error: Cannot use parentheses when calling a Sub'.
                // (also calling a function, sub or property on an object that might be located in an array)
                //
                // Invalid:
                // 1. array access (fails at runtime with 'Type mismatch')
                let ident = self.ident_deep();
                if self.at(T![=]) {
                    // assignment
                    self.consume(T![=]);
                    let value = self.expression();
                    Stmt::Assignment {
                        full_ident: ident,
                        value: Box::new(value),
                    }
                } else if self.at_new_line_or_eof() {
                    // sub call without args
                    self.fail_if_using_parentheses_when_calling_sub(&ident);
                    Stmt::SubCall {
                        fn_name: ident,
                        args: Vec::new(),
                    }
                } else {
                    // sub call with args
                    self.fail_if_using_parentheses_when_calling_sub(&ident);

                    // TODO this whole undoing of first parsing too much is a bit of a hack
                    //   we should probably try to find a better way to handle this

                    let (patched_ident, part_of_expression) = Self::fix_sub_ident(ident);
                    let args = self.sub_arguments(part_of_expression);
                    Stmt::SubCall {
                        fn_name: patched_ident,
                        args,
                    }
                }
            }
            unexpected => {
                let full = self.peek_full();
                panic!(
                    "{}:{} Unexpected token: {}",
                    full.line, full.column, unexpected,
                );
            }
        };
        if consume_delimiter {
            self.consume_line_delimiter();
        }
        stmt
    }

    /// When calling a Sub
    /// If the last part of the ident has any array indices
    /// we need to re-evaluate them as part of the arguments
    /// because they are part of the argument expression.
    fn fix_sub_ident(ident: FullIdent) -> (FullIdent, Option<Expr>) {
        let mut part_of_expression: Option<Expr> = None;
        let mut last_access = ident;

        if last_access.property_accesses.is_empty() {
            // if there are no property accesses, we can just use the base
            let base_indices = last_access.base.array_indices();
            if !base_indices.is_empty() {
                if let Some(indices) = base_indices.last() {
                    part_of_expression = indices.last().and_then(|y| y.clone())
                }
                let mut base_copy = last_access.base.clone();
                base_copy.set_array_indices(
                    base_copy
                        .array_indices()
                        .iter()
                        .take(base_indices.len() - 1)
                        .cloned()
                        .collect(),
                );
                last_access = FullIdent {
                    base: base_copy,
                    property_accesses: vec![],
                };
            };
        } else {
            while !last_access.property_accesses.is_empty() {
                let last = last_access.property_accesses.last().unwrap();
                if last.array_indices.len() == 1 {
                    let last = last.clone();
                    // from a previous check we know that there is only one array index
                    part_of_expression
                        .clone_from(last.array_indices.first().unwrap().clone().first().unwrap());

                    last_access = FullIdent {
                        base: last_access.base,
                        property_accesses: last_access
                            .property_accesses
                            .iter()
                            .take(last_access.property_accesses.len() - 1)
                            .cloned()
                            .collect(),
                    };
                } else {
                    break;
                }
            }
        }
        let patched_ident = last_access;
        (patched_ident, part_of_expression)
    }

    /// A sub call statement like `something(1,2)` or `SomeArray(1).SomeSub(1,2,3)` is not valid
    /// However a sub call statement like `something(2)` is valid as the `(2)` is considered the first argument
    /// On windows you get a 'compilation error: Cannot use parentheses when calling a Sub'
    fn fail_if_using_parentheses_when_calling_sub(&mut self, ident: &FullIdent) {
        let last_part_indices = ident
            .property_accesses
            .last()
            .map(|i| &i.array_indices)
            .unwrap_or_else(|| ident.base.array_indices());
        if !last_part_indices.is_empty() && last_part_indices.first().unwrap().len() > 1 {
            // array access
            let full = self.peek_full();
            panic!(
                "{}:{} compilation error: Cannot use parentheses when calling a Sub",
                full.line, full.column
            );
        }
    }

    fn statement_call(&mut self) -> Stmt {
        self.consume(T![call]);
        let ident = self.ident_deep();
        // TODO if there are no args the parens should be omitted
        //   to validate on windows
        Stmt::Call(ident)
    }

    fn sub_arguments(&mut self, mut first_expression_part: Option<Expr>) -> Vec<Option<Expr>> {
        let mut args = Vec::new();
        // TODO first_expression_part might be ignored here!
        // TODO we should be smarter here instead of having all these end conditions
        while !self.at(T![:])
            && !self.at(T![nl])
            && !self.at(T![EOF])
            && !self.at(T![else])
            && !self.at(T![end])
        {
            // Empty arguments are allowed, however working with them is tricky.
            // For now I only found `TypeName(arg) = "Error"` to detect a missing argument.
            if self.at(T![,]) && first_expression_part.is_none() {
                self.consume(T![,]);
                args.push(None);
                continue;
            }
            let arg = self.expression_with_prefix(first_expression_part);
            // consume it only once
            first_expression_part = None;
            args.push(Some(arg));
            if self.at(T![,]) {
                self.consume(T![,]);
            } else {
                break;
            }
        }
        args
    }

    fn statement_dim(&mut self) -> Stmt {
        self.consume(T![dim]);
        let mut vars = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let name = self.identifier("variable name");
            let bounds = self.parenthesized_arguments();
            vars.push((name, bounds));
            if self.at(T![,]) {
                self.consume(T![,]);
            } else {
                break;
            }
        }
        Stmt::Dim { vars }
    }

    fn statement_redim(&mut self) -> Stmt {
        self.consume(T![redim]);
        let mut preserve = false;
        if self.at(T![preserve]) {
            self.consume(T![preserve]);
            preserve = true;
        }
        let mut var_bounds = Vec::new();
        let first_var = self.consume(T![ident]);
        let var_name = self.text(&first_var).to_string();
        let bounds = self.parenthesized_arguments();
        var_bounds.push((var_name, bounds));
        while self.at(T![,]) {
            self.consume(T![,]);
            let ident = self.consume(T![ident]);
            let name = self.text(&ident).to_string();
            let bounds = self.parenthesized_arguments();
            var_bounds.push((name, bounds));
        }

        Stmt::ReDim {
            preserve,
            var_bounds,
        }
    }

    fn statement_const(&mut self) -> Stmt {
        self.consume(T![const]);

        // multiple constants can be defined in one line
        let mut constants = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let ident = self.consume(T![ident]);
            let name = self.text(&ident).to_string();
            self.consume(T![=]);
            let literal = self.parse_const_literal();
            constants.push((name, literal));
            if self.at(T![,]) {
                self.consume(T![,]);
            } else {
                break;
            }
        }
        Stmt::Const(constants)
    }

    fn statement_with(&mut self) -> Stmt {
        self.consume(T![with]);
        let object = self.ident_deep();
        self.consume_line_delimiter();
        let body = self.block(true, &[T![end]]);
        self.consume(T![end]);
        self.consume(T![with]);
        Stmt::With { object, body }
    }

    fn statement_exit(&mut self) -> Stmt {
        self.consume(T![exit]);
        match self.peek() {
            T![do] => {
                self.consume(T![do]);
                Stmt::ExitDo
            }
            T![for] => {
                self.consume(T![for]);
                Stmt::ExitFor
            }
            T![function] => {
                self.consume(T![function]);
                Stmt::ExitFunction
            }
            T![property] => {
                self.consume(T![property]);
                Stmt::ExitProperty
            }
            T![sub] => {
                self.consume(T![sub]);
                Stmt::ExitSub
            }
            other => panic!("Exit not supported for {}", other),
        }
    }

    fn statement_on(&mut self) -> Stmt {
        // error handling
        self.consume(T![on]);
        self.consume(T![error]);
        let error_clause = if self.at(T![resume]) {
            self.consume(T![resume]);
            self.consume(T![next]);
            ErrorClause::ResumeNext
        } else if self.at(T![goto]) {
            self.consume(T![goto]);
            let token = self.consume(T![integer_literal]);
            let number: usize = self
                .text(&token)
                .parse()
                .expect("Expected integer after `goto`");
            if number != 0 {
                panic!("Expected `goto 0` after `on error`")
            }
            ErrorClause::Goto0
        } else {
            panic!("Expected `resume next` or `goto 0` after `on error`")
        };
        Stmt::OnError { error_clause }
    }

    fn statement_select(&mut self) -> Stmt {
        self.consume(T![select]);
        self.consume(T![case]);
        let expr = self.expression();
        self.consume_line_delimiter();
        let mut cases: Vec<Case> = Vec::new();
        let mut else_stmt = None;
        while !self.at(T![end]) {
            if else_stmt.is_some() {
                panic!("`else` statement must be last in `select case` block")
            }
            self.consume(T![case]);
            if self.at(T![else]) {
                self.consume(T![else]);
                self.consume_optional_line_delimiter();
                let block = self.block(true, &[T![end], T![case]]);
                else_stmt = Some(block);
            } else {
                let mut tests = Vec::new();
                // comma separated list of expressions
                let first_expr = self.expression();
                tests.push(first_expr);
                while self.at(T![,]) {
                    self.consume(T![,]);
                    let expr = self.expression();
                    tests.push(expr);
                }
                self.consume_optional_line_delimiter();
                let body = self.block(true, &[T![end], T![case]]);
                cases.push(Case { tests, body });
            }
        }
        self.consume(T![end]);
        self.consume(T![select]);
        Stmt::SelectCase {
            test_expr: Box::new(expr),
            cases,
            else_stmt,
        }
    }

    fn statement_for(&mut self) -> Stmt {
        self.consume(T![for]);

        if self.at(T![each]) {
            self.consume(T![each]);
            let element = self.next().unwrap();
            let element_name = self.text(&element).to_string();
            self.consume(T![in]);
            let group = Box::new(self.expression());
            self.consume_line_delimiter();

            let body = self.block(true, &[T![next]]);

            self.consume(T![next]);

            Stmt::ForEachStmt {
                element: element_name,
                group,
                body,
            }
        } else {
            let counter = self.next().unwrap();
            let counter_name = self.text(&counter).to_string();
            self.consume(T![=]);
            let start = self.expression();
            self.consume(T![to]);
            let end = self.expression();
            let step = if self.at(T![step]) {
                self.consume(T![step]);
                Some(Box::new(self.expression()))
            } else {
                None
            };
            self.consume_line_delimiter();

            let body = self.block(true, &[T![next]]);

            self.consume(T![next]);

            Stmt::ForStmt {
                counter: counter_name,
                start: Box::new(start),
                end: Box::new(end),
                step,
                body,
            }
        }
    }

    fn statement_do(&mut self) -> Stmt {
        self.consume(T![do]);
        let mut check = DoLoopCheck::None;

        if self.at(T![while]) {
            self.consume(T![while]);
            check = DoLoopCheck::Pre(DoLoopCondition::While(Box::new(self.expression())))
        } else if self.at(T![until]) {
            self.consume(T![until]);
            check = DoLoopCheck::Pre(DoLoopCondition::Until(Box::new(self.expression())))
        }

        let body = self.block(true, &[T![loop]]);
        self.consume(T![loop]);

        if self.at(T![while]) {
            self.consume(T![while]);
            check = DoLoopCheck::Post(DoLoopCondition::While(Box::new(self.expression())))
        } else if self.at(T![until]) {
            self.consume(T![until]);
            check = DoLoopCheck::Post(DoLoopCondition::Until(Box::new(self.expression())))
        }

        Stmt::DoLoop { check, body }
    }

    fn statement_while(&mut self) -> Stmt {
        self.consume(T![while]);
        let condition = self.expression();
        self.consume(T![nl]);

        let body = self.block(true, &[T![wend]]);

        self.consume(T![wend]);

        Stmt::WhileStmt {
            condition: Box::new(condition),
            body,
        }
    }

    fn statement_set(&mut self) -> Stmt {
        self.consume(T![set]);
        let var = self.ident_deep();
        self.consume(T![=]);
        let rhs = match self.peek() {
            T![nothing] => {
                self.consume(T![nothing]);
                SetRhs::Nothing
            }
            _ => {
                let expr = self.expression();
                SetRhs::Expr(Box::new(expr))
            }
        };
        Stmt::Set { var, rhs }
    }

    fn statement_if(&mut self) -> Stmt {
        self.consume(T![if]);

        let condition = self.expression();

        self.consume(T![then]);

        // if we have a newline, it's a block if statement
        if self.at(T![nl]) {
            self.consume(T![nl]);
            let body = self.block(true, &[T![end], T![else], T![elseif]]);
            let mut elseif_statements = Vec::new();
            while self.at(T![elseif]) {
                self.consume(T![elseif]);
                let condition = self.expression();
                self.consume(T![then]);
                self.consume_optional_line_delimiter();
                let block = self.block(true, &[T![end], T![else], T![elseif]]);
                elseif_statements.push((Box::new(condition), block));
            }
            let else_stmt = if self.at(T![else]) {
                self.consume(T![else]);
                self.consume_optional_line_delimiter();
                Some(self.block(true, &[T![end]]))
            } else {
                None
            };
            self.consume(T![end]);
            self.consume(T![if]);
            Stmt::IfStmt {
                condition: Box::new(condition),
                body,
                elseif_statements,
                else_stmt,
            }
        } else {
            // single line if statement
            // can contain multiple statements if separated by colons
            let body = self.block(false, &[T![else], T![elseif], T![end], T![EOF]]);
            let mut elseif_statements = Vec::new();
            while self.at(T![elseif]) {
                self.consume(T![elseif]);
                let condition = self.expression();
                self.consume(T![then]);
                let elseif_body = self.block(false, &[T![else], T![elseif], T![end], T![EOF]]);
                elseif_statements.push((Box::new(condition), elseif_body));
            }
            let else_stmt = if self.at(T![else]) {
                self.consume(T![else]);
                let else_body = self.block(false, &[T![end], T![EOF]]);
                Some(else_body)
            } else {
                None
            };

            // optional "End If" if we still have not encountered a newline
            if self.at(T![end]) {
                self.consume(T![end]);
                self.consume(T![if]);
            }

            Stmt::IfStmt {
                condition: Box::new(condition),
                body,
                elseif_statements,
                else_stmt,
            }
        }
    }

    fn multi_parenthesized_arguments(&mut self) -> Vec<Vec<Option<Expr>>> {
        let mut arguments = Vec::new();
        while self.at(T!['(']) {
            let group = self.parenthesized_optional_arguments();
            arguments.push(group);
        }
        arguments
    }

    fn optional_parenthesized_property_arguments(&mut self) -> Vec<(String, ArgumentType)> {
        let mut property_arguments = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['(']);
            while !self.at(T![')']) {
                // modifiers for the property
                let argument_type = if self.at(T![byval]) {
                    self.consume(T![byval]);
                    ArgumentType::ByVal
                } else if self.at(T![byref]) {
                    self.consume(T![byref]);
                    ArgumentType::ByRef
                } else {
                    ArgumentType::ByVal
                };
                let ident = self.consume(T![ident]);
                let arg_name = self.text(&ident).to_string();
                property_arguments.push((arg_name, argument_type));
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![')']);
        }
        property_arguments
    }

    // TODO git rid of this stuff, the new FnCall should be able to handle this
    pub(crate) fn ident_part(&mut self) -> IdentPart {
        // example input: `foo` or `foo(1)` or `foo(1, 2)`
        let name = self.identifier("identifier");

        let array_indices = self.multi_parenthesized_arguments();
        IdentPart {
            name,
            array_indices,
        }
    }

    /// Parse a deep identifier, which can contain multiple array and property accesses.
    /// example input: `foo(x + 1).bar.baz(2,3).name`
    pub(crate) fn ident_deep(&mut self) -> FullIdent {
        let base = if self.at(T![.]) {
            self.consume(T![.]);
            IdentBase::Partial(self.ident_part())
        } else if self.at(T![me]) {
            self.consume(T![me]);
            // TODO can we have array indices on me?
            IdentBase::Me {
                array_indices: vec![],
            }
        } else {
            IdentBase::Complete(self.ident_part())
        };

        let mut property_accesses = vec![];
        // TODO in windows you can't have a space between the property and the dot
        while self.at(T![.]) {
            self.consume(T![.]);
            let part = self.ident_part();
            property_accesses.push(part);
        }
        FullIdent {
            base,
            property_accesses,
        }
    }

    pub fn type_(&mut self) -> ast::Type {
        let ident = self
            .next()
            .expect("Tried to parse type, but there were no more tokens");
        assert_eq!(
            ident.kind,
            T![ident],
            "Expected identifier at start of type, but found `{}`",
            ident.kind
        );
        let name = self.text(&ident).to_string();

        let mut generics = Vec::new();

        if self.at(T![<]) {
            self.consume(T![<]);
            while !self.at(T![>]) {
                // Generic parameters are also types
                let generic = self.type_();
                generics.push(generic);
                if self.at(T![,]) {
                    self.consume(T![,]);
                }
            }
            self.consume(T![>]);
        }

        ast::Type { name, generics }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::ast::IdentBase;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_ident_deep() {
        let input = "base.prop.prop2(1).     prop3(2,3).   _\nprop4(1)(3).prop5";
        let mut parser = Parser::new(input);
        let ident = parser.ident_deep();
        assert_eq!(
            ident,
            FullIdent {
                base: IdentBase::ident("base"),
                property_accesses: vec![
                    IdentPart {
                        name: "prop".to_string(),
                        array_indices: vec![]
                    },
                    IdentPart {
                        name: "prop2".to_string(),
                        array_indices: vec![vec![Some(Expr::int(1))]]
                    },
                    IdentPart {
                        name: "prop3".to_string(),
                        array_indices: vec![vec![Some(Expr::int(2)), Some(Expr::int(3))]]
                    },
                    IdentPart {
                        name: "prop4".to_string(),
                        array_indices: vec![vec![Some(Expr::int(1))], vec![Some(Expr::int(3))]]
                    },
                    IdentPart {
                        name: "prop5".to_string(),
                        array_indices: vec![]
                    }
                ]
            }
        );
    }

    #[test]
    fn test_parse_ident_deep_relative() {
        let input = ".prop.prop2";
        let mut parser = Parser::new(input);
        let ident = parser.ident_deep();
        assert_eq!(
            ident,
            FullIdent {
                base: IdentBase::partial("prop"),
                property_accesses: vec![IdentPart::ident("prop2")]
            }
        );
    }

    #[test]
    fn test_parse_ident_deep_with_keyword_property() {
        // property is a keyword in VBScript
        let input = ".property.property.other";
        let mut parser = Parser::new(input);
        let ident = parser.ident_deep();
        assert_eq!(
            ident,
            FullIdent {
                base: IdentBase::partial("property"),
                property_accesses: vec![IdentPart::ident("property"), IdentPart::ident("other")]
            }
        );
    }

    #[test]
    #[should_panic(expected = "0:0 Expected identifier as identifier, but found `<EOF>`")]
    fn test_parse_ident_deep_fail_with_trailing_dot() {
        let input = "a.b.";
        let mut parser = Parser::new(input);
        parser.ident_deep();
    }
}
