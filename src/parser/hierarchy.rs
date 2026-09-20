use crate::T;
use crate::lexer::{Span, Token, TokenKind};
use crate::parser::ast::ExprKind::WithScoped;
use crate::parser::ast::{
    Argument, ArgumentType, Case, DoLoopCheck, DoLoopCondition, ErrorClause, Expr, ExprKind,
    FullIdent, Item, ItemKind, MemberAccess, MemberDefinitions, PropertyType, PropertyVisibility,
    SetRhs, Stmt, StmtKind, VarDecl, Visibility,
};
use crate::parser::{ParseError, Parser};
use std::collections::HashSet;

impl<I> Parser<'_, I>
where
    I: Iterator<Item = Token>,
{
    pub fn file(&mut self) -> Result<Vec<Item>, ParseError> {
        let mut items = Vec::new();
        while !self.at(T![EOF]) {
            // Skip newlines, our token iter newline handling can't catch all cases
            // one example is ":\r\n".
            if self.at(T![nl]) {
                self.consume(T![nl])?;
                continue;
            }
            let item = self.item()?;
            items.push(item);
        }
        Ok(items)
    }

    pub fn item(&mut self) -> Result<Item, ParseError> {
        let start = self.start();
        let item = match self.peek() {
            T![option] => self.item_option()?,
            vis @ T![public] | vis @ T![private] => {
                let visibility = match self.consume(vis)?.kind {
                    T![public] => Visibility::Public,
                    T![private] => Visibility::Private,
                    _ => unreachable!(),
                };

                match self.peek() {
                    T![function] => {
                        let function = self.statement_function(visibility)?;
                        let item = ItemKind::Statement(self.spanned(function, start));
                        self.consume_line_delimiter()?;
                        item
                    }
                    T![sub] => {
                        let sub = self.statement_sub(visibility)?;
                        let item = ItemKind::Statement(self.spanned(sub, start));
                        self.consume_line_delimiter()?;
                        item
                    }
                    T![const] => self.item_const(visibility)?,
                    // the keywords that can be used as a name, see `identifier()`
                    T![ident] | T![default] | T![error] | T![property] | T![step] => {
                        self.item_variable(visibility)?
                    }
                    _ => {
                        let peek = self.peek_full()?;
                        return Err(ParseError::new(
                            format!(
                                "Expected `function`, `sub` or `const` after visibility, but found `{}`",
                                peek.kind
                            ),
                            peek.line,
                            peek.column,
                        ));
                    }
                }
            }
            T![class] => self.item_class()?,
            T![const] => self.item_const(Visibility::Public)?,
            _ => {
                // this must be a statement
                let stmt = self.statement(true)?;
                ItemKind::Statement(stmt)
            }
        };
        Ok(self.spanned(item, start))
    }

    fn item_const(&mut self, visibility: Visibility) -> Result<ItemKind, ParseError> {
        self.consume(T![const])?;
        let mut values = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let name = self.identifier("const name")?;
            self.consume(T![=])?;
            let literal = self.parse_const_literal()?;
            values.push((name, literal));
            if self.at(T![,]) {
                self.consume(T![,])?;
            } else {
                break;
            }
        }
        self.consume_line_delimiter()?;
        Ok(ItemKind::Const { visibility, values })
    }

    fn item_variable(&mut self, visibility: Visibility) -> Result<ItemKind, ParseError> {
        let mut vars = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let name = self.identifier("variable name")?;
            let bounds = self.const_bounds()?;
            vars.push(VarDecl { name, bounds });
            if self.at(T![,]) {
                self.consume(T![,])?;
            } else {
                break;
            }
        }
        self.consume_line_delimiter()?;
        Ok(ItemKind::Variable { visibility, vars })
    }

    fn item_class(&mut self) -> Result<ItemKind, ParseError> {
        let class_token = self.consume(T![class])?;
        let name = self.identifier("class name")?;
        self.consume_line_delimiter()?;
        let mut members = Vec::new();
        let mut member_accessors = Vec::new();
        let mut methods = Vec::new();
        let mut dims = Vec::new();
        while !self.at(T![end]) {
            let member_start = self.start();
            let mut default = None;
            let visibility = if self.at(T![public]) {
                self.consume(T![public])?;
                if self.at(T![default]) {
                    self.consume(T![default])?;
                    default = Some(true);
                }
                Visibility::Public
            } else if self.at(T![private]) {
                self.consume(T![private])?;
                Visibility::Private
            } else {
                Visibility::Default
            };

            match self.peek() {
                T![property] => member_accessors.push(self.class_property(default, visibility)?),
                T![function] => {
                    let function = self.class_function(visibility)?;
                    methods.push(self.spanned(function, member_start));
                }
                T![sub] => {
                    let sub = self.class_sub(visibility)?;
                    methods.push(self.spanned(sub, member_start));
                }
                T![dim] => {
                    // visibility can not be set for dims
                    if visibility != Visibility::Default {
                        let peek = self.peek_full()?;
                        return Err(ParseError::new(
                            format!(
                                "Class dim does not support visibility but found '{}'",
                                peek.line,
                            ),
                            peek.line,
                            peek.column,
                        ));
                    }
                    dims.push(self.class_dim()?);
                }
                T![nl] => {
                    // Skip newlines, our token iter newline handling can't catch all cases
                    // one example is ":\r\n".
                    self.consume(T![nl])?;
                }
                _ => {
                    members.push(self.class_member(visibility)?);
                }
            }
        }
        self.consume(T![end])?;
        self.consume(T![class])?;
        self.consume_line_delimiter()?;
        let mut member_names = HashSet::new();
        let name_redefined = |name: &str| {
            ParseError::new(
                format!("Name redefined '{name}'"),
                class_token.line,
                class_token.column,
            )
        };
        for member in &members {
            for VarDecl { name, .. } in &member.properties {
                let lower = name.to_ascii_lowercase();
                if member_names.contains(&lower) {
                    return Err(name_redefined(name));
                }
                member_names.insert(lower);
            }
        }
        for dim in &dims {
            for VarDecl { name, .. } in dim {
                let lower = name.to_ascii_lowercase();
                if member_names.contains(&lower) {
                    return Err(name_redefined(name));
                }
                member_names.insert(lower);
            }
        }
        for method in &methods {
            if let StmtKind::Sub { name, .. } = &method.node {
                let lower = name.to_ascii_lowercase();
                if member_names.contains(&lower) {
                    return Err(name_redefined(name));
                }
                member_names.insert(lower);
            }
            if let StmtKind::Function { name, .. } = &method.node {
                let lower = name.to_ascii_lowercase();
                if member_names.contains(&lower) {
                    return Err(name_redefined(name));
                }
                member_names.insert(lower);
            }
        }
        Ok(ItemKind::Class {
            name,
            members,
            dims,
            member_accessors,
            methods,
        })
    }

    fn class_member(&mut self, visibility: Visibility) -> Result<MemberDefinitions, ParseError> {
        // properties
        if visibility == Visibility::Default {
            let peek = self.peek_full()?;
            return Err(ParseError::new(
                format!(
                    "Expected visibility for class member but found '{}'",
                    peek.kind
                ),
                peek.line,
                peek.column,
            ));
        };
        // like a dim we can have multiple properties in one line of which some can be arrays
        let mut properties = Vec::new();
        while {
            let name = self.identifier("class member")?;
            let bounds = self.const_bounds()?;
            properties.push(VarDecl { name, bounds });
            self.at(T![,])
        } {
            self.consume(T![,])?;
        }
        let member_definitions = MemberDefinitions {
            visibility,
            properties,
        };
        self.consume_line_delimiter()?;
        Ok(member_definitions)
    }

    fn class_dim(&mut self) -> Result<Vec<VarDecl>, ParseError> {
        self.consume(T![dim])?;
        let mut vars = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let name = self.identifier("variable name")?;
            let bounds = self.const_bounds()?;
            vars.push(VarDecl { name, bounds });
            if self.at(T![,]) {
                self.consume(T![,])?;
            } else {
                break;
            }
        }
        self.consume_line_delimiter()?;
        Ok(vars)
    }

    fn class_sub(&mut self, visibility: Visibility) -> Result<StmtKind, ParseError> {
        self.consume(T![sub])?;
        let method_name = self.identifier("Sub name")?;
        let parameters = self.optional_declaration_parameter_list("Sub")?;
        self.consume_line_delimiter()?;
        let body = self.block(true, &[T![end]])?;
        self.consume(T![end])?;
        self.consume(T![sub])?;
        self.consume_line_delimiter()?;
        Ok(StmtKind::Sub {
            visibility,
            name: method_name.clone(),
            parameters,
            body,
        })
    }

    fn class_function(&mut self, visibility: Visibility) -> Result<StmtKind, ParseError> {
        self.consume(T![function])?;
        let method_name = self.identifier("Function name")?;
        let parameters = self.optional_declaration_parameter_list("Function")?;
        self.consume_line_delimiter()?;
        let body = self.block(true, &[T![end]])?;
        self.consume(T![end])?;
        self.consume(T![function])?;
        self.consume_line_delimiter()?;
        Ok(StmtKind::Function {
            visibility,
            name: method_name.clone(),
            parameters,
            body,
        })
    }

    fn class_property(
        &mut self,
        default: Option<bool>,
        visibility: Visibility,
    ) -> Result<MemberAccess, ParseError> {
        let property_visibility = match visibility {
            Visibility::Public => PropertyVisibility::Public {
                default: default.unwrap_or(false),
            },
            Visibility::Private => PropertyVisibility::Private,
            Visibility::Default => PropertyVisibility::Public {
                default: default.unwrap_or(false),
            },
        };

        self.consume(T![property])?;
        // let (Variant), get or set (Object)
        let property_type = match self.peek() {
            T![let] => {
                self.consume(T![let])?;
                PropertyType::Let
            }
            T![set] => {
                self.consume(T![set])?;
                PropertyType::Set
            }
            T![get] => {
                self.consume(T![get])?;
                PropertyType::Get
            }
            other => {
                let peek = self.peek_full()?;
                return Err(ParseError::new(
                    format!(
                        "Expected `let`, `set` or `get` in class property definition, but found `{other}`"
                    ),
                    peek.line,
                    peek.column,
                ));
            }
        };

        let name = self.identifier("property name")?;
        let property_arguments = self.optional_parenthesized_property_arguments()?;

        let property_body = self.block(true, &[T![end]])?;
        self.consume(T![end])?;
        self.consume(T![property])?;
        self.consume_line_delimiter()?;
        Ok(MemberAccess {
            visibility: property_visibility,
            name,
            property_type,
            args: property_arguments,
            body: property_body,
        })
    }

    fn item_option(&mut self) -> Result<ItemKind, ParseError> {
        self.consume(T![option])?;
        let explicit = match self.next() {
            Some(explicit) => explicit,
            None => {
                return Err(self.end_of_input_error("Expected identifier after `option`"));
            }
        };
        if explicit.kind != T![ident] || !self.text(&explicit).eq_ignore_ascii_case("explicit") {
            return Err(ParseError::new(
                "Expected `explicit` after `option`",
                explicit.line,
                explicit.column,
            ));
        }
        self.consume_line_delimiter()?;
        Ok(ItemKind::OptionExplicit)
    }

    fn statement_sub(&mut self, visibility: Visibility) -> Result<StmtKind, ParseError> {
        self.consume(T![sub])?;

        let name = self.identifier("sub name")?;
        let parameters = self.optional_declaration_parameter_list("Sub")?;
        self.consume_optional_line_delimiter()?;
        let body = self.block(true, &[T![end]])?;

        self.consume(T![end])?;
        self.consume(T![sub])?;

        Ok(StmtKind::Sub {
            visibility,
            name,
            parameters,
            body,
        })
    }

    fn statement_function(&mut self, visibility: Visibility) -> Result<StmtKind, ParseError> {
        self.consume(T![function])?;

        let name = self.identifier("function name")?;

        let parameters = self.optional_declaration_parameter_list("Function")?;

        self.consume_optional_line_delimiter()?;
        // do we need to do something special with the returned value?
        let body = self.block(true, &[T![end]])?;

        self.consume(T![end])?;
        self.consume(T![function])?;

        Ok(StmtKind::Function {
            visibility,
            name,
            parameters,
            body,
        })
    }

    fn const_bounds(&mut self) -> Result<Option<Vec<usize>>, ParseError> {
        let mut bounds = None;
        if self.at(T!['(']) {
            self.consume(T!['('])?;
            let mut dims = vec![];
            while !self.at(T![')']) {
                let dim = self.consume(T![integer_literal])?;
                let dim: usize = match self.text(&dim).parse() {
                    Ok(dim) => dim,
                    Err(_) => {
                        return Err(ParseError::new(
                            "Expected integer literal as bound",
                            dim.line,
                            dim.column,
                        ));
                    }
                };
                dims.push(dim);
                if self.at(T![,]) {
                    self.consume(T![,])?;
                }
            }
            self.consume(T![')'])?;
            bounds = Some(dims);
        }
        Ok(bounds)
    }

    /// Parse a list of parameters for a function or sub declaration.
    fn optional_declaration_parameter_list(
        &mut self,
        item_type: &str,
    ) -> Result<Vec<Argument>, ParseError> {
        let mut parameters: Vec<Argument> = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['('])?;
            while !self.at(T![')']) {
                // see https://learn.microsoft.com/en-us/previous-versions/windows/internet-explorer/ie-developer/scripting-articles/tt223ahx(v=vs.84)
                // optional modifier
                let modifier = if self.at(T![byval]) {
                    self.consume(T![byval])?;
                    Argument::ByVal
                } else if self.at(T![byref]) {
                    self.consume(T![byref])?;
                    Argument::ByRef
                } else {
                    Argument::ByVal
                };

                let parameter_name = self.identifier(item_type)?;
                // In case of array parameters there is a () allowed after the parameter name
                // this is optional and totally unclear in the documentation
                // so we just ignore it.
                // I could not find any runtime validation that would fail if something is passed that
                // is not an array.
                if self.at(T!['(']) {
                    self.consume(T!['('])?;
                    self.consume(T![')'])?;
                }
                parameters.push(modifier(parameter_name));
                if self.at(T![,]) {
                    self.consume(T![,])?;
                }
            }
            self.consume(T![')'])?;
        }
        Ok(parameters)
    }

    pub(crate) fn identifier(&mut self, item_type: &str) -> Result<String, ParseError> {
        let id = self.identifier_opt()?;
        match id {
            None => {
                let peek = self.peek_full()?;
                Err(ParseError::new(
                    format!(
                        "Expected identifier as {}, but found `{}`",
                        item_type, peek.kind
                    ),
                    peek.line,
                    peek.column,
                ))
            }
            Some(id) => Ok(id),
        }
    }

    pub(crate) fn identifier_opt(&mut self) -> Result<Option<String>, ParseError> {
        let peek = self.peek_full()?;
        Ok(match peek.kind {
            // The keywords that `cscript` on Windows accepts as a name. `explicit` and
            // `erase` are valid as well but are no keywords for us.
            T![ident] | T![property] | T![step] | T![default] | T![error] => {
                self.next().map(|token| self.text(&token).to_string())
            }
            _ => None,
        })
    }

    pub(crate) fn member_identifier(&mut self) -> Result<String, ParseError> {
        const ITEM_TYPE: &str = "member identifier";
        let ident = match self.next() {
            Some(ident) => ident,
            None => {
                let peek_full = self.peek_full()?;
                return Err(ParseError::new(
                    format!(
                        "Expected identifier as {}, but found `{}`",
                        ITEM_TYPE, peek_full.kind
                    ),
                    peek_full.line,
                    peek_full.column,
                ));
            }
        };
        match ident.kind {
            T![ident]
            | T![true]
            | T![false]
            | T![not]
            | T![and]
            | T![or]
            | T![xor]
            | T![eqv]
            | T![imp]
            | T![mod]
            | T![is]
            | T![call]
            | T![dim]
            | T![sub]
            | T![function]
            | T![get]
            | T![let]
            | T![const]
            | T![if]
            | T![else]
            | T![elseif]
            | T![end]
            | T![then]
            | T![exit]
            | T![while]
            | T![wend]
            | T![do]
            | T![loop]
            | T![until]
            | T![for]
            | T![to]
            | T![each]
            | T![in]
            | T![select]
            | T![case]
            | T![byref]
            | T![byval]
            | T![option]
            | T![nothing]
            | T![empty]
            | T![null]
            | T![class]
            | T![set]
            | T![new]
            | T![public]
            | T![private]
            | T![next]
            | T![on]
            | T![resume]
            | T![goto]
            | T![with]
            | T![redim]
            | T![preserve]
            | T![property]
            | T![me]
            | T![stop]
            | T![step]
            | T![unused] => Ok(self.text(&ident).to_string()),
            _ => Err(ParseError::new(
                format!(
                    "Expected identifier as {}, but found `{}`",
                    ITEM_TYPE, ident.kind
                ),
                ident.line,
                ident.column,
            )),
        }
    }

    /// Parse a block of statements until we reach an `end` token.
    pub fn block(
        &mut self,
        multi_line: bool,
        end_tokens: &[TokenKind],
    ) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !end_tokens.contains(&self.peek()) {
            if !self.at(T![nl]) && !self.at(T![:]) {
                let stmt = self.statement(false)?;
                stmts.push(stmt);
            }
            if end_tokens.contains(&self.peek()) {
                break;
            } else if self.at(T![nl]) {
                if multi_line {
                    self.consume(T![nl])?;
                } else {
                    break;
                }
            } else if self.at(T![:]) {
                self.consume(T![:])?;
            }
        }
        Ok(stmts)
    }

    pub fn statement(&mut self, consume_delimiter: bool) -> Result<Stmt, ParseError> {
        self.enter_nested()?;
        let result = self.statement_inner(consume_delimiter);
        self.leave_nested();
        result
    }

    fn statement_inner(&mut self, consume_delimiter: bool) -> Result<Stmt, ParseError> {
        let start = self.start();
        let kind = match self.peek() {
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
            T![stop] => {
                self.consume(T![stop])?;
                Ok(StmtKind::Stop)
            }
            T![sub] => self.statement_sub(Visibility::Default),
            T![function] => self.statement_function(Visibility::Default),
            T![private] | T![public] => {
                let visibility = match self.peek() {
                    T![public] => {
                        self.consume(T![public])?;
                        Visibility::Public
                    }
                    T![private] => {
                        self.consume(T![private])?;
                        Visibility::Private
                    }
                    _ => unreachable!(),
                };
                match self.peek() {
                    T![sub] => self.statement_sub(visibility),
                    T![function] => self.statement_function(visibility),
                    _ => {
                        let full = self.peek_full()?;
                        return Err(ParseError::new(
                            format!(
                                "Expected `sub` or `function` after visibility, but found `{}`",
                                full.kind
                            ),
                            full.line,
                            full.column,
                        ));
                    }
                }
            }
            // the keywords that can also be used as identifier, see `identifier()`
            T![ident] | T![me] | T![.] | T![property] | T![step] | T![default] | T![error] => {
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
                let ident = self.ident_deep()?;
                if self.at(T![=]) {
                    // assignment
                    self.consume(T![=])?;
                    let value = self.expression()?;
                    Ok(StmtKind::Assignment {
                        full_ident: ident,
                        value: Box::new(value),
                    })
                } else if self.at_new_line_or_eof() {
                    // sub call without args, or with a single argument in parentheses
                    self.fail_if_using_parentheses_when_calling_sub(&ident)?;
                    let (fn_name, argument) = self.fix_sub_ident(ident);
                    Ok(StmtKind::SubCall {
                        fn_name,
                        args: argument.into_iter().map(Some).collect(),
                    })
                } else {
                    self.fail_if_using_empty_parentheses_when_calling_sub(&ident)?;

                    // sub call with args
                    self.fail_if_using_parentheses_when_calling_sub(&ident)?;

                    let (patched_ident, part_of_expression) = self.fix_sub_ident(ident);
                    let args = self.sub_arguments(part_of_expression)?;

                    Ok(StmtKind::SubCall {
                        fn_name: patched_ident,
                        args,
                    })
                }
            }
            unexpected => {
                let full = self.peek_full()?;
                return Err(ParseError::new(
                    format!("Unexpected token: {unexpected}"),
                    full.line,
                    full.column,
                ));
            }
        }?;
        let stmt = self.spanned(kind, start);
        if consume_delimiter {
            self.consume_line_delimiter()?;
        }
        Ok(stmt)
    }

    /// When calling a Sub
    /// If the last part of the ident has any function application with single argument
    /// we need to re-evaluate it as part of the sub arguments. As it is part of the argument expression.
    ///
    /// eg `SomeArray(1).Accessor(1,2,3)(1)` should be parsed as `SomeArray(1).Accessor(1,2,3)` + `1` as argument
    fn fix_sub_ident(&self, ident: FullIdent) -> (FullIdent, Option<Expr>) {
        let mut part_of_expression: Option<Expr> = None;
        let mut patched_ident = ident.clone();

        // if the outer expression is a function application with a single argument
        // we need to re-evaluate it as part of the sub arguments
        let outer: Expr = *ident.0;

        if let ExprKind::FnApplication { callee, args } = outer.node
            && let [Some(arg)] = args.as_slice()
        {
            // What looked like the arguments of a call is an expression in parentheses:
            // `Foo (a)` passes `a` by value and `Foo (a) + 1` is the start of an expression.
            let after_callee = callee.span.end as usize;
            let outer_end = (outer.span.end as usize).max(after_callee);
            let offset = self.input[after_callee..outer_end].find('(').unwrap_or(0);
            let span = Span {
                start: (after_callee + offset) as u32,
                end: outer.span.end,
            };
            let paren = ExprKind::Paren(Box::new(arg.clone()));
            part_of_expression = Some(Expr::with_span(paren, span));
            patched_ident = FullIdent::new(*callee)
        }

        (patched_ident, part_of_expression)
    }

    /// Specific case where we have a sub call like `Foo(), 1`
    /// These however are valid: `Foo(1), 1` (translates to `Foo 1, 1`) and `Foo()`
    ///
    /// WScript returns: 'compilation error: Expected end of statement'
    fn fail_if_using_empty_parentheses_when_calling_sub(
        &mut self,
        ident: &FullIdent,
    ) -> Result<(), ParseError> {
        let inner = &ident.0;
        if let ExprKind::FnApplication { args, .. } = &inner.node
            && args.is_empty()
            && self.at(T![,])
        {
            let full = self.peek_full()?;
            return Err(ParseError::new(
                "compilation error: Expected end of statement",
                full.line,
                full.column,
            ));
        }
        Ok(())
    }

    /// A sub call statement like `something(1,2)` or `SomeArray(1).SomeSub(1,2,3)` is not valid
    /// However a sub call statement like `something(2)` is valid as the `(2)` is considered the first argument
    ///
    /// WScript returns: 'compilation error: Cannot use parentheses when calling a Sub'
    fn fail_if_using_parentheses_when_calling_sub(
        &mut self,
        ident: &FullIdent,
    ) -> Result<(), ParseError> {
        let inner = &ident.0;
        if let ExprKind::FnApplication { args, .. } = &inner.node
            && args.len() > 1
        {
            let full = self.peek_full()?;
            return Err(ParseError::new(
                "compilation error: Cannot use parentheses when calling a Sub",
                full.line,
                full.column,
            ));
        };
        Ok(())
    }

    fn statement_call(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![call])?;
        let ident = self.ident_deep()?;
        // TODO if there are no args the parens should be omitted
        //   to validate on windows
        Ok(StmtKind::Call(ident))
    }

    fn sub_arguments(
        &mut self,
        mut first_expression_part: Option<Expr>,
    ) -> Result<Vec<Option<Expr>>, ParseError> {
        let mut args = Vec::new();
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
                self.consume(T![,])?;
                args.push(None);
                continue;
            }
            let arg = self.expression_with_prefix(first_expression_part)?;
            // consume it only once
            first_expression_part = None;
            args.push(Some(arg));
            if self.at(T![,]) {
                self.consume(T![,])?;
            } else {
                break;
            }
        }
        // the statement ends right after the first part: `If a Then Foo (1) Else Bar`
        if let Some(arg) = first_expression_part {
            args.push(Some(arg));
        }
        Ok(args)
    }

    fn statement_dim(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![dim])?;
        let mut vars = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let name = self.identifier("variable name")?;
            let bounds = self.parenthesized_arguments()?;
            vars.push((name, bounds));
            if self.at(T![,]) {
                self.consume(T![,])?;
            } else {
                break;
            }
        }
        Ok(StmtKind::Dim { vars })
    }

    fn statement_redim(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![redim])?;
        let mut preserve = false;
        if self.at(T![preserve]) {
            self.consume(T![preserve])?;
            preserve = true;
        }
        let mut var_bounds = Vec::new();
        let var_name = self.identifier("variable name")?;
        let bounds = self.parenthesized_arguments()?;
        var_bounds.push((var_name, bounds));
        while self.at(T![,]) {
            self.consume(T![,])?;
            let name = self.identifier("variable name")?;
            let bounds = self.parenthesized_arguments()?;
            var_bounds.push((name, bounds));
        }

        Ok(StmtKind::ReDim {
            preserve,
            var_bounds,
        })
    }

    fn statement_const(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![const])?;

        // multiple constants can be defined in one line
        let mut constants = Vec::new();
        while !self.at(T![nl]) && !self.at(T![EOF]) {
            let name = self.identifier("variable name")?;
            self.consume(T![=])?;
            let literal = self.parse_const_literal()?;
            constants.push((name, literal));
            if self.at(T![,]) {
                self.consume(T![,])?;
            } else {
                break;
            }
        }
        Ok(StmtKind::Const(constants))
    }

    fn statement_with(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![with])?;
        let object = self.ident_deep()?;
        self.consume_line_delimiter()?;
        let body = self.block(true, &[T![end]])?;
        self.consume(T![end])?;
        self.consume(T![with])?;
        Ok(StmtKind::With { object, body })
    }

    fn statement_exit(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![exit])?;
        let res = match self.peek() {
            T![do] => {
                self.consume(T![do])?;
                StmtKind::ExitDo
            }
            T![for] => {
                self.consume(T![for])?;
                StmtKind::ExitFor
            }
            T![function] => {
                self.consume(T![function])?;
                StmtKind::ExitFunction
            }
            T![property] => {
                self.consume(T![property])?;
                StmtKind::ExitProperty
            }
            T![sub] => {
                self.consume(T![sub])?;
                StmtKind::ExitSub
            }
            other => {
                let peek = self.peek_full()?;
                return Err(ParseError::new(
                    format!("Exit not supported for `{other}`"),
                    peek.line,
                    peek.column,
                ));
            }
        };
        Ok(res)
    }

    fn statement_on(&mut self) -> Result<StmtKind, ParseError> {
        // error handling
        self.consume(T![on])?;
        self.consume(T![error])?;
        let error_clause = if self.at(T![resume]) {
            self.consume(T![resume])?;
            self.consume(T![next])?;
            ErrorClause::ResumeNext
        } else if self.at(T![goto]) {
            self.consume(T![goto])?;
            let token = self.consume(T![integer_literal])?;
            let number: usize = match self.text(&token).parse() {
                Ok(number) => number,
                Err(_) => {
                    return Err(ParseError::new(
                        "Expected integer after `goto`",
                        token.line,
                        token.column,
                    ));
                }
            };
            if number != 0 {
                return Err(ParseError::new(
                    "Expected `goto 0` after `on error`",
                    token.line,
                    token.column,
                ));
            }
            ErrorClause::Goto0
        } else {
            let peek = self.peek_full()?;
            return Err(ParseError::new(
                "Expected `resume next` or `goto 0` after `on error`",
                peek.line,
                peek.column,
            ));
        };
        Ok(StmtKind::OnError { error_clause })
    }

    fn statement_select(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![select])?;
        self.consume(T![case])?;
        let expr = self.expression()?;
        self.consume_line_delimiter()?;
        let mut cases: Vec<Case> = Vec::new();
        let mut else_stmt = None;
        while !self.at(T![end]) {
            if else_stmt.is_some() {
                let peek = self.peek_full()?;
                return Err(ParseError::new(
                    "`else` statement must be last in `select case` block",
                    peek.line,
                    peek.column,
                ));
            }
            self.consume(T![case])?;
            if self.at(T![else]) {
                self.consume(T![else])?;
                self.consume_optional_line_delimiter()?;
                let block = self.block(true, &[T![end], T![case]])?;
                else_stmt = Some(block);
            } else {
                let mut tests = Vec::new();
                // comma separated list of expressions
                let first_expr = self.expression()?;
                tests.push(first_expr);
                while self.at(T![,]) {
                    self.consume(T![,])?;
                    let expr = self.expression()?;
                    tests.push(expr);
                }
                self.consume_optional_line_delimiter()?;
                let body = self.block(true, &[T![end], T![case]])?;
                cases.push(Case { tests, body });
            }
        }
        self.consume(T![end])?;
        self.consume(T![select])?;
        Ok(StmtKind::SelectCase {
            test_expr: Box::new(expr),
            cases,
            else_stmt,
        })
    }

    fn statement_for(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![for])?;

        if self.at(T![each]) {
            self.consume(T![each])?;
            let element = self.next().ok_or_else(|| {
                self.end_of_input_error("Expected loop variable after `for each`")
            })?;
            let element_name = self.text(&element).to_string();
            self.consume(T![in])?;
            let group = Box::new(self.expression()?);
            self.consume_line_delimiter()?;

            let body = self.block(true, &[T![next]])?;

            self.consume(T![next])?;

            Ok(StmtKind::ForEachStmt {
                element: element_name,
                group,
                body,
            })
        } else {
            let counter = self
                .next()
                .ok_or_else(|| self.end_of_input_error("Expected loop counter after `for`"))?;
            let counter_name = self.text(&counter).to_string();
            self.consume(T![=])?;
            let start = self.expression()?;
            self.consume(T![to])?;
            let end = self.expression()?;
            let step = if self.at(T![step]) {
                self.consume(T![step])?;
                Some(Box::new(self.expression()?))
            } else {
                None
            };
            self.consume_line_delimiter()?;

            let body = self.block(true, &[T![next]])?;

            self.consume(T![next])?;

            Ok(StmtKind::ForStmt {
                counter: counter_name,
                start: Box::new(start),
                end: Box::new(end),
                step,
                body,
            })
        }
    }

    fn statement_do(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![do])?;
        let mut check = DoLoopCheck::None;

        if self.at(T![while]) {
            self.consume(T![while])?;
            check = DoLoopCheck::Pre(DoLoopCondition::While(Box::new(self.expression()?)))
        } else if self.at(T![until]) {
            self.consume(T![until])?;
            check = DoLoopCheck::Pre(DoLoopCondition::Until(Box::new(self.expression()?)))
        }

        let body = self.block(true, &[T![loop]])?;
        self.consume(T![loop])?;

        if self.at(T![while]) {
            self.consume(T![while])?;
            check = DoLoopCheck::Post(DoLoopCondition::While(Box::new(self.expression()?)))
        } else if self.at(T![until]) {
            self.consume(T![until])?;
            check = DoLoopCheck::Post(DoLoopCondition::Until(Box::new(self.expression()?)))
        }

        Ok(StmtKind::DoLoop { check, body })
    }

    fn statement_while(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![while])?;
        let condition = self.expression()?;
        self.consume_line_delimiter()?;

        let body = self.block(true, &[T![wend]])?;

        self.consume(T![wend])?;

        Ok(StmtKind::WhileStmt {
            condition: Box::new(condition),
            body,
        })
    }

    fn statement_set(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![set])?;
        let var = self.ident_deep()?;
        self.consume(T![=])?;
        let rhs = match self.peek() {
            T![nothing] => {
                self.consume(T![nothing])?;
                SetRhs::Nothing
            }
            _ => {
                let expr = self.expression()?;
                SetRhs::Expr(Box::new(expr))
            }
        };
        Ok(StmtKind::Set { var, rhs })
    }

    fn statement_if(&mut self) -> Result<StmtKind, ParseError> {
        self.consume(T![if])?;

        let condition = self.expression()?;

        self.consume(T![then])?;

        // if we have a newline, it's a block if statement
        if self.at(T![nl]) {
            self.consume(T![nl])?;
            let body = self.block(true, &[T![end], T![else], T![elseif]])?;
            let mut elseif_statements = Vec::new();
            while self.at(T![elseif]) {
                self.consume(T![elseif])?;
                let condition = self.expression()?;
                self.consume(T![then])?;
                self.consume_optional_line_delimiter()?;
                let block = self.block(true, &[T![end], T![else], T![elseif]])?;
                elseif_statements.push((Box::new(condition), block));
            }
            let else_stmt = if self.at(T![else]) {
                self.consume(T![else])?;
                self.consume_optional_line_delimiter()?;
                Some(self.block(true, &[T![end]])?)
            } else {
                None
            };
            self.consume(T![end])?;
            self.consume(T![if])?;
            Ok(StmtKind::IfStmt {
                condition: Box::new(condition),
                body,
                elseif_statements,
                else_stmt,
            })
        } else {
            // single line if statement
            // can contain multiple statements if separated by colons
            let body = self.block(false, &[T![else], T![elseif], T![end], T![EOF]])?;
            let mut elseif_statements = Vec::new();
            while self.at(T![elseif]) {
                self.consume(T![elseif])?;
                let condition = self.expression()?;
                self.consume(T![then])?;
                let elseif_body = self.block(false, &[T![else], T![elseif], T![end], T![EOF]])?;
                elseif_statements.push((Box::new(condition), elseif_body));
            }
            let else_stmt = if self.at(T![else]) {
                self.consume(T![else])?;
                let else_body = self.block(false, &[T![end], T![EOF]])?;
                Some(else_body)
            } else {
                None
            };

            // optional "End If" if we still have not encountered a newline
            if self.at(T![end]) {
                self.consume(T![end])?;
                self.consume(T![if])?;
            }

            Ok(StmtKind::IfStmt {
                condition: Box::new(condition),
                body,
                elseif_statements,
                else_stmt,
            })
        }
    }

    fn optional_parenthesized_property_arguments(
        &mut self,
    ) -> Result<Vec<(String, ArgumentType)>, ParseError> {
        let mut property_arguments = Vec::new();
        if self.at(T!['(']) {
            self.consume(T!['('])?;
            while !self.at(T![')']) {
                // modifiers for the property
                let argument_type = if self.at(T![byval]) {
                    self.consume(T![byval])?;
                    ArgumentType::ByVal
                } else if self.at(T![byref]) {
                    self.consume(T![byref])?;
                    ArgumentType::ByRef
                } else {
                    ArgumentType::ByVal
                };
                let arg_name = self.identifier("argument name")?;
                property_arguments.push((arg_name, argument_type));
                if self.at(T![,]) {
                    self.consume(T![,])?;
                }
            }
            self.consume(T![')'])?;
        }
        Ok(property_arguments)
    }

    fn ident_deep(&mut self) -> Result<FullIdent, ParseError> {
        let mut expr = self.ident_deep_lhs()?;
        let start = expr.span.start;
        loop {
            match self.peek() {
                // highest binding power
                // _. is the same as . but disallows whitespace between the dot and the base.
                // The property itself is allowed to be prefixed with whitespace.
                T![_.] => {
                    self.consume(T![_.])?;
                    let property = self.member_identifier()?;
                    let member = ExprKind::MemberExpression {
                        base: Box::new(expr),
                        property,
                    };
                    expr = self.spanned(member, start);
                    continue;
                }
                T!['('] => {
                    //self.consume(T!['(']);
                    let args = self.parenthesized_optional_arguments()?;
                    //self.consume(T![')']);
                    let application = ExprKind::FnApplication {
                        callee: Box::new(expr),
                        args,
                    };
                    expr = self.spanned(application, start);
                    continue;
                }
                _ => {
                    break;
                }
            }
        }
        Ok(FullIdent(Box::new(expr)))
    }

    fn ident_deep_lhs(&mut self) -> Result<Expr, ParseError> {
        let start = self.start();
        if let Some(literal) = self.literal()? {
            return Ok(self.spanned(ExprKind::Literal(literal), start));
        }

        let kind = match self.peek() {
            // `Me` is an expression, it can not be declared as a name
            T![me] => {
                let me = self.consume(T![me])?;
                ExprKind::Ident(self.text(&me).to_string())
            }
            T![ident] | T![property] | T![step] | T![error] | T![default] => {
                let ident = self.identifier("identifier base")?;
                ExprKind::Ident(ident)
            }
            T![.] => {
                // the object of the with block is implied, it has an empty span
                let base = self.spanned(WithScoped, start);
                self.consume(T![.])?;
                let property = self.identifier("property")?;
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
                self.consume(T!['('])?;
                let expr = self.parse_expression(0)?;
                self.consume(T![')'])?;
                ExprKind::Paren(Box::new(expr))
            }
            kind => {
                let token = self.peek_full()?;
                return Err(ParseError::new(
                    format!("Unknown start of identifier: {kind}"),
                    token.line,
                    token.column,
                ));
            }
        };
        Ok(self.spanned(kind, start))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    fn parse_ident_deep(input: &str) -> FullIdent {
        let mut parser = Parser::new(input);
        parser.ident_deep().unwrap()
    }

    #[test]
    fn test_parse_ident_deep_mixed() {
        let input = "base.prop.prop2(1).     prop3(2,3).   _\nprop4(1)(3).prop5";
        let ident = parse_ident_deep(input);
        #[rustfmt::skip]
        assert_eq!(
            ident,
            FullIdent::new(
                Expr::member(
                    Expr::fn_application(
                        Expr::fn_application(Expr::member(
                            Expr::fn_application(
                                Expr::member(
                                    Expr::fn_application(
                                        Expr::member(
                                            Expr::member(
                                                Expr::ident("base"),
                                                "prop",
                                            ),
                                            "prop2"
                                        ),
                                        vec![Expr::int(1), ],
                                    ),
                                    "prop3"
                                ),
                                vec![Expr::int(2), Expr::int(3), ],
                            ),
                            "prop4"
                        ),
                        vec![Expr::int(1),],
                        ),
                    vec![Expr::int(3),],
                    ),
                "prop5"
                )
            )
        );
    }

    #[test]
    fn test_parse_ident_deep_absolute() {
        let input = "obj.prop";
        let ident = parse_ident_deep(input);
        #[rustfmt::skip]
        assert_eq!(
            ident,
            FullIdent::new(
                Expr::member(
                    Expr::ident("obj"),
                    "prop"
                )
            )
        );
    }

    #[test]
    fn test_parse_ident_deep_relative() {
        let input = ".prop.prop2";
        let ident = parse_ident_deep(input);
        #[rustfmt::skip]
        assert_eq!(
            ident,
            FullIdent::new(
                Expr::member(
                    Expr::member(
                        ExprKind::WithScoped.into(),
                        "prop"
                    ),
                    "prop2"
                )
            )
        );
    }

    #[test]
    fn test_parse_ident_deep_with_keyword_property() {
        // property is a keyword in VBScript
        let input = ".property.property.other";
        let ident = parse_ident_deep(input);
        #[rustfmt::skip]
        assert_eq!(
            ident,
            FullIdent::new(
                Expr::member(
                    Expr::member(
                        Expr::member(
                            WithScoped.into(),
                            "property"
                        ),
                        "property"
                    ),
                "other"
            ))
        );
    }

    #[test]
    #[should_panic(
        expected = "ParseError at line 1, column 5: Expected identifier as member identifier, but found `<EOF>`"
    )]
    fn test_parse_ident_deep_fail_with_trailing_dot() {
        let input = "a.b.";
        parse_ident_deep(input);
    }
}
