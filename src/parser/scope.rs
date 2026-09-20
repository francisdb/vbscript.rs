//! The names that are declared in a scope, to fail with "Name redefined" where `cscript`
//! on Windows does.
//!
//! A script has one scope for all of its code outside of a procedure, and every sub,
//! function or property has one of its own. A block like an `If` or a loop has none. A
//! procedure does not see the names of the script for this: `Dim x` in a sub is fine with
//! a `Dim x` or a `Sub x` in the script. The members of a class are a scope as well, with
//! rules of their own, see [`ClassScope`].

use super::ast::{Name, PropertyType};
use super::{ParseError, Parser};
use std::collections::{HashMap, HashSet};

/// What a name was declared as.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Declared {
    /// By `Dim`, `Public`, `Private` or `ReDim`, or as a parameter. A function or property
    /// also has its own name as a variable, for the value it returns. A sub has not.
    Variable,
    Const,
    /// A sub or function of the script.
    Procedure,
    Class,
}

/// The names of a scope in lower case, names do not differ by case.
pub(crate) type Scope = HashMap<String, Declared>;

impl Parser<'_> {
    pub(crate) fn name_redefined(&self, name: &Name) -> ParseError {
        let (line, column) = name.span.line_column(self.input);
        ParseError::new(format!("Name redefined '{}'", name.node), line, column)
    }

    fn scope(&mut self) -> &mut Scope {
        self.locals.as_mut().unwrap_or(&mut self.globals)
    }

    /// Declare a name in the scope we are in.
    ///
    /// Only a sub or function of the script can be declared again, the last one is used.
    pub(crate) fn declare(&mut self, name: &Name, declared: Declared) -> Result<(), ParseError> {
        match self.scope().insert(name.to_ascii_lowercase(), declared) {
            None | Some(Declared::Procedure) if declared == Declared::Procedure => Ok(()),
            None => Ok(()),
            Some(_) => Err(self.name_redefined(name)),
        }
    }

    /// A `ReDim` declares its variable unless there is one, or a constant, in the scope
    /// already. That makes `ReDim a(1)` followed by `Dim a` fail, and the other way round
    /// not.
    pub(crate) fn declare_redim(&mut self, name: &Name) -> Result<(), ParseError> {
        let lower = name.to_ascii_lowercase();
        match self.scope().get(&lower) {
            Some(Declared::Variable | Declared::Const) => Ok(()),
            Some(Declared::Procedure | Declared::Class) => Err(self.name_redefined(name)),
            None => {
                self.scope().insert(lower, Declared::Variable);
                Ok(())
            }
        }
    }
}

/// The scope of a procedure: its parameters, and for a function or property its own name.
pub(crate) fn procedure_scope<'a>(names: impl IntoIterator<Item = &'a Name>) -> Scope {
    names
        .into_iter()
        .map(|name| (name.to_ascii_lowercase(), Declared::Variable))
        .collect()
}

/// The members of a class.
///
/// A variable can not share its name with any other member, and a sub or function not
/// with a member that was declared before it. A property only fails on a variable and on
/// a property of its own kind: `Get`, `Let` and `Set` go together, and `cscript` on
/// Windows also accepts a property named like a sub or function that comes before it.
#[derive(Default)]
pub(crate) struct ClassScope {
    variables: HashSet<String>,
    methods: HashSet<String>,
    properties: HashSet<(String, u8)>,
}

impl ClassScope {
    fn has_property(&self, lower: &str) -> bool {
        (0..3).any(|kind| self.properties.contains(&(lower.to_string(), kind)))
    }

    /// False if the name is taken.
    pub(crate) fn variable(&mut self, name: &Name) -> bool {
        let lower = name.to_ascii_lowercase();
        !self.methods.contains(&lower) && !self.has_property(&lower) && self.variables.insert(lower)
    }

    /// False if the name is taken.
    pub(crate) fn method(&mut self, name: &Name) -> bool {
        let lower = name.to_ascii_lowercase();
        !self.variables.contains(&lower) && !self.has_property(&lower) && self.methods.insert(lower)
    }

    /// False if the name is taken.
    pub(crate) fn property(&mut self, name: &Name, property_type: &PropertyType) -> bool {
        let lower = name.to_ascii_lowercase();
        let kind = match property_type {
            PropertyType::Get => 0,
            PropertyType::Let => 1,
            PropertyType::Set => 2,
        };
        !self.variables.contains(&lower) && self.properties.insert((lower, kind))
    }
}
