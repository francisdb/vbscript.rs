# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.1](https://github.com/francisdb/vbscript.rs/compare/v0.5.0...v0.5.1) - 2026-09-20

### Fixed

- *(parser)* fail on a name that is declared twice
- *(parser)* a parameter can not be named like its procedure or twice
- *(parser)* a class variable can be named default or property

### Other

- parse Elektra and Galaxy again

## [0.5.0](https://github.com/francisdb/vbscript.rs/compare/v0.4.2...v0.5.0) - 2026-09-20

### Added

- *(ast)* [**breaking**] add spans to the cases of a Select Case and to properties
- *(ast)* [**breaking**] keep the Default of a sub or function of a class

### Fixed

- *(lexer)* [**breaking**] the column of a token and of a parse error counts characters

### Other

- *(parser)* [**breaking**] make the internals of the parser and the lexer private

## [0.4.2](https://github.com/francisdb/vbscript.rs/compare/v0.4.1...v0.4.2) - 2026-09-20

### Fixed

- *(parser)* a sub or function can not be declared in a procedure
- *(lexer)* accept the forms of a date literal that Windows takes
- *(lexer)* the dot of a with block right after an operator or keyword ([#80](https://github.com/francisdb/vbscript.rs/pull/80))

### Other

- resolve the TODOs that were checked on Windows
- add a fuzz target for the lexer and the parser ([#79](https://github.com/francisdb/vbscript.rs/pull/79))

## [0.4.1](https://github.com/francisdb/vbscript.rs/compare/v0.4.0...v0.4.1) - 2026-09-20

### Added

- *(lexer)* add LineIndex to get the line and column of a span

### Fixed

- *(lexer)* a line end is on the line that it ends
- *(parser)* a parameter without ByVal or ByRef is passed by reference

### Other

- document the crate with examples
- make the corpus work on windows
- fetch the test corpus on windows

## [0.4.0](https://github.com/francisdb/vbscript.rs/compare/v0.3.1...v0.4.0) - 2026-09-20

### Added

- *(ast)* [**breaking**] add spans to the names in the syntax tree
- *(ast)* add a visitor to walk the syntax tree
- *(ast)* [**breaking**] add the Stop statement
- *(ast)* [**breaking**] add a node for an expression in parentheses
- *(ast)* [**breaking**] add source spans to expressions, statements and items

### Fixed

- *(lexer)* endif and enum are reserved words
- *(parser)* accept the keywords that are valid names wherever a name is declared
- *(parser)* parse `With New Foo` as a New expression
- *(parser)* keep the argument of a sub call that is followed by else or end

### Other

- *(ast)* [**breaking**] use VarDecl for Dim and a struct for ReDim
- remove the outdated vbsfmt reference from the README
- *(parser)* drop the list of keywords that are accepted as member name
- build the docs and fail on rustdoc warnings
- turn the bare URLs in doc comments into links
- *(ast)* [**breaking**] replace the name and bounds tuples with a VarDecl struct
- *(ast)* [**breaking**] take an i32 in Expr::int and Lit::int
- *(lexer)* [**breaking**] remove the public Token constructors
- [**breaking**] remove the empty interpreter feature
- *(parser)* [**breaking**] remove the public parse_literal

## [0.3.1](https://github.com/francisdb/vbscript.rs/compare/v0.3.0...v0.3.1) - 2026-09-20

### Added

- *(parser)* make ParseError usable as a std error

### Fixed

- *(lexer)* a word that follows a dot is always an identifier
- *(lexer)* do not panic on reserved words that have no token kind
- *(parser)* give hex and octal literals the value they have on Windows
- *(parser)* give every parse error a position instead of line 0
- *(lexer)* report the real position of error, whitespace, comment and EOF tokens
- *(parser)* return a ParseError instead of panicking or overflowing the stack
- *(lexer)* accept floats with a multi-digit integral and an exponent
- *(lexer)* only accept ASCII digits in numbers and identifiers ([#41](https://github.com/francisdb/vbscript.rs/pull/41))

### Other

- *(lexer)* resolve keywords outside of the generated lexer
- complete the package metadata
- fix the docs.rs link, testscripts path and branch name in the README

## [0.3.0](https://github.com/francisdb/vbscript.rs/compare/v0.2.5...v0.3.0) - 2026-09-08

### Other

- *(ast)* [**breaking**] remove unused identifier and type scaffolding ([#39](https://github.com/francisdb/vbscript.rs/pull/39))

## [0.2.5](https://github.com/francisdb/vbscript.rs/compare/v0.2.4...v0.2.5) - 2026-09-08

### Fixed

- *(lexer)* treat a dot right after Then or Else as a with-statement dot

### Other

- bump the wine vbscript test corpus to 9ce1651515d
- *(deps)* bump actions/cache from 5 to 6 ([#35](https://github.com/francisdb/vbscript.rs/pull/35))

## [0.2.4](https://github.com/francisdb/vbscript.rs/compare/v0.2.3...v0.2.4) - 2026-06-22

### Added

- *(lexer)* support bracketed (escaped) identifiers ([#32](https://github.com/francisdb/vbscript.rs/pull/32))

### Fixed

- *(lexer)* accept day 13-31 in #M/D/YYYY# date literals ([#34](https://github.com/francisdb/vbscript.rs/pull/34))
- *(lexer)* accept the optional O in octal literals ([#33](https://github.com/francisdb/vbscript.rs/pull/33))

### Other

- *(deps)* bump actions/checkout from 6 to 7 ([#30](https://github.com/francisdb/vbscript.rs/pull/30))
- pin vbs test corpus and exclude/skip new failing scripts ([#31](https://github.com/francisdb/vbscript.rs/pull/31))
- *(deps)* bump actions/cache from 4 to 5 ([#28](https://github.com/francisdb/vbscript.rs/pull/28))

## [0.2.3](https://github.com/francisdb/vbscript.rs/compare/v0.2.2...v0.2.3) - 2025-12-08

### Fixed

- parsing long numbers ([#23](https://github.com/francisdb/vbscript.rs/pull/23))

### Other

- *(deps)* update logos requirement from 0.15.0 to 0.16.0 ([#27](https://github.com/francisdb/vbscript.rs/pull/27))
- *(deps)* update criterion requirement from 0.7.0 to 0.8.0 ([#26](https://github.com/francisdb/vbscript.rs/pull/26))
- *(deps)* bump actions/checkout from 5 to 6 ([#25](https://github.com/francisdb/vbscript.rs/pull/25))
- remove excludes for NBSP ([#24](https://github.com/francisdb/vbscript.rs/pull/24))
- *(deps)* bump actions/checkout from 4 to 5 ([#19](https://github.com/francisdb/vbscript.rs/pull/19))
- clippy fixes ([#20](https://github.com/francisdb/vbscript.rs/pull/20))
- *(deps)* update criterion requirement from 0.6.0 to 0.7.0 ([#16](https://github.com/francisdb/vbscript.rs/pull/16))
- new clippy rules ([#17](https://github.com/francisdb/vbscript.rs/pull/17))

## [0.2.2](https://github.com/francisdb/vbscript.rs/compare/v0.2.1...v0.2.2) - 2025-05-19

### Other

- *(deps)* update criterion requirement from 0.5.1 to 0.6.0 ([#12](https://github.com/francisdb/vbscript.rs/pull/12))
- *(deps)* rust-2024 ([#13](https://github.com/francisdb/vbscript.rs/pull/13))

## [0.2.1](https://github.com/francisdb/vbscript.rs/compare/v0.2.0...v0.2.1) - 2024-12-09

### Fixed

- allow 'type' as member identifier

### Other

- *(deps)* update logos requirement from 0.14.0 to 0.15.0 (#11)
- fix new clippy warnings
- readme update

## [0.2.0](https://github.com/francisdb/vbscript.rs/compare/v0.1.0...v0.2.0) - 2024-05-27

### Added
- parsing returns errors instead of panicking ([#7](https://github.com/francisdb/vbscript.rs/pull/7))
- more generic expression parsing ([#5](https://github.com/francisdb/vbscript.rs/pull/5))

### Fixed
- fully parse wine test scripts ([#6](https://github.com/francisdb/vbscript.rs/pull/6))

### Other
- fix clone of test repos
- sparse checkout of vpinball scripts
