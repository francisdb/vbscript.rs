# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.4](https://github.com/francisdb/vbscript.rs/compare/v0.2.3...v0.2.4) - 2025-12-15

### Other

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
