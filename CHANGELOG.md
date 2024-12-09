# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
