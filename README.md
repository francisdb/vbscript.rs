# vbscript.rs
Rust VBScript lexer and parser

https://crates.io/crates/vbscript

## Documentation

https://docs.rs/vpin

## Example code

Check the [examples folder](examples/)

## Rationale

On October 2023 [Microsoft deprecated VBScript](https://learn.microsoft.com/en-us/windows/whats-new/deprecated-features). A timeline is available at the [Windows IT Pro Blog](https://techcommunity.microsoft.com/t5/windows-it-pro-blog/vbscript-deprecation-timelines-and-next-steps/ba-p/4148301).

However, there are still many legacy systems that use VBScript. This project aims to provide a lexer, parser and maybe later interpreter for VBScript, so that it can be used in Rust projects.

## Projects using vbscript.rs

https://github.com/francisdb/vbsfmt

## Other links

* The [wine](https://www.winehq.org/) project has a similar lexer/parser/interpreter in C, but it's COM based and not very easy to use as a library: https://gitlab.winehq.org/wine/wine/-/tree/master/dlls/vbscript

* MS VBScript Language Reference https://docs.microsoft.com/en-us/previous-versions/t0aew7h6(v=vs.85)
* Vbsedit VBScript help: https://www.vbsedit.com/html/1c457e66-a6b2-4545-b2dd-33a59d8661e8.asp
* Vbsedit Example vbs files: https://www.vbsedit.com/scripts/default.asp
* vpx-js vbscript bnf grammar and transpiler https://github.com/vpdb/vpx-js/blob/master/lib/scripting/grammar/grammar.bnf
* Rosetta code VBScript bnf grammar https://rosettacode.org/wiki/BNF_Grammar#VBScript , used by [vbscript_in_js](https://github.com/kastner/vbscript_in_js)
* wine vbscript bison grammar https://gitlab.winehq.org/wine/wine/-/blob/master/dlls/vbscript/parser.y

## Running the integration tests

Make sure you populate the `testsctipts` folder, feel free to add more vbs files to the folder.

```bash
./testsctipts/populate.sh
```

Run the tests. They will also be included in the default `cargo test` run.

```bash
RUST_BACKTRACE=1 cargo test -- --nocapture try_lexing_all_vbs_files
RUST_BACKTRACE=1 cargo test -- --nocapture try_parsing_all_vbs_files
```

## Running the benchmarks

To run this benchmark, use the following command:

```bash
cargo bench
```

## Making a release

We use https://github.com/MarcoIeni/release-plz which creates a release pr on every commit to master

## Attributions

Thanks go to Domenic Quirl for his excellent blog post on parsing basics. This project was started from his blog post, you can find it here:
https://domenicquirl.github.io/blog/parsing-basics/
https://github.com/domenicquirl/blog/tree/master/parsing-basics
