# vbscript.rs
Rust VBScript lexer and parser

https://crates.io/crates/vbscript

## Documentation

https://docs.rs/vpin

## Example code

Check the [examples folder](examples/)

## Projects using vbscript.rs

https://github.com/francisdb/vbsfmt

## Other links

* Vbsedit VBScript help: https://www.vbsedit.com/html/1c457e66-a6b2-4545-b2dd-33a59d8661e8.asp
* Vbsedit Example vbs files: https://www.vbsedit.com/scripts/default.asp

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

## Making a release

We use https://github.com/MarcoIeni/release-plz which creates a release pr on every commit to master

## Attributions

Thanks go to Domenic Quirl for his excellent blog post on parsing basics. This project was started from his blog post, you can find it here:
https://domenicquirl.github.io/blog/parsing-basics/
https://github.com/domenicquirl/blog/tree/master/parsing-basics
