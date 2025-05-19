use std::time::Duration;

use criterion::{BatchSize, Criterion, Throughput, criterion_group, criterion_main};
use indoc::indoc;
use vbscript::lexer::Lexer;
use vbscript::parser::Parser;

pub fn lex_function(c: &mut Criterion) {
    let input = indoc! { r#"
        'tests stuff
        Function foo(bar)
            foo = bar + 1
        End Function
    "#};
    bench_lexer(c, "function", input);
}

pub fn lex_class(c: &mut Criterion) {
    let input = indoc! { r#"
        Class foo
            Public Sub bar()
                foo = bar(1)
            End Sub
            Public Function bar(bar)
                bar = bar + 1
            End Function
        End Class

        Dim obj
        Set obj = New foo
        obj.bar
        Dim x
        x = obj.bar(1)
    "#};
    bench_lexer(c, "class", input);
}

pub fn lex_script(c: &mut Criterion) {
    let input = indoc! {r#"
        'tests stuff
        Function foo(bar)
            foo = bar + 1
        End Function

        Sub bar()
            foo = foo(1)
        End Sub

        Dim x
        x = 1

        If x = 1 Then
            bar()
        End If

        Do While x < 10
            x = x + 1
        Loop
    "#};
    bench_lexer(c, "script", input);
}

pub fn parse_complex_expression(c: &mut Criterion) {
    let input = indoc! {r#"
        x = a(1,2).b.c("test").d(1).e & "test" & foo(1,2).bar.baz(1).qux
    "#};
    bench_parser(c, "complex_expression", input);
}

fn bench_lexer(c: &mut Criterion, name: &str, input: &str) {
    // Lexing: measured in bytes
    let mut group = c.benchmark_group("lexer");
    group.measurement_time(Duration::from_millis(7500));

    // To measure throughput, we need to tell `criterion`
    // how big our input is.
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_with_input(name, input, |b, input| {
        b.iter_batched(
            || Lexer::new(input),         // <- Our lexer is made HERE
            |mut lexer| lexer.tokenize(), // <- and runs HERE
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

fn bench_parser(c: &mut Criterion, name: &str, input: &str) {
    // Lexing: measured in bytes
    let mut group = c.benchmark_group("parser");
    group.measurement_time(Duration::from_millis(7500));

    // To measure throughput, we need to tell `criterion`
    // how big our input is.
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_with_input(name, input, |b, input| {
        b.iter_batched(
            || Parser::new(input),      // <- Our lexer is made HERE
            |mut parser| parser.file(), // <- and runs HERE
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

criterion_group!(
    benches,
    lex_function,
    lex_script,
    lex_class,
    parse_complex_expression
);
criterion_main!(benches);
