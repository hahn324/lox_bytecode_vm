use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use lox_bytecode_vm::vm::Vm;
use std::{fs, path::PathBuf};

fn run_benchmark(file_name: &str, c: &mut Criterion) {
    let file_path: PathBuf = ["benches", "scripts", file_name].iter().collect();
    let source = fs::read_to_string(&file_path).unwrap();
    let mut vm = Vm::new(false);
    c.bench_with_input(
        BenchmarkId::new(&file_name[..file_name.find('.').unwrap()], file_name),
        &source,
        |b, i| b.iter(|| vm.interpret(i)),
    );
}

fn binary_trees(c: &mut Criterion) {
    run_benchmark("binary_trees.lox", c);
}

fn equality(c: &mut Criterion) {
    run_benchmark("equality.lox", c);
}

fn fib(c: &mut Criterion) {
    run_benchmark("fib.lox", c);
}

fn instantiation(c: &mut Criterion) {
    run_benchmark("instantiation.lox", c);
}

fn invocation(c: &mut Criterion) {
    run_benchmark("invocation.lox", c);
}

fn method_call(c: &mut Criterion) {
    run_benchmark("method_call.lox", c);
}

fn properties(c: &mut Criterion) {
    run_benchmark("properties.lox", c);
}

fn string_equality(c: &mut Criterion) {
    run_benchmark("string_equality.lox", c);
}

fn trees(c: &mut Criterion) {
    run_benchmark("trees.lox", c);
}

fn zoo(c: &mut Criterion) {
    run_benchmark("zoo.lox", c);
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = binary_trees, equality, fib, instantiation, invocation, method_call, properties, string_equality, trees, zoo
}
criterion_main!(benches);
