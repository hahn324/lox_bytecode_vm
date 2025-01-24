use lox_bytecode_vm::{
    chunk::{Chunk, OpCode},
    vm::Vm,
};
use std::env;

fn main() {
    let debug_flag = match env::args().nth(1) {
        Some(val) => val == "--debug",
        None => false,
    };
    let mut vm = Vm::new(debug_flag);
    let mut chunk = Chunk::new();
    chunk.write_constant(1.2, 1);
    chunk.write_constant(3.4, 1);
    chunk.write_chunk(OpCode::Add as u8, 1);
    chunk.write_constant(5.6, 1);
    chunk.write_chunk(OpCode::Divide as u8, 1);
    chunk.write_chunk(OpCode::Negate as u8, 1);

    chunk.write_chunk(OpCode::Return as u8, 2);
    vm.interpret(&chunk);
}
