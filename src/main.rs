use lox_bytecode_vm::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
};

fn main() {
    let mut chunk = Chunk::new();
    for _ in 0..=256 {
        chunk.write_constant(1.2, 1);
    }
    chunk.write_chunk(OpCode::OpReturn as u8, 2);
    disassemble_chunk(&chunk, "test chunk");
}
