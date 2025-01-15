use crate::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");

    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{offset:04} ");
    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.get_line(offset));
    }

    let instruction = chunk.code[offset];
    match OpCode::from(instruction) {
        OpCode::OpReturn => simple_instruction("OpReturn", offset),
        OpCode::OpConstant => constant_instruction("OpConstant", chunk, offset),
        OpCode::OpConstantLong => constant_long_instruction("OpConstantLong", chunk, offset),
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant = chunk.code[offset + 1];
    println!(
        "{name:<16} {constant:4} '{:?}'",
        chunk.constants[constant as usize]
    );
    offset + 2
}

fn constant_long_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let right_byte = chunk.code[offset + 1];
    let middle_byte = chunk.code[offset + 2];
    let left_byte = chunk.code[offset + 3];
    let constant = ((right_byte as u32) << 16) + ((middle_byte as u32) << 8) + left_byte as u32;
    println!(
        "{name:<16} {constant:4} '{:?}'",
        chunk.constants[constant as usize]
    );
    offset + 4
}
