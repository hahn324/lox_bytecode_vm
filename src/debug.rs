use crate::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");

    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{offset:04} ");
    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.get_line(offset));
    }

    let instruction = chunk.code[offset];
    match OpCode::from(instruction) {
        OpCode::Return => simple_instruction("OP_RETURN", offset),
        OpCode::Constant => constant_instruction("OP_CONSTANT", chunk, offset),
        OpCode::ConstantLong => constant_long_instruction("OP_CONSTANTLONG", chunk, offset),
        OpCode::Negate => simple_instruction("OP_NEGATE", offset),
        OpCode::Add => simple_instruction("OP_ADD", offset),
        OpCode::Subtract => simple_instruction("OP_SUBTRACT", offset),
        OpCode::Multiply => simple_instruction("OP_MULTIPLY", offset),
        OpCode::Divide => simple_instruction("OP_DIVIDE", offset),
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
    let right_byte = chunk.code[offset + 1] as usize;
    let middle_byte = chunk.code[offset + 2] as usize;
    let left_byte = chunk.code[offset + 3] as usize;
    let constant = (right_byte << 16) + (middle_byte << 8) + left_byte;
    println!("{name:<16} {constant:4} '{:?}'", chunk.constants[constant]);
    offset + 4
}
