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
        OpCode::Nil => simple_instruction("OP_NIL", offset),
        OpCode::True => simple_instruction("OP_TRUE", offset),
        OpCode::False => simple_instruction("OP_FALSE", offset),
        OpCode::Not => simple_instruction("OP_NOT", offset),
        OpCode::Equal => simple_instruction("OP_EQUAL", offset),
        OpCode::Greater => simple_instruction("OP_GREATER", offset),
        OpCode::Less => simple_instruction("OP_LESS", offset),
        OpCode::Print => simple_instruction("OP_PRINT", offset),
        OpCode::Pop => simple_instruction("OP_POP", offset),
        OpCode::DefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", chunk, offset),
        OpCode::DefineGlobalLong => {
            constant_long_instruction("OP_DEFINE_GLOBAL_LONG", chunk, offset)
        }
        OpCode::GetGlobal => constant_instruction("OP_GET_GLOBAL", chunk, offset),
        OpCode::GetGlobalLong => constant_long_instruction("OP_GET_GLOBAL_LONG", chunk, offset),
        OpCode::SetGlobal => constant_instruction("OP_SET_GLOBAL", chunk, offset),
        OpCode::SetGlobalLong => constant_long_instruction("OP_SET_GLOBAL_LONG", chunk, offset),
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
