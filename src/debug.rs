use crate::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");

    let mut ip = 0;
    while ip < chunk.code.len() {
        ip = disassemble_instruction(chunk, ip);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, ip: usize) -> usize {
    print!("{ip:04} ");
    if ip > 0 && chunk.get_line(ip) == chunk.get_line(ip - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.get_line(ip));
    }

    let instruction = chunk.code[ip];
    match OpCode::from(instruction) {
        OpCode::Return => simple_instruction("OP_RETURN", ip),
        OpCode::Constant => constant_instruction("OP_CONSTANT", chunk, ip, false),
        OpCode::ConstantLong => constant_long_instruction("OP_CONSTANTLONG", chunk, ip, false),
        OpCode::Negate => simple_instruction("OP_NEGATE", ip),
        OpCode::Add => simple_instruction("OP_ADD", ip),
        OpCode::Subtract => simple_instruction("OP_SUBTRACT", ip),
        OpCode::Multiply => simple_instruction("OP_MULTIPLY", ip),
        OpCode::Divide => simple_instruction("OP_DIVIDE", ip),
        OpCode::Nil => simple_instruction("OP_NIL", ip),
        OpCode::True => simple_instruction("OP_TRUE", ip),
        OpCode::False => simple_instruction("OP_FALSE", ip),
        OpCode::Not => simple_instruction("OP_NOT", ip),
        OpCode::Equal => simple_instruction("OP_EQUAL", ip),
        OpCode::Greater => simple_instruction("OP_GREATER", ip),
        OpCode::Less => simple_instruction("OP_LESS", ip),
        OpCode::Print => simple_instruction("OP_PRINT", ip),
        OpCode::Pop => simple_instruction("OP_POP", ip),
        OpCode::DefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", chunk, ip, true),
        OpCode::DefineGlobalLong => {
            constant_long_instruction("OP_DEFINE_GLOBAL_LONG", chunk, ip, true)
        }
        OpCode::GetGlobal => constant_instruction("OP_GET_GLOBAL", chunk, ip, true),
        OpCode::GetGlobalLong => constant_long_instruction("OP_GET_GLOBAL_LONG", chunk, ip, true),
        OpCode::SetGlobal => constant_instruction("OP_SET_GLOBAL", chunk, ip, true),
        OpCode::SetGlobalLong => constant_long_instruction("OP_SET_GLOBAL_LONG", chunk, ip, true),
    }
}

fn simple_instruction(name: &str, ip: usize) -> usize {
    println!("{name}");
    ip + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, ip: usize, is_global: bool) -> usize {
    let offset = chunk.code[ip + 1];
    if is_global {
        println!("{name:<16} {offset:4} Global({offset})",);
    } else {
        println!(
            "{name:<16} {offset:4} '{:?}'",
            chunk.constants[offset as usize]
        );
    }
    ip + 2
}

fn constant_long_instruction(name: &str, chunk: &Chunk, ip: usize, is_global: bool) -> usize {
    let right_byte = chunk.code[ip + 1] as usize;
    let middle_byte = chunk.code[ip + 2] as usize;
    let left_byte = chunk.code[ip + 3] as usize;
    let offset = (right_byte << 16) + (middle_byte << 8) + left_byte;
    if is_global {
        println!("{name:<16} {offset:4} Global({offset})",);
    } else {
        println!("{name:<16} {offset:4} '{:?}'", chunk.constants[offset]);
    }
    ip + 4
}
