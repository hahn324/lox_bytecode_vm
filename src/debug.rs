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
        OpCode::Constant => offset_instruction("OP_CONSTANT", chunk, ip, InstructionType::Load),
        OpCode::ConstantLong => {
            offset_long_instruction("OP_CONSTANTLONG", chunk, ip, InstructionType::Load)
        }
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
        OpCode::DefineGlobal => {
            offset_instruction("OP_DEFINE_GLOBAL", chunk, ip, InstructionType::Global)
        }
        OpCode::DefineGlobalLong => {
            offset_long_instruction("OP_DEFINE_GLOBAL_LONG", chunk, ip, InstructionType::Global)
        }
        OpCode::GetGlobal => {
            offset_instruction("OP_GET_GLOBAL", chunk, ip, InstructionType::Global)
        }
        OpCode::GetGlobalLong => {
            offset_long_instruction("OP_GET_GLOBAL_LONG", chunk, ip, InstructionType::Global)
        }
        OpCode::SetGlobal => {
            offset_instruction("OP_SET_GLOBAL", chunk, ip, InstructionType::Global)
        }
        OpCode::SetGlobalLong => {
            offset_long_instruction("OP_SET_GLOBAL_LONG", chunk, ip, InstructionType::Global)
        }
        OpCode::GetLocal => offset_instruction("OP_GET_LOCAL", chunk, ip, InstructionType::Local),
        OpCode::GetLocalLong => {
            offset_instruction("OP_GET_LOCAL_LONG", chunk, ip, InstructionType::Local)
        }
        OpCode::SetLocal => offset_instruction("OP_SET_LOCAL", chunk, ip, InstructionType::Local),
        OpCode::SetLocalLong => {
            offset_instruction("OP_SET_LOCAL_LONG", chunk, ip, InstructionType::Local)
        }
        OpCode::JumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", false, chunk, ip),
        OpCode::Jump => jump_instruction("OP_JUMP", false, chunk, ip),
        OpCode::Loop => jump_instruction("OP_LOOP", true, chunk, ip),
        OpCode::Call => offset_instruction("OP_CALL", chunk, ip, InstructionType::Call),
    }
}

fn simple_instruction(name: &str, ip: usize) -> usize {
    println!("{name}");
    ip + 1
}

enum InstructionType {
    Global,
    Local,
    Load,
    Call,
}

fn offset_instruction(
    name: &str,
    chunk: &Chunk,
    ip: usize,
    instruction_type: InstructionType,
) -> usize {
    let offset = chunk.code[ip + 1];
    match instruction_type {
        InstructionType::Load => {
            println!(
                "{name:<16} {offset:4} '{:?}'",
                chunk.constants[offset as usize]
            )
        }
        InstructionType::Global => println!("{name:<16} Global({offset})",),
        InstructionType::Local => println!("{name:<16} Local({offset})",),
        InstructionType::Call => println!("{name:<16} {offset:4}",),
    }
    ip + 2
}

fn offset_long_instruction(
    name: &str,
    chunk: &Chunk,
    ip: usize,
    instruction_type: InstructionType,
) -> usize {
    let left_byte = chunk.code[ip + 1] as usize;
    let middle_byte = chunk.code[ip + 2] as usize;
    let right_byte = chunk.code[ip + 3] as usize;
    let offset = (left_byte << 16) + (middle_byte << 8) + right_byte;
    match instruction_type {
        InstructionType::Load => {
            println!("{name:<16} {offset:4} '{:?}'", chunk.constants[offset])
        }
        InstructionType::Global => println!("{name:<16} {ip:4} Global({offset})",),
        InstructionType::Local => println!("{name:<16} {ip:4} Local({offset})",),
        InstructionType::Call => println!("{name:<16} {offset:4}",),
    }
    ip + 4
}

fn jump_instruction(name: &str, back: bool, chunk: &Chunk, ip: usize) -> usize {
    let left_byte = chunk.code[ip + 1] as usize;
    let right_byte = chunk.code[ip + 2] as usize;
    let mut jump = ((left_byte << 8) + right_byte) as i32;
    if back {
        jump = jump * -1;
    }
    println!("{name:<16} {ip:4} -> {}", ip as i32 + 3 + jump);
    ip + 3
}
