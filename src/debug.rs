use crate::chunk::{Chunk, OpCode};
use crate::value::Value;
use crate::vm::Vm;

pub fn disassemble_chunk(chunk: &Chunk, name: &str, vm: &Vm) {
    println!("== {name} ==");

    let mut ip = 0;
    while ip < chunk.code.len() {
        ip = disassemble_instruction(chunk, ip, vm);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, mut ip: usize, vm: &Vm) -> usize {
    print!("{ip:04} ");
    if ip > 0 && chunk.get_line(ip) == chunk.get_line(ip - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.get_line(ip));
    }

    let instruction = chunk.code[ip];
    match OpCode::from(instruction) {
        OpCode::Return => simple_instruction("OP_RETURN", ip),
        OpCode::Constant => offset_instruction("OP_CONSTANT", chunk, ip, InstructionType::Load, vm),
        OpCode::ConstantLong => {
            offset_long_instruction("OP_CONSTANTLONG", chunk, ip, InstructionType::Load, vm)
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
            offset_instruction("OP_DEFINE_GLOBAL", chunk, ip, InstructionType::Global, vm)
        }
        OpCode::DefineGlobalLong => offset_long_instruction(
            "OP_DEFINE_GLOBAL_LONG",
            chunk,
            ip,
            InstructionType::Global,
            vm,
        ),
        OpCode::GetGlobal => {
            offset_instruction("OP_GET_GLOBAL", chunk, ip, InstructionType::Global, vm)
        }
        OpCode::GetGlobalLong => {
            offset_long_instruction("OP_GET_GLOBAL_LONG", chunk, ip, InstructionType::Global, vm)
        }
        OpCode::SetGlobal => {
            offset_instruction("OP_SET_GLOBAL", chunk, ip, InstructionType::Global, vm)
        }
        OpCode::SetGlobalLong => {
            offset_long_instruction("OP_SET_GLOBAL_LONG", chunk, ip, InstructionType::Global, vm)
        }
        OpCode::GetLocal => {
            offset_instruction("OP_GET_LOCAL", chunk, ip, InstructionType::Local, vm)
        }
        OpCode::GetLocalLong => {
            offset_long_instruction("OP_GET_LOCAL_LONG", chunk, ip, InstructionType::Local, vm)
        }
        OpCode::SetLocal => {
            offset_instruction("OP_SET_LOCAL", chunk, ip, InstructionType::Local, vm)
        }
        OpCode::SetLocalLong => {
            offset_long_instruction("OP_SET_LOCAL_LONG", chunk, ip, InstructionType::Local, vm)
        }
        OpCode::JumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", false, chunk, ip),
        OpCode::Jump => jump_instruction("OP_JUMP", false, chunk, ip),
        OpCode::Loop => jump_instruction("OP_LOOP", true, chunk, ip),
        OpCode::Call => offset_instruction("OP_CALL", chunk, ip, InstructionType::Call, vm),
        OpCode::Closure => {
            let offset = chunk.code[ip + 1] as usize;
            print!("{:<16} {offset:4} '", "OP_CLOSURE");
            let function = &chunk.constants[offset];
            vm.print_value(function);
            print!("'\n");
            ip += 2;
            if let Value::Function(function) = function {
                for _ in 0..function.upvalue_count {
                    let upvalue_type = if chunk.code[ip + 1] == 1 {
                        "local"
                    } else {
                        "upvalue"
                    };
                    let index = chunk.code[ip + 2];
                    println!("{ip:04}      |                     {upvalue_type} {index}");
                    ip += 2;
                }
            }
            ip
        }
        OpCode::ClosureLong => {
            let left_byte = chunk.code[ip + 1] as usize;
            let middle_byte = chunk.code[ip + 2] as usize;
            let right_byte = chunk.code[ip + 3] as usize;
            let offset = (left_byte << 16) + (middle_byte << 8) + right_byte;
            print!("{:<16} {offset:4} '", "OP_CLOSURE_LONG");
            let function = &chunk.constants[offset];
            vm.print_value(function);
            print!("'\n");
            ip += 4;
            if let Value::Function(function) = function {
                for _ in 0..function.upvalue_count {
                    let upvalue_type = if chunk.code[ip + 1] == 1 {
                        "local"
                    } else {
                        "upvalue"
                    };
                    let index = chunk.code[ip + 2];
                    println!("{ip:04}      |                     {upvalue_type} {index}");
                    ip += 2;
                }
            }
            ip
        }
        OpCode::GetUpvalue => {
            offset_instruction("OP_GET_UPVALUE", chunk, ip, InstructionType::UpValue, vm)
        }
        OpCode::GetUpvalueLong => offset_long_instruction(
            "OP_GET_UPVALUE_LONG",
            chunk,
            ip,
            InstructionType::UpValue,
            vm,
        ),
        OpCode::SetUpvalue => {
            offset_instruction("OP_SET_UPVALUE", chunk, ip, InstructionType::UpValue, vm)
        }
        OpCode::SetUpvalueLong => offset_long_instruction(
            "OP_SET_UPVALUE_LONG",
            chunk,
            ip,
            InstructionType::UpValue,
            vm,
        ),
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
    UpValue,
}

fn offset_instruction(
    name: &str,
    chunk: &Chunk,
    ip: usize,
    instruction_type: InstructionType,
    vm: &Vm,
) -> usize {
    let offset = chunk.code[ip + 1] as usize;
    print!("{name:<16} {offset:4} '");
    match instruction_type {
        InstructionType::Load => vm.print_value(&chunk.constants[offset]),
        InstructionType::Global => {
            for (key, val) in vm.global_names.iter() {
                if *val == offset {
                    vm.print_value(&Value::String(*key));
                    break;
                }
            }
        }
        InstructionType::Local | InstructionType::Call | InstructionType::UpValue => (),
    }
    print!("'\n");
    ip + 2
}

fn offset_long_instruction(
    name: &str,
    chunk: &Chunk,
    ip: usize,
    instruction_type: InstructionType,
    vm: &Vm,
) -> usize {
    let left_byte = chunk.code[ip + 1] as usize;
    let middle_byte = chunk.code[ip + 2] as usize;
    let right_byte = chunk.code[ip + 3] as usize;
    let offset = (left_byte << 16) + (middle_byte << 8) + right_byte;
    print!("{name:<16} {offset:4} '");
    match instruction_type {
        InstructionType::Load => vm.print_value(&chunk.constants[offset]),
        InstructionType::Global => {
            for (key, val) in vm.global_names.iter() {
                if *val == offset {
                    vm.print_value(&Value::String(*key));
                    break;
                }
            }
        }
        InstructionType::Local | InstructionType::Call | InstructionType::UpValue => (),
    }
    print!("'\n");
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
