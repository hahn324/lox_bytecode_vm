use crate::{
    chunk::{Chunk, OpCode},
    compiler::compile,
    debug::disassemble_instruction,
    value::Value,
};

pub enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

pub struct Vm<'c> {
    chunk: Option<&'c Chunk>,
    ip: usize,
    stack: Vec<Value>,
    debug_trace: bool,
}
impl<'c> Vm<'c> {
    pub fn new(debug_trace: bool) -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: Vec::with_capacity(256),
            debug_trace,
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        compile(source);
        InterpretResult::InterpretOk
    }

    fn run(&mut self) -> InterpretResult {
        macro_rules! binary_op {
            ($op:tt) => {
                {
                    let b = self.stack.pop().expect("Stack should be non-empty during binary op.");
                    let a = self.stack.pop().expect("Stack should be non-empty during binary op.");
                    self.stack.push(a $op b);
                }
            };
        }

        if let Some(chunk) = self.chunk {
            loop {
                if self.debug_trace {
                    println!("          {:?}", self.stack);
                    disassemble_instruction(chunk, self.ip);
                }
                let instruction = chunk.code[self.ip];
                self.ip += 1;
                match OpCode::from(instruction) {
                    OpCode::Return => {
                        println!("{:?}", self.stack.pop());
                        return InterpretResult::InterpretOk;
                    }
                    OpCode::Constant => {
                        let constants_idx = chunk.code[self.ip] as usize;
                        let constant = chunk.constants[constants_idx];
                        self.ip += 1;
                        self.stack.push(constant);
                    }
                    OpCode::ConstantLong => {
                        let right_byte = chunk.code[self.ip] as usize;
                        let middle_byte = chunk.code[self.ip + 1] as usize;
                        let left_byte = chunk.code[self.ip + 2] as usize;
                        let constants_idx = (right_byte << 16) + (middle_byte << 8) + left_byte;
                        let constant = chunk.constants[constants_idx];
                        self.ip += 3;
                        self.stack.push(constant);
                    }
                    OpCode::Negate => {
                        let operand = self
                            .stack
                            .pop()
                            .expect("Stack should be non-empty during negate op.");
                        self.stack.push(-operand);
                    }
                    OpCode::Add => binary_op!(+),
                    OpCode::Subtract => binary_op!(-),
                    OpCode::Multiply => binary_op!(*),
                    OpCode::Divide => binary_op!(/),
                }
            }
        } else {
            return InterpretResult::InterpretRuntimeError;
        }
    }
}
