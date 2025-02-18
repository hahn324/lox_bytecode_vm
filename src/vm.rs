use crate::{
    chunk::{Chunk, OpCode},
    compiler::compile,
    debug::disassemble_instruction,
    value::{StringInterner, Value},
};

pub enum InterpretResult {
    InterpretOk,
    InterpretCompileError,
    InterpretRuntimeError,
}

fn is_falsey(value: Value) -> bool {
    match value {
        Value::Nil => true,
        Value::Bool(val) => !val,
        _ => false,
    }
}

pub struct Vm {
    chunk: Option<Chunk>,
    ip: usize,
    stack: Vec<Value>,
    debug_trace: bool,
    strings: StringInterner,
}
impl Vm {
    pub fn new(debug_trace: bool) -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: Vec::with_capacity(256),
            debug_trace,
            strings: StringInterner::default(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        match compile(source, &mut self.strings, self.debug_trace) {
            Some(chunk) => {
                self.chunk = Some(chunk);
                self.ip = 0;
                self.run()
            }
            None => InterpretResult::InterpretCompileError,
        }
    }

    fn run(&mut self) -> InterpretResult {
        macro_rules! binary_op {
            ($value_type:path, $op:tt) => {
                {
                    let b = self.stack.pop().expect("Stack should be non-empty during binary op.");
                    let a = self.stack.pop().expect("Stack should be non-empty during binary op.");
                    match (a, b) {
                        (Value::Number(a_val), Value::Number(b_val)) => self.stack.push($value_type(a_val $op b_val)),
                        _ => {
                            self.runtime_error("Operands must be numbers.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
            };
        }

        if let Some(ref chunk) = self.chunk {
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
                        let constant = chunk.constants[constants_idx].clone();
                        self.ip += 1;
                        self.stack.push(constant);
                    }
                    OpCode::ConstantLong => {
                        let right_byte = chunk.code[self.ip] as usize;
                        let middle_byte = chunk.code[self.ip + 1] as usize;
                        let left_byte = chunk.code[self.ip + 2] as usize;
                        let constants_idx = (right_byte << 16) + (middle_byte << 8) + left_byte;
                        let constant = chunk.constants[constants_idx].clone();
                        self.ip += 3;
                        self.stack.push(constant);
                    }
                    OpCode::Negate => {
                        let operand = self
                            .stack
                            .pop()
                            .expect("Stack should be non-empty during NEGATE op.");
                        match operand {
                            Value::Number(val) => self.stack.push(Value::Number(-val)),
                            _ => {
                                self.runtime_error("Operand must be a number.");
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                    }
                    OpCode::Add => {
                        let b = self
                            .stack
                            .pop()
                            .expect("Stack should be non-empty during binary op.");
                        let a = self
                            .stack
                            .pop()
                            .expect("Stack should be non-empty during binary op.");
                        match (a, b) {
                            (Value::String(a_val), Value::String(b_val)) => {
                                let new_string = format!(
                                    "{}{}",
                                    self.strings.lookup(a_val),
                                    self.strings.lookup(b_val)
                                );
                                let str_id = self.strings.intern(new_string.as_str());
                                self.stack.push(Value::String(str_id));
                            }
                            (Value::Number(a_val), Value::Number(b_val)) => {
                                self.stack.push(Value::Number(a_val + b_val))
                            }
                            _ => {
                                self.runtime_error("Operands must be two numbers or two strings.");
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                    }
                    OpCode::Subtract => binary_op!(Value::Number, -),
                    OpCode::Multiply => binary_op!(Value::Number, *),
                    OpCode::Divide => binary_op!(Value::Number, /),
                    OpCode::Nil => self.stack.push(Value::Nil),
                    OpCode::True => self.stack.push(Value::Bool(true)),
                    OpCode::False => self.stack.push(Value::Bool(false)),
                    OpCode::Not => {
                        let operand = self
                            .stack
                            .pop()
                            .expect("Stack should be non-empty during NOT op.");
                        self.stack.push(Value::Bool(is_falsey(operand)));
                    }
                    OpCode::Equal => {
                        let a = self.stack.pop().unwrap();
                        let b = self.stack.pop().unwrap();
                        self.stack.push(Value::Bool(a.eq(&b)));
                    }
                    OpCode::Greater => binary_op!(Value::Bool, >),
                    OpCode::Less => binary_op!(Value::Bool, <),
                }
            }
        } else {
            return InterpretResult::InterpretRuntimeError;
        }
    }

    fn runtime_error(&self, err_msg: &str) {
        eprintln!("{err_msg}");
        if let Some(ref chunk) = self.chunk {
            let line = chunk.get_line(self.ip - 1);
            eprintln!("[line {line}] in script.");
        }
        // self.reset_stack();
    }
}
