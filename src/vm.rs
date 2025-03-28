use crate::{
    chunk::{Chunk, OpCode},
    compiler::compile,
    debug::disassemble_instruction,
    value::{StrId, StringInterner, Value},
};
use rustc_hash::FxHashMap;

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
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    debug_trace: bool,
    pub strings: StringInterner,
    pub global_names: FxHashMap<StrId, usize>,
    pub global_values: Vec<Value>,
}
impl Vm {
    pub fn new(debug_trace: bool) -> Self {
        Self {
            chunk: Chunk::new(), // Empty chunk placeholder
            ip: 0,
            stack: Vec::with_capacity(256),
            debug_trace,
            strings: StringInterner::default(),
            global_names: FxHashMap::default(),
            global_values: Vec::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        match compile(source, self, self.debug_trace) {
            Some(chunk) => {
                self.chunk = chunk;
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

        loop {
            if self.debug_trace {
                println!("          {:?}", self.stack);
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.chunk.code[self.ip];
            self.ip += 1;
            match OpCode::from(instruction) {
                OpCode::Return => {
                    // Exit interpreter.
                    return InterpretResult::InterpretOk;
                }
                OpCode::Constant => {
                    let offset = self.get_offset_short();
                    let constant = self.chunk.constants[offset];
                    self.stack.push(constant);
                }
                OpCode::ConstantLong => {
                    let offset = self.get_offset_long();
                    let constant = self.chunk.constants[offset];
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
                OpCode::Print => {
                    if let Some(operand) = self.stack.pop() {
                        match operand {
                            Value::Number(val) => println!("{val}"),
                            Value::Bool(val) => println!("{val}"),
                            Value::String(str_id) => {
                                println!("{}", self.strings.lookup(str_id));
                            }
                            Value::Nil => println!("Nil"),
                            Value::Undefined => println!("Undefined"),
                        }
                    }
                }
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::DefineGlobal => {
                    let offset = self.get_offset_short();
                    self.global_values[offset] = self.stack.pop().expect(
                        "Stack pop will always return Some variant when defining a global.",
                    );
                }
                OpCode::DefineGlobalLong => {
                    let offset = self.get_offset_long();
                    self.global_values[offset] = self.stack.pop().expect(
                        "Stack pop will always return Some variant when defining a global.",
                    );
                }
                OpCode::GetGlobal => {
                    let offset = self.get_offset_short();
                    let value = self.global_values[offset];
                    match value {
                        Value::Undefined => {
                            self.undefined_global_variable(offset);
                            return InterpretResult::InterpretRuntimeError;
                        }
                        _ => (),
                    }
                    self.stack.push(value);
                }
                OpCode::GetGlobalLong => {
                    let offset = self.get_offset_long();
                    let value = self.global_values[offset];
                    match value {
                        Value::Undefined => {
                            self.undefined_global_variable(offset);
                            return InterpretResult::InterpretRuntimeError;
                        }
                        _ => (),
                    }
                    self.stack.push(value);
                }
                OpCode::SetGlobal => {
                    let offset = self.get_offset_short();
                    match self.global_values[offset] {
                        Value::Undefined => {
                            self.undefined_global_variable(offset);
                            return InterpretResult::InterpretRuntimeError;
                        }
                        _ => (),
                    }
                    self.global_values[offset] = self.peek();
                }
                OpCode::SetGlobalLong => {
                    let offset = self.get_offset_long();
                    match self.global_values[offset] {
                        Value::Undefined => {
                            self.undefined_global_variable(offset);
                            return InterpretResult::InterpretRuntimeError;
                        }
                        _ => (),
                    }
                    self.global_values[offset] = self.peek();
                }
                OpCode::GetLocal => {
                    let slot = self.get_offset_short();
                    let value = self.stack[slot];
                    self.stack.push(value);
                }
                OpCode::GetLocalLong => {
                    let slot = self.get_offset_long();
                    let value = self.stack[slot];
                    self.stack.push(value);
                }
                OpCode::SetLocal => {
                    let slot = self.get_offset_short();
                    let value = self.peek();
                    self.stack[slot] = value;
                }
                OpCode::SetLocalLong => {
                    let slot = self.get_offset_long();
                    let value = self.peek();
                    self.stack[slot] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.get_offset_medium();
                    if is_falsey(self.peek()) {
                        self.ip += offset;
                    }
                }
                OpCode::Jump => self.ip += self.get_offset_medium(),
                OpCode::Loop => self.ip -= self.get_offset_medium(),
            }
        }
    }

    fn peek(&self) -> Value {
        self.stack[self.stack.len() - 1]
    }

    /// Reads 1 byte offset operand.
    fn get_offset_short(&mut self) -> usize {
        let offset = self.chunk.code[self.ip] as usize;
        self.ip += 1;
        offset
    }

    /// Reads 2 byte offset operand.
    fn get_offset_medium(&mut self) -> usize {
        let left_byte = self.chunk.code[self.ip] as usize;
        let right_byte = self.chunk.code[self.ip + 1] as usize;
        let offset = (left_byte << 8) + right_byte;
        self.ip += 2;
        offset
    }

    /// Reads 3 byte offset operand.
    fn get_offset_long(&mut self) -> usize {
        let left_byte = self.chunk.code[self.ip] as usize;
        let middle_byte = self.chunk.code[self.ip + 1] as usize;
        let right_byte = self.chunk.code[self.ip + 2] as usize;
        let offset = (left_byte << 16) + (middle_byte << 8) + right_byte;
        self.ip += 3;
        offset
    }

    fn runtime_error(&mut self, err_msg: &str) {
        eprintln!("Runtime Error: {err_msg}");
        let line = self.chunk.get_line(self.ip - 1);
        eprintln!("[line {line}] in script.");
        self.reset_stack();
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn undefined_global_variable(&mut self, offset: usize) {
        for (str_id, val_offset) in self.global_names.iter() {
            if offset == *val_offset {
                let var_name = self.strings.lookup(*str_id);
                self.runtime_error(&format!("Undefined variable {}.", var_name));
                return;
            }
        }
    }
}
