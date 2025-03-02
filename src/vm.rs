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
    chunk: Option<Chunk>,
    ip: usize,
    stack: Vec<Value>,
    debug_trace: bool,
    strings: StringInterner,
    globals: FxHashMap<StrId, Value>,
}
impl Vm {
    pub fn new(debug_trace: bool) -> Self {
        Self {
            chunk: None,
            ip: 0,
            stack: Vec::with_capacity(256),
            debug_trace,
            strings: StringInterner::default(),
            globals: FxHashMap::default(),
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

        if let Some(chunk) = self.chunk.take() {
            loop {
                if self.debug_trace {
                    println!("          {:?}", self.stack);
                    disassemble_instruction(&chunk, self.ip);
                }
                let instruction = chunk.code[self.ip];
                self.ip += 1;
                match OpCode::from(instruction) {
                    OpCode::Return => {
                        // Exit interpreter.
                        return InterpretResult::InterpretOk;
                    }
                    OpCode::Constant => {
                        let constant = self.get_constant(&chunk);
                        self.stack.push(constant);
                    }
                    OpCode::ConstantLong => {
                        let constant = self.get_constant_long(&chunk);
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
                            }
                        }
                    }
                    OpCode::Pop => {
                        self.stack.pop();
                    }
                    OpCode::DefineGlobal => {
                        let constant = self.get_constant(&chunk);
                        let str_id = match constant {
                            Value::String(id) => id,
                            _ => unreachable!("Will only ever be a Value::String variant."),
                        };
                        let value = self.stack[self.stack.len() - 1];
                        self.globals.insert(str_id, value);
                        self.stack.pop();
                    }
                    OpCode::DefineGlobalLong => {
                        let constant = self.get_constant_long(&chunk);
                        let str_id = match constant {
                            Value::String(id) => id,
                            _ => unreachable!("Will only ever be a Value::String variant."),
                        };
                        let value = self.stack[self.stack.len() - 1];
                        self.globals.insert(str_id, value);
                        self.stack.pop();
                    }
                    OpCode::GetGlobal => {
                        let constant = self.get_constant(&chunk);
                        let str_id = match constant {
                            Value::String(id) => id,
                            _ => unreachable!("Will only ever be a Value::String variant."),
                        };
                        let value = match self.globals.get(&str_id) {
                            Some(value) => *value,
                            None => {
                                let var_name = self.strings.lookup(str_id);
                                self.runtime_error(&format!("Undefined variable {}.", var_name));
                                return InterpretResult::InterpretRuntimeError;
                            }
                        };
                        self.stack.push(value);
                    }
                    OpCode::GetGlobalLong => {
                        let constant = self.get_constant_long(&chunk);
                        let str_id = match constant {
                            Value::String(id) => id,
                            _ => unreachable!("Will only ever be a Value::String variant."),
                        };
                        let value = match self.globals.get(&str_id) {
                            Some(value) => *value,
                            None => {
                                let var_name = self.strings.lookup(str_id);
                                self.runtime_error(&format!("Undefined variable {}.", var_name));
                                return InterpretResult::InterpretRuntimeError;
                            }
                        };
                        self.stack.push(value);
                    }
                    OpCode::SetGlobal => {
                        let constant = self.get_constant(&chunk);
                        let str_id = match constant {
                            Value::String(id) => id,
                            _ => unreachable!("Will only ever be a Value::String variant."),
                        };
                        let value = self.stack[self.stack.len() - 1];
                        match self.globals.insert(str_id, value) {
                            Some(_) => (),
                            None => {
                                self.globals.remove(&str_id);
                                let var_name = self.strings.lookup(str_id);
                                self.runtime_error(&format!("Undefined variable {}.", var_name));
                                // NOTE: Value on stack will not get popped since returning before
                                // OpCode::Pop op for expression statement... Memory leak in REPL?
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                    }
                    OpCode::SetGlobalLong => {
                        let constant = self.get_constant_long(&chunk);
                        let str_id = match constant {
                            Value::String(id) => id,
                            _ => unreachable!("Will only ever be a Value::String variant."),
                        };
                        let value = self.stack[self.stack.len() - 1];
                        match self.globals.insert(str_id, value) {
                            Some(_) => (),
                            None => {
                                self.globals.remove(&str_id);
                                let var_name = self.strings.lookup(str_id);
                                self.runtime_error(&format!("Undefined variable {}.", var_name));
                                // NOTE: Value on stack will not get popped since returning before
                                // OpCode::Pop op for expression statement... Memory leak in REPL?
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                    }
                }
            }
        } else {
            return InterpretResult::InterpretRuntimeError;
        }
    }

    fn get_constant(&mut self, chunk: &Chunk) -> Value {
        let constants_idx = chunk.code[self.ip] as usize;
        let constant = chunk.constants[constants_idx];
        self.ip += 1;
        constant
    }

    fn get_constant_long(&mut self, chunk: &Chunk) -> Value {
        let right_byte = chunk.code[self.ip] as usize;
        let middle_byte = chunk.code[self.ip + 1] as usize;
        let left_byte = chunk.code[self.ip + 2] as usize;
        let constants_idx = (right_byte << 16) + (middle_byte << 8) + left_byte;
        let constant = chunk.constants[constants_idx];
        self.ip += 3;
        constant
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
