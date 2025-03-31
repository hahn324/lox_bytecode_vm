use crate::{
    chunk::OpCode,
    compiler::compile,
    debug::disassemble_instruction,
    value::{LoxFunction, StrId, StringInterner, Value},
};
use rustc_hash::FxHashMap;
use std::rc::Rc;
use std::time::SystemTime;

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * u8::MAX as usize;

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

#[derive(Debug, Clone, Default)]
struct CallFrame {
    function: Rc<LoxFunction>,
    ip: usize,
    frame_ptr: usize,
}

pub struct Vm {
    frames: [Option<CallFrame>; FRAMES_MAX],
    frame_count: usize,
    stack: [Value; STACK_MAX],
    stack_count: usize,
    debug_trace: bool,
    pub strings: StringInterner,
    pub global_names: FxHashMap<StrId, usize>,
    pub global_values: Vec<Value>,
}
impl Vm {
    pub fn new(debug_trace: bool) -> Self {
        let mut vm = Self {
            frames: [const { None }; FRAMES_MAX],
            frame_count: 0,
            stack: [const { Value::Undefined }; STACK_MAX],
            stack_count: 0,
            debug_trace,
            strings: StringInterner::default(),
            global_names: FxHashMap::default(),
            global_values: Vec::new(),
        };
        // Initialize global native functions
        vm.define_native("clock", clock_native);

        vm
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        match compile(source, self, self.debug_trace) {
            Some(function) => {
                if self.debug_trace {
                    println!("== String Map ==");
                    println!("{:?}", self.strings.id_map);
                }
                self.push(Value::Function(Rc::clone(&function)));
                self.frames[0] = Some(CallFrame {
                    function,
                    ip: 0,
                    frame_ptr: 0,
                });
                self.frame_count += 1;
                self.run()
            }
            None => InterpretResult::InterpretCompileError,
        }
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_count] = value;
        self.stack_count += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_count -= 1;
        self.stack[self.stack_count].clone()
    }

    fn run(&mut self) -> InterpretResult {
        macro_rules! binary_op {
            ($frame:ident,$value_type:path, $op:tt) => {
                {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::Number(a_val), Value::Number(b_val)) => self.push($value_type(a_val $op b_val)),
                        _ => {
                            self.runtime_error(&$frame, "Operands must be numbers.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
            };
        }

        let mut current_frame = self.frames[self.frame_count - 1]
            .take()
            .expect("Will always be Some(CallFrame).");

        loop {
            if self.debug_trace {
                println!("          {:?}", &self.stack[1..self.stack_count]);
                disassemble_instruction(&current_frame.function.chunk, current_frame.ip);
            }
            let instruction = current_frame.function.chunk.code[current_frame.ip];
            current_frame.ip += 1;
            match OpCode::from(instruction) {
                OpCode::Return => {
                    let result = self.pop();
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return InterpretResult::InterpretOk;
                    }

                    self.stack_count = current_frame.frame_ptr;
                    self.push(result);
                    current_frame = self.frames[self.frame_count - 1]
                        .take()
                        .expect("Will always be Some(CallFrame).");
                }
                OpCode::Constant => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let constant = current_frame.function.chunk.constants[offset].clone();
                    self.push(constant);
                }
                OpCode::ConstantLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let constant = current_frame.function.chunk.constants[offset].clone();
                    self.push(constant);
                }
                OpCode::Negate => {
                    let operand = self.pop();
                    match operand {
                        Value::Number(val) => self.push(Value::Number(-val)),
                        _ => {
                            self.runtime_error(&current_frame, "Operand must be a number.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpCode::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    match (a, b) {
                        (Value::String(a_val), Value::String(b_val)) => {
                            let new_string = format!(
                                "{}{}",
                                self.strings.lookup(a_val),
                                self.strings.lookup(b_val)
                            );
                            let str_id = self.strings.intern(new_string.as_str());
                            self.push(Value::String(str_id));
                        }
                        (Value::Number(a_val), Value::Number(b_val)) => {
                            self.push(Value::Number(a_val + b_val))
                        }
                        _ => {
                            self.runtime_error(
                                &current_frame,
                                "Operands must be two numbers or two strings.",
                            );
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpCode::Subtract => binary_op!(current_frame, Value::Number, -),
                OpCode::Multiply => binary_op!(current_frame, Value::Number, *),
                OpCode::Divide => binary_op!(current_frame, Value::Number, /),
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Not => {
                    let operand = self.pop();
                    self.push(Value::Bool(is_falsey(operand)));
                }
                OpCode::Equal => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(Value::Bool(a.eq(&b)));
                }
                OpCode::Greater => binary_op!(current_frame, Value::Bool, >),
                OpCode::Less => binary_op!(current_frame, Value::Bool, <),
                OpCode::Print => match self.pop() {
                    Value::Number(val) => println!("{val}"),
                    Value::Bool(val) => println!("{val}"),
                    Value::String(str_id) => {
                        println!("{}", self.strings.lookup(str_id));
                    }
                    Value::Nil => println!("Nil"),
                    Value::Undefined => println!("Undefined"),
                    Value::Function(lox_fun) => match lox_fun.name {
                        Some(name_id) => {
                            println!("<fn {}>", self.strings.lookup(name_id))
                        }
                        None => println!("<script>"),
                    },
                    Value::Native(_) => println!("<native fn>"),
                },
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    self.global_values[offset] = self.pop();
                }
                OpCode::DefineGlobalLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    self.global_values[offset] = self.pop();
                }
                OpCode::GetGlobal => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let value = self.global_values[offset].clone();
                    match value {
                        Value::Undefined => {
                            self.undefined_global_variable(&current_frame, offset);
                            return InterpretResult::InterpretRuntimeError;
                        }
                        _ => (),
                    }
                    self.push(value);
                }
                OpCode::GetGlobalLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let value = self.global_values[offset].clone();
                    match value {
                        Value::Undefined => {
                            self.undefined_global_variable(&current_frame, offset);
                            return InterpretResult::InterpretRuntimeError;
                        }
                        _ => (),
                    }
                    self.push(value);
                }
                OpCode::SetGlobal => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    match self.global_values[offset] {
                        Value::Undefined => {
                            self.undefined_global_variable(&current_frame, offset);
                            return InterpretResult::InterpretRuntimeError;
                        }
                        _ => (),
                    }
                    self.global_values[offset] = self.peek(0);
                }
                OpCode::SetGlobalLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    match self.global_values[offset] {
                        Value::Undefined => {
                            self.undefined_global_variable(&current_frame, offset);
                            return InterpretResult::InterpretRuntimeError;
                        }
                        _ => (),
                    }
                    self.global_values[offset] = self.peek(0);
                }
                OpCode::GetLocal => {
                    let slot = Vm::get_offset_short(&mut current_frame);
                    let value = self.stack[current_frame.frame_ptr + slot].clone();
                    self.push(value);
                }
                OpCode::GetLocalLong => {
                    let slot = Vm::get_offset_long(&mut current_frame);
                    let value = self.stack[current_frame.frame_ptr + slot].clone();
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let slot = Vm::get_offset_short(&mut current_frame);
                    let value = self.peek(0);
                    self.stack[current_frame.frame_ptr + slot] = value;
                }
                OpCode::SetLocalLong => {
                    let slot = Vm::get_offset_long(&mut current_frame);
                    let value = self.peek(0);
                    self.stack[current_frame.frame_ptr + slot] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = Vm::get_offset_medium(&mut current_frame);
                    if is_falsey(self.peek(0)) {
                        current_frame.ip += offset;
                    }
                }
                OpCode::Jump => current_frame.ip += Vm::get_offset_medium(&mut current_frame),
                OpCode::Loop => current_frame.ip -= Vm::get_offset_medium(&mut current_frame),
                OpCode::Call => {
                    let arg_count = Vm::get_offset_short(&mut current_frame);
                    if !self.call_value(self.peek(arg_count), arg_count as u8, &current_frame) {
                        return InterpretResult::InterpretRuntimeError;
                    }
                    // Switch to new CallFrame if one was added.
                    if self.frames[self.frame_count - 1].is_some() {
                        self.frames[self.frame_count - 2] = Some(current_frame);
                        current_frame = self.frames[self.frame_count - 1]
                            .take()
                            .expect("Will be new CallFrame.");
                    }
                }
            }
        }
    }

    fn peek(&self, offset: usize) -> Value {
        self.stack[self.stack_count - 1 - offset].clone()
    }

    fn call(
        &mut self,
        function: Rc<LoxFunction>,
        arg_count: u8,
        current_frame: &CallFrame,
    ) -> bool {
        if arg_count != function.arity {
            self.runtime_error(
                current_frame,
                &format!("Expected {} arguments but got {arg_count}", function.arity),
            );
            return false;
        }

        if self.frame_count == FRAMES_MAX {
            self.runtime_error(current_frame, "Sack overflow.");
            return false;
        }

        let frame = CallFrame {
            function,
            ip: 0,
            frame_ptr: self.stack_count - arg_count as usize - 1,
        };
        self.frames[self.frame_count] = Some(frame);
        self.frame_count += 1;
        true
    }

    fn call_value(&mut self, callee: Value, arg_count: u8, current_frame: &CallFrame) -> bool {
        match callee {
            Value::Function(lox_fun) => self.call(lox_fun, arg_count, current_frame),
            Value::Native(native_fn) => {
                let result = native_fn(arg_count, self.stack_count - arg_count as usize);
                self.stack_count -= arg_count as usize + 1;
                self.push(result);
                true
            }
            _ => {
                self.runtime_error(current_frame, "Can only call functions and classes.");
                false
            }
        }
    }

    /// Reads 1 byte offset operand.
    fn get_offset_short(frame: &mut CallFrame) -> usize {
        let offset = frame.function.chunk.code[frame.ip] as usize;
        frame.ip += 1;
        offset
    }

    /// Reads 2 byte offset operand.
    fn get_offset_medium(frame: &mut CallFrame) -> usize {
        let left_byte = frame.function.chunk.code[frame.ip] as usize;
        let right_byte = frame.function.chunk.code[frame.ip + 1] as usize;
        let offset = (left_byte << 8) + right_byte;
        frame.ip += 2;
        offset
    }

    /// Reads 3 byte offset operand.
    fn get_offset_long(frame: &mut CallFrame) -> usize {
        let left_byte = frame.function.chunk.code[frame.ip] as usize;
        let middle_byte = frame.function.chunk.code[frame.ip + 1] as usize;
        let right_byte = frame.function.chunk.code[frame.ip + 2] as usize;
        let offset = (left_byte << 16) + (middle_byte << 8) + right_byte;
        frame.ip += 3;
        offset
    }

    fn runtime_error(&mut self, current_frame: &CallFrame, err_msg: &str) {
        eprintln!("Runtime Error: {err_msg}");

        for frame_idx in (0..self.frame_count).rev() {
            let frame = self.frames[frame_idx].as_ref().unwrap_or(current_frame);
            let line = frame.function.chunk.get_line(frame.ip - 1);
            let name = match frame.function.name {
                Some(str_id) => &format!("{}()", self.strings.lookup(str_id)),
                None => "script",
            };
            eprintln!("[line {line}] in {name}.");
        }
        self.reset_stack();
    }

    fn define_native(&mut self, name: &str, function: fn(u8, usize) -> Value) {
        let str_id = self.strings.intern(name);
        self.push(Value::String(str_id));
        let globals_slot = self.global_values.len();
        self.global_values.push(Value::Native(function));
        self.global_names.insert(str_id, globals_slot);
        self.pop();
    }

    fn reset_stack(&mut self) {
        self.stack_count = 0;
    }

    fn undefined_global_variable(&mut self, current_frame: &CallFrame, offset: usize) {
        for (str_id, val_offset) in self.global_names.iter() {
            if offset == *val_offset {
                let var_name = self.strings.lookup(*str_id);
                self.runtime_error(current_frame, &format!("Undefined variable {}.", var_name));
                return;
            }
        }
    }
}

// Native Functions
fn clock_native(_: u8, _: usize) -> Value {
    Value::Number(
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("SystemTime::now() will always be after UNIX EPOCH.")
            .as_secs_f64(),
    )
}
