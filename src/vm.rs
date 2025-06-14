use crate::{
    ALLOCATED,
    chunk::OpCode,
    compiler::compile,
    debug::disassemble_instruction,
    gc,
    value::{
        BoundMethod, LoxClass, LoxClosure, LoxFunction, LoxInstance, LoxUpvalue, StrId,
        StringInterner, Value,
    },
};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::cmp;
use std::rc::{Rc, Weak};
use std::sync::atomic::Ordering::Relaxed;
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

#[derive(Debug, Clone)]
struct CallFrame {
    closure: Rc<LoxClosure>,
    ip: usize,
    frame_ptr: usize,
}

pub struct Vm {
    frames: [Option<CallFrame>; FRAMES_MAX],
    frame_count: usize,
    pub stack: [Value; STACK_MAX],
    stack_count: usize,
    debug_trace: bool,
    pub strings: StringInterner,
    pub global_names: FxHashMap<StrId, usize>,
    pub global_values: Vec<Value>,
    open_upvalues: Option<Rc<RefCell<LoxUpvalue>>>,
    init_string: StrId,
    pub instances: Vec<Weak<LoxInstance>>,
    pub next_gc: usize,
    pub gc_heap_grow_factor: usize,
}
impl Vm {
    pub fn new(debug_trace: bool) -> Self {
        let mut strings = StringInterner::default();
        let init_string = strings.intern("init");
        let mut vm = Self {
            frames: [const { None }; FRAMES_MAX],
            frame_count: 0,
            stack: [const { Value::Undefined }; STACK_MAX],
            stack_count: 0,
            debug_trace,
            strings,
            global_names: FxHashMap::default(),
            global_values: Vec::new(),
            open_upvalues: None,
            init_string,
            instances: Vec::with_capacity(100),
            next_gc: 1024 * 1024 * 10,
            gc_heap_grow_factor: 2,
        };
        // Initialize global native functions
        vm.define_native("clock", clock_native, 0);

        vm
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        match compile(source, self, self.debug_trace) {
            Some(function) => {
                let closure = Rc::new(LoxClosure::new(function));
                self.push(Value::Closure(closure.clone()));
                self.frames[0] = Some(CallFrame {
                    closure,
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
        let popped_val = self.stack[self.stack_count].clone();
        self.stack[self.stack_count] = Value::Undefined;
        popped_val
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

        let mut current_frame = match self.frames[self.frame_count - 1].take() {
            Some(frame) => frame,
            None => {
                eprintln!("Frames:");
                for idx in 0..self.frame_count {
                    eprintln!("{idx}: {:?}", self.frames[idx]);
                }
                panic!(
                    "Should always be Some(CallFrame). frame_count: {}",
                    self.frame_count
                );
            }
        };

        loop {
            if self.debug_trace {
                print!("[");
                for idx in 0..self.stack_count {
                    self.print_value(&self.stack[idx]);
                    print!(", ");
                }
                println!("]");
                disassemble_instruction(
                    &current_frame.closure.function.chunk,
                    current_frame.ip,
                    &self,
                );
            }
            let instruction = current_frame.closure.function.chunk.code[current_frame.ip];
            current_frame.ip += 1;
            match OpCode::from(instruction) {
                OpCode::Return => {
                    let result = self.pop();
                    self.close_upvalues(current_frame.frame_ptr);
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return InterpretResult::InterpretOk;
                    }

                    while self.stack_count > current_frame.frame_ptr {
                        self.pop();
                    }
                    self.push(result);
                    current_frame = self.frames[self.frame_count - 1]
                        .take()
                        .expect("Will always be Some(CallFrame).");
                }
                OpCode::Constant => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let constant = current_frame.closure.function.chunk.constants[offset].clone();
                    self.push(constant);
                }
                OpCode::ConstantLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let constant = current_frame.closure.function.chunk.constants[offset].clone();
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
                OpCode::Print => {
                    let value = self.pop();
                    self.print_value(&value);
                    print!("\n");
                }
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
                            .expect("Will always be a new CallFrame.");
                    }
                }
                OpCode::Closure => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let lox_fun = current_frame.closure.function.chunk.constants[offset].clone();
                    let closure = match lox_fun {
                        Value::Function(function) => {
                            self.create_closure(&mut current_frame, function)
                        }
                        _ => unreachable!("Will always be Value::Function variant."),
                    };

                    self.push(Value::Closure(Rc::new(closure)));
                }
                OpCode::ClosureLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let lox_fun = current_frame.closure.function.chunk.constants[offset].clone();
                    let closure = match lox_fun {
                        Value::Function(function) => {
                            self.create_closure(&mut current_frame, function)
                        }
                        _ => unreachable!("Will always be Value::Function variant."),
                    };

                    self.push(Value::Closure(Rc::new(closure)));
                }
                OpCode::GetUpvalue => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let upvalue = current_frame.closure.upvalues[offset].borrow();
                    let value = match upvalue.location {
                        0 => upvalue.closed.clone(),
                        _ => self.stack[upvalue.location].clone(),
                    };
                    self.push(value);
                }
                OpCode::GetUpvalueLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let upvalue = current_frame.closure.upvalues[offset].borrow();
                    let value = match upvalue.location {
                        0 => upvalue.closed.clone(),
                        _ => self.stack[upvalue.location].clone(),
                    };
                    self.push(value);
                }
                OpCode::SetUpvalue => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let mut upvalue = current_frame.closure.upvalues[offset].borrow_mut();
                    match upvalue.location {
                        0 => upvalue.closed = self.peek(0),
                        _ => self.stack[upvalue.location] = self.peek(0),
                    }
                }
                OpCode::SetUpvalueLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let mut upvalue = current_frame.closure.upvalues[offset].borrow_mut();
                    match upvalue.location {
                        0 => upvalue.closed = self.peek(0),
                        _ => self.stack[upvalue.location] = self.peek(0),
                    }
                }
                OpCode::CloseUpvalue => {
                    let upvalue_slot = self.stack_count - 1;
                    self.close_upvalues(upvalue_slot);
                    self.pop();
                }
                OpCode::Class => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let value = current_frame.closure.function.chunk.constants[offset].clone();
                    match value {
                        Value::String(str_id) => {
                            self.push(Value::Class(Rc::new(LoxClass::new(str_id))))
                        }
                        _ => unreachable!("Will always be String variant for Class name."),
                    }
                }
                OpCode::ClassLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let value = current_frame.closure.function.chunk.constants[offset].clone();
                    match value {
                        Value::String(str_id) => {
                            self.push(Value::Class(Rc::new(LoxClass::new(str_id))))
                        }
                        _ => unreachable!("Will always be String variant for ClassLong name."),
                    }
                }
                OpCode::GetProperty => {
                    let instance = match self.peek(0) {
                        Value::Instance(instance) => instance,
                        _ => {
                            self.runtime_error(&current_frame, "Only instances have properties.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let value = current_frame.closure.function.chunk.constants[offset].clone();
                    let name = match value {
                        Value::String(str_id) => str_id,
                        _ => unreachable!("Will always be String variant for GetProperty."),
                    };

                    if let Some(value) = instance.fields.borrow().get(&name) {
                        self.pop();
                        self.push(value.clone());
                    } else if !self.bind_method(Rc::clone(&instance.klass), name, &current_frame) {
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
                OpCode::GetPropertyLong => {
                    let instance = match self.peek(0) {
                        Value::Instance(instance) => instance,
                        _ => {
                            self.runtime_error(&current_frame, "Only instances have properties.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let value = current_frame.closure.function.chunk.constants[offset].clone();
                    let name = match value {
                        Value::String(str_id) => str_id,
                        _ => unreachable!("Will always be String variant for GetPropertyLong."),
                    };

                    if let Some(value) = instance.fields.borrow().get(&name) {
                        self.pop();
                        self.push(value.clone());
                    } else {
                        self.runtime_error(
                            &current_frame,
                            &format!("Undefined property '{}'.", self.strings.lookup(name)),
                        );
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
                OpCode::SetProperty => {
                    let instance = match self.peek(1) {
                        Value::Instance(instance) => instance,
                        _ => {
                            self.runtime_error(&current_frame, "Only instances have fields.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let key_value = current_frame.closure.function.chunk.constants[offset].clone();
                    let str_id = match key_value {
                        Value::String(str_id) => str_id,
                        _ => unreachable!("Will always be String variant for SetProperty."),
                    };

                    instance.fields.borrow_mut().insert(str_id, self.peek(0));
                    let value = self.pop();
                    self.pop();
                    self.push(value);
                }
                OpCode::SetPropertyLong => {
                    let instance = match self.peek(1) {
                        Value::Instance(instance) => instance,
                        _ => {
                            self.runtime_error(&current_frame, "Only instances have fields.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let key_value = current_frame.closure.function.chunk.constants[offset].clone();
                    let str_id = match key_value {
                        Value::String(str_id) => str_id,
                        _ => unreachable!("Will always be String variant for SetPropertyLong."),
                    };

                    instance.fields.borrow_mut().insert(str_id, self.peek(0));
                    let value = self.pop();
                    self.pop();
                    self.push(value);
                }
                OpCode::Method => {
                    let offset = Vm::get_offset_short(&mut current_frame);
                    let method_name =
                        current_frame.closure.function.chunk.constants[offset].clone();
                    let str_id = match method_name {
                        Value::String(str_id) => str_id,
                        _ => unreachable!("Will always be String variant for Method."),
                    };
                    self.define_method(str_id);
                }
                OpCode::MethodLong => {
                    let offset = Vm::get_offset_long(&mut current_frame);
                    let method_name =
                        current_frame.closure.function.chunk.constants[offset].clone();
                    let str_id = match method_name {
                        Value::String(str_id) => str_id,
                        _ => unreachable!("Will always be String variant for Method."),
                    };
                    self.define_method(str_id);
                }
                OpCode::Invoke => {
                    let str_offset = Vm::get_offset_short(&mut current_frame);
                    let method_str_id =
                        match current_frame.closure.function.chunk.constants[str_offset].clone() {
                            Value::String(str_id) => str_id,
                            _ => unreachable!("Will always be String variant for Invoke."),
                        };
                    let arg_count = Vm::get_offset_short(&mut current_frame);

                    if !self.invoke(method_str_id, arg_count as u8, &current_frame) {
                        return InterpretResult::InterpretRuntimeError;
                    }
                    // Switch to new CallFrame if one was added.
                    if self.frames[self.frame_count - 1].is_some() {
                        self.frames[self.frame_count - 2] = Some(current_frame);
                        current_frame = self.frames[self.frame_count - 1]
                            .take()
                            .expect("Will always be a new CallFrame.");
                    }
                }
                OpCode::InvokeLong => {
                    let str_offset = Vm::get_offset_long(&mut current_frame);
                    let method_str_id =
                        match current_frame.closure.function.chunk.constants[str_offset].clone() {
                            Value::String(str_id) => str_id,
                            _ => unreachable!("Will always be String variant for InvokeLong."),
                        };
                    let arg_count = Vm::get_offset_short(&mut current_frame);

                    if !self.invoke(method_str_id, arg_count as u8, &current_frame) {
                        return InterpretResult::InterpretRuntimeError;
                    }
                    // Switch to new CallFrame if one was added.
                    if self.frames[self.frame_count - 1].is_some() {
                        self.frames[self.frame_count - 2] = Some(current_frame);
                        current_frame = self.frames[self.frame_count - 1]
                            .take()
                            .expect("Will always be a new CallFrame.");
                    }
                }
                OpCode::Inherit => {
                    let superclass = match self.peek(1) {
                        Value::Class(klass) => klass,
                        _ => {
                            self.runtime_error(&current_frame, "Superclass must be a class.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    let subclass = match self.peek(0) {
                        Value::Class(klass) => klass,
                        _ => unreachable!("sublcass will always be variant Class."),
                    };
                    subclass
                        .methods
                        .replace(superclass.methods.borrow().clone());
                    self.pop(); // Subclass.
                }
                OpCode::GetSuper => {
                    let name_offset = Vm::get_offset_short(&mut current_frame);
                    let name_str_id =
                        match current_frame.closure.function.chunk.constants[name_offset].clone() {
                            Value::String(str_id) => str_id,
                            _ => unreachable!("Will always be String variant for GetSuper."),
                        };
                    match self.pop() {
                        Value::Class(superclass) => {
                            if !self.bind_method(superclass, name_str_id, &current_frame) {
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                        _ => unreachable!("Will always be Class variant in GetSuper."),
                    }
                }
                OpCode::GetSuperLong => {
                    let name_offset = Vm::get_offset_long(&mut current_frame);
                    let name_str_id =
                        match current_frame.closure.function.chunk.constants[name_offset].clone() {
                            Value::String(str_id) => str_id,
                            _ => unreachable!("Will always be String variant for GetSuperLong."),
                        };
                    match self.pop() {
                        Value::Class(superclass) => {
                            if !self.bind_method(superclass, name_str_id, &current_frame) {
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                        _ => unreachable!("Will always be Class variant in GetSuper."),
                    }
                }
                OpCode::SuperInvoke => {
                    let str_offset = Vm::get_offset_short(&mut current_frame);
                    let method_str_id =
                        match current_frame.closure.function.chunk.constants[str_offset].clone() {
                            Value::String(str_id) => str_id,
                            _ => unreachable!("Will always be String variant for SuperInvoke."),
                        };
                    let arg_count = Vm::get_offset_short(&mut current_frame);
                    match self.pop() {
                        Value::Class(superclass) => {
                            if !self.invoke_from_class(
                                superclass,
                                method_str_id,
                                arg_count as u8,
                                &current_frame,
                            ) {
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                        _ => unreachable!("Will always be Class variant in SuperInvoke."),
                    }

                    // Switch to new CallFrame if one was added.
                    if self.frames[self.frame_count - 1].is_some() {
                        self.frames[self.frame_count - 2] = Some(current_frame);
                        current_frame = self.frames[self.frame_count - 1]
                            .take()
                            .expect("Will always be a new CallFrame.");
                    }
                }
                OpCode::SuperInvokeLong => {
                    let str_offset = Vm::get_offset_long(&mut current_frame);
                    let method_str_id =
                        match current_frame.closure.function.chunk.constants[str_offset].clone() {
                            Value::String(str_id) => str_id,
                            _ => unreachable!("Will always be String variant for SuperInvokeLong."),
                        };
                    let arg_count = Vm::get_offset_short(&mut current_frame);
                    match self.pop() {
                        Value::Class(superclass) => {
                            if !self.invoke_from_class(
                                superclass,
                                method_str_id,
                                arg_count as u8,
                                &current_frame,
                            ) {
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                        _ => unreachable!("Will always be Class variant in SuperInvokeLong."),
                    }

                    // Switch to new CallFrame if one was added.
                    if self.frames[self.frame_count - 1].is_some() {
                        self.frames[self.frame_count - 2] = Some(current_frame);
                        current_frame = self.frames[self.frame_count - 1]
                            .take()
                            .expect("Will always be a new CallFrame.");
                    }
                }
            }
        }
    }

    fn print_function(&self, function: &LoxFunction) {
        match function.name {
            Some(name_id) => {
                print!("<fn {}>", self.strings.lookup(name_id))
            }
            None => print!("<script>"),
        }
    }

    pub fn print_value(&self, value: &Value) {
        match value {
            Value::Number(val) => print!("{val}"),
            Value::Bool(val) => print!("{val}"),
            Value::String(str_id) => {
                print!("{}", self.strings.lookup(*str_id));
            }
            Value::Nil => print!("nil"),
            Value::Undefined => print!("Undefined"),
            Value::Function(lox_fun) => self.print_function(lox_fun),
            Value::Native(_, _) => print!("<native fn>"),
            Value::Closure(closure) => self.print_function(&closure.function),
            Value::Class(klass) => {
                print!("{}", self.strings.lookup(klass.name));
            }
            Value::Instance(instance) => {
                print!("{} instance", self.strings.lookup(instance.klass.name));
            }
            Value::Method(bound_method) => self.print_function(&bound_method.method.function),
        }
    }

    fn peek(&self, offset: usize) -> Value {
        self.stack[self.stack_count - 1 - offset].clone()
    }

    fn call(&mut self, closure: Rc<LoxClosure>, arg_count: u8, current_frame: &CallFrame) -> bool {
        if arg_count != closure.function.arity {
            self.runtime_error(
                current_frame,
                &format!(
                    "Expected {} arguments but got {arg_count}.",
                    closure.function.arity
                ),
            );
            return false;
        }

        if self.frame_count == FRAMES_MAX {
            self.runtime_error(current_frame, "Stack overflow.");
            return false;
        }

        let frame = CallFrame {
            closure,
            ip: 0,
            frame_ptr: self.stack_count - arg_count as usize - 1,
        };
        self.frames[self.frame_count] = Some(frame);
        self.frame_count += 1;
        true
    }

    fn call_value(&mut self, callee: Value, arg_count: u8, current_frame: &CallFrame) -> bool {
        match callee {
            Value::Method(bound_method) => {
                self.stack[self.stack_count - arg_count as usize - 1] =
                    bound_method.receiver.clone();
                self.call(Rc::clone(&bound_method.method), arg_count, current_frame)
            }
            Value::Class(klass) => {
                if ALLOCATED.load(Relaxed) > self.next_gc {
                    gc::collect_garbage(self);
                }
                let new_instance = Rc::new(LoxInstance::new(klass.clone()));
                self.instances.push(Rc::downgrade(&new_instance));
                self.stack[self.stack_count - arg_count as usize - 1] =
                    Value::Instance(new_instance);
                if let Some(initializer) = klass.methods.borrow().get(&self.init_string) {
                    return self.call(Rc::clone(initializer), arg_count, current_frame);
                }
                if arg_count != 0 {
                    self.runtime_error(
                        current_frame,
                        &format!("Expected 0 arguments but got {arg_count}."),
                    );
                    return false;
                }
                return true;
            }
            Value::Closure(lox_closure) => self.call(lox_closure, arg_count, current_frame),
            Value::Native(native_fn, arity) => {
                if arg_count != arity {
                    self.runtime_error(
                        current_frame,
                        &format!("Expected {arity} arguments but got {arg_count}."),
                    );
                    return false;
                }
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

    fn invoke_from_class(
        &mut self,
        klass: Rc<LoxClass>,
        name: StrId,
        arg_count: u8,
        current_frame: &CallFrame,
    ) -> bool {
        if let Some(method) = klass.methods.borrow().get(&name) {
            self.call(Rc::clone(method), arg_count, current_frame)
        } else {
            self.runtime_error(
                current_frame,
                &format!("Undefined property '{}'.", self.strings.lookup(name)),
            );
            false
        }
    }

    fn invoke(&mut self, name: StrId, arg_count: u8, current_frame: &CallFrame) -> bool {
        let receiver = self.peek(arg_count as usize);
        match receiver {
            Value::Instance(instance) => {
                if let Some(value) = instance.fields.borrow().get(&name) {
                    self.stack[self.stack_count - arg_count as usize - 1] = value.clone();
                    self.call_value(value.clone(), arg_count, current_frame)
                } else {
                    self.invoke_from_class(instance.klass.clone(), name, arg_count, current_frame)
                }
            }
            _ => {
                self.runtime_error(current_frame, "Only instances have methods.");
                false
            }
        }
    }

    fn bind_method(&mut self, klass: Rc<LoxClass>, name: StrId, current_frame: &CallFrame) -> bool {
        if let Some(method) = klass.methods.borrow().get(&name) {
            let bound = Value::Method(Rc::new(BoundMethod::new(self.peek(0), Rc::clone(method))));
            self.pop();
            self.push(bound);
            true
        } else {
            self.runtime_error(
                &current_frame,
                &format!("Undefined property '{}'.", self.strings.lookup(name)),
            );
            false
        }
    }

    fn create_closure(&mut self, frame: &mut CallFrame, function: Rc<LoxFunction>) -> LoxClosure {
        let mut closure = LoxClosure::new(function);

        for _ in 0..closure.upvalues.capacity() {
            let is_local = frame.closure.function.chunk.code[frame.ip];
            frame.ip += 1;
            let index = frame.closure.function.chunk.code[frame.ip] as usize;
            frame.ip += 1;

            if is_local == 1 {
                closure
                    .upvalues
                    .push(self.capture_upvalue(frame.frame_ptr + index));
            } else {
                closure.upvalues.push(frame.closure.upvalues[index].clone());
            }
        }

        closure
    }

    fn capture_upvalue(&mut self, stack_offset: usize) -> Rc<RefCell<LoxUpvalue>> {
        let mut prev_upvalue = None;
        let mut cur_upvalue = self.open_upvalues.clone();
        while let Some(ref upvalue) = cur_upvalue {
            let cur_location = upvalue.borrow().location;
            match cur_location.cmp(&stack_offset) {
                cmp::Ordering::Less => break,
                cmp::Ordering::Equal => return Rc::clone(upvalue),
                cmp::Ordering::Greater => {
                    prev_upvalue = cur_upvalue.clone();
                    let next_upvalue = upvalue.borrow().next.clone();
                    cur_upvalue = next_upvalue;
                }
            }
        }

        let created_upvalue = Rc::new(RefCell::new(LoxUpvalue {
            location: stack_offset,
            closed: Value::Nil,
            next: cur_upvalue.clone(),
        }));

        match prev_upvalue {
            Some(upvalue) => {
                upvalue.borrow_mut().next = Some(Rc::clone(&created_upvalue));
            }
            None => {
                self.open_upvalues = Some(Rc::clone(&created_upvalue));
            }
        }

        created_upvalue
    }

    fn close_upvalues(&mut self, last_idx: usize) {
        while let Some(upvalue) = self.open_upvalues.clone() {
            let location = upvalue.borrow().location;
            if location < last_idx {
                break;
            }
            upvalue.borrow_mut().closed = self.stack[location].clone();
            upvalue.borrow_mut().location = 0;
            self.open_upvalues = upvalue.borrow().next.clone();
        }
    }

    fn define_method(&mut self, method_str_id: StrId) {
        let method = match self.peek(0) {
            Value::Closure(closure) => closure,
            _ => unreachable!("Top of stack during METHOD OP will always be the closure."),
        };
        let klass = match self.peek(1) {
            Value::Class(klass) => klass,
            _ => unreachable!("Below method closure will always be Class."),
        };
        klass.methods.borrow_mut().insert(method_str_id, method);
        self.pop();
    }

    /// Reads 1 byte offset operand.
    fn get_offset_short(frame: &mut CallFrame) -> usize {
        let offset = frame.closure.function.chunk.code[frame.ip] as usize;
        frame.ip += 1;
        offset
    }

    /// Reads 2 byte offset operand.
    fn get_offset_medium(frame: &mut CallFrame) -> usize {
        let left_byte = frame.closure.function.chunk.code[frame.ip] as usize;
        let right_byte = frame.closure.function.chunk.code[frame.ip + 1] as usize;
        let offset = (left_byte << 8) + right_byte;
        frame.ip += 2;
        offset
    }

    /// Reads 3 byte offset operand.
    fn get_offset_long(frame: &mut CallFrame) -> usize {
        let left_byte = frame.closure.function.chunk.code[frame.ip] as usize;
        let middle_byte = frame.closure.function.chunk.code[frame.ip + 1] as usize;
        let right_byte = frame.closure.function.chunk.code[frame.ip + 2] as usize;
        let offset = (left_byte << 16) + (middle_byte << 8) + right_byte;
        frame.ip += 3;
        offset
    }

    fn runtime_error(&mut self, current_frame: &CallFrame, err_msg: &str) {
        eprintln!("{err_msg}");

        for frame_idx in (0..self.frame_count).rev() {
            let frame = self.frames[frame_idx].as_ref().unwrap_or(current_frame);
            let line = frame.closure.function.chunk.get_line(frame.ip - 1);
            let name = match frame.closure.function.name {
                Some(str_id) => &format!("{}()", self.strings.lookup(str_id)),
                None => "script",
            };
            eprintln!("[line {line}] in {name}.");
        }
        self.reset_stack();
    }

    fn define_native(&mut self, name: &str, function: fn(u8, usize) -> Value, arity: u8) {
        let str_id = self.strings.intern(name);
        self.push(Value::String(str_id));
        let globals_slot = self.global_values.len();
        self.global_values.push(Value::Native(function, arity));
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
                self.runtime_error(
                    current_frame,
                    &format!("Undefined variable '{}'.", var_name),
                );
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
