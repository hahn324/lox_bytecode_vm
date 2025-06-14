use crate::chunk::Chunk;
use rustc_hash::FxHashMap;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
pub struct StrId(u32);

#[derive(Debug, Clone)]
pub struct LoxFunction {
    pub arity: u8,
    pub upvalue_count: usize,
    pub chunk: Chunk,
    pub name: Option<StrId>,
}

impl LoxFunction {
    pub fn new(name: Option<StrId>) -> Self {
        Self {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoxUpvalue {
    pub location: usize,
    pub closed: Value,
    pub next: Option<Rc<RefCell<LoxUpvalue>>>,
}

#[derive(Debug, Clone)]
pub struct LoxClosure {
    pub function: Rc<LoxFunction>,
    pub upvalues: Vec<Rc<RefCell<LoxUpvalue>>>,
}

impl LoxClosure {
    pub fn new(function: Rc<LoxFunction>) -> Self {
        let upvalue_count = function.upvalue_count;
        Self {
            function,
            upvalues: Vec::with_capacity(upvalue_count),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoxClass {
    pub name: StrId,
    pub methods: RefCell<FxHashMap<StrId, Rc<LoxClosure>>>,
}

impl LoxClass {
    pub fn new(name: StrId) -> Self {
        Self {
            name,
            methods: RefCell::new(FxHashMap::default()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    pub klass: Rc<LoxClass>,
    pub fields: RefCell<FxHashMap<StrId, Value>>,
    pub is_marked: Cell<bool>,
}

impl LoxInstance {
    pub fn new(klass: Rc<LoxClass>) -> Self {
        Self {
            klass,
            fields: RefCell::new(FxHashMap::default()),
            is_marked: Cell::new(false),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: Rc<LoxClosure>,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: Rc<LoxClosure>) -> Self {
        Self { receiver, method }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(StrId),
    Nil,
    Undefined,
    Function(Rc<LoxFunction>),
    Native(fn(u8, usize) -> Value, u8),
    Closure(Rc<LoxClosure>),
    Class(Rc<LoxClass>),
    Instance(Rc<LoxInstance>),
    Method(Rc<BoundMethod>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => *lhs == *rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => *lhs == *rhs,
            (Value::String(lhs), Value::String(rhs)) => *lhs == *rhs,
            (Value::Nil, Value::Nil) => true,
            (Value::Function(lhs), Value::Function(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Closure(lhs), Value::Closure(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Class(lhs), Value::Class(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Instance(lhs), Value::Instance(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Method(lhs), Value::Method(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }
}

#[derive(Default)]
pub struct StringInterner {
    pub id_map: FxHashMap<&'static str, StrId>,
    vec: Vec<String>,
}

impl StringInterner {
    pub fn intern(&mut self, name: &str) -> StrId {
        if let Some(&idx) = self.id_map.get(name) {
            return idx;
        }

        let str_id = StrId(self.vec.len() as u32);
        let str_ref = self.alloc(name);
        self.id_map.insert(str_ref, str_id);

        debug_assert!(self.lookup(str_id) == name);
        debug_assert!(self.intern(name) == str_id);

        str_id
    }

    pub fn lookup(&self, value: StrId) -> &str {
        self.vec[value.0 as usize].as_str()
    }

    fn alloc(&mut self, name: &str) -> &'static str {
        let idx = self.vec.len();
        self.vec.push(String::from(name));
        // SAFETY: We never drop or move the interned String, so reference will
        // always be valid while StringInterner instance exists.
        unsafe { &*(self.vec[idx].as_str() as *const str) }
    }
}
