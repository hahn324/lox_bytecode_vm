use crate::chunk::Chunk;
use rustc_hash::FxHashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Copy, Eq, Hash)]
pub struct StrId(u32);

#[derive(Debug, Clone)]
pub struct LoxFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Option<StrId>,
}

impl LoxFunction {
    pub fn new(name: Option<StrId>) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name,
        }
    }
}

impl Default for LoxFunction {
    fn default() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: None,
        }
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
    Native(fn(u8, usize) -> Value),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => *lhs == *rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => *lhs == *rhs,
            (Value::String(lhs), Value::String(rhs)) => *lhs == *rhs,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

#[derive(Default)]
pub struct StringInterner {
    pub id_map: FxHashMap<String, StrId>,
    vec: Vec<String>,
}

impl StringInterner {
    pub fn intern(&mut self, name: &str) -> StrId {
        if let Some(&idx) = self.id_map.get(name) {
            return idx;
        }

        let str_id = StrId(self.vec.len() as u32);
        self.id_map.insert(String::from(name), str_id);
        self.vec.push(String::from(name));

        debug_assert!(self.lookup(str_id) == name);
        debug_assert!(self.intern(name) == str_id);

        str_id
    }

    pub fn lookup(&self, value: StrId) -> &str {
        self.vec[value.0 as usize].as_str()
    }
}
