use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(Rc<String>),
    Nil,
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => *lhs == *rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => *lhs == *rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}
