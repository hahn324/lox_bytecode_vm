use crate::value::Value;

#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
    ConstantLong,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
}
impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::Return,
            1 => OpCode::Constant,
            2 => OpCode::ConstantLong,
            3 => OpCode::Negate,
            4 => OpCode::Add,
            5 => OpCode::Subtract,
            6 => OpCode::Multiply,
            7 => OpCode::Divide,
            8 => OpCode::Nil,
            9 => OpCode::True,
            10 => OpCode::False,
            11 => OpCode::Not,
            12 => OpCode::Equal,
            13 => OpCode::Greater,
            14 => OpCode::Less,
            _ => panic!("Unknown byte code {byte}"),
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<(usize, usize)>,
}
impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write_chunk(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.add_line(line);
    }

    pub fn write_constant(&mut self, value: Value, line: usize) {
        let constant_offset = self.add_constant(value);
        if constant_offset < 256 {
            self.code.push(OpCode::Constant as u8);
            self.add_line(line);
            self.code.push(constant_offset as u8);
            self.add_line(line);
        } else {
            self.code.push(OpCode::ConstantLong as u8);
            self.add_line(line);
            self.code.extend_from_slice(&[
                (constant_offset >> 16) as u8,
                ((constant_offset >> 8) & 0xff) as u8,
                (constant_offset & 0xff) as u8,
            ]);
            self.add_line(line);
            self.add_line(line);
            self.add_line(line);
        }
    }

    pub fn add_line(&mut self, line: usize) {
        if let Some(last_line) = self.lines.last_mut() {
            if last_line.0 == line {
                last_line.1 += 1;
            } else {
                self.lines.push((line, 1));
            }
        } else {
            self.lines.push((line, 1));
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn get_line(&self, offset: usize) -> usize {
        let mut ctr = offset + 1;
        for (line, len) in self.lines.iter() {
            if *len >= ctr {
                return *line;
            } else {
                ctr -= *len;
            }
        }
        0
    }
}
