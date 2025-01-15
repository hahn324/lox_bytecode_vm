use crate::value::Value;

#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum OpCode {
    OpReturn,
    OpConstant,
    OpConstantLong,
}
impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::OpReturn,
            1 => OpCode::OpConstant,
            2 => OpCode::OpConstantLong,
            _ => panic!("Unknown byte code {byte}"),
        }
    }
}
impl From<OpCode> for u8 {
    fn from(instruction: OpCode) -> Self {
        instruction.into()
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
            self.code.push(OpCode::OpConstant as u8);
            self.add_line(line);
            self.code.push(constant_offset as u8);
            self.add_line(line);
        } else {
            self.code.push(OpCode::OpConstantLong as u8);
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
