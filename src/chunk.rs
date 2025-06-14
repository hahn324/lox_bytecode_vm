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
    Print,
    Pop,
    DefineGlobal,
    DefineGlobalLong,
    GetGlobal,
    GetGlobalLong,
    SetGlobal,
    SetGlobalLong,
    GetLocal,
    GetLocalLong,
    SetLocal,
    SetLocalLong,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
    Closure,
    ClosureLong,
    GetUpvalue,
    GetUpvalueLong,
    SetUpvalue,
    SetUpvalueLong,
    CloseUpvalue,
    Class,
    ClassLong,
    SetProperty,
    SetPropertyLong,
    GetProperty,
    GetPropertyLong,
    Method,
    MethodLong,
    Invoke,
    InvokeLong,
    Inherit,
    GetSuper,
    GetSuperLong,
    SuperInvoke,
    SuperInvokeLong,
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
            15 => OpCode::Print,
            16 => OpCode::Pop,
            17 => OpCode::DefineGlobal,
            18 => OpCode::DefineGlobalLong,
            19 => OpCode::GetGlobal,
            20 => OpCode::GetGlobalLong,
            21 => OpCode::SetGlobal,
            22 => OpCode::SetGlobalLong,
            23 => OpCode::GetLocal,
            24 => OpCode::GetLocalLong,
            25 => OpCode::SetLocal,
            26 => OpCode::SetLocalLong,
            27 => OpCode::JumpIfFalse,
            28 => OpCode::Jump,
            29 => OpCode::Loop,
            30 => OpCode::Call,
            31 => OpCode::Closure,
            32 => OpCode::ClosureLong,
            33 => OpCode::GetUpvalue,
            34 => OpCode::GetUpvalueLong,
            35 => OpCode::SetUpvalue,
            36 => OpCode::SetUpvalueLong,
            37 => OpCode::CloseUpvalue,
            38 => OpCode::Class,
            39 => OpCode::ClassLong,
            40 => OpCode::SetProperty,
            41 => OpCode::SetPropertyLong,
            42 => OpCode::GetProperty,
            43 => OpCode::GetPropertyLong,
            44 => OpCode::Method,
            45 => OpCode::MethodLong,
            46 => OpCode::Invoke,
            47 => OpCode::InvokeLong,
            48 => OpCode::Inherit,
            49 => OpCode::GetSuper,
            50 => OpCode::GetSuperLong,
            51 => OpCode::SuperInvoke,
            52 => OpCode::SuperInvokeLong,
            _ => panic!("Unknown byte code {byte}"),
        }
    }
}

#[derive(Debug, Clone)]
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

    pub fn push_val_offset_op(
        &mut self,
        val_offset: usize,
        line: usize,
        op: OpCode,
        op_long: OpCode,
    ) {
        if val_offset < 256 {
            self.code.push(op as u8);
            self.add_line(line);
            self.code.push(val_offset as u8);
            self.add_line(line);
        } else {
            self.code.push(op_long as u8);
            self.add_line(line);
            self.code.extend_from_slice(&[
                (val_offset >> 16) as u8,
                ((val_offset >> 8) & 0xff) as u8,
                (val_offset & 0xff) as u8,
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
