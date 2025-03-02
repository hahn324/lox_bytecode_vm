use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    scanner::{Scanner, Token, TokenType},
    value::{StringInterner, Value},
};

pub fn compile(source: &str, strings: &mut StringInterner, debug_mode: bool) -> Option<Chunk> {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner, strings);
    parser.advance();
    while parser.current.is_some() {
        parser.declaration();
    }
    end_compiler(&mut parser, debug_mode);
    match parser.had_error {
        true => None,
        false => Some(parser.chunk),
    }
}

fn end_compiler(parser: &mut Parser, debug_mode: bool) {
    parser.emit_return();
    if debug_mode {
        disassemble_chunk(&parser.chunk, "code");
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl From<TokenType> for Precedence {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Plus | TokenType::Minus => Precedence::Term,
            TokenType::Star | TokenType::Slash => Precedence::Factor,
            TokenType::BangEqual | TokenType::EqualEqual => Precedence::Equality,
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => Precedence::Comparison,
            _ => Precedence::None,
        }
    }
}

struct Parser<'src, 'vm> {
    current: Option<Token<'src>>,
    previous: Option<Token<'src>>,
    scanner: Scanner<'src>,
    chunk: Chunk,
    had_error: bool,
    panic_mode: bool,
    strings: &'vm mut StringInterner,
}

impl<'src, 'vm> Parser<'src, 'vm> {
    fn new(scanner: Scanner<'src>, strings: &'vm mut StringInterner) -> Self {
        Self {
            current: None,
            previous: None,
            scanner,
            chunk: Chunk::new(),
            had_error: false,
            panic_mode: false,
            strings,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current.take();

        loop {
            self.current = self.scanner.next();

            match self.current {
                Some(token) => match token.token_type {
                    TokenType::Error => self.error_at(token, ""),
                    _ => break,
                },
                None => break,
            }
        }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    fn run_prefix_rule(&mut self, token_type: TokenType, can_assign: bool) -> bool {
        match token_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus | TokenType::Bang => self.unary(),
            TokenType::Number => self.number(),
            TokenType::Nil | TokenType::True | TokenType::False => self.literal(),
            TokenType::String => self.string(),
            TokenType::Identifier => self.variable(can_assign),
            _ => {
                self.error_at(
                    self.previous.expect("Will always be Some variant."),
                    "Expect expression.",
                );
                return false;
            }
        }
        true
    }

    fn run_infix_rule(&mut self, token_type: TokenType) {
        match token_type {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Slash
            | TokenType::BangEqual
            | TokenType::EqualEqual
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => self.binary(),
            _ => (),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let can_assign = precedence <= Precedence::Assignment;
        let found_prefix = match self.previous {
            Some(ref token) => self.run_prefix_rule(token.token_type, can_assign),
            None => false,
        };
        if !found_prefix {
            return;
        }

        while let Some(current) = self.current {
            let token_precedence = Precedence::from(current.token_type);
            if token_precedence < precedence {
                break;
            }
            self.advance();
            self.run_infix_rule(
                self.previous
                    .expect("Will always be Some variant.")
                    .token_type,
            );
        }

        if can_assign && self.match_token(TokenType::Equal) {
            self.error_at(
                self.previous.expect("Just set to = token."),
                "Invalid assignment target.",
            );
        }
    }

    fn identifier_constant(&mut self, name: Token) -> usize {
        let str_id = self.strings.intern(name.lexeme);
        let identifier = Value::String(str_id);
        self.current_chunk().add_constant(identifier)
    }

    fn parse_variable(&mut self, error_message: &str) -> usize {
        self.consume(TokenType::Identifier, error_message);
        let name = self.previous.expect("Just consumed Identifier Token.");
        self.identifier_constant(name)
    }

    fn define_variable(&mut self, global_offset: usize) {
        let line = match self.previous {
            Some(ref token) => token.line,
            None => 1,
        };
        self.current_chunk().push_constant_ops(
            global_offset,
            line,
            OpCode::DefineGlobal,
            OpCode::DefineGlobalLong,
        );
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn var_declaration(&mut self) {
        let global_offset = self.parse_variable("expect variable name.");

        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil as u8);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global_offset);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop as u8);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print as u8);
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while let Some(ref current) = self.current {
            if let Some(ref previous) = self.previous {
                if previous.token_type == TokenType::Semicolon {
                    return;
                }
            }
            match current.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }
            self.advance();
        }
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn number(&mut self) {
        let value = Value::Number(match self.previous {
            Some(ref token) => token.lexeme.parse::<f64>().expect(&format!(
                "Failed to parse number literal '{}'.",
                token.lexeme
            )),
            None => {
                panic!("There was no consumed previous Token when Parser::number() was called.")
            }
        });
        self.emit_constant(value);
    }

    fn string(&mut self) {
        // Trims the leading and trailing '"' characters from the lexeme to make the String.
        let str_val = match self.previous {
            Some(ref token) => &token.lexeme[1..token.lexeme.len() - 1],
            None => {
                panic!("There was no consumed previous Token when Parser::string() was called.")
            }
        };
        let str_id = self.strings.intern(str_val);
        self.emit_constant(Value::String(str_id));
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let offset = self.identifier_constant(name);
        let line = match self.previous {
            Some(ref token) => token.line,
            None => 1,
        };

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.current_chunk().push_constant_ops(
                offset,
                line,
                OpCode::SetGlobal,
                OpCode::SetGlobalLong,
            );
        } else {
            self.current_chunk().push_constant_ops(
                offset,
                line,
                OpCode::GetGlobal,
                OpCode::GetGlobalLong,
            );
        }
    }

    fn variable(&mut self, can_assign: bool) {
        let previous = self.previous.expect("Will always be Some variant.");
        self.named_variable(previous, can_assign);
    }

    fn unary(&mut self) {
        let operator_type = self
            .previous
            .as_ref()
            .expect("Expect to have consumed a unary token.")
            .token_type;

        // Compile the operand.
        self.parse_precedence(Precedence::Unary);

        // Emit the operator instruction.
        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            TokenType::Bang => self.emit_byte(OpCode::Not as u8),
            _ => unreachable!("Expects only tokens that are unary operators."),
        }
    }

    fn binary(&mut self) {
        let operator_type = self
            .previous
            .as_ref()
            .expect("Expect to have consumed a binary token.")
            .token_type;

        let precedence = match operator_type {
            TokenType::Plus | TokenType::Minus => Precedence::Factor,
            TokenType::Star | TokenType::Slash => Precedence::Unary,
            TokenType::BangEqual | TokenType::EqualEqual => Precedence::Comparison,
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => Precedence::Term,
            _ => unreachable!("Expects only tokens that are binary operators."),
        };

        self.parse_precedence(precedence);

        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Subtract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal as u8),
            TokenType::Greater => self.emit_byte(OpCode::Greater as u8),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_byte(OpCode::Less as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            _ => unreachable!("Expects only tokens that are binary operators."),
        }
    }

    fn literal(&mut self) {
        let token_type = self
            .previous
            .as_ref()
            .expect("Expect to have consumed a literal token.")
            .token_type;
        match token_type {
            TokenType::Nil => self.emit_byte(OpCode::Nil as u8),
            TokenType::True => self.emit_byte(OpCode::True as u8),
            TokenType::False => self.emit_byte(OpCode::False as u8),
            _ => unreachable!(),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn emit_byte(&mut self, byte: u8) {
        let line = match self.previous {
            Some(ref token) => token.line,
            None => 1,
        };
        self.current_chunk().write_chunk(byte, line);
    }

    fn emit_constant(&mut self, value: Value) {
        let line = match self.previous {
            Some(ref token) => token.line,
            None => 1,
        };
        let offset = self.current_chunk().add_constant(value);
        self.current_chunk().push_constant_ops(
            offset,
            line,
            OpCode::Constant,
            OpCode::ConstantLong,
        );
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return as u8);
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        match self.current {
            Some(token) => {
                if token.token_type == token_type {
                    self.advance()
                } else {
                    self.error_at(token, message);
                }
            }
            None => self.error_at(
                Token::new(TokenType::Eof, "", self.scanner.line - 1),
                message,
            ),
        }
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        if let Some(ref current) = self.current {
            current.token_type == token_type
        } else {
            false
        }
    }

    fn error_at(&mut self, token: Token<'src>, message: &str) {
        if self.panic_mode {
            return;
        }

        self.panic_mode = true;

        match token.token_type {
            TokenType::Error => println!("[line {}] Error: {}", token.line, token.lexeme),
            TokenType::Eof => println!("[line {}] Error at end: {}", token.line, message),
            _ => println!(
                "[line {}] Error at '{}': {}",
                token.line, token.lexeme, message
            ),
        }

        self.had_error = true;
    }
}
