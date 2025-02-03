use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    scanner::{Scanner, Token, TokenType},
    value::Value,
};

pub fn compile(source: &str, debug_mode: bool) -> Option<Chunk> {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner);
    parser.advance();
    parser.expression();
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
            _ => Precedence::None,
        }
    }
}

struct Parser<'src> {
    current: Option<Token<'src>>,
    previous: Option<Token<'src>>,
    scanner: Scanner<'src>,
    chunk: Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl<'src> Parser<'src> {
    fn new(scanner: Scanner<'src>) -> Self {
        Self {
            current: None,
            previous: None,
            scanner,
            chunk: Chunk::new(),
            had_error: false,
            panic_mode: false,
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

    fn run_prefix_rule(&mut self, token_type: TokenType) -> bool {
        match token_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus => self.unary(),
            TokenType::Number => self.number(),
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
            TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash => {
                self.binary()
            }
            _ => (),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let found_prefix = match self.previous {
            Some(ref token) => self.run_prefix_rule(token.token_type),
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
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self) {
        let value = match self.previous {
            Some(ref token) => token.lexeme.parse::<f64>().expect(&format!(
                "Failed to parse number literal '{}'.",
                token.lexeme
            )),
            None => {
                panic!("There should always be a previous Token if Parser::number() is called.")
            }
        };
        self.emit_constant(value);
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
            _ => unreachable!("Expects only tokens that are binary operators."),
        };
        self.parse_precedence(precedence);

        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Subtract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            _ => unreachable!("Expects only tokens that are binary operators."),
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
        self.current_chunk().write_constant(value, line);
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
            None => self.error_at(Token::new(TokenType::Eof, "", self.scanner.line), message),
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
