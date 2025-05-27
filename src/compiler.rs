use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    scanner::{Scanner, Token, TokenType},
    value::{LoxFunction, StrId, Value},
    vm::Vm,
};
use std::cell::Cell;
use std::rc::Rc;

pub fn compile(source: &str, vm: &mut Vm, debug_mode: bool) -> Option<Rc<LoxFunction>> {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner, vm, debug_mode);
    parser.advance();
    while parser.current.is_some() {
        parser.declaration();
    }

    let compiler = end_compiler(&mut parser);
    match parser.had_error.get() {
        true => None,
        false => Some(Rc::new(compiler.function)),
    }
}

fn end_compiler<'src>(parser: &mut Parser<'src, '_>) -> Compiler<'src> {
    parser.emit_return();

    let compiler = parser
        .compilers
        .pop()
        .expect("parser.compilers will always be non-empty.");

    if parser.debug_mode {
        let current_chunk = &compiler.function.chunk;
        let name = match compiler.function.name {
            Some(name_id) => parser.vm.strings.lookup(name_id),
            None => "<script>",
        };
        disassemble_chunk(current_chunk, name, &parser.vm);
    }
    compiler
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
                // Primary,
}

impl From<TokenType> for Precedence {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::LeftParen | TokenType::Dot => Precedence::Call,
            TokenType::Plus | TokenType::Minus => Precedence::Term,
            TokenType::Star | TokenType::Slash => Precedence::Factor,
            TokenType::BangEqual | TokenType::EqualEqual => Precedence::Equality,
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => Precedence::Comparison,
            TokenType::And => Precedence::And,
            TokenType::Or => Precedence::Or,
            _ => Precedence::None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Local<'src> {
    name: Token<'src>,
    depth: i32,
    is_captured: bool,
}

#[derive(Debug, Clone, Copy)]
struct Upvalue {
    index: usize,
    is_local: bool,
}

#[derive(Debug, Clone, Copy)]
enum FunctionType {
    Function,
    Initializer,
    Method,
    Script,
}

#[derive(Debug, Clone)]
struct Compiler<'src> {
    function: LoxFunction,
    function_type: FunctionType,
    locals: Vec<Local<'src>>,
    scope_depth: i32,
    upvalues: Vec<Upvalue>,
}

impl<'src> Compiler<'src> {
    fn new(function_type: FunctionType, name: Option<StrId>) -> Self {
        let lexeme = match function_type {
            FunctionType::Function => "",
            _ => "this",
        };

        let placeholder = Token {
            token_type: TokenType::Identifier,
            lexeme,
            line: 0,
        };
        let locals = vec![Local {
            name: placeholder,
            depth: 0,
            is_captured: false,
        }];
        Self {
            function: LoxFunction::new(name),
            function_type,
            locals,
            scope_depth: 0,
            upvalues: vec![],
        }
    }
}

struct ClassCompiler {
    has_super_class: bool,
}
impl ClassCompiler {
    fn new() -> Self {
        Self {
            has_super_class: false,
        }
    }
}

struct Parser<'src, 'vm> {
    current: Option<Token<'src>>,
    previous: Option<Token<'src>>,
    scanner: Scanner<'src>,
    had_error: Cell<bool>,
    panic_mode: Cell<bool>,
    debug_mode: bool,
    vm: &'vm mut Vm,
    compilers: Vec<Compiler<'src>>,
    class_compilers: Vec<ClassCompiler>,
    loop_depth: u32,
    loop_condition_offsets: Vec<usize>,
}

impl<'src, 'vm> Parser<'src, 'vm> {
    fn new(scanner: Scanner<'src>, vm: &'vm mut Vm, debug_mode: bool) -> Self {
        let compiler = Compiler::new(FunctionType::Script, None);
        Self {
            current: None,
            previous: None,
            scanner,
            had_error: Cell::new(false),
            panic_mode: Cell::new(false),
            debug_mode,
            vm,
            compilers: vec![compiler],
            class_compilers: vec![],
            loop_depth: 0,
            loop_condition_offsets: Vec::new(),
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
        match self.compilers.last_mut() {
            Some(compiler) => &mut compiler.function.chunk,
            None => unreachable!("Will always be Some compiler when this method is called."),
        }
    }

    fn run_prefix_rule(&mut self, token_type: TokenType, can_assign: bool) -> bool {
        match token_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus | TokenType::Bang => self.unary(),
            TokenType::Number => self.number(),
            TokenType::Nil | TokenType::True | TokenType::False => self.literal(),
            TokenType::String => self.string(),
            TokenType::Identifier => self.variable(can_assign),
            TokenType::Super => self.super_(),
            TokenType::This => self.this_(),
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

    fn run_infix_rule(&mut self, token_type: TokenType, can_assign: bool) {
        match token_type {
            TokenType::LeftParen => self.call(),
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
            TokenType::And => self.and_(),
            TokenType::Or => self.or_(),
            TokenType::Dot => self.dot(can_assign),
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
                can_assign,
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
        let str_id = self.vm.strings.intern(name.lexeme);
        match self.vm.global_names.get(&str_id) {
            Some(offset) => *offset,
            None => {
                let offset = self.vm.global_values.len();
                self.vm.global_values.push(Value::Undefined);
                self.vm.global_names.insert(str_id, offset);
                offset
            }
        }
    }

    fn resolve_local(&self, compiler_idx: usize, name: Token<'src>) -> Option<usize> {
        for (idx, local) in self.compilers[compiler_idx].locals.iter().enumerate().rev() {
            if local.name.lexeme == name.lexeme {
                if local.depth == -1 {
                    self.error_at(name, "Can't read local variable in its own initializer.");
                }
                return Some(idx);
            }
        }

        None
    }

    fn add_upvalue(&mut self, compiler_idx: usize, index: usize, is_local: bool) -> usize {
        match self.compilers.get_mut(compiler_idx) {
            Some(compiler) => {
                for (idx, upvalue) in compiler.upvalues.iter().enumerate() {
                    if upvalue.index == index && upvalue.is_local == is_local {
                        return idx;
                    }
                }

                compiler.upvalues.push(Upvalue { index, is_local });
                compiler.function.upvalue_count = compiler.upvalues.len();
                compiler.upvalues.len() - 1
            }
            None => unreachable!("Will always be Some compiler."),
        }
    }

    fn resolve_upvalue(&mut self, compiler_idx: usize, name: Token<'src>) -> Option<usize> {
        if compiler_idx == 0 {
            return None;
        }

        let enclosing_idx = compiler_idx - 1;
        match self.resolve_local(enclosing_idx, name) {
            Some(local) => {
                self.compilers[enclosing_idx].locals[local].is_captured = true;
                Some(self.add_upvalue(compiler_idx, local, true))
            }
            None => match self.resolve_upvalue(enclosing_idx, name) {
                Some(upvalue) => Some(self.add_upvalue(compiler_idx, upvalue, false)),
                None => None,
            },
        }
    }

    fn add_local(&mut self, name: Token<'src>) {
        let local = Local {
            name,
            depth: -1,
            is_captured: false,
        };
        if let Some(compiler) = self.compilers.last_mut() {
            compiler.locals.push(local);
        }
    }

    fn declare_variable(&mut self) {
        if let Some(compiler) = self.compilers.last_mut() {
            if compiler.scope_depth == 0 {
                return;
            }

            let name = self.previous.expect("Just consumed Identifier Token.");
            let mut var_exists = false;
            for local in compiler.locals.iter().rev() {
                if local.depth < compiler.scope_depth {
                    break;
                }

                if local.name.lexeme == name.lexeme {
                    var_exists = true;
                    break;
                }
            }
            if var_exists {
                self.error_at(name, "Already a variable with this name in this scope.");
            }

            self.add_local(name);
        }
    }

    fn parse_variable(&mut self, error_message: &str) -> usize {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable();
        if let Some(compiler) = self.compilers.last() {
            if compiler.scope_depth > 0 {
                return 0;
            }
        }

        let name = self.previous.expect("Just consumed Identifier Token.");
        self.identifier_constant(name)
    }

    fn mark_initialized(&mut self) {
        if let Some(compiler) = self.compilers.last_mut() {
            if compiler.scope_depth == 0 {
                return;
            }
            // The compiler locals Vec will always be non-empty here, so will always
            // produce a valid index.
            let idx = compiler.locals.len() - 1;
            compiler.locals[idx].depth = compiler.scope_depth;
        }
    }

    fn define_variable(&mut self, global_offset: usize) {
        if let Some(compiler) = self.compilers.last() {
            if compiler.scope_depth > 0 {
                self.mark_initialized();
                return;
            }
        }

        let line = match self.previous {
            Some(ref token) => token.line,
            None => 1,
        };
        self.current_chunk().push_val_offset_op(
            global_offset,
            line,
            OpCode::DefineGlobal,
            OpCode::DefineGlobalLong,
        );
    }

    fn argument_list(&mut self) -> u8 {
        let mut arg_count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error_at(
                        self.previous.unwrap(),
                        "Can't have more than 255 arguments.",
                    );
                    break;
                }
                arg_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    fn and_(&mut self) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);

        self.emit_byte(OpCode::Pop as u8);
        self.parse_precedence(Precedence::And);

        self.patch_jump(end_jump);
    }

    fn or_(&mut self) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit_byte(OpCode::Pop as u8);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && self.current.is_some() {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn function(&mut self, function_type: FunctionType) {
        let name = match function_type {
            FunctionType::Function | FunctionType::Method | FunctionType::Initializer => {
                Some(self.vm.strings.intern(self.previous.unwrap().lexeme))
            }
            FunctionType::Script => None,
        };
        self.compilers.push(Compiler::new(function_type, name));
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                let compiler_idx = self.compilers.len() - 1;
                if self.compilers[compiler_idx].function.arity == 255 {
                    self.error_at(
                        self.current.unwrap_or_else(|| self.create_eof_token()),
                        "Can't have more than 255 parameters.",
                    );
                }
                self.compilers[compiler_idx].function.arity += 1;
                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        let compiler = end_compiler(self);
        self.add_constant_and_emit(
            Value::Function(Rc::new(compiler.function)),
            OpCode::Closure,
            OpCode::ClosureLong,
        );

        for upvalue in compiler.upvalues {
            match upvalue.is_local {
                true => self.emit_byte(1),
                false => self.emit_byte(0),
            }
            self.emit_byte(upvalue.index as u8);
        }
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");
        let str_id = self.vm.strings.intern(&self.previous.unwrap().lexeme);
        let function_type = match str_id == self.vm.strings.intern("init") {
            true => FunctionType::Initializer,
            false => FunctionType::Method,
        };

        self.function(function_type);
        self.add_constant_and_emit(Value::String(str_id), OpCode::Method, OpCode::MethodLong);
    }

    fn class_declaration(&mut self) {
        // Consumes class identifier. If global_offset value is 0, then not a global var.
        let global_offset = self.parse_variable("Expect class name.");

        let class_name = self.previous.expect("Will contain class name.");
        let str_id = self.vm.strings.intern(class_name.lexeme);
        self.add_constant_and_emit(Value::String(str_id), OpCode::Class, OpCode::ClassLong);
        self.define_variable(global_offset);

        self.class_compilers.push(ClassCompiler::new());

        if self.match_token(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expect superclass name.");
            self.variable(false);
            if class_name.lexeme == self.previous.unwrap().lexeme {
                self.error_at(self.previous.unwrap(), "A class can't inherit from itself.");
            }

            self.begin_scope();
            self.add_local(self.synthetic_token("super"));
            self.define_variable(0);

            self.named_variable(class_name, false);
            self.emit_byte(OpCode::Inherit as u8);
            if let Some(compiler) = self.class_compilers.last_mut() {
                compiler.has_super_class = true;
            }
        }

        self.named_variable(class_name, false);
        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");
        while !self.check(TokenType::RightBrace) && self.current.is_some() {
            self.method();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        self.emit_byte(OpCode::Pop as u8);

        if self.class_compilers[self.class_compilers.len() - 1].has_super_class {
            self.end_scope();
        }

        self.class_compilers.pop();
    }

    fn fun_declaration(&mut self) {
        let global_offset = self.parse_variable("Expect function name");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global_offset);
    }

    fn var_declaration(&mut self) {
        let global_offset = self.parse_variable("Expect variable name.");

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

    fn for_statement(&mut self) {
        self.loop_depth += 1;
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.match_token(TokenType::Semicolon) {
            // No initializer
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().code.len();
        let exit_jump = match self.match_token(TokenType::Semicolon) {
            true => None,
            false => {
                self.expression();
                self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

                // Jump out of the loop if the condition is false.
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
                self.emit_byte(OpCode::Pop as u8);
                Some(exit_jump)
            }
        };

        if !self.match_token(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.current_chunk().code.len();
            self.expression();
            self.emit_byte(OpCode::Pop as u8);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }
        self.loop_condition_offsets.push(loop_start);

        self.statement();
        self.emit_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit_byte(OpCode::Pop as u8);
        }
        self.end_scope();
        self.loop_depth -= 1;
        self.loop_condition_offsets.pop();
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after 'if'.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop as u8);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(then_jump);
        self.emit_byte(OpCode::Pop as u8);

        if self.match_token(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn switch_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'switch'.");
        self.expression();
        // Creates local with lexeme "switch" to hold switch expression condition.
        let switch_value_token = Token {
            token_type: TokenType::Identifier,
            lexeme: "switch",
            line: self.previous.expect("Will always be Some variant").line,
        };
        let switch_value_offset = self.compilers[self.compilers.len() - 1].locals.len();
        self.add_local(switch_value_token);
        self.mark_initialized();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");

        self.consume(TokenType::LeftBrace, "Expect '{' before switch cases.");
        let mut case_statement_jumps = Vec::new();
        while self.match_token(TokenType::Case) {
            self.expression();
            self.consume(TokenType::Colon, "Expect ':' after case expression.");
            // Evaluate case value expression.
            let line = self.previous.expect("Will always be Some variant").line;
            self.current_chunk().push_val_offset_op(
                switch_value_offset,
                line,
                OpCode::GetLocal,
                OpCode::GetLocalLong,
            );
            self.emit_byte(OpCode::Equal as u8);

            // Jump to next case if false.
            let skip_case_jump = self.emit_jump(OpCode::JumpIfFalse);

            // Else execute case statement(s) and exit switch block.
            self.emit_byte(OpCode::Pop as u8);
            self.statement();
            case_statement_jumps.push(self.emit_jump(OpCode::Jump));

            self.patch_jump(skip_case_jump);
            self.emit_byte(OpCode::Pop as u8);
        }

        if self.match_token(TokenType::Default) {
            self.consume(TokenType::Colon, "Expect ':' after default.");
            self.statement();
        }
        self.consume(
            TokenType::RightBrace,
            "Expect '}' after final switch case or default.",
        );

        for jump_offset in case_statement_jumps {
            self.patch_jump(jump_offset);
        }
        self.end_scope();
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print as u8);
    }

    fn return_statement(&mut self) {
        let function_type = self.compilers[self.compilers.len() - 1].function_type;
        match function_type {
            FunctionType::Script => {
                self.error_at(self.previous.unwrap(), "Can't return from top-level code.")
            }
            _ => (),
        }

        if self.match_token(TokenType::Semicolon) {
            self.emit_return();
        } else {
            match function_type {
                FunctionType::Initializer => self.error_at(
                    self.previous.unwrap(),
                    "Can't return a value from an initializer.",
                ),
                _ => (),
            }
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit_byte(OpCode::Return as u8);
        }
    }

    fn while_statement(&mut self) {
        self.loop_depth += 1;
        let loop_start = self.current_chunk().code.len();
        self.loop_condition_offsets.push(loop_start);
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop as u8);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::Pop as u8);
        self.loop_depth -= 1;
        self.loop_condition_offsets.pop();
    }

    fn continue_statement(&mut self) {
        if self.loop_depth == 0 {
            self.error_at(
                self.previous.expect("Will always be Some variant"),
                "continue statements must be enclosed in a loop.",
            );
        } else {
            let offset = self.loop_condition_offsets[self.loop_condition_offsets.len() - 1];
            self.emit_loop(offset);
            self.consume(TokenType::Semicolon, "Expect ';' after 'continue'.");
        }
    }

    fn synchronize(&mut self) {
        self.panic_mode.set(false);

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
        if self.match_token(TokenType::Class) {
            self.class_declaration();
        } else if self.match_token(TokenType::Fun) {
            self.fun_declaration();
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode.get() {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::Return) {
            self.return_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::Switch) {
            self.switch_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.match_token(TokenType::Continue) {
            self.continue_statement();
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
        self.add_constant_and_emit(value, OpCode::Constant, OpCode::ConstantLong);
    }

    fn string(&mut self) {
        // Trims the leading and trailing '"' characters from the lexeme to make the String.
        let str_val = match self.previous {
            Some(ref token) => &token.lexeme[1..token.lexeme.len() - 1],
            None => {
                panic!("There was no consumed previous Token when Parser::string() was called.")
            }
        };
        let str_id = self.vm.strings.intern(str_val);
        self.add_constant_and_emit(
            Value::String(str_id),
            OpCode::Constant,
            OpCode::ConstantLong,
        );
    }

    fn named_variable(&mut self, name: Token<'src>, can_assign: bool) {
        let (mut get_op, mut get_op_long) = (OpCode::GetLocal, OpCode::GetLocalLong);
        let (mut set_op, mut set_op_long) = (OpCode::SetLocal, OpCode::SetLocalLong);

        let compiler_idx = self.compilers.len() - 1;
        let offset = match self.resolve_local(compiler_idx, name) {
            Some(idx) => idx,
            None => match self.resolve_upvalue(compiler_idx, name) {
                Some(idx) => {
                    (get_op, get_op_long) = (OpCode::GetUpvalue, OpCode::GetUpvalueLong);
                    (set_op, set_op_long) = (OpCode::SetUpvalue, OpCode::SetUpvalueLong);
                    idx
                }
                None => {
                    (get_op, get_op_long) = (OpCode::GetGlobal, OpCode::GetGlobalLong);
                    (set_op, set_op_long) = (OpCode::SetGlobal, OpCode::SetGlobalLong);
                    self.identifier_constant(name)
                }
            },
        };

        let line = match self.previous {
            Some(ref token) => token.line,
            None => 1,
        };

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.current_chunk()
                .push_val_offset_op(offset, line, set_op, set_op_long);
        } else {
            self.current_chunk()
                .push_val_offset_op(offset, line, get_op, get_op_long);
        }
    }

    fn variable(&mut self, can_assign: bool) {
        let previous = self.previous.expect("Will always be Some variant.");
        self.named_variable(previous, can_assign);
    }

    fn synthetic_token(&self, text: &'src str) -> Token<'src> {
        Token::new(TokenType::Synthetic, text, 0)
    }

    fn super_(&mut self) {
        match self.class_compilers.last() {
            Some(current_class) => {
                if !current_class.has_super_class {
                    self.error_at(
                        self.previous.expect("Will have 'super' token"),
                        "Can't use 'super' in a class with no superclass.",
                    );
                }
            }
            None => self.error_at(
                self.previous.expect("Will have 'super' token"),
                "Can't use 'super' outside of a class.",
            ),
        }

        self.consume(TokenType::Dot, "Expect '.' after 'super'.");
        self.consume(TokenType::Identifier, "Expect superclass method name.");
        let name = self.vm.strings.intern(self.previous.unwrap().lexeme);

        self.named_variable(self.synthetic_token("this"), false);

        if self.match_token(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable(self.synthetic_token("super"), false);
            self.add_constant_and_emit(
                Value::String(name),
                OpCode::SuperInvoke,
                OpCode::SuperInvokeLong,
            );
            self.emit_byte(arg_count);
        } else {
            self.named_variable(self.synthetic_token("super"), false);
            self.add_constant_and_emit(Value::String(name), OpCode::GetSuper, OpCode::GetSuperLong);
        }
    }

    fn this_(&mut self) {
        if self.class_compilers.len() == 0 {
            self.error_at(
                self.previous.expect("Will contain 'this' token."),
                "Can't use 'this' outside of a class.",
            );
            return;
        }
        self.variable(false);
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

    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCode::Call as u8, arg_count);
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self
            .vm
            .strings
            .intern(self.previous.expect("Will contain property name.").lexeme);

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.add_constant_and_emit(
                Value::String(name),
                OpCode::SetProperty,
                OpCode::SetPropertyLong,
            );
        } else if self.match_token(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.add_constant_and_emit(Value::String(name), OpCode::Invoke, OpCode::InvokeLong);
            self.emit_byte(arg_count);
        } else {
            self.add_constant_and_emit(
                Value::String(name),
                OpCode::GetProperty,
                OpCode::GetPropertyLong,
            );
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

    fn add_constant_and_emit(&mut self, value: Value, op: OpCode, op_long: OpCode) {
        let line = match self.previous {
            Some(ref token) => token.line,
            None => 1,
        };
        let offset = self.current_chunk().add_constant(value);
        self.current_chunk()
            .push_val_offset_op(offset, line, op, op_long);
    }

    fn patch_jump(&mut self, offset: usize) {
        // -2 to adjust for the bytecode for the jump offset itself.
        let jump = self.current_chunk().code.len() - offset - 2;

        if jump > u16::MAX as usize {
            self.error_at(
                self.previous.expect("Will always be Some variant."),
                "Too much code to jump over.",
            );
        }

        self.current_chunk().code[offset] = ((jump >> 8) & 0xff) as u8;
        self.current_chunk().code[offset + 1] = (jump & 0xff) as u8;
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::Loop as u8);

        let offset = self.current_chunk().code.len() - loop_start + 2;
        if offset > u16::MAX as usize {
            self.error_at(
                self.previous.expect("Will always be Some variant."),
                "Loop body too large.",
            );
        }

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_byte(instruction as u8);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.current_chunk().code.len() - 2
    }

    fn emit_return(&mut self) {
        match self.compilers[self.compilers.len() - 1].function_type {
            FunctionType::Initializer => self.emit_bytes(OpCode::GetLocal as u8, 0),
            _ => self.emit_byte(OpCode::Nil as u8),
        }
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
            None => {
                self.error_at(
                    self.current.unwrap_or_else(|| self.create_eof_token()),
                    message,
                );
            }
        }
    }

    fn create_eof_token(&self) -> Token<'src> {
        Token::new(TokenType::Eof, "", self.scanner.line - 1)
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

    fn begin_scope(&mut self) {
        if let Some(compiler) = self.compilers.last_mut() {
            compiler.scope_depth += 1;
        }
    }

    fn end_scope(&mut self) {
        let compiler_idx = self.compilers.len() - 1;

        self.compilers[compiler_idx].scope_depth -= 1;

        let mut local_count = self.compilers[compiler_idx].locals.len();
        while local_count > 0
            && self.compilers[compiler_idx].locals[local_count - 1].depth
                > self.compilers[compiler_idx].scope_depth
        {
            if self.compilers[compiler_idx].locals[local_count - 1].is_captured {
                self.emit_byte(OpCode::CloseUpvalue as u8);
            } else {
                self.emit_byte(OpCode::Pop as u8);
            }
            self.compilers[compiler_idx].locals.pop();
            local_count -= 1;
        }
    }

    fn error_at(&self, token: Token<'src>, message: &str) {
        if self.panic_mode.get() {
            return;
        }

        self.panic_mode.set(true);

        match token.token_type {
            TokenType::Error => println!("[line {}] Error: {}", token.line, token.lexeme),
            TokenType::Eof => println!("[line {}] Error at end: {}", token.line, message),
            _ => println!(
                "[line {}] Error at '{}': {}",
                token.line, token.lexeme, message
            ),
        }

        self.had_error.set(true);
    }
}
