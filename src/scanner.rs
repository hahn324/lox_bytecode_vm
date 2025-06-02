use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'src> {
    pub token_type: TokenType,
    pub lexeme: &'src str,
    pub line: usize,
}
impl<'src> Token<'src> {
    pub fn new(token_type: TokenType, lexeme: &'src str, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            line,
        }
    }
}
impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} '{}'", self.token_type, self.lexeme)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Colon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String,
    Number,
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Switch,
    Case,
    Default,
    Continue,

    Error,
    Eof,
    Synthetic,
}

pub struct Scanner<'src> {
    rest: &'src str,
    pub line: usize,
}
impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            rest: source,
            line: 1,
        }
    }

    fn if_match_else(
        &mut self,
        start: &'src str,
        offset: usize,
        target: char,
        if_type: TokenType,
        else_type: TokenType,
    ) -> Token<'src> {
        if self.rest.starts_with(target) {
            self.rest = &self.rest[target.len_utf8()..];
            Token::new(if_type, &start[..offset + target.len_utf8()], self.line)
        } else {
            Token::new(else_type, &start[..offset], self.line)
        }
    }
}

impl<'src> Iterator for Scanner<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_str = &self.rest[..c.len_utf8()];
            let start = self.rest;
            self.rest = chars.as_str();

            match c {
                '(' => return Some(Token::new(TokenType::LeftParen, c_str, self.line)),
                ')' => return Some(Token::new(TokenType::RightParen, c_str, self.line)),
                '{' => return Some(Token::new(TokenType::LeftBrace, c_str, self.line)),
                '}' => return Some(Token::new(TokenType::RightBrace, c_str, self.line)),
                ';' => return Some(Token::new(TokenType::Semicolon, c_str, self.line)),
                ':' => return Some(Token::new(TokenType::Colon, c_str, self.line)),
                ',' => return Some(Token::new(TokenType::Comma, c_str, self.line)),
                '.' => return Some(Token::new(TokenType::Dot, c_str, self.line)),
                '-' => return Some(Token::new(TokenType::Minus, c_str, self.line)),
                '+' => return Some(Token::new(TokenType::Plus, c_str, self.line)),
                '*' => return Some(Token::new(TokenType::Star, c_str, self.line)),
                '/' => match self.rest.starts_with('/') {
                    true => {
                        // A comment goes until the end of the line.
                        while !self.rest.starts_with('\n') && chars.next().is_some() {
                            self.rest = chars.as_str();
                        }
                    }
                    false => return Some(Token::new(TokenType::Slash, c_str, self.line)),
                },
                '!' => {
                    return Some(self.if_match_else(
                        start,
                        c.len_utf8(),
                        '=',
                        TokenType::BangEqual,
                        TokenType::Bang,
                    ));
                }
                '=' => {
                    return Some(self.if_match_else(
                        start,
                        c.len_utf8(),
                        '=',
                        TokenType::EqualEqual,
                        TokenType::Equal,
                    ));
                }
                '<' => {
                    return Some(self.if_match_else(
                        start,
                        c.len_utf8(),
                        '=',
                        TokenType::LessEqual,
                        TokenType::Less,
                    ));
                }
                '>' => {
                    return Some(self.if_match_else(
                        start,
                        c.len_utf8(),
                        '=',
                        TokenType::GreaterEqual,
                        TokenType::Greater,
                    ));
                }
                ' ' | '\r' | '\t' => (),
                '\n' => self.line += 1,
                '"' => {
                    if let Some(offset) = self.rest.find('"') {
                        let lexeme = &start[..=offset + 1];
                        // Accounts for multi-line strings.
                        self.line += lexeme.matches('\n').count();
                        self.rest = &self.rest[offset + 1..];
                        return Some(Token::new(TokenType::String, lexeme, self.line));
                    } else {
                        self.rest = &self.rest[self.rest.len()..];
                        return Some(Token::new(
                            TokenType::Error,
                            "Unterminated string.",
                            self.line,
                        ));
                    }
                }
                '0'..='9' => {
                    let next_non_digit = start
                        .find(|c: char| !c.is_ascii_digit())
                        .unwrap_or_else(|| start.len());
                    // Set self.rest to start on the first non-digit char.
                    self.rest = &self.rest[next_non_digit - 1..];
                    if self.rest.starts_with('.') {
                        let next_non_digit = self.rest[1..]
                            .find(|c: char| !c.is_ascii_digit())
                            .unwrap_or_else(|| self.rest[1..].len());
                        // Only update self.rest if more digits were found after the '.'
                        if next_non_digit > 0 {
                            self.rest = &self.rest[next_non_digit + 1..];
                        }
                    }
                    return Some(Token::new(
                        TokenType::Number,
                        &start[..start.len() - self.rest.len()],
                        self.line,
                    ));
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let next_non_ident = start
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                        .unwrap_or_else(|| start.len());

                    let lexeme = &start[..next_non_ident];
                    self.rest = &self.rest[next_non_ident - c.len_utf8()..];

                    let token_type = match lexeme {
                        "and" => TokenType::And,
                        "class" => TokenType::Class,
                        "else" => TokenType::Else,
                        "false" => TokenType::False,
                        "for" => TokenType::For,
                        "fun" => TokenType::Fun,
                        "if" => TokenType::If,
                        "nil" => TokenType::Nil,
                        "or" => TokenType::Or,
                        "print" => TokenType::Print,
                        "return" => TokenType::Return,
                        "super" => TokenType::Super,
                        "this" => TokenType::This,
                        "true" => TokenType::True,
                        "var" => TokenType::Var,
                        "while" => TokenType::While,
                        "switch" => TokenType::Switch,
                        "case" => TokenType::Case,
                        "default" => TokenType::Default,
                        "continue" => TokenType::Continue,
                        _ => TokenType::Identifier,
                    };

                    return Some(Token::new(token_type, lexeme, self.line));
                }
                _ => {
                    return Some(Token::new(
                        TokenType::Error,
                        format!("Unexpected character '{c_str}'.").leak(),
                        self.line,
                    ));
                }
            }
        }
    }
}
