use crate::*;

const BASE_PREFIXES: &[(char, u32)] = &[('x', 16), ('o', 8), ('b', 2)];

#[derive(Debug)]
pub struct Lexer<'a> {
    src: &'a str,
    was_at: usize,
    at_byte: usize,
    float_t: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, float_t: usize) -> Self {
        Self {
            src,
            was_at: 0,
            at_byte: 0,
            float_t,
        }
    }

    fn slice(&self) -> &'a str { &self.src[self.span()] }

    pub fn span(&self) -> Span { self.was_at..self.at_byte }

    fn next_char(&mut self) -> Option<char> {
        let c = self.src[self.at_byte..].chars().nth(0);
        self.at_byte += c.map_or(0, |c| c.len_utf8());
        c
    }

    fn peek_next(&mut self) -> Option<char> { self.src[self.at_byte..].chars().nth(0) }

    fn get_char_esc(&mut self) -> Option<u32> {
        match self.next_char()? {
            '\\' => match self.next_char()? {
                '0' => Some('\0' as u32),
                't' => Some('\t' as u32),
                'n' => Some('\n' as u32),
                'r' => Some('\r' as u32),
                '"' => Some('\"' as u32),
                '\'' => Some('\'' as u32),
                '\\' => Some('\\' as u32),

                'x' => {
                    let b = self.next_char()?.to_string() + self.next_char()?.to_string().as_str();

                    Some(u8::from_str_radix(&b, 16).ok()? as u32)
                },
                'u' => {
                    let b = self.next_char()?.to_string()
                        + self.next_char()?.to_string().as_str()
                        + self.next_char()?.to_string().as_str()
                        + self.next_char()?.to_string().as_str()
                        + self.next_char()?.to_string().as_str();

                    Some(u32::from_str_radix(&b, 16).ok()?)
                },
                _ => None,
            },
            c => Some(c as u32),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<LexResult<'a>> {
        macro_rules! while_char {
            ($e: expr) => {
                while_char!($e, || {
                    self.next_char();
                });
            };
            ($e: expr, $a: expr) => {
                while let Some(c) = self.peek_next() {
                    #[allow(clippy::blocks_in_conditions)]
                    if $e(c) {
                        $a();
                    } else {
                        break;
                    }
                }
            };
            (; $e: expr) => {
                while_char!(; $e, || {
                    self.next_char();
                });
            };
            (; $e: expr, $a: expr) => {
                let mut not_ok = true;
                while let Some(c) = self.peek_next() {
                    #[allow(clippy::blocks_in_conditions)]
                    if $e(c) {
                        $a();
                    } else {
                        not_ok = false;
                        break;
                    }
                }

                if not_ok {
                    return Some(Err(LexError::UnexpectedEof));
                }
            };
        }

        self.was_at = self.at_byte;

        Some(match self.next_char() {
            Some('\n') => Ok(Token::Newline),
            Some(c) if c.is_whitespace() => {
                while_char!(|c: char| c.is_whitespace() && c != '\n');
                return self.next();
            },
            Some(c) if c.is_alphabetic() || matches!(c, '$' | '#' | '@' | '%' | '.') => {
                while_char!(is_ident);
                let s = self.slice();
                let is_num = !s.chars().skip(1).any(|c| !c.is_ascii_digit());

                match (c, is_num) {
                    ('$', false) => Err(LexError::InvalidReg),
                    ('#', false) => Err(LexError::InvalidMem),

                    ('r' | 'R' | '$', true) => {
                        s[1..].parse().map_or(Err(LexError::IntValueError), |v| {
                            Ok(Token::Reg(ast::Register::General(v)))
                        })
                    },
                    ('m' | 'M' | '#', true) => s[1..]
                        .parse()
                        .map_or(Err(LexError::IntValueError), |v| Ok(Token::Heap(v))),
                    ('%', true) => s[1..]
                        .parse()
                        .map_or(Err(LexError::IntValueError), |v| Ok(Token::Int(v))),

                    ('%', false) => Ok(Token::Port(&s[1..])),
                    ('@', _) => Ok(Token::Macro(&s[1..])),
                    ('.', _) => Ok(Token::Label(&s[1..])),

                    _ if s.eq_ignore_ascii_case("pc") => Ok(Token::Reg(ast::Register::Pc)),
                    _ if s.eq_ignore_ascii_case("sp") => Ok(Token::Reg(ast::Register::Sp)),
                    _ => Ok(Token::Name(s)),
                }
            },
            Some('~') => {
                if matches!(self.peek_next(), Some('+' | '-')) {
                    self.next_char();
                }

                while_char!(|c: char| c.is_ascii_digit());
                self.slice()[1..]
                    .parse::<i128>()
                    .map_or(Err(LexError::IntValueError), |v| Ok(Token::Relative(v)))
            },
            Some(c) if c.is_ascii_digit() || c == '-' => match (c, self.peek_next()) {
                ('0', Some(c)) if BASE_PREFIXES.iter().any(|i| i.0 == c) => {
                    self.next_char();
                    while_char!(|c: char| c.is_ascii_hexdigit());
                    u128::from_str_radix(
                        &self.slice()[2..],
                        BASE_PREFIXES.iter().find(|i| i.0 == c).unwrap().1,
                    )
                    .map_or(Err(LexError::IntValueError), |v| Ok(Token::Int(v)))
                },
                _ => {
                    let mut after_dot = false;
                    while_char!(|c: char| {
                        if !after_dot && c == '.' {
                            after_dot = true;
                            return true;
                        }

                        c.is_ascii_digit()
                    });

                    if !after_dot {
                        self.slice()
                            .parse::<i128>()
                            .map_or(Err(LexError::IntValueError), |v| Ok(Token::Int(v as u128)))
                    } else if self.float_t == 32 {
                        self.slice()
                            .parse::<f32>()
                            .map_or(Err(LexError::FloatValueError), |v| {
                                Ok(Token::Int(v.to_bits() as u128))
                            })
                    } else if self.float_t == 64 {
                        self.slice()
                            .parse::<f64>()
                            .map_or(Err(LexError::FloatValueError), |v| {
                                Ok(Token::Int(v.to_bits() as u128))
                            })
                    } else {
                        Err(LexError::CantParseFloatInPrec)
                    }
                },
            },
            Some('/') => match self.next_char() {
                Some('/') => {
                    while_char!(|c| c != '\n');
                    return self.next();
                },
                Some('*') => {
                    while let Some(c) = self.next_char() {
                        if c == '*' && self.peek_next() == Some('/') {
                            self.next_char();
                            break;
                        }
                    }

                    return self.next();
                },
                _ => Err(LexError::ImproperComment),
            },
            Some('\'') => {
                if let Some(c) = self.get_char_esc() {
                    if self.next_char() == Some('\'') {
                        Ok(Token::Int(c as u128))
                    } else {
                        Err(LexError::UnclosedStr)
                    }
                } else {
                    Err(LexError::CharError)
                }
            },
            Some('"') => {
                let mut ch = Vec::new();
                while_char!(; |c| c != '"', || ch.push(self.get_char_esc().unwrap()));
                self.next_char();
                Ok(Token::String(ch))
            },
            Some('[') => Ok(Token::SqBrStart),
            Some(']') => Ok(Token::SqBrEnd),
            Some(c) if matches!(c, '=' | '<' | '>') => match self.peek_next() {
                Some('=') => {
                    self.next_char();
                    match c {
                        '<' => Ok(Token::CmpLe),
                        '>' => Ok(Token::CmpGe),
                        '=' => Ok(Token::CmpEq),
                        _ => unreachable!(),
                    }
                },
                Some(_) => Err(LexError::UnknownChar),
                None => Err(LexError::UnexpectedEof),
            },
            Some(_) => Err(LexError::UnknownChar),
            None => return None,
        })
    }
}

#[derive(Debug, Clone, strum::EnumTryAs)]
pub enum Token<'a> {
    Name(&'a str),
    Int(u128),
    Reg(ast::Register),
    Heap(u128),
    Macro(&'a str),
    Newline,
    Label(&'a str),
    String(Vec<u32>),
    SqBrStart,
    SqBrEnd,
    Port(&'a str),
    CmpLe,
    CmpGe,
    CmpEq,
    Relative(i128),
}

#[derive(Debug, Clone)]
pub enum LexError {
    InvalidMem,
    InvalidReg,
    IntValueError,
    ImproperComment,
    UnknownChar,
    UnclosedStr,
    CharError,
    UnexpectedEof,
    FloatValueError,
    CantParseFloatInPrec,
}

pub type LexResult<'a> = Result<Token<'a>, LexError>;

fn is_ident(c: char) -> bool { c.is_alphanumeric() || matches!(c, '_') }
