use crate::*;

pub struct Lexer<'a> {
    src: &'a str,
    was_at: usize,
    at_byte: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            was_at: 0,
            at_byte: 0,
        }
    }

    fn slice(&self) -> &'a str { &self.src[self.span()] }

    fn span(&self) -> Span { self.was_at..self.at_byte }

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
                    let b = self.next_char()?.to_string()
                        + self.next_char()?.to_string().as_str();

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
                while_char!($e, || { self.next_char(); });
            };
            ($e: expr, $a: expr) => {
                while let Some(c) = self.peek_next() {
                    if $e(c) {
                        $a();
                    } else {
                        break;
                    }
                }
            };
        }

        self.was_at = self.at_byte;

        let t = match self.next_char() {
            Some('\n') => Ok(Token::Newline),
            Some(c) if c.is_whitespace() => {
                while_char!(char::is_whitespace);
                return self.next();
            },
            Some(c) if c.is_alphabetic() || matches!(c, '$' | '#' | '@' | '%') => {
                while_char!(char::is_alphanumeric);
                let s = self.slice();
                let is_num = s.chars().skip(1).any(|c| c.is_numeric());

                match (c, is_num) {
                    ('$', false) => Err(LexError::InvalidReg),
                    ('#', false) => Err(LexError::InvalidMem),

                    ('r' | 'R' | '$', true) => s[1..]
                        .parse()
                        .map_or(Err(LexError::RegIdxError), |v| Ok(Token::Reg(v))),
                    ('m' | 'M' | '#', true) => s[1..]
                        .parse()
                        .map_or(Err(LexError::MemIdxError), |v| Ok(Token::Heap(v))),

                    ('@', _) => Ok(Token::Macro(&s[1..])),
                    ('%', _) => Ok(Token::Port(&s[1..])),

                    _ => Ok(Token::Name(s)),
                }
            },
            Some(c) if c.is_numeric() => match (c, self.peek_next()) {
                ('0', Some('x')) => {
                    self.next_char();
                    while_char!(|c| matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F'));
                    u128::from_str_radix(&self.slice()[2..], 16)
                        .map_or(Err(LexError::IntValueError), |v| Ok(Token::Int(v)))
                },
                _ => {
                    while_char!(char::is_numeric);
                    self.slice()
                        .parse()
                        .map_or(Err(LexError::IntValueError), |v| Ok(Token::Int(v)))
                },
            },
            Some('.') => {
                while_char!(char::is_alphanumeric);
                Ok(Token::Label(&self.slice()[1..]))
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
                while_char!(|c| c != '"', || ch.push(self.get_char_esc().unwrap()));
                self.next_char();
                Ok(Token::String(ch))
            },
            Some('[') => Ok(Token::SqBrStart),
            Some(']') => Ok(Token::SqBrEnd),
            Some(_) => Err(LexError::UnknownChar),
            None => return None,
        };

        let s = self.span();
        Some((s, t))
    }
}

#[derive(Debug, Clone)]
pub enum Token<'a> {
    Name(&'a str),
    Int(u128),
    Reg(u128),
    Heap(u128),
    Macro(&'a str),
    Newline,
    Label(&'a str),
    String(Vec<u32>),
    SqBrStart,
    SqBrEnd,
    Port(&'a str),
}

#[derive(Debug, Clone)]
pub enum LexError {
    InvalidMem,
    InvalidReg,
    IntValueError,
    RegIdxError,
    MemIdxError,
    ImproperComment,
    UnknownChar,
    UnclosedStr,
    CharError,
}

pub type LexResult<'a> = (Span, Result<Token<'a>, LexError>);
