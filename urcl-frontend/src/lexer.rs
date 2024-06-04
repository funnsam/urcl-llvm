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

    fn span(&self) -> Span {
        self.was_at..self.at_byte
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.src[self.at_byte..].chars().nth(0);
        self.at_byte += c.map_or(0, |c| c.len_utf8());
        c
    }

    fn peek_next(&mut self) -> Option<char> {
        self.src[self.at_byte..].chars().nth(0)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<LexResult<'a>> {
        macro_rules! while_char {
            ($e: expr) => {
                while let Some(c) = self.peek_next() {
                    if $e(c) {
                        self.next_char();
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
            Some(c) if c.is_alphabetic() || matches!(c, '$' | '#' | '@') => {
                while_char!(char::is_alphanumeric);
                let s = &self.src[self.span()];
                let is_num = s.chars().skip(1).any(|c| c.is_numeric());

                match (c, is_num) {
                    ('$', false) => Err(LexError::InvalidReg),
                    ('#', false) => Err(LexError::InvalidMem),

                    ('r' | 'R' | '$', true) => s[1..].parse().map_or(Err(LexError::RegIdxTooBig), |v| Ok(Token::Reg(v))),
                    ('m' | 'M' | '#', true) => s[1..].parse().map_or(Err(LexError::MemIdxTooBig), |v| Ok(Token::Heap(v))),
                    ('@', _) => Ok(Token::Macro(&s[1..])),
                    _ => Ok(Token::Name(s)),
                }
            },
            Some(c) if c.is_numeric() => {
                while_char!(char::is_numeric);
                if let Ok(v) = self.src[self.span()].parse() {
                    Ok(Token::Int(v))
                } else {
                    Err(LexError::IntValueTooBig)
                }
            },
            Some('/') => {
                match self.next_char() {
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
                }
            },
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
}

#[derive(Debug, Clone)]
pub enum LexError {
    InvalidMem,
    InvalidReg,
    IntValueTooBig,
    RegIdxTooBig,
    MemIdxTooBig,
    ImproperComment,
    UnknownChar,
}

pub type LexResult<'a> = (Span, Result<Token<'a>, LexError>);
