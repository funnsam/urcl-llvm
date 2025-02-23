use core::str::FromStr;

use dashu::{Integer, Real};
use num_traits::ToPrimitive;
use urcl_ast::{Port, Register};

pub type Lexer<'a> = logos::Lexer<'a, Token<'a>>;
pub type LexResult<'a> = Result<Token<'a>, ()>;

#[derive(Debug, Clone, logos::Logos, strum::EnumTryAs)]
#[logos(skip r"\s+")]
pub enum Token<'a> {
    #[regex(r"[\+\-]?(\d+|0b[01]+|0o[0-7]+|0x[0-9a-zA-Z]+)", callback = |lex| parse_int(lex.slice()))]
    Integer(Integer),
    #[regex(r"\%(\d+|0b[01]+|0o[0-7]+|0x[0-9a-zA-Z]+)", callback = |lex| Port::from_repr(parse_int(&lex.slice()[1..])?.to_usize()?))]
    #[regex(r"\%[a-zA-Z][0-9a-zA-Z]*", callback = |lex| Port::from_str(&lex.slice()[1..]).ok())]
    Port(Port),
    #[regex(r"[rR](\d+|0b[01]+|0o[0-7]+|0x[0-9a-zA-Z]+)", callback = |lex| Some(Register::General(parse_int(&lex.slice()[1..])?.to_u16()?)))]
    #[token("pc", callback = |_| Register::Pc, ignore(ascii_case))]
    #[token("sp", callback = |_| Register::Sp, ignore(ascii_case))]
    Reg(Register),
    #[regex(r"[mM](\d+|0b[01]+|0o[0-7]+|0x[0-9a-zA-Z]+)", callback = |lex| parse_int(&lex.slice()[1..]))]
    Heap(Integer),
    #[regex(r"\~[\+\-]?(\d+|0b[01]+|0o[0-7]+|0x[0-9a-zA-Z]+)", callback = |lex| parse_int(&lex.slice()[1..]))]
    Relative(Integer),
    #[regex(r"\d+\.\d*", callback = |lex| lex.slice().parse().ok())]
    Float(Real),

    #[regex(r#""([^"]|\\")*""#, callback = |lex| parse_str(lex.slice()))]
    String(Vec<u32>),

    #[regex(r"[a-zA-Z][0-9a-zA-Z_]*")]
    Name(&'a str),
    #[regex(r"\@[a-zA-Z][0-9a-zA-Z_]*", callback = |lex| Some(&lex.slice()[1..]))]
    Macro(&'a str),
    #[regex(r"\.[a-zA-Z][0-9a-zA-Z_]*", callback = |lex| Some(&lex.slice()[1..]))]
    Label(&'a str),

    #[token("\n", priority = 999)]
    Newline,
    #[token("[")]
    SqBrStart,
    #[token("]")]
    SqBrEnd,
    #[token("<=")]
    CmpLe,
    #[token(">=")]
    CmpGe,
    #[token("==")]
    CmpEq,
}

fn parse_int(lex: &str) -> Option<Integer> {
    if lex.as_bytes()[0].is_ascii_digit() {
        match lex.as_bytes().get(2) {
            Some(b'b') => Some(-Integer::from_str_radix(&lex[3..], 2).ok()?),
            Some(b'o') => Some(-Integer::from_str_radix(&lex[3..], 8).ok()?),
            Some(b'x') => Some(-Integer::from_str_radix(&lex[3..], 16).ok()?),
            _ => lex.parse().ok(),
        }
    } else {
        match lex.as_bytes().get(1) {
            Some(b'b') => Integer::from_str_radix(&lex[2..], 2).ok(),
            Some(b'o') => Integer::from_str_radix(&lex[2..], 8).ok(),
            Some(b'x') => Integer::from_str_radix(&lex[2..], 16).ok(),
            _ => lex.parse().ok(),
        }
    }
}

fn parse_str(lex: &str) -> Option<Vec<u32>> {
    fn get_char_esc<I: Iterator<Item = char>>(iter: &mut I) -> Option<u32> {
        match iter.next()? {
            '\\' => match iter.next()? {
                '0' => Some('\0' as u32),
                't' => Some('\t' as u32),
                'n' => Some('\n' as u32),
                'r' => Some('\r' as u32),
                '"' => Some('\"' as u32),
                '\'' => Some('\'' as u32),
                '\\' => Some('\\' as u32),

                'x' => {
                    let b = iter.next()?.to_string() + iter.next()?.to_string().as_str();

                    Some(u8::from_str_radix(&b, 16).ok()? as u32)
                },
                'u' => {
                    let mut b = String::with_capacity(4);
                    for _ in 0..4 { b.push(iter.next()?) };

                    u32::from_str_radix(&b, 16).ok()
                },
                _ => None,
            },
            c => Some(c as u32),
        }
    }

    let mut lex = lex.chars();
    let mut res = Vec::new();
    while !lex.as_str().is_empty() {
        res.push(get_char_esc(&mut lex)?);
    }
    Some(res)
}
