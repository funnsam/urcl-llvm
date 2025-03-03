use dashu::{Decimal, Integer};
use num_traits::ToPrimitive;
use urcl_ast::Register;

pub type Lexer<'a> = logos::Lexer<'a, Token<'a>>;
pub type LexResult<'a> = Result<Token<'a>, ()>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, logos::Logos, strum::EnumTryAs)]
#[logos(skip r"\s")]
#[logos(skip r"//[^\n]*")]
pub enum Token<'a> {
    #[regex(r"[\+\-]?(\d+|0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+)", callback = |lex| parse_int(lex.slice()))]
    #[regex(r"'([^\\']|\\'|\\[^']+)'", callback = |lex| parse_char(&lex.slice()[1..]))]
    Integer(Integer),
    #[regex(r"\%(\d+|0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+)", callback = |lex| parse_int(&lex.slice()[1..])?.to_u8(), priority = 999)]
    PortInt(u8),
    #[token("%", callback = |lex| continue_name(lex, 1))]
    Port(&'a str),
    #[regex(r"[rR](\d+|0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+)", callback = |lex| Some(Register::General(parse_int(&lex.slice()[1..])?.to_u16()?)))]
    #[token("pc", callback = |_| Register::Pc, ignore(ascii_case))]
    #[token("sp", callback = |_| Register::Sp, ignore(ascii_case))]
    Reg(Register),
    #[regex(r"[mM](\d+|0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+)", callback = |lex| parse_int(&lex.slice()[1..]))]
    Heap(Integer),
    #[regex(r"\~[\+\-]?(\d+|0b[01]+|0o[0-7]+|0x[0-9a-fA-F]+)", callback = |lex| parse_int(&lex.slice()[1..]))]
    Relative(Integer),
    #[regex(r"[\+\-]?\d+\.\d*([Ee][\+\-]?\d+)?", callback = |lex| Some(lex.slice().parse::<Decimal>().ok()?.into()))]
    Float(DecimalHash),

    #[regex(r#""([^"]|\\")*""#, callback = |lex| parse_str(&lex.slice()[1..]))]
    String(Vec<u32>),

    #[regex(r"[a-zA-Z_]", callback = |lex| continue_name(lex, 0))]
    Name(&'a str),
    #[token("@", callback = |lex| continue_name(lex, 1))]
    Macro(&'a str),
    #[token(".", callback = |lex| continue_name(lex, 1))]
    Label(&'a str),

    #[token("\n", priority = 999)]
    Newline,
    #[token("(")]
    ParenStart,
    #[token(")")]
    ParenEnd,
    #[token("[")]
    BrStart,
    #[token("]")]
    BrEnd,
    #[token("<=")]
    CmpLe,
    #[token(">=")]
    CmpGe,
    #[token("==")]
    CmpEq,
}

fn parse_int(lex: &str) -> Option<Integer> {
    if lex.as_bytes()[0].is_ascii_digit() {
        match lex.as_bytes().get(1) {
            Some(b'b') => Integer::from_str_radix(&lex[2..], 2).ok(),
            Some(b'o') => Integer::from_str_radix(&lex[2..], 8).ok(),
            Some(b'x') => Integer::from_str_radix(&lex[2..], 16).ok(),
            _ => lex.parse().ok(),
        }
    } else {
        match lex.as_bytes().get(2) {
            Some(b'b') => Some(-Integer::from_str_radix(&lex[3..], 2).ok()?),
            Some(b'o') => Some(-Integer::from_str_radix(&lex[3..], 8).ok()?),
            Some(b'x') => Some(-Integer::from_str_radix(&lex[3..], 16).ok()?),
            _ => lex.parse().ok(),
        }
    }
}

fn parse_char(lex: &str) -> Option<Integer> {
    let mut lex = lex.chars();
    let ch = get_char_esc(&mut lex)?.into();

    (lex.as_str().len() <= 1).then_some(ch)
}

fn parse_str(lex: &str) -> Option<Vec<u32>> {
    let mut lex = lex.chars();
    let mut res = Vec::new();
    while lex.as_str().len() > 1 {
        res.push(get_char_esc(&mut lex)?);
    }
    Some(res)
}

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
                for _ in 0..4 {
                    b.push(iter.next()?)
                }

                u32::from_str_radix(&b, 16).ok()
            },
            _ => None,
        },
        c => Some(c as u32),
    }
}

fn continue_name<'a>(lex: &mut Lexer<'a>, start: usize) -> &'a str {
    loop {
        let mut chars = lex.remainder().chars();

        match chars.next() {
            Some('/') => {
                if matches!(chars.next(), Some('/' | '*')) {
                    break;
                } else {
                    lex.bump(1);
                }
            },
            Some(ch) if ch.is_whitespace() => break,
            Some('%') if start == 0 => break,
            Some('(' | ')') | None => break,
            Some(ch) => lex.bump(ch.len_utf8()),
        }
    }

    &lex.slice()[start..]
}

impl Token<'_> {
    pub fn is_macro(&self, m: &str) -> bool {
        if let Token::Macro(t) = self {
            t.eq_ignore_ascii_case(m)
        } else {
            false
        }
    }

    pub fn is_name(&self, m: &str) -> bool {
        if let Token::Name(t) = self {
            t.eq_ignore_ascii_case(m)
        } else {
            false
        }
    }

    pub fn is_macro_or_name(&self, m: &str) -> bool {
        if let Token::Macro(t) | Token::Name(t) = self {
            t.eq_ignore_ascii_case(m)
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DecimalHash(Decimal);

impl core::hash::Hash for DecimalHash {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.repr().significand().hash(state);
        self.repr().exponent().hash(state);
    }
}

impl From<Decimal> for DecimalHash {
    fn from(value: Decimal) -> Self { Self(value) }
}

impl From<DecimalHash> for Decimal {
    fn from(value: DecimalHash) -> Self { value.0 }
}

impl core::ops::Deref for DecimalHash {
    type Target = Decimal;

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl core::ops::DerefMut for DecimalHash {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
}
