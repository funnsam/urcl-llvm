use core::fmt;

#[derive(Debug, Clone)]
pub struct Program {
    // headers
    pub bits: usize,
    pub registers: usize,
    pub stack_size: usize,
    pub heap_size: usize,

    // body
    pub instructions: Vec<Instruction>,
    pub dw: Vec<u128>,
}

macro_rules! instr {
    ($($name: ident [$($arg: tt)*]),* $(,)?) => {
        #[derive(Debug, Clone)]
        pub enum Instruction {
            $($name($(instr!($arg)),*)),*
        }

        impl Instruction {
            pub(crate) fn properties(i: &str) -> Option<InstProperties> {
                $(if i.eq_ignore_ascii_case(stringify!($name)) {
                    return Some(InstProperties {
                        operands: &[$(instr!(opk $arg)),*],
                    });
                })*

                None
            }

            pub(crate) fn construct(i: &str, mut o: Vec<Any>) -> Self {
                use Instruction::*;
                o.reverse();

                $(if i.eq_ignore_ascii_case(stringify!($name)) {
                    return $name($(instr!(fromany $arg o.pop().unwrap())),*);
                })*

                panic!();
            }
        }
    };

    (r) => { Register };
    (i) => { Immediate };
    (a) => { Any };

    (opk r) => { OperandKind::Register };
    (opk i) => { OperandKind::Immediate };
    (opk a) => { OperandKind::Any };

    (fromany r $e: expr) => {
        $e.try_as_register().unwrap()
    };
    (fromany i $e: expr) => {
        $e.try_as_immediate().unwrap()
    };
    (fromany a $e: expr) => {
        $e
    };
}

instr! {
    Add [r a a],
    Rsh [r a],
    Lod [r a],
    Str [a a],
    Bge [a a a],
    Nor [r a a],
    Sub [r a a],
    Jmp [a],
    Mov [r a],
    Nop [],
    Imm [r a],
    Lsh [r a],
    Inc [r a],
    Dec [r a],
    Neg [r a],
    And [r a a],
    Or [r a a],
    Not [r a],
    Xnor [r a a],
    Xor [r a a],
    Nand [r a a],
    Brl [a a a],
    Brg [a a a],
    Bre [a a a],
    Bne [a a a],
    Bod [a a],
    Bev [a a],
    Ble [a a a],
    Brz [a a],
    Bnz [a a],
    Brn [a a],
    Brp [a a],
    Psh [a],
    Pop [r],
    Cal [a],
    Ret [],
    Hlt [],
    Cpy [a a],
    Brc [a a a],
    Bnc [a a a],
    Mlt [r a a],
    Div [r a a],
    Mod [r a a],
    // TODO:
    Llod [r a a],
    Lstr [a a a],
    // TODO:
    Out [a a],
    In [r a],
}

#[derive(Clone)]
pub enum Register {
    General(u128),
    Pc,
    Sp,
}

#[derive(Clone)]
pub struct Immediate(pub u128);

#[derive(Clone, strum::EnumTryAs)]
pub enum Any {
    Register(Register),
    Immediate(Immediate),
}

impl fmt::Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::General(r) => write!(f, "R{r}"),
            Self::Pc => write!(f, "PC"),
            Self::Sp => write!(f, "SP"),
        }
    }
}

impl fmt::Debug for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.0) }
}

impl fmt::Debug for Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(r) => r.fmt(f),
            Self::Immediate(r) => r.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct InstProperties {
    pub operands: &'static [OperandKind],
}

#[derive(Debug, Clone)]
pub enum OperandKind {
    Register,
    Immediate,
    Any,
}

#[derive(Debug, Clone, strum::EnumString)]
#[strum(ascii_case_insensitive)]
pub enum Port {
    CpuBus    = 0,
    Text      = 1,
    Numb      = 2,
    Supported = 5,
    Special   = 6,
    Profile   = 7,
    X         = 8,
    Y         = 9,
    #[strum(serialize = "color", serialize = "colour")]
    Color     = 10,
    Buffer    = 11,
    GSpecial  = 15,
    Ascii8    = 16,
    Char5     = 17,
    Char6     = 18,
    Ascii7    = 19,
    Utf8      = 20,
    TSpecial  = 23,
    Int       = 24,
    Uint      = 25,
    Bin       = 26,
    Hex       = 27,
    Float     = 28,
    Fixed     = 29,
    NSpecial  = 31,
    Addr      = 32,
    Bus       = 33,
    Page      = 34,
    SSpecial  = 39,
    Rng       = 40,
    Note      = 41,
    Instr     = 42,
    NLeg      = 43,
    Wait      = 44,
    NAddr     = 45,
    Data      = 46,
    MSpecial  = 47,
    Ud1,
    Ud2,
    Ud3,
    Ud4,
    Ud5,
    Ud6,
    Ud7,
    Ud8,
    Ud9,
    Ud11,
    Ud12,
    Ud13,
    Ud14,
    Ud15,
    Ud16,
}
