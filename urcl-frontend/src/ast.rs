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
        #[derive(Debug, Clone, strum::EnumDiscriminants)]
        #[strum_discriminants(derive(strum::Display))]
        #[strum_discriminants(strum(serialize_all = "lowercase"))]
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
    Bsr [r a a],
    Bsl [r a a],
    Srs [r a],
    Bss [r a a],
    SetE [r a a],
    SetNe [r a a],
    SetG [r a a],
    SetL [r a a],
    SetGe [r a a],
    SetLe [r a a],
    SetC [r a a],
    SetNc [r a a],
    LLod [r a a],
    LStr [a a a],
    SDiv [r a a],
    SBrl [a a a],
    SBrg [r a a],
    SBle [a a a],
    SBge [a a a],
    SSetG [r a a],
    SSetL [r a a],
    SSetGe [r a a],
    SSetLe [r a a],
    Abs [r a],
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
            Self::General(r) => write!(f, "r{r}"),
            Self::Pc => write!(f, "pc"),
            Self::Sp => write!(f, "sp"),
        }
    }
}

impl fmt::Debug for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.0 as i128) }
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
    // general
    CpuBus    = 0,
    Text      = 1,
    Numb      = 2,
    Supported = 5,
    Special   = 6,
    Profile   = 7,
    // graphics
    X         = 8,
    Y         = 9,
    #[strum(serialize = "color", serialize = "colour")]
    Color     = 10,
    Buffer    = 11,
    GSpecial  = 15,
    // text
    Ascii8    = 16,
    Char5     = 17,
    Char6     = 18,
    Ascii7    = 19,
    Utf8      = 20,
    TSpecial  = 23,
    // numbers
    Int       = 24,
    Uint      = 25,
    Bin       = 26,
    Hex       = 27,
    Float     = 28,
    Fixed     = 29,
    NSpecial  = 31,
    // storage
    Addr      = 32,
    Bus       = 33,
    Page      = 34,
    SSpecial  = 39,
    // miscellaneous
    Rng       = 40,
    Note      = 41,
    Instr     = 42,
    NLeg      = 43,
    Wait      = 44,
    NAddr     = 45,
    Data      = 46,
    MSpecial  = 47,
    // user defined
    Ud1       = 48,
    Ud2       = 49,
    Ud3       = 50,
    Ud4       = 51,
    Ud5       = 52,
    Ud6       = 53,
    Ud7       = 54,
    Ud8       = 55,
    Ud9       = 56,
    Ud10      = 57,
    Ud11      = 58,
    Ud12      = 59,
    Ud13      = 60,
    Ud14      = 61,
    Ud15      = 62,
    Ud16      = 63,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let j = format!("{self:?}");
        let i = InstructionDiscriminants::from(self).to_string();
        let k = j.get((i.len() + 1)..(j.len() - 1)).unwrap_or("");
        write!(f, "{i} {}", k.replace(',', ""))
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "bits {}", self.bits)?;
        writeln!(f, "minreg {}", self.registers)?;
        writeln!(f, "minstack {}", self.stack_size)?;
        writeln!(f, "minheap {}", self.heap_size)?;
        writeln!(f, "\n// inst")?;

        for i in self.instructions.iter() {
            writeln!(f, "{i}")?;
        }

        writeln!(f, "\n// dw")?;
        for w in self.dw.iter() {
            writeln!(f, "dw {}", *w as i128)?;
        }

        Ok(())
    }
}
