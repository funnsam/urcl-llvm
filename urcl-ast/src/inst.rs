use crate::*;

macro_rules! inst {
    ($($name: ident [$($arg: tt)*] $([$($ai: tt)*])?),* $(,)?) => {
        #[derive(Debug, Clone, strum::EnumDiscriminants)]
        #[strum_discriminants(derive(strum::Display))]
        #[strum_discriminants(strum(serialize_all = "lowercase"))]
        pub enum Instruction {
            $($name($(inst!($arg)),*)),*
        }

        impl Instruction {
            pub fn properties(i: &str) -> Option<InstProperties> {
                $(if i.eq_ignore_ascii_case(stringify!($name)) {
                    return Some(InstProperties {
                        operands: &[$(inst!(op kind $arg)),*],
                        port_v2: inst!(gen $($($ai)*)?),
                    });
                })*

                None
            }

            pub fn construct(i: &str, mut o: Vec<Any>) -> Self {
                use Instruction::*;
                o.reverse();

                $(if i.eq_ignore_ascii_case(stringify!($name)) {
                    return $name($(inst!(from any $arg o.pop().unwrap())),*);
                })*

                panic!();
            }
        }
    };

    (r) => { Register };
    (i) => { AnyInt };
    (f) => { AnyFloat };
    (a) => { Any };

    (op kind r) => { OperandKind::Register };
    (op kind i) => { OperandKind::AnyInt };
    (op kind f) => { OperandKind::AnyFloat };
    (op kind a) => { OperandKind::Any };

    (from any r $e: expr) => {
        $e.try_as_register().unwrap()
    };
    (from any i $e: expr) => {
        $e.try_as_any_int().unwrap()
    };
    (from any f $e: expr) => {
        $e.try_as_any_float().unwrap()
    };
    (from any a $e: expr) => {
        $e
    };

    (gen) => { None };
    (gen $($i: tt)+) => { Some(&[$($i),+]) };
}

inst! {
    Add [r i i],
    Rsh [r i],
    Lod [r i],
    Str [i a],
    Bge [i i i],
    Nor [r i i],
    Sub [r i i],
    Jmp [i],
    Mov [r a],
    Nop [],
    Imm [r a],
    Lsh [r i],
    Inc [r i],
    Dec [r i],
    Neg [r i],
    And [r i i],
    Or [r i i],
    Not [r i],
    Xnor [r i i],
    Xor [r i i],
    Nand [r i i],
    Brl [i i i],
    Brg [i i i],
    Bre [i i i],
    Bne [i i i],
    Bod [i i],
    Bev [i i],
    Ble [i i i],
    Brz [i i],
    Bnz [i i],
    Brn [i i],
    Brp [i i],
    Psh [a],
    Pop [r],
    Cal [i],
    Ret [],
    Hlt [],
    Cpy [i i],
    Brc [i i i],
    Bnc [i i i],
    Mlt [r i i],
    Div [r i i],
    Mod [r i i],
    Bsr [r i i],
    Bsl [r i i],
    Srs [r i],
    Bss [r i i],
    SetE [r i i],
    SetNe [r i i],
    SetG [r i i],
    SetL [r i i],
    SetGe [r i i],
    SetLe [r i i],
    SetC [r i i],
    SetNc [r i i],
    LLod [r i i],
    LStr [i i a],
    SDiv [r i i],
    SBrl [i i i],
    SBrg [i i i],
    SBle [i i i],
    SBge [i i i],
    SSetG [r i i],
    SSetL [r i i],
    SSetGe [r i i],
    SSetLe [r i i],
    Abs [r i],
    Umlt [r i i],
    SUmlt [r i i],
    Out [a a],
    In [r a] [1 0],

    ItoF [r i],
    FtoI [r f],
    FRtoI [r f],
    FAdd [r f f],
    FSub [r f f],
    FMlt [r f f],
    FDiv [r f f],
    FSqrt [r f],
    FAbs [r f],
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let j = format!("{self:?}");
        let i = InstructionDiscriminants::from(self).to_string();
        let k = j.get((i.len() + 1)..(j.len() - 1)).unwrap_or("");
        write!(f, "{i} {}", k.replace(',', ""))
    }
}

impl Instruction {
    pub(crate) fn fmt_san(&self, f: &mut fmt::Formatter, bits: u32) -> fmt::Result {
        let j = format!("{self:?}");
        let i = InstructionDiscriminants::from(self).to_string();
        let k = j.get((i.len() + 1)..(j.len() - 1)).unwrap_or("");
        let v = k
            .split(", ")
            .map(|i| match i.chars().nth(0) {
                Some(c) if c.is_ascii_digit() => {
                    let v = i.parse::<Natural>().unwrap();
                    (((Natural::ONE << bits as usize) - 1_u8) & v).to_string()
                },
                _ => i.to_string(),
            })
            .collect::<Vec<_>>()
            .join(" ");
        write!(f, "{i} {v}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperandKind {
    Register,
    Any,
    AnyInt,
    AnyFloat,
    AnyImm,
}

#[derive(Debug, Clone)]
pub struct InstProperties {
    pub operands: &'static [OperandKind],
    pub port_v2: Option<&'static [usize]>,
}

impl OperandKind {
    pub fn can_int(self) -> bool { matches!(self, Self::AnyInt | Self::Any) }
    pub fn can_float(self) -> bool { matches!(self, Self::AnyFloat | Self::Any) }
    pub fn can_reg(self) -> bool { self != Self::AnyImm }
}
