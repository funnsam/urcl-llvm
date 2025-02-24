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
    (i) => { Immediate };
    (a) => { Any };

    (op kind r) => { OperandKind::Register };
    (op kind i) => { OperandKind::Immediate };
    (op kind a) => { OperandKind::Any };

    (from any r $e: expr) => {
        $e.try_as_register().unwrap()
    };
    (from any i $e: expr) => {
        $e.try_as_immediate().unwrap()
    };
    (from any a $e: expr) => {
        $e
    };

    (gen) => { None };
    (gen $($i: tt)+) => { Some(&[$($i),+]) };
}

inst! {
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
    SBrg [a a a],
    SBle [a a a],
    SBge [a a a],
    SSetG [r a a],
    SSetL [r a a],
    SSetGe [r a a],
    SSetLe [r a a],
    Abs [r a],
    Out [a a],
    In [r a] [1 0],
    Umlt [r a a],
    SUmlt [r a a],

    ItoF [r a],
    FtoI [r a],
    FRtoI [r a],
    FAdd [r a a],
    FSub [r a a],
    FMlt [r a a],
    FDiv [r a a],
    FSqrt [r a],
    FAbs [r a],
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

#[derive(Debug, Clone)]
pub enum OperandKind {
    Register,
    Immediate,
    Any,
}

#[derive(Debug, Clone)]
pub struct InstProperties {
    pub operands: &'static [OperandKind],
    pub port_v2: Option<&'static [usize]>,
}
