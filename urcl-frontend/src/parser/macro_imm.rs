use dashu::Integer;

use super::Parser;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::EnumString)]
#[strum(ascii_case_insensitive)]
pub(crate) enum MacroImm {
    Bits,
    MinReg,
    MinHeap,
    MinStack,
    Heap,
    Msb,
    SMsb,
    Max,
    SMax,
    UHalf,
    LHalf,
}

impl<'a> Parser<'a> {
    pub(crate) fn eval_macro_imm(&self, mi: MacroImm, heap_size: u64) -> Integer {
        match mi {
            MacroImm::Bits => self.bits().into(),
            MacroImm::MinReg => self.registers().into(),
            MacroImm::MinStack => self.min_stack().into(),
            MacroImm::MinHeap => self.min_heap().into(),
            MacroImm::Heap => heap_size.into(),
            MacroImm::Max => self.bits_umax().into(),
            MacroImm::SMax => self.bits_smax().into(),
            MacroImm::Msb => self.bits_umsb().into(),
            MacroImm::SMsb => self.bits_smsb().into(),
            MacroImm::UHalf | MacroImm::LHalf => todo!(),
        }
    }
}
