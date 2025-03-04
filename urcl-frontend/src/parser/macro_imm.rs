use dashu::{Integer, Natural};

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
            MacroImm::Max => self.bits_umax(),
            MacroImm::SMax => self.bits_smax(),
            MacroImm::Msb => self.bits_umsb(),
            MacroImm::SMsb => self.bits_smsb(),
            MacroImm::UHalf => self.bits_uhalf(),
            MacroImm::LHalf => self.bits_lhalf(),
        }
        .into()
    }

    pub(crate) fn bits_vals(&self) -> Natural { bits_vals(self.bits() as _) }

    pub(crate) fn bits_umax(&self) -> Natural { bits_max(self.bits() as _) }

    pub(crate) fn bits_umsb(&self) -> Natural { bits_vals(self.bits().saturating_sub(1) as _) }

    pub(crate) fn bits_smax(&self) -> Natural { bits_max(self.bits().saturating_sub(1) as _) }

    pub(crate) fn bits_smsb(&self) -> Natural { bits_vals(self.bits().saturating_sub(2) as _) }

    pub(crate) fn bits_uhalf(&self) -> Natural { self.bits_umax() ^ self.bits_lhalf() }

    pub(crate) fn bits_lhalf(&self) -> Natural { bits_max((self.bits() as usize + 1) / 2) }
}

fn bits_vals(bits: usize) -> Natural { Natural::ONE << bits }

fn bits_max(bits: usize) -> Natural { bits_vals(bits) - 1_usize }
