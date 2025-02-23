use core::fmt;
use crate::{Immediate, Register};

#[derive(Clone, strum::EnumTryAs)]
pub enum Any {
    Register(Register),
    Immediate(Immediate),
}

impl fmt::Debug for Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(r) => r.fmt(f),
            Self::Immediate(r) => r.fmt(f),
        }
    }
}
