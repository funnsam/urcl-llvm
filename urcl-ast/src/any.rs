use crate::{FloatImm, IntImm, Register};
use core::fmt;

#[derive(Clone, strum::EnumTryAs)]
pub enum Any {
    Register(Register),
    IntImm(IntImm),
    FloatImm(FloatImm),
}

impl fmt::Debug for Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(r) => r.fmt(f),
            Self::IntImm(r) => r.fmt(f),
            Self::FloatImm(r) => r.fmt(f),
        }
    }
}

#[derive(Clone, strum::EnumTryAs)]
pub enum AnyImm {
    Int(IntImm),
    Float(FloatImm),
}

impl fmt::Debug for AnyImm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(r) => r.fmt(f),
            Self::Float(r) => r.fmt(f),
        }
    }
}
