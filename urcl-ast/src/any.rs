use crate::{AnyImm, FloatImm, IntImm, Register};
use core::fmt;

#[derive(Clone, strum::EnumTryAs)]
pub enum Any {
    Register(Register),
    IntImm(IntImm),
    FloatImm(FloatImm),
    Undefined,
}

impl Any {
    pub fn try_as_any_int(self) -> Option<AnyInt> {
        match self {
            Self::Register(r) => Some(AnyInt::Register(r)),
            Self::IntImm(i) => Some(AnyInt::IntImm(i)),
            _ => None,
        }
    }

    pub fn try_as_any_float(self) -> Option<AnyFloat> {
        match self {
            Self::Register(r) => Some(AnyFloat::Register(r)),
            Self::FloatImm(i) => Some(AnyFloat::FloatImm(i)),
            _ => None,
        }
    }

    pub fn try_as_any_imm(self) -> Option<AnyImm> {
        match self {
            Self::IntImm(i) => Some(AnyImm::IntImm(i)),
            Self::FloatImm(i) => Some(AnyImm::FloatImm(i)),
            Self::Undefined => Some(AnyImm::Undefined),
            _ => None,
        }
    }
}

impl fmt::Debug for Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(r) => r.fmt(f),
            Self::IntImm(r) => r.fmt(f),
            Self::FloatImm(r) => r.fmt(f),
            Self::Undefined => "undefined".fmt(f),
        }
    }
}

#[derive(Clone, strum::EnumTryAs)]
pub enum AnyInt {
    Register(Register),
    IntImm(IntImm),
}

impl From<AnyInt> for Any {
    fn from(value: AnyInt) -> Self {
        match value {
            AnyInt::Register(r) => Self::Register(r),
            AnyInt::IntImm(i) => Self::IntImm(i),
        }
    }
}

impl fmt::Debug for AnyInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(r) => r.fmt(f),
            Self::IntImm(r) => r.fmt(f),
        }
    }
}

#[derive(Clone, strum::EnumTryAs)]
pub enum AnyFloat {
    Register(Register),
    FloatImm(FloatImm),
}

impl From<AnyFloat> for Any {
    fn from(value: AnyFloat) -> Self {
        match value {
            AnyFloat::Register(r) => Self::Register(r),
            AnyFloat::FloatImm(i) => Self::FloatImm(i),
        }
    }
}

impl fmt::Debug for AnyFloat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(r) => r.fmt(f),
            Self::FloatImm(r) => r.fmt(f),
        }
    }
}
