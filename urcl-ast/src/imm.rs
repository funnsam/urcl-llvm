use core::fmt;
use dashu::{Decimal, Integer};
use std::borrow::Cow;

#[derive(Clone)]
pub enum IntImm {
    Value(Integer),
    InstLoc(usize),
}

impl fmt::Debug for IntImm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Value(v) => write!(f, "{v}"),
            Self::InstLoc(l) => write!(f, "/* inst loc: */ {l}"),
        }
    }
}

impl fmt::Display for IntImm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Value(v) => v.fmt(f),
            Self::InstLoc(l) => l.fmt(f),
        }
    }
}

impl<'a> From<&'a IntImm> for Cow<'a, Integer> {
    fn from(v: &'a IntImm) -> Self {
        match v {
            IntImm::Value(v) => Cow::Borrowed(v),
            IntImm::InstLoc(l) => Cow::Owned(Integer::from(*l)),
        }
    }
}

impl<'a> TryFrom<&'a IntImm> for usize {
    type Error = <usize as TryFrom<&'a Integer>>::Error;

    fn try_from(value: &'a IntImm) -> Result<Self, Self::Error> {
        match value {
            IntImm::Value(v) => v.try_into(),
            IntImm::InstLoc(l) => Ok(*l),
        }
    }
}

impl<'a> TryFrom<&'a IntImm> for u64 {
    type Error = <u64 as TryFrom<&'a Integer>>::Error;

    fn try_from(value: &'a IntImm) -> Result<Self, Self::Error> {
        match value {
            IntImm::Value(v) => v.try_into(),
            IntImm::InstLoc(l) => Ok(*l as _),
        }
    }
}

impl From<IntImm> for Integer {
    fn from(v: IntImm) -> Self {
        match v {
            IntImm::Value(v) => v,
            IntImm::InstLoc(l) => l.into(),
        }
    }
}

#[derive(Clone)]
pub struct FloatImm(pub Decimal);

impl fmt::Debug for FloatImm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for FloatImm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<Decimal> for FloatImm {
    fn from(value: Decimal) -> Self {
        Self(value)
    }
}

impl From<FloatImm> for Decimal {
    fn from(value: FloatImm) -> Self {
        value.0
    }
}
