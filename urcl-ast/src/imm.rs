use core::fmt;
use std::borrow::Cow;
use dashu::Integer;

#[derive(Clone)]
pub enum Immediate {
    Value(Integer),
    InstLoc(usize),
}

impl fmt::Debug for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Value(v) => write!(f, "{v}"),
            Self::InstLoc(l) => write!(f, "/* inst loc: */ {l}"),
        }
    }
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Value(v) => v.fmt(f),
            Self::InstLoc(l) => l.fmt(f),
        }
    }
}

impl<'a> From<&'a Immediate> for Cow<'a, Integer> {
    fn from(v: &'a Immediate) -> Self {
        match v {
            Immediate::Value(v) => Cow::Borrowed(v),
            Immediate::InstLoc(l) => Cow::Owned(Integer::from(*l)),
        }
    }
}

impl<'a> TryFrom<&'a Immediate> for usize {
    type Error = <usize as TryFrom<&'a Integer>>::Error;

    fn try_from(value: &'a Immediate) -> Result<Self, Self::Error> {
        match value {
            Immediate::Value(v) => v.try_into(),
            Immediate::InstLoc(l) => Ok(*l),
        }
    }
}

impl<'a> TryFrom<&'a Immediate> for u64 {
    type Error = <u64 as TryFrom<&'a Integer>>::Error;

    fn try_from(value: &'a Immediate) -> Result<Self, Self::Error> {
        match value {
            Immediate::Value(v) => v.try_into(),
            Immediate::InstLoc(l) => Ok(*l as _),
        }
    }
}
