use dashu::Integer;
use logos::Span;
use urcl_ast::{Any, Immediate, OperandKind};

use super::{error::ParseError, Parser};

pub(crate) type RawOp<'a> = (super::operand::RawOperand<'a>, Span);

#[derive(Debug, Clone)]
pub(crate) enum MacroExpr<'a> {
    Add(RawOp<'a>, RawOp<'a>),
    Nor(RawOp<'a>, RawOp<'a>),
}

macro_rules! eval {
    ($self:tt $hs:tt $($o:tt),* => $a:expr) => {{
        $(
            let $o = $self.finalize_to_imm($o, $hs);
        )*
        Immediate::Value($a($($o),*))
    }};
}

impl<'a> Parser<'a> {
    fn finalize_to_imm(&mut self, op: &RawOp<'a>, heap_size: u64) -> Integer {
        match self.finalize(op, heap_size) {
            Any::Immediate(i) => i.into(),
            _ => {
                self.errors.push((ParseError::InvalidOperand(&OperandKind::Immediate), op.1.clone()));
                Integer::ONE
            },
        }
    }

    pub(crate) fn eval_macro_expr(&mut self, expr: &MacroExpr<'a>, heap_size: u64) -> Immediate {
        match expr {
            MacroExpr::Add(a, b) => eval!(self heap_size a, b => |a, b| {
                a + b
            }),
            MacroExpr::Nor(a, b) => eval!(self heap_size a, b => |a, b| {
                self.bits_umax() - (a | b)
            }),
        }
    }
}
