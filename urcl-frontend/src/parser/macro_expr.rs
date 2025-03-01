use dashu::{base::Sign, Integer, Natural};
use logos::Span;
use num_traits::ToPrimitive;
use urcl_ast::{Any, Immediate, OperandKind};

use super::{error::ParseError, Parser};

pub(crate) type RawOp<'a> = (super::operand::RawOperand<'a>, Span);

#[derive(Debug, Clone)]
pub(crate) enum MacroExpr<'a> {
    Add(RawOp<'a>, RawOp<'a>),
    Sub(RawOp<'a>, RawOp<'a>),
    Mlt(RawOp<'a>, RawOp<'a>),
    Umlt(RawOp<'a>, RawOp<'a>),
    SUmlt(RawOp<'a>, RawOp<'a>),
    Div(RawOp<'a>, RawOp<'a>),
    Sdiv(RawOp<'a>, RawOp<'a>),
    Mod(RawOp<'a>, RawOp<'a>),
    Abs(RawOp<'a>),
    Bsl(RawOp<'a>, RawOp<'a>),
    Bsr(RawOp<'a>, RawOp<'a>),
    Bss(RawOp<'a>, RawOp<'a>),

    Or(RawOp<'a>, RawOp<'a>),
    Nor(RawOp<'a>, RawOp<'a>),
    And(RawOp<'a>, RawOp<'a>),
    Nand(RawOp<'a>, RawOp<'a>),
    Xor(RawOp<'a>, RawOp<'a>),
    Xnor(RawOp<'a>, RawOp<'a>),
    Not(RawOp<'a>),

    /*
    SetE(RawOp<'a>, RawOp<'a>),
    SetNe(RawOp<'a>, RawOp<'a>),
    SetG(RawOp<'a>, RawOp<'a>),
    SetL(RawOp<'a>, RawOp<'a>),
    SetGe(RawOp<'a>, RawOp<'a>),
    SetLe(RawOp<'a>, RawOp<'a>),
    SetC(RawOp<'a>, RawOp<'a>),
    SetNc(RawOp<'a>, RawOp<'a>),
    SSetG(RawOp<'a>, RawOp<'a>),
    SSetL(RawOp<'a>, RawOp<'a>),
    SSetGe(RawOp<'a>, RawOp<'a>),
    SSetLe(RawOp<'a>, RawOp<'a>),
    */
}

macro_rules! eval {
    ($self:tt $hs:tt $($o:tt),* => $a:expr) => {{
        $(
            let $o = $self.finalize_to_nat($o, $hs);
        )*
        Immediate::Value(Integer::from($a % $self.bits_vals()))
    }};
}

impl<'a> Parser<'a> {
    fn finalize_to_nat(&mut self, op: &RawOp<'a>, heap_size: u64) -> Natural {
        match self.finalize(op, heap_size) {
            Any::Immediate(i) => {
                let int = Integer::from(i);

                if int.sign() == Sign::Positive {
                    int.into_parts().1
                } else {
                    self.bits_umax() - int.into_parts().1
                }
            },
            _ => {
                self.errors.push((ParseError::InvalidOperand(&OperandKind::Immediate), op.1.clone()));
                Natural::ONE
            },
        }
    }

    fn sign(&self, a: Natural) -> Integer {
        let a = a % self.bits_vals();

        if a > self.bits_smax() {
            Integer::from_parts(Sign::Negative, self.bits_vals() - a)
        } else {
            Integer::from_parts(Sign::Positive, a)
        }
    }

    fn unsign(&self, a: Integer) -> Natural {
        if a.sign() == Sign::Positive {
            a.into_parts().1
        } else {
            self.bits_vals() - a.into_parts().1 % self.bits_vals()
        }
    }

    pub(crate) fn eval_macro_expr(&mut self, expr: &MacroExpr<'a>, h: u64) -> Immediate {
        use MacroExpr as M;


        match expr {
            M::Add(a, b) => eval!(self h a, b => a + b),
            M::Sub(a, b) => eval!(self h a, b => self.bits_vals() + a - b),
            M::Mlt(a, b) => eval!(self h a, b => a * b),
            M::Umlt(a, b) => eval!(self h a, b => (a * b) / self.bits_vals()),
            M::SUmlt(a, b) => eval!(self h a, b => self.unsign(self.sign(a) * self.sign(b) >> self.bits() as usize)),
            M::Div(a, b) => eval!(self h a, b => a / b),
            M::Sdiv(a, b) => eval!(self h a, b => self.unsign(self.sign(a) / self.sign(b))),
            M::Mod(a, b) => eval!(self h a, b => a % b),
            M::Abs(a) => eval!(self h a => self.sign(a).into_parts().1),
            M::Bsl(a, b) => eval!(self h a, b => a << b.to_usize().unwrap()),
            M::Bsr(a, b) => eval!(self h a, b => a >> b.to_usize().unwrap()),
            M::Bss(a, b) => eval!(self h a, b => self.unsign(self.sign(a) >> b.to_usize().unwrap())),

            M::Or(a, b) => eval!(self h a, b => a | b),
            M::Nor(a, b) => eval!(self h a, b => self.bits_umax() - (a | b)),
            M::And(a, b) => eval!(self h a, b => a & b),
            M::Nand(a, b) => eval!(self h a, b => self.bits_umax() - (a & b)),
            M::Xor(a, b) => eval!(self h a, b => a ^ b),
            M::Xnor(a, b) => eval!(self h a, b => self.bits_umax() - (a ^ b)),
            M::Not(a) => eval!(self h a => self.bits_umax() - a),

            /*
            M::SetE(a, b) => eval!(self h a, b => todo!()),
            M::SetNe(a, b) => eval!(self h a, b => todo!()),
            M::SetG(a, b) => eval!(self h a, b => todo!()),
            M::SetL(a, b) => eval!(self h a, b => todo!()),
            M::SetGe(a, b) => eval!(self h a, b => todo!()),
            M::SetLe(a, b) => eval!(self h a, b => todo!()),
            M::SetC(a, b) => eval!(self h a, b => todo!()),
            M::SetNc(a, b) => eval!(self h a, b => todo!()),
            M::SSetG(a, b) => eval!(self h a, b => todo!()),
            M::SSetL(a, b) => eval!(self h a, b => todo!()),
            M::SSetGe(a, b) => eval!(self h a, b => todo!()),
            M::SSetLe(a, b) => eval!(self h a, b => todo!()),
            */
        }
    }
}
