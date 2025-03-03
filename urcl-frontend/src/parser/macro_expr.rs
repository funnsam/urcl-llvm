use core::str::FromStr;

use dashu::{Integer, Natural, base::Sign};
use logos::Span;
use num_traits::ToPrimitive;
use urcl_ast::{Any, IntImm, OperandKind};

use super::{Parser, error::ParseError, macro_imm::MacroImm, operand::RawOperand};

pub(crate) type RawOp<'a> = (super::operand::RawOperand<'a>, Span);

#[derive(Debug, Clone)]
pub(crate) enum MacroExpr<'a> {
    MacroImm(MacroImm),

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
}

impl<'a> From<MacroExpr<'a>> for RawOperand<'a> {
    fn from(value: MacroExpr<'a>) -> Self { Self::MacroExpr(Box::new(value)) }
}

impl<'a> Parser<'a> {
    fn finalize_to_nat(&self, op: &RawOp<'a>, heap_size: u64) -> Natural {
        match self.finalize(op, heap_size) {
            Any::IntImm(i) => {
                let int = Integer::from(i);

                if int.sign() == Sign::Positive {
                    int.into_parts().1
                } else {
                    self.bits_vals() - int.into_parts().1 & self.bits_umax()
                }
            },
            _ => {
                self.error_at(
                    ParseError::InvalidOperand(OperandKind::AnyInt),
                    op.1.clone(),
                );
                Natural::ONE
            },
        }
    }

    fn sign(&self, a: Natural) -> Integer {
        let a = a & self.bits_umax();

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
            self.bits_vals() - a.into_parts().1 & self.bits_umax()
        }
    }

    fn set(&self, b: bool) -> Natural { if b { self.bits_umax() } else { Natural::ZERO } }

    pub(crate) fn eval_macro_expr(&self, expr: &MacroExpr<'a>, heap_size: u64) -> IntImm {
        use MacroExpr as M;

        macro_rules! eval {
            ($($o:tt),* => $a:expr) => {{
                $(
                    let $o = self.finalize_to_nat($o, heap_size) & self.bits_umax();
                )*
                IntImm::Value(Integer::from($a & self.bits_umax()))
            }};
        }

        match expr {
            M::MacroImm(mi) => IntImm::Value(self.eval_macro_imm(*mi, heap_size)),

            M::Add(a, b) => eval!(a, b => a + b),
            M::Sub(a, b) => eval!(a, b => self.bits_vals() + a - b),
            M::Mlt(a, b) => eval!(a, b => a * b),
            M::Umlt(a, b) => eval!(a, b => (a * b) >> self.bits() as usize),
            M::SUmlt(a, b) => {
                eval!(a, b => self.unsign(self.sign(a) * self.sign(b) >> self.bits() as usize))
            },
            M::Div(a, b) => eval!(a, b => a / b),
            M::Sdiv(a, b) => eval!(a, b => self.unsign(self.sign(a) / self.sign(b))),
            M::Mod(a, b) => eval!(a, b => a % b),
            M::Abs(a) => eval!(a => self.sign(a).into_parts().1),
            M::Bsl(a, b) => eval!(a, b => a << b.to_usize().unwrap()),
            M::Bsr(a, b) => eval!(a, b => a >> b.to_usize().unwrap()),
            M::Bss(a, b) => eval!(a, b => self.unsign(self.sign(a) >> b.to_usize().unwrap())),

            M::Or(a, b) => eval!(a, b => a | b),
            M::Nor(a, b) => eval!(a, b => self.bits_umax() - (a | b)),
            M::And(a, b) => eval!(a, b => a & b),
            M::Nand(a, b) => eval!(a, b => self.bits_umax() - (a & b)),
            M::Xor(a, b) => eval!(a, b => a ^ b),
            M::Xnor(a, b) => eval!(a, b => self.bits_umax() - (a ^ b)),
            M::Not(a) => eval!(a => self.bits_umax() - a),

            M::SetE(a, b) => eval!(a, b => self.set(a == b)),
            M::SetNe(a, b) => eval!(a, b => self.set(a != b)),
            M::SetG(a, b) => eval!(a, b => self.set(a > b)),
            M::SetL(a, b) => eval!(a, b => self.set(a < b)),
            M::SetGe(a, b) => eval!(a, b => self.set(a >= b)),
            M::SetLe(a, b) => eval!(a, b => self.set(a <= b)),
            M::SetC(a, b) => eval!(a, b => self.set(a + b > self.bits_umax())),
            M::SetNc(a, b) => eval!(a, b => self.set(a + b <= self.bits_umax())),
            M::SSetG(a, b) => eval!(a, b => self.set(self.sign(a) > self.sign(b))),
            M::SSetL(a, b) => eval!(a, b => self.set(self.sign(a) < self.sign(b))),
            M::SSetGe(a, b) => eval!(a, b => self.set(self.sign(a) >= self.sign(b))),
            M::SSetLe(a, b) => eval!(a, b => self.set(self.sign(a) <= self.sign(b))),
        }
    }

    pub(crate) fn parse_macro_expr(&mut self, name: &str) -> Option<(MacroExpr<'a>, Span)> {
        macro_rules! first {
            ($first:tt $($_:tt)*) => {
                $first
            };
        }

        macro_rules! expr {
            ($expr:tt $($op:tt),*) => {{
                $(
                    let $op = self.parse_operand(OperandKind::AnyInt)
                    .map_or_else(
                        |e| {
                            let span = e.1.clone();
                            self.error_at(e.0, e.1);
                            (RawOperand::IntImm(IntImm::InstLoc(0)), span)
                        },
                        |o| o,
                    );
                )*
                let start = first!($($op)*).1.start;
                Some((MacroExpr::$expr($($op),*), start..self.span().end))
            }};
        }

        if let Ok(mi) = MacroImm::from_str(name) {
            return Some((MacroExpr::MacroImm(mi), self.span()));
        }

        match name.to_ascii_lowercase().as_str() {
            "add" | "+" => expr!(Add a, b),
            "sub" | "-" => expr!(Sub a, b),
            "mlt" | "*" => expr!(Mlt a, b),
            "umlt" | "^*" => expr!(Umlt a, b),
            "sumlt" | "~^*" => expr!(SUmlt a, b),
            "div" | "/" => expr!(Div a, b),
            "sdiv" | "~/" => expr!(Sdiv a, b),
            "mod" | "%" => expr!(Mod a, b),
            "abs" | "+|" => expr!(Abs a),
            "bsl" | "<<" => expr!(Bsl a, b),
            "bsr" | ">>" => expr!(Bsr a, b),
            "bss" | "~>>" => expr!(Bss a, b),
            "or" | "|" => expr!(Or a, b),
            "nor" | "!|" => expr!(Nor a, b),
            "and" | "&" => expr!(And a, b),
            "nand" | "!&" => expr!(Nand a, b),
            "xor" | "^" => expr!(Xor a, b),
            "xnor" | "!^" => expr!(Xnor a, b),
            "not" | "!" => expr!(Not a),
            "sete" | "==" => expr!(SetE a, b),
            "setne" | "!=" => expr!(SetNe a, b),
            "setg" | ">" => expr!(SetG a, b),
            "setl" | "<" => expr!(SetL a, b),
            "setge" | ">=" => expr!(SetGe a, b),
            "setle" | "<=" => expr!(SetLe a, b),
            "setc" | "^+" => expr!(SetC a, b),
            "setnc" | "!^+" => expr!(SetNc a, b),
            "ssetg" | "~>" => expr!(SSetG a, b),
            "ssetl" | "~<" => expr!(SSetL a, b),
            "ssetge" | "~>=" => expr!(SSetGe a, b),
            "ssetle" | "~<=" => expr!(SSetLe a, b),
            _ => None,
        }
    }
}
