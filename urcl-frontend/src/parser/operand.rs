use core::str::FromStr;

use dashu::{Integer, Natural};
use logos::Span;
use num_traits::ToPrimitive;
use urcl_ast::{Any, Immediate, OperandKind, Port, Register};

use crate::{lexer::{LexResult, Token}, parser::util::expect_some_token};

use super::{macro_expr::MacroExpr, ParseError, Parser};

#[derive(Debug, Clone)]
pub(crate) enum RawOperand<'a> {
    Register(Register),
    Immediate(Immediate),
    Heap(Integer),
    Label(&'a str),
    MacroExpr(Box<MacroExpr<'a>>),
}

impl<'a> Parser<'a> {
    pub(crate) fn parse_operand(
        &mut self,
        ok: &'static OperandKind,
    ) -> Result<(RawOperand<'a>, Span), (ParseError, Span)> {
        let t = self.next_token();
        self.parse_operand_with_option(t, ok)
    }

    pub(crate) fn parse_operand_with_option(
        &mut self,
        t: Option<LexResult<'a>>,
        ok: &'static OperandKind,
    ) -> Result<(RawOperand<'a>, Span), (ParseError, Span)> {
        match t {
            Some(Ok(t)) => self.parse_operand_with_token(t, ok),
            Some(Err(e)) => Err((ParseError::LexError(e), self.span())),
            None => Err((ParseError::UnexpectedEof, self.span())),
        }
    }

    pub(crate) fn parse_operand_with_token(
        &mut self,
        t: Token<'a>,
        ok: &'static OperandKind,
    ) -> Result<(RawOperand<'a>, Span), (ParseError, Span)> {
        if let Some(def) = self.defines.get(&t) {
            return Ok(def.clone());
        }

        match (t, ok) {
            (Token::Reg(r), OperandKind::Register | OperandKind::Any) => {
                Ok((RawOperand::Register(r), self.span()))
            },
            (Token::Integer(i), OperandKind::Immediate | OperandKind::Any) => {
                Ok((RawOperand::Immediate(Immediate::Value(i)), self.span()))
            },
            (Token::Heap(h), OperandKind::Immediate | OperandKind::Any) => {
                Ok((RawOperand::Heap(h), self.span()))
            },
            (Token::Macro(m), OperandKind::Immediate | OperandKind::Any) => {
                self.parse_macro_expr(m)
                    .ok_or((ParseError::UnknownMacro, self.span()))
                    .map(|(m, s)| (m.into(), s))
            },
            (Token::ParenStart, OperandKind::Immediate | OperandKind::Any) => {
                let inner = self.parse_operand(&OperandKind::Immediate);

                if inner.is_ok() {
                    expect_some_token!(self self.peek_next(), Token::ParenEnd => {
                        self.next_token();
                    }, {});
                }

                inner
            },
            (Token::Label(l), OperandKind::Immediate | OperandKind::Any) => {
                Ok((RawOperand::Label(l), self.span()))
            },
            (Token::Relative(r), OperandKind::Immediate | OperandKind::Any) => Ok((
                RawOperand::Immediate(Immediate::InstLoc(
                    (Integer::from(self.instructions.len()) + r)
                        .to_usize()
                        .ok_or((ParseError::InvalidRelative, self.span()))?,
                )),
                self.span(),
            )),
            (Token::Port(p), OperandKind::Immediate | OperandKind::Any) => {
                let p = Port::from_str(p).map_err(|_| (ParseError::UnknownPort, self.span()))?;

                Ok((
                    RawOperand::Immediate(Immediate::Value((p as usize).into())),
                    self.span(),
                ))
            },
            (Token::PortInt(p), OperandKind::Immediate | OperandKind::Any) => Ok((
                RawOperand::Immediate(Immediate::Value(p.into())),
                self.span(),
            )),
            _ => Err((ParseError::InvalidOperand(ok), self.span())),
        }
    }

    pub(crate) fn finalize(&self, op: &(RawOperand<'a>, Span), heap_size: u64) -> Any {
        match &op.0 {
            RawOperand::Register(r) => Any::Register(r.clone()),
            RawOperand::Immediate(i) => Any::Immediate(i.clone()),
            RawOperand::Heap(h) => Any::Immediate(Immediate::Value(h + self.dw.len())),
            RawOperand::Label(l) => Any::Immediate(self.labels.get(l).map_or_else(
                || {
                    self.error_at(ParseError::UnknownLabel, op.1.clone());
                    Immediate::Value(Integer::ZERO)
                },
                |v| v.clone(),
            )),
            RawOperand::MacroExpr(mx) => {
                Any::Immediate(self.eval_macro_expr(mx, heap_size))
            },
        }
    }

    pub(crate) fn bits_vals(&self) -> Natural { Natural::ONE << self.bits() as usize }

    pub(crate) fn bits_umax(&self) -> Natural { self.bits_vals() - Natural::ONE }

    pub(crate) fn bits_umsb(&self) -> Natural {
        Natural::ONE << self.bits().saturating_sub(1) as usize
    }

    pub(crate) fn bits_smax(&self) -> Natural {
        (Natural::ONE << self.bits().saturating_sub(1) as usize) - Natural::ONE
    }

    pub(crate) fn bits_smsb(&self) -> Natural {
        Natural::ONE << self.bits().saturating_sub(2) as usize
    }
}
