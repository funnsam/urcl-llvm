use core::str::FromStr;

use dashu::Integer;
use logos::Span;
use num_traits::ToPrimitive;
use urcl_ast::{Immediate, OperandKind, Port, Register};

use crate::lexer::{LexResult, Token};

use super::{MacroImm, ParseError, Parser};

#[derive(Debug, Clone)]
pub(crate) enum RawOperand<'a> {
    Register(Register),
    Immediate(Immediate),
    Heap(Integer),
    MacroImm(MacroImm),
    Label(&'a str),
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
            Some(Ok(t)) => {
                if let Some(def) = self.defines.get(&t) {
                    Ok(def.clone())
                } else {
                    self.parse_operand_with_token(t, ok)
                }
            },
            Some(Err(e)) => Err((ParseError::LexError(e), self.span())),
            None => Err((ParseError::UnexpectedEof, self.span())),
        }
    }

    pub(crate) fn parse_operand_with_token(
        &mut self,
        t: Token<'a>,
        ok: &'static OperandKind,
    ) -> Result<(RawOperand<'a>, Span), (ParseError, Span)> {
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
            (Token::Macro(m), OperandKind::Immediate | OperandKind::Any) => Ok((
                RawOperand::MacroImm(
                    MacroImm::from_str(m).map_err(|_| (ParseError::UnknownMacro, self.span()))?,
                ),
                self.span(),
            )),
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
}
