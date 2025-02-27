pub mod error;
mod macros;
mod operand;
mod util;

use std::collections::HashMap;

use crate::lexer::{LexResult, Lexer, Token};

use dashu::Integer;
use error::ParseError;
use logos::Span;
use num_traits::ToPrimitive;
use operand::RawOperand;
use urcl_ast::{Any, Immediate, InstProperties, Instruction, OperandKind, Program};

type MidInst<'a> = (&'a str, Vec<(RawOperand<'a>, Span)>, Span);

#[derive(Debug)]
pub struct Parser<'a> {
    lex: Lexer<'a>,
    peeked: Option<LexResult<'a>>,
    start: usize,

    bits: Option<u32>,
    registers: Option<u16>,
    min_stack: Option<u64>,
    min_heap: Option<u64>,

    labels: HashMap<&'a str, Immediate>,
    defines: HashMap<Token<'a>, (RawOperand<'a>, Span)>,

    instructions: Vec<(MidInst<'a>, Span)>,
    dw: Vec<(RawOperand<'a>, Span)>,

    errors: Vec<(ParseError, Span)>,

    port_v2: bool,
}

#[derive(Debug, Clone, strum::EnumString)]
#[strum(ascii_case_insensitive)]
pub(crate) enum MacroImm {
    Bits,
    MinReg,
    MinHeap,
    MinStack,
    Heap,
    Msb,
    SMsb,
    Max,
    SMax,
    UHalf,
    LHalf,
}

impl<'a> Parser<'a> {
    pub fn new(lex: Lexer<'a>) -> Self {
        Self {
            lex,
            peeked: None,
            start: 0,

            bits: None,
            registers: None,
            min_stack: None,
            min_heap: None,

            labels: HashMap::new(),
            defines: HashMap::new(),

            instructions: Vec::new(),
            dw: Vec::new(),

            errors: Vec::new(),

            port_v2: false,
        }
    }

    fn parse_inst_inner(
        &mut self,
        errors: &mut bool,
        oprs: &mut [(RawOperand<'a>, Span)],
        nth: usize,
        i: &InstProperties,
    ) {
        if matches!(self.peek_next(), Some(Ok(Token::Newline)) | None) {
            *errors = true;
            self.error(ParseError::UnexpectedNewline);
        }

        let opr = self.parse_operand(&i.operands[nth]);
        opr.map_or_else(
            |e| {
                *errors = true;
                self.errors.push(e);
            },
            |o| oprs[nth] = o,
        );
    }

    fn parse_inst(&mut self, n: &'a str) -> Result<MidInst<'a>, ()> {
        if let Some(i) = Instruction::properties(n) {
            let mut oprs = vec![
                (RawOperand::Immediate(Immediate::Value(Integer::ZERO)), 0..0);
                i.operands.len()
            ];
            let mut errors = false;

            if let (true, Some(j)) = (self.port_v2, i.port_v2) {
                for nth in j.iter().copied() {
                    self.parse_inst_inner(&mut errors, &mut oprs, nth, &i);
                    if errors {
                        break;
                    }
                }
            } else {
                for nth in 0..i.operands.len() {
                    self.parse_inst_inner(&mut errors, &mut oprs, nth, &i);
                    if errors {
                        break;
                    }
                }
            }

            self.expect_nl();
            (!errors).then_some((n, oprs, self.total_span())).ok_or(())
        } else {
            self.error(ParseError::UnknownInst);
            self.wait_nl();
            Err(())
        }
    }

    fn parse_add_dw(&mut self) {
        let mut in_sq_bracket = false;

        while let Some(t) = self.next_token() {
            match t {
                Ok(Token::SqBrStart) => {
                    if in_sq_bracket {
                        self.error(ParseError::UnexpectedToken);
                    }

                    in_sq_bracket = true;
                },
                Ok(Token::SqBrEnd) => {
                    if !in_sq_bracket {
                        self.error(ParseError::UnexpectedToken);
                    }

                    in_sq_bracket = false;
                },
                Ok(Token::Newline) => {
                    if !in_sq_bracket {
                        break;
                    }
                },
                Ok(Token::String(s)) => {
                    for c in s.iter() {
                        self.dw.push((
                            RawOperand::Immediate(Immediate::Value((*c).into())),
                            self.span(),
                        ));
                    }
                },
                Ok(_) => match self.parse_operand_with_option(Some(t), &OperandKind::Immediate) {
                    Ok(w) => self.dw.push(w),
                    Err(e) => self.errors.push(e),
                },
                Err(e) => self.error(ParseError::LexError(e)),
            }
        }
    }

    pub fn parse_program(mut self, max_ram: u64) -> Result<Program, Vec<(ParseError, Span)>> {
        let mut pending_labels: Vec<&str> = Vec::new();

        while let Some(t) = self.next_token() {
            self.start = self.span().start;
            match t {
                Ok(Token::Macro(name)) => self.parse_macro(name),
                Ok(t) if t.is_macro_or_name("bits") => {
                    if let Some(Ok(Token::CmpLe | Token::CmpGe | Token::CmpEq)) = self.peek_next() {
                        self.next_token();
                    }

                    if self.bits.is_some() {
                        self.total_span_error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        self.bits = Some(self.parse_u32());
                        self.expect_nl();
                    }
                },
                Ok(t) if t.is_macro_or_name("minreg") => {
                    if self.registers.is_some() {
                        self.total_span_error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        self.registers = Some(self.parse_u16());
                        self.expect_nl();
                    }
                },
                Ok(t) if t.is_macro_or_name("minstack") => {
                    if self.min_stack.is_some() {
                        self.total_span_error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        self.min_stack = Some(self.parse_u64());
                        self.expect_nl();
                    }
                },
                Ok(t) if t.is_macro_or_name("minheap") => {
                    if self.min_heap.is_some() {
                        self.total_span_error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        self.min_heap = Some(self.parse_u64());
                        self.expect_nl();
                    }
                },
                Ok(t) if t.is_name("dw") => {
                    pending_labels.iter().for_each(|i| {
                        self.labels
                            .insert(i, Immediate::Value(self.dw.len().into()));
                    });
                    pending_labels.clear();
                    self.parse_add_dw();
                },
                Ok(Token::Name(n)) => {
                    pending_labels.iter().for_each(|i| {
                        self.labels
                            .insert(i, Immediate::InstLoc(self.instructions.len()));
                    });
                    pending_labels.clear();
                    if let Ok(i) = self.parse_inst(n) {
                        self.instructions.push((i, self.total_span()));
                    }
                },
                Ok(Token::Label(l)) => {
                    if self.labels.contains_key(l) || pending_labels.contains(&l) {
                        self.error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        pending_labels.push(l);
                        self.expect_nl();
                    }
                },
                Ok(Token::Newline) => {},
                Ok(_) => self.error(ParseError::UnexpectedToken),
                Err(e) => self.error(ParseError::LexError(e)),
            }
        }

        pending_labels.iter().for_each(|i| {
            self.labels
                .insert(i, Immediate::InstLoc(self.instructions.len()));
        });

        let heap_size = self.min_heap().max(
            max_ram
                .saturating_sub(self.min_stack())
                .saturating_sub(self.dw.len() as _),
        );

        let instructions = core::mem::take(&mut self.instructions);
        let dw = core::mem::take(&mut self.dw);
        let dw_len = dw.len() as u128;

        let p = Program {
            bits: self.bits(),
            registers: self.registers(),
            min_stack: self.min_stack(),
            min_heap: self.min_heap(),

            heap_size,

            instructions: instructions
                .into_iter()
                .map(|(i, span)| {
                    (
                        Instruction::construct(
                            i.0,
                            i.1.iter()
                                .map(|i| self.finalize(i, dw_len, heap_size))
                                .collect(),
                        ),
                        span,
                    )
                })
                .collect(),
            dw: dw
                .iter()
                .map(|i| {
                    self.finalize(i, dw_len, heap_size)
                        .try_as_immediate()
                        .unwrap()
                })
                .collect(),
        };

        if self.errors.is_empty() {
            Ok(p)
        } else {
            Err(self.errors)
        }
    }

    fn finalize(&mut self, op: &(RawOperand, Span), dw_len: u128, heap_size: u64) -> Any {
        match &op.0 {
            RawOperand::Register(r) => Any::Register(r.clone()),
            RawOperand::Immediate(i) => Any::Immediate(i.clone()),
            RawOperand::Heap(h) => Any::Immediate(Immediate::Value(dw_len + h)),
            RawOperand::MacroImm(MacroImm::Bits) => {
                Any::Immediate(Immediate::Value(self.bits().into()))
            },
            RawOperand::MacroImm(MacroImm::MinReg) => {
                Any::Immediate(Immediate::Value(self.registers().into()))
            },
            RawOperand::MacroImm(MacroImm::MinStack) => {
                Any::Immediate(Immediate::Value(self.min_stack().into()))
            },
            RawOperand::MacroImm(MacroImm::MinHeap) => {
                Any::Immediate(Immediate::Value(self.min_heap().into()))
            },
            RawOperand::MacroImm(MacroImm::Heap) => {
                Any::Immediate(Immediate::Value(heap_size.into()))
            },
            RawOperand::MacroImm(MacroImm::Max) => Any::Immediate(Immediate::Value(
                (Integer::ONE << self.bits().to_usize().unwrap()) - 1,
            )),
            RawOperand::MacroImm(MacroImm::SMax) => Any::Immediate(Immediate::Value(
                (Integer::ONE << (self.bits().to_usize().unwrap() - 1)) - 1,
            )),
            RawOperand::MacroImm(MacroImm::Msb) => Any::Immediate(Immediate::Value(
                Integer::ONE << (self.bits().to_usize().unwrap() - 1),
            )),
            RawOperand::MacroImm(MacroImm::SMsb) => Any::Immediate(Immediate::Value(
                Integer::ONE << (self.bits().to_usize().unwrap() - 2),
            )),
            RawOperand::MacroImm(MacroImm::UHalf | MacroImm::LHalf) => todo!(),
            RawOperand::Label(l) => Any::Immediate(self.labels.get(l).map_or_else(
                || {
                    self.errors.push((ParseError::UnknownLabel, op.1.clone()));
                    Immediate::Value(Integer::ZERO)
                },
                |v| v.clone(),
            )),
        }
    }
}
