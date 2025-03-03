pub mod error;
mod macro_expr;
mod macro_imm;
mod macros;
mod operand;
mod util;

use core::cell::RefCell;
use std::collections::{HashMap, HashSet};

use crate::lexer::{LexResult, Lexer, Token};

use dashu::Integer;
use error::ParseError;
use logos::Span;
use operand::RawOperand;
use urcl_ast::{InstProperties, Instruction, IntImm, OperandKind, Program};

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

    labels: HashMap<&'a str, IntImm>,
    defines: HashMap<Token<'a>, (RawOperand<'a>, Span)>,
    features: HashSet<&'a str>,

    instructions: Vec<(MidInst<'a>, Span)>,
    dw: Vec<(RawOperand<'a>, Span)>,

    errors: RefCell<Vec<(ParseError, Span)>>,
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
            features: HashSet::new(),

            errors: RefCell::new(Vec::new()),
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

        let opr = self.parse_operand(i.operands[nth]);
        opr.map_or_else(
            |e| {
                *errors = true;
                self.error_at(e.0, e.1);
            },
            |o| oprs[nth] = o,
        );
    }

    fn parse_inst(&mut self, n: &'a str) -> Result<MidInst<'a>, ()> {
        if let Some(i) = Instruction::properties(n) {
            let mut oprs =
                vec![(RawOperand::IntImm(IntImm::Value(Integer::ZERO)), 0..0); i.operands.len()];
            let mut errors = false;

            if let (true, Some(j)) = (self.feature("port_v2"), i.port_v2) {
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

            if errors {
                self.wait_nl()
            } else {
                self.expect_nl()
            };
            (!errors).then_some((n, oprs, self.total_span())).ok_or(())
        } else {
            self.error(ParseError::UnknownInst);
            self.wait_nl();
            Err(())
        }
    }

    fn parse_add_dw(&mut self) {
        let mut in_bracket = false;

        while let Some(t) = self.next_token() {
            match t {
                Ok(Token::BrStart) => {
                    if in_bracket {
                        self.error(ParseError::UnexpectedToken);
                    }

                    in_bracket = true;
                },
                Ok(Token::BrEnd) => {
                    if !in_bracket {
                        self.error(ParseError::UnexpectedToken);
                    }

                    in_bracket = false;
                },
                Ok(Token::Newline) => {
                    if !in_bracket {
                        break;
                    }
                },
                Ok(Token::String(s)) => {
                    for c in s.iter() {
                        self.dw
                            .push((RawOperand::IntImm(IntImm::Value((*c).into())), self.span()));
                    }
                },
                Ok(_) => match self.parse_operand_with_option(Some(t), OperandKind::AnyImm) {
                    Ok(w) => self.dw.push(w),
                    Err(e) => self.error_at(e.0, e.1),
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
                        self.labels.insert(i, IntImm::Value(self.dw.len().into()));
                    });
                    pending_labels.clear();
                    self.parse_add_dw();
                },
                Ok(Token::Name(n)) => {
                    pending_labels.iter().for_each(|i| {
                        self.labels
                            .insert(i, IntImm::InstLoc(self.instructions.len()));
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
                .insert(i, IntImm::InstLoc(self.instructions.len()));
        });

        let heap_size = self.min_heap().max(
            max_ram
                .saturating_sub(self.min_stack())
                .saturating_sub(self.dw.len() as _),
        );

        let instructions = core::mem::take(&mut self.instructions);

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
                            i.1.iter().map(|i| self.finalize(i, heap_size)).collect(),
                        ),
                        span,
                    )
                })
                .collect(),
            dw: self
                .dw
                .iter()
                .map(|i| self.finalize(i, heap_size).try_as_any_imm().unwrap())
                .collect(),
        };

        if self.errors.borrow().is_empty() {
            Ok(p)
        } else {
            Err(self.errors.into_inner())
        }
    }
}
