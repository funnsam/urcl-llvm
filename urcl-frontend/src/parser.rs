use std::{collections::HashMap, str::FromStr};

use crate::*;

use dashu::{ibig, Integer, Natural};
use num_traits::ToPrimitive;
use urcl_ast::*;

type MidInst<'a> = (&'a str, Vec<(RawOperand<'a>, Span)>, Span);

#[derive(Debug)]
pub struct Parser<'a> {
    lex: lexer::Lexer<'a>,
    peeked: Option<lexer::LexResult<'a>>,
    start: usize,

    bits: Option<usize>,
    registers: Option<Natural>,
    min_stack: Option<Natural>,
    min_heap: Option<Natural>,

    labels: HashMap<&'a str, Immediate>,
    defines: HashMap<&'a str, (RawOperand<'a>, Span)>,

    instructions: Vec<(MidInst<'a>, Span)>,
    dw: Vec<(RawOperand<'a>, Span)>,

    errors: Vec<(ParseError, Span)>,

    port_v2: bool,
}

#[derive(Debug, Clone)]
enum RawOperand<'a> {
    Register(Register),
    Immediate(Immediate),
    Heap(Integer),
    MacroImm(MacroImm),
    Label(&'a str),
}

#[derive(Debug, Clone, strum::EnumString)]
#[strum(ascii_case_insensitive)]
enum MacroImm {
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
    pub fn new(lex: lexer::Lexer<'a>) -> Self {
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

    fn next_token(&mut self) -> Option<lexer::LexResult<'a>> {
        if self.peeked.is_some() {
            core::mem::take(&mut self.peeked)
        } else {
            self.lex.next()
        }
    }

    fn peek_next(&mut self) -> Option<lexer::LexResult<'a>> {
        if self.peeked.is_none() {
            self.peeked = self.lex.next();
        }

        self.peeked.clone()
    }

    fn span(&self) -> Span { self.lex.span() }
    fn total_span(&self) -> Span { self.start..self.lex.span().end }

    fn wait_nl(&mut self) {
        while let Some(t) = self.next_token() {
            match t {
                Ok(lexer::Token::Newline) => break,
                Ok(_) => {},
                Err(e) => self.errors.push((ParseError::LexError(e), self.span())),
            }
        }
    }

    fn parse_operand(
        &mut self,
        ok: &'static OperandKind,
    ) -> Result<(RawOperand<'a>, Span), (ParseError, Span)> {
        let t = self.next_token();
        self.parse_operand_with(t, ok)
    }

    fn parse_operand_with(
        &mut self,
        t: Option<lexer::LexResult<'a>>,
        ok: &'static OperandKind,
    ) -> Result<(RawOperand<'a>, Span), (ParseError, Span)> {
        match (t, ok) {
            (Some(Ok(lexer::Token::Name(n))), _) => {
                let r = self
                    .defines
                    .get(n)
                    .cloned()
                    .ok_or((ParseError::UnknownName, self.span()))?;

                match (&r.0, ok) {
                    (_, OperandKind::Any)
                    | (RawOperand::Register(_), OperandKind::Register)
                    | (
                        RawOperand::Immediate(_)
                        | RawOperand::Heap(_)
                        | RawOperand::MacroImm(_)
                        | RawOperand::Label(_),
                        OperandKind::Immediate,
                    ) => Ok(r),
                    _ => Err((ParseError::InvalidOperand(ok), self.span())),
                }
            },
            (
                Some(Ok(lexer::Token::Reg(r))),
                OperandKind::Register | OperandKind::Any,
            ) => Ok((RawOperand::Register(r), self.span())),
            (
                Some(Ok(lexer::Token::Integer(i))),
                OperandKind::Immediate | OperandKind::Any,
            ) => Ok((RawOperand::Immediate(Immediate::Value(i)), self.span())),
            (
                Some(Ok(lexer::Token::Heap(h))),
                OperandKind::Immediate | OperandKind::Any,
            ) => Ok((RawOperand::Heap(h), self.span())),
            (
                Some(Ok(lexer::Token::Macro(m))),
                OperandKind::Immediate | OperandKind::Any,
            ) => Ok((
                RawOperand::MacroImm(
                    MacroImm::from_str(m).map_err(|_| (ParseError::UnknownMacro, self.span()))?,
                ),
                self.span(),
            )),
            (
                Some(Ok(lexer::Token::Label(l))),
                OperandKind::Immediate | OperandKind::Any,
            ) => Ok((RawOperand::Label(l), self.span())),
            (
                Some(Ok(lexer::Token::Relative(r))),
                OperandKind::Immediate | OperandKind::Any,
            ) => Ok((
                RawOperand::Immediate(Immediate::InstLoc(
                    self.instructions.len() + r as usize,
                )),
                self.span(),
            )),
            (
                Some(Ok(lexer::Token::Port(p))),
                OperandKind::Immediate | OperandKind::Any,
            ) => Ok((
                RawOperand::Immediate(Immediate::Value(
                    Port::from_str(p).map_err(|_| (ParseError::UnknownPort, self.span()))?
                        as _,
                )),
                self.span(),
            )),
            (Some(Ok(_)), _) => Err((ParseError::InvalidOperand(ok), self.span())),
            (Some(Err(e)), _) => Err((ParseError::LexError(e), self.span())),
            (None, _) => Err((ParseError::UnexpectedEof, self.span())),
        }
    }

    fn parse_inst_inner(
        &mut self,
        errors: &mut bool,
        oprs: &mut [(RawOperand<'a>, Span)],
        nth: usize,
        i: &InstProperties,
    ) {
        if matches!(self.peek_next(), Some(Ok(lexer::Token::Newline)) | None) {
            *errors = true;
            self.errors
                .push((ParseError::UnexpectedNewline, self.span()));
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
            let mut oprs =
                vec![(RawOperand::Immediate(Immediate::Value(0)), 0..0); i.operands.len()];
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
            self.errors.push((ParseError::UnknownInst, self.span()));
            self.wait_nl();
            Err(())
        }
    }

    fn expect_nl(&mut self) {
        if !matches!(self.peek_next(), Some(Ok(lexer::Token::Newline)) | None) {
            self.errors.push((ParseError::ExpectedNewline, self.span()));
            self.wait_nl();
        }
    }

    fn parse_add_dw(&mut self) {
        while let Some(t) = self.next_token() {
            match t {
                Ok(lexer::Token::SqBrStart | lexer::Token::SqBrEnd) => {}, // yes, we ignore it
                Ok(lexer::Token::Newline) => break,
                Ok(lexer::Token::String(s)) => {
                    for c in s.iter() {
                        self.dw.push((
                            RawOperand::Immediate(Immediate::Value(*c as _)),
                            self.span(),
                        ));
                    }
                },
                Ok(_) => match self.parse_operand_with(Some(t), &OperandKind::Immediate) {
                    Ok(w) => self.dw.push(w),
                    Err(e) => self.errors.push(e),
                },
                Err(e) => self.errors.push((ParseError::LexError(e), self.span())),
            }
        }
    }

    pub fn parse_program(
        mut self,
        max_ram: usize,
    ) -> Result<Program, Vec<(ParseError, Span)>> {
        let mut pending_labels: Vec<&str> = Vec::new();

        while let Some(t) = self.next_token() {
            self.start = self.span().start;
            match t {
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if n.eq_ignore_ascii_case("bits") =>
                {
                    if let Some(Ok(
                        lexer::Token::CmpLe | lexer::Token::CmpGe | lexer::Token::CmpEq,
                    )) = self.peek_next()
                    {
                        self.next_token();
                    }

                    if self.bits.is_some() {
                        self.errors
                            .push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        self.bits = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_integer())
                                .unwrap_or_else(|| {
                                    self.errors.push((ParseError::ExpectedInt, self.span()));
                                    0
                                }),
                        );
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if n.eq_ignore_ascii_case("minreg") =>
                {
                    if self.registers.is_some() {
                        self.errors
                            .push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        self.registers = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .unwrap_or_else(|| {
                                    self.errors.push((ParseError::ExpectedInt, self.span()));
                                    0
                                }) as _,
                        );
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if n.eq_ignore_ascii_case("minstack") =>
                {
                    if self.min_stack.is_some() {
                        self.errors
                            .push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        self.min_stack = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .unwrap_or_else(|| {
                                    self.errors.push((ParseError::ExpectedInt, self.span()));
                                    0
                                }) as _,
                        );
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if n.eq_ignore_ascii_case("minheap") =>
                {
                    if self.min_heap.is_some() {
                        self.errors
                            .push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        self.min_heap = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .unwrap_or_else(|| {
                                    self.errors.push((ParseError::ExpectedInt, self.span()));
                                    0
                                }) as _,
                        );
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Name(n)) if n.eq_ignore_ascii_case("dw") => {
                    pending_labels.iter().for_each(|i| {
                        self.labels
                            .insert(i, Immediate::Value(self.dw.len() as u128));
                    });
                    pending_labels.clear();
                    self.parse_add_dw();
                },
                Ok(lexer::Token::Macro(n)) if n.eq_ignore_ascii_case("define") => {
                    match self.next_token() {
                        Some(Ok(lexer::Token::Name(n))) => {
                            let _ = self.parse_operand(&OperandKind::Any).map(|v| {
                                self.defines.insert(n, v);
                            });
                            self.expect_nl();
                        },
                        Some(Ok(_)) => {
                            self.errors.push((ParseError::UnexpectedToken, self.span()));
                            self.wait_nl();
                        },
                        Some(Err(e)) => {
                            self.errors.push((ParseError::LexError(e), self.span()));
                            self.wait_nl();
                        },
                        None => {
                            self.errors.push((ParseError::UnexpectedEof, self.span()));
                        },
                    }
                },
                Ok(lexer::Token::Macro(n)) if n.eq_ignore_ascii_case("port_v2") => {
                    self.expect_nl();
                    self.port_v2 = true;
                },
                Ok(lexer::Token::Macro(_)) => {
                    self.wait_nl();
                    self.errors
                        .push((ParseError::UnknownMacro, self.total_span()));
                },
                Ok(lexer::Token::Name(n)) => {
                    pending_labels.iter().for_each(|i| {
                        self.labels
                            .insert(i, Immediate::InstLoc(self.instructions.len()));
                    });
                    pending_labels.clear();
                    if let Ok(i) = self.parse_inst(n) {
                        self.instructions.push((i, self.total_span()));
                    }
                },
                Ok(lexer::Token::Label(l)) => {
                    if self.labels.contains_key(l) || pending_labels.contains(&l) {
                        self.errors
                            .push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        pending_labels.push(l);
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Newline) => {},
                Ok(_) => self.errors.push((ParseError::UnexpectedToken, self.span())),
                Err(e) => self.errors.push((ParseError::LexError(e), self.span())),
            }
        }

        pending_labels.iter().for_each(|i| {
            self.labels
                .insert(i, Immediate::InstLoc(self.instructions.len()));
        });

        let heap_size = self.min_heap.unwrap().max(
            max_ram
                .saturating_sub(self.min_stack.unwrap())
                .saturating_sub(self.dw.len()),
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
                    (Instruction::construct(
                        i.0,
                        i.1.iter()
                            .map(|i| self.finalize(i, dw_len, heap_size))
                            .collect(),
                    ), span)
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

    fn bits(&self) -> usize { self.bits.unwrap_or(8) }
    fn registers(&self) -> Natural { self.registers.as_ref().unwrap_or(&ibig!(8)).clone() }
    fn min_stack(&self) -> Natural { self.min_stack.as_ref().unwrap_or(&ibig!(8)).clone() }
    fn min_heap(&self) -> Natural { self.min_heap.as_ref().unwrap_or(&ibig!(16)).clone() }

    fn finalize(&mut self, op: &(RawOperand, Span), dw_len: u128, heap_size: usize) -> Any {
        match &op.0 {
            RawOperand::Register(r) => Any::Register(r.clone()),
            RawOperand::Immediate(i) => Any::Immediate(i.clone()),
            RawOperand::Heap(h) => Any::Immediate(Immediate::Value(dw_len + h)),
            RawOperand::MacroImm(MacroImm::Bits) => {
                Any::Immediate(Immediate::Value(self.bits()))
            },
            RawOperand::MacroImm(MacroImm::MinReg) => {
                Any::Immediate(Immediate::Value(self.registers()))
            },
            RawOperand::MacroImm(MacroImm::MinStack) => {
                Any::Immediate(Immediate::Value(self.min_stack()))
            },
            RawOperand::MacroImm(MacroImm::MinHeap) => {
                Any::Immediate(Immediate::Value(self.min_heap()))
            },
            RawOperand::MacroImm(MacroImm::Heap) => {
                Any::Immediate(Immediate::Value(heap_size.into()))
            },
            RawOperand::MacroImm(MacroImm::Max) => {
                Any::Immediate(Immediate::Value((Integer::ONE << self.bits().to_usize().unwrap()) - 1))
            },
            RawOperand::MacroImm(MacroImm::SMax) => Any::Immediate(Immediate::Value(
                (Integer::ONE << (self.bits().to_usize().unwrap() - 1)) - 1,
            )),
            RawOperand::MacroImm(MacroImm::Msb) => {
                Any::Immediate(Immediate::Value(Integer::ONE << (self.bits().to_usize().unwrap() - 1)))
            },
            RawOperand::MacroImm(MacroImm::SMsb) => {
                Any::Immediate(Immediate::Value(Integer::ONE << (self.bits().to_usize().unwrap() - 2)))
            },
            RawOperand::MacroImm(MacroImm::UHalf | MacroImm::LHalf) => todo!(),
            RawOperand::Label(l) => Any::Immediate(
                self.labels
                    .get(l)
                    .map_or_else(
                        || {
                            self.errors.push((ParseError::UnknownLabel, op.1.clone()));
                            Immediate::Value(Integer::ZERO)
                        },
                        |v| v.clone(),
                    )
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError(()),
    InvalidOperand(&'static OperandKind),
    UnexpectedEof,
    ExpectedNewline,
    UnknownMacro,
    ItemRedefined,
    ExpectedInt,
    UnknownInst,
    UnexpectedToken,
    UnknownLabel,
    UnknownPort,
    UnexpectedNewline,
    InvalidRelative,
    UnknownName,
}
