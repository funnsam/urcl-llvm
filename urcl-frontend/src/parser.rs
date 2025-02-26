use std::{collections::HashMap, str::FromStr};

use crate::*;

use dashu::Integer;
use num_traits::ToPrimitive;
use urcl_ast::*;

type MidInst<'a> = (&'a str, Vec<(RawOperand<'a>, Span)>, Span);

#[derive(Debug)]
pub struct Parser<'a> {
    lex: lexer::Lexer<'a>,
    peeked: Option<lexer::LexResult<'a>>,
    start: usize,

    bits: Option<u32>,
    registers: Option<u16>,
    min_stack: Option<u64>,
    min_heap: Option<u64>,

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

    fn error(&mut self, err: ParseError) {
        self.errors.push((err, self.span()));
    }

    fn total_span_error(&mut self, err: ParseError) {
        self.errors.push((err, self.total_span()));
    }

    fn wait_nl(&mut self) {
        while let Some(t) = self.next_token() {
            match t {
                Ok(lexer::Token::Newline) => break,
                Ok(_) => {},
                Err(e) => self.error(ParseError::LexError(e)),
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
                    (Integer::from(self.instructions.len()) + r)
                        .to_usize()
                        .ok_or((ParseError::InvalidRelative, self.span()))?,
                )),
                self.span(),
            )),
            (
                Some(Ok(lexer::Token::Port(p))),
                OperandKind::Immediate | OperandKind::Any,
            ) => Ok((
                RawOperand::Immediate(Immediate::Value((p as usize).into())),
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
            let mut oprs =
                vec![(RawOperand::Immediate(Immediate::Value(Integer::ZERO)), 0..0); i.operands.len()];
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

    fn parse_int(&mut self) -> Integer {
        self.next_token()
            .and_then(|t| t.ok())
            .and_then(|t| t.try_as_integer())
            .unwrap_or_else(|| {
                self.error(ParseError::ExpectedInt);
                Integer::ZERO
            })
    }

    fn opt_unwrap_or_err_default<T: Default>(&mut self, opt: Option<T>, err: ParseError) -> T {
        opt.unwrap_or_else(|| {
            self.error(err);
            T::default()
        })
    }

    fn parse_u64(&mut self) -> u64 {
        let int = self.parse_int().to_u64();
        self.opt_unwrap_or_err_default(int, ParseError::InvalidValue)
    }

    fn parse_u32(&mut self) -> u32 {
        let int = self.parse_int().to_u32();
        self.opt_unwrap_or_err_default(int, ParseError::InvalidValue)
    }

    fn parse_u16(&mut self) -> u16 {
        let int = self.parse_int().to_u16();
        self.opt_unwrap_or_err_default(int, ParseError::InvalidValue)
    }

    fn expect_nl(&mut self) {
        if !matches!(self.peek_next(), Some(Ok(lexer::Token::Newline)) | None) {
            self.error(ParseError::ExpectedNewline);
            self.wait_nl();
        }
    }

    fn parse_add_dw(&mut self) {
        let mut in_sq_bracket = false;

        while let Some(t) = self.next_token() {
            match t {
                Ok(lexer::Token::SqBrStart) => {
                    if in_sq_bracket {
                        self.error(ParseError::UnexpectedToken);
                    }

                    in_sq_bracket = true;
                },
                Ok(lexer::Token::SqBrEnd) => {
                    if !in_sq_bracket {
                        self.error(ParseError::UnexpectedToken);
                    }

                    in_sq_bracket = false;
                },
                Ok(lexer::Token::Newline) => if !in_sq_bracket { break },
                Ok(lexer::Token::String(s)) => {
                    for c in s.iter() {
                        self.dw.push((
                            RawOperand::Immediate(Immediate::Value((*c).into())),
                            self.span(),
                        ));
                    }
                },
                Ok(_) => match self.parse_operand_with(Some(t), &OperandKind::Immediate) {
                    Ok(w) => self.dw.push(w),
                    Err(e) => self.errors.push(e),
                },
                Err(e) => self.error(ParseError::LexError(e)),
            }
        }
    }

    pub fn parse_program(
        mut self,
        max_ram: u64,
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
                        self.total_span_error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        self.bits = Some(self.parse_u32());
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if n.eq_ignore_ascii_case("minreg") =>
                {
                    if self.registers.is_some() {
                        self.total_span_error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        self.registers = Some(self.parse_u16());
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if n.eq_ignore_ascii_case("minstack") =>
                {
                    if self.min_stack.is_some() {
                        self.total_span_error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        self.min_stack = Some(self.parse_u64());
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if n.eq_ignore_ascii_case("minheap") =>
                {
                    if self.min_heap.is_some() {
                        self.total_span_error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        self.min_heap = Some(self.parse_u64());
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Name(n)) if n.eq_ignore_ascii_case("dw") => {
                    pending_labels.iter().for_each(|i| {
                        self.labels
                            .insert(i, Immediate::Value(self.dw.len().into()));
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
                            self.error(ParseError::UnexpectedToken);
                            self.wait_nl();
                        },
                        Some(Err(e)) => {
                            self.error(ParseError::LexError(e));
                            self.wait_nl();
                        },
                        None => {
                            self.error(ParseError::UnexpectedEof);
                        },
                    }
                },
                Ok(lexer::Token::Macro(n)) if n.eq_ignore_ascii_case("port_v2") => {
                    self.expect_nl();
                    self.port_v2 = true;
                },
                Ok(lexer::Token::Macro(_)) => {
                    self.wait_nl();
                    self.error(ParseError::UnknownMacro);
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
                        self.error(ParseError::ItemRedefined);
                        self.wait_nl();
                    } else {
                        pending_labels.push(l);
                        self.expect_nl();
                    }
                },
                Ok(lexer::Token::Newline) => {},
                Ok(_) => self.error(ParseError::UnexpectedToken),
                Err(e) => self.error(ParseError::LexError(e)),
            }
        }

        pending_labels.iter().for_each(|i| {
            self.labels
                .insert(i, Immediate::InstLoc(self.instructions.len()));
        });

        let heap_size = self.min_heap().max(
            max_ram.saturating_sub(self.min_stack()).saturating_sub(self.dw.len() as _)
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

    fn bits(&self) -> u32 { self.bits.unwrap_or(8) }
    fn registers(&self) -> u16 { self.registers.unwrap_or(8) }
    fn min_stack(&self) -> u64 { self.min_stack.unwrap_or(8) }
    fn min_heap(&self) -> u64 { self.min_heap.unwrap_or(16) }

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
    InvalidValue,
}
