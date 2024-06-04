use std::{collections::HashMap, str::FromStr};

use crate::*;

#[derive(Debug)]
pub struct Parser<'a> {
    lex: lexer::Lexer<'a>,
    peeked: Option<lexer::LexResult<'a>>,
    start: usize,

    bits: Option<usize>,
    registers: Option<usize>,
    stack_size: Option<usize>,
    heap_size: Option<usize>,

    instructions: Vec<(&'a str, Vec<(RawOperand<'a>, Span)>, Span)>,
    labels: HashMap<&'a str, u128>,
    dw: Vec<(RawOperand<'a>, Span)>,

    errors: Vec<(ParseError, Span)>,
}

#[derive(Debug)]
enum RawOperand<'a> {
    Register(ast::Register),
    Immediate(ast::Immediate),
    Heap(u128),
    MacroImm(MacroImm),
    Label(&'a str),
}

#[derive(Debug, strum::EnumString)]
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
            stack_size: None,
            heap_size: None,

            instructions: Vec::new(),
            labels: HashMap::new(),
            dw: Vec::new(),

            errors: Vec::new(),
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
        ok: &'static ast::OperandKind,
    ) -> Result<(RawOperand<'a>, Span), (ParseError, Span)> {
        let t = self.next_token();
        self.parse_operand_with(t, ok)
    }

    fn parse_operand_with(
        &mut self,
        t: Option<lexer::LexResult<'a>>,
        ok: &'static ast::OperandKind,
    ) -> Result<(RawOperand<'a>, Span), (ParseError, Span)> {
        match (t, ok) {
            (
                Some(Ok(lexer::Token::Reg(r))),
                ast::OperandKind::Register | ast::OperandKind::Any,
            ) => Ok((RawOperand::Register(ast::Register(r)), self.span())),
            (
                Some(Ok(lexer::Token::Int(i))),
                ast::OperandKind::Immediate | ast::OperandKind::Any,
            ) => Ok((RawOperand::Immediate(ast::Immediate(i)), self.span())),
            (
                Some(Ok(lexer::Token::Heap(h))),
                ast::OperandKind::Immediate | ast::OperandKind::Any,
            ) => Ok((RawOperand::Heap(h), self.span())),
            (
                Some(Ok(lexer::Token::Macro(m))),
                ast::OperandKind::Immediate | ast::OperandKind::Any,
            ) => Ok((
                RawOperand::MacroImm(
                    MacroImm::from_str(m).map_err(|_| (ParseError::UnknownMacro, self.span()))?,
                ),
                self.span(),
            )),
            (
                Some(Ok(lexer::Token::Label(l))),
                ast::OperandKind::Immediate | ast::OperandKind::Any,
            ) => Ok((RawOperand::Label(l), self.span())),
            (Some(Ok(_)), _) => Err((ParseError::InvalidOperand(ok), self.span())),
            (Some(Err(e)), _) => Err((ParseError::LexError(e), self.span())),
            (None, _) => Err((ParseError::UnexpectedEof, self.span())),
        }
    }

    fn parse_inst(
        &mut self,
        n: &'a str,
    ) -> Result<(&'a str, Vec<(RawOperand<'a>, Span)>, Span), ()> {
        if let Some(i) = ast::Instruction::properties(n) {
            let mut oprs = Vec::with_capacity(i.operands.len());
            for op in i.operands.iter() {
                let opr = self.parse_operand(op);
                opr.map_or_else(
                    |e| self.errors.push(e),
                    |o| oprs.push(o),
                );
            }

            if !matches!(self.peek_next(), Some(Ok(lexer::Token::Newline)) | None) {
                self.errors.push((ParseError::ExpectedNewline, self.span()));
            }

            Ok((n, oprs, self.total_span()))
        } else {
            self.errors.push((ParseError::UnknownInst, self.span()));
            self.wait_nl();
            Err(())
        }
    }

    fn parse_add_dw(&mut self) {
        while let Some(t) = self.next_token() {
            match t {
                Ok(lexer::Token::SqBrStart | lexer::Token::SqBrEnd) => {}, // yes, we ignore it
                Ok(lexer::Token::Newline) => break,
                Ok(lexer::Token::String(s)) => {
                    for c in s.iter() {
                        self.dw
                            .push((RawOperand::Immediate(ast::Immediate(*c as _)), self.span()));
                    }
                },
                Ok(_) => {
                    match self.parse_operand_with(Some(t), &ast::OperandKind::Immediate) {
                        Ok(w) => self.dw.push(w),
                        Err(e) => self.errors.push(e),
                    }
                },
                Err(e) => self.errors.push((ParseError::LexError(e), self.span())),
            }
        }
    }


    pub fn parse_program(mut self) -> Result<ast::Program, Vec<(ParseError, Span)>> {
        let mut pending_labels: Vec<&str> = Vec::new();

        while let Some(t) = self.next_token() {
            self.start = self.span().start;
            match t {
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if matches!(n.to_lowercase().as_str(), "bits") =>
                {
                    match self.peek_next() {
                        Some(Ok(
                            lexer::Token::CmpLe | lexer::Token::CmpGe | lexer::Token::CmpEq,
                        )) => {
                            self.next_token();
                        },
                        _ => {},
                    }

                    if self.bits.is_some() {
                        self.errors.push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        self.bits = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .unwrap_or_else(|| {
                                    self.errors.push((ParseError::ExpectedInt, self.span()));
                                    0
                                })
                                as _,
                        );
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if matches!(n.to_lowercase().as_str(), "minreg") =>
                {
                    if self.registers.is_some() {
                        self.errors.push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        self.registers = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .unwrap_or_else(|| {
                                    self.errors.push((ParseError::ExpectedInt, self.span()));
                                    0
                                })
                                as _,
                        );
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if matches!(n.to_lowercase().as_str(), "minstack") =>
                {
                    if self.stack_size.is_some() {
                        self.errors.push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        self.stack_size = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .unwrap_or_else(|| {
                                    self.errors.push((ParseError::ExpectedInt, self.span()));
                                    0
                                })
                                as _,
                        );
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if matches!(n.to_lowercase().as_str(), "minheap") =>
                {
                    if self.heap_size.is_some() {
                        self.errors.push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        self.heap_size = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .unwrap_or_else(|| {
                                    self.errors.push((ParseError::ExpectedInt, self.span()));
                                    0
                                })
                                as _,
                        );
                    }
                },
                Ok(lexer::Token::Name(n)) if matches!(n.to_lowercase().as_str(), "dw") => {
                    pending_labels.iter().for_each(|i| { self.labels.insert(&i, self.dw.len() as _); });
                    pending_labels.clear();
                    self.parse_add_dw();
                },
                Ok(lexer::Token::Name(n)) => {
                    pending_labels.iter().for_each(|i| { self.labels.insert(&i, self.instructions.len() as _); });
                    pending_labels.clear();
                    if let Ok(i) = self.parse_inst(n) {
                        self.instructions.push(i);
                    }
                },
                Ok(lexer::Token::Label(l)) => {
                    if self.labels.contains_key(l) || pending_labels.contains(&l) {
                        self.errors.push((ParseError::ItemRedefined, self.total_span()));
                        self.wait_nl();
                    } else {
                        pending_labels.push(l);
                    }
                },
                Ok(lexer::Token::Newline) => {},
                Ok(_) => self.errors.push((ParseError::UnexpectedToken, self.span())),
                Err(e) => self.errors.push((ParseError::LexError(e), self.span())),
            }
        }

        pending_labels.iter().for_each(|i| { self.labels.insert(&i, self.instructions.len() as _); });

        self.bits = Some(self.bits.unwrap_or(8));
        self.registers = Some(self.registers.unwrap_or(8));
        self.stack_size = Some(self.stack_size.unwrap_or(8));
        self.heap_size = Some(self.heap_size.unwrap_or(16));

        let instructions = core::mem::take(&mut self.instructions);
        let dw = core::mem::take(&mut self.dw);

        let p = ast::Program {
            bits: self.bits.unwrap(),
            registers: self.registers.unwrap(),
            stack_size: self.stack_size.unwrap(),
            heap_size: self.heap_size.unwrap(),

            instructions: instructions
                .iter()
                .map(|i| {
                    ast::Instruction::construct(
                        i.0,
                        i.1.iter().map(|i| self.finalize(i)).collect(),
                    )
                })
                .collect(),
            dw: dw
                .iter()
                .map(|i| self.finalize(i).try_as_immediate().unwrap().0).collect(),
        };

        if self.errors.is_empty() {
            Ok(p)
        } else {
            Err(self.errors)
        }
    }

    fn finalize(&mut self, op: &(RawOperand, Span)) -> ast::Any {
        match &op.0 {
            RawOperand::Register(r) => ast::Any::Register(r.clone()),
            RawOperand::Immediate(i) => ast::Any::Immediate(i.clone()),
            RawOperand::Heap(h) => ast::Any::Immediate(ast::Immediate(self.dw.len() as u128 + h)),
            RawOperand::MacroImm(MacroImm::Bits) => ast::Any::Immediate(ast::Immediate(self.bits.unwrap() as _)),
            RawOperand::MacroImm(MacroImm::MinReg) => {
                ast::Any::Immediate(ast::Immediate(self.registers.unwrap() as _))
            },
            RawOperand::MacroImm(MacroImm::MinStack) => {
                ast::Any::Immediate(ast::Immediate(self.stack_size.unwrap() as _))
            },
            RawOperand::MacroImm(MacroImm::MinHeap | MacroImm::Heap) => {
                ast::Any::Immediate(ast::Immediate(self.heap_size.unwrap() as _))
            },
            RawOperand::MacroImm(_) => todo!(),
            RawOperand::Label(l) => ast::Any::Immediate(ast::Immediate(
                        *self.labels
                        .get(l)
                        .unwrap_or_else(|| { self.errors.push((ParseError::UnknownLabel, op.1.clone())); &0 }),
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError(lexer::LexError),
    InvalidOperand(&'static ast::OperandKind),
    UnexpectedEof,
    ExpectedNewline,
    UnknownMacro,
    ItemRedefined,
    ExpectedInt,
    UnknownInst,
    UnexpectedToken,
    UnknownLabel,
}
