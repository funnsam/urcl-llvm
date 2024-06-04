use std::{collections::HashMap, str::FromStr};

use crate::*;

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
}

enum RawOperand<'a> {
    Register(ast::Register),
    Immediate(ast::Immediate),
    Heap(u128),
    MacroImm(MacroImm),
    Label(&'a str),
}

#[derive(strum::EnumString)]
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
    ) -> Result<(&'a str, Vec<(RawOperand<'a>, Span)>, Span), (ParseError, Span)> {
        let i = ast::Instruction::properties(n).ok_or((ParseError::UnknownInst, self.span()))?; // TODO: error
        let mut oprs = Vec::with_capacity(i.operands.len());
        for op in i.operands.iter() {
            let opr = self.parse_operand(op)?;
            oprs.push(opr);
        }

        if !matches!(self.peek_next(), Some(Ok(lexer::Token::Newline)) | None) {
            return Err((ParseError::ExpectedNewline, self.span()));
        }

        Ok((n, oprs, self.total_span()))
    }

    fn parse_add_dw(&mut self) -> Result<(), (ParseError, Span)> {
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
                    let w = self.parse_operand_with(Some(t), &ast::OperandKind::Immediate)?;
                    self.dw.push(w);
                },
                Err(e) => return Err((ParseError::LexError(e), self.span())),
            }
        }

        Ok(())
    }

    pub fn parse_program(mut self) -> ParseResult {
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
                        return Err((ParseError::ItemRedefined, self.total_span()));
                    } else {
                        self.bits = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .ok_or((ParseError::ExpectedInt, self.span()))?
                                as _,
                        );
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if matches!(n.to_lowercase().as_str(), "minreg") =>
                {
                    if self.registers.is_some() {
                        return Err((ParseError::ItemRedefined, self.total_span()));
                    } else {
                        self.registers = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .ok_or((ParseError::ExpectedInt, self.span()))?
                                as _,
                        );
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if matches!(n.to_lowercase().as_str(), "minstack") =>
                {
                    if self.stack_size.is_some() {
                        return Err((ParseError::ItemRedefined, self.total_span()));
                    } else {
                        self.stack_size = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .ok_or((ParseError::ExpectedInt, self.span()))?
                                as _,
                        );
                    }
                },
                Ok(lexer::Token::Name(n) | lexer::Token::Macro(n))
                    if matches!(n.to_lowercase().as_str(), "minheap") =>
                {
                    if self.heap_size.is_some() {
                        return Err((ParseError::ItemRedefined, self.total_span()));
                    } else {
                        self.heap_size = Some(
                            self.next_token()
                                .and_then(|t| t.ok())
                                .and_then(|t| t.try_as_int())
                                .ok_or((ParseError::ExpectedInt, self.span()))?
                                as _,
                        );
                    }
                },
                Ok(lexer::Token::Name(n)) if matches!(n.to_lowercase().as_str(), "dw") => {
                    self.parse_add_dw()?;
                },
                Ok(lexer::Token::Name(n)) => {
                    let i = self.parse_inst(n)?;
                    self.instructions.push(i);
                },
                Ok(lexer::Token::Label(l)) => {
                    if self
                        .labels
                        .insert(l, self.instructions.len() as _)
                        .is_some()
                    {
                        return Err((ParseError::ItemRedefined, self.total_span()));
                    }
                },
                Ok(lexer::Token::Newline) => {},
                Ok(t) => todo!("{t:?}"),
                Err(e) => return Err((ParseError::LexError(e), self.total_span())),
            }
        }

        let bits = self.bits.unwrap_or(8);
        let registers = self.registers.unwrap_or(8);
        let stack_size = self.stack_size.unwrap_or(8);
        let heap_size = self.heap_size.unwrap_or(16);
        let dw_size = self.dw.len();

        Ok(ast::Program {
            bits,
            registers,
            stack_size,
            heap_size,

            instructions: self
                .instructions
                .into_iter()
                .map(|i| {
                    Ok(ast::Instruction::construct(
                        i.0,
                        i.1.iter()
                            .map(|i| {
                                finalize(bits, registers, stack_size, heap_size, &self.labels, dw_size, i)
                            })
                            .try_collect()?,
                    ))
                })
                .try_collect()?,
            dw: self
                .dw
                .into_iter()
                .map(|i| {
                    finalize(bits, registers, stack_size, heap_size, &self.labels, dw_size, &i)
                        .map(|i| i.try_as_immediate().unwrap().0)
                })
                .try_collect()?,
        })
    }
}

fn finalize<'a>(
    bits: usize,
    registers: usize,
    stack_size: usize,
    heap_size: usize,
    labels: &HashMap<&'a str, u128>,
    dw_size: usize,
    op: &(RawOperand, Span),
) -> Result<ast::Any, (ParseError, Span)> {
    match &op.0 {
        RawOperand::Register(r) => Ok(ast::Any::Register(r.clone())),
        RawOperand::Immediate(i) => Ok(ast::Any::Immediate(i.clone())),
        RawOperand::Heap(h) => Ok(ast::Any::Immediate(ast::Immediate(dw_size as u128 + h))),
        RawOperand::MacroImm(MacroImm::Bits) => Ok(ast::Any::Immediate(ast::Immediate(bits as _))),
        RawOperand::MacroImm(MacroImm::MinReg) => {
            Ok(ast::Any::Immediate(ast::Immediate(registers as _)))
        },
        RawOperand::MacroImm(MacroImm::MinStack) => {
            Ok(ast::Any::Immediate(ast::Immediate(stack_size as _)))
        },
        RawOperand::MacroImm(MacroImm::MinHeap | MacroImm::Heap) => {
            Ok(ast::Any::Immediate(ast::Immediate(heap_size as _)))
        },
        RawOperand::MacroImm(_) => todo!(),
        RawOperand::Label(l) => Ok(ast::Any::Immediate(ast::Immediate(
            *labels
                .get(l)
                .ok_or((ParseError::UnknownLabel, op.1.clone()))?,
        ))),
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

pub type ParseResult = Result<ast::Program, (ParseError, Span)>;
