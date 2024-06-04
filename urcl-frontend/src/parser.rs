use crate::*;

pub struct Parser<'a> {
    lex: lexer::Lexer<'a>,
    peeked: Option<lexer::LexResult<'a>>,

    bits: Option<usize>,
    registers: Option<usize>,
    stack_size: Option<usize>,
    heap_size: Option<usize>,

    instructions: Vec<(ast::Instruction, Span)>,
}

impl<'a> Parser<'a> {
    pub fn new(lex: lexer::Lexer<'a>) -> Self {
        Self {
            lex,
            peeked: None,

            bits: None,
            registers: None,
            stack_size: None,
            heap_size: None,

            instructions: Vec::new(),
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

    fn parse_inst(&mut self, n: &str, s: Span) -> Result<(ast::Instruction, Span), (ParseError, Span)> {
        let i = ast::Instruction::properties(n).unwrap(); // TODO: error
        let mut oprs = Vec::with_capacity(i.operands.len());
        for op in i.operands.iter() {
            self.next_token();
            oprs.push(ast::Any::Register(ast::Register(0)));
        }

        Ok((ast::Instruction::construct(n, oprs), s.start..0))
    }

    pub fn parse_program(mut self) -> ParseResult {
        while let Some(t) = self.next_token() {
            match t {
                (_, Ok(lexer::Token::Newline)) => {},
                (s, Ok(lexer::Token::Name(n))) => {
                    let i = self.parse_inst(n, s)?;
                    self.instructions.push(i);
                },
                (_, Ok(t)) => println!("{t:?}"),
                (s, Err(e)) => return Err((ParseError::LexError(e), s)),
            }
        }

        Ok(ast::Program {
            bits: self.bits.unwrap_or(8),
            registers: self.registers.unwrap_or(8),
            stack_size: self.registers.unwrap_or(8),
            heap_size: self.heap_size.unwrap_or(16),

            instructions: self.instructions.into_iter().map(|i| i.0).collect(),
        })
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError(lexer::LexError),
}

pub type ParseResult = Result<ast::Program, (ParseError, Span)>;
