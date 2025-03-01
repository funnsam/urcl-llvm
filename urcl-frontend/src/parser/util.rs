use dashu::Integer;
use logos::Span;
use num_traits::ToPrimitive;

use crate::lexer::{LexResult, Token};

use super::{ParseError, Parser};

macro_rules! expect_some_token {
    ($self:tt $t:expr, $($rest:tt)*) => {{
        if let Some(t) = $t {
            $crate::parser::util::expect_token!($self t, $($rest)*);
        } else {
            $self.error(ParseError::UnexpectedEof);
        }
    }};
}

macro_rules! expect_token {
    ($self:tt $t:expr, $pat:pat => $stmt:expr , $else:expr) => {{
        match $t {
            Ok($pat) => $stmt,
            #[allow(unreachable_patterns)]
            Ok(_) => { $self.error(ParseError::UnexpectedToken); $else },
            Err(e) => { $self.error(ParseError::LexError(e)); $else },
        }
    }};
}

pub(crate) use {expect_some_token, expect_token};

impl<'a> Parser<'a> {
    pub(crate) fn next_token(&mut self) -> Option<LexResult<'a>> {
        if self.peeked.is_some() {
            core::mem::take(&mut self.peeked)
        } else {
            self.lex.next()
        }
    }

    pub(crate) fn peek_next(&mut self) -> Option<LexResult<'a>> {
        if self.peeked.is_none() {
            self.peeked = self.lex.next();
        }

        self.peeked.clone()
    }

    pub(crate) fn span(&self) -> Span { self.lex.span() }
    pub(crate) fn total_span(&self) -> Span { self.start..self.lex.span().end }

    pub(crate) fn error(&self, err: ParseError) {
        self.error_at(err, self.span());
    }

    pub(crate) fn total_span_error(&self, err: ParseError) {
        self.error_at(err, self.total_span());
    }

    pub(crate) fn error_at(&self, err: ParseError, span: Span) {
        self.errors.borrow_mut().push((err, span));
    }

    pub(crate) fn expect_nl(&mut self) {
        if !matches!(self.peek_next(), Some(Ok(Token::Newline)) | None) {
            self.error(ParseError::ExpectedNewline);
            self.wait_nl();
        }
    }

    pub(crate) fn wait_nl(&mut self) {
        while let Some(t) = self.next_token() {
            match t {
                Ok(Token::Newline) => break,
                Ok(_) => {},
                Err(e) => self.error(ParseError::LexError(e)),
            }
        }
    }

    pub(crate) fn parse_int(&mut self) -> Integer {
        self.next_token()
            .and_then(|t| t.ok())
            .and_then(|t| t.try_as_integer())
            .unwrap_or_else(|| {
                self.error(ParseError::ExpectedInt);
                Integer::ZERO
            })
    }

    pub(crate) fn opt_unwrap_or_err_default<T: Default>(
        &mut self,
        opt: Option<T>,
        err: ParseError,
    ) -> T {
        opt.unwrap_or_else(|| {
            self.error(err);
            T::default()
        })
    }

    pub(crate) fn parse_u64(&mut self) -> u64 {
        let int = self.parse_int().to_u64();
        self.opt_unwrap_or_err_default(int, ParseError::InvalidValue)
    }

    pub(crate) fn parse_u32(&mut self) -> u32 {
        let int = self.parse_int().to_u32();
        self.opt_unwrap_or_err_default(int, ParseError::InvalidValue)
    }

    pub(crate) fn parse_u16(&mut self) -> u16 {
        let int = self.parse_int().to_u16();
        self.opt_unwrap_or_err_default(int, ParseError::InvalidValue)
    }

    pub(crate) fn bits(&self) -> u32 { self.bits.unwrap_or(8) }
    pub(crate) fn registers(&self) -> u16 { self.registers.unwrap_or(8) }
    pub(crate) fn min_stack(&self) -> u64 { self.min_stack.unwrap_or(8) }
    pub(crate) fn min_heap(&self) -> u64 { self.min_heap.unwrap_or(16) }

    pub(crate) fn feature(&self, f: &str) -> bool { self.features.contains(f) }
}
