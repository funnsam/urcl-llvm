use urcl_ast::OperandKind;

use crate::{lexer::Token, parser::util::expect_some_token};

use super::{Parser, error::ParseError, operand::RawOperand};

impl Parser<'_> {
    fn unknown_macro(&mut self) {
        self.wait_nl();
        self.total_span_error(ParseError::UnknownMacro);
    }

    pub(crate) fn parse_macro(&mut self, name: &str) {
        match name {
            _ if name.eq_ignore_ascii_case("define") => {
                expect_some_token!(self self.next_token(), t => {
                    match self.parse_operand(&OperandKind::Any) {
                        Ok(v) => _ = self.defines.insert(t, v),
                        Err(e) => self.errors.push(e),
                    }
                    self.expect_nl();
                }, self.wait_nl());
            },
            _ if name.eq_ignore_ascii_case("feature") => {
                expect_some_token!(self self.next_token(), Token::Name(n) => {
                    self.features.insert(n);
                    self.expect_nl();
                }, self.wait_nl());
            },
            _ => {
                if let Some(Ok(dest)) = self.peek_next() {
                    if dest != Token::Newline {
                        self.next_token();

                        if let Some(mx) = self.parse_macro_expr(name) {
                            let raw = RawOperand::MacroExpr(Box::new(mx.0));
                            self.defines.insert(dest, (raw, mx.1));
                        } else {
                            self.unknown_macro();
                        }
                    } else {
                        self.unknown_macro();
                    }
                } else {
                    self.unknown_macro();
                }
            },
        }
    }
}
