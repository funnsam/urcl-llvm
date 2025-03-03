use urcl_ast::OperandKind;

use crate::{lexer::Token, parser::util::expect_some_token};

use super::{Parser, error::ParseError};

impl Parser<'_> {
    fn unknown_macro(&mut self) {
        self.wait_nl();
        self.total_span_error(ParseError::UnknownMacro);
    }

    pub(crate) fn parse_macro(&mut self, name: &str) {
        match name {
            _ if name.eq_ignore_ascii_case("define") => {
                expect_some_token!(self self.next_token(), t => {
                    match self.parse_operand(OperandKind::Any) {
                        Ok(v) => _ = self.defines.insert(t, v),
                        Err(e) => self.error_at(e.0, e.1),
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
                            self.defines.insert(dest, (mx.0.into(), mx.1));
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
