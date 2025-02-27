use urcl_ast::OperandKind;

use super::{error::ParseError, Parser};

impl<'a> Parser<'a> {
    pub(crate) fn parse_macro(&mut self, name: &str) {
        match name {
            _ if name.eq_ignore_ascii_case("define") => {
                match self.next_token() {
                    Some(Ok(t)) => {
                        let _ = self.parse_operand(&OperandKind::Any).map(|v| {
                            self.defines.insert(t, v);
                        });
                        self.expect_nl();
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
            _ if name.eq_ignore_ascii_case("port_v2") => {
                self.expect_nl();
                self.port_v2 = true;
            },
            _ => {
                self.wait_nl();
                self.error(ParseError::UnknownMacro);
            },
        }
    }
}
