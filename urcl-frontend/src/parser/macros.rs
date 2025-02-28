use urcl_ast::OperandKind;

use crate::{lexer::Token, parser::util::expect_some_token};

use super::{Parser, error::ParseError, macro_expr::MacroExpr};

macro_rules! expr {
    ($self:tt $expr:tt $($op:tt),*) => {{
        let dest = $self.
        $(
            let $op = 0;
        )*
        MacroExpr::$expr($($op),*)
    }};
}

impl Parser<'_> {
    pub(crate) fn parse_macro(&mut self, name: &str) {
        match name {
            _ if name.eq_ignore_ascii_case("define") => match self.next_token() {
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
            },
            _ if name.eq_ignore_ascii_case("feature") => {
                expect_some_token!(self self.next_token(), Token::Name(n) => {
                    self.features.insert(n);
                });
                self.expect_nl();
            },
            _ if name.eq_ignore_ascii_case("add") => expr!(self Add a, b),
            _ => {
                self.error(ParseError::UnknownMacro);
                self.wait_nl();
            },
        }
    }
}
