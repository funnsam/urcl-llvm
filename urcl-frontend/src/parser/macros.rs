use urcl_ast::{Immediate, OperandKind};

use crate::{lexer::Token, parser::util::expect_some_token};

use super::{Parser, error::ParseError, operand::RawOperand, macro_expr::MacroExpr};

macro_rules! expr {
    ($self:tt $expr:tt $($op:tt),*) => {{
        expect_some_token!($self $self.next_token(), dest => {
            $(
                let $op = $self.parse_operand(&OperandKind::Immediate)
                    .map_or_else(
                        |e| {
                            let span = e.1.clone();
                            $self.errors.push(e);
                            (RawOperand::Immediate(Immediate::InstLoc(0)), span)
                        },
                        |o| o,
                    );
            )*
            $self.defines.insert(dest, (
                RawOperand::MacroExpr(Box::new(MacroExpr::$expr($($op),*))),
                $self.total_span(),
            ));
            $self.expect_nl();
        }, $self.wait_nl());
    }};
}

impl Parser<'_> {
    pub(crate) fn parse_macro(&mut self, name: &str) {
        match name {
            _ if name.eq_ignore_ascii_case("define") => expect_some_token!(self self.next_token(),
                t => {
                    let _ = self.parse_operand(&OperandKind::Any).map(|v| {
                        self.defines.insert(t, v);
                    });
                    self.expect_nl();
                }, self.wait_nl()
            ),
            _ if name.eq_ignore_ascii_case("feature") => {
                expect_some_token!(self self.next_token(), Token::Name(n) => {
                    self.features.insert(n);
                    self.expect_nl();
                }, self.wait_nl());
            },

            _ if name.eq_ignore_ascii_case("add") => expr!(self Add a, b),
            _ if name.eq_ignore_ascii_case("sub") => expr!(self Sub a, b),
            _ if name.eq_ignore_ascii_case("mlt") => expr!(self Mlt a, b),
            _ if name.eq_ignore_ascii_case("umlt") => expr!(self Umlt a, b),
            _ if name.eq_ignore_ascii_case("sumlt") => expr!(self SUmlt a, b),
            _ if name.eq_ignore_ascii_case("div") => expr!(self Div a, b),
            _ if name.eq_ignore_ascii_case("sdiv") => expr!(self Sdiv a, b),
            _ if name.eq_ignore_ascii_case("mod") => expr!(self Mod a, b),
            _ if name.eq_ignore_ascii_case("abs") => expr!(self Abs a),
            _ if name.eq_ignore_ascii_case("bsl") => expr!(self Bsl a, b),
            _ if name.eq_ignore_ascii_case("bsr") => expr!(self Bsr a, b),
            _ if name.eq_ignore_ascii_case("bss") => expr!(self Bss a, b),
            _ if name.eq_ignore_ascii_case("or") => expr!(self Or a, b),
            _ if name.eq_ignore_ascii_case("nor") => expr!(self Nor a, b),
            _ if name.eq_ignore_ascii_case("and") => expr!(self And a, b),
            _ if name.eq_ignore_ascii_case("nand") => expr!(self Nand a, b),
            _ if name.eq_ignore_ascii_case("xor") => expr!(self Xor a, b),
            _ if name.eq_ignore_ascii_case("xnor") => expr!(self Xnor a, b),
            _ if name.eq_ignore_ascii_case("not") => expr!(self Not a),

            _ => {
                self.error(ParseError::UnknownMacro);
                self.wait_nl();
            },
        }
    }
}
