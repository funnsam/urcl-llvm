use urcl_ast::OperandKind;

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError(()),
    InvalidOperand(OperandKind),
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
