pub mod lexer;
pub mod parser;

/// Codespan measured in bytes
pub type Span = core::ops::Range<usize>;
