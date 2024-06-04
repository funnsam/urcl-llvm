pub mod lexer;
pub mod parser;
pub mod ast;

/// Codespan measured in bytes
pub type Span = core::ops::Range<usize>;
