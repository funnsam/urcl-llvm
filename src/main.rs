fn main() {
    let src = "\
bits >= 8
minreg 2
minheap 0
minstack 0
// run rom

.loop
lod r2 r1
bge .loop2 r2 1
hlt
.loop2
// out %text r2
out 1 r2
add r1 r1 1
bge .loop r0 r0

.str
dw [\"Hello, World!\" 0]
";
    let lexer = urcl_frontend::lexer::Lexer::new(src);
    let parser = urcl_frontend::parser::Parser::new(lexer);
    println!("{:?}", parser.parse_program());
}
