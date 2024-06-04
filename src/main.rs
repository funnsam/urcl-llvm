fn main() {
    let src = "\
.loop
lod r2 r1
bge .loop2 r2 1
hlt
.loop2
out %text r2
add r1 r1 1
bge .loop r0 r0

.str
dw [\"Hello, World!\" 0]
";
    let lexer = urcl_frontend::lexer::Lexer::new(src);

    for t in lexer {
        println!("{t:?}");
    }
}
