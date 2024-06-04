fn main() {
    let src = /* "\
bits >= 8
minreg 2
minheap 0
minstack 0
run rom

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
" */ "\
ADD R1/*  // */ $2 0o23
OUT %2 'F'//*/
LSTR #2 @SMSB    0x89234
";
    let lexer = urcl_frontend::lexer::Lexer::new(src);

    for t in lexer {
        println!("{t:?}");
    }
}
