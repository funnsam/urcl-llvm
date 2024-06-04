fn main() {
    let src = "\
@define   urcl      asdf // hjkl
add r1 $2 3 /*
            asdfaaas
            addwqs
            */
rsh r1 R2";
    let lexer = urcl_frontend::lexer::Lexer::new(src);

    for t in lexer {
        println!("{t:?}");
    }
}
