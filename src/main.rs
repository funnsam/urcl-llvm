fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let lexer = urcl_frontend::lexer::Lexer::new(&src);
    let parser = urcl_frontend::parser::Parser::new(lexer);
    println!("{}", parser.parse_program().unwrap());
}
