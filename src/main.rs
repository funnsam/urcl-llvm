fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let lexer = urcl_frontend::lexer::Lexer::new(&src);
    let parser = urcl_frontend::parser::Parser::new(lexer);
    // println!("{}", parser.parse_program().unwrap());
    let program = parser.parse_program().unwrap();

    let ctx = urcl_llvm_backend::CodegenContext::new();
    let mut codegen = urcl_llvm_backend::Codegen::new(&ctx, program);
    codegen.generate_code();
    codegen.dump();
    codegen.write_obj();
}
