fn main() {
    let path = std::env::args().nth(1).unwrap();
    let path_ref = std::path::Path::new(&path);
    let src = std::fs::read_to_string(path_ref).unwrap();
    let lexer = urcl_frontend::lexer::Lexer::new(&src);
    let parser = urcl_frontend::parser::Parser::new(lexer);
    let program = parser.parse_program().unwrap();

    // let mut san_path = std::path::PathBuf::from(path);
    // san_path.set_extension("san.urcl");
    // std::fs::write(san_path, program.to_string()).unwrap();

    let ctx = urcl_llvm_backend::CodegenContext::new();
    let mut codegen = urcl_llvm_backend::Codegen::new(&ctx, program);
    codegen.generate_code();
    codegen.dump();
    codegen.write_obj(
        urcl_llvm_backend::FileType::Object,
        std::path::Path::new("urcl.o"),
    );
    codegen.write_obj(
        urcl_llvm_backend::FileType::Assembly,
        std::path::Path::new("urcl.asm"),
    );
}
