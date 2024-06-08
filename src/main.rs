use clap::*;

#[derive(Parser, Debug)]
struct Args {
    urcl: String,

    #[arg(long)]
    triple: Option<String>,
}

fn main() {
    let args = Args::parse();

    let src = std::fs::read_to_string(&args.urcl).unwrap();
    let lexer = urcl_frontend::lexer::Lexer::new(&src);
    let parser = urcl_frontend::parser::Parser::new(lexer);
    let program = parser.parse_program().unwrap();

    let ctx = urcl_llvm_backend::CodegenContext::new();
    let mut codegen = urcl_llvm_backend::Codegen::new(&ctx, &program);
    codegen.generate_code();
    codegen.dump();
    codegen.write_obj(
        args.triple.as_deref(),
        urcl_llvm_backend::FileType::Object,
        std::path::Path::new("urcl.o"),
    );
    codegen.dump_opt();

    // println!("{program}");
}
