use clap::*;

#[derive(Parser, Debug)]
struct Args {
    urcl: String,

    #[arg(long)]
    triple: Option<String>,

    #[arg(short = 'O', default_value_t = 0, value_parser = clap::value_parser!(u32).range(0..=3))]
    opt: u32,

    #[arg(long, default_value_t = 0)]
    max_heap: usize,
    #[arg(long, default_value_t = 0)]
    max_stack: usize,

    #[arg(long)]
    use_global: bool,
}

fn main() {
    let args = Args::parse();
    let opt: urcl_llvm_backend::OptimizationLevel = unsafe {
        core::mem::transmute(args.opt.clone())
    };

    let src = std::fs::read_to_string(&args.urcl).unwrap();
    let lexer = urcl_frontend::lexer::Lexer::new(&src);
    let parser = urcl_frontend::parser::Parser::new(lexer);
    let program = parser.parse_program(args.max_heap, args.max_stack).unwrap();

    let ctx = urcl_llvm_backend::CodegenContext::new();
    let mut codegen = urcl_llvm_backend::Codegen::new(&ctx, &program);
    let target = urcl_llvm_backend::Codegen::get_machine(args.triple.as_deref(), opt.clone());
    codegen.generate_code(
        &target,
        args.use_global,
    );
    codegen.dump();
    codegen.optimize(&target, opt.clone());
    codegen.dump_opt();
    codegen.write_obj(
        &target,
        urcl_llvm_backend::FileType::Object,
        std::path::Path::new("urcl.o"),
    );

    // println!("{program}");
}
