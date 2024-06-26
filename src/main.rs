use clap::*;

#[derive(Parser, Debug)]
struct Args {
    urcl: String,

    #[arg(long)]
    triple: Option<String>,
    #[arg(long)]
    features: Option<String>,

    #[arg(short = 'O', default_value_t = 0, value_parser = clap::value_parser!(u32).range(0..=3))]
    opt: u32,

    #[arg(long, default_value_t = 0)]
    max_ram: usize,

    #[arg(long)]
    use_global: bool,

    #[arg(short, default_value = "urcl.o")]
    output_file: String,
    #[arg(long)]
    emit_assembly: bool,

    #[arg(long, default_value_t = 32, value_parser = float_ty)]
    float: usize,
}

fn float_ty(s: &str) -> Result<usize, String> {
    let v = match s.chars().next() {
        Some('f') => s[1..].parse::<usize>(),
        _ => s.parse(),
    }
    .map_err(|e| e.to_string())?;

    if matches!(v, 16 | 32 | 64 | 128) {
        Ok(v)
    } else {
        Err("invalid float bit width, can only be 16, 32, 64 or 128".to_string())
    }
}

fn main() {
    let args = Args::parse();
    let opt: urcl_llvm_backend::OptimizationLevel = unsafe { core::mem::transmute(args.opt) };

    let src = std::fs::read_to_string(&args.urcl).unwrap();
    let lexer = urcl_frontend::lexer::Lexer::new(&src, args.float);
    let parser = urcl_frontend::parser::Parser::new(lexer);
    let program = parser.parse_program(args.max_ram).unwrap();

    let ctx = urcl_llvm_backend::CodegenContext::new();
    let mut codegen = urcl_llvm_backend::Codegen::new(&ctx, &program);
    let target = urcl_llvm_backend::Codegen::get_machine(
        args.triple.as_deref(),
        args.features.as_deref(),
        opt.clone(),
    );
    codegen.generate_code(
        &target,
        &urcl_llvm_backend::CodegenOptions {
            use_global: args.use_global,
            float_type: args.float,
        },
    );
    codegen.dump();
    codegen.optimize(&target, opt);
    codegen.dump_opt();
    codegen.write_obj(
        &target,
        if args.emit_assembly {
            urcl_llvm_backend::FileType::Assembly
        } else {
            urcl_llvm_backend::FileType::Object
        },
        std::path::Path::new(&args.output_file),
    );

    // println!("{program}");
}
