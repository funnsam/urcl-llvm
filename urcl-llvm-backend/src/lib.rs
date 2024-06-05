use urcl_frontend::ast;
use inkwell::*;

pub struct CodegenContext(context::Context);

impl CodegenContext {
    pub fn new() -> Self {
        Self(context::Context::create())
    }
}

pub struct Codegen<'a> {
    context: &'a context::Context,
    builder: builder::Builder<'a>,
    module : module::Module<'a>,

    program: ast::Program,
}

impl<'a> Codegen<'a> {
    pub fn new(context: &'a CodegenContext, program: ast::Program) -> Self {
        Self {
            context: &context.0,
            builder: context.0.create_builder(),
            module: context.0.create_module("urcl"),

            program,
        }
    }

    pub fn generate_code(&mut self) {
        let word_t = self.context.custom_width_int_type(self.program.bits as u32);
        let mach_t = self.context.custom_width_int_type(32);
        let void = self.context.void_type();

        let main = self.module.add_function("urcl_main", word_t.fn_type(&[], false), None);
        let port_out = self.module.add_function("urcl_out", void.fn_type(&[word_t.into(); 2], false), Some(module::Linkage::External));

        let entry = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry);

        let mut registers = Vec::with_capacity(self.program.registers - 1);

        for r in 1..=self.program.registers {
            registers.push(self.builder.build_alloca(word_t, &format!("r{r}")).unwrap());
        }

        let ram = word_t.array_type((self.program.stack_size + self.program.heap_size + self.program.dw.len()) as u32);
        let ram = self.builder.build_alloca(ram, "ram").unwrap();
        let dw = word_t.const_array(self.program.dw.iter().map(|i| word_t.const_int(*i as _, false)).collect::<Vec<_>>().as_slice());
        self.builder.build_store(ram, dw).unwrap();

        let inst_cnt = self.builder.build_alloca(mach_t, "inst_cnt").unwrap();
        self.builder.build_store(inst_cnt, mach_t.const_zero()).unwrap();

        let get_reg = |r: &_| match r {
            ast::Register::General(0) => word_t.const_zero(),
            ast::Register::General(g) => {
                self.builder.build_load(word_t, registers[*g as usize], "reg_load").unwrap().into_int_value()
            },
            ast::Register::Pc => todo!(),
            ast::Register::Sp => todo!(),
        };

        let set_reg = |r: &_, v: values::IntValue| match r {
            ast::Register::General(0) => {},
            ast::Register::General(g) => {
                self.builder.build_store(registers[*g as usize], v).unwrap();
            },
            ast::Register::Pc => todo!(),
            ast::Register::Sp => todo!(),
        };

        let get_any = |v: &_| match v {
            ast::Any::Register(r) => get_reg(r),
            ast::Any::Immediate(i) => word_t.const_int(i.0 as u64, false),
        };

        let inst_bb = (0..=self.program.instructions.len())
            .map(|i| self.context.append_basic_block(main, &format!("inst_{i}")))
            .collect::<Vec<basic_block::BasicBlock>>();

        self.builder.build_unconditional_branch(inst_bb[0]).unwrap();

        for (pc, i) in self.program.instructions.iter().enumerate() {
            self.builder.position_at_end(inst_bb[pc]);

            let inst_cnt_v = self.builder.build_load(mach_t, inst_cnt, "inst_cnt_v").unwrap().into_int_value();
            let update = self.builder.build_int_add(inst_cnt_v, mach_t.const_int(1, false), "inst_cnt_v_update").unwrap();
            self.builder.build_store(inst_cnt, update).unwrap();

            match i {
                ast::Instruction::Imm(d, v) | ast::Instruction::Mov(d, v) => {
                    let v = get_any(v);
                    set_reg(d, v);
                    self.builder.build_unconditional_branch(inst_bb[pc + 1]).unwrap();
                },
                ast::Instruction::Out(p, v) => {
                    let p = get_any(p);
                    let v = get_any(v);
                    self.builder.build_call(port_out, &[p.into(), v.into()], "port_out").unwrap();
                    self.builder.build_unconditional_branch(inst_bb[pc + 1]).unwrap();
                }
                _ => todo!("{i}"),
            }
        }

        self.builder.position_at_end(*inst_bb.last().unwrap());
        let inst_cnt_v = self.builder.build_load(mach_t, inst_cnt, "inst_cnt_v").unwrap();
        self.builder.build_return(Some(&inst_cnt_v)).unwrap();
    }

    pub fn dump(&self) {
        self.module.print_to_stderr();
    }

    pub fn write_obj(&self) {
        targets::Target::initialize_all(&targets::InitializationConfig::default());

        let triple = targets::TargetMachine::get_default_triple();
        let target = targets::Target::from_triple(&triple).unwrap();
        let cpu = targets::TargetMachine::get_host_cpu_name();
        let features = targets::TargetMachine::get_host_cpu_features();
        let reloc = targets::RelocMode::Default;
        let model = targets::CodeModel::Default;
        let opt = OptimizationLevel::Default;

        let target_machine = target.create_target_machine(
            &triple,
            cpu.to_str().unwrap(),
            features.to_str().unwrap(),
            opt,
            reloc,
            model
        ).unwrap();

        target_machine.write_to_file(&self.module, targets::FileType::Object, &std::path::Path::new("urcl.obj")).unwrap();
    }
}
