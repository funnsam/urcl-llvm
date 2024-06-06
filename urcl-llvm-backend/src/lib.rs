use inkwell::*;
use urcl_frontend::ast;

pub use targets::FileType;
pub struct CodegenContext(context::Context);

impl Default for CodegenContext {
    fn default() -> Self { Self::new() }
}

impl CodegenContext {
    pub fn new() -> Self { Self(context::Context::create()) }
}

pub struct Codegen<'a> {
    context: &'a context::Context,
    builder: builder::Builder<'a>,
    module: module::Module<'a>,

    program: &'a ast::Program,
}

impl<'a> Codegen<'a> {
    pub fn new(context: &'a CodegenContext, program: &'a ast::Program) -> Self {
        Self {
            context: &context.0,
            builder: context.0.create_builder(),
            module: context.0.create_module("urcl"),

            program,
        }
    }

    pub fn generate_code(&mut self) {
        let word_t = self.context.custom_width_int_type(self.program.bits as u32);
        let mach_t = self.context.custom_width_int_type(64);
        let void = self.context.void_type();

        let word_1 = word_t.const_int(1, false);
        let word_0 = word_t.const_zero();

        let main = self
            .module
            .add_function("urcl_main", mach_t.fn_type(&[], false), None);
        let port_out = self.module.add_function(
            "urcl_out",
            void.fn_type(&[word_t.into(); 2], false),
            Some(module::Linkage::External),
        );

        let entry = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry);

        let registers = (1..=self.program.registers)
            .map(|r| self.builder.build_alloca(word_t, &format!("r{r}")).unwrap())
            .collect::<Vec<_>>();

        let ram_size = self.program.stack_size + self.program.heap_size + self.program.dw.len();
        let ram = word_t.array_type(ram_size as _);
        let ram = self.builder.build_alloca(ram, "ram").unwrap();
        let dw = word_t.const_array(
            self.program
                .dw
                .iter()
                .map(|i| word_t.const_int(*i as _, false))
                .collect::<Vec<_>>()
                .as_slice(),
        );
        self.builder.build_store(ram, dw).unwrap();

        let stack_ptr = self.builder.build_alloca(word_t, "sp").unwrap();
        self.builder.build_store(stack_ptr, word_t.const_int(ram_size as _, false)).unwrap();

        let inst_cnt = self.builder.build_alloca(mach_t, "inst_cnt").unwrap();
        self.builder
            .build_store(inst_cnt, mach_t.const_zero())
            .unwrap();

        let inst_bb = (0..=self.program.instructions.len())
            .map(|i| self.context.append_basic_block(main, &format!("inst_{i}")))
            .collect::<Vec<basic_block::BasicBlock>>();

        let ret = || {
            let inst_cnt_v = self
                .builder
                .build_load(mach_t, inst_cnt, "inst_cnt_v")
                .unwrap();
            self.builder.build_return(Some(&inst_cnt_v)).unwrap();
        };

        self.builder.build_unconditional_branch(inst_bb[0]).unwrap();

        for (pc, i) in self.program.instructions.iter().enumerate() {
            let gen_big_switch_table = |v| {
                self.builder
                    .build_switch(
                        v,
                        *inst_bb.last().unwrap(),
                        &inst_bb
                            .iter()
                            .enumerate()
                            .map(|(i, b)| (word_t.const_int(i as _, false), *b))
                            .collect::<Vec<_>>(),
                    )
                    .unwrap();
            };

            let get_reg = |r: &_| match r {
                ast::Register::General(0) => word_0,
                ast::Register::General(g) => self
                    .builder
                    .build_load(word_t, registers[*g as usize - 1], "reg_load")
                    .unwrap()
                    .into_int_value(),
                ast::Register::Pc => word_t.const_int(pc as _, false),
                ast::Register::Sp => self
                    .builder
                    .build_load(word_t, stack_ptr, "reg_load")
                    .unwrap()
                    .into_int_value(),
            };

            let set_reg = |r: &_, v| match r {
                ast::Register::General(0) => true,
                ast::Register::General(g) => {
                    self.builder
                        .build_store(registers[*g as usize - 1], v)
                        .unwrap();
                    true
                },
                ast::Register::Pc => {
                    gen_big_switch_table(v);
                    false
                },
                ast::Register::Sp => {
                    self.builder.build_store(stack_ptr, v).unwrap();
                    true
                },
            };

            let get_any = |v: &_| match v {
                ast::Any::Register(r) => get_reg(r),
                ast::Any::Immediate(i) => word_t.const_int(i.0 as u64, false),
            };

            let read_ram = |a| unsafe {
                let p = self
                    .builder
                    .build_gep(word_t, ram, &[a], "ram_gep")
                    .unwrap();
                self.builder
                    .build_load(word_t, p, "read_ram")
                    .unwrap()
                    .into_int_value()
            };

            let write_ram = |a, v: values::IntValue| unsafe {
                let p = self
                    .builder
                    .build_gep(word_t, ram, &[a], "ram_gep")
                    .unwrap();
                self.builder.build_store(p, v).unwrap();
            };

            let cond_br = |c, d: &_| match d {
                ast::Any::Immediate(d) => {
                    self.builder
                        .build_conditional_branch(c, inst_bb[d.0 as usize], inst_bb[pc + 1])
                        .unwrap();
                },
                ast::Any::Register(d) => {
                    let e = self.context.append_basic_block(main, "big_switch_table");
                    self.builder
                        .build_conditional_branch(c, e, inst_bb[pc + 1])
                        .unwrap();
                    self.builder.position_at_end(e);
                    gen_big_switch_table(get_reg(d));
                },
            };

            let uncond_br = |d: &_| match d {
                ast::Any::Immediate(d) => {
                    self.builder
                        .build_unconditional_branch(inst_bb[d.0 as usize])
                        .unwrap();
                },
                ast::Any::Register(d) => gen_big_switch_table(get_reg(d)),
            };

            self.builder.position_at_end(inst_bb[pc]);

            let inst_cnt_v = self
                .builder
                .build_load(mach_t, inst_cnt, "inst_cnt_v")
                .unwrap()
                .into_int_value();
            let update = self
                .builder
                .build_int_add(inst_cnt_v, mach_t.const_int(1, false), "inst_cnt_v_update")
                .unwrap();
            self.builder.build_store(inst_cnt, update).unwrap();

            let push = |v| {
                let sp_old = self.builder.build_load(word_t, stack_ptr, "stack_ptr_pre_dec").unwrap().into_int_value();
                let sp_new = self.builder.build_int_sub(sp_old, word_1, "stack_ptr_dec").unwrap();
                self.builder.build_store(stack_ptr, sp_new).unwrap();

                write_ram(sp_new, v);
            };

            let pop = || {
                let sp_old = self.builder.build_load(word_t, stack_ptr, "stack_ptr_pre_inc").unwrap().into_int_value();
                let sp_new = self.builder.build_int_add(sp_old, word_1, "stack_ptr_inc").unwrap();
                self.builder.build_store(stack_ptr, sp_new).unwrap();

                read_ram(sp_old)
            };

            macro_rules! gen {
                (2op $d: tt $a: tt $b: tt $gen: expr) => {{
                    let a = get_any($a);
                    let b = get_any($b);
                    if set_reg($d, $gen(a, b)) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (1op $d: tt $a: tt $gen: expr) => {{
                    let a = get_any($a);
                    if set_reg($d, $gen(a)) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (none $d: tt $gen: expr) => {{
                    if set_reg($d, $gen()) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (2bc $d: tt $a: tt $b: tt $cond: expr) => {{
                    let a = get_any($a);
                    let b = get_any($b);
                    let c = $cond(a, b);
                    cond_br(c, $d);
                }};
                (1bc $d: tt $a: tt $cond: expr) => {{
                    let a = get_any($a);
                    let c = $cond(a);
                    cond_br(c, $d);
                }};
            }

            match i {
                ast::Instruction::Add(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_add(a, b, "add").unwrap()
                }),
                ast::Instruction::Rsh(d, a) => gen!(1op d a |a| {
                        self.builder.build_right_shift(a, word_1, false, "rsh_urcl").unwrap()
                }),
                ast::Instruction::Lod(d, a) => gen!(1op d a read_ram),
                ast::Instruction::Str(a, v) => {
                    let a = get_any(a);
                    let v = get_any(v);
                    write_ram(a, v);
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                ast::Instruction::Bge(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGE, a, b, "urcl_bge_cmp").unwrap()
                }),
                ast::Instruction::Nor(d, a, b) => gen!(2op d a b |a, b| {
                    let c = self.builder.build_or(a, b, "nor_urcl_1").unwrap();
                    self.builder.build_not(c, "nor_urcl_2").unwrap()
                }),
                ast::Instruction::Sub(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_sub(a, b, "sub_urcl").unwrap()
                }),
                ast::Instruction::Imm(d, v) | ast::Instruction::Mov(d, v) => gen!(1op d v |v| v),
                ast::Instruction::Hlt() => {
                    ret();
                },
                ast::Instruction::Jmp(i) => uncond_br(i),
                ast::Instruction::Lsh(d, a) => gen!(1op d a |a| {
                    self.builder.build_left_shift(a, word_1, "lsh_urcl").unwrap()
                }),
                ast::Instruction::Inc(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_add(a, word_1, "inc_urcl").unwrap()
                }),
                ast::Instruction::Dec(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_sub(a, word_1, "dec_urcl").unwrap()
                }),
                ast::Instruction::Neg(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_neg(a, "neg_urcl").unwrap()
                }),
                ast::Instruction::And(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_and(a, b, "and_urcl").unwrap()
                }),
                ast::Instruction::Or(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_or(a, b, "or_urcl").unwrap()
                }),
                ast::Instruction::Not(d, a) => gen!(1op d a |a| {
                    self.builder.build_not(a, "not_urcl").unwrap()
                }),
                ast::Instruction::Xnor(d, a, b) => gen!(2op d a b |a, b| {
                    let c = self.builder.build_xor(a, b, "xnor_urcl_1").unwrap();
                    self.builder.build_not(c, "xnor_urcl_2").unwrap()
                }),
                ast::Instruction::Xor(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_xor(a, b, "xor_urcl").unwrap()
                }),
                ast::Instruction::Nand(d, a, b) => gen!(2op d a b |a, b| {
                    let c = self.builder.build_and(a, b, "nand_urcl_1").unwrap();
                    self.builder.build_not(c, "nand_urcl_2").unwrap()
                }),
                ast::Instruction::Brl(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULT, a, b, "urcl_brl_cmp").unwrap()
                }),
                ast::Instruction::Brg(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGT, a, b, "urcl_brg_cmp").unwrap()
                }),
                ast::Instruction::Bre(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::EQ, a, b, "urcl_bre_cmp").unwrap()
                }),
                ast::Instruction::Bne(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::NE, a, b, "urcl_bne_cmp").unwrap()
                }),
                ast::Instruction::Bod(d, a) => gen!(1bc d a |a| {
                    let b = self.builder.build_and(a, word_1, "urcl_bod_b0").unwrap();
                    self.builder.build_int_compare(IntPredicate::NE, b, word_0, "urcl_bod_cmp").unwrap()
                }),
                ast::Instruction::Bev(d, a) => gen!(1bc d a |a| {
                    let b = self.builder.build_and(a, word_1, "urcl_bev_b0").unwrap();
                    self.builder.build_int_compare(IntPredicate::EQ, b, word_0, "urcl_bev_cmp").unwrap()
                }),
                ast::Instruction::Ble(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULE, a, b, "urcl_ble_cmp").unwrap()
                }),
                ast::Instruction::Brz(d, a) => gen!(1bc d a |a| {
                    self.builder.build_int_compare(IntPredicate::EQ, a, word_0, "urcl_brz_cmp").unwrap()
                }),
                ast::Instruction::Bnz(d, a) => gen!(1bc d a |a| {
                    self.builder.build_int_compare(IntPredicate::NE, a, word_0, "urcl_bnz_cmp").unwrap()
                }),
                ast::Instruction::Psh(v) => {
                    push(get_any(v));
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                ast::Instruction::Pop(d) => gen!(none d pop),
                ast::Instruction::Cal(a) => {
                    push(word_t.const_int(pc as u64 + 1, false));
                    uncond_br(a);
                },
                ast::Instruction::Ret() => {
                    gen_big_switch_table(pop());
                },
                ast::Instruction::Cpy(a, b) => {
                    write_ram(get_any(a), read_ram(get_any(b)));
                },
                ast::Instruction::Out(p, v) => {
                    let p = get_any(p);
                    let v = get_any(v);
                    self.builder
                        .build_call(port_out, &[p.into(), v.into()], "urcl_out")
                        .unwrap();
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                _ => {
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                    println!("{i}");
                },
            }
        }

        self.builder.position_at_end(*inst_bb.last().unwrap());
        ret();
    }

    pub fn dump(&self) {
        self.module
            .print_to_file(std::path::Path::new("urcl.ll"))
            .unwrap();
    }

    pub fn write_obj(&self, ft: targets::FileType, path: &std::path::Path) {
        targets::Target::initialize_all(&targets::InitializationConfig::default());

        let triple = targets::TargetMachine::get_default_triple();
        let target = targets::Target::from_triple(&triple).unwrap();
        let cpu = targets::TargetMachine::get_host_cpu_name();
        let features = targets::TargetMachine::get_host_cpu_features();
        let reloc = targets::RelocMode::DynamicNoPic;
        let model = targets::CodeModel::Default;
        let opt = OptimizationLevel::Aggressive;

        let target_machine = target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.to_str().unwrap(),
                opt,
                reloc,
                model,
            )
            .unwrap();

        target_machine
            .write_to_file(&self.module, ft, path)
            .unwrap();
    }
}
