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
        let port_in = self.module.add_function(
            "urcl_in",
            word_t.fn_type(&[word_t.into()], false),
            Some(module::Linkage::External),
        );

        let entry = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry);

        let registers = (1..=self.program.registers)
            .map(|r| {
                let r = self.builder.build_alloca(word_t, &format!("r{r}")).unwrap();
                self.builder.build_store(r, word_0).unwrap();
                r
            })
            .collect::<Vec<_>>();

        let ram_size = self.program.stack_size + self.program.heap_size + self.program.dw.len();
        let ram_t = word_t.array_type(ram_size as _);
        let ram = self.builder.build_alloca(ram_t, "ram").unwrap();
        let dw = word_t.const_array(
            self.program
                .dw
                .iter()
                .map(|i| word_t.const_int(*i as _, false))
                .collect::<Vec<_>>()
                .as_slice(),
        );
        self.builder.build_store(ram, ram_t.const_zero()).unwrap();
        self.builder.build_store(ram, dw).unwrap();

        let stack_ptr = self.builder.build_alloca(word_t, "sp").unwrap();
        self.builder
            .build_store(stack_ptr, word_t.const_int(ram_size as _, false))
            .unwrap();

        let inst_cnt = self.builder.build_alloca(mach_t, "inst_cnt").unwrap();
        self.builder
            .build_store(inst_cnt, mach_t.const_zero())
            .unwrap();

        let inst_bb = (0..=self.program.instructions.len())
            .map(|i| self.context.append_basic_block(main, &format!("inst_{i}")))
            .collect::<Vec<basic_block::BasicBlock>>();
        let big_switch_bb = self.context.append_basic_block(main, "big_switch_table");
        let big_switch_to = self.builder.build_alloca(word_t, "big_switch_to").unwrap();

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
                self.builder.build_store(big_switch_to, v).unwrap();
                self.builder.build_unconditional_branch(big_switch_bb).unwrap();
                /* self.builder
                    .build_switch(
                        v,
                        *inst_bb.last().unwrap(),
                        &inst_bb
                            .iter()
                            .enumerate()
                            .map(|(i, b)| (word_t.const_int(i as _, false), *b))
                            .collect::<Vec<_>>(),
                    )
                    .unwrap(); */
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
                    .build_in_bounds_gep(word_t, ram, &[a], "ram_gep")
                    .unwrap();
                self.builder
                    .build_load(word_t, p, "read_ram")
                    .unwrap()
                    .into_int_value()
            };

            let write_ram = |a, v: values::IntValue| unsafe {
                let p = self
                    .builder
                    .build_in_bounds_gep(word_t, ram, &[a], "ram_gep")
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
                let sp_old = self
                    .builder
                    .build_load(word_t, stack_ptr, "stack_ptr_pre_dec")
                    .unwrap()
                    .into_int_value();
                let sp_new = self
                    .builder
                    .build_int_sub(sp_old, word_1, "stack_ptr_dec")
                    .unwrap();
                self.builder.build_store(stack_ptr, sp_new).unwrap();

                write_ram(sp_new, v);
            };

            let pop = || {
                let sp_old = self
                    .builder
                    .build_load(word_t, stack_ptr, "stack_ptr_pre_inc")
                    .unwrap()
                    .into_int_value();
                let sp_new = self
                    .builder
                    .build_int_add(sp_old, word_1, "stack_ptr_inc")
                    .unwrap();
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
                (2set $d: tt $a: tt $b: tt $gen: expr) => {{
                    let a = get_any($a);
                    let b = get_any($b);
                    let c = $gen(a, b);
                    let c = self.builder.build_int_s_extend(c, word_t, "set_sext").unwrap();
                    if set_reg($d, c) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
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
                    let c = self.builder.build_or(a, b, "nor_1").unwrap();
                    self.builder.build_not(c, "nor_2").unwrap()
                }),
                ast::Instruction::Sub(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_sub(a, b, "sub").unwrap()
                }),
                ast::Instruction::Imm(d, v) | ast::Instruction::Mov(d, v) => gen!(1op d v |v| v),
                ast::Instruction::Jmp(i) => uncond_br(i),
                ast::Instruction::Nop() => {
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                ast::Instruction::Lsh(d, a) => gen!(1op d a |a| {
                    self.builder.build_left_shift(a, word_1, "lsh").unwrap()
                }),
                ast::Instruction::Inc(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_add(a, word_1, "inc").unwrap()
                }),
                ast::Instruction::Dec(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_sub(a, word_1, "dec").unwrap()
                }),
                ast::Instruction::Neg(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_neg(a, "neg").unwrap()
                }),
                ast::Instruction::And(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_and(a, b, "and").unwrap()
                }),
                ast::Instruction::Or(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_or(a, b, "or").unwrap()
                }),
                ast::Instruction::Not(d, a) => gen!(1op d a |a| {
                    self.builder.build_not(a, "not").unwrap()
                }),
                ast::Instruction::Xnor(d, a, b) => gen!(2op d a b |a, b| {
                    let c = self.builder.build_xor(a, b, "xnor_1").unwrap();
                    self.builder.build_not(c, "xnor_2").unwrap()
                }),
                ast::Instruction::Xor(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_xor(a, b, "xor").unwrap()
                }),
                ast::Instruction::Nand(d, a, b) => gen!(2op d a b |a, b| {
                    let c = self.builder.build_and(a, b, "nand_1").unwrap();
                    self.builder.build_not(c, "nand_2").unwrap()
                }),
                ast::Instruction::Brl(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULT, a, b, "brl_cmp").unwrap()
                }),
                ast::Instruction::Brg(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGT, a, b, "brg_cmp").unwrap()
                }),
                ast::Instruction::Bre(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::EQ, a, b, "bre_cmp").unwrap()
                }),
                ast::Instruction::Bne(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::NE, a, b, "bne_cmp").unwrap()
                }),
                ast::Instruction::Bod(d, a) => gen!(1bc d a |a| {
                    let b = self.builder.build_and(a, word_1, "bod_b0").unwrap();
                    self.builder.build_int_compare(IntPredicate::NE, b, word_0, "bod_cmp").unwrap()
                }),
                ast::Instruction::Bev(d, a) => gen!(1bc d a |a| {
                    let b = self.builder.build_and(a, word_1, "bev_b0").unwrap();
                    self.builder.build_int_compare(IntPredicate::EQ, b, word_0, "bev_cmp").unwrap()
                }),
                ast::Instruction::Ble(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULE, a, b, "ble_cmp").unwrap()
                }),
                ast::Instruction::Brz(d, a) => gen!(1bc d a |a| {
                    self.builder.build_int_compare(IntPredicate::EQ, a, word_0, "brz_cmp").unwrap()
                }),
                ast::Instruction::Bnz(d, a) => gen!(1bc d a |a| {
                    self.builder.build_int_compare(IntPredicate::NE, a, word_0, "bnz_cmp").unwrap()
                }),
                ast::Instruction::Brn(d, a) => gen!(1bc d a |a| {
                    let s = self.builder
                        .build_right_shift(
                            a,
                            word_t.const_int((self.program.bits - 1) as _, false),
                            false,
                            "brn_sign",
                        )
                        .unwrap();
                    self.builder
                        .build_int_compare(IntPredicate::NE, s, word_0, "brn_cmp")
                        .unwrap()
                }),
                ast::Instruction::Brp(d, a) => gen!(1bc d a |a| {
                    let s = self.builder
                        .build_right_shift(
                            a,
                            word_t.const_int((self.program.bits - 1) as _, false),
                            false,
                            "brp_sign",
                        )
                        .unwrap();
                    self.builder
                        .build_int_compare(IntPredicate::EQ, s, word_0, "brp_cmp")
                        .unwrap()
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
                ast::Instruction::Hlt() => ret(),
                ast::Instruction::Cpy(a, b) => {
                    write_ram(get_any(a), read_ram(get_any(b)));
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                ast::Instruction::Brc(d, a, b) => {
                    gen!(2bc d a b |a: values::IntValue<'a>, b: values::IntValue<'a>| {
                        let add_ov = intrinsics::Intrinsic::find("llvm.uadd.with.overflow")
                            .unwrap()
                            .get_declaration(&self.module, &[word_t.into()])
                            .unwrap();

                        let add = self.builder
                            .build_call(add_ov, &[a.into(), b.into()], "brc_check")
                            .unwrap()
                            .try_as_basic_value()
                            .unwrap_left()
                            .into_struct_value();
                        self.builder
                            .build_extract_value(add, 1, "brc")
                            .unwrap()
                            .into_int_value()
                    })
                },
                ast::Instruction::Bnc(d, a, b) => {
                    gen!(2bc d a b |a: values::IntValue<'a>, b: values::IntValue<'a>| {
                        let add_ov = intrinsics::Intrinsic::find("llvm.uadd.with.overflow")
                            .unwrap()
                            .get_declaration(&self.module, &[word_t.into()])
                            .unwrap();

                        let add = self.builder
                            .build_call(add_ov, &[a.into(), b.into()], "bnc_check")
                            .unwrap()
                            .try_as_basic_value()
                            .unwrap_left()
                            .into_struct_value();
                        let ov = self.builder
                            .build_extract_value(add, 1, "bnc_ov")
                            .unwrap()
                            .into_int_value();
                        self.builder
                            .build_not(ov, "bnc")
                            .unwrap()
                    })
                },
                ast::Instruction::Mlt(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_mul(a, b, "mul").unwrap()
                }),
                ast::Instruction::Div(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_unsigned_div(a, b, "div").unwrap()
                }),
                ast::Instruction::Mod(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_unsigned_rem(a, b, "mod").unwrap()
                }),
                ast::Instruction::Bsr(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_right_shift(a, b, false, "bsr").unwrap()
                }),
                ast::Instruction::Bsl(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_left_shift(a, b, "bsl").unwrap()
                }),
                ast::Instruction::Srs(d, a) => gen!(1op d a |a| {
                    self.builder.build_right_shift(a, word_1, true, "srs").unwrap()
                }),
                ast::Instruction::Bss(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_right_shift(a, b, true, "bss").unwrap()
                }),
                ast::Instruction::SetE(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::EQ, a, b, "sete_cmp").unwrap()
                }),
                ast::Instruction::SetNe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::NE, a, b, "setne_cmp").unwrap()
                }),
                ast::Instruction::SetG(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGT, a, b, "setg_cmp").unwrap()
                }),
                ast::Instruction::SetL(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULT, a, b, "setl_cmp").unwrap()
                }),
                ast::Instruction::SetGe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGE, a, b, "setge_cmp").unwrap()
                }),
                ast::Instruction::SetLe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULE, a, b, "setle_cmp").unwrap()
                }),
                ast::Instruction::SetC(d, a, b) => {
                    gen!(2set d a b |a: values::IntValue<'a>, b: values::IntValue<'a>| {
                        let add_ov = intrinsics::Intrinsic::find("llvm.uadd.with.overflow")
                            .unwrap()
                            .get_declaration(&self.module, &[word_t.into()])
                            .unwrap();

                        let add = self.builder
                            .build_call(add_ov, &[a.into(), b.into()], "setc_check")
                            .unwrap()
                            .try_as_basic_value()
                            .unwrap_left()
                            .into_struct_value();
                        self.builder
                            .build_extract_value(add, 1, "brc")
                            .unwrap()
                            .into_int_value()
                    })
                },
                ast::Instruction::SetNc(d, a, b) => {
                    gen!(2set d a b |a: values::IntValue<'a>, b: values::IntValue<'a>| {
                        let add_ov = intrinsics::Intrinsic::find("llvm.uadd.with.overflow")
                            .unwrap()
                            .get_declaration(&self.module, &[word_t.into()])
                            .unwrap();

                        let add = self.builder
                            .build_call(add_ov, &[a.into(), b.into()], "setnc_check")
                            .unwrap()
                            .try_as_basic_value()
                            .unwrap_left()
                            .into_struct_value();
                        let ov = self.builder
                            .build_extract_value(add, 1, "bnc_ov")
                            .unwrap()
                            .into_int_value();
                        self.builder
                            .build_not(ov, "bnc")
                            .unwrap()
                    })
                },
                ast::Instruction::LLod(d, a, b) => gen!(2op d a b |a, b| {
                    let addr = self.builder.build_int_add(a, b, "llod_addr").unwrap();
                    read_ram(addr)
                }),
                ast::Instruction::LStr(a, o, v) => {
                    let a = get_any(a);
                    let o = get_any(o);
                    let v = get_any(v);
                    let addr = self.builder.build_int_add(a, o, "lstr_addr").unwrap();
                    write_ram(addr, v);
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                ast::Instruction::SDiv(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_signed_div(a, b, "sdiv").unwrap()
                }),
                ast::Instruction::SBrl(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SLT, a, b, "sbrl_cmp").unwrap()
                }),
                ast::Instruction::SBrg(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SGT, a, b, "sbrg_cmp").unwrap()
                }),
                ast::Instruction::SBle(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SLE, a, b, "sble_cmp").unwrap()
                }),
                ast::Instruction::SBge(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SGE, a, b, "sbge_cmp").unwrap()
                }),
                ast::Instruction::SSetG(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SGT, a, b, "ssetg_cmp").unwrap()
                }),
                ast::Instruction::SSetL(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SLT, a, b, "ssetl_cmp").unwrap()
                }),
                ast::Instruction::SSetGe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SGE, a, b, "ssetge_cmp").unwrap()
                }),
                ast::Instruction::SSetLe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SLE, a, b, "ssetle_cmp").unwrap()
                }),
                ast::Instruction::Abs(d, a) => gen!(1op d a |a: values::IntValue<'a>| {
                    let abs = intrinsics::Intrinsic::find("llvm.abs")
                        .unwrap()
                        .get_declaration(&self.module, &[word_t.into()])
                        .unwrap();

                    self.builder
                        .build_call(abs, &[a.into(), word_0.into()], "abs")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_int_value()
                }),
                ast::Instruction::Out(p, v) => {
                    let p = get_any(p);
                    let v = get_any(v);
                    self.builder
                        .build_call(port_out, &[p.into(), v.into()], "out")
                        .unwrap();
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                ast::Instruction::In(r, p) => gen!(1op r p |p: values::IntValue<'a>| {
                    self.builder
                        .build_call(port_in, &[p.into()], "in")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_int_value()
                }),
            }
        }

        self.builder.position_at_end(*inst_bb.last().unwrap());
        ret();

        self.builder.position_at_end(big_switch_bb);
        let v = self.builder.build_load(word_t, big_switch_to, "get_addr").unwrap();
        self.builder
            .build_switch(
                v.into_int_value(),
                *inst_bb.last().unwrap(),
                &inst_bb
                .iter()
                .enumerate()
                .map(|(i, b)| (word_t.const_int(i as _, false), *b))
                .collect::<Vec<_>>(),
            )
            .unwrap();
    }

    pub fn dump(&self) {
        self.module
            .print_to_file(std::path::Path::new("urcl.ll"))
            .unwrap();
        self.module
            .write_bitcode_to_path(std::path::Path::new("urcl.bc"));
    }

    pub fn dump_opt(&self) {
        self.module
            .print_to_file(std::path::Path::new("urcl.opt.ll"))
            .unwrap();
        self.module
            .write_bitcode_to_path(std::path::Path::new("urcl.opt.bc"));
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

        let pass = passes::PassBuilderOptions::create();
        self.module
            .run_passes(OPT_O3_PASSES, &target_machine, pass)
            .unwrap();

        target_machine
            .write_to_file(&self.module, ft, path)
            .unwrap();
    }
}

// HACK:
// if it doesn't work on your machine then copy ```
// llvm-as-17 < /dev/null | opt-17 --print-pipeline-passes -O3 2> /dev/null
// ```
const OPT_O3_PASSES: &str = "annotation2metadata,forceattrs,inferattrs,coro-early,function<eager-inv>(lower-expect,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;no-switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch>,sroa<modify-cfg>,early-cse<>,callsite-splitting),openmp-opt,ipsccp,called-value-propagation,globalopt,function<eager-inv>(mem2reg,instcombine<max-iterations=1000;no-use-loop-info>,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch>),require<globals-aa>,function(invalidate<aa>),require<profile-summary>,cgscc(devirt<4>(inline<only-mandatory>,inline,function-attrs<skip-non-recursive>,argpromotion,openmp-opt-cgscc,function<eager-inv;no-rerun>(sroa<modify-cfg>,early-cse<memssa>,speculative-execution,jump-threading,correlated-propagation,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch>,instcombine<max-iterations=1000;no-use-loop-info>,aggressive-instcombine,constraint-elimination,libcalls-shrinkwrap,tailcallelim,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch>,reassociate,loop-mssa(loop-instsimplify,loop-simplifycfg,licm<no-allowspeculation>,loop-rotate<header-duplication;no-prepare-for-lto>,licm<allowspeculation>,simple-loop-unswitch<nontrivial;trivial>),simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch>,instcombine<max-iterations=1000;no-use-loop-info>,loop(loop-idiom,indvars,loop-deletion,loop-unroll-full),sroa<modify-cfg>,vector-combine,mldst-motion<no-split-footer-bb>,gvn<>,sccp,bdce,instcombine<max-iterations=1000;no-use-loop-info>,jump-threading,correlated-propagation,adce,memcpyopt,dse,move-auto-init,loop-mssa(licm<allowspeculation>),coro-elide,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;hoist-common-insts;sink-common-insts;speculate-blocks;simplify-cond-branch>,instcombine<max-iterations=1000;no-use-loop-info>),function-attrs,function(require<should-not-run-function-passes>),coro-split)),deadargelim,coro-cleanup,globalopt,globaldce,elim-avail-extern,rpo-function-attrs,recompute-globalsaa,function<eager-inv>(float2int,lower-constant-intrinsics,chr,loop(loop-rotate<header-duplication;no-prepare-for-lto>,loop-deletion),loop-distribute,inject-tli-mappings,loop-vectorize<no-interleave-forced-only;no-vectorize-forced-only;>,loop-load-elim,instcombine<max-iterations=1000;no-use-loop-info>,simplifycfg<bonus-inst-threshold=1;forward-switch-cond;switch-range-to-icmp;switch-to-lookup;no-keep-loops;hoist-common-insts;sink-common-insts;speculate-blocks;simplify-cond-branch>,slp-vectorizer,vector-combine,instcombine<max-iterations=1000;no-use-loop-info>,loop-unroll<O3>,transform-warning,sroa<preserve-cfg>,instcombine<max-iterations=1000;no-use-loop-info>,loop-mssa(licm<allowspeculation>),alignment-from-assumptions,loop-sink,instsimplify,div-rem-pairs,tailcallelim,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch>),globaldce,constmerge,cg-profile,rel-lookup-table-converter,function(annotation-remarks),verify";
