use inkwell::*;
use urcl_ast as ast;

pub use inkwell::OptimizationLevel;
pub use targets::FileType;

pub struct CodegenContext(context::Context);

impl Default for CodegenContext {
    fn default() -> Self { Self::new() }
}

impl CodegenContext {
    pub fn new() -> Self { Self(context::Context::create()) }
}

pub struct CodegenOptions {
    pub use_global: bool,
    pub float_type: usize,
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

    pub fn generate_code(&mut self, target: &targets::TargetMachine, options: &CodegenOptions) {
        let word_t = self.context.custom_width_int_type(self.program.bits as u32);
        let mach_t = self
            .context
            .ptr_sized_int_type(&target.get_target_data(), None);
        let float_t = match options.float_type {
            16 => self.context.f16_type(),
            32 => self.context.f32_type(),
            64 => self.context.f64_type(),
            128 => self.context.f128_type(),
            _ => panic!("invalid float bit width"),
        };
        let void = self.context.void_type();

        let word_1 = word_t.const_int(1, false);
        let word_0 = word_t.const_zero();

        let zext_or_trunc = |i: values::IntValue<'a>, t: types::IntType<'a>| {
            use core::cmp::Ordering::*;
            match i.get_type().get_bit_width().cmp(&t.get_bit_width()) {
                Greater => self.builder.build_int_truncate(i, t, "conv_trunc").unwrap(),
                Less => self.builder.build_int_z_extend(i, t, "conv_zext").unwrap(),
                Equal => i,
            }
        };

        let main = self
            .module
            .add_function("urcl_main", mach_t.fn_type(&[], false), None);
        let port_out = self.module.add_function(
            "urcl_out",
            void.fn_type(&[mach_t.into(); 2], false),
            Some(module::Linkage::External),
        );
        let port_in = self.module.add_function(
            "urcl_in",
            word_t.fn_type(&[mach_t.into()], false),
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

        let ram_size = self.program.min_stack + self.program.heap_size + self.program.dw.len();
        let ram_t = word_t.array_type(ram_size as _);

        let ram = if options.use_global {
            let g = self.module.add_global(ram_t, None, "ram");
            g.set_externally_initialized(false);
            g.set_initializer(&ram_t.const_zero());
            g.set_linkage(module::Linkage::Internal);
            g.as_pointer_value()
        } else {
            self.builder.build_alloca(ram_t, "ram").unwrap()
        };
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
                self.builder
                    .build_unconditional_branch(big_switch_bb)
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

            let ram_gep = |a| unsafe {
                let a = if mach_t.get_bit_width() > word_t.get_bit_width() {
                    self.builder
                        .build_int_z_extend(a, mach_t, "ram_gep_zext")
                        .unwrap()
                } else {
                    a
                };

                self.builder
                    .build_in_bounds_gep(word_t, ram, &[a], "ram_gep")
                    .unwrap()
            };

            let read_ram = |a| {
                self.builder
                    .build_load(word_t, ram_gep(a), "read_ram")
                    .unwrap()
                    .into_int_value()
            };

            let write_ram = |a, v: values::IntValue| {
                self.builder.build_store(ram_gep(a), v).unwrap();
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

            let bit_itof = |i: values::IntValue<'a>| {
                let float_it = self.context.custom_width_int_type(options.float_type as _);
                let i = zext_or_trunc(i, float_it);
                self.builder
                    .build_bitcast(i, float_t, "bitw_itof")
                    .unwrap()
                    .into_float_value()
            };

            let bit_ftoi = |f: values::FloatValue<'a>| {
                let float_it = self.context.custom_width_int_type(options.float_type as _);
                let i = self
                    .builder
                    .build_bitcast(f, float_it, "bitw_ftoi")
                    .unwrap()
                    .into_int_value();

                zext_or_trunc(i, word_t)
            };

            macro_rules! gen {
                (2op $d: tt $a: tt $b: tt $gen: expr) => {{
                    let a = get_any($a);
                    let b = get_any($b);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, $gen(a, b)) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (1op $d: tt $a: tt $gen: expr) => {{
                    let a = get_any($a);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, $gen(a)) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (none $d: tt $gen: expr) => {{
                    #[allow(clippy::blocks_in_conditions)]
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
                    let c = self
                        .builder
                        .build_int_s_extend(c, word_t, "set_sext")
                        .unwrap();
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, c) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (2fop $d: tt $a: tt $b: tt $gen: expr) => {{
                    let a = get_any($a);
                    let a = bit_itof(a);
                    let b = get_any($b);
                    let b = bit_itof(b);
                    let c = $gen(a, b);
                    let c = bit_ftoi(c);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, c) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (1fop $d: tt $a: tt $gen: expr) => {{
                    let a = get_any($a);
                    let a = bit_itof(a);
                    let c = $gen(a);
                    let c = bit_ftoi(c);
                    #[allow(clippy::blocks_in_conditions)]
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
                        .build_call(
                            port_out,
                            &[
                                zext_or_trunc(p, mach_t).into(),
                                zext_or_trunc(v, mach_t).into(),
                            ],
                            "out",
                        )
                        .unwrap();
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                ast::Instruction::In(r, p) => gen!(1op r p |p: values::IntValue<'a>| {
                    let i = self.builder
                        .build_call(port_in, &[zext_or_trunc(p, mach_t).into()], "in")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_int_value();
                    zext_or_trunc(i, word_t)
                }),
                ast::Instruction::Umlt(d, a, b) => gen!(2op d a b |a, b| {
                    let ext_t = self.context.custom_width_int_type(self.program.bits as u32 * 2);
                    let a = self.builder.build_int_z_extend(a, ext_t, "umlt_a_zext").unwrap();
                    let b = self.builder.build_int_z_extend(b, ext_t, "umlt_b_zext").unwrap();
                    let r = self.builder.build_int_mul(a, b, "umlt_mlt").unwrap();
                    let s = self.builder.build_right_shift(r, ext_t.const_int(self.program.bits as _, false), false, "uml_rsh").unwrap();
                    self.builder.build_int_truncate(s, word_t, "umlt_trunc").unwrap()
                }),
                ast::Instruction::SUmlt(d, a, b) => gen!(2op d a b |a, b| {
                    let ext_t = self.context.custom_width_int_type(self.program.bits as u32 * 2);
                    let a = self.builder.build_int_s_extend(a, ext_t, "sumlt_a_sext").unwrap();
                    let b = self.builder.build_int_s_extend(b, ext_t, "sumlt_b_sext").unwrap();
                    let r = self.builder.build_int_mul(a, b, "sumlt_mlt").unwrap();
                    let s = self.builder.build_right_shift(r, ext_t.const_int(self.program.bits as _, false), false, "suml_rsh").unwrap();
                    self.builder.build_int_truncate(s, word_t, "sumlt_trunc").unwrap()
                }),
                ast::Instruction::ItoF(d, a) => gen!(1op d a |a| {
                    let f = self.builder.build_signed_int_to_float(a, float_t, "itof").unwrap();
                    bit_ftoi(f)
                }),
                ast::Instruction::FtoI(d, a) => gen!(1op d a |a| {
                    let f = bit_itof(a);
                    self.builder.build_float_to_signed_int(f, word_t, "ftoi").unwrap()
                }),
                ast::Instruction::FRtoI(d, a) => gen!(1op d a |a| {
                    let f = bit_itof(a);
                    let round = intrinsics::Intrinsic::find("llvm.round")
                        .unwrap()
                        .get_declaration(&self.module, &[float_t.into()])
                        .unwrap();
                    let r = self.builder.build_call(round, &[f.into()], "frtoi_round")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_float_value();

                    self.builder.build_float_to_signed_int(r, word_t, "frtoi").unwrap()
                }),
                ast::Instruction::FAdd(d, a, b) => gen!(2fop d a b |a, b| {
                    self.builder.build_float_add(a, b, "fadd").unwrap()
                }),
                ast::Instruction::FSub(d, a, b) => gen!(2fop d a b |a, b| {
                    self.builder.build_float_sub(a, b, "fsub").unwrap()
                }),
                ast::Instruction::FMlt(d, a, b) => gen!(2fop d a b |a, b| {
                    self.builder.build_float_mul(a, b, "fmlt").unwrap()
                }),
                ast::Instruction::FDiv(d, a, b) => gen!(2fop d a b |a, b| {
                    self.builder.build_float_div(a, b, "fdiv").unwrap()
                }),
                ast::Instruction::FSqrt(d, a) => gen!(1fop d a |a: values::FloatValue<'a>| {
                    let round = intrinsics::Intrinsic::find("llvm.sqrt")
                        .unwrap()
                        .get_declaration(&self.module, &[float_t.into()])
                        .unwrap();
                    self.builder.build_call(round, &[a.into()], "fsqrt")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_float_value()
                }),
                ast::Instruction::FAbs(d, a) => gen!(1fop d a |a: values::FloatValue<'a>| {
                    let round = intrinsics::Intrinsic::find("llvm.fabs")
                        .unwrap()
                        .get_declaration(&self.module, &[float_t.into()])
                        .unwrap();
                    self.builder.build_call(round, &[a.into()], "fabs")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_float_value()
                }),
            }
        }

        self.builder.position_at_end(*inst_bb.last().unwrap());
        ret();

        self.builder.position_at_end(big_switch_bb);
        let v = self
            .builder
            .build_load(word_t, big_switch_to, "get_addr")
            .unwrap();
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

    pub fn optimize(&self, target: &targets::TargetMachine, opt: OptimizationLevel) {
        let pass = passes::PassBuilderOptions::create();
        self.module
            .run_passes(&Self::get_passes(opt as _), target, pass)
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

    pub fn get_machine(triple: Option<&str>, features: Option<&str>, opt: OptimizationLevel) -> targets::TargetMachine {
        targets::Target::initialize_all(&targets::InitializationConfig::default());

        let triple = triple.map_or_else(
            targets::TargetMachine::get_default_triple,
            targets::TargetTriple::create,
        );
        let target = targets::Target::from_triple(&triple).unwrap();
        let cpu = targets::TargetMachine::get_host_cpu_name();
        let ft = targets::TargetMachine::get_host_cpu_features();
        let reloc = targets::RelocMode::DynamicNoPic;
        let model = targets::CodeModel::Default;

        target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.unwrap_or_else(|| ft.to_str().unwrap()),
                opt,
                reloc,
                model,
            )
            .unwrap()
    }

    pub fn write_obj(
        &self,
        tm: &targets::TargetMachine,
        ft: targets::FileType,
        path: &std::path::Path,
    ) {
        tm.write_to_file(&self.module, ft, path).unwrap();
    }

    fn get_passes(lv: usize) -> String {
        let mut suffix = std::process::Command::new("opt");
        let suffix = suffix
            .arg("--version")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .map_or("-17", |_| "");

        let mut proc = std::process::Command::new("sh");
        let proc = proc.arg("-c").arg(format!(
            "llvm-as{suffix} < /dev/null | opt{suffix} --print-pipeline-passes -O{lv} 2> /dev/null"
        ));

        let out = proc.output().unwrap();
        // if !out.status.success() {
        //     panic!("failed trying to get optimization passes (returned {}) {out:?}", out.status);
        // }

        let mut s = String::from_utf8(out.stdout)
            .unwrap()
            .replace("BitcodeWriterPass", "")
            .replace(",,", "")
            .trim_end()
            .to_string();

        if s.chars().last().map_or(false, |c| c == ',') {
            s.pop();
        }

        s
    }
}
