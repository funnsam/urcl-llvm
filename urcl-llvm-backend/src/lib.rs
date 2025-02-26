use inkwell::{debug_info::AsDIScope, *};
use urcl_ast::*;

pub use inkwell::{targets::FileType, OptimizationLevel};

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
    pub native_addr: bool,
    pub bounds_safety: bool,
}

pub struct Codegen<'a> {
    context: &'a context::Context,
    builder: builder::Builder<'a>,
    module: module::Module<'a>,

    di_builder: debug_info::DebugInfoBuilder<'a>,
    di_cu: debug_info::DICompileUnit<'a>,

    program: &'a Program,
}

impl<'a> Codegen<'a> {
    pub fn new(context: &'a CodegenContext, program: &'a Program, file_name: &'a str) -> Self {
        let module = context.0.create_module("urcl");
        module.add_basic_value_flag(
            "Debug Info Version",
            inkwell::module::FlagBehavior::Warning,
            context.0.i32_type().const_int(3, false),
        );

        let (di_builder, di_cu) = module.create_debug_info_builder(
            true,
            debug_info::DWARFSourceLanguage::C,
            file_name,
            ".",
            "urcl-llvm",
            false,
            "",
            0,
            "",
            debug_info::DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );

        Self {
            context: &context.0,
            builder: context.0.create_builder(),
            module,

            di_builder,
            di_cu,

            program,
        }
    }

    fn val_to_ptr(&self, v: values::IntValue<'a>) -> values::PointerValue<'a> {
        let temp = self.builder.build_alloca(v.get_type(), "temp_val_to_ptr").unwrap();
        self.builder.build_store(temp, v).unwrap();
        temp
    }

    pub fn generate_code(&mut self, target: &targets::TargetMachine, options: &CodegenOptions, range_to_line: &dyn Fn(&std::ops::Range<usize>) -> u32) {
        let u8_t = self.context.custom_width_int_type(8);
        let word_t = self.context.custom_width_int_type(self.program.bits);
        let mach_t = self.context.ptr_sized_int_type(&target.get_target_data(), None);
        let ptr_t = self.context.ptr_type(AddressSpace::default());

        let float_t = match options.float_type {
            16 => self.context.f16_type(),
            32 => self.context.f32_type(),
            64 => self.context.f64_type(),
            128 => self.context.f128_type(),
            _ => panic!("invalid float bit width"),
        };
        let void_t = self.context.void_type();

        let di_word_t = self.di_builder.create_basic_type(
            "word",
            self.program.bits as u64,
            7,
            0,
        ).unwrap();
        let di_mach_t = self.di_builder.create_basic_type(
            "host_word",
            mach_t.get_bit_width() as u64,
            7,
            0,
        ).unwrap();

        let ram_size = self.program.min_stack + self.program.heap_size + self.program.dw.len() as u64;
        let ram_t = word_t.array_type(ram_size);

        let di_ram_t = self.di_builder.create_array_type(
            di_word_t.as_type(),
            self.program.bits as u64 * ram_size,
            0,
            &[0..ram_size as i64],
        );

        let word_1 = word_t.const_int(1, false);
        let word_0 = word_t.const_zero();
        let ram_size_const = word_t.const_int(ram_size, false);

        let native_addr = options.native_addr && self.program.bits == mach_t.get_bit_width();

        let word_t_from_imm = |i: &Immediate| match i {
            Immediate::Value(v) => {
                let unsigned = word_t.const_int_arbitrary_precision(v.as_sign_words().1);

                if v.sign() == dashu::base::Sign::Negative {
                    unsigned.const_neg()
                } else {
                    unsigned
                }
            },
            Immediate::InstLoc(l) => word_t.const_int(*l as _, false),
        };

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
            void_t.fn_type(&[u8_t.into(), ptr_t.into()], false),
            Some(module::Linkage::External),
        );
        let port_in = self.module.add_function(
            "urcl_in",
            void_t.fn_type(&[ptr_t.into(), u8_t.into()], false),
            Some(module::Linkage::External),
        );

        let di_main_t = self.di_builder.create_subroutine_type(
            self.di_cu.get_file(),
            Some(di_mach_t.as_type()),
            &[],
            inkwell::debug_info::DIFlagsConstants::PUBLIC,
        );
        let di_main = self.di_builder.create_function(
            self.di_cu.as_debug_info_scope(),
            "urcl_main",
            None,
            self.di_cu.get_file(),
            0,
            di_main_t,
            true,
            true,
            0,
            debug_info::DIFlagsConstants::PUBLIC,
            false,
        );
        main.set_subprogram(di_main);

        let lexical_block = self.di_builder.create_lexical_block(
            di_main.as_debug_info_scope(),
            self.di_cu.get_file(),
            0,
            0,
        );

        let entry = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry);
        let inst_bb = (0..=self.program.instructions.len())
            .map(|i| self.context.append_basic_block(main, &format!("inst_{i}")))
            .collect::<Vec<basic_block::BasicBlock>>();
        let imm_to_addr_or_val = |imm: &Immediate| {
            if native_addr {
                match imm {
                    // TODO: const int w/ bits > 64
                    Immediate::Value(v) => word_t.const_int(v.try_into().unwrap(), false),
                    Immediate::InstLoc(l) => unsafe { inst_bb[*l].get_address() }
                        .unwrap()
                        .const_to_int(word_t),
                }
            } else {
                word_t.const_int(imm.try_into().unwrap(), false)
            }
        };

        let loc = self.di_builder.create_debug_location(
            self.context,
            0,
            0,
            lexical_block.as_debug_info_scope(),
            None,
        );
        let registers = (1..=self.program.registers)
            .map(|r| {
                let name = format!("r{r}");
                let d = self.di_builder.create_auto_variable(
                    lexical_block.as_debug_info_scope(),
                    &name,
                    self.di_cu.get_file(),
                    0,
                    di_word_t.as_type(),
                    false,
                    0,
                    0,
                );

                let r = self.builder.build_alloca(word_t, &name).unwrap();
                let s = self.builder.build_store(r, word_0).unwrap();
                self.di_builder.insert_declare_before_instruction(r, Some(d), None, loc, s);
                r
            })
            .collect::<Vec<_>>();

        let di_ram = self.di_builder.create_auto_variable(
            lexical_block.as_debug_info_scope(),
            "ram",
            self.di_cu.get_file(),
            0,
            di_ram_t.as_type(),
            false,
            0,
            0,
        );
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
                .map(|i| imm_to_addr_or_val(i))
                .collect::<Vec<_>>()
                .as_slice(),
        );
        let s = self.builder.build_store(ram, ram_t.const_zero()).unwrap();
        self.builder.build_store(ram, dw).unwrap();
        self.di_builder.insert_declare_before_instruction(ram, Some(di_ram), None, loc, s);

        let stack_ptr = self.builder.build_alloca(word_t, "sp").unwrap();
        let s = self.builder
            .build_store(stack_ptr, ram_size_const)
            .unwrap();
        let d = self.di_builder.create_auto_variable(
            lexical_block.as_debug_info_scope(),
            "sp",
            self.di_cu.get_file(),
            0,
            di_word_t.as_type(),
            false,
            0,
            0,
        );
        self.di_builder.insert_declare_before_instruction(stack_ptr, Some(d), None, loc, s);

        let inst_cnt = self.builder.build_alloca(mach_t, "inst_cnt").unwrap();
        self.builder
            .build_store(inst_cnt, mach_t.const_zero())
            .unwrap();

        let big_switch_bb = self.context.append_basic_block(main, "big_switch_table");
        let big_switch_to = self.builder.build_alloca(word_t, "big_switch_to").unwrap();

        let ram_check_fail_bb = self.context.append_basic_block(main, "ram_check_fail");
        let ram_check_fail_addr = self.builder.build_alloca(word_t, "ram_check_fail_addr").unwrap();

        let ret = || {
            let inst_cnt_v = self
                .builder
                .build_load(mach_t, inst_cnt, "inst_cnt_v")
                .unwrap();
            self.builder.build_return(Some(&inst_cnt_v)).unwrap();
        };

        self.builder.build_unconditional_branch(inst_bb[0]).unwrap();

        for (pc, (i, line)) in self.program.instructions.iter().enumerate() {
            let loc = self.di_builder.create_debug_location(
                self.context,
                range_to_line(line),
                0,
                lexical_block.as_debug_info_scope(),
                None,
            );
            self.builder.set_current_debug_location(loc);

            let indir_jump_to = |v: values::IntValue| {
                if !native_addr {
                    self.builder.build_store(big_switch_to, v).unwrap();
                    self.builder
                        .build_unconditional_branch(big_switch_bb)
                        .unwrap();
                } else {
                    let v = self
                        .builder
                        .build_int_to_ptr(v, ptr_t, "target")
                        .unwrap();
                    self.builder.build_indirect_branch(v, &inst_bb).unwrap();
                }
            };

            let get_reg = |r: &_| match r {
                Register::General(0) => word_0,
                Register::General(g) => self
                    .builder
                    .build_load(word_t, registers[*g as usize - 1], "reg_load")
                    .unwrap()
                    .into_int_value(),
                Register::Pc => word_t.const_int(pc as _, false),
                Register::Sp => self
                    .builder
                    .build_load(word_t, stack_ptr, "reg_load")
                    .unwrap()
                    .into_int_value(),
            };

            let set_reg = |r: &_, v: values::IntValue| match r {
                Register::General(0) => true,
                Register::General(g) => {
                    self.builder
                        .build_store(registers[*g as usize - 1], v)
                        .unwrap();
                    true
                },
                Register::Pc => {
                    indir_jump_to(v);
                    false
                },
                Register::Sp => {
                    self.builder.build_store(stack_ptr, v).unwrap();
                    true
                },
            };

            let get_any = |v: &_| match v {
                Any::Register(r) => get_reg(r),
                Any::Immediate(i) => imm_to_addr_or_val(i),
            };

            let get_reg_ptr = |v: &_| match v {
                Register::General(0) => self.val_to_ptr(word_1),
                Register::General(i) => registers[*i as usize - 1],
                Register::Pc => self.val_to_ptr(word_t.const_int(pc as _, false)),
                Register::Sp => stack_ptr,
            };

            let get_any_ptr = |v: &_| match v {
                Any::Register(r) => get_reg_ptr(r),
                Any::Immediate(i) => self.val_to_ptr(word_t_from_imm(i)),
            };

            let ram_gep = |a| unsafe {
                if options.bounds_safety {
                    let ok = self.builder.build_int_compare(IntPredicate::ULT, a, ram_size_const, "ram_check").unwrap();
                    let after = self.context.append_basic_block(main, "ram_check_ok");
                    self.builder.build_store(ram_check_fail_addr, a).unwrap();
                    self.builder.build_conditional_branch(ok, after, ram_check_fail_bb).unwrap();
                    self.builder.position_at_end(after);
                }

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
                Any::Immediate(d) => {
                    self.builder
                        .build_conditional_branch(c, inst_bb[usize::try_from(d).unwrap()], inst_bb[pc + 1])
                        .unwrap();
                },
                Any::Register(d) => {
                    let e = self.context.append_basic_block(main, "indir_jump");
                    self.builder
                        .build_conditional_branch(c, e, inst_bb[pc + 1])
                        .unwrap();
                    self.builder.position_at_end(e);
                    indir_jump_to(get_reg(d));
                },
            };

            let uncond_br = |d: &_| match d {
                Any::Immediate(d) => {
                    self.builder
                        .build_unconditional_branch(inst_bb[usize::try_from(d).unwrap()])
                        .unwrap();
                },
                Any::Register(d) => indir_jump_to(get_reg(d)),
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
                    .build_bit_cast(i, float_t, "bitw_itof")
                    .unwrap()
                    .into_float_value()
            };

            let bit_ftoi = |f: values::FloatValue<'a>| {
                let float_it = self.context.custom_width_int_type(options.float_type as _);
                let i = self
                    .builder
                    .build_bit_cast(f, float_it, "bitw_ftoi")
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
                Instruction::Add(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_add(a, b, "add").unwrap()
                }),
                Instruction::Rsh(d, a) => gen!(1op d a |a| {
                        self.builder.build_right_shift(a, word_1, false, "rsh_urcl").unwrap()
                }),
                Instruction::Lod(d, a) => gen!(1op d a read_ram),
                Instruction::Str(a, v) => {
                    let a = get_any(a);
                    let v = get_any(v);
                    write_ram(a, v);
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Bge(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGE, a, b, "urcl_bge_cmp").unwrap()
                }),
                Instruction::Nor(d, a, b) => gen!(2op d a b |a, b| {
                    let c = self.builder.build_or(a, b, "nor_1").unwrap();
                    self.builder.build_not(c, "nor_2").unwrap()
                }),
                Instruction::Sub(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_sub(a, b, "sub").unwrap()
                }),
                Instruction::Imm(d, v) | Instruction::Mov(d, v) => gen!(1op d v |v| v),
                Instruction::Jmp(i) => uncond_br(i),
                Instruction::Nop() => {
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Lsh(d, a) => gen!(1op d a |a| {
                    self.builder.build_left_shift(a, word_1, "lsh").unwrap()
                }),
                Instruction::Inc(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_add(a, word_1, "inc").unwrap()
                }),
                Instruction::Dec(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_sub(a, word_1, "dec").unwrap()
                }),
                Instruction::Neg(d, a) => gen!(1op d a |a| {
                    self.builder.build_int_neg(a, "neg").unwrap()
                }),
                Instruction::And(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_and(a, b, "and").unwrap()
                }),
                Instruction::Or(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_or(a, b, "or").unwrap()
                }),
                Instruction::Not(d, a) => gen!(1op d a |a| {
                    self.builder.build_not(a, "not").unwrap()
                }),
                Instruction::Xnor(d, a, b) => gen!(2op d a b |a, b| {
                    let c = self.builder.build_xor(a, b, "xnor_1").unwrap();
                    self.builder.build_not(c, "xnor_2").unwrap()
                }),
                Instruction::Xor(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_xor(a, b, "xor").unwrap()
                }),
                Instruction::Nand(d, a, b) => gen!(2op d a b |a, b| {
                    let c = self.builder.build_and(a, b, "nand_1").unwrap();
                    self.builder.build_not(c, "nand_2").unwrap()
                }),
                Instruction::Brl(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULT, a, b, "brl_cmp").unwrap()
                }),
                Instruction::Brg(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGT, a, b, "brg_cmp").unwrap()
                }),
                Instruction::Bre(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::EQ, a, b, "bre_cmp").unwrap()
                }),
                Instruction::Bne(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::NE, a, b, "bne_cmp").unwrap()
                }),
                Instruction::Bod(d, a) => gen!(1bc d a |a| {
                    let b = self.builder.build_and(a, word_1, "bod_b0").unwrap();
                    self.builder.build_int_compare(IntPredicate::NE, b, word_0, "bod_cmp").unwrap()
                }),
                Instruction::Bev(d, a) => gen!(1bc d a |a| {
                    let b = self.builder.build_and(a, word_1, "bev_b0").unwrap();
                    self.builder.build_int_compare(IntPredicate::EQ, b, word_0, "bev_cmp").unwrap()
                }),
                Instruction::Ble(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULE, a, b, "ble_cmp").unwrap()
                }),
                Instruction::Brz(d, a) => gen!(1bc d a |a| {
                    self.builder.build_int_compare(IntPredicate::EQ, a, word_0, "brz_cmp").unwrap()
                }),
                Instruction::Bnz(d, a) => gen!(1bc d a |a| {
                    self.builder.build_int_compare(IntPredicate::NE, a, word_0, "bnz_cmp").unwrap()
                }),
                Instruction::Brn(d, a) => gen!(1bc d a |a| {
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
                Instruction::Brp(d, a) => gen!(1bc d a |a| {
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
                Instruction::Psh(v) => {
                    push(get_any(v));
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Pop(d) => gen!(none d pop),
                Instruction::Cal(a) => {
                    push(if !native_addr {
                        word_t.const_int(pc as u64 + 1, false)
                    } else {
                        unsafe { inst_bb[pc + 1].get_address() }.unwrap().const_to_int(word_t)
                    });
                    uncond_br(a);
                },
                Instruction::Ret() => {
                    indir_jump_to(pop());
                },
                Instruction::Hlt() => ret(),
                Instruction::Cpy(a, b) => {
                    write_ram(get_any(a), read_ram(get_any(b)));
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Brc(d, a, b) => {
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
                Instruction::Bnc(d, a, b) => {
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
                Instruction::Mlt(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_mul(a, b, "mul").unwrap()
                }),
                Instruction::Div(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_unsigned_div(a, b, "div").unwrap()
                }),
                Instruction::Mod(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_unsigned_rem(a, b, "mod").unwrap()
                }),
                Instruction::Bsr(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_right_shift(a, b, false, "bsr").unwrap()
                }),
                Instruction::Bsl(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_left_shift(a, b, "bsl").unwrap()
                }),
                Instruction::Srs(d, a) => gen!(1op d a |a| {
                    self.builder.build_right_shift(a, word_1, true, "srs").unwrap()
                }),
                Instruction::Bss(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_right_shift(a, b, true, "bss").unwrap()
                }),
                Instruction::SetE(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::EQ, a, b, "sete_cmp").unwrap()
                }),
                Instruction::SetNe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::NE, a, b, "setne_cmp").unwrap()
                }),
                Instruction::SetG(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGT, a, b, "setg_cmp").unwrap()
                }),
                Instruction::SetL(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULT, a, b, "setl_cmp").unwrap()
                }),
                Instruction::SetGe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::UGE, a, b, "setge_cmp").unwrap()
                }),
                Instruction::SetLe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::ULE, a, b, "setle_cmp").unwrap()
                }),
                Instruction::SetC(d, a, b) => {
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
                Instruction::SetNc(d, a, b) => {
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
                Instruction::LLod(d, a, b) => gen!(2op d a b |a, b| {
                    let addr = self.builder.build_int_add(a, b, "llod_addr").unwrap();
                    read_ram(addr)
                }),
                Instruction::LStr(a, o, v) => {
                    let a = get_any(a);
                    let o = get_any(o);
                    let v = get_any(v);
                    let addr = self.builder.build_int_add(a, o, "lstr_addr").unwrap();
                    write_ram(addr, v);
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::SDiv(d, a, b) => gen!(2op d a b |a, b| {
                    self.builder.build_int_signed_div(a, b, "sdiv").unwrap()
                }),
                Instruction::SBrl(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SLT, a, b, "sbrl_cmp").unwrap()
                }),
                Instruction::SBrg(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SGT, a, b, "sbrg_cmp").unwrap()
                }),
                Instruction::SBle(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SLE, a, b, "sble_cmp").unwrap()
                }),
                Instruction::SBge(d, a, b) => gen!(2bc d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SGE, a, b, "sbge_cmp").unwrap()
                }),
                Instruction::SSetG(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SGT, a, b, "ssetg_cmp").unwrap()
                }),
                Instruction::SSetL(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SLT, a, b, "ssetl_cmp").unwrap()
                }),
                Instruction::SSetGe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SGE, a, b, "ssetge_cmp").unwrap()
                }),
                Instruction::SSetLe(d, a, b) => gen!(2set d a b |a, b| {
                    self.builder.build_int_compare(IntPredicate::SLE, a, b, "ssetle_cmp").unwrap()
                }),
                Instruction::Abs(d, a) => gen!(1op d a |a: values::IntValue<'a>| {
                    let abs = intrinsics::Intrinsic::find("llvm.abs")
                        .unwrap()
                        .get_declaration(&self.module, &[word_t.into()])
                        .unwrap();
                    let fals = self.context.custom_width_int_type(1).const_zero();

                    self.builder
                        .build_call(abs, &[a.into(), fals.into()], "abs")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_int_value()
                }),
                Instruction::Out(p, v) => {
                    let p = get_any(p);
                    let v = get_any_ptr(v);

                    self.builder
                        .build_call(port_out, &[zext_or_trunc(p, u8_t).into(), v.into()], "out")
                        .unwrap();
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::In(r, p) => {
                    let r = get_reg_ptr(r);
                    let p = get_any(p);

                    self.builder
                        .build_call(port_in, &[r.into(), zext_or_trunc(p, u8_t).into()], "")
                        .unwrap();
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Umlt(d, a, b) => gen!(2op d a b |a, b| {
                    let ext_t = self.context.custom_width_int_type(self.program.bits * 2);
                    let a = self.builder.build_int_z_extend(a, ext_t, "umlt_a_zext").unwrap();
                    let b = self.builder.build_int_z_extend(b, ext_t, "umlt_b_zext").unwrap();
                    let r = self.builder.build_int_mul(a, b, "umlt_mlt").unwrap();
                    let s = self.builder.build_right_shift(r, ext_t.const_int(self.program.bits as _, false), false, "uml_rsh").unwrap();
                    self.builder.build_int_truncate(s, word_t, "umlt_trunc").unwrap()
                }),
                Instruction::SUmlt(d, a, b) => gen!(2op d a b |a, b| {
                    let ext_t = self.context.custom_width_int_type(self.program.bits * 2);
                    let a = self.builder.build_int_s_extend(a, ext_t, "sumlt_a_sext").unwrap();
                    let b = self.builder.build_int_s_extend(b, ext_t, "sumlt_b_sext").unwrap();
                    let r = self.builder.build_int_mul(a, b, "sumlt_mlt").unwrap();
                    let s = self.builder.build_right_shift(r, ext_t.const_int(self.program.bits as _, false), false, "suml_rsh").unwrap();
                    self.builder.build_int_truncate(s, word_t, "sumlt_trunc").unwrap()
                }),
                Instruction::ItoF(d, a) => gen!(1op d a |a| {
                    let f = self.builder.build_signed_int_to_float(a, float_t, "itof").unwrap();
                    bit_ftoi(f)
                }),
                Instruction::FtoI(d, a) => gen!(1op d a |a| {
                    let f = bit_itof(a);
                    self.builder.build_float_to_signed_int(f, word_t, "ftoi").unwrap()
                }),
                Instruction::FRtoI(d, a) => gen!(1op d a |a| {
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
                Instruction::FAdd(d, a, b) => gen!(2fop d a b |a, b| {
                    self.builder.build_float_add(a, b, "fadd").unwrap()
                }),
                Instruction::FSub(d, a, b) => gen!(2fop d a b |a, b| {
                    self.builder.build_float_sub(a, b, "fsub").unwrap()
                }),
                Instruction::FMlt(d, a, b) => gen!(2fop d a b |a, b| {
                    self.builder.build_float_mul(a, b, "fmlt").unwrap()
                }),
                Instruction::FDiv(d, a, b) => gen!(2fop d a b |a, b| {
                    self.builder.build_float_div(a, b, "fdiv").unwrap()
                }),
                Instruction::FSqrt(d, a) => gen!(1fop d a |a: values::FloatValue<'a>| {
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
                Instruction::FAbs(d, a) => gen!(1fop d a |a: values::FloatValue<'a>| {
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

        if !native_addr {
            self.builder.position_at_end(big_switch_bb);
            let v = self
                .builder
                .build_load(word_t, big_switch_to, "get_addr")
                .unwrap();
            self.builder
                .build_switch(
                    v.into_int_value(),
                    *inst_bb.last().unwrap(),
                    &inst_bb.iter()
                        .enumerate()
                        .map(|(i, b)| (word_t.const_int(i as _, false), *b))
                        .collect::<Vec<_>>(),
                )
                .unwrap();
        }

        self.builder.position_at_end(ram_check_fail_bb);
        if options.bounds_safety {
            let ram_check_fail = options.bounds_safety.then(|| self.module.add_function(
                "memory_oob",
                void_t.fn_type(&[ptr_t.into()], false),
                Some(module::Linkage::External),
            ));

            self.builder.build_call(
                ram_check_fail.unwrap(),
                &[ram_check_fail_addr.into()],
                "",
            ).unwrap();
        }
        ret();

        self.di_builder.finalize();
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

    pub fn get_machine(
        triple: Option<&str>,
        features: Option<&str>,
        opt: OptimizationLevel,
    ) -> targets::TargetMachine {
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
        let major = support::get_llvm_version().0;
        let suffix = format!("-{major}");

        let mut cmd = std::process::Command::new(format!("opt{suffix}"));
        let suffix = cmd
            .arg("--version")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .map_or_else(|_| "".to_string(), |_| suffix);

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
