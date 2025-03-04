mod float;
mod float_size;
mod util;

use float_size::FloatSize;
use inkwell::{
    AddressSpace, IntPredicate, basic_block, builder, context, debug_info, debug_info::AsDIScope,
    intrinsics, module, targets, types, values,
};
use urcl_ast::{Any, AnyFloat, AnyInt, Instruction, Program, Register};

pub use inkwell::{OptimizationLevel, targets::FileType};

pub struct CodegenContext(context::Context);

impl Default for CodegenContext {
    fn default() -> Self { Self::new() }
}

impl CodegenContext {
    #[must_use]
    pub fn new() -> Self { Self(context::Context::create()) }
}

pub struct CodegenOptions {
    pub use_global: bool,
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
    options: &'a CodegenOptions,

    word: types::IntType<'a>,
    float: types::FloatType<'a>,
    float_size: FloatSize,
}

impl<'a> Codegen<'a> {
    #[must_use]
    pub fn new(
        context: &'a CodegenContext,
        program: &'a Program,
        options: &'a CodegenOptions,
        file_name: &'a str,
    ) -> Self {
        let module = context.0.create_module("urcl");
        module.add_basic_value_flag(
            "Debug Info Version",
            module::FlagBehavior::Warning,
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

        let word = context.0.custom_width_int_type(program.bits);

        let (float, float_size) = match program.bits {
            128.. => (context.0.f128_type(), FloatSize::_128),
            64.. => (context.0.f64_type(), FloatSize::_64),
            32.. => (context.0.f32_type(), FloatSize::_32),
            _ => (context.0.f16_type(), FloatSize::_16),
        };

        Self {
            context: &context.0,
            builder: context.0.create_builder(),
            module,

            di_builder,
            di_cu,

            program,
            options,

            word,
            float,
            float_size,
        }
    }

    pub fn generate_code(
        &mut self,
        target: &targets::TargetMachine,
        range_to_line: &dyn Fn(&std::ops::Range<usize>) -> u32,
    ) {
        let i8_t = self.context.i8_type();
        let mach_t = self
            .context
            .ptr_sized_int_type(&target.get_target_data(), None);
        let ptr_t = self.context.ptr_type(AddressSpace::default());
        let void_t = self.context.void_type();

        let di_word_t = self.di_basic_type("urcl_t", self.program.bits.into());
        let di_mach_t = self.di_basic_type("size_t", mach_t.get_bit_width().into());

        let ram_size =
            self.program.min_stack + self.program.heap_size + self.program.dw.len() as u64;
        let ram_t = self.word.array_type(ram_size);

        let di_ram_t = self.di_builder.create_array_type(
            di_word_t.as_type(),
            u64::from(self.program.bits) * ram_size,
            0,
            #[allow(clippy::single_range_in_vec_init)]
            &[0..ram_size as i64],
        );

        let word_1 = self.word.const_int(1, false);
        let word_0 = self.word.const_zero();
        let ram_size_const = self.word.const_int(ram_size, false);

        let native_addr = self.options.native_addr && self.program.bits >= mach_t.get_bit_width();

        let main = self
            .module
            .add_function("urcl_main", mach_t.fn_type(&[], false), None);
        let port_out = self.module.add_function(
            "urcl_out",
            void_t.fn_type(&[i8_t.into(), ptr_t.into()], false),
            Some(module::Linkage::External),
        );
        let port_in = self.module.add_function(
            "urcl_in",
            void_t.fn_type(&[ptr_t.into(), i8_t.into()], false),
            Some(module::Linkage::External),
        );

        let di_main_t = self.di_builder.create_subroutine_type(
            self.di_cu.get_file(),
            Some(di_mach_t.as_type()),
            &[],
            debug_info::DIFlagsConstants::PUBLIC,
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

                let r = self.builder.build_alloca(self.word, &name).unwrap();
                let s = self.builder.build_store(r, word_0).unwrap();
                self.di_builder
                    .insert_declare_before_instruction(r, Some(d), None, loc, s);
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
        let ram = if self.options.use_global {
            let g = self.module.add_global(ram_t, None, "ram");
            g.set_externally_initialized(false);
            g.set_initializer(&ram_t.const_zero());
            g.set_linkage(module::Linkage::Internal);
            g.as_pointer_value()
        } else {
            self.builder.build_alloca(ram_t, "ram").unwrap()
        };
        let dw = self.word.const_array(
            self.program
                .dw
                .iter()
                .map(|i| self.imm_to_addr_or_word(i, native_addr, &inst_bb))
                .collect::<Vec<_>>()
                .as_slice(),
        );
        let s = self.builder.build_store(ram, ram_t.const_zero()).unwrap();
        self.builder.build_store(ram, dw).unwrap();
        self.di_builder
            .insert_declare_before_instruction(ram, Some(di_ram), None, loc, s);

        let stack_ptr = self.builder.build_alloca(self.word, "sp").unwrap();
        let s = self.builder.build_store(stack_ptr, ram_size_const).unwrap();
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
        self.di_builder
            .insert_declare_before_instruction(stack_ptr, Some(d), None, loc, s);

        let inst_cnt = self.builder.build_alloca(mach_t, "inst_cnt").unwrap();
        self.builder
            .build_store(inst_cnt, mach_t.const_zero())
            .unwrap();
        let temp_var = self.builder.build_alloca(self.word, "temp_var").unwrap();

        let big_switch_bb = self.context.append_basic_block(main, "big_switch_table");
        let big_switch_to = self
            .builder
            .build_alloca(self.word, "big_switch_to")
            .unwrap();

        let ram_check_fail_bb = self.context.append_basic_block(main, "ram_check_fail");
        let ram_check_fail_addr = self
            .builder
            .build_alloca(self.word, "ram_check_fail_addr")
            .unwrap();

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
                if native_addr {
                    let v = self.builder.build_int_to_ptr(v, ptr_t, "target").unwrap();
                    self.builder.build_indirect_branch(v, &inst_bb).unwrap();
                } else {
                    self.builder.build_store(big_switch_to, v).unwrap();
                    self.builder
                        .build_unconditional_branch(big_switch_bb)
                        .unwrap();
                }
            };

            let get_reg = |r: &_| match r {
                Register::General(0) => word_0,
                Register::General(g) => self
                    .builder
                    .build_load(self.word, registers[*g as usize - 1], "reg_load")
                    .unwrap()
                    .into_int_value(),
                Register::Pc => self.word.const_int(pc as _, false),
                Register::Sp => self
                    .builder
                    .build_load(self.word, stack_ptr, "reg_load")
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

            let get_any_int = |v: &_| match v {
                AnyInt::Register(r) => get_reg(r),
                AnyInt::IntImm(i) => self.int_imm_to_addr_or_word(i, native_addr, &inst_bb),
            };

            let get_any_float = |v: &_| match v {
                AnyFloat::Register(r) => get_reg(r),
                AnyFloat::FloatImm(f) => {
                    // SAFETY: f.to_string() should return a valid float
                    // TODO: make float without using llvm utils
                    let f = unsafe { self.float.const_float_from_string(&f.to_string()) };
                    self.bit_ftoi(f)
                },
            };

            // TODO: use get_any_* instead of this
            let get_any = |v: &_| match v {
                Any::Register(r) => get_reg(r),
                Any::IntImm(i) => self.int_imm_to_addr_or_word(i, native_addr, &inst_bb),
                Any::FloatImm(f) => {
                    // SAFETY: f.to_string() should return a valid float
                    // TODO: make float without using llvm utils
                    let f = unsafe { self.float.const_float_from_string(&f.to_string()) };
                    self.bit_ftoi(f)
                },
                Any::Undefined => self.word.get_undef(),
            };

            let get_reg_ptr = |v: &_| match v {
                Register::General(0) => {
                    self.builder.build_store(temp_var, word_0).unwrap();
                    temp_var
                },
                Register::General(i) => registers[*i as usize - 1],
                Register::Pc => {
                    let pc = self.word.const_int(pc as _, false);
                    self.builder.build_store(temp_var, pc).unwrap();
                    temp_var
                },
                Register::Sp => stack_ptr,
            };

            let get_any_ptr = |v: &_| match v {
                Any::Register(r) => get_reg_ptr(r),
                _ => {
                    self.builder.build_store(temp_var, get_any(v)).unwrap();
                    temp_var
                },
            };

            let ram_gep = |a| unsafe {
                if self.options.bounds_safety {
                    let ok = self
                        .builder
                        .build_int_compare(IntPredicate::ULT, a, ram_size_const, "ram_check")
                        .unwrap();
                    let after = self.context.append_basic_block(main, "ram_check_ok");
                    self.builder.build_store(ram_check_fail_addr, a).unwrap();
                    self.builder
                        .build_conditional_branch(ok, after, ram_check_fail_bb)
                        .unwrap();
                    self.builder.position_at_end(after);
                }

                let a = if mach_t.get_bit_width() > self.word.get_bit_width() {
                    self.builder
                        .build_int_z_extend(a, mach_t, "ram_gep_zext")
                        .unwrap()
                } else {
                    a
                };

                self.builder
                    .build_in_bounds_gep(self.word, ram, &[a], "ram_gep")
                    .unwrap()
            };

            let read_ram = |a| {
                self.builder
                    .build_load(self.word, ram_gep(a), "read_ram")
                    .unwrap()
                    .into_int_value()
            };

            let write_ram = |a, v: values::IntValue| {
                self.builder.build_store(ram_gep(a), v).unwrap();
            };

            let cond_br = |c, d: &_| match d {
                AnyInt::Register(d) => {
                    let e = self.context.append_basic_block(main, "indir_jump");
                    self.builder
                        .build_conditional_branch(c, e, inst_bb[pc + 1])
                        .unwrap();
                    self.builder.position_at_end(e);
                    indir_jump_to(get_reg(d));
                },
                AnyInt::IntImm(d) => {
                    self.builder
                        .build_conditional_branch(
                            c,
                            inst_bb[usize::try_from(d).unwrap()],
                            inst_bb[pc + 1],
                        )
                        .unwrap();
                },
            };

            let uncond_br = |d: &_| match d {
                AnyInt::Register(d) => indir_jump_to(get_reg(d)),
                AnyInt::IntImm(d) => {
                    self.builder
                        .build_unconditional_branch(inst_bb[usize::try_from(d).unwrap()])
                        .unwrap();
                },
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
                    .build_load(self.word, stack_ptr, "stack_ptr_pre_dec")
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
                    .build_load(self.word, stack_ptr, "stack_ptr_pre_inc")
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
                (2op $d:tt $a:tt $b:tt $gen:expr) => {{
                    let $a = get_any_int($a);
                    let $b = get_any_int($b);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, $gen) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (1aop $d:tt $a:tt $gen:expr) => {{
                    let $a = get_any($a);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, $gen) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (1op $d:tt $a:tt $gen:expr) => {{
                    let $a = get_any_int($a);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, $gen) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (none $d:tt $gen:expr) => {{
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, $gen) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (2bc $d:tt $a:tt $b:tt $cond:expr) => {{
                    let $a = get_any_int($a);
                    let $b = get_any_int($b);
                    cond_br($cond, $d);
                }};
                (1bc $d:tt $a:tt $cond:expr) => {{
                    let $a = get_any_int($a);
                    let c = $cond;
                    cond_br(c, $d);
                }};
                (2set $d:tt $a:tt $b:tt $gen:expr) => {{
                    let $a = get_any_int($a);
                    let $b = get_any_int($b);
                    let c = self
                        .builder
                        .build_int_s_extend($gen, self.word, "set_sext")
                        .unwrap();
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, c) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (2fop $d:tt $a:tt $b:tt $gen:expr) => {{
                    let $a = get_any_float($a);
                    let $a = self.bit_itof($a);
                    let $b = get_any_float($b);
                    let $b = self.bit_itof($b);
                    let c = self.bit_ftoi($gen);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, c) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (1fop $d:tt $a:tt $gen:expr) => {{
                    let $a = get_any_float($a);
                    let $a = self.bit_itof($a);
                    let c = self.bit_ftoi($gen);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, c) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
                (ftoi $d:tt $a:tt $gen:expr) => {{
                    let $a = get_any_float($a);
                    let $a = self.bit_itof($a);
                    #[allow(clippy::blocks_in_conditions)]
                    if set_reg($d, $gen) {
                        self.builder
                            .build_unconditional_branch(inst_bb[pc + 1])
                            .unwrap();
                    }
                }};
            }

            match i {
                Instruction::Add(d, a, b) => gen!(2op d a b {
                    self.builder.build_int_add(a, b, "add").unwrap()
                }),
                Instruction::Rsh(d, a) => gen!(1op d a {
                        self.builder.build_right_shift(a, word_1, false, "rsh_urcl").unwrap()
                }),
                Instruction::Lod(d, a) => gen!(1op d a read_ram(a)),
                Instruction::Str(a, v) => {
                    let a = get_any_int(a);
                    let v = get_any(v);
                    write_ram(a, v);
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Bge(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::UGE, a, b, "urcl_bge_cmp").unwrap()
                }),
                Instruction::Nor(d, a, b) => gen!(2op d a b {
                    let c = self.builder.build_or(a, b, "nor_1").unwrap();
                    self.builder.build_not(c, "nor_2").unwrap()
                }),
                Instruction::Sub(d, a, b) => gen!(2op d a b {
                    self.builder.build_int_sub(a, b, "sub").unwrap()
                }),
                Instruction::Imm(d, v) | Instruction::Mov(d, v) => gen!(1aop d v v),
                Instruction::Jmp(i) => uncond_br(i),
                Instruction::Nop() => {
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Lsh(d, a) => gen!(1op d a {
                    self.builder.build_left_shift(a, word_1, "lsh").unwrap()
                }),
                Instruction::Inc(d, a) => gen!(1op d a {
                    self.builder.build_int_add(a, word_1, "inc").unwrap()
                }),
                Instruction::Dec(d, a) => gen!(1op d a {
                    self.builder.build_int_sub(a, word_1, "dec").unwrap()
                }),
                Instruction::Neg(d, a) => gen!(1op d a {
                    self.builder.build_int_neg(a, "neg").unwrap()
                }),
                Instruction::And(d, a, b) => gen!(2op d a b {
                    self.builder.build_and(a, b, "and").unwrap()
                }),
                Instruction::Or(d, a, b) => gen!(2op d a b {
                    self.builder.build_or(a, b, "or").unwrap()
                }),
                Instruction::Not(d, a) => gen!(1op d a {
                    self.builder.build_not(a, "not").unwrap()
                }),
                Instruction::Xnor(d, a, b) => gen!(2op d a b {
                    let c = self.builder.build_xor(a, b, "xnor_1").unwrap();
                    self.builder.build_not(c, "xnor_2").unwrap()
                }),
                Instruction::Xor(d, a, b) => gen!(2op d a b {
                    self.builder.build_xor(a, b, "xor").unwrap()
                }),
                Instruction::Nand(d, a, b) => gen!(2op d a b {
                    let c = self.builder.build_and(a, b, "nand_1").unwrap();
                    self.builder.build_not(c, "nand_2").unwrap()
                }),
                Instruction::Brl(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::ULT, a, b, "brl_cmp").unwrap()
                }),
                Instruction::Brg(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::UGT, a, b, "brg_cmp").unwrap()
                }),
                Instruction::Bre(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::EQ, a, b, "bre_cmp").unwrap()
                }),
                Instruction::Bne(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::NE, a, b, "bne_cmp").unwrap()
                }),
                Instruction::Bod(d, a) => gen!(1bc d a {
                    let b = self.builder.build_and(a, word_1, "bod_b0").unwrap();
                    self.builder.build_int_compare(IntPredicate::NE, b, word_0, "bod_cmp").unwrap()
                }),
                Instruction::Bev(d, a) => gen!(1bc d a {
                    let b = self.builder.build_and(a, word_1, "bev_b0").unwrap();
                    self.builder.build_int_compare(IntPredicate::EQ, b, word_0, "bev_cmp").unwrap()
                }),
                Instruction::Ble(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::ULE, a, b, "ble_cmp").unwrap()
                }),
                Instruction::Brz(d, a) => gen!(1bc d a {
                    self.builder.build_int_compare(IntPredicate::EQ, a, word_0, "brz_cmp").unwrap()
                }),
                Instruction::Bnz(d, a) => gen!(1bc d a {
                    self.builder.build_int_compare(IntPredicate::NE, a, word_0, "bnz_cmp").unwrap()
                }),
                Instruction::Brn(d, a) => gen!(1bc d a {
                    let s = self.builder
                        .build_right_shift(
                            a,
                            self.word.const_int((self.program.bits - 1).into(), false),
                            false,
                            "brn_sign",
                        )
                        .unwrap();
                    self.builder
                        .build_int_compare(IntPredicate::NE, s, word_0, "brn_cmp")
                        .unwrap()
                }),
                Instruction::Brp(d, a) => gen!(1bc d a {
                    let s = self.builder
                        .build_right_shift(
                            a,
                            self.word.const_int((self.program.bits - 1).into(), false),
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
                Instruction::Pop(d) => gen!(none d pop()),
                Instruction::Cal(a) => {
                    push(if native_addr {
                        unsafe { inst_bb[pc + 1].get_address() }
                            .unwrap()
                            .const_to_int(self.word)
                    } else {
                        self.word.const_int(pc as u64 + 1, false)
                    });
                    uncond_br(a);
                },
                Instruction::Ret() => {
                    indir_jump_to(pop());
                },
                Instruction::Hlt() => ret(),
                Instruction::Cpy(a, b) => {
                    write_ram(get_any_int(a), read_ram(get_any_int(b)));
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Brc(d, a, b) => {
                    gen!(2bc d a b {
                        let add_ov = intrinsics::Intrinsic::find("llvm.uadd.with.overflow")
                            .unwrap()
                            .get_declaration(&self.module, &[self.word.into()])
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
                    });
                },
                Instruction::Bnc(d, a, b) => {
                    gen!(2bc d a b {
                        let add_ov = intrinsics::Intrinsic::find("llvm.uadd.with.overflow")
                            .unwrap()
                            .get_declaration(&self.module, &[self.word.into()])
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
                    });
                },
                Instruction::Mlt(d, a, b) => gen!(2op d a b {
                    self.builder.build_int_mul(a, b, "mul").unwrap()
                }),
                Instruction::Div(d, a, b) => gen!(2op d a b {
                    self.builder.build_int_unsigned_div(a, b, "div").unwrap()
                }),
                Instruction::Mod(d, a, b) => gen!(2op d a b {
                    self.builder.build_int_unsigned_rem(a, b, "mod").unwrap()
                }),
                Instruction::Bsr(d, a, b) => gen!(2op d a b {
                    self.builder.build_right_shift(a, b, false, "bsr").unwrap()
                }),
                Instruction::Bsl(d, a, b) => gen!(2op d a b {
                    self.builder.build_left_shift(a, b, "bsl").unwrap()
                }),
                Instruction::Srs(d, a) => gen!(1op d a {
                    self.builder.build_right_shift(a, word_1, true, "srs").unwrap()
                }),
                Instruction::Bss(d, a, b) => gen!(2op d a b {
                    self.builder.build_right_shift(a, b, true, "bss").unwrap()
                }),
                Instruction::SetE(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::EQ, a, b, "sete_cmp").unwrap()
                }),
                Instruction::SetNe(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::NE, a, b, "setne_cmp").unwrap()
                }),
                Instruction::SetG(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::UGT, a, b, "setg_cmp").unwrap()
                }),
                Instruction::SetL(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::ULT, a, b, "setl_cmp").unwrap()
                }),
                Instruction::SetGe(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::UGE, a, b, "setge_cmp").unwrap()
                }),
                Instruction::SetLe(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::ULE, a, b, "setle_cmp").unwrap()
                }),
                Instruction::SetC(d, a, b) => {
                    gen!(2set d a b {
                        let add_ov = intrinsics::Intrinsic::find("llvm.uadd.with.overflow")
                            .unwrap()
                            .get_declaration(&self.module, &[self.word.into()])
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
                    });
                },
                Instruction::SetNc(d, a, b) => {
                    gen!(2set d a b {
                        let add_ov = intrinsics::Intrinsic::find("llvm.uadd.with.overflow")
                            .unwrap()
                            .get_declaration(&self.module, &[self.word.into()])
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
                    });
                },
                Instruction::LLod(d, a, b) => gen!(2op d a b {
                    let addr = self.builder.build_int_add(a, b, "llod_addr").unwrap();
                    read_ram(addr)
                }),
                Instruction::LStr(a, o, v) => {
                    let a = get_any_int(a);
                    let o = get_any_int(o);
                    let v = get_any(v);
                    let addr = self.builder.build_int_add(a, o, "lstr_addr").unwrap();
                    write_ram(addr, v);
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::SDiv(d, a, b) => gen!(2op d a b {
                    self.builder.build_int_signed_div(a, b, "sdiv").unwrap()
                }),
                Instruction::SBrl(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::SLT, a, b, "sbrl_cmp").unwrap()
                }),
                Instruction::SBrg(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::SGT, a, b, "sbrg_cmp").unwrap()
                }),
                Instruction::SBle(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::SLE, a, b, "sble_cmp").unwrap()
                }),
                Instruction::SBge(d, a, b) => gen!(2bc d a b {
                    self.builder.build_int_compare(IntPredicate::SGE, a, b, "sbge_cmp").unwrap()
                }),
                Instruction::SSetG(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::SGT, a, b, "ssetg_cmp").unwrap()
                }),
                Instruction::SSetL(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::SLT, a, b, "ssetl_cmp").unwrap()
                }),
                Instruction::SSetGe(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::SGE, a, b, "ssetge_cmp").unwrap()
                }),
                Instruction::SSetLe(d, a, b) => gen!(2set d a b {
                    self.builder.build_int_compare(IntPredicate::SLE, a, b, "ssetle_cmp").unwrap()
                }),
                Instruction::Abs(d, a) => gen!(1op d a {
                    let abs = intrinsics::Intrinsic::find("llvm.abs")
                        .unwrap()
                        .get_declaration(&self.module, &[self.word.into()])
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
                        .build_call(
                            port_out,
                            &[self.zext_or_trunc(p, i8_t).into(), v.into()],
                            "out",
                        )
                        .unwrap();
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::In(r, p) => {
                    let r = get_reg_ptr(r);
                    let p = get_any(p);

                    self.builder
                        .build_call(port_in, &[r.into(), self.zext_or_trunc(p, i8_t).into()], "")
                        .unwrap();
                    self.builder
                        .build_unconditional_branch(inst_bb[pc + 1])
                        .unwrap();
                },
                Instruction::Umlt(d, a, b) => gen!(2op d a b {
                    let ext_t = self.context.custom_width_int_type(self.program.bits * 2);
                    let a = self.builder.build_int_z_extend(a, ext_t, "umlt_a_zext").unwrap();
                    let b = self.builder.build_int_z_extend(b, ext_t, "umlt_b_zext").unwrap();
                    let r = self.builder.build_int_mul(a, b, "umlt_mlt").unwrap();
                    let s = self.builder.build_right_shift(r, ext_t.const_int(self.program.bits.into(), false), false, "uml_rsh").unwrap();
                    self.builder.build_int_truncate(s, self.word, "umlt_trunc").unwrap()
                }),
                Instruction::SUmlt(d, a, b) => gen!(2op d a b {
                    let ext_t = self.context.custom_width_int_type(self.program.bits * 2);
                    let a = self.builder.build_int_s_extend(a, ext_t, "sumlt_a_sext").unwrap();
                    let b = self.builder.build_int_s_extend(b, ext_t, "sumlt_b_sext").unwrap();
                    let r = self.builder.build_int_mul(a, b, "sumlt_mlt").unwrap();
                    let s = self.builder.build_right_shift(r, ext_t.const_int(self.program.bits.into(), false), false, "suml_rsh").unwrap();
                    self.builder.build_int_truncate(s, self.word, "sumlt_trunc").unwrap()
                }),
                Instruction::ItoF(d, a) => gen!(1op d a {
                    let f = self.builder.build_signed_int_to_float(a, self.float, "itof").unwrap();
                    self.bit_ftoi(f)
                }),
                Instruction::FtoI(d, a) => gen!(ftoi d a {
                    self.builder.build_float_to_signed_int(a, self.word, "ftoi").unwrap()
                }),
                Instruction::FRtoI(d, a) => gen!(ftoi d a {
                    let round = intrinsics::Intrinsic::find("llvm.round")
                        .unwrap()
                        .get_declaration(&self.module, &[self.float.into()])
                        .unwrap();
                    let r = self.builder.build_call(round, &[a.into()], "frtoi_round")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_float_value();

                    self.builder.build_float_to_signed_int(r, self.word, "frtoi").unwrap()
                }),
                Instruction::FAdd(d, a, b) => gen!(2fop d a b {
                    self.builder.build_float_add(a, b, "fadd").unwrap()
                }),
                Instruction::FSub(d, a, b) => gen!(2fop d a b {
                    self.builder.build_float_sub(a, b, "fsub").unwrap()
                }),
                Instruction::FMlt(d, a, b) => gen!(2fop d a b {
                    self.builder.build_float_mul(a, b, "fmlt").unwrap()
                }),
                Instruction::FDiv(d, a, b) => gen!(2fop d a b {
                    self.builder.build_float_div(a, b, "fdiv").unwrap()
                }),
                Instruction::FSqrt(d, a) => gen!(1fop d a {
                    let round = intrinsics::Intrinsic::find("llvm.sqrt")
                        .unwrap()
                        .get_declaration(&self.module, &[self.float.into()])
                        .unwrap();
                    self.builder.build_call(round, &[a.into()], "fsqrt")
                        .unwrap()
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_float_value()
                }),
                Instruction::FAbs(d, a) => gen!(1fop d a {
                    let round = intrinsics::Intrinsic::find("llvm.fabs")
                        .unwrap()
                        .get_declaration(&self.module, &[self.float.into()])
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
        if !native_addr {
            let v = self
                .builder
                .build_load(self.word, big_switch_to, "get_addr")
                .unwrap();
            self.builder
                .build_switch(
                    v.into_int_value(),
                    *inst_bb.last().unwrap(),
                    &inst_bb
                        .iter()
                        .enumerate()
                        .map(|(i, b)| (self.word.const_int(i as _, false), *b))
                        .collect::<Vec<_>>(),
                )
                .unwrap();
        } else {
            ret();
        }

        self.builder.position_at_end(ram_check_fail_bb);
        if self.options.bounds_safety {
            let ram_check_fail = self.options.bounds_safety.then(|| {
                self.module.add_function(
                    "memory_oob",
                    void_t.fn_type(&[ptr_t.into()], false),
                    Some(module::Linkage::External),
                )
            });

            self.builder
                .build_call(ram_check_fail.unwrap(), &[ram_check_fail_addr.into()], "")
                .unwrap();
        }
        ret();

        self.di_builder.finalize();
    }
}
