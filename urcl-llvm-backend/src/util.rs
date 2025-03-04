use crate::{float, float_size::FloatSize, Codegen};
use inkwell::{basic_block, debug_info, passes, support, targets, types, values, OptimizationLevel};
use urcl_ast::{AnyImm, IntImm};

impl<'a> Codegen<'a> {
    pub fn di_basic_type(&self, name: &str, size: u64) -> debug_info::DIBasicType<'a> {
        self.di_builder.create_basic_type(name, size, 7, 0).unwrap()
    }

    pub fn int_imm_to_word(&self, i: &IntImm) -> values::IntValue<'a> {
        match i {
            IntImm::Value(v) if v.is_zero() => self.word.const_zero(),
            IntImm::Value(v) => {
                let unsigned = self.word.const_int_arbitrary_precision(v.as_sign_words().1);

                if v.sign() == dashu::base::Sign::Negative {
                    unsigned.const_neg()
                } else {
                    unsigned
                }
            },
            IntImm::InstLoc(l) => self.word.const_int(*l as _, false),
        }
    }

    pub fn int_imm_to_addr_or_word(
        &self,
        i: &IntImm,
        native_addr: bool,
        inst_bb: &[basic_block::BasicBlock<'a>],
    ) -> values::IntValue<'a> {
        if native_addr {
            match i {
                IntImm::Value(v) => {
                    let unsigned = self.word.const_int_arbitrary_precision(v.as_sign_words().1);

                    if v.sign() == dashu::base::Sign::Negative {
                        unsigned.const_neg()
                    } else {
                        unsigned
                    }
                },
                IntImm::InstLoc(l) => unsafe { inst_bb[*l].get_address() }
                    .unwrap()
                    .const_to_int(self.word),
            }
        } else {
            self.int_imm_to_word(i)
        }
    }

    pub fn imm_to_addr_or_word(
        &self,
        i: &AnyImm,
        native_addr: bool,
        inst_bb: &[basic_block::BasicBlock<'a>],
    ) -> values::IntValue<'a> {
        match i {
            AnyImm::IntImm(i) => self.int_imm_to_addr_or_word(i, native_addr, inst_bb),
            AnyImm::FloatImm(f) => {
                match self.float_size {
                    FloatSize::_16 => {
                        let bits = float::u16::from_dec(f.0.clone());
                        println!("{bits:04x}");
                        self.word.const_int(bits as u64, false)
                    },
                    FloatSize::_32 => {
                        let bits = f.0.to_f32().value().to_bits();
                        self.word.const_int(bits as u64, false)
                    },
                    FloatSize::_64 => {
                        let bits = f.0.to_f64().value().to_bits();
                        self.word.const_int(bits, false)
                    },
                    FloatSize::_128 => {
                        let bits = float::u128::from_dec(f.0.clone());
                        println!("{bits:032x}");
                        self.word.const_int_arbitrary_precision(&[bits as u64, (bits >> 64) as u64])
                    },
                }
            },
            AnyImm::Undefined => self.word.get_undef(),
        }
    }

    pub fn zext_or_trunc(&self, i: values::IntValue<'a>, t: types::IntType<'a>) -> values::IntValue<'a> {
        use core::cmp::Ordering as O;

        match i.get_type().get_bit_width().cmp(&t.get_bit_width()) {
            O::Greater => self.builder.build_int_truncate(i, t, "conv_trunc").unwrap(),
            O::Less => self.builder.build_int_z_extend(i, t, "conv_zext").unwrap(),
            O::Equal => i,
        }
    }

    pub fn bit_itof(&self, i: values::IntValue<'a>) -> values::FloatValue<'a> {
        let float_it = self.context.custom_width_int_type(self.float_size as _);
        let i = self.zext_or_trunc(i, float_it);
        self.builder
            .build_bit_cast(i, self.float, "bitw_itof")
            .unwrap()
            .into_float_value()
    }

    pub fn bit_ftoi(&self, f: values::FloatValue<'a>) -> values::IntValue<'a> {
        let float_it = self.context.custom_width_int_type(self.float_size as _);
        let i = self
            .builder
            .build_bit_cast(f, float_it, "bitw_ftoi")
            .unwrap()
            .into_int_value();

        self.zext_or_trunc(i, self.word)
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
            .map_or_else(|_| String::new(), |_| suffix);

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

        if s.ends_with(',') {
            s.pop();
        }

        s
    }
}
