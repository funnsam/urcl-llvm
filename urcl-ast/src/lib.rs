use core::fmt;

pub use any::*;
use dashu::Natural;
pub use imm::*;
pub use inst::*;
pub use port::*;
pub use reg::*;

mod any;
mod imm;
mod inst;
mod port;
mod reg;

#[derive(Debug, Clone)]
pub struct Program {
    pub bits: usize,
    pub registers: usize,

    pub min_stack: usize,
    pub min_heap: usize,

    pub heap_size: usize,

    // body
    pub instructions: Vec<(Instruction, core::ops::Range<usize>)>,
    pub dw: Vec<Immediate>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "bits {}", self.bits)?;
        writeln!(f, "minreg {}", self.registers)?;
        writeln!(f, "minstack {}", self.min_stack)?;
        writeln!(f, "minheap {}", self.min_heap)?;
        writeln!(f, "// target heap {}", self.heap_size)?;
        writeln!(f, "\n// inst")?;

        for (i, _) in self.instructions.iter() {
            i.fmt_san(f, self.bits)?;
            writeln!(f)?;
        }

        writeln!(f, "\n// dw")?;
        for w in self.dw.iter() {
            writeln!(f, "dw {w:?}")?;
        }

        Ok(())
    }
}
