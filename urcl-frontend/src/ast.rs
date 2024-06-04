use core::fmt;

#[derive(Debug, Clone)]
pub struct Program {
    // headers
    pub bits: usize,
    pub registers: usize,
    pub stack_size: usize,
    pub heap_size: usize,

    // body
    pub instructions: Vec<Instruction>,
}

macro_rules! instr {
    ($($name: ident [$($arg: tt)*]),* $(,)?) => {
        #[derive(Debug, Clone)]
        pub enum Instruction {
            $($name($(instr!($arg)),*)),*
        }

        impl Instruction {
            pub(crate) fn properties(i: &str) -> Option<InstProperties> {
                use OperandKind::*;

                match i.to_ascii_lowercase().as_str() {
                    $(
                        casey::lower!(stringify!($name)) => Some(InstProperties {
                            operands: &[$(instr!($arg)),*],
                        }),
                    )*
                    _ => None,
                }
            }

            pub(crate) fn construct(i: &str, mut o: Vec<Any>) -> Self {
                use Instruction::*;
                o.reverse();

                match i.to_ascii_lowercase().as_str() {
                    $(
                        casey::lower!(stringify!($name)) => $name($(instr!(fromany $arg o.pop().unwrap())),*),
                    )*
                    _ => panic!(),
                }
            }
        }
    };

    (r) => { Register };
    (i) => { Immediate };
    (a) => { Any };

    (fromany r $e: expr) => {
        match $e {
            Any::Register(r) => r,
            _ => panic!(),
        }
    };
    (fromany i $e: expr) => {
        match $e {
            Any::Immediate(i) => i,
            _ => panic!(),
        }
    };
    (fromany a $e: expr) => {
        $e
    };
}

instr! {
    Add [r a a],
    Rsh [r a],
    Lod [r a],
    Str [a a],
    Bge [a a a],
    Nor [r a a],
    Imm [r a],
    Out [a a],
    Hlt [],
}

#[derive(Clone)]
pub struct Register(pub u128);

#[derive(Clone)]
pub struct Immediate(pub u128);

#[derive(Clone)]
pub enum Any {
    Register(Register),
    Immediate(Immediate),
}

impl fmt::Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl fmt::Debug for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for Any {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Register(r) => write!(f, "{r:?}"),
            Self::Immediate(r) => write!(f, "{r:?}"),
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self {
            bits: 8,
            registers: 8,
            stack_size: 8,
            heap_size: 16,

            instructions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct InstProperties {
    pub operands: &'static [OperandKind],
}

#[derive(Debug, Clone)]
pub(crate) enum OperandKind {
    Register,
    Immediate,
    Any,
}
