use fehler::throws;

pub(crate) mod utils;
mod instructions;
mod regs;
#[cfg(test)]
mod test;

use instructions::{
    ArithmeticOp, IndexType, Instruction, JmpAddrMode, MemAddressMode, RegType, StackOperand, Value, ValueSource
};
use regs::{Addr, FlagsRegister, Registers, Word};

pub use regs::Reg;
#[derive(Debug, Clone, Copy)]
pub enum VmError {
    InvalidInstruction(Addr),
}

type Error = VmError;

pub type Result<T = (), E = VmError> = std::prelude::v1::Result<T, E>;

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidInstruction(val) => write!(f, "Invalid instruction {val:x}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AddrOrWord {
    Addr(Addr),
    Word(Word)
}

fn to_addr(slice: &[u8]) -> Addr {
    Addr::from_le_bytes(slice.try_into().unwrap())
}

#[derive(Debug)]
struct State {
    registers: Registers,
    memory: Box<[Word; Addr::MAX as usize]>,
}


impl State {
    pub fn new() -> Self {
        Self {
            registers: Registers::default(),
            memory: Box::new([0; Addr::MAX as usize]),
        }
    }

    #[throws]
    #[inline]
    pub fn set_reset(&mut self, addr: Addr) {
        self.mem_write_addr(0xFFFC, addr)?;
    }

    #[throws]
    #[inline]
    pub fn set_nmi(&mut self, addr: Addr) {
        self.mem_write_addr(0xFFFA, addr)?;
    }

    #[throws]
    #[inline]
    pub fn set_irq(&mut self, addr: Addr) {
        self.mem_write_addr(0xFFFE, addr)?;
    }

    const STACK_START: Addr = 0x100;
    pub const fn stack_addr(&self) -> Addr {
        self.registers.s.0 as Addr + Self::STACK_START
    }

    #[throws]
    pub fn stack_push(&mut self, val: Word) {
        self.mem_write(self.stack_addr(), val)?;
        self.registers.s -= 1;
    }

    pub fn stack_pop(&mut self) -> Result<Word> {
        let val = self.mem_read(self.stack_addr())?;
        self.registers.s += 1;
        Ok(val.into())
    }

    #[throws]
    #[inline]
    pub fn mem_write(&mut self, addr: Addr, val: Word) {
        self.memory[addr as usize] = val;
    }

    #[throws]
    #[inline]
    pub fn mem_read(&self, addr: Addr) -> u8 {
        self.memory[addr as usize]
    }

    #[throws]
    #[inline]
    pub fn mem_write_addr(&mut self, addr: Addr, val: Addr) {
        let [lo, hi] = val.to_le_bytes();
        self.mem_write(addr, lo)?;
        self.mem_write(addr + 1, hi)?;
    }

    #[throws]
    #[inline]
    pub fn mem_read_addr(&self, addr: Addr) -> Addr {
        let lo = self.mem_read(addr)?;
        let hi = self.mem_read(addr + 1)?;
        Addr::from_ne_bytes([lo, hi])
    }

    #[throws]
    fn reg_write(&mut self, reg: RegType, new: Word) {
        let reg_value = self.registers.get_register(reg);
        let flags = &mut self.registers.p;
        flags.set(FlagsRegister::Z, reg_value == 0 || new == reg_value.0);
        flags.set(FlagsRegister::N, reg_value < new);
        *self.registers.get_register_mut(reg) = Reg(new);
    }

    #[throws]
    fn reg_read(&self, reg: RegType) -> Word {
        self.registers.get_register(reg).0
    }

    #[throws]
    fn resolve_addr(&self, mem: MemAddressMode) -> Addr {
        match mem {
            MemAddressMode::Zero {
                indirect,
                addr,
                index,
            } => {
                if !indirect {
                    let addr = index
                        .map(|index| self.registers.get_index(index).0)
                        .unwrap_or_default()
                        + addr;
                    addr as Addr
                } else {
                    match index.expect("todo: indirect absolute") {
                        IndexType::X => {
                            self.mem_read_addr((self.registers.x + addr) as Addr)?
                        }
                        IndexType::Y => {
                            let addr = self.mem_read_addr(addr as Addr)?;
                            addr + self.registers.y.0 as Addr
                        }
                    }
                }
            }
            MemAddressMode::Absolute { addr, index } => {
                index
                    .map(|index| self.registers.get_index(index).0)
                    .unwrap_or_default() as Addr
                    + addr
            }
        }
    }

    #[throws]
    fn value_write(&mut self, value_source: Value, value: Word) {
        match value_source {
            Value::Implied(reg) => self.registers.get_register_mut(reg).0 = value,
            Value::Mem(mem) => self.mem_write(self.resolve_addr(mem)?, value)?,
            Value::Immediate(_) => unreachable!(),
        }
    }

    #[throws]
    fn value_read<I>(&self, value_source: ValueSource<I>) -> Word where I: Into<Word> {
        match value_source {
            ValueSource::<I>::Immediate(val) => val.into(),
            ValueSource::<I>::Implied(reg) => self.registers.get_register(reg).0,
            ValueSource::<I>::Mem(mem) => self.mem_read(self.resolve_addr(mem)?)?
        }
    }

    #[throws]
    fn jump_relative(&mut self, offset: Word) {
        // offset is actually a signed number;
        let offset = i8::from_ne_bytes(offset.to_ne_bytes());
        self.registers.pc = self.registers.pc.overflowing_add_signed(offset as i16).0;
    }

    #[throws]
    fn jump_abs(&mut self, addr: Addr) {
        self.registers.pc = addr;
    }

    #[throws]
    pub fn execute(&mut self) -> bool {
        let pc = self.registers.pc as usize;
        // get current instruction
        let instr = &self.memory[pc..pc+3];
        let instr = Instruction::decode(instr.try_into().unwrap());
        let mut jumped = false;
        match instr {
            Instruction::Load { reg, from } => {
                self.reg_write(reg, self.value_read(from)?)?;
            }
            Instruction::Store { reg, into } => {
                let r = self.reg_read(reg)?;
                let addr = self.resolve_addr(into)?;
                self.mem_write(addr, r)?;
            }
            Instruction::Arithmetic{ lhs, rhs, op} => {
                let result = {
                    let lhs = self.value_read(lhs)?;
                    let rhs = self.value_read(rhs)?;
                    match op {
                        ArithmeticOp::Add => lhs + rhs,
                        o => todo!("{o:?}"),
                    }
                };
                self.value_write(lhs, result)?;
            }
            Instruction::StackPop(operand) => {
                let value = self.stack_pop()?;
                match operand {
                    StackOperand::A => self.reg_write(RegType::A, value)?,
                    StackOperand::P => {
                        self.registers.p = FlagsRegister::from_bits(value).unwrap();
                    }
                }
            }
            Instruction::StackPush(operand) => {
                let value = match operand {
                    StackOperand::A => self.registers.a.0,
                    StackOperand::P => self.registers.p.bits(),
                };
                self.stack_push(value)?;
            }
            Instruction::Jump { addr, with_return } => {
                let addr = match addr {
                    JmpAddrMode::Absolute(val) => val,
                    JmpAddrMode::Indirect(addr) => self.mem_read_addr(addr)?,
                };
                if with_return {
                    let [lo, hi] = self.registers.pc.to_le_bytes();
                    self.stack_push(hi)?;
                    self.stack_push(lo)?;
                }
                self.registers.pc = addr;
                jumped = true;
            }
            Instruction::Noop => (),
            ins => {
                println!("Missing instruction impl: {ins:?}")
            }
            /* InstructionType::AddIm => {
            let (result, overflow) = self.registers.a.checked_add(args[0]);
            if overflow {
            self.registers.p.insert(FlagsRegister::C);
            }
            self.register_write(RegType::A, result.0)?;
            }
            InstructionType::JumpWithReturn => {
            let next = self.registers.pc + i.bytecount()-1;
            let [lo, hi] = next.to_le_bytes();
            self.stack_push(lo)?;
            self.stack_push(hi)?;
            self.jump_abs(to_addr(args))?;
            jumped = true;
            }
            InstructionType::ReturnFromSub => {
            let lo = self.stack_pop()?;
            let hi = self.stack_pop()?;
            let addr = Addr::from_le_bytes([lo, hi]);
            self.jump_abs(addr)?;
            jumped = true;
            }
            InstructionType::Noop => (), */
        }
        if !jumped {
            self.registers.pc += instr.bytecount() as u16;
        }
        true
    }

    pub fn flash_memory(&mut self, bytes: &[u8]) {
        let mem = self.memory.as_mut_slice();
        mem.copy_from_slice(&bytes[..mem.len().min(bytes.len())]);
    }
}

pub const fn u8(i: i8) -> u8 {
    u8::from_ne_bytes(i.to_ne_bytes())
}



fn main() {
    let mut state = State::new();
    let asm = include_bytes!("../6502_tests/bin_files/6502_functional_test.bin");
    state.flash_memory(asm);

    let mut i = 0;
    while state.execute().unwrap() {
        println!("step {i} {:?}", state.registers);
        i += 1;
    }
    println!("{state:?}");
}
