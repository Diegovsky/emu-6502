use std::{cmp::Ordering, collections::HashMap, mem::size_of};

// use derive_more::{ From, TryInto };

use bitflags::Flags;
use bytemuck::Pod;
use fehler::throws;

pub(crate) mod utils;
mod instructions;
mod regs;
#[cfg(test)]
mod test;

use instructions::{
    AccOp, IndexType, Instruction, MemAddressMode, RegType, Value, ValueSource
};
use regs::{Addr, FlagsRegister, Registers, Word};

pub use regs::Reg;

use crate::instructions::UnOp;
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

fn relative_slice<T>(slice: &[T], start: usize, offset: usize) -> &[T] {
    let end = start + offset;
    &slice[start..end]
}

/* #[derive(From, TryInto)]
pub enum Num {
    Word(Word),
    Addr(Addr)
} */

/// A common trait all numbers implement.
pub trait Num: Pod {}
impl<N> Num for N where N: Pod {}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Interrupt {
    NMI,
    Reset,
    Irq,
    Brk
}

impl Interrupt {
    pub fn vector(self) -> Addr {
        match self {
            Self::NMI => 0xFFFA,
            Self::Reset => 0xFFFC,
            Self::Irq | Self::Brk => 0xFFFE
        }
    }
}


#[derive(Debug)]
struct State {
    registers: Registers,
    memory: Box<[Word; Addr::MAX as usize+1]>,
}


impl State {
    pub fn new() -> Self {
        Self {
            registers: Registers::default(),
            memory: Box::new([0; Addr::MAX as usize+1]),
        }
    }

    #[inline]
    pub fn set_vector(&mut self, interrupt: Interrupt, addr: Addr) {
        self.mem_write(interrupt.vector(), addr);
    }

    const STACK_START: Addr = 0x100;
    pub const fn stack_addr(&self) -> Addr {
        self.registers.s.0 as Addr + Self::STACK_START
    }

    #[inline]
    pub fn stack_push<N: Num>(&mut self, num: N) {
        let addr = self.stack_addr();
        self.mem_write(addr, num);
        self.registers.s -= size_of::<N>() as _;
    }

    pub fn stack_pop<N: Num>(&mut self) -> Result<N> {
        let val = self.mem_read::<N>(self.stack_addr());
        self.registers.s += size_of::<N>() as u8;
        Ok(val)
    }

    #[inline]
    pub fn mem_write<N: Num>(&mut self, addr: Addr, num: N) {
        let bytes = bytemuck::bytes_of(&num);
        self.memory[addr as usize..][..bytes.len()].copy_from_slice(bytes);
    }

    #[inline]
    #[track_caller]
    pub fn mem_read<N: Num>(&self, addr: Addr) -> N {
        let addr = addr as usize;
        *bytemuck::from_bytes(relative_slice(&*self.memory, addr, size_of::<N>()))
    }

    fn reg_write(&mut self, reg: RegType, new: Word) {
        let flags = &mut self.registers.p;
        flags.set(FlagsRegister::Zero, new == 0);
        flags.set(FlagsRegister::Negative, new >> 7 == 1);
        *self.registers.get_register_mut(reg) = Reg(new);
    }

    fn reg_read(&self, reg: RegType) -> Word {
        self.registers.get_register(reg).0
    }

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
                            self.mem_read((self.registers.x + addr) as Addr)
                        }
                        IndexType::Y => {
                            let addr = self.mem_read::<Addr>(addr as Addr);
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

    fn value_write(&mut self, value: Value, num: Word) {
        match value {
            Value::Implied(reg) => self.reg_write(reg, num),
            Value::Mem(mem) => self.mem_write(self.resolve_addr(mem), num),
            Value::Immediate(_) => unreachable!(),
        }
    }

    fn value_read<I>(&self, value_source: ValueSource<I>) -> Word where I: Into<Word> {
        match value_source {
            ValueSource::<I>::Immediate(val) => val.into(),
            ValueSource::<I>::Implied(reg) => self.registers.get_register(reg).0,
            ValueSource::<I>::Mem(mem) => self.mem_read(self.resolve_addr(mem))
        }
    }

    pub fn receive_interrupt(&mut self, interrupt: Interrupt) {
        if interrupt != Interrupt::Reset {
            let pc = self.registers.pc;
            self.stack_push(pc);
        }
        self.registers.pc = self.mem_read(interrupt.vector());
    }

    fn add(&mut self, reg: RegType, rhs: Word) {
        let lhs = self.reg_read(reg);
        let flags = &mut self.registers.p;
        let (result, wrapped) = lhs.overflowing_add(rhs + (*flags&FlagsRegister::Carry).bits());
        flags.set(FlagsRegister::Carry, wrapped);
        self.reg_write(reg, result);
    }

    fn sub(&mut self, reg: RegType, rhs: Word) {
        let lhs = self.reg_read(reg);
        let flags = &mut self.registers.p;
        let borrow = (*flags&FlagsRegister::Carry).bits();
        let (result, wrapped) = lhs.overflowing_sub(rhs - borrow);
        flags.set(FlagsRegister::Carry, !wrapped);
        self.reg_write(reg, dbg!(result));
    }

    pub fn execute(&mut self, instruction: Instruction) -> bool {
        let mut jumped = false;
        match instruction {
            Instruction::Break => {self.receive_interrupt(Interrupt::Brk); jumped = true},
            Instruction::AccOp{ operand, op} => {
                let rhs = self.value_read(operand);
                match op {
                    AccOp::Add => self.add(RegType::A, rhs),
                    AccOp::Sub => self.sub(RegType::A, rhs),
                    o => todo!("{o:?}"),
                }
            }
            Instruction::Jump { addr, indirect, with_return } => {
                if with_return {
                    self.stack_push(self.registers.pc);
                }
                if !indirect {
                    self.registers.pc = addr;
                } else {
                    self.registers.pc = self.mem_read(addr);
                }
                jumped = true;
            }
            Instruction::Noop => (),
            Instruction::ChangeFlag(flag, val) => self.registers.p.set(flag, val),
            Instruction::Transfer { from, to } => self.value_write(to, self.value_read(from)),
            Instruction::TransferXS { rev } => {
                if rev {
                    self.reg_write(RegType::X, self.registers.s.0);
                } else {
                    self.registers.s.0 = self.reg_read(RegType::X);
                }
            }
            Instruction::Branch { bit, if_set, offset } => {
                if self.registers.p.intersects(bit) == if_set {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(offset as _);
                }
            }
            Instruction::UnOp { operand, operator } => {
                let val = self.value_read(operand);
                let res = match operator {
                    UnOp::Increment => val.wrapping_add(1),
                    UnOp::Decrement => val.wrapping_sub(1),
                    UnOp::ShiftLeft => val<<1,
                    UnOp::ShiftRight => val>>1,
                    o => unimplemented!("{o:?}"),
                };
                self.value_write(operand, res);
            },
            Instruction::Compare { reg, operand } => {
                // Save register value
                let lhs = self.reg_read(reg);

                self.registers.p.remove(FlagsRegister::Carry);
                self.sub(reg, self.value_read(operand));
                // Restore register value
                self.registers.get_register_mut(reg).0 = lhs;
            }
            ins => panic!("Missing instruction impl: {ins:?}"),
        };
        jumped
    }

    pub fn execute_next(&mut self) -> Result {
        let pc = self.registers.pc as usize;
        // get current instruction
        let instr = &self.memory[pc..pc+3];
        let instr = match Instruction::decode(instr.try_into().unwrap()) {
            Some(i) => i,
            None => return Err(Error::InvalidInstruction(pc as u16)),
        };
        // println!("INSTRUCTION: {}", format!("{instr:?}").split_once(' ').unwrap().0);
        println!("{instr:?}");
        let jumped = self.execute(instr);
        if !jumped {
            self.registers.pc += instr.bytecount() as u16;
        }
        Ok(())
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
    let ops: Vec<Option<&str>> = serde_json::from_str(include_str!("../resources/ops.json")).unwrap();
    let asm = include_bytes!("../6502_tests/bin_files/6502_functional_test.bin");
    state.flash_memory(asm);
    state.registers.pc = 0x400;

    let mut i = 0;
    let mut last_pc = state.registers.pc;
    loop {
        println!("step {i} {:?}", state.registers);
        if let Err(e) = state.execute_next() {
            eprintln!("ERROR: {e}");
            break
        }
        if state.registers.pc == last_pc {
            eprintln!("ERROR");
            break
        }
        i += 1;
        last_pc = state.registers.pc;
    }

    let search_range = 10;
    println!("Possible instructions around PC");
    for offset in -search_range/2..search_range/2 {
        let addr = state.registers.pc as i32 + offset;
        let val = state.mem_read::<Word>(addr as u16);
        println!("PC{}{}: {:?}", if offset < 0 { '-' } else {'+'}, offset.abs(), ops[val as usize]);
    }
    println!("Last instruction should have been: {:?}", ops[state.mem_read::<Word>(state.registers.pc) as usize]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cmp_zero_flags_test() {
        let mut state = State::new();
        state.reg_write(RegType::A, 0);
        state.execute(Instruction::Compare { reg: RegType::A, operand: ValueSource::Immediate(0) });
        let flags = state.registers.p;
        assert_eq!(flags, FlagsRegister::Zero|FlagsRegister::Carry);
    }

    #[test]
    fn cmp_lt_flags_test() {
        let mut state = State::new();
        state.reg_write(RegType::A, 0);
        state.execute(Instruction::Compare { reg: RegType::A, operand: ValueSource::Immediate(10) });
        let flags = state.registers.p;
        assert_eq!(flags, FlagsRegister::Negative);
    }

    #[test]
    fn cmp_gt_flags_test() {
        let mut state = State::new();
        state.reg_write(RegType::A, 10);
        state.execute(Instruction::Compare { reg: RegType::A, operand: ValueSource::Immediate(0) });
        let flags = state.registers.p;
        assert_eq!(flags, FlagsRegister::Carry);
    }
}
