use macro_rules_attribute::apply;

use crate::regs::{Addr, Word};
use crate::utils::multivalue;

mod decode;

#[apply(multivalue)]
pub enum IndexType {
    X,
    Y,
}

#[apply(multivalue)]
pub enum RegType {
    A,
    S,
    Index(IndexType),
}

impl RegType {
    pub const X: Self = RegType::Index(IndexType::X);
    pub const Y: Self = RegType::Index(IndexType::Y);
}

#[apply(multivalue)]
pub enum MemAddressMode {
    Absolute {
        addr: Addr,
        index: Option<IndexType>,
    },
    Zero {
        addr: Word,
        indirect: bool,
        index: Option<IndexType>,
    },
}

impl MemAddressMode {
    pub const fn zpg(addr: Word) -> Self {
        MemAddressMode::Zero { addr, indirect: false, index: None }
    }
    pub const fn zpg_indexed(addr: Word, index: IndexType) -> Self {
        MemAddressMode::Zero { addr, indirect: false, index: Some(index) }
    }
    pub const fn abs(addr: Addr) -> Self {
        Self::Absolute { addr, index: None }
    }
    pub const fn abs_indexed(addr: Addr, index: IndexType) -> Self {
        MemAddressMode::Absolute { addr, index: Some(index) }
    }
    pub const fn bytecount(&self) -> usize {
        match self {
            MemAddressMode::Absolute { .. } => 2,
            MemAddressMode::Zero { .. } => 1,
        }
    }
}

#[apply(multivalue)]
pub enum ValueSource<I = Word> {
    Immediate(I),
    Implied(RegType),
    Mem(MemAddressMode),
}

impl ValueSource {
    pub fn into_value(self) -> Option<Value> {
        match self {
            Self::Mem(m) => Some(Value::Mem(m)),
            Self::Implied(i) => Some(Value::Implied(i)),
            Self::Immediate(_) => None,
        }
    }
}

impl<I> ValueSource<I> {
    pub fn mem(self) -> Option<MemAddressMode> {
        match self {
            Self::Mem(m) => Some(m),
            _ => None,
        }
    }
}

impl<I> From<RegType> for ValueSource<I> {
    fn from(value: RegType) -> Self {
        Self::Implied(value)
    }
}

impl<I> From<MemAddressMode> for ValueSource<I> {
    fn from(value: MemAddressMode) -> Self {
        Self::Mem(value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Void {}

impl From<Void> for Word {
    fn from(_: Void) -> Self {
        unreachable!()
    }
}

pub type Value = ValueSource<Void>;

impl<T> ValueSource<T> {
    pub fn bytecount(self) -> usize {
        match self {
            Self::Implied(_) => 0,
            Self::Immediate(_) => 1,
            Self::Mem(mem) => mem.bytecount(),
        }
    }
}

#[apply(multivalue)]
pub enum Flag {
    Negative,
    Overflow,
    Break,
    Decimal,
    InterruptDisable,
    Zero,
    Carry,
}

#[apply(multivalue)]
pub enum StackOperand {
    A,
    P,
}

#[apply(multivalue)]
pub enum JmpAddrMode {
    Absolute(Addr),
    Indirect(Addr),
}

#[apply(multivalue)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Or,
    Xor,
    And,
}

#[apply(multivalue)]
pub enum BitOp {
    ShiftLeft,
    ShiftRight,
    RotateLeft,
    RotateRight,
}

#[apply(multivalue)]
pub enum Instruction {
    Bit {
        operand: Value,
        operator: BitOp,
    },
    Arithmetic {
        lhs: Value,
        rhs: ValueSource,
        op: ArithmeticOp,
    },
    StackPush(StackOperand),
    StackPop(StackOperand),
    ClearBit(Flag),
    Noop,
    Jump {
        addr: JmpAddrMode,
        with_return: bool,
    },
    ReturnFromSub,
    Increment(Value),
    Decrement(Value),
    Transfer {
        from: ValueSource,
        to: Value,
    },
    Load {
        reg: RegType,
        from: ValueSource,
    },
    Store {
        reg: RegType,
        into: MemAddressMode,
    },
    Branch {
        bit: Flag,
        offset: i8,
    },
}

impl Instruction {
    pub fn argcount(self) -> usize {
        match self {
            Self::ReturnFromSub
            | Self::Noop
            | Self::ClearBit(_)
            | Self::StackPop(_)
            | Self::StackPush(_) => 0,

            Self::Transfer { from, to } => from.bytecount() + to.bytecount(),

            Self::Increment(addr) | Self::Decrement(addr) => addr.bytecount(),

            Self::Load { reg: _, from: addr } => addr.bytecount(),
            Self::Arithmetic { rhs: value, .. } => value.bytecount(),

            Self::Store { into: addr, .. } => addr.bytecount(),
            Self::Bit { operand, .. } => operand.bytecount(),

            Self::Jump { .. } => std::mem::size_of::<Addr>(),

            Self::Branch { .. } => 1,
        }
    }
    pub fn bytecount(self) -> usize {
        self.argcount() + 1
    }
}
