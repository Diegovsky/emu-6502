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

impl Value {
    pub fn into_value_source(self) -> ValueSource {
        match self {
            Self::Mem(m) => ValueSource::Mem(m),
            Self::Implied(i) => ValueSource::Implied(i),
            Self::Immediate(i) => ValueSource::Immediate(i.into()),
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

    pub fn bytecount(self) -> usize {
        match self {
            Self::Implied(_) => 0,
            Self::Immediate(_) => 1,
            Self::Mem(mem) => mem.bytecount(),
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
pub enum AccOp {
    Add,
    Sub,
    Or,
    Xor,
    And,
}

#[apply(multivalue)]
pub enum UnOp {
    ShiftLeft,
    ShiftRight,
    RotateLeft,
    RotateRight,
    Increment,
    Decrement,
    Bit,
}

#[apply(multivalue)]
pub enum Instruction {
    UnOp {
        operand: Value,
        operator: UnOp,
    },
    AccOp {
        operand: ValueSource,
        op: AccOp,
    },
    Compare {
        reg: RegType,
        operand: ValueSource,
    },
    Break,
    ChangeFlag(Flag, bool),
    Noop,
    Jump {
        addr: Addr,
        indirect: bool,
        with_return: bool,
    },
    ReturnFromSub{ is_interrupt: bool },
    StackTransfer { is_acc: bool, to_stack: bool },
    TransferXS{rev:bool},
    Transfer {
        from: ValueSource,
        to: Value,
    },
    Branch {
        bit: Flag,
        if_set: bool,
        offset: i8,
    },
}

impl Instruction {
    pub fn argcount(self) -> usize {
        match self {
            Self::ReturnFromSub{..}
            | Self::Break
            | Self::Noop
            | Self::ChangeFlag(..)
            | Self::TransferXS {..}
            | Self::StackTransfer {..} => 0,

            Self::Compare { operand, .. } => operand.bytecount(),

            Self::Transfer { from, to } => from.bytecount() + to.bytecount(),

            Self::AccOp { operand: value, .. } => value.bytecount(),

            Self::UnOp { operand, .. } => operand.bytecount(),

            Self::Jump { .. } => std::mem::size_of::<Addr>(),

            Self::Branch { .. } => 1,
        }
    }
    pub fn bytecount(self) -> usize {
        self.argcount() + 1
    }
}
