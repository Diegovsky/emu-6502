use std::ops::{ Add, AddAssign, Sub, SubAssign };

use crate::instructions::{IndexType, RegType};

pub type Word = u8;
pub type Addr = u16;

#[derive(Default, Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
#[repr(transparent)]
pub struct Reg(pub Word);

impl Reg {
    pub fn checked_add(self, rhs: Word) -> (Self, bool) {
        let (val, status) = self.0.overflowing_add(rhs);
        (Self(val), status)
    }
    pub fn checked_sub(self, rhs: Word) -> (Self, bool) {
        let (val, status) = self.0.overflowing_sub(rhs);
        (Self(val), status)
    }
}

/* impl From<Word> for Reg {
    fn from(value: Word) -> Self {
        Self(value)
    }
} */

impl Add<Word> for Reg {
    type Output = Word;
    fn add(self, rhs: Word) -> Self::Output {
        self.0.wrapping_add(rhs)
    }
}

impl Sub<Word> for Reg {
    type Output = Word;
    fn sub(self, rhs: Word) -> Self::Output {
        self.0.wrapping_sub(rhs)
    }
}

impl AddAssign<Word> for Reg {
    fn add_assign(&mut self, rhs: Word) {
        *self = Self(*self + rhs)
    }
}


impl SubAssign<Word> for Reg {
    fn sub_assign(&mut self, rhs: Word) {
        *self = Self(*self - rhs)
    }
}

impl PartialEq<Word> for Reg {
    fn eq(&self, other: &Word) -> bool {
        self.0.eq(other)
    }
}

impl PartialOrd<Word> for Reg {
    fn partial_cmp(&self, other: &Word) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
    pub struct FlagsRegister: Word {
        const N = 1<<7;
        const V = 1<<6;
        const B = 1<<4;
        const D = 1<<3;
        const I = 1<<2;
        const Z = 1<<1;
        const C = 1<<0;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Registers {
    pub a: Reg,
    pub x: Reg,
    pub y: Reg,
    pub p: FlagsRegister,
    pub s: Reg,
    pub pc: u16,
}

impl Registers {
    #[inline]
    pub fn get_register(&self, register_type: RegType) -> Reg {
        match register_type {
            RegType::S => self.s,
            RegType::A => self.a,
            RegType::Index(index) => self.get_index(index),
        }
    }
    #[inline]
    pub fn get_register_mut(&mut self, register_type: RegType) -> &mut Reg {
        match register_type {
            RegType::S => &mut self.s,
            RegType::A => &mut self.a,
            RegType::Index(index) => self.get_index_mut(index),
        }
    }
    #[inline]
    pub fn get_index(&self, index: IndexType) -> Reg {
        match index {
            IndexType::X => self.x,
            IndexType::Y => self.y,
        }
    }

    #[inline]
    pub fn get_index_mut(&mut self, index: IndexType) -> &mut Reg {
        match index {
            IndexType::X => &mut self.x,
            IndexType::Y => &mut self.y,
        }
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            a: Default::default(),
            x: Default::default(),
            y: Default::default(),
            p: FlagsRegister::default(),
            s: Reg(0xFDu8),
            pc: Default::default(),
        }
    }
}


