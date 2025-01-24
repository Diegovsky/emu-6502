use super::*;
use crate::regs::FlagsRegister as Flag;
use num_enum::TryFromPrimitive;

// Based on https://www.masswerk.at/6502/6502_instruction_set.html#layout
// and https://llx.com/Neil/a2/opcodes.html

#[derive(TryFromPrimitive, PartialEq, PartialOrd, Eq)]
#[repr(u8)]
enum C1 {
    ORA = 0b000,
    AND,
    EOR,
    ADC,
    STA,
    LDA,
    CMP,
    SBC
}

#[derive(TryFromPrimitive, PartialEq, PartialOrd, Eq)]
#[repr(u8)]
enum C2 {
    ASL = 0b000,
    ROL,
    LSR,
    ROR,
    STX,
    LDX,
    DEC,
    INC,
}
#[derive(TryFromPrimitive, PartialEq, PartialOrd, Eq)]
#[repr(u8)]
enum C3 {
    BIT = 0b001,
    STY = 0b100,
    LDY = 0b101,
    CPY = 0b110,
    CPX = 0b111,
}


impl Instruction {
    fn decode_weird(ins: u8) -> Option<Self> {
        Some(match ins {
            // PHP
            0x08 => Self::StackTransfer { is_acc: false, to_stack: true  },
            // PLP
            0x28 => Self::StackTransfer { is_acc: false, to_stack: false  },
            // PHA
            0x48 => Self::StackTransfer { is_acc: false, to_stack: true  },
            // PLA
            0x68 => Self::StackTransfer { is_acc: false, to_stack: true  },
            // DEY
            0x88 => Self::UnOp { operand: RegType::Y.into(), operator: UnOp::Decrement },
            // TAY
            0xA8 => Self::Transfer { from: RegType::A.into(), to: RegType::Y.into() },
            // INY
            0xC8 => Self::UnOp { operand: RegType::Y.into(), operator: UnOp::Increment },
            // INX
            0xE8 => Self::UnOp { operand: RegType::X.into(), operator: UnOp::Increment },
            // --
            // CLC
            0x18 => Self::ChangeFlag(Flag::Carry, false),
            // SEC
            0x38 => Self::ChangeFlag(Flag::Carry, true),
            // CLI
            0x58 => Self::ChangeFlag(Flag::InterruptDisable, false),
            // SEI
            0x78 => Self::ChangeFlag(Flag::InterruptDisable, true),
            // TYA
            0x98 => Self::Transfer { from: RegType::Y.into(), to: RegType::A.into() },
            // CLV
            0xB8 => Self::ChangeFlag(Flag::Overflow, false),
            // CLD
            0xD8 => Self::ChangeFlag(Flag::Decimal, false),
            // SED
            0xF8 => Self::ChangeFlag(Flag::Decimal, true),
            // TXS
            0x9A => Self::TransferXS { rev: false },
            // TSX
            0xBA => Self::TransferXS { rev: true },
            // DEX
            0xCA => Self::UnOp { operand: RegType::X.into(), operator: UnOp::Decrement },
            // NOP
            0xEA => Self::Noop,
            _ => return None
        })
    }
    #[rustfmt::skip]
    pub fn decode(bytes: &[u8; 3]) -> Option<Self> {
        println!("{bytes:x?}");
        let ins = bytes[0];
        if let Some(v) = Self::decode_weird(ins) {
            return Some(v);
        }
        let args = [bytes[1], bytes[2]];
        let a = ins >> 5;
        let b = (ins >> 2) & 0b111;
        let c = ins & 0b11;
        match c {
            1 => {
                let a = C1::try_from(a).ok()?;
                let operand: ValueSource = match b {
                    0b000 => MemAddressMode::Zero { addr: args[0], indirect: true, index: Some(IndexType::X) }.into(),
                    0b001 => MemAddressMode::zpg(args[0]).into(),
                    0b010 => ValueSource::Immediate(args[0]),
                    0b011 => MemAddressMode::abs(Addr::from_le_bytes(args)).into(),
                    0b100 => MemAddressMode::Zero { addr: args[0], indirect: true, index: Some(IndexType::Y) }.into(),
                    0b101 => MemAddressMode::zpg_indexed(args[0], IndexType::Y).into(),
                    0b110 => MemAddressMode::abs_indexed(Addr::from_le_bytes(args), IndexType::Y).into(),
                    0b111 => MemAddressMode::abs_indexed(Addr::from_le_bytes(args), IndexType::X).into(),
                    _ => unreachable!(),
                };
                let ins = match a {
                    C1::ORA => Self::AccOp { operand, op: AccOp::Or },
                    C1::AND => Self::AccOp { operand, op: AccOp::And },
                    C1::EOR => Self::AccOp { operand, op: AccOp::Xor },
                    C1::ADC => Self::AccOp { operand, op: AccOp::Add },
                    C1::STA => Self::Transfer { from: RegType::A.into(), to: operand.into_value()? },
                    C1::LDA => Self::Transfer { to: RegType::A.into(), from: operand },
                    C1::CMP => Self::Compare { reg: RegType::A, operand, },
                    C1::SBC => Self::AccOp { operand, op: AccOp::Sub },
                };
                Some(ins)
            },
            2 => {
                let a = C2::try_from(a).ok()?;
                let mut operand: ValueSource = match b {
                    0b000 => ValueSource::Immediate(args[0]),
                    0b001 => MemAddressMode::zpg(args[0]).into(),
                    0b010 => RegType::A.into(),
                    0b011 => MemAddressMode::abs(Addr::from_le_bytes(args)).into(),
                    0b101 => MemAddressMode::zpg_indexed(args[0], IndexType::Y).into(),
                    0b111 => MemAddressMode::abs_indexed(Addr::from_le_bytes(args), IndexType::X).into(),
                    b if b < 8 => return None,
                    _ => unreachable!(),
                };
                // X-indexed is replaced by Y-indexed for these instructions
                if matches!(a, C2::LDX|C2::STX) {
                    if let ValueSource::Mem(MemAddressMode::Zero { index, .. }) = &mut operand {
                        *index = Some(IndexType::Y)
                    }
                    if let ValueSource::Mem(MemAddressMode::Absolute { index, .. }) = &mut operand {
                        // Weirdly, STX doesn't support this address mode
                        if a == C2::STX {
                            return None
                        }
                        *index = Some(IndexType::Y)
                    }
                }

                // Instructions DEC and INC don't operate on the accumulator.
                if a >= C2::DEC && matches!(operand, ValueSource::Implied(_)) {
                    return None
                }

                // LDX is the only instruction here that takes an Immediate operand.
                if a == C2::LDX {
                    return Some(Self::Transfer { to: RegType::X.into(), from: operand })
                }
                let operand = operand.into_value()?;
                let ins = match a {
                    C2::ASL => Self::UnOp { operand, operator: UnOp::ShiftLeft },
                    C2::ROL => Self::UnOp { operand, operator: UnOp::RotateLeft },
                    C2::LSR => Self::UnOp { operand, operator: UnOp::ShiftRight },
                    C2::ROR => Self::UnOp { operand, operator: UnOp::RotateRight },
                    C2::STX => Self::Transfer { from: RegType::X.into(), to: operand },
                    C2::LDX => unreachable!("Handled above"),
                    C2::DEC => Self::UnOp { operand, operator: UnOp::Decrement },
                    C2::INC => Self::UnOp { operand, operator: UnOp::Increment },
                };
                Some(ins)
            }
            // The most esoteric ones.
            0 => {
                // JMP
                if a == 0b010 || a == 0b011{
                    let addr = Addr::from_le_bytes(args);
                    return Some(Self::Jump { addr, indirect: (a&1) != 0, with_return: false })
                }
                // The following instructions simply don't follow the 
                // established a and b encoding.

                // The "branch" instructions follow a xxy10000 encoding.
                if ins & 0b00011111 == 0b10000 {
                    // x indentifies the flag
                    let x = ins >> 6;
                    // y represents the compared value
                    let y = (ins >> 5)&1;
                    let if_set = y == 1;
                    let offset = i8::from_le_bytes([args[0]]);
                    let branch = match x {
                        0b00 => Self::Branch { bit: Flag::Negative, offset, if_set },
                        0b01 => Self::Branch { bit: Flag::Overflow, offset, if_set },
                        0b10 => Self::Branch { bit: Flag::Carry, offset, if_set },
                        0b11 => Self::Branch { bit: Flag::Zero, offset, if_set },
                        _ => unreachable!(),
                    };
                    return Some(branch)
                }

                // Well, this part kinda makes sense
                if let Ok(a) = C3::try_from(a) {
                    let operand = match b {
                        0b000 => ValueSource::Immediate(args[0]),
                        0b001 => MemAddressMode::zpg(args[0]).into(),
                        0b011 => MemAddressMode::abs(Addr::from_le_bytes(args)).into(),
                        0b101 => MemAddressMode::zpg_indexed(args[0], IndexType::Y).into(),
                        0b111 => MemAddressMode::abs_indexed(Addr::from_le_bytes(args), IndexType::X).into(),
                        b if b < 8 => return None,
                        _ => unreachable!(),
                    };
                    let ins = match a {
                        C3::BIT if matches!(b, 0b001|0b011) => Self::UnOp { operand: operand.into_value()?, operator: UnOp::Bit },
                        C3::STY if matches!(b, 0b001..=0b101) => Self::Transfer { from: RegType::Y.into(), to: operand.into_value()? },
                        C3::LDY => Self::Transfer { to: RegType::Y.into(), from: operand },
                        C3::CPY if matches!(b, 0b001..=0b011) => Self::Compare { reg: RegType::Y, operand },
                        C3::CPX if matches!(b, 0b001..=0b011) => Self::Compare { reg: RegType::X, operand },
                        _ => return None,
                    };
                    return Some(ins)
                }

                // The remaining ones
                Some(match ins {
                    0x00 => Self::Break,
                    0x20 => Self::Jump { addr: Addr::from_le_bytes(args), indirect: false, with_return: true },
                    0x40 => Self::ReturnFromSub { is_interrupt: true },
                    0x60 => Self::ReturnFromSub { is_interrupt: false },
                    // If we reached here, it's safe to assume the instruction
                    // is invalid
                    _ => return None

                })

            }
            _ => None
        }
    }
}
