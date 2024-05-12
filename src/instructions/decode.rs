use super::*;

// Based on https://www.masswerk.at/6502/6502_instruction_set.html#layout
// and https://llx.com/Neil/a2/opcodes.html

impl Instruction {
    fn decode_common_rhs(b: u8, args: &[u8; 2]) -> Option<MemAddressMode> {
        if b & 1 == 0 {
            return None
        }
        let src = match b {
            1 => MemAddressMode::zpg(args[0]),
            3 => MemAddressMode::abs(Addr::from_le_bytes(*args)),
            5 => MemAddressMode::zpg_indexed(args[0], IndexType::X),
            7 => MemAddressMode::abs_indexed(Addr::from_le_bytes(*args), IndexType::X),
            _ => unreachable!(),
        };
        Some(src)
    }
    fn decode_c1_rhs(b: u8, args: &[u8; 2]) -> ValueSource {
        match b {
            0 => ValueSource::Mem(MemAddressMode::Zero {
                addr: args[0],
                indirect: true,
                index: Some(IndexType::X),
            }),
            2 => ValueSource::Immediate(args[0]),
            4 => ValueSource::Mem(MemAddressMode::Absolute {
                addr: Addr::from_le_bytes(*args),
                index: None,
            }),
            1 | 3 | 5 | 7 => Self::decode_common_rhs(b, args).unwrap().into(),
            8.. => unreachable!("Invalid b value"),
            _ => todo!(),
        }
    }
    fn decode_c1(a: u8, b: u8, args: &[u8; 2]) -> Self {
        let reg = RegType::A;
        let rhs = Instruction::decode_c1_rhs(b, args);
        if matches!(a, 0..=3 | 7) {
            Self::Arithmetic {
                lhs: reg.into(),
                rhs,
                op: match a {
                    0 => ArithmeticOp::Or,
                    1 => ArithmeticOp::And,
                    2 => ArithmeticOp::Xor,
                    3 => ArithmeticOp::Add,
                    7 => ArithmeticOp::Sub,
                    _ => unreachable!(),
                },
            }
        } else {
            match a {
                4 => Self::Store {
                    reg,
                    into: rhs.mem().unwrap(),
                },
                5 => Self::Load { reg, from: rhs },
                6 => Self::Load { reg, from: rhs },
                _ => unreachable!(),
            }
        }
    }
    fn decode_c2(a: u8, b: u8, args: &[u8; 2]) -> Self {
        type V = ValueSource;
        let rhs: V = match b {
            // I love exceptions, don't you?
            6 if a == 4 => V::Implied(RegType::S),
            6 if a == 5 => V::Implied(RegType::X),
            5 if a == 4 => MemAddressMode::zpg_indexed(args[0], IndexType::Y).into(),
            5 if a == 5 => MemAddressMode::zpg_indexed(args[0], IndexType::Y).into(),
            0 if a == 5 => ValueSource::Immediate(args[0]),
            2 if a == 7 => return Self::Noop,
            2 if a <= 3 => V::Implied(RegType::A),
            2 if a > 3 => V::Implied(RegType::A),
            7 if a == 4 => unreachable!("Invalid bits for b {b:b}"),
            1 | 3 | 5 | 7 => Self::decode_common_rhs(b, args).unwrap().into(),
            _ => unreachable!("Invalid bits for b {b:b}"),
        };
        if a == 5 {
            return Self::Transfer { to: RegType::X.into(), from: rhs }
        }
        let operand = rhs.into_value().unwrap();
        match a {
            0 => Self::Bit { operator: BitOp::ShiftLeft, operand },
            1 => Self::Bit { operator: BitOp::RotateLeft, operand },
            2 => Self::Bit { operator: BitOp::ShiftRight, operand },
            3 => Self::Bit { operator: BitOp::RotateRight, operand },
            4 => Self::Transfer { from: RegType::X.into(), to: operand },
            6 => Self::Decrement(operand),
            7 => Self::Increment(operand),
            _ => unreachable!(),
        }
    }
    fn decode_c0(a: u8, b: u8, args: &[u8; 2]) -> Self {
        // This is the most insane part. Expect the ugliest code in earth
        match b {

        }
    }
    pub fn decode(bytes: &[u8; 3]) -> Self {
        let ins = bytes[0];
        let args = &[bytes[1], bytes[2]];
        let a = ins >> 5;
        let b = (ins >> 2) & 0b111;
        let c = ins & 0b11;
        match c {
            1 => Self::decode_c1(a, b, args),
            2 => Self::decode_c2(a, b, args),
            _ => unreachable!(),
        }
    }
}
