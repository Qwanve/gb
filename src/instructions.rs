use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Noop,
    LoadBCFrom16Imm { new_value: u16 },
    IncrementBC,
    DecrementB,
    LoadBFrom8Imm { new_value: u8 },
    DecrementC,
    LoadCFrom8Imm { new_value: u8 },
    LoadDEFrom16Imm { new_value: u16 },
    StoreAAtDE,
    IncrementDE,
    IncrementD,
    JumpRelative { offset: i8 },
    LoadAFromDE,
    IncrementE,
    RotateRightWithCarryA,
    JumpRelativeIfNotZero { offset: i8 },
    LoadHLFrom16Imm { new_value: u16 },
    StoreAAtHLAndIncrement,
    IncrementHL,
    IncrementH,
    LoadHFrom8Imm { new_value: u8 },
    JumpRelativeIfZero { offset: i8 },
    LoadAFromHLAndInc,
    IncrementL,
    DecrementL,
    JumpRelativeIfNotCarry { offset: i8 },
    LoadSPFrom16Imm { new_value: u16 },
    StoreAAtHLAndDecrement,
    LoadAFrom8Imm { new_value: u8 },
    LoadBFromHL,
    LoadBFromA,
    LoadCFromL,
    LoadCFromHL,
    LoadDFromHL,
    LoadHFromL,
    StoreAAtHL,
    LoadAFromB,
    LoadAFromH,
    LoadAFromL,
    XorCWithA,
    XorHLWithA,
    OrCWithA,
    OrAWithA,
    PopBC,
    Jump { address: u16 },
    CallIfNotZero { address: u16 },
    PushBC,
    AddAWith8Imm { value: u8 },
    Return,
    Complex(ComplexInstruction),
    Call { address: u16 },
    PushDE,
    SubtractAWith8Imm { value: u8 },
    OutputAToPort { address: u8 },
    PopHL,
    PushHL,
    AndAWith8Imm { value: u8 },
    StoreAAt16Imm { address: u16 },
    LoadAFromPort { address: u8 },
    LoadAFrom16Imm { address: u16 },
    PopAF,
    DisableInterrupts,
    PushAF,
    CompareAWith8Imm { value: u8 },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ComplexInstruction {
    RotateRightWithCarryC,
    RotateRightWithCarryD,
    ShiftRightLogicalB,
}

impl Instruction {
    pub fn size(&self) -> u16 {
        match self {
            Instruction::Noop => 1,
            Instruction::LoadBCFrom16Imm { .. } => 3,
            Instruction::IncrementBC => 1,
            Instruction::DecrementB => 1,
            Instruction::LoadBFrom8Imm { .. } => 2,
            Instruction::DecrementC => 1,
            Instruction::LoadCFrom8Imm { .. } => 2,
            Instruction::LoadDEFrom16Imm { .. } => 3,
            Instruction::StoreAAtDE => 1,
            Instruction::IncrementDE => 1,
            Instruction::IncrementD => 1,
            Instruction::JumpRelative { .. } => 2,
            Instruction::LoadAFromDE => 1,
            Instruction::IncrementE => 1,
            Instruction::RotateRightWithCarryA => 1,
            Instruction::JumpRelativeIfNotZero { .. } => 2,
            Instruction::LoadHLFrom16Imm { .. } => 3,
            Instruction::StoreAAtHLAndIncrement => 1,
            Instruction::IncrementHL => 1,
            Instruction::IncrementH => 1,
            Instruction::LoadHFrom8Imm { .. } => 2,
            Instruction::JumpRelativeIfZero { .. } => 2,
            Instruction::LoadAFromHLAndInc => 1,
            Instruction::IncrementL => 1,
            Instruction::DecrementL => 1,
            Instruction::JumpRelativeIfNotCarry { .. } => 2,
            Instruction::LoadSPFrom16Imm { .. } => 3,
            Instruction::StoreAAtHLAndDecrement => 1,
            Instruction::LoadAFrom8Imm { .. } => 2,
            Instruction::LoadBFromHL => 1,
            Instruction::LoadBFromA => 1,
            Instruction::LoadCFromL => 1,
            Instruction::LoadCFromHL => 1,
            Instruction::LoadDFromHL => 1,
            Instruction::LoadHFromL => 1,
            Instruction::StoreAAtHL => 1,
            Instruction::LoadAFromB => 1,
            Instruction::LoadAFromH => 1,
            Instruction::LoadAFromL => 1,
            Instruction::XorCWithA => 1,
            Instruction::XorHLWithA => 1,
            Instruction::OrCWithA => 1,
            Instruction::OrAWithA => 1,
            Instruction::PopBC => 1,
            Instruction::Jump { .. } => 3,
            Instruction::CallIfNotZero { .. } => 3,
            Instruction::PushBC => 1,
            Instruction::AddAWith8Imm { .. } => 2,
            Instruction::Return => 1,
            Instruction::Complex(..) => 2,
            Instruction::Call { .. } => 3,
            Instruction::PushDE => 1,
            Instruction::SubtractAWith8Imm { .. } => 2,
            Instruction::OutputAToPort { .. } => 2,
            Instruction::PopHL => 1,
            Instruction::PushHL => 1,
            Instruction::AndAWith8Imm { .. } => 2,
            Instruction::StoreAAt16Imm { .. } => 3,
            Instruction::LoadAFromPort { .. } => 2,
            Instruction::LoadAFrom16Imm { .. } => 3,
            Instruction::PopAF => 1,
            Instruction::DisableInterrupts => 1,
            Instruction::PushAF => 1,
            Instruction::CompareAWith8Imm { .. } => 2,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Instruction::Noop => format!("NOP"),
            Instruction::LoadBCFrom16Imm { new_value } => format!("LD BC, ${new_value:04X}"),
            Instruction::IncrementBC => format!("INC BC"),
            Instruction::DecrementB => format!("DEC B"),
            Instruction::LoadBFrom8Imm { new_value } => format!("LD B, ${new_value:02X}"),
            Instruction::DecrementC => format!("DEC C"),
            Instruction::LoadCFrom8Imm { new_value } => format!("LD C, ${new_value:02X}"),
            Instruction::LoadDEFrom16Imm { new_value } => format!("LD DE, ${new_value:04X}"),
            Instruction::StoreAAtDE => format!("LD (DE), A"),
            Instruction::IncrementDE => format!("INC DE"),
            Instruction::IncrementD => format!("INC D"),
            Instruction::JumpRelative { offset } => format!(
                "JR {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::LoadAFromDE => format!("LD A, (DE)"),
            Instruction::IncrementE => format!("INC E"),
            Instruction::RotateRightWithCarryA => format!("RRA"),
            Instruction::JumpRelativeIfNotZero { offset } => format!(
                "JR NZ, {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::LoadHLFrom16Imm { new_value } => format!("LD HL, ${new_value:04X}"),
            Instruction::StoreAAtHLAndIncrement => format!("LD (HL+), A"),
            Instruction::IncrementHL => format!("INC HL"),
            Instruction::IncrementH => format!("INC H"),
            Instruction::LoadHFrom8Imm { new_value } => format!("LD H, ${new_value:02X}"),
            Instruction::JumpRelativeIfZero { offset } => format!(
                "JR Z, {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::LoadAFromHLAndInc => format!("LD A, (HL+)"),
            Instruction::IncrementL => format!("INC L"),
            Instruction::DecrementL => format!("DEC L"),
            Instruction::LoadSPFrom16Imm { new_value } => format!("LD SP, ${new_value:04X}"),
            Instruction::StoreAAtHLAndDecrement => format!("LD (HL-), A"),
            Instruction::LoadAFrom8Imm { new_value } => format!("LD A, ${new_value:02X}"),
            Instruction::LoadBFromHL => format!("LD B, (HL)"),
            Instruction::LoadBFromA => format!("LD B, A"),
            Instruction::LoadCFromL => format!("LD C, L"),
            Instruction::LoadCFromHL => format!("LD C, (HL)"),
            Instruction::LoadDFromHL => format!("LD D, (HL)"),
            Instruction::LoadHFromL => format!("LD H, L"),
            Instruction::StoreAAtHL => format!("LD (HL), A"),
            Instruction::LoadAFromB => format!("LD A, B"),
            Instruction::LoadAFromH => format!("LD A, H"),
            Instruction::LoadAFromL => format!("LD A, L"),
            Instruction::XorCWithA => format!("XOR C"),
            Instruction::XorHLWithA => format!("XOR (HL)"),
            Instruction::OrCWithA => format!("OR C"),
            Instruction::OrAWithA => format!("OR A"),
            Instruction::PopBC => format!("POP BC"),
            Instruction::Jump { address } => format!("JP ${address:04X}"),
            Instruction::CallIfNotZero { address } => format!("CALL NZ, ${address:04X}"),
            Instruction::PushBC => format!("PUSH BC"),
            Instruction::AddAWith8Imm { value } => format!("ADD A, ${value:02X}"),
            Instruction::Return => format!("RET"),
            Instruction::Complex(ci) => format!("{ci}"),
            Instruction::Call { address } => format!("CALL ${address:04X}"),
            Instruction::PushDE => format!("PUSH DE"),
            Instruction::SubtractAWith8Imm { value } => format!("SUB A, ${value:02X}"),
            Instruction::OutputAToPort { address } => format!("LD ${address:02X}, A"),
            Instruction::PopHL => format!("POP HL"),
            Instruction::PushHL => format!("PUSH HL"),
            Instruction::AndAWith8Imm { value } => format!("AND ${value:02X}"),
            Instruction::StoreAAt16Imm { address } => format!("LD ${address:04X}, A"),
            Instruction::LoadAFromPort { address } => format!("LD A, ${address:02X}"),
            Instruction::LoadAFrom16Imm { address } => format!("LD A, ${address:04X}"),
            Instruction::PopAF => format!("POP AF"),
            Instruction::DisableInterrupts => format!("DI"),
            Instruction::PushAF => format!("PUSH AF"),
            Instruction::CompareAWith8Imm { value } => format!("CP ${value:02X}"),
        };
        write!(f, "{str}")
    }
}

impl Display for ComplexInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ComplexInstruction::RotateRightWithCarryC => format!("RR C"),
            ComplexInstruction::RotateRightWithCarryD => format!("RR D"),
            ComplexInstruction::ShiftRightLogicalB => format!("SRL B"),
        };
        write!(f, "{str}")
    }
}

pub struct InstructionBuilder([u8; 3]);

pub enum InstructionParseError {
    UnknownInstruction(u8),
    UnknownComplexInstruction(u8),
    IllegalInstruction(u8),
}

impl InstructionBuilder {
    pub fn new(value: [u8; 3]) -> Self {
        InstructionBuilder(value)
    }
    pub fn s8_imm(&self) -> i8 {
        i8::from_le_bytes([self.0[1]])
    }
    pub fn u8_imm(&self) -> u8 {
        u8::from_le_bytes([self.0[1]])
    }
    pub fn u16_imm(&self) -> u16 {
        u16::from_le_bytes([self.0[1], self.0[2]])
    }

    pub fn parse(self) -> Result<Instruction, InstructionParseError> {
        Ok(match self.0[0] {
            0x00 => Instruction::Noop,
            0x01 => {
                let new_value = self.u16_imm();
                Instruction::LoadBCFrom16Imm { new_value }
            }
            0x03 => Instruction::IncrementBC,
            0x05 => Instruction::DecrementB,
            0x06 => {
                let new_value = self.u8_imm();
                Instruction::LoadBFrom8Imm { new_value }
            }
            0x0D => Instruction::DecrementC,
            0x0E => {
                let new_value = self.u8_imm();
                Instruction::LoadCFrom8Imm { new_value }
            }
            0x11 => {
                let new_value = self.u16_imm();
                Instruction::LoadDEFrom16Imm { new_value }
            }
            0x12 => Instruction::StoreAAtDE,
            0x13 => Instruction::IncrementDE,
            0x14 => Instruction::IncrementD,
            0x18 => {
                let offset = self.s8_imm();
                Instruction::JumpRelative { offset }
            }
            0x1A => Instruction::LoadAFromDE,
            0x1C => Instruction::IncrementE,
            0x1F => Instruction::RotateRightWithCarryA,
            0x20 => {
                let offset = self.s8_imm();
                Instruction::JumpRelativeIfNotZero { offset }
            }
            0x21 => {
                let new_value = self.u16_imm();
                Instruction::LoadHLFrom16Imm { new_value }
            }
            0x22 => Instruction::StoreAAtHLAndIncrement,
            0x23 => Instruction::IncrementHL,
            0x24 => Instruction::IncrementH,
            0x26 => {
                let new_value = self.u8_imm();
                Instruction::LoadHFrom8Imm { new_value }
            }
            0x28 => {
                let offset = self.s8_imm();
                Instruction::JumpRelativeIfZero { offset }
            }
            0x2A => Instruction::LoadAFromHLAndInc,
            0x2C => Instruction::IncrementL,
            0x2D => Instruction::DecrementL,
            0x30 => {
                let offset = self.s8_imm();
                Instruction::JumpRelativeIfNotCarry { offset }
            }
            0x31 => {
                let new_value = self.u16_imm();
                Instruction::LoadSPFrom16Imm { new_value }
            }
            0x32 => Instruction::StoreAAtHLAndDecrement,
            0x3E => {
                let new_value = self.u8_imm();
                Instruction::LoadAFrom8Imm { new_value }
            }
            0x46 => Instruction::LoadBFromHL,
            0x47 => Instruction::LoadBFromA,
            0x4D => Instruction::LoadCFromL,
            0x4E => Instruction::LoadCFromHL,
            0x56 => Instruction::LoadDFromHL,
            0x65 => Instruction::LoadHFromL,
            0x77 => Instruction::StoreAAtHL,
            0x78 => Instruction::LoadAFromB,
            0x7C => Instruction::LoadAFromH,
            0x7D => Instruction::LoadAFromL,
            0xA9 => Instruction::XorCWithA,
            0xAE => Instruction::XorHLWithA,
            0xB1 => Instruction::OrCWithA,
            0xB7 => Instruction::OrAWithA,
            0xC1 => Instruction::PopBC,
            0xC3 => {
                let address = self.u16_imm();
                Instruction::Jump { address }
            }
            0xC4 => {
                let address = self.u16_imm();
                Instruction::CallIfNotZero { address }
            }
            0xC5 => Instruction::PushBC,
            0xC6 => {
                let value = self.u8_imm();
                Instruction::AddAWith8Imm { value }
            }
            0xC9 => Instruction::Return,
            0xCB => Instruction::Complex(InstructionBuilder::parse_complex(self.u8_imm())?),
            0xCD => {
                let address = self.u16_imm();
                Instruction::Call { address }
            }
            0xD5 => Instruction::PushDE,
            0xD6 => {
                let value = self.u8_imm();
                Instruction::SubtractAWith8Imm { value }
            }
            0xE0 => {
                let address = self.u8_imm();
                Instruction::OutputAToPort { address }
            }
            0xE1 => Instruction::PopHL,
            0xE5 => Instruction::PushHL,
            0xE6 => {
                let value = self.u8_imm();
                Instruction::AndAWith8Imm { value }
            }
            0xEA => {
                let address = self.u16_imm();
                Instruction::StoreAAt16Imm { address }
            }
            0xF0 => {
                let address = self.u8_imm();
                Instruction::LoadAFromPort { address }
            }
            0xF1 => Instruction::PopAF,
            0xF3 => Instruction::DisableInterrupts,
            0xF5 => Instruction::PushAF,
            0xFA => {
                let address = self.u16_imm();
                Instruction::LoadAFrom16Imm { address }
            }
            0xFE => {
                let value = self.u8_imm();
                Instruction::CompareAWith8Imm { value }
            }
            value => Err(InstructionParseError::UnknownInstruction(value))?,
        })
    }

    pub fn parse_complex(op_code: u8) -> Result<ComplexInstruction, InstructionParseError> {
        Ok(match op_code {
            0x19 => ComplexInstruction::RotateRightWithCarryC,
            0x1A => ComplexInstruction::RotateRightWithCarryD,
            0x38 => ComplexInstruction::ShiftRightLogicalB,
            value => Err(InstructionParseError::UnknownComplexInstruction(value))?,
        })
    }
}
