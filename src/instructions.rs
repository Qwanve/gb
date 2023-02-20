use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Noop,
    LoadCFrom8Imm { new_value: u8 },
    LoadDEFrom16Imm { new_value: u16 },
    StoreAAtDE,
    IncrementD,
    IncrementE,
    JumpRelativeIfNotZero { offset: i8 },
    LoadHLFrom16Imm { new_value: u16 },
    LoadAFromHLAndInc,
    LoadBFromA,
    Jump { address: u16 },
}

impl Instruction {
    pub fn size(&self) -> u16 {
        match self {
            Instruction::Noop => 1,
            Instruction::LoadCFrom8Imm { .. } => 2,
            Instruction::LoadDEFrom16Imm { .. } => 3,
            Instruction::StoreAAtDE => 1,
            Instruction::IncrementD => 1,
            Instruction::IncrementE => 1,
            Instruction::JumpRelativeIfNotZero { .. } => 2,
            Instruction::LoadHLFrom16Imm { .. } => 3,
            Instruction::LoadAFromHLAndInc => 1,
            Instruction::LoadBFromA => 1,
            Instruction::Jump { .. } => 3,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Instruction::Noop => format!("NOP"),
            Instruction::LoadCFrom8Imm { new_value } => format!("LD C, ${new_value:02X}"),
            Instruction::LoadDEFrom16Imm { new_value } => format!("LD DE, ${new_value:04X}"),
            Instruction::StoreAAtDE => format!("LD (DE), A"),
            Instruction::IncrementD => format!("INC D"),
            Instruction::IncrementE => format!("INC E"),
            Instruction::JumpRelativeIfNotZero { offset } => format!(
                "JR NZ, {sign}{mag:#X}",
                sign = if offset.is_positive() { '+' } else { '-' },
                mag = offset.abs()
            ),
            Instruction::LoadHLFrom16Imm { new_value } => format!("LD HL, ${new_value:04X}"),
            Instruction::LoadAFromHLAndInc => format!("LD A, (HL+)"),
            Instruction::LoadBFromA => format!("LD B, A"),
            Instruction::Jump { address } => format!("JP ${address:04X}"),
        };
        write!(f, "{str}")
    }
}
