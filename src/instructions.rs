use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Noop,
    LoadBCFrom16Imm { new_value: u16 },
    IncrementBC,
    DecrementC,
    LoadCFrom8Imm { new_value: u8 },
    LoadDEFrom16Imm { new_value: u16 },
    StoreAAtDE,
    IncrementD,
    JumpRelative { offset: i8 },
    IncrementE,
    JumpRelativeIfNotZero { offset: i8 },
    LoadHLFrom16Imm { new_value: u16 },
    IncrementHL,
    LoadAFromHLAndInc,
    LoadSPFrom16Imm { new_value: u16 },
    LoadAFrom8Imm { new_value: u8 },
    LoadBFromA,
    LoadAFromB,
    LoadAFromH,
    LoadAFromL,
    OrCWithA,
    Jump { address: u16 },
    LoadHFromL,
    Return,
    Call { address: u16 },
    OutputAToPort { address: u8 },
    PopHL,
    PushHL,
    StoreAAt16Imm { address: u16 },
    PopAF,
    DisableInterrupts,
    PushAF,
}

impl Instruction {
    pub fn size(&self) -> u16 {
        match self {
            Instruction::Noop => 1,
            Instruction::LoadBCFrom16Imm { .. } => 3,
            Instruction::IncrementBC => 1,
            Instruction::DecrementC => 1,
            Instruction::LoadCFrom8Imm { .. } => 2,
            Instruction::LoadDEFrom16Imm { .. } => 3,
            Instruction::StoreAAtDE => 1,
            Instruction::IncrementD => 1,
            Instruction::JumpRelative { .. } => 2,
            Instruction::IncrementE => 1,
            Instruction::JumpRelativeIfNotZero { .. } => 2,
            Instruction::LoadHLFrom16Imm { .. } => 3,
            Instruction::IncrementHL => 1,
            Instruction::LoadAFromHLAndInc => 1,
            Instruction::LoadSPFrom16Imm { .. } => 3,
            Instruction::LoadAFrom8Imm { .. } => 2,
            Instruction::LoadBFromA => 1,
            Instruction::LoadAFromB => 1,
            Instruction::LoadAFromH => 1,
            Instruction::LoadAFromL => 1,
            Instruction::OrCWithA => 1,
            Instruction::Jump { .. } => 3,
            Instruction::LoadHFromL => 1,
            Instruction::Return => 1,
            Instruction::Call { .. } => 3,
            Instruction::OutputAToPort { .. } => 2,
            Instruction::PopHL => 1,
            Instruction::PushHL => 1,
            Instruction::StoreAAt16Imm { .. } => 3,
            Instruction::PopAF => 1,
            Instruction::DisableInterrupts => 1,
            Instruction::PushAF => 1,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Instruction::Noop => format!("NOP"),
            Instruction::LoadBCFrom16Imm { new_value } => format!("LD BC, ${new_value:04X}"),
            Instruction::IncrementBC => format!("INC BC"),
            Instruction::DecrementC => format!("DEC C"),
            Instruction::LoadCFrom8Imm { new_value } => format!("LD C, ${new_value:02X}"),
            Instruction::LoadDEFrom16Imm { new_value } => format!("LD DE, ${new_value:04X}"),
            Instruction::StoreAAtDE => format!("LD (DE), A"),
            Instruction::IncrementD => format!("INC D"),
            Instruction::JumpRelative { offset } => format!(
                "JR {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::IncrementE => format!("INC E"),
            Instruction::JumpRelativeIfNotZero { offset } => format!(
                "JR NZ, {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::LoadHLFrom16Imm { new_value } => format!("LD HL, ${new_value:04X}"),
            Instruction::IncrementHL => format!("INC HL"),
            Instruction::LoadAFromHLAndInc => format!("LD A, (HL+)"),
            Instruction::LoadSPFrom16Imm { new_value } => format!("LD SP, ${new_value:04X}"),
            Instruction::LoadAFrom8Imm { new_value } => format!("LD A, ${new_value:02X}"),
            Instruction::LoadBFromA => format!("LD B, A"),
            Instruction::LoadAFromB => format!("LD A, B"),
            Instruction::LoadAFromH => format!("LD A, H"),
            Instruction::LoadAFromL => format!("LD A, L"),
            Instruction::OrCWithA => format!("OR C"),
            Instruction::Jump { address } => format!("JP ${address:04X}"),
            Instruction::LoadHFromL => format!("LD H, L"),
            Instruction::Return => format!("RET"),
            Instruction::Call { address } => format!("CALL ${address:04X}"),
            Instruction::OutputAToPort { address } => format!("LD ${address:02X}, A"),
            Instruction::PopHL => format!("POP HL"),
            Instruction::PushHL => format!("PUSH HL"),
            Instruction::StoreAAt16Imm { address } => format!("LD ${address:04X}, A"),
            Instruction::PopAF => format!("POP AF"),
            Instruction::DisableInterrupts => format!("DI"),
            Instruction::PushAF => format!("PUSH AF"),
        };
        write!(f, "{str}")
    }
}
