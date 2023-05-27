use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    Noop,
    LoadBCFrom16Imm { new_value: u16 },
    IncrementBC,
    IncrementB,
    DecrementB,
    LoadBFrom8Imm { new_value: u8 },
    StoreSPAt16Imm { address: u16 },
    AddHLWithBC,
    DecrementBC,
    IncrementC,
    DecrementC,
    LoadCFrom8Imm { new_value: u8 },
    LoadDEFrom16Imm { new_value: u16 },
    StoreAAtDE,
    IncrementDE,
    IncrementD,
    LoadDFrom8Imm { new_value: u8 },
    JumpRelative { offset: i8 },
    AddHLWithDE,
    LoadAFromDE,
    DecrementDE,
    IncrementE,
    DecrementE,
    LoadEFrom8Imm { new_value: u8 },
    RotateRightWithCarryA,
    JumpRelativeIfNotZero { offset: i8 },
    LoadHLFrom16Imm { new_value: u16 },
    StoreAAtHLAndIncrement,
    IncrementHL,
    IncrementH,
    DecrementH,
    LoadHFrom8Imm { new_value: u8 },
    DecimalAdjustA,
    JumpRelativeIfZero { offset: i8 },
    AddHLWithHL,
    LoadAFromHLAndInc,
    DecrementHL,
    IncrementL,
    DecrementL,
    LoadLFrom8Imm { new_value: u8 },
    ComplimentA,
    JumpRelativeIfNotCarry { offset: i8 },
    LoadSPFrom16Imm { new_value: u16 },
    StoreAAtHLAndDecrement,
    IncrementSP,
    IncrementAtHL,
    DecrementAtHL,
    Store8ImmAtHL { value: u8 },
    JumpRelativeIfCarry { offset: i8 },
    AddSPToHL,
    DecrementSP,
    IncrementA,
    DecrementA,
    LoadAFrom8Imm { new_value: u8 },
    LoadBFromB,
    LoadBFromC,
    LoadBFromD,
    LoadBFromE,
    LoadBFromH,
    LoadBFromL,
    LoadBFromHL,
    LoadBFromA,
    LoadCFromB,
    LoadCFromC,
    LoadCFromD,
    LoadCFromE,
    LoadCFromH,
    LoadCFromL,
    LoadCFromHL,
    LoadCFromA,
    LoadDFromB,
    LoadDFromC,
    LoadDFromD,
    LoadDFromE,
    LoadDFromH,
    LoadDFromL,
    LoadDFromHL,
    LoadDFromA,
    LoadEFromB,
    LoadEFromC,
    LoadEFromD,
    LoadEFromE,
    LoadEFromH,
    LoadEFromL,
    LoadEFromHL,
    LoadEFromA,
    LoadHFromB,
    LoadHFromC,
    LoadHFromD,
    LoadHFromE,
    LoadHFromH,
    LoadHFromL,
    LoadHFromHL,
    LoadHFromA,
    LoadLFromB,
    LoadLFromC,
    LoadLFromD,
    LoadLFromE,
    LoadLFromH,
    LoadLFromL,
    LoadLFromHL,
    LoadLFromA,
    StoreBAtHL,
    StoreCAtHL,
    StoreDAtHL,
    StoreEAtHL,
    StoreHAtHL,
    StoreLAtHL,
    StoreAAtHL,
    LoadAFromB,
    LoadAFromC,
    LoadAFromD,
    LoadAFromE,
    LoadAFromH,
    LoadAFromL,
    LoadAFromHL,
    LoadAFromA,
    AddAWithB,
    AddWithCarryAWithH,
    XorCWithA,
    XorEWithA,
    XorLWithA,
    XorHLWithA,
    XorAWithA,
    OrBWithA,
    OrCWithA,
    OrHLWithA,
    OrAWithA,
    CompareAWithB,
    CompareAWithC,
    CompareAWithD,
    CompareAWithE,
    ReturnIfNotZero,
    PopBC,
    JumpIfNotZero { address: u16 },
    Jump { address: u16 },
    CallIfNotZero { address: u16 },
    PushBC,
    AddAWith8Imm { value: u8 },
    Reset0,
    ReturnIfZero,
    Return,
    JumpIfZero { address: u16 },
    Complex(ComplexInstruction),
    CallIfZero { address: u16 },
    Call { address: u16 },
    AddWithCarryAWith8Imm { value: u8 },
    Reset1,
    ReturnIfNotCarry,
    PopDE,
    JumpIfNotCarry { address: u16 },
    CallIfNotCarry { address: u16 },
    PushDE,
    SubtractAWith8Imm { value: u8 },
    Reset2,
    ReturnIfCarry,
    ReturnInterrupt,
    JumpIfCarry { address: u16 },
    CallIfCarry { address: u16 },
    SubtractWithCarryAWith8Imm { value: u8 },
    Reset3,
    OutputAToPort8Imm { address: u8 },
    PopHL,
    OutputAToPortC,
    PushHL,
    AndAWith8Imm { value: u8 },
    Reset4,
    AddSPWithS8Imm { value: i8 },
    JumpHL,
    StoreAAt16Imm { address: u16 },
    Xor8ImmWithA { value: u8 },
    Reset5,
    LoadAFromPort8Imm { address: u8 },
    LoadAFrom16Imm { address: u16 },
    PopAF,
    LoadAFromPortC,
    DisableInterrupts,
    PushAF,
    OrAWith8Imm { value: u8 },
    Reset6,
    LoadHLFromSPWithS8Imm { value: i8 },
    LoadSPFromHL,
    EnableInterrupts,
    CompareAWith8Imm { value: u8 },
    Reset7,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ComplexInstruction {
    RotateRightWithCarryB,
    RotateRightWithCarryC,
    RotateRightWithCarryD,
    RotateRightWithCarryE,
    ShiftLeftLogicalAtHL,
    SwapB,
    SwapA,
    ShiftRightLogicalB,
    ShiftRightLogicalA,
    Bit5AtHL,
}

impl Instruction {
    pub fn size(&self) -> u16 {
        match self {
            Instruction::Noop => 1,
            Instruction::LoadBCFrom16Imm { .. } => 3,
            Instruction::IncrementBC => 1,
            Instruction::IncrementB => 1,
            Instruction::DecrementB => 1,
            Instruction::LoadBFrom8Imm { .. } => 2,
            Instruction::StoreSPAt16Imm { .. } => 3,
            Instruction::AddHLWithBC => 1,
            Instruction::DecrementBC => 1,
            Instruction::IncrementC => 1,
            Instruction::DecrementC => 1,
            Instruction::LoadCFrom8Imm { .. } => 2,
            Instruction::LoadDEFrom16Imm { .. } => 3,
            Instruction::StoreAAtDE => 1,
            Instruction::IncrementDE => 1,
            Instruction::IncrementD => 1,
            Instruction::LoadDFrom8Imm { .. } => 2,
            Instruction::JumpRelative { .. } => 2,
            Instruction::AddHLWithDE => 1,
            Instruction::LoadAFromDE => 1,
            Instruction::DecrementDE => 1,
            Instruction::IncrementE => 1,
            Instruction::DecrementE => 1,
            Instruction::LoadEFrom8Imm { .. } => 2,
            Instruction::RotateRightWithCarryA => 1,
            Instruction::JumpRelativeIfNotZero { .. } => 2,
            Instruction::LoadHLFrom16Imm { .. } => 3,
            Instruction::StoreAAtHLAndIncrement => 1,
            Instruction::IncrementHL => 1,
            Instruction::IncrementH => 1,
            Instruction::DecrementH => 1,
            Instruction::LoadHFrom8Imm { .. } => 2,
            Instruction::DecimalAdjustA => 1,
            Instruction::JumpRelativeIfZero { .. } => 2,
            Instruction::AddHLWithHL => 1,
            Instruction::LoadAFromHLAndInc => 1,
            Instruction::DecrementHL => 1,
            Instruction::IncrementL => 1,
            Instruction::DecrementL => 1,
            Instruction::LoadLFrom8Imm { .. } => 2,
            Instruction::ComplimentA => 1,
            Instruction::JumpRelativeIfNotCarry { .. } => 2,
            Instruction::LoadSPFrom16Imm { .. } => 3,
            Instruction::StoreAAtHLAndDecrement => 1,
            Instruction::IncrementSP => 1,
            Instruction::IncrementAtHL => 1,
            Instruction::DecrementAtHL => 1,
            Instruction::Store8ImmAtHL { .. } => 2,
            Instruction::JumpRelativeIfCarry { .. } => 2,
            Instruction::AddSPToHL => 1,
            Instruction::DecrementSP => 1,
            Instruction::IncrementA => 1,
            Instruction::DecrementA => 1,
            Instruction::LoadAFrom8Imm { .. } => 2,
            Instruction::LoadBFromB => 1,
            Instruction::LoadBFromC => 1,
            Instruction::LoadBFromD => 1,
            Instruction::LoadBFromE => 1,
            Instruction::LoadBFromH => 1,
            Instruction::LoadBFromL => 1,
            Instruction::LoadBFromHL => 1,
            Instruction::LoadBFromA => 1,
            Instruction::LoadCFromB => 1,
            Instruction::LoadCFromC => 1,
            Instruction::LoadCFromD => 1,
            Instruction::LoadCFromE => 1,
            Instruction::LoadCFromH => 1,
            Instruction::LoadCFromL => 1,
            Instruction::LoadCFromHL => 1,
            Instruction::LoadCFromA => 1,
            Instruction::LoadDFromB => 1,
            Instruction::LoadDFromC => 1,
            Instruction::LoadDFromD => 1,
            Instruction::LoadDFromE => 1,
            Instruction::LoadDFromH => 1,
            Instruction::LoadDFromL => 1,
            Instruction::LoadDFromHL => 1,
            Instruction::LoadDFromA => 1,
            Instruction::LoadEFromB => 1,
            Instruction::LoadEFromC => 1,
            Instruction::LoadEFromD => 1,
            Instruction::LoadEFromE => 1,
            Instruction::LoadEFromH => 1,
            Instruction::LoadEFromL => 1,
            Instruction::LoadEFromHL => 1,
            Instruction::LoadEFromA => 1,
            Instruction::LoadHFromB => 1,
            Instruction::LoadHFromC => 1,
            Instruction::LoadHFromD => 1,
            Instruction::LoadHFromE => 1,
            Instruction::LoadHFromH => 1,
            Instruction::LoadHFromL => 1,
            Instruction::LoadHFromHL => 1,
            Instruction::LoadHFromA => 1,
            Instruction::LoadLFromB => 1,
            Instruction::LoadLFromC => 1,
            Instruction::LoadLFromD => 1,
            Instruction::LoadLFromE => 1,
            Instruction::LoadLFromH => 1,
            Instruction::LoadLFromL => 1,
            Instruction::LoadLFromHL => 1,
            Instruction::LoadLFromA => 1,
            Instruction::StoreBAtHL => 1,
            Instruction::StoreCAtHL => 1,
            Instruction::StoreDAtHL => 1,
            Instruction::StoreEAtHL => 1,
            Instruction::StoreHAtHL => 1,
            Instruction::StoreLAtHL => 1,
            Instruction::StoreAAtHL => 1,
            Instruction::LoadAFromB => 1,
            Instruction::LoadAFromC => 1,
            Instruction::LoadAFromD => 1,
            Instruction::LoadAFromE => 1,
            Instruction::LoadAFromH => 1,
            Instruction::LoadAFromL => 1,
            Instruction::LoadAFromHL => 1,
            Instruction::LoadAFromA => 1,
            Instruction::AddAWithB => 1,
            Instruction::AddWithCarryAWithH => 1,
            Instruction::XorCWithA => 1,
            Instruction::XorEWithA => 1,
            Instruction::XorLWithA => 1,
            Instruction::XorHLWithA => 1,
            Instruction::XorAWithA => 1,
            Instruction::OrBWithA => 1,
            Instruction::OrCWithA => 1,
            Instruction::OrHLWithA => 1,
            Instruction::OrAWithA => 1,
            Instruction::CompareAWithB => 1,
            Instruction::CompareAWithC => 1,
            Instruction::CompareAWithD => 1,
            Instruction::CompareAWithE => 1,
            Instruction::ReturnIfNotZero => 1,
            Instruction::PopBC => 1,
            Instruction::JumpIfNotZero { .. } => 3,
            Instruction::Jump { .. } => 3,
            Instruction::CallIfNotZero { .. } => 3,
            Instruction::PushBC => 1,
            Instruction::AddAWith8Imm { .. } => 2,
            Instruction::Reset0 => 1,
            Instruction::ReturnIfZero => 1,
            Instruction::Return => 1,
            Instruction::JumpIfZero { .. } => 3,
            Instruction::Complex(..) => 2,
            Instruction::CallIfZero { .. } => 3,
            Instruction::Call { .. } => 3,
            Instruction::AddWithCarryAWith8Imm { .. } => 2,
            Instruction::Reset1 => 1,
            Instruction::ReturnIfNotCarry => 1,
            Instruction::PopDE => 1,
            Instruction::JumpIfNotCarry { .. } => 3,
            Instruction::CallIfNotCarry { .. } => 3,
            Instruction::PushDE => 1,
            Instruction::SubtractAWith8Imm { .. } => 2,
            Instruction::Reset2 => 1,
            Instruction::ReturnIfCarry => 1,
            Instruction::ReturnInterrupt => 1,
            Instruction::JumpIfCarry { .. } => 3,
            Instruction::CallIfCarry { .. } => 3,
            Instruction::SubtractWithCarryAWith8Imm { .. } => 2,
            Instruction::Reset3 => 1,
            Instruction::OutputAToPort8Imm { .. } => 2,
            Instruction::PopHL => 1,
            Instruction::OutputAToPortC => 1,
            Instruction::PushHL => 1,
            Instruction::AndAWith8Imm { .. } => 2,
            Instruction::Reset4 => 1,
            Instruction::AddSPWithS8Imm { .. } => 2,
            Instruction::JumpHL => 1,
            Instruction::StoreAAt16Imm { .. } => 3,
            Instruction::Xor8ImmWithA { .. } => 2,
            Instruction::Reset5 => 1,
            Instruction::LoadAFromPort8Imm { .. } => 2,
            Instruction::LoadAFrom16Imm { .. } => 3,
            Instruction::PopAF => 1,
            Instruction::LoadAFromPortC => 1,
            Instruction::DisableInterrupts => 1,
            Instruction::PushAF => 1,
            Instruction::OrAWith8Imm { .. } => 2,
            Instruction::Reset6 => 1,
            Instruction::LoadHLFromSPWithS8Imm { .. } => 2,
            Instruction::LoadSPFromHL => 1,
            Instruction::EnableInterrupts => 1,
            Instruction::CompareAWith8Imm { .. } => 2,
            Instruction::Reset7 => 1,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Instruction::Noop => format!("NOP"),
            Instruction::LoadBCFrom16Imm { new_value } => format!("LD BC, ${new_value:04X}"),
            Instruction::IncrementBC => format!("INC BC"),
            Instruction::IncrementB => format!("INC B"),
            Instruction::DecrementB => format!("DEC B"),
            Instruction::LoadBFrom8Imm { new_value } => format!("LD B, ${new_value:02X}"),
            Instruction::StoreSPAt16Imm { address } => format!("LD ${address:04X}, SP"),
            Instruction::AddHLWithBC => format!("ADD HL, BC"),
            Instruction::DecrementBC => format!("DEC BC"),
            Instruction::IncrementC => format!("INC C"),
            Instruction::DecrementC => format!("DEC C"),
            Instruction::LoadCFrom8Imm { new_value } => format!("LD C, ${new_value:02X}"),
            Instruction::LoadDEFrom16Imm { new_value } => format!("LD DE, ${new_value:04X}"),
            Instruction::StoreAAtDE => format!("LD (DE), A"),
            Instruction::IncrementDE => format!("INC DE"),
            Instruction::IncrementD => format!("INC D"),
            Instruction::LoadDFrom8Imm { new_value } => format!("LD D, ${new_value:02X}"),
            Instruction::JumpRelative { offset } => format!(
                "JR {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::AddHLWithDE => format!("ADD HL, DE"),
            Instruction::LoadAFromDE => format!("LD A, (DE)"),
            Instruction::DecrementDE => format!("DEC DE"),
            Instruction::IncrementE => format!("INC E"),
            Instruction::DecrementE => format!("DEC E"),
            Instruction::LoadEFrom8Imm { new_value } => format!("LD E, ${new_value:02X}"),
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
            Instruction::DecrementH => format!("DEC H"),
            Instruction::LoadHFrom8Imm { new_value } => format!("LD H, ${new_value:02X}"),
            Instruction::DecimalAdjustA => format!("DAA"),
            Instruction::JumpRelativeIfZero { offset } => format!(
                "JR Z, {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::AddHLWithHL => format!("ADD HL, HL"),
            Instruction::LoadAFromHLAndInc => format!("LD A, (HL+)"),
            Instruction::DecrementHL => format!("DEC HL"),
            Instruction::IncrementL => format!("INC L"),
            Instruction::DecrementL => format!("DEC L"),
            Instruction::LoadLFrom8Imm { new_value } => format!("LD L, ${new_value:02X}"),
            Instruction::ComplimentA => format!("CPL"),
            Instruction::JumpRelativeIfNotCarry { offset } => format!(
                "JR NC, {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::LoadSPFrom16Imm { new_value } => format!("LD SP, ${new_value:04X}"),
            Instruction::StoreAAtHLAndDecrement => format!("LD (HL-), A"),
            Instruction::IncrementSP => format!("INC SP"),
            Instruction::IncrementAtHL => format!("INC (HL)"),
            Instruction::DecrementAtHL => format!("DEC (HL)"),
            Instruction::Store8ImmAtHL { value } => format!("LD (HL), ${value:02X}"),
            Instruction::JumpRelativeIfCarry { offset } => format!(
                "JR C, {sign}{mag:#X}",
                sign = if offset.is_negative() { '-' } else { '+' },
                mag = offset.abs()
            ),
            Instruction::AddSPToHL => format!("ADD HL, SP"),
            Instruction::DecrementSP => format!("DEC SP"),
            Instruction::IncrementA => format!("INC A"),
            Instruction::DecrementA => format!("DEC A"),
            Instruction::LoadAFrom8Imm { new_value } => format!("LD A, ${new_value:02X}"),
            Instruction::LoadBFromB => format!("LD B, B"),
            Instruction::LoadBFromC => format!("LD B, C"),
            Instruction::LoadBFromD => format!("LD B, D"),
            Instruction::LoadBFromE => format!("LD B, E"),
            Instruction::LoadBFromH => format!("LD B, H"),
            Instruction::LoadBFromL => format!("LD B, L"),
            Instruction::LoadBFromHL => format!("LD B, (HL)"),
            Instruction::LoadBFromA => format!("LD B, A"),
            Instruction::LoadCFromB => format!("LD C, B"),
            Instruction::LoadCFromC => format!("LD C, C"),
            Instruction::LoadCFromD => format!("LD C, D"),
            Instruction::LoadCFromE => format!("LD C, E"),
            Instruction::LoadCFromH => format!("LD C, H"),
            Instruction::LoadCFromL => format!("LD C, L"),
            Instruction::LoadCFromHL => format!("LD C, (HL)"),
            Instruction::LoadCFromA => format!("LD C, A"),
            Instruction::LoadDFromB => format!("LD D, B"),
            Instruction::LoadDFromC => format!("LD D, C"),
            Instruction::LoadDFromD => format!("LD D, D"),
            Instruction::LoadDFromE => format!("LD D, E"),
            Instruction::LoadDFromH => format!("LD D, H"),
            Instruction::LoadDFromL => format!("LD D, L"),
            Instruction::LoadDFromHL => format!("LD D, (HL)"),
            Instruction::LoadDFromA => format!("LD D, A"),
            Instruction::LoadEFromB => format!("LD E, B"),
            Instruction::LoadEFromC => format!("LD E, C"),
            Instruction::LoadEFromD => format!("LD E, D"),
            Instruction::LoadEFromE => format!("LD E, E"),
            Instruction::LoadEFromH => format!("LD E, H"),
            Instruction::LoadEFromL => format!("LD E, L"),
            Instruction::LoadEFromHL => format!("LD E, (HL)"),
            Instruction::LoadEFromA => format!("LD E, A"),
            Instruction::LoadHFromB => format!("LD H, B"),
            Instruction::LoadHFromC => format!("LD H, C"),
            Instruction::LoadHFromD => format!("LD H, D"),
            Instruction::LoadHFromE => format!("LD H, E"),
            Instruction::LoadHFromH => format!("LD H, H"),
            Instruction::LoadHFromL => format!("LD H, L"),
            Instruction::LoadHFromHL => format!("LD H, (HL)"),
            Instruction::LoadHFromA => format!("LD H, A"),
            Instruction::LoadLFromB => format!("LD L, B"),
            Instruction::LoadLFromC => format!("LD L, C"),
            Instruction::LoadLFromD => format!("LD L, D"),
            Instruction::LoadLFromE => format!("LD L, E"),
            Instruction::LoadLFromH => format!("LD L, H"),
            Instruction::LoadLFromL => format!("LD L, L"),
            Instruction::LoadLFromHL => format!("LD L, (HL)"),
            Instruction::LoadLFromA => format!("LD L, A"),
            Instruction::StoreBAtHL => format!("LD (HL), B"),
            Instruction::StoreCAtHL => format!("LD (HL), C"),
            Instruction::StoreDAtHL => format!("LD (HL), D"),
            Instruction::StoreEAtHL => format!("LD (HL), E"),
            Instruction::StoreHAtHL => format!("LD (HL), H"),
            Instruction::StoreLAtHL => format!("LD (HL), L"),
            Instruction::StoreAAtHL => format!("LD (HL), A"),
            Instruction::LoadAFromB => format!("LD A, B"),
            Instruction::LoadAFromC => format!("LD A, C"),
            Instruction::LoadAFromD => format!("LD A, D"),
            Instruction::LoadAFromE => format!("LD A, E"),
            Instruction::LoadAFromH => format!("LD A, H"),
            Instruction::LoadAFromL => format!("LD A, L"),
            Instruction::LoadAFromHL => format!("LD A, (HL)"),
            Instruction::LoadAFromA => format!("LD A, A"),
            Instruction::AddAWithB => format!("ADD A, B"),
            Instruction::AddWithCarryAWithH => format!("ADC A, H"),
            Instruction::XorCWithA => format!("XOR C"),
            Instruction::XorEWithA => format!("XOR E"),
            Instruction::XorLWithA => format!("XOR L"),
            Instruction::XorHLWithA => format!("XOR (HL)"),
            Instruction::XorAWithA => format!("XOR A"),
            Instruction::OrBWithA => format!("OR B"),
            Instruction::OrCWithA => format!("OR C"),
            Instruction::OrHLWithA => format!("OR (HL), A"),
            Instruction::OrAWithA => format!("OR A"),
            Instruction::CompareAWithB => format!("CP B"),
            Instruction::CompareAWithC => format!("CP C"),
            Instruction::CompareAWithD => format!("CP D"),
            Instruction::CompareAWithE => format!("CP E"),
            Instruction::ReturnIfNotZero => format!("RET NZ"),
            Instruction::PopBC => format!("POP BC"),
            Instruction::JumpIfNotZero { address } => format!("JP NZ, ${address:04X}"),
            Instruction::Jump { address } => format!("JP ${address:04X}"),
            Instruction::CallIfNotZero { address } => format!("CALL NZ, ${address:04X}"),
            Instruction::PushBC => format!("PUSH BC"),
            Instruction::AddAWith8Imm { value } => format!("ADD A, ${value:02X}"),
            Instruction::Reset0 => format!("RST 0"),
            Instruction::ReturnIfZero => format!("RET Z"),
            Instruction::Return => format!("RET"),
            Instruction::JumpIfZero { address } => format!("JP Z, ${address:04X}"),
            Instruction::Complex(ci) => format!("{ci}"),
            Instruction::CallIfZero { address } => format!("CALL Z, ${address:04X}"),
            Instruction::Call { address } => format!("CALL ${address:04X}"),
            Instruction::AddWithCarryAWith8Imm { value } => format!("ADC A, ${value:02X}"),
            Instruction::Reset1 => format!("RST 1"),
            Instruction::ReturnIfNotCarry => format!("RET NC"),
            Instruction::PopDE => format!("POP DE"),
            Instruction::JumpIfNotCarry { address } => format!("JP NC, ${address:04X}"),
            Instruction::CallIfNotCarry { address } => format!("CALL NC, ${address:04X}"),
            Instruction::PushDE => format!("PUSH DE"),
            Instruction::SubtractAWith8Imm { value } => format!("SUB A, ${value:02X}"),
            Instruction::Reset2 => format!("RST 2"),
            Instruction::ReturnIfCarry => format!("RET C"),
            Instruction::ReturnInterrupt => format!("RETI"),
            Instruction::JumpIfCarry { address } => format!("JP C, ${address:04X}"),
            Instruction::CallIfCarry { address } => format!("CALL C, ${address:04X}"),
            Instruction::SubtractWithCarryAWith8Imm { value } => format!("SBC ${value:02X}"),
            Instruction::Reset3 => format!("RST 3"),
            Instruction::OutputAToPort8Imm { address } => format!("LD ${address:02X}, A"),
            Instruction::PopHL => format!("POP HL"),
            Instruction::OutputAToPortC => format!("LD (C), A"),
            Instruction::PushHL => format!("PUSH HL"),
            Instruction::AndAWith8Imm { value } => format!("AND ${value:02X}"),
            Instruction::Reset4 => format!("RST 4"),
            Instruction::AddSPWithS8Imm { value } => format!(
                "ADD SP, {sign}${mag:02X}",
                sign = if value.is_negative() { '-' } else { '+' },
                mag = value.abs()
            ),
            Instruction::JumpHL => format!("JP HL"),
            Instruction::StoreAAt16Imm { address } => format!("LD ${address:04X}, A"),
            Instruction::Xor8ImmWithA { value } => format!("XOR ${value:02X}"),
            Instruction::Reset5 => format!("RST 5"),
            Instruction::LoadAFromPort8Imm { address } => format!("LD A, ${address:02X}"),
            Instruction::LoadAFrom16Imm { address } => format!("LD A, ${address:04X}"),
            Instruction::PopAF => format!("POP AF"),
            Instruction::LoadAFromPortC => format!("LD A, (C)"),
            Instruction::DisableInterrupts => format!("DI"),
            Instruction::PushAF => format!("PUSH AF"),
            Instruction::OrAWith8Imm { value } => format!("OR ${value:02X}"),
            Instruction::Reset6 => format!("RST 6"),
            Instruction::LoadHLFromSPWithS8Imm { value } => format!(
                "LD HL, SP {sign} ${mag:02X}",
                sign = if value.is_negative() { '-' } else { '+' },
                mag = value.abs()
            ),
            Instruction::LoadSPFromHL => format!("LD SP, HL"),
            Instruction::EnableInterrupts => format!("EI"),
            Instruction::CompareAWith8Imm { value } => format!("CP ${value:02X}"),
            Instruction::Reset7 => format!("RST 7"),
        };
        write!(f, "{str}")
    }
}

impl Display for ComplexInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ComplexInstruction::RotateRightWithCarryB => format!("RR B"),
            ComplexInstruction::RotateRightWithCarryC => format!("RR C"),
            ComplexInstruction::RotateRightWithCarryD => format!("RR D"),
            ComplexInstruction::RotateRightWithCarryE => format!("RR E"),
            ComplexInstruction::ShiftLeftLogicalAtHL => format!("SLA (HL)"),
            ComplexInstruction::SwapB => format!("SWAP B"),
            ComplexInstruction::SwapA => format!("SWAP A"),
            ComplexInstruction::ShiftRightLogicalB => format!("SRL B"),
            ComplexInstruction::ShiftRightLogicalA => format!("SRL A"),
            ComplexInstruction::Bit5AtHL => format!("BIT 5, (HL)"),
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
            0x04 => Instruction::IncrementB,
            0x05 => Instruction::DecrementB,
            0x06 => {
                let new_value = self.u8_imm();
                Instruction::LoadBFrom8Imm { new_value }
            }
            0x08 => {
                let address = self.u16_imm();
                Instruction::StoreSPAt16Imm { address }
            }
            0x09 => Instruction::AddHLWithBC,
            0x0B => Instruction::DecrementBC,
            0x0C => Instruction::IncrementC,
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
            0x16 => {
                let new_value = self.u8_imm();
                Instruction::LoadDFrom8Imm { new_value }
            }
            0x18 => {
                let offset = self.s8_imm();
                Instruction::JumpRelative { offset }
            }
            0x19 => Instruction::AddHLWithDE,
            0x1A => Instruction::LoadAFromDE,
            0x1B => Instruction::DecrementDE,
            0x1C => Instruction::IncrementE,
            0x1D => Instruction::DecrementE,
            0x1E => {
                let new_value = self.u8_imm();
                Instruction::LoadEFrom8Imm { new_value }
            }
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
            0x25 => Instruction::DecrementH,
            0x26 => {
                let new_value = self.u8_imm();
                Instruction::LoadHFrom8Imm { new_value }
            }
            0x27 => Instruction::DecimalAdjustA,
            0x28 => {
                let offset = self.s8_imm();
                Instruction::JumpRelativeIfZero { offset }
            }
            0x29 => Instruction::AddHLWithHL,
            0x2A => Instruction::LoadAFromHLAndInc,
            0x2B => Instruction::DecrementHL,
            0x2C => Instruction::IncrementL,
            0x2D => Instruction::DecrementL,
            0x2E => {
                let new_value = self.u8_imm();
                Instruction::LoadLFrom8Imm { new_value }
            }
            0x2F => Instruction::ComplimentA,
            0x30 => {
                let offset = self.s8_imm();
                Instruction::JumpRelativeIfNotCarry { offset }
            }
            0x31 => {
                let new_value = self.u16_imm();
                Instruction::LoadSPFrom16Imm { new_value }
            }
            0x32 => Instruction::StoreAAtHLAndDecrement,
            0x33 => Instruction::IncrementSP,
            0x34 => Instruction::IncrementAtHL,
            0x35 => Instruction::DecrementAtHL,
            0x36 => {
                let value = self.u8_imm();
                Instruction::Store8ImmAtHL { value }
            }
            0x38 => {
                let offset = self.s8_imm();
                Instruction::JumpRelativeIfCarry { offset }
            }
            0x39 => Instruction::AddSPToHL,
            0x3B => Instruction::DecrementSP,
            0x3C => Instruction::IncrementA,
            0x3D => Instruction::DecrementA,
            0x3E => {
                let new_value = self.u8_imm();
                Instruction::LoadAFrom8Imm { new_value }
            }
            0x40 => Instruction::LoadBFromB,
            0x41 => Instruction::LoadBFromC,
            0x42 => Instruction::LoadBFromD,
            0x43 => Instruction::LoadBFromE,
            0x44 => Instruction::LoadBFromH,
            0x45 => Instruction::LoadBFromL,
            0x46 => Instruction::LoadBFromHL,
            0x47 => Instruction::LoadBFromA,
            0x48 => Instruction::LoadCFromB,
            0x49 => Instruction::LoadCFromC,
            0x4A => Instruction::LoadCFromD,
            0x4B => Instruction::LoadCFromE,
            0x4C => Instruction::LoadCFromH,
            0x4D => Instruction::LoadCFromL,
            0x4E => Instruction::LoadCFromHL,
            0x4F => Instruction::LoadCFromA,
            0x50 => Instruction::LoadDFromB,
            0x51 => Instruction::LoadDFromC,
            0x52 => Instruction::LoadDFromD,
            0x53 => Instruction::LoadDFromE,
            0x54 => Instruction::LoadDFromH,
            0x55 => Instruction::LoadDFromL,
            0x56 => Instruction::LoadDFromHL,
            0x57 => Instruction::LoadDFromA,
            0x58 => Instruction::LoadEFromB,
            0x59 => Instruction::LoadEFromC,
            0x5A => Instruction::LoadEFromD,
            0x5B => Instruction::LoadEFromE,
            0x5C => Instruction::LoadEFromH,
            0x5D => Instruction::LoadEFromL,
            0x5E => Instruction::LoadEFromHL,
            0x5F => Instruction::LoadEFromA,
            0x60 => Instruction::LoadHFromB,
            0x61 => Instruction::LoadHFromC,
            0x62 => Instruction::LoadHFromD,
            0x63 => Instruction::LoadHFromE,
            0x64 => Instruction::LoadHFromH,
            0x65 => Instruction::LoadHFromL,
            0x66 => Instruction::LoadHFromHL,
            0x67 => Instruction::LoadHFromA,
            0x68 => Instruction::LoadLFromB,
            0x69 => Instruction::LoadLFromC,
            0x6A => Instruction::LoadLFromD,
            0x6B => Instruction::LoadLFromE,
            0x6C => Instruction::LoadLFromH,
            0x6D => Instruction::LoadLFromL,
            0x6E => Instruction::LoadLFromHL,
            0x6F => Instruction::LoadLFromA,
            0x70 => Instruction::StoreBAtHL,
            0x71 => Instruction::StoreCAtHL,
            0x72 => Instruction::StoreDAtHL,
            0x73 => Instruction::StoreEAtHL,
            0x74 => Instruction::StoreHAtHL,
            0x75 => Instruction::StoreLAtHL,
            0x77 => Instruction::StoreAAtHL,
            0x78 => Instruction::LoadAFromB,
            0x79 => Instruction::LoadAFromC,
            0x7A => Instruction::LoadAFromD,
            0x7B => Instruction::LoadAFromE,
            0x7C => Instruction::LoadAFromH,
            0x7D => Instruction::LoadAFromL,
            0x7E => Instruction::LoadAFromHL,
            0x7F => Instruction::LoadAFromA,
            0x80 => Instruction::AddAWithB,
            0x8C => Instruction::AddWithCarryAWithH,
            0xA9 => Instruction::XorCWithA,
            0xAB => Instruction::XorEWithA,
            0xAD => Instruction::XorLWithA,
            0xAE => Instruction::XorHLWithA,
            0xAF => Instruction::XorAWithA,
            0xB0 => Instruction::OrBWithA,
            0xB1 => Instruction::OrCWithA,
            0xB6 => Instruction::OrHLWithA,
            0xB7 => Instruction::OrAWithA,
            0xB8 => Instruction::CompareAWithB,
            0xB9 => Instruction::CompareAWithC,
            0xBA => Instruction::CompareAWithD,
            0xBB => Instruction::CompareAWithE,
            0xC0 => Instruction::ReturnIfNotZero,
            0xC1 => Instruction::PopBC,
            0xC2 => {
                let address = self.u16_imm();
                Instruction::JumpIfNotZero { address }
            }
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
            0xC7 => Instruction::Reset0,
            0xC8 => Instruction::ReturnIfZero,
            0xC9 => Instruction::Return,
            0xCA => {
                let address = self.u16_imm();
                Instruction::JumpIfZero { address }
            }
            0xCB => Instruction::Complex(InstructionBuilder::parse_complex(self.u8_imm())?),
            0xCC => {
                let address = self.u16_imm();
                Instruction::CallIfZero { address }
            }
            0xCD => {
                let address = self.u16_imm();
                Instruction::Call { address }
            }
            0xCE => {
                let value = self.u8_imm();
                Instruction::AddWithCarryAWith8Imm { value }
            }
            0xCF => Instruction::Reset1,
            0xD0 => Instruction::ReturnIfNotCarry,
            0xD1 => Instruction::PopDE,
            0xD2 => {
                let address = self.u16_imm();
                Instruction::JumpIfNotCarry { address }
            }
            0xD4 => {
                let address = self.u16_imm();
                Instruction::CallIfNotCarry { address }
            }
            0xD5 => Instruction::PushDE,
            0xD6 => {
                let value = self.u8_imm();
                Instruction::SubtractAWith8Imm { value }
            }
            0xD7 => Instruction::Reset2,
            0xD8 => Instruction::ReturnIfCarry,
            0xD9 => Instruction::ReturnInterrupt,
            0xDA => {
                let address = self.u16_imm();
                Instruction::JumpIfCarry { address }
            }
            0xDC => {
                let address = self.u16_imm();
                Instruction::CallIfCarry { address }
            }
            0xDE => {
                let value = self.u8_imm();
                Instruction::SubtractWithCarryAWith8Imm { value }
            }
            0xDF => Instruction::Reset3,
            0xE0 => {
                let address = self.u8_imm();
                Instruction::OutputAToPort8Imm { address }
            }
            0xE1 => Instruction::PopHL,
            0xE2 => Instruction::OutputAToPortC,
            0xE5 => Instruction::PushHL,
            0xE6 => {
                let value = self.u8_imm();
                Instruction::AndAWith8Imm { value }
            }
            0xE7 => Instruction::Reset4,
            0xE8 => {
                let value = self.s8_imm();
                Instruction::AddSPWithS8Imm { value }
            }
            0xE9 => Instruction::JumpHL,
            0xEA => {
                let address = self.u16_imm();
                Instruction::StoreAAt16Imm { address }
            }
            0xEE => {
                let value = self.u8_imm();
                Instruction::Xor8ImmWithA { value }
            }
            0xEF => Instruction::Reset5,
            0xF0 => {
                let address = self.u8_imm();
                Instruction::LoadAFromPort8Imm { address }
            }
            0xF1 => Instruction::PopAF,
            0xF2 => Instruction::LoadAFromPortC,
            0xF3 => Instruction::DisableInterrupts,
            0xF5 => Instruction::PushAF,
            0xF6 => {
                let value = self.u8_imm();
                Instruction::OrAWith8Imm { value }
            }
            0xF7 => Instruction::Reset6,
            0xF8 => {
                let value = self.s8_imm();
                Instruction::LoadHLFromSPWithS8Imm { value }
            }
            0xF9 => Instruction::LoadSPFromHL,
            0xFA => {
                let address = self.u16_imm();
                Instruction::LoadAFrom16Imm { address }
            }
            0xFB => Instruction::EnableInterrupts,
            0xFE => {
                let value = self.u8_imm();
                Instruction::CompareAWith8Imm { value }
            }
            0xFF => Instruction::Reset7,
            value => Err(InstructionParseError::UnknownInstruction(value))?,
        })
    }

    pub fn parse_complex(op_code: u8) -> Result<ComplexInstruction, InstructionParseError> {
        Ok(match op_code {
            0x18 => ComplexInstruction::RotateRightWithCarryB,
            0x19 => ComplexInstruction::RotateRightWithCarryC,
            0x1A => ComplexInstruction::RotateRightWithCarryD,
            0x1B => ComplexInstruction::RotateRightWithCarryE,
            0x26 => ComplexInstruction::ShiftLeftLogicalAtHL,
            0x30 => ComplexInstruction::SwapB,
            0x37 => ComplexInstruction::SwapA,
            0x38 => ComplexInstruction::ShiftRightLogicalB,
            0x3F => ComplexInstruction::ShiftRightLogicalA,
            0x6E => ComplexInstruction::Bit5AtHL,
            value => Err(InstructionParseError::UnknownComplexInstruction(value))?,
        })
    }
}
