use std::ops::ControlFlow;

use instructions::{ComplexInstruction, Instruction, InstructionBuilder, InstructionParseError};
use log::{debug, error, info, trace, warn};
use memory_management_unit::MemoryManagementUnit;
use registers::Registers;

pub mod cartridge;
pub mod instructions;
pub mod memory_management_unit;
pub mod registers;

///An enum of all Gameboy Models
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Model {
    ///A rare early variant of DGM bootcode. Only minor differences from DMG
    DMG0,
    ///The original Gameboy
    DMG,
    ///The Gameboy Pocket
    MGB,
    ///The Super Gameboy. A SNES cart that allowed playing Gameboy games on a SNES
    SGB,
    ///The Super Gameboy 2. A successor to the SGB that allowed playing compatible Gameboy games in color
    SGB2,
    ///A rare early variant of CGB bootcode. Only minor differences from CGB
    CGB0,
    ///The Gameboy Color
    CGB,
    ///The Gameboy Color in Gameboy compatibility mode
    CGB_DMG,
    ///An early variant of the AGB bootcode. It contains a TOCTTOU bug
    AGB0,
    ///The Gameboy Advanced
    AGB,
    ///The Gameboy Advance in Gameboy compatibility mode
    AGB_DMG,
}

pub struct Core<'rom> {
    mmu: MemoryManagementUnit<'rom>,
    registers: Registers,
    ime: InterruptMasterEnable,
    dots: u16,
    //TODO: This should be done better
    serial_output: String,
}

impl Core<'_> {
    pub fn new<'rom>(rom: &'rom [u8], model: Model) -> Option<Core<'rom>> {
        let mmu = MemoryManagementUnit::new(rom).ok()?;
        let registers = Registers::init(model);
        Some(Core {
            mmu,
            registers,
            ime: InterruptMasterEnable::Enabled,
            dots: 0,
            serial_output: String::new(),
        })
    }

    fn read_instruction_at(&self, address: u16) -> Instruction {
        let byte = self.mmu.read(address);
        let imm_1 = self.mmu.read(address + 1);
        let imm_2 = self.mmu.read(address + 2);
        let res = InstructionBuilder::new([byte, imm_1, imm_2]).parse();
        match res {
            Ok(instr) => instr,
            Err(InstructionParseError::UnknownInstruction(v)) => {
                todo!("Unknown instruction {v:#04X} @ {:#04X}", self.registers.pc)
            }
            Err(InstructionParseError::UnknownComplexInstruction(v)) => {
                todo!(
                    "Unknown complex instruction 0xCB {v:#02X} @ {:#04X}",
                    self.registers.pc
                )
            }
            Err(InstructionParseError::IllegalInstruction(v)) => {
                panic!("Illegal Instruction {v:#02X} @ {:#04X}", self.registers.pc)
            }
        }
    }
    fn read_instruction(&mut self) -> Instruction {
        self.read_instruction_at(self.registers.pc)
    }

    fn execute(&mut self, instruction: Instruction) {
        self.registers.pc = self.registers.pc.wrapping_add(instruction.size());
        match instruction {
            Instruction::Noop => {}
            Instruction::LoadBCFrom16Imm { new_value } => *self.registers.bc.get_mut() = new_value,
            Instruction::IncrementBC => {
                let bc = self.registers.bc.get_mut();
                *bc = bc.wrapping_add(1);
            }
            Instruction::IncrementB => {
                let b = self.registers.bc.split_mut().high;
                *b = b.wrapping_add(1);
                self.registers
                    .set_zero(*self.registers.bc.split().high == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.bc.split().high & 0x0F == 0);
            }
            Instruction::DecrementB => {
                let c = self.registers.bc.split_mut().high;
                *c = c.wrapping_sub(1);
                self.registers
                    .set_zero(*self.registers.bc.split().high == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.bc.split().low & 0x0F == 0x0F);
            }
            Instruction::LoadBFrom8Imm { new_value } => {
                *self.registers.bc.split_mut().high = new_value
            }
            Instruction::StoreSPAt16Imm { address } => {
                self.mmu.write_u16(address, self.registers.sp)
            }
            Instruction::IncrementC => {
                let c = self.registers.bc.split_mut().low;
                *c = c.wrapping_add(1);
                self.registers.set_zero(*self.registers.bc.split().low == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.bc.split().low & 0x0F == 0);
            }
            Instruction::DecrementC => {
                let c = self.registers.bc.split_mut().low;
                *c = c.wrapping_sub(1);
                self.registers.set_zero(*self.registers.bc.split().low == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.bc.split().low & 0x0F == 0x0F);
            }
            Instruction::LoadCFrom8Imm { new_value } => {
                *self.registers.bc.split_mut().low = new_value
            }
            Instruction::LoadDEFrom16Imm { new_value } => *self.registers.de.get_mut() = new_value,
            Instruction::StoreAAtDE => self.mmu.write(*self.registers.de.get(), self.registers.a),
            Instruction::IncrementDE => {
                let de = self.registers.de.get_mut();
                *de = de.wrapping_add(1);
            }
            Instruction::IncrementD => {
                let d = self.registers.de.split_mut().high;
                *d = d.wrapping_add(1);
                self.registers
                    .set_zero(*self.registers.de.split().high == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.de.split().high & 0x0F == 0);
            }
            Instruction::JumpRelative { offset } => {
                self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
            }
            Instruction::LoadAFromDE => {
                let new_value = self.mmu.read(*self.registers.de.get());
                self.registers.a = new_value;
            }
            Instruction::IncrementE => {
                let e = self.registers.de.split_mut().low;
                *e = e.wrapping_add(1);
                self.registers.set_zero(*self.registers.de.split().low == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.de.split().low & 0x0F == 0);
            }
            Instruction::DecrementE => {
                let e = self.registers.de.split_mut().low;
                *e = e.wrapping_sub(1);
                self.registers.set_zero(*self.registers.de.split().low == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.de.split().low & 0x0F == 0x0F);
            }
            Instruction::RotateRightWithCarryA => {
                let bit7 = (self.registers.carry() as u8) << 7;
                let a = &mut self.registers.a;
                let bit0 = *a & 0b1 != 0;
                *a >>= 1;
                *a |= bit7;
                self.registers.set_zero(false);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(bit0);
            }
            Instruction::JumpRelativeIfNotZero { offset } => {
                if !self.registers.zero() {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
                }
            }
            Instruction::LoadHLFrom16Imm { new_value } => *self.registers.hl.get_mut() = new_value,
            Instruction::StoreAAtHLAndIncrement => {
                let hl = self.registers.hl.get_mut();
                self.mmu.write(*hl, self.registers.a);
                *hl = hl.wrapping_add(1);
            }
            Instruction::IncrementHL => {
                let hl = self.registers.hl.get_mut();
                *hl = hl.wrapping_add(1);
            }
            Instruction::IncrementH => {
                let (res, half_carry) = self.registers.hl.split().high.overflowing_add(1);
                *self.registers.hl.split_mut().high = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementH => {
                let (res, half_carry) = self.registers.hl.split().high.overflowing_sub(1);
                *self.registers.hl.split_mut().high = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadHFrom8Imm { new_value } => {
                *self.registers.hl.split_mut().high = new_value;
            }
            Instruction::JumpRelativeIfZero { offset } => {
                if self.registers.zero() {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
                }
            }
            Instruction::AddHLWithHL => {
                let hl = *self.registers.hl.get();
                let (res, carry) = hl.overflowing_add(hl);
                *self.registers.hl.get_mut() = res;
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(carry);
                self.registers.set_carry(carry);
            }
            Instruction::LoadAFromHLAndInc => {
                let byte = self.mmu.read(*self.registers.hl.get());
                self.registers.a = byte;
                *self.registers.hl.get_mut() += 1;
            }
            Instruction::IncrementL => {
                let (res, half_carry) = self.registers.hl.split().low.overflowing_add(1);
                *self.registers.hl.split_mut().low = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementL => {
                let (res, half_carry) = self.registers.hl.split().low.overflowing_sub(1);
                *self.registers.hl.split_mut().low = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadLFrom8Imm { new_value } => {
                *self.registers.hl.split_mut().low = new_value
            }
            Instruction::JumpRelativeIfNotCarry { offset } => {
                if !self.registers.carry() {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
                }
            }
            Instruction::LoadSPFrom16Imm { new_value } => self.registers.sp = new_value,
            Instruction::StoreAAtHLAndDecrement => {
                let hl = self.registers.hl.get_mut();
                self.mmu.write(*hl, self.registers.a);
                *hl = hl.wrapping_sub(1);
            }
            Instruction::DecrementAtHL => {
                let (res, half_carry) = self.mmu.read(*self.registers.hl.get()).overflowing_sub(1);
                self.mmu.write(*self.registers.hl.get(), res);
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                self.registers.set_half_carry(half_carry);
            }
            Instruction::JumpRelativeIfCarry { offset } => {
                if self.registers.carry() {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
                }
            }
            Instruction::IncrementA => {
                let (res, half_carry) = self.registers.a.overflowing_add(1);
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementA => {
                let (res, half_carry) = self.registers.a.overflowing_sub(1);
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadAFrom8Imm { new_value } => self.registers.a = new_value,
            Instruction::LoadBFromHL => {
                let new_value = self.mmu.read(*self.registers.hl.get());
                *self.registers.bc.split_mut().high = new_value;
            }
            Instruction::LoadBFromA => *self.registers.bc.split_mut().high = self.registers.a,
            Instruction::LoadCFromL => {
                *self.registers.bc.split_mut().low = *self.registers.hl.split().low
            }
            Instruction::LoadCFromHL => {
                let new_value = self.mmu.read(*self.registers.hl.get());
                *self.registers.bc.split_mut().low = new_value;
            }
            Instruction::LoadCFromA => *self.registers.bc.split_mut().low = self.registers.a,
            Instruction::LoadDFromHL => {
                let new_value = self.mmu.read(*self.registers.hl.get());
                *self.registers.de.split_mut().high = new_value;
            }
            Instruction::LoadDFromA => *self.registers.de.split_mut().high = self.registers.a,
            Instruction::LoadEFromL => {
                *self.registers.de.split_mut().low = *self.registers.hl.split().low
            }
            Instruction::LoadEFromA => *self.registers.de.split_mut().low = self.registers.a,
            Instruction::LoadLFromHL => {
                *self.registers.hl.split_mut().low = self.mmu.read(*self.registers.hl.get())
            }
            Instruction::LoadLFromA => *self.registers.hl.split_mut().low = self.registers.a,
            Instruction::StoreBAtHL => self
                .mmu
                .write(*self.registers.hl.get(), *self.registers.bc.split().high),
            Instruction::StoreCAtHL => self
                .mmu
                .write(*self.registers.hl.get(), *self.registers.bc.split().low),
            Instruction::StoreDAtHL => self
                .mmu
                .write(*self.registers.hl.get(), *self.registers.de.split().high),
            Instruction::StoreEAtHL => self
                .mmu
                .write(*self.registers.hl.get(), *self.registers.de.split().low),
            Instruction::StoreAAtHL => self.mmu.write(*self.registers.hl.get(), self.registers.a),
            Instruction::LoadAFromB => self.registers.a = *self.registers.bc.split().high,
            Instruction::LoadAFromC => self.registers.a = *self.registers.bc.split().low,
            Instruction::LoadAFromD => self.registers.a = *self.registers.de.split().high,
            Instruction::LoadAFromE => self.registers.a = *self.registers.de.split().low,
            Instruction::LoadAFromH => self.registers.a = *self.registers.hl.split().high,
            Instruction::LoadAFromL => self.registers.a = *self.registers.hl.split().low,
            Instruction::LoadAFromHL => self.registers.a = self.mmu.read(*self.registers.hl.get()),
            Instruction::XorCWithA => {
                self.registers.a ^= self.registers.bc.split().low;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::XorLWithA => {
                self.registers.a ^= self.registers.hl.split().low;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::XorHLWithA => {
                self.registers.a ^= self.mmu.read(*self.registers.hl.get());
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::XorAWithA => {
                self.registers.a ^= self.registers.a;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::OrBWithA => {
                self.registers.a |= self.registers.bc.split().high;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::OrCWithA => {
                self.registers.a |= self.registers.bc.split().low;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::OrHLWithA => {
                self.registers.a |= self.mmu.read(*self.registers.hl.get());
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::OrAWithA => {
                self.registers.a |= self.registers.a;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::CompareAWithE => {
                let (intermediate, overflow) = self
                    .registers
                    .a
                    .overflowing_sub(*self.registers.de.split().low);
                self.registers.set_zero(intermediate == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify these flags
                self.registers.set_half_carry(overflow);
                self.registers.set_carry(overflow);
            }
            Instruction::PopBC => {
                *self.registers.bc.get_mut() = self.mmu.read_u16(self.registers.sp);
                self.registers.sp += 2;
            }
            Instruction::JumpIfNotZero { address } => {
                if !self.registers.zero() {
                    self.registers.pc = address;
                }
            }
            Instruction::Jump { address } => self.registers.pc = address,
            Instruction::CallIfNotZero { address } => {
                if !self.registers.zero() {
                    self.registers.sp -= 2;
                    self.mmu.write_u16(self.registers.sp, self.registers.pc);
                    self.registers.pc = address;
                }
            }
            Instruction::LoadHFromL => {
                *self.registers.hl.split_mut().high = *self.registers.hl.split().low
            }
            Instruction::LoadHFromA => *self.registers.hl.split_mut().high = self.registers.a,
            Instruction::PushBC => {
                self.registers.sp -= 2;
                self.mmu
                    .write_u16(self.registers.sp, *self.registers.bc.get());
            }
            Instruction::AddAWith8Imm { value } => {
                let (res, carry) = self.registers.a.overflowing_add(value);
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                //TODO: Fix half carry flag
                self.registers.set_half_carry(carry);
                self.registers.set_carry(carry);
            }
            Instruction::ReturnIfZero => {
                if self.registers.zero() {
                    self.registers.pc = self.mmu.read_u16(self.registers.sp);
                    self.registers.sp += 2;
                }
            }
            Instruction::Return => {
                self.registers.pc = self.mmu.read_u16(self.registers.sp);
                self.registers.sp += 2;
            }
            Instruction::JumpIfZero { address } => {
                if self.registers.zero() {
                    self.registers.pc = address;
                }
            }
            Instruction::Complex(ci) => self.execute_complex(ci),
            Instruction::Call { address } => {
                //TODO: Verify
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = address;
            }
            Instruction::AddWithCarryAWith8Imm { value } => {
                let (res, carry1) = self.registers.a.overflowing_add(value);
                let (res, carry2) = res.overflowing_add(self.registers.carry() as u8);
                let carry = carry1 || carry2;
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(carry);
                self.registers.set_carry(carry);
            }
            Instruction::ReturnIfNotCarry => {
                if !self.registers.carry() {
                    self.registers.pc = self.mmu.read_u16(self.registers.sp);
                    self.registers.sp += 2;
                }
            }
            Instruction::PopDE => {
                *self.registers.de.get_mut() = self.mmu.read_u16(self.registers.sp);
                self.registers.sp += 2;
            }
            Instruction::JumpIfNotCarry { address } => {
                if !self.registers.carry() {
                    self.registers.pc = address;
                }
            }
            Instruction::PushDE => {
                self.registers.sp -= 2;
                self.mmu
                    .write_u16(self.registers.sp, *self.registers.de.get());
            }
            Instruction::SubtractAWith8Imm { value } => {
                let (res, carry) = self.registers.a.overflowing_sub(value);
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                self.registers.set_half_carry(carry);
                self.registers.set_carry(carry);
            }
            Instruction::JumpIfCarry { address } => {
                if self.registers.carry() {
                    self.registers.pc = address;
                }
            }
            Instruction::OutputAToPort { address } => {
                let address = 0xFF00 + u16::from(address);
                self.mmu.write(address, self.registers.a)
            }
            Instruction::PopHL => {
                *self.registers.hl.get_mut() = self.mmu.read_u16(self.registers.sp);
                self.registers.sp += 2;
            }
            Instruction::PushHL => {
                self.registers.sp -= 2;
                self.mmu
                    .write_u16(self.registers.sp, *self.registers.hl.get());
            }
            Instruction::AndAWith8Imm { value } => {
                self.registers.a &= value;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(true);
                self.registers.set_carry(false);
            }
            Instruction::JumpHL => self.registers.pc = *self.registers.hl.get(),
            Instruction::StoreAAt16Imm { address } => self.mmu.write(address, self.registers.a),
            Instruction::Xor8ImmWithA { value } => {
                self.registers.a ^= value;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::LoadAFromPort { address } => {
                let address = 0xFF00 + u16::from(address);
                self.registers.a = self.mmu.read(address);
            }
            Instruction::LoadAFrom16Imm { address } => {
                let new_value = self.mmu.read(address);
                self.registers.a = new_value;
            }
            Instruction::PopAF => {
                let mut new_value = self.mmu.read_u16(self.registers.sp);
                if new_value & 0x000F != 0 {
                    warn!("Attempted to pop invalid value into F. Masking lower bits");
                    new_value &= 0xFFF0;
                }
                self.registers.set_af(new_value);
                debug_assert_eq!(self.registers.af(), new_value);
                self.registers.sp += 2;
            }
            Instruction::DisableInterrupts => self.disable_ime(),
            Instruction::PushAF => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.af());
            }
            Instruction::LoadSPFromHL => self.registers.sp = *self.registers.hl.get(),
            Instruction::CompareAWith8Imm { value } => {
                let (intermediate, overflow) = self.registers.a.overflowing_sub(value);
                self.registers.set_zero(intermediate == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify these flags
                self.registers.set_half_carry(overflow);
                self.registers.set_carry(overflow);
            }
        }
    }

    pub fn execute_complex(&mut self, instruction: ComplexInstruction) {
        match instruction {
            ComplexInstruction::RotateRightWithCarryC => {
                let bit7 = (self.registers.carry() as u8) << 7;
                let c = self.registers.bc.split_mut().low;
                let bit0 = *c & 0b1 != 0;
                *c >>= 1;
                *c |= bit7;
                let c = self.registers.bc.split().low;
                self.registers.set_zero(*c == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(bit0);
            }
            ComplexInstruction::RotateRightWithCarryD => {
                let bit7 = (self.registers.carry() as u8) << 7;
                let d = self.registers.de.split_mut().high;
                let bit0 = *d & 0b1 != 0;
                *d >>= 1;
                *d |= bit7;
                let d = self.registers.de.split().high;
                self.registers.set_zero(*d == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(bit0);
            }
            ComplexInstruction::RotateRightWithCarryE => {
                let bit7 = (self.registers.carry() as u8) << 7;
                let e = self.registers.de.split_mut().low;
                let bit0 = *e & 0b1 != 0;
                *e >>= 1;
                *e |= bit7;
                let e = self.registers.de.split().low;
                self.registers.set_zero(*e == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(bit0);
            }
            ComplexInstruction::ShiftRightLogicalB => {
                let b = self.registers.bc.split_mut().high;
                //This seems weird
                let bit0 = *b & 0b1 != 0;
                *b >>= 1;
                let b = self.registers.bc.split().high;
                self.registers.set_zero(*b == 0);
                self.registers.set_subtraction(false);
                //This is right
                self.registers.set_half_carry(false);
                self.registers.set_carry(bit0);
            }
        }
    }

    //TODO: Break reason
    pub fn step(&mut self) -> ControlFlow<(), ()> {
        self.dots += 4;
        if self.dots == 456 {
            self.dots = 0;
            self.mmu.io_registers_mut().lcd_mut().inc_ly();
            if self.mmu.io_registers().lcd().ly() >= 144 {
                self.mmu.unlock_vram();
            } else {
                self.mmu.lock_vram();
            }
        }
        let instr = self.read_instruction();
        trace!("{} | {instr}", self.registers);
        self.execute(instr);
        //TODO: Accurate clocks for timers
        //TODO: Delay
        if let ControlFlow::Break(output) = self
            .mmu
            .io_registers_mut()
            .serial_transfer_mut()
            .step_clock()
        {
            //TODO: Verify this is correct
            self.serial_output.push(output as char);
            info!("Serial Output: {}", self.serial_output);
            self.mmu.request_interrupt(3);
        }
        self.mmu.update_timers(1);
        //TODO: Generate and run interrupts
        if self.mmu.io_registers().lcd().ly() == 144 {
            self.mmu.request_interrupt(0);
        }

        let interrupts = self.mmu.interrupt_enable() & self.mmu.interrupts();
        if self.ime.is_enabled() && interrupts.any() {
            for interrupt in interrupts.iter_ones() {
                debug_assert!(interrupt <= 4);
                let interrupt_address = 0x40 + u16::try_from(interrupt).unwrap() * 0x8;
                debug!("Servicing interrupt {interrupt:#04X}");
                self.mmu.acknowledge_interrupt(interrupt);
                self.disable_ime();
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = interrupt_address;
            }
        }
        self.update_ime();
        match instr {
            _ => ControlFlow::Continue(()),
        }
    }

    pub fn disable_ime(&mut self) {
        self.ime = InterruptMasterEnable::Disabled;
    }

    pub fn enable_ime(&mut self) {
        self.ime = match self.ime {
            InterruptMasterEnable::Disabled => InterruptMasterEnable::Enabling,
            InterruptMasterEnable::Enabling => {
                error!("Warning: IME wasn't updated properly");
                InterruptMasterEnable::Enabled
            }
            InterruptMasterEnable::Enabled => InterruptMasterEnable::Enabled,
        }
    }

    pub fn update_ime(&mut self) {
        self.ime = match self.ime {
            InterruptMasterEnable::Enabling => InterruptMasterEnable::Enabled,
            v => v,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum InterruptMasterEnable {
    Disabled,
    Enabling,
    Enabled,
}

impl InterruptMasterEnable {
    pub fn is_enabled(&self) -> bool {
        *self == InterruptMasterEnable::Enabled
    }
}
