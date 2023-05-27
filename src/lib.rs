use std::ops::ControlFlow;

use instructions::{ComplexInstruction, Instruction, InstructionBuilder, InstructionParseError};
use log::{debug, error, info, trace, warn};
use memory_management_unit::MemoryManagementUnit;
use registers::Registers;
use registers::{is_half_carry, is_half_carry_sub, is_half_carry_u16};

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
                let half_carry = is_half_carry(*b, 1);
                *b = b.wrapping_add(1);
                self.registers
                    .set_zero(*self.registers.bc.split().high == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementB => {
                let c = self.registers.bc.split_mut().high;
                let half_carry = is_half_carry(*c, (-1i8) as u8);
                *c = c.wrapping_sub(1);
                self.registers
                    .set_zero(*self.registers.bc.split().high == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadBFrom8Imm { new_value } => {
                *self.registers.bc.split_mut().high = new_value
            }
            Instruction::StoreSPAt16Imm { address } => {
                self.mmu.write_u16(address, self.registers.sp)
            }
            Instruction::AddHLWithBC => {
                let hl = self.registers.hl.get_mut();
                let bc = *self.registers.bc.get();
                let (res, carry) = hl.overflowing_add(bc);
                let half_carry = is_half_carry_u16(*hl, bc);
                *hl = res;
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::DecrementBC => *self.registers.bc.get_mut() -= 1,
            Instruction::IncrementC => {
                let c = self.registers.bc.split_mut().low;
                let half_carry = is_half_carry(*c, 1);
                *c = c.wrapping_add(1);
                self.registers.set_zero(*self.registers.bc.split().low == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementC => {
                let c = self.registers.bc.split_mut().low;
                let half_carry = is_half_carry(*c, (-1i8) as u8);
                *c = c.wrapping_sub(1);
                self.registers.set_zero(*self.registers.bc.split().low == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
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
                let half_carry = is_half_carry(*d, 1);
                *d = d.wrapping_add(1);
                self.registers
                    .set_zero(*self.registers.de.split().high == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadDFrom8Imm { new_value } => {
                *self.registers.de.split_mut().high = new_value
            }
            Instruction::JumpRelative { offset } => {
                self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
            }
            Instruction::AddHLWithDE => {
                let hl = self.registers.hl.get_mut();
                let de = *self.registers.de.get();
                let (res, carry) = hl.overflowing_add(de);
                let half_carry = is_half_carry_u16(*hl, de);
                *hl = res;
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::LoadAFromDE => {
                let new_value = self.mmu.read(*self.registers.de.get());
                self.registers.a = new_value;
            }
            Instruction::DecrementDE => *self.registers.de.get_mut() -= 1,
            Instruction::IncrementE => {
                let e = self.registers.de.split_mut().low;
                let half_carry = is_half_carry(*e, 1);
                *e = e.wrapping_add(1);
                self.registers.set_zero(*self.registers.de.split().low == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementE => {
                let e = self.registers.de.split_mut().low;
                let half_carry = is_half_carry(*e, (-1i8) as u8);
                *e = e.wrapping_sub(1);
                self.registers.set_zero(*self.registers.de.split().low == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadEFrom8Imm { new_value } => {
                *self.registers.de.split_mut().low = new_value;
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
                let h = self.registers.hl.split_mut().high;
                let half_carry = is_half_carry(*h, 1);
                *h = h.wrapping_add(1);
                self.registers
                    .set_zero(*self.registers.hl.split().high == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementH => {
                let h = self.registers.hl.split_mut().high;
                let half_carry = is_half_carry(*h, (-1i8) as u8);
                *h = h.wrapping_sub(1);
                self.registers
                    .set_zero(*self.registers.hl.split().high == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadHFrom8Imm { new_value } => {
                *self.registers.hl.split_mut().high = new_value;
            }
            Instruction::DecimalAdjustA => {
                if !self.registers.subtraction() {
                    if self.registers.carry() || self.registers.a > 0x99 {
                        self.registers.a = self.registers.a.wrapping_add(0x60);
                        self.registers.set_carry(true);
                    }
                    if self.registers.half_carry() || self.registers.a & 0x0F > 0x09 {
                        self.registers.a = self.registers.a.wrapping_add(0x06);
                    }
                } else {
                    if self.registers.carry() {
                        self.registers.a = self.registers.a.wrapping_sub(0x60);
                    }
                    if self.registers.half_carry() {
                        self.registers.a = self.registers.a.wrapping_sub(0x06);
                    }
                }
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_half_carry(false);
            }
            Instruction::JumpRelativeIfZero { offset } => {
                if self.registers.zero() {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
                }
            }
            Instruction::AddHLWithHL => {
                let hl = *self.registers.hl.get();
                //TODO: 16-bit half-carry
                let half_carry = is_half_carry_u16(hl, hl);
                let (res, carry) = hl.overflowing_add(hl);
                *self.registers.hl.get_mut() = res;
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::LoadAFromHLAndInc => {
                let byte = self.mmu.read(*self.registers.hl.get());
                self.registers.a = byte;
                *self.registers.hl.get_mut() += 1;
            }
            Instruction::DecrementHL => *self.registers.hl.get_mut() -= 1,
            Instruction::IncrementL => {
                let l = self.registers.hl.split_mut().low;
                let half_carry = is_half_carry(*l, 1);
                *l = l.wrapping_add(1);
                self.registers.set_zero(*self.registers.hl.split().low == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementL => {
                let l = self.registers.hl.split_mut().low;
                let half_carry = is_half_carry(*l, (-1i8) as u8);
                *l = l.wrapping_sub(1);
                self.registers.set_zero(*self.registers.hl.split().low == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadLFrom8Imm { new_value } => {
                *self.registers.hl.split_mut().low = new_value
            }
            Instruction::ComplimentA => {
                self.registers.set_subtraction(true);
                self.registers.set_half_carry(true);
                self.registers.a = !self.registers.a;
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
            Instruction::IncrementSP => {
                self.registers.sp = self.registers.sp.wrapping_add(1);
            }
            Instruction::IncrementAtHL => {
                let hl = *self.registers.hl.get();
                let mem = self.mmu.read(hl);
                let half_carry = is_half_carry(mem, 1);
                let res = mem.wrapping_add(1);
                self.mmu.write(hl, res);
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementAtHL => {
                let hl = *self.registers.hl.get();
                let mem = self.mmu.read(hl);
                let half_carry = is_half_carry(mem, (-1i8) as u8);
                let res = mem.wrapping_sub(1);
                self.mmu.write(hl, res);
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::Store8ImmAtHL { value } => self.mmu.write(*self.registers.hl.get(), value),
            Instruction::JumpRelativeIfCarry { offset } => {
                if self.registers.carry() {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
                }
            }
            Instruction::AddSPToHL => {
                let hl = self.registers.hl.get_mut();
                let half_carry = is_half_carry_u16(*hl, self.registers.sp);
                let (res, overflow) = hl.overflowing_add(self.registers.sp);
                *hl = res;
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(overflow);
            }
            Instruction::DecrementSP => self.registers.sp = self.registers.sp.wrapping_sub(1),
            Instruction::IncrementA => {
                let half_carry = is_half_carry(self.registers.a, 1);
                self.registers.a = self.registers.a.wrapping_add(1);
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::DecrementA => {
                let half_carry = is_half_carry(self.registers.a, (-1i8) as u8);
                self.registers.a = self.registers.a.wrapping_sub(1);
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify half carry register
                self.registers.set_half_carry(half_carry);
            }
            Instruction::LoadAFrom8Imm { new_value } => self.registers.a = new_value,
            Instruction::LoadBFromB => {
                *self.registers.bc.split_mut().high = *self.registers.bc.split().high
            }
            Instruction::LoadBFromC => {
                *self.registers.bc.split_mut().high = *self.registers.bc.split().low
            }
            Instruction::LoadBFromD => {
                *self.registers.bc.split_mut().high = *self.registers.de.split().high
            }
            Instruction::LoadBFromE => {
                *self.registers.bc.split_mut().high = *self.registers.de.split().low
            }
            Instruction::LoadBFromH => {
                *self.registers.bc.split_mut().high = *self.registers.hl.split().high
            }
            Instruction::LoadBFromL => {
                *self.registers.bc.split_mut().high = *self.registers.hl.split().low
            }
            Instruction::LoadBFromHL => {
                let new_value = self.mmu.read(*self.registers.hl.get());
                *self.registers.bc.split_mut().high = new_value;
            }
            Instruction::LoadBFromA => *self.registers.bc.split_mut().high = self.registers.a,
            Instruction::LoadCFromB => {
                *self.registers.bc.split_mut().low = *self.registers.bc.split().high
            }
            Instruction::LoadCFromC => {
                *self.registers.bc.split_mut().low = *self.registers.bc.split().low
            }
            Instruction::LoadCFromD => {
                *self.registers.bc.split_mut().low = *self.registers.de.split().high
            }
            Instruction::LoadCFromE => {
                *self.registers.bc.split_mut().low = *self.registers.de.split().low
            }
            Instruction::LoadCFromH => {
                *self.registers.bc.split_mut().low = *self.registers.hl.split().high
            }
            Instruction::LoadCFromL => {
                *self.registers.bc.split_mut().low = *self.registers.hl.split().low
            }
            Instruction::LoadCFromHL => {
                let new_value = self.mmu.read(*self.registers.hl.get());
                *self.registers.bc.split_mut().low = new_value;
            }
            Instruction::LoadCFromA => *self.registers.bc.split_mut().low = self.registers.a,
            Instruction::LoadDFromB => {
                *self.registers.de.split_mut().high = *self.registers.bc.split().high
            }
            Instruction::LoadDFromC => {
                *self.registers.de.split_mut().high = *self.registers.bc.split().low
            }
            Instruction::LoadDFromD => {
                *self.registers.de.split_mut().high = *self.registers.de.split().high
            }
            Instruction::LoadDFromE => {
                *self.registers.de.split_mut().high = *self.registers.de.split().low
            }
            Instruction::LoadDFromH => {
                *self.registers.de.split_mut().high = *self.registers.hl.split().high
            }
            Instruction::LoadDFromL => {
                *self.registers.de.split_mut().high = *self.registers.hl.split().low
            }
            Instruction::LoadDFromHL => {
                let new_value = self.mmu.read(*self.registers.hl.get());
                *self.registers.de.split_mut().high = new_value;
            }
            Instruction::LoadDFromA => *self.registers.de.split_mut().high = self.registers.a,
            Instruction::LoadEFromB => {
                *self.registers.de.split_mut().low = *self.registers.bc.split().high
            }
            Instruction::LoadEFromC => {
                *self.registers.de.split_mut().low = *self.registers.bc.split().low
            }
            Instruction::LoadEFromD => {
                *self.registers.de.split_mut().low = *self.registers.de.split().high
            }
            Instruction::LoadEFromE => {
                *self.registers.de.split_mut().low = *self.registers.de.split().low
            }
            Instruction::LoadEFromH => {
                *self.registers.de.split_mut().low = *self.registers.hl.split().high
            }
            Instruction::LoadEFromL => {
                *self.registers.de.split_mut().low = *self.registers.hl.split().low
            }
            Instruction::LoadEFromHL => {
                *self.registers.de.split_mut().low = self.mmu.read(*self.registers.hl.get())
            }
            Instruction::LoadEFromA => *self.registers.de.split_mut().low = self.registers.a,
            Instruction::LoadHFromB => {
                *self.registers.hl.split_mut().high = *self.registers.bc.split().high
            }
            Instruction::LoadHFromC => {
                *self.registers.hl.split_mut().high = *self.registers.bc.split().low
            }
            Instruction::LoadHFromD => {
                *self.registers.hl.split_mut().high = *self.registers.de.split().high
            }
            Instruction::LoadHFromE => {
                *self.registers.hl.split_mut().high = *self.registers.de.split().low
            }
            Instruction::LoadHFromH => {
                *self.registers.hl.split_mut().high = *self.registers.hl.split().high
            }
            Instruction::LoadHFromL => {
                *self.registers.hl.split_mut().high = *self.registers.hl.split().low
            }
            Instruction::LoadHFromHL => {
                *self.registers.hl.split_mut().high = self.mmu.read(*self.registers.hl.get())
            }
            Instruction::LoadHFromA => *self.registers.hl.split_mut().high = self.registers.a,
            Instruction::LoadLFromB => {
                *self.registers.hl.split_mut().low = *self.registers.bc.split().high
            }
            Instruction::LoadLFromC => {
                *self.registers.hl.split_mut().low = *self.registers.bc.split().low
            }
            Instruction::LoadLFromD => {
                *self.registers.hl.split_mut().low = *self.registers.de.split().high
            }
            Instruction::LoadLFromE => {
                *self.registers.hl.split_mut().low = *self.registers.de.split().low
            }
            Instruction::LoadLFromH => {
                *self.registers.hl.split_mut().low = *self.registers.hl.split().high
            }
            Instruction::LoadLFromL => {
                *self.registers.hl.split_mut().low = *self.registers.hl.split().low
            }
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
            Instruction::StoreHAtHL => self
                .mmu
                .write(*self.registers.hl.get(), *self.registers.hl.split().high),
            Instruction::StoreLAtHL => self
                .mmu
                .write(*self.registers.hl.get(), *self.registers.hl.split().low),
            Instruction::StoreAAtHL => self.mmu.write(*self.registers.hl.get(), self.registers.a),
            Instruction::LoadAFromB => self.registers.a = *self.registers.bc.split().high,
            Instruction::LoadAFromC => self.registers.a = *self.registers.bc.split().low,
            Instruction::LoadAFromD => self.registers.a = *self.registers.de.split().high,
            Instruction::LoadAFromE => self.registers.a = *self.registers.de.split().low,
            Instruction::LoadAFromH => self.registers.a = *self.registers.hl.split().high,
            Instruction::LoadAFromL => self.registers.a = *self.registers.hl.split().low,
            Instruction::LoadAFromHL => self.registers.a = self.mmu.read(*self.registers.hl.get()),
            Instruction::LoadAFromA => self.registers.a = self.registers.a,
            Instruction::AddAWithB => {
                let a = &mut self.registers.a;
                let b = self.registers.bc.split_mut().high;
                let half_carry = is_half_carry(*a, *b);
                let (res, carry) = a.overflowing_add(*b);
                *a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::AddWithCarryAWithH => {
                let h = *self.registers.hl.split().high;
                let half_carry1 = is_half_carry(self.registers.a, h);
                let (res, carry1) = self.registers.a.overflowing_add(h);
                let half_carry2 = is_half_carry(res, self.registers.carry() as u8);
                let (res, carry2) = res.overflowing_add(self.registers.carry() as u8);
                let half_carry = half_carry1 || half_carry2;
                let carry = carry1 || carry2;
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::XorCWithA => {
                self.registers.a ^= self.registers.bc.split().low;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::XorEWithA => {
                self.registers.a ^= self.registers.de.split().low;
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
            Instruction::CompareAWithB => {
                let (intermediate, overflow) = self
                    .registers
                    .a
                    .overflowing_sub(*self.registers.bc.split().high);
                self.registers.set_zero(intermediate == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify these flags
                // self.registers.set_half_carry(overflow);
                self.registers.set_carry(overflow);
            }
            Instruction::CompareAWithC => {
                let (intermediate, overflow) = self
                    .registers
                    .a
                    .overflowing_sub(*self.registers.bc.split().low);
                self.registers.set_zero(intermediate == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify these flags
                // self.registers.set_half_carry(overflow);
                self.registers.set_carry(overflow);
            }
            Instruction::CompareAWithD => {
                let (intermediate, overflow) = self
                    .registers
                    .a
                    .overflowing_sub(*self.registers.de.split().high);
                self.registers.set_zero(intermediate == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify these flags
                // self.registers.set_half_carry(overflow);
                self.registers.set_carry(overflow);
            }
            Instruction::CompareAWithE => {
                let (intermediate, overflow) = self
                    .registers
                    .a
                    .overflowing_sub(*self.registers.de.split().low);
                self.registers.set_zero(intermediate == 0);
                self.registers.set_subtraction(true);
                //TODO: Verify these flags
                // self.registers.set_half_carry(overflow);
                self.registers.set_carry(overflow);
            }
            Instruction::ReturnIfNotZero => {
                if !self.registers.zero() {
                    self.registers.pc = self.mmu.read_u16(self.registers.sp);
                    self.registers.sp += 2;
                }
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
            Instruction::PushBC => {
                self.registers.sp -= 2;
                self.mmu
                    .write_u16(self.registers.sp, *self.registers.bc.get());
            }
            Instruction::AddAWith8Imm { value } => {
                let half_carry = is_half_carry(self.registers.a, value);
                let (res, carry) = self.registers.a.overflowing_add(value);
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                //TODO: Fix half carry flag
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::Reset0 => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = 0x0000;
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
            Instruction::CallIfZero { address } => {
                //TODO: Verify
                if self.registers.zero() {
                    self.registers.sp -= 2;
                    self.mmu.write_u16(self.registers.sp, self.registers.pc);
                    self.registers.pc = address;
                }
            }
            Instruction::Call { address } => {
                //TODO: Verify
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = address;
            }
            Instruction::AddWithCarryAWith8Imm { value } => {
                let half_carry1 = is_half_carry(self.registers.a, value);
                let (res, carry1) = self.registers.a.overflowing_add(value);
                let half_carry2 = is_half_carry(res, self.registers.carry() as u8);
                let (res, carry2) = res.overflowing_add(self.registers.carry() as u8);
                let half_carry = half_carry1 || half_carry2;
                let carry = carry1 || carry2;
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::Reset1 => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = 0x0008;
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
            Instruction::CallIfNotCarry { address } => {
                if !self.registers.carry() {
                    self.registers.sp -= 2;
                    self.mmu.write_u16(self.registers.sp, self.registers.pc);
                    self.registers.pc = address;
                }
            }
            Instruction::PushDE => {
                self.registers.sp -= 2;
                self.mmu
                    .write_u16(self.registers.sp, *self.registers.de.get());
            }
            Instruction::SubtractAWith8Imm { value } => {
                let half_carry = is_half_carry_sub(self.registers.a, value);
                let (res, carry) = self.registers.a.overflowing_sub(value);
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::Reset2 => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = 0x0010;
            }
            Instruction::ReturnIfCarry => {
                if self.registers.carry() {
                    self.registers.pc = self.mmu.read_u16(self.registers.sp);
                    self.registers.sp += 2;
                }
            }
            Instruction::ReturnInterrupt => {
                self.registers.pc = self.mmu.read_u16(self.registers.sp);
                self.registers.sp += 2;
                self.enable_ime();
            }
            Instruction::JumpIfCarry { address } => {
                if self.registers.carry() {
                    self.registers.pc = address;
                }
            }
            Instruction::CallIfCarry { address } => {
                if self.registers.carry() {
                    self.registers.sp -= 2;
                    self.mmu.write_u16(self.registers.sp, self.registers.pc);
                    self.registers.pc = address;
                }
            }
            Instruction::SubtractWithCarryAWith8Imm { value } => {
                let half_carry1 = is_half_carry_sub(self.registers.a, value);
                let (res, carry1) = self.registers.a.overflowing_sub(value);
                let half_carry2 = is_half_carry_sub(res, self.registers.carry() as u8);
                let (res, carry2) = res.overflowing_sub(self.registers.carry() as u8);
                let half_carry = half_carry1 || half_carry2;
                let carry = carry1 || carry2;
                self.registers.a = res;
                self.registers.set_zero(res == 0);
                self.registers.set_subtraction(true);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(carry);
            }
            Instruction::Reset3 => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = 0x0018;
            }
            Instruction::OutputAToPort8Imm { address } => {
                let address = 0xFF00 + u16::from(address);
                self.mmu.write(address, self.registers.a)
            }
            Instruction::PopHL => {
                *self.registers.hl.get_mut() = self.mmu.read_u16(self.registers.sp);
                self.registers.sp += 2;
            }
            Instruction::OutputAToPortC => {
                let c = *self.registers.bc.split().low;
                let address = 0xFF00 + u16::from(c);
                self.mmu.write(address, self.registers.a)
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
            Instruction::Reset4 => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = 0x0020;
            }
            Instruction::AddSPWithS8Imm { value } => {
                let [sp_low, mut sp_high] = self.registers.sp.to_le_bytes();
                // This instruction works as a signed 8 bit addition
                // But flags are weird, so it's easier to do it as an unsigned addition
                let unsigned_value = value as u8;
                let half_carry = is_half_carry(sp_low, unsigned_value);
                let (sp_low, overflow) = sp_low.overflowing_add(unsigned_value);
                // Adjust back to 16-bit
                if overflow && value.is_positive() {
                    sp_high = sp_high.wrapping_add(1);
                } else if !overflow && !value.is_positive() {
                    sp_high = sp_high.wrapping_sub(1);
                }

                self.registers.sp = u16::from_le_bytes([sp_low, sp_high]);
                self.registers.set_zero(false);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(overflow);
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
            Instruction::Reset5 => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = 0x0028;
            }
            Instruction::LoadAFromPort8Imm { address } => {
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
                    // warn!("Attempted to pop invalid value into F. Masking lower bits");
                    new_value &= 0xFFF0;
                }
                self.registers.set_af(new_value);
                debug_assert_eq!(self.registers.af(), new_value);
                self.registers.sp += 2;
            }
            Instruction::LoadAFromPortC => {
                let c = *self.registers.bc.split().low;
                let address = 0xFF00 + u16::from(c);
                self.registers.a = self.mmu.read(address);
            }
            Instruction::DisableInterrupts => self.disable_ime(),
            Instruction::PushAF => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.af());
            }
            Instruction::OrAWith8Imm { value } => {
                self.registers.a |= value;
                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            Instruction::Reset6 => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = 0x0030;
            }
            Instruction::LoadHLFromSPWithS8Imm { value } => {
                let [sp_low, mut sp_high] = self.registers.sp.to_le_bytes();
                // This instruction works as a signed 8 bit addition
                // But flags are weird, so it's easier to do it as an unsigned addition
                let unsigned_value = value as u8;
                let half_carry = is_half_carry(sp_low, unsigned_value);
                let (sp_low, overflow) = sp_low.overflowing_add(unsigned_value);
                // Adjust back to 16-bit
                if overflow && value.is_positive() {
                    sp_high = sp_high.wrapping_add(1);
                } else if !overflow && !value.is_positive() {
                    sp_high = sp_high.wrapping_sub(1);
                }

                *self.registers.hl.get_mut() = u16::from_le_bytes([sp_low, sp_high]);
                self.registers.set_zero(false);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(overflow);
            }
            Instruction::LoadSPFromHL => self.registers.sp = *self.registers.hl.get(),
            Instruction::EnableInterrupts => self.enable_ime(),
            Instruction::CompareAWith8Imm { value } => {
                let (intermediate, overflow) = self.registers.a.overflowing_sub(value);
                self.registers.set_zero(intermediate == 0);
                self.registers.set_subtraction(true);
                let half_carry = is_half_carry_sub(self.registers.a, value);
                //TODO: Verify these flags
                self.registers.set_half_carry(half_carry);
                self.registers.set_carry(overflow);
            }
            Instruction::Reset7 => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = 0x0038;
            }
        }
    }

    pub fn execute_complex(&mut self, instruction: ComplexInstruction) {
        match instruction {
            ComplexInstruction::RotateRightWithCarryB => {
                let bit7 = (self.registers.carry() as u8) << 7;
                let b = self.registers.bc.split_mut().high;
                let bit0 = *b & 0b1 != 0;
                *b >>= 1;
                *b |= bit7;
                let b = self.registers.bc.split().high;
                self.registers.set_zero(*b == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(bit0);
            }
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
            ComplexInstruction::ShiftLeftLogicalAtHL => {
                let value = self.mmu.read(*self.registers.hl.get());
                let carry = value & 0b10000000 != 0;
                self.mmu.write(*self.registers.hl.get(), value << 1);
                self.registers.set_zero(value << 1 == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(carry);
            }
            ComplexInstruction::SwapB => {
                let b = self.registers.bc.split_mut().high;
                *b = b.rotate_right(4);

                let b = self.registers.bc.split().high;
                self.registers.set_zero(*b == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
            }
            ComplexInstruction::SwapA => {
                self.registers.a = self.registers.a.rotate_right(4);

                self.registers.set_zero(self.registers.a == 0);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(false);
                self.registers.set_carry(false);
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
            ComplexInstruction::ShiftRightLogicalA => {
                let a = &mut self.registers.a;
                //This seems weird
                let bit0 = *a & 0b1 != 0;
                *a >>= 1;
                let a = &self.registers.a;
                self.registers.set_zero(*a == 0);
                self.registers.set_subtraction(false);
                //This is right
                self.registers.set_half_carry(false);
                self.registers.set_carry(bit0);
            }
            ComplexInstruction::Bit5AtHL => {
                let value = self.mmu.read(*self.registers.hl.get());
                let bit5 = 0b00100000;
                let bit5 = value & bit5 != 0;
                self.registers.set_zero(!bit5);
                self.registers.set_subtraction(false);
                self.registers.set_half_carry(true);
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
        let is_loop = match instr {
            Instruction::JumpRelative { offset: -2 } => true,
            _ => false,
        };
        if (!self.ime.is_enabled() || !self.mmu.interrupt_enable().any()) && is_loop {
            return ControlFlow::Break(());
        }
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
