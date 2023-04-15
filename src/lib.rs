use std::ops::ControlFlow;

use instructions::Instruction;
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
}

impl Core<'_> {
    pub fn new<'rom>(rom: &'rom [u8], model: Model) -> Option<Core<'rom>> {
        let mmu = MemoryManagementUnit::new(rom).ok()?;
        let registers = Registers::init(model);
        Some(Core {
            mmu,
            registers,
            ime: InterruptMasterEnable::Enabled,
        })
    }

    fn read_instruction_at(&self, address: u16) -> Instruction {
        let byte = self.mmu.read(address);
        match byte {
            0x00 => Instruction::Noop,
            0x01 => {
                let new_value = self.mmu.read_u16(address + 1);
                Instruction::LoadBCFrom16Imm { new_value }
            }
            0x03 => Instruction::IncrementBC,
            0x0D => Instruction::DecrementC,
            0x0E => {
                let new_value = self.mmu.read(address + 1);
                Instruction::LoadCFrom8Imm { new_value }
            }
            0x11 => {
                let new_value = self.mmu.read_u16(address + 1);
                Instruction::LoadDEFrom16Imm { new_value }
            }
            0x12 => Instruction::StoreAAtDE,
            0x14 => Instruction::IncrementD,
            0x18 => {
                let offset = self.mmu.read(address + 1) as i8;
                Instruction::JumpRelative { offset }
            }
            0x1C => Instruction::IncrementE,
            0x20 => {
                let offset = self.mmu.read(address + 1) as i8;
                Instruction::JumpRelativeIfNotZero { offset }
            }
            0x21 => {
                let new_value = self.mmu.read_u16(address + 1);
                Instruction::LoadHLFrom16Imm { new_value }
            }
            0x23 => Instruction::IncrementHL,
            0x2A => Instruction::LoadAFromHLAndInc,
            0x31 => {
                let new_value = self.mmu.read_u16(address + 1);
                Instruction::LoadSPFrom16Imm { new_value }
            }
            0x3E => {
                let new_value = self.mmu.read(address + 1);
                Instruction::LoadAFrom8Imm { new_value }
            }
            0x47 => Instruction::LoadBFromA,
            0x78 => Instruction::LoadAFromB,
            0x7C => Instruction::LoadAFromH,
            0x7D => Instruction::LoadAFromL,
            0xB1 => Instruction::OrCWithA,
            0xC3 => {
                let address = self.mmu.read_u16(address + 1);
                Instruction::Jump { address }
            }
            0xC5 => Instruction::LoadHFromL,
            0xCD => {
                let address = self.mmu.read_u16(address + 1);
                Instruction::Call { address }
            }
            0xC9 => Instruction::Return,
            0xE0 => {
                let address = self.mmu.read(address + 1);
                Instruction::OutputAToPort { address }
            }
            0xE1 => Instruction::PopHL,
            0xE5 => Instruction::PushHL,
            0xEA => {
                let address = self.mmu.read_u16(address + 1);
                Instruction::StoreAAt16Imm { address }
            }
            0xF1 => Instruction::PopAF,
            0xF3 => Instruction::DisableInterrupts,
            0xF5 => Instruction::PushAF,
            value => todo!(
                "Unknown instruction {value:#04X} @ {:#06X}",
                self.registers.pc
            ),
        }
    }
    fn read_instruction(&mut self) -> Instruction {
        self.read_instruction_at(self.registers.pc)
    }

    fn execute(&mut self, instruction: Instruction) {
        self.registers.pc += instruction.size();
        match instruction {
            Instruction::Noop => {}
            Instruction::LoadBCFrom16Imm { new_value } => *self.registers.bc.get_mut() = new_value,
            Instruction::IncrementBC => {
                let bc = self.registers.bc.get_mut();
                *bc = bc.wrapping_add(1);
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
            Instruction::IncrementE => {
                let e = self.registers.de.split_mut().low;
                *e = e.wrapping_add(1);
                self.registers.set_zero(*self.registers.de.split().low == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.de.split().low & 0x0F == 0);
            }
            Instruction::JumpRelativeIfNotZero { offset } => {
                if !self.registers.zero() {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
                }
            }
            Instruction::LoadHLFrom16Imm { new_value } => *self.registers.hl.get_mut() = new_value,
            Instruction::IncrementHL => {
                let hl = self.registers.hl.get_mut();
                *hl = hl.wrapping_add(1);
            }
            Instruction::LoadAFromHLAndInc => {
                let byte = self.mmu.read(*self.registers.hl.get());
                self.registers.a = byte;
                *self.registers.hl.get_mut() += 1;
            }
            Instruction::LoadSPFrom16Imm { new_value } => self.registers.sp = new_value,
            Instruction::LoadAFrom8Imm { new_value } => self.registers.a = new_value,
            Instruction::LoadBFromA => *self.registers.bc.split_mut().high = self.registers.a,
            Instruction::LoadAFromB => self.registers.a = *self.registers.bc.split().high,
            Instruction::LoadAFromH => self.registers.a = *self.registers.hl.split().high,
            Instruction::LoadAFromL => self.registers.a = *self.registers.hl.split().low,
            Instruction::OrCWithA => {
                self.registers.a = self.registers.a & self.registers.bc.split().low;
            }
            Instruction::Jump { address } => self.registers.pc = address,
            Instruction::LoadHFromL => {
                *self.registers.hl.split_mut().high = *self.registers.hl.split().low
            }
            Instruction::Return => {
                self.registers.pc = self.mmu.read_u16(self.registers.sp);
                self.registers.sp += 2;
            }
            Instruction::Call { address } => {
                //TODO: Verify
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.pc);
                self.registers.pc = address;
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
            Instruction::StoreAAt16Imm { address } => self.mmu.write(address, self.registers.a),
            Instruction::PopAF => {
                let new_value = self.mmu.read_u16(self.registers.sp);
                self.registers.set_af(new_value);
                debug_assert_eq!(self.registers.af(), new_value);
                self.registers.sp += 2;
            }
            Instruction::DisableInterrupts => self.disable_ime(),
            Instruction::PushAF => {
                self.registers.sp -= 2;
                self.mmu.write_u16(self.registers.sp, self.registers.af());
            }
        }
    }

    //TODO: Break reason
    pub fn step(&mut self) -> ControlFlow<(), ()> {
        let instr = self.read_instruction();
        println!("{} | {instr}", self.registers);
        self.execute(instr);
        //TODO: Accurate clocks for timers
        //TODO: Delay
        self.mmu.update_timers(1);
        //TODO: Generate and run interrupts
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
                eprintln!("Warning: IME wasn't updated properly");
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
