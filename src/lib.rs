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
}

impl Core<'_> {
    pub fn new<'rom>(rom: &'rom [u8], model: Model) -> Option<Core<'rom>> {
        let mmu = MemoryManagementUnit::new(rom).ok()?;
        let registers = Registers::init(model);
        Some(Core { mmu, registers })
    }

    fn read_instruction_at(&self, address: u16) -> Instruction {
        let byte = self.mmu.read(address);
        match byte {
            0x00 => Instruction::Noop,
            0x0E => {
                let new_value = self.mmu.read(address + 1);
                Instruction::LoadCFrom8Imm { new_value }
            }
            0x11 => {
                let new_value = self.mmu.read_u16(address + 1);
                Instruction::LoadDEFrom16Imm { new_value }
            }
            0x12 => Instruction::StoreAAtDE,
            0x1C => Instruction::IncrementE,
            0x20 => {
                let offset = self.mmu.read(address + 1) as i8;
                Instruction::JumpRelativeIfNotZero { offset }
            }
            0x21 => {
                let new_value = self.mmu.read_u16(address + 1);
                Instruction::LoadHLFrom16Imm { new_value }
            }
            0x2A => Instruction::LoadAFromHLAndInc,
            0x47 => Instruction::LoadBFromA,
            0xC3 => {
                let address = self.mmu.read_u16(address + 1);
                Instruction::Jump { address }
            }
            value => todo!("Unknown instruction {value:#04X}"),
        }
    }
    fn read_instruction(&mut self) -> Instruction {
        self.read_instruction_at(self.registers.pc)
    }

    fn execute(&mut self, instruction: Instruction) {
        self.registers.pc += instruction.size();
        match instruction {
            Instruction::Noop => {}
            Instruction::LoadCFrom8Imm { new_value } => {
                *self.registers.bc.split_mut().1 = new_value
            }
            Instruction::LoadDEFrom16Imm { new_value } => *self.registers.de.get_mut() = new_value,
            Instruction::StoreAAtDE => self.mmu.write(*self.registers.de.get(), self.registers.a),
            Instruction::IncrementE => {
                *self.registers.de.split_mut().1 += 1;
                self.registers.set_zero(*self.registers.de.split().1 == 0);
                self.registers.set_subtraction(false);
                //TODO: Verify half carry register
                self.registers
                    .set_half_carry(self.registers.de.split().1 & 0x0F == 0);
            }
            Instruction::JumpRelativeIfNotZero { offset } => {
                if !self.registers.zero() {
                    self.registers.pc = self.registers.pc.wrapping_add_signed(i16::from(offset));
                }
            }
            Instruction::LoadHLFrom16Imm { new_value } => *self.registers.hl.get_mut() = new_value,
            Instruction::LoadAFromHLAndInc => {
                let byte = self.mmu.read(*self.registers.hl.get());
                self.registers.a = byte;
                *self.registers.hl.get_mut() += 1;
            }
            Instruction::LoadBFromA => *self.registers.bc.split_mut().0 = self.registers.a,
            Instruction::Jump { address } => self.registers.pc = address,
        }
    }

    //TODO: Break reason
    pub fn step(&mut self) -> ControlFlow<(), ()> {
        let instr = self.read_instruction();
        println!("{} | {instr}", self.registers);
        self.execute(instr);
        match instr {
            _ => ControlFlow::Continue(()),
        }
    }
}
