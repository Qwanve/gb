pub mod header;
pub mod memory_bank_controller;

use memory_bank_controller::MemoryBankController;
use thiserror::Error;

use self::header::Header;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CartridgeType {
    RomOnly,
    MBC1,
    MBC1WithRam,
    MBC1WithRamAndBattery,
    MBC2,
    MBC2WithBattery,
    RomWithRam,
    RomWithRamAndBattery,
    MMM01,
    MMM01WithRam,
    MMM01WithRamAndBattery,
    MBC3WithTimerAndBattery,
    MBC3WithTimerAndRamAndBattery,
    MBC3,
    MBC3WithRam,
    MBC3WithRamAndBattery,
    MBC5,
    MBC5WithRam,
    MBC5WithRamAndBattery,
    MBC5WithRumble,
    MBC5WithRumbleAndRam,
    MBC5WithRumbleAndRamAndBattery,
    MBC6,
    MBC7WithSensorAndRumbleAndRamAndBattery,
    PocketCamera,
    BandaiTama5,
    HuC3,
    HuC1WithRamAndBattery,
}

impl TryFrom<u8> for CartridgeType {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0x00 => CartridgeType::RomOnly,
            0x01 => CartridgeType::MBC1,
            0x02 => CartridgeType::MBC1WithRam,
            0x03 => CartridgeType::MBC1WithRamAndBattery,
            0x05 => CartridgeType::MBC2,
            0x06 => CartridgeType::MBC2WithBattery,
            0x08 => CartridgeType::RomWithRam,
            0x09 => CartridgeType::RomWithRamAndBattery,
            0x0B => CartridgeType::MMM01,
            0x0C => CartridgeType::MMM01WithRam,
            0x0D => CartridgeType::MMM01WithRamAndBattery,
            0x0F => CartridgeType::MBC3WithTimerAndBattery,
            0x10 => CartridgeType::MBC3WithTimerAndRamAndBattery,
            0x11 => CartridgeType::MBC3,
            0x12 => CartridgeType::MBC3WithRam,
            0x13 => CartridgeType::MBC3WithRamAndBattery,
            0x19 => CartridgeType::MBC5,
            0x1A => CartridgeType::MBC5WithRam,
            0x1B => CartridgeType::MBC5WithRamAndBattery,
            0x1C => CartridgeType::MBC5WithRumble,
            0x1D => CartridgeType::MBC5WithRumbleAndRam,
            0x1E => CartridgeType::MBC5WithRumbleAndRamAndBattery,
            0x20 => CartridgeType::MBC6,
            0x22 => CartridgeType::MBC7WithSensorAndRumbleAndRamAndBattery,
            0xFC => CartridgeType::PocketCamera,
            0xFD => CartridgeType::BandaiTama5,
            0xFE => CartridgeType::HuC3,
            0xFF => CartridgeType::HuC1WithRamAndBattery,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Error)]
pub enum CartridgeParseError {
    #[error(transparent)]
    Header(#[from] header::HeaderParseError),
}

pub struct Cartridge<'rom> {
    header: Header<'rom>,
    memory_bank_controller: Box<dyn MemoryBankController>,
}

impl Cartridge<'_> {
    pub fn new(rom: &[u8]) -> Result<Cartridge, CartridgeParseError> {
        let header = Header::read(rom)?;
        let mbc: Box<dyn MemoryBankController> = match header.cartridge_type() {
            CartridgeType::RomOnly => Box::new(memory_bank_controller::RomOnly::new(rom)),
            CartridgeType::MBC1 => {
                Box::new(memory_bank_controller::MBC1::new(rom, header.rom_banks()))
            }
            v => todo!("Memory Bank Controller: {v:?}"),
        };

        Ok(Cartridge {
            header,
            memory_bank_controller: mbc,
        })
    }

    pub const fn header(&self) -> &Header {
        &self.header
    }

    pub const fn mbc(&self) -> &dyn MemoryBankController {
        &*self.memory_bank_controller
    }

    pub fn mbc_mut(&mut self) -> &mut dyn MemoryBankController {
        &mut *self.memory_bank_controller
    }
}
