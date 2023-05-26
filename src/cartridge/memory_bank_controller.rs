use log::warn;

const UNINIT: u8 = 0xCD;
pub trait MemoryBankController {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);
}

pub struct RomOnly {
    rom: [u8; 32 * 1024],
}
impl RomOnly {
    pub fn new(rom: &[u8]) -> RomOnly {
        RomOnly {
            rom: rom.try_into().unwrap_or_else(|_| {
                let mut rom = rom.to_vec();
                rom.resize(32 * 1024, UNINIT);
                rom.try_into().unwrap()
            }),
        }
    }
}
impl MemoryBankController for RomOnly {
    fn read(&self, address: u16) -> u8 {
        if address > 0x7FFF {
            warn!("Invalid ROM read address `{address}`");
            UNINIT
        } else {
            self.rom[usize::from(address)]
        }
    }

    fn write(&mut self, address: u16, _value: u8) {
        if address > 0x7FFF {
            warn!("Invalid ROM write address `{address}`");
        } else {
            warn!("Writing to ROM");
            // self.rom[usize::from(address)] = value;
        }
    }
}

pub struct MBC1 {
    banks: Vec<[u8; 16 * 1024]>,
    bank_selection: u8,
    high_bank_selection: u8,
    banking_mode: bool,
}

impl MBC1 {
    pub fn new(rom: &[u8], rom_banks: u16) -> MBC1 {
        let banks: Vec<_> = rom
            .chunks_exact(0x4000)
            .map(<[u8; 16 * 1024]>::try_from)
            .map(Result::unwrap)
            .collect();
        if banks.len() != usize::from(rom_banks) {
            warn!(
                "Mismatch in bank sizes. Found {} banks, expected {rom_banks} banks",
                banks.len()
            );
        }
        MBC1 {
            banks,
            banking_mode: false,
            bank_selection: 0,
            high_bank_selection: 0,
        }
    }

    pub fn currently_selected_bank(&self) -> u8 {
        let mut bank = self.bank_selection | (self.high_bank_selection << 5);
        if usize::from(bank) > self.banks.len() {
            let mask = self.banks.len().ilog2();
            //256KiB -> bank & 0b00001111
            // 32KiB -> bank & 0b00000001
            bank = bank & u8::MAX >> (8 - mask);
        }
        bank
    }
}

impl MemoryBankController for MBC1 {
    fn read(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x3FFF => {
                if !self.banking_mode {
                    self.banks[0][usize::from(address)]
                } else {
                    self.banks[(self.high_bank_selection as usize) << 5][usize::from(address)]
                }
            }
            0x4000..=0x7FFF => {
                if self.bank_selection == 0 {
                    self.banks[1][usize::from(address) - 0x4000]
                } else {
                    self.banks[usize::from(self.currently_selected_bank())]
                        [usize::from(address) - 0x4000]
                }
            }
            _ => {
                log::error!("Invalid ROM read address `{address}`");
                UNINIT
            }
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1FFF => log::error!("Tried to enable external ram that doesn't exist"),
            0x2000..=0x3FFF => {
                //Rom bank selection
                let value = value & 0b00011111;
                self.bank_selection = value;
            }
            0x4000..=0x5FFF => {
                //Upper rom bank selection
                if self.banks.len() >= 64 {
                    let value = value & 0b00000011;
                    self.high_bank_selection = value;
                }
            }
            0x6000..=0x7FFF => {
                //Banking mode select
                let value = (value & 0b00000001) == 1;
                self.banking_mode = value;
            }
            _ => log::error!("Invalid ROM write address `{address}`"),
        }
    }
}
