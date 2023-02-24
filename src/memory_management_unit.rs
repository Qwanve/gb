use super::cartridge::{Cartridge, CartridgeParseError};
use bitvec::{bitarr, field::BitField, order::Msb0, prelude::BitArray, view::BitView, BitArr};
use thiserror::Error;

const UNINIT: u8 = 0xCD;
#[derive(Debug, Error)]
pub enum MMUError {
    #[error(transparent)]
    CartridgeError(#[from] CartridgeParseError),
}

pub struct MemoryManagementUnit<'rom> {
    cartridge: Cartridge<'rom>,
    vram: VRam,
    wram: WRam,
    sprite_attribute_table: [SpriteAttributes; 40],
    io_registers: IORegisters,
    hram: [u8; 0x7E],
    interrupt_enable_register: BitArr!(for 5, in u8, Msb0),
}

impl MemoryManagementUnit<'_> {
    pub fn new<'rom>(rom: &'rom [u8]) -> Result<MemoryManagementUnit<'rom>, MMUError> {
        let cartridge = Cartridge::new(rom)?;
        //TODO: Gameboy color support
        let vram = VRam::new(false);
        let wram = WRam::new(false);
        let sprite_attribute_table = [SpriteAttributes::new(); 40];
        let io_registers = IORegisters::new();
        let hram = [0; 0x7E];
        let interrupt_enable_register = bitarr![u8, Msb0; 0; 5];
        Ok(MemoryManagementUnit {
            cartridge,
            vram,
            wram,
            sprite_attribute_table,
            io_registers,
            hram,
            interrupt_enable_register,
        })
    }

    pub fn read(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x3FFF => self.cartridge.mbc().read(address),
            0x4000..=0x7FFF => self.cartridge.mbc().read(address),
            0x8000..=0x9FFF => self.read_vram(address),
            0xA000..=0xBFFF => self.cartridge.mbc().read(address),
            0xC000..=0xCFFF => self.wram.bank_zero[(address - 0xC000) as usize],
            0xD000..=0xDFFF => self.read_wram_switchable_bank(address),
            0xE000..=0xFDFF => self.read(address & 0b0001_1111_1111_1111),
            0xFE00..=0xFE9F => self.read_sprite_attribute_table(address),
            0xFEA0..=0xFEFF => {
                log::warn!("Read from prohibited area");
                //TODO: Make this accurate to hardware
                UNINIT
            }
            0xFF00..=0xFF7F => self.read_io_registers(address),
            0xFF80..=0xFFFE => self.hram[(address - 0xFF80) as usize],
            0xFFFF..=0xFFFF => self.interrupt_enable_register.data[0],
        }
    }

    pub fn read_u16(&self, address: u16) -> u16 {
        u16::from_le_bytes([self.read(address), self.read(address + 1)])
    }

    const fn read_vram(&self, address: u16) -> u8 {
        let bank = match self.vram.bank_one {
            Some(bank) if self.io_registers.vram_bank_select & 0b1 != 0 => bank,
            None | Some(_) => self.vram.bank_zero,
        };
        bank[(address - 0x8000) as usize]
    }

    const fn read_wram_switchable_bank(&self, address: u16) -> u8 {
        let bank_select = self.io_registers.wram_bank_select & 0b111;
        let bank = match self.wram.other_banks {
            WRamSwitchableBanks::Gameboy(bank) => bank,
            WRamSwitchableBanks::GameboyColor(banks) if bank_select == 0 => banks[1],
            WRamSwitchableBanks::GameboyColor(banks) => banks[bank_select as usize],
        };
        bank[(address - 0xD000) as usize]
    }

    const fn read_sprite_attribute_table(&self, address: u16) -> u8 {
        let address = address - 0xFE00;
        let index = address / 4;
        let sprite = self.sprite_attribute_table[index as usize];
        match index % 4 {
            0 => sprite.x_pos,
            1 => sprite.y_pos,
            2 => sprite.tile_index,
            3 => sprite.flags.data[0],
            _ => unreachable!(),
        }
    }

    fn read_io_registers(&self, address: u16) -> u8 {
        match address {
            0x0000..=0xFEFF => unreachable!(),
            0xFF80..=0xFFFF => unreachable!(),
            0xFF00 => todo!("Read joystick"),
            0xFF01 => todo!("Read serial transfer"),
            0xFF02 => todo!("Read serial transfer control"),
            0xFF04 => todo!("Read divider register"),
            0xFF05 => todo!("Read timer counter"),
            0xFF06 => todo!("Read timer modulo"),
            0xFF07 => todo!("Read timer control"),
            0xFF0F => todo!("Read interrupt flag"),
            0xFF10 => todo!("Read channel 1 sweep"),
            0xFF11 => todo!("Read channel 1 length timer and duty cycle"),
            0xFF12 => todo!("Read channel 1 volume and envelope"),
            0xFF14 => todo!("Read channel 1 control"),
            0xFF16 => todo!("Read channel 2 length timer and duty cycle"),
            0xFF17 => todo!("Read channel 2 volume and envelope"),
            0xFF19 => todo!("Read channel 2 control"),
            0xFF1A => todo!("Read channel 3 DAC enable"),
            0xFF1C => todo!("Read channel 3 output level"),
            0xFF1E => todo!("Read channel 3 control"),
            0xFF21 => todo!("Read channel 4 volume and envelope"),
            0xFF22 => todo!("Read channel 4 frequency and randomness"),
            0xFF23 => todo!("Read channel 4 control"),
            0xFF24 => todo!("Read master volume and VIN panning"),
            0xFF25 => todo!("Read channel panning"),
            0xFF26 => todo!("Read sound on/off"),
            0xFF30..=0xFF3F => todo!("Read wave pattern ram"),
            0xFF40 => todo!("Read LCD control"),
            0xFF41 => todo!("Read LCD status"),
            0xFF42 => todo!("Read viewport Y position"),
            0xFF43 => todo!("Read viewport X position"),
            0xFF44 => todo!("Read LCD Y"),
            0xFF45 => todo!("Read LCD Y compare"),
            0xFF46 => todo!("Read DMA OBJ source address"),
            0xFF47 => todo!("Read BG palette data"),
            0xFF48..=0xFF49 => todo!("Read OBJ palette"),
            0xFF4A => todo!("Read window Y"),
            0xFF4B => todo!("Read window X"),
            //Unknown
            0xFF4C => todo!("Read KEY0"),
            0xFF4D => todo!("Read speed switch"),
            0xFF4F => todo!("Read VRAM bank selection"),
            0xFF50 => todo!("Read boot rom disable"),
            0xFF55 => todo!("Read DMA VRAM source address"),
            0xFF56 => todo!("Read infrared comm. port"),
            0xFF6C => todo!("Read object priority mode"),
            0xFF68 => todo!("Read color palette selection / background palette index"),
            0xFF69 => todo!("Read background palette data"),
            0xFF6A => todo!("Read OBJ palette index"),
            0xFF6B => todo!("Read OBJ palette data"),
            0xFF70 => todo!("Read WRAM bank selection"),
            0xFF72..=0xFF75 => todo!("Read undocumented register"),
            0xFF76 => todo!("Read channel 1 and 2 digital output"),
            0xFF77 => todo!("Read channel 3 and 4 digital output"),

            0xFF13 | 0xFF18 | 0xFF1B | 0xFF1D | 0xFF20 | 0xFF51..=0xFF54 => {
                log::warn!("Read write-only I/O register {address}");
                UNINIT
            }
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF15
            | 0xFF1F
            | 0xFF27..=0xFF2F
            | 0xFF4E
            | 0xFF57..=0xFF67
            | 0xFF6D..=0xFF6F
            | 0xFF71
            | 0xFF78..=0xFF7F => {
                log::warn!("Read in unmapped I/O register {address}");
                UNINIT
            }
        }
    }

    pub fn write(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x3FFF => todo!("Write to cartridge"),
            0x4000..=0x7FFF => todo!("Write to cartridge"),
            0x8000..=0x9FFF => todo!("Write to VRAM"),
            0xA000..=0xBFFF => todo!("Write to External Ram"),
            0xC000..=0xCFFF => self.wram.bank_zero[(address - 0xC000) as usize] = value,
            0xD000..=0xDFFF => self.write_wram_switchable_bank(address, value),
            0xE000..=0xFDFF => todo!("Write to Echo RAM"),
            0xFE00..=0xFE9F => todo!("Write to Sprite Attribute Table"),
            0xFEA0..=0xFEFF => todo!("Write to prohibited area"),
            0xFF00..=0xFF7F => self.write_io_registers(address, value),
            0xFF80..=0xFFFE => todo!("Write to High RAM"),
            0xFFFF => todo!("Write to Interrupt Enable Register"),
        }
    }

    pub fn write_wram_switchable_bank(&mut self, address: u16, value: u8) {
        let bank_select = self.io_registers.wram_bank_select & 0b111;
        let mut bank = match self.wram.other_banks {
            WRamSwitchableBanks::Gameboy(bank) => bank,
            WRamSwitchableBanks::GameboyColor(banks) if bank_select == 0 => banks[1],
            WRamSwitchableBanks::GameboyColor(banks) => banks[bank_select as usize],
        };
        bank[(address - 0xD000) as usize] = value;
    }

    pub fn write_io_registers(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0xFEFF | 0xFF80..=0xFFFF => unreachable!(),
            0xFF04 => self.io_registers.timers.divide_register = 0,
            //TODO: Verify this behavior
            0xFF05 => self.io_registers.timers.timer_register = value,
            0xFF06 => self.io_registers.timers.timer_modulo = value,
            0xFF07 => {
                let enable = value & 0b0000_0100 != 0;
                let clock = ClockSpeed::try_from(value & 0b0000_0011).unwrap();
                self.io_registers.timers.timer_enable = enable;
                self.io_registers.timers.clock_speed = clock;
            }
            _ => todo!("Write to I/O register ${address:04X}"),
        }
    }

    pub fn update_timers(&mut self, clock: u8) {
        self.io_registers.timers.update(clock)
    }
}

struct VRam {
    bank_zero: [u8; 8 * 1024],
    bank_one: Option<[u8; 8 * 1024]>,
}

impl VRam {
    fn new(gbc: bool) -> Self {
        VRam {
            bank_zero: [0; 8 * 1024],
            bank_one: if gbc { Some([0; 8 * 1024]) } else { None },
        }
    }
}

enum WRamSwitchableBanks {
    Gameboy([u8; 4 * 1024]),
    GameboyColor([[u8; 4 * 1024]; 7]),
}

struct WRam {
    bank_zero: [u8; 4 * 1024],
    other_banks: WRamSwitchableBanks,
}

impl WRam {
    fn new(gbc: bool) -> Self {
        WRam {
            bank_zero: [0; 4 * 1024],
            other_banks: if gbc {
                WRamSwitchableBanks::GameboyColor([[0; 4 * 1024]; 7])
            } else {
                WRamSwitchableBanks::Gameboy([0; 4 * 1024])
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct SpriteAttributes {
    y_pos: u8,
    x_pos: u8,
    tile_index: u8,
    flags: BitArr!(for 8, in u8, Msb0),
}

impl SpriteAttributes {
    fn new() -> Self {
        SpriteAttributes {
            y_pos: 0,
            x_pos: 0,
            tile_index: 0,
            flags: bitarr![u8, Msb0; 0; 8],
        }
    }
}

struct IORegisters {
    joypad: u8,
    serial_transfer: (u8, u8),
    timers: Timers,
    // audio: Audio,
    // wave: WavePattern,
    // lcd_control: LCDControl,
    vram_bank_select: u8,
    boot_rom_disable: bool,
    // vram_dma: VRamDMA,
    // palettes: Pallets,
    wram_bank_select: u8,
}

impl IORegisters {
    fn new() -> Self {
        IORegisters {
            joypad: 0,
            serial_transfer: (0, 0),
            timers: Timers::new(),
            vram_bank_select: 0,
            boot_rom_disable: false,
            wram_bank_select: 0,
        }
    }
}

struct Timers {
    timer_enable: bool,
    clock_speed: ClockSpeed,
    divide_register: u8,
    timer_register: u8,
    timer_modulo: u8,
    clocks: u16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ClockSpeed {
    OncePer1024,
    OncePer16,
    OncePer64,
    OncePer256,
}

impl TryFrom<u8> for ClockSpeed {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0b00 => Ok(ClockSpeed::OncePer1024),
            0b01 => Ok(ClockSpeed::OncePer16),
            0b10 => Ok(Self::OncePer64),
            0b11 => Ok(Self::OncePer256),
            _ => Err(()),
        }
    }
}

impl From<ClockSpeed> for u16 {
    fn from(value: ClockSpeed) -> Self {
        match value {
            ClockSpeed::OncePer1024 => 1024,
            ClockSpeed::OncePer16 => 16,
            ClockSpeed::OncePer64 => 64,
            ClockSpeed::OncePer256 => 256,
        }
    }
}

impl Timers {
    pub fn new() -> Self {
        Timers {
            timer_enable: false,
            clock_speed: ClockSpeed::OncePer1024,
            //TODO: Different Models have different values here
            divide_register: 0x18,
            timer_register: 0,
            timer_modulo: 0,
            clocks: 0,
        }
    }

    pub fn update(&mut self, clocks: u8) {
        if self.timer_enable {
            self.divide_register = self.divide_register.wrapping_add(clocks);
            self.clocks = (self.clocks + u16::from(clocks)) % u16::from(self.clock_speed);
            if self.clocks == 0 {
                let (new, overflow) = self.timer_register.overflowing_add(1);
                if overflow {
                    self.timer_register = self.timer_modulo;
                    //TODO: Generate interrupts
                } else {
                    self.timer_register = new;
                }
            }
        }
    }
}
