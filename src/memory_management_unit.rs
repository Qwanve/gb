use super::cartridge::Cartridge;
pub struct MemoryManagementUnit<'rom> {
    cartridge: Cartridge<'rom>,
    vram: VRam,
    wram: WRam,
    sprite_attribute_table: [SpriteAttributes; 40],
    io_registers: IORegisters,
    hram: [u8; 0x7E],
    interrupt_enable_register: u8,
}

struct VRam {
    bank_zero: [u8; 8 * 1024],
    bank_one: Option<[u8; 8 * 1024]>,
}

struct WRam {
    bank_zero: [u8; 4 * 1024],
    other_banks: Vec<[u8; 4 * 1024]>,
}

struct SpriteAttributes {
    y_pos: u8,
    x_pos: u8,
    tile_index: u8,
    flags: bitvec::BitArr!(for 8, in u8, bitvec::order::Msb0),
}

struct IORegisters {
    joypad: u8,
    serial_transfer: (u8, u8),
    // timers: Timers,
    // audio: Audio,
    // wave: WavePattern,
    // lcd_control: LCDControl,
    vram_bank_select: u8,
    boot_rom_disable: bool,
    // vram_dma: VRamDMA,
    // palettes: Pallets,
    wram_bank_select: u8,
}
