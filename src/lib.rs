pub mod cartridge;
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
