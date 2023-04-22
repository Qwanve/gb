use crate::cartridge::CartridgeType;
use std::{fmt::Display, num::Wrapping};
use thiserror::Error;

pub mod license;

static NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

#[derive(Debug, Error)]
pub enum HeaderParseError {
    #[error("address {0} not found")]
    InvalidLength(usize),
    #[error("property {0} invalid at address {1}")]
    InvalidPropertyValue(String, usize),
    #[error("logo validation failed")]
    InvalidLogo,
    #[error("header checksum failed")]
    InvalidHeaderChecksum,
    #[error("global checksum failed")]
    InvalidGlobalChecksum,
}

///A parsed and validated rom Header.
#[derive(Debug, PartialEq, Eq)]
pub struct Header<'rom> {
    title: Option<&'rom str>,
    manufacturer_code: Option<&'rom str>,
    cgb_flag: Option<CGBFlag>,
    new_licensee_code: Option<&'rom str>,
    sgb_flag: bool,
    cartridge_type: CartridgeType,
    rom_size: u8,
    ram_size: u8,
    oversea_exclusive: bool,
    old_licensee_code: u8,
    mask_rom_version_number: u8,
    header_checksum: u8,
    global_checksum: u16,
}

impl Header<'_> {
    ///Reads and validates a header from the given ROM bytes.
    ///
    ///# Errors
    /// - [`HeaderParseError::InvalidLength`]: If the length of the rom is too
    /// short and a property fails to read, this will be returned with
    /// the position of the property
    /// - [`HeaderParseError::InvalidPropertyValue`]: If a value contains an unknown
    /// value in a mandatory field, this will be returned with the position
    /// and field name of the property
    /// - [`HeaderParseError::InvalidLogo`]: If the ROM header contains an invalid
    /// Nintendo logo, this error will be returned
    /// - [`HeaderParseError::InvalidHeaderChecksum`]: If the ROM header checksum
    /// fails to match the calculated checksume, this error will be returned
    /// - [`HeaderParseError::InvalidGlobalChecksum`]: This is not currently used
    /// but may be returned in later versions
    pub fn read(rom: &'_ [u8]) -> Result<Header<'_>, HeaderParseError> {
        let cgb_flag = CGBFlag::try_from(
            rom.get(0x143)
                .copied()
                .ok_or(HeaderParseError::InvalidLength(0x143))?,
        )
        .ok();
        let mut result = Header {
            //TODO: Lossy UTF-8? Glueskrad uses latin-2 encoding
            title: {
                if cgb_flag.is_some() {
                    std::str::from_utf8(
                        rom.get(0x134..=0x142)
                            .ok_or(HeaderParseError::InvalidLength(0x134))?,
                    )
                    .ok()
                } else {
                    std::str::from_utf8(
                        rom.get(0x134..=0x143)
                            .ok_or(HeaderParseError::InvalidLength(0x134))?,
                    )
                    .ok()
                }
            },
            manufacturer_code: std::str::from_utf8(
                rom.get(0x13F..=0x142)
                    .ok_or(HeaderParseError::InvalidLength(0x13F))?,
            )
            .ok(),
            cgb_flag,
            new_licensee_code: {
                std::str::from_utf8(
                    rom.get(0x144..=0x145)
                        .ok_or(HeaderParseError::InvalidLength(0x13F))?,
                )
                .ok()
            },
            sgb_flag: rom
                .get(0x146)
                .copied()
                .ok_or(HeaderParseError::InvalidLength(0x146))?
                == 0x03,
            cartridge_type: CartridgeType::try_from(
                rom.get(0x147)
                    .copied()
                    .ok_or(HeaderParseError::InvalidLength(0x147))?,
            )
            .map_err(|_| HeaderParseError::InvalidPropertyValue("Cartridge Type".into(), 0x147))?,
            rom_size: rom
                .get(0x148)
                .copied()
                .ok_or(HeaderParseError::InvalidLength(0x148))?,
            ram_size: rom
                .get(0x149)
                .copied()
                .ok_or(HeaderParseError::InvalidLength(0x149))?,
            oversea_exclusive: {
                let value = rom
                    .get(0x14A)
                    .copied()
                    .ok_or(HeaderParseError::InvalidLength(0x14A))?;
                if value != 0 && value != 1 {
                    log::warn!("Unknown Destination Code `{value}`");
                }
                value == 1
            },
            old_licensee_code: rom
                .get(0x14B)
                .copied()
                .ok_or(HeaderParseError::InvalidLength(0x14B))?,
            mask_rom_version_number: rom
                .get(0x14C)
                .copied()
                .ok_or(HeaderParseError::InvalidLength(0x14C))?,
            header_checksum: rom
                .get(0x14D)
                .copied()
                .ok_or(HeaderParseError::InvalidLength(0x14D))?,
            global_checksum: {
                let bytes = rom
                    .get(0x14E..=0x14F)
                    .ok_or(HeaderParseError::InvalidLength(0x14E))?;
                let byte_array = bytes.try_into().unwrap();
                u16::from_be_bytes(byte_array)
            },
        };

        //If Gameboy Color, adjust title to shorter size
        if result.cgb_flag.is_some() {
            if let Some(ref mut x) = result.title {
                if let Some(ref mut mfc) = result.manufacturer_code {
                    if mfc.contains('\0') {
                        *mfc = "????";
                    } else {
                        *x = &x[..=11];
                    }
                }
            }
        } else {
            // If not CGB, we don't have a manufacturer code
            if let Some(ref mut x) = result.manufacturer_code {
                *x = "";
            }
        }

        //If we need the new licensee code and it doesn't exist, error
        if result.old_licensee_code == 0x33 && result.new_licensee_code.is_none() {
            return Err(HeaderParseError::InvalidPropertyValue(
                "New Licensee Code".into(),
                0x144,
            ));
        }

        //Verify Rom size is appropriate
        if !(0..=8).contains(&result.rom_size) {
            return Err(HeaderParseError::InvalidPropertyValue(
                "Rom Size".into(),
                0x148,
            ));
        }

        //Verify Ram size is appropriate
        if !(0..=7).contains(&result.ram_size) {
            return Err(HeaderParseError::InvalidPropertyValue(
                "Ram Size".into(),
                0x149,
            ));
        }
        if !Header::verify_logo(rom)? {
            return Err(HeaderParseError::InvalidLogo);
        }
        if !result.verify_header_checksum(rom)? {
            return Err(HeaderParseError::InvalidHeaderChecksum);
        }
        //Just warn on failed global checksum here
        if !result.verify_rom_checksum(rom)? {
            log::error!("Global Checksum Invalid");
        }
        Ok(result)
    }

    fn verify_logo(rom: &[u8]) -> Result<bool, HeaderParseError> {
        let logo = &rom
            .get(0x104..=0x133)
            .ok_or(HeaderParseError::InvalidLength(0x104))?;
        Ok(*logo == NINTENDO_LOGO)
    }

    fn verify_header_checksum(&self, rom: &[u8]) -> Result<bool, HeaderParseError> {
        let check_sum = rom
            .get(0x134..=0x14C)
            .ok_or(HeaderParseError::InvalidLength(0x134))?
            .iter()
            .fold(Wrapping(0u8), |x, byte| x - Wrapping(*byte) - Wrapping(1))
            .0;
        Ok(check_sum == self.header_checksum)
    }

    fn verify_rom_checksum(&self, rom: &[u8]) -> Result<bool, HeaderParseError> {
        let sum = rom
            .get(0x0..=0x14D)
            .ok_or(HeaderParseError::InvalidLength(0x0))?
            .iter()
            .chain(
                rom.get(0x150..)
                    .ok_or(HeaderParseError::InvalidLength(0x150))?
                    .iter(),
            )
            .copied()
            .map(|x| Wrapping(u16::from(x)))
            .sum::<Wrapping<u16>>()
            .0;
        Ok(sum == self.global_checksum)
    }

    #[must_use]
    pub const fn title(&self) -> Option<&str> {
        self.title
    }

    #[must_use]
    pub const fn manufacturer_code(&self) -> Option<&str> {
        self.manufacturer_code
    }

    #[must_use]
    pub const fn cgb_flag(&self) -> Option<CGBFlag> {
        self.cgb_flag
    }

    #[must_use]
    pub const fn new_licensee_code(&self) -> Option<&str> {
        self.new_licensee_code
    }

    #[must_use]
    pub const fn sgb_flag(&self) -> bool {
        self.sgb_flag
    }

    #[must_use]
    pub const fn cartridge_type(&self) -> CartridgeType {
        self.cartridge_type
    }

    #[must_use]
    pub const fn rom_size(&self) -> u8 {
        self.rom_size
    }

    pub const fn rom_banks(&self) -> u16 {
        let value = self.rom_size as u16;
        2u16.pow((value + 1) as u32)
    }

    #[must_use]
    pub const fn ram_size(&self) -> u8 {
        self.ram_size
    }

    #[must_use]
    pub const fn oversea_exclusive(&self) -> bool {
        self.oversea_exclusive
    }

    #[must_use]
    pub const fn old_licensee_code(&self) -> u8 {
        self.old_licensee_code
    }

    #[must_use]
    pub const fn rom_version(&self) -> u8 {
        self.mask_rom_version_number
    }

    #[must_use]
    pub const fn header_checksum(&self) -> u8 {
        self.header_checksum
    }

    #[must_use]
    pub const fn global_checksum(&self) -> u16 {
        self.global_checksum
    }
}

impl Display for Header<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:>16} ", self.title().unwrap_or("No Title"))?;
        write!(f, "v{} ", self.rom_version())?;
        write!(f, "by {}, ", self.licensee_from_code())?;
        if let Some(flag) = self.cgb_flag() {
            let code = self.manufacturer_code().unwrap_or("????");
            write!(f, "CGB-{code} ")?;
            write!(f, "CGB: {flag:?}")
        } else {
            write!(f, "DMG-????")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CGBFlag {
    GameboySupport,
    GameboyColorOnly,
    PGBMode,
}

impl TryFrom<u8> for CGBFlag {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0..=0x7F => Err(()),
            _ if value & 0b0100_1100 == 0 => Ok(Self::GameboySupport),
            _ if value & 0b0000_1100 == 0 => Ok(Self::GameboyColorOnly),
            _ if value & 0b0000_1100 != 0 => Ok(Self::PGBMode),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Header;

    #[test]
    fn verify_header() {
        use std::path::PathBuf;
        let path = PathBuf::from("./test.gb");
        assert!(path.try_exists().unwrap());
        let rom = std::fs::read(path).unwrap();
        let _header = Header::read(&rom).unwrap();
    }
}
