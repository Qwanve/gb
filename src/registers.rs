use std::fmt::Display;

use super::Model;
use bitvec::prelude as bv;

/// A container for the cpu registers. Contains all registers a-e, h and l in split forms,
/// as well as the stack pointer and program counter, and the flag values.
pub struct Registers {
    pub a: u8,
    f: bv::BitArr!(for 4, in u8, bv::Msb0),
    pub bc: SplitRegister,
    pub de: SplitRegister,
    pub hl: SplitRegister,
    pub pc: u16,
    pub sp: u16,
}

impl Registers {
    /// Creates and initializes a new set of registers based on the given model.
    #[must_use]
    pub fn init(model: Model) -> Registers {
        match model {
            Model::DMG0 => Registers {
                a: 0x01,
                f: bv::bitarr![u8, bv::Msb0; 0, 0, 0, 0],
                bc: SplitRegister(0xFF13),
                de: SplitRegister(0x00C1),
                hl: SplitRegister(0x8403),
                pc: 0x100,
                sp: 0xFFFE,
            },
            _ => todo!("Different model register initial values : {model:?}"),
        }
    }

    /// Returns the registers flags as a grouped u8.
    /// The upper four bits are Zero, Subtract, Half-carry and Carry in order
    /// The lower four bits should be zero. This, however, is only checked in debug mode
    #[must_use]
    pub fn flags(&self) -> u8 {
        let res = self.f.into_inner()[0];
        debug_assert_eq!(res & 0xF, 0);
        res
    }

    ///Returns a copy of the zero flag
    ///
    ///This is set when the result of the last operation was zero
    #[must_use]
    pub fn zero(&self) -> bool {
        self.f[0]
    }

    ///Set the zero flag
    pub fn set_zero(&mut self, value: bool) {
        self.f.set(0, value)
    }

    ///Returns a copy of the subtraction flag
    ///
    ///This is set when the last operation was a subtraction
    /// Used in Binary Coded Decimal scenarios
    #[must_use]
    pub fn subtraction(&self) -> bool {
        self.f[1]
    }

    ///Set the subtraction flag
    pub fn set_subtraction(&mut self, value: bool) {
        self.f.set(1, value)
    }

    ///Returns a copy of the half carry flag
    ///
    ///This is set in 8-bit addition/subtraction when the 4th bit overflows
    /// Used in Binary Coded Decimal scenarios
    #[must_use]
    pub fn half_carry(&self) -> bool {
        self.f[2]
    }

    ///Set the half carry flag
    pub fn set_half_carry(&mut self, value: bool) {
        self.f.set(2, value)
    }

    ///Returns a copy of the carry flag
    ///
    ///This is set when the 8th bit overflows in 8-bit operations or the 16th in 16-bit operations
    #[must_use]
    pub fn carry(&self) -> bool {
        self.f[3]
    }

    ///Set the carry flag
    pub fn set_carry(&mut self, value: bool) {
        self.f.set(3, value)
    }

    ///Constructs and returns AF
    pub fn af(&self) -> u16 {
        u16::from_be_bytes([self.a, self.flags()])
    }

    pub fn set_af(&mut self, new_value: u16) {
        let bytes = new_value.to_le_bytes();
        self.a = bytes[1];
        self.f.data = [bytes[0] & 0xF0];
    }
}

impl Display for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "A: {:02X} ", self.a)?;
        write!(f, "F: {:02X} ", self.flags())?;
        write!(f, "B: {:02X} ", self.bc.split().high)?;
        write!(f, "C: {:02X} ", self.bc.split().low)?;
        write!(f, "D: {:02X} ", self.de.split().high)?;
        write!(f, "E: {:02X} ", self.de.split().low)?;
        write!(f, "H: {:02X} ", self.hl.split().high)?;
        write!(f, "L: {:02X} ", self.hl.split().low)?;
        write!(f, "SP: {:04X} ", self.sp)?;
        //TODO: Rom Bank number
        write!(f, "PC: {:04X} ", self.pc)
    }
}

///A simple wrapper that emulates a 16-bit register that can also be used
/// as two 8-bit registers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SplitRegister(u16);

pub struct SplitMut<'a> {
    pub high: &'a mut u8,
    pub low: &'a mut u8,
}

pub struct Split<'a> {
    pub high: &'a u8,
    pub low: &'a u8,
}

impl SplitRegister {
    ///Creates a new splittable register
    #[must_use]
    pub const fn new(v: u16) -> Self {
        SplitRegister(v)
    }
    ///Splits the register into mutable 8bit components.
    ///Returns a tuple of (high, low) registers
    ///
    ///# Example
    ///```
    ///# use gb::registers::SplitRegister;
    ///let mut x = SplitRegister::new(0);
    ///let registers = x.split_mut();
    ///*registers.high = 0x10;
    ///*registers.low = 0x15;
    ///assert_eq!(*x.get(), 0x1015);
    ///```
    #[must_use]
    pub fn split_mut<'a>(&'a mut self) -> SplitMut<'a> {
        let s: &'a mut (u8, u8) =
            unsafe { &mut *(std::ptr::addr_of_mut!(self.0).cast::<(u8, u8)>()) };
        // let s: &'a mut (u8, u8) = unsafe { std::mem::transmute(&mut self.0) };
        // (&mut s.1, &mut s.0)
        SplitMut {
            high: &mut s.1,
            low: &mut s.0,
        }
    }
    ///Splits the register into immutable 8-bit components.
    ///Returns a tuple of (high, low) registers
    ///
    ///# Example
    ///```
    ///# use gb::registers::SplitRegister;
    ///let x = SplitRegister::new(0x5613);
    ///let registers = x.split();
    ///assert_eq!(*registers.high, 0x56);
    ///assert_eq!(*registers.low, 0x13);
    ///````
    #[must_use]
    pub const fn split<'a>(&'a self) -> Split<'a> {
        let s: &'a (u8, u8) = unsafe { &*(std::ptr::addr_of!(self.0).cast::<(u8, u8)>()) };
        // let s: &'a mut (u8, u8) = unsafe { std::mem::transmute(&mut self.0) };
        // (&s.1, &s.0)
        Split {
            high: &s.1,
            low: &s.0,
        }
    }
    ///Returns a mutable reference to the combined 16-bit register
    ///
    ///# Example
    ///```
    ///# use gb::registers::SplitRegister;
    ///let mut x = SplitRegister::new(0x1234);
    ///*x.get_mut() = 0xBEEF;
    ///assert_eq!(*x.get(), 0xBEEF);
    ///```
    #[must_use]
    pub fn get_mut(&mut self) -> &mut u16 {
        &mut self.0
    }
    ///Returns a reference to the combined 16-bit register
    ///
    ///# Example
    ///```
    ///# use gb::registers::SplitRegister;
    ///let x = SplitRegister::new(0x8675);
    ///assert_eq!(*x.get(), 0x8675);
    ///```
    #[must_use]
    pub const fn get(&self) -> &u16 {
        &self.0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn new_cpu() {
        let r = Registers::init(Model::DMG0);
        assert_eq!(r.a, 0x01);
        assert_eq!(r.flags(), 0b0000_0000);
        assert_eq!(r.zero(), false);
        assert_eq!(r.subtraction(), false);
        assert_eq!(r.half_carry(), false);
        assert_eq!(r.carry(), false);
        assert_eq!(*r.bc.split().high, 0xFF);
        assert_eq!(*r.bc.split().low, 0x13);
        assert_eq!(*r.de.split().high, 0x00);
        assert_eq!(*r.de.split().low, 0xC1);
        assert_eq!(*r.hl.split().high, 0x84);
        assert_eq!(*r.hl.split().low, 0x03);
        assert_eq!(r.pc, 0x100);
        assert_eq!(r.sp, 0xFFFE);
    }
}
