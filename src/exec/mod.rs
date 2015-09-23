//! Execution engines will run a 68000 program. Given a CPU state and a memory
//! layout, they decode instructions and execute them according to some scheme.
//! Execution engines will stop after any branch, at which point the program
//! counter indicates the location of the next instruction to be executed. There
//! is no cycle counting.

use std::fmt;

use instruction::Size;

pub mod interpret;

/// CPU state representation. The whole thing!
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct CPU {
    #[doc="Data registers"]              pub data: [u32; 8],
    #[doc="Address registers"]           pub addr: [u32; 8],
    #[doc="Supervisor stack pointer"]    pub ssp: u32,
    #[doc="Program counter"]             pub pc: u32,
    #[doc="Status register"]             pub status: u16,
}

// Exception vector numbers
pub const EXC_RESET_SSP:     u32 =   0;
pub const EXC_RESET_PC:      u32 =   1;
pub const EXC_ILLEGAL:       u32 =   4;
pub const EXC_ZERO_DIV:      u32 =   5;
pub const EXC_CHK:           u32 =   6;
pub const EXC_TRAPV:         u32 =   7;
pub const EXC_PRIV:          u32 =   8;
pub const EXC_TRACE:         u32 =   9;
pub const EXC_INT_BASE:      u32 =  24;
pub const EXC_TRAP_BASE:     u32 =  32;
pub const EXC_USER_BASE:     u32 =  64;

pub const SUPERVISOR_BIT:    u16 =  0b0010_0000_000_00000;
pub const X_BIT:             u16 =  0b0000_0000_000_10000;
pub const N_BIT:             u16 =  0b0000_0000_000_01000;
pub const Z_BIT:             u16 =  0b0000_0000_000_00100;
pub const V_BIT:             u16 =  0b0000_0000_000_00010;
pub const C_BIT:             u16 =  0b0000_0000_000_00001;

impl CPU {
    pub fn new() -> CPU {
        CPU {
            data: [0; 8],
            addr: [0; 8],
            ssp: 0,
            pc: 0,
            status: 0,
        }
    }

    pub fn cc_x(&self) -> bool { (self.status & X_BIT) != 0 }
    pub fn cc_n(&self) -> bool { (self.status & N_BIT) != 0 }
    pub fn cc_z(&self) -> bool { (self.status & Z_BIT) != 0 }
    pub fn cc_v(&self) -> bool { (self.status & V_BIT) != 0 }
    pub fn cc_c(&self) -> bool { (self.status & C_BIT) != 0 }

    pub fn supervisor(&self) -> bool { (self.status & SUPERVISOR_BIT) != 0 }
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "68000 status:\n  \
            d0={:08x} d1={:08x} d2={:08x} d3={:08x}  X={}\n  \
            d4={:08x} d5={:08x} d6={:08x} d7={:08x}  N={}\n  \
            a0={:08x} a1={:08x} a2={:08x} a3={:08x}  Z={}\n  \
            a4={:08x} a5={:08x} a6={:08x} a7={:08x}  V={}\n  \
            status={:08b} {:03b}    ssp={:08x} pc={:08x}  C={}",
            self.data[0], self.data[1], self.data[2], self.data[3],
            (self.status >> 4) & 1,
            self.data[4], self.data[5], self.data[6], self.data[7],
            (self.status >> 3) & 1,
            self.addr[0], self.addr[1], self.addr[2], self.addr[3],
            (self.status >> 2) & 1,
            self.addr[4], self.addr[5], self.addr[6], self.addr[7],
            (self.status >> 1) & 1,
            self.status >> 8,
            (self.status >> 5) & 0x7,
            self.ssp, self.pc,
            (self.status >> 0) & 1,
        )
    }
}

/// A trait representing a memory layout
pub trait Memory {
    fn ref16(&self, addr: u32) -> Option<&u16>;
    fn ref16_mut(&mut self, addr: u32) -> Option<&mut u16>;

    fn read8(&self, addr: u32) -> u8 {
        match self.ref16(addr) {
            Some(x) =>
                if (addr & 1) == 0 {
                    (*x >> 8) as u8
                } else {
                    *x as u8
                },

            None => 0
        }
    }

    fn write8(&mut self, addr: u32, data: u8) {
        match self.ref16_mut(addr) {
            Some(x) =>
                if (addr & 1) == 0 {
                    *x = ((data as u16) << 8) | (*x & 0xff);
                } else {
                    *x = (data as u16) | (*x & 0xff00);
                },

            None => { }
        }
    }

    fn read16(&self, addr: u32) -> u16 {
        match self.ref16(addr) {
            Some(x) => *x,
            None    => 0
        }
    }

    fn write16(&mut self, addr: u32, data: u16) {
        match self.ref16_mut(addr) {
            Some(x) => *x = data,
            None    => { }
        }
    }

    fn read32(&self, addr: u32) -> u32 {
        ((self.read16(addr) as u32) << 16) | (self.read16(addr+1) as u32)
    }

    fn write32(&mut self, addr: u32, data: u32) {
        self.write16(addr, (data >> 16) as u16);
        self.write16(addr+1, data as u16);
    }

    fn readsz(&self, addr: u32, sz: Size) -> u32 {
        match sz {
            Size::Byte => self.read8(addr) as u32,
            Size::Word => self.read16(addr) as u32,
            Size::Long => self.read32(addr)
        }
    }

    fn writesz(&mut self, addr: u32, sz: Size, data: u32) {
        match sz {
            Size::Byte => self.write8(addr, data as u8),
            Size::Word => self.write16(addr, data as u16),
            Size::Long => self.write32(addr, data as u32),
        }
    }
}

impl<'a> Memory for &'a mut [u16] {
    fn ref16(&self, addr: u32) -> Option<&u16> {
        let index = (addr >> 1) as usize;
        if index >= self.len() { None } else { Some(&self[index]) }
    }

    fn ref16_mut(&mut self, addr: u32) -> Option<&mut u16> {
        let index = (addr >> 1) as usize;
        if index >= self.len() { None } else { Some(&mut self[index]) }
    }
}

impl<'a> Memory for &'a [u16] {
    fn ref16(&self, addr: u32) -> Option<&u16> {
        let index = (addr >> 1) as usize;
        if index >= self.len() { None } else { Some(&self[index]) }
    }

    fn ref16_mut(&mut self, _addr: u32) -> Option<&mut u16> {
        None
    }
}
