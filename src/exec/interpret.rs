// exec/interpret.rs -- Slow instruction interpreter
// Copyright (C) 2015 Alex Iadicicco

use decode::careful::CarefulDecoder;
use decode::Decoder;
use exec::{CPU, Memory};
use instruction::*;

pub struct Interpreter<M> {
    cpu: CPU,
    mem: M,

    decoder: CarefulDecoder,

    icache_base: u32,
    icache: Vec<u16>
}

impl<M> Interpreter<M> {
    pub fn new(cpu: CPU, mem: M) -> Interpreter<M> {
        Interpreter {
            cpu: cpu,
            mem: mem,

            decoder: CarefulDecoder::new(),

            icache_base: 0,
            icache: Vec::new(),
        }
    }

    pub fn cpu(&self) -> &CPU { &self.cpu }

    pub fn cpu_mut(&mut self) -> &mut CPU { &mut self.cpu }
}

impl<M: Memory> Interpreter<M> {
    fn icache_fetch(&mut self) {
        self.icache_base = self.cpu.pc;

        self.icache.truncate(0);
        self.icache.push(self.mem.read16(self.icache_base +  0));
        self.icache.push(self.mem.read16(self.icache_base +  2));
        self.icache.push(self.mem.read16(self.icache_base +  4));
        self.icache.push(self.mem.read16(self.icache_base +  6));
        self.icache.push(self.mem.read16(self.icache_base +  8));
        self.icache.push(self.mem.read16(self.icache_base + 10));
    }

    fn set_flags(&mut self, x: bool, n: bool, z: bool, v: bool, c: bool) {
        let bits =
            (if x { 0b10000 } else { 0 }) |
            (if n { 0b01000 } else { 0 }) |
            (if z { 0b00100 } else { 0 }) |
            (if v { 0b00010 } else { 0 }) |
            (if c { 0b00001 } else { 0 });
        self.cpu.status = (self.cpu.status & 0xff00) | bits;
    }

    fn execute_once(&mut self) -> bool {
        self.icache_fetch();

        let (inst, len) = match self.decoder.decode(&self.icache[..]) {
            Ok(i) => i,
            Err(_) => return false
        };

        println!("pc={:08x} {:?}", self.cpu.pc, inst);

        let res = match inst {
            ADD_to_Data(sz, ea, dn) => {
                let sh = (sz.size() << 3) - 1;
                let source = ea.value_of(sz, self);
                let dest = self.cpu.data[dn as usize];
                let sm = ((source >> sh) & 1) == 1;
                let dm = ((dest   >> sh) & 1) == 1;
                let result = source.wrapping_add(dest);
                let rm = ((result >> sh) & 1) == 1;
                self.cpu.data[dn as usize] = match sz {
                    Size::Byte => (dest & 0xffffff00) | (result & 0x000000ff),
                    Size::Word => (dest & 0xffff0000) | (result & 0x0000ffff),
                    Size::Long => result
                };
                let overflow = (sm && dm && !rm) || (!sm && !dm && rm);
                let carry = (sm && dm) || (!rm && dm) || (sm && !rm);
                self.set_flags(carry, rm, result == 0, overflow, carry);
                true
            },

            JMP(ea) => {
                self.cpu.pc = ea.addr_of(Size::Word, self);
                false
            },

            MOVEQ(x, dn) => {
                self.cpu.data[dn as usize] = (x as i32) as u32;
                true
            },

            _ => false
        };

        if res { self.cpu.pc += (len << 1) as u32; }

        res
    }

    pub fn execute(&mut self) {
        while self.execute_once() { }
    }
}

trait Evaluable<M> {
    fn value_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32;
    fn addr_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32;
}

impl<M: Memory> Evaluable<M> for EA {
    fn value_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32 {
        use instruction::EA::*;

        let addr = match self {
            DataDirect(dn) => return ee.cpu.data[dn as usize],
            AddrDirect(an) => return ee.cpu.addr[an as usize],
            ImmByte(x) => return x as u32,
            ImmWord(x) => return x as u32,
            ImmLong(x) => return x,

            ea => ea.addr_of(sz, ee)
        };

        match sz {
            Size::Byte => ee.mem.read8(addr) as u32,
            Size::Word => ee.mem.read16(addr) as u32,
            Size::Long => {
                let lo = ee.mem.read16(addr) as u32;
                let hi = ee.mem.read16(addr) as u32;
                lo | (hi << 16)
            },
        }
    }

    fn addr_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32 {
        use instruction::EA::*;

        match self {
            AddrIndirect(an) => ee.cpu.addr[an as usize],
            AddrPostInc(an) => {
                let width = match sz {
                    Size::Byte => if an == 7 { 2 } else { 1 },
                    Size::Word => 2,
                    Size::Long => 4,
                };
                let a = ee.cpu.addr[an as usize];
                ee.cpu.addr[an as usize] =
                    ee.cpu.addr[an as usize].wrapping_add(width as u32);
                a
            },
            AddrPreDec(an) => {
                let width = match sz {
                    Size::Byte => if an == 7 { 2 } else { 1 },
                    Size::Word => 2,
                    Size::Long => 4,
                };
                ee.cpu.addr[an as usize] =
                    ee.cpu.addr[an as usize].wrapping_sub(width as u32);
                ee.cpu.addr[an as usize]
            },
            AddrDisplace(an, disp) => {
                ee.cpu.addr[an as usize].wrapping_add(
                    (disp as i32) as u32
                )
            },
            AddrIndex(an, idx, disp) => {
                let (isz, ireg) = match idx {
                    Index::Data(sz, dn) => (sz, ee.cpu.data[dn as usize]),
                    Index::Addr(sz, an) => (sz, ee.cpu.addr[an as usize]),
                };
                let idisp = match isz {
                    Size::Byte => (ireg as  i8) as i32,
                    Size::Word => (ireg as i16) as i32,
                    Size::Long => (ireg       ) as i32,
                };
                ee.cpu.addr[an as usize].wrapping_add(
                    ((disp as i32) as u32).wrapping_add(idisp as u32)
                )
            },
            AbsWord(at) => (at as i32) as u32,
            AbsLong(at) => at,
            PcDisplace(disp) => {
                ee.cpu.pc.wrapping_add(2u32.wrapping_add(
                    (disp as i32) as u32
                ))
            },
            PcIndex(idx, disp) => {
                let (isz, ireg) = match idx {
                    Index::Data(sz, dn) => (sz, ee.cpu.data[dn as usize]),
                    Index::Addr(sz, an) => (sz, ee.cpu.addr[an as usize]),
                };
                let idisp = match isz {
                    Size::Byte => (ireg as  i8) as i32,
                    Size::Word => (ireg as i16) as i32,
                    Size::Long => (ireg       ) as i32,
                };
                ee.cpu.pc.wrapping_add(2u32.wrapping_add(
                    ((disp as i32) as u32).wrapping_add(idisp as u32)
                ))
            },

            _ => 0
        }
    }
}
