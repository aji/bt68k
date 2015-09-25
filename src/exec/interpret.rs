// exec/interpret.rs -- Slow instruction interpreter
// Copyright (C) 2015 Alex Iadicicco

use decode::prefix::PrefixDecoder;
use decode::Decoder;
use exec;
use exec::{CPU, Memory};
use instruction::*;

pub struct Interpreter<M> {
    cpu: CPU,
    mem: M,

    decoder: PrefixDecoder,

    icache_base: u32,
    icache: Vec<u16>,

    debug: bool,
}

impl<M> Interpreter<M> {
    pub fn new(cpu: CPU, mem: M, debug: bool) -> Interpreter<M> {
        Interpreter {
            cpu: cpu,
            mem: mem,

            decoder: PrefixDecoder::new(),

            icache_base: 0,
            icache: Vec::new(),

            debug: debug,
        }
    }

    pub fn cpu(&self) -> &CPU { &self.cpu }

    pub fn cpu_mut(&mut self) -> &mut CPU { &mut self.cpu }

    pub fn mem(&self) -> &M { &self.mem }

    pub fn mem_mut(&mut self) -> &mut M { &mut self.mem }
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

    pub fn reset(&mut self) {
        self.cpu.pc   = self.mem.read32(exec::EXC_RESET_PC);
        self.cpu.ssp  = self.mem.read32(exec::EXC_RESET_SSP);
        self.cpu.status |= exec::SUPERVISOR_BIT;
    }

    pub fn interrupt(&mut self, lev: u32) {
        self.trap(lev + exec::EXC_INT_BASE);
    }

    fn trap(&mut self, exc: u32) {
        let vec = exc << 2;
        self.mem.write16(self.cpu.ssp,    self.cpu.status);
        self.mem.write32(self.cpu.ssp+2,  self.cpu.pc);
        self.cpu.pc = self.mem.read32(vec);
        self.cpu.status |= exec::SUPERVISOR_BIT;
    }

    fn push32(&mut self, data: u32) {
        let sp = self.cpu.sp();
        *sp = sp.wrapping_sub(4);
        self.mem.write32(*sp, data);
    }

    fn pop32(&mut self) -> u32 {
        let sp = self.cpu.sp();
        *sp = sp.wrapping_add(4);
        self.mem.read32(sp.wrapping_sub(4))
    }

    fn push16(&mut self, data: u16) {
        let sp = self.cpu.sp();
        *sp = sp.wrapping_sub(2);
        self.mem.write16(*sp, data)
    }

    fn pop16(&mut self) -> u16 {
        let sp = self.cpu.sp();
        *sp = sp.wrapping_add(2);
        self.mem.read16(sp.wrapping_sub(2))
    }

    fn execute_once(&mut self) -> bool {
        self.icache_fetch();

        let (inst, len) = match self.decoder.decode(&self.icache[..]) {
            Ok(i) => i,
            Err(_) => return false
        };

        if self.debug {
            println!("pc={:08x} {:?}", self.cpu.pc, inst);
        }

        let pcnext = self.cpu.pc + ((len << 1) as u32);

        let res = match inst {
            ADD_to_Data(sz, ea, dn) => {
                let sh = (sz.size() << 3) - 1;
                let source = ea.value_of(sz, self);
                let dest = self.cpu.data[dn as usize];
                let sm = ((source >> sh) & 1) == 1;
                let dm = ((dest   >> sh) & 1) == 1;
                let result = sz.masked(source.wrapping_add(dest));
                let rm = ((result >> sh) & 1) == 1;
                let prev = self.cpu.data[dn as usize] & !sz.mask();
                self.cpu.data[dn as usize] = prev | result;
                let overflow = (sm && dm && !rm) || (!sm && !dm && rm);
                let carry = (sm && dm) || (!rm && dm) || (sm && !rm);
                self.set_flags(carry, rm, result == 0, overflow, carry);
                true
            },

            ADD_to_EA(sz, dn, ea) => {
                let sh = (sz.size() << 3) - 1;
                let source = self.cpu.data[dn as usize];
                let destat = ea.addr_of(sz, self);
                let dest = self.mem.readsz(destat, sz);
                let sm = ((source >> sh) & 1) == 1;
                let dm = ((dest   >> sh) & 1) == 1;
                let result = sz.masked(source.wrapping_add(dest));
                let rm = ((result >> sh) & 1) == 1;
                self.mem.writesz(destat, sz, result);
                let overflow = (sm && dm && !rm) || (!sm && !dm && rm);
                let carry = (sm && dm) || (!rm && dm) || (sm && !rm);
                self.set_flags(carry, rm, result == 0, overflow, carry);
                true
            },

            ADDA(sz, ea, an) => {
                let source = ea.value_of(sz, self);
                let dest = self.cpu.addr[an as usize];
                let result = source.wrapping_add(dest);
                self.cpu.addr[an as usize] = result;
                true
            },

            ADDI(sz, source, ea) => {
                let sh = (sz.size() << 3) - 1;
                let dest = ea.value_of(sz, self);
                let sm = ((source >> sh) & 1) == 1;
                let dm = ((dest   >> sh) & 1) == 1;
                let result = sz.masked(source.wrapping_add(dest));
                let rm = ((result >> sh) & 1) == 1;
                ea.write(sz, self, result);
                let overflow = (sm && dm && !rm) || (!sm && !dm && rm);
                let carry = (sm && dm) || (!rm && dm) || (sm && !rm);
                self.set_flags(carry, rm, result == 0, overflow, carry);
                true
            },

            ADDQ(sz, source8, ea) => {
                let sh = (sz.size() << 3) - 1;
                let source = source8 as u32;
                let dest = ea.value_of(sz, self);
                let sm = ((source >> sh) & 1) == 1;
                let dm = ((dest   >> sh) & 1) == 1;
                let result = sz.masked(source.wrapping_add(dest));
                let rm = ((result >> sh) & 1) == 1;
                ea.write(sz, self, result);
                let overflow = (sm && dm && !rm) || (!sm && !dm && rm);
                let carry = (sm && dm) || (!rm && dm) || (sm && !rm);
                self.set_flags(carry, rm, result == 0, overflow, carry);
                true
            },

            BRA(disp) => {
                let offs = 2u32.wrapping_add((disp as i32) as u32);
                self.cpu.pc = self.cpu.pc.wrapping_add(offs);
                false
            },

            BSR(disp) => {
                let offs = 2u32.wrapping_add((disp as i32) as u32);
                self.push32(pcnext);
                self.cpu.pc = self.cpu.pc.wrapping_add(offs);
                false
            },

            CLR(sz, ea) => {
                ea.write(sz, self, 0);
                let x = self.cpu.cc_x();
                self.set_flags(x, false, true, false, false);
                true
            },

            JMP(ea) => {
                self.cpu.pc = ea.addr_of(Size::Word, self);
                false
            },

            JSR(ea) => {
                self.cpu.pc = ea.addr_of(Size::Word, self);
                self.push32(pcnext);
                false
            },

            LEA(ea, an) => {
                let data = ea.addr_of(Size::Word, self);
                EA::AddrDirect(an).write(Size::Word, self, data);
                true
            },

            MOVE(sz, src, dst) => {
                let result = sz.masked(src.value_of(sz, self));
                dst.write(sz, self, result);
                let sh = (sz.size() << 3) - 1;
                let rm = ((result >> sh) & 1) == 1;
                let x = self.cpu.cc_x();
                self.set_flags(x, rm, result == 0, false, false);
                true
            },

            MOVE_to_CCR(ea) => {
                let data = ea.value_of(Size::Byte, self) as u16;
                self.cpu.status &= 0xff00;
                self.cpu.status |= data & 0x1f;
                true
            },

            MOVE_to_SR(ea) => {
                if self.cpu.supervisor() {
                    self.cpu.status = ea.value_of(Size::Word, self) as u16;
                    true
                } else {
                    self.trap(exec::EXC_PRIV);
                    false
                }
            },

            MOVEA(sz, ea, an) => {
                let result = sz.masked(ea.value_of(sz, self));
                EA::AddrDirect(an).write(Size::Word, self, result);
                true
            }

            MOVEQ(x, dn) => {
                self.cpu.data[dn as usize] = (x as i32) as u32;
                true
            },

            NOP => true,

            PEA(ea) => {
                let data = ea.addr_of(Size::Long, self);
                self.push32(data);
                true
            },

            RTS => {
                self.cpu.pc = self.pop32();
                false
            },

            _ => false
        };

        if res { self.cpu.pc = pcnext; }

        res
    }

    pub fn execute(&mut self) {
        while self.execute_once() { }
    }
}

trait Evaluable<M> {
    fn value_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32;
    fn write(self, sz: Size, ee: &mut Interpreter<M>, data: u32);
    fn addr_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32;
}

impl<M: Memory> Evaluable<M> for EA {
    fn value_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32 {
        use instruction::EA::*;

        let addr = match self {
            DataDirect(dn) => return ee.cpu.data[dn as usize],
            AddrDirect(an) => return {
                if an == 7 && ee.cpu.supervisor() {
                    ee.cpu.ssp
                } else {
                    ee.cpu.addr[an as usize]
                }
            },
            ImmByte(x) => return x as u32,
            ImmWord(x) => return x as u32,
            ImmLong(x) => return x,

            ea => ea.addr_of(sz, ee)
        };

        ee.mem.readsz(addr, sz)
    }

    fn write(self, sz: Size, ee: &mut Interpreter<M>, data: u32) {
        use instruction::EA::*;

        let addr = match self {
            DataDirect(dn) => {
                let prev = ee.cpu.data[dn as usize] & !sz.mask();
                ee.cpu.data[dn as usize] = prev | sz.masked(data);
                return;
            },
            AddrDirect(an) => {
                if an == 7 && ee.cpu.supervisor() {
                    ee.cpu.ssp = data;
                } else {
                    ee.cpu.addr[an as usize] = data;
                }
                return;
            },
            ImmByte(_) => { return; },
            ImmWord(_) => { return; },
            ImmLong(_) => { return; },

            ea => ea.addr_of(sz, ee)
        };

        ee.mem.writesz(addr, sz, data);
    }

    fn addr_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32 {
        use instruction::EA::*;

        match self {
            AddrIndirect(an) => {
                if an == 7 && ee.cpu.supervisor() {
                    ee.cpu.ssp
                } else {
                    ee.cpu.addr[an as usize]
                }
            },
            AddrPostInc(an) => {
                if an == 7 {
                    let sp = ee.cpu.sp();
                    let a = *sp;
                    *sp = sp.wrapping_add(match sz {
                        Size::Byte | Size::Word => 2,
                        Size::Long => 4
                    });
                    return a;
                }
                let a = ee.cpu.addr[an as usize];
                ee.cpu.addr[an as usize] =
                    ee.cpu.addr[an as usize].wrapping_add(sz.size() as u32);
                a
            },
            AddrPreDec(an) => {
                if an == 7 {
                    let sp = ee.cpu.sp();
                    *sp = sp.wrapping_sub(match sz {
                        Size::Byte | Size::Word => 2,
                        Size::Long => 4
                    });
                    return *sp;
                }
                ee.cpu.addr[an as usize] =
                    ee.cpu.addr[an as usize].wrapping_sub(sz.size() as u32);
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
