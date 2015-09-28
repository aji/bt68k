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

    fn set_flags_x(&mut self, x: bool, n: bool, z: bool, v: bool, c: bool) {
        let bits =
            (if x { 0b10000 } else { 0 }) |
            (if n { 0b01000 } else { 0 }) |
            (if z { 0b00100 } else { 0 }) |
            (if v { 0b00010 } else { 0 }) |
            (if c { 0b00001 } else { 0 });
        self.cpu.status = (self.cpu.status & 0xffe0) | bits;
    }

    fn set_flag_z(&mut self, z: bool) {
        let bits = if z { 0b00100 } else { 0 };
        self.cpu.status = (self.cpu.status & 0xfffb) | bits;
    }

    fn set_flags(&mut self, n: bool, z: bool, v: bool, c: bool) {
        let bits =
            (if n { 0b01000 } else { 0 }) |
            (if z { 0b00100 } else { 0 }) |
            (if v { 0b00010 } else { 0 }) |
            (if c { 0b00001 } else { 0 });
        self.cpu.status = (self.cpu.status & 0xfff0) | bits;
    }

    #[allow(non_snake_case)]
    pub fn flags_add(&mut self, sz: Size, inS: u32, inD: u32, inR: u32) {
        let sh = sz.size() << 3;
        let mask = {
            ((inS >> sh - 3) & 0b100) |
            ((inD >> sh - 2) & 0b010) |
            ((inR >> sh - 1) & 0b001)
        };
        let (N, V, C) = match mask {
            // Sm Dm Rm
            0b__0__0__0 => (false, false, false,),
            0b__0__0__1 => ( true,  true, false,),
            0b__0__1__0 => (false, false,  true,),
            0b__0__1__1 => ( true, false, false,),
            0b__1__0__0 => (false, false,  true,),
            0b__1__0__1 => ( true, false, false,),
            0b__1__1__0 => (false,  true,  true,),
            0b__1__1__1 => ( true, false,  true,),
                      _ => (false, false, false,),
        };
        self.set_flags_x(C, N, inR == 0, V, C);
    }

    #[allow(non_snake_case)]
    pub fn flags_sub(&mut self, sz: Size, inS: u32, inD: u32, inR: u32) {
        let sh = sz.size() << 3;
        let mask = {
            ((inS >> sh - 3) & 0b100) |
            ((inD >> sh - 2) & 0b010) |
            ((inR >> sh - 1) & 0b001)
        };
        let (N, V, C) = match mask {
            // Sm Dm Rm
            0b__0__0__0 => (false, false, false,),
            0b__0__0__1 => ( true, false,  true,),
            0b__0__1__0 => (false,  true, false,),
            0b__0__1__1 => ( true, false, false,),
            0b__1__0__0 => (false, false,  true,),
            0b__1__0__1 => ( true,  true,  true,),
            0b__1__1__0 => (false, false, false,),
            0b__1__1__1 => ( true, false,  true,),
                      _ => (false, false, false,),
        };
        self.set_flags_x(C, N, inR == 0, V, C);
    }

    #[allow(non_snake_case)]
    pub fn flags_logic(&mut self, sz: Size, inR: u32) {
        let sh = (sz.size() << 3) - 1;
        self.set_flags(((inR >> sh) & 1) != 0, inR == 0, false, false);
    }

    pub fn test_cc(&self, cc: Cond) -> bool {
        use instruction::Cond::*;

        let n = (self.cpu.status & 0b1000) != 0;
        let z = (self.cpu.status & 0b0100) != 0;
        let v = (self.cpu.status & 0b0010) != 0;
        let c = (self.cpu.status & 0b0001) != 0;

        match cc {
            True            => true,
            False           => false,
            High            => !c && !z,
            LowOrSame       => c | z,
            HighOrSame      => !c,
            Low             => c,
            NotEqual        => !z,
            Equal           => z,
            OverflowClear   => !v,
            OverflowSet     => v,
            Plus            => !n,
            Minus           => n,
            GreaterOrEqual  => (n && v) || (!n && !v),
            LessThan        => (n && !v) || (!n && v),
            GreaterThan     => (n && v && !z) || (!n && !v && !z),
            LessOrEqual     => z || (n && !v) || (!n && v),
        }
    }
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

    #[allow(dead_code)]
    fn push16(&mut self, data: u16) {
        let sp = self.cpu.sp();
        *sp = sp.wrapping_sub(2);
        self.mem.write16(*sp, data)
    }

    #[allow(dead_code)]
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
                ea.prepare(sz, self);
                let source = ea.value_of(sz, self);
                let dest = self.cpu.data[dn as usize];
                let result = sz.masked(source.wrapping_add(dest));
                let prev = self.cpu.data[dn as usize] & !sz.mask();
                self.cpu.data[dn as usize] = prev | result;
                self.flags_add(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            ADD_to_EA(sz, dn, ea) => {
                ea.prepare(sz, self);
                let source = self.cpu.data[dn as usize];
                let dest = ea.value_of(sz, self);
                let result = sz.masked(source.wrapping_add(dest));
                ea.write(sz, self, result);
                self.flags_add(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            ADDA(sz, ea, an) => {
                ea.prepare(sz, self);
                let source = ea.value_of(sz, self);
                let dest = self.cpu.addr[an as usize];
                let result = source.wrapping_add(dest);
                self.cpu.addr[an as usize] = result;
                ea.finish(sz, self);
                true
            },

            ADDI(sz, source, ea) => {
                ea.prepare(sz, self);
                let dest = ea.value_of(sz, self);
                let result = sz.masked(source.wrapping_add(dest));
                ea.write(sz, self, result);
                self.flags_add(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            ADDQ(sz, source8, ea) => {
                ea.prepare(sz, self);
                let source = source8 as u32;
                let dest = ea.value_of(sz, self);
                let result = sz.masked(source.wrapping_add(dest));
                ea.write(sz, self, result);
                self.flags_add(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            AND_to_Data(sz, ea, dn) => {
                ea.prepare(sz, self);
                let source = ea.value_of(sz, self);
                let dest = self.cpu.data[dn as usize];
                let result = source & dest;
                let prev = self.cpu.data[dn as usize] & !sz.mask();
                self.cpu.data[dn as usize] = prev | result;
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            AND_to_EA(sz, dn, ea) => {
                ea.prepare(sz, self);
                let source = self.cpu.data[dn as usize];
                let dest = ea.value_of(sz, self);
                let result = source & dest;
                ea.write(sz, self, result);
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            ANDI(sz, source, ea) => {
                ea.prepare(sz, self);
                let dest = ea.value_of(sz, self);
                let result = sz.masked(source & dest);
                ea.write(sz, self, result);
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            Bcc(cc, disp) => {
                if self.test_cc(cc) {
                    self.cpu.pc = self.cpu.pc
                        .wrapping_add(2)
                        .wrapping_add((disp as i32) as u32);
                } else {
                    self.cpu.pc = pcnext;
                }
                false
            },

            BCHG_Data(dn, ea) => {
                if let EA::DataDirect(dx) = ea {
                    let dest = self.cpu.data[dx as usize];
                    let bit = 1 << (self.cpu.data[dn as usize] & 0x1f);
                    self.cpu.data[dx as usize] = dest ^ bit;
                    self.set_flag_z((dest & bit) == 0);
                } else {
                    ea.prepare(Size::Byte, self);
                    let dest = ea.value_of(Size::Byte, self);
                    let bit = 1 << (self.cpu.data[dn as usize] & 0x7);
                    ea.write(Size::Byte, self, dest ^ bit);
                    self.set_flag_z((dest & bit) == 0);
                    ea.finish(Size::Byte, self);
                }
                true
            },

            BCHG_Imm(bitnr, ea) => {
                if let EA::DataDirect(dx) = ea {
                    let dest = self.cpu.data[dx as usize];
                    let bit = 1 << ((bitnr as u32) & 0x1f);
                    self.cpu.data[dx as usize] = dest ^ bit;
                    self.set_flag_z((dest & bit) == 0);
                } else {
                    ea.prepare(Size::Byte, self);
                    let dest = ea.value_of(Size::Byte, self);
                    let bit = 1 << ((bitnr as u32) & 0x7);
                    ea.write(Size::Byte, self, dest ^ bit);
                    self.set_flag_z((dest & bit) == 0);
                    ea.finish(Size::Byte, self);
                }
                true
            },

            BCLR_Data(dn, ea) => {
                if let EA::DataDirect(dx) = ea {
                    let dest = self.cpu.data[dx as usize];
                    let bit = 1 << (self.cpu.data[dn as usize] & 0x1f);
                    self.cpu.data[dx as usize] = dest & !bit;
                    self.set_flag_z((dest & bit) == 0);
                } else {
                    ea.prepare(Size::Byte, self);
                    let dest = ea.value_of(Size::Byte, self);
                    let bit = 1 << (self.cpu.data[dn as usize] & 0x7);
                    ea.write(Size::Byte, self, dest & !bit);
                    self.set_flag_z((dest & bit) == 0);
                    ea.finish(Size::Byte, self);
                }
                true
            },

            BCLR_Imm(bitnr, ea) => {
                if let EA::DataDirect(dx) = ea {
                    let dest = self.cpu.data[dx as usize];
                    let bit = 1 << ((bitnr as u32) & 0x1f);
                    self.cpu.data[dx as usize] = dest & !bit;
                    self.set_flag_z((dest & bit) == 0);
                } else {
                    ea.prepare(Size::Byte, self);
                    let dest = ea.value_of(Size::Byte, self);
                    let bit = 1 << ((bitnr as u32) & 0x7);
                    ea.write(Size::Byte, self, dest & !bit);
                    self.set_flag_z((dest & bit) == 0);
                    ea.finish(Size::Byte, self);
                }
                true
            },

            BRA(disp) => {
                let offs = 2u32.wrapping_add((disp as i32) as u32);
                self.cpu.pc = self.cpu.pc.wrapping_add(2).wrapping_add(offs);
                false
            },

            BSET_Data(dn, ea) => {
                if let EA::DataDirect(dx) = ea {
                    let dest = self.cpu.data[dx as usize];
                    let bit = 1 << (self.cpu.data[dn as usize] & 0x1f);
                    self.cpu.data[dx as usize] = dest | bit;
                    self.set_flag_z((dest & bit) == 0);
                } else {
                    ea.prepare(Size::Byte, self);
                    let dest = ea.value_of(Size::Byte, self);
                    let bit = 1 << (self.cpu.data[dn as usize] & 0x7);
                    ea.write(Size::Byte, self, dest | bit);
                    self.set_flag_z((dest & bit) == 0);
                    ea.finish(Size::Byte, self);
                }
                true
            },

            BSET_Imm(bitnr, ea) => {
                if let EA::DataDirect(dx) = ea {
                    let dest = self.cpu.data[dx as usize];
                    let bit = 1 << ((bitnr as u32) & 0x1f);
                    self.cpu.data[dx as usize] = dest | bit;
                    self.set_flag_z((dest & bit) == 0);
                } else {
                    ea.prepare(Size::Byte, self);
                    let dest = ea.value_of(Size::Byte, self);
                    let bit = 1 << ((bitnr as u32) & 0x7);
                    ea.write(Size::Byte, self, dest | bit);
                    self.set_flag_z((dest & bit) == 0);
                    ea.finish(Size::Byte, self);
                }
                true
            },

            BTST_Data(dn, ea) => {
                if let EA::DataDirect(dx) = ea {
                    let dest = self.cpu.data[dx as usize];
                    let bit = 1 << (self.cpu.data[dn as usize] & 0x1f);
                    self.set_flag_z((dest & bit) == 0);
                } else {
                    ea.prepare(Size::Byte, self);
                    let dest = ea.value_of(Size::Byte, self);
                    let bit = 1 << (self.cpu.data[dn as usize] & 0x7);
                    self.set_flag_z((dest & bit) == 0);
                    ea.finish(Size::Byte, self);
                }
                true
            },

            BTST_Imm(bitnr, ea) => {
                if let EA::DataDirect(dx) = ea {
                    let dest = self.cpu.data[dx as usize];
                    let bit = 1 << ((bitnr as u32) & 0x1f);
                    self.set_flag_z((dest & bit) == 0);
                } else {
                    ea.prepare(Size::Byte, self);
                    let dest = ea.value_of(Size::Byte, self);
                    let bit = 1 << ((bitnr as u32) & 0x7);
                    self.set_flag_z((dest & bit) == 0);
                    ea.finish(Size::Byte, self);
                }
                true
            },

            BSR(disp) => {
                let offs = 2u32.wrapping_add((disp as i32) as u32);
                self.push32(pcnext);
                self.cpu.pc = self.cpu.pc
                    .wrapping_add(2)
                    .wrapping_add(offs);
                false
            },

            CLR(sz, ea) => {
                ea.prepare(sz, self);
                ea.write(sz, self, 0);
                self.set_flags(false, true, false, false);
                ea.finish(sz, self);
                true
            },

            CMP(sz, ea, dn) => {
                ea.prepare(sz, self);
                let source = ea.value_of(sz, self);
                let dest = self.cpu.data[dn as usize];
                let result = sz.masked(dest.wrapping_sub(source));
                self.flags_sub(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            CMPA(sz, ea, an) => {
                ea.prepare(sz, self);
                let source = ea.value_of(sz, self);
                let dest = self.cpu.addr[an as usize];
                let result = dest.wrapping_sub(source);
                self.flags_sub(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            CMPI(sz, source, ea) => {
                let dest = ea.value_of(sz, self);
                let result = sz.masked(dest.wrapping_sub(source));
                self.flags_sub(sz, source, dest, result);
                true
            },

            DBcc(cc, dn, disp) => {
                if self.test_cc(cc) {
                    let res = (self.cpu.data[dn as usize] as u16)
                        .wrapping_sub(1);
                    EA::DataDirect(dn).write(Size::Word, self, res as u32);
                    if res != 0xffff { // 0xffff is -1
                        self.cpu.pc = self.cpu.pc
                            .wrapping_add(2)
                            .wrapping_add((disp as i32) as u32);
                    } else {
                        self.cpu.pc = pcnext;
                    }
                } else {
                    self.cpu.pc = pcnext;
                }
                false
            },

            EOR(sz, dn, ea) => {
                ea.prepare(sz, self);
                let source = self.cpu.data[dn as usize];
                let dest = ea.value_of(sz, self);
                let result = source ^ dest;
                ea.write(sz, self, result);
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            EORI(sz, source, ea) => {
                ea.prepare(sz, self);
                let dest = ea.value_of(sz, self);
                let result = sz.masked(source ^ dest);
                ea.write(sz, self, result);
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            EXG_Data(dx, dy) => {
                let dx = dx as usize;
                let dy = dy as usize;
                let t = self.cpu.data[dx];
                self.cpu.data[dx] = self.cpu.data[dy];
                self.cpu.data[dy] = t;
                true
            },

            EXG_Addr(ax, ay) => {
                let ax = ax as usize;
                let ay = ay as usize;
                let t = self.cpu.addr[ax];
                self.cpu.addr[ax] = self.cpu.addr[ay];
                self.cpu.addr[ay] = t;
                true
            },

            EXG_Both(dx, ay) => {
                let dx = dx as usize;
                let ay = ay as usize;
                let t = self.cpu.data[dx];
                self.cpu.data[dx] = self.cpu.addr[ay];
                self.cpu.addr[ay] = t;
                true
            },

            EXT(sz, dn) => {
                let x = self.cpu.data[dn as usize];
                let result = match sz {
                    Size::Word => ((x as  i8) as i16) as u32,
                    Size::Long => ((x as i16) as i32) as u32,

                    Size::Byte => {
                        self.trap(exec::EXC_ILLEGAL);
                        return false;
                    },
                };
                EA::DataDirect(dn).write(sz, self, result);
                true
            },

            ILLEGAL => {
                self.trap(exec::EXC_ILLEGAL);
                false
            },

            JMP(ea) => {
                ea.prepare(Size::Word, self);
                self.cpu.pc = ea.addr_of(self);
                ea.finish(Size::Word, self);
                false
            },

            JSR(ea) => {
                ea.prepare(Size::Word, self);
                self.cpu.pc = ea.addr_of(self);
                self.push32(pcnext);
                ea.finish(Size::Word, self);
                false
            },

            LEA(ea, an) => {
                ea.prepare(Size::Word, self);
                let data = ea.addr_of(self);
                EA::AddrDirect(an).write(Size::Word, self, data);
                ea.finish(Size::Word, self);
                true
            },

            LINK(an, disp) => {
                let sp = EA::AddrDirect(7);
                let addr = self.cpu.addr[an as usize];
                self.push32(addr);
                self.cpu.addr[an as usize] = sp.value_of(Size::Long, self);
                let newsp = self.cpu.addr[an as usize]
                    .wrapping_add((disp as i32) as u32);
                sp.write(Size::Long, self, newsp);
                true
            },

            MOVE(sz, src, dst) => {
                src.prepare(sz, self);
                dst.prepare(sz, self);
                let result = sz.masked(src.value_of(sz, self));
                dst.write(sz, self, result);
                self.flags_logic(sz, result);
                src.finish(sz, self);
                dst.finish(sz, self);
                true
            },

            MOVE_to_CCR(ea) => {
                ea.prepare(Size::Byte, self);
                let data = ea.value_of(Size::Byte, self) as u16;
                self.cpu.status &= 0xff00;
                self.cpu.status |= data & 0x1f;
                ea.finish(Size::Byte, self);
                true
            },

            MOVE_to_SR(ea) => {
                if self.cpu.supervisor() {
                    ea.prepare(Size::Word, self);
                    self.cpu.status = ea.value_of(Size::Word, self) as u16;
                    ea.finish(Size::Word, self);
                    true
                } else {
                    self.trap(exec::EXC_PRIV);
                    false
                }
            },

            MOVEA(sz, ea, an) => {
                ea.prepare(sz, self);
                let result = sz.masked(ea.value_of(sz, self));
                EA::AddrDirect(an).write(Size::Word, self, result);
                ea.finish(sz, self);
                true
            }

            MOVEQ(x, dn) => {
                let result = (x as i32) as u32;
                self.cpu.data[dn as usize] = result;
                self.flags_logic(Size::Long, result);
                true
            },

            NEG(sz, ea) => {
                ea.prepare(sz, self);
                let result = (0u32).wrapping_sub(ea.value_of(sz, self));
                ea.write(sz, self, result);
                ea.finish(sz, self);
                true
            },

            NEGX(sz, ea) => {
                ea.prepare(sz, self);
                let result = (0u32)
                    .wrapping_sub(ea.value_of(sz, self))
                    .wrapping_sub(if self.cpu.cc_x() { 1 } else { 0 });
                ea.write(sz, self, result);
                ea.finish(sz, self);
                true
            },

            NOP => true,

            OR_to_Data(sz, ea, dn) => {
                ea.prepare(sz, self);
                let source = ea.value_of(sz, self);
                let dest = self.cpu.data[dn as usize];
                let result = source | dest;
                let prev = self.cpu.data[dn as usize] & !sz.mask();
                self.cpu.data[dn as usize] = prev | result;
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            OR_to_EA(sz, dn, ea) => {
                ea.prepare(sz, self);
                let source = self.cpu.data[dn as usize];
                let dest = ea.value_of(sz, self);
                let result = source | dest;
                ea.write(sz, self, result);
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            ORI(sz, source, ea) => {
                ea.prepare(sz, self);
                let dest = ea.value_of(sz, self);
                let result = sz.masked(source | dest);
                ea.write(sz, self, result);
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            PEA(ea) => {
                ea.prepare(Size::Long, self);
                let data = ea.addr_of(self);
                self.push32(data);
                ea.finish(Size::Long, self);
                true
            },

            RESET => {
                if self.cpu.supervisor() {
                    self.reset();
                } else {
                    self.trap(exec::EXC_PRIV);
                }
                false
            },

            RTE => {
                if self.cpu.supervisor() {
                    self.cpu.status = self.pop16();
                    self.cpu.pc = self.pop32();
                } else {
                    self.trap(exec::EXC_PRIV);
                }
                false
            },

            RTS => {
                self.cpu.pc = self.pop32();
                false
            },

            Scc(cc, ea) => {
                ea.prepare(Size::Byte, self);
                if self.test_cc(cc) {
                    ea.write(Size::Byte, self, 0xff);
                } else {
                    ea.write(Size::Byte, self, 0x00);
                }
                ea.finish(Size::Byte, self);
                true
            },

            SUB_to_Data(sz, ea, dn) => {
                ea.prepare(sz, self);
                let source = ea.value_of(sz, self);
                let dest = self.cpu.data[dn as usize];
                let result = sz.masked(dest.wrapping_sub(source));
                let prev = self.cpu.data[dn as usize] & !sz.mask();
                self.cpu.data[dn as usize] = prev | result;
                self.flags_sub(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            SUB_to_EA(sz, dn, ea) => {
                ea.prepare(sz, self);
                let source = self.cpu.data[dn as usize];
                let dest = ea.value_of(sz, self);
                let result = sz.masked(dest.wrapping_sub(source));
                ea.write(sz, self, result);
                self.flags_sub(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            SUBA(sz, ea, an) => {
                ea.prepare(sz, self);
                let source = ea.value_of(sz, self);
                let dest = self.cpu.addr[an as usize];
                let result = dest.wrapping_sub(source);
                self.cpu.addr[an as usize] = result;
                ea.finish(sz, self);
                true
            },

            SUBI(sz, source, ea) => {
                ea.prepare(sz, self);
                let dest = ea.value_of(sz, self);
                let result = sz.masked(dest.wrapping_sub(source));
                ea.write(sz, self, result);
                self.flags_sub(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            SUBQ(sz, source8, ea) => {
                ea.prepare(sz, self);
                let source = source8 as u32;
                let dest = ea.value_of(sz, self);
                let result = sz.masked(dest.wrapping_sub(source));
                ea.write(sz, self, result);
                self.flags_sub(sz, source, dest, result);
                ea.finish(sz, self);
                true
            },

            SWAP(dn) => {
                let dn = dn as usize;
                let hi = (self.cpu.data[dn] >> 16) & 0x0000ffff;
                let lo = (self.cpu.data[dn] << 16) & 0xffff0000;
                let result = hi | lo;
                self.cpu.data[dn] = result;
                self.flags_logic(Size::Long, result);
                true
            },

            TAS(ea) => {
                ea.prepare(Size::Byte, self);
                let mut result = ea.value_of(Size::Byte, self);
                self.flags_logic(Size::Byte, result);
                result |= 0x80;
                ea.write(Size::Byte, self, result);
                ea.finish(Size::Byte, self);
                true
            },

            TRAP(vec) => {
                self.trap(exec::EXC_TRAP_BASE + (vec as u32));
                false
            },

            TRAPV => {
                if self.cpu.cc_v() {
                    self.trap(exec::EXC_TRAPV);
                }
                false
            },

            TST(sz, ea) => {
                ea.prepare(sz, self);
                let result = ea.value_of(sz, self);
                self.flags_logic(sz, result);
                ea.finish(sz, self);
                true
            },

            UNLK(an) => {
                let tmp = self.cpu.addr[an as usize];
                EA::AddrDirect(7).write(Size::Long, self, tmp);
                self.cpu.addr[an as usize] = self.pop32();
                true
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
    fn prepare(self, sz: Size, ee: &mut Interpreter<M>);
    fn finish(self, sz: Size, ee: &mut Interpreter<M>);
    fn value_of(self, sz: Size, ee: &mut Interpreter<M>) -> u32;
    fn write(self, sz: Size, ee: &mut Interpreter<M>, data: u32);
    fn addr_of(self, ee: &mut Interpreter<M>) -> u32;
}

impl<M: Memory> Evaluable<M> for EA {
    fn prepare(self, sz: Size, ee: &mut Interpreter<M>) {
        if let EA::AddrPreDec(an) = self {
            if an == 7 {
                let sp = ee.cpu.sp();
                *sp = sp.wrapping_sub(match sz {
                    Size::Byte | Size::Word => 2,
                    Size::Long => 4
                });
            } else {
                ee.cpu.addr[an as usize] = ee.cpu.addr[an as usize]
                    .wrapping_sub(sz.size() as u32);
            }
        }
    }

    fn finish(self, sz: Size, ee: &mut Interpreter<M>) {
        if let EA::AddrPostInc(an) = self {
            if an == 7 {
                let sp = ee.cpu.sp();
                *sp = sp.wrapping_add(match sz {
                    Size::Byte | Size::Word => 2,
                    Size::Long => 4
                });
            } else {
                ee.cpu.addr[an as usize] = ee.cpu.addr[an as usize]
                    .wrapping_add(sz.size() as u32);
            }
        }
    }

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

            ea => ea.addr_of(ee)
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

            ea => ea.addr_of(ee)
        };

        ee.mem.writesz(addr, sz, data);
    }

    fn addr_of(self, ee: &mut Interpreter<M>) -> u32 {
        use instruction::EA::*;

        match self {
            AddrIndirect(an) |
            AddrPreDec(an) |
            AddrPostInc(an) => {
                if an == 7 && ee.cpu.supervisor() {
                    ee.cpu.ssp
                } else {
                    ee.cpu.addr[an as usize]
                }
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
