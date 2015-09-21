// decode/common.rs -- Common decoding operations
// Copyright (C) 2015 Alex Iadicicco

use instruction::*;

#[inline]
pub fn bit(pc: &[u16], n: u16) -> u8 { ((pc[0] >> n) & 0b1) as u8 }

#[inline]
pub fn pair(pc: &[u16], n: u16) -> u8 { ((pc[0] >> n) & 0b11) as u8 }

#[inline]
pub fn triple(pc: &[u16], n: u16) -> u8 { ((pc[0] >> n) & 0b111) as u8 }

#[inline]
pub fn immediate(pc: &[u16], sz: Size, at: &mut usize) -> u32 {
    match sz {
        Size::Byte => { *at += 1; (pc[*at - 1] & 0xff) as u32 },
        Size::Word => { *at += 1; pc[*at - 1] as u32 },
        Size::Long => {
            *at += 2;
            ((pc[*at - 2] as u32) << 16) | (pc[*at - 1] as u32)
        }
    }
}

#[inline]
pub fn decode_indexed(pc: &[u16], addr: Option<u8>) -> EA {
    let sz = if bit(pc, 11) == 0 { Size::Word } else { Size::Long};
    let idx = if bit(pc, 15) == 0 {
        Index::Data(sz, triple(pc, 12))
    } else {
        Index::Addr(sz, triple(pc, 12))
    };
    match addr {
        Some(reg) => EA::AddrIndex(reg, idx, (pc[0] & 0xff) as i8),
        None      => EA::PcIndex(       idx, (pc[0] & 0xff) as i8),
    }
}

#[inline]
pub fn decode_ea_real(pc: &[u16], mode: u8, reg: u8, sz: Size, at: &mut usize)
-> Option<EA> {
    let ext = &pc[*at..];
    match mode {
        0b000 => Some(EA::DataDirect(reg)),
        0b001 => Some(EA::AddrDirect(reg)),
        0b010 => Some(EA::AddrIndirect(reg)),
        0b011 => Some(EA::AddrPostInc(reg)),
        0b100 => Some(EA::AddrPreDec(reg)),
        0b101 => { *at += 1; Some(EA::AddrDisplace(reg, ext[0] as i16)) },
        0b110 => { *at += 1; Some(decode_indexed(ext, Some(reg))) },
        0b111 => match reg {
            0b000 => { *at += 1; Some(EA::AbsWord(ext[0] as i16)) },
            0b001 => { Some(EA::AbsLong(immediate(pc, Size::Long, at))) },
            0b010 => { *at += 1; Some(EA::PcDisplace(ext[0] as i16)) },
            0b011 => { *at += 1; Some(decode_indexed(ext, None)) },
            0b100 => match sz {
                Size::Byte => { *at += 1; Some(EA::ImmByte(ext[0] as u8)) },
                Size::Word => { *at += 1; Some(EA::ImmWord(ext[0])) },
                Size::Long => { Some(EA::ImmLong(immediate(pc, Size::Long, at))) },
            },
            _ => None,
        },
        _ => None,
    }
}

#[inline]
pub fn decode_ea(pc: &[u16], n: u16, sz: Size, len: &mut usize) -> Option<EA> {
    decode_ea_real(pc, triple(pc, n+3), triple(pc, n), sz, len)
}

#[inline]
pub fn decode_ae(pc: &[u16], n: u16, sz: Size, len: &mut usize) -> Option<EA> {
    decode_ea_real(pc, triple(pc, n), triple(pc, n+3), sz, len)
}

#[inline]
pub fn decode_size(pc: &[u16], n: u16) -> Option<Size> {
    match pair(pc, n) {
        0b00 => Some(Size::Byte),
        0b01 => Some(Size::Word),
        0b10 => Some(Size::Long),
        _ => None
    }
}

#[inline]
pub fn decode_cond(pc: &[u16], n: u16) -> Option<Cond> {
    let cc = (pc[0] >> n) & 0xf;
    match cc {
        0b0000 => Some(Cond::True),
        0b0001 => Some(Cond::False),
        0b0010 => Some(Cond::High),
        0b0011 => Some(Cond::LowOrSame),
        0b0100 => Some(Cond::HighOrSame),
        0b0101 => Some(Cond::Low),
        0b0110 => Some(Cond::NotEqual),
        0b0111 => Some(Cond::Equal),
        0b1000 => Some(Cond::OverflowClear),
        0b1001 => Some(Cond::OverflowSet),
        0b1010 => Some(Cond::Plus),
        0b1011 => Some(Cond::Minus),
        0b1100 => Some(Cond::GreaterOrEqual),
        0b1101 => Some(Cond::LessThan),
        0b1110 => Some(Cond::GreaterThan),
        0b1111 => Some(Cond::LessOrEqual),
        _ => None
    }
}
