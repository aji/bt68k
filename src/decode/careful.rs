// decode/careful.rs -- Careful 68000 instruction decoder
// Copyright (C) 2015 Alex Iadicicco

/// A careful but slow decoder. Follows the spec as closely as possible.
/// Alternate decoders are checked against this decoder for correctness, but
/// otherwise its use is limited. It can also be used for generating instruction
/// decoder tables.
///
/// The architecture is naive but straightforward and correct. Each valid
/// instruction encoding is described in a strict pattern micro-language. This
/// pattern is taken verbatim from the 68000 programmer's reference manual.
/// Patterns are tried in turn against the encoded instruction word until a
/// match is found. Patterns are written to be mutually exclusive so that no
/// more than one is matched for a given instruction word.

pub use decode::{Decoder, DecodeResult};
use instruction::*;

/// A value signaling the reason for a decode failure in a sub-decoder.
/// Mismatches should be resolved by trying a later decoder, while Internal
/// errors should never occur during normal use (even for invalid instructions)
/// and indicate a programming error in the module itself.
enum MatchError {
    Internal,
    Mismatch,
}

// EA type bit masks. These are the categories of effective address described by
// the PRM. Certain instructions are only valid for some kinds of EA. Other
// instructions are encoded in ways that map to invalid EA usage in an
// instruction, so we have to inspect the EA field before selecting a decoder.
const ANY_EA: u16     = 0b111111111111;
const DATA: u16       = 0b111111111101;
const MEMORY: u16     = 0b111111111100;
const ALTERABLE: u16  = 0b000111111111;
const CONTROL: u16    = 0b011111100100;

// EA type bit flags
const DATA_DIRECT: u16     = 0b000000000001;
const ADDR_DIRECT: u16     = 0b000000000010;
const ADDR_INDIRECT: u16   = 0b000000000100;
const ADDR_POSTINC: u16    = 0b000000001000;
const ADDR_PREDEC: u16     = 0b000000010000;
const ADDR_DISPLACE: u16   = 0b000000100000;
const ADDR_INDEX: u16      = 0b000001000000;
const ABS_SHORT: u16       = 0b000010000000;
const ABS_LONG: u16        = 0b000100000000;
const PC_DISPLACE: u16     = 0b001000000000;
const PC_INDEX: u16        = 0b010000000000;
const IMMEDIATE: u16       = 0b100000000000;

// Cond type bit masks, same situation as with EAs
const ANY_COND: u16             = 0b_1111_1111_1111_1111;
const TRUE_COND: u16            = 0b_1111_1111_1111_1100;

// Cond type bit flags
const CC_TRUE: u16              = 0b_0000_0000_0000_0001;
const CC_FALSE: u16             = 0b_0000_0000_0000_0010;
const CC_HIGH: u16              = 0b_0000_0000_0000_0100;
const CC_LOW_OR_SAME: u16       = 0b_0000_0000_0000_1000;
const CC_HIGH_OR_SAME: u16      = 0b_0000_0000_0001_0000;
const CC_LOW: u16               = 0b_0000_0000_0010_0000;
const CC_NOT_EQUAL: u16         = 0b_0000_0000_0100_0000;
const CC_EQUAL: u16             = 0b_0000_0000_1000_0000;
const CC_OVERFLOW_CLEAR: u16    = 0b_0000_0001_0000_0000;
const CC_OVERFLOW_SET: u16      = 0b_0000_0010_0000_0000;
const CC_PLUS: u16              = 0b_0000_0100_0000_0000;
const CC_MINUS: u16             = 0b_0000_1000_0000_0000;
const CC_GREATER_OR_EQUAL: u16  = 0b_0001_0000_0000_0000;
const CC_LESS_THAN: u16         = 0b_0010_0000_0000_0000;
const CC_GREATER_THAN: u16      = 0b_0100_0000_0000_0000;
const CC_LESS_OR_EQUAL: u16     = 0b_1000_0000_0000_0000;

/// An effective address specifier. Explains the kind of EA, using the bit
/// flags above, and includes the register field if relevant.
struct EASpec(u16, u8);

fn reg_no(reg: u16) -> Result<u8, MatchError> {
    if (reg & 0b111) == reg { Ok(reg as u8) }
    else                    { Err(MatchError::Internal) }
}

/// Given an encoded mode and register field for an effective address, produces
/// a description of the EA as an EASpec. This can be used for matching against
/// an EA mask
fn describe_ea(mode: u16, reg: u16) -> Result<EASpec, MatchError> {
    match mode {
        0b000 => Ok(EASpec(DATA_DIRECT,    try!(reg_no(reg)))),
        0b001 => Ok(EASpec(ADDR_DIRECT,    try!(reg_no(reg)))),
        0b010 => Ok(EASpec(ADDR_INDIRECT,  try!(reg_no(reg)))),
        0b011 => Ok(EASpec(ADDR_POSTINC,   try!(reg_no(reg)))),
        0b100 => Ok(EASpec(ADDR_PREDEC,    try!(reg_no(reg)))),
        0b101 => Ok(EASpec(ADDR_DISPLACE,  try!(reg_no(reg)))),
        0b110 => Ok(EASpec(ADDR_INDEX,     try!(reg_no(reg)))),
        0b111 => match reg {
            0b000 => Ok(EASpec(ABS_SHORT,    0)),
            0b001 => Ok(EASpec(ABS_LONG,     0)),
            0b010 => Ok(EASpec(PC_DISPLACE,  0)),
            0b011 => Ok(EASpec(PC_INDEX,     0)),
            0b100 => Ok(EASpec(IMMEDIATE,    0)),
            _ => Err(MatchError::Internal)
            },
        _ => Err(MatchError::Internal),
    }
}

/// Given an encoded condition code, produces the bit that corresponds to it
/// in the condition code masks
fn describe_cc(cc: u16) -> Option<u16> {
    match cc {
        0b0000 => Some(CC_TRUE),
        0b0001 => Some(CC_FALSE),
        0b0010 => Some(CC_HIGH),
        0b0011 => Some(CC_LOW_OR_SAME),
        0b0100 => Some(CC_HIGH_OR_SAME),
        0b0101 => Some(CC_LOW),
        0b0110 => Some(CC_NOT_EQUAL),
        0b0111 => Some(CC_EQUAL),
        0b1000 => Some(CC_OVERFLOW_CLEAR),
        0b1001 => Some(CC_OVERFLOW_SET),
        0b1010 => Some(CC_PLUS),
        0b1011 => Some(CC_MINUS),
        0b1100 => Some(CC_GREATER_OR_EQUAL),
        0b1101 => Some(CC_LESS_THAN),
        0b1110 => Some(CC_GREATER_THAN),
        0b1111 => Some(CC_LESS_OR_EQUAL),
        _ => None
    }
}

/// A matcher. Each matcher matches a fixed number of bits in an encoded
/// instruction word. A sequence of matchers totaling 16 encoded bits
/// corresponds to an entire instruction matcher.
#[derive(Copy, Clone)]
enum Matcher {
    /// Matches any number of bits. The meaning will be determined by the
    /// decoder.
    Any(u8),
    /// Matches an entire sequence of 16 bits. Used for instructions with only
    /// one valid encoding.
    Word(u16),
    /// Matches a sequence of 4 bits.
    Nibble(u8),
    /// Matches a single bit
    Bit(u8),
    /// Matches a 2 bit size field, any value but 0b11
    Size,
    /// Matches a 6 bit effective address field. Valid settings are determined
    /// by the given EA mask (see above)
    EA(u16),
    /// Same as EA, but the mode and register subfields are swapped. (Used for
    /// matching MOVE)
    AE(u16),
    /// Matches a 4 bit condition code field. Valid settings are determined by
    /// the given CC mask (see above)
    Cond(u16),
}

impl Matcher {
    /// Computes the number of bits this matcher consumes
    fn size(&self) -> u8 {
        match *self {
        Matcher::Any(n)      =>  n,
        Matcher::Word(_)     => 16,
        Matcher::Nibble(_)   =>  4,
        Matcher::Bit(_)      =>  1,
        Matcher::Size        =>  2,
        Matcher::EA(_)       =>  6,
        Matcher::AE(_)       =>  6,
        Matcher::Cond(_)     =>  4,
        }
    }

    /// Determines if the matcher matches the given bits. The matcher is
    /// compared against the entire value, so extra higher order bits must be
    /// masked off by the caller.
    fn matches(&self, x: u16) -> bool {
        match *self {
        Matcher::Any(_)      => true,
        Matcher::Word(n)     => n == x,
        Matcher::Nibble(n)   => n == x as u8,
        Matcher::Bit(n)      => n == x as u8,
        Matcher::Size => match x {
            0b00 => true,
            0b01 => true,
            0b10 => true,
            _ => false,
            },
        Matcher::EA(mask) => match describe_ea(x >> 3, x & 7) {
            Ok(ea) => (mask & ea.0) != 0,
            Err(_) => false,
            },
        Matcher::AE(mask) => match describe_ea(x & 7, x >> 3) {
            Ok(ea) => (mask & ea.0) != 0,
            Err(_) => false,
            },
        Matcher::Cond(mask) => match describe_cc(x & 15) {
            Some(cc) => (mask & cc) != 0,
            None => false,
            },
        }
    }
}

/// A decoder function for a single instruction match
type DecodeFn = Box<Fn(&[u16]) -> DecodeResult<MatchError>>;

/// A macro for generating an instruction decoder expression. `$matchers` is
/// an array of Matcher instances totaling 16 bits, and `body` is a closure
/// that accepts a single slice of 16 bit words, starting with the instruction
/// to be decoded.
macro_rules! decoder {
    ($matchers:expr, $body:expr) => (
        SingleDecoder::new(
            $matchers.iter().cloned().collect(),
            $body
        )
    )
}

#[inline]
fn bit(pc: &[u16], n: u16) -> u8 { ((pc[0] >> n) & 0b1) as u8 }

#[inline]
fn pair(pc: &[u16], n: u16) -> u8 { ((pc[0] >> n) & 0b11) as u8 }

#[inline]
fn triple(pc: &[u16], n: u16) -> u8 { ((pc[0] >> n) & 0b111) as u8 }

#[inline]
fn immediate(pc: &[u16], sz: Size, at: &mut usize) -> u32 {
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
fn decode_indexed(pc: &[u16], addr: Option<u8>) -> EA {
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
fn decode_ea_real(pc: &[u16], mode: u8, reg: u8, sz: Size, at: &mut usize)
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
fn decode_ea(pc: &[u16], n: u16, sz: Size, len: &mut usize) -> Option<EA> {
    decode_ea_real(pc, triple(pc, n+3), triple(pc, n), sz, len)
}

#[inline]
fn decode_ae(pc: &[u16], n: u16, sz: Size, len: &mut usize) -> Option<EA> {
    decode_ea_real(pc, triple(pc, n), triple(pc, n+3), sz, len)
}

#[inline]
fn decode_size(pc: &[u16], n: u16) -> Option<Size> {
    match pair(pc, n) {
        0b00 => Some(Size::Byte),
        0b01 => Some(Size::Word),
        0b10 => Some(Size::Long),
        _ => None
    }
}

#[inline]
fn decode_cond(pc: &[u16], n: u16) -> Option<Cond> {
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

/// Creates a vector of `SingleDecoder` instances
fn all_decoders() -> Vec<SingleDecoder> { vec![
    decoder!([ // ABCD Dy, Dx
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Nibble(0b0000),
        Matcher::Bit(0),
        Matcher::Any(3)
    ], |pc| {
        Ok((ABCD_Data(triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // ABCD -(Ay), -(Ax)
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Nibble(0b0000),
        Matcher::Bit(1),
        Matcher::Any(3)
    ], |pc| {
        Ok((ABCD_Addr(triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // ADD <ea>, Dn
        Matcher::Nibble(0b1101),
        Matcher::Any(3),
        Matcher::Bit(0),
        Matcher::Size,
        Matcher::EA(ANY_EA),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((ADD_to_Data(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // ADD Dn, <ea>
        Matcher::Nibble(0b1101),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::EA(MEMORY & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((ADD_to_EA(size, triple(pc, 9), ea), len))
    }),

    decoder!([ // ADD <ea>, An
        Matcher::Nibble(0b1101),
        Matcher::Any(3),
        Matcher::Any(1),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(ANY_EA),
    ], |pc| {
        let mut len = 1;
        let size = if bit(pc, 8) == 0 { Size::Word } else { Size::Long };
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((ADDA(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // ADDI #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b0110),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let imm = immediate(pc, size, &mut len);
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((ADDI(size, imm, ea), len))
    }),

    decoder!([ // ADDQ #<data>, <ea>
        Matcher::Nibble(0b0101),
        Matcher::Any(3),
        Matcher::Bit(0),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let imm = triple(pc, 9);
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((ADDQ(size, imm, ea), len))
    }),

    decoder!([ // ADDX Dy, Dx
        Matcher::Nibble(0b1101),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::Bit(0),
        Matcher::Bit(0),
        Matcher::Bit(0),
        Matcher::Any(3),
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        Ok((ADDX_Data(size, triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // ADDX -(Ay), -(Ax)
        Matcher::Nibble(0b1101),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::Bit(0),
        Matcher::Bit(0),
        Matcher::Bit(1),
        Matcher::Any(3),
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        Ok((ADDX_Addr(size, triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // AND <ea>, Dn
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(0),
        Matcher::Size,
        Matcher::EA(DATA),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((AND_to_Data(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // AND Dn, <ea>
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::EA(MEMORY & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((AND_to_EA(size, triple(pc, 9), ea), len))
    }),

    decoder!([ // ANDI #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b0010),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let imm = immediate(pc, size, &mut len);
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((ANDI(size, imm, ea), len))
    }),

    decoder!([ // ANDI #xxx, CCR
        Matcher::Word(0b00000010_00111100)
    ], |pc| {
        Ok((ANDI_to_CCR(pc[1] as u8), 2))
    }),

    decoder!([ // ANDI #xxx, SR
        Matcher::Word(0b00000010_01111100)
    ], |pc| {
        Ok((ANDI_to_SR(pc[1]), 2))
    }),

    decoder!([ // ASd Dx, Dy
        Matcher::Nibble(0b1110),
        Matcher::Any(3), // Dx
        Matcher::Any(1), // direction
        Matcher::Size,
        Matcher::Bit(1),
        Matcher::Bit(0), Matcher::Bit(0),
        Matcher::Any(3), // Dy
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        Ok((ASd_Data(size, dir, triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // ASd #<data>, Dy
        Matcher::Nibble(0b1110),
        Matcher::Any(3), // Dx
        Matcher::Any(1), // direction
        Matcher::Size,
        Matcher::Bit(0),
        Matcher::Bit(0), Matcher::Bit(0),
        Matcher::Any(3), // Dy
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        Ok((ASd_to_Data(size, dir, triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // ASd <ea>
        Matcher::Nibble(0b1110),
        Matcher::Bit(0), Matcher::Bit(0), Matcher::Bit(0),
        Matcher::Any(1), // direction
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(MEMORY & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((ASd_EA(dir, ea), len))
    }),

    decoder!([ // Bcc <label>
        Matcher::Nibble(0b0110),
        Matcher::Cond(TRUE_COND), // condition
        Matcher::Any(8), // displacement
    ], |pc| {
        let cond = decode_cond(pc, 8).unwrap();
        let disp = (pc[0] & 0xff) as i8;
        Ok(if disp == 0 {
            (Bcc(cond, pc[1] as i16), 2)
        } else {
            (Bcc(cond, disp as i16), 1)
        })
    }),

    decoder!([ // BCHG Dn, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(0), Matcher::Bit(1),
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((BCHG_Data(triple(pc, 9), ea), len))
    }),

    decoder!([ // BCHG #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b1000),
        Matcher::Bit(0), Matcher::Bit(1),
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 2;
        let imm = pc[1] as u8;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((BCHG_Imm(imm, ea), len))
    }),

    decoder!([ // BCLR Dn, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(1), Matcher::Bit(0),
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((BCLR_Data(triple(pc, 9), ea), len))
    }),

    decoder!([ // BCLR #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b1000),
        Matcher::Bit(1), Matcher::Bit(0),
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 2;
        let imm = pc[1] as u8;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((BCLR_Imm(imm, ea), len))
    }),

    decoder!([ // BRA <label>
        Matcher::Nibble(0b0110),
        Matcher::Nibble(0b0000),
        Matcher::Any(8), // displacement
    ], |pc| {
        let disp = (pc[0] & 0xff) as i8;
        Ok(if disp == 0 {
            (BRA(pc[1] as i16), 2)
        } else {
            (BRA(disp as i16), 1)
        })
    }),

    decoder!([ // BSET Dn, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((BSET_Data(triple(pc, 9), ea), len))
    }),

    decoder!([ // BSET #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b1000),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 2;
        let imm = pc[1] as u8;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((BSET_Imm(imm, ea), len))
    }),

    decoder!([ // BSR <label>
        Matcher::Nibble(0b0110),
        Matcher::Nibble(0b0001),
        Matcher::Any(8), // displacement
    ], |pc| {
        let disp = (pc[0] & 0xff) as i8;
        Ok(if disp == 0 {
            (BSR(pc[1] as i16), 2)
        } else {
            (BSR(disp as i16), 1)
        })
    }),

    decoder!([ // BTST Dn, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(0), Matcher::Bit(0),
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((BTST_Data(triple(pc, 9), ea), len))
    }),

    decoder!([ // BTST #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b1000),
        Matcher::Bit(0), Matcher::Bit(0),
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 2;
        let imm = pc[1] as u8;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((BTST_Imm(imm, ea), len))
    }),

    decoder!([ // CHK <ea>, Dn
        Matcher::Nibble(0b0100),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(1), Matcher::Bit(0),
        Matcher::EA(DATA)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((CHK(ea, triple(pc, 9)), len))
    }),

    decoder!([ // CLR <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b0010),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((CLR(size, ea), len))
    }),

    decoder!([ // CMP <ea>, Dn
        Matcher::Nibble(0b1011),
        Matcher::Any(3), // Dn
        Matcher::Bit(0),
        Matcher::Size,
        Matcher::EA(ANY_EA)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((CMP(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // CMPA <ea>, An
        Matcher::Nibble(0b1011),
        Matcher::Any(3), // Dn
        Matcher::Any(1), // size
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(ANY_EA)
    ], |pc| {
        let mut len = 1;
        let size = if bit(pc, 8) == 0 { Size::Word } else { Size::Long };
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((CMPA(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // CMPI #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b1100),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let imm = immediate(pc, size, &mut len);
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((CMPI(size, imm, ea), len))
    }),

    decoder!([ // CMPM (Ay)+, (Ax)+
        Matcher::Nibble(0b1011),
        Matcher::Any(3), // Ax
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::Bit(0), Matcher::Bit(0), Matcher::Bit(1),
        Matcher::Any(3), // Ay
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        Ok((CMPM(size, triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // DBcc Dn, <label>
        Matcher::Nibble(0b0101),
        Matcher::Cond(ANY_COND),
        Matcher::Nibble(0b1100),
        Matcher::Bit(1),
        Matcher::Any(3), // Dn
    ], |pc| {
        let cond = decode_cond(pc, 8).unwrap();
        Ok((DBcc(cond, triple(pc, 0), pc[1] as i16), 2))
    }),

    decoder!([ // DIVS <ea>, Dn
        Matcher::Nibble(0b1000),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((DIVS(ea, triple(pc, 9)), len))
    }),

    decoder!([ // DIVU <ea>, Dn
        Matcher::Nibble(0b1000),
        Matcher::Any(3),
        Matcher::Bit(0), Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((DIVU(ea, triple(pc, 9)), len))
    }),

    decoder!([ // EOR Dn, <ea>
        Matcher::Nibble(0b1011),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((EOR(size, triple(pc, 9), ea), len))
    }),

    decoder!([ // EORI #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b1010),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let imm = immediate(pc, size, &mut len);
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((EORI(size, imm, ea), len))
    }),

    decoder!([ // EORI #xxx, CCR
        Matcher::Word(0b0000101000111100),
    ], |pc| {
        Ok((EORI_to_CCR(pc[1] as u8), 2))
    }),

    decoder!([ // EORI #xxx, SR
        Matcher::Word(0b0000101001111100),
    ], |pc| {
        Ok((EORI_to_SR(pc[1]), 2))
    }),

    decoder!([ // EXG Dx, Dy
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Nibble(0b0100),
        Matcher::Bit(0),
        Matcher::Any(3),
    ], |pc| {
        Ok((EXG_Data(triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // EXG Ax, Ay
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Nibble(0b0100),
        Matcher::Bit(1),
        Matcher::Any(3),
    ], |pc| {
        Ok((EXG_Addr(triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // EXG Dx, Ay
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Nibble(0b1000),
        Matcher::Bit(1),
        Matcher::Any(3),
    ], |pc| {
        Ok((EXG_Both(triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // EXT.w Dn
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1000),
        Matcher::Nibble(0b1000),
        Matcher::Bit(0),
        Matcher::Any(3),
    ], |pc| {
        Ok((EXT(Size::Word, triple(pc, 0)), 1))
    }),

    decoder!([ // EXT.l Dn
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1000),
        Matcher::Nibble(0b1100),
        Matcher::Bit(0),
        Matcher::Any(3),
    ], |pc| {
        Ok((EXT(Size::Long, triple(pc, 0)), 1))
    }),

    decoder!([ // ILLEGAL
        Matcher::Word(0b0100101011111100),
    ], |_| {
        Ok((ILLEGAL, 1))
    }),

    decoder!([ // JMP <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1110),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(CONTROL)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((JMP(ea), len))
    }),

    decoder!([ // JSR <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1110),
        Matcher::Bit(1), Matcher::Bit(0),
        Matcher::EA(CONTROL)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((JSR(ea), len))
    }),

    decoder!([ // LEA <ea>, An
        Matcher::Nibble(0b0100),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(CONTROL)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((LEA(ea, triple(pc, 9)), len))
    }),

    decoder!([ // LINK An, #<displacement>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1110),
        Matcher::Nibble(0b0101),
        Matcher::Bit(0),
        Matcher::Any(3),
    ], |pc| {
        Ok((LINK(triple(pc, 0), pc[1] as i16), 2))
    }),

    decoder!([ // LSd Dx, Dy
        Matcher::Nibble(0b1110),
        Matcher::Any(3), // Dx
        Matcher::Any(1), // direction
        Matcher::Size,
        Matcher::Bit(1),
        Matcher::Bit(0), Matcher::Bit(1),
        Matcher::Any(3), // Dy
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        Ok((LSd_Data(size, dir, triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // LSd #<data>, Dy
        Matcher::Nibble(0b1110),
        Matcher::Any(3), // Dx
        Matcher::Any(1), // direction
        Matcher::Size,
        Matcher::Bit(0),
        Matcher::Bit(0), Matcher::Bit(1),
        Matcher::Any(3), // Dy
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        Ok((LSd_to_Data(size, dir, triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // LSd <ea>
        Matcher::Nibble(0b1110),
        Matcher::Bit(0), Matcher::Bit(0), Matcher::Bit(1),
        Matcher::Any(1), // direction
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(MEMORY & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((LSd_EA(dir, ea), len))
    }),

    decoder!([ // MOVE <ea>, <ea>
        Matcher::Bit(0), Matcher::Bit(0),
        Matcher::Any(2),
        Matcher::AE(DATA & ALTERABLE),
        Matcher::EA(ANY_EA)
    ], |pc| {
        let mut len = 1;
        let size = match (pc[0] >> 12) & 0x3 {
            0b01 => Size::Byte,
            0b11 => Size::Word,
            0b10 => Size::Long,
            _ => return Err(MatchError::Mismatch)
        };
        // source comes first!
        let src = decode_ea(pc, 0, size, &mut len).unwrap();
        let dst = decode_ae(pc, 6, size, &mut len).unwrap();
        Ok((MOVE(size, src, dst), len))
    }),

    decoder!([ // MOVE CCR, <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b0010),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((MOVE_from_CCR(ea), len))
    }),

    decoder!([ // MOVE <ea>, CCR
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b0100),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA),
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((MOVE_to_CCR(ea), len))
    }),

    decoder!([ // MOVE <ea>, SR
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b0110),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA),
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((MOVE_to_SR(ea), len))
    }),

    decoder!([ // MOVE SR, <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b0000),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((MOVE_from_SR(ea), len))
    }),

    decoder!([ // MOVE USP, An
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1110),
        Matcher::Nibble(0b0110),
        Matcher::Bit(1),
        Matcher::Any(3),
    ], |pc| {
        Ok((MOVE_from_USP(triple(pc, 0)), 1))
    }),

    decoder!([ // MOVE An, USP
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1110),
        Matcher::Nibble(0b0110),
        Matcher::Bit(0),
        Matcher::Any(3),
    ], |pc| {
        Ok((MOVE_to_USP(triple(pc, 0)), 1))
    }),

    decoder!([ // MOVEA <ea>, An
        Matcher::Bit(0), Matcher::Bit(0),
        Matcher::Bit(1), Matcher::Any(1),
        Matcher::Any(3),
        Matcher::Bit(0), Matcher::Bit(0), Matcher::Bit(1),
        Matcher::EA(ANY_EA),
    ], |pc| {
        let mut len = 1;
        let size = if bit(pc, 12) == 0 { Size::Long } else { Size::Word };
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((MOVEA(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // MOVEM <register list>, <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1000),
        Matcher::Bit(1),
        Matcher::Any(1),
        Matcher::EA((CONTROL & ALTERABLE) | ADDR_PREDEC),
    ], |pc| {
        let mut len = 2;
        let size = if bit(pc, 6) == 0 { Size::Word } else { Size::Long };
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((MOVEM_Save(size, pc[1], ea), len))
    }),

    decoder!([ // MOVEM <register list>, <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1100),
        Matcher::Bit(1),
        Matcher::Any(1),
        Matcher::EA(CONTROL | ADDR_POSTINC),
    ], |pc| {
        let mut len = 2;
        let size = if bit(pc, 6) == 0 { Size::Word } else { Size::Long };
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((MOVEM_Load(size, ea, pc[1]), len))
    }),

    decoder!([ // MOVEP Dx, d(Ay)
        Matcher::Nibble(0b0000),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::Any(1),
        Matcher::Bit(0), Matcher::Bit(0), Matcher::Bit(1),
        Matcher::Any(3),
    ], |pc| {
        let size = if bit(pc, 6) == 0 { Size::Word } else { Size::Long };
        Ok((MOVEP_Save(size, triple(pc, 9), pc[1] as i16, triple(pc, 0)), 2))
    }),

    decoder!([ // MOVEP d(Ay), Dx
        Matcher::Nibble(0b0000),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(0),
        Matcher::Any(1),
        Matcher::Bit(0), Matcher::Bit(0), Matcher::Bit(1),
        Matcher::Any(3),
    ], |pc| {
        let size = if bit(pc, 6) == 0 { Size::Word } else { Size::Long };
        Ok((MOVEP_Load(size, pc[1] as i16, triple(pc, 0), triple(pc, 9)), 2))
    }),

    decoder!([ // MOVEQ #<data>, Dn
        Matcher::Nibble(0b0111),
        Matcher::Any(3),
        Matcher::Bit(0),
        Matcher::Any(8),
    ], |pc| {
        Ok((MOVEQ((pc[0] & 0xff) as i8, triple(pc, 9)), 1))
    }),

    decoder!([ // MULS <ea>, Dn
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(1), Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((MULS(ea, triple(pc, 9)), len))
    }),

    decoder!([ // MULU <ea>, Dn
        Matcher::Nibble(0b1100),
        Matcher::Any(3),
        Matcher::Bit(0), Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((MULS(ea, triple(pc, 9)), len))
    }),

    decoder!([ // NBCD <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1000),
        Matcher::Bit(0), Matcher::Bit(0),
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((NBCD(ea), len))
    }),

    decoder!([ // NEG <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b0100),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((NEG(size, ea), len))
    }),

    decoder!([ // NEGX <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b0000),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((NEG(size, ea), len))
    }),

    decoder!([ // NOP
        Matcher::Word(0b0100111001110001),
    ], |_| {
        Ok((NOP, 1))
    }),

    decoder!([ // NOT
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b0110),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((NOT(size, ea), len))
    }),

    decoder!([ // OR <ea>, Dn
        Matcher::Nibble(0b1000),
        Matcher::Any(3),
        Matcher::Bit(0),
        Matcher::Size,
        Matcher::EA(DATA)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((OR_to_Data(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // OR Dn, <ea>
        Matcher::Nibble(0b1000),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::EA(MEMORY & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((OR_to_EA(size, triple(pc, 9), ea), len))
    }),

    decoder!([ // ORI #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b0000),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let imm = immediate(pc, size, &mut len);
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((ORI(size, imm, ea), len))
    }),

    decoder!([ // ORI #xxx, CCR
        Matcher::Word(0b0000000000111100)
    ], |pc| {
        Ok((ORI_to_CCR(pc[1] as u8), 2))
    }),

    decoder!([ // ORI #xxx, SR
        Matcher::Word(0b0000000001111100)
    ], |pc| {
        Ok((ORI_to_SR(pc[1]), 2))
    }),

    decoder!([ // PEA <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1000),
        Matcher::Bit(0), Matcher::Bit(1),
        Matcher::EA(CONTROL)
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((PEA(ea), len))
    }),

    decoder!([ // RESET
        Matcher::Word(0b0100111001110000)
    ], |_| {
        Ok((RESET, 1))
    }),

    decoder!([ // ROd Dx, Dy
        Matcher::Nibble(0b1110),
        Matcher::Any(3), // Dx
        Matcher::Any(1), // direction
        Matcher::Size,
        Matcher::Bit(1),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::Any(3), // Dy
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        Ok((ROd_Data(size, dir, triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // ROd #<data>, Dy
        Matcher::Nibble(0b1110),
        Matcher::Any(3), // Dx
        Matcher::Any(1), // direction
        Matcher::Size,
        Matcher::Bit(0),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::Any(3), // Dy
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        Ok((ROd_to_Data(size, dir, triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // ROd <ea>
        Matcher::Nibble(0b1110),
        Matcher::Bit(0), Matcher::Bit(1), Matcher::Bit(1),
        Matcher::Any(1), // direction
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(MEMORY & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((ROd_EA(dir, ea), len))
    }),

    decoder!([ // ROXd Dx, Dy
        Matcher::Nibble(0b1110),
        Matcher::Any(3), // Dx
        Matcher::Any(1), // direction
        Matcher::Size,
        Matcher::Bit(1),
        Matcher::Bit(1), Matcher::Bit(0),
        Matcher::Any(3), // Dy
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        Ok((ROXd_Data(size, dir, triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // ROXd #<data>, Dy
        Matcher::Nibble(0b1110),
        Matcher::Any(3), // Dx
        Matcher::Any(1), // direction
        Matcher::Size,
        Matcher::Bit(0),
        Matcher::Bit(1), Matcher::Bit(0),
        Matcher::Any(3), // Dy
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        Ok((ROXd_to_Data(size, dir, triple(pc, 9), triple(pc, 0)), 1))
    }),

    decoder!([ // ROXd <ea>
        Matcher::Nibble(0b1110),
        Matcher::Bit(0), Matcher::Bit(1), Matcher::Bit(0),
        Matcher::Any(1), // direction
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(MEMORY & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
        let ea = decode_ea(pc, 0, Size::Word, &mut len).unwrap();
        Ok((ROXd_EA(dir, ea), len))
    }),

    decoder!([ // RTE
        Matcher::Word(0b0100111001110011),
    ], |_| {
        Ok((RTE, 1))
    }),

    decoder!([ // RTR
        Matcher::Word(0b0100111001110111),
    ], |_| {
        Ok((RTR, 1))
    }),

    decoder!([ // RTS
        Matcher::Word(0b0100111001110101),
    ], |_| {
        Ok((RTS, 1))
    }),

    decoder!([ // SBCD Dy, Dx
        Matcher::Nibble(0b1000),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Nibble(0b0000),
        Matcher::Bit(0),
        Matcher::Any(3)
    ], |pc| {
        Ok((SBCD_Data(triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // SBCD -(Ay), -(Ax)
        Matcher::Nibble(0b1000),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Nibble(0b0000),
        Matcher::Bit(1),
        Matcher::Any(3)
    ], |pc| {
        Ok((SBCD_Addr(triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // Scc <ea>
        Matcher::Nibble(0b0101),
        Matcher::Cond(ANY_COND),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let cond = decode_cond(pc, 8).unwrap();
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((Scc(cond, ea), len))
    }),

    decoder!([ // STOP #xxx
        Matcher::Word(0b0100111001110010),
    ], |pc| {
        Ok((STOP(pc[1]), 2))
    }),

    decoder!([ // SUB <ea>, Dn
        Matcher::Nibble(0b1001),
        Matcher::Any(3),
        Matcher::Bit(0),
        Matcher::Size,
        Matcher::EA(ANY_EA),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((SUB_to_Data(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // SUB Dn, <ea>
        Matcher::Nibble(0b1001),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::EA(MEMORY & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((SUB_to_EA(size, triple(pc, 9), ea), len))
    }),

    decoder!([ // SUBA <ea>, An
        Matcher::Nibble(0b1001),
        Matcher::Any(3),
        Matcher::Any(1),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(ANY_EA),
    ], |pc| {
        let mut len = 1;
        let size = if bit(pc, 8) == 0 { Size::Word } else { Size::Long };
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((SUBA(size, ea, triple(pc, 9)), len))
    }),

    decoder!([ // SUBI #<data>, <ea>
        Matcher::Nibble(0b0000),
        Matcher::Nibble(0b0100),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let imm = immediate(pc, size, &mut len);
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((SUBI(size, imm, ea), len))
    }),

    decoder!([ // SUBQ #<data>, <ea>
        Matcher::Nibble(0b0101),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE)
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let imm = triple(pc, 9);
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((SUBQ(size, imm, ea), len))
    }),

    decoder!([ // SUBX Dy, Dx
        Matcher::Nibble(0b1001),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::Bit(0),
        Matcher::Bit(0),
        Matcher::Bit(0),
        Matcher::Any(3),
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        Ok((SUBX_Data(size, triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // SUBX -(Ay), -(Ax)
        Matcher::Nibble(0b1001),
        Matcher::Any(3),
        Matcher::Bit(1),
        Matcher::Size,
        Matcher::Bit(0),
        Matcher::Bit(0),
        Matcher::Bit(1),
        Matcher::Any(3),
    ], |pc| {
        let size = decode_size(pc, 6).unwrap();
        Ok((SUBX_Addr(size, triple(pc, 0), triple(pc, 9)), 1))
    }),

    decoder!([ // SWAP Dn
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1000),
        Matcher::Nibble(0b0100),
        Matcher::Bit(0),
        Matcher::Any(3),
    ], |pc| {
        Ok((SWAP(triple(pc, 0)), 1))
    }),

    decoder!([ // TAS <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1010),
        Matcher::Bit(1), Matcher::Bit(1),
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let ea = decode_ea(pc, 0, Size::Byte, &mut len).unwrap();
        Ok((TAS(ea), len))
    }),

    decoder!([ // TRAP #<vector>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1110),
        Matcher::Nibble(0b0100),
        Matcher::Any(4),
    ], |pc| {
        Ok((TRAP((pc[0] & 0xf) as u8), 1))
    }),

    decoder!([ // TRAPV
        Matcher::Word(0b0100111001110110),
    ], |_| {
        Ok((TRAPV, 1))
    }),

    decoder!([ // TST <ea>
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1010),
        Matcher::Size,
        Matcher::EA(DATA & ALTERABLE),
    ], |pc| {
        let mut len = 1;
        let size = decode_size(pc, 6).unwrap();
        let ea = decode_ea(pc, 0, size, &mut len).unwrap();
        Ok((TST(size, ea), len))
    }),

    decoder!([ // UNLK An
        Matcher::Nibble(0b0100),
        Matcher::Nibble(0b1110),
        Matcher::Nibble(0b0101),
        Matcher::Bit(1),
        Matcher::Any(3),
    ], |pc| {
        Ok((UNLK(triple(pc, 0)), 1))
    }),
] }

/// A decoder for a single instruction. Pairs a list of matchers with a decoder
/// to produce a complete instruction decoding. The `decode` function will first
/// check the instruction word aganist the matchers, resulting in a
/// `MatchError::Mismatch` if a mismatch is found, and then invokes the decoder
/// to produce the final result. The decode step may fail, so a simple match is
/// not indicative of a successful decode.
struct SingleDecoder {
    matchers: Vec<Matcher>,
    decode_fn: DecodeFn,
}

impl SingleDecoder {
    fn new<F: 'static>(matchers: Vec<Matcher>, decode: F) -> SingleDecoder
    where F: Fn(&[u16]) -> DecodeResult<MatchError> {
        SingleDecoder {
            matchers:   matchers,
            decode_fn:  Box::new(decode),
        }
    }
}

impl Decoder<MatchError> for SingleDecoder {
    fn decode(&self, pc: &[u16]) -> DecodeResult<MatchError> {
        let instr = pc[0] as u32;
        let mut offset = 16;

        for m in self.matchers.iter() {
            let sz = m.size();
            if sz > offset {
                return Err(MatchError::Internal)
            }
            offset -= sz;
            let field: u32 = (instr >> offset) & ((1 << sz) - 1);
            if !m.matches(field as u16) {
                return Err(MatchError::Mismatch);
            }
        }

        if offset != 0 {
            return Err(MatchError::Internal)
        }

        (self.decode_fn)(pc)
    }
}

/// The top level `Decoder` for careful instruction decoding. Internally this
/// contains a list of `SingleDecoders` which are tried in sequence until one
/// finds a correct match. The goal is that no `SingleDecoder` will match an
/// instruction that is not completely unique, producing a correct decoding
/// every time and never relying on precedences or other guesswork.
pub struct CarefulDecoder {
    decoders: Vec<SingleDecoder>
}

impl CarefulDecoder {
    pub fn new() -> CarefulDecoder {
        CarefulDecoder {
            decoders: all_decoders()
        }
    }
}

impl Decoder<()> for CarefulDecoder {
    fn decode(&self, pc: &[u16]) -> DecodeResult<()> {
        for d in self.decoders.iter() {
            match d.decode(pc) {
                Ok(result) => return Ok(result),
                Err(MatchError::Mismatch) => { },
                _ => panic!()
            }
        }

        Err(())
    }
}

#[test]
fn test_matchers() {
    assert!( Matcher::Any(3).matches(0b001));

    assert!( Matcher::Word(10).matches(10));
    assert!(!Matcher::Word(10).matches(11));

    assert!( Matcher::Nibble(10).matches(10));
    assert!(!Matcher::Nibble(10).matches(11));

    assert!( Matcher::Bit(0).matches(0));
    assert!(!Matcher::Bit(0).matches(1));

    assert!( Matcher::Size.matches(0));
    assert!( Matcher::Size.matches(1));
    assert!( Matcher::Size.matches(2));
    assert!(!Matcher::Size.matches(3));
    assert!(!Matcher::Size.matches(4));

    assert!( Matcher::EA(ANY_EA).matches(0b000_000));
    assert!( Matcher::EA(ANY_EA).matches(0b111_010));
    assert!(!Matcher::EA(ANY_EA).matches(0b111_111));

    assert!( Matcher::EA(DATA).matches(0b000_010));
    assert!( Matcher::EA(DATA).matches(0b011_111));
    assert!(!Matcher::EA(DATA).matches(0b001_001));

    assert!( Matcher::AE(ANY_EA).matches(0b000_000));
    assert!( Matcher::AE(ANY_EA).matches(0b010_111));
    assert!(!Matcher::AE(ANY_EA).matches(0b111_111));

    assert!( Matcher::AE(DATA).matches(0b010_000));
    assert!( Matcher::AE(DATA).matches(0b111_011));
    assert!(!Matcher::AE(DATA).matches(0b001_001));

    assert!( Matcher::Cond(TRUE_COND).matches(0b0100));
    assert!( Matcher::Cond(TRUE_COND).matches(0b0101));
    assert!(!Matcher::Cond(TRUE_COND).matches(0b0001));
}

#[cfg(test)]
fn check_decode(d: &CarefulDecoder, pc: &[u16], i: Instruction) {
    match d.decode(pc) {
        Ok((result, _)) => {
            println!("{:?} should be {:?} ?", result, i);
            assert!(result == i);
        },
        Err(_) => { panic!(); },
    }
}

#[test]
fn test_decode_ea() {
    let d = CarefulDecoder::new();

    check_decode(&d,
        &[0b1101_001_0_01_000100],
        ADD_to_Data(Size::Word, EA::DataDirect(4), 1)
    );
    check_decode(&d,
        &[0b1101_001_0_01_001101],
        ADD_to_Data(Size::Word, EA::AddrDirect(5), 1)
    );
    check_decode(&d,
        &[0b1101_001_0_01_010111],
        ADD_to_Data(Size::Word, EA::AddrIndirect(7), 1)
    );
    check_decode(&d,
        &[0b1101_001_0_01_011000],
        ADD_to_Data(Size::Word, EA::AddrPostInc(0), 1)
    );
    check_decode(&d,
        &[0b1101_001_0_01_100010],
        ADD_to_Data(Size::Word, EA::AddrPreDec(2), 1)
    );

    check_decode(&d, &[
            0b1101_001_0_01_101011,
            0b11111111_11111111
        ],
        ADD_to_Data(Size::Word, EA::AddrDisplace(3, -1), 1)
    );
    check_decode(&d, &[
            0b1101_001_0_01_101011,
            0b01111111_11111111
        ],
        ADD_to_Data(Size::Word, EA::AddrDisplace(3, 0x7fff), 1)
    );

    check_decode(&d, &[
            0b1101_001_0_01_110111,
            0b0_010_0_000_00000101,
        ],
        ADD_to_Data(
            Size::Word,
            EA::AddrIndex(7, Index::Data(Size::Word, 2), 5),
            1
        )
    );
    check_decode(&d, &[
            0b1101_001_0_01_110111,
            0b1_101_1_000_11111111,
        ],
        ADD_to_Data(
            Size::Word,
            EA::AddrIndex(7, Index::Addr(Size::Long, 5), -1),
            1
        )
    );

    check_decode(&d, &[
            0b1101_001_0_01_111000,
            0b11111111_11111111,
        ],
        ADD_to_Data(Size::Word, EA::AbsWord(-1), 1)
    );
    check_decode(&d, &[
            0b1101_001_0_01_111001,
            0b00000000_00000000,
            0b00000000_00001111,
        ],
        ADD_to_Data(Size::Word, EA::AbsLong(15), 1)
    );

    check_decode(&d, &[
            0b1101_001_0_01_111010,
            0b00000111_00000000,
        ],
        ADD_to_Data(Size::Word, EA::PcDisplace(0x0700), 1)
    );
    check_decode(&d, &[
            0b1101_001_0_01_111011,
            0b1_101_1_000_11111111,
        ],
        ADD_to_Data(Size::Word, EA::PcIndex(Index::Addr(Size::Long, 5), -1), 1)
    );

    check_decode(&d, &[
            0b1101_001_0_00_111100,
            0b00000110_11111111,
        ],
        ADD_to_Data(Size::Byte, EA::ImmByte(0xff), 1)
    );
    check_decode(&d, &[
            0b1101_001_0_01_111100,
            0b00000110_11111111,
        ],
        ADD_to_Data(Size::Word, EA::ImmWord(0x06ff), 1)
    );
    check_decode(&d, &[
            0b1101_001_0_10_111100,
            0b00000110_11111111,
            0b00000001_00110111,
        ],
        ADD_to_Data(Size::Long, EA::ImmLong(0x06ff0137), 1)
    );
}

#[test]
fn test_decoders() {
    let d = CarefulDecoder::new();

    check_decode(&d,
        &[0b1100_010_10000_0_011],
        ABCD_Data(3, 2)
    );
    check_decode(&d,
        &[0b1100_100_10000_1_101],
        ABCD_Addr(5, 4)
    );
    check_decode(&d,
        &[0b1101_001_0_10_000100],
        ADD_to_Data(Size::Long, EA::DataDirect(4), 1)
    );
    check_decode(&d,
        &[0b1101_001_1_10_100100],
        ADD_to_EA(Size::Long, 1, EA::AddrPreDec(4))
    );
    check_decode(&d,
        &[0b1101_111_1_11_000111],
        ADDA(Size::Long, EA::DataDirect(7), 7)
    );
    check_decode(&d, &[
            0b0000_0110_01_010011,
            0b00000001_00001111,
        ],
        ADDI(Size::Word, 271, EA::AddrIndirect(3))
    );
    check_decode(&d,
        &[0b0101_101_0_10_100010],
        ADDQ(Size::Long, 5, EA::AddrPreDec(2))
    );
    check_decode(&d,
        &[0b1101_101_1_10_00_0_010],
        ADDX_Data(Size::Long, 2, 5)
    );
    check_decode(&d,
        &[0b1101_101_1_10_00_1_010],
        ADDX_Addr(Size::Long, 2, 5)
    );
    check_decode(&d,
        &[0b1100_101_0_10_100010],
        AND_to_Data(Size::Long, EA::AddrPreDec(2), 5)
    );
    check_decode(&d,
        &[0b1100_001_1_10_100100],
        AND_to_EA(Size::Long, 1, EA::AddrPreDec(4))
    );
    check_decode(&d, &[
            0b0000_0010_00_010011,
            0b00000010_00001111,
        ],
        ANDI(Size::Byte, 15, EA::AddrIndirect(3))
    );
    // ASd_Data
    // ASd_to_Data
    // ASd_EA
    // Bcc
    // BCHG_Data
    // BCHG_Imm
    // BCLR_Data
    // BCLR_Imm
    // BRA
    // BSET_Data
    // BSET_Imm
    // BSR
    // BTST_Data
    // BTST_Imm
    // CHK
    // CLR
    // CMP
    // CMPA
    // CMPI
    // CMPM
    // DBcc
    // DIVS
    // DIVU
    // EOR
    // EORI
    // EORI_to_CCR
    // EORI_to_SR
    // EXG_Data
    // EXG_Addr
    // EXG_Both
    // EXT
    check_decode(&d, &[0x4afc], ILLEGAL);
    // JMP
    // JSR
    // LEA
    // LINK
    // LSd_Data
    // LSd_to_Data
    // LSd_EA
    check_decode(&d,
        &[0x296c, 0x0003, 0x0005], // GNU m68k-as output
        MOVE(Size::Long, EA::AddrDisplace(4, 3), EA::AddrDisplace(4, 5))
    );
    // MOVE_from_CCR
    // MOVE_to_CCR
    // MOVE_to_SR
    // MOVE_from_SR
    // MOVE_from_USP
    // MOVE_to_USP
    // MOVEA
    // MOVEM_Save
    // MOVEM_Load
    // MOVEP_Save
    // MOVEP_Load
    // MOVEQ
    // MULS
    // MULU
    // NBCD
    // NEG
    // NEGX
    check_decode(&d, &[0x4e71], NOP);
    // NOT
    // OR_to_Data
    // OR_to_EA
    // ORI
    // ORI_to_CCR
    // ORI_to_SR
    // PEA
    check_decode(&d, &[0x4e70], RESET);
    // ROd_Data
    // ROd_to_Data
    // ROd_EA
    // ROXd_Data
    // ROXd_to_Data
    // ROXd_EA
    check_decode(&d, &[0x4e73], RTE);
    check_decode(&d, &[0x4e77], RTR);
    check_decode(&d, &[0x4e75], RTS);
    // SBCD_Data
    // SBCD_Addr
    // Scc
    // STOP
    // SUB_to_Data
    // SUB_to_EA
    // SUBA
    // SUBI
    // SUBQ
    // SUBX_Data
    // SUBX_Addr
    // SWAP
    // TAS
    // TRAP
    check_decode(&d, &[0x4e76], TRAPV);
    // TST
    // UNLK
}

#[test]
fn test_gotchas() {
    let d = CarefulDecoder::new();

    check_decode(&d,
        &[0b1101_100_1_10_000011],
        ADDX_Data(Size::Long, 3, 4),
    );
    check_decode(&d,
        &[0b1101_100_0_10_000011],
        ADD_to_Data(Size::Long, EA::DataDirect(3), 4)
    );
}

#[test]
fn test_all_patterns() {
    let ds = all_decoders();

    for i in 0..0x10000 {
        let pc = [i as u16, 0, 0, 0, 0, 0, 0];
        let mut matches = 0;
        println!("0b{:016b}", i);
        for d in ds.iter() {
            if let Ok(inst) = d.decode(&pc) {
                println!("  -> {:?}", inst);
                matches += 1;
            }
        }
        assert!(matches < 2);
    }
}
