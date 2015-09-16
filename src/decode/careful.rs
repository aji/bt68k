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
fn imm_long(pc: &[u16], at: &mut usize) -> u32 {
    *at += 2;
    ((pc[*at - 2] as u32) << 16) | (pc[*at - 1] as u32)
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
            0b001 => { Some(EA::AbsLong(imm_long(pc, at))) },
            0b010 => { *at += 1; Some(EA::PcDisplace(ext[0] as i16)) },
            0b011 => { *at += 1; Some(decode_indexed(ext, None)) },
            0b100 => match sz {
                Size::Byte => { *at += 1; Some(EA::ImmByte(ext[0] as u8)) },
                Size::Word => { *at += 1; Some(EA::ImmWord(ext[0])) },
                Size::Long => { Some(EA::ImmLong(imm_long(pc, at))) },
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
        let imm = match size {
            Size::Byte | Size::Word => { len += 1; pc[1] as u32 },
            Size::Long => imm_long(pc, &mut len)
        };
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
    // ADDA
    // ADDI
    // ADDQ
    // ADDX_Data
    // ADDX_Addr
    // AND_to_Data
    // AND_to_EA
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
