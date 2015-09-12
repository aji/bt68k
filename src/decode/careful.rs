pub use decode::{Decoder, DecodeResult};
use instruction::*;

enum MatchError {
    Internal,
    Mismatch,
}

const ANY_EA: u16     = 0b111111111111;
const DATA: u16       = 0b111111111101;
const MEMORY: u16     = 0b111111111100;
const ALTERABLE: u16  = 0b000111111111;
const CONTROL: u16    = 0b011111100100;

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

struct EASpec(u16, u8);

fn reg_no(reg: u16) -> Result<u8, MatchError> {
    if (reg & 0b111) == reg { Ok(reg as u8) }
    else                    { Err(MatchError::Internal) }
}

fn decode_ea(mode: u16, reg: u16) -> Result<EASpec, MatchError> {
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

#[derive(Copy, Clone)]
enum Matcher {
    Any(u8),
    Word(u16),
    Nibble(u8),
    Bit(u8),
    Size,
    EA(u16),
    AE(u16),
}

impl Matcher {
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
        Matcher::EA(mask) => match decode_ea(x >> 3, x & 7) {
            Ok(ea) => (mask & ea.0) != 0,
            Err(_) => false,
            },
        Matcher::AE(mask) => match decode_ea(x & 7, x >> 3) {
            Ok(ea) => (mask & ea.0) != 0,
            Err(_) => false,
            },
        }
    }
}

macro_rules! decoder {
    ($matchers:expr, $body:expr) => (
        SingleDecoder::new(
            $matchers.iter().cloned().collect(),
            |pc| { $body }
        )
    )
}

fn all_decoders() -> Vec<SingleDecoder> { vec![
    decoder!([ // ADD <ea>, Dn
        Matcher::Nibble(0b1101),
        Matcher::Any(3),
        Matcher::Bit(0),
        Matcher::Size,
        Matcher::EA(ANY_EA)
    ], {
        Ok((Instruction::ADD_to_Data(Size::Long, EA::DataDirect(4), 4), 1))
    }),
] }

type DecodeFn = Box<Fn(&[u16]) -> DecodeResult<MatchError>>;

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
            println!("{:?} == {:?} ?", result, i);
            assert!(result == i);
        },
        Err(_) => { panic!(); },
    }
}

#[test]
fn test_decoders() {
    let d = CarefulDecoder::new();

    // some simple decodes of common instructions
    check_decode(&d,
        &[0b1101_100_010_000100],
        ADD_to_Data(Size::Long, EA::DataDirect(4), 4)
    );
}
