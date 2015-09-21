// decode/prefix.rs -- Prefix-based instruction decoder
// Copyright (C) 2015 Alex Iadicicco

/// With the 68000 instruction set, we can look at the first 4 bits of an
/// encoded instruction word and significantly reduce the number of possible
/// decodings.

pub use decode::{Decoder, DecodeResult};
pub use decode::common::*;
use instruction::*;

pub struct PrefixDecoder;

impl PrefixDecoder {
    pub fn new() -> PrefixDecoder {
        PrefixDecoder
    }
}

impl Decoder<()> for PrefixDecoder {
    fn decode(&self, pc: &[u16]) -> DecodeResult {
        match pc[0] >> 12 {
        0b0000 => bit_manip_movep_imm(pc),
        0b0001 => move_size(pc),
        0b0010 => move_size(pc),
        0b0011 => move_size(pc),
        0b0100 => miscellaneous(pc),
        0b0101 => addq_subq_scc_dbcc(pc),
        0b0110 => bcc_bsr(pc),
        0b0111 => moveq(pc),
        0b1000 => or_div_sbcd(pc),
        0b1001 => sub_subx(pc),
        0b1010 => Err(()),
        0b1011 => cmp_eor(pc),
        0b1100 => and_mul_abcd_exg(pc),
        0b1101 => add_addx(pc),
        0b1110 => shift_rotate(pc),
        0b1111 => Err(()),

        _ => Err(())
        }
    }
}

macro_rules! unwrap {
    ($value:expr) => {
        match $value {
            Some(x) => x,
            None    => return Err(())
        }
    }
}

fn bit_manip_movep_imm(_pc: &[u16]) -> DecodeResult { Err(()) }

fn move_size(pc: &[u16]) -> DecodeResult {
    let mut len = 1;

    let size = match (pc[0] >> 12) & 0x3 {
        0b01 => Size::Byte,
        0b11 => Size::Word,
        0b10 => Size::Long,
        _ => return Err(()),
    };

    // source comes first!
    let src = unwrap!(decode_ea(pc, 0, size, &mut len));

    if triple(pc, 6) == 0b001 {
        Ok((MOVEA(size, src, triple(pc, 9)), len))
    } else {
        let dst = unwrap!(decode_ae(pc, 6, size, &mut len));
        Ok((MOVE(size, src, dst), len))
    }
}

fn miscellaneous(_pc: &[u16]) -> DecodeResult { Err(()) }

fn addq_subq_scc_dbcc(_pc: &[u16]) -> DecodeResult { Err(()) }

fn bcc_bsr(_pc: &[u16]) -> DecodeResult { Err(()) }

fn moveq(_pc: &[u16]) -> DecodeResult { Err(()) }

fn or_div_sbcd(_pc: &[u16]) -> DecodeResult { Err(()) }

fn sub_subx(_pc: &[u16]) -> DecodeResult { Err(()) }

fn cmp_eor(_pc: &[u16]) -> DecodeResult { Err(()) }

fn and_mul_abcd_exg(_pc: &[u16]) -> DecodeResult { Err(()) }

fn add_addx(_pc: &[u16]) -> DecodeResult { Err(()) }

fn shift_rotate(_pc: &[u16]) -> DecodeResult { Err(()) }

#[test]
fn verify_all_patterns() {
    use decode::careful::CarefulDecoder;

    let reference = CarefulDecoder::new();
    let target = PrefixDecoder::new();

    for i in 0..0x10000 {
        // The other elements are to verify that any extension words are decoded
        // identically as well.
        let pc = [i as u16, 1, 2, 3, 4, 5, 6];

        let rd = reference.decode(&pc);
        let td = target.decode(&pc);

        if rd.is_err() || td.is_err() {
            // TODO: Some decoder indicated they were unable to decode the
            // instruction. Right now we only care about the cases where both
            // decoders work, but eventually we'll want to fail when the
            // reference decodes something and the target does not.
            continue;
        }

        let (ri, rlen) = rd.unwrap();
        let (ti, tlen) = td.unwrap();

        println!("0b{:016b}:", i);
        println!("  ref: {:?} len={}", ri, rlen);
        println!("  tgt: {:?} len={}", ti, tlen);

        assert!(ri == ti);
        assert!(rlen == tlen);
    }
}
