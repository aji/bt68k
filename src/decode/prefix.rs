// decode/prefix.rs -- Prefix-based instruction decoder
// Copyright (C) 2015 Alex Iadicicco

/// With the 68000 instruction set, we can look at the first 4 bits of an
/// encoded instruction word and significantly reduce the number of possible
/// decodings.

pub use decode::{Decoder, DecodeResult};
use instruction::*;

pub struct PrefixDecoder;

impl PrefixDecoder {
    pub fn new() -> PrefixDecoder {
        PrefixDecoder
    }
}

impl Decoder<()> for PrefixDecoder {
    fn decode(&self, pc: &[u16]) -> DecodeResult<()> {
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

fn bit_manip_movep_imm(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn move_size(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn miscellaneous(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn addq_subq_scc_dbcc(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn bcc_bsr(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn moveq(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn or_div_sbcd(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn sub_subx(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn cmp_eor(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn and_mul_abcd_exg(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn add_addx(pc: &[u16]) -> DecodeResult<()> { Err(()) }

fn shift_rotate(pc: &[u16]) -> DecodeResult<()> { Err(()) }
