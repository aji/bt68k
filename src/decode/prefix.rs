// decode/prefix.rs -- Prefix-based instruction decoder
// Copyright (C) 2015 Alex Iadicicco

//! With the 68000 instruction set, we can look at the first 4 bits of an
//! encoded instruction word and significantly reduce the number of possible
//! decodings.

pub use decode::{Decoder, DecodeResult};
pub use decode::common::*;
use instruction::*;

/// The prefix decoder speeds up instruction decode time by looking at the first
/// nibble of the instruction word to significantly narrow down the number of
/// possible decodings.
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
        0b0001 => move_byte(pc),
        0b0010 => move_long(pc),
        0b0011 => move_word(pc),
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

fn bit_manip_movep_imm(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    if bit(pc, 8) == 0 {
        match triple(pc, 9) {
            0b000 => {
                if pc[0] ==        0b0000000000111100 {
                    let imm = immediate(pc, Size::Byte, &mut len);
                    Ok((ORI_to_CCR(imm as u8), len))
                } else if pc[0] == 0b0000000001111100 {
                    let imm = immediate(pc, Size::Byte, &mut len);
                    Ok((ORI_to_SR(imm as u16), len))
                } else {
                    let size = unwrap!(decode_size(pc, 6));
                    let imm = immediate(pc, size, &mut len);
                    let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
                    Ok((ORI(size, imm, ea), len))
                }
            },
            0b001 => {
                if pc[0] ==        0b0000001000111100 {
                    let imm = immediate(pc, Size::Byte, &mut len);
                    Ok((ANDI_to_CCR(imm as u8), len))
                } else if pc[0] == 0b0000001001111100 {
                    let imm = immediate(pc, Size::Byte, &mut len);
                    Ok((ANDI_to_SR(imm as u16), len))
                } else {
                    let size = unwrap!(decode_size(pc, 6));
                    let imm = immediate(pc, size, &mut len);
                    let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
                    Ok((ANDI(size, imm, ea), len))
                }
            },
            0b010 => {
                let size = unwrap!(decode_size(pc, 6));
                let imm = immediate(pc, size, &mut len);
                let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
                Ok((SUBI(size, imm, ea), len))
            },
            0b011 => {
                let size = unwrap!(decode_size(pc, 6));
                let imm = immediate(pc, size, &mut len);
                let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
                Ok((ADDI(size, imm, ea), len))
            },
            0b100 => {
                len += 1;
                let ea = unwrap!(decode_ea(pc, 0, Size::Byte, &mut len));
                match pair(pc, 6) {
                    0b00 => Ok((BTST_Imm(pc[1] as u8, ea), len)),
                    0b01 => Ok((BCHG_Imm(pc[1] as u8, ea), len)),
                    0b10 => Ok((BCLR_Imm(pc[1] as u8, ea), len)),
                    0b11 => Ok((BSET_Imm(pc[1] as u8, ea), len)),
                    _ => Err(())
                }
            },
            0b101 => {
                if pc[0] ==        0b0000101000111100 {
                    let imm = immediate(pc, Size::Byte, &mut len);
                    Ok((EORI_to_CCR(imm as u8), len))
                } else if pc[0] == 0b0000101001111100 {
                    let imm = immediate(pc, Size::Byte, &mut len);
                    Ok((EORI_to_SR(imm as u16), len))
                } else {
                    let size = unwrap!(decode_size(pc, 6));
                    let imm = immediate(pc, size, &mut len);
                    let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
                    Ok((EORI(size, imm, ea), len))
                }
            },
            0b110 => {
                let size = unwrap!(decode_size(pc, 6));
                let imm = immediate(pc, size, &mut len);
                let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
                Ok((CMPI(size, imm, ea), len))
            },
            _ => Err(()),
        }
    } else {
        if triple(pc, 3) == 1 {
            let size = if bit(pc, 6) == 0 { Size::Word} else { Size::Long };
            let data = triple(pc, 9);
            let addr = triple(pc, 0);
            if bit(pc, 7) == 1 {
                Ok((MOVEP_Save(size, data, pc[1] as i16, addr), 2))
            } else {
                Ok((MOVEP_Load(size, pc[1] as i16, addr, data), 2))
            }
        } else {
            let ea = unwrap!(decode_ea(pc, 0, Size::Byte, &mut len));
            match pair(pc, 6) {
                0b00 => Ok((BTST_Data(triple(pc, 9), ea), len)),
                0b01 => Ok((BCHG_Data(triple(pc, 9), ea), len)),
                0b10 => Ok((BCLR_Data(triple(pc, 9), ea), len)),
                0b11 => Ok((BSET_Data(triple(pc, 9), ea), len)),
                _ => Err(())
            }
        }
    }
}

fn move_byte(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    let size = Size::Byte;

    // source comes first!
    let src = unwrap!(decode_ea(pc, 0, size, &mut len));
    let dst = unwrap!(decode_ae(pc, 6, size, &mut len));
    Ok((MOVE(size, src, dst), len))
}

fn move_word(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    let size = Size::Word;

    // source comes first!
    let src = unwrap!(decode_ea(pc, 0, size, &mut len));

    if triple(pc, 6) == 0b001 {
        Ok((MOVEA(size, src, triple(pc, 9)), len))
    } else {
        let dst = unwrap!(decode_ae(pc, 6, size, &mut len));
        Ok((MOVE(size, src, dst), len))
    }
}

fn move_long(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    let size = Size::Long;

    // source comes first!
    let src = unwrap!(decode_ea(pc, 0, size, &mut len));

    if triple(pc, 6) == 0b001 {
        Ok((MOVEA(size, src, triple(pc, 9)), len))
    } else {
        let dst = unwrap!(decode_ae(pc, 6, size, &mut len));
        Ok((MOVE(size, src, dst), len))
    }
}

fn miscellaneous(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    if bit(pc, 8) == 1 {
        let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
        return if bit(pc, 6) == 0 {
            Ok((CHK(ea, triple(pc, 9)), len))
        } else {
            Ok((LEA(ea, triple(pc, 9)), len))
        }
    }
    match triple(pc, 9) {
        0b000 => {
            let size = unwrap!(decode_size(pc, 6));
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            Ok((NEGX(size, ea), len))
        },
        0b001 => {
            let size = unwrap!(decode_size(pc, 6));
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            Ok((CLR(size, ea), len))
        },
        0b010 => if pair(pc, 6) == 0b11 {
            let ea = unwrap!(decode_ea(pc, 0, Size::Byte, &mut len));
            Ok((MOVE_to_CCR(ea), len))
        } else {
            let size = unwrap!(decode_size(pc, 6));
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            Ok((NEG(size, ea), len))
        },
        0b011 => if pair(pc, 6) == 0b11 {
            let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
            Ok((MOVE_to_SR(ea), len))
        } else {
            let size = unwrap!(decode_size(pc, 6));
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            Ok((NOT(size, ea), len))
        },
        0b100 => match pair(pc, 6) {
            0b00 => {
                let ea = unwrap!(decode_ea(pc, 0, Size::Byte, &mut len));
                Ok((NBCD(ea), len))
            },
            0b01 => if triple(pc, 3) == 0 {
                Ok((SWAP(triple(pc, 0)), 1))
            } else {
                let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
                Ok((PEA(ea), len))
            },
            _ => {
                let size = if bit(pc, 6) == 0 { Size::Word } else { Size::Long };
                if triple(pc, 3) == 0 {
                    Ok((EXT(size, triple(pc, 0)), 1))
                } else {
                    len += 1;
                    let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
                    Ok((MOVEM_Save(size, pc[1], ea), len))
                }
            }
        },
        0b101 => if pair(pc, 6) == 0b11 {
            if pc[0] == 0b0100101011111100 {
                Ok((ILLEGAL, 1))
            } else {
                let ea = unwrap!(decode_ea(pc, 0, Size::Byte, &mut len));
                Ok((TAS(ea), len))
            }
        } else {
            let size = unwrap!(decode_size(pc, 6));
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            Ok((TST(size, ea), len))
        },
        0b110 => {
            len += 1;
            let size = if bit(pc, 6) == 0 { Size::Word } else { Size::Long };
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            Ok((MOVEM_Load(size, ea, pc[1]), len))
        },
        0b111 => match pair(pc, 6) {
            0b01 => match triple(pc, 3) {
                0b010 => Ok((LINK(triple(pc, 0), pc[1] as i16), 2)),
                0b011 => Ok((UNLK(triple(pc, 0)), 1)),
                0b100 => Ok((MOVE_to_USP(triple(pc, 0)), 1)),
                0b101 => Ok((MOVE_from_USP(triple(pc, 0)), 1)),
                0b110 => match triple(pc, 0) {
                    0b000 => Ok((RESET, 1)),
                    0b001 => Ok((NOP, 1)),
                    0b010 => Ok((STOP(pc[1]), 2)),
                    0b011 => Ok((RTE, 1)),
                    0b101 => Ok((RTS, 1)),
                    0b110 => Ok((TRAPV, 1)),
                    0b111 => Ok((RTR, 1)),
                    _ => Err(()),
                },
                _ => Ok((TRAP((pc[0] & 0xf) as u8), 1))
            },
            0b10 => {
                let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
                Ok((JSR(ea), len))
            },
            0b11 => {
                let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
                Ok((JMP(ea), len))
            },
            _ => Err(())
        },
        _ => Err(()),
    }
}

fn addq_subq_scc_dbcc(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    if pair(pc, 6) == 0b11 {
        let cond = unwrap!(decode_cond(pc, 8));
        if triple(pc, 3) == 1 {
            Ok((DBcc(cond, triple(pc, 0), pc[1] as i16), 2))
        } else {
            let ea = unwrap!(decode_ea(pc, 0, Size::Byte, &mut len));
            Ok((Scc(cond, ea), len))
        }
    } else {
        let size = unwrap!(decode_size(pc, 6));
        let imm = triple(pc, 9);
        let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
        if bit(pc, 8) == 0 {
            Ok((ADDQ(size, imm, ea), len))
        } else {
            Ok((SUBQ(size, imm, ea), len))
        }
    }
}

fn bcc_bsr(pc: &[u16]) -> DecodeResult {
    let (disp, len) = {
        let d = (pc[0] & 0xff) as i8;
        if d == 0 { (pc[1] as i16, 2) } else { (d as i16, 1) }
    };

    match (pc[0] >> 8) & 0xf {
        0b0000 => Ok((BRA(disp), len)),
        0b0001 => Ok((BSR(disp), len)),
        _ => Ok((Bcc(decode_cond(pc, 8).unwrap(), disp), len)),
    }
}

fn moveq(pc: &[u16]) -> DecodeResult {
    Ok((MOVEQ((pc[0] & 0xff) as i8, triple(pc, 9)), 1))
}

fn or_div_sbcd(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    if pair(pc, 6) == 0b11 {
        let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
        if bit(pc, 8) == 0 {
            Ok((DIVU(ea, triple(pc, 9)), len))
        } else {
            Ok((DIVS(ea, triple(pc, 9)), len))
        }
    } else {
        let reg = triple(pc, 9);
        let mode = triple(pc, 3);
        if triple(pc, 6) == 0b100 && (mode == 0 || mode == 1) {
            let reg2 = triple(pc, 0);
            if mode == 0 {
                Ok((SBCD_Data(reg2, reg), 1))
            } else {
                Ok((SBCD_Addr(reg2, reg), 1))
            }
        } else {
            let size = unwrap!(decode_size(pc, 6));
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            if bit(pc, 8) == 0 {
                Ok((OR_to_Data(size, ea, reg), len))
            } else {
                Ok((OR_to_EA(size, reg, ea), len))
            }
        }
    }
}

fn sub_subx(pc: &[u16]) -> DecodeResult {
    let mut len = 1;

    if pair(pc, 6) == 0b11 {
        let size = if bit(pc, 8) == 0 { Size::Word } else { Size::Long };
        let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
        Ok((SUBA(size, ea, triple(pc, 9)), len))
    } else {
        let reg = triple(pc, 9);
        let flag = bit(pc, 8);
        let size = unwrap!(decode_size(pc, 6));
        let mode = triple(pc, 3);
        if flag == 1 && (mode == 0 || mode == 1) {
            let reg2 = triple(pc, 0);
            if mode == 0 {
                Ok((SUBX_Data(size, reg2, reg), 1))
            } else {
                Ok((SUBX_Addr(size, reg2, reg), 1))
            }
        } else {
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            if flag == 0 {
                Ok((SUB_to_Data(size, ea, reg), len))
            } else {
                Ok((SUB_to_EA(size, reg, ea), len))
            }
        }
    }
}

fn cmp_eor(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    match triple(pc, 6) {
        0b000 => {
            let ea = unwrap!(decode_ea(pc, 0, Size::Byte, &mut len));
            Ok((CMP(Size::Byte, ea, triple(pc, 9)), len))
        },
        0b001 => {
            let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
            Ok((CMP(Size::Word, ea, triple(pc, 9)), len))
        },
        0b010 => {
            let ea = unwrap!(decode_ea(pc, 0, Size::Long, &mut len));
            Ok((CMP(Size::Long, ea, triple(pc, 9)), len))
        },
        0b011 => {
            let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
            Ok((CMPA(Size::Word, ea, triple(pc, 9)), len))
        },
        0b100 => {
            if triple(pc, 3) == 1 {
                Ok((CMPM(Size::Byte, triple(pc, 0), triple(pc, 9)), 1))
            } else {
                let ea = unwrap!(decode_ea(pc, 0, Size::Byte, &mut len));
                Ok((EOR(Size::Byte, triple(pc, 9), ea), len))
            }
        },
        0b101 => {
            if triple(pc, 3) == 1 {
                Ok((CMPM(Size::Word, triple(pc, 0), triple(pc, 9)), 1))
            } else {
                let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
                Ok((EOR(Size::Word, triple(pc, 9), ea), len))
            }
        },
        0b110 => {
            if triple(pc, 3) == 1 {
                Ok((CMPM(Size::Long, triple(pc, 0), triple(pc, 9)), 1))
            } else {
                let ea = unwrap!(decode_ea(pc, 0, Size::Long, &mut len));
                Ok((EOR(Size::Long, triple(pc, 9), ea), len))
            }
        },
        0b111 => {
            let ea = unwrap!(decode_ea(pc, 0, Size::Long, &mut len));
            Ok((CMPA(Size::Long, ea, triple(pc, 9)), len))
        },
        _ => Err(()),
    }
}

fn and_mul_abcd_exg(pc: &[u16]) -> DecodeResult {
    let mut len = 1;
    if pair(pc, 6) == 0b11 {
        let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
        if bit(pc, 8) == 0 {
            Ok((MULU(ea, triple(pc, 9)), len))
        } else {
            Ok((MULS(ea, triple(pc, 9)), len))
        }
    } else {
        let reg = triple(pc, 9);
        let reg2 = triple(pc, 0);
        let mode = triple(pc, 3);
        if triple(pc, 6) == 0b100 && (mode == 0 || mode == 1) {
            if mode == 0 {
                Ok((ABCD_Data(reg2, reg), 1))
            } else {
                Ok((ABCD_Addr(reg2, reg), 1))
            }
        } else if triple(pc, 6) == 0b101 && mode == 0b000 {
            Ok((EXG_Data(reg, reg2), 1))
        } else if triple(pc, 6) == 0b101 && mode == 0b001 {
            Ok((EXG_Addr(reg, reg2), 1))
        } else if triple(pc, 6) == 0b110 && mode == 0b001 {
            Ok((EXG_Both(reg, reg2), 1))
        } else {
            let size = unwrap!(decode_size(pc, 6));
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            if bit(pc, 8) == 0 {
                Ok((AND_to_Data(size, ea, reg), len))
            } else {
                Ok((AND_to_EA(size, reg, ea), len))
            }
        }
    }
}

fn add_addx(pc: &[u16]) -> DecodeResult {
    let mut len = 1;

    if pair(pc, 6) == 0b11 {
        let size = if bit(pc, 8) == 0 { Size::Word } else { Size::Long };
        let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
        Ok((ADDA(size, ea, triple(pc, 9)), len))
    } else {
        let reg = triple(pc, 9);
        let flag = bit(pc, 8);
        let size = unwrap!(decode_size(pc, 6));
        let mode = triple(pc, 3);
        if flag == 1 && (mode == 0 || mode == 1) {
            let reg2 = triple(pc, 0);
            if mode == 0 {
                Ok((ADDX_Data(size, reg2, reg), 1))
            } else {
                Ok((ADDX_Addr(size, reg2, reg), 1))
            }
        } else {
            let ea = unwrap!(decode_ea(pc, 0, size, &mut len));
            if flag == 0 {
                Ok((ADD_to_Data(size, ea, reg), len))
            } else {
                Ok((ADD_to_EA(size, reg, ea), len))
            }
        }
    }
}

fn shift_rotate(pc: &[u16]) -> DecodeResult {
    let dir = if bit(pc, 8) == 0 { Dir::Right } else { Dir::Left };
    if pair(pc, 6) == 0b11 {
        let mut len = 1;
        let ea = unwrap!(decode_ea(pc, 0, Size::Word, &mut len));
        match pair(pc, 9) {
            0b00 => Ok(( ASd_EA(dir, ea), len)),
            0b01 => Ok(( LSd_EA(dir, ea), len)),
            0b10 => Ok((ROXd_EA(dir, ea), len)),
            0b11 => Ok(( ROd_EA(dir, ea), len)),
            _ => Err(()),
        }
    } else {
        let size = decode_size(pc, 6).unwrap();
        let by = triple(pc, 9);
        let target = triple(pc, 0);
        if bit(pc, 5) == 0 {
            match pair(pc, 3) {
                0b00 => Ok(( ASd_to_Data(size, dir, by, target), 1)),
                0b01 => Ok(( LSd_to_Data(size, dir, by, target), 1)),
                0b10 => Ok((ROXd_to_Data(size, dir, by, target), 1)),
                0b11 => Ok(( ROd_to_Data(size, dir, by, target), 1)),
                _ => Err(()),
            }
        } else {
            match pair(pc, 3) {
                0b00 => Ok(( ASd_Data(size, dir, by, target), 1)),
                0b01 => Ok(( LSd_Data(size, dir, by, target), 1)),
                0b10 => Ok((ROXd_Data(size, dir, by, target), 1)),
                0b11 => Ok(( ROd_Data(size, dir, by, target), 1)),
                _ => Err(()),
            }
        }
    }
}

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

        // Skip patterns the reference doesn't know how to decode
        if rd.is_err() { continue; }

        println!("0b{:016b}:", i);

        let (ri, rlen) = rd.unwrap();
        println!("  ref: {:?} len={}", ri, rlen);

        let (ti, tlen) = td.unwrap();
        println!("  tgt: {:?} len={}", ti, tlen);

        assert!(ri == ti);
        assert!(rlen == tlen);
    }
}
