/// An orthogonal version of the 68000 instruction set. Capable of representing
/// more situations than the 

use instruction::*;

pub struct Ortho {
    pub size: Option<Size>,
    pub opc: Opcode
}

pub enum Op {
    EA(EA),
    CCR,
    SR,
    USP,
    Disp(i16)
}

pub enum Opcode {
    ABCD     (Op, Op),
    ADD      (Op, Op),
    ADDX     (Op, Op),
    AND      (Op, Op),
    ASd      (Dir, Op, Op),
    Bcc      (Cond, Op),
    BCHG     (Op, Op),
    BCLR     (Op, Op),
    BRA      (Op),
    BSET     (Op, Op),
    BSR      (Op),
    BTST     (Op, Op),
    CHK      (Op, Op),
    CLR      (Op),
    CMP      (Op, Op),
    DBcc     (Cond, Op, Op),
    DIVS     (Op, Op),
    DIVU     (Op, Op),
    EOR      (Op, Op),
    EXG      (Op, Op),
    EXT      (Op),
    ILLEGAL,
    JMP      (Op),
    JSR      (Op),
    LOp      (Op, Op),
    LINK     (Op, Op),
    LSd      (Dir, Op, Op),
    MOVE     (Op, Op),
    MOVEM,
    MOVEP,
    MULS     (Op, Op),
    MULU     (Op, Op),
    NBCD     (Op),
    NEG      (Op),
    NEGX     (Op),
    NOP,
    NOT      (Op),
    OR       (Op, Op),
    POp      (Op),
    RESET,
    ROd      (Dir, Op, Op),
    ROXd     (Dir, Op, Op),
    RTE,
    RTR,
    RTS,
    Scc      (Cond, Op),
    STOP     (Op),
    SUB      (Op, Op),
    SUBX     (Op, Op),
    SWAP     (Op),
    TAS      (Op),
    TRAP     (Op),
    TRAPV,
    TST      (Op),
    UNLK     (Op),
}

impl Ortho {
    fn from_instruction(i: Instruction) -> Ortho {
        match i {
        ABCD_Data(dy, dx) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::ABCD(
                Op::EA(EA::DataDirect(dy)),
                Op::EA(EA::DataDirect(dx)),
            ),
        },

        ABCD_Addr(ay, ax) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::ABCD(
                Op::EA(EA::AddrPreDec(ay)),
                Op::EA(EA::AddrPreDec(ax)),
            ),
        },

        ADD_to_Data(sz, ea, dn) => Ortho {
            size: Some(sz),
            opc: Opcode::ADD(
                Op::EA(ea),
                Op::EA(EA::DataDirect(dn)),
            ),
        },

        ADD_to_EA(sz, dn, ea) => Ortho {
            size: Some(sz),
            opc: Opcode::ADD(
                Op::EA(EA::DataDirect(dn)),
                Op::EA(ea),
            ),
        },

        ADDA(sz, ea, an) => Ortho {
            size: Some(sz),
            opc: Opcode::ADD(
                Op::EA(ea),
                Op::EA(EA::AddrDirect(an)),
            ),
        },

        ADDI(sz, imm, ea) => Ortho {
            size: Some(sz),
            opc: Opcode::ADD(
                Op::EA(EA::imm_of_size(sz, imm)),
                Op::EA(ea),
            ),
        },

        ADDQ(sz, imm, ea) => Ortho {
            size: Some(sz),
            opc: Opcode::ADD(
                Op::EA(EA::imm_of_size(sz, imm)),
                Op::EA(ea),
            ),
        },

        ADDX_Data(sz, dy, dx) => Ortho {
            size: Some(sz),
            opc: Opcode::ADDX(
                Op::EA(EA::DataDirect(dy)),
                Op::EA(EA::DataDirect(dx)),
            ),
        },

        ADDX_Addr(sz, ay, ax) => Ortho {
            size: Some(sz),
            opc: Opcode::ADDX(
                Op::EA(EA::AddrPreDec(ay)),
                Op::EA(EA::AddrPreDec(ax)),
            ),
        },

        AND_to_Data(sz, ea, dn) => Ortho {
            size: Some(sz),
            opc: Opcode::AND(
                Op::EA(ea),
                Op::EA(EA::DataDirect(dn)),
            ),
        },

        AND_to_EA(sz, dn, ea) => Ortho {
            size: Some(sz),
            opc: Opcode::AND(
                Op::EA(EA::DataDirect(dn)),
                Op::EA(ea)
            ),
        },

        ANDI(sz, imm, ea) => Ortho {
            size: Some(sz),
            opc: Opcode::AND(
                Op::EA(EA::imm_of_size(sz, imm)),
                Op::EA(ea),
            ),
        },

        ANDI_to_CCR(imm) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::AND(
                Op::EA(EA::ImmByte(imm)),
                Op::CCR,
            ),
        },

        ANDI_to_SR(imm) => Ortho {
            size: Some(Size::Word),
            opc: Opcode::AND(
                Op::EA(EA::ImmWord(imm)),
                Op::SR,
            ),
        },

        ASd_Data(sz, dir, dx, dy) => Ortho {
            size: Some(sz),
            opc: Opcode::ASd(
                dir,
                Op::EA(EA::DataDirect(dx)),
                Op::EA(EA::DataDirect(dy)),
            ),
        },

        ASd_to_Data(sz, dir, imm, dy) => Ortho {
            size: Some(sz),
            opc: Opcode::ASd(
                dir,
                Op::EA(EA::ImmByte(imm)),
                Op::EA(EA::DataDirect(dy)),
            ),
        },

        ASd_EA(dir, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::ASd(
                dir,
                Op::EA(EA::ImmByte(1)),
                Op::EA(ea),
            ),
        },

        Bcc(cond, imm) => Ortho {
            size: None,
            opc: Opcode::Bcc(
                cond,
                Op::Disp(imm)
            ),
        },

        BCHG_Data(dn, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::BCHG(
                Op::EA(EA::DataDirect(dn)),
                Op::EA(ea)
            ),
        },

        BCHG_Imm(imm, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::BCLR(
                Op::EA(EA::ImmByte(imm)),
                Op::EA(ea)
            ),
        },

        BCLR_Data(dn, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::BCHG(
                Op::EA(EA::DataDirect(dn)),
                Op::EA(ea)
            ),
        },

        BCLR_Imm(imm, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::BCLR(
                Op::EA(EA::ImmByte(imm)),
                Op::EA(ea)
            ),
        },

        BRA(imm) => Ortho {
            size: None,
            opc: Opcode::BRA(
                Op::Disp(imm)
            ),
        },

        BSET_Data(dn, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::BSET(
                Op::EA(EA::DataDirect(dn)),
                Op::EA(ea)
            ),
        },

        BSET_Imm(imm, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::BSET(
                Op::EA(EA::ImmByte(imm)),
                Op::EA(ea)
            ),
        },

        BSR(imm) => Ortho {
            size: None,
            opc: Opcode::BSR(
                Op::Disp(imm)
            ),
        },

        BTST_Data(dn, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::BTST(
                Op::EA(EA::DataDirect(dn)),
                Op::EA(ea)
            ),
        },

        BTST_Imm(imm, ea) => Ortho {
            size: Some(Size::Byte),
            opc: Opcode::BTST(
                Op::EA(EA::ImmByte(imm)),
                Op::EA(ea)
            ),
        },

        CHK(ea, dn) => Ortho {
            size: Some(Size::Word),
            opc: Opcode::CHK(
                Op::EA(ea),
                Op::EA(EA::DataDirect(dn))
            ),
        },

        CLR(sz, ea) => Ortho {
            size: Some(sz),
            opc: Opcode::CLR(
                Op::EA(ea)
            ),
        },

        CMP(sz, ea, dn) => Ortho {
            size: Some(sz),
            opc: Opcode::CMP(
                Op::EA(ea),
                Op::EA(EA::DataDirect(dn)),
            ),
        },

        CMPA(sz, ea, an) => Ortho {
            size: Some(sz),
            opc: Opcode::CMP(
                Op::EA(ea),
                Op::EA(EA::AddrDirect(an)),
            ),
        },

        CMPI(sz, imm, ea) => Ortho {
            size: Some(sz),
            opc: Opcode::CMP(
                Op::EA(EA::imm_of_size(sz, imm)),
                Op::EA(ea),
            ),
        },

        CMPM(sz, ay, ax) => Ortho {
            size: Some(sz),
            opc: Opcode::CMP(
                Op::EA(EA::AddrPreDec(ay)),
                Op::EA(EA::AddrPreDec(ax)),
            ),
        },

        DBcc(cond, dn, imm) => Ortho {
            size: Some(Size::Word),
            opc: Opcode::DBcc(
                cond,
                Op::EA(EA::DataDirect(dn)),
                Op::Disp(imm)
            ),
        },

        DIVS(ea, dn) => Ortho {
            size: Some(Size::Word),
            opc: Opcode::DIVS(
                Op::EA(ea),
                Op::EA(EA::DataDirect(dn)),
            ),
        },

        DIVU(ea, dn) => Ortho {
            size: Some(Size::Word),
            opc: Opcode::DIVU(
                Op::EA(ea),
                Op::EA(EA::DataDirect(dn)),
            ),
        },

        _ => unimplemented!()
        }
    }
}
