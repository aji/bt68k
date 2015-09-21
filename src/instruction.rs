pub use self::Instruction::*;

pub type Dx = u8;
pub type Dy = u8;
pub type Dn = u8;

pub type Ax = u8;
pub type Ay = u8;
pub type An = u8;

pub type RegMask = u16;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Index {
    Data(Size, Dn),
    Addr(Size, An),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Size {
    Byte,
    Word,
    Long
}

impl Size {
    pub fn size(self) -> usize {
        match self {
            Size::Byte => 1,
            Size::Word => 2,
            Size::Long => 4,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Dir {
    Left,
    Right,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum EA {
    DataDirect(Dn),
    AddrDirect(An),
    AddrIndirect(An),
    AddrPostInc(An),
    AddrPreDec(An),
    AddrDisplace(An, i16),
    AddrIndex(An, Index, i8),
    AbsWord(i16),
    AbsLong(u32),
    PcDisplace(i16),
    PcIndex(Index, i8),
    ImmByte(u8),
    ImmWord(u16),
    ImmLong(u32),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Cond {
    True,
    False,
    High,
    LowOrSame,
    HighOrSame,
    Low,
    NotEqual,
    Equal,
    OverflowClear,
    OverflowSet,
    Plus,
    Minus,
    GreaterOrEqual,
    LessThan,
    GreaterThan,
    LessOrEqual
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instruction {
    ABCD_Data(Dy, Dx),
    ABCD_Addr(Ay, Ax),
    ADD_to_Data(Size, EA, Dn),
    ADD_to_EA(Size, Dn, EA),
    ADDA(Size, EA, An),
    ADDI(Size, u32, EA),
    ADDQ(Size, u8, EA),
    ADDX_Data(Size, Dy, Dx),
    ADDX_Addr(Size, Ay, Ax),
    AND_to_Data(Size, EA, Dn),
    AND_to_EA(Size, Dn, EA),
    ANDI(Size, u32, EA),
    ANDI_to_CCR(u8),
    ANDI_to_SR(u16),
    ASd_Data(Size, Dir, Dx, Dy),
    ASd_to_Data(Size, Dir, u8, Dy),
    ASd_EA(Dir, EA),
    Bcc(Cond, i16),
    BCHG_Data(Dn, EA),
    BCHG_Imm(u8, EA),
    BCLR_Data(Dn, EA),
    BCLR_Imm(u8, EA),
    BRA(i16),
    BSET_Data(Dn, EA),
    BSET_Imm(u8, EA),
    BSR(i16),
    BTST_Data(Dn, EA),
    BTST_Imm(u8, EA),
    CHK(EA, Dn),
    CLR(Size, EA),
    CMP(Size, EA, Dn),
    CMPA(Size, EA, An),
    CMPI(Size, u32, EA),
    CMPM(Size, Ay, Ax),
    DBcc(Cond, Dn, i16),
    DIVS(EA, Dn),
    DIVU(EA, Dn),
    EOR(Size, Dn, EA),
    EORI(Size, u32, EA),
    EORI_to_CCR(u8),
    EORI_to_SR(u16),
    EXG_Data(Dx, Dy),
    EXG_Addr(Ax, Ay),
    EXG_Both(Dx, Ay),
    EXT(Size, Dn),
    ILLEGAL,
    JMP(EA),
    JSR(EA),
    LEA(EA, An),
    LINK(An, i16),
    LSd_Data(Size, Dir, Dx, Dy),
    LSd_to_Data(Size, Dir, u8, Dy),
    LSd_EA(Dir, EA),
    MOVE(Size, EA, EA),
    MOVE_from_CCR(EA),
    MOVE_to_CCR(EA),
    MOVE_to_SR(EA),
    MOVE_from_SR(EA),
    MOVE_from_USP(An),
    MOVE_to_USP(An),
    MOVEA(Size, EA, An),
    MOVEM_Save(Size, RegMask, EA),
    MOVEM_Load(Size, EA, RegMask),
    MOVEP_Save(Size, Dx, i16, Ay),
    MOVEP_Load(Size, i16, Ay, Dx),
    MOVEQ(i8, Dn),
    MULS(EA, Dn),
    MULU(EA, Dn),
    NBCD(EA),
    NEG(Size, EA),
    NEGX(Size, EA),
    NOP,
    NOT(Size, EA),
    OR_to_Data(Size, EA, Dn),
    OR_to_EA(Size, Dn, EA),
    ORI(Size, u32, EA),
    ORI_to_CCR(u8),
    ORI_to_SR(u16),
    PEA(EA),
    RESET,
    ROd_Data(Size, Dir, Dx, Dy),
    ROd_to_Data(Size, Dir, u8, Dy),
    ROd_EA(Dir, EA),
    ROXd_Data(Size, Dir, Dx, Dy),
    ROXd_to_Data(Size, Dir, u8, Dy),
    ROXd_EA(Dir, EA),
    RTE,
    RTR,
    RTS,
    SBCD_Data(Dy, Dx),
    SBCD_Addr(Ay, Ax),
    Scc(Cond, EA),
    STOP(u16),
    SUB_to_Data(Size, EA, Dn),
    SUB_to_EA(Size, Dn, EA),
    SUBA(Size, EA, An),
    SUBI(Size, u32, EA),
    SUBQ(Size, u8, EA),
    SUBX_Data(Size, Dy, Dx),
    SUBX_Addr(Size, Ay, Ax),
    SWAP(Dn),
    TAS(EA),
    TRAP(u8),
    TRAPV,
    TST(Size, EA),
    UNLK(An),
}

pub trait Immediate {
    fn as_u8(self)   -> u8;
    fn as_u16(self)  -> u16;
    fn as_u32(self)  -> u32;
}

impl Immediate for u8 {
    fn as_u8(self)   -> u8   { self as u8  }
    fn as_u16(self)  -> u16  { self as u16 }
    fn as_u32(self)  -> u32  { self as u32 }
}

impl Immediate for u16 {
    fn as_u8(self)   -> u8   { self as u8  }
    fn as_u16(self)  -> u16  { self as u16 }
    fn as_u32(self)  -> u32  { self as u32 }
}

impl Immediate for u32 {
    fn as_u8(self)   -> u8   { self as u8  }
    fn as_u16(self)  -> u16  { self as u16 }
    fn as_u32(self)  -> u32  { self as u32 }
}

impl EA {
    pub fn imm_of_size<T: Immediate>(sz: Size, x: T) -> EA {
        match sz {
            Size::Byte  => EA::ImmByte(x.as_u8()),
            Size::Word  => EA::ImmWord(x.as_u16()),
            Size::Long  => EA::ImmLong(x.as_u32()),
        }
    }
}
