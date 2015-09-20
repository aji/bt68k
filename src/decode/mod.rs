use instruction;

pub mod careful;
pub mod prefix;

pub type DecodeResult<E> = Result<(instruction::Instruction, usize), E>;

pub trait Decoder<E> {
    fn decode(&self, pc: &[u16]) -> DecodeResult<E>;
}
