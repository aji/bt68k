use instruction;

#[cfg(test)]
pub mod careful;

pub mod common;
pub mod prefix;

pub type DecodeResult = Result<(instruction::Instruction, usize), ()>;

pub trait Decoder<E> {
    fn decode(&self, pc: &[u16]) -> DecodeResult;
}
