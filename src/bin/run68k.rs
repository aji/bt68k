extern crate bt68k;

use bt68k::exec::*;
use bt68k::exec::interpret::*;

pub fn main() {
    let mut rom = [
        0x7401,         // moveq #1, d2
        0xd482,         // addl d2, d2
        0x4efa, 0xfffc, // jmp -4(pc)
    ];

    let mut ee = Interpreter::new(CPU::new(), &mut rom[..]);

    println!("running 35 loops of the CPU");
    for _ in 0..35 {
        ee.execute();
        println!("{}", ee.cpu());
    }
}
