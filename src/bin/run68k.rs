extern crate bt68k;

use bt68k::exec::*;
use bt68k::exec::interpret::*;

pub fn main() {
    let mut rom = [
        0x7401,         // moveq #1, d2
        0x7001,         // moveq #1, d0
        0xd482,         // addl d2, d2
        0xd482,         // addl d2, d2
        0xd482,         // addl d2, d2
        0xd482,         // addl d2, d2
        0xd482,         // addl d2, d2
        0xd482,         // addl d2, d2
        0xd482,         // addl d2, d2
        0xd482,         // addl d2, d2
        0xd480,         // addl d0, d2
        0xd402,         // addb d2, d2
        0x4efa, 0xfffc, // jmp -4(pc)
    ];

    let mut ee = Interpreter::new(CPU::new(), &mut rom[..], true);

    println!("running 9 loops of the CPU");
    for _ in 0..9 {
        ee.execute();
        println!("{}", ee.cpu());
    }
}
