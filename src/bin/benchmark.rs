extern crate bt68k;
extern crate time;

use bt68k::exec::*;
use bt68k::exec::interpret::*;

fn timed<F: FnMut()>(name: &'static str, mut target: F) {
    let start = time::now();
    target();
    let duration = time::now() - start;
    println!("{} finished in {}ms", name, duration.num_milliseconds());
}

pub fn main() {
    let mut rom = [
        0xd482,         // addl d2, d2
        0x4efa, 0xfffc, // jmp -4(pc)
    ];

    let mut ee = Interpreter::new(CPU::new(), &mut rom[..], false);

    timed("100,000 loops (200,000 instructions)", || {
        for _ in 0..100_000 { ee.execute(); }
    });

    println!("{}", ee.cpu());
}
