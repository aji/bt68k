extern crate bt68k;

use bt68k::exec::*;
use bt68k::exec::interpret::*;

fn run_test<F, M>(name: &'static str, code: M, verify: F)
where F: Fn(&Interpreter<M>) -> bool, M: Memory + Copy {
    println!("Testing {}...", name);

    let ok = {
        let mut ee = Interpreter::new(CPU::new(), code, false);
        ee.execute();
        verify(&ee)
    };

    if !ok {
        println!("Failed! Re-running with debug");
        let mut ee = Interpreter::new(CPU::new(), code, true);
        ee.execute();
        println!("{}", ee.cpu());
        panic!("test failure");
    }
}

macro_rules! test {
    ($name:expr, $prog:expr, $func:expr) => (
        run_test($name, &$prog[..], $func)
    )
}

pub fn main() {
-- tests go here --
}
