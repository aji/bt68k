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
test!("just moveq",[0x7002,0x723c,0x74ff,0x4efa,0xfffe],|ee|{
	(ee.cpu().data[0]) ==  2 &&
	(ee.cpu().data[1]) == 60 &&
	(ee.cpu().data[2]) == (-1i32 as u32)

});
test!("addl (3 + 5)",[0x7003,0x7205,0xd081,0x4efa,0xfffe],|ee|{
	(ee.cpu().data[0]) == 8
});
}
