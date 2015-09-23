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
test!("just move",[0x203c,0x0000,0x1234,0x4efa,0xfffe],|ee|{
	(ee.cpu().data[0]) == 0x1234

});
test!("just move, again",[0x203c,0x1234,0x5678,0x2200,0x4efa,0xfffe],|ee|{
	(ee.cpu().data[1]) == 0x12345678

});
test!("move word affects lower word only",[0x203c,0x1234,0x5678,0x223c,0xabcd,0xefab,0x3200,0x4efa,0xfffe],|ee|{
	(ee.cpu().data[1]) == 0xabcd5678

});
test!("move byte affects lower byte only",[0x203c,0x1234,0x5678,0x223c,0xabcd,0xefab,0x1200,0x4efa,0xfffe],|ee|{
	(ee.cpu().data[1]) == 0xabcdef78

});
test!("just movea",[0x207c,0x1234,0x5678,0x4efa,0xfffe],|ee|{
	(ee.cpu().addr[0]) == 0x12345678

});
test!("just movea, again",[0x207c,0x1234,0x5678,0x2248,0x4efa,0xfffe],|ee|{
	(ee.cpu().addr[1]) == 0x12345678

});
test!("movea word affects whole register",[0x207c,0x1234,0x5678,0x227c,0xabcd,0xefab,0x3248,0x4efa,0xfffe],|ee|{
	(ee.cpu().addr[1]) == 0x00005678

});
test!("move to ccr",[0x44fc,0x0007,0x4efa,0xfffe],|ee|{
	((ee.cpu().status) & 0xff) == 0x07

});
test!("move to sr",[0x46fc,0x7777,0x4efa,0xfffe],|ee|{
	(ee.cpu().status) == 0x7777

});
test!("addl (3 + 5)",[0x7003,0x7205,0xd081,0x4efa,0xfffe],|ee|{
	(ee.cpu().data[0]) == 8
});
}
