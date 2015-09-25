extern crate bt68k;

use bt68k::exec::*;
use bt68k::exec::interpret::*;

#[derive(Clone)]
struct TestMemory {
    rom: Vec<u16>,
    ram: Vec<u16>,
}

impl<'a> Memory for TestMemory {
    fn ref16(&self, addr: u32) -> Option<&u16> {
        if addr > 0xf000 {
            Some(&self.ram[((addr - 0xf000) >> 1) as usize])
        } else if (addr as usize) < self.rom.len() << 1 {
            Some(&self.rom[(addr >> 1) as usize])
        } else {
            None
        }
    }

    fn ref16_mut(&mut self, addr: u32) -> Option<&mut u16> {
        if addr > 0xf000 {
            Some(&mut self.ram[((addr - 0xf000) >> 1) as usize])
        } else {
            None
        }
    }
}

fn run_test<F>(name: &'static str, code: &[u16], verify: F)
where F: Fn(&Interpreter<TestMemory>) -> bool {
    println!("Testing {}...", name);

    let mem = TestMemory {
        rom: code.to_vec(),
        ram: (0..0x0800).map(|_| 0).collect(),
    };

    let ok = {
        let mut ee = Interpreter::new(CPU::new(), mem.clone(), false);
        ee.execute();
        verify(&ee)
    };

    if !ok {
        println!("Failed! Re-running with debug");
        let mut ee = Interpreter::new(CPU::new(), mem.clone(), true);
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
