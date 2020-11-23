use dynasmrt::{ExecutableBuffer, AssemblyOffset};

global_asm!(include_str!("runtime.asm"));

extern "C" {
    fn enter_exec(target: *const u8) -> u64;
}

pub struct Executable {
    pub(crate) buffer: ExecutableBuffer,
    pub(crate) entry: AssemblyOffset,
}

impl Executable {
    pub fn run(&self) -> u64 {
        let entry = self.buffer.ptr(self.entry);
        unsafe {
            enter_exec(entry)
        }
    }
}

pub(crate) extern "C" fn do_read() -> u64 {
    let mut s = String::new();
    match std::io::stdin().read_line(&mut s) {
        Ok(_) => {
            match s.trim().parse::<u64>() {
                Ok(x) => x,
                Err(e) => {
                    log::debug!("do_read: parse failed: {:?}", e);
                    0
                }
            }
        }
        Err(e) => {
            log::debug!("do_read: read failed: {:?}", e);
            0
        }
    }
}

pub(crate) extern "C" fn do_write(x: u64) {
    println!("{}", x);
}