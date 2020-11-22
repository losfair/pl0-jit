use dynasmrt::{ExecutableBuffer, AssemblyOffset};

global_asm!(include_str!("runtime.asm"));

extern "C" {
    fn enter_exec(target: *const u8) -> u64;
}

pub struct Executable {
    pub(crate) buffer: ExecutableBuffer,
}

impl Executable {
    pub fn run(&self) -> u64 {
        let entry = self.buffer.ptr(AssemblyOffset(0));
        unsafe {
            enter_exec(entry)
        }
    }
}
