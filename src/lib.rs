// use crate::value::Value;

pub mod closure;
pub mod compile;
pub mod context;
pub mod error;
pub mod gc;
pub mod parse;
pub mod thread;
pub mod util;
pub mod value;
pub mod vm;
pub mod table;


pub(crate) const fn num_bits<T>() -> usize {
    std::mem::size_of::<T>() * 8
}

pub(crate) fn log_2(x: usize) -> Option<u32> {
    if x == 0 {
        None
    } else {
        Some(num_bits::<usize>() as u32 - x.leading_zeros() - 1)
    }
}


#[macro_export]
macro_rules! debug_println {
    ($($arg:expr),+) => {
        #[cfg(debug_assertions)]
        {
            if let Ok(s) = std::env::var("DPRINT") {
                if s=="1"{
                    println!($($arg),+);
                }
            }
        }
    };
}

pub fn test() {
    use crate::*;
    // use compile::*;
    // use context::*;
    use parse::*;
    // use thread::*;
    let src = std::fs::read_to_string("test.lua").unwrap();
    let tokens = tokenize("test.lua", &src).unwrap();
    let expr = parse_impl(tokens).unwrap();
    println!("{:#?}", expr);
    let module = crate::compile::compile(&vec![expr]).unwrap();
    println!("{:#?}", module);
    let context = context::Context::new();
    let mut thread = thread::Thread::new(context.new_module(module), &[]);
    vm::Vm::execute(&context, &mut thread).unwrap();
}
pub(crate) fn get_3xu8(i: u32) -> [u8; 3] {
    let bytes = i.to_le_bytes();
    assert!(bytes[3] == 0);
    [bytes[0], bytes[1], bytes[2]]
}
pub(crate) fn u32_from_3xu8(bytes: [u8; 3]) -> u32 {
    u32::from_le_bytes([bytes[0], bytes[1], bytes[2], 0])
}

pub(crate) fn protected<F: FnOnce() -> T, T>(f: F) -> T {
    fn handler(sig: std::os::raw::c_int) {
        if sig == libc::SIGSEGV {
            panic!("Segmentation fault");
        }
    }
    let old_h = unsafe { libc::signal(libc::SIGSEGV, handler as usize) };
    let r = f();
    unsafe { libc::signal(libc::SIGSEGV, old_h) };
    r
}
