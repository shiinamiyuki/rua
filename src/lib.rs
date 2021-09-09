pub mod bytecode;
pub mod closure;
pub mod compile;
pub mod gc;
pub mod parse;
pub mod runtime;
pub mod state;
pub mod table;
pub mod value;
pub mod vm;

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
