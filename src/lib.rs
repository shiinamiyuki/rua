use std::{alloc::Layout, any::TypeId, cell::UnsafeCell, marker::PhantomData, mem::size_of};

pub mod api;
pub mod bind;
pub mod bytecode;
pub mod closure;
pub mod compile;
pub mod gc;
#[cfg(feature = "complete")]
pub mod na_bind;
pub mod parse;
pub mod runtime;
pub mod state;
pub mod stdlib;
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

pub(crate) struct StackPool<T, const N: usize> {
    pool: Vec<*mut T>,
    last_pos: usize,
}
impl<T, const N: usize> StackPool<T, N> {
    pub(crate) fn new() -> Self {
        Self {
            pool: vec![],
            last_pos: 0,
        }
    }
    pub(crate) fn allocate(&mut self, x: T) -> *mut T {
        unsafe {
            if self.last_pos == N || self.pool.is_empty() {
                self.last_pos = 0;
                let layout = Layout::new::<[T; N]>();
                self.pool
                    .push(std::mem::transmute(std::alloc::alloc(layout)));
                self.allocate(x)
            } else {
                let p = self.pool.last().unwrap().add(self.last_pos);
                std::ptr::write(p, x);
                self.last_pos += 1;
                p
            }
        }
    }
    pub(crate) fn free(&mut self, p: *mut T) {
        unsafe {
            if self.last_pos == 0 {
                assert!(self.pool[self.pool.len() - 2].add(N - 1) == p);
            } else {
                assert!(self.pool.last().unwrap().add(self.last_pos - 1) == p);
            }
            std::ptr::drop_in_place(p);
            if self.last_pos == 0 {
                let last = self.pool.pop().unwrap();
                let layout = Layout::new::<[T; N]>();
                std::alloc::dealloc(std::mem::transmute(last), layout);
                if self.pool.is_empty() {
                    self.last_pos = 0;
                } else {
                    self.last_pos = N - 1;
                }
            } else {
                self.last_pos -= 1;
            }
        }
    }
}
impl<T, const N: usize> Drop for StackPool<T, N> {
    fn drop(&mut self) {
        unsafe {
            let layout = Layout::new::<[T; N]>();

            if self.pool.len() > 1 {
                for i in (0..self.last_pos).rev() {
                    std::ptr::drop_in_place(self.pool.last().unwrap().add(i));
                }
                std::alloc::dealloc(std::mem::transmute(self.pool.last().unwrap()), layout);
            }
            if !self.pool.is_empty() {
                for i in 0..self.pool.len() - 1 {
                    let p = self.pool[i];
                    for i in (0..N).rev() {
                        std::ptr::drop_in_place(p.add(i));
                    }
                    std::alloc::dealloc(std::mem::transmute(p), layout);
                }
            }
        }
    }
}

struct StackNode<T> {
    data: Option<T>,
    prev: *mut StackNode<T>,
}
pub(crate) struct Stack<T> {
    pool: StackPool<StackNode<T>, 256>,
    last: *mut StackNode<T>,
    len: usize,
}
struct StackIterator<'a, T> {
    node: *mut StackNode<T>,
    phantom: PhantomData<&'a T>,
}
impl<'a, T> Iterator for StackIterator<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let item = unsafe { self.node.as_ref()?.data.as_ref().unwrap() };
        self.node = unsafe { (*self.node).prev };
        Some(item)
    }
}
impl<T> Stack<T> {
    pub(crate) fn new() -> Self {
        Self {
            pool: StackPool::new(),
            last: std::ptr::null_mut(),
            len: 0,
        }
    }
    pub(crate) fn is_empty(&self) -> bool {
        self.last.is_null()
    }
    pub(crate) fn push(&mut self, value: T) {
        let node = self.pool.allocate(StackNode {
            data: Some(value),
            prev: self.last,
        });
        self.last = node;
        self.len += 1;
    }
    pub(crate) fn iter<'a>(&'a self) -> StackIterator<'a, T> {
        StackIterator {
            node: self.last,
            phantom: PhantomData {},
        }
    }
    pub(crate) fn last<'a>(&'a self) -> Option<&'a T> {
        unsafe {
            if self.last.is_null() {
                None
            } else {
                (*self.last).data.as_ref()
            }
        }
    }
    pub(crate) fn last_mut<'a>(&'a mut self) -> Option<&'a mut T> {
        unsafe {
            if self.last.is_null() {
                None
            } else {
                (*self.last).data.as_mut()
            }
        }
    }
    pub(crate) fn pop(&mut self) -> Option<T> {
        unsafe {
            if self.last.is_null() {
                None
            } else {
                self.len -= 1;
                let p = self.last;
                let ret = std::mem::replace(&mut (*p).data, None).unwrap();
                self.last = (*self.last).prev;
                self.pool.free(p);
                Some(ret)
            }
        }
    }
    pub(crate) fn len(&self) -> usize {
        self.len
    }
}
impl<T> Drop for Stack<T> {
    fn drop(&mut self) {
        unsafe {
            let mut p = self.last;
            while !p.is_null() {
                let prev = (*p).prev;
                self.pool.free(p);
                p = prev;
            }
        }
    }
}

pub(crate) fn dummy_convert_ref<'a, 'b, T: 'static, U: 'static>(x: &'a T) -> &'b U {
    if TypeId::of::<T>() == TypeId::of::<U>() {
        unsafe { std::mem::transmute(x) }
    } else {
        unreachable!()
    }
}
pub(crate) fn dummy_convert<T: 'static + Sized, U: 'static + Sized>(x: T) -> U {
    if TypeId::of::<T>() == TypeId::of::<U>() {
        unsafe {
            debug_assert!(size_of::<T>() == size_of::<U>());
            let y = std::mem::transmute_copy(&x);
            std::mem::forget(x);
            y
        }
    } else {
        unreachable!()
    }
}
pub(crate) struct CloneCell<T> {
    cell: UnsafeCell<T>,
}
impl<T: Clone> CloneCell<T> {
    pub(crate) fn new(x: T) -> Self {
        Self {
            cell: UnsafeCell::new(x),
        }
    }
    pub(crate) fn set(&self, x: T) {
        unsafe {
            let ptr = self.cell.get();
            *ptr = x;
        }
    }
    pub(crate) fn get(&self) -> T {
        unsafe { (*self.cell.get()).clone() }
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
