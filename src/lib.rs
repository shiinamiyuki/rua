use std::{any::TypeId, marker::PhantomData, mem::size_of};

pub mod api;
pub mod bind;
pub mod bytecode;
pub mod closure;
pub mod compile;
pub mod gc;
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

struct StackNode<T> {
    data: Option<T>,
    prev: *mut StackNode<T>,
}
pub(crate) struct Stack<T> {
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
            last: std::ptr::null_mut(),
            len: 0,
        }
    }
    pub(crate) fn is_empty(&self) -> bool {
        self.last.is_null()
    }
    pub(crate) fn push(&mut self, value: T) {
        let node = Box::into_raw(Box::new(StackNode {
            data: Some(value),
            prev: self.last,
        }));
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
                Box::from_raw(p);
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
                Box::from_raw(p);
                p = prev;
            }
        }
    }
}

pub(crate) fn dummy_convert_ref<T: 'static, U: 'static>(x: &T) -> &U {
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
