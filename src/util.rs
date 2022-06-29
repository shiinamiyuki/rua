use std::{
    alloc::Layout,
    cell::Cell,
    ops::{Index, IndexMut, Range},
    slice::SliceIndex,
};

// pub trait Chainable {
//     fn prev(&self) -> &Cell<*const Self>;
//     fn next(&self) -> &Cell<*const Self>;
// }
// pub struct InstrusiveLinkedList<T: Chainable> {
//     head: Cell<*const T>,
//     tail: Cell<*const T>,
// }

pub struct FixedVec<T> {
    data: *mut T,
    len: usize,
    capacity: usize,
}
unsafe impl<T> Send for FixedVec<T> {}
unsafe impl<T> Sync for FixedVec<T> {}
impl<T> FixedVec<T>
where
    T: Clone,
{
    pub(crate) fn new(capacity: usize) -> Self {
        unsafe {
            Self {
                data: std::alloc::alloc(Layout::array::<T>(capacity).unwrap()) as *mut T,
                len: 0,
                capacity,
            }
        }
    }
    pub(crate) fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            let val = unsafe { std::ptr::read(self.data.offset(self.len as isize)) };
            Some(val)
        }
    }
    pub(crate) fn push(&mut self, val: T) -> Option<()> {
        if self.len + 1 < self.capacity {
            unsafe {
                std::ptr::write(self.data.offset(self.len as isize), val);
            }
            self.len += 1;
            Some(())
        } else {
            None
        }
    }
    pub(crate) fn resize(&mut self, new_len: usize, val: T) -> Option<()> {
        if new_len > self.capacity {
            None
        } else {
            while self.len < new_len {
                self.push(val.clone());
            }
            while self.len > new_len {
                self.pop().unwrap();
            }
            Some(())
        }
    }
    pub(crate) fn len(&self) -> usize {
        self.len
    }
}
impl<T> Index<usize> for FixedVec<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.len {
            panic!("out of range");
        } else {
            unsafe { &*self.data.offset(index as isize) }
        }
    }
}
impl<T> Index<Range<usize>> for FixedVec<T> {
    type Output = [T];
    fn index(&self, index: Range<usize>) -> &Self::Output {
        if index.end > self.len || index.start >= self.len {
            panic!("out of range");
        } else {
            unsafe {
                std::slice::from_raw_parts(
                    self.data.offset(index.start as isize),
                    index.end - index.start,
                )
            }
        }
    }
}

impl<T> IndexMut<usize> for FixedVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index >= self.len {
            panic!("out of range");
        } else {
            unsafe { &mut *self.data.offset(index as isize) }
        }
    }
}
impl<T> IndexMut<Range<usize>> for FixedVec<T> {
    fn index_mut(&mut self, index: Range<usize>) -> &mut Self::Output {
        if index.end > self.len || index.start >= self.len {
            panic!("out of range");
        } else {
            unsafe {
                std::slice::from_raw_parts_mut(
                    self.data.offset(index.start as isize),
                    index.end - index.start,
                )
            }
        }
    }
}
// impl<T> std::ops::RangeBounds<T> for FixedVec<T> {
//     fn start_bound(&self) -> std::ops::Bound<&T> {
//         std::ops::Bound::Included(unsafe { self.data.as_ref().unwrap() })
//     }
//     fn end_bound(&self) -> std::ops::Bound<&T> {
//         std::ops::Bound::Excluded(unsafe { self.data.offset(self.len as isize).as_ref().unwrap() })
//     }
// }
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
