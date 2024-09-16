use std::mem::*;

pub fn alloc<T>(count: uint): ?*raw T {
    if unsafe std::intrin::malloc(size_of::<T>().checked_mul(count)?, align_of::<T>()) is ?ptr {
        ptr.cast()
    }
}

pub fn realloc<T>(addr: *raw T, count: uint): ?*raw T {
    let size = size_of::<T>().checked_mul(count)?;
    if unsafe std::intrin::realloc(addr.cast(), size, align_of::<T>()) is ?ptr {
        ptr.cast()
    }
}

pub fn new<T>(t: T): *mut T {
    if alloc::<T>(1) is ?ptr {
        unsafe {
            *ptr = t;
            &mut *ptr
        }
    } else {
        panic("new(): out of memory!");
    }
}
