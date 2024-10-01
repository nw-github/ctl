use std::mem::*;

pub fn alloc<T>(count: uint): ?*raw T {
    let size = size_of::<T>().checked_mul(count)?;
    if unsafe std::intrin::malloc(size, align_of::<T>()) is ?ptr {
        ptr.cast()
    }
}

pub fn realloc<T>(addr: *raw T, count: uint): ?*raw T {
    let size = size_of::<T>().checked_mul(count)?;
    if unsafe std::intrin::realloc(addr.cast(), size, align_of::<T>()) is ?ptr {
        ptr.cast()
    }
}

pub fn new<T>(val: T): *mut T {
    if alloc::<T>(1) is ?ptr {
        unsafe {
            ptr.write(val);
            &mut *ptr
        }
    } else {
        panic("new(): out of memory!");
    }
}
