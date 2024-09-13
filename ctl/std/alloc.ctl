use std::mem::*;

pub fn alloc<T>(count: uint): ?*raw T {
    if unsafe std::intrin::malloc(size_of::<T>().checked_mul(count)?, align_of::<T>()) is ?ptr {
        ptr as *raw T
    }
}

pub fn realloc<T>(addr: *mut T, count: uint): ?*raw T {
    let size = size_of::<T>().checked_mul(count)?;
    if unsafe std::intrin::realloc(addr as *raw c_void, size, align_of::<T>()) is ?ptr {
        ptr as *raw T
    }
}

pub fn new<T>(t: T): *mut T {
    if alloc::<T>(1) is ?ptr {
        unsafe {
            *ptr = t;
            ptr as *mut T
        }
    } else {
        panic("new(): out of memory!");
    }
}
