use std::mem::size_of;
use std::intrin;

pub fn alloc<T>(count: uint): ?*raw T {
    if intrin::malloc(size_of::<T>() * count) is ?ptr {
        ptr as *raw T
    }
}

pub fn realloc<T>(addr: *mut T, count: uint): ?*raw T {
    if intrin::realloc(unsafe addr as *mut c_void, size_of::<T>() * count) is ?ptr {
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
