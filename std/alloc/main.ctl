use std::mem::*;

@(feature(not(boehm)))
mod lib {
    pub extern fn malloc(size: uint): ?^mut c_void;
    pub extern fn realloc(addr: ^mut c_void, size: uint): ?^mut c_void;
}

@(feature(boehm))
mod lib {
    @(c_opaque, c_name(GC_MALLOC))
    pub extern fn malloc(size: uint): ?^mut c_void;

    @(c_opaque, c_name(GC_REALLOC))
    pub extern fn realloc(addr: ^mut c_void, size: uint): ?^mut c_void;
}

pub fn alloc<T>(count: uint): ?^mut T {
    // TODO: alignment
    let size = size_of::<T>().checked_mul(count)?;
    if unsafe lib::malloc(size) is ?ptr {
        ptr.cast()
    }
}

pub fn realloc<T>(addr: ^mut T, count: uint): ?^mut T {
    let size = size_of::<T>().checked_mul(count)?;
    if unsafe lib::realloc(addr.cast(), size) is ?ptr {
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
        panic("out of memory attempting to allocate {std::mem::size_of::<T>()} byte object");
    }
}

pub mod collections {
    pub use super::vec::Vec;
    pub use super::map::Map;
    pub use super::set::Set;
}
