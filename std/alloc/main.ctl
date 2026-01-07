use std::mem::*;
use std::deps::*;

fn _malloc(size: uint): ?^mut void {
    $[feature(boehm)]
    return unsafe libgc::GC_malloc(size);

    $[feature(not(boehm))]
    return unsafe libc::malloc(size);
}

fn _realloc(ptr: ^mut void, size: uint): ?^mut void {
    $[feature(boehm)]
    return unsafe libgc::GC_realloc(ptr, size);

    $[feature(not(boehm))]
    return unsafe libc::realloc(ptr, size);
}

pub fn alloc<T>(count: uint): ?^mut T {
    // TODO: alignment
    let size = size_of::<T>().checked_mul(count)?;
    if _malloc(size) is ?ptr {
        ptr.cast()
    }
}

pub fn realloc<T>(addr: ^mut T, count: uint): ?^mut T {
    let size = size_of::<T>().checked_mul(count)?;
    if _realloc(addr.cast(), size) is ?ptr {
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
