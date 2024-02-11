use std::ptr::RawMut;
use std::mem::size_of;

mod builtin {
    #(c_opaque, c_name(CTL_MALLOC))
    pub import fn malloc(size: uint): ?*mut c_void;

    #(c_opaque, c_name(CTL_REALLOC))
    pub import fn realloc(addr: *mut c_void, size: uint): ?*mut c_void;
}

pub fn alloc<T>(count: uint): ?RawMut<T> {
    if builtin::malloc(size_of::<T>() * count) is ?ptr {
        RawMut::from_ptr(unsafe ptr as *mut T)
    }
}

pub fn realloc<T>(addr: *mut T, count: uint): ?RawMut<T> {
    if builtin::realloc(unsafe addr as *mut c_void, size_of::<T>() * count) is ?ptr {
        RawMut::from_ptr(unsafe ptr as *mut T)
    }
}
