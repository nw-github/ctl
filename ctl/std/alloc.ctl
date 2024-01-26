use core::ptr::RawMut;
use core::mem::size_of;

#{c_macro}
extern fn CTL_MALLOC(size: usize): ?*mut c_void;
#{c_macro}
extern fn CTL_REALLOC(addr: *mut c_void, size: usize): ?*mut c_void;

pub fn alloc<T>(count: usize): ?RawMut<T> {
    if CTL_MALLOC(size_of::<T>() * count) is ?ptr {
        RawMut::from_ptr(unsafe ptr as *mut T)
    }
}

pub fn realloc<T>(addr: *mut T, count: usize): ?RawMut<T> {
    unsafe if CTL_REALLOC(addr as *mut c_void, size_of::<T>() * count) is ?ptr {
        RawMut::from_ptr(ptr as *mut T)
    }
}
