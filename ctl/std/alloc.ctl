use core::ptr::RawMut;
use core::mem::size_of;

pub fn alloc<T>(count: usize): ?RawMut<T> {
    #{c_macro}
    extern fn CTL_MALLOC(size: usize): ?*mut c_void;

    match CTL_MALLOC(size_of::<T>() * count) {
        ?ptr => RawMut::from_ptr(unsafe ptr as *mut T),
        null => null,
    }
}

pub fn realloc<T>(addr: *mut T, count: usize): ?RawMut<T> {
    #{c_macro}
    extern fn CTL_REALLOC(addr: *mut c_void, size: usize): ?*mut c_void;

    unsafe match CTL_REALLOC(addr as *mut c_void, size_of::<T>() * count) {
        ?ptr => RawMut::from_ptr(ptr as *mut T),
        null => null,
    }
}
