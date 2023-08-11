use core::ptr::RawMut;
use core::mem::size_of;

pub fn alloc<T>(count: usize) ?RawMut<T> {
    [c_macro]
    extern fn GC_MALLOC(size: usize) ?*mut c_void;

    return match GC_MALLOC(size_of::<T>() * count) {
        ?ptr => RawMut::from_ptr(unsafe ptr as *mut T),
        null => null,
    };
}

pub fn realloc<T>(addr: *mut T, count: usize) ?RawMut<T> {
    [c_macro]
    extern fn GC_REALLOC(addr: *mut c_void, size: usize) ?*mut c_void;

    return unsafe match GC_REALLOC(addr as *mut c_void, size_of::<T>() * count) {
        ?ptr => RawMut::from_ptr(ptr as *mut T),
        null => null,
    };
}
