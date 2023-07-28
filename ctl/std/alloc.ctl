use core::ptr::RawMut;
use core::mem::size_of;

extern fn ctl_malloc(size: usize) ?*mut c_void;
extern fn ctl_realloc(addr: *mut c_void, size: usize) ?*mut c_void;

pub fn alloc<T>(count: usize) ?RawMut<T> {
    return match ctl_malloc(size_of::<T>() * count) {
        ?ptr => RawMut::from_ptr(ptr as *mut T),
        null => null,
    };
}

pub fn realloc<T>(addr: *mut T, count: usize) ?RawMut<T> {
    return match ctl_realloc(addr as *mut c_void, size_of::<T>() * count) {
        ?ptr => RawMut::from_ptr(ptr as *mut T),
        null => null,
    };
}
