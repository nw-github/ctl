use core::mem::RawMut;
use core::mem::size_of;
use core::option::Option;

extern fn ctl_malloc(size: usize) ?*mut c_void;
extern fn ctl_realloc(addr: *mut c_void, size: usize) ?*mut c_void;

pub fn alloc<T>(count: usize) ?RawMut<T> {
    return match ctl_malloc(size_of::<T>() * count) {
        Option::Some(ptr) => RawMut::from_ptr(ptr as *mut T),
        Option::None => null,
    };
}

pub fn realloc<T>(addr: *mut T, count: usize) ?RawMut<T> {
    return match ctl_realloc(addr as *mut c_void, size_of::<T>() * count) {
        Option::Some(ptr) => RawMut::from_ptr(ptr as *mut T),
        Option::None => null,
    };
}
