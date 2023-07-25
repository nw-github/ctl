use core::mem::RawMut;
use core::mem::size_of;

pub fn alloc<T>(count: usize) ?RawMut<T> {
    extern fn ctl_malloc(size: usize) usize;

    return RawMut::from_addr(ctl_malloc(size_of::<T>() * count));
}

pub fn realloc<T>(addr: *mut T, count: usize) ?RawMut<T> {
    extern fn ctl_realloc(addr: *mut u8, size: usize) usize;

    return RawMut::from_addr(ctl_realloc(addr as *mut u8, size_of::<T>() * count));
}
