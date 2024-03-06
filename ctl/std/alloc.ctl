use std::mem::size_of;

mod builtin {
    #(c_opaque, c_name(CTL_MALLOC))
    pub import fn malloc(size: uint): ?*raw c_void;

    #(c_opaque, c_name(CTL_REALLOC))
    pub import fn realloc(addr: *mut c_void, size: uint): ?*raw c_void;
}

pub fn alloc<T>(count: uint): ?*raw T {
    if (builtin::malloc(size_of::<T>() * count) is ?ptr) {
        ptr as *raw T
    }
}

pub fn realloc<T>(addr: *mut T, count: uint): ?*raw T {
    if (builtin::realloc(unsafe addr as *mut c_void, size_of::<T>() * count) is ?ptr) {
        ptr as *raw T
    }
}

pub fn new<T>(t: T): *mut T {
    if (alloc::<T>(1) is ?ptr) {
        unsafe {
            *ptr = t;
            ptr as *mut T
        }
    } else {
        panic("new(): out of memory!");
    }
}
