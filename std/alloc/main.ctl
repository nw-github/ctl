use std::mem::Layout;
use std::deps::{libgc, libc};

pub unsafe trait Allocator {
    fn alloc(this, layout: Layout): ?^mut u8;
    unsafe fn resize(this, ptr: ^mut u8, layout: Layout): ?^mut u8;
    unsafe fn free(this, ptr: ^mut u8, layout: Layout);
}

$[cfg("!ctl:no-gc")]
pub struct DefaultAllocator {
    unsafe impl Allocator {
        fn alloc(this, layout: Layout): ?^mut u8 {
            // TODO: alignment
            unsafe libgc::GC_malloc(layout.size()).cast()
        }

        unsafe fn resize(this, ptr: ^mut u8, layout: Layout): ?^mut u8 {
            unsafe libgc::GC_realloc(ptr.cast(), layout.size()).cast()
        }

        unsafe fn free(this, _ptr: ^mut u8, _layout: Layout) {}
    }
}

$[feature(hosted), cfg("ctl:no-gc")]
pub struct DefaultAllocator {
    unsafe impl Allocator {
        fn alloc(this, layout: Layout): ?^mut u8 {
            // TODO: alignment
            unsafe libc::malloc(layout.size()).cast()
        }

        unsafe fn resize(this, ptr: ^mut u8, layout: Layout): ?^mut u8 {
            unsafe libc::realloc(ptr.cast(), layout.size()).cast()
        }

        unsafe fn free(this, ptr: ^mut u8, _layout: Layout) {
            unsafe libc::free(?ptr.cast())
        }
    }
}

pub fn new<T>(val: T): *mut T {
    let layout = Layout::of_val(&val);
    if layout.size() == 0 {
        return unsafe std::ptr::raw_dangling() as *mut T;
    }

    guard DefaultAllocator().alloc(layout) is ?ptr else {
        panic("out of memory attempting to allocate object of type {
            std::reflect::type_name_of_val(&val)} with layout {layout:?}");
    }

    let ptr = ptr.cast::<T>();
    unsafe {
        ptr.write(val);
        &mut *ptr
    }
}

pub mod collections {
    pub use super::vec::Vec;
    pub use super::map::Map;
    pub use super::set::Set;
}
