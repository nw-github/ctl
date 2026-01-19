use std::mem::*;
use std::deps::{libgc, libc};

fn ptr_cast<T, U>(ptr: ?^mut T): ?^mut U => unsafe std::mem::bit_cast(ptr);

pub unsafe trait Allocator {
    fn alloc(this, layout: Layout): ?^mut u8;
    unsafe fn resize(this, ptr: ^mut u8, layout: Layout): ?^mut u8;
    unsafe fn free(this, ptr: ^mut u8, layout: Layout);
}

$[cfg("!ctl:no-gc")]
pub struct DefaultAllocator {
    impl Allocator {
        fn alloc(this, layout: Layout): ?^mut u8 {
            // TODO: alignment
            ptr_cast(unsafe libgc::GC_malloc(layout.size()))
        }

        unsafe fn resize(this, ptr: ^mut u8, layout: Layout): ?^mut u8 {
            ptr_cast(unsafe libgc::GC_realloc(ptr.cast(), layout.size()))
        }

        unsafe fn free(this, _ptr: ^mut u8, _layout: Layout) {}
    }
}

$[feature(hosted), cfg("ctl:no-gc")]
pub struct DefaultAllocator {
    impl Allocator {
        fn alloc(this, layout: Layout): ?^mut u8 {
            // TODO: alignment
            ptr_cast(unsafe libc::malloc(layout.size()))
        }

        unsafe fn resize(this, ptr: ^mut u8, layout: Layout): ?^mut u8 {
            ptr_cast(unsafe libc::realloc(ptr.cast(), layout.size()))
        }

        unsafe fn free(this, ptr: ^mut u8, _layout: Layout) {
            unsafe libc::free(?ptr.cast())
        }
    }
}

pub fn new<T>(val: T): *mut T {
    let layout = Layout::of_val(&val);
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
