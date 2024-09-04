use core::intrin;
pub use intrin::size_of;
pub use intrin::align_of;

pub fn size_of_val<T>(_: *T): uint {
    size_of::<T>()
}

pub fn align_of_val<T>(_: *T): uint {
    align_of::<T>()
}

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`.
pub unsafe fn copy<T>(kw dst: *raw T, kw src: *raw T, kw num: uint) {
    unsafe intrin::memcpy(dst as *mut c_void, src as *c_void, num * size_of::<T>());
}

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`. Behaves as if
/// `src` is first copied to a temporary buffer, then copied to dst.
pub unsafe fn copy_overlapping<T>(kw dst: *raw T, kw src: *raw T, kw num: uint) {
    unsafe intrin::memmove(dst as *mut c_void, src as *c_void, num * size_of::<T>());
}

pub unsafe fn compare<T>(lhs: *raw T, rhs: *raw T, num: uint): bool {
    unsafe intrin::memcmp(lhs as *c_void, rhs as *c_void, num * size_of::<T>()) == 0
}

pub unsafe fn zeroed<T>(): T {
    mut t: T;
    unsafe intrin::memset(&mut t as *mut c_void, 0, size_of::<T>());
    t
}

pub fn swap<T>(lhs: *mut T, rhs: *mut T) {
    let tmp = *lhs;
    *lhs = *rhs;
    *rhs = tmp;
}

pub fn replace<T>(ptr: *mut T, val: T): T {
    let old = *ptr;
    *ptr = val;
    old
}

pub unsafe fn transmute<In, Out>(from: In): Out {
    // TODO: this is fine since we transpile to c, but whenever a spec gets written this usage of 
    // unions in CTL code should be considered UB
    unsafe union Transmuter<T, U> {
        from: T,
        to: U,
    }
    // static_assert(size_of::<In>() == size_of::<Out>());
    unsafe Transmuter::<In, Out>(from:).to
}
