#(intrinsic(size_of))
pub import fn size_of<T>(): uint;

pub fn size_of_val<T>(_: *T): uint {
    size_of::<T>()
}

mod builtin {
    #(c_opaque, c_name(CTL_MEMCPY))
    pub import fn memcpy(dst: *mut c_void, src: *c_void, len: uint): *mut c_void;

    #(c_opaque, c_name(CTL_MEMMOVE))
    pub import fn memmove(dst: *mut c_void, src: *c_void, len: uint): *mut c_void;

    #(c_opaque, c_name(CTL_MEMCMP))
    pub import fn memcmp(dst: *c_void, src: *c_void, len: uint): c_int;
}

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`.
pub unsafe fn copy<T>(kw dst: *mut T, kw src: *T, kw num: uint) {
    unsafe builtin::memcpy(dst as *mut c_void, src as *c_void, num * size_of::<T>());
}

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`. Behaves as if
/// `src` is first copied to a temporary buffer, then copied to dst.
pub unsafe fn copy_overlapping<T>(kw dst: *mut T, kw src: *T, kw num: uint) {
    unsafe builtin::memmove(dst as *mut c_void, src as *c_void, num * size_of::<T>());
}

pub unsafe fn compare<T>(lhs: *T, rhs: *T, num: uint): bool {
    unsafe builtin::memcmp(lhs as *c_void, rhs as *c_void, num * size_of::<T>()) == 0
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

pub unsafe fn transmute<In, Out>(i: In): Out {
    // TODO: this is fine since we transpile to c, but whenever a spec gets written this usage of 
    // unions in CTL code should be considered UB
    unsafe union Transmuter<T, U> {
        from: T,
        to: U,
    }
    // static_assert(size_of::<In>() == size_of::<Out>());
    unsafe Transmuter::<In, Out>(from: i).to
}
