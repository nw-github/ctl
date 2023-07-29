// TODO: we need a more sophisticated system for intrinsics
pub extern fn size_of<T>() usize;

pub fn size_of_val<T>(_: *T) usize {
    return size_of::<T>();
}

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`.
pub fn copy<T>(kw dst: *mut T, kw src: *T, kw num: usize) {
    extern fn memcpy(dst: *mut c_void, src: *c_void, len: usize) *mut c_void;

    memcpy(dst as *mut c_void, src as *c_void, num * size_of::<T>());
}

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`. Behaves as if
/// `src` is first copied to a temporary buffer, then copied to dst.
pub fn move<T>(kw dst: *mut T, kw src: *T, kw num: usize) {
    extern fn memmove(dst: *mut c_void, src: *c_void, len: usize) *mut c_void;

    memmove(dst as *mut c_void, src as *c_void, num * size_of::<T>());
}

pub fn swap<T>(lhs: *mut T, rhs: *mut T) {
    let tmp = *lhs;
    *lhs = *rhs;
    *rhs = tmp;
}

pub fn replace<T>(ptr: *mut T, val: T) T {
    let old = *ptr;
    *ptr = val;
    return old;
}

pub fn compare<T>(lhs: *T, rhs: *T, num: usize) bool {
    extern fn memcmp(dst: *c_void, src: *c_void, len: usize) c_int;

    return memcmp(lhs as *c_void, rhs as *c_void, num * size_of::<T>()) == 0;
}

pub /*unsafe*/ fn transmute<In, Out>(i: In) Out {
    // TODO: this is fine since we transpile to c, but whenever a spec gets written this usage of 
    // unions in CTL code should be considered UB

    // TODO: uncomment when the syntax is fixed
    // unsafe union Trasmuter<T, U> {
    //     t: T,
    //     u: U,
    // }
    // static_assert(size_of::<In>() == size_of::<Out>());
    // return unsafe { Trasmuter::<_, Out>(t: i).u };

    unsafe union Transmuter<T, U> {
        In(T),
        Out(U),
    }

    return Transmuter::In::<In, Out>(i).Out;
}
