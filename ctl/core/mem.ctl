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

pub struct NonNull<T> {
    addr: usize,

    pub fn from_ptr<U>(ptr: *U) NonNull<U> {
        return NonNull(addr: ptr as usize);
    }

    pub fn from_addr<U>(addr: usize) ?NonNull<U> {
        return if addr > 0 {
            yield NonNull::<U>(addr:);
        };
    }

    pub fn dangling<U>() NonNull<U> {
        return NonNull(addr: 0xDEADBEEF);
    }

    pub fn add(this, count: usize) NonNull<T> {
        return NonNull(addr: this.addr + count * size_of::<T>());
    }

    pub fn sub(this, count: usize) NonNull<T> {
        return NonNull(addr: this.addr - count * size_of::<T>());
    }

    pub /*unsafe*/ fn as_ptr(this) *T {
        return this.addr as *T;
    }

    pub /*unsafe*/ fn as_mut_ptr(this) *mut T {
        return this.addr as *mut T;
    }

    pub /*unsafe*/ fn write(this, t: T) {
        copy(dst: this.as_mut_ptr(), src: &t, num: 1);

        // FIXME: when destructors are implemented, we will need a way to forget `t`
    }

    pub /*unsafe*/ fn read(this) T {
        mut t: T;
        copy(dst: &mut t, src: this.as_ptr(), num: 1);
        return t;
    }
}
