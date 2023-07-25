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

pub /*unsafe*/ fn offset<U>(ptr: *U, count: usize) *U {
    return ((ptr as usize) + count * size_of::<U>()) as *U;
}

pub /*unsafe*/ fn offset_mut<T>(ptr: *mut T, count: usize) *mut T {
    return ((ptr as usize) + count * size_of::<T>()) as *mut T;
}

pub struct Raw<T> {
    addr: usize,

    pub fn from_ptr<U>(ptr: *U) Raw<U> {
        return Raw(addr: ptr as usize);
    }

    pub fn from_addr<U>(addr: usize) ?Raw<U> {
        return if addr > 0 {
            yield Raw::<U>(addr:);
        };
    }

    pub fn from_mut<U>(ptr: RawMut<U>) Raw<U> {
        return Raw::from_ptr(ptr.as_ptr());
    }

    pub fn dangling<U>() Raw<U> {
        return Raw(addr: 0xDEADBEEF);
    }

    pub fn add(this, count: usize) Raw<T> {
        return Raw(addr: this.addr + count * size_of::<T>());
    }

    pub fn sub(this, count: usize) Raw<T> {
        return Raw(addr: this.addr - count * size_of::<T>());
    }

    pub /*unsafe*/ fn as_ptr(this) *T {
        return this.addr as *T;
    }

    pub /*unsafe*/ fn as_mut_ptr(this) *mut T {
        return this.addr as *mut T;
    }

    pub /*unsafe*/ fn read(this) T {
        mut t: T;
        copy(dst: &mut t, src: this.as_ptr(), num: 1);
        return t;
    }
}

pub struct RawMut<T> {
    addr: usize,

    pub fn from_ptr<U>(ptr: *U) RawMut<U> {
        return RawMut(addr: ptr as usize);
    }

    pub fn from_addr<U>(addr: usize) ?RawMut<U> {
        return if addr > 0 {
            yield RawMut::<U>(addr:);
        };
    }

    pub fn dangling<U>() RawMut<U> {
        return RawMut(addr: 0xDEADBEEF);
    }

    pub fn add(this, count: usize) RawMut<T> {
        return RawMut(addr: this.addr + count * size_of::<T>());
    }

    pub fn sub(this, count: usize) RawMut<T> {
        return RawMut(addr: this.addr - count * size_of::<T>());
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
