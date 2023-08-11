use core::mem;

pub unsafe fn offset<U>(ptr: *U, count: usize) *U {
    return unsafe ((ptr as usize) + count * mem::size_of::<U>()) as *U;
}

pub unsafe fn offset_mut<T>(ptr: *mut T, count: usize) *mut T {
    return unsafe ((ptr as usize) + count * mem::size_of::<T>()) as *mut T;
}

pub fn eq<T>(lhs: *T, rhs: *T) bool {
    return lhs as usize == rhs as usize;
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
        return Raw::from_ptr(unsafe ptr.as_ptr());
    }

    pub fn dangling<U>() Raw<U> {
        return Raw(addr: 0xDEADBEEF);
    }

    pub fn add(this, count: usize) Raw<T> {
        return Raw(addr: this.addr + count * mem::size_of::<T>());
    }

    pub fn sub(this, count: usize) Raw<T> {
        return Raw(addr: this.addr - count * mem::size_of::<T>());
    }

    pub unsafe fn as_ptr(this) *T {
        return unsafe this.addr as *T;
    }

    pub unsafe fn as_mut_ptr(this) *mut T {
        return unsafe this.addr as *mut T;
    }

    pub unsafe fn read(this) T {
        mut t: T;
        unsafe mem::copy(dst: &mut t, src: this.as_ptr(), num: 1);
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
        return RawMut(addr: this.addr + count * mem::size_of::<T>());
    }

    pub fn sub(this, count: usize) RawMut<T> {
        return RawMut(addr: this.addr - count * mem::size_of::<T>());
    }

    pub unsafe fn as_ptr(this) *T {
        return unsafe this.addr as *T;
    }

    pub unsafe fn as_mut_ptr(this) *mut T {
        return unsafe this.addr as *mut T;
    }

    pub unsafe fn write(this, t: T) {
        unsafe mem::copy(dst: this.as_mut_ptr(), src: &t, num: 1);

        // FIXME: when destructors are implemented, we will need a way to forget `t`
    }

    pub unsafe fn read(this) T {
        mut t: T;
        unsafe mem::copy(dst: &mut t, src: this.as_ptr(), num: 1);
        return t;
    }
}
