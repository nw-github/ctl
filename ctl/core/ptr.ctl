use core::mem;

pub unsafe fn offset<T>(ptr: *T, count: usize): *T {
    unsafe ((ptr as usize) + count * mem::size_of::<T>()) as *T
}

pub unsafe fn offset_mut<T>(ptr: *mut T, count: usize): *mut T {
    unsafe ((ptr as usize) + count * mem::size_of::<T>()) as *mut T
}

pub fn eq<T>(lhs: *T, rhs: *T): bool {
    lhs as usize == rhs as usize
}

pub struct Raw<T> {
    addr: usize,

    pub fn from_ptr<U>(ptr: *U): Raw<U> {
        Raw(addr: ptr as usize)
    }

    pub fn from_addr<U>(addr: usize): ?Raw<U> {
        if addr > 0 {
            Raw::<U>(addr:)
        }
    }

    pub fn from_mut<U>(ptr: RawMut<U>): Raw<U> {
        Raw::from_ptr(unsafe ptr.as_ptr())
    }

    pub fn dangling<U>(): Raw<U> {
        Raw(addr: 0xDEADBEEF)
    }

    pub fn add(this, count: usize): Raw<T> {
        Raw(addr: this.addr + count * mem::size_of::<T>())
    }

    pub fn sub(this, count: usize): Raw<T> {
        Raw(addr: this.addr - count * mem::size_of::<T>())
    }

    pub unsafe fn as_ptr(this): *T {
        unsafe this.addr as *T
    }

    pub unsafe fn as_mut_ptr(this): *mut T {
        unsafe this.addr as *mut T
    }

    pub unsafe fn read(this): T {
        mut t: T;
        unsafe mem::copy(dst: &mut t, src: this.as_ptr(), num: 1);
        t
    }
}

pub struct RawMut<T> {
    addr: usize,

    pub fn from_ptr<U>(ptr: *U): RawMut<U> {
        RawMut(addr: ptr as usize)
    }

    pub fn from_addr<U>(addr: usize): ?RawMut<U> {
        if addr > 0 {
            RawMut::<U>(addr:)
        }
    }

    pub fn dangling<U>(): RawMut<U> {
        RawMut(addr: 0xDEADBEEF)
    }

    pub fn add(this, count: usize): RawMut<T> {
        RawMut(addr: this.addr + count * mem::size_of::<T>())
    }

    pub fn sub(this, count: usize): RawMut<T> {
        RawMut(addr: this.addr - count * mem::size_of::<T>())
    }

    pub unsafe fn as_ptr(this): *T {
        unsafe this.addr as *T
    }

    pub unsafe fn as_mut_ptr(this): *mut T {
        unsafe this.addr as *mut T
    }

    pub unsafe fn write(this, t: T): *mut T {
        let dst = unsafe this.as_mut_ptr();
        unsafe mem::copy(dst:, src: &t, num: 1);
        dst
    }

    pub unsafe fn read(this): T {
        mut t: T;
        unsafe mem::copy(dst: &mut t, src: this.as_ptr(), num: 1);
        t
    }
}
