use core::mem;

pub unsafe fn offset<T>(ptr: *T, count: uint): *T {
    unsafe ((ptr as uint) + count * mem::size_of::<T>()) as *T
}

pub unsafe fn offset_mut<T>(ptr: *mut T, count: uint): *mut T {
    unsafe ((ptr as uint) + count * mem::size_of::<T>()) as *mut T
}

pub fn eq<T>(lhs: *T, rhs: *T): bool {
    lhs as uint == rhs as uint
}

pub struct Raw<T> {
    addr: uint,

    pub fn from_ptr(ptr: *T): Raw<T> {
        Raw(addr: ptr as uint)
    }

    pub fn from_addr(addr: uint): ?Raw<T> {
        if addr > 0 {
            Raw::<T>(addr:)
        }
    }

    pub fn from_mut(ptr: RawMut<T>): Raw<T> {
        Raw::from_ptr(unsafe ptr.as_ptr())
    }

    pub fn dangling(): Raw<T> {
        Raw(addr: 0xDEADBEEF)
    }

    pub fn add(this, count: uint): Raw<T> {
        Raw(addr: this.addr + count * mem::size_of::<T>())
    }

    pub fn sub(this, count: uint): Raw<T> {
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
    addr: uint,

    pub fn from_ptr(ptr: *T): RawMut<T> {
        RawMut(addr: ptr as uint)
    }

    pub fn from_addr(addr: uint): ?RawMut<T> {
        if addr > 0 {
            RawMut::<T>(addr:)
        }
    }

    pub fn dangling(): RawMut<T> {
        RawMut(addr: 0xDEADBEEF)
    }

    pub fn add(this, count: uint): RawMut<T> {
        RawMut(addr: this.addr + count * mem::size_of::<T>())
    }

    pub fn sub(this, count: uint): RawMut<T> {
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
