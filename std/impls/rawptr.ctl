use std::fmt::*;
use std::span::Span;

pub extension RawImpl<T> for ^T {
    pub fn cast<U>(my this): ^U {
        this as ^U
    }

    pub fn addr(my this): uint {
        this as uint
    }

    pub fn offset(my this, offs: int): ^T {
        this + offs
    }

    pub fn uoffset(my this, offs: uint): ^T {
        this + offs
    }

    pub unsafe fn read(my this): T {
        unsafe *this
    }

    pub unsafe fn read_volatile(my this): T {
        unsafe std::ptr::read_volatile(this)
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) {
            mut opts = *f.options();
            opts.alt = true;
            (this as uint).hex(&mut f.with_options(opts));
        }
    }
}

pub extension RawMutImpl<T> for ^mut T {
    pub fn cast<U>(my this): ^mut U {
        this as ^mut U
    }

    pub fn addr(my this): uint {
        this as uint
    }

    pub fn offset(my this, offs: int): ^mut T {
        this + offs
    }

    pub fn uoffset(my this, offs: uint): ^mut T {
        this + offs
    }

    pub unsafe fn read(my this): T {
        unsafe *this
    }

    pub unsafe fn write(my this, val: T) {
        unsafe *this = val;
    }

    pub unsafe fn replace(my this, val: T): T {
        unsafe {
            let old = *this;
            *this = val;
            old
        }
    }

    pub unsafe fn swap(my this, val: ^mut T) {
        unsafe {
            let tmp = *this;
            *this = *val;
            *val = tmp;
        }
    }

    pub unsafe fn read_volatile(my this): T {
        unsafe std::ptr::read_volatile(this)
    }

    pub unsafe fn write_volatile(my this, val: T) {
        unsafe std::ptr::write_volatile(this, val)
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => (this as ^T).dbg(f);
    }
}

