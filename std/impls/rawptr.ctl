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

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            // TODO: just format (this as uint) when format specifiers are added
            mut buffer: [u8; std::mem::size_of::<uint>() * 2 + 2];
            unsafe {
                let res = (*this as uint).to_str_radix_unchecked(16, buffer[2u..]);
                // kinda gross, but avoids multiple calls to fmt()
                mut ptr = res.as_raw() as ^mut u8;
                *--ptr = b'x';
                *--ptr = b'0';
                str::from_utf8_unchecked(Span::new(ptr, res.len() + 2)).fmt(f);
            }
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

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            (this as ^T).fmt(f);
        }
    }
}

