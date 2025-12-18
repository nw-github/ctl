use std::fmt::*;
use std::span::Span;
use std::intrin;

pub extension RawImpl<T> for ^T {
    pub fn cast<U>(my this): ^U => this as ^U;
    pub fn addr(my this): uint => this as uint;
    pub fn add_signed(my this, offs: int): ^T => intrin::ptr_add_signed(this, offs);
    pub fn add(my this, offs: uint): ^T => intrin::ptr_add_unsigned(this, offs);
    pub fn sub_signed(my this, offs: uint): ^T => intrin::ptr_sub_unsigned(this, offs);
    pub fn sub(my this, offs: uint): ^T => intrin::ptr_sub_unsigned(this, offs);
    pub fn sub_ptr(my this, rhs: ^T): int => intrin::ptr_diff(this, rhs);

    pub unsafe fn read(my this): T => unsafe *this;
    pub unsafe fn read_volatile(my this): T => unsafe std::ptr::read_volatile(this);

    @(intrinsic(unary_op))
    pub fn ++(mut this) { (*this)++; }

    @(intrinsic(unary_op))
    pub fn --(mut this) { (*this)--; }

    impl Debug {
        fn dbg(this, f: *mut Formatter) {
            let opts = f.options();
            (*this as uint).hex(&mut f.with_options(Options(
                alt: true,
                sign: opts.sign,
                width: opts.width,
                upper: opts.upper,
            )));
        }
    }
}

pub extension RawMutImpl<T> for ^mut T {
    pub fn cast<U>(my this): ^mut U => this as ^mut U;
    pub fn addr(my this): uint => this as uint;
    pub fn add_signed(my this, offs: int): ^mut T => intrin::ptr_add_signed(this, offs) as ^mut T;
    pub fn add(my this, offs: uint): ^mut T => intrin::ptr_add_unsigned(this, offs) as ^mut T;
    pub fn sub_signed(my this, offs: int): ^mut T => intrin::ptr_sub_signed(this, offs) as ^mut T;
    pub fn sub(my this, offs: uint): ^mut T => intrin::ptr_sub_unsigned(this, offs) as ^mut T;
    pub fn sub_ptr(my this, rhs: ^T): int => intrin::ptr_diff(this, rhs);

    pub unsafe fn read(my this): T => unsafe *this;
    pub unsafe fn write(my this, val: T) => unsafe *this = val;

    pub unsafe fn read_volatile(my this): T => unsafe std::ptr::read_volatile(this);
    pub unsafe fn write_volatile(my this, val: T) => unsafe std::ptr::write_volatile(this, val);

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

    @(intrinsic(unary_op))
    pub fn ++(mut this) { (*this)++; }

    @(intrinsic(unary_op))
    pub fn --(mut this) { (*this)--; }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => (*this as ^T).dbg(f);
    }
}

