use std::fmt::*;
use std::intrin;
use std::ops::Eq;

extension<T> ^T {
    pub fn cast<U>(my this): ^U => this as ^U;
    $[intrinsic(numeric_cast)]
    pub fn addr(my this): uint => this.addr();
    pub fn add_signed(my this, offs: int): ^T => intrin::ptr_add_signed(this, offs);
    pub fn add(my this, offs: uint): ^T => intrin::ptr_add_unsigned(this, offs);
    pub fn sub_signed(my this, offs: uint): ^T => intrin::ptr_sub_unsigned(this, offs);
    pub fn sub(my this, offs: uint): ^T => intrin::ptr_sub_unsigned(this, offs);
    pub fn sub_ptr(my this, rhs: ^T): uint => uint::from(intrin::ptr_diff(this, rhs));
    pub fn sub_ptr_signed(my this, rhs: ^T): int => intrin::ptr_diff(this, rhs);

    pub unsafe fn read(my this): T => unsafe *this;
    pub unsafe fn read_volatile(my this): T => unsafe std::ptr::read_volatile(this);
    pub unsafe fn read_unaligned(my this): T {
        unsafe std::mem::Uninit::<T>::assume_init_by(|=this, buf| {
            unsafe std::mem::copy_no_overlap(dst: buf, src: this, num: 1);
        }).0
    }

    $[intrinsic(unary_op)]
    pub fn ++(mut this) { (*this)++; }

    $[intrinsic(unary_op)]
    pub fn --(mut this) { (*this)--; }

    impl Debug {
        fn dbg(this, f: *mut Formatter) {
            mut opts = *f.options();
            if opts.alt and std::mem::size_of_val(this) == 8 {
                mut buf = [b'0'; 17];
                buf[8] = b'`';
                unsafe {
                    mut bytes = std::mem::Uninit::from_mut(&mut buf).as_bytes_mut();
                    (this.addr() & 0xffff_ffff).write_digits(bytes, 16, opts.upper);
                    (this.addr() >> 32).write_digits(bytes[..8u], 16, opts.upper);
                    f.pad(str::from_utf8_unchecked(buf[..]));
                }
            } else {
                opts.alt = true;
                this.addr().hex(&mut f.with_options(opts));
            }
        }
    }

    impl Pointer {
        fn ptr(this, f: *mut Formatter) => this.dbg(f);
    }

    impl Eq<^T> {
        $[intrinsic(binary_op)]
        fn eq(this, rhs: *^T): bool => this == rhs;

        $[intrinsic(binary_op)]
        fn ne(this, rhs: *^T): bool => this != rhs;
    }

    impl Eq<^mut T> {
        $[intrinsic(binary_op)]
        fn eq(this, rhs: *^mut T): bool => this == rhs;

        $[intrinsic(binary_op)]
        fn ne(this, rhs: *^mut T): bool => this != rhs;
    }
}

extension<T> ^mut T {
    pub fn cast<U>(my this): ^mut U => this as ^mut U;
    $[intrinsic(numeric_cast)]
    pub fn addr(my this): uint => this.addr();
    pub fn add_signed(my this, offs: int): ^mut T => intrin::ptr_add_signed(this, offs) as ^mut T;
    pub fn add(my this, offs: uint): ^mut T => intrin::ptr_add_unsigned(this, offs) as ^mut T;
    pub fn sub_signed(my this, offs: int): ^mut T => intrin::ptr_sub_signed(this, offs) as ^mut T;
    pub fn sub(my this, offs: uint): ^mut T => intrin::ptr_sub_unsigned(this, offs) as ^mut T;
    pub fn sub_ptr(my this, rhs: ^T): int => intrin::ptr_diff(this, rhs);

    pub unsafe fn read(my this): T => unsafe *this;
    pub unsafe fn read_volatile(my this): T => unsafe std::ptr::read_volatile(this);
    pub unsafe fn read_unaligned(my this): T => unsafe (this as ^T).read_unaligned();

    pub unsafe fn write(my this, val: T) => unsafe *this = val;
    pub unsafe fn write_volatile(my this, val: T) => unsafe std::ptr::write_volatile(this, val);
    pub unsafe fn write_unaligned(my this, val: T) {
        unsafe std::mem::copy_no_overlap(dst: this, src: &raw val, num: 1);
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

    $[intrinsic(unary_op)]
    pub fn ++(mut this) { (*this)++; }

    $[intrinsic(unary_op)]
    pub fn --(mut this) { (*this)--; }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => (*this as ^T).dbg(f);
    }

    impl Pointer {
        fn ptr(this, f: *mut Formatter) => (*this as ^T).ptr(f);
    }

    impl Eq<^T> {
        $[intrinsic(binary_op)]
        fn eq(this, rhs: *^T): bool => this == rhs;

        $[intrinsic(binary_op)]
        fn ne(this, rhs: *^T): bool => this != rhs;
    }

    impl Eq<^mut T> {
        $[intrinsic(binary_op)]
        fn eq(this, rhs: *^mut T): bool => this == rhs;

        $[intrinsic(binary_op)]
        fn ne(this, rhs: *^mut T): bool => this != rhs;
    }
}
