use core::hash::*;
use core::ops::*;
use core::fmt::*;
use core::span::*;
use core::reflect::*;
use core::string::str;
use core::intrin;

static DIGITS: *[u8; 36] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

extension _<T> for T {
    pub fn as_byte_span(this): [u8..] {
        unsafe Span::new(this as *raw u8, core::mem::size_of::<T>())
    }
}

pub extension NumericExt<T: Numeric> for T {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.as_byte_span());
        }
    }

    impl Cmp<T> {
        #(intrinsic(binary_op))
        fn cmp(this, rhs: *T): Ordering { this <=> rhs }

        #(intrinsic(binary_op))
        fn ge(this, rhs: *T): bool { this >= rhs }

        #(intrinsic(binary_op))
        fn gt(this, rhs: *T): bool { this > rhs }

        #(intrinsic(binary_op))
        fn le(this, rhs: *T): bool { this <= rhs }

        #(intrinsic(binary_op))
        fn lt(this, rhs: *T): bool { this < rhs }
    }

    impl Eq<T> {
        #(intrinsic(binary_op))
        fn eq(this, rhs: *T): bool { this == rhs }

        #(intrinsic(binary_op))
        fn ne(this, rhs: *T): bool { this != rhs }
    }

    #(intrinsic(binary_op))
    pub fn +(this, rhs: T): T { this + rhs }

    #(intrinsic(binary_op))
    pub fn -(this, rhs: T): T { this - rhs }

    #(intrinsic(binary_op))
    pub fn *(this, rhs: T): T { this * rhs }

    #(intrinsic(binary_op))
    pub fn /(this, rhs: T): T { this / rhs }

    #(intrinsic(binary_op))
    pub fn %(this, rhs: T): T { this % rhs }

    pub fn max(this, rhs: T): T {
        if *this > rhs { *this } else { rhs }
    }

    pub fn min(this, rhs: T): T {
        if *this > rhs { rhs } else { *this }
    }

    pub fn clamp(this, low: T, hi: T): T {
        if *this < low {
            low
        } else if *this > hi {
            hi
        } else {
            *this
        }
    }
}

mod gcc_intrin {
    use super::Integral;

    // TODO: compiler independent fallback for these functions
    #(c_opaque)
    pub extern fn __builtin_add_overflow<T: Integral>(x: T, y: T, res: *mut T): bool;

    #(c_opaque)
    pub extern fn __builtin_sub_overflow<T: Integral>(x: T, y: T, res: *mut T): bool;

    #(c_opaque)
    pub extern fn __builtin_mul_overflow<T: Integral>(x: T, y: T, res: *mut T): bool;
}

pub extension IntegralExt<T: Numeric + Integral> for T {
    #(intrinsic(binary_op))
    pub fn &(this, rhs: T): T { this & rhs }

    #(intrinsic(binary_op))
    pub fn |(this, rhs: T): T { this | rhs }

    #(intrinsic(binary_op))
    pub fn ^(this, rhs: T): T { this ^ rhs }

    #(intrinsic(binary_op))
    pub fn <<(this, rhs: T): T { this << rhs }

    #(intrinsic(binary_op))
    pub fn >>(this, rhs: T): T { this >> rhs }

    #(intrinsic(unary_op))
    pub fn !(this): T { !*this }

    #(intrinsic(unary_op))
    pub fn ++(mut this) { (*this)++; }

    #(intrinsic(unary_op))
    pub fn --(mut this) { (*this)--; }

    #(inline)
    pub fn wrapping_add(this, rhs: T): T { *this + rhs }

    #(inline)
    pub fn wrapping_sub(this, rhs: T): T { *this - rhs }

    #(inline)
    pub fn wrapping_mul(this, rhs: T): T { *this * rhs }

    #(inline)
    pub fn wrapping_div(this, rhs: T): T { *this / rhs }

    #(inline)
    pub fn overflowing_add(this, rhs: T): (T, bool) {
        mut out: T;
        let res = gcc_intrin::__builtin_add_overflow(*this, rhs, &mut out);
        (out, res)
    }

    #(inline)
    pub fn overflowing_sub(this, rhs: T): (T, bool) {
        mut out: T;
        let res = gcc_intrin::__builtin_sub_overflow(*this, rhs, &mut out);
        (out, res)
    }

    #(inline)
    pub fn overflowing_mul(this, rhs: T): (T, bool) {
        mut out: T;
        let res = gcc_intrin::__builtin_mul_overflow(*this, rhs, &mut out);
        (out, res)
    }

    #(inline)
    pub fn checked_add(this, rhs: T): ?T {
        mut out: T;
        if !gcc_intrin::__builtin_add_overflow(*this, rhs, &mut out) {
            out
        }
    }

    #(inline)
    pub fn checked_sub(this, rhs: T): ?T {
        mut out: T;
        if !gcc_intrin::__builtin_sub_overflow(*this, rhs, &mut out) {
            out
        }
    }

    #(inline)
    pub fn checked_mul(this, rhs: T): ?T {
        mut out: T;
        if !gcc_intrin::__builtin_mul_overflow(*this, rhs, &mut out) {
            out
        }
    }

    #(intrinsic)
    pub fn max_value(): T { T::max_value() }

    #(intrinsic)
    pub fn min_value(): T { T::min_value() }
}

pub extension SignedExt<T: Numeric + Signed> for T {
    pub fn abs(this): T {
        intrin::numeric_abs(*this)
    }

    pub unsafe fn to_str_radix_unchecked(this, radix: u32, buf: [mut u8..]): str {
        let radix: T = intrin::numeric_cast(radix);
        mut pos = buf.len();
        mut val = this.abs();
        loop {
            unsafe *buf.get_mut_unchecked(--pos) = DIGITS[intrin::numeric_cast(val % radix)];
            val = val / radix;
        } while val != intrin::numeric_cast(0);

        if this < intrin::numeric_cast(0) {
            unsafe *buf.get_mut_unchecked(--pos) = b'-';
        }

        unsafe str::from_utf8_unchecked(buf[pos..])
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            // FIXME: fix this when there is a safer way to deal with uninitialized memory
            //        size_of should be size_of<T>
            mut buffer: [u8; core::mem::size_of::<u128>() * 8 + 1];
            unsafe this.to_str_radix_unchecked(10, buffer[..]).format(f);
        }
    }

    #(intrinsic(unary_op))
    pub fn -(this): T { -*this }
}

pub extension UnsignedExt<T: Numeric + Unsigned> for T {
    pub unsafe fn to_str_radix_unchecked(this, radix: u32, buf: [mut u8..]): str {
        let radix: T = intrin::numeric_cast(radix);
        mut pos = buf.len();
        mut val = *this;
        loop {
            unsafe *buf.get_mut_unchecked(--pos) = DIGITS[intrin::numeric_cast(val % radix)];
            val = val / radix;
        } while val != intrin::numeric_cast(0);

        unsafe str::from_utf8_unchecked(buf[pos..])
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            mut buffer: [u8; core::mem::size_of::<u128>() * 8 + 1];
            unsafe this.to_str_radix_unchecked(10, buffer[..]).format(f);
        }
    }
}

pub extension CharExt for char {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.as_byte_span());
        }
    }

    impl Eq<This> {
        #(intrinsic(binary_op))
        fn eq(this, rhs: *This): bool { this == rhs }

        #(intrinsic(binary_op))
        fn ne(this, rhs: *This): bool { this != rhs }
    }

    impl Cmp<This> {
        #(intrinsic(binary_op))
        fn cmp(this, rhs: *This): Ordering { this <=> rhs }

        #(intrinsic(binary_op))
        fn ge(this, rhs: *This): bool { this >= rhs }

        #(intrinsic(binary_op))
        fn gt(this, rhs: *This): bool { this > rhs }

        #(intrinsic(binary_op))
        fn le(this, rhs: *This): bool { this <= rhs }

        #(intrinsic(binary_op))
        fn lt(this, rhs: *This): bool { this < rhs }
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            unsafe this.encode_utf8_unchecked(this.len_utf8(), [0u8; 4][..]).format(f);
        }
    }

    pub fn len_utf8(this): uint {
        let cp = *this as u32;
        if cp < 0x80 {
            1
        } else if cp < 0x800 {
            2
        } else if cp < 0x10000 {
            3
        } else {
            4
        }
    }

    pub fn encode_utf8(this, buf: [mut u8..]): str {
        let len = this.len_utf8();
        if buf.len() < len {
            core::panic("char::encode_utf8(): buffer size is insufficient");
        }

        unsafe this.encode_utf8_unchecked(len, buf)
    }

    pub fn is_ascii_upper(this): bool {
        this is 'A'..='Z'
    }

    pub fn is_ascii_lower(this): bool {
        this is 'a'..='z'
    }

    pub fn make_ascii_upper(mut this) {
        if this.is_ascii_upper() {
            *this = this.to_ascii_upper();
        }
    }

    pub fn make_ascii_lower(mut this) {
        if this.is_ascii_lower() {
            *this = this.to_ascii_lower();
        }
    }

    pub fn to_ascii_upper(this): char {
        if this.is_ascii_upper() {
            this.toggled_ascii_case()
        } else {
            *this
        }
    }

    pub fn to_ascii_lower(this): char {
        if this.is_ascii_lower() {
            this.toggled_ascii_case()
        } else {
            *this
        }
    }

    fn toggled_ascii_case(this): char {
        (*this as u32 ^ 0b100000) as! char
    }

    unsafe fn encode_utf8_unchecked(this, len_utf8: uint, buf: [mut u8..]): str {
        unsafe {
            let cp = *this as u32;
            mut ptr = buf.as_raw();
            match len_utf8 {
                1 => {
                    *ptr = cp as! u8;
                }
                2 => {
                    *ptr++ = ((cp >> 6) | 0xc0) as! u8;
                    *ptr   = ((cp & 0x3f) | 0x80) as! u8;
                }
                3 => {
                    *ptr++ = ((cp >> 12) | 0xe0) as! u8;
                    *ptr++ = (((cp >> 6) & 0x3f) | 0x80) as! u8;
                    *ptr   = ((cp & 0x3f) | 0x80) as! u8;
                }
                4 => {
                    *ptr++ = ((cp >> 18) | 0xf0) as! u8;
                    *ptr++ = (((cp >> 12) & 0x3f) | 0x80) as! u8;
                    *ptr++ = (((cp >> 6) & 0x3f) | 0x80) as! u8;
                    *ptr   = ((cp & 0x3f) | 0x80) as! u8;
                }
                _ => core::unreachable_unchecked(),
            }
            str::from_utf8_unchecked(buf[..len_utf8])
        }
    }
}

pub extension BoolExt for bool {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.as_byte_span());
        }
    }

    impl Eq<This> {
        #(intrinsic(binary_op))
        fn eq(this, rhs: *This): bool { this == rhs }

        #(intrinsic(binary_op))
        fn ne(this, rhs: *This): bool { this != rhs }
    }

    #(intrinsic(unary_op))
    pub fn !(this): This { !*this }

    #(intrinsic(binary_op))
    pub fn &(this, rhs: This): This { this & rhs }

    #(intrinsic(binary_op))
    pub fn |(this, rhs: This): This { this | rhs }

    #(intrinsic(binary_op))
    pub fn ^(this, rhs: This): This { this ^ rhs }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            if *this { "true".format(f) } else { "false".format(f) }
        }
    }

    pub fn then_some<T>(this, t: T): ?T {
        if *this { t }
    }
}

pub extension VoidExt for void {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash([0u8][..]);
        }
    }

    impl Eq<This> {
        fn eq(this, _rhs: *This): bool { true }

        fn ne(this, _rhs: *This): bool { false }
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            "void".format(f);
        }
    }
}

use super::ryu::Float32Ext;
use super::ryu::Float64Ext;

mod libm {
    pub extern fn sqrt(num: f64): f64;
    pub extern fn sin(n: f64): f64;
    pub extern fn cos(n: f64): f64;
    pub extern fn tan(n: f64): f64;
    pub extern fn floor(n: f64): f64;
    pub extern fn ceil(n: f64): f64;

    pub extern fn sqrtf(num: f32): f32;
    pub extern fn sinf(n: f32): f32;
    pub extern fn cosf(n: f32): f32;
    pub extern fn tanf(n: f32): f32;
    pub extern fn floorf(n: f32): f32;
    pub extern fn ceilf(n: f32): f32;
}

pub extension F32Ext for f32 {
    pub fn to_bits(this): u32 {
        unsafe core::mem::transmute(*this)
    }

    pub fn sqrt(this): f32 {
        libm::sqrtf(*this)
    }

    pub fn sin(this): f32 {
        libm::sinf(*this)
    }

    pub fn cos(this): f32 {
        libm::cosf(*this)
    }

    pub fn tan(this): f32 {
        libm::tanf(*this)
    }

    pub fn floor(this): f32 {
        libm::floorf(*this)
    }

    pub fn ceil(this): f32 {
        libm::ceilf(*this)
    }

    pub fn pi(): f32 {
        3.14159265358979323846
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            super::ryu::Buffer::new().format(*this).format(f);
        }
    }
}

pub extension F64Ext for f64 {
    pub fn to_bits(this): u64 {
        unsafe core::mem::transmute(*this)
    }

    pub fn sqrt(this): f64 {
        libm::sqrt(*this)
    }

    pub fn sin(this): f64 {
        libm::sin(*this)
    }

    pub fn cos(this): f64 {
        libm::cos(*this)
    }

    pub fn tan(this): f64 {
        libm::tan(*this)
    }

    pub fn floor(this): f64 {
        libm::floor(*this)
    }

    pub fn ceil(this): f64 {
        libm::ceil(*this)
    }

    pub fn pi(): f64 {
        3.14159265358979323846
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            super::ryu::Buffer::new().format(*this).format(f);
        }
    }
}
