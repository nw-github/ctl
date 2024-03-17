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

    pub unsafe fn as_byte_span_mut(mut this): [mut u8..] {
        unsafe SpanMut::new(this as *raw u8, core::mem::size_of::<T>())
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
    pub fn ++(mut this): T { (*this)++ }

    #(intrinsic(unary_op))
    pub fn --(mut this): T { (*this)-- }

    impl PreInc<T> {
        #(intrinsic(unary_op))
        fn pre_inc(mut this): T { ++(*this) }
    }

    impl PreDec<T> {
        #(intrinsic(unary_op))
        fn pre_dec(mut this): T { --(*this) }
    }

    pub fn wrapping_add(this, rhs: T): T { *this + rhs }

    pub fn wrapping_sub(this, rhs: T): T { *this - rhs }

    pub fn wrapping_mul(this, rhs: T): T { *this * rhs }

    pub fn wrapping_div(this, rhs: T): T { *this / rhs }

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

        unsafe str::from_utf8_unchecked(buf.as_span().subspan(pos..))
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            mut buffer = [b'0'; core::mem::size_of::<i32>() * 8 + 1];
            unsafe this.to_str_radix_unchecked(10, buffer.as_byte_span_mut()).format(f);
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

        unsafe str::from_utf8_unchecked(buf.as_span().subspan(pos..))
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            mut buffer = [b'0'; core::mem::size_of::<i32>() * 8];
            unsafe this.to_str_radix_unchecked(10, buffer.as_byte_span_mut()).format(f);
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
            unsafe this.encode_utf8_unchecked(
                this.len_utf8(),
                [0u8; 4].as_byte_span_mut(),
            ).format(f);
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
            str::from_utf8_unchecked(buf.subspan(..len_utf8).as_span())
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
}

pub extension VoidExt for void {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash([0u8].as_byte_span());
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

mod libc {
    pub import fn sqrt(num: f64): f64;
    pub import fn sqrtf(num: f32): f32;
}

pub extension F32Ext for f32 {
    pub fn to_bits(this): u32 {
        unsafe core::mem::transmute(*this)
    }

    pub fn sqrt(this): f32 {
        libc::sqrtf(*this)
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
        libc::sqrt(*this)
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            super::ryu::Buffer::new().format(*this).format(f);
        }
    }
}
