use core::hash::*;
use core::ops::*;
use core::fmt::*;
use core::span::*;
use core::reflect::*;
use core::string::str;

#(intrinsic)
import fn numeric_abs<T: Signed>(_: T): T;

#(intrinsic)
import fn numeric_cast<T: Numeric, U: Numeric>(_: T): U;

static DIGITS: *[u8; 36] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

extension<T> _ for T {
    pub fn as_byte_span(this): [u8..] {
        unsafe Span::new(this as *raw u8, core::mem::size_of::<T>())
    }

    pub unsafe fn as_byte_span_mut(mut this): [mut u8..] {
        unsafe SpanMut::new(this as *raw u8, core::mem::size_of::<T>())
    }
}

pub extension<T: Numeric> NumberExt for T {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.as_byte_span());
        }
    }

    impl Cmp<T> {
        #(binary_op(cmp))
        fn cmp(this, rhs: *T): Ordering { this <=> rhs }

        #(binary_op(ge))
        fn ge(this, rhs: *T): bool { this >= rhs }

        #(binary_op(gt))
        fn gt(this, rhs: *T): bool { this > rhs }

        #(binary_op(le))
        fn le(this, rhs: *T): bool { this <= rhs }

        #(binary_op(lt))
        fn lt(this, rhs: *T): bool { this < rhs }
    }

    impl Eq<T> {
        #(binary_op(eq))
        fn eq(this, rhs: *T): bool { this == rhs }

        #(binary_op(ne))
        fn ne(this, rhs: *T): bool { this != rhs }
    }

    impl Add<T, T> {
        #(binary_op(add))
        fn add(this, rhs: T): T { this + rhs }
    }

    impl Sub<T, T> {
        #(binary_op(sub))
        fn sub(this, rhs: T): T { this - rhs }
    }

    impl Mul<T, T> {
        #(binary_op(mul))
        fn mul(this, rhs: T): T { this * rhs }
    }

    impl Div<T, T> {
        #(binary_op(div))
        fn div(this, rhs: T): T { this / rhs }
    }

    impl Rem<T, T> {
        #(binary_op(rem))
        fn rem(this, rhs: T): T { this % rhs }
    }

    impl And<T, T> {
        #(binary_op(and))
        fn and(this, rhs: T): T { this & rhs }
    }

    impl Or<T, T> {
        #(binary_op(or))
        fn or(this, rhs: T): T { this | rhs }
    }

    impl Xor<T, T> {
        #(binary_op(xor))
        fn xor(this, rhs: T): T { this ^ rhs }
    }

    impl Shl<T, T> {
        #(binary_op(shl))
        fn shl(this, rhs: T): T { this << rhs }
    }

    impl Shr<T, T> {
        #(binary_op(shr))
        fn shr(this, rhs: T): T { this >> rhs }
    }

    pub fn max(this, rhs: T): T {
        if *this > rhs { *this } else { rhs }
    }

    pub fn min(this, rhs: T): T {
        if *this > rhs { rhs } else { *this }
    }
}

pub extension<T: Numeric + Signed> SignedExt for T {
    pub fn abs(this): T {
        numeric_abs(*this)
    }

    pub fn to_str_radix_ex(this, radix: u32, buf: [mut u8..]): str {
        if radix < 2 || radix > 36 {
            core::panic("to_str_radix(): invalid radix");
        }

        let radix: T = numeric_cast(radix);
        mut pos = buf.len();
        mut val = this.abs();
        loop {
            *buf.get_mut(--pos)! = DIGITS[numeric_cast::<T, int>(val % radix)];
            val = val / radix;
        } while val != numeric_cast(0);

        if this < numeric_cast(0) {
            *buf.get_mut(--pos)! = b'-';
        }

        unsafe str::from_utf8_unchecked(buf.as_span().subspan(pos..))
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            mut buffer = [b'0'; core::mem::size_of::<i32>() * 8 + 1];
            this.to_str_radix_ex(10, unsafe buffer.as_byte_span_mut()).format(f);
        }
    }
}

pub extension<T: Numeric + Unsigned> UnsignedExt for T {
    pub fn to_str_radix_ex(this, radix: u32, buf: [mut u8..]): str {
        if radix < 2 || radix > 36 {
            core::panic("to_str_radix(): invalid radix");
        }

        let radix: T = numeric_cast(radix);
        mut pos = buf.len();
        mut val = *this;
        loop {
            *buf.get_mut(--pos)! = DIGITS[numeric_cast::<T, int>(val % radix)];
            val = val / radix;
        } while val != numeric_cast(0);

        unsafe str::from_utf8_unchecked(buf.as_span().subspan(pos..))
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            mut buffer = [b'0'; core::mem::size_of::<i32>() * 8];
            this.to_str_radix_ex(10, unsafe buffer.as_byte_span_mut()).format(f);
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
        #(binary_op(eq))
        fn eq(this, rhs: *This): bool { this == rhs }

        #(binary_op(ne))
        fn ne(this, rhs: *This): bool { this != rhs }
    }

    impl Cmp<This> {
        #(binary_op(cmp))
        fn cmp(this, rhs: *This): Ordering { this <=> rhs }

        #(binary_op(ge))
        fn ge(this, rhs: *This): bool { this >= rhs }

        #(binary_op(gt))
        fn gt(this, rhs: *This): bool { this > rhs }

        #(binary_op(le))
        fn le(this, rhs: *This): bool { this <= rhs }

        #(binary_op(lt))
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
        #(binary_op(eq))
        fn eq(this, rhs: *This): bool { this == rhs }

        #(binary_op(ne))
        fn ne(this, rhs: *This): bool { this != rhs }
    }

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
        fn eq(this, rhs: *This): bool { true }

        fn ne(this, rhs: *This): bool { false }
    }

    impl Format {
        fn format<F: Formatter>(this, f: *mut F) {
            "void".format(f);
        }
    }
}
