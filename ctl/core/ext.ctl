use core::hash::*;
use core::ops::*;
use core::fmt::*;
use core::span::*;
use core::string::str;

extension<T> _ for T {
    pub fn as_byte_span(this): [u8..] {
        unsafe Span::new(this as *raw u8, core::mem::size_of::<T>())
    }
}

pub extension<T: core::reflect::Numeric> NumberExt for T {
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
                SpanMut::new(unsafe &mut [0u8; 4] as *raw u8, 4),
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
            let ptr = buf.as_raw();
            match len_utf8 {
                1 => {
                    *ptr = cp as! u8;
                }
                2 => {
                    *ptr = ((cp >> 6) | 0xc0) as! u8;
                    *core::ptr::raw_add(ptr, 1) = ((cp & 0x3f) | 0x80) as! u8;
                }
                3 => {
                    *ptr = ((cp >> 12) | 0xe0) as! u8;
                    *core::ptr::raw_add(ptr, 1) = (((cp >> 6) & 0x3f) | 0x80) as! u8;
                    *core::ptr::raw_add(ptr, 2) = ((cp & 0x3f) | 0x80) as! u8;
                }
                4 => {
                    *ptr = ((cp >> 18) | 0xf0) as! u8;
                    *core::ptr::raw_add(ptr, 1) = (((cp >> 12) & 0x3f) | 0x80) as! u8;
                    *core::ptr::raw_add(ptr, 2) = (((cp >> 6) & 0x3f) | 0x80) as! u8;
                    *core::ptr::raw_add(ptr, 3) = ((cp & 0x3f) | 0x80) as! u8;
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
            f.write(if *this { "true".as_bytes() } else { "false".as_bytes() });
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
            f.write("void".as_bytes());
        }
    }
}
