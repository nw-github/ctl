use std::hash::*;
use std::ops::*;
use std::fmt::*;

pub extension CharImpl for char {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) => (*this as u32).hash(h);
    }

    impl Eq<This> {
        @(intrinsic(binary_op))
        fn eq(this, rhs: *This): bool => this == rhs;

        @(intrinsic(binary_op))
        fn ne(this, rhs: *This): bool => this != rhs;
    }

    impl Cmp<This> {
        @(intrinsic(binary_op))
        fn cmp(this, rhs: *This): Ordering => this <=> rhs;

        @(intrinsic(binary_op))
        fn ge(this, rhs: *This): bool => this >= rhs;

        @(intrinsic(binary_op))
        fn gt(this, rhs: *This): bool => this > rhs;

        @(intrinsic(binary_op))
        fn le(this, rhs: *This): bool => this <= rhs;

        @(intrinsic(binary_op))
        fn lt(this, rhs: *This): bool => this < rhs;
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => write(f, "'{this}'");
    }

    impl Format {
        fn fmt(this, f: *mut Formatter) => f.pad(this.encode_utf8(&mut [0; 4]));
    }

    pub unsafe fn from_u32_unchecked(v: u32): char => unsafe std::mem::transmute(v);

    pub fn from_u32(cp: u32): ?char {
        if cp <= char::max_value() as u32
            and !(LEAD_SURROGATE_MIN..=TRAIL_SURROGATE_MAX).contains(&cp)
        {
            unsafe char::from_u32_unchecked(cp)
        }
    }

    pub fn len_utf8(my this): uint {
        match this as u32 {
            ..0x80 => 1,
            ..0x800 => 2,
            ..0x10000 => 3,
            _ => 4,
        }
    }

    pub fn len_utf16(my this): uint => this as u32 <= 0xffff then 1 else 2;

    pub fn is_ascii(my this): bool => (this as u32) < 0b1000_0000;
    pub fn is_ascii_upper(my this): bool => this is 'A'..='Z';
    pub fn is_ascii_lower(my this): bool => this is 'a'..='z';
    pub fn is_ascii_digit(my this): bool => this is '0'..='9';
    pub fn is_ascii_hexdigit(my this): bool => this is '0'..='9' | 'a'..='f' | 'A'..='F';
    pub fn is_ascii_whitespace(my this): bool => this is '\t' | '\n' | '\x0C' | '\r' | ' ';

    pub fn make_ascii_upper(mut this) => *this = this.to_ascii_upper();
    pub fn make_ascii_lower(mut this) => *this = this.to_ascii_lower();

    pub fn to_ascii_upper(my this): char {
        unsafe char::from_u32_unchecked(this as u32 ^ (0b10_0000 * this.is_ascii_lower() as u32))
    }

    pub fn to_ascii_lower(my this): char {
        unsafe char::from_u32_unchecked(this as u32 ^ (0b10_0000 * this.is_ascii_upper() as u32))
    }

    pub fn to_digit(my this, radix: u32): ?u32 {
        // don't mind if I do
        // https://github.com/rust-lang/rust/blob/9afe7136958edaa403f0b0eb00f0353c125b7352/library/core/src/char/methods.rs#L378

        // If not a digit, a number greater than radix will be created.
        mut digit = (this as u32).wrapping_sub('0' as u32);
        if radix > 10 {
            guard radix <= 36 else {
                panic("invalid radix {radix}: must be <= 36");
            }

            if digit < 10 {
                return digit;
            }
            // Force the 6th bit to be set to ensure ascii is lower case.
            digit = (this as u32 | 0b10_0000).wrapping_sub('a' as u32).saturating_add(10);
        }
        (digit < radix).then_some(digit)
    }

    pub fn min_value(): char => '\0';
    pub fn max_value(): char => '\u{10ffff}';
    pub fn replacement_marker(): char => '\u{fffd}';

    pub fn encode_utf8(my this, buf: *mut [u8; 4]): str {
        unsafe this.encode_utf8_unchecked(buf.as_raw_mut())
    }

    pub unsafe fn encode_utf8_unchecked(my this, ptr: ^mut u8): str {
        unsafe {
            let cp = this as u32;
            let len_utf8 = this.len_utf8();
            match len_utf8 {
                1 => {
                    *ptr = cp as! u8;
                }
                2 => {
                    *ptr        = ((cp >> 6) | 0xc0) as! u8;
                    *ptr.add(1) = ((cp & 0x3f) | 0x80) as! u8;
                }
                3 => {
                    *ptr        = ((cp >> 12) | 0xe0) as! u8;
                    *ptr.add(1) = (((cp >> 6) & 0x3f) | 0x80) as! u8;
                    *ptr.add(2) = ((cp & 0x3f) | 0x80) as! u8;
                }
                4 => {
                    *ptr        = ((cp >> 18) | 0xf0) as! u8;
                    *ptr.add(1) = (((cp >> 12) & 0x3f) | 0x80) as! u8;
                    *ptr.add(2) = (((cp >> 6) & 0x3f) | 0x80) as! u8;
                    *ptr.add(3) = ((cp & 0x3f) | 0x80) as! u8;
                }
                _ => std::hint::unreachable_unchecked(),
            }
            str::from_utf8_unchecked(Span::new(ptr, len_utf8))
        }
    }

    pub fn encode_utf16(my this): (u16, ?u16) {
        let cp = this as u32;
        if cp <= 0xffff {
            (cp as! u16, null)
        } else {
            (((cp >> 10) + LEAD_OFFSET) as! u16, ((cp & 0x3ff) + TRAIL_SURROGATE_MIN) as! u16)
        }
    }
}

const LEAD_SURROGATE_MIN: u32  = 0xd800;
// const LEAD_SURROGATE_MAX: u32  = 0xdbff;
const TRAIL_SURROGATE_MIN: u32 = 0xdc00;
const TRAIL_SURROGATE_MAX: u32 = 0xdfff;
const LEAD_OFFSET: u32         = LEAD_SURROGATE_MIN - (0x10000 >> 10);
// const SURROGATE_OFFSET: u32    = 0x10000 - (LEAD_SURROGATE_MIN << 10) - TRAIL_SURROGATE_MIN;
