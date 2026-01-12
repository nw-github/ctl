use std::hash::*;
use std::ops::*;
use std::fmt::*;
use std::reflect::*;
use super::ByteSpanExt;

pub extension U8Impl for u8 {
    pub fn is_ascii(my this): bool => this < 0b1000_0000;
    pub fn is_ascii_whitespace(my this): bool => this is b'\t' | b'\n' | b'\x0C' | b'\r' | b' ';
    pub fn is_ascii_upper(my this): bool => this is b'A'..=b'Z';
    pub fn is_ascii_lower(my this): bool => this is b'a'..=b'z';
    pub fn is_ascii_digit(my this): bool => this is b'0'..=b'9';
    pub fn is_ascii_hexdigit(my this): bool => this is b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F';

    pub fn make_ascii_upper(mut this) => *this = this.to_ascii_upper();
    pub fn make_ascii_lower(mut this) => *this = this.to_ascii_upper();

    pub fn to_ascii_upper(my this): u8 => this ^ (0b100000 * this.is_ascii_upper() as u8);
    pub fn to_ascii_lower(my this): u8 => this ^ (0b100000 * this.is_ascii_lower() as u8);
}

mod gcc_intrin {
    use super::Integral;
    use super::Unsigned;

    // TODO: compiler independent fallback for these functions
    $[c_macro]
    pub extern fn __builtin_add_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;

    $[c_macro]
    pub extern fn __builtin_sub_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;

    $[c_macro]
    pub extern fn __builtin_mul_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;

    $[c_macro]
    pub extern fn __builtin_popcountg<T: Unsigned>(u: T): c_int;

    $[c_macro]
    pub extern fn __builtin_clzg<T: Unsigned>(u: T): c_int;

    $[c_macro]
    pub extern fn __builtin_ctzg<T: Unsigned>(u: T): c_int;
}

pub extension IntegralImpl<T: Integral> for T {
    impl TotallyOrdered { }

    pub fn +(this, rhs: T): T {
        let (val, _overflow) = this.overflowing_add(rhs);
        $[feature(overflow_checks)]
        if _overflow {
            panic("integer overflow adding values {val} and {rhs}");
        }
        val
    }

    pub fn -(this, rhs: T): T {
        let (val, _overflow) = this.overflowing_sub(rhs);
        $[feature(overflow_checks)]
        if _overflow {
            panic("integer overflow subtracting values {val} and {rhs}");
        }
        val
    }

    pub fn *(this, rhs: T): T {
        let (val, _overflow) = this.overflowing_mul(rhs);
        $[feature(overflow_checks)]
        if _overflow {
            panic("integer overflow multiplying values {val} and {rhs}");
        }
        val
    }

    pub fn /(this, rhs: T): T {
        let (val, _overflow) = this.overflowing_div(rhs);
        $[feature(overflow_checks)]
        if _overflow {
            panic("integer overflow dividing values {val} and {rhs}");
        }
        val
    }

    $[intrinsic(binary_op)]
    pub fn %(this, rhs: T): T => this % rhs;

    pub fn +=(mut this, rhs: T) => *this = *this + rhs;

    pub fn -=(mut this, rhs: T) => *this = *this - rhs;

    pub fn *=(mut this, rhs: T) => *this = *this * rhs;

    pub fn /=(mut this, rhs: T) => *this = *this / rhs;

    $[intrinsic(binary_op)]
    pub fn %=(mut this, rhs: T) => *this %= rhs;

    $[intrinsic(binary_op)]
    pub fn &(this, rhs: T): T => this & rhs;

    $[intrinsic(binary_op)]
    pub fn |(this, rhs: T): T => this | rhs;

    $[intrinsic(binary_op)]
    pub fn ^(this, rhs: T): T => this ^ rhs;

    $[intrinsic(binary_op)]
    pub fn <<(this, rhs: u32): T => this << rhs;

    $[intrinsic(binary_op)]
    pub fn >>(this, rhs: u32): T => this >> rhs;

    $[intrinsic(binary_op)]
    pub fn &=(mut this, rhs: T) => *this &= rhs;

    $[intrinsic(binary_op)]
    pub fn |=(mut this, rhs: T) => *this |= rhs;

    $[intrinsic(binary_op)]
    pub fn ^=(mut this, rhs: T) => *this ^= rhs;

    $[intrinsic(binary_op)]
    pub fn <<=(mut this, rhs: u32) => *this <<= rhs;

    $[intrinsic(binary_op)]
    pub fn >>=(mut this, rhs: u32) => *this >>= rhs;

    $[intrinsic(unary_op)]
    pub fn !(this): T => !this;

    $[intrinsic(unary_op)]
    pub fn ++(mut this) { (*this)++; }

    $[intrinsic(unary_op)]
    pub fn --(mut this) { (*this)--; }

    $[inline]
    pub fn wrapping_add(this, rhs: T): T => this.overflowing_add(rhs).0;

    $[inline]
    pub fn wrapping_sub(this, rhs: T): T => this.overflowing_sub(rhs).0;

    $[inline]
    pub fn wrapping_mul(this, rhs: T): T => this.overflowing_mul(rhs).0;

    $[inline]
    pub fn wrapping_div(this, rhs: T): T => this.overflowing_div(rhs).0;

    $[inline]
    pub fn overflowing_add(this, rhs: T): (T, bool) {
        mut out: T;
        let res = unsafe gcc_intrin::__builtin_add_overflow(*this, rhs, &raw mut out);
        (out, res)
    }

    $[inline]
    pub fn overflowing_sub(this, rhs: T): (T, bool) {
        mut out: T;
        let res = unsafe gcc_intrin::__builtin_sub_overflow(*this, rhs, &raw mut out);
        (out, res)
    }

    $[inline]
    pub fn overflowing_mul(this, rhs: T): (T, bool) {
        mut out: T;
        let res = unsafe gcc_intrin::__builtin_mul_overflow(*this, rhs, &raw mut out);
        (out, res)
    }

    $[inline]
    pub fn overflowing_div(this, rhs: T): (T, bool) {
        // Only signed division can overflow
        if T::min_value() != 0.cast() {
            if this == T::min_value() and rhs == (-1).cast() {
                return (*this, true);
            }
        }
        (unsafe this.unchecked_div(rhs), false)
    }

    $[inline]
    pub fn checked_add(this, rhs: T): ?T => this.overflowing_add(rhs) is (out, false) then out;

    $[inline]
    pub fn checked_sub(this, rhs: T): ?T => this.overflowing_sub(rhs) is (out, false) then out;

    $[inline]
    pub fn checked_mul(this, rhs: T): ?T => this.overflowing_mul(rhs) is (out, false) then out;

    $[inline]
    pub fn checked_div(this, rhs: T): ?T => this.overflowing_div(rhs) is (out, false) then out;

    $[intrinsic(binary_op)]
    pub unsafe fn unchecked_add(this, rhs: T): T => this + rhs;

    $[intrinsic(binary_op)]
    pub unsafe fn unchecked_sub(this, rhs: T): T => this - rhs;

    $[intrinsic(binary_op)]
    pub unsafe fn unchecked_mul(this, rhs: T): T => this * rhs;

    $[intrinsic(binary_op)]
    pub unsafe fn unchecked_div(this, rhs: T): T => this / rhs;

    $[inline]
    pub fn saturating_add(this, rhs: T): T {
        if this.checked_add(rhs) is ?out {
            out
        } else if rhs < 0.cast() {
            T::min_value()
        } else {
            T::max_value()
        }
    }

    $[intrinsic]
    pub fn max_value(): T => T::max_value();

    $[intrinsic]
    pub fn min_value(): T => T::min_value();

    /// C-style cast from `this` to type U with overflow/truncation.
    $[intrinsic(numeric_cast)]
    pub fn cast<U: Integral>(my this): U => this.cast();

    /// Cast `this` to type `U` if the value of this is exactly representable in U
    $[inline]
    pub fn try_cast<U: Integral>(my this): ?U {
        let rhs: U = this.cast();
        this == rhs.cast() then rhs
    }

    $[inline]
    pub fn swap_bytes(my mut this): T {
        // TODO: make calls to compiler intrinsics when possible
        //  GCC & clang are smart enough to optimize this as-is to a bswap instruction
        //  when possible, other compilers (cough cough MSVC) may not.
        let span = this.as_byte_span_mut();
        for i in 0u..span.len() / 2 {
            span.swap(i, span.len() - i - 1);
        }
        this
    }

    fn from_str_radix_common(chars: std::string::Chars, radix: u32): ?T {
        mut value: ?T = null;
        for ch in chars {
            let digit = ch.to_digit(radix)?.try_cast::<T>()?;
            if &mut value is ?value {
                *value = value.checked_mul(radix.try_cast::<T>()?)?.checked_add(digit)?;
            } else {
                value = digit;
            }
        }
        value
    }

    /// Formats the digits of `this` into `buf` according to `radix`. Does not add a sign or prefix.
    ///
    /// `radix` must be between 2 and 36 inclusive
    /// `buf`   must have a length of at least size_of::<T> * 8
    ///
    /// Returns the position of the start of the digits within `buf`
    unsafe fn write_digits(my mut this, buf: [mut u8..], radix: u32, upper: bool): uint {
        static UPPER_DIGITS: *[u8; 36] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        static LOWER_DIGITS: *[u8; 36] = b"0123456789abcdefghijklmnopqrstuvwxyz";

        let digits = upper then UPPER_DIGITS else LOWER_DIGITS;
        let radix = radix as! int;
        mut pos = buf.len();
        loop {
            // TODO: do this at CTL compile time
            let digit = if std::mem::size_of::<T>() >= std::mem::size_of::<int>() {
                let radix: T = radix.cast();
                let digit: int = (this % radix).cast();
                this /= radix;
                digit
            } else {
                let dividend: int = this.cast();
                this = (dividend / radix).cast();
                (dividend % radix).cast()
            };
            let digit = (digit < 0 then -digit else digit).cast::<uint>();
            unsafe *buf.as_raw_mut().add(--pos) = *digits.get_unchecked(digit);
        } while this != 0.cast();

        pos
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => this.format_into(f, 10, null);
    }

    impl Format {
        fn fmt(this, f: *mut Formatter) => this.format_into(f, 10, null);
        fn bin(this, f: *mut Formatter) => this.format_into(f, 2, "0b");
        fn hex(this, f: *mut Formatter) => this.format_into(f, 16, "0x");
        fn oct(this, f: *mut Formatter) => this.format_into(f, 8, "0o");
    }

    fn format_into(my mut this, f: *mut Formatter, radix: u32, prefix: ?str) {
        if std::mem::size_of::<T>() <= std::mem::size_of::<u128>() {
            mut buf: [u8; /* std::mem::size_of::<u128>() * 8 */ 128];
            let start = unsafe this.write_digits(buf.subspan_mut_unchecked(..), radix, f.options().upper);
            let digits = unsafe str::from_utf8_unchecked(buf.subspan_unchecked(start..));
            f.pad_integral(negative: this < 0.cast(), digits:, prefix:);
        } else {
            "<TODO: format int >128 bits>".fmt(f);
        }
    }
}

pub extension SignedImpl<T: Signed> for T {
    // TODO: complain if -this == this (overflow) in debug mode
    pub fn abs(my this): T => this < 0.cast() then -this else this;

    $[intrinsic(unary_op)]
    pub fn -(this): T => -this;

    pub fn from_str_radix(s: str, radix: u32): ?T {
        mut chars = s.chars();
        let negative = match chars.next()? {
            '-' => true,
            '+' => false,
            _ => {
                chars = s.chars();
                false
            }
        };

        let val = T::from_str_radix_common(chars, radix)?;
        negative then val.checked_mul((-1).cast()) else val
    }
}

pub extension UnsignedImpl<T: Unsigned> for T {
    pub fn from_str_radix(s: str, radix: u32): ?T {
        mut chars = s.chars();
        if !(chars.next()? is '+') {
            chars = s.chars();
        }

        T::from_str_radix_common(chars, radix)
    }

    // TODO: popcountg and friends only work for unsigned integers/BitInts under 128 bits, and are
    // only supported on later versions of clang & gcc. Generate a fallback for other types.
    //
    // to implement this generically for signed types, we need a way to convert from iN to uN
    // generically.
    //
    // If we allowed traits to carry associated types:
    //       trait Signed { type UI: Unsigned; }
    //
    //       // SignedImpl
    //       pub fn count_ones(this) => (this as! T::UI).count_ones();
    // In the meantime, an intrinsic might be the way to go

    pub fn count_ones(my this): u32 => unsafe gcc_intrin::__builtin_popcountg(this) as! u32;
    pub fn count_zeros(my this): u32 => unsafe gcc_intrin::__builtin_popcountg(!this) as! u32;
    pub fn leading_zeros(my this): u32 => unsafe gcc_intrin::__builtin_clzg(this) as! u32;
    pub fn trailing_zeros(my this): u32 => unsafe gcc_intrin::__builtin_ctzg(this) as! u32;
    pub fn is_power_of_two(my this): bool => this.count_ones() == 1;
}

pub extension U32Impl for u32 {
    pub fn from_le_bytes(bytes: [u8; 4]): u32 => unsafe std::mem::bit_cast(bytes);
    pub fn from_ne_bytes(bytes: [u8; 4]): u32 => unsafe std::mem::bit_cast(bytes);
    pub fn from_be_bytes(bytes: [u8; 4]): u32 => unsafe std::mem::bit_cast::<[u8; 4], u32>(bytes).swap_bytes();
}

mod test {
    unittest "format numeric bounds" {
        assert_eq("{i32::min_value()}".to_str(), "-2147483648");
        assert_eq("{i32::max_value()}".to_str(), "2147483647");
        assert_eq("{u32::max_value()}".to_str(), "4294967295");

        assert_eq("{i64::min_value()}".to_str(), "-9223372036854775808");
        assert_eq("{i64::max_value()}".to_str(), "9223372036854775807");
        assert_eq("{u64::max_value()}".to_str(), "18446744073709551615");

        assert_eq("{i65::min_value()}".to_str(), "-18446744073709551616");
        assert_eq("{i65::max_value()}".to_str(), "18446744073709551615");
        assert_eq("{u65::max_value()}".to_str(), "36893488147419103231");

        assert_eq("{-0x1_0000_0000_0000_0000i65}".to_str(), "-18446744073709551616");
        assert_eq("{ 0x0_ffff_ffff_ffff_ffffi65}".to_str(), "18446744073709551615");
        assert_eq("{ 0x1_ffff_ffff_ffff_ffffu65}".to_str(), "36893488147419103231");
    }

    unittest "div overflow" {
        let [a, b] = [i2::min_value(), -1];
        let x = a.checked_div(b);
        let y = a.wrapping_div(b);
        let z = a.overflowing_div(b);

        assert_eq(x, null);
        assert_eq(y, -2);
        assert_eq(z.0, -2);
        assert_eq(z.1, true);
    }
}