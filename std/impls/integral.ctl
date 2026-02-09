use std::fmt::{Debug, Format, Formatter};
use std::reflect::{Integral, Signed, Unsigned};
use std::mem::Uninit;

extension u8 {
    pub fn is_ascii(my this): bool => (this as char).is_ascii();
    pub fn is_ascii_whitespace(my this): bool => (this as char).is_ascii_whitespace();
    pub fn is_ascii_upper(my this): bool => (this as char).is_ascii_upper();
    pub fn is_ascii_lower(my this): bool => (this as char).is_ascii_lower();
    pub fn is_ascii_alphabetic(my this): bool => (this as char).is_ascii_alphabetic();
    pub fn is_ascii_alphanumeric(my this): bool => (this as char).is_ascii_alphanumeric();
    pub fn is_ascii_digit(my this): bool => (this as char).is_ascii_digit();
    pub fn is_ascii_hexdigit(my this): bool => (this as char).is_ascii_hexdigit();

    pub fn make_ascii_upper(mut this) => *this = this.to_ascii_upper();
    pub fn make_ascii_lower(mut this) => *this = this.to_ascii_upper();

    pub fn to_ascii_upper(my this): u8 => this ^ (0b10_0000 * this.is_ascii_upper() as u8);
    pub fn to_ascii_lower(my this): u8 => this ^ (0b10_0000 * this.is_ascii_lower() as u8);
}

mod gcc_intrin {
    use super::{Integral, Unsigned};

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

extension<Int: Integral> Int {
    impl std::ops::TotallyOrdered { }

    pub fn +(this, rhs: Int): Int {
        let (val, _overflow) = this.overflowing_add(rhs);
        $[cfg("!ctl:no-overflow-checks")]
        if _overflow {
            panic("integer overflow adding values {val} and {rhs}");
        }
        val
    }

    pub fn -(this, rhs: Int): Int {
        let (val, _overflow) = this.overflowing_sub(rhs);
        $[cfg("!ctl:no-overflow-checks")]
        if _overflow {
            panic("integer overflow subtracting values {val} and {rhs}");
        }
        val
    }

    pub fn *(this, rhs: Int): Int {
        let (val, _overflow) = this.overflowing_mul(rhs);
        $[cfg("!ctl:no-overflow-checks")]
        if _overflow {
            panic("integer overflow multiplying values {val} and {rhs}");
        }
        val
    }

    pub fn /(this, rhs: Int): Int {
        let (val, _overflow) = this.overflowing_div(rhs);
        $[cfg("!ctl:no-overflow-checks")]
        if _overflow {
            panic("integer overflow dividing values {val} and {rhs}");
        }
        val
    }

    $[intrinsic(binary_op)]
    pub fn %(this, rhs: Int): Int => this % rhs;

    pub fn +=(mut this, rhs: Int) => *this = *this + rhs;

    pub fn -=(mut this, rhs: Int) => *this = *this - rhs;

    pub fn *=(mut this, rhs: Int) => *this = *this * rhs;

    pub fn /=(mut this, rhs: Int) => *this = *this / rhs;

    $[intrinsic(binary_op)]
    pub fn %=(mut this, rhs: Int) => *this %= rhs;

    $[intrinsic(binary_op)]
    pub fn &(this, rhs: Int): Int => this & rhs;

    $[intrinsic(binary_op)]
    pub fn |(this, rhs: Int): Int => this | rhs;

    $[intrinsic(binary_op)]
    pub fn ^(this, rhs: Int): Int => this ^ rhs;

    $[intrinsic(binary_op)]
    pub fn <<(this, rhs: u32): Int => this << rhs;

    $[intrinsic(binary_op)]
    pub fn >>(this, rhs: u32): Int => this >> rhs;

    $[intrinsic(binary_op)]
    pub fn &=(mut this, rhs: Int) => *this &= rhs;

    $[intrinsic(binary_op)]
    pub fn |=(mut this, rhs: Int) => *this |= rhs;

    $[intrinsic(binary_op)]
    pub fn ^=(mut this, rhs: Int) => *this ^= rhs;

    $[intrinsic(binary_op)]
    pub fn <<=(mut this, rhs: u32) => *this <<= rhs;

    $[intrinsic(binary_op)]
    pub fn >>=(mut this, rhs: u32) => *this >>= rhs;

    $[intrinsic(unary_op)]
    pub fn !(this): Int => !this;

    $[intrinsic(unary_op)]
    pub fn ++(mut this) { (*this)++; }

    $[intrinsic(unary_op)]
    pub fn --(mut this) { (*this)--; }

    $[inline]
    pub fn wrapping_add(this, rhs: Int): Int => this.overflowing_add(rhs).0;

    $[inline]
    pub fn wrapping_sub(this, rhs: Int): Int => this.overflowing_sub(rhs).0;

    $[inline]
    pub fn wrapping_mul(this, rhs: Int): Int => this.overflowing_mul(rhs).0;

    $[inline]
    pub fn wrapping_div(this, rhs: Int): Int => this.overflowing_div(rhs).0;

    $[inline]
    pub fn overflowing_add(this, rhs: Int): (Int, bool) {
        unsafe Uninit::assume_init_by(|=this, =rhs, out| {
            unsafe gcc_intrin::__builtin_add_overflow(*this, rhs, out)
        })
    }

    $[inline]
    pub fn overflowing_sub(this, rhs: Int): (Int, bool) {
        unsafe Uninit::assume_init_by(|=this, =rhs, out| {
            unsafe gcc_intrin::__builtin_sub_overflow(*this, rhs, out)
        })
    }

    $[inline]
    pub fn overflowing_mul(this, rhs: Int): (Int, bool) {
        unsafe Uninit::assume_init_by(|=this, =rhs, out| {
            unsafe gcc_intrin::__builtin_mul_overflow(*this, rhs, out)
        })
    }

    $[inline]
    pub fn overflowing_div(this, rhs: Int): (Int, bool) {
        // Only signed division can overflow
        if Int::is_signed() {
            if this == Int::min_value() and rhs == (-1).cast() {
                return (*this, true);
            }
        }
        (unsafe this.unchecked_div(rhs), false)
    }

    $[inline]
    pub fn checked_add(this, rhs: Int): ?Int => this.overflowing_add(rhs) is (out, false) then out;

    $[inline]
    pub fn checked_sub(this, rhs: Int): ?Int => this.overflowing_sub(rhs) is (out, false) then out;

    $[inline]
    pub fn checked_mul(this, rhs: Int): ?Int => this.overflowing_mul(rhs) is (out, false) then out;

    $[inline]
    pub fn checked_div(this, rhs: Int): ?Int => this.overflowing_div(rhs) is (out, false) then out;

    $[intrinsic(binary_op)]
    pub unsafe fn unchecked_add(this, rhs: Int): Int => this + rhs;

    $[intrinsic(binary_op)]
    pub unsafe fn unchecked_sub(this, rhs: Int): Int => this - rhs;

    $[intrinsic(binary_op)]
    pub unsafe fn unchecked_mul(this, rhs: Int): Int => this * rhs;

    $[intrinsic(binary_op)]
    pub unsafe fn unchecked_div(this, rhs: Int): Int => this / rhs;

    $[inline]
    pub fn saturating_add(this, rhs: Int): Int {
        if this.checked_add(rhs) is ?out {
            out
        } else if rhs < 0.cast() {
            Int::min_value()
        } else {
            Int::max_value()
        }
    }

    $[intrinsic]
    pub fn max_value(): Int => Int::max_value();

    $[intrinsic]
    pub fn min_value(): Int => Int::min_value();

    /// C-style cast from `from` to This with wraparound/truncation.
    $[intrinsic(numeric_cast)]
    pub fn from<U: Integral>(from: U): Int => Int::from(from);

    /// Cast `this` to type `U` if the value of this is exactly representable in U
    $[inline]
    pub fn try_cast<U: Integral>(my this): ?U {
        let rhs: U = this.cast();
        this == rhs.cast() then rhs
    }

    $[inline]
    pub fn try_from<U: Integral>(val: U): ?Int => val.try_cast::<Int>();

    $[inline]
    pub fn saturating_cast<U: Integral>(my this): U {
        // TODO: actually saturate
        this.cast()
    }

    $[inline]
    pub fn swap_bytes(my mut this): Int {
        // TODO: make calls to compiler intrinsics when possible
        //  GCC & clang are smart enough to optimize this as-is to a bswap instruction
        //  when possible, other compilers (cough cough MSVC) may not.
        let span = this.as_byte_span_mut();
        for i in 0u..span.len() / 2 {
            span.swap(i, span.len() - i - 1);
        }
        this
    }

    /// Formats the digits of `this` into `buf` according to `radix`. Does not add a sign or prefix.
    ///
    /// `radix` must be between 2 and 36 inclusive
    /// `buf`   must have a length of at least size_of::<T> * 8
    ///
    /// Returns the position of the start of the digits within `buf`
    lib unsafe fn write_digits(my mut this, buf: [mut Uninit<u8>..], radix: int, upper: bool): uint {
        static UPPER_DIGITS: *[u8; 36] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        static LOWER_DIGITS: *[u8; 36] = b"0123456789abcdefghijklmnopqrstuvwxyz";

        let digits = upper then UPPER_DIGITS else LOWER_DIGITS;
        mut pos = buf.len();
        loop {
            // TODO: do this at CTL compile time
            let digit = if std::mem::size_of::<Int>() >= std::mem::size_of::<int>() {
                let radix: Int = radix.cast();
                let digit: int = (this % radix).cast();
                this /= radix;
                digit
            } else {
                let dividend: int = this.cast();
                this = (dividend / radix).cast();
                (dividend % radix).cast()
            };
            let digit = (digit < 0 then -digit else digit).cast::<uint>();
            unsafe *buf.as_raw_mut().add(--pos) = Uninit::new(*digits.get_unchecked(digit));
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

    fn format_into(this, f: *mut Formatter, radix: int, prefix: ?str) {
        // TODO: const if
        if std::mem::size_of::<Int>() <= std::mem::size_of::<u128>() {
            mut buf = Uninit::<[u8; 128]>::uninit();
            let start = unsafe this.write_digits(buf.as_bytes_mut(), radix, f.options().upper);
            let digits = unsafe str::from_utf8_unchecked(buf.assume_init_bytes(start..));
            f.pad_integral(negative: this < 0.cast(), digits:, prefix:);
        } else {
            // Don't generate a large stack buffer for small types
            this.format_large(f, radix, prefix)
        }
    }

    $[inline(never)]
    fn format_large(this, f: *mut Formatter, radix: int, prefix: ?str) {
        mut buf = Uninit::<[u8; 65535]>::uninit();
        let start = unsafe this.write_digits(buf.as_bytes_mut(), radix, f.options().upper);
        let digits = unsafe str::from_utf8_unchecked(buf.assume_init_bytes(start..));
        f.pad_integral(negative: this < 0.cast(), digits:, prefix:);
    }

    pub fn from_str_radix(s: str, radix: u32): ?Int {
        fn parse_digits<T: Integral>(chars: std::str::Chars, radix: u32): ?T {
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

        mut chars = s.chars();
        if Int::is_signed() {
            let negative = match chars.next()? {
                '-' => true,
                '+' => false,
                _ => {
                    chars = s.chars();
                    false
                }
            };

            let val = parse_digits::<Int>(chars, radix)?;
            negative then val.checked_mul((-1).cast()) else val
        } else {
            if !(chars.next()? is '+') {
                chars = s.chars();
            }

            parse_digits(chars, radix)
        }
    }

    pub fn is_signed(): bool => Int::min_value() < 0.cast();
}

extension<SInt: Signed> SInt {
    // TODO: complain if -this == this (overflow) in debug mode
    pub fn abs(my this): SInt => this < 0.cast() then -this else this;

    $[intrinsic(unary_op)]
    pub fn -(this): SInt => -this;
}

extension<UInt: Unsigned> UInt {
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

    pub fn count_ones(my this): u32 => unsafe gcc_intrin::__builtin_popcountg(this).cast();
    pub fn count_zeros(my this): u32 => unsafe gcc_intrin::__builtin_popcountg(!this).cast();
    pub fn leading_zeros(my this): u32 => unsafe gcc_intrin::__builtin_clzg(this).cast();
    pub fn trailing_zeros(my this): u32 => unsafe gcc_intrin::__builtin_ctzg(this).cast();
    pub fn is_power_of_two(my this): bool => this.count_ones() == 1;
}

use std::mem::{size_of, bit_cast};

extension u16 {
    pub fn from_le_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_ne_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_be_bytes(bytes: [u8; size_of::<This>()]): This => This::from_le_bytes(bytes).swap_bytes();

    pub fn to_le_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_ne_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_be_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this.swap_bytes());
}

extension u32 {
    // TODO: make these a macro
    pub fn from_le_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_ne_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_be_bytes(bytes: [u8; size_of::<This>()]): This => This::from_le_bytes(bytes).swap_bytes();

    pub fn to_le_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_ne_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_be_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this.swap_bytes());
}

extension u64 {
    pub fn from_le_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_ne_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_be_bytes(bytes: [u8; size_of::<This>()]): This => This::from_le_bytes(bytes).swap_bytes();

    pub fn to_le_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_ne_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_be_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this.swap_bytes());
}

extension uint {
    pub fn from_le_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_ne_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_be_bytes(bytes: [u8; size_of::<This>()]): This => This::from_le_bytes(bytes).swap_bytes();

    pub fn to_le_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_ne_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_be_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this.swap_bytes());

    $[intrinsic(numeric_cast)]
    pub fn to_raw<T>(my this): ^T => This::to_raw(this);

    $[intrinsic(numeric_cast)]
    pub fn to_raw_mut<T>(my this): ^mut T => This::to_raw_mut(this);
}

extension int {
    pub fn from_le_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_ne_bytes(bytes: [u8; size_of::<This>()]): This => unsafe bit_cast(bytes);
    pub fn from_be_bytes(bytes: [u8; size_of::<This>()]): This => This::from_le_bytes(bytes).swap_bytes();

    pub fn to_le_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_ne_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this);
    pub fn to_be_bytes(my this): [u8; size_of::<This>()] => unsafe bit_cast(this.swap_bytes());

    $[intrinsic(numeric_cast)]
    pub fn to_raw<T>(my this): ^T => This::to_raw(this);

    $[intrinsic(numeric_cast)]
    pub fn to_raw_mut<T>(my this): ^mut T => This::to_raw_mut(this);
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