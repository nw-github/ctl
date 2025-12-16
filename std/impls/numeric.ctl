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

pub extension NumericImpl<T: Numeric> for T {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.as_byte_span());
        }
    }

    impl Cmp<T> {
        @(intrinsic(binary_op))
        fn cmp(this, rhs: *T): Ordering { this <=> rhs }

        @(intrinsic(binary_op))
        fn ge(this, rhs: *T): bool { this >= rhs }

        @(intrinsic(binary_op))
        fn gt(this, rhs: *T): bool { this > rhs }

        @(intrinsic(binary_op))
        fn le(this, rhs: *T): bool { this <= rhs }

        @(intrinsic(binary_op))
        fn lt(this, rhs: *T): bool { this < rhs }
    }

    impl Eq<T> {
        @(intrinsic(binary_op))
        fn eq(this, rhs: *T): bool { this == rhs }

        @(intrinsic(binary_op))
        fn ne(this, rhs: *T): bool { this != rhs }
    }

    impl TotalCmp { }

    @(intrinsic(binary_op))
    pub fn +(this, rhs: T): T { this + rhs }

    @(intrinsic(binary_op))
    pub fn -(this, rhs: T): T { this - rhs }

    @(intrinsic(binary_op))
    pub fn *(this, rhs: T): T { this * rhs }

    @(intrinsic(binary_op))
    pub fn /(this, rhs: T): T { this / rhs }

    @(intrinsic(binary_op))
    pub fn %(this, rhs: T): T { this % rhs }

    /// C-style cast from `this` to type U with overflow/truncation.
    @(intrinsic(numeric_cast))
    pub fn cast<U: Numeric>(my this): U { this.cast() }
}

mod gcc_intrin {
    use super::Integral;
    use super::Unsigned;

    // TODO: compiler independent fallback for these functions
    @(c_opaque)
    pub extern fn __builtin_add_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;

    @(c_opaque)
    pub extern fn __builtin_sub_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;

    @(c_opaque)
    pub extern fn __builtin_mul_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;

    @(c_opaque)
    pub extern fn __builtin_popcountg<T: Unsigned>(u: T): c_int;

    @(c_opaque)
    pub extern fn __builtin_clzg<T: Unsigned>(u: T): c_int;

    @(c_opaque)
    pub extern fn __builtin_ctzg<T: Unsigned>(u: T): c_int;
}

pub extension IntegralImpl<T: Integral> for T {
    @(intrinsic(binary_op))
    pub fn &(this, rhs: T): T { this & rhs }

    @(intrinsic(binary_op))
    pub fn |(this, rhs: T): T { this | rhs }

    @(intrinsic(binary_op))
    pub fn ^(this, rhs: T): T { this ^ rhs }

    @(intrinsic(binary_op))
    pub fn <<(this, rhs: u32): T { this << rhs }

    @(intrinsic(binary_op))
    pub fn >>(this, rhs: u32): T { this >> rhs }

    @(intrinsic(unary_op))
    pub fn !(this): T { !this }

    @(intrinsic(unary_op))
    pub fn ++(mut this) { (*this)++; }

    @(intrinsic(unary_op))
    pub fn --(mut this) { (*this)--; }

    @(inline)
    pub fn wrapping_add(this, rhs: T): T { this + rhs }

    @(inline)
    pub fn wrapping_sub(this, rhs: T): T { this - rhs }

    @(inline)
    pub fn wrapping_mul(this, rhs: T): T { this * rhs }

    @(inline)
    pub fn wrapping_div(this, rhs: T): T { this / rhs }

    @(inline)
    pub fn overflowing_add(this, rhs: T): (T, bool) {
        mut out: T;
        let res = unsafe gcc_intrin::__builtin_add_overflow(*this, rhs, &raw mut out);
        (out, res)
    }

    @(inline)
    pub fn overflowing_sub(this, rhs: T): (T, bool) {
        mut out: T;
        let res = unsafe gcc_intrin::__builtin_sub_overflow(*this, rhs, &raw mut out);
        (out, res)
    }

    @(inline)
    pub fn overflowing_mul(this, rhs: T): (T, bool) {
        mut out: T;
        let res = unsafe gcc_intrin::__builtin_mul_overflow(*this, rhs, &raw mut out);
        (out, res)
    }

    @(inline)
    pub fn checked_add(this, rhs: T): ?T => this.overflowing_add(rhs) is (out, false) then out;

    @(inline)
    pub fn checked_sub(this, rhs: T): ?T => this.overflowing_sub(rhs) is (out, false) then out;

    @(inline)
    pub fn checked_mul(this, rhs: T): ?T => this.overflowing_mul(rhs) is (out, false) then out;

    @(inline)
    pub fn saturating_add(this, rhs: T): T {
        if this.checked_add(rhs) is ?out {
            out
        } else if rhs < 0.cast() {
            T::min_value()
        } else {
            T::max_value()
        }
    }

    @(intrinsic)
    pub fn max_value(): T => T::max_value();

    @(intrinsic)
    pub fn min_value(): T => T::min_value();

    /// Cast `this` to type `U` if the value of this is exactly representable in U
    @(inline)
    pub fn try_cast<U: Numeric>(my this): ?U {
        let rhs: U = this.cast();
        this == rhs.cast() then rhs
    }

    @(inline)
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

    @(feature(alloc))
    pub fn to_str_radix(this, radix: u32): str {
        guard radix is 2..=36 else {
            panic("to_str_radix(): invalid radix");
        }

        let neg = this < 0u.cast();
        mut buf = @[b'0'; std::mem::size_of::<T>() * 8 + 2 + neg as uint][..];
        unsafe {
            mut pos = this.write_digits(buf, radix, upper: false);
            if neg {
                *buf.get_mut_unchecked(--pos) = b'-';
            }

            str::from_utf8_unchecked(buf[pos..])
        }
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
    pub unsafe fn write_digits(my mut this, buf: [mut u8..], radix: u32, upper: bool): uint {
        fn casting_divmod<T: Numeric, U: Numeric>(dividend: T, divisor: U): (T, T) {
            // TODO: do this at CTL compile time
            if std::mem::size_of::<T>() >= std::mem::size_of::<U>() {
                let divisor: T = divisor.cast();
                (dividend / divisor, dividend % divisor)
            } else {
                let dividend: U = dividend.cast();
                ((dividend / divisor).cast(), (dividend % divisor).cast())
            }
        }

        static UPPER_DIGITS: [u8; 36] = *b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        static LOWER_DIGITS: [u8; 36] = *b"0123456789abcdefghijklmnopqrstuvwxyz";

        let digits = upper then &UPPER_DIGITS else &LOWER_DIGITS;
        mut pos = buf.len();
        loop {
            let (v, digit) = casting_divmod(this, radix as! i32);
            // Only relevant for signed but should get optimized away for unsigned
            let digit = this < 0.cast() then -digit.cast::<int>() else digit.cast();
            unsafe *buf.as_raw_mut().uoffset(--pos) = digits[digit];
            this = v;
        } while this != 0.cast();

        pos
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => this.format_into(f, 10);
    }

    impl Format {
        fn fmt(this, f: *mut Formatter) => this.format_into(f, 10);
        fn bin(this, f: *mut Formatter) => this.format_into(f, 2);
        fn hex(this, f: *mut Formatter) => this.format_into(f, 16);
        fn oct(this, f: *mut Formatter) => this.format_into(f, 8);
    }

    fn format_into(my mut this, f: *mut Formatter, radix: u32) {
        if std::mem::size_of::<T>() <= std::mem::size_of::<u128>() {
            mut buf: [u8; std::mem::size_of::<u128>() * 8];
            let pos = unsafe this.write_digits(buf[..], radix, f.options().upper);
            let buf = unsafe str::from_utf8_unchecked(buf[pos..]);
            f.pad_integral(negative: this < 0.cast(), value: buf, prefix: match radix {
                 2 => ?"0b",
                 8 => ?"0o",
                16 => ?"0x",
                 _ => null,
            });
        } else {
            "<TODO: format int >128 bits>".fmt(f);
        }
    }
}

pub extension SignedImpl<T: Signed> for T {
    pub fn abs(this): T => std::intrin::numeric_abs(*this);

    @(intrinsic(unary_op))
    pub fn -(this): T => -this;

    @(inline)
    pub fn overflowing_div(this, rhs: T): (T, bool) {
        if this == T::min_value() and rhs == (-1).cast() {
            (*this, true)
        } else {
            (this / rhs, false)
        }
    }

    @(inline)
    pub fn checked_div(this, rhs: T): ?T => this.overflowing_div(rhs) is (out, false) then out;

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
    /// This exists for parity with SignedImpl::overflowing_div. Unsigned division cannot overflow,
    /// and as such this function always returns false.
    @(inline)
    pub fn overflowing_div(this, rhs: T): (T, bool) => (this / rhs, false);

    @(inline)
    pub fn checked_div(this, rhs: T): ?T => this / rhs;

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

    pub fn count_ones(this): u32 => unsafe gcc_intrin::__builtin_popcountg(*this) as! u32;
    pub fn count_zeros(this): u32 => unsafe gcc_intrin::__builtin_popcountg(!*this) as! u32;
    pub fn leading_zeros(this): u32 => unsafe gcc_intrin::__builtin_clzg(*this) as! u32;
    pub fn trailing_zeros(this): u32 => unsafe gcc_intrin::__builtin_ctzg(*this) as! u32;
}
