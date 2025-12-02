use std::hash::*;
use std::ops::*;
use std::fmt::*;
use std::reflect::*;
use super::ByteSpanExt;

static DIGITS: [u8; 36] = *b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

pub extension U8Impl for u8 {
    pub fn is_ascii(my this): bool {
        this < 0b1000_0000
    }

    pub fn is_ascii_whitespace(my this): bool {
        this is b'\t' | b'\n' | b'\x0C' | b'\r' | b' '
    }

    pub fn is_ascii_upper(my this): bool {
        this is b'A'..=b'Z'
    }

    pub fn is_ascii_lower(my this): bool {
        this is b'a'..=b'z'
    }

    pub fn is_ascii_digit(my this): bool {
        this is b'0'..=b'9'
    }

    pub fn is_ascii_hexdigit(my this): bool {
        this is b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F'
    }

    pub fn make_ascii_upper(mut this) {
        *this = this.to_ascii_upper();
    }

    pub fn make_ascii_lower(mut this) {
        *this = this.to_ascii_upper();
    }

    pub fn to_ascii_upper(my this): u8 {
        this ^ (0b100000 * this.is_ascii_upper() as u8)
    }

    pub fn to_ascii_lower(my this): u8 {
        this ^ (0b100000 * this.is_ascii_lower() as u8)
    }
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

    // TODO: compiler independent fallback for these functions
    @(c_opaque)
    pub extern fn __builtin_add_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;

    @(c_opaque)
    pub extern fn __builtin_sub_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;

    @(c_opaque)
    pub extern fn __builtin_mul_overflow<T: Integral>(x: T, y: T, res: ^mut T): bool;
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
    pub fn checked_add(this, rhs: T): ?T {
        this.overflowing_add(rhs) is (out, false) then out
    }

    @(inline)
    pub fn checked_sub(this, rhs: T): ?T {
        this.overflowing_sub(rhs) is (out, false) then out
    }

    @(inline)
    pub fn checked_mul(this, rhs: T): ?T {
        this.overflowing_mul(rhs) is (out, false) then out
    }

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
    pub fn max_value(): T { T::max_value() }

    @(intrinsic)
    pub fn min_value(): T { T::min_value() }

    /// Cast `this` to type `U` if the value of this is exactly representable in U
    @(inline)
    pub fn try_cast<U: Numeric>(my this): ?U {
        let rhs: U = this.cast();
        this == rhs.cast() then rhs
    }

    @(inline)
    pub fn bswap(my mut this): T {
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
}

pub extension SignedImpl<T: Signed> for T {
    pub fn abs(this): T {
        std::intrin::numeric_abs(*this)
    }

    pub unsafe fn to_str_radix_unchecked(my this, radix: u32, buf: [mut u8..]): str {
        mut pos = buf.len();
        mut val = this < 0u.cast() then this else -this;
        loop {
            let (v, digit) = casting_divmod(val, radix as! i32);
            unsafe *buf.get_mut_unchecked(--pos) = DIGITS[-digit.cast::<int>()];
            val = v;
        } while val != 0u.cast();

        if this < 0u.cast() {
            unsafe *buf.get_mut_unchecked(--pos) = b'-';
        }

        unsafe str::from_utf8_unchecked(buf[pos..])
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            // FIXME: fix this when there is a safer way to deal with uninitialized memory
            //        size_of should be size_of<T>
            mut buffer: [u8; std::mem::size_of::<u128>() * 8 + 1];
            unsafe this.to_str_radix_unchecked(10, buffer[..]).fmt(f);
        }
    }

    @(intrinsic(unary_op))
    pub fn -(this): T { -this }

    @(inline)
    pub fn overflowing_div(this, rhs: T): (T, bool) {
        if this == T::min_value() and rhs == (-1).cast() {
            (*this, true)
        } else {
            (this / rhs, false)
        }
    }

    @(inline)
    pub fn checked_div(this, rhs: T): ?T {
        this.overflowing_div(rhs) is (out, false) then out
    }

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

    @(feature(alloc))
    pub fn to_str_radix(this, radix: u32): str {
        guard radix is 2..=36 else {
            panic("to_str_radix(): invalid radix");
        }

        mut buffer = @[b'0'; std::mem::size_of::<T>() * 8 + 1];
        unsafe {
            this.to_str_radix_unchecked(radix, buffer[..])
        }
    }
}

pub extension UnsignedImpl<T: Unsigned> for T {
    pub unsafe fn to_str_radix_unchecked(my mut this, radix: u32, buf: [mut u8..]): str {
        mut pos = buf.len();
        loop {
            let (v, digit) = casting_divmod(this, radix);
            unsafe *buf.get_mut_unchecked(--pos) = DIGITS[digit.cast()];
            this = v;
        } while this != 0u.cast();

        unsafe str::from_utf8_unchecked(buf[pos..])
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            mut buffer: [u8; std::mem::size_of::<u128>() * 8];
            unsafe this.to_str_radix_unchecked(10, buffer[..]).fmt(f);
        }
    }

    /// This exists for parity with SignedExt::overflowing_div. Unsigned division cannot overflow,
    /// and as such this function always returns false.
    @(inline)
    pub fn overflowing_div(this, rhs: T): (T, bool) {
        (this / rhs, false)
    }

    @(inline)
    pub fn checked_div(this, rhs: T): ?T {
        this / rhs
    }

    pub fn from_str_radix(s: str, radix: u32): ?T {
        mut chars = s.chars();
        if !(chars.next()? is '+') {
            chars = s.chars();
        }

        T::from_str_radix_common(chars, radix)
    }

    @(feature(alloc))
    pub fn to_str_radix(this, radix: u32): str {
        guard radix is 2..=36 else {
            panic("to_str_radix(): invalid radix");
        }

        mut buffer = @[b'0'; std::mem::size_of::<T>() * 8];
        unsafe {
            this.to_str_radix_unchecked(radix, buffer[..])
        }
    }
}

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
