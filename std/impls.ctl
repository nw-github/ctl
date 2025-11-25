use std::hash::*;
use std::ops::*;
use std::fmt::*;
use std::span::*;
use std::reflect::*;

static DIGITS: [u8; 36] = *b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

extension _<T> for T {
    pub fn as_byte_span(this): [u8..] {
        unsafe Span::new((&raw *this).cast(), std::mem::size_of::<T>())
    }

    pub fn as_byte_span_mut(mut this): [mut u8..] {
        unsafe SpanMut::new((&raw mut *this).cast(), std::mem::size_of::<T>())
    }
}

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

pub extension CharImpl for char {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.as_byte_span());
        }
    }

    impl Eq<This> {
        @(intrinsic(binary_op))
        fn eq(this, rhs: *This): bool { this == rhs }

        @(intrinsic(binary_op))
        fn ne(this, rhs: *This): bool { this != rhs }
    }

    impl Cmp<This> {
        @(intrinsic(binary_op))
        fn cmp(this, rhs: *This): Ordering { this <=> rhs }

        @(intrinsic(binary_op))
        fn ge(this, rhs: *This): bool { this >= rhs }

        @(intrinsic(binary_op))
        fn gt(this, rhs: *This): bool { this > rhs }

        @(intrinsic(binary_op))
        fn le(this, rhs: *This): bool { this <= rhs }

        @(intrinsic(binary_op))
        fn lt(this, rhs: *This): bool { this < rhs }
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            unsafe this.encode_utf8_unchecked(this.len_utf8(), [0u8; 4][..]).fmt(f);
        }
    }

    pub unsafe fn from_u32_unchecked(v: u32): char {
        unsafe std::mem::transmute(v)
    }

    pub fn len_utf8(my this): uint {
        let cp = this as u32;
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

    pub fn encode_utf8(my this, buf: [mut u8..]): str {
        let len = this.len_utf8();
        if buf.len() < len {
            panic("char::encode_utf8(): buffer size is insufficient");
        }

        unsafe this.encode_utf8_unchecked(len, buf)
    }

    pub fn is_ascii(my this): bool {
        (this as u32) < 0b1000_0000
    }

    pub fn is_ascii_upper(my this): bool {
        this is 'A'..='Z'
    }

    pub fn is_ascii_lower(my this): bool {
        this is 'a'..='z'
    }

    pub fn is_ascii_digit(my this): bool {
        this is '0'..='9'
    }

    pub fn is_ascii_hexdigit(my this): bool {
        this is '0'..='9' | 'a'..='f' | 'A'..='F'
    }

    pub fn make_ascii_upper(mut this) {
        *this = this.to_ascii_upper();
    }

    pub fn make_ascii_lower(mut this) {
        *this = this.to_ascii_lower();
    }

    pub fn to_ascii_upper(my this): char {
        unsafe char::from_u32_unchecked(this as u32 ^ (0b10_0000 * this.is_ascii_upper() as u32))
    }

    pub fn to_ascii_lower(my this): char {
        unsafe char::from_u32_unchecked(this as u32 ^ (0b10_0000 * this.is_ascii_lower() as u32))
    }

    pub fn to_digit(my this, radix: u32): ?u32 {
        // don't mind if I do
        // https://github.com/rust-lang/rust/blob/9afe7136958edaa403f0b0eb00f0353c125b7352/library/core/src/char/methods.rs#L378

        // If not a digit, a number greater than radix will be created.
        mut digit = (this as u32).wrapping_sub('0' as u32);
        if radix > 10 {
            guard radix <= 36 else {
                panic("to_digit: radix is too high (maximum 36)");
            }

            if digit < 10 {
                return digit;
            }
            // Force the 6th bit to be set to ensure ascii is lower case.
            digit = (this as u32 | 0b10_0000).wrapping_sub('a' as u32).saturating_add(10);
        }
        (digit < radix).then_some(digit)
    }

    pub fn min_value(): char {
        '\0'
    }

    pub fn max_value(): char {
        '\u{10ffff}'
    }

    pub fn is_ascii_whitespace(my this): bool {
        this is '\t' | '\n' | '\x0C' | '\r' | ' '
    }

    unsafe fn encode_utf8_unchecked(my this, len_utf8: uint, buf: [mut u8..]): str {
        unsafe {
            let cp = this as u32;
            mut ptr = buf.as_raw_mut();
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
                _ => {
                    *ptr++ = ((cp >> 18) | 0xf0) as! u8;
                    *ptr++ = (((cp >> 12) & 0x3f) | 0x80) as! u8;
                    *ptr++ = (((cp >> 6) & 0x3f) | 0x80) as! u8;
                    *ptr   = ((cp & 0x3f) | 0x80) as! u8;
                }
            }
            str::from_utf8_unchecked(buf[..len_utf8])
        }
    }
}

pub extension BoolImpl for bool {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.as_byte_span());
        }
    }

    impl Eq<This> {
        @(intrinsic(binary_op))
        fn eq(this, rhs: *This): bool { this == rhs }

        @(intrinsic(binary_op))
        fn ne(this, rhs: *This): bool { this != rhs }
    }

    @(intrinsic(unary_op))
    pub fn !(this): This { !*this }

    @(intrinsic(binary_op))
    pub fn &(this, rhs: This): This { this & rhs }

    @(intrinsic(binary_op))
    pub fn |(this, rhs: This): This { this | rhs }

    @(intrinsic(binary_op))
    pub fn ^(this, rhs: This): This { this ^ rhs }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            *this then "true".fmt(f) else "false".fmt(f)
        }
    }

    pub fn then_some<T>(my this, t: T): ?T {
        this then t
    }
}

pub extension VoidImpl for void {
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
        fn fmt<F: Formatter>(this, f: *mut F) {
            "void".fmt(f);
        }
    }
}

pub extension RawImpl<T> for ^T {
    pub fn cast<U>(my this): ^U {
        this as ^U
    }

    pub fn addr(my this): uint {
        this as uint
    }

    pub fn offset(my this, offs: int): ^T {
        this + offs
    }

    pub fn uoffset(my this, offs: uint): ^T {
        this + offs
    }

    pub unsafe fn read(my this): T {
        unsafe *this
    }

    pub unsafe fn read_volatile(my this): T {
        unsafe std::ptr::read_volatile(this)
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            // TODO: just format (this as uint) when format specifiers are added
            mut buffer: [u8; std::mem::size_of::<uint>() * 2 + 2];
            unsafe {
                let res = (*this as uint).to_str_radix_unchecked(16, buffer[2u..]);
                // kinda gross, but avoids multiple calls to fmt()
                mut ptr = res.as_raw() as ^mut u8;
                *--ptr = b'x';
                *--ptr = b'0';
                str::from_utf8_unchecked(Span::new(ptr, res.len() + 2)).fmt(f);
            }
        }
    }
}


pub extension RawMutImpl<T> for ^mut T {
    pub fn cast<U>(my this): ^mut U {
        this as ^mut U
    }

    pub fn addr(my this): uint {
        this as uint
    }

    pub fn offset(my this, offs: int): ^mut T {
        this + offs
    }

    pub fn uoffset(my this, offs: uint): ^mut T {
        this + offs
    }

    pub unsafe fn read(my this): T {
        unsafe *this
    }

    pub unsafe fn write(my this, val: T) {
        unsafe *this = val;
    }

    pub unsafe fn read_volatile(my this): T {
        unsafe std::ptr::read_volatile(this)
    }

    pub unsafe fn write_volatile(my this, val: T) {
        unsafe std::ptr::write_volatile(this, val)
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            (this as ^T).fmt(f);
        }
    }
}

use super::ryu::Float32Ext;
use super::ryu::Float64Ext;

mod libm {
    pub extern fn sqrt(n: f64): f64;
    pub extern fn sin(n: f64): f64;
    pub extern fn cos(n: f64): f64;
    pub extern fn tan(n: f64): f64;
    pub extern fn floor(n: f64): f64;
    pub extern fn ceil(n: f64): f64;

    pub extern fn sqrtf(n: f32): f32;
    pub extern fn sinf(n: f32): f32;
    pub extern fn cosf(n: f32): f32;
    pub extern fn tanf(n: f32): f32;
    pub extern fn floorf(n: f32): f32;
    pub extern fn ceilf(n: f32): f32;
}

pub extension F32Impl for f32 {
    pub fn to_bits(my this): u32 {
        unsafe std::mem::transmute(this)
    }

    pub fn from_bits(v: u32): f32 {
        unsafe std::mem::transmute(v)
    }

    pub fn sqrt(my this): f32 {
        unsafe libm::sqrtf(this)
    }

    pub fn sin(my this): f32 {
        unsafe libm::sinf(this)
    }

    pub fn cos(my this): f32 {
        unsafe libm::cosf(this)
    }

    pub fn tan(my this): f32 {
        unsafe libm::tanf(this)
    }

    pub fn floor(my this): f32 {
        unsafe libm::floorf(this)
    }

    pub fn ceil(my this): f32 {
        unsafe libm::ceilf(this)
    }

    pub fn pi(): f32 {
        3.14159265358979323846
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            super::ryu::Buffer::new().format(*this).fmt(f);
        }
    }
}

pub extension F64Impl for f64 {
    pub fn to_bits(my this): u64 {
        unsafe std::mem::transmute(this)
    }

    pub fn from_bits(v: u64): f64 {
        unsafe std::mem::transmute(v)
    }

    pub fn sqrt(my this): f64 {
        unsafe libm::sqrt(this)
    }

    pub fn sin(my this): f64 {
        unsafe libm::sin(this)
    }

    pub fn cos(my this): f64 {
        unsafe libm::cos(this)
    }

    pub fn tan(my this): f64 {
        unsafe libm::tan(this)
    }

    pub fn floor(my this): f64 {
        unsafe libm::floor(this)
    }

    pub fn ceil(my this): f64 {
        unsafe libm::ceil(this)
    }

    pub fn pi(): f64 {
        3.14159265358979323846
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            super::ryu::Buffer::new().format(*this).fmt(f);
        }
    }
}

pub extension DynAnyImpl for *dyn std::any::Any {
    pub fn downcast<T>(my this): ?*T {
        if this.type_id() == TypeId::get::<T>() {
            unsafe this.downcast_unchecked()
        }
    }

    pub unsafe fn downcast_unchecked<T>(my this): *T {
        // TODO: maybe there should be a safer way to do this
        unsafe *(&this as **T)
    }
}

pub extension DynMutAnyImpl for *dyn mut std::any::Any {
    pub fn downcast<T>(my this): ?*T {
        if this.type_id() == TypeId::get::<T>() {
            unsafe this.downcast_unchecked()
        }
    }

    pub fn downcast_mut<T>(my mut this): ?*mut T {
        if this.type_id() == TypeId::get::<T>() {
            unsafe this.downcast_unchecked_mut()
        }
    }

    pub unsafe fn downcast_unchecked<T>(my this): *T {
        unsafe *(&this as **T)
    }

    pub unsafe fn downcast_unchecked_mut<T>(my this): *mut T {
        unsafe *(&this as **mut T)
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
