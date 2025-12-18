use std::hash::*;
use std::ops::*;
use std::fmt::*;
use super::ByteSpanExt;

pub extension BoolImpl for bool {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) => h.hash(this.as_byte_span());
    }

    impl Eq<This> {
        @(intrinsic(binary_op))
        fn eq(this, rhs: *This): bool => this == rhs;

        @(intrinsic(binary_op))
        fn ne(this, rhs: *This): bool => this != rhs;
    }

    @(intrinsic(unary_op))
    pub fn !(this): This => !*this;

    @(intrinsic(binary_op))
    pub fn &(this, rhs: This): This => this & rhs;

    @(intrinsic(binary_op))
    pub fn |(this, rhs: This): This => this | rhs;

    @(intrinsic(binary_op))
    pub fn ^(this, rhs: This): This => this ^ rhs;

    @(intrinsic(binary_op))
    pub fn &=(mut this, rhs: This) => *this &= rhs;

    @(intrinsic(binary_op))
    pub fn |=(mut this, rhs: This) => *this |= rhs;

    @(intrinsic(binary_op))
    pub fn ^=(mut this, rhs: This) => *this ^= rhs;

    impl Debug {
        fn dbg(this, f: *mut Formatter) { f.write_str(*this then "true" else "false"); }
    }

    impl Format {
        fn fmt(this, f: *mut Formatter) => f.pad(*this then "true" else "false");
    }

    pub fn then_some<T>(my this, t: T): ?T => this then t;
}
