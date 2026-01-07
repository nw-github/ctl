use std::reflect::Numeric;
use std::hash::*;
use std::ops::*;
use super::ByteSpanExt;

pub extension NumericImpl<T: Numeric> for T {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) => h.hash(this.as_byte_span());
    }

    impl Cmp<T> {
        $[intrinsic(binary_op)]
        fn cmp(this, rhs: *T): Ordering => this <=> rhs;

        $[intrinsic(binary_op)]
        fn ge(this, rhs: *T): bool => this >= rhs;

        $[intrinsic(binary_op)]
        fn gt(this, rhs: *T): bool => this > rhs;

        $[intrinsic(binary_op)]
        fn le(this, rhs: *T): bool => this <= rhs;

        $[intrinsic(binary_op)]
        fn lt(this, rhs: *T): bool => this < rhs;
    }

    impl Eq<T> {
        $[intrinsic(binary_op)]
        fn eq(this, rhs: *T): bool => this == rhs;

        $[intrinsic(binary_op)]
        fn ne(this, rhs: *T): bool => this != rhs;
    }

    $[intrinsic(binary_op)]
    pub fn +(this, rhs: T): T => this + rhs;

    $[intrinsic(binary_op)]
    pub fn -(this, rhs: T): T => this - rhs;

    $[intrinsic(binary_op)]
    pub fn *(this, rhs: T): T => this * rhs;

    $[intrinsic(binary_op)]
    pub fn /(this, rhs: T): T => this / rhs;

    $[intrinsic(binary_op)]
    pub fn %(this, rhs: T): T => this % rhs;

    $[intrinsic(binary_op)]
    pub fn +=(mut this, rhs: T) => *this += rhs;

    $[intrinsic(binary_op)]
    pub fn -=(mut this, rhs: T) => *this -= rhs;

    $[intrinsic(binary_op)]
    pub fn *=(mut this, rhs: T) => *this *= rhs;

    $[intrinsic(binary_op)]
    pub fn /=(mut this, rhs: T) => *this /= rhs;

    $[intrinsic(binary_op)]
    pub fn %=(mut this, rhs: T) => *this %= rhs;
}
