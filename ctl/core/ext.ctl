use core::hash::*;
use core::ops::*;
use core::span::Span;

extension<T> _ for T {
    pub fn hash_bytes<H: Hasher>(this, h: *mut H) {
        h.hash(unsafe Span::new(this as *raw u8, core::mem::size_of::<T>()));
    }
}

pub extension<T: core::reflect::Numeric> NumberExt for T {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            this.hash_bytes(h)
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
            this.hash_bytes(h)
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
}

pub extension BoolExt for bool {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            this.hash_bytes(h)
        }
    }

    impl Eq<This> {
        #(binary_op(eq))
        fn eq(this, rhs: *This): bool { this == rhs }

        #(binary_op(ne))
        fn ne(this, rhs: *This): bool { this != rhs }
    }
}
