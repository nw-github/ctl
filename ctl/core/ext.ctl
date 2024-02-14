use core::hash::*;
use core::span::Span;
use core::ops::Eq;

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

    impl Eq<T> {
        fn eq(this, rhs: *T): bool {
            unsafe core::mem::compare(this as *raw T, rhs as *raw T, 1)
        }
    }
}

pub extension CharExt for char {
    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            this.hash_bytes(h)
        }
    }

    impl Eq<char> {
        fn eq(this, rhs: *char): bool {
            *this == *rhs
        }
    }
}
