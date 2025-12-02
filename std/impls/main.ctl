use std::hash::*;
use std::ops::*;
use std::fmt::*;
use std::span::*;

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

extension ByteSpanExt<T> for T {
    pub fn as_byte_span(this): [u8..] {
        unsafe Span::new((&raw *this).cast(), std::mem::size_of::<T>())
    }

    pub fn as_byte_span_mut(mut this): [mut u8..] {
        unsafe SpanMut::new((&raw mut *this).cast(), std::mem::size_of::<T>())
    }
}

pub use boolean::*;
pub use character::*;
pub use dynany::*;
pub use float::*;
pub use numeric::*;
pub use rawptr::*;
