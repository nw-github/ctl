use std::hash::*;
use std::ops::*;
use std::fmt::*;

extension void {
    impl Hash {
        fn hash<H: Hasher>(this, _: *mut H) {}
    }

    impl Eq<This> {
        fn eq(this, _: *This): bool => true;
        fn ne(this, _: *This): bool => false;
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => write(f, "void");
    }
}

extension<T> T {
    fn as_byte_span(this): [u8..] {
        unsafe Span::new((&raw *this).cast(), std::mem::size_of::<T>())
    }

    fn as_byte_span_mut(mut this): [mut u8..] {
        unsafe SpanMut::new((&raw mut *this).cast(), std::mem::size_of::<T>())
    }
}
