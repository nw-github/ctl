use std::hash::*;
use std::ops::*;
use std::fmt::*;
use std::reflect::{Tuple, SafeFnPtr};

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

extension<Args: Tuple, R, F: SafeFnPtr<Args, R>> F {
    impl Fn<Args, R> {
        fn invoke(this, args: Args): R => std::intrin::invoke_with_tuple(this, args);
    }
}

extension<Args: Tuple, R> *dyn Fn<Args, R> {
    impl Fn<Args, R> {
        // TODO: This call is not recursive because the . operator will check dynamic calls
        // before checking extensions, but this syntax seems ambiguous
        fn invoke(this, args: Args): R => (*this).invoke(args);
    }
}

extension<Args: Tuple, R> *dyn mut Fn<Args, R> {
    impl Fn<Args, R> {
        fn invoke(this, args: Args): R => (*this).invoke(args);
    }
}
