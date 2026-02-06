use std::reflect::Integral;
use std::reflect::Array;
use std::range::RangeBounds;

extension<A: Array<T>, T> A {
    impl std::span::AsSpan<T> {
        $[inline(always)]
        fn as_span(this): [T..] => unsafe Span::new(this.as_raw(), this.len());

        $[inline(always)]
        fn len(this): uint => std::mem::size_of::<A>() / std::mem::size_of::<T>();

        $[inline(always)]
        fn as_raw(this): ^T => this as ^A as ^T;
    }

    impl std::span::AsSpanMut<T> {
        $[inline(always)]
        fn as_span_mut(mut this): [mut T..] => unsafe SpanMut::new(this.as_raw_mut(), this.len());

        $[inline(always)]
        fn as_raw_mut(mut this): ^mut T => this as ^mut A as ^mut T;
    }

    $[inline(always)]
    pub fn []<I: Integral>(this, idx: I): *T => &this.as_span()[idx];

    $[inline(always)]
    pub fn []<I: Integral>(mut this, idx: I): *mut T => &mut this.as_span_mut()[idx];

    $[inline(always)]
    pub fn []=<I: Integral>(mut this, idx: I, val: T) => this.as_span_mut()[idx] = val;

    $[inline(always)]
    pub fn []<R: RangeBounds<uint>>(this, r: R): [T..] => this.as_span()[r];

    $[inline(always)]
    pub fn []<R: RangeBounds<uint>>(mut this, r: R): [mut T..] => this.as_span_mut()[r];

    $[inline(always)]
    pub fn []=<R: RangeBounds<uint>>(mut this, r: R, rhs: [T..]) => this.as_span_mut()[r] = rhs;

    impl std::fmt::Debug {
        $[inline(always)]
        fn dbg(this, f: *mut std::fmt::Formatter) => this[..].dbg(f);
    }
}

extension<A: Array<T>, T: std::hash::Hash> A {
    impl std::hash::Hash {
        $[inline(always)]
        fn hash<H: std::hash::Hasher>(this, h: *mut H) => this[..].hash(h);
    }
}

extension<A: Array<T>, T: std::ops::Eq<T>> A {
    $[inline(always)]
    pub fn ==(this, rhs: *A): bool => this[..] == rhs[..];
}

extension<A: Array<T>, T: std::ops::Cmp<T>> A {
    $[inline(always)]
    pub fn <=>(this, rhs: *A): std::ops::Ordering => this[..] <=> rhs[..];
}
