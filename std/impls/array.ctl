use std::reflect::Integral;
use std::reflect::Array;
use std::range::RangeBounds;

pub extension ArrayImpl<A: Array<T>, T> for A {
    @(inline(always))
    pub fn len(this): uint => std::mem::size_of::<A>() / std::mem::size_of::<T>();

    @(inline(always))
    pub fn as_span(this): [T..] => unsafe Span::new(this.as_raw(), this.len());

    @(inline(always))
    pub fn as_span_mut(mut this): [mut T..] => unsafe SpanMut::new(this.as_raw_mut(), this.len());

    @(inline(always))
    pub fn iter(this): std::span::Iter<T> => this.as_span().iter();

    @(inline(always))
    pub fn iter_mut(mut this): std::span::IterMut<T> => this.as_span_mut().iter_mut();

    @(inline(always))
    pub fn get(this, idx: uint): ?*T => this.as_span().get(idx);

    @(inline(always))
    pub fn get_mut(mut this, idx: uint): ?*mut T => this.as_span_mut().get_mut(idx);

    @(inline(always))
    pub unsafe fn get_unchecked(this, idx: uint): *T => unsafe this.as_span().get_unchecked(idx);

    @(inline(always))
    pub unsafe fn get_mut_unchecked(mut this, idx: uint): *mut T {
        unsafe this.as_span_mut().get_mut_unchecked(idx)
    }

    @(inline(always))
    pub fn as_raw(this): ^T => this as ^T;

    @(inline(always))
    pub fn as_raw_mut(this): ^mut T => this as ^mut T;

    @(inline(always))
    pub fn []<I: Integral>(this, idx: I): *T => &this.as_span()[idx];

    @(inline(always))
    pub fn []<I: Integral>(mut this, idx: I): *mut T => &mut this.as_span_mut()[idx];

    @(inline(always))
    pub fn []=<I: Integral>(mut this, idx: I, val: T) => this.as_span_mut()[idx] = val;

    @(inline(always))
    pub fn []<R: RangeBounds<uint>>(this, r: R): [T..] => this.as_span()[r];

    @(inline(always))
    pub fn []<R: RangeBounds<uint>>(mut this, r: R): [mut T..] => this.as_span_mut()[r];

    @(inline(always))
    pub fn []=<R: RangeBounds<uint>>(mut this, r: R, rhs: [T..]) => this.as_span_mut()[r] = rhs;
}

pub extension ArrayDebug<A: Array<T>, T: std::fmt::Debug> for A {
    impl std::fmt::Debug {
        @(inline(always))
        fn dbg(this, f: *mut std::fmt::Formatter) => this[..].dbg(f);
    }
}

pub extension ArrayHash<A: Array<T>, T: std::hash::Hash> for A {
    impl std::hash::Hash {
        @(inline(always))
        fn hash<H: std::hash::Hasher>(this, h: *mut H) => this[..].hash(h);
    }
}

pub extension ArrayEq<A: Array<T>, T: std::ops::Eq<T>> for A {
    @(inline(always))
    pub fn ==(this, rhs: *A): bool => this[..] == rhs[..];
}
