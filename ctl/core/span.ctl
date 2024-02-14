use core::ptr::Raw;
use core::ptr::RawMut;
use core::range::RangeBounds;
use core::range::Bound;
use core::iter::Iterator;
use core::panic;

#(lang(span))
pub struct Span<T> {
    ptr: *T,
    len: uint,

    pub unsafe fn new(ptr: *T, len: uint): [T..] {
        Span(ptr:, len:)
    }

    pub fn len(this): uint {
        this.len
    }

    pub fn is_empty(this): bool {
        this.len == 0
    }

    pub fn get(this, idx: uint): ?*T {
        if idx < this.len {
            unsafe this.get_unchecked(idx)
        }
    }

    pub unsafe fn get_unchecked(this, idx: uint): *T {
        unsafe core::ptr::offset(this.ptr, idx)
    }

    pub fn as_raw(this): Raw<T> {
        Raw::from_ptr(this.ptr)
    }

    pub fn iter(this): Iter<T> {
        Iter(
            ptr: this.ptr,
            end: unsafe core::ptr::offset(this.ptr, this.len),
        )
    }

    pub fn subspan<R: RangeBounds<uint>>(this, range: R): [T..] {
        let start = match range.begin() {
            Bound::Inclusive(start) => start,
            Bound::Exclusive(start) => start + 1,
            Bound::Unbounded => 0,
        };

        let end = match range.end() {
            Bound::Inclusive(end) => end + 1,
            Bound::Exclusive(end) => end,
            Bound::Unbounded => this.len(),
        };

        if end < start || start > this.len || end > this.len {
            panic("Span::subspan(): invalid range!");
        }

        Span(
            ptr: unsafe core::ptr::offset(this.ptr, start),
            len: end - start,
        )
    }
}

#(lang(span_mut))
pub struct SpanMut<T> {
    ptr: *mut T,
    len: uint,

    pub unsafe fn new(ptr: *mut T, len: uint): [mut T..] {
        SpanMut(ptr:, len:)
    }

    pub fn len(this): uint {
        this.len
    }

    pub fn is_empty(this): bool {
        this.len == 0
    }

    pub fn get(this, idx: uint): ?*T {
        if idx < this.len {
            unsafe this.get_unchecked(idx)
        }
    }

    pub fn get_mut(this, idx: uint): ?*mut T {
        if idx < this.len {
            unsafe this.get_mut_unchecked(idx)
        }
    }

    pub unsafe fn get_unchecked(this, idx: uint): *T {
        unsafe core::ptr::offset(this.ptr, idx)
    }

    pub unsafe fn get_mut_unchecked(this, idx: uint): *mut T {
        unsafe core::ptr::offset_mut(this.ptr, idx)
    }

    pub fn as_raw(this): Raw<T> {
        Raw::from_ptr(this.ptr)
    }

    pub fn as_raw_mut(this): RawMut<T> {
        RawMut::from_ptr(this.ptr)
    }

    pub fn iter(this): Iter<T> {
        Iter(
            ptr: this.ptr,
            end: unsafe core::ptr::offset(this.ptr, this.len),
        )
    }

    pub fn iter_mut(this): IterMut<T> {
        IterMut(
            ptr: this.ptr,
            end: unsafe core::ptr::offset_mut(this.ptr, this.len),
        )
    }

    pub fn subspan<R: RangeBounds<uint>>(this, range: R): [mut T..] {
        let start = match range.begin() {
            Bound::Inclusive(start) => start,
            Bound::Exclusive(start) => start + 1,
            Bound::Unbounded => 0,
        };

        let end = match range.end() {
            Bound::Inclusive(end) => end + 1,
            Bound::Exclusive(end) => end,
            Bound::Unbounded => this.len(),
        };

        if end < start || start > this.len || end > this.len {
            panic("SpanMut::subspan(): invalid range!");
        }

        SpanMut(
            ptr: unsafe core::ptr::offset_mut(this.ptr, start),
            len: end - start,
        )
    }
}

pub struct Iter<T> {
    ptr: *T,
    end: *T,

    impl Iterator<*T> {
        fn next(mut this): ?*T {
            if !core::ptr::eq(this.ptr, this.end) {
                core::mem::replace(&mut this.ptr, unsafe core::ptr::offset(this.ptr, 1))
            }
        }
    }
}

pub struct IterMut<T> {
    ptr: *mut T,
    end: *mut T,

    impl Iterator<*mut T> {
        fn next(mut this): ?*mut T {
            if !core::ptr::eq(this.ptr, this.end) {
                core::mem::replace(&mut this.ptr, unsafe core::ptr::offset_mut(this.ptr, 1))
            }
        }
    }
}

pub fn compare<T>(lhs: [T..], rhs: [T..]): bool {
    if lhs.len() != rhs.len() {
        return false;
    }

    unsafe core::mem::compare(lhs.as_raw().as_ptr(), rhs.as_raw().as_ptr(), lhs.len())
}
