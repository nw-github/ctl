use core::range::RangeBounds;
use core::range::Bound;
use core::iter::Iterator;
use core::panic;

#(lang(span))
pub struct Span<T> {
    ptr: *raw T,
    len: uint,

    pub unsafe fn new(ptr: *raw T, len: uint): [T..] {
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
        unsafe core::ptr::raw_add(this.ptr, idx) as *T
    }

    pub fn as_raw(this): *raw T {
        this.ptr
    }

    pub fn iter(this): Iter<T> {
        Iter(
            ptr: this.ptr,
            end: unsafe core::ptr::raw_add(this.ptr, this.len),
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
            ptr: unsafe core::ptr::raw_add(this.ptr, start),
            len: end - start,
        )
    }
}

#(lang(span_mut))
pub struct SpanMut<T> {
    ptr: *raw T,
    len: uint,

    pub unsafe fn new(ptr: *raw T, len: uint): [mut T..] {
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
        unsafe core::ptr::raw_add(this.ptr, idx) as *T
    }

    pub unsafe fn get_mut_unchecked(this, idx: uint): *mut T {
        unsafe core::ptr::raw_add(this.ptr, idx) as *mut T
    }

    pub fn as_raw(this): *raw T {
        this.ptr
    }

    pub fn iter(this): Iter<T> {
        Iter(
            ptr: this.ptr,
            end: unsafe core::ptr::raw_add(this.ptr, this.len),
        )
    }

    pub fn iter_mut(this): IterMut<T> {
        IterMut(
            ptr: this.ptr,
            end: unsafe core::ptr::raw_add(this.ptr, this.len),
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
            ptr: unsafe core::ptr::raw_add(this.ptr, start),
            len: end - start,
        )
    }
}

pub struct Iter<T> {
    ptr: *raw T,
    end: *raw T,

    impl Iterator<*T> {
        fn next(mut this): ?*T {
            if this.ptr != this.end {
                unsafe core::mem::replace::<*raw T>(
                    &mut this.ptr,
                    core::ptr::raw_add(this.ptr, 1),
                ) as *T
            }
        }
    }
}

pub struct IterMut<T> {
    ptr: *raw T,
    end: *raw T,

    impl Iterator<*mut T> {
        fn next(mut this): ?*mut T {
            if this.ptr != this.end {
                unsafe core::mem::replace::<*raw T>(
                    &mut this.ptr,
                    core::ptr::raw_add(this.ptr, 1),
                ) as *mut T
            }
        }
    }
}

pub fn compare<T>(lhs: [T..], rhs: [T..]): bool {
    if lhs.len() != rhs.len() {
        return false;
    }

    unsafe core::mem::compare(lhs.ptr, rhs.ptr, lhs.len())
}
