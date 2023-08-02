use core::ptr::Raw;
use core::ptr::RawMut;
use core::range::RangeBounds;
use core::range::Bound;

pub struct Span<T> {
    ptr: *T,
    len: usize,

    pub unsafe fn new<U>(ptr: *U, len: usize) [U..] {
        return Span(ptr:, len:);
    }

    pub fn len(this) usize {
        return this.len;
    }

    pub fn is_empty(this) bool {
        return this.len != 0;
    }

    pub fn get(this, idx: usize) ?*T {
        return if idx < this.len {
            yield this.get_unchecked(idx);
        };
    }

    pub unsafe fn get_unchecked(this, idx: usize) *T {
        return core::ptr::offset(this.ptr, idx);
    }

    pub fn as_raw(this) Raw<T> {
        return Raw::from_ptr(this.ptr);
    }

    pub fn iter(this) Iter<T> {
        return Iter(
            ptr: this.ptr,
            end: core::ptr::offset(this.ptr, this.len)
        );
    }

    pub fn subspan<R: RangeBounds<usize> >(this, range: R) [T..] {
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

        return Span(
            ptr: (this.ptr as usize + start * core::mem::size_of::<T>()) as *T,
            len: end - start
        );
    }
}

pub struct SpanMut<T> {
    ptr: *mut T,
    len: usize,

    pub unsafe fn new<U>(ptr: *mut U, len: usize) [mut U..] {
        return SpanMut::<U>(ptr:, len:);
    }

    pub fn len(this) usize {
        return this.len;
    }

    pub fn is_empty(this) bool {
        return this.len != 0;
    }

    pub fn get(this, idx: usize) ?*T {
        return if idx < this.len {
            yield this.get_unchecked(idx);
        };
    }

    pub fn get_mut(this, idx: usize) ?*mut T {
        return if idx < this.len {
            yield this.get_mut_unchecked(idx);
        };
    }

    pub unsafe fn get_unchecked(this, idx: usize) *T {
        return core::ptr::offset(this.ptr, idx);
    }

    pub unsafe fn get_mut_unchecked(this, idx: usize) *mut T {
        return core::ptr::offset_mut(this.ptr, idx);
    }

    pub fn as_raw(this) Raw<T> {
        return Raw::from_ptr(this.ptr);
    }

    pub fn as_raw_mut(this) RawMut<T> {
        return RawMut::from_ptr(this.ptr);
    }

    pub fn iter(this) Iter<T> {
        return Iter(
            ptr: this.ptr,
            end: core::ptr::offset(this.ptr, this.len)
        );
    }

    pub fn iter_mut(this) IterMut<T> {
        return IterMut(
            ptr: this.ptr,
            end: core::ptr::offset_mut(this.ptr, this.len)
        );
    }

    pub fn subspan<R: RangeBounds<usize> >(this, range: R) [mut T..] {
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

        return SpanMut(
            ptr: (this.ptr as usize + start * core::mem::size_of::<T>()) as *mut T,
            len: end - start
        );
    }
}

pub struct Iter<T>: core::iter::Iter<*T> {
    ptr: *T,
    end: *T,

    pub fn next(mut this) ?*T {
        return if !core::ptr::eq(this.ptr, this.end) {
            yield core::mem::replace(&mut this.ptr, core::ptr::offset(this.ptr, 1));
        };
    }
}

pub struct IterMut<T>: core::iter::Iter<*mut T> {
    ptr: *mut T,
    end: *mut T,

    pub fn next(mut this) ?*mut T {
        return if !core::ptr::eq(this.ptr, this.end) {
            yield core::mem::replace(&mut this.ptr, core::ptr::offset_mut(this.ptr, 1));
        };
    }
}
