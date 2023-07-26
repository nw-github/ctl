use core::ptr::Raw;
use core::ptr::RawMut;

pub struct Span<T> {
    ptr: *T,
    len: usize,

    pub /* unsafe */ fn new<U>(ptr: *U, len: usize) [U..] {
        // these are the same type, but we have to convince the type system
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
            yield core::ptr::offset(this.ptr, idx);
        };
    }

    pub fn as_raw(this) Raw<T> {
        return Raw::from_ptr(this.ptr);
    }
}

pub struct SpanMut<T> {
    ptr: *mut T,
    len: usize,

    pub /* unsafe */ fn new<U>(ptr: *mut U, len: usize) [mut U..] {
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
            yield core::ptr::offset(this.ptr, idx);
        };
    }

    pub fn get_mut(this, idx: usize) ?*mut T {
        return if idx < this.len {
            yield core::ptr::offset_mut(this.ptr, idx);
        };
    }

    pub fn as_raw(this) Raw<T> {
        return Raw::from_ptr(this.ptr);
    }

    pub fn as_raw_mut(this) RawMut<T> {
        return RawMut::from_ptr(this.ptr);
    }
}
