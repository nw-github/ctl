use core::ptr::Raw;
use core::ptr::RawMut;
use core::mem;
use core::iter::Iterator;
use core::span::*;

[lang(vec)]
pub struct Vec<T> {
    ptr: RawMut<T>,
    len: usize,
    cap: usize,

    pub fn new<U>() Vec<U> {
        Vec::<U>(ptr: RawMut::dangling(), len: 0, cap: 0)
    }

    pub fn with_capacity<U>(cap: usize) Vec<U> {
        mut self: Vec<U> = Vec::new();
        self.reserve(cap);
        self
    }

    pub fn from_span<U>(span: [U..]) Vec<U> {
        mut self: [U] = Vec::with_capacity(span.len());
        unsafe {
            mem::copy(
                dst: self.as_raw_mut().as_mut_ptr(),
                src: span.as_raw().as_ptr(),
                num: span.len()
            );
            self.set_len(span.len());
        }
        self
    }

    pub fn len(this) usize {
        this.len
    }

    pub fn is_empty(this) bool {
        this.len == 0
    }

    pub fn capacity(this) usize {
        this.cap
    }

    pub fn as_span(this) [T..] {
        unsafe Span::new(this.ptr.as_mut_ptr(), this.len)
    }

    pub fn as_span_mut(this) [mut T..] {
        unsafe SpanMut::new(this.ptr.as_mut_ptr(), this.len)
    }

    pub fn iter(this) Iter<T> {
        this.as_span().iter()
    }

    pub fn iter_mut(this) IterMut<T> {
        this.as_span_mut().iter_mut()
    }

    pub fn push(mut this, t: T) {
        if !this.can_insert(1) {
            this.grow();
        }

        unsafe this.ptr.add(this.len++).write(t);
    }

    pub fn push_within_capacity(mut this, t: T) ?T {
        if this.can_insert(1) {
            unsafe this.ptr.add(this.len++).write(t);
            null
        } else {
            t
        }
    }

    pub fn pop(mut this) ?T {
        if this.len > 0 {
            unsafe this.ptr.add(--this.len).read()
        }
    }

    pub fn append(mut this, rhs: *mut Vec<T>) {
        if !this.can_insert(rhs.len) {
            this.grow();
        }

        unsafe mem::copy(
            dst: this.ptr.add(this.len).as_mut_ptr(),
            src: rhs.ptr.as_ptr(),
            num: rhs.len
        );

        this.len += mem::replace(&mut rhs.len, 0);
    }

    pub fn extend<I: Iterator<*T> >(mut this, iter: I) {
        for elem in iter {
            this.push(*elem);
        }
    }

    pub fn clear(mut this) {
        this.len = 0;
    }

    pub fn insert(mut this, kw idx: usize, t: T) {
        if idx > this.len {
            panic("Vec::insert(): index > len!");
        }

        if !this.can_insert(1) {
            this.grow();
        }

        let src = this.ptr.add(idx);
        if idx < this.len {
            unsafe mem::move(
                dst: this.ptr.add(idx + 1).as_mut_ptr(), 
                src: src.as_ptr(), 
                num: this.len - idx
            );
        }

        unsafe src.write(t);
        this.len++;
    }

    pub fn remove(mut this, idx: usize) T {
        if idx >= this.len {
            panic("Vec::remove(): index out of bounds!");
        }

        unsafe {
            let ptr = this.ptr.add(idx);
            let t   = ptr.read();
            if idx + 1 < this.len {
                mem::move(
                    dst: ptr.as_mut_ptr(),
                    src: ptr.add(1).as_ptr(),
                    num: this.len - idx
                );
            }

            this.len--;
            t
        }
    }

    pub fn swap_remove(mut this, idx: usize) T {
        if idx >= this.len {
            panic("Vec::swap_remove(): index out of bounds!");
        }

        this.len--;

        let ptr = this.ptr.add(idx);
        if idx < this.len {
            unsafe mem::replace(ptr.as_mut_ptr(), this.ptr.add(this.len).read())
        } else {
            unsafe ptr.read()
        }
    }

    pub fn truncate(mut this, len: usize) {
        if len < this.len {
            this.len = len;
        }
    }

    pub fn reserve(mut this, add: usize) {
        this._reserve(this.len + add);
    }

    pub fn get(this, idx: usize) ?*T {
        if idx < this.len {
            unsafe this.ptr.add(idx).as_ptr()
        }
    }

    pub fn get_mut(mut this, idx: usize) ?*mut T {
        if idx < this.len {
            unsafe this.ptr.add(idx).as_mut_ptr()
        }
    }

    pub fn as_raw(this) Raw<T> {
        Raw::from_mut(this.ptr)
    }

    pub fn as_raw_mut(mut this) RawMut<T> {
        this.ptr
    }

    pub unsafe fn set_len(mut this, len: usize) {
        this.len = len;
    }

    fn grow(mut this) {
        this._reserve(if this.cap > 0 { this.cap } else { 1 });
    }

    fn can_insert(this, count: usize) bool {
        this.len + count <= this.cap
    }

    fn _reserve(mut this, cap: usize) {
        if cap <= this.cap {
            return;
        }

        let ptr = if this.len == 0 {
            std::alloc::alloc::<T>(cap)
        } else {
            std::alloc::realloc(unsafe this.ptr.as_mut_ptr(), cap)
        };
        match ptr {
            ?ptr => {
                this.ptr = ptr;
                this.cap = cap;
            },
            null => panic("Vec::reserve(): out of memory!"),
        }
    }
}
