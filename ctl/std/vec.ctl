use std::mem;
use std::span::*;

#(lang(vec))
pub struct Vec<T> {
    ptr: *raw T,
    len: uint,
    cap: uint,

    pub fn new(): Vec<T> {
        Vec(ptr: std::ptr::raw_dangling(), len: 0, cap: 0)
    }

    pub fn with_capacity(cap: uint): Vec<T> {
        mut self: Vec<T> = Vec::new();
        self.reserve(cap);
        self
    }

    pub fn from_span(span: [T..]): Vec<T> {
        mut self: [T] = Vec::with_capacity(span.len());
        unsafe {
            mem::copy(
                dst: self.ptr,
                src: span.as_raw(),
                num: span.len(),
            );
            self.set_len(span.len());
        }
        self
    }

    pub fn len(this): uint {
        this.len
    }

    pub fn is_empty(this): bool {
        this.len == 0
    }

    pub fn capacity(this): uint {
        this.cap
    }

    pub fn as_span(this): [T..] {
        unsafe Span::new(this.ptr, this.len)
    }

    pub fn as_span_mut(mut this): [mut T..] {
        unsafe SpanMut::new(this.ptr, this.len)
    }

    pub fn iter(this): Iter<T> {
        this.as_span().iter()
    }

    pub fn iter_mut(mut this): IterMut<T> {
        this.as_span_mut().iter_mut()
    }

    pub fn push(mut this, t: T) {
        if !this.can_insert(1) {
            this.grow();
        }

        unsafe *std::ptr::raw_add(this.ptr, this.len++) = t;
    }

    pub fn push_within_capacity(mut this, t: T): ?T {
        if this.can_insert(1) {
            unsafe *std::ptr::raw_add(this.ptr, this.len++) = t;
            null
        } else {
            t
        }
    }

    pub fn pop(mut this): ?T {
        if this.len > 0 {
            unsafe *std::ptr::raw_add(this.ptr, --this.len)
        }
    }

    pub fn append(mut this, rhs: *mut Vec<T>) {
        if !this.can_insert(rhs.len) {
            this.grow();
        }

        unsafe mem::copy(
            dst: std::ptr::raw_add::<T>(this.ptr, this.len),
            src: rhs.ptr,
            num: rhs.len,
        );

        this.len += mem::replace(&mut rhs.len, 0);
    }

    pub fn extend<I: Iterator<*T>>(mut this, iter: I) {
        for elem in iter {
            this.push(*elem);
        }
    }

    pub fn clear(mut this) {
        this.len = 0;
    }

    pub fn insert(mut this, kw idx: uint, t: T) {
        if idx > this.len {
            panic("Vec::insert(): index > len!");
        }

        if !this.can_insert(1) {
            this.grow();
        }

        let src = std::ptr::raw_add(this.ptr, idx);
        if idx < this.len {
            unsafe mem::copy_overlapping(
                dst: std::ptr::raw_add::<T>(src, 1), 
                src:, 
                num: this.len - idx,
            );
        }

        unsafe *src = t;
        this.len++;
    }

    pub fn remove(mut this, idx: uint): T {
        if idx >= this.len {
            panic("Vec::remove(): index out of bounds!");
        }

        unsafe {
            let dst = std::ptr::raw_add(this.ptr, idx);
            let t   = unsafe *dst;
            if idx + 1 < this.len {
                mem::copy_overlapping(
                    dst:,
                    src: std::ptr::raw_add::<T>(dst, 1),
                    num: this.len - idx,
                );
            }

            this.len--;
            t
        }
    }

    pub fn swap_remove(mut this, idx: uint): T {
        if idx >= this.len {
            panic("Vec::swap_remove(): index out of bounds!");
        }

        this.len--;

        let ptr = std::ptr::raw_add(this.ptr, idx);
        if idx < this.len {
            unsafe mem::replace(ptr as *mut T, unsafe *std::ptr::raw_add(this.ptr, this.len))
        } else {
            unsafe *ptr
        }
    }

    pub fn truncate(mut this, len: uint) {
        if len < this.len {
            this.len = len;
        }
    }

    pub fn reserve(mut this, add: uint) {
        this._reserve(this.len + add);
    }

    pub fn get(this, idx: uint): ?*T {
        if idx < this.len {
            unsafe std::ptr::raw_add(this.ptr, idx) as *T
        }
    }

    pub fn get_mut(mut this, idx: uint): ?*mut T {
        if idx < this.len {
            unsafe std::ptr::raw_add(this.ptr, idx) as *mut T
        }
    }

    pub fn as_raw(this): *raw T {
        this.ptr
    }

    pub unsafe fn set_len(mut this, len: uint) {
        this.len = len;
    }

    fn grow(mut this) {
        this._reserve(if this.cap > 0 { this.cap * 2 } else { 1 });
    }

    fn can_insert(this, count: uint): bool {
        this.len + count <= this.cap
    }

    fn _reserve(mut this, cap: uint) {
        if cap <= this.cap {
            return;
        }

        let ptr = if this.cap == 0 {
            std::alloc::alloc::<T>(cap)
        } else {
            std::alloc::realloc(unsafe this.ptr as *mut T, cap)
        };
        if (ptr is ?ptr) {
            this.ptr = ptr;
            this.cap = cap;
        } else {
            panic("Vec::_reserve(): out of memory!");
        }
    }
}
