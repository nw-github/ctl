use std::mem;
use std::span::*;
use std::range::RangeBounds;

#(lang(vec))
pub struct Vec<T> {
    ptr: *raw T,
    len: uint,
    cap: uint,

    pub fn new(): This {
        Vec(ptr: std::ptr::raw_dangling(), len: 0, cap: 0)
    }

    pub fn with_capacity(cap: uint): This {
        mut self: This = Vec::new();
        self.reserve(cap);
        self
    }

    pub fn from_span(span: [T..]): This {
        mut self: This = Vec::with_capacity(span.len());
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
        this[..].iter()
    }

    pub fn iter_mut(mut this): IterMut<T> {
        this[..].iter_mut()
    }

    pub fn push(mut this, t: T) {
        if !this.can_insert(1) {
            this.grow();
        }

        unsafe *(this.ptr + this.len++) = t;
    }

    pub fn push_within_capacity(mut this, t: T): ?T {
        if this.can_insert(1) {
            unsafe *(this.ptr + this.len++) = t;
            null
        } else {
            t
        }
    }

    pub fn pop(mut this): ?T {
        if this.len > 0 {
            unsafe *(this.ptr + --this.len)
        }
    }

    pub fn append(mut this, rhs: *mut This) {
        if !this.can_insert(rhs.len) {
            this.grow();
        }

        unsafe mem::copy(
            dst: this.ptr + this.len,
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

        let src = this.ptr + idx;
        if idx < this.len {
            unsafe mem::copy_overlapping(
                dst: src + 1, 
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
            let dst = this.ptr + idx;
            let res = *dst;
            if idx + 1 < this.len {
                mem::copy_overlapping(
                    dst:,
                    src: dst + 1,
                    num: this.len - idx,
                );
            }

            this.len--;
            res
        }
    }

    pub fn swap_remove(mut this, idx: uint): T {
        if idx >= this.len {
            panic("Vec::swap_remove(): index out of bounds!");
        }

        this.len--;

        let ptr = this.ptr + idx;
        if idx < this.len {
            unsafe mem::replace(ptr as *mut T, *(this.ptr + this.len))
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
            unsafe (this.ptr + idx) as *T
        }
    }

    pub fn get_mut(mut this, idx: uint): ?*mut T {
        if idx < this.len {
            unsafe (this.ptr + idx) as *mut T
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
        if ptr is ?ptr {
            this.ptr = ptr;
            this.cap = cap;
        } else {
            panic("Vec::_reserve(): out of memory!");
        }
    }

    impl core::iter::FromIter<T> {
        fn from_iter<I: Iterator<T>>(iter: I): This {
            mut self: [T] = @[]; // TODO: size hint
            for item in iter {
                self.push(item);
            }
            self
        }
    }

    #(inline(always))
    pub fn [](this, idx: uint): *T {
        &this[..][idx]
    }

    #(inline(always))
    pub fn [](mut this, idx: uint): *mut T {
        &mut this[..][idx]
    }

    #(inline(always))
    pub fn []=(mut this, idx: uint, val: T) {
        this[..][idx] = val;
    }

    #(inline(always))
    pub fn []<R: RangeBounds<uint>>(this, range: R): [T..] {
        this.as_span()[range]
    }

    #(inline(always))
    pub fn []<R: RangeBounds<uint>>(mut this, range: R): [mut T..] {
        this.as_span_mut()[range]
    }

    // TODO: remove these when RangeFull can implement rangebounds
    #(inline(always))
    pub fn [](this, _: std::range::RangeFull): [T..] {
        this.as_span()
    }

    #(inline(always))
    pub fn [](mut this, _: std::range::RangeFull): [mut T..] {
        this.as_span_mut()
    }
}
