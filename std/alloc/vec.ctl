use std::mem;
use std::span::*;
use std::range::RangeBounds;
use std::reflect::*;

@(feature(alloc))
@(lang(vec))
pub struct Vec<T> {
    ptr: ^mut T,
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

    pub fn push(mut this, val: T) {
        if !this.can_insert(1) {
            this.grow();
        }

        unsafe this.ptr.uoffset(this.len++).write(val);
    }

    pub fn push_within_capacity(mut this, val: T): ?T {
        if this.can_insert(1) {
            unsafe this.ptr.uoffset(this.len++).write(val);
            null
        } else {
            val
        }
    }

    pub fn pop(mut this): ?T {
        if this.len > 0 {
            unsafe this.ptr.uoffset(--this.len).read()
        }
    }

    pub fn append(mut this, rhs: *mut This) {
        if !this.can_insert(rhs.len) {
            this.grow();
        }

        unsafe mem::copy(
            dst: this.ptr.uoffset(this.len),
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

    pub fn insert(mut this, kw idx: uint, val: T) {
        if idx > this.len {
            panic("index {idx} is greater than length {this.len}");
        }

        if !this.can_insert(1) {
            this.grow();
        }

        let src = this.ptr.uoffset(idx);
        if idx < this.len {
            unsafe mem::copy_overlapping(dst: src.offset(1), src:, num: this.len - idx);
        }

        unsafe src.write(val);
        this.len++;
    }

    pub fn remove(mut this, idx: uint): T {
        if idx >= this.len {
            panic("index {idx} is out of bounds for vector of length {this.len}");
        }

        unsafe {
            let dst = this.ptr.uoffset(idx);
            let res = dst.read();
            if idx + 1 < this.len {
                mem::copy_overlapping(dst:, src: dst.offset(1), num: this.len - idx);
            }

            this.len--;
            res
        }
    }

    pub fn swap_remove(mut this, idx: uint): T {
        if idx >= this.len {
            panic("index {idx} is out of bounds for vector of length {this.len}");
        }

        this.len--;

        let ptr = this.ptr.uoffset(idx);
        if idx < this.len {
            unsafe mem::replace(&mut *ptr, this.ptr.uoffset(this.len).read())
        } else {
            unsafe ptr.read()
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
            unsafe &*this.ptr.uoffset(idx)
        }
    }

    pub fn get_mut(mut this, idx: uint): ?*mut T {
        if idx < this.len {
            unsafe &mut *this.ptr.uoffset(idx)
        }
    }

    pub fn as_raw(this): ^T {
        this.ptr
    }

    pub fn as_raw_mut(this): ^mut T {
        this.ptr
    }

    pub unsafe fn set_len(mut this, len: uint) {
        this.len = len;
    }

    fn grow(mut this) {
        this._reserve(this.cap * 2);
    }

    fn can_insert(this, count: uint): bool {
        this.len + count <= this.cap
    }

    fn _reserve(mut this, cap: uint) {
        let cap = cap.max(1);
        if cap <= this.cap {
            return;
        }

        // TODO: some kind of "if const"
        if std::mem::size_of::<T>() == 0 {
            this.cap = cap;
            return;
        }

        let ptr = if this.cap == 0 {
            std::alloc::alloc::<T>(cap)
        } else {
            std::alloc::realloc(this.ptr, cap)
        };
        if ptr is ?ptr {
            this.ptr = ptr;
            this.cap = cap;
        } else {
            let bytes = cap * std::mem::size_of::<T>();
            panic("out of memory trying to allocate {bytes} bytes");
        }
    }

    impl std::iter::FromIter<T> {
        fn from_iter<I: Iterator<T>>(iter: I): This {
            mut self: [T] = @[]; // TODO: size hint
            for item in iter {
                self.push(item);
            }
            self
        }
    }

    @(inline(always))
    pub fn []<I: Integral>(this, idx: I): *T {
        &this[..][idx]
    }

    @(inline(always))
    pub fn []<I: Integral>(mut this, idx: I): *mut T {
        &mut this[..][idx]
    }

    @(inline(always))
    pub fn []=<I: Integral>(mut this, idx: I, val: T) {
        this[..][idx] = val;
    }

    @(inline(always))
    pub fn []<R: RangeBounds<uint>>(this, range: R): [T..] {
        this.as_span()[range]
    }

    @(inline(always))
    pub fn []<R: RangeBounds<uint>>(mut this, range: R): [mut T..] {
        this.as_span_mut()[range]
    }

    @(inline(always))
    pub fn []=<R: RangeBounds<uint>>(mut this, range: R, rhs: [T..]) {
        this[range] = rhs;
    }
}
