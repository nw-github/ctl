use std::{mem, mem::{Layout, Uninit}};
use std::range::RangeBounds;
use std::reflect::Integral;

$[feature(alloc)]
$[lang(vec)]
pub struct Vec<T> {
    ptr: ^mut T,
    len: uint,
    cap: uint,
    allocator: super::DefaultAllocator,

    pub fn new(): This {
        Vec(ptr: std::ptr::raw_dangling(), len: 0, cap: 0, allocator: super::DefaultAllocator())
    }

    pub fn with_capacity(cap: uint): This {
        mut self: This = Vec::new();
        self.reserve(add: cap);
        self
    }

    pub fn from_span(span: [T..]): This {
        mut self: This = Vec::with_capacity(span.len());
        unsafe {
            mem::copy_no_overlap(dst: self.ptr, src: span.as_raw(), num: span.len());
            self.set_len(span.len());
        }
        self
    }

    pub fn deinit(mut this) {
        if this.cap != 0 {
            unsafe this.allocator.free(this.ptr.cast(), Layout::of::<T>().array(this.len)!);
            this.cap = 0;
            this.len = 0;
        }
    }

    pub fn capacity(this): uint => this.cap;

    pub fn spare_capacity(this): uint => this.cap - this.len;

    pub fn spare_capacity_mut(mut this): [mut Uninit<T>..] {
        unsafe SpanMut::new(
            ptr: Uninit::from_raw_mut(this.ptr.add(this.len)),
            len: this.cap - this.len,
        )
    }

    pub fn push(mut this, val: T) {
        if !this.can_insert(1) {
            this.grow();
        }

        unsafe this.push_unchecked(val);
    }

    pub unsafe fn push_unchecked(mut this, val: T) {
        unsafe this.ptr.add(this.len++).write(val);
    }

    pub fn push_within_capacity(mut this, val: T): ?T {
        if this.can_insert(1) {
            unsafe this.push_unchecked(val);
            null
        } else {
            val
        }
    }

    pub fn pop(mut this): ?T {
        if this.len > 0 {
            unsafe this.ptr.add(--this.len).read()
        }
    }

    pub fn append(mut this, rhs: *mut This) {
        if !this.can_insert(rhs.len) {
            this.reserve(add: rhs.len);
        }

        unsafe mem::copy_no_overlap(
            dst: this.ptr.add(this.len),
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

        let src = this.ptr.add(idx);
        if idx < this.len {
            unsafe mem::copy(dst: src.add(1), src:, num: this.len - idx);
        }

        unsafe src.write(val);
        this.len++;
    }

    pub fn remove(mut this, idx: uint): T {
        if idx >= this.len {
            panic("index {idx} is out of bounds for vector of length {this.len}");
        }

        unsafe {
            let dst = this.ptr.add(idx);
            let res = dst.read();
            if idx + 1 < this.len {
                mem::copy(dst:, src: dst.add(1), num: this.len - idx);
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

        let ptr = this.ptr.add(idx);
        if idx < this.len {
            unsafe mem::replace(&mut *ptr, this.ptr.add(this.len).read())
        } else {
            unsafe ptr.read()
        }
    }

    pub fn truncate(mut this, len: uint) {
        if len < this.len {
            this.len = len;
        }
    }

    pub fn reserve(mut this, kw add: uint) => this.do_reserve(this.len + add);

    pub unsafe fn set_len(mut this, len: uint) {
        this.len = len;
    }

    fn grow(mut this) => this.do_reserve(this.cap * 2);

    fn can_insert(this, count: uint): bool => this.len + count <= this.cap;

    fn do_reserve(mut this, cap: uint) {
        let cap = cap.max(1);
        if cap <= this.cap {
            return;
        }

        // TODO: some kind of "if const"
        if mem::size_of::<T>() == 0 {
            this.cap = cap;
            return;
        }

        guard Layout::of::<T>().array(cap) is ?layout else {
            panic("trying to make an allocation larger than the max size possible");
        }

        let ptr = if this.cap == 0 {
            this.allocator.alloc(layout)
        } else {
            unsafe this.allocator.resize(this.ptr.cast(), layout)
        };
        if ptr is ?ptr {
            this.ptr = ptr.cast();
            this.cap = cap;
        } else {
            let bytes = cap * mem::size_of::<T>();
            panic("out of memory trying to allocate {bytes} bytes");
        }
    }

    impl std::span::AsSpan<T> {
        fn as_span(this): [T..] => unsafe Span::new(this.ptr, this.len);
        fn len(this): uint => this.len;
        fn as_raw(this): ^T => this.ptr;
        fn get(this, idx: uint): ?*T {
            if idx < this.len {
                unsafe &*this.ptr.add(idx)
            }
        }
    }

    impl std::span::AsSpanMut<T> {
        fn as_span_mut(mut this): [mut T..] => unsafe SpanMut::new(this.ptr, this.len);
        fn as_raw_mut(mut this): ^mut T => this.ptr;
        fn get_mut(mut this, idx: uint): ?*mut T {
            if idx < this.len {
                unsafe &mut *this.ptr.add(idx)
            }
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

    impl std::fmt::Debug {
        fn dbg(this, f: *mut std::fmt::Formatter) => this[..].dbg(f);
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
}

unittest "@[T; N] initialization" {
    mut x = @[0xff; 5];
    assert_eq(x.len(), 5);
    assert_eq(x.pop(), 0xff);
    assert_eq(x.pop(), 0xff);
    assert_eq(x.pop(), 0xff);
    assert_eq(x.pop(), 0xff);
    assert_eq(x.pop(), 0xff);
    assert_eq(x.pop(), null);
}

unittest "list initialization" {
    mut x = @[1, 2, 3, 4];
    assert_eq(x.len(), 4);
    assert_eq(x.pop(), 4);
    assert_eq(x.pop(), 3);
    assert_eq(x.pop(), 2);
    assert_eq(x.pop(), 1);
    assert_eq(x.pop(), null);
}

unittest "removal" {
    mut vec = @[1, 2, 3, 4, 5, 6, 7];
    assert_eq(vec.swap_remove(1), 2);
    assert_eq(vec.len(), 6);
    assert_eq(vec.remove(3), 4);
    assert_eq(vec.len(), 5);
    assert_eq(vec[..], [1, 7, 3, 5, 6][..]);
}

unittest "push and insert" {
    mut x: [int] = @[];
    x.push(1);
    x.push(2);
    x.push(3);
    assert_eq(x.len(), 3);

    x.insert(idx: 1, 4);
    assert_eq(x.len(), 4);

    assert_eq(x.pop(), 3);
    assert_eq(x.pop(), 2);
    assert_eq(x.pop(), 4);
    assert_eq(x.pop(), 1);
    assert_eq(x.pop(), null);
}
