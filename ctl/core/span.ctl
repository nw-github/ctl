use core::range::RangeBounds;
use core::range::Bound;
use core::iter::Iterator;
use core::panic;
use core::reflect::*;
use core::ext::*;

#(lang(span))
pub struct Span<T> {
    ptr: *raw T,
    len: uint,

    pub unsafe fn new(ptr: *raw T, len: uint): [T..] {
        Span(ptr:, len:)
    }

    pub fn from_ptr(ptr: *T): [T..] {
        unsafe Span::new(ptr as *raw T, 1)
    }

    pub fn empty(): [T..] {
        Span(ptr: core::ptr::raw_dangling(), len: 0)
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
        unsafe (this.ptr + idx) as *T
    }

    pub fn as_raw(this): *raw T {
        this.ptr
    }

    pub fn iter(this): Iter<T> {
        Iter(ptr: this.ptr, end: this.ptr + this.len)
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
            Bound::Unbounded => this.len,
        };

        if end < start or start > this.len or end > this.len {
            panic("Span::subspan(): invalid range!");
        }

        unsafe Span::new(this.ptr + start, end - start)
    }

    pub fn first(this): ?*T {
        this.get(0)
    }

    pub fn last(this): ?*T {
        if !this.is_empty() {
            unsafe this.get_unchecked(this.len() - 1)
        }
    }

    #(inline)
    pub fn []<I: Numeric + Integral>(this, idx: I): *T {
        unsafe raw_subscript_checked(this.ptr, this.len, idx) as *T
    }

    #(inline(always))
    pub fn []<R: RangeBounds<uint>>(this, range: R): [T..] {
        this.subspan(range)
    }

    // TODO: remove this when RangeFull can implement rangebounds
    #(inline(always))
    pub fn [](this, _: core::range::RangeFull): [T..] {
        *this
    }
}

#(lang(span_mut))
pub struct SpanMut<T> {
    ptr: *raw T,
    len: uint,

    pub unsafe fn new(ptr: *raw T, len: uint): [mut T..] {
        SpanMut(ptr:, len:)
    }

    pub fn from_ptr(ptr: *mut T): [mut T..] {
        unsafe SpanMut::new(ptr as *raw T, 1)
    }

    pub fn empty(): [mut T..] {
        SpanMut(ptr: core::ptr::raw_dangling(), len: 0)
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
        unsafe (this.ptr + idx) as *T
    }

    pub unsafe fn get_mut_unchecked(this, idx: uint): *mut T {
        unsafe (this.ptr + idx) as *mut T
    }

    pub fn as_span(this): [T..] {
        unsafe Span::new(this.ptr, this.len)
    }

    pub fn as_raw(this): *raw T {
        this.ptr
    }

    pub fn iter(this): Iter<T> {
        Iter(ptr: this.ptr, end: this.ptr + this.len)
    }

    pub fn iter_mut(this): IterMut<T> {
        IterMut(ptr: this.ptr, end: this.ptr + this.len)
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

        if end < start or start > this.len or end > this.len {
            panic("SpanMut::subspan(): invalid range!");
        }

        unsafe SpanMut::new(this.ptr + start, end - start)
    }

    pub fn fill(this, t: T) {
        // TODO: use memset when possible
        for item in this.iter_mut() {
            *item = t;
        }
    }

    pub fn swap(this, a: uint, b: uint) {
        core::mem::swap(&mut this[a], &mut this[b]);
    }

    pub fn first(this): ?*T {
        this.get(0)
    }

    pub fn first_mut(this): ?*mut T {
        this.get_mut(0)
    }

    pub fn last(this): ?*T {
        if !this.is_empty() {
            unsafe this.get_unchecked(this.len() - 1)
        }
    }

    pub fn last_mut(this): ?*mut T {
        if !this.is_empty() {
            unsafe this.get_mut_unchecked(this.len() - 1)
        }
    }

    #(inline)
    pub fn []<I: Numeric + Integral>(this, idx: I): *mut T {
        unsafe raw_subscript_checked(this.ptr, this.len, idx) as *mut T
    }

    #(inline)
    pub fn []=<I: Numeric + Integral>(this, idx: I, val: T) {
        unsafe {
            *raw_subscript_checked(this.ptr, this.len, idx) = val;
        }
    }

    #(inline(always))
    pub fn []<R: RangeBounds<uint>>(this, range: R): [mut T..] {
        this.subspan(range)
    }

    // TODO: remove this when RangeFull can implement rangebounds
    #(inline(always))
    pub fn [](this, _: core::range::RangeFull): [mut T..] {
        *this
    }

    #(inline(always))
    pub fn []=<R: RangeBounds<uint>>(this, range: R, rhs: [T..]) {
        let subspan = this.subspan(range);
        if subspan.len() != rhs.len() {
            panic("Span assignment requires that both sides are the same length");
        }

        // copy_overlapping?
        unsafe core::mem::copy(
            dst: subspan.as_raw(),
            src: rhs.as_raw(),
            num: subspan.len(),
        );
    }

    // TODO: remove this when RangeFull can implement rangebounds
    #(inline(always))
    pub fn []=(this, _: core::range::RangeFull, rhs: [T..]) {
        this[0u..] = rhs;
    }
}

pub struct Iter<T> {
    ptr: *raw T,
    end: *raw T,

    impl Iterator<*T> {
        fn next(mut this): ?*T {
            if this.ptr != this.end {
                unsafe this.ptr++ as *T
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
                unsafe this.ptr++ as *mut T
            }
        }
    }
}

#(inline(always))
fn raw_subscript_checked<T, I: Numeric + Integral>(ptr: *raw T, len: uint, idx: I): *raw T {
    if !core::intrin::numeric_lt(idx, len) or idx < 0.cast() {
        panic("Span::[]: index out of bounds");
    }

    core::intrin::raw_offset(ptr, idx)
}

// TODO: make these member functions/impls when the syntax allows for it

pub mod ext {
    pub extension SpanEq<T: core::ops::Eq<T>> for [T..] {
        pub fn ==(this, rhs: *[T..]): bool {
            if this.len() != rhs.len() {
                return false;
            }

            for (l, r) in this.iter().zip::<*T, super::Iter<T>>(rhs.iter()) {
                if l != r {
                    return false;
                }
            }

            true
        }
    }

    pub extension SpanMutEq<T: core::ops::Eq<T>> for [mut T..] {
        pub fn ==(this, rhs: *[T..]): bool {
            this.as_span() == rhs
        }
    }

    pub extension SpanMutSort<T: core::ops::Cmp<T>> for [mut T..] {
        pub fn sort(this) {
            guard !this.is_empty() else {
                return;
            }

            let end      = this.len() - 1;
            let pivot    = &this[end];
            mut part_idx = 0u;
            for j in 0u..end {
                if this[j] <= pivot {
                    this.swap(part_idx, j);
                    part_idx++;
                }
            }

            this.swap(part_idx, end);
            this[0u..part_idx].sort();
            this[part_idx + 1..].sort();
        }
    }

    pub extension SpanFormat<T: core::fmt::Format> for [T..] {
        impl core::fmt::Format {
            fn format<F: core::fmt::Formatter>(this, f: *mut F) {
                "[".format(f);
                for (i, item) in this.iter().enumerate() {
                    if i > 0 {
                        ", ".format(f);
                    }
                    item.format(f);
                }
                "]".format(f);
            }
        }
    }

    pub extension SpanMutFormat<T: core::fmt::Format> for [mut T..] {
        impl core::fmt::Format {
            fn format<F: core::fmt::Formatter>(this, f: *mut F) {
                this.as_span().format(f)
            }
        }
    }
}

