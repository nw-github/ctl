use core::range::RangeBounds;
use core::reflect::*;

@(lang(span))
pub struct Span<T> {
    ptr: ^T,
    len: uint,

    pub unsafe fn new(ptr: ^T, len: uint): [T..] {
        Span(ptr:, len:)
    }

    pub fn from_ptr(ptr: *T): [T..] {
        unsafe Span::new(&raw *ptr, 1)
    }

    pub fn empty(): [T..] {
        Span(ptr: core::ptr::raw_dangling(), len: 0)
    }

    pub fn len(my this): uint {
        this.len
    }

    pub fn is_empty(my this): bool {
        this.len == 0
    }

    pub fn get(my this, idx: uint): ?*T {
        if idx < this.len {
            unsafe this.get_unchecked(idx)
        }
    }

    pub unsafe fn get_unchecked(my this, idx: uint): *T {
        unsafe (this.ptr + idx) as *T
    }

    pub fn as_raw(my this): ^T {
        this.ptr
    }

    pub fn iter(my this): Iter<T> {
        Iter(ptr: this.ptr, end: this.ptr + this.len)
    }

    pub fn subspan<R: RangeBounds<uint>>(my this, range: R): [T..] {
        let start = match range.begin() {
            :Inclusive(start) => start,
            :Exclusive(start) => start + 1,
            :Unbounded => 0,
        };

        let end = match range.end() {
            :Inclusive(end) => end + 1,
            :Exclusive(end) => end,
            :Unbounded => this.len,
        };

        if end < start or start > this.len or end > this.len {
            panic("Span::subspan(): invalid range!");
        }

        unsafe Span::new(this.ptr + start, end - start)
    }

    pub fn first(my this): ?*T {
        this.get(0)
    }

    pub fn last(my this): ?*T {
        if !this.is_empty() {
            unsafe this.get_unchecked(this.len() - 1)
        }
    }

    @(inline)
    pub fn []<I: Integral>(my this, idx: I): *T {
        unsafe raw_subscript_checked(this.ptr as ^mut T, this.len, idx) as *T
    }

    @(inline(always))
    pub fn []<R: RangeBounds<uint>>(my this, range: R): [T..] {
        this.subspan(range)
    }
}

@(lang(span_mut))
pub struct SpanMut<T> {
    ptr: ^mut T,
    len: uint,

    pub unsafe fn new(ptr: ^mut T, len: uint): [mut T..] {
        SpanMut(ptr:, len:)
    }

    pub fn from_ptr(ptr: *mut T): [mut T..] {
        unsafe SpanMut::new(&raw mut *ptr, 1)
    }

    pub fn empty(): [mut T..] {
        SpanMut(ptr: core::ptr::raw_dangling(), len: 0)
    }

    pub fn len(my this): uint {
        this.len
    }

    pub fn is_empty(my this): bool {
        this.len == 0
    }

    pub fn get(my this, idx: uint): ?*T {
        if idx < this.len {
            unsafe this.get_unchecked(idx)
        }
    }

    pub fn get_mut(my this, idx: uint): ?*mut T {
        if idx < this.len {
            unsafe this.get_mut_unchecked(idx)
        }
    }

    pub unsafe fn get_unchecked(my this, idx: uint): *T {
        unsafe (this.ptr + idx) as *T
    }

    pub unsafe fn get_mut_unchecked(my this, idx: uint): *mut T {
        unsafe (this.ptr + idx) as *mut T
    }

    pub fn as_span(my this): [T..] {
        this
    }

    pub fn as_raw(my this): ^T {
        this.ptr
    }

    pub fn as_raw_mut(my this): ^mut T {
        this.ptr
    }

    pub fn iter(this): Iter<T> {
        Iter(ptr: this.ptr, end: this.ptr + this.len)
    }

    pub fn iter_mut(my this): IterMut<T> {
        IterMut(ptr: this.ptr, end: this.ptr + this.len)
    }

    pub fn subspan<R: RangeBounds<uint>>(my this, range: R): [mut T..] {
        let start = match range.begin() {
            :Inclusive(start) => start,
            :Exclusive(start) => start + 1,
            :Unbounded => 0,
        };

        let end = match range.end() {
            :Inclusive(end) => end + 1,
            :Exclusive(end) => end,
            :Unbounded => this.len(),
        };

        if end < start or start > this.len or end > this.len {
            panic("SpanMut::subspan(): invalid range!");
        }

        unsafe SpanMut::new(this.ptr + start, end - start)
    }

    pub fn fill(my this, t: T) {
        // TODO: use memset when possible
        for item in this.iter_mut() {
            *item = t;
        }
    }

    pub fn swap(my this, a: uint, b: uint) {
        core::mem::swap(&mut this[a], &mut this[b]);
    }

    pub fn first(my this): ?*T {
        this.get(0)
    }

    pub fn first_mut(my this): ?*mut T {
        this.get_mut(0)
    }

    pub fn last(my this): ?*T {
        if !this.is_empty() {
            unsafe this.get_unchecked(this.len() - 1)
        }
    }

    pub fn last_mut(my this): ?*mut T {
        if !this.is_empty() {
            unsafe this.get_mut_unchecked(this.len() - 1)
        }
    }

    @(inline)
    pub fn []<I: Integral>(my this, idx: I): *mut T {
        unsafe raw_subscript_checked(this.ptr, this.len, idx) as *mut T
    }

    @(inline)
    pub fn []=<I: Integral>(my this, idx: I, val: T) {
        unsafe {
            *raw_subscript_checked(this.ptr, this.len, idx) = val;
        }
    }

    @(inline(always))
    pub fn []<R: RangeBounds<uint>>(my this, range: R): [mut T..] {
        this.subspan(range)
    }

    @(inline(always))
    pub fn []=<R: RangeBounds<uint>>(my this, range: R, rhs: [T..]) {
        let subspan = this.subspan(range);
        if subspan.len() != rhs.len() {
            panic("Span assignment requires that both sides are the same length");
        }

        // copy_overlapping?
        unsafe core::mem::copy(
            dst: subspan.as_raw_mut(),
            src: rhs.as_raw(),
            num: subspan.len(),
        );
    }
}

pub struct Iter<T> {
    ptr: ^T,
    end: ^T,

    impl Iterator<*T> {
        fn next(mut this): ?*T {
            if this.ptr != this.end {
                unsafe this.ptr++ as *T
            }
        }
    }
}

pub struct IterMut<T> {
    ptr: ^mut T,
    end: ^mut T,

    impl Iterator<*mut T> {
        fn next(mut this): ?*mut T {
            if this.ptr != this.end {
                unsafe this.ptr++ as *mut T
            }
        }
    }
}

@(inline(always))
fn raw_subscript_checked<T, I: Integral>(ptr: ^mut T, len: uint, idx: I): ^mut T {
    if idx.try_cast::<uint>() is ?idx and (0u..len).contains(&idx) {
        ptr + idx
    } else {
        panic("Span::[]: index out of bounds");
    }
}

// TODO: make these member functions/impls when the syntax allows for it

pub mod ext {
    use core::ops::Eq;
    use core::ops::Cmp;
    use core::hash::Hash;
    use core::hash::Hasher;
    use core::fmt::Format;
    use core::fmt::Formatter;

    pub extension SpanEq<T: Eq<T>> for [T..] {
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

    pub extension SpanHash<T: Hash> for [T..] {
        impl Hash {
            fn hash<H: Hasher>(this, hasher: *mut H) {
                for v in this.iter() {
                    v.hash(hasher);
                }
            }
        }
    }

    pub extension SpanMutHash<T: Hash> for [mut T..] {
        impl Hash {
            fn hash<H: Hasher>(this, hasher: *mut H) {
                for v in this.iter() {
                    v.hash(hasher);
                }
            }
        }
    }

    pub extension SpanMutEq<T: Eq<T>> for [mut T..] {
        pub fn ==(this, rhs: *[T..]): bool {
            this.as_span() == rhs
        }
    }

    pub extension SpanMutSort<T: Cmp<T>> for [mut T..] {
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

    pub extension SpanFormat<T: Format> for [T..] {
        impl Format {
            fn fmt<F: Formatter>(this, f: *mut F) {
                "[".fmt(f);
                for (i, item) in this.iter().enumerate() {
                    if i > 0 {
                        ", ".fmt(f);
                    }
                    item.fmt(f);
                }
                "]".fmt(f);
            }
        }
    }

    pub extension SpanMutFormat<T: Format> for [mut T..] {
        impl Format {
            fn fmt<F: Formatter>(this, f: *mut F) {
                this.as_span().fmt(f)
            }
        }
    }
}
