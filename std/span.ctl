use std::range::RangeBounds;
use std::reflect::*;

@(lang(span))
pub struct Span<T> {
    ptr: ^T,
    len: uint,

    pub unsafe fn new(ptr: ^T, len: uint): This => Span(ptr:, len:);
    pub fn from_ptr(ptr: *T): This => unsafe Span::new(ptr, 1);
    pub fn empty(): This => Span(ptr: std::ptr::raw_dangling(), len: 0);

    pub fn len(my this): uint => this.len;
    pub fn is_empty(my this): bool => this.len == 0;

    pub fn get(my this, idx: uint): ?*T {
        if idx < this.len {
            unsafe this.get_unchecked(idx)
        }
    }

    pub unsafe fn get_unchecked(my this, idx: uint): *T => unsafe &*this.ptr.add(idx);

    pub fn as_raw(my this): ^T => this.ptr;

    pub fn iter(my this): Iter<T> => Iter(ptr: this.ptr, end: this.ptr.add(this.len));

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
            panic("invalid range {start}..{end} in span of len {this.len}");
        }

        unsafe Span::new(this.ptr.add(start), end - start)
    }

    pub fn first(my this): ?*T => this.get(0);

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
    pub fn []<R: RangeBounds<uint>>(my this, range: R): [T..] => this.subspan(range);
}

@(lang(span_mut))
pub struct SpanMut<T> {
    ptr: ^mut T,
    len: uint,

    pub unsafe fn new(ptr: ^mut T, len: uint): This => SpanMut(ptr:, len:);
    pub fn from_ptr(ptr: *mut T): This => unsafe SpanMut::new(ptr, 1);
    pub fn empty(): This => SpanMut(ptr: std::ptr::raw_dangling(), len: 0);

    pub fn len(my this): uint => this.len;
    pub fn is_empty(my this): bool => this.len == 0;

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

    pub unsafe fn get_unchecked(my this, idx: uint): *T => unsafe &*this.ptr.add(idx);
    pub unsafe fn get_mut_unchecked(my this, idx: uint): *mut T => unsafe &mut *this.ptr.add(idx);

    pub fn as_span(my this): [T..] => this;
    pub fn as_raw(my this): ^T => this.ptr;
    pub fn as_raw_mut(my this): ^mut T => this.ptr;

    pub fn iter(this): Iter<T> => Iter(ptr: this.ptr, end: this.ptr.add(this.len));
    pub fn iter_mut(my this): IterMut<T> => IterMut(ptr: this.ptr, end: this.ptr.add(this.len));

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
            panic("invalid range {start}..{end} in span of len {this.len}");
        }

        unsafe SpanMut::new(this.ptr.add(start), end - start)
    }

    pub fn fill(my this, t: T) {
        // TODO: use memset when possible
        for item in this.iter_mut() {
            *item = t;
        }
    }

    pub fn swap(my this, a: uint, b: uint) => std::mem::swap(&mut this[a], &mut this[b]);

    pub fn first(my this): ?*T => this.get(0);
    pub fn first_mut(my this): ?*mut T => this.get_mut(0);

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
        unsafe &mut *raw_subscript_checked(this.ptr, this.len, idx)
    }

    @(inline)
    pub fn []=<I: Integral>(my this, idx: I, val: T) {
        unsafe {
            *raw_subscript_checked(this.ptr, this.len, idx) = val;
        }
    }

    @(inline(always))
    pub fn []<R: RangeBounds<uint>>(my this, range: R): [mut T..] => this.subspan(range);

    @(inline(always))
    pub fn []=<R: RangeBounds<uint>>(my this, range: R, rhs: [T..]) {
        let subspan = this.subspan(range);
        if subspan.len() != rhs.len() {
            panic("span assignment requires that both sides are the same length");
        }

        // copy_overlapping?
        unsafe std::mem::copy(
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
        ptr.add(idx)
    } else {
        panic("index {idx} is out of bounds for span of length {len}");
    }
}

// TODO: make these member functions/impls when the syntax allows for it

pub mod ext {
    use std::ops::Eq;
    use std::ops::Cmp;
    use std::hash::Hash;
    use std::hash::Hasher;
    use std::fmt::Debug;
    use std::fmt::Formatter;

    pub extension SpanEq<T: Eq<T>> for [T..] {
        pub fn ==(this, rhs: *[T..]): bool {
            if this.len() != rhs.len() {
                return false;
            }

            for (l, r) in this.iter().zip(rhs.iter()) {
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
            fn hash<H: Hasher>(this, hasher: *mut H) => this.as_span().hash(hasher);
        }
    }

    pub extension SpanMutEq<T: Eq<T>> for [mut T..] {
        pub fn ==(this, rhs: *[T..]): bool => this.as_span() == rhs;
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

    pub extension SpanDebug<T: Debug> for [T..] {
        impl Debug {
            fn dbg(this, f: *mut Formatter) {
                f.write_char('[');
                for (i, item) in this.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ");
                    }
                    write(f, "{item:?}");
                }
                f.write_char(']');
            }
        }
    }

    pub extension SpanMutDebug<T: Debug> for [mut T..] {
        impl Debug {
            fn dbg(this, f: *mut Formatter) {
                this.as_span().dbg(f)
            }
        }
    }
}
