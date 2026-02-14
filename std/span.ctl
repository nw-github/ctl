use std::range::RangeBounds;
use std::fmt::{Debug, Formatter};
use std::ops::{Eq, Cmp, Ordering};
use std::hash::{Hash, Hasher};
use std::reflect::Integral;

$[lang(span)]
pub struct Span<T> {
    ptr: ^T,
    len: uint,

    pub unsafe fn new(ptr: ^T, len: uint): This => Span(ptr:, len:);
    pub fn from_ptr(ptr: *T): This => unsafe Span::new(ptr, 1);
    pub fn empty(): This => Span(ptr: std::ptr::raw_dangling(), len: 0);

    impl AsSpan<T> {
        fn as_span(this): [T..] => *this;

        fn len(this): uint => this.len;
        fn is_empty(this): bool => this.len == 0;

        fn get(this, idx: uint): ?*T {
            if idx < this.len {
                unsafe this.get_unchecked(idx)
            }
        }

        unsafe fn get_unchecked(this, idx: uint): *T => unsafe &*this.ptr.add(idx);

        fn as_raw(this): ^T => this.ptr;
        fn iter(this): Iter<T> => Iter(ptr: this.ptr, end: this.ptr.add(this.len));

        fn subspan<R: RangeBounds<uint>>(this, range: R): ?This {
            let (start, end) = range_bounds(range, this.len);
            if start <= end and end <= this.len {
                unsafe Span::new(this.ptr.add(start), end - start)
            }
        }

        unsafe fn subspan_unchecked<R: RangeBounds<uint>>(this, range: R): [T..] {
            let (start, end) = range_bounds(range, this.len);
            unsafe Span::new(this.ptr.add(start), end - start)
        }

        fn first(this): ?*T => this.get(0);

        fn last(this): ?*T {
            if !this.is_empty() {
                unsafe this.get_unchecked(this.len() - 1)
            }
        }
    }

    $[inline]
    pub fn []<I: Integral>(my this, idx: I): *T {
        unsafe raw_subscript_checked(this.ptr as ^mut T, this.len, idx) as *T
    }

    $[inline(always)]
    pub fn []<R: RangeBounds<uint>>(my this, range: R): This {
        if this.subspan(range) is ?span {
            span
        } else {
            let (start, end) = range_bounds(range, this.len);
            panic("invalid range {start}..{end} in span of len {this.len}");
        }
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) {
            f.dbg_list(|=this, =f, list| this.iter().for_each(|=list, item| list.value(item)));
        }
    }
}

$[lang(span_mut)]
pub struct SpanMut<T> {
    ptr: ^mut T,
    len: uint,

    pub unsafe fn new(ptr: ^mut T, len: uint): This => SpanMut(ptr:, len:);
    pub fn from_ptr(ptr: *mut T): This => unsafe SpanMut::new(ptr, 1);
    pub fn empty(): This => SpanMut(ptr: std::ptr::raw_dangling(), len: 0);

    impl AsSpan<T> {
        fn as_span(this): [T..] => *this;
    }

    pub fn get_mut(my this, idx: uint): ?*mut T {
        if idx < this.len {
            unsafe this.get_mut_unchecked(idx)
        }
    }

    pub unsafe fn get_mut_unchecked(my this, idx: uint): *mut T => unsafe &mut *this.ptr.add(idx);

    pub fn as_raw_mut(my this): ^mut T => this.ptr;

    pub fn iter_mut(my this): IterMut<T> => IterMut(ptr: this.ptr, end: this.ptr.add(this.len));

    pub fn subspan_mut<R: RangeBounds<uint>>(my this, range: R): ?This {
        let (start, end) = range_bounds(range, this.len);
        if start <= end and end <= this.len {
            unsafe SpanMut::new(this.ptr.add(start), end - start)
        }
    }

    pub unsafe fn subspan_mut_unchecked<R: RangeBounds<uint>>(my this, range: R): This {
        let (start, end) = range_bounds(range, this.len);
        unsafe SpanMut::new(this.ptr.add(start), end - start)
    }

    pub fn fill(my this, t: T) {
        // TODO: use memset when possible
        for item in this.iter_mut() {
            *item = t;
        }
    }

    pub fn swap(my this, a: uint, b: uint) => std::mem::swap(&mut this[a], &mut this[b]);

    pub fn first_mut(my this): ?*mut T => this.get_mut(0);

    pub fn last_mut(my this): ?*mut T {
        if !this.is_empty() {
            unsafe this.get_mut_unchecked(this.len() - 1)
        }
    }

    pub fn sort_by<F: Fn(*T, *T) => Ordering>(my this, f: F) {
        fn quick_sort<T, F: Fn(*T, *T) => Ordering>(self: [mut T..], f: *F) {
            guard self.len > 1 else {
                return;
            }

            let end = self.len() - 1;
            let pivot = unsafe self.get_unchecked(end);
            mut part_idx = 0u;
            for i in 0u..end {
                if f(unsafe self.get_unchecked(i), pivot) is :Less {
                    self.swap(part_idx++, i);
                }
            }

            self.swap(part_idx, end);
            unsafe {
                quick_sort(self.subspan_mut_unchecked(0u..part_idx), f);
                quick_sort(self.subspan_mut_unchecked(part_idx + 1..), f);
            }
        }

        quick_sort(this, &f)
    }

    pub fn sort_by_key<K: Cmp<K>, F: Fn(*T) => K>(my this, f: F) {
        this.sort_by(|=f, a, b| f(a).cmp(&f(b)))
    }

    $[inline]
    pub fn []<I: Integral>(my this, idx: I): *mut T {
        unsafe &mut *raw_subscript_checked(this.ptr, this.len, idx)
    }

    $[inline]
    pub fn []=<I: Integral>(my this, idx: I, val: T) {
        unsafe {
            *raw_subscript_checked(this.ptr, this.len, idx) = val;
        }
    }

    $[inline(always)]
    pub fn []<R: RangeBounds<uint>>(my this, range: R): This {
        if this.subspan_mut(range) is ?span {
            span
        } else {
            let (start, end) = range_bounds(range, this.len);
            panic("invalid range {start}..{end} in span of len {this.len}");
        }
    }

    $[inline(always)]
    pub fn []=<R: RangeBounds<uint>>(my this, range: R, rhs: Span<T>) {
        let subspan = this[range];
        if subspan.len() != rhs.len() {
            panic("span assignment requires that both sides are the same length");
        }

        // copy_overlapping?
        unsafe std::mem::copy_no_overlap(
            dst: subspan.as_raw_mut(),
            src: rhs.as_raw(),
            num: subspan.len(),
        );
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => this.as_span().dbg(f);
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

$[inline(always)]
fn raw_subscript_checked<T, I: Integral>(ptr: ^mut T, len: uint, idx: I): ^mut T {
    if idx.try_cast::<uint>() is ?idx and (0u..len).contains(&idx) {
        ptr.add(idx)
    } else {
        panic("index {idx} is out of bounds for span of length {len}");
    }
}

$[inline(always)]
fn range_bounds<R: RangeBounds<uint>>(range: R, len: uint): (uint, uint) {
    let start = match range.begin() {
        :Inclusive(start) => start,
        :Exclusive(start) => start + 1,
        :Unbounded => 0,
    };

    let end = match range.end() {
        :Inclusive(end) => end + 1,
        :Exclusive(end) => end,
        :Unbounded => len,
    };

    (start, end)
}

pub trait AsSpan<T> {
    fn as_span(this): [T..];

    fn len(this): uint => this.as_span().len();
    fn is_empty(this): bool => this.len() == 0;

    fn get(this, idx: uint): ?*T => this.as_span().get(idx);
    fn subspan<R: RangeBounds<uint>>(this, r: R): ?[T..] => this.as_span().subspan(r);
    fn as_raw(this): ^T => this.as_span().as_raw();
    fn iter(this): Iter<T> => this.as_span().iter();
    fn first(this): ?*T => this.as_span().first();
    fn last(this): ?*T => this.as_span().last();

    unsafe fn get_unchecked(this, idx: uint): *T => unsafe this.as_span().get_unchecked(idx);
    unsafe fn subspan_unchecked<R: RangeBounds<uint>>(this, r: R): [T..] => unsafe this.as_span().subspan_unchecked(r);
}

pub trait AsSpanMut<T>: AsSpan<T> {
    fn as_span_mut(mut this): [mut T..];

    fn get_mut(mut this, idx: uint): ?*mut T => this.as_span_mut().get_mut(idx);
    fn as_raw_mut(mut this): ^mut T => this.as_span_mut().as_raw_mut();
    fn iter_mut(mut this): IterMut<T> => this.as_span_mut().iter_mut();
    fn first_mut(mut this): ?*mut T => this.as_span_mut().first_mut();
    fn last_mut(mut this): ?*mut T => this.as_span_mut().last_mut();

    fn fill(mut this, t: T) => this.as_span_mut().fill(t);
    fn swap(mut this, a: uint, b: uint) => this.as_span_mut().swap(a, b);

    unsafe fn get_mut_unchecked(mut this, idx: uint): ?*mut T => unsafe this.as_span_mut().get_mut_unchecked(idx);
    unsafe fn subspan_mut_unchecked<R: RangeBounds<uint>>(mut this, r: R): [mut T..] => unsafe this.as_span_mut().subspan_mut_unchecked(r);

    fn sort_by<F: Fn(*T, *T) => Ordering>(mut this, f: F) => this.as_span_mut().sort_by(f);
    fn sort_by_key<K: Cmp<K>, F: Fn(*T) => K>(mut this, f: F) => this.as_span_mut().sort_by_key(f);
}

extension<S: AsSpan<T>, T> *S {
    impl AsSpan<T> {
        fn as_span(this): [T..] => (*this).as_span();
        fn len(this): uint => (*this).len();
        fn is_empty(this): bool => (*this).is_empty();
        fn get(this, idx: uint): ?*T => (*this).get(idx);
        fn subspan<R: RangeBounds<uint>>(this, r: R): ?[T..] => (*this).subspan(r);
        fn as_raw(this): ^T => (*this).as_raw();
        fn iter(this): Iter<T> => (*this).iter();
        fn first(this): ?*T => (*this).first();
        fn last(this): ?*T => (*this).last();
        unsafe fn get_unchecked(this, idx: uint): *T => unsafe (*this).get_unchecked(idx);
        unsafe fn subspan_unchecked<R: RangeBounds<uint>>(this, r: R): [T..] => unsafe (*this).subspan_unchecked(r);
    }
}

extension<S: AsSpan<T>, T> *mut S {
    impl AsSpan<T> {
        fn as_span(this): [T..] => (*this).as_span();
        fn len(this): uint => (*this).len();
        fn is_empty(this): bool => (*this).is_empty();
        fn get(this, idx: uint): ?*T => (*this).get(idx);
        fn subspan<R: RangeBounds<uint>>(this, r: R): ?[T..] => (*this).subspan(r);
        fn as_raw(this): ^T => (*this).as_raw();
        fn iter(this): Iter<T> => (*this).iter();
        fn first(this): ?*T => (*this).first();
        fn last(this): ?*T => (*this).last();
        unsafe fn get_unchecked(this, idx: uint): *T => unsafe (*this).get_unchecked(idx);
        unsafe fn subspan_unchecked<R: RangeBounds<uint>>(this, r: R): [T..] => unsafe (*this).subspan_unchecked(r);
    }
}

extension<S: AsSpanMut<T>, T> *mut S {
    impl AsSpanMut<T> {
        fn as_span_mut(mut this): [mut T..] => (*this).as_span_mut();
        fn get_mut(mut this, idx: uint): ?*mut T => (*this).get_mut(idx);
        fn as_raw_mut(mut this): ^mut T => (*this).as_raw_mut();
        fn iter_mut(mut this): IterMut<T> => (*this).iter_mut();
        fn first_mut(mut this): ?*mut T => (*this).first_mut();
        fn last_mut(mut this): ?*mut T => (*this).last_mut();
        fn fill(mut this, t: T) => (*this).fill(t);
        fn swap(mut this, a: uint, b: uint) => (*this).swap(a, b);
        unsafe fn get_mut_unchecked(mut this, idx: uint): ?*mut T => unsafe (*this).get_mut_unchecked(idx);
        unsafe fn subspan_mut_unchecked<R: RangeBounds<uint>>(mut this, r: R): [mut T..] => unsafe (*this).subspan_mut_unchecked(r);
        fn sort_by<F: Fn(*T, *T) => Ordering>(mut this, f: F) => (*this).sort_by(f);
        fn sort_by_key<K: Cmp<K>, F: Fn(*T) => K>(mut this, f: F) => (*this).sort_by_key(f);
    }
}

// TODO: make these member functions/impls when the syntax allows for it

extension<T: Eq<T>> [T..] {
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

    pub fn ==(this, rhs: *[mut T..]): bool => this == rhs.as_span();
}

extension<T: Cmp<T>> [T..] {
    pub fn <=>(this, rhs: *[T..]): Ordering {
        for (l, r) in this.iter().zip(rhs.iter()) {
            let ord = l <=> r;
            if ord != :Equal {
                return ord;
            }
        }

        this.len().cmp(&rhs.len())
    }

    pub fn <=>(this, rhs: *[mut T..]): Ordering => this <=> rhs.as_span();
}

extension<T: Hash> [T..] {
    impl Hash {
        fn hash<H: Hasher>(this, hasher: *mut H) {
            for v in this.iter() {
                v.hash(hasher);
            }
        }
    }
}

extension<T: Hash> [mut T..] {
    impl Hash {
        fn hash<H: Hasher>(this, hasher: *mut H) => this.as_span().hash(hasher);
    }
}

extension<T: Eq<T>> [mut T..] {
    pub fn ==(this, rhs: *[T..]): bool => this.as_span() == rhs;
    pub fn ==(this, rhs: *[mut T..]): bool => this.as_span() == rhs.as_span();
}

extension<T: Cmp<T>> [mut T..] {
    pub fn <=>(this, rhs: *[T..]): Ordering => this.as_span() <=> rhs;
    pub fn <=>(this, rhs: *[mut T..]): Ordering => this.as_span() <=> rhs.as_span();

    pub fn sort(my this) => this.sort_by(T::cmp);
}

extension<T: Cmp<T>, S: AsSpanMut<T>> S {
    pub fn sort(mut this) => this.as_span_mut().sort_by(T::cmp);
}

unittest "slice with ranges" {
    mut x = [1, 2, 3, 4];
    assert_eq(x[1u..2u], [2][..]);
    assert_eq(x[1u..=2u], [2, 3][..]);
    assert_eq(x[..2u], [1, 2][..]);
    assert_eq(x[..=2u], [1, 2, 3][..]);
    assert_eq(x[..], [1, 2, 3, 4][..]);
}

unittest "span comparisons of equal length" {
    let a = [1, 2, 3].as_span();
    let b = [1, 2, 3].as_span();

    assert_eq(a == b, true);
    assert_eq(a < b, false);
    assert_eq(a > b, false);

    let a = [1, 2, 3].as_span();
    let b = [1, 4, 3].as_span();

    assert_eq(a == b, false);
    assert_eq(a < b, true);
    assert_eq(a > b, false);

    let a = [1, 4, 3].as_span();
    let b = [1, 2, 3].as_span();

    assert_eq(a == b, false);
    assert_eq(a < b, false);
    assert_eq(a > b, true);
}

unittest "span comparisons of different lengths" {
    let a = [1, 2, 3].as_span();
    let b = [1, 2, 3, 4].as_span();

    assert_eq(a == b, false);
    assert_eq(a < b, true);
    assert_eq(a > b, false);

    let a = [1, 2, 3, 4].as_span();
    let b = [1, 2, 3].as_span();

    assert_eq(a == b, false);
    assert_eq(a < b, false);
    assert_eq(a > b, true);
}
