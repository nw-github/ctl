use std::intrin;
use std::range::RangeBounds;

pub use intrin::size_of;
pub use intrin::align_of;

pub fn size_of_val<T>(_: *T): uint => size_of::<T>();
pub fn align_of_val<T>(_: *T): uint => align_of::<T>();

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`.
pub unsafe fn copy_no_overlap<T>(kw dst: ^mut T, kw src: ^T, kw num: uint) {
    unsafe intrin::memcpy(dst.cast(), src.cast(), num * size_of::<T>());
}

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`. Behaves as if
/// `src` is first copied to a temporary buffer, then copied to dst.
pub unsafe fn copy<T>(kw dst: ^mut T, kw src: ^T, kw num: uint) {
    unsafe intrin::memmove(dst.cast(), src.cast(), num * size_of::<T>());
}

pub unsafe fn compare<T>(lhs: ^T, rhs: ^T, num: uint): bool {
    unsafe intrin::memcmp(lhs.cast(), rhs.cast(), num * size_of::<T>()) == 0
}

pub unsafe fn zeroed<T>(): T {
    unsafe Uninit::<T>::assume_init_by(|out| unsafe intrin::memset(out.cast(), 0, size_of::<T>())).0
}

pub fn swap<T>(lhs: *mut T, rhs: *mut T) => unsafe (lhs as ^mut T).swap(rhs);
pub fn replace<T>(ptr: *mut T, val: T): T => unsafe (ptr as ^mut T).replace(val);

pub unsafe fn bit_cast<In, Out>(from: In): Out {
    unsafe union Transmuter<T, U> {
        from: T,
        to: U,
    }
    debug_assert(size_of::<In>() == size_of::<Out>());
    unsafe Transmuter::<In, Out>(from:).to
}

$[lang(mutable), layout(transparent)]
pub struct Mutable<T> {
    value: T,

    pub fn new(value: T): This => Mutable(value:);

    // Safe to cast away const as the language ensures variables of type Mutable<T> and types that
    // contain it are never generated as `const`
    pub fn get(this): *mut T => unsafe &raw this.value as ^mut T as *mut T;

    pub fn into_inner(my this): T => this.value;
}

pub struct Layout {
    size:  uint,
    align: uint,

    pub fn of<T>(): This => This(size: size_of::<T>(), align: align_of::<T>());
    pub fn of_val<T>(_: *T): This => This::of::<T>();

    pub unsafe fn new_unchecked(kw size: uint, kw align: uint): ?This => This(size:, align:);

    pub fn new(kw size: uint, kw align: uint): ?This {
        if align != 0 and align.is_power_of_two() {
            This(size:, align:)
        }
    }

    pub fn size(this): uint => this.size;
    pub fn align(this): uint => this.align;
    pub fn array(this, n: uint): ?This => This(size: this.size.checked_mul(n)?, align: this.align);
}

// $[layout(transparent)]
unsafe union Storage<T> {
    value: T,
    uninit: void,
}

unsafe fn uninitialized<T>(): T => unsafe Storage::<T>(uninit: {}).value;

$[layout(transparent)]
pub struct Uninit<T> {
    // TODO: we would like to store 'Storage' directly but layout(transparent) doesn't support
    // ignoring zero size fields
    // inner: Storage<T>,

    // pub fn new(value: T): This => This(inner: Storage(value:));
    // pub fn uninit(): This => This(inner: Storage(uninit: {}));

    inner: T,

    pub fn new(value: T): This => This(inner: value);
    pub fn uninit(): This => This(inner: unsafe uninitialized());

    pub unsafe fn from_ptr(foo: *T): *This => unsafe &*(foo as ^T).cast();
    pub unsafe fn from_mut(foo: *mut T): *mut This => unsafe &mut *(foo as ^mut T).cast();

    pub fn from_raw(foo: ^T): ^This => (foo as ^T).cast();
    pub fn from_raw_mut(foo: ^mut T): ^mut This => (foo as ^mut T).cast();

    pub unsafe fn assume_init(my this): T => this.inner;
    pub unsafe fn assume_init_ptr(this): *T => &this.inner;
    pub unsafe fn assume_init_mut(mut this): *mut T => &mut this.inner;

    pub unsafe fn assume_init_by<R, F: Fn(^mut T) => R>(f: F): (T, R) {
        mut out = This::uninit();
        let res = f(out.as_raw_mut());
        (unsafe out.assume_init(), res)
    }

    pub fn as_raw(this): ^T => &raw this.inner;
    pub fn as_raw_mut(mut this): ^mut T => &raw mut this.inner;

    pub fn as_bytes(this): [Uninit<u8>..] {
        unsafe Span::new(this.as_raw().cast(), std::mem::size_of::<T>())
    }

    pub fn as_bytes_mut(mut this): [mut Uninit<u8>..] {
        unsafe SpanMut::new(this.as_raw_mut().cast(), std::mem::size_of::<T>())
    }

    pub unsafe fn assume_init_bytes<R: RangeBounds<uint>>(this, r: R): [u8..] {
        unsafe std::mem::bit_cast(this.as_bytes()[r])
    }

    pub unsafe fn assume_init_bytes_mut<R: RangeBounds<uint>>(mut this, r: R): [mut u8..] {
        unsafe std::mem::bit_cast(this.as_bytes_mut()[r])
    }
}
