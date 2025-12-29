use std::intrin;
pub use intrin::size_of;
pub use intrin::align_of;

pub fn size_of_val<T>(_: *T): uint => size_of::<T>();
pub fn align_of_val<T>(_: *T): uint => align_of::<T>();

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`.
pub unsafe fn copy<T>(kw dst: ^mut T, kw src: ^T, kw num: uint) {
    unsafe intrin::memcpy(dst.cast(), src.cast(), num * size_of::<T>());
}

/// Copies `num` T's from `src` to `dst` without destroying the contents in `dst`. Behaves as if
/// `src` is first copied to a temporary buffer, then copied to dst.
pub unsafe fn copy_overlapping<T>(kw dst: ^mut T, kw src: ^T, kw num: uint) {
    unsafe intrin::memmove(dst.cast(), src.cast(), num * size_of::<T>());
}

pub unsafe fn compare<T>(lhs: ^T, rhs: ^T, num: uint): bool {
    unsafe intrin::memcmp(lhs.cast(), rhs.cast(), num * size_of::<T>()) == 0
}

pub unsafe fn zeroed<T>(): T {
    mut t: T;
    unsafe intrin::memset((&raw mut t).cast(), 0, size_of::<T>());
    t
}

pub fn swap<T>(lhs: *mut T, rhs: *mut T) => unsafe (lhs as ^mut T).swap(rhs);
pub fn replace<T>(ptr: *mut T, val: T): T => unsafe (ptr as ^mut T).replace(val);

pub unsafe fn transmute<In, Out>(from: In): Out {
    unsafe union Transmuter<T, U> {
        from: T,
        to: U,
    }
    debug_assert(size_of::<In>() == size_of::<Out>());
    unsafe Transmuter::<In, Out>(from:).to
}

@(lang(mutable))
pub struct Mutable<T> {
    value: T,

    pub fn new(value: T): This => Mutable(value:);

    // Safe to cast away const as the language ensures variables of type Mutable<T> and types that
    // contain it are never generated as `const`
    pub fn get(this): *mut T => unsafe &raw this.value as ^mut T as *mut T;

    pub fn into_inner(my this): T => this.value;
}
