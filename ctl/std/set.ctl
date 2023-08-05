use core::hash::Hash;
use core::ops::Eq;

[lang(set)]
pub struct Set<T: Hash + Eq<T> > {
    inner: [T: void],

    pub fn new<U: Hash + Eq<U> >() Set<U> {
        return Set(inner: [:]);
    }

    pub fn with_capacity<U: Hash + Eq<U> >(cap: usize) Set<U> {
        return Set(inner: std::map::Map::with_capacity(cap:));
    }

    pub fn insert(mut this, key: T) bool {
        return this.inner.insert(key, {}) is ?_;
    }

    pub fn remove(mut this, key: *T) bool {
        return this.inner.remove(key) is ?_;
    }

    pub fn clear(mut this) {
        return this.inner.clear();
    }

    pub fn contains(this, key: *T) bool {
        return this.inner.get(key) is ?_;
    }

    pub fn len(this) usize {
        return this.inner.len();
    }
}
