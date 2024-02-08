use core::hash::Hash;
use core::ops::Eq;

#(lang(set))
pub struct Set<T: Hash + Eq<T>> {
    inner: [T: void],

    pub fn new(): Set<T> {
        Set(inner: [:])
    }

    pub fn with_capacity(cap: usize): Set<T> {
        Set(inner: std::map::Map::with_capacity(cap:))
    }

    pub fn from_iter<I: core::iter::Iterator<T>>(iter: I): Set<T> {
        mut self: {T} = #[]; // TODO: size hint
        for i in iter {
            self.insert(i);
        }
        self
    }

    pub fn insert(mut this, key: T): bool {
        this.inner.insert(key, {}) is ?_
    }

    pub fn remove(mut this, key: *T): bool {
        this.inner.remove(key) is ?_
    }

    pub fn clear(mut this) {
        this.inner.clear()
    }

    pub fn contains(this, key: *T): bool {
        this.inner.get(key) is ?_
    }

    pub fn len(this): usize {
        this.inner.len()
    }
}
