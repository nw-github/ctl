use std::hash::Hash;
use std::ops::Eq;
use std::map::Map;

#(lang(set))
pub struct Set<T: Hash + Eq<T>> {
    inner: Map<T, void>,

    pub fn new(): This {
        Set(inner: Map::new())
    }

    pub fn with_capacity(cap: uint): This {
        Set(inner: Map::with_capacity(cap:))
    }

    pub fn from_iter<I: Iterator<T>>(iter: I): This {
        mut self: {T} = #[]; // TODO: size hint
        for i in iter {
            self.insert(i);
        }
        self
    }

    pub fn insert(mut this, key: T): bool {
        this.inner.insert(key, {}) is null
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

    pub fn len(this): uint {
        this.inner.len()
    }

    pub fn iter(this): Iter<T> {
        Iter(iter: this.inner.iter())
    }
}

pub struct Iter<T> {
    iter: std::map::Iter<T, void>,

    impl Iterator<*T> {
        fn next(mut this): ?*T {
            if (this.iter.next() is ?item) {
                item.0
            }
        }
    }
}
