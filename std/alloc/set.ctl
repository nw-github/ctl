use std::hash::Hash;
use std::ops::Eq;
use std::iter::FromIter;

@(feature(alloc))
@(lang(set))
pub struct Set<T: Hash + Eq<T>> {
    inner: [T: void],

    pub fn new(): This {
        Set(inner: Map::new())
    }

    pub fn with_capacity(cap: uint): This {
        Set(inner: Map::with_capacity(cap:))
    }

    /// Returns true if the key was not previously in the set
    pub fn insert(mut this, key: T): bool {
        this.inner.insert(key, {}) is null
    }

    pub fn remove(mut this, key: *T): bool {
        this.inner.remove(key).is_some()
    }

    pub fn clear(mut this) {
        this.inner.clear()
    }

    pub fn contains(this, key: *T): bool {
        this.inner.contains(key)
    }

    pub fn len(this): uint {
        this.inner.len()
    }

    pub fn is_empty(this): bool => this.inner.len() == 0;

    pub fn iter(this): std::alloc::map::Keys<T, void> {
        this.inner.keys()
    }

    impl FromIter<T> {
        fn from_iter<I: Iterator<T>>(iter: I): This {
            mut self: #[T] = #[]; // TODO: size hint
            for i in iter {
                self.insert(i);
            }
            self
        }
    }
}

unittest "init" {
    let set = #[10, 20, 30];
    assert(set.contains(&10));
    assert(set.contains(&20));
    assert(set.contains(&30));
    assert(!set.contains(&40));
}

unittest "insert and remove" {
    mut set: #[str] = #[];
    assert_eq(set.len(), 0);

    assert(set.insert("jersey"));
    assert(set.insert("intermediate"));
    assert(set.insert("history"));
    assert(set.insert("germany"));
    assert(set.insert("farce"));

    assert_eq(set.len(), 5);

    set.clear();
    assert_eq(set.len(), 0);

    assert(set.insert("eternal"));
    assert(set.insert("dragonfruit"));
    assert(set.insert("cat"));
    assert(set.insert("banana"));
    assert(set.insert("apple"));
    assert_eq(set.len(), 5);

    assert(set.remove(&"cat"));
    assert_eq(set.len(), 4);

    assert(!set.insert("apple"));
    assert_eq(set.len(), 4);
}
