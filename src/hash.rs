use derive_more::{Deref, DerefMut};
use std::hash::Hash;
use zwohash::{HashMap as ZwoHashMap, HashSet as ZwoHashSet};

#[derive(Debug, Clone, Deref, DerefMut)]
pub struct HashMap<K, V>(ZwoHashMap<K, V>);

impl<K: PartialEq + Hash + Eq, V: PartialEq + Hash + Eq> PartialEq for HashMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<K: PartialEq + Hash + Eq, V: PartialEq + Hash + Eq> Eq for HashMap<K, V> {}

impl<K, V> HashMap<K, V> {
    pub fn new() -> Self {
        Self(ZwoHashMap::default())
    }
}

impl<K, V> IntoIterator for HashMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::collections::hash_map::IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<K: Hash + Eq, V> FromIterator<(K, V)> for HashMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(ZwoHashMap::from_iter(iter))
    }
}

impl<K: Hash + Eq, V, const N: usize> From<[(K, V); N]> for HashMap<K, V> {
    fn from(value: [(K, V); N]) -> Self {
        Self::from_iter(value)
    }
}

impl<K, V> Default for HashMap<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

#[derive(Debug, Clone, Deref, DerefMut)]
pub struct HashSet<T>(ZwoHashSet<T>);

impl<T> HashSet<T> {
    pub fn new() -> Self {
        Self(ZwoHashSet::default())
    }
}

impl<T> Default for HashSet<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> IntoIterator for HashSet<T> {
    type Item = T;
    type IntoIter = std::collections::hash_set::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T: Hash + Eq> FromIterator<T> for HashSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(ZwoHashSet::from_iter(iter))
    }
}

impl<T: Hash + Eq, const N: usize> From<[T; N]> for HashSet<T> {
    fn from(value: [T; N]) -> Self {
        Self::from_iter(value)
    }
}
