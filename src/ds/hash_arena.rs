use std::{
    cell::{Cell, UnsafeCell},
    hash::{BuildHasher, BuildHasherDefault, Hash},
};

use hashbrown::{HashTable, hash_table::Entry};
use zwohash::ZwoHasher;

pub struct HashArena<T, const BLOCK_SZ: usize = 4096> {
    indices: UnsafeCell<HashTable<usize>>,
    storage: UnsafeCell<Vec<Vec<T>>>,
    len: Cell<usize>,
}

impl<T: Hash + Eq, const BLOCK_SZ: usize> HashArena<T, BLOCK_SZ> {
    pub fn new() -> Self {
        Self {
            indices: UnsafeCell::new(Default::default()),
            storage: UnsafeCell::new(vec![Vec::with_capacity(BLOCK_SZ)]),
            len: Cell::new(0),
        }
    }

    pub fn insert(&self, item: T) -> usize {
        let hasher = BuildHasherDefault::<ZwoHasher>::new();
        let storage = unsafe { &mut *self.storage.get() };

        let eq = |&k: &usize| &item == unsafe { Self::get_by_id_unchecked(storage, k) };
        let hash = |&k: &usize| hasher.hash_one(unsafe { Self::get_by_id_unchecked(storage, k) });

        let table = unsafe { &mut *self.indices.get() };
        match table.entry(hasher.hash_one(&item), eq, hash) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let last = storage.len() - 1;
                let last = unsafe { storage.get_unchecked_mut(last) };
                if last.len() < BLOCK_SZ {
                    last.push(item);
                } else {
                    let mut block = Vec::with_capacity(BLOCK_SZ);
                    block.push(item);
                    storage.push(block);
                }

                let key = self.len.get();
                entry.insert(key);
                self.len.update(|v| v + 1);
                key
            }
        }
    }

    pub fn get(&self, key: usize) -> &T {
        if key >= self.len() {
            panic!("Key {key} is out of bounds");
        }

        // Safety: The blocks are never resized, so any &T we hand out is valid for the lifetime of
        // self
        unsafe { Self::get_by_id_unchecked(&*self.storage.get(), key) }
    }

    pub fn len(&self) -> usize {
        self.len.get()
    }

    unsafe fn get_by_id_unchecked(storage: &[Vec<T>], key: usize) -> &T {
        unsafe { storage.get_unchecked(key / BLOCK_SZ).get_unchecked(key % BLOCK_SZ) }
    }
}

impl<const N: usize, const BZ: usize, T: Hash + Eq> From<[T; N]> for HashArena<T, BZ> {
    fn from(value: [T; N]) -> Self {
        let v = HashArena::new();
        for item in value {
            v.insert(item);
        }
        v
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {
        let v = HashArena::<i32, 4>::new();
        assert_eq!(v.insert(10), 0);
        assert_eq!(v.insert(15), 1);
        assert_eq!(v.insert(20), 2);
        assert_eq!(v.insert(25), 3);
        assert_eq!(v.insert(30), 4);

        assert_eq!(v.insert(10), 0);

        assert_eq!(v.get(0), &10);
        assert_eq!(v.get(1), &15);
        assert_eq!(v.get(2), &20);
        assert_eq!(v.get(3), &25);
        assert_eq!(v.get(4), &30);
    }
}
