use std::hash::*;
use std::ops::Eq;

union Bucket<K, V> {
    Some(K, V),
    None,
    Tombstone,
}

#(lang(map))
pub struct Map<K: Hash + Eq<K>, V /*, H: Hasher + Default */> {
    buckets: [mut Bucket<K, V>..],
    len:     uint,

    pub fn new(): Map<K, V> {
        Map(buckets: std::span::SpanMut::empty(), len: 0)
    }

    pub fn with_capacity(cap: uint): Map<K, V> {
        mut self: Map<K, V> = Map::new();
        self.adjust_cap(cap);
        self
    }

    pub fn get(this, key: *K): ?*V {
        if this.buckets.get(this.entry_pos(key)) is ?:Some(_, val) {
            val
        }
    }

    pub fn get_mut(mut this, key: *K): ?*mut V {
        if this.buckets.get_mut(this.entry_pos(key)) is ?:Some(_, val) {
            val
        }
    }

    pub fn insert(mut this, key: K, val: V): ?V {
        if this.len + 1 > this.capacity() * 3 / 4 {
            this.adjust_cap(this.capacity() * 2);
        }

        match this.buckets.get_mut(this.entry_pos(&key))? {
            :Some(_, prev) => std::mem::replace(prev, val) as V,
            entry => {
                if !(entry is :Tombstone) {
                    this.len++;
                }

                *entry = :Some(key, val);
                null
            }
        }
    }

    pub fn remove(mut this, key: *K): ?V {
        match this.buckets.get_mut(this.entry_pos(key))? {
            :Tombstone | :None => null,
            entry => {
                this.len--;
                if std::mem::replace(entry, :Tombstone) is :Some(_, v) {
                    v
                } else {
                    unreachable();
                }
            }
        }
    }

    pub fn clear(mut this) {
        if this.len > 0 {
            this.buckets.fill(:None);
            this.len = 0;
        }
    }

    pub fn contains(this, key: *K): bool {
        this.get(key).is_some()
    }

    pub fn len(this): uint {
        this.len
    }

    pub fn capacity(this): uint {
        this.buckets.len()
    }

    pub fn iter(this): Iter<K, V> {
        Iter(buckets: this.buckets)
    }

    pub fn iter_mut(mut this): IterMut<K, V> {
        IterMut(buckets: this.buckets)
    }

    pub fn keys(this): Keys<K, V> {
        Keys(iter: this.iter())
    }

    pub fn values(this): Values<K, V> {
        Values(iter: this.iter())
    }

    pub fn values_mut(mut this): ValuesMut<K, V> {
        ValuesMut(iter: this.iter_mut())
    }

    fn entry_pos(this, k: *K): uint {
        guard !this.buckets.is_empty() else {
            return 0;
        }

        mut idx = {
            mut h = Fnv1a::new();
            k.hash(&mut h);
            h.finish() as! uint
        } % this.capacity();

        mut tombstone: ?uint = null;
        loop {
            match this.buckets.get(idx)! {
                :Some(key, _) => if k.eq(key) { break idx; }
                :None => break tombstone ?? idx,
                :Tombstone => {
                    tombstone.get_or_insert(idx);
                }
            }

            idx = (idx + 1) % this.capacity();
        }
    }

    fn adjust_cap(mut this, cap: uint) {
        let cap = if cap > 8 { cap } else { 8 };
        guard cap > this.capacity() else {
            return;
        }

        this.len = 0;
        let old = std::mem::replace(&mut this.buckets, @[Bucket::<K, V>::None; cap][..]);
        for val in old.iter() {
            if val is :Some(key, _) {
                this.len++;
                this.buckets[this.entry_pos(key)] = *val;
            }
        }
    }

    pub fn [](this, key: *K): *V {
        if this.get(key) is ?item {
            item
        } else {
            panic("Map::[]: attempt to retrieve invalid key");
        }
    }

    pub fn [](mut this, key: *K): *mut V {
        if this.get_mut(key) is ?item {
            item
        } else {
            panic("Map::[]: attempt to retrieve invalid key");
        }
    }

    pub fn [](mut this, key: K, kw fallback: V): *mut V {
        // TODO: do this more efficiently
        if this.get_mut(&key) is ?item {
            item
        } else {
            if this.len + 1 > this.capacity() * 3 / 4 {
                this.adjust_cap(this.capacity() * 2);
            }

            match this.buckets.get_mut(this.entry_pos(&key))! {
                :Some(_, prev) => {
                    *prev = fallback;
                    prev
                },
                entry => {
                    if !(entry is :Tombstone) {
                        this.len++;
                    }

                    *entry = :Some(key, fallback);
                    if entry is :Some(_, val) {
                        val
                    } else {
                        unreachable();
                    }
                }
            }
        }
    }

    pub fn []=(mut this, key: K, val: V) {
        this.insert(key, val);
    }
}

pub struct Iter<K, V> {
    buckets: [Bucket<K, V>..],

    impl Iterator<(*K, *V)> {
        fn next(mut this): ?(*K, *V) {
            for (i, bucket) in this.buckets.iter().enumerate() {
                if bucket is :Some(key, val) {
                    this.buckets = this.buckets[i + 1..];
                    break (key, val);
                }
            }
        }
    }
}

pub struct IterMut<K, V> {
    buckets: [mut Bucket<K, V>..],

    impl Iterator<(*K, *mut V)> {
        fn next(mut this): ?(*K, *mut V) {
            for (i, bucket) in this.buckets.iter_mut().enumerate() {
                if bucket is :Some(key, val) {
                    this.buckets = this.buckets[i + 1..];
                    break (key, val);
                }
            }
        }
    }
}

pub struct Keys<K, V> {
    iter: Iter<K, V>,

    impl Iterator<*K> {
        fn next(mut this): ?*K {
            if this.iter.next() is ?item {
                item.0
            }
        }
    }
}

pub struct Values<K, V> {
    iter: Iter<K, V>,

    impl Iterator<*V> {
        fn next(mut this): ?*V {
            if this.iter.next() is ?item {
                item.1
            }
        }
    }
}

pub struct ValuesMut<K, V> {
    iter: IterMut<K, V>,

    impl Iterator<*mut V> {
        fn next(mut this): ?*mut V {
            if this.iter.next() is ?item {
                item.1
            }
        }
    }
}

struct Fnv1a {
    val: u64 = 0,

    pub fn new(): Fnv1a {
        Fnv1a()
    }

    impl Hasher {
        fn hash(mut this, data: [u8..]) {
            for byte in data.iter() {
                this.val *= 0x100000001b3;
                this.val += (this.val << 1) + (this.val << 4) + (this.val << 5) +
                            (this.val << 7) + (this.val << 8) + (this.val << 40);
                this.val ^= *byte as u64;
            }
        }

        fn finish(this): u64 {
            this.val
        }
    }
}
