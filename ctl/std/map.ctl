use std::hash::*;
use std::ops::Eq;
use std::vec::Vec;

union Bucket<K, V> {
    Some(K, V),
    None,
    Tombstone,
}

#(lang(map))
pub struct Map<K: Hash + Eq<K>, V /*, H: Hasher + Default */> {
    buckets: [Bucket<K, V>],
    len:     uint,

    pub fn new(): Map<K, V> {
        Map(buckets: @[], len: 0)
    }

    pub fn with_capacity(cap: uint): Map<K, V> {
        mut self: Map<K, V> = Map::new();
        self.adjust_cap(cap);
        self
    }

    pub fn get(this, key: *K): ?*V {
        if this.buckets.get(this.entry_pos(key)) is ?Bucket::Some(_, val) {
            val
        }
    }

    pub fn get_mut(mut this, key: *K): ?*mut V {
        if this.buckets.get_mut(this.entry_pos(key)) is ?Bucket::Some(_, val) {
            val
        }
    }

    pub fn insert(mut this, key: K, val: V): ?V {
        if this.len + 1 > this.buckets.len() * 3 / 4 {
            this.adjust_cap(this.buckets.len() * 2);
        }

        match this.buckets.get_mut(this.entry_pos(&key))? {
            Bucket::Some(_, prev) => std::mem::replace(prev, val) as V,
            entry => {
                if !(entry is Bucket::Tombstone) {
                    this.len++;
                }

                *entry = Bucket::Some(key, val);
                null
            }
        }
    }

    pub fn remove(mut this, key: *K): ?V {
        match this.buckets.get_mut(this.entry_pos(key))? {
            Bucket::None => null,
            Bucket::Tombstone => null,
            entry => {
                this.len--;
                if std::mem::replace(entry, Bucket::Tombstone()) is Bucket::Some(_, v) {
                    v
                } else {
                    unreachable();
                }
            }
        }
    }

    pub fn clear(mut this) {
        if this.len > 0 {
            for entry in this.buckets.iter_mut() {
                if !(entry is Bucket::None) {
                    *entry = Bucket::None();
                }
            }

            this.len = 0;
        }
    }

    pub fn contains(this, key: *K): bool {
        this.buckets.get(this.entry_pos(key)) is ?Bucket::Some(_, _)
    }

    pub fn len(this): uint {
        this.len
    }

    pub fn iter(this): Iter<K, V> {
        Iter(buckets: this.buckets.as_span())
    }

    pub fn iter_mut(this): IterMut<K, V> {
        IterMut(buckets: this.buckets.as_span_mut())
    }

    fn entry_pos(this, k: *K): uint {
        if this.buckets.is_empty() {
            return 0;
        }

        mut idx = {
            mut h = Fnv1a::new();
            k.hash(&mut h);
            h.finish() as! uint
        } % this.buckets.len();

        mut tombstone: ?uint = null;
        loop {
            match this.buckets.get(idx)! {
                Bucket::Some(key, _) => if k.eq(key) {
                    break idx;
                }
                //break tombstone ?? idx;
                Bucket::None => break tombstone.unwrap_or(idx),
                Bucket::Tombstone => {
                    tombstone.get_or_insert(idx);
                }
            }

            idx = (idx + 1) % this.buckets.len();
        }
    }

    fn adjust_cap(mut this, cap: uint) {
        let cap = if cap > 8 { cap } else { 8 };
        if cap <= this.buckets.len() {
            return;
        }

        let old = this.buckets.len();
        this.buckets.reserve(cap - old);
        while this.buckets.len() < cap {
            this.buckets.push(Bucket::None());
        }

        this.len = 0;

        mut i = 0u;
        while i < old {
            if this.buckets.get(i)! is Bucket::Some(key, _) {
                this.len++;
                let j = this.entry_pos(key);
                if i != j {
                    std::mem::swap(this.buckets.get_mut(i)!, this.buckets.get_mut(j)!);
                }
            }

            i++;
        }
    }
}

pub struct Iter<K, V> {
    buckets: [Bucket<K, V>..],

    impl Iterator<(*K, *V)> {
        fn next(mut this): ?(*K, *V) {
            mut pos = 0u;
            for bucket in this.buckets.iter() {
                pos++;
                if bucket is Bucket::Some(key, val) {
                    this.buckets = this.buckets.subspan(pos..);
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
            mut pos = 0u;
            for bucket in this.buckets.iter_mut() {
                pos++;
                if bucket is Bucket::Some(key, val) {
                    this.buckets = this.buckets.subspan(pos..);
                    break (key, val);
                }
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
