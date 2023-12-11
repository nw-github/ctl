use core::hash::Hash;
use core::ops::Eq;
use std::vec::Vec;

struct SomeBucket<K, V> {
    key: K,
    val: V,
}

union Bucket<K, V> {
    Some(SomeBucket<K, V>),
    None,
    Tombstone,

    pub fn unwrap(this) SomeBucket<K, V> {
        match *this {
            Bucket::Some(e) => e,
            _ => {
                panic("Bucket::unwrap(): value is not Bucket::Some!");
            },
        }
    }
}

[lang(map)]
pub struct Map<K: Hash + Eq<K>, V /*, H: Hasher + Default */> {
    buckets: [Bucket<K, V>],
    len:     usize,

    pub fn new<_K: Hash + Eq<_K>, _V>() Map<_K, _V> {
        Map::<_K, _V>(
            buckets: Vec::with_capacity(20),
            len: 0,
        )
    }

    pub fn with_capacity<_K: Hash + Eq<_K>, _V>(cap: usize) Map<_K, _V> {
        mut self: [_K: _V] = Map::new();
        self.adjust_cap(cap);
        self
    }

    pub fn get(this, key: *K) ?*V {
        match this.buckets.get(this.entry_pos(key))! {
            Bucket::Some(entry) => &entry.val,
            _ => null,
        }
    }

    pub fn get_mut(mut this, key: *K) ?*mut V {
        match this.buckets.get_mut(this.entry_pos(key))! {
            Bucket::Some(entry) => &mut entry.val,
            _ => null,
        }
    }

    pub fn insert(mut this, key: K, val: V) ?V {
        if this.len + 1 > this.buckets.len() * 3 / 4 {
            this.adjust_cap(this.buckets.len() * 2);
        }

        match this.buckets.get_mut(this.entry_pos(&key))! {
            Bucket::Some(entry) => core::mem::replace(&mut entry.val, val),
            entry => {
                if !(entry is Bucket::Tombstone) {
                    this.len++;
                }

                *entry = Bucket::Some(SomeBucket(key:, val:));
                yield null;
            }
        }
    }

    pub fn remove(mut this, key: *K) ?V {
        match this.buckets.get_mut(this.entry_pos(key))! {
            Bucket::None => null,
            Bucket::Tombstone => null,
            entry => {
                this.len--;
                yield core::mem::replace(entry, Bucket::Tombstone()).unwrap().val;
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

    pub fn contains(this, key: *K) bool {
        this.buckets.get(this.entry_pos(key))! is Bucket::Some(_)
    }

    pub fn len(this) usize {
        this.len
    }

    fn entry_pos(this, key: *K) usize {
        mut idx = {
            mut h = Fnv1a::new();
            key.hash(&mut h);
            yield h.finish() as! usize;
        } % this.buckets.len();

        mut tombstone: ?usize = null;
        loop {
            match this.buckets.get(idx)! {
                Bucket::Some(entry) => if key.eq(&entry.key) {
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

    fn adjust_cap(mut this, cap: usize) {
        let cap = if cap > 8 { yield cap; } else { yield 8; };
        if cap <= this.buckets.len() {
            return;
        }

        let old = this.buckets.len();
        this.buckets.reserve(cap - old);
        while this.buckets.len() < cap {
            this.buckets.push(Bucket::None());
        }

        this.len = 0;

        mut i = 0usize;
        while i < old {
            match this.buckets.get(i)! {
                Bucket::Some(entry) => {
                    this.len++;

                    let j = this.entry_pos(&entry.key);
                    if i != j {
                        core::mem::swap(
                            this.buckets.get_mut(i)!,
                            this.buckets.get_mut(j)!,
                        );
                    }
                }
            }

            i++;
        }
    }
}

struct Fnv1a {
    val: u64 = 0,

    pub fn new() Fnv1a {
        Fnv1a()
    }

    impl core::hash::Hasher {
        fn hash(mut this, data: [u8..]) {
            for byte in data.iter() {
                this.val *= 0x100000001b3;
                this.val += (this.val << 1) + (this.val << 4) + (this.val << 5) +
                            (this.val << 7) + (this.val << 8) + (this.val << 40);
                this.val ^= *byte as u64;
            }
        }

        fn finish(this) u64 {
            this.val
        }
    }
}
