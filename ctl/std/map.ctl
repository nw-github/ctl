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

    pub fn unwrap(this): SomeBucket<K, V> {
        if this is Bucket::Some(e) {
            *e
        } else {
            panic("Bucket::unwrap(): value is not Bucket::Some!");
        }
    }
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
        if this.buckets.get(this.entry_pos(key)?)? is Bucket::Some(entry) {
            &entry.val
        }
    }

    pub fn get_mut(mut this, key: *K): ?*mut V {
        if this.buckets.get_mut(this.entry_pos(key)?)? is Bucket::Some(entry) {
            &mut entry.val
        }
    }

    pub fn insert(mut this, key: K, val: V): ?V {
        if this.len + 1 > this.buckets.len() * 3 / 4 {
            this.adjust_cap(this.buckets.len() * 2);
        }

        match this.buckets.get_mut(this.entry_pos(&key)?)? {
            Bucket::Some(entry) => core::mem::replace(&mut entry.val, val) as V,
            entry => {
                if !(entry is Bucket::Tombstone) {
                    this.len++;
                }

                *entry = Bucket::Some(SomeBucket(key:, val:));
                null
            }
        }
    }

    pub fn remove(mut this, key: *K): ?V {
        match this.buckets.get_mut(this.entry_pos(key)?)? {
            Bucket::None => null,
            Bucket::Tombstone => null,
            entry => {
                this.len--;
                core::mem::replace(entry, Bucket::Tombstone()).unwrap().val
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
        if this.entry_pos(key) is ?idx {
            this.buckets.get(idx)! is Bucket::Some(_)
        } else {
            false
        }
    }

    pub fn len(this): uint {
        this.len
    }

    fn entry_pos(this, key: *K): ?uint {
        if this.buckets.is_empty() {
            return null;
        }

        mut idx = {
            mut h = Fnv1a::new();
            key.hash(&mut h);
            h.finish() as! uint
        } % this.buckets.len();

        mut tombstone: ?uint = null;
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
            if this.buckets.get(i)! is Bucket::Some(entry) {
                this.len++;
                let j = this.entry_pos(&entry.key)!;
                if i != j {
                    core::mem::swap(
                        this.buckets.get_mut(i)!,
                        this.buckets.get_mut(j)!,
                    );
                }
            }

            i++;
        }
    }
}

struct Fnv1a {
    val: u64 = 0,

    pub fn new(): Fnv1a {
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

        fn finish(this): u64 {
            this.val
        }
    }
}
