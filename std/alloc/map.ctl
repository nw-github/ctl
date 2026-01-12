use std::hash::*;
use std::ops::Eq;

union Bucket<K, V> {
    Some(K, V),
    None,
    Tombstone,
}

$[feature(alloc)]
$[lang(map)]
pub struct Map<K: Hash + Eq<K>, V /*, H: Hasher + Default */> {
    buckets: [Bucket<K, V>],
    len:     uint,

    pub fn new(): Map<K, V> => Map(buckets: Vec::new(), len: 0);

    pub fn with_capacity(cap: uint): Map<K, V> {
        mut self: Map<K, V> = Map::new();
        self.adjust_cap(cap);
        self
    }

    pub fn deinit(mut this) {
        this.buckets.deinit();
        this.len = 0;
    }

    pub fn get(this, key: *K): ?*V {
        this.buckets.get(this.entry_pos(key)) is ?:Some(_, val) then val
    }

    pub fn get_mut(mut this, key: *K): ?*mut V {
        this.buckets.get_mut(this.entry_pos(key)) is ?:Some(_, val) then val
    }

    /// Returns the old value associated with `key`, or null if it doesn't exist
    pub fn insert(mut this, key: K, val: V): ?V {
        if this.len + 1 > this.capacity() * 3 / 4 {
            this.adjust_cap(this.capacity() * 2);
        }

        match this.buckets.get_mut(this.entry_pos(&key))? {
            :Some(_, prev) => std::mem::replace(prev, val) as V,
            entry => {
                if entry is :None {
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
            this.buckets[..].fill(:None);
            this.len = 0;
        }
    }

    pub fn contains(this, key: *K): bool => this.get(key).is_some();

    // TODO: len() currenty includes tombstones, so this isn't a useful number to return
    pub fn len(this): uint => this.len;
    pub fn is_empty(this): bool => this.len == 0;
    pub fn capacity(this): uint => this.buckets.len();

    pub fn iter(this): Iter<K, V> => Iter(buckets: this.buckets[..]);
    pub fn iter_mut(mut this): IterMut<K, V> => IterMut(buckets: this.buckets[..]);
    pub fn keys(this): Keys<K, V> => Keys(iter: this.iter());
    pub fn values(this): Values<K, V> => Values(iter: this.iter());
    pub fn values_mut(mut this): ValuesMut<K, V> => ValuesMut(iter: this.iter_mut());

    fn entry_pos(this, k: *K): uint {
        guard !this.buckets.is_empty() else {
            return 0;
        }

        mut idx = {
            mut h = Fnv1a();
            k.hash(&mut h);
            h.finish() as! uint
        } % this.capacity();

        mut tombstone: ?uint = null;
        loop {
            match &this.buckets[idx] {
                :Some(key, _) => if k == key { break idx; },
                :None => break tombstone ?? idx,
                :Tombstone => { tombstone.get_or_insert(idx); }
            }

            idx = (idx + 1) % this.capacity();
        }
    }

    fn adjust_cap(mut this, cap: uint) {
        let cap = cap.max(8);
        guard cap > this.capacity() else {
            return;
        }

        this.len = 0;
        let old = std::mem::replace(&mut this.buckets, @[Bucket::<K, V>::None; cap]);
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
            panic("attempt to retrieve invalid key '{key:?}'");
        }
    }

    pub fn [](mut this, key: *K): *mut V {
        if this.get_mut(key) is ?item {
            item
        } else {
            panic("attempt to retrieve invalid key '{key:?}'");
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

    impl std::fmt::Debug {
        fn dbg(this, f: *mut std::fmt::Formatter) {
            f.write_str("[");
            for (i, (key, value)) in this.iter().enumerate() {
                if i > 0 {
                    f.write_str(", ");
                }
                write(f, "{key:?}: {value:?}");
            }
            f.write_str("]");
        }
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
        fn next(mut this): ?*K => this.iter.next() is ?item then item.0;
    }
}

pub struct Values<K, V> {
    iter: Iter<K, V>,

    impl Iterator<*V> {
        fn next(mut this): ?*V => this.iter.next() is ?item then item.1;
    }
}

pub struct ValuesMut<K, V> {
    iter: IterMut<K, V>,

    impl Iterator<*mut V> {
        fn next(mut this): ?*mut V => this.iter.next() is ?item then item.1;
    }
}

const FNV1A_BASIS: u64 = 0xcbf29ce484222325;
const FNV1A_PRIME: u64 = 0x00000100000001b3;

struct Fnv1a {
    val: u64 = FNV1A_BASIS,

    impl Hasher {
        fn hash(mut this, data: [u8..]) {
            for byte in data.iter() {
                this.val = (this.val ^ *byte as u64).wrapping_mul(FNV1A_PRIME);
            }
        }

        fn finish(this): u64 => this.val;
    }
}

unittest "init" {
    let map = [10: "a", 20: "b", 30: "c"];
    assert_eq(map[&10], "a");
    assert_eq(map[&20], "b");
    assert_eq(map[&30], "c");
}

unittest "insert and remove" {
    mut map: [str: str] = [:];
    assert_eq(map.len(), 0);

    assert_eq(map.insert("e", "eternal"), null);
    assert_eq(map.insert("d", "dragonfruit"), null);
    assert_eq(map.insert("c", "cat"), null);
    assert_eq(map.insert("b", "banana"), null);
    assert_eq(map.insert("a", "apple"), null);

    assert_eq(map.len(), 5);

    map.clear();
    assert_eq(map.len(), 0);

    assert_eq(map.insert("e", "eternal"), null);
    assert_eq(map.insert("d", "dragonfruit"), null);
    assert_eq(map.insert("c", "cat"), null);
    assert_eq(map.insert("b", "banana"), null);
    assert_eq(map.insert("a", "apple"), null);
    assert_eq(map.len(), 5);

    assert_eq(map.remove(&"c"), "cat");
    assert_eq(map.len(), 4);

    assert_eq(map.insert("a", "asthma"), "apple");
    assert_eq(map.len(), 4);
}
