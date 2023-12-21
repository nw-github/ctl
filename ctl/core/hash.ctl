pub trait Hasher {
    fn hash(mut this, data: [u8..]);
    fn finish(this): u64;
}

pub trait Hash {
    fn hash<H: Hasher>(this, hasher: *mut H);
}
