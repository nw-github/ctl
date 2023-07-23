pub trait Hasher { }

pub trait Hash {
    fn hash<H: Hasher>(this, hasher: *mut H);
}
