#(lang(iter))
pub trait Iterator<T> {
    fn next(mut this): ?T;

    fn enumerate(this): Enumerate<T, This> {
        Enumerate::new(*this)
    }

    fn take(this, count: uint): Take<T, This> {
        Take::new(*this, count)
    }
}

pub struct Enumerate<T, I: Iterator<T>> {
    idx: uint,
    iter: I,

    pub fn new(iter: I): This {
        Enumerate(iter:, idx: 0)
    }

    impl Iterator<(uint, T)> {
        fn next(mut this): ?(uint, T) {
            if (this.iter.next() is ?val) {
                (this.idx++, val)
            }
        }
    }
}

pub struct Take<T, I: Iterator<T>> {
    count: uint,
    iter: I,

    pub fn new(iter: I, count: uint): This {
        Take(iter:, count:)
    }

    impl Iterator<T> {
        fn next(mut this): ?T {
            if this.count == 0 {
                return null;
            }

            this.count--;
            this.iter.next()
        }
    }
}
