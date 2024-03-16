#(lang(iter))
pub trait Iterator<T> {
    fn next(mut this): ?T;

    fn enumerate(this): Enumerate<T, This> {
        Enumerate::new(*this)
    }

    fn take(this, count: uint): Take<T, This> {
        Take::new(*this, count)
    }

    fn zip<U, I: Iterator<U>>(this, rhs: I): Zip<T, U, This, I> {
        Zip::new(*this, rhs)
    }

    fn collect<I: FromIter<T>>(this): I {
        I::from_iter(*this)
    }
}

pub trait FromIter<T> {
    fn from_iter<I: Iterator<T>>(t: I): This;
}

pub struct Enumerate<T, I: Iterator<T>> {
    idx: uint,
    iter: I,

    pub fn new(iter: I): This {
        Enumerate(iter:, idx: 0)
    }

    impl Iterator<(uint, T)> {
        fn next(mut this): ?(uint, T) {
            if this.iter.next() is ?val {
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

pub struct Zip<T, U, I: Iterator<T>, J: Iterator<U>> {
    iter1: I,
    iter2: J,

    pub fn new(iter1: I, iter2: J): This {
        Zip(iter1:, iter2:)
    }

    impl Iterator<(T, U)> {
        fn next(mut this): ?(T, U) {
            if (this.iter1.next(), this.iter2.next()) is (?a, ?b) {
                (a, b)
            }
        }
    }
}
