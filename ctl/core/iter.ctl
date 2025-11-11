@(lang(iter))
pub trait Iterator<T> {
    fn next(mut this): ?T;

    fn enumerate(my this): Enumerate<T, This> {
        Enumerate::new(this)
    }

    fn take(my this, count: uint): Take<T, This> {
        Take::new(this, count)
    }

    fn zip<U, I: Iterator<U>>(my this, rhs: I): Zip<T, U, This, I> {
        Zip::new(this, rhs)
    }

    fn chain<I: Iterator<T>>(my this, rhs: I): Chain<T, This, I> {
        Chain::new(this, rhs)
    }

    fn peekable(my this): Peekable<T, This> {
        Peekable::new(this)
    }

    fn count(my mut this): uint {
        mut count = 0u;
        while this.next().is_some() { count++; }
        count
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
            this.iter.next() is ?val then (this.idx++, val)
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
            (this.iter1.next(), this.iter2.next()) is (?a, ?b) then (a, b)
        }
    }
}

pub struct Chain<T, I: Iterator<T>, J: Iterator<T>> {
    iter1: I,
    iter2: J,

    pub fn new(iter1: I, iter2: J): This {
        Chain(iter1:, iter2:)
    }

    impl Iterator<T> {
        fn next(mut this): ?T {
            if this.iter1.next() is ?a {
                return a;
            }
            this.iter2.next()
        }
    }
}

pub struct Peekable<T, I: Iterator<T>> {
    iter: I,
    item: ?T,

    pub fn new(iter: I): This {
        Peekable(iter:, item: null)
    }

    pub fn peek(mut this): ?*T {
        this.prime();
        this.item.as_ptr()
    }

    impl Iterator<T> {
        fn next(mut this): ?T {
            this.prime();
            this.item.take()
        }
    }

    @(inline(always))
    fn prime(mut this) {
        if this.item is null {
            this.item = this.iter.next();
        }
    }
}

pub struct Once<T> {
    val: ?T,

    pub fn new(val: T): This {
        Once(val:)
    }

    impl Iterator<T> {
        fn next(mut this): ?T {
            this.val.take()
        }
    }
}

pub fn once<T>(val: T): Once<T> {
    Once::new(val)
}
