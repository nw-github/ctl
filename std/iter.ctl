$[lang(iter)]
pub trait Iterator<T> {
    fn next(mut this): ?T;

    fn enumerate(my this): Enumerate<T, This> => Enumerate::new(this);
    fn take(my this, count: uint): Take<T, This> => Take::new(this, count);
    fn skip(my this, count: uint): Skip<T, This> => Skip::new(this, count);
    fn zip<U, I: Iterator<U>>(my this, rhs: I): Zip<T, U, This, I> => Zip::new(this, rhs);
    fn chain<I: Iterator<T>>(my this, rhs: I): Chain<T, This, I> => Chain::new(this, rhs);
    fn peekable(my this): Peekable<T, This> => Peekable::new(this);
    fn map<U, F: Fn(T) => U>(my this, f: F): Map<T, U, This, F> => Map::new(this, f);
    fn flat_map<U, F: Fn(T) => ?U>(my this, f: F): FlatMap<T, U, This, F> => FlatMap::new(this, f);
    fn filter<F: Fn(*T) => bool>(my this, f: F): Filter<T, This, F> => Filter::new(this, f);
    fn collect<C: FromIter<T>>(my this): C => C::from_iter(this);

    fn count(my mut this): uint {
        mut count = 0u;
        while this.next().is_some() { count++; }
        count
    }

    fn nth(mut this, n: uint): ?T {
        for _ in 0u..n {
            this.next();
        }

        this.next()
    }

    fn advance_by(mut this, mut n: uint): ?uint {
        while this.next().is_some() and n != 0 { n--; }

        n == 0 then null else n
    }

    fn any<F: Fn(T) => bool>(mut this, f: F): bool => this.position(f).is_some();

    fn all<F: Fn(T) => bool>(mut this, f: F): bool {
        while this.next() is ?next {
            if !f(next) {
                return false;
            }
        }
        true
    }

    fn position<F: Fn(T) => bool>(mut this, f: F): ?uint {
        mut idx = 0u;
        while this.next() is ?next {
            if f(next) {
                break idx;
            }
            idx++;
        }
    }

    fn fold<Acc, F: Fn(Acc, T) => Acc>(my mut this, mut acc: Acc, f: F): Acc {
        while this.next() is ?next {
            acc = f(acc, next);
        }
        acc
    }

    fn reduce<F: Fn(T, T) => T>(my mut this, f: F): ?T => ?this.fold(this.next()?, f);
}

pub trait FromIter<T> {
    fn from_iter<I: Iterator<T>>(iter: I): This;
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

    $[inline(always)]
    fn prime(mut this) {
        if this.item is null {
            this.item = this.iter.next();
        }
    }
}

pub struct Skip<T, I: Iterator<T>> {
    count: uint,
    iter: I,

    pub fn new(iter: I, count: uint): This {
        Skip(iter:, count:)
    }

    impl Iterator<T> {
        fn next(mut this): ?T {
            while this.count != 0 and this.iter.next() is ?_ {
                this.count -= 1;
            }

            this.iter.next()
        }
    }
}

pub struct Once<T> {
    val: ?T,

    pub fn new(val: T): This => Once(val:);

    impl Iterator<T> {
        fn next(mut this): ?T => this.val.take();
    }
}

pub struct Repeat<T> {
    val: T,

    pub fn new(val: T): This => Repeat(val:);

    impl Iterator<T> {
        fn next(mut this): ?T { this.val }
    }
}

pub struct Map<T, U, I: Iterator<T>, F: Fn(T) => U> {
    f: F,
    iter: I,

    pub fn new(iter: I, f: F): This => This(iter:, f:);

    impl Iterator<U> {
        fn next(mut this): ?U {
            if this.iter.next() is ?next {
                (this.f)(next)
            }
        }
    }
}

pub struct FlatMap<T, U, I: Iterator<T>, F: Fn(T) => ?U> {
    f: F,
    iter: I,

    pub fn new(iter: I, f: F): This => This(iter:, f:);

    impl Iterator<U> {
        fn next(mut this): ?U {
            if this.iter.next() is ?next and (this.f)(next) is ?next {
                next
            }
        }
    }
}

pub struct Filter<T, I: Iterator<T>, F: Fn(*T) => bool> {
    f: F,
    iter: I,

    pub fn new(iter: I, f: F): This => This(iter:, f:);

    impl Iterator<T> {
        fn next(mut this): ?T {
            while this.iter.next() is ?next {
                if (this.f)(&next) {
                    break next;
                }
            }
        }
    }
}

pub fn once<T>(val: T): Once<T> => Once::new(val);
pub fn repeat<T>(val: T): Repeat<T> => Repeat::new(val);

mod tests {
    unittest "zip even" {
        let a = [1, 2, 3];
        let b = [3, 2, 1];
        mut iter = a.iter().zip(b.iter());

        assert_eq("{iter.next():?}".to_str(), "Some((1, 3))");
        assert_eq("{iter.next():?}".to_str(), "Some((2, 2))");
        assert_eq("{iter.next():?}".to_str(), "Some((3, 1))");
        assert(iter.next().is_null());
    }
}
