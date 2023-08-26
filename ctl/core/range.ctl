pub trait RangeBounds<T> {
    fn begin(this) Bound<T>;
    fn end(this) Bound<T>;
}

pub union Bound<T> {
    Inclusive(T),
    Exclusive(T),
    Unbounded,
}

// ..bar
[lang(range_to)]
pub struct RangeTo<T> {
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this) Bound<T> {
            return Bound::Unbounded();
        }

        fn end(this) Bound<T> {
            return Bound::Exclusive(this.end);
        }
    }
}

// ..=bar
[lang(range_to_inclusive)]
pub struct RangeToInclusive<T> {
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this) Bound<T> {
            return Bound::Unbounded();
        }

        fn end(this) Bound<T> {
            return Bound::Inclusive(this.end);
        }
    }
}

// foo..
[lang(range_from)]
pub struct RangeFrom<T> {
    pub start: T,

    impl RangeBounds<T> {
        fn begin(this) Bound<T> {
            return Bound::Inclusive(this.start);
        }

        fn end(this) Bound<T> {
            return Bound::Unbounded();
        }
    }
}

// foo..bar
[lang(range)]
pub struct Range<T> {
    pub start: T,
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this) Bound<T> {
            return Bound::Inclusive(this.start);
        }

        fn end(this) Bound<T> {
            return Bound::Exclusive(this.end);
        }
    }
}

// foo..=bar
[lang(range_inclusive)]
pub struct RangeInclusive<T> {
    pub start: T,
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this) Bound<T> {
            return Bound::Inclusive(this.start);
        }

        fn end(this) Bound<T> {
            return Bound::Inclusive(this.end);
        }
    }
}

// ..
[lang(range_full)]
pub struct RangeFull<T> {
    impl RangeBounds<T> {
        fn begin(this) Bound<T> {
            return Bound::Unbounded();
        }

        fn end(this) Bound<T> {
            return Bound::Unbounded();
        }
    }
}
