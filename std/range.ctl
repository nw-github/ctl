use std::reflect::*;

@(lang(range_bounds))
pub trait RangeBounds<T> {
    fn begin(this): Bound<T>;
    fn end(this): Bound<T>;
}

pub union Bound<T> {
    Inclusive(T),
    Exclusive(T),
    Unbounded,
}

// ..bar
@(lang(range_to))
pub struct RangeTo<T> {
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> => :Unbounded;
        fn end(this): Bound<T> => :Exclusive(this.end);
    }
}

// ..=bar
@(lang(range_to_inclusive))
pub struct RangeToInclusive<T> {
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> => :Unbounded;
        fn end(this): Bound<T> => :Inclusive(this.end);
    }
}

// foo..
@(lang(range_from))
pub struct RangeFrom<T: Integral> {
    pub start: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> => :Inclusive(this.start);
        fn end(this): Bound<T> => :Unbounded;
    }
}

// foo..bar
@(lang(range))
pub struct Range<T> {
    pub start: T,
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> => :Inclusive(this.start);
        fn end(this): Bound<T> => :Exclusive(this.end);
    }
}

// foo..=bar
@(lang(range_inclusive))
pub struct RangeInclusive<T> {
    pub start: T,
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> => :Inclusive(this.start);
        fn end(this): Bound<T> => :Inclusive(this.end);
    }
}

// ..
@(lang(range_full))
pub struct RangeFull {
    impl<T> RangeBounds<T> {
        fn begin(this): Bound<T> => :Unbounded;
        fn end(this): Bound<T> => :Unbounded;
    }
}

mod ext {
    use super::*;
    use std::ops::*;

    pub extension RangeFromExt<T: Integral> for RangeFrom<T> {
        impl Iterator<T> {
            fn next(mut this): ?T {
                this.start < this.start.wrapping_add(1.cast()) then this.start++
            }
        }
    }

    pub extension RangeExt<T: Inc + Cmp<T>> for Range<T> {
        impl Iterator<T> {
            fn next(mut this): ?T => this.start < this.end then this.start++;
        }

        pub fn contains(this, rhs: *T): bool => this.start <= rhs and this.end > rhs;
    }

    pub extension RangeInclusiveExt<T: Integral> for RangeInclusive<T> {
        impl Iterator<T> {
            fn next(mut this): ?T {
                if this.start < this.end {
                    this.start++
                } else if this.start == this.end {
                    // This wouldn't work for u0 but that doesn't compile anyway
                    if this.end.checked_sub(1.cast()) is ?end {
                        this.end = end;
                        this.start
                    } else {
                        let val = this.start;
                        this.start++;
                        val
                    }
                }
            }
        }

        pub fn contains(this, rhs: *T): bool => this.start <= rhs and this.end >= rhs;
    }
}
