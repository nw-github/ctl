use core::iter::Iterator;
use core::reflect::*;
use core::ext::*;

#(lang(range_bounds))
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
#(lang(range_to))
pub struct RangeTo<T> {
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> {
            Bound::Unbounded
        }

        fn end(this): Bound<T> {
            Bound::Exclusive(this.end)
        }
    }
}

// ..=bar
#(lang(range_to_inclusive))
pub struct RangeToInclusive<T> {
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> {
            Bound::Unbounded
        }

        fn end(this): Bound<T> {
            Bound::Inclusive(this.end)
        }
    }
}

// foo..
#(lang(range_from))
pub struct RangeFrom<T: Numeric + Integral> {
    pub start: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> {
            Bound::Inclusive(this.start)
        }

        fn end(this): Bound<T> {
            Bound::Unbounded
        }
    }
}

// foo..bar
#(lang(range))
pub struct Range<T> {
    pub start: T,
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> {
            Bound::Inclusive(this.start)
        }

        fn end(this): Bound<T> {
            Bound::Exclusive(this.end)
        }
    }
}

// foo..=bar
#(lang(range_inclusive))
pub struct RangeInclusive<T> {
    pub start: T,
    pub end: T,

    impl RangeBounds<T> {
        fn begin(this): Bound<T> {
            Bound::Inclusive(this.start)
        }

        fn end(this): Bound<T> {
            Bound::Inclusive(this.end)
        }
    }
}

// ..
#(lang(range_full))
pub struct RangeFull {
//     impl<T> RangeBounds<T> {
//         fn begin(this): Bound<T> {
//             Bound::Unbounded()
//         }
// 
//         fn end(this): Bound<T> {
//             Bound::Unbounded()
//         }
//     }
}

pub mod ext {
    use super::*;

    pub extension RangeFromExt<T: Numeric + Integral> for RangeFrom<T> {
        impl Iterator<T> {
            fn next(mut this): ?T {
                if this.start < this.start.wrapping_add(1.cast()) {
                    this.start++
                }
            }
        }
    }

    pub extension RangeExt<T: Numeric + Integral> for Range<T> {
        impl Iterator<T> {
            fn next(mut this): ?T {
                if this.start < this.end {
                    this.start++
                }
            }
        }

        pub fn contains(this, rhs: *T): bool {
            this.start <= rhs and this.end > rhs
        }
    }

    pub extension RangeInclusiveExt<T: Numeric + Integral> for RangeInclusive<T> {
        impl Iterator<T> {
            fn next(mut this): ?T {
                if this.start < this.end {
                    this.start++
                } else if this.start == this.end {
                    // avoid overflow at the upper end
                    this.end--;
                    this.start
                }
            }
        }

        pub fn contains(this, rhs: *T): bool {
            this.start <= rhs and this.end >= rhs
        }
    }
}
