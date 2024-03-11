use core::iter::Iterator;
use core::reflect::*;
use core::ext::*;

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

    impl Iterator<T> {
        fn next(mut this): ?T {
            if this.start < this.start.wrapping_add(core::intrin::numeric_cast(1)) {
                this.start++
            }
        }
    }
}

// foo..bar
#(lang(range))
pub struct Range<T: Numeric + Integral> {
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

    impl Iterator<T> {
        fn next(mut this): ?T {
            if this.start < this.end {
                this.start++
            }
        }
    }
}

// extension<T: Numeric> RangeExt for Range<T> {
//     
// }

// foo..=bar
#(lang(range_inclusive))
pub struct RangeInclusive<T: Numeric + Integral> {
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
