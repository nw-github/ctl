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
pub struct RangeTo<T>: RangeBounds<T> {
    pub end: T,

    pub fn begin(this) Bound<T> {
        return Bound::Unbounded();
    }

    pub fn end(this) Bound<T> {
        return Bound::Exclusive(this.end);
    }
}

// ..=bar
pub struct RangeToInclusive<T>: RangeBounds<T> {
    pub start: T,
    pub end: T,

    pub fn begin(this) Bound<T> {
        return Bound::Unbounded();
    }

    pub fn end(this) Bound<T> {
        return Bound::Inclusive(this.end);
    }
}

// foo..
pub struct RangeFrom<T>: RangeBounds<T> {
    pub start: T,

    pub fn begin(this) Bound<T> {
        return Bound::Inclusive(this.start);
    }

    pub fn end(this) Bound<T> {
        return Bound::Unbounded();
    }
}

// foo..bar
pub struct Range<T>: RangeBounds<T> {
    pub start: T,
    pub end: T,

    pub fn begin(this) Bound<T> {
        return Bound::Inclusive(this.start);
    }

    pub fn end(this) Bound<T> {
        return Bound::Exclusive(this.end);
    }
}

// foo..=bar
pub struct RangeInclusive<T>: RangeBounds<T> {
    pub start: T,
    pub end: T,

    pub fn begin(this) Bound<T> {
        return Bound::Inclusive(this.start);
    }

    pub fn end(this) Bound<T> {
        return Bound::Inclusive(this.end);
    }
}

// ..
pub struct RangeFull<T>: RangeBounds<T> {
    pub fn begin(this) Bound<T> {
        return Bound::Unbounded();
    }

    pub fn end(this) Bound<T> {
        return Bound::Unbounded();
    }
}

/*
extend<T> RangeFull: RangeBounds<T> {

}
 */
