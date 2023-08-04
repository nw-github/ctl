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
[lang(range_to_inclusive)]
pub struct RangeToInclusive<T>: RangeBounds<T> {
    pub end: T,

    pub fn begin(this) Bound<T> {
        return Bound::Unbounded();
    }

    pub fn end(this) Bound<T> {
        return Bound::Inclusive(this.end);
    }
}

// foo..
[lang(range_from)]
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
[lang(range)]
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
[lang(range_inclusive)]
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
[lang(range_full)]
pub struct RangeFull<T>: RangeBounds<T> {
    pub fn begin(this) Bound<T> {
        return Bound::Unbounded();
    }

    pub fn end(this) Bound<T> {
        return Bound::Unbounded();
    }
}
