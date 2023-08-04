[lang(option)]
pub union Option<T> {
    Some(T),
    None,

    pub fn unwrap(this) T {
        match *this {
            ?inner => {
                return inner;
            },
            null => {
                panic("Option::unwrap(): value is null!");
            },
        }
    }

    pub fn unwrap_or(this, or: T) T {
        return match *this {
            ?val => val,
            null => or,
        };
    }

    pub fn as_mut(mut this) ?*mut T {
        return match this {
            ?val => val,
            null => null,
        };
    }

    pub fn get_or_insert(mut this, or: T) *mut T {
        return match this {
            ?val => val,
            empty => {
                *empty = or;
                yield this.as_mut()!;
            }
        };
    }
}
