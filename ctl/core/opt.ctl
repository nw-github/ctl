#{lang(option)}
pub union Option<T> {
    Some(T),
    None,

    pub fn unwrap(this): T {
        match *this {
            ?inner => inner,
            null => {
                panic("Option::unwrap(): value is null!");
            },
        }
    }

    pub fn unwrap_or(this, or: T): T {
        match *this {
            ?val => val,
            null => or,
        }
    }

    pub fn as_mut(mut this): ?*mut T {
        match this {
            ?val => val,
            null => null,
        }
    }

    pub fn get_or_insert(mut this, or: T): *mut T {
        match this {
            ?val => val,
            empty => {
                *empty = or;
                this.as_mut()!
            }
        }
    }
}
