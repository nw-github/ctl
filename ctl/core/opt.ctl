use core::panic;

#(lang(option))
pub union Option<T> {
    Some(T),
    None,

    pub fn unwrap_or(this, or: T): T {
        if this is ?val {
            *val
        } else {
            or
        }
    }

    pub fn as_mut(mut this): ?*mut T {
        if this is ?val {
            val
        }
    }

    pub fn get_or_insert(mut this, or: T): *mut T {
        if this is ?val {
            val
        } else {
            *this = or;
            this.as_mut()!
        }
    }

    impl core::ops::Unwrap<T> {
        fn unwrap(this): T {
            if this is ?inner {
                *inner
            } else {
                panic("Option::unwrap(): value is null!");
            }
        }
    }
}
