use core::panic;

#(lang(option))
pub union Option<T> {
    Some(T),
    None,

    pub fn unwrap_or(this, rhs: T): T {
        if this is ?val {
            *val
        } else {
            rhs
        }
    }

    pub fn as_mut(mut this): ?*mut T {
        if this is ?val {
            val
        }
    }

    pub fn get_or_insert(mut this, rhs: T): *mut T {
        if this is ?val {
            val
        } else {
            *this = rhs;
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
