@(lang(option))
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

    pub fn as_ptr(this): ?*T {
        if this is ?val {
            val
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
            this.insert(rhs)
        }
    }

    pub fn insert(mut this, rhs: T): *mut T {
        // TODO: do this more efficiently without the unwrap
        *this = rhs;
        this.as_mut()!
    }

    pub fn take(mut this): This {
        core::mem::replace(this, null)
    }

    pub fn is_some(this): bool {
        this is ?_
    }

    pub fn is_null(this): bool {
        this is null
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

pub mod ext {
    pub extension OptionFormat<T: core::fmt::Format> for ?T {
        impl core::fmt::Format {
            fn fmt<F: core::fmt::Formatter>(this, f: *mut F) {
                if this is ?rhs {
                    "Some(".fmt(f);
                    rhs.fmt(f);
                    ")".fmt(f);
                } else {
                    "null".fmt(f);
                }
            }
        }
    }

    pub extension OptionCopied<T /*: Copy */> for ?*T {
        pub fn copied(my this): ?T {
            if this is ?val {
                *val
            }
        }
    }

    pub extension OptionMutCopied<T /*: Copy */> for ?*mut T {
        pub fn copied(my this): ?T {
            if this is ?val {
                *val
            }
        }
    }

    pub extension OptionEq<T: core::ops::Eq<T>> for ?T {
        pub fn ==(this, rhs: *?T): bool {
            match (this, rhs) {
                (null, null) => true,
                (?lhs, ?rhs) => lhs == rhs,
                _ => false,
            }
        }

        pub fn ==(this, rhs: *T): bool {
            this is ?lhs and lhs == rhs
        }
    }

    pub extension OptionFlatten<T> for ??T {
        pub fn flatten(my this): ?T {
            if this is ? ?val {
                val
            }
        }
    }
}
