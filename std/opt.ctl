@(lang(option))
pub union Option<T> {
    null,
    Some(T),

    pub fn unwrap_or(my this, rhs: T): T => this is ?val then val else rhs;

    pub fn as_ptr(this): ?*T => this is ?val then val;
    pub fn as_mut(mut this): ?*mut T => this is ?val then val;

    pub fn get_or_insert(mut this, rhs: T): *mut T => this is ?val then val else this.insert(rhs);

    pub fn insert(mut this, rhs: T): *mut T {
        // TODO: do this more efficiently without the unwrap
        *this = rhs;
        this.as_mut()!
    }

    pub fn take(mut this): This => std::mem::replace(this, null);

    pub fn is_some(this): bool => this is ?_;
    pub fn is_null(this): bool => this is null;

    impl std::ops::Unwrap<T> {
        fn unwrap(this): T {
            if this is ?inner {
                *inner
            } else {
                panic("attempt to unwrap null Option");
            }
        }
    }
}

pub mod ext {
    pub extension OptionCopied<T /*: Copy */> for ?*T {
        pub fn copied(my this): ?T => this is ?val then *val;
    }

    pub extension OptionMutCopied<T /*: Copy */> for ?*mut T {
        pub fn copied(my this): ?T => this is ?val then *val;
    }

    pub extension OptionEq<T: std::ops::Eq<T>> for ?T {
        pub fn ==(this, rhs: *?T): bool {
            match (this, rhs) {
                (null, null) => true,
                (?lhs, ?rhs) => lhs == rhs,
                _ => false,
            }
        }

        pub fn ==(this, rhs: *T): bool => this is ?lhs and lhs == rhs;
    }

    pub extension OptionFlatten<T> for ??T {
        pub fn flatten(my this): ?T => this is ??val then val;
    }
}
