$[lang(option)]
pub union Option<T> {
    null,
    Some(T),

    pub fn unwrap_or(my this, rhs: T): T => this is ?val then val else rhs;

    pub fn as_ptr(this): ?*T => this is ?val then val;
    pub fn as_mut(mut this): ?*mut T => this is ?val then val;

    pub fn get_or_insert(mut this, rhs: T): *mut T => this is ?val then val else this.insert(rhs);

    pub fn insert(mut this, rhs: T): *mut T {
        *this = rhs;
        unsafe this.as_mut().unwrap_unchecked()
    }

    pub fn take(mut this): This => std::mem::replace(this, null);

    pub fn is_some(this): bool => this is ?_;
    pub fn is_null(this): bool => this is null;

    pub fn map<U, F: Fn(T) => U>(my this, f: F): ?U => this is ?val then f(val);
    pub fn and_then<U, F: Fn(T) => ?U>(my this, f: F): ?U => this is ?val then f(val) else null;
    pub fn filter<F: Fn(T) => bool>(my this, f: F): ?T => this is ?val and f(val) then val;

    pub unsafe fn unwrap_unchecked(my this): T {
        if this is ?inner {
            inner
        } else {
            unsafe std::hint::unreachable_unchecked()
        }
    }

    impl std::ops::Unwrap<T> {
        fn unwrap(my this): T {
            if this is ?inner {
                inner
            } else {
                panic("attempt to unwrap null Option");
            }
        }
    }
}

extension<T /*: Copy */> ?*T {
    pub fn copied(my this): ?T => this is ?val then *val;
}

extension<T /*: Copy */> ?*mut T {
    pub fn copied(my this): ?T => this is ?val then *val;
}

extension<T: std::ops::Eq<T>> ?T {
    pub fn ==(this, rhs: *?T): bool {
        match (this, rhs) {
            (null, null) => true,
            (?lhs, ?rhs) => lhs == rhs,
            _ => false,
        }
    }

    pub fn ==(this, rhs: *T): bool => this is ?lhs and lhs == rhs;
}

extension<T> ??T {
    pub fn flatten(my this): ?T => this is ??val then val;
}
