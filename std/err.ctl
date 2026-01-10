pub union Result<T, E> {
    Ok(T),
    Err(E),

    pub fn ok(my this): ?T =>this is Ok(val) then val;

    pub fn is_ok(this): bool => this is Ok(_);
    pub fn is_err(this): bool => this is Err(_);

    pub fn unwrap_err(my this): E {
        if this is Err(inner) {
            inner
        } else {
            panic("attempt to unwrap_err Result with value {this:?}");
        }
    }

    impl std::ops::Unwrap<T> {
        fn unwrap(my this): T {
            if this is Ok(inner) {
                inner
            } else {
                panic("attempt to unwrap Result with value {this:?}");
            }
        }
    }
}
