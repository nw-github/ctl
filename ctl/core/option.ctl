pub union Option<T> { // ?T
    Some(T),
    None,

    // ?? operator
    // pub fn unwrap_or(this, or: fn() T) T {
    //     match this {
    //         Option::Some(value) => yield *value,
    //         Option::None        => yield or(),
    //     }
    // }

    // ! operator
    pub fn unwrap(this) T {
        match *this {
            Option::Some(inner) => return inner,
            Option::None => ::core::panic("attempt to unwrap() empty Option"),
        }
    }
}
