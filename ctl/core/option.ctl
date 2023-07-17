pub union Option<T> { // ?T
    Some(T),
    None,

    // ?? operator
    // pub fn unwrap_or(this, or: fn() T) T {
    //     return match this {
    //         Option::Some(value) => *value,
    //         Option::None        => or(),
    //     };
    // }

    // ! operator
    pub fn unwrap(this) T {
        match *this {
            Option::Some(inner) => {
                return inner;
            },
            Option::None => {
                ::core::panic("Option::unwrap(): value is null!");
            },
        }
    }
}
