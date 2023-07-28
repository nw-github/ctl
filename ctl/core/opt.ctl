pub union Option<T> { // ?T
    Some(T),
    None,

    // ?? operator
    // pub fn unwrap_or(this, or: fn() T) T {
    //     return match this {
    //         ?value => *value,
    //         null   => or(),
    //     };
    // }

    // ! operator
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
}
