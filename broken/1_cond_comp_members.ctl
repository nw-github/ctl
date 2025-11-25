union Hello {
    @(feature(test))
    A,
}

struct Hi {
    @(feature(test))
    x: i32,
}

unsafe union Hi2 {
    @(feature(test))
    x: i32,
}

// Conditional compilation does not work for any type of member variable
