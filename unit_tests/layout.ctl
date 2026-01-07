use std::mem::size_of;

unittest "ptrs" {
    assert_eq(size_of::<*void>(), size_of::<int>());
    assert_eq(size_of::<*void>(), size_of::<uint>());
    assert_eq(size_of::<*void>(), size_of::<*mut void>());
    assert_eq(size_of::<*void>(), size_of::<^void>());

    assert_eq(size_of::<?*void>(), size_of::<*void>());
    assert_eq(size_of::<?*mut void>(), size_of::<*void>());
    assert_eq(size_of::<?^void>(), size_of::<*void>());
    assert_eq(size_of::<?fn()>(), size_of::<*void>());
}

unittest "c layout" {
    struct T {}

    $[layout(C)]
    struct MidPadding {
        x: i32,
        y: u8,
        z: i32,
    }

    $[layout(C)]
    union TailPadding {
        shared x: i32,

        Baz(u8),
        Quux(u16),
    }

    assert_eq(size_of::<T>(), 0);
    assert_eq(size_of::<MidPadding>(), 12);
    assert_eq(size_of::<TailPadding>(), 12);
}

unittest "auto layout" {
    struct MidPadding {
        x: i32,
        y: u8,
        z: i32,
    }

    union TailPadding {
        shared x: i32,

        Baz(u8),
        Quux(u16),
    }

    assert_eq(size_of::<MidPadding>(), 12);
    assert_eq(size_of::<TailPadding>(), 8);
}
