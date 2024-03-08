use super::assert;

static OUTER: int = 10;

fn func(): int { OUTER }

pub fn statics() {
    static INNER: int = 20;
    assert(func() + INNER == 30, "INNER + OUTER != 30");
}

pub fn void_assigns() {
    struct A {
        b: void,
    }

    fn hello() { }

    mut x = A(b: void);
    x.b = hello();
    let _y = x.b;
    let _x = if true { void } else { void };
}

pub fn booleans() {
    let x = 5 == 2;
    assert(x == false, "5 == 2 should be false");
    assert((!x) == true, "! should negate");
    assert((!!x) == false, "!! should be a no op");
}

pub fn sizes() {
    use std::mem::size_of;

    struct T {}

    assert(size_of::<*T>() == size_of::<int>(), "sizeof *T and int differs!");
    assert(size_of::<*T>() == size_of::<uint>(), "sizeof *T and uint differs!");
    assert(size_of::<*T>() == size_of::<*mut T>(), "sizeof *T and *mut T differs!");
    assert(size_of::<*T>() == size_of::<*raw T>(), "sizeof *T and *raw T differs!");
    assert(size_of::<T>() == 0, "empty struct is not zero size!");

    assert(size_of::<?*i32>() == size_of::<*i32>(), "?*i32 variant is not optimized away!");
    assert(size_of::<?*mut i32>() == size_of::<*i32>(), "?*mut i32 variant is not optimized away!");
    assert(size_of::<?*raw i32>() == size_of::<*i32>(), "?*raw i32 variant is not optimized away!");
    assert(size_of::<?fn()>() == size_of::<*i32>(), "?fn() variant is not optimized away!");

    struct MidPadding {
        x: i32,
        y: u8,
        z: i32,
    }

    assert(size_of::<MidPadding>() == 12, "MidPadding is not 8 bytes");

    union TailPadding {
        shared x: i32,

        Baz(u8),
        Quux(u16),
    }

    assert(size_of::<TailPadding>() == 12, "TailPadding is not 12 bytes");
}
