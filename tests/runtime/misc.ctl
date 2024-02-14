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
    let y = x.b;
    let x = if true { void } else { void };
}

pub fn booleans() {
    let x = 5 == 2;
    assert(x == false, "5 == 2 should be false");
    assert((!x) == true, "! should negate");
    assert((!!x) == false, "!! should be a no op");
}
