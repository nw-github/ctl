use std::ops::*;

struct Foo {
    val: int,

    impl Eq<This> {
        fn eq(this, rhs: *This): bool {
            this.val == rhs.val
        }
    }

    impl Cmp<This> {
        fn cmp(this, rhs: *This): Ordering {
            this.val <=> rhs.val
        }
    }
}

unittest "eq" {
    let a = Foo(val: 10);
    let b = Foo(val: 10);
    assert_eq(a < b, false);
    assert_eq(a > b, false);
    assert_eq(a <= b, true);
    assert_eq(a >= b, true);
    assert_eq(a == b, true);
    assert_eq(a != b, false);
}

unittest "greater" {
    let a = Foo(val: 10);
    let b = Foo(val: 5);
    assert_eq(a < b, false);
    assert_eq(a > b, true);
    assert_eq(a <= b, false);
    assert_eq(a >= b, true);
    assert_eq(a == b, false);
    assert_eq(a != b, true);
}

unittest "less" {
    let a = Foo(val: 10);
    let b = Foo(val: 15);
    assert_eq(a < b, true);
    assert_eq(a > b, false);
    assert_eq(a <= b, true);
    assert_eq(a >= b, false);
    assert_eq(a == b, false);
    assert_eq(a != b, true);
}
