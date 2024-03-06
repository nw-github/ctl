use super::assert;
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

pub fn cmp_eq() {
    let nums = @[
        (Foo(val: 10), Foo(val: 15), [true, false, true, false, false, true]),
        (Foo(val: 10), Foo(val: 10), [false, false, true, true, true, false]),
        (Foo(val: 10), Foo(val: 5), [false, true, false, true, false, true]),
    ];

    for (a, b, arr) in nums.iter() {
        assert((a < b) == arr[0], "");
        assert((a > b) == arr[1], "");
        assert((a <= b) == arr[2], "");
        assert((a >= b) == arr[3], "");
        assert((a == b) == arr[4], "");
        assert((a != b) == arr[5], "");
    }
}
