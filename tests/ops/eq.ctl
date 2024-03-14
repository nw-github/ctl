// Output: false false true true true false

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

fn main() {
    let a = Foo(val: 10);
    let b = Foo(val: 10);
    println("{a < b} {a > b} {a <= b} {a >= b} {a == b} {a != b}");
}
