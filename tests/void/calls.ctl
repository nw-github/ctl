// Output: Passed!
use std::sync::{Atomic, ext::*};

trait Foo {
    fn bar(this);
}

struct Bar {
    impl Foo {
        fn bar(this) => hello();
    }
}

static VALUE: Atomic<int> = Atomic::new(0);

fn main() {
    let _ = hello();
    let _ = fnptr(hello);
    let _ = dynamic(&Bar());
    assert_eq(VALUE.load(), 3);
    println("Passed!");
}

fn hello() {
    VALUE.fetch_add(1);
}

fn fnptr(x: fn()) => x();
fn dynamic(f: *dyn Foo) => f.bar();
