// Output: 10
// Output: 0

trait Bar: Baz {
    fn bar(mut this, x: int);
}

trait Baz: Bar {
    fn baz(mut this, x: int);
}

struct Test {
    x: int,

    impl Bar {
        fn bar(mut this, x: int) {
            this.x += x;
        }
    }

    impl Baz {
        fn baz(mut this, x: int) {
            this.x -= x;
        }
    }
}

fn main() {
    mut foo = Test(x: 0);
    mut x: *dyn mut Bar = &mut foo;
    x.bar(10);
    println("{foo.x}");

    x.baz(10);
    println("{foo.x}");
}
