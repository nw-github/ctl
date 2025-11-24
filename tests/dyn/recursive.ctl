// Error: trait recursively requires implementation of itself
// Error: trait recursively requires implementation of itself

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

fn main() {}
