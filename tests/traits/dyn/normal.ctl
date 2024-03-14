// Output: 10
// Output: 15

trait Foo {
    fn foo(mut this, x: int);
}

struct Test {
    x: int,

    impl Foo {
        fn foo(mut this, x: int) {
            this.x = x;
        }
    }
}

struct Test2 {
    x: int,

    impl Foo {
        fn foo(mut this, x: int) {
            this.x = x + 5;
        }
    }
}

fn main() {
    mut foo = Test(x: 0);
    mut bar = Test2(x: 0);

    mut x: *dyn mut Foo = &mut foo;
    x.foo(10);
    println("{foo.x}");

    x = &mut bar;
    x.foo(10);
    println("{bar.x}");
}
