// Output: 10

trait Foo {
    fn foo(mut this, x: int);
}

trait Bar: Foo { }

struct Test {
    x: int,
    
    impl Foo {
        fn foo(mut this, x: int) {
            this.x = x;
        }
    }

    impl Bar {}
}

fn main() {
    mut foo = Test(x: 0);
    mut x: *dyn mut Bar = &mut foo;
    x.foo(10);
    println("{foo.x}");
}
