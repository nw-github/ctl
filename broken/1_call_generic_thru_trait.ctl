trait B : A<int> {
    fn bar(this);
}

trait A<T> {
    fn foo(this, rhs: T);
}

struct Foo {
    impl B {
        fn bar(this) {
            this.foo(10);
        }
    }

    impl A<int> {
        fn foo(this, rhs: int) {
            println("fooing: {rhs}");
        }
    }
}

fn main() {
    let x: *dyn B = &Foo();
    println("x.foo(): {x.foo(5)}"); // broken
}
