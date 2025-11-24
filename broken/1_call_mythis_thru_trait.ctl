trait Test : std::any::Any {
    fn foo(my this);
}

extern fn test(x: *dyn Test) {
    // this obviously shouldnt work
    x.foo();
}

struct Foo {
    a: int,

    impl Test {
        fn foo(my this) {
            println("Foo from bar: {this.a}")
        }
    }
}

fn main() {
    test(&Foo(a: 10))
}
