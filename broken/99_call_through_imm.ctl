trait Test : std::any::Any {
    fn test(mut this);
}

struct Foo {
    x: *mut i32,
    y: *dyn mut Test,

    fn hi(this) {
        *this.x = 1; // 'this' is immutable, this should fail
        this.y.test(); // this should also fail

        let x = this.x; // however until anything stops this from happening, addressing it is pointless
        *x = 4;
    }
}

struct Bar {
    a: int,

    impl Test {
        fn test(mut this) {
            this.a = 4;
            println("Test from bar: {this.a}!");
        }
    }
}

fn main() {
    mut bar = Bar(a: 10);
    mut foo = Foo(x: &mut 5, y: &mut bar);

    foo.hi();
}
