use super::assert;

trait Foo {
    fn foo(mut this, x: int);
}

pub fn normal() {
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

    mut foo = Test(x: 0);
    mut bar = Test2(x: 0);

    mut x: *dyn mut Foo = &mut foo;
    x.foo(10);
    assert(foo.x == 10, "foo.x != 10");

    x = &mut bar;
    x.foo(10);
    assert(bar.x == 15, "bar.x != 15");
}

pub fn dependant() {
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

    mut foo = Test(x: 0);
    mut x: *dyn mut Bar = &mut foo;
    x.foo(10);
    assert(foo.x == 10, "foo.x != 10");
}

pub fn recursive() {
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

    mut foo = Test(x: 0);
    mut x: *dyn mut Bar = &mut foo;
    x.bar(10);
    assert(foo.x == 10, "foo.x != 10");
    x.baz(10);
    assert(foo.x == 0, "foo.x != 0");
}
