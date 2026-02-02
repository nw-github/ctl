unittest "dependent" {
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

    mut foo = Test(x: 0);
    mut x: *dyn mut Bar = &mut foo;
    x.foo(10);

    assert_eq(foo.x, 10);
}

unittest "normal" {
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

    mut foo = Test(x: 0);
    mut bar = Test2(x: 0);

    mut x: *dyn mut Foo = &mut foo;
    x.foo(10);
    assert_eq(foo.x, 10);

    x = &mut bar;
    x.foo(10);
    assert_eq(bar.x, 15);
}

unittest "transitive impl" {
    trait Foo {
        fn add(this, x: *mut int);
    }

    trait Bar: Foo {
        fn sub(this, x: *mut int);
    }

    struct Test {
        impl Foo {
            fn add(this, x: *mut int) {
                *x += 1;
            }
        }

        impl Bar {
            fn sub(this, x: *mut int) {
                *x -= 1;
            }
        }
    }

    mut num = 0;
    let foo: *dyn Bar = &Test();
    foo.add(&mut num);
    assert_eq(num, 1);

    foo.sub(&mut num);
    assert_eq(num, 0);
}

unittest "dyn to dyn cast" {
    // Output: a.foo: 10
    // Output: a.quux: 35
    // Output: b.foo: 10
    // Output: b.bar: 5
    // Output: b.baz: 20
    // Output: b.quux: 35
    // Output: c.quux: 35
    // Output: c_from_b.quux: 35
    // Output: a_from_b.foo: 10
    // Output: a_from_b.quux: 35
    // Output: c_from_a.quux: 35

    trait A : C {
        fn foo(this): int;
    }

    trait B : A {
        fn bar(this): int;
        fn baz(this): int;
    }

    trait C {
        fn quux(this): int;
    }

    struct Hello {
        a: int,
        b: int,
        c: int,

        impl A {
            fn foo(this): int { this.a }
        }

        impl B {
            fn bar(this): int { this.b }
            fn baz(this): int { this.c }
        }

        impl C {
            fn quux(this): int { this.a + this.b + this.c }
        }
    }

    mut hi = Hello(a: 10, b: 5, c: 20);
    let a: *dyn mut A = &mut hi;
    let a: *dyn A = a;
    let b: *dyn B = &hi;
    let c: *dyn C = &hi;

    assert_eq(a.foo(), 10);
    assert_eq(a.quux(), 35);
    assert_eq(b.foo(), 10);
    assert_eq(b.bar(), 5);
    assert_eq(b.baz(), 20);
    assert_eq(b.quux(), 35);
    assert_eq(c.quux(), 35);

    let c_from_b: *dyn C = b;
    assert_eq(c_from_b.quux(), 35);

    let a_from_b: *dyn A = b;
    assert_eq(a_from_b.foo(), 10);
    assert_eq(a_from_b.quux(), 35);

    let c_from_a: *dyn C = a;
    assert_eq(c_from_a.quux(), 35);
}
