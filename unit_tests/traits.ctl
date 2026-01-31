unittest "impl<T>" {
    trait ReturnTuple<T, U> {
        fn get(this): (T, U);
    }

    trait Default {
        fn default(): This;
    }

    struct Foo {
        impl<T: Default, U: Default> ReturnTuple<T, U> {
            fn get(this): (T, U) => (T::default(), U::default());
        }
    }

    fn foo<T: ReturnTuple<int, uint>>(t: T) {
        let (a, b) = t.get();
        assert_eq(a, 3);
        assert_eq(b, 5);
    }

    extension int {
        impl Default {
            fn default(): int => 3;
        }
    }

    extension uint {
        impl Default {
            fn default(): uint => 5;
        }
    }

    foo(Foo());
}

unittest "generic super trait" {
    trait A<T> : C<T> {
        fn foo(this): T;
    }

    trait B<T> : A<T> {
        fn bar(this): T;
        fn baz(this): T;
    }

    trait C<T> {
        fn quux(this): T;
    }

    struct Hello<V: std::ops::Add<V, V>> {
        a: V,
        b: V,
        c: V,

        impl A<V> {
            fn foo(this): V { this.a }
        }

        impl B<V> {
            fn bar(this): V { this.b }
            fn baz(this): V { this.c }
        }

        impl C<V> {
            fn quux(this): V { this.a + this.b + this.c }
        }
    }

    let hi = Hello(a: 10, b: 5, c: 20);
    let a: *dyn A<int> = &hi;
    let b: *dyn B<int> = &hi;
    let c: *dyn C<int> = &hi;

    assert_eq(a.foo(), 10);
    assert_eq(a.quux(), 35);

    assert_eq(b.foo(), 10);
    assert_eq(b.bar(), 5);
    assert_eq(b.baz(), 20);
    assert_eq(b.quux(), 35);

    assert_eq(c.quux(), 35);

    let c_from_b: *dyn C<int> = b;
    assert_eq(c_from_b.quux(), 35);

    let a_from_b: *dyn A<int> = b;
    assert_eq(a_from_b.foo(), 10);
    assert_eq(a_from_b.quux(), 35);

    let c_from_a: *dyn C<int> = a;
    assert_eq(c_from_a.quux(), 35);
}

unittest "super trait" {
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

    let hi = Hello(a: 10, b: 5, c: 20);
    let a: *dyn A = &hi;
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
