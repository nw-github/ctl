unittest "all pointers 2" {
    mut baza = Baz(foo: Foo(a: 1, b: 2));
    mut bazb = Baz(foo: Foo(a: 3, b: 4));
    match Quux::A(Bar(elem: [&mut baza, &mut bazb])) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            return assert_eq([baza.foo.a, baza.foo.b, bazb.foo.a, bazb.foo.b], [5, 5, 5, 5]);
        }
        Quux::B => {}
    }

    panic("fail!");
}

struct Foo {
    a: int,
    b: int,
}

struct Baz {
    foo: Foo,
}

struct Bar {
    elem: [*mut Baz; 2],
}

union Quux {
    A(Bar),
    B,
}
