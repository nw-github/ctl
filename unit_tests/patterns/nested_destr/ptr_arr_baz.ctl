unittest "ptr array baz" {
    mut elem = &mut [
        Baz(foo: Foo(a: 1, b: 2)),
        Baz(foo: Foo(a: 3, b: 4)),
    ];
    match Quux::A(Bar(elem:)) {
        Quux::A((elem: [(foo: (a:, b:)), (foo: (a: a2, b: b2))])) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;
            assert_eq([elem[0].foo.a, elem[0].foo.b, elem[1].foo.a, elem[1].foo.b], [5, 5, 5, 5]);
            return;
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
    elem: *mut [Baz; 2],
}

union Quux {
    A(Bar),
    B,
}
