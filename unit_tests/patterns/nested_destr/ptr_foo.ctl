unittest "ptr bar" {
    mut fooa = Foo(a: 1, b: 2);
    mut foob = Foo(a: 3, b: 4);
    match Quux::A(Bar(elem: [Baz(foo: &mut fooa), Baz(foo: &mut foob)])) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            assert_eq(fooa.a, 5);
            assert_eq(fooa.b, 5);
            assert_eq(foob.a, 5);
            assert_eq(foob.b, 5);
            return;
        }
        Quux::B => { }
    }

    panic("fail");
}

struct Foo {
    a: int,
    b: int,
}

struct Baz {
    foo: *mut Foo,
}

struct Bar {
    elem: [Baz; 2],
}

union Quux {
    A(Bar),
    B,
}
