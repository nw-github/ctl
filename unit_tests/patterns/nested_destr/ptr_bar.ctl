unittest "ptr bar" {
    mut bar = Bar(elem: [
        Baz(foo: Foo(a: 1, b: 2)), 
        Baz(foo: Foo(a: 3, b: 4))
    ]);
    match Quux::A(&mut bar) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            assert_eq(bar.elem[0].foo.a, 5);
            assert_eq(bar.elem[0].foo.b, 5);
            assert_eq(bar.elem[1].foo.a, 5);
            assert_eq(bar.elem[1].foo.b, 5);
            return;
        }
        Quux::B => { }
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
    elem: [Baz; 2],
}

union Quux {
    A(*mut Bar),
    B,
}
