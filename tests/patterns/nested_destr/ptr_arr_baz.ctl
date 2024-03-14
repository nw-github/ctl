// Output: 5 5 5 5

fn main() {
    mut elem = &mut [
        Baz(foo: Foo(a: 1, b: 2)), 
        Baz(foo: Foo(a: 3, b: 4)),
    ];
    match Quux::A(Bar(elem:)) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            print("{elem[0].foo.a} ");
            print("{elem[0].foo.b} ");
            print("{elem[1].foo.a} ");
            print("{elem[1].foo.b} ");
        }
        Quux::B => { }
    }
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
