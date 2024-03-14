// Output: 5 5 5 5

fn main() {
    mut baza = Baz(foo: Foo(a: 1, b: 2));
    mut bazb = Baz(foo: Foo(a: 3, b: 4));
    match Quux::A(Bar(elem: [&mut baza, &mut bazb])) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            print("{baza.foo.a} ");
            print("{baza.foo.b} ");
            print("{bazb.foo.a} ");
            print("{bazb.foo.b} ");
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
    elem: [*mut Baz; 2],
}

union Quux {
    A(Bar),
    B,
}
