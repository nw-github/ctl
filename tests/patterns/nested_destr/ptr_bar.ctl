// Output: 5 5 5 5

fn main() {
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

            print("{bar.elem[0].foo.a} ");
            print("{bar.elem[0].foo.b} ");
            print("{bar.elem[1].foo.a} ");
            print("{bar.elem[1].foo.b} ");
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
    elem: [Baz; 2],
}

union Quux {
    A(*mut Bar),
    B,
}
