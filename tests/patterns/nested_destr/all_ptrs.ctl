// Output: 5 5 5 5

fn main() {
    mut fooa = Foo(a: 1, b: 2);
    mut foob = Foo(a: 3, b: 4);
    match Quux::A(&mut Bar(elem: &mut [&mut Baz(foo: &mut fooa), &mut Baz(foo: &mut foob)])) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            print("{fooa.a} ");
            print("{fooa.b} ");
            print("{foob.a} ");
            print("{foob.b} ");
        }
        Quux::B => { }
    }
}

struct Foo {
    a: int,
    b: int,
}

struct Baz {
    foo: *mut Foo,
}

struct Bar {
    elem: *mut [*mut Baz; 2],
}

union Quux {
    A(*mut Bar),
    B,
}
