unittest "array nested pointer" {
    mut foo = Foo(a: 10, b: 10);
    by_val(Baz::A(&mut foo));
    assert_eq([foo.a, foo.b], [5, 5]);

    mut arr = [10, 10];
    by_val(Baz::B(&mut arr));
    assert_eq(arr, [5, 5]);

    mut foo = Foo(a: 10, b: 10);
    by_ref(&mut Baz::A(&mut foo));
    assert_eq([foo.a, foo.b], [5, 5]);

    mut arr = [10, 10];
    by_ref(&mut Baz::B(&mut arr));
    assert_eq(arr, [5, 5]);
}

struct Foo {
    a: int,
    b: int,
}

union Baz {
    A(*mut Foo),
    B(*mut [int; 2]),
}

fn by_val(x: Baz) {
    match x {
        Baz::A((a:, b:)) => {
            *a = 5;
            *b = 5;
        }
        Baz::B([x, y]) => {
            *x = 5;
            *y = 5;
        }
    }
}

fn by_ref(x: *mut Baz) {
    match x {
        Baz::A((a:, b:)) => {
            *a = 5;
            *b = 5;
        }
        Baz::B([x, y]) => {
            *x = 5;
            *y = 5;
        }
    }
}
