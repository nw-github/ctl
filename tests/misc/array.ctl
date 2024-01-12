use super::assert;

pub fn subscript(a: [i32; 4], b: *mut [i32; 4]) {
    b[0] = a[0];
    b[1] = a[1];
    b[2] = a[2];
    b[3] = a[3];
}

mod val {
    pub fn end([x, y, ...end]: [i32; 4], b: *mut [i32; 4]) {
        b[0] = x;
        b[1] = y;
        b[2] = end[0];
        b[3] = end[1];
    }

    pub fn start([...start, x, y]: [i32; 4], b: *mut [i32; 4]) {
        b[0] = start[0];
        b[1] = start[1];
        b[2] = x;
        b[3] = y;
    }

    pub fn mid([x, ...mid, y]: [i32; 4], b: *mut [i32; 4]) {
        b[0] = x;
        b[1] = mid[0];
        b[2] = mid[1];
        b[3] = y;
    }
}

mod ptr {
    pub fn regular([a, b, c, d]: [i32; 4], [x, y, z, w]: *mut [i32; 4]) {
        *x = a;
        *y = b;
        *z = c;
        *w = d;
    }

    pub fn end([a, b, c, d]: [i32; 4], [x, y, ...end]: *mut [i32; 4]) {
        *x = a;
        *y = b;
        end[0] = c;
        end[1] = d;
    }

    pub fn start([a, b, c, d]: [i32; 4], [...start, x, y]: *mut [i32; 4]) {
        start[0] = a;
        start[1] = b;
        *x = c;
        *y = d;
    }

    pub fn mid([a, b, c, d]: [i32; 4], [x, ...mid, y]: *mut [i32; 4]) {
        *x = a;
        mid[0] = b;
        mid[1] = c;
        *y = d;
    }
}

struct Foo {
    a: i32,
    b: i32,
}

pub fn pattern_stuff() {
    mut funcs = @[
        subscript, 
        val::end, 
        val::start, 
        val::mid,
        ptr::regular,
        ptr::end,
        ptr::start,
        ptr::mid,
    ];
    for func in funcs.iter() {
        let a = [1, 2, 3, 4];
        mut b = [5, 6, 7, 8];
        (*func)(a, &mut b);

        assert(b[0] == a[0], "b[0] != a[0]");
        assert(b[1] == a[1], "b[1] != a[1]");
        assert(b[2] == a[2], "b[2] != a[2]");
        assert(b[3] == a[3], "b[3] != a[3]");
    }
}

pub fn nested_ptr() {
    union Baz {
        A(*mut Foo),
        B(*mut [i32; 2]),
    }

    fn by_val(x: Baz) {
        match x {
            Baz::A({a, b}) => {
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
            Baz::A({a, b}) => {
                *a = 5;
                *b = 5;
            }
            Baz::B([x, y]) => {
                *x = 5;
                *y = 5;
            }
        }
    }

    mut foo = Foo(a: 10, b: 10);
    by_val(Baz::A(&mut foo));
    assert(foo.a == 5, "foo.a wasnt set to 5");
    assert(foo.b == 5, "foo.b wasnt set to 5");

    mut arr = [10, 10];
    by_val(Baz::B(&mut arr));
    assert(arr[0] == 5, "arr[0] wasnt set to 5");
    assert(arr[1] == 5, "arr[1] wasnt set to 5");

    mut foo = Foo(a: 10, b: 10);
    by_ref(&mut Baz::A(&mut foo));
    assert(foo.a == 5, "foo.a wasnt set to 5");
    assert(foo.b == 5, "foo.b wasnt set to 5");

    mut arr = [10, 10];
    by_ref(&mut Baz::B(&mut arr));
    assert(arr[0] == 5, "arr[0] wasnt set to 5");
    assert(arr[1] == 5, "arr[1] wasnt set to 5");
}

pub fn nested_destructure_1() {
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

            assert(bar.elem[0].foo.a == 5, "first Foo::a was not set to 5");
            assert(bar.elem[0].foo.b == 5, "first Foo::b was not set to 5");
            assert(bar.elem[1].foo.a == 5, "second Foo::a was not set to 5");
            assert(bar.elem[1].foo.b == 5, "second Foo::b was not set to 5");
        }
        Quux::B => {
            assert(false, "Quux::A matched Quux::B");
        }
    }
}

pub fn nested_destructure_2() {
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

            assert(elem[0].foo.a == 5, "first Foo::a was not set to 5");
            assert(elem[0].foo.b == 5, "first Foo::b was not set to 5");
            assert(elem[1].foo.a == 5, "second Foo::a was not set to 5");
            assert(elem[1].foo.b == 5, "second Foo::b was not set to 5");
        }
        Quux::B => {
            assert(false, "Quux::A matched Quux::B");
        }
    }
}

pub fn nested_destructure_3() {
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

    mut baza = Baz(foo: Foo(a: 1, b: 2));
    mut bazb = Baz(foo: Foo(a: 3, b: 4));
    match Quux::A(Bar(elem: [&mut baza, &mut bazb])) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            assert(baza.foo.a == 5, "first Foo::a was not set to 5");
            assert(baza.foo.b == 5, "first Foo::b was not set to 5");
            assert(bazb.foo.a == 5, "second Foo::a was not set to 5");
            assert(bazb.foo.b == 5, "second Foo::b was not set to 5");
        }
        Quux::B => {
            assert(false, "Quux::A matched Quux::B");
        }
    }
}

pub fn nested_destructure_4() {
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

    mut fooa = Foo(a: 1, b: 2);
    mut foob = Foo(a: 3, b: 4);
    match Quux::A(Bar(elem: [Baz(foo: &mut fooa), Baz(foo: &mut foob)])) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            assert(fooa.a == 5, "first Foo::a was not set to 5");
            assert(fooa.b == 5, "first Foo::b was not set to 5");
            assert(foob.a == 5, "second Foo::a was not set to 5");
            assert(foob.b == 5, "second Foo::b was not set to 5");
        }
        Quux::B => {
            assert(false, "Quux::A matched Quux::B");
        }
    }
}

pub fn nested_destructure_5() {
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

    mut fooa = Foo(a: 1, b: 2);
    mut foob = Foo(a: 3, b: 4);
    match Quux::A(&mut Bar(elem: &mut [&mut Baz(foo: &mut fooa), &mut Baz(foo: &mut foob)])) {
        Quux::A({elem: [{foo: {a, b}}, {foo: {a: a2, b: b2}}]}) => {
            *a = 5;
            *b = 5;
            *a2 = 5;
            *b2 = 5;

            assert(fooa.a == 5, "first Foo::a was not set to 5");
            assert(fooa.b == 5, "first Foo::b was not set to 5");
            assert(foob.a == 5, "second Foo::a was not set to 5");
            assert(foob.b == 5, "second Foo::b was not set to 5");
        }
        Quux::B => {
            assert(false, "Quux::A matched Quux::B");
        }
    }
}
