unittest "destructure compile" {
    struct WithArr<T> {
        t: [T; 3],

        fn new<I: Iterator<T>>(mut t: I): ?WithArr<T> {
            if [t.next()] is [?a] and (t.next(),) is (?b) and [t.next()][..] is [?c] {
                WithArr(t: [a, b, *c])
            }
        }
    }

    let foo = WithArr::new([1, 2, 3].iter())!;
    assert_eq([*foo.t[0], *foo.t[1], *foo.t[2]], [1, 2, 3]);
}

unittest "first param" {
    cool(&[1, 2]);

    fn cool([a, b]: *[u8; 2]) {
        assert_eq(*a, 1);
        assert_eq(*b, 2);
    }
}

unittest "basic integer" {
    let x = 5;

    match x {
        0 => panic("0 matched x = 5"),
        1 => panic("1 matched x = 5"),
        5 => {},
        _ => panic("5 didnt match x = 5"),
    }

    match -x {
        0 => panic("0 matched x = -5"),
        1 => panic("1 matched x = -5"),
        -5 => {},
        _ => panic("-5 didnt match x = -5"),
    }

    match x {
        0..5 => panic("0..5 matches x = 5"),
        _ => {}
    }

    match x {
        0..=5 => {},
        _ => panic("0..=5 didn't match x = 5"),
    }

    match -x {
        -10..=5 => {},
        _ => panic("-10..=5 didn't match x = -5"),
    }

    match -x {
        ..10 => {}
        _ => panic("..=10 didn't match x = -5"),
    }

    match -x {
        ..=-5 => {}
        _ => panic("..=10 didn't match x = -5"),
    }
}

unittest "basic string" {
    match "hello" {
        "goodbye" => panic("hello matched 'goodbye'"),
        "oi" => panic("hello matched 'oi'"),
        "bonjour" => panic("hello matched 'bonjour'"),
        "hello" => {}
        _ => panic("hello didn't match 'hello'"),
    }
}

struct Foo { a: int, b: int }

unittest "basic struct" {
    let x = Foo(a: 10, b: 15);
    let (a:, b:) = x;
    assert_eq(a, 10);
    assert_eq(b, 15);
    match x {
        (mut a:, b:) => {
            a += 5;
            assert_eq(a, 15);
            assert_eq(b, 15);
        },
        _ => panic("fail!"),
    }
}

unittest "basic union" {
    union Bar { A, B(int) }

    match Bar::B(20) {
        Bar::A => panic("fail!"),
        Bar::B(val) => assert_eq(val, 20),
    }
}

unittest "optional struct" {
    let i = ?Foo(a: 10, b: 20);
    match i {
        null => panic("fail"),
        ?(mut a:, b:) => {
            a += 5;
            assert_eq(a, 15);
            assert_eq(b, 20);
        },
    }
}

unittest "or pattern with primitive" {
    let x = 5;
    match x {
        0 => panic("0 matched x = 5"),
        1 | 5 => {},
        _ => panic("5 didnt match x = 5"),
    }

    match (0, -x) {
        (1, -5) => panic("1 matched 0"),
        (0, -5) => {},
        (0, _n) => panic("match order was wrong"),
        _ => panic("-5 didnt match x = -5"),
    }

    match x {
        1 | 2 | 3..4 => panic("5 matched 1 | 2 | 3..4"),
        100 | 5..6 => {},
        _ => panic("100 | 5..6 didnt match x = 5"),
    }
}

unittest "span with middle rest pattern" {
    mut span = [10, 10, 10, 10, 10][..];
    match span {
        [a, b, ...mid, c, d] => {
            *a = 1;
            *b = 2;
            mid[0] = 3;
            *c = 4;
            *d = 5;

            assert_eq(span[0], 1);
            assert_eq(span[1], 2);
            assert_eq(span[2], 3);
            assert_eq(span[3], 4);
            assert_eq(span[4], 5);
        }
        _ => panic("span pattern didnt match"),
    }
}

unittest "span with nested destructure" {
    let span = [Foo(a: 10, b: 10), Foo(a: 10, b: 10)][..];
    match span {
        [(a:, b:), (a: a2, b: b2)] => {
            *a = 1;
            *b = 2;
            *a2 = 3;
            *b2 = 4;

            assert_eq(span[0].a, 1);
            assert_eq(span[0].b, 2);
            assert_eq(span[1].a, 3);
            assert_eq(span[1].b, 4);
        }
        _ => panic("span pattern didnt match"),
    }
}

unittest "span ptr destructure" {
    mut fooa = Foo(a: 10, b: 10);
    mut foob = Foo(a: 10, b: 10);
    match [&mut fooa, &mut foob][..] {
        [(a:, b:), (a: a2, b: b2)] => {
            *a = 1;
            *b = 2;
            *a2 = 3;
            *b2 = 4;

            assert_eq(fooa.a, 1);
            assert_eq(fooa.b, 2);
            assert_eq(foob.a, 3);
            assert_eq(foob.b, 4);
        }
        _ => panic("span pattern didnt match"),
    }
}

unittest "union with inner struct" {
    union Quux { A, B(Foo) }

    let i = Quux::B(Foo(a: 10, b: 20));
    match i {
        Quux::A => panic("fail!"),
        Quux::B((a:, b:)) => {
            assert_eq(a, 10);
            assert_eq(b, 20);
        },
    }
}
