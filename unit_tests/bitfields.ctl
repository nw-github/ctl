unittest "bitfield assignment ops" {
    packed struct Foo {
        a: u64,
        b: u4,
        c: u2,
        d: u2,
        e: u8,
    }

    mut foo = Foo(a: 5, b: 4, c: 3, d: 2, e: 1);
    foo.a += 2;
    foo.b -= 4;
    foo.d = 1;

    assert_eq(foo.a, 7);
    assert_eq(foo.b, 0);
    assert_eq(foo.c, 3);
    assert_eq(foo.d, 1);
    assert_eq(foo.e, 1);

    assert_eq(std::mem::size_of_val(&foo), 16);
    assert_eq(std::mem::align_of_val(&foo), 8);
}

unittest "two words" {
    packed struct Foo {
        a: u2,
        x: u68,
    }

    mut foo = Foo(a: 0, x: 0x1ffffffffffffffff);
    assert_eq(foo.x, 0x1ffffffffffffffff);

    foo.x += 20;
    assert_eq(foo.x, 0x20000000000000013);
}

unittest "various types" {
    union Foo { A, B, C }

    packed struct Packed {
        a: Foo,
        b: char,
        c: i4,
        d: u4,
        e: bool,
    }

    mut x = Packed(a: :A, b: 'A', c: -1, d: 2, e: false);
    assert_eq(std::mem::size_of_val(&x), 4);

    assert_eq(x.a as u2, Foo::A as u2);
    assert_eq(x.b, 'A');
    assert_eq(x.c, -1);
    assert_eq(x.d, 2);
    assert_eq(x.e, false);

    x.a = :B;
    x.b = 'B';
    x.c -= 1;
    x.d += 2;
    x.e = !x.e;

    assert_eq(x.a as u2, Foo::B as u2);
    assert_eq(x.b, 'B');
    assert_eq(x.c, -2);
    assert_eq(x.d, 4);
    assert_eq(x.e, true);

    x.a = :C;
    x.b = 'C';
    x.c -= 1;
    x.d += 2;
    x.e = !x.e;

    assert_eq(x.a as u2, Foo::C as u2);
    assert_eq(x.b, 'C');
    assert_eq(x.c, -3);
    assert_eq(x.d, 6);
    assert_eq(x.e, false);
}
