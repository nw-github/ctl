static OUTER: int = 10;

unittest "basic" {
    // Output: 30

    fn func(): int => OUTER;

    static INNER: int = 20;
    assert_eq(func() + INNER, 30);
}

unittest "order" {
    static FOO: int = BAR + 5;
    static BAR: int = 10;

    static BAZ: int = 10;
    static QUUX: int = BAZ + 5;

    assert_eq(FOO, 15);
    assert_eq(BAR, 10);
    assert_eq(BAZ, 10);
    assert_eq(QUUX, 15);
}
