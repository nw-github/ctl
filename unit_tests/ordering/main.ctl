fn double(n: *mut int): int {
    let old = *n;
    *n *= 2;
    old
}

fn add(a: int, b: int): int => a + b;

unittest "increment" {
    for _ in 0..=2 {
        mut x = 1;
        let y = x++  // x = 2, ret = 1
                + 
                ++x; // x = 3, ret = 3

        assert_eq(x, 3);
        assert_eq(y, 4);
    }
}

unittest "instance" {
    struct A { x: int, y: int }

    mut x = 1;
    let y = add(
        A(x: x++,           // x = 2, ret = 1
          y: double(&mut x) // x = 4, ret = 2
        ).x,                // 1
        double(&mut x)      // x = 8, ret = 4
    );

    assert_eq(x, 8);
    assert_eq(y, 5);
}

unittest "keyword call" {
    fn test(a: int, b: int, c: int) {
        assert_eq(a, 2);
        assert_eq(b, 2);
        assert_eq(c, 3);
    }

    mut x = 0;
    for _ in 0..=2 {
        test(
            c: {x = 3; x},
            b: --x,
            a: x--,
        );
    }

    assert_eq(x, 1);
}

unittest "kw first" {
    fn regular(a: *str, b: i32) {
        assert_eq(*a, "hello");
        assert_eq(b, 10);
    }

    regular(a: &"hello", 10);
}

unittest "positional call" {
    for _ in 0..=2 {
        mut x = 1;
        mut y = double(&mut x)  // x = 2, ret = 1
            + add(
                double(&mut x), // x = 4, ret = 2
                double(&mut x), // x = 8, ret = 4
            );                       // x = 8, ret = 6

        assert_eq(x, 8);
        assert_eq(y, 7);
    }
}

unittest "one kw parameter" {
    fn hello(x: int, kw y: int, z: int) {
        assert_eq(x, 10);
        assert_eq(y, 11);
        assert_eq(z, 12);
    }

    hello(y: 11, 10, 12);
    hello(10, y: 11, 12);
    hello(10, 12, y: 11);
}

unittest "blocks" {
    fn double(n: *mut int): int => std::mem::replace(n, *n * 2);

    fn test(trigger_else: bool): (int, int) {
        let val2 = if !trigger_else { 2 } else { 0 };

        mut x = 1;
        let y = if { double(&mut x) == 2 } {   // should always fail
            1
        } else if { double(&mut x) == val2 } { // succeeds if trigger else
            2
        } else {
            double(&mut x)
        };

        (x, y)
    }

    let (a, b) = test(false);
    assert_eq(a, 4);
    assert_eq(b, 2);

    let (a, b) = test(true);
    assert_eq(a, 8);
    assert_eq(b, 4);
}
