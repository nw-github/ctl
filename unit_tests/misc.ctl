unittest "array_ptr_deref" {
    let x = &[1, 2];
    let y = *x;

    assert_eq(x[0], 1);
    assert_eq(x[1], 2);
    assert_eq(y[0], 1);
    assert_eq(y[1], 2);
    assert(!std::ptr::eq(x, &y));

    let x = *b"ab";
    assert_eq(x[0], 97);
    assert_eq(x[1], 98);
}

unittest "boolean" {
    let x = 5 == 2;
    assert_eq(x, false);
    assert_eq(!x, true);
    assert_eq(!!x, false);
}

unittest "if expression" {
    fn opt_one_branch(a: bool): ?i64 {
        if a { 5 }
    }

    fn opt_two_branches(a: bool, b: bool): ?i64 {
        if a {
            10
        } else if b {
            15
        }
    }

    fn opt_else(a: bool, b: bool): i64 {
        if a {
            20
        } else if b {
            25
        } else {
            30
        }
    }

    assert_eq(opt_one_branch(false), null);
    assert_eq(opt_one_branch(true), Some(5));

    assert_eq(opt_two_branches(false, false), null);
    assert_eq(opt_two_branches(false, true), Some(15));
    assert_eq(opt_two_branches(true, false), Some(10));
    assert_eq(opt_two_branches(true, true), Some(10));

    assert_eq(opt_else(false, false), 30);
    assert_eq(opt_else(false, true), 25);
    assert_eq(opt_else(true, false), 20);
    assert_eq(opt_else(true, true), 20);
}

unittest "nested array" {
    mut arr2 = [[1, 2], [3, 4]];
    arr2[0] = [5, 6];

    let ref = &arr2[0];
    let copy = arr2[0];
    assert_eq("arr2[0] (ref): {ref:?}".to_str(), "arr2[0] (ref): [5, 6]");
    assert_eq("arr2[0] (val): {copy:?}".to_str(), "arr2[0] (val): [5, 6]");
    assert_eq("arr2: {arr2:?}".to_str(), "arr2: [[5, 6], [3, 4]]");

    mut arr3 = [arr2];
    assert_eq("arr3: {arr3:?}".to_str(), "arr3: [[[5, 6], [3, 4]]]");

    arr3[0] = [[7, 8], [9, 0]];
    assert_eq("arr3: {arr3:?}".to_str(), "arr3: [[[7, 8], [9, 0]]]");

    assert_eq("arr2: {arr2:?}".to_str(), "arr2: [[5, 6], [3, 4]]");
}

unittest "variant inference" {
    union Foo {
        A,
        B(i32),
        C(x: i32)
    }

    match Foo::A {
        :A => return,
        :B(_x) => panic("fail"),
        :C{x: _} => panic("fail"),
    }

    panic("fail");
}

unittest "float literals" {
    let a = [
        10e4,
        10e+5,
        10e-5,

        10.0e5,
        10.5e+5,
        10.5e-5,
    ];

    let b = [
        10e4f64,
        10e+5f64,
        10e-5f64,

        10.0E5f64,
        10.5e+5f64,
        10.5e-5f64,
    ];

    let c = [
        10e4f32,
        10e+5f32,
        10e-5f32,

        10.0e5f32,
        10.5e+5f32,
        10.5e-5f32,
    ];

    let d = [10., 20.];
    assert_eq("{a:?}".to_str(), "[100000.0, 1000000.0, 0.0001, 1000000.0, 1050000.0, 0.000105]");
    assert_eq("{b:?}".to_str(), "[100000.0, 1000000.0, 0.0001, 1000000.0, 1050000.0, 0.000105]");
    assert_eq("{c:?}".to_str(), "[100000.0, 1000000.0, 0.0001, 1000000.0, 1050000.0, 0.000105]");
    assert_eq("{d:?}".to_str(), "[10.0, 20.0]");
}
