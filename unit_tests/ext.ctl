unittest "subscript" {
    extension Test for int {
        pub fn [](this, i: int): int {
            mut base = 1;
            for _ in 0..i {
                base *= 10;
            }
            (this / base) % 10
        }
    }

    assert_eq(1234[0], 4);
    assert_eq(1234[1], 3);
    assert_eq(1234[2], 2);
    assert_eq(1234[3], 1);
}
