// Output: Pass!

// https://github.com/dtolnay/ryu/blob/master/tests/s2f_test.rs

fn assert_eq(a: f32, b: f32) {
    if a != b {
        panic("assertion failed: {a} != {b}");
    }
}

fn test_basic() {
    assert_eq(0.0, f32::parse("0").unwrap());
    assert_eq(-0.0, f32::parse("-0").unwrap());
    assert_eq(1.0, f32::parse("1").unwrap());
    assert_eq(-1.0, f32::parse("-1").unwrap());
    assert_eq(123456792.0, f32::parse("123456789").unwrap());
    assert_eq(299792448.0, f32::parse("299792458").unwrap());
}

fn test_min_max() {
    // assert_eq(1e-45, f32::parse("1e-45").unwrap());
    // assert_eq(f32::MIN_POSITIVE, f32::parse("1.1754944e-38").unwrap());
    // assert_eq(f32::MAX, f32::parse("3.4028235e+38").unwrap());
}

fn test_mantissa_rounding_overflow() {
    assert_eq(1.0, f32::parse("0.999999999").unwrap());
    assert_eq(f32::inf(), f32::parse("3.4028236e+38").unwrap());
    // assert_eq(1.1754944e-38, f32::parse("1.17549430e-38").unwrap()); // FLT_MIN
    // assert_eq(1.1754944e-38, f32::parse("1.17549431e-38").unwrap());
    // assert_eq(1.1754944e-38, f32::parse("1.17549432e-38").unwrap());
    // assert_eq(1.1754944e-38, f32::parse("1.17549433e-38").unwrap());
    // assert_eq(1.1754944e-38, f32::parse("1.17549434e-38").unwrap());
    // assert_eq(1.1754944e-38, f32::parse("1.17549435e-38").unwrap());
}

fn test_trailing_zeros() {
    assert_eq(26843550.0, f32::parse("26843549.5").unwrap());
    assert_eq(50000004.0, f32::parse("50000002.5").unwrap());
    assert_eq(99999992.0, f32::parse("99999989.5").unwrap());
}

fn main() {
    test_basic();
    test_min_max();
    test_mantissa_rounding_overflow();
    test_trailing_zeros();

    println("Pass!");
}
