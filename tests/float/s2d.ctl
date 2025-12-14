// Output: Pass!

// https://github.com/dtolnay/ryu/blob/master/tests/s2d_test.rs

fn assert_eq<T: std::ops::Eq<T> + std::fmt::Format>(a: T, b: T) {
    if a != b {
        panic("assertion failed: {a} != {b}");
    }
}

fn test_bad_input() {
    // assert_eq(Error::MalformedInput, f64::parse("x").unwrap_err());
    // assert_eq(Error::MalformedInput, f64::parse("1..1").unwrap_err());
    // assert_eq(Error::MalformedInput, f64::parse("..").unwrap_err());
    // assert_eq(Error::MalformedInput, f64::parse("1..1").unwrap_err());
    // assert_eq(Error::MalformedInput, f64::parse("1ee1").unwrap_err());
    // assert_eq(Error::MalformedInput, f64::parse("1e.1").unwrap_err());
    // assert_eq(Error::InputTooShort,  f64::parse("").unwrap_err());
    // assert_eq(Error::InputTooLong,   f64::parse("123456789012345678").unwrap_err());
    // assert_eq(Error::InputTooLong,   f64::parse("1e12345").unwrap_err());
}

fn test_basic() {
    assert_eq(0.0, f64::parse("0").unwrap());
    assert_eq(-0.0, f64::parse("-0").unwrap());
    assert_eq(1.0, f64::parse("1").unwrap());
    assert_eq(2.0, f64::parse("2").unwrap());
    assert_eq(123456789.0, f64::parse("123456789").unwrap());
    assert_eq(123.456, f64::parse("123.456").unwrap());
    assert_eq(123.456, f64::parse("123456e-3").unwrap());
    assert_eq(123.456, f64::parse("1234.56e-1").unwrap());
    assert_eq(1.453, f64::parse("1.453").unwrap());
    assert_eq(1453.0, f64::parse("1.453e+3").unwrap());
    assert_eq(0.0, f64::parse(".0").unwrap());
    assert_eq(1.0, f64::parse("1e0").unwrap());
    assert_eq(1.0, f64::parse("1E0").unwrap());
    assert_eq(1.0, f64::parse("000001.000000").unwrap());
    assert_eq(0.2316419, f64::parse("0.2316419").unwrap());
}

fn test_min_max() {
    assert_eq(1.7976931348623157e308, f64::parse("1.7976931348623157e308").unwrap());
    assert_eq(5E-324, f64::parse("5E-324").unwrap());
}

fn test_mantissa_rounding_overflow() {
    // This results in binary mantissa that is all ones and requires rounding up
    // because it is closer to 1 than to the next smaller float. This is a
    // regression test that the mantissa overflow is handled correctly by
    // increasing the exponent.
    assert_eq(1.0, f64::parse("0.99999999999999999").unwrap());
    // This number overflows the mantissa *and* the IEEE exponent.
    assert_eq(f64::inf(), f64::parse("1.7976931348623159e308").unwrap());
}

fn test_underflow() {
    assert_eq(0.0, f64::parse("2.4e-324").unwrap());
    assert_eq(0.0, f64::parse("1e-324").unwrap());
    assert_eq(0.0, f64::parse("9.99999e-325").unwrap());
    // These are just about halfway between 0 and the smallest float.
    // The first is just below the halfway point, the second just above.
    assert_eq(0.0, f64::parse("2.4703282292062327e-324").unwrap());
    assert_eq(5e-324, f64::parse("2.4703282292062328e-324").unwrap());
}

fn test_overflow() {
    assert_eq(f64::inf(), f64::parse("2e308").unwrap());
    assert_eq(f64::inf(), f64::parse("1e309").unwrap());
}

fn test_table_size_denormal() {
    assert_eq(5e-324, f64::parse("4.9406564584124654e-324").unwrap());
}

fn test_issue157() {
    assert_eq(1.2999999999999999E+154, f64::parse("1.2999999999999999E+154").unwrap());
}

fn test_issue173() {
    // Denormal boundary
    assert_eq(2.2250738585072012e-308, f64::parse("2.2250738585072012e-308").unwrap());
    assert_eq(2.2250738585072013e-308, f64::parse("2.2250738585072013e-308").unwrap());
    assert_eq(2.2250738585072014e-308, f64::parse("2.2250738585072014e-308").unwrap());
}

fn main() {
    test_bad_input();
    test_basic();
    test_min_max();
    test_mantissa_rounding_overflow();
    test_underflow();
    test_overflow();
    test_table_size_denormal();
    test_issue157();
    test_issue173();

    println("Pass!");
}
