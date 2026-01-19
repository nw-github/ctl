use super::common::*;
use super::d2s;
use super::d2s_intrinsics::*;
use super::d2s_full_table::*;

const DOUBLE_EXPONENT_BIAS: uint = 1023;

fn floor_log2(value: u64): u32 => 63u32.wrapping_sub(value.leading_zeros());

$[inline(always)]
fn infinity(signed: bool): f64 {
    let ieee = ((signed as u64) << (d2s::DOUBLE_EXPONENT_BITS + d2s::DOUBLE_MANTISSA_BITS))
        | (0x7ff << d2s::DOUBLE_MANTISSA_BITS);
    f64::from_bits(ieee)
}

pub fn s2d(buffer: [u8..]): Result<f64, ParseError> {
    guard !buffer.is_empty() else {
        return :Err(:InputTooShort);
    }

    let len = buffer.len();
    mut [m10digits, e10digits] = [0i32; 2];
    mut [dot_index, e_index] = [len; 2];
    mut m10 = 0u64;
    mut e10 = 0i32;
    mut [signed_m, signed_e] = [false; 2];
    mut i = 0u;
    if unsafe *buffer.get_unchecked(0) == b'-' {
        signed_m = true;
        i++;
    }

    while buffer.get(i).copied() is ?c {
        if c == b'.' {
            guard dot_index == len else {
                return :Err(:MalformedInput);
            }

            dot_index = i;
            i++;
            continue;
        } else if c < b'0' or c > b'9' {
            break;
        } else if m10digits >= 17 {
            return :Err(:InputTooLong);
        }

        m10 = 10 * m10 + (c - b'0') as u64;
        if m10 != 0 {
            m10digits++;
        }

        i++;
    }

    if buffer.get(i) is Some(b'e' | b'E') {
        e_index = i++;
        match buffer.get(i) {
            ?b'-' => { signed_e = true; i++; }
            ?b'+' => { i++; },
            _ => {}
        }

        while buffer.get(i).copied() is ?c {
            if c < b'0' or c > b'9' {
                return :Err(:MalformedInput);
            } else if e10digits > 3 {
                return :Err(:InputTooLong);
            }

            e10 = 10 * e10 + (c - b'0') as i32;
            if e10 != 0 {
                e10digits++;
            }
            i++;
        }
    }

    guard i >= len else {
        return :Err(:MalformedInput);
    }

    if signed_e {
        e10 = -e10;
    }

    e10 -= dot_index < e_index then (e_index - dot_index - 1).cast() else 0;
    if m10 == 0 {
        return :Ok(signed_m then -0.0 else 0.0);
    } else if m10digits + e10 <= -324 or m10 == 0 /* ??? */ {
        let ieee = (signed_m as u64) << (d2s::DOUBLE_EXPONENT_BITS + d2s::DOUBLE_MANTISSA_BITS);
        return :Ok(f64::from_bits(ieee));
    } else if m10digits + e10 >= 310 {
        return :Ok(infinity(signed_m));
    }

    // Convert to binary float m2 * 2^e2, while retaining information about whether the conversion
    // was exact (trailing_zeros).
    let (e2, m2, mut trailing_zeros) = if e10 >= 0 {
        // The length of m * 10^e in bits is:
        //   log2(m10 * 10^e10) = log2(m10) + e10 log2(10) = log2(m10) + e10 + e10 * log2(5)
        //
        // We want to compute the DOUBLE_MANTISSA_BITS + 1 top-most bits (+1 for
        // the implicit leading one in IEEE format). We therefore choose a
        // binary output exponent of
        //   log2(m10 * 10^e10) - (DOUBLE_MANTISSA_BITS + 1).
        //
        // We use floor(log2(5^e10)) so that we get at least this many bits;
        // better to have an additional bit than to not have enough bits.
        let e2: i32 = floor_log2(m10)
            .wrapping_add(e10.cast())
            .wrapping_add(log2_pow5(e10).cast())
            .wrapping_sub(d2s::DOUBLE_MANTISSA_BITS + 1)
            .cast();

        // We now compute [m10 * 10^e10 / 2^e2] = [m10 * 5^e10 / 2^(e2-e10)].
        // To that end, we use the DOUBLE_POW5_SPLIT table.
        let j = e2
            .wrapping_sub(e10)
            .wrapping_sub(ceil_log2_pow5(e10))
            .wrapping_add(d2s::DOUBLE_POW5_BITCOUNT);
        debug_assert(j >= 0);
        debug_assert(e10 < DOUBLE_POW5_SPLIT.len().cast());
        let m2 = mul_shift_64(
            m10,
            unsafe DOUBLE_POW5_SPLIT.get_unchecked(e10.cast()),
            j.cast(),
        );

        // We also compute if the result is exact, i.e.,
        //   [m10 * 10^e10 / 2^e2] == m10 * 10^e10 / 2^e2.
        // This can only be the case if 2^e2 divides m10 * 10^e10, which in turn
        // requires that the largest power of 2 that divides m10 + e10 is
        // greater than e2. If e2 is less than e10, then the result must be
        // exact. Otherwise we use the existing multiple_of_power_of_2 function.
        let trailing_zeros =
            e2 < e10 or e2 - e10 < 64 and multiple_of_power_of_2(m10, (e2 - e10).cast());
        (e2, m2, trailing_zeros)
    } else {
        let e2: i32 = floor_log2(m10)
            .wrapping_add(e10.cast())
            .wrapping_sub(ceil_log2_pow5(-e10).cast())
            .wrapping_sub(d2s::DOUBLE_MANTISSA_BITS + 1)
            .cast();
        let j = e2
            .wrapping_sub(e10)
            .wrapping_add(ceil_log2_pow5(-e10))
            .wrapping_sub(1)
            .wrapping_add(d2s::DOUBLE_POW5_INV_BITCOUNT);
        debug_assert(-e10 < DOUBLE_POW5_INV_SPLIT.len().cast());
        let m2 = mul_shift_64(
            m10,
            unsafe DOUBLE_POW5_INV_SPLIT.get_unchecked((-e10).cast()),
            j.cast(),
        );
        let trailing_zeros = multiple_of_power_of_5(m10, (-e10).cast());
        (e2, m2, trailing_zeros)
    };

    // Compute the final IEEE exponent.
    mut ieee_e2: u32 = i32::max(0, e2 + DOUBLE_EXPONENT_BIAS.cast() + floor_log2(m2).cast()).cast();
    if ieee_e2 > 0x7fe {
        // Final IEEE exponent is larger than the maximum representable; return +/-Infinity.
        return :Ok(infinity(signed_m));
    }

    // We need to figure out how much we need to shift m2. The tricky part is
    // that we need to take the final IEEE exponent into account, so we need to
    // reverse the bias and also special-case the value 0.
    let shift = (ieee_e2 == 0 then 1i32 else ieee_e2.cast())
        .wrapping_sub(e2)
        .wrapping_sub(DOUBLE_EXPONENT_BIAS.cast())
        .wrapping_sub(d2s::DOUBLE_MANTISSA_BITS.cast());
    debug_assert(shift >= 0);
    let shift = u32::from(shift);

    // We need to round up if the exact value is more than 0.5 above the value
    // we computed. That's equivalent to checking if the last removed bit was 1
    // and either the value was not just trailing zeros or the result would
    // otherwise be odd.
    //
    // We need to update trailing_zeros given that we have the exact output
    // exponent ieee_e2 now.
    trailing_zeros &= (m2 & ((1 << (shift - 1)) - 1)) == 0;
    let last_removed_bit = (m2 >> (shift - 1)) & 1;
    let round_up = last_removed_bit != 0 and (!trailing_zeros or ((m2 >> shift) & 1) != 0);

    let mut ieee_m2 = (m2 >> shift).wrapping_add(round_up as u64);
    debug_assert(ieee_m2 <= 1 << (d2s::DOUBLE_MANTISSA_BITS + 1));
    ieee_m2 &= (1 << d2s::DOUBLE_MANTISSA_BITS) - 1;
    if ieee_m2 == 0 and round_up {
        // Due to how the IEEE represents +/-Infinity, we don't need to check for overflow here.
        ieee_e2++;
    }

    let ieee = ((((signed_m as u64) << d2s::DOUBLE_EXPONENT_BITS) | ieee_e2 as u64)
        << d2s::DOUBLE_MANTISSA_BITS) | ieee_m2;
    :Ok(f64::from_bits(ieee))
}

// https://github.com/dtolnay/ryu/blob/master/tests/s2d_test.rs
mod test {
    use super::ParseError;

    unittest "bad input" {
        assert_eq(ParseError::MalformedInput, super::s2d("x".as_bytes()).unwrap_err());
        assert_eq(ParseError::MalformedInput, super::s2d("1..1".as_bytes()).unwrap_err());
        assert_eq(ParseError::MalformedInput, super::s2d("..".as_bytes()).unwrap_err());
        assert_eq(ParseError::MalformedInput, super::s2d("1..1".as_bytes()).unwrap_err());
        assert_eq(ParseError::MalformedInput, super::s2d("1ee1".as_bytes()).unwrap_err());
        assert_eq(ParseError::MalformedInput, super::s2d("1e.1".as_bytes()).unwrap_err());
        assert_eq(ParseError::InputTooShort,  super::s2d("".as_bytes()).unwrap_err());
        assert_eq(ParseError::InputTooLong,   super::s2d("123456789012345678".as_bytes()).unwrap_err());
        assert_eq(ParseError::InputTooLong,   super::s2d("1e12345".as_bytes()).unwrap_err());
    }

    unittest "basic" {
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

    unittest "min max" {
        assert_eq(1.7976931348623157e308, f64::parse("1.7976931348623157e308").unwrap());
        assert_eq(5E-324, f64::parse("5E-324").unwrap());
    }

    unittest "mantissa rounding overflow" {
        // This results in binary mantissa that is all ones and requires rounding up
        // because it is closer to 1 than to the next smaller float. This is a
        // regression test that the mantissa overflow is handled correctly by
        // increasing the exponent.
        assert_eq(1.0, f64::parse("0.99999999999999999").unwrap());
        // This number overflows the mantissa *and* the IEEE exponent.
        assert_eq(f64::inf(), f64::parse("1.7976931348623159e308").unwrap());
    }

    unittest "underflow" {
        assert_eq(0.0, f64::parse("2.4e-324").unwrap());
        assert_eq(0.0, f64::parse("1e-324").unwrap());
        assert_eq(0.0, f64::parse("9.99999e-325").unwrap());
        // These are just about halfway between 0 and the smallest float.
        // The first is just below the halfway point, the second just above.
        assert_eq(0.0, f64::parse("2.4703282292062327e-324").unwrap());
        assert_eq(5e-324, f64::parse("2.4703282292062328e-324").unwrap());
    }

    unittest "overflow" {
        assert_eq(f64::inf(), f64::parse("2e308").unwrap());
        assert_eq(f64::inf(), f64::parse("1e309").unwrap());
    }

    unittest "table size denormal" {
        assert_eq(5e-324, f64::parse("4.9406564584124654e-324").unwrap());
    }

    unittest "issue157" {
        assert_eq(1.2999999999999999E+154, f64::parse("1.2999999999999999E+154").unwrap());
    }

    unittest "issue173" {
        // Denormal boundary
        assert_eq(2.2250738585072012e-308, f64::parse("2.2250738585072012e-308").unwrap());
        assert_eq(2.2250738585072013e-308, f64::parse("2.2250738585072013e-308").unwrap());
        assert_eq(2.2250738585072014e-308, f64::parse("2.2250738585072014e-308").unwrap());
    }
}
