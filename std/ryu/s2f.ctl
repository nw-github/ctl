use super::common::*;
use super::f2s;
use super::debug_assert;
use super::f2s_intrinsics::*;

const FLOAT_EXPONENT_BIAS: uint = 127;

fn floor_log2(value: u32): u32 => 31u32.wrapping_sub(value.leading_zeros());

@(inline(always))
fn infinity(signed: bool): f32 {
    let ieee = ((signed as u32) << (f2s::FLOAT_EXPONENT_BITS + f2s::FLOAT_MANTISSA_BITS))
        | (0xff << f2s::FLOAT_MANTISSA_BITS);
    f32::from_bits(ieee)
}

pub fn s2f(buffer: [u8..]): Result<f32, ParseError> {
    guard !buffer.is_empty() else {
        return :Err(:InputTooShort);
    }

    let len = buffer.len();
    mut [m10digits, e10digits] = [0i32; 2];
    mut [dot_index, e_index] = [len; 2];
    mut [signed_m, signed_e] = [false; 2];
    mut m10 = 0u32;
    mut e10 = 0i32;
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
        } else if m10digits >= 9 {
            return :Err(:InputTooLong);
        }

        m10 = 10 * m10 + (c - b'0') as u32;
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
                // TODO: Be more lenient. Return +/-Infinity or +/-0 instead.
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

    e10 -= dot_index < e_index then (e_index - dot_index - 1) as! i32 else 0;
    if m10 == 0 {
        return :Ok(signed_m then -0.0 else 0.0);
    } else if m10digits + e10 <= -46 or m10 == 0 /* ??? */ {
        // Number is less than 1e-46, which should be rounded down to 0; return +/-0.0.
        let ieee = (signed_m as u32) << (f2s::FLOAT_EXPONENT_BITS + f2s::FLOAT_MANTISSA_BITS);
        return :Ok(f32::from_bits(ieee));
    } else if m10digits + e10 >= 40 {
        // Number is larger than 1e+39, which should be rounded to +/-Infinity.
        return :Ok(infinity(signed_m));
    }

    // Convert to binary float m2 * 2^e2, while retaining information about
    // whether the conversion was exact (trailing_zeros).
    let (e2, m2, mut trailing_zeros) = if e10 >= 0 {
        // The length of m * 10^e in bits is:
        //   log2(m10 * 10^e10) = log2(m10) + e10 log2(10) = log2(m10) + e10 + e10 * log2(5)
        //
        // We want to compute the FLOAT_MANTISSA_BITS + 1 top-most bits (+1 for
        // the implicit leading one in IEEE format). We therefore choose a
        // binary output exponent of
        //   log2(m10 * 10^e10) - (FLOAT_MANTISSA_BITS + 1).
        //
        // We use floor(log2(5^e10)) so that we get at least this many bits; better to
        // have an additional bit than to not have enough bits.
        let e2 = floor_log2(m10)
            .wrapping_add(e10 as! u32)
            .wrapping_add(log2_pow5(e10) as! u32)
            .wrapping_sub(f2s::FLOAT_MANTISSA_BITS + 1) as! i32;

        // We now compute [m10 * 10^e10 / 2^e2] = [m10 * 5^e10 / 2^(e2-e10)].
        // To that end, we use the FLOAT_POW5_SPLIT table.
        let j = e2
            .wrapping_sub(e10)
            .wrapping_sub(ceil_log2_pow5(e10))
            .wrapping_add(FLOAT_POW5_BITCOUNT);
        debug_assert(j >= 0);
        let m2 = mul_pow5_div_pow2(m10, e10 as! u32, j);

        // We also compute if the result is exact, i.e.,
        //   [m10 * 10^e10 / 2^e2] == m10 * 10^e10 / 2^e2.
        // This can only be the case if 2^e2 divides m10 * 10^e10, which in turn
        // requires that the largest power of 2 that divides m10 + e10 is
        // greater than e2. If e2 is less than e10, then the result must be
        // exact. Otherwise we use the existing multiple_of_power_of_2 function.
        let trailing_zeros =
            e2 < e10 or e2 - e10 < 64 and multiple_of_power_of_2_32(m10, (e2 - e10) as! u32);
        (e2, m2, trailing_zeros)
    } else {
        let e2 = floor_log2(m10)
            .wrapping_add(e10 as! u32)
            .wrapping_sub(ceil_log2_pow5(-e10) as! u32)
            .wrapping_sub(f2s::FLOAT_MANTISSA_BITS + 1) as! i32;

        // We now compute [m10 * 10^e10 / 2^e2] = [m10 / (5^(-e10) 2^(e2-e10))].
        let j = e2
            .wrapping_sub(e10)
            .wrapping_add(ceil_log2_pow5(-e10))
            .wrapping_sub(1)
            .wrapping_add(FLOAT_POW5_INV_BITCOUNT);
        let m2 = mul_pow5_inv_div_pow2(m10, -e10 as! u32, j);

        // We also compute if the result is exact, i.e.,
        //   [m10 / (5^(-e10) 2^(e2-e10))] == m10 / (5^(-e10) 2^(e2-e10))
        //
        // If e2-e10 >= 0, we need to check whether (5^(-e10) 2^(e2-e10))
        // divides m10, which is the case iff pow5(m10) >= -e10 AND pow2(m10) >=
        // e2-e10.
        //
        // If e2-e10 < 0, we have actually computed [m10 * 2^(e10 e2) /
        // 5^(-e10)] above, and we need to check whether 5^(-e10) divides (m10 *
        // 2^(e10-e2)), which is the case iff pow5(m10 * 2^(e10-e2)) = pow5(m10)
        // >= -e10.
        let trailing_zeros =
            (e2 < e10 or (e2 - e10 < 32 and multiple_of_power_of_2_32(m10, (e2 - e10) as! u32)))
                and multiple_of_power_of_5_32(m10, -e10 as! u32);
        (e2, m2, trailing_zeros)
    };

    // Compute the final IEEE exponent.
    let mut ieee_e2 = i32::max(0, e2 + FLOAT_EXPONENT_BIAS as! i32 + floor_log2(m2) as! i32) as! u32;
    if ieee_e2 > 0x7fe {
        // Final IEEE exponent is larger than the maximum representable; return +/-Infinity.
        return :Ok(infinity(signed_m));
    }

    // We need to figure out how much we need to shift m2. The tricky part is
    // that we need to take the final IEEE exponent into account, so we need to
    // reverse the bias and also special-case the value 0.
    let shift = (ieee_e2 == 0 then 1i32 else ieee_e2 as! i32)
        .wrapping_sub(e2)
        .wrapping_sub(FLOAT_EXPONENT_BIAS as! i32)
        .wrapping_sub(f2s::FLOAT_MANTISSA_BITS as! i32) as! u32;
    debug_assert(shift >= 0);

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

    let mut ieee_m2 = (m2 >> shift).wrapping_add(round_up as u32);
    debug_assert(ieee_m2 <= 1 << (f2s::FLOAT_MANTISSA_BITS + 1));
    ieee_m2 &= (1 << f2s::FLOAT_MANTISSA_BITS) - 1;
    if ieee_m2 == 0 and round_up {
        // Rounding up may overflow the mantissa.
        // In this case we move a trailing zero of the mantissa into the exponent.
        // Due to how the IEEE represents +/-Infinity, we don't need to check for overflow here.
        ieee_e2++;
    }

    let ieee = ((((signed_m as u32) << f2s::FLOAT_EXPONENT_BITS) | ieee_e2 as u32)
        << f2s::FLOAT_MANTISSA_BITS) | ieee_m2;
    :Ok(f32::from_bits(ieee))
}