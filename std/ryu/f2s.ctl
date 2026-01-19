use super::common::*;
use super::f2s_intrinsics::*;
use super::pretty::*;

pub const FLOAT_MANTISSA_BITS: u32 = 23;
pub const FLOAT_EXPONENT_BITS: u32 = 8;
const FLOAT_BIAS: i32 = 127;

// A floating decimal representing m * 10^e.
pub struct FloatingDecimal32 {
    pub mantissa: u32,
    // Decimal exponent's range is -45 to 38 inclusive, and can fit in i16 if needed.
    pub exponent: i32,
}

pub fn f2d(ieee_mantissa: u32, ieee_exponent: u32): FloatingDecimal32 {
    let (e2, m2) = if ieee_exponent == 0 {
        (
            // We subtract 2 so that the bounds computation has 2 additional bits.
            1i32 - FLOAT_BIAS - FLOAT_MANTISSA_BITS.cast() - 2,
            ieee_mantissa,
        )
    } else {
        (
            ieee_exponent.cast() - FLOAT_BIAS - FLOAT_MANTISSA_BITS.cast() - 2,
            (1u32 << FLOAT_MANTISSA_BITS) | ieee_mantissa,
        )
    };
    let accept_bounds = m2 & 1 == 0;

    // Step 2: Determine the interval of valid decimal representations.
    let mv = m2 * 4;
    let mp = m2 * 4 + 2;
    // Implicit bool -> int conversion. True is 1, false is 0.
    let mm_shift = (ieee_mantissa != 0 or ieee_exponent <= 1) as u32;
    let mm = m2 * 4 - 1 - mm_shift;

    // Step 3: Convert to a decimal power base using 64-bit arithmetic.
    mut vr: u32;
    mut vp: u32;
    mut vm: u32;
    mut e10: i32;
    mut vm_is_trailing_zeros = false;
    mut vr_is_trailing_zeros = false;
    mut last_removed_digit = 0u8;
    if e2 >= 0 {
        let q = log10_pow2(e2);
        e10 = q.cast();
        let k = FLOAT_POW5_INV_BITCOUNT + pow5bits(q.cast()) - 1;
        let i = -e2 + q.cast() + k;
        vr = mul_pow5_inv_div_pow2(mv, q, i);
        vp = mul_pow5_inv_div_pow2(mp, q, i);
        vm = mul_pow5_inv_div_pow2(mm, q, i);
        if q != 0 and (vp - 1) / 10 <= vm / 10 {
            // We need to know one removed digit even if we are not going to loop below. We could use
            // q = X - 1 above, except that would require 33 bits for the result, and we've found that
            // 32-bit arithmetic is faster even on 64-bit machines.
            let l = FLOAT_POW5_INV_BITCOUNT + pow5bits(q.cast() - 1) - 1;
            last_removed_digit =
                (mul_pow5_inv_div_pow2(mv, q - 1, -e2 + q.cast() - 1 + l) % 10).cast();
        }
        if q <= 9 {
            // The largest power of 5 that fits in 24 bits is 5^10, but q <= 9 seems to be safe as well.
            // Only one of mp, mv, and mm can be a multiple of 5, if any.
            if mv % 5 == 0 {
                vr_is_trailing_zeros = multiple_of_power_of_5_32(mv, q);
            } else if accept_bounds {
                vm_is_trailing_zeros = multiple_of_power_of_5_32(mm, q);
            } else {
                vp -= multiple_of_power_of_5_32(mp, q) as u32;
            }
        }
    } else {
        let q = log10_pow5(-e2);
        e10 = i32::from(q) + e2;
        let i = -e2 - i32::from(q);
        let k = pow5bits(i) - FLOAT_POW5_BITCOUNT;
        mut j = i32::from(q) - k;
        vr = mul_pow5_div_pow2(mv, i.cast(), j);
        vp = mul_pow5_div_pow2(mp, i.cast(), j);
        vm = mul_pow5_div_pow2(mm, i.cast(), j);
        if q != 0 and (vp - 1) / 10 <= vm / 10 {
            j = q.cast() - 1 - (pow5bits(i + 1) - FLOAT_POW5_BITCOUNT);
            last_removed_digit = (mul_pow5_div_pow2(mv, (i + 1).cast(), j) % 10).cast();
        }
        if q <= 1 {
            // {vr,vp,vm} is trailing zeros if {mv,mp,mm} has at least q trailing 0 bits.
            // mv = 4 * m2, so it always has at least two trailing 0 bits.
            vr_is_trailing_zeros = true;
            if accept_bounds {
                // mm = mv - 1 - mm_shift, so it has 1 trailing 0 bit iff mm_shift == 1.
                vm_is_trailing_zeros = mm_shift == 1;
            } else {
                // mp = mv + 2, so it always has at least one trailing 0 bit.
                vp--;
            }
        } else if q < 31 {
            // TODO(ulfjack): Use a tighter bound here.
            vr_is_trailing_zeros = multiple_of_power_of_2_32(mv, q - 1);
        }
    }

    // Step 4: Find the shortest decimal representation in the interval of valid representations.
    mut removed = 0i32;
    let output = if vm_is_trailing_zeros or vr_is_trailing_zeros {
        // General case, which happens rarely (~4.0%).
        while vp / 10 > vm / 10 {
            vm_is_trailing_zeros &= vm - (vm / 10) * 10 == 0;
            vr_is_trailing_zeros &= last_removed_digit == 0;
            last_removed_digit = (vr % 10).cast();
            vr /= 10;
            vp /= 10;
            vm /= 10;
            removed++;
        }
        if vm_is_trailing_zeros {
            while vm % 10 == 0 {
                vr_is_trailing_zeros &= last_removed_digit == 0;
                last_removed_digit = (vr % 10).cast();
                vr /= 10;
                vp /= 10;
                vm /= 10;
                removed++;
            }
        }
        if vr_is_trailing_zeros and last_removed_digit == 5 and vr % 2 == 0 {
            // Round even if the exact number is .....50..0.
            last_removed_digit = 4;
        }
        // We need to take vr + 1 if vr is outside bounds or we need to round up.
        vr + ((vr == vm and (!accept_bounds or !vm_is_trailing_zeros)) or last_removed_digit >= 5)
            as u32
    } else {
        // Specialized for the common case (~96.0%). Percentages below are relative to this.
        // Loop iterations below (approximately):
        // 0: 13.6%, 1: 70.7%, 2: 14.1%, 3: 1.39%, 4: 0.14%, 5+: 0.01%
        while vp / 10 > vm / 10 {
            last_removed_digit = (vr % 10).cast();
            vr /= 10;
            vp /= 10;
            vm /= 10;
            removed++;
        }
        // We need to take vr + 1 if vr is outside bounds or we need to round up.
        vr + (vr == vm or last_removed_digit >= 5) as u32
    };

    FloatingDecimal32(
        exponent: e10 + removed,
        mantissa: output,
    )
}
