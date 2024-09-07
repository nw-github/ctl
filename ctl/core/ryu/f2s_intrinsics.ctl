use super::debug_assert;
use super::d2s;
use super::d2s_full_table::*;

pub const FLOAT_POW5_INV_BITCOUNT: i32 = 61; // d2s::DOUBLE_POW5_INV_BITCOUNT - 64;
pub const FLOAT_POW5_BITCOUNT: i32 = 61; //d2s::DOUBLE_POW5_BITCOUNT - 64;

fn pow5factor_32(mut value: u32): u32 {
    mut count = 0u32;
    loop {
        debug_assert(value != 0);
        let q = value / 5;
        let r = value % 5;
        if r != 0 {
            break;
        }
        value = q;
        count++;
    }
    count
}

// Returns true if value is divisible by 5^p.
pub fn multiple_of_power_of_5_32(value: u32, p: u32): bool {
    pow5factor_32(value) >= p
}

// Returns true if value is divisible by 2^p.
pub fn multiple_of_power_of_2_32(value: u32, p: u32): bool {
    // __builtin_ctz doesn't appear to be faster here.
    value & ((1u32 << p) - 1) == 0
}

// It seems to be slightly faster to avoid uint128_t here, although the
// generated code for uint128_t looks slightly nicer.
fn mul_shift_32(m: u32, factor: u64, shift: i32): u32 {
    debug_assert(shift > 32);

    // The casts here help MSVC to avoid calls to the __allmul library function.
    let factor_lo = factor as! u32;
    let factor_hi = (factor >> 32) as! u32;
    let bits0 = m as u64 * factor_lo as u64;
    let bits1 = m as u64 * factor_hi as u64;

    let sum = (bits0 >> 32) + bits1;
    let shifted_sum = sum >> (shift - 32) as u32;
    debug_assert(shifted_sum <= u32::max_value() as u64);
    shifted_sum as! u32
}

pub fn mul_pow5_inv_div_pow2(m: u32, q: u32, j: i32): u32 {
    mul_shift_32(m, DOUBLE_POW5_INV_SPLIT[q].1 + 1, j)
}

pub fn mul_pow5_div_pow2(m: u32, i: u32, j: i32): u32 {
    mul_shift_32(m, DOUBLE_POW5_SPLIT[i].1, j)
}
