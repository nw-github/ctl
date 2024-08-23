use super::debug_assert;
use core::ext::*;

pub fn pow5_factor(mut value: u64): u32 {
    const M_INV_5: u64 = 14757395258967641293; // 5 * m_inv_5 = 1 (mod 2^64)
    const N_DIV_5: u64 = 3689348814741910323; // #{ n | n = 0 (mod 2^64) } = 2^64 / 5

    mut count = 0u32;
    loop {
        debug_assert(value != 0);
        value = value.wrapping_mul(M_INV_5);
        if value > N_DIV_5 {
            break;
        }
        count++;
    }
    count
}

// Returns true if value is divisible by 5^p.
pub fn multiple_of_power_of_5(value: u64, p: u32): bool {
    // I tried a case distinction on p, but there was no performance difference.
    pow5_factor(value) >= p
}

// Returns true if value is divisible by 2^p.
pub fn multiple_of_power_of_2(value: u64, p: u32): bool {
    debug_assert(value != 0);
    debug_assert(p < 64);
    // __builtin_ctzll doesn't appear to be faster here.
    (value & ((1u64 << p) - 1)) == 0
}

pub fn mul_shift_64(m: u64, mul: *(u64, u64), j: u32): u64 {
    let b0 = m as u128 * mul.0 as u128;
    let b2 = m as u128 * mul.1 as u128;
    (((b0 >> 64) + b2) >> (j - 64)) as! u64
}

pub unsafe fn mul_shift_all_64(
    m: u64,
    mul: *(u64, u64),
    j: u32,
    vp: *raw u64,
    vm: *raw u64,
    mm_shift: u32,
): u64 {
    unsafe {
        *vp = mul_shift_64(m * 4 + 2, mul, j);
        *vm = mul_shift_64(m * 4 - 1 - mm_shift as u64, mul, j);
    }
    mul_shift_64(m * 4, mul, j)
}
