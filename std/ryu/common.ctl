pub union ParseError {
    InputTooShort,
    InputTooLong,
    MalformedInput,
}

// Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
pub fn pow5bits(e: i32): i32 /* or u32 -> u32 */ {
    // This approximation works up to the point that the multiplication
    // overflows at e = 3529. If the multiplication were done in 64 bits, it
    // would fail at 5^4004 which is just greater than 2^9297.
    debug_assert(e >= 0);
    debug_assert(e <= 3528);
    (((e as! u32 * 1217359) >> 19) + 1) as! i32
}

// Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
pub fn log10_pow2(e: i32): u32 /* or u32 -> u32 */ {
    // The first value this approximation fails for is 2^1651 which is just greater than 10^297.
    debug_assert(e >= 0);
    debug_assert(e <= 1650);
    (e as! u32 * 78913) >> 18
}

// Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
pub fn log10_pow5(e: i32): u32 /* or u32 -> u32 */ {
    // The first value this approximation fails for is 5^2621 which is just greater than 10^1832.
    debug_assert(e >= 0);
    debug_assert(e <= 2620);
    (e as! u32 * 732923) >> 20
}

// Returns e == 0 ? 1 : [log_2(5^e)]; requires 0 <= e <= 3528.
pub fn log2_pow5(e: i32): i32 /* or u32 -> u32 */ {
    // This approximation works up to the point that the multiplication
    // overflows at e = 3529. If the multiplication were done in 64 bits, it
    // would fail at 5^4004 which is just greater than 2^9297.
    debug_assert(e >= 0);
    debug_assert(e <= 3528);
    ((e as! u32 * 1217359) >> 19) as! i32
}

pub fn ceil_log2_pow5(e: i32): i32 /* or u32 -> u32 */ {
    log2_pow5(e) + 1
}
