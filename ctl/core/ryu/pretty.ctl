use super::debug_assert;
use super::d2s::*;
use super::f2s::*;
use core::ext::*;

pub unsafe fn format32(f: f32, res: *raw u8): uint {
    unsafe {
        let bits = f.to_bits();
        let sign = (bits >> (FLOAT_MANTISSA_BITS + FLOAT_EXPONENT_BITS)) & 1 != 0;
        let ieee_mantissa = bits & ((1u32 << FLOAT_MANTISSA_BITS) - 1);
        let ieee_exponent = (bits >> FLOAT_MANTISSA_BITS) & ((1u32 << FLOAT_EXPONENT_BITS) - 1);
        mut index = 0i;
        if sign {
            *res = b'-';
            index++;
        }

        if ieee_exponent == 0 and ieee_mantissa == 0 {
            core::mem::copy(dst: res + index, src: b"0.0" as *raw u8, num: 3);
            return sign as uint + 3;
        }

        let {exponent, mantissa} = f2d(ieee_mantissa, ieee_exponent);
        let length = decimal_length9(mantissa) as int;
        let k = exponent as int;
        let kk = length + k; // 10^(kk-1) <= v < 10^kk
        debug_assert(k >= -45);

        if 0 <= k and kk <= 13 {
            // 1234e7 -> 12340000000.0
            write_mantissa(mantissa, res + index + length);
            for i in length..kk {
                *(res + index + i) = b'0';
            }
            *(res + index + kk) = b'.';
            *(res + index + kk + 1) = b'0';
            index as uint + kk as uint + 2
        } else if 0 < kk and kk <= 13 {
            // 1234e-2 -> 12.34
            write_mantissa(mantissa, (res + index + length + 1));
            core::mem::copy_overlapping(
                dst: res + index, 
                src: res + index + 1, 
                num: kk as uint,
            );
            *(res + index + kk) = b'.';
            index as uint + length as uint + 1
        } else if -6 < kk and kk <= 0 {
            // 1234e-6 -> 0.001234
            *(res + index) = b'0';
            *(res + index + 1) = b'.';
            let offset = 2 - kk;
            for i in 2..offset {
                *(res + index + i) = b'0';
            }
            write_mantissa(mantissa, res + index + length + offset);
            index as uint + length as uint + offset as uint
        } else if length == 1 {
            // 1e30
            *(res + index) = b'0' + mantissa as! u8;
            *(res + index + 1) = b'e';
            index as uint + 2 + write_exponent2(kk - 1, res + index + 2)
        } else {
            // 1234e30 -> 1.234e33
            write_mantissa(mantissa, res + index + length + 1);
            *(res + index) = *(res + index + 1);
            *(res + index + 1) = b'.';
            *(res + index + length + 1) = b'e';
            index as uint
                + length as uint
                + 2
                + write_exponent2(kk - 1, res + index + length + 2)
        }
    }
}

pub unsafe fn format64(f: f64, res: *raw u8): uint {
    unsafe {
        let bits = f.to_bits();
        let sign = ((bits >> (DOUBLE_MANTISSA_BITS + DOUBLE_EXPONENT_BITS)) & 1) != 0;
        let ieee_mantissa = bits & ((1u64 << DOUBLE_MANTISSA_BITS) - 1);
        let ieee_exponent =
            (bits >> DOUBLE_MANTISSA_BITS) as! u32 & ((1u32 << DOUBLE_EXPONENT_BITS) - 1);

        mut index = 0i;
        if sign {
            *res = b'-';
            index++;
        }

        if ieee_exponent == 0 and ieee_mantissa == 0 {
            core::mem::copy(dst: res + index, src: b"0.0" as *raw u8, num: 3);
            return sign as uint + 3;
        }

        let {exponent, mantissa} = d2d(ieee_mantissa, ieee_exponent);
        let length = decimal_length17(mantissa) as int;
        let k = exponent as int;
        let kk = length + k; // 10^(kk-1) <= v < 10^kk
        debug_assert(k >= -324);

        if 0 <= k and kk <= 16 {
            // 1234e7 -> 12340000000.0
            write_mantissa_long(mantissa, res + index + length);
            for i in length..kk {
                *(res + index + i) = b'0';
            }
            *(res + index + kk) = b'.';
            *(res + index + kk + 1) = b'0';
            index as uint + kk as uint + 2
        } else if 0 < kk and kk <= 16 {
            // 1234e-2 -> 12.34
            write_mantissa_long(mantissa, res + index + length + 1);
            core::mem::copy_overlapping(
                dst: res + index, 
                src: res + index + 1, 
                num: kk as uint,
            );
            *(res + index + kk) = b'.';
            index as uint + length as uint + 1
        } else if -5 < kk and kk <= 0 {
            // 1234e-6 -> 0.001234
            *(res + index) = b'0';
            *(res + index + 1) = b'.';
            let offset = 2 - kk;
            for i in 2..offset {
                *(res + index + i) = b'0';
            }
            write_mantissa_long(mantissa, res + index + length + offset);
            index as uint + length as uint + offset as uint
        } else if length == 1 {
            // 1e30
            *(res + index) = b'0' + mantissa as! u8;
            *(res + index + 1) = b'e';
            index as uint + 2 + write_exponent3(kk - 1, res + index + 2)
        } else {
            // 1234e30 -> 1.234e33
            write_mantissa_long(mantissa, res + index + length + 1);
            *(res + index) = *(res + index + 1);
            *(res + index + 1) = b'.';
            *(res + index + length + 1) = b'e';
            index as uint
                + length as uint
                + 2
                + write_exponent3(kk - 1, res + index + length + 2)
        }
    }
}

fn decimal_length9(v: u32): u32 {
    // Function precondition: v is not a 10-digit number.
    // (f2s: 9 digits are sufficient for round-tripping.)
    debug_assert(v < 1000000000);

    if v >= 100000000 {
        9
    } else if v >= 10000000 {
        8
    } else if v >= 1000000 {
        7
    } else if v >= 100000 {
        6
    } else if v >= 10000 {
        5
    } else if v >= 1000 {
        4
    } else if v >= 100 {
        3
    } else if v >= 10 {
        2
    } else {
        1
    }
}

fn decimal_length17(v: u64): u32 {
    // This is slightly faster than a loop.
    // The average output length is 16.38 digits, so we check high-to-low.
    // Function precondition: v is not an 18, 19, or 20-digit number.
    // (17 digits are sufficient for round-tripping.)
    debug_assert(v < 100000000000000000);

    if v >= 10000000000000000 {
        17
    } else if v >= 1000000000000000 {
        16
    } else if v >= 100000000000000 {
        15
    } else if v >= 10000000000000 {
        14
    } else if v >= 1000000000000 {
        13
    } else if v >= 100000000000 {
        12
    } else if v >= 10000000000 {
        11
    } else if v >= 1000000000 {
        10
    } else if v >= 100000000 {
        9
    } else if v >= 10000000 {
        8
    } else if v >= 1000000 {
        7
    } else if v >= 100000 {
        6
    } else if v >= 10000 {
        5
    } else if v >= 1000 {
        4
    } else if v >= 100 {
        3
    } else if v >= 10 {
        2
    } else {
        1
    }
}

// A table of all two-digit numbers. This is used to speed up decimal digit
// generation by copying pairs of digits into the final output.
static DIGIT_TABLE: [u8; 200] = *b"00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899";

unsafe fn write_exponent3(mut k: int, mut res: *raw u8): uint {
    unsafe {
        let sign = k < 0;
        if sign {
            *res++ = b'-';
            k = -k;
        }

        debug_assert(k < 1000);
        if k >= 100 {
            *res = b'0' + (k / 100) as! u8;
            k %= 100;
            core::mem::copy(dst: res + 1, src: &raw DIGIT_TABLE[k * 2], num: 2);
            sign as uint + 3
        } else if k >= 10 {
            core::mem::copy(dst: res, src: &raw DIGIT_TABLE[k * 2], num: 2);
            sign as uint + 2
        } else {
            *res = b'0' + k as! u8;
            sign as uint + 1
        }
    }
}

unsafe fn write_exponent2(mut k: int, mut res: *raw u8): uint {
    unsafe {
        let sign = k < 0;
        if sign {
            *res++ = b'-';
            k = -k;
        }

        debug_assert(k < 100);
        if k >= 10 {
            core::mem::copy(dst: res, src: &raw DIGIT_TABLE[k * 2], num: 2);
            sign as uint + 2
        } else {
            *res = b'0' + k as! u8;
            sign as uint + 1
        }
    }
}

unsafe fn write_mantissa_long(mut output: u64, mut res: *raw u8) {
    unsafe {
        if output >> 32 != 0 {
            // One expensive 64-bit division.
            mut output2 = (output - (output / 100000000) * 100000000) as! u32;
            output /= 100000000;

            let c = output2 % 10000;
            output2 /= 10000;
            let d = output2 % 10000;
            core::mem::copy(dst: res - 2, src: &raw DIGIT_TABLE[(c % 100) << 1], num: 2);
            core::mem::copy(dst: res - 4, src: &raw DIGIT_TABLE[(c / 100) << 1], num: 2);
            core::mem::copy(dst: res - 6, src: &raw DIGIT_TABLE[(d % 100) << 1], num: 2);
            core::mem::copy(dst: res - 8, src: &raw DIGIT_TABLE[(d / 100) << 1], num: 2);
            res -= 8;
        }
        write_mantissa(output as! u32, res);
    }
}

unsafe fn write_mantissa(mut output: u32, mut res: *raw u8) {
    unsafe {
        while output >= 10000 {
            let c = output - (output / 10000) * 10000;
            output /= 10000;
            core::mem::copy(dst: res - 2, src: &raw DIGIT_TABLE[(c % 100) << 1], num: 2);
            core::mem::copy(dst: res - 4, src: &raw DIGIT_TABLE[(c / 100) << 1], num: 2);
            res -= 4;
        }

        if output >= 100 {
            let c = (output % 100) << 1;
            output /= 100;
            core::mem::copy(dst: res - 2, src: &raw DIGIT_TABLE[c], num: 2);
            res -= 2;
        }

        if output >= 10 {
            core::mem::copy(dst: res - 2, src: &raw DIGIT_TABLE[output << 1], num: 2);
        } else {
            *(res - 1) = b'0' + output as! u8;
        }
    }
}
