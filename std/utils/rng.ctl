// This is a partial port of [fastrand](https://github.com/smol-rs/fastrand?tab=readme-ov-file),
// dual licensed under Apache 2.0/MIT.

use std::mem::Uninit;

pub struct Rng {
    state: u64,

    pub fn seeded_by(seed: u64): This => This(state: seed);

    pub fn fill(mut this, bytes: [mut u8..]) {
        this.fill_uninit(unsafe std::mem::bit_cast(bytes))
    }

    $[inline]
    pub fn fill_uninit(mut this, bytes: [mut Uninit<u8>..]) {
        let (chunks, remainder) = (bytes.len() / 8, bytes.len() % 8);
        for i in 0u..chunks {
            bytes[i * 8..][..8u] = Uninit::new(this.gen_u64()).as_bytes();
        }

        if remainder != 0 {
            bytes[chunks * 8..][..remainder] = Uninit::new(this.gen_u64()).as_bytes()[..remainder];
        }
    }

    pub fn int<Int: std::reflect::Integral>(mut this): Int {
        mut val = Uninit::<Int>::uninit();
        this.fill_uninit(val.as_bytes_mut());
        unsafe val.assume_init()
    }

    /// Returns a random `f32` in the range `0.0..1.0`
    pub fn f32(mut this): f32 {
        const F32_MANTISSA_DIGITS: u32 = 24;

        let b = 32u32;
        let f = F32_MANTISSA_DIGITS - 1;
        f32::from_bits((1 << (b - 2)) - (1 << f) + (this.gen_u32() >> (b - f))) - 1.0
    }

    /// Returns a random `f64` in the range `0.0..1.0`
    pub fn f64(mut this): f64 {
        const F64_MANTISSA_DIGITS: u32 = 53;

        let b = 64u32;
        let f = F64_MANTISSA_DIGITS - 1;
        f64::from_bits((1 << (b - 2)) - (1 << f) + (this.gen_u64() >> (b - f))) - 1.0
    }

    $[inline]
    fn gen_u32(mut this): u32 => this.gen_u64().cast();

    $[inline]
    fn gen_u64(mut this): u64 {
        // Constants for WyRand taken from: https://github.com/wangyi-fudan/wyhash/blob/master/wyhash.h#L151
        // Updated for the final v4.2 implementation with improved constants for better entropy output.
        const WY_CONST_0: u64 = 0x2d35_8dcc_aa6c_78a5;
        const WY_CONST_1: u64 = 0x8bb8_4b93_962e_acc9;

        let s = this.state.wrapping_add(WY_CONST_0);
        this.state = s;
        let t = u128::from(s) * u128::from(s ^ WY_CONST_1);
        u64::from(t) ^ u64::from(t >> 64)
    }
}
