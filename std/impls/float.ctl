use std::fmt::*;
use std::ryu::*;

mod libm {
    pub extern fn sqrt(n: f64): f64;
    pub extern fn sin(n: f64): f64;
    pub extern fn cos(n: f64): f64;
    pub extern fn tan(n: f64): f64;
    pub extern fn floor(n: f64): f64;
    pub extern fn ceil(n: f64): f64;

    pub extern fn sqrtf(n: f32): f32;
    pub extern fn sinf(n: f32): f32;
    pub extern fn cosf(n: f32): f32;
    pub extern fn tanf(n: f32): f32;
    pub extern fn floorf(n: f32): f32;
    pub extern fn ceilf(n: f32): f32;
}

pub extension F32Impl for f32 {
    pub fn parse(s: str): ?f32 => s2f::s2f(s.as_bytes()) is :Ok(v) then v;

    pub fn to_bits(my this): u32 => unsafe std::mem::transmute(this);
    pub fn from_bits(v: u32): f32 => unsafe std::mem::transmute(v);

    pub fn sqrt(my this): f32 => unsafe libm::sqrtf(this);
    pub fn sin(my this): f32 => unsafe libm::sinf(this);
    pub fn cos(my this): f32 => unsafe libm::cosf(this);
    pub fn tan(my this): f32 => unsafe libm::tanf(this);
    pub fn floor(my this): f32 => unsafe libm::floorf(this);
    pub fn ceil(my this): f32 => unsafe libm::ceilf(this);

    // TODO: make these constants when that is supported
    pub fn pi(): f32 => 3.14159265358979323846;
    pub fn nan(): f32 => 0.0 / 0.0;
    pub fn inf(): f32 => 1.0 / 0.0;
    pub fn neg_inf(): f32 => -1.0 / 0.0;
    pub fn min_value(): f32 => -3.40282347e+38;
    pub fn max_value(): f32 => 3.40282347e+38;
    pub fn min_pos(): f32 => 1.17549435e-38;
    pub fn epsilon(): f32 => 1.19209290e-07;

    pub fn copysign(my this, y: f32): f32 {
        let x = this.to_bits() & !(1 << 31);
        let y = y.to_bits() & (1 << 31);
        f32::from_bits(x | y)
    }

    pub fn abs(my this): f32 => f32::from_bits(this.to_bits() & !(1 << 31));
    pub fn is_sign_positive(my this): bool => !this.is_sign_negative();
    pub fn is_sign_negative(my this): bool => this.to_bits() & (1 << 31) != 0;

    pub fn min(my this, rhs: This): This {
        if this < rhs {
            this
        } else if rhs < this {
            rhs
        } else if this == rhs {
            // prefer -0.0
            this.is_sign_negative() and rhs.is_sign_positive() then this else rhs
        } else {
            // At least one input is NaN. Use `+` to perform NaN propagation and quieting.
            this + rhs
        }
    }

    pub fn max(my this, y: This): This {
        if this > y {
            this
        } else if y > this {
            y
        } else if this == y {
            this.is_sign_positive() and y.is_sign_negative() then this else y
        } else {
            this + y
        }
    }

    pub fn clamp(my mut this, min: This, max: This): This {
        debug_assert(min <= max, "min > max, or either was NaN. min = {min:?}, max = {max:?}");
        if this < min {
            this = min;
        }
        if this > max {
            this = max;
        }
        this
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) {
            f.write_str(Buffer::new().format(*this));
        }
    }

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            f.pad(Buffer::new().format(*this));
        }
    }
}

pub extension F64Impl for f64 {
    pub fn parse(s: str): ?f64 => s2d::s2d(s.as_bytes()) is :Ok(v) then v;

    pub fn to_bits(my this): u64 => unsafe std::mem::transmute(this);
    pub fn from_bits(v: u64): f64 => unsafe std::mem::transmute(v);

    pub fn sqrt(my this): f64 => unsafe libm::sqrt(this);
    pub fn sin(my this): f64 => unsafe libm::sin(this);
    pub fn cos(my this): f64 => unsafe libm::cos(this);
    pub fn tan(my this): f64 => unsafe libm::tan(this);
    pub fn floor(my this): f64 => unsafe libm::floor(this);
    pub fn ceil(my this): f64 => unsafe libm::ceil(this);

    pub fn pi(): f64 => 3.14159265358979323846;
    pub fn nan(): f64 => 0.0 / 0.0;
    pub fn inf(): f64 => 1.0 / 0.0;
    pub fn neg_inf(): f64 => -1.0 / 0.0;
    pub fn min_value(): f64 => -1.7976931348623157e+308;
    pub fn max_value(): f64 => 1.7976931348623157e+308;
    pub fn min_pos(): f64 => 2.2250738585072014e-308;
    pub fn epsilon(): f64 => 2.2204460492503131e-16;

    pub fn copysign(my this, y: f64): f64 {
        let x = this.to_bits() & !(1 << 63);
        let y = y.to_bits() & (1 << 63);
        f64::from_bits(x | y)
    }

    pub fn abs(my this): f64 => f64::from_bits(this.to_bits() & !(1 << 63));
    pub fn is_sign_positive(my this): bool => !this.is_sign_negative();
    pub fn is_sign_negative(my this): bool => this.to_bits() & (1 << 63) != 0;

    pub fn min(my this, rhs: This): This {
        if this < rhs {
            this
        } else if rhs < this {
            rhs
        } else if this == rhs {
            this.is_sign_negative() and rhs.is_sign_positive() then this else rhs
        } else {
            // At least one input is NaN. Use `+` to perform NaN propagation and quieting.
            this + rhs
        }
    }

    pub fn max(my this, y: This): This {
        if this > y {
            this
        } else if y > this {
            y
        } else if this == y {
            this.is_sign_positive() and y.is_sign_negative() then this else y
        } else {
            this + y
        }
    }

    pub fn clamp(my mut this, min: This, max: This): This {
        debug_assert(min <= max, "min > max, or either was NaN. min = {min:?}, max = {max:?}");
        if this < min {
            this = min;
        }
        if this > max {
            this = max;
        }
        this
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) {
            f.write_str(Buffer::new().format(*this));
        }
    }

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            f.pad(Buffer::new().format(*this));
        }
    }
}
