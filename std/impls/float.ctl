use std::hash::*;
use std::span::*;
use std::ops::*;
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
    pub fn to_bits(my this): u32 {
        unsafe std::mem::transmute(this)
    }

    pub fn from_bits(v: u32): f32 {
        unsafe std::mem::transmute(v)
    }

    pub fn sqrt(my this): f32 {
        unsafe libm::sqrtf(this)
    }

    pub fn sin(my this): f32 {
        unsafe libm::sinf(this)
    }

    pub fn cos(my this): f32 {
        unsafe libm::cosf(this)
    }

    pub fn tan(my this): f32 {
        unsafe libm::tanf(this)
    }

    pub fn floor(my this): f32 {
        unsafe libm::floorf(this)
    }

    pub fn ceil(my this): f32 {
        unsafe libm::ceilf(this)
    }

    pub fn pi(): f32 {
        3.14159265358979323846
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            Buffer::new().format(*this).fmt(f);
        }
    }
}

pub extension F64Impl for f64 {
    pub fn to_bits(my this): u64 {
        unsafe std::mem::transmute(this)
    }

    pub fn from_bits(v: u64): f64 {
        unsafe std::mem::transmute(v)
    }

    pub fn sqrt(my this): f64 {
        unsafe libm::sqrt(this)
    }

    pub fn sin(my this): f64 {
        unsafe libm::sin(this)
    }

    pub fn cos(my this): f64 {
        unsafe libm::cos(this)
    }

    pub fn tan(my this): f64 {
        unsafe libm::tan(this)
    }

    pub fn floor(my this): f64 {
        unsafe libm::floor(this)
    }

    pub fn ceil(my this): f64 {
        unsafe libm::ceil(this)
    }

    pub fn pi(): f64 {
        3.14159265358979323846
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            Buffer::new().format(*this).fmt(f);
        }
    }
}
