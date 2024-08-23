use super::span::Span;
use super::string::str;
use super::panic;
use core::ext::*;

static NAN: str = "NaN";
static INFINITY: str = "inf";
static NEG_INFINITY: str = "-inf";

pub struct Buffer {
    bytes: [u8; 24],

    pub fn new(): This {
        Buffer(bytes: [0; 24])
    }

    pub fn format<F: Float>(mut this, f: F): str {
        if f.is_nonfinite() {
            f.format_nonfinite()
        } else {
            this.format_finite(f)
        }
    }

    pub fn format_finite<F: Float>(mut this, f: F): str {
        unsafe {
            let p = &mut this.bytes as *raw u8;
            let n = f.write_to_ryu_buffer(p);
            debug_assert(n <= 24);
            str::from_utf8_unchecked(Span::new(p, n))
        }
    }
}

trait Float {
    fn is_nonfinite(this): bool;
    fn format_nonfinite(this): str;
    unsafe fn write_to_ryu_buffer(this, result: *raw u8): uint;
}

pub extension Float32Ext for f32 {
    impl Float {
        fn is_nonfinite(this): bool {
            const EXP_MASK: u32 = 0x7f800000;
            let bits = this.to_bits();
            bits & EXP_MASK == EXP_MASK
        }

        #(cold)
        fn format_nonfinite(this): str {
            let bits = this.to_bits();
            if bits & 0x007fffff != 0 {
                NAN
            } else if bits & 0x80000000 != 0 {
                NEG_INFINITY
            } else {
                INFINITY
            }
        }

        unsafe fn write_to_ryu_buffer(this, result: *raw u8): uint {
            unsafe pretty::format32(*this, result)
        }
    }
}

pub extension Float64Ext for f64 {
    impl Float {
        fn is_nonfinite(this): bool {
            const EXP_MASK: u64 = 0x7ff0000000000000;
            let bits = this.to_bits();
            bits & EXP_MASK == EXP_MASK
        }

        #(cold)
        fn format_nonfinite(this): str {
            let bits = this.to_bits();
            if bits & 0x000fffffffffffff != 0 {
                NAN
            } else if bits & 0x8000000000000000 != 0 {
                NEG_INFINITY
            } else {
                INFINITY
            }
        }

        unsafe fn write_to_ryu_buffer(this, result: *raw u8): uint {
            unsafe pretty::format64(*this, result)
        }
    }
}

pub fn debug_assert(b: bool) {
    if !b {
        panic("debug assertion failed!");
    }
}
