use core::reflect::*;

pub extension StringExt for str {
    pub fn repeat(this, n: uint): str {
        let num = this.len();
        mut buf: [u8] = std::vec::Vec::with_capacity(num * n);
        for i in 0u..n {
            unsafe std::mem::copy(
                dst: buf.as_raw() + num * i,
                src: this.as_raw(),
                num:,
            );
        }
        unsafe {
            buf.set_len(num * n);
            str::from_utf8_unchecked(buf.as_span())
        }
    }
}

pub extension StdSignedExt<T: Numeric + Signed> for T {
    pub fn to_str_radix(this, radix: u32): str {
        if radix < 2 || radix > 36 {
            core::panic("to_str_radix(): invalid radix");
        }

        mut buffer = @[b'0'; core::mem::size_of::<i32>() * 8 + 1];
        unsafe {
            this.to_str_radix_unchecked(radix, buffer.as_span_mut())
        }
    }
}

pub extension StdUnsignedExt<T: Numeric + Unsigned> for T {
    pub fn to_str_radix(this, radix: u32): str {
        if radix < 2 || radix > 36 {
            core::panic("to_str_radix(): invalid radix");
        }

        mut buffer = @[b'0'; core::mem::size_of::<i32>() * 8];
        unsafe {
            this.to_str_radix_unchecked(radix, buffer.as_span_mut())
        }
    }
}
