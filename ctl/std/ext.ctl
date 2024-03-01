use core::reflect::*;

pub extension StringExt for str {
    pub fn repeat(this, n: uint): str {
        let num = this.len();
        mut buf: [u8] = std::vec::Vec::with_capacity(num * n);
        mut i = 0u;
        while i < n {
            unsafe std::mem::copy::<u8>(
                dst: std::ptr::raw_add(buf.as_raw(), num * i),
                src: this.as_ptr() as *raw u8,
                num:,
            );

            ++i;
        }
        unsafe {
            buf.set_len(num * n);
            str::from_utf8_unchecked(buf.as_span())
        }
    }
}

pub extension<T: Numeric + Signed> StdSignedExt for T {
    pub fn to_str_radix(this, radix: u32): str {
        mut buffer = @[b'0'; core::mem::size_of::<i32>() * 8 + 1];
        this.to_str_radix_ex(radix, buffer.as_span_mut())
    }
}

pub extension<T: Numeric + Unsigned> StdUnsignedExt for T {
    pub fn to_str_radix(this, radix: u32): str {
        mut buffer = @[b'0'; core::mem::size_of::<i32>() * 8];
        this.to_str_radix_ex(radix, buffer.as_span_mut())
    }
}
