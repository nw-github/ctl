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
            str::from_utf8_unchecked(buf[..])
        }
    }

    pub fn +(this, rhs: str): str {
        let llen = this.len();
        let rlen = rhs.len();
        mut buf: [u8] = std::vec::Vec::with_capacity(llen + rlen);
        unsafe {
            std::mem::copy(dst: buf.as_raw(), src: this.as_raw(), num: llen);
            std::mem::copy(dst: buf.as_raw() + llen, src: rhs.as_raw(), num: rlen);
            buf.set_len(llen + rlen);
            str::from_utf8_unchecked(buf[..])
        }
    }
}

pub extension StdSignedExt<T: Numeric + Signed> for T {
    pub fn to_str_radix(this, radix: u32): str {
        guard radix is 2..=36 else {
            core::panic("to_str_radix(): invalid radix");
        }

        mut buffer = @[b'0'; core::mem::size_of::<T>() * 8 + 1];
        unsafe {
            this.to_str_radix_unchecked(radix, buffer[..])
        }
    }
}

pub extension StdUnsignedExt<T: Numeric + Unsigned> for T {
    pub fn to_str_radix(this, radix: u32): str {
        guard radix is 2..=36 else {
            core::panic("to_str_radix(): invalid radix");
        }

        mut buffer = @[b'0'; core::mem::size_of::<T>() * 8];
        unsafe {
            this.to_str_radix_unchecked(radix, buffer[..])
        }
    }
}
