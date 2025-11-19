use core::reflect::*;
use core::fmt::Format;
use core::fmt::Formatter;

pub extension StringExt for str {
    pub fn repeat(this, n: uint): str {
        let num = this.len();
        mut buf: [u8] = std::vec::Vec::with_capacity(num * n);
        for i in 0u..n {
            unsafe std::mem::copy(
                dst: buf.as_raw_mut() + num * i,
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
            std::mem::copy(dst: buf.as_raw_mut(), src: this.as_raw(), num: llen);
            std::mem::copy(dst: buf.as_raw_mut().uoffset(llen), src: rhs.as_raw(), num: rlen);
            buf.set_len(llen + rlen);
            str::from_utf8_unchecked(buf[..])
        }
    }
}

pub extension SignedExt<T: Signed> for T {
    pub fn to_str_radix(this, radix: u32): str {
        guard radix is 2..=36 else {
            panic("to_str_radix(): invalid radix");
        }

        mut buffer = @[b'0'; core::mem::size_of::<T>() * 8 + 1];
        unsafe {
            this.to_str_radix_unchecked(radix, buffer[..])
        }
    }
}

pub extension UnsignedExt<T: Unsigned> for T {
    pub fn to_str_radix(this, radix: u32): str {
        guard radix is 2..=36 else {
            panic("to_str_radix(): invalid radix");
        }

        mut buffer = @[b'0'; core::mem::size_of::<T>() * 8];
        unsafe {
            this.to_str_radix_unchecked(radix, buffer[..])
        }
    }
}

pub extension VecFormat<T: Format> for [T] {
    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            this[..].fmt(f);
        }
    }
}

pub extension MapFormat<K: Format, V: Format> for [K: V] {
    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            "[".fmt(f);
            for (i, (key, value)) in this.iter().enumerate() {
                if i > 0 {
                    ", ".fmt(f);
                }

                key.fmt(f);
                ": ".fmt(f);
                value.fmt(f);
            }
            "]".fmt(f);
        }
    }
}

pub extension SetFormat<T: Format> for #[T] {
    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            "\{".fmt(f);
            for (i, item) in this.iter().enumerate() {
                if i > 0 {
                    ", ".fmt(f);
                }
                item.fmt(f);
            }
            "\}".fmt(f);
        }
    }
}
