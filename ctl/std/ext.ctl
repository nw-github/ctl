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
