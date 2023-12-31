pub extension StringExt for str {
    pub fn repeat(this, n: usize): str {
        let num = this.len();
        mut buf: [u8] = std::vec::Vec::with_capacity(num * n);
        mut i = 0usize;
        while i < n {
            unsafe core::mem::copy::<u8>(
                dst: core::ptr::offset_mut(buf.as_raw_mut().as_mut_ptr(), num * i),
                src: this.as_ptr(),
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
