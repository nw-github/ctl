pub fn repeat(s: str, n: usize): str {
    mut buf: [u8] = Vec::with_capacity(s.len() * n);
    mut i = 0usize;
    while i < n {
        unsafe core::mem::copy(
            // type mismatch: expected type '*mut T', got '*mut u8'
            dst: core::ptr::offset_mut(buf.as_raw_mut().as_mut_ptr(), i * n),
            src: s.as_ptr(),
            num: s.len(),
        );
    }
    unsafe str::from_utf8_unchecked(buf.as_span())
}
