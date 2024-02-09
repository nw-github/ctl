pub fn repeat(s: str, n: uint): str {
    mut buf: [u8] = Vec::with_capacity(s.len() * n);
    mut i = 0u;
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

fn something_opt<T>(t: ?T) {}

fn main() {
    // should be able to infer T = int, since T is implicitly convertible to ?T
    something_opt(10);
}
