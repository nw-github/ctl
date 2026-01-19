pub struct CStr {
    span: [c_char..],

    pub unsafe fn new(ptr: ?^c_char): This {
        ptr is ?ptr
            then This(span: unsafe Span::new(ptr, std::intrin::strlen(ptr)))
            else This::empty()
    }

    pub fn empty(): This => This(span: unsafe Span::new(b"\0".as_raw().cast(), 0));

    pub fn as_str(my this): ?str => str::from_utf8(this.as_bytes());

    pub unsafe fn as_str_unchecked(my this): str {
        unsafe str::from_utf8_unchecked(this.as_bytes())
    }

    pub fn as_bytes(my this): [u8..] {
        unsafe Span::new(this.span.as_raw().cast(), this.span.len())
    }

    pub fn as_bytes_with_nul(my this): [u8..] {
        unsafe Span::new(this.span.as_raw().cast(), this.span.len() + 1)
    }

    pub fn as_raw(my this): ^c_char => this.span.as_raw().cast();
}
