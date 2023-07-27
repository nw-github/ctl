use core::span::Span;
use core::hash::Hash;
use core::hash::Hasher;

pub struct str: Hash {
    span: [u8..],

    pub fn from_c_str(ptr: *c_char) str {
        extern fn strlen(ptr: *c_char) usize;

        return str(span: Span::new(ptr as *u8, strlen(ptr)));
    }

    pub fn len(this) usize {
        return this.span.len();
    }

    pub fn is_empty(this) bool {
        return this.span.is_empty();
    }

    pub fn as_ptr(this) *u8 {
        return this.span.as_raw().as_ptr();
    }

    pub fn as_c_str(this) *c_char {
        return this.span.as_raw().as_ptr() as *c_char;
    }

    pub fn as_bytes(this) [u8..] {
        return this.span;
    }

    pub fn hash<H: Hasher>(this, h: *mut H) {
        h.hash(this.span);
    }
}
