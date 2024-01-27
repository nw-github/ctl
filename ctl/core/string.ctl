use core::span::Span;
use core::hash::Hash;
use core::hash::Hasher;
use core::ops::Eq;
use core::range::RangeBounds;
use core::iter::Iterator;
use core::panic;
use core::unreachable;

mod builtin {
    #{c_macro, c_name(__builtin_strlen)}
    pub extern fn strlen(ptr: *c_char): usize;
}

#{lang(string)}
pub struct str {
    span: [u8..],

    pub fn from_c_str(ptr: *c_char): str {
        // TODO: validate UTF-8
        str(span: unsafe Span::new(ptr as *u8, builtin::strlen(ptr)))
    }

    pub unsafe fn from_utf8_unchecked(span: [u8..]): str {
        str(span:)
    }

    pub fn len(this): usize {
        this.span.len()
    }

    pub fn is_empty(this): bool {
        this.span.is_empty()
    }

    pub fn as_ptr(this): *u8 {
        unsafe this.span.as_raw().as_ptr()
    }

    pub fn as_c_str(this): *c_char {
        unsafe this.span.as_raw().as_ptr() as *c_char
    }

    pub fn as_bytes(this): [u8..] {
        this.span
    }

    pub fn chars(this): Chars {
        Chars(s: this.as_bytes())
    }

    pub fn substr<R: RangeBounds<usize>>(this, range: R): str {
        let span = this.span.subspan(range);
        if span.get(0) is ?ch {
            if !is_char_boundary(*ch) {
                panic("str::substr(): range does not start at char boundary");
            }
        }
        if span.get(span.len()) is ?ch {
            if !is_char_boundary(*ch) {
                panic("str::substr(): range does not end at char boundary");
            }
        }
        str(span:)
    }

    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.span);
        }
    }

    impl Eq<str> {
        fn eq(this, rhs: *str): bool {
            if this.len() != rhs.len() {
                false
            } else {
                core::mem::compare(this.as_ptr(), rhs.as_ptr(), this.len())
            }
        }
    }
}

pub struct Chars {
    s: [u8..],

    impl Iterator<char> {
        fn next(mut this): ?char {
            unsafe if this.s.get(0) is ?cp {
                mut cp = *cp as u32 & 0xff;
                if cp < 0x80 {
                    this.s = this.s.subspan(1usize..);
                } else if cp >> 5 == 0x6 {
                    cp = ((cp << 6) & 0x7ff) + (*this.s.get_unchecked(1) as u32 & 0x3f);
                    this.s = this.s.subspan(2usize..);
                } else if cp >> 4 == 0xe {
                    cp = (
                        (cp << 12) & 0xffff) + 
                        (((*this.s.get_unchecked(1) as u32 & 0xff) << 6) & 0xfff
                    );
                    cp += *this.s.get_unchecked(2) as u32 & 0x3f;
                    this.s = this.s.subspan(3usize..);
                } else if cp >> 4 == 0x1e {
                    cp = (
                        (cp << 18) & 0x1fffff) + 
                        (((*this.s.get_unchecked(1) as u32 & 0xff) << 12) & 0x3ffff
                    );
                    cp += ((*this.s.get_unchecked(2) as u32 & 0xff) << 6) & 0xfff;
                    cp += *this.s.get_unchecked(3) as u32 & 0x3f;
                    this.s = this.s.subspan(4usize..);
                } else {
                    unreachable();
                }

                cp as! char
            }
        }
    }
}

fn is_char_boundary(b: u8): bool {
    // From the Rust standard library:
    // This is bit magic equivalent to: b < 128 || b >= 192
    b as! i8 >= -0x40
}
