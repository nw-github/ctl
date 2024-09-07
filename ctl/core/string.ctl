use core::span::Span;
use core::hash::Hash;
use core::hash::Hasher;
use core::ops::Eq;
use core::range::RangeBounds;
use core::iter::Iterator;
use core::panic;
use core::fmt::*;
use core::intrin;
use core::reflect::*;
use core::ext::U8Ext;

#(lang(string))
pub struct str {
    span: [u8..],

    pub fn from_utf8(span: [u8..]): ?str {
        // TODO: actually validate

        unsafe {
            str::from_utf8_unchecked(span)
        }
    }

    pub unsafe fn from_utf8_unchecked(span: [u8..]): str {
        str(span:)
    }

    pub unsafe fn from_cstr(s: *c_char): ?str {
        str::from_utf8(unsafe core::span::Span::new(s as *raw u8, core::intrin::strlen(s)))
    }

    pub unsafe fn from_cstr_unchecked(s: *c_char): str {
        str(span: unsafe core::span::Span::new(s as *raw u8, core::intrin::strlen(s)))
    }

    pub fn len(this): uint {
        this.span.len()
    }

    pub fn is_empty(this): bool {
        this.span.is_empty()
    }

    pub fn as_raw(this): *raw u8 {
        this.span.as_raw()
    }

    pub fn as_bytes(this): [u8..] {
        this.span
    }

    pub fn chars(this): Chars {
        Chars(s: this.as_bytes())
    }

    pub fn substr<R: RangeBounds<uint>>(this, range: R): str {
        let span = this.span[range];
        if span.get(0) is ?ch and !is_char_boundary(*ch) {
            panic("str::substr(): range does not start at char boundary");
        }
        if span.get(span.len()) is ?ch and !is_char_boundary(*ch) {
            panic("str::substr(): range does not end at char boundary");
        }
        str(span:)
    }

    pub fn trim_start(this): str {
        for (i, ch) in this.as_bytes().iter().enumerate() {
            if !ch.is_ascii_whitespace() {
                return this[i..];
            }
        }
        this[this.len()..]
    }

    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) {
            h.hash(this.span);
        }
    }

    impl Eq<str> {
        fn eq(this, rhs: *str): bool {
            use core::span::ext::SpanEq;
            use core::ext::NumericExt;

            this.as_bytes() == rhs.as_bytes()
        }
    }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            f.write_str(*this);
        }
    }

    pub fn []<I: Numeric + Integral>(this, idx: I): *u8 {
        &this.span[idx]
    }

    pub fn []<R: RangeBounds<uint>>(this, range: R): str {
        this.substr(range)
    }
}

pub struct Chars {
    s: [u8..],

    impl Iterator<char> {
        fn next(mut this): ?char {
            unsafe if this.s.get(0) is ?cp {
                mut cp = *cp as u32 & 0xff;
                if cp < 0x80 {
                    this.s = this.s[1u..];
                } else if cp >> 5 == 0x6 {
                    cp = ((cp << 6) & 0x7ff) + (*this.s.get_unchecked(1) as u32 & 0x3f);
                    this.s = this.s[2u..];
                } else if cp >> 4 == 0xe {
                    cp = (
                        (cp << 12) & 0xffff) + 
                        (((*this.s.get_unchecked(1) as u32 & 0xff) << 6) & 0xfff
                    );
                    cp += *this.s.get_unchecked(2) as u32 & 0x3f;
                    this.s = this.s[3u..];
                } else if cp >> 4 == 0x1e {
                    cp = (
                        (cp << 18) & 0x1fffff) + 
                        (((*this.s.get_unchecked(1) as u32 & 0xff) << 12) & 0x3ffff
                    );
                    cp += ((*this.s.get_unchecked(2) as u32 & 0xff) << 6) & 0xfff;
                    cp += *this.s.get_unchecked(3) as u32 & 0x3f;
                    this.s = this.s[4u..];
                } else {
                    core::unreachable_unchecked();
                }

                cp as! char
            }
        }
    }
}

fn is_char_boundary(b: u8): bool {
    // From the Rust standard library:
    // This is bit magic equivalent to: b < 128 or b >= 192
    b as! i8 >= -0x40
}
