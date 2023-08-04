use core::span::Span;
use core::hash::Hash;
use core::hash::Hasher;
use core::ops::Eq;
use core::range::RangeBounds;

[lang(string)]
pub struct str: Hash + Eq<str> {
    span: [u8..],

    pub fn from_c_str(ptr: *c_char) str {
        extern fn strlen(ptr: *c_char) usize;
        // TODO: validate UTF-8
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

    pub fn eq(this, rhs: *str) bool {
        if this.len() != rhs.len() {
            return false;
        }

        return core::mem::compare(this.as_ptr(), rhs.as_ptr(), this.len());
    }

    pub fn chars(this) Chars {
        return Chars(s: this.as_bytes());
    }

    pub fn substr<R: RangeBounds<usize> >(this, range: R) str {
        let span = this.span.subspan(range);
        match span.get(0) {
            ?ch => if !is_char_boundary(*ch) {
                panic("str::substr(): range does not start at char boundary");
            }
        }
        match span.get(span.len()) {
            ?ch => if !is_char_boundary(*ch) {
                panic("str::substr(): range does not end at char boundary");
            }
        }
        return str(span:);
    }
}

pub struct Chars: core::iter::Iter<char> {
    s: [u8..],

    pub fn next(mut this) ?char {
        return match this.s.get(0) {
            ?cp => {
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

                yield cp as! char;
            }
            null => null,
        };
    }
}

fn is_char_boundary(b: u8) bool {
    // From the Rust standard library:
    // This is bit magic equivalent to: b < 128 || b >= 192
    return b as! i8 >= -0x40;
}
