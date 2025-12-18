use std::hash::*;
use std::ops::Eq;
use std::range::RangeBounds;
use std::fmt::*;
use std::reflect::*;

@(lang(string))
pub struct str {
    span: [u8..],

    pub fn from_utf8(span: [u8..]): ?str {
        // TODO: actually validate

        unsafe {
            str::from_utf8_unchecked(span)
        }
    }

    pub unsafe fn from_utf8_unchecked(span: [u8..]): str => str(span:);

    pub unsafe fn from_cstr(s: ^c_char): ?str {
        str::from_utf8(unsafe std::span::Span::new((&raw *s).cast(), std::intrin::strlen(s)))
    }

    pub unsafe fn from_cstr_unchecked(s: ^c_char): str {
        str(span: unsafe std::span::Span::new((&raw *s).cast(), std::intrin::strlen(s)))
    }

    pub fn len(this): uint => this.span.len();
    pub fn is_empty(this): bool => this.span.is_empty();
    pub fn as_raw(this): ^u8 => this.span.as_raw();
    pub fn as_bytes(this): [u8..] => this.span;

    pub fn chars(this): Chars  => Chars(s: this.as_bytes());
    pub fn char_indices(this): CharIndices  => CharIndices(chars: this.chars(), len: this.len());

    pub fn substr<R: RangeBounds<uint>>(this, range: R): ?str {
        let span = this.span[range];
        if span.first() is ?ch and !is_char_boundary(*ch) {
            return null;
        } else if span.last() is ?ch and !is_char_boundary(*ch) {
            return null;
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
        fn hash<H: Hasher>(this, h: *mut H) => h.hash(this.span);
    }

    impl Eq<str> {
        fn eq(this, rhs: *str): bool => this.as_bytes() == rhs.as_bytes();
    }

    impl Debug {
        fn dbg(this, f: *mut Formatter) => write(f, "\"{this}\"");
    }

    impl Format {
        fn fmt(this, f: *mut Formatter) => f.pad(*this);
    }

    pub fn []<I: Integral>(this, idx: I): *u8 => &this.span[idx];
    pub fn []<R: RangeBounds<uint>>(this, range: R): str => this.substr(range).unwrap();

    @(feature(alloc))
    pub fn repeat(this, n: uint): str {
        let num = this.len();
        mut buf: [u8] = Vec::with_capacity(num * n);
        for i in 0u..n {
            unsafe std::mem::copy(
                dst: buf.as_raw_mut().add(num * i),
                src: this.as_raw(),
                num:,
            );
        }
        unsafe {
            buf.set_len(num * n);
            str::from_utf8_unchecked(buf[..])
        }
    }

    @(feature(alloc))
    pub fn +(this, rhs: str): str {
        let llen = this.len();
        let rlen = rhs.len();
        mut buf: [u8] = Vec::with_capacity(llen + rlen);
        unsafe {
            std::mem::copy(dst: buf.as_raw_mut(), src: this.as_raw(), num: llen);
            std::mem::copy(dst: buf.as_raw_mut().add(llen), src: rhs.as_raw(), num: rlen);
            buf.set_len(llen + rlen);
            str::from_utf8_unchecked(buf[..])
        }
    }

    @(feature(alloc))
    pub fn +=(mut this, rhs: str) {
        *this = this + rhs;
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
                    std::panic::unreachable_unchecked();
                }

                char::from_u32_unchecked(cp)
            }
        }
    }
}

pub struct CharIndices {
    len: uint,
    chars: Chars,

    impl Iterator<(uint, char)> {
        fn next(mut this): ?(uint, char) {
            this.chars.next() is ?ch then (this.len - this.chars.s.len(), ch)
        }
    }
}

fn is_char_boundary(b: u8): bool {
    // From the Rust standard library:
    // This is bit magic equivalent to: b < 128 or b >= 192
    b as! i8 >= -0x40
}
