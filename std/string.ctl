use std::hash::*;
use std::ops::Eq;
use std::range::RangeBounds;
use std::fmt::*;
use std::reflect::*;

@(lang(string))
pub struct str {
    span: [u8..],

    pub fn from_utf8(span: [u8..]): ?str {
        mut iter = span.iter();
        while iter.next() is ?byte0 {
            let len = utf8::sequence_length(*byte0)?;
            mut cp = *byte0 as u32;
            match len {
                1 => {},
                2 => {
                    let byte1 = *iter.next()?;
                    cp = ((cp << 6) & 0x7ff) + (byte1 as u32 & 0x3f);
                }
                3 => {
                    let byte1 = *iter.next()?;
                    let byte2 = *iter.next()?;
                    cp = ((cp << 12) & 0xffff) + ((byte1 as u32 << 6) & 0xfff);
                    cp += byte2 as u32 & 0x3f;
                }
                4 => {
                    let byte1 = *iter.next()?;
                    let byte2 = *iter.next()?;
                    let byte3 = *iter.next()?;
                    cp = ((cp << 18) & 0x1fffff) + ((byte1 as u32 << 12) & 0x3ffff);
                    cp += (byte2 as u32 << 6) & 0xfff;
                    cp += byte3 as u32 & 0x3f;
                }
                _ => unsafe std::hint::unreachable_unchecked(),
            }

            if char::from_u32(cp)?.len_utf8() != len {
                // Overlong sequence
                return null;
            }
        }

        unsafe str::from_utf8_unchecked(span)
    }

    pub unsafe fn from_utf8_unchecked(span: [u8..]): str => str(span:);

    pub unsafe fn from_cstr(s: ^c_char): ?str {
        str::from_utf8(unsafe Span::new(s.cast(), std::intrin::strlen(s)))
    }

    pub unsafe fn from_cstr_unchecked(s: ^c_char): str {
        str(span: unsafe Span::new(s.cast(), std::intrin::strlen(s)))
    }

    pub fn len(this): uint => this.span.len();
    pub fn is_empty(this): bool => this.span.is_empty();
    pub fn as_raw(this): ^u8 => this.span.as_raw();
    pub fn as_bytes(this): [u8..] => this.span;

    pub fn chars(this): Chars => Chars(s: this.as_bytes());
    pub fn char_indices(this): CharIndices => CharIndices(chars: this.chars());
    pub fn utf16(this): Utf16 => Utf16(chars: this.chars(), trail: null);

    pub fn strip_prefix(this, prefix: str): ?str {
        guard prefix.len() <= this.len() and this[..prefix.len()] == prefix else {
            return null;
        }

        this[prefix.len()..]
    }

    pub fn substr<R: RangeBounds<uint>>(this, range: R): ?str {
        let span = this.span.subspan(range)?;
        if span.first() is ?ch and !utf8::is_char_boundary(*ch) {
            return null;
        } else if span.last() is ?ch and !utf8::is_char_boundary(*ch) {
            return null;
        }
        str(span:)
    }

    pub fn find(this, rhs: str): ?uint {
        guard this.len() >= rhs.len() else {
            return null;
        }

        // TODO: use something standard or add an intrinsic
        let cmp = unsafe std::deps::libc::memmem(
            haystack: this.span.as_raw().cast(),
            hlen: this.span.len(),
            needle: rhs.span.as_raw().cast(),
            nlen: rhs.span.len(),
        );
        if cmp is ?val {
            return this.span.as_raw().cast::<void>().sub_ptr(val) as! uint
        }
    }

    pub unsafe fn substr_unchecked<R: RangeBounds<uint>>(this, range: R): str {
        str(span: unsafe this.span.subspan_unchecked(range))
    }

    pub fn trim_start(this): str {
        for (i, ch) in this.char_indices() {
            if !ch.is_ascii_whitespace() {
                return this[i..];
            }
        }
        this[this.len()..]
    }

    pub fn ==(this, rhs: *str): bool => this.as_bytes() == rhs.as_bytes();
    pub fn <=>(this, rhs: *str): std::ops::Ordering => this.as_bytes() <=> rhs.as_bytes();

    impl Hash {
        fn hash<H: Hasher>(this, h: *mut H) => h.hash(this.span);
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
            unsafe if this.s.get(0) is ?lead {
                mut cp = *lead as u32;
                match utf8::sequence_length(*lead) {
                    ?1 => {
                        this.s = this.s.subspan_unchecked(1u..);
                    }
                    ?2 => {
                        cp = ((cp << 6) & 0x7ff) + (*this.s.get_unchecked(1) as u32 & 0x3f);
                        this.s = this.s.subspan_unchecked(2u..);
                    }
                    ?3 => {
                        cp = (
                            (cp << 12) & 0xffff) +
                            ((*this.s.get_unchecked(1) as u32 << 6) & 0xfff
                        );
                        cp += *this.s.get_unchecked(2) as u32 & 0x3f;
                        this.s = this.s.subspan_unchecked(3u..);
                    }
                    ?4 => {
                        cp = (
                            (cp << 18) & 0x1fffff) +
                            ((*this.s.get_unchecked(1) as u32 << 12) & 0x3ffff
                        );
                        cp += (*this.s.get_unchecked(2) as u32 << 6) & 0xfff;
                        cp += *this.s.get_unchecked(3) as u32 & 0x3f;
                        this.s = this.s.subspan_unchecked(4u..);
                    }
                    _ => std::hint::unreachable_unchecked()
                }

                char::from_u32_unchecked(cp)
            }
        }
    }
}

pub struct CharIndices {
    offs: uint = 0,
    chars: Chars,

    impl Iterator<(uint, char)> {
        fn next(mut this): ?(uint, char) {
            if this.chars.next() is ?ch {
                let offs = this.offs;
                this.offs += ch.len_utf8();
                (offs, ch)
            }
        }
    }

    pub fn offset(this): uint => this.offs;
}

pub struct Utf16 {
    chars: Chars,
    trail: ?u16,

    impl Iterator<u16> {
        fn next(mut this): ?u16 {
            if this.trail.take() is ?next {
                next
            } else if this.chars.next() is ?ch {
                let (lead, trail) = ch.encode_utf16();
                this.trail = trail;
                lead
            }
        }
    }
}

mod utf8 {
    // From the Rust standard library:
    // This is bit magic equivalent to: b < 128 or b >= 192
    pub fn is_char_boundary(b: u8): bool => b as! i8 >= -0x40;

    pub fn sequence_length(lead: u8): ?uint {
        if lead < 0x80 {
            1
        } else if lead >> 5 == 0x6 {
            2
        } else if lead >> 4 == 0xe {
            3
        } else if lead >> 3 == 0x1e {
            4
        } else {
            null
        }
    }
}

unittest "from_utf8 overlong encoding" {
    assert_eq(str::from_utf8(b"\xc0\xaf\xc1\x81"[..]), null); // Overlong encoding of '/A'
    assert_eq(str::from_utf8(b"\x2f\x41"[..]), "/A");
}
