pub trait Write {
    fn write_str(mut this, data: str);
    fn write_char(mut this, data: char) => this.write_str(data.encode_utf8(&mut [0; 4]));
}

struct Pad {
    fill: char,
    width: u16,

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            for _ in 0u16..this.width {
                f.write.write_char(this.fill);
            }
        }
    }
}

pub struct Formatter {
    write: *dyn mut Write,
    opts: Options,

    pub fn new(write: *dyn mut Write): This => Formatter(write:, opts: Options());

    pub fn with_options(this, opts: Options): Formatter => Formatter(write: this.write, opts:);

    pub fn options(this): *Options => &this.opts;

    // Ported from the Rust standard library
    pub fn pad(mut this, s: str) {
        let width = this.opts.width;
        let prec = this.opts.prec;
        if width == 0 and prec == 0 {
            return this.write.write_str(s);
        }

        // The `precision` field can be interpreted as a maximum width for the
        // string being formatted.
        let (s, char_count) = if prec != 0 {
            mut iter = s.char_indices();
            let remaining = match iter.advance_by(prec as uint) {
                ?remaining => remaining,
                null => 0,
            };

            // SAFETY: The offset of `.char_indices()` is guaranteed to be
            // in-bounds and between character boundaries.
            let truncated = unsafe s.substr_unchecked(..iter.offset());
            (truncated, prec as uint - remaining)
        } else {
            // Use the optimized char counting algorithm for the full string.
            (s, s.chars().count())
        };

        // The `width` field is more of a minimum width parameter at this point.
        if char_count < width as uint {
            // If we're under the minimum width, then fill up the minimum width
            // with the specified string + some alignment.
            let post_padding = this.padding(width - char_count as! u16, :Left);
            this.write.write_str(s);
            post_padding.fmt(this)
        } else {
            // If we're over the minimum width or there is no minimum width, we
            // can just emit the string.
            this.write.write_str(s)
        }
    }

    // Ported from the Rust standard library
    pub fn pad_integral(mut this, kw negative: bool, kw mut prefix: ?str, kw digits: str) {
        mut width = digits.len();
        mut sign = if negative {
            width++;
            '-'
        } else if this.opts.sign is :Plus {
            width++;
            '+'
        };

        if this.opts.alt and prefix is ?prefix {
            width += prefix.len();
        } else {
            prefix = null;
        }

        fn write_prefix(f: *mut Formatter, sign: ?char, prefix: ?str) {
            if sign is ?sign {
                f.write.write_char(sign);
            }
            if prefix is ?prefix {
                f.write.write_str(prefix);
            }
        }

        let min = this.opts.width;
        if width >= min as uint {
            // We're over the minimum width, so then we can just write the bytes.
            write_prefix(this, sign, prefix);
            this.write.write_str(digits);
        } else if this.opts.zero {
            // The sign and prefix goes before the padding if the fill character is zero
            let old_options = this.opts;
            this.opts.fill = '0';
            this.opts.align = :Right;
            write_prefix(this, sign, prefix);
            let post_padding = this.padding(min - width as! u16, :Right);
            this.write.write_str(digits);
            post_padding.fmt(this);
            this.opts = old_options;
        } else {
            // Otherwise, the sign and prefix goes after the padding
            let post_padding = this.padding(min - width as! u16, :Right);
            write_prefix(this, sign, prefix);
            this.write.write_str(digits);
            post_padding.fmt(this)
        }
    }

    fn padding(mut this, padding: u16, align: Align): Pad {
        let fill = this.opts.fill;
        let padding_left = match this.opts.align is :None then align else this.opts.align {
            :Center => padding / 2,
            :Right => padding,
            _ => 0,
        };

        Pad(fill:, width: padding_left).fmt(this);
        Pad(fill:, width: padding - padding_left)
    }

    impl Write {
        fn write_str(mut this, data: str) => this.write.write_str(data);
        fn write_char(mut this, data: char) => this.write.write_char(data);
    }
}

// codegen.rs depends on the names of the member variables/functions of the below structures/traits

pub union Sign { None, Plus, Minus }

pub union Align { None, Left, Right, Center }

pub packed struct Options {
    pub width: u16   = 0,     // {val:10}     width    = 10
    pub prec:  u16   = 0,     // {val:.5}     prec     = 5
    pub fill:  char  = ' ',   // {val: <5}    fill     = ' '
    pub align: Align = :None, // {val:>5}     align    = :Right
    pub upper: bool  = false, // {val:X}      upper    = true
    pub alt:   bool  = false, // {val:#x}     alt      = true
    pub sign:  Sign  = :None, // {val:+}      sign     = :Plus
    pub zero:  bool  = false, // {val:05}     zero     = true
}

@(lang(fmt_debug))
pub trait Debug {
    fn dbg(this, f: *mut Formatter); // {:?}
}

@(lang(fmt_pointer))
pub trait Pointer {
    fn ptr(this, f: *mut Formatter); // {:p}
}

@(lang(fmt_format))
pub trait Format {
    fn fmt(this, f: *mut Formatter);

    fn hex(this, f: *mut Formatter) => this.fmt(f); // {:x}
    fn bin(this, f: *mut Formatter) => this.fmt(f); // {:b}
    fn oct(this, f: *mut Formatter) => this.fmt(f); // {:o}
    fn exp(this, f: *mut Formatter) => this.fmt(f); // {:e}
}

@(lang(fmt_arg))
struct Argument {
    value:  ^void,
    format: fn(^void, *mut Formatter),
    opts:   Options,
}

@(lang(fmt_args))
pub struct Arguments {
    parts: [str..],
    args:  [Argument..],

    pub fn empty(): This => Arguments(parts: Span::empty(), args: Span::empty());

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            for (part, arg) in this.parts.iter().zip(this.args.iter()) {
                f.write_str(*part);
                ({arg.format}(arg.value, &mut f.with_options(arg.opts)));
            }

            if this.parts.len() > this.args.len() {
                f.write_str(unsafe *this.parts.get_unchecked(this.parts.len() - 1));
            }
        }
    }
}

@(feature(alloc))
pub struct StringBuilder {
    buffer: [u8] = @[],

    pub fn new(): This => StringBuilder();

    impl Write {
        fn write_str(mut this, data: str) {
            let len = data.len();
            guard len != 0 else {
                return;
            }

            this.buffer.reserve(add: len);
            unsafe {
                std::mem::copy(
                    dst: this.buffer.as_raw_mut().add(this.buffer.len()),
                    src: data.as_raw(),
                    num: len,
                );
                this.buffer.set_len(this.buffer.len() + len);
            }
        }
    }

    pub fn into_str(my this): str => unsafe str::from_utf8_unchecked(this.buffer[..]);
}

pub fn write<T: Write, U: Format>(write: *mut T, args: U) {
    args.fmt(&mut Formatter::new(write))
}

pub fn writeln<T: Write, U: Format>(write: *mut T, args: U) {
    args.fmt(&mut Formatter::new(write));
    write.write_char('\n');
}

mod ext {
    use super::*;

    @(feature(alloc))
    pub extension ToStrExt<T: Format> for T {
        pub fn to_str(this): str {
            mut builder = StringBuilder::new();
            this.fmt(&mut Formatter::new(&mut builder));
            builder.into_str()
        }
    }

    pub extension FmtFormatPtr<T: Format> for *T {
        impl Format {
            fn fmt(this, f: *mut Formatter) => (**this).fmt(f);
            fn bin(this, f: *mut Formatter) => (**this).bin(f);
            fn hex(this, f: *mut Formatter) => (**this).hex(f);
            fn oct(this, f: *mut Formatter) => (**this).oct(f);
            fn exp(this, f: *mut Formatter) => (**this).exp(f);
        }
    }

    pub extension FmtDebugPtr<T: Debug> for *T {
        impl Debug {
            fn dbg(this, f: *mut Formatter) => (**this).dbg(f);
        }
    }

    pub extension FmtPointerPtr<T> for *T {
        impl Pointer {
            fn ptr(this, f: *mut Formatter) => (*this as ^T).dbg(f);
        }
    }

    pub extension FmtFormatMutPtr<T: Format> for *mut T {
        impl Format {
            fn fmt(this, f: *mut Formatter) => (**this).fmt(f);
            fn bin(this, f: *mut Formatter) => (**this).bin(f);
            fn hex(this, f: *mut Formatter) => (**this).hex(f);
            fn oct(this, f: *mut Formatter) => (**this).oct(f);
            fn exp(this, f: *mut Formatter) => (**this).exp(f);
        }
    }

    pub extension FmtDebugMutPtr<T: Debug> for *mut T {
        impl Debug {
            fn dbg(this, f: *mut Formatter) => (**this).dbg(f);
        }
    }

    pub extension FmtPointerMutPtr<T> for *mut T {
        impl Pointer {
            fn ptr(this, f: *mut Formatter) => (*this as ^T).dbg(f);
        }
    }
}

mod test {
    extern fn sprintf(dst: ^mut c_char, fmt: ^c_char, ...): c_int;

    unittest "format pointer to format type" {
        let ptr = &10i32;
        mut buf = [0u8; 1024];
        let len = unsafe sprintf(
            dst: buf.as_raw_mut().cast(),
            fmt: "Cool: %p %d!\0".as_raw().cast(),
            ptr,
            *ptr,
        ) as! uint;

        let ctl = "Cool: {ptr:p} {ptr}!".to_str();
        assert_eq(ctl.len(), len);
        assert_eq(ctl.as_bytes(), buf[..len]);
    }

    unittest "format pointer to non-format type" {
        struct NoFmt {}

        let ptr = &NoFmt();
        mut buf = [0u8; 1024];
        let len = unsafe sprintf(
            dst: buf.as_raw_mut().cast(),
            fmt: "%p\0".as_raw().cast(),
            ptr,
        ) as! uint;

        let ctl = "{ptr:p}".to_str();
        assert_eq(ctl.len(), len);
        assert_eq(ctl.as_bytes(), buf[..len]);
    }
}
