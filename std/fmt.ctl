pub trait Write {
    fn write_str(mut this, data: str): ?uint;
    fn write_char(mut this, data: char): ?uint => this.write_str(data.encode_utf8(&mut [0u8; 4]));
}

pub struct Formatter {
    write: *dyn mut Write,
    opts: Options,

    pub fn new(write: *dyn mut Write): This => Formatter(write:, opts: Options());

    pub fn with_options(this, opts: Options): Formatter => Formatter(write: this.write, opts:);

    pub fn options(this): *Options => &this.opts;

    pub fn pad(mut this, value: str) {
        // TODO: actually pad
        this.write.write_str(value);
    }

    pub fn pad_integral(mut this, kw negative: bool, kw prefix: ?str, kw value: str) {
        // TODO: actually pad
        if negative {
            this.write.write_char('-');
        } else if this.opts.sign is :Plus {
            this.write.write_char('+');
        }

        if this.opts.alt and prefix is ?prefix {
            this.write.write_str(prefix);
        }

        this.write.write_str(value);
    }

    impl Write {
        fn write_str(mut this, data: str): ?uint => this.write.write_str(data);
        fn write_char(mut this, data: char): ?uint => this.write.write_char(data);
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

@(lang(fmt_format))
pub trait Format {
    fn fmt(this, f: *mut Formatter);

    fn hex(this, f: *mut Formatter) => this.fmt(f); // {:x}
    fn bin(this, f: *mut Formatter) => this.fmt(f); // {:b}
    fn oct(this, f: *mut Formatter) => this.fmt(f); // {:o}
    fn ptr(this, f: *mut Formatter) => this.fmt(f); // {:p}
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

    pub fn empty(): This {
        Arguments(parts: std::span::Span::empty(), args: std::span::Span::empty())
    }

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            for (part, arg) in this.parts.iter().zip::<*Argument, std::span::Iter<Argument>>(this.args.iter()) {
                f.write_str(*part);
                ({arg.format}(arg.value, &mut f.with_options(arg.opts)));
            }

            if this.parts.len() > this.args.len() {
                f.write_str(*this.parts.last()!);
            }
        }
    }
}

@(feature(alloc))
pub struct StringBuilder {
    buffer: [u8] = @[],

    pub fn new(): This => StringBuilder();

    impl Write {
        fn write_str(mut this, data: str): ?uint {
            let len = data.len();
            guard len != 0 else {
                return 0;
            }

            let new_len = this.buffer.len() + len;
            this.buffer.reserve(new_len);
            unsafe {
                std::mem::copy(
                    dst: this.buffer.as_raw_mut().add(this.buffer.len()),
                    src: data.as_raw(),
                    num: len,
                );
                this.buffer.set_len(new_len);
                len
            }
        }
    }

    pub fn into_str(my this): str => unsafe str::from_utf8_unchecked(this.buffer[..]);
}

pub fn write<T: Write, U: Format>(write: *mut T, args: U) {
    args.fmt(&mut Formatter::new(write))
}

pub fn writeln<T: Write, U: Format>(write: *mut T, args: U) {
    "{args}\n".fmt(&mut Formatter::new(write))
}

pub mod ext {
    use super::*;

    @(feature(alloc))
    pub extension FormatToStrExt<T: Format> for T {
        pub fn to_str(this): str {
            mut builder = StringBuilder::new();
            this.fmt(&mut Formatter::new(&mut builder));
            builder.into_str()
        }
    }
}
