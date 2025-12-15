pub trait Write {
    fn write(mut this, data: [u8..]): ?uint;

    fn write_str(mut this, data: str): ?uint {
        this.write(data.as_bytes())
    }
}

pub struct Formatter {
    write: *dyn mut Write,
    opts: Options,

    pub fn new(write: *dyn mut Write): This {
        Formatter(write:, opts: Options())
    }

    impl Write {
        fn write(mut this, data: [u8..]): ?uint {
            this.write.write(data)
        }
    }
}

// codegen.rs depends on the names of the member variables/functions of the below structures/traits

pub union Align { None, Left, Right, Center }

pub packed struct Options {
    pub width: u16   = 0,     // {val:10}     width    = 10
    pub prec:  u16   = 0,     // {val:.5}     prec     = 5
    pub fill:  char  = ' ',   // {val: <5}    fill     = ' '
    pub align: Align = :None, // {val:>5}     align    = :Right
    pub upper: bool  = false, // {val:X}      upper    = true
    pub alt:   bool  = false, // {val:#x}     alt      = true
    pub sign:  bool  = false, // {val:+}      sign     = true
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
    value:  ^c_void,
    format: fn(^c_void, *mut Formatter),
    opts:   Options,
}

@(lang(fmt_args))
pub struct Arguments {
    parts: [str..],
    args:  [Argument..],

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            for (part, arg) in this.parts.iter().zip::<*Argument, std::span::Iter<Argument>>(this.args.iter()) {
                part.fmt(f);
                f.opts = arg.opts;
                ({arg.format}(arg.value, f)); // TODO: this shouldn't be necessary
            }

            if this.parts.len() > this.args.len() {
                this.parts.last()!.fmt(f);
            }
        }
    }
}

@(feature(alloc))
pub extension FormatStrExt<T: Format> for T {
    pub fn to_str(this): str {
        mut formatter = StringFormatter();
        this.fmt(&mut Formatter::new(&mut formatter));
        unsafe str::from_utf8_unchecked(formatter.buffer[..])
    }
}

@(feature(alloc))
@(lang(string_formatter))
struct StringFormatter {
    buffer: [u8] = @[],

    impl Write {
        fn write(mut this, data: [u8..]): ?uint {
            this.write_str(str::from_utf8(data)?)
        }

        fn write_str(mut this, data: str): ?uint {
            if data.is_empty() {
                return 0;
            }

            unsafe {
                this.write_unchecked(data.as_bytes())
            }
        }
    }

    unsafe fn write_unchecked(mut this, data: [u8..]): uint {
        let len = data.len();
        let new_len = this.buffer.len() + len;
        this.buffer.reserve(new_len);
        unsafe {
            std::mem::copy(
                dst: this.buffer.as_raw_mut() + this.buffer.len(),
                src: data.as_raw(),
                num: len,
            );
            this.buffer.set_len(new_len);
            len
        }
    }
}
