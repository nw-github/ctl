@(feature(hosted))
mod inner {
    extern static CTL_ARGV: ^^c_char;
    extern static CTL_ARGC: c_int;

    pub struct RawArgsIterator {
        argc: c_int,
        argv: ^^c_char,

        unsafe fn new(argc: c_int, argv: ^^c_char): This {
            RawArgsIterator(argc:, argv:)
        }

        impl Iterator<[u8..]> {
            fn next(mut this): ?[u8..] {
                if this.argc > 0 {
                    let arg = unsafe *this.argv;
                    let length = unsafe std::intrin::strlen(arg);

                    this.argc--;
                    this.argv = this.argv.offset(1);
                    unsafe std::span::Span::new(arg.cast::<u8>(), length)
                }
            }
        }
    }

    pub struct ArgsIterator {
        raw_iter: RawArgsIterator,

        unsafe fn new(argc: c_int, argv: ^^c_char): This {
            ArgsIterator(raw_iter: unsafe RawArgsIterator::new(argc, argv))
        }

        impl Iterator<str> {
            fn next(mut this): ?str {
                if this.raw_iter.next() is ?val {
                    // TODO: This is (to me quite suprisingly) the default behavior of
                    // std::env::args() in Rust. For now, mimic it but will probably change in the
                    // future.
                    str::from_utf8(val).unwrap()
                }
            }
        }
    }

    pub fn args_raw(): RawArgsIterator {
        unsafe RawArgsIterator::new(argc: CTL_ARGC, argv: CTL_ARGV)
    }

    pub fn args(): ArgsIterator {
        unsafe ArgsIterator::new(argc: CTL_ARGC, argv: CTL_ARGV)
    }
}

@(feature(hosted))
pub use inner::*;
