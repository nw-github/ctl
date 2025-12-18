extern static CTL_ARGV: ?^mut ^mut c_char;
extern static CTL_ARGC: c_int;

pub struct RawArgsIter {
    argc: c_int,
    argv: ?^mut ^mut c_char,

    impl Iterator<[u8..]> {
        fn next(mut this): ?[u8..] {
            if this.argc > 0 and this.argv is ?argv {
                let arg = unsafe *argv;
                let length = unsafe std::intrin::strlen(arg);

                this.argc--;
                this.argv = argv.add(1);
                unsafe std::span::Span::new(arg.cast::<u8>(), length)
            }
        }
    }
}

pub struct ArgsIter {
    iter: RawArgsIter,

    impl Iterator<str> {
        fn next(mut this): ?str {
            if this.iter.next() is ?val {
                // TODO: This is (to me quite suprisingly) the default behavior of
                // std::env::args() in Rust. For now, mimic it but will probably change in the
                // future.
                str::from_utf8(val).unwrap()
            }
        }
    }
}

pub fn args_raw(): RawArgsIter { unsafe RawArgsIter(argc: CTL_ARGC, argv: CTL_ARGV) }
pub fn args(): ArgsIter { ArgsIter(iter: args_raw()) }
