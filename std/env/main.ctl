use std::runtime::{CTL_ARGC, CTL_ARGV};

pub struct RawArgsIter {
    argc: c_int,
    argv: ?^mut ^mut c_char,

    impl Iterator<[u8..]> {
        fn next(mut this): ?[u8..] {
            if this.argc > 0 and this.argv is ?argv {
                let arg = unsafe *argv;

                this.argc--;
                this.argv = argv.add(1);
                unsafe Span::new(arg.cast::<u8>(), std::intrin::strlen(arg))
            }
        }
    }
}

pub struct ArgsIter {
    iter: RawArgsIter,

    impl Iterator<str> {
        fn next(mut this): ?str {
            if this.iter.next() is ?val {
                str::from_utf8(val)!
            }
        }
    }
}

pub fn args_raw(): RawArgsIter { unsafe RawArgsIter(argc: CTL_ARGC, argv: CTL_ARGV) }
pub fn args(): ArgsIter { ArgsIter(iter: args_raw()) }
