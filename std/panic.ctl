use std::fmt::*;

pub struct SourceLocation {
    pub file: str,
    pub line: u32,
    pub col:  u32,
    pub func: ?str,

    @(intrinsic(source_location))
    pub fn here(): This => SourceLocation::here();

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            write(f, "{this.file}:{this.line}:{this.col}");
        }
    }
}

pub use std::intrin::unreachable_unchecked;

pub fn unreachable(loc: SourceLocation = SourceLocation::here()): never {
    panic("entered unreachable code", loc:);
}

pub fn panic<T: Format>(args: T, loc: SourceLocation = SourceLocation::here()): never {
    std::intrin::panic("{args}", loc)
}

@(feature(hosted, io))
@(panic_handler)
fn panic_handler(args: Arguments, loc: SourceLocation): never {
    if loc.func is ?func {
        eprintln("fatal error in function '{func}' at {loc}:\n\t{args}");
    } else {
        eprintln("fatal error at {loc}: {args}");
    }

    unsafe super::libc::abort();
}
