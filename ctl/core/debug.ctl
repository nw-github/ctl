use core::string::str;
use core::fmt::*;
use core::ext::*;

pub struct SourceLocation {
    file: str,
    line: u32,
    char: u32,

    // #(intrinsic(source_here))
    // pub fn here(): This { SourceLocation::here() }

    impl Format {
        fn fmt<F: Formatter>(this, f: *mut F) {
            this.file.fmt(f);
            ":".fmt(f);
            this.line.fmt(f);
            ":".fmt(f);
            this.char.fmt(f);
        }
    }
}
