use std::fmt;
use std::libc;

struct Stdio {
    impl fmt::Write {
        fn write_str(mut this, s: str) {
            if !s.is_empty() {
                unsafe libc::write(1, s.as_raw().cast(), s.len());
            }
        }
    }
}

struct Stderr {
    impl fmt::Write {
        fn write_str(mut this, s: str) {
            if !s.is_empty() {
                unsafe libc::write(2, s.as_raw().cast(), s.len());
            }
        }
    }
}

pub fn println<T: fmt::Format>(args: T) {
    fmt::writeln(&mut Stdio(), args);
}

pub fn print<T: fmt::Format>(args: T) {
    fmt::write(&mut Stdio(), args);
}

pub fn eprintln<T: fmt::Format>(args: T) {
    fmt::writeln(&mut Stderr(), args);
}

pub fn eprint<T: fmt::Format>(args: T) {
    fmt::write(&mut Stderr(), args);
}
