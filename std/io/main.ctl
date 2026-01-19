use std::fmt;
use std::deps::libc::posix;

pub struct Stdout {
    impl fmt::Write {
        fn write_str(mut this, s: str) {
            if !s.is_empty() {
                unsafe posix::write(1, s.as_raw().cast(), s.len());
            }
        }
    }
}

pub struct Stderr {
    impl fmt::Write {
        fn write_str(mut this, s: str) {
            if !s.is_empty() {
                unsafe posix::write(2, s.as_raw().cast(), s.len());
            }
        }
    }
}

pub fn println<T: fmt::Format>(args: T) {
    fmt::writeln(&mut Stdout(), args);
}

pub fn print<T: fmt::Format>(args: T) {
    fmt::write(&mut Stdout(), args);
}

pub fn eprintln<T: fmt::Format>(args: T) {
    fmt::writeln(&mut Stderr(), args);
}

pub fn eprint<T: fmt::Format>(args: T) {
    fmt::write(&mut Stderr(), args);
}
