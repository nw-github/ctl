use std::fmt;

extern fn write(fd: c_int, buf: ^void, count: uint): int;

struct Stdio {
    impl fmt::Write {
        fn write_str(mut this, s: str): ?uint {
            if !s.is_empty() {
                unsafe write(1, s.as_raw().cast(), s.len()) as! uint
            }
        }
    }
}

struct Stderr {
    impl fmt::Write {
        fn write_str(mut this, s: str): ?uint {
            if !s.is_empty() {
                unsafe write(2, s.as_raw().cast(), s.len()) as! uint
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
