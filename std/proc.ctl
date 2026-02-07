use std::deps::libc;

/// Exit the process with exit code `code`
pub fn exit(code: u32): never => libc::exit(code.cast());

pub fn abort(): never => libc::abort();

/// Print a message to stderr, then exit the process with exit code `code`
pub fn fatal<T: std::fmt::Format>(msg: T, kw code: u32 = 1): never {
    eprintln(msg);
    exit(code)
}
