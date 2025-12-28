use std::libc;

/// Exit the process with exit code `code`
pub fn exit(code: u32): never => unsafe libc::exit(code as! c_int);

pub fn abort(): never => unsafe libc::abort();

/// Print a message to stderr, then exit the process with exit code `code`
pub fn fatal<T: std::fmt::Format>(msg: T, kw code: c_int = 1): never {
    eprintln(msg);
    unsafe libc::exit(code)
}
