import fn write(fd: c_int, buf: *c_void, count: uint): int;
import fn abort(): never;

pub fn println(s: str) {
    print(s);
    write(1, unsafe &b'\n' as *c_void, 1);
}

pub fn print(s: str) {
    write(1, unsafe s.as_ptr() as *c_void, s.len());
}

pub fn eprintln(s: str) {
    eprint(s);
    write(2, unsafe &b'\n' as *c_void, 1);
}

pub fn eprint(s: str) {
    write(2, unsafe s.as_ptr() as *c_void, s.len());
}

#(lang(convert_argv))
fn convert_argv(argc: c_int, argv: **c_char): [str..] {
    mut result: [str] = std::vec::Vec::with_capacity(argc as! uint);
    for arg in (unsafe std::span::Span::new(ptr: argv as *raw *c_char, len: argc as! uint)).iter() {
        result.push(str::from_c_str(*arg));
    }
    result.as_span()
}

#(lang(panic_handler))
fn panic_handler(s: str): never {
    eprint("fatal error: ");
    eprintln(s);
    abort();
}

#(autouse)
mod prelude {
    pub use super::vec::Vec;
    pub use super::map::Map;
    pub use super::set::Set;
    pub use super::ext::*;
}

pub use core::*;
