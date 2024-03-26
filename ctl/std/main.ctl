mod libc {
    pub import fn abort(): never;
    pub import fn exit(code: c_int): never;
}

pub fn exit(code: u32): never {
    libc::exit(code as! c_int)
}

#(lang(convert_argv))
fn convert_argv(argc: c_int, argv: **c_char): [str..] {
    mut result: [str] = std::vec::Vec::with_capacity(argc as! uint);
    unsafe {
        for arg in std::span::Span::new(ptr: argv as *raw *c_char, len: argc as! uint).iter() {
            result.push(
                str::from_utf8_unchecked(core::span::Span::new(
                    *arg as *raw u8,
                    core::intrin::strlen(*arg)
                ))
            );
        }
    }
    result[..]
}

#(lang(panic_handler))
fn panic_handler(s: str): never {
    io::eprint("fatal error: ");
    io::eprintln(s);
    libc::abort();
}

#(autouse)
mod prelude {
    pub use super::vec::Vec;
    pub use super::map::Map;
    pub use super::set::Set;
    pub use super::ext::*;
    pub use super::io::*;
}

pub use core::*;
