import fn abort(): never;

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
    io::eprint("fatal error: ");
    io::eprintln(s);
    abort();
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
