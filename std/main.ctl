pub use intrin::panic;
pub use intrin::unreachable_unchecked;

pub fn unreachable(): never {
    panic("entered unreachable code");
}

@(feature(hosted))
mod libc {
    pub extern fn abort(): never;
    pub extern fn exit(code: c_int): never;
}

@(feature(hosted))
pub fn exit(code: u32): never {
    unsafe libc::exit(code as! c_int)
}

@(feature(hosted))
@(lang(convert_argv))
fn convert_argv(argc: c_int, argv: **c_char): [str..] {
    mut result: [str] = Vec::with_capacity(argc as! uint);
    unsafe {
        for arg in std::span::Span::new(ptr: &raw *argv, len: argc as! uint).iter() {
            result.push(
                str::from_utf8_unchecked(std::span::Span::new(
                    (&raw **arg).cast(),
                    std::intrin::strlen(*arg),
                ))
            );
        }
    }
    result[..]
}

@(feature(hosted, io))
@(lang(panic_handler))
fn panic_handler(s: str): never {
    io::eprint("fatal error: ");
    io::eprintln(s);
    unsafe libc::abort();
}

@(autouse)
mod prelude {
    pub use super::panic;
    pub use super::unreachable;
    pub use super::string::str;
    pub use super::opt::Option::*;
    pub use super::iter::Iterator;
    pub use super::impls::*;
    pub use super::span::ext::*;
    pub use super::opt::ext::*;
    pub use super::range::ext::*;
    pub use super::any::ext::*;

    @(feature(alloc))
    pub use super::alloc::collections::*;
    @(feature(alloc))
    pub use super::alloc::ext::*;

    @(feature(io))
    pub use super::io::*;
}
