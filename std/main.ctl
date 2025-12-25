@(feature(hosted))
mod libc {
    pub struct JmpBuf {
        pad: [u8; 0xc8],
    }

    pub extern fn abort(): never;
    pub extern fn exit(code: c_int): never;

    // The C macro `setjmp` calls _setjmp, which does not save the signal mask
    pub extern fn _setjmp(env: *mut JmpBuf): c_int;
    pub extern fn longjmp(env: *mut JmpBuf, val: c_int): never;
}

@(feature(hosted))
pub fn exit(code: u32): never {
    unsafe libc::exit(code as! c_int)
}

mod ryu;

@(feature(io))
pub mod io;

@(feature(hosted))
pub mod env;

@(feature(alloc))
pub mod alloc;

@(autouse)
mod prelude {
    pub use super::panic::panic;
    pub use super::panic::unreachable;
    pub use super::panic::assert;
    pub use super::panic::assert_eq;
    pub use super::panic::assert_ne;
    pub use super::panic::debug_assert;
    pub use super::string::str;
    pub use super::span::Span;
    pub use super::span::SpanMut;
    pub use super::opt::Option::*;
    pub use super::iter::Iterator;
    pub use super::impls::ext::*;
    pub use super::span::ext::*;
    pub use super::opt::ext::*;
    pub use super::range::ext::*;
    pub use super::any::ext::*;
    pub use super::fmt::ext::*;
    pub use super::fmt::write;
    pub use super::fmt::writeln;

    @(feature(alloc))
    pub use super::alloc::collections::*;
    @(feature(alloc))
    pub use super::alloc::ext::*;
    @(feature(io))
    pub use super::io::*;
}
