@(feature(hosted))
mod libc {
    pub extern fn abort(): never;
    pub extern fn exit(code: c_int): never;
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
    pub use super::string::str;
    pub use super::opt::Option::*;
    pub use super::iter::Iterator;
    pub use super::impls::*;
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
