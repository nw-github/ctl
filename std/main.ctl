$[feature(hosted)]
pub mod proc;

$[feature(io)]
pub mod io;

$[feature(hosted)]
pub mod env;

$[feature(alloc)]
pub mod alloc;

mod deps;
mod runtime;
mod ryu;

$[autouse]
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
    pub use super::err::Result;
    pub use super::err::Result::*;

    $[feature(alloc)]
    pub use super::alloc::collections::*;
    $[feature(io)]
    pub use super::io::*;
}
