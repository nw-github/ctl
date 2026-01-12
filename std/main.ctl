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
    pub use super::panic::{panic, unreachable, assert, assert_eq, assert_ne, debug_assert};
    pub use super::string::str;
    pub use super::span::{Span, SpanMut};
    pub use super::opt::Option::{null, Some};
    pub use super::err::{Result, Result::{Ok, Err}};
    pub use super::iter::Iterator;
    pub use super::impls::ext::*;
    pub use super::span::ext::*;
    pub use super::opt::ext::*;
    pub use super::range::ext::*;
    pub use super::any::ext::*;
    pub use super::fmt::ext::*;
    pub use super::fmt::{write, writeln};
    pub use super::ops::{Fn, ext::*};

    $[feature(alloc)]
    pub use super::alloc::collections::*;
    $[feature(io)]
    pub use super::io::{print, println, eprint, eprintln};
}
