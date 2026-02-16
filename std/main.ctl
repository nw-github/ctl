$[feature(hosted)]
pub mod proc;

$[feature(io)]
pub mod io;

$[feature(hosted)]
pub mod env;

$[feature(hosted)]
pub mod time;

$[feature(alloc)]
pub mod alloc;

$[cfg(test)]
pub mod test;

mod deps;
mod ryu;

$[feature(hosted)]
mod runtime;

$[autouse]
mod prelude {
    pub use super::panic::{panic, unreachable, assert, assert_eq, assert_ne, debug_assert};
    pub use super::str::str;
    pub use super::span::{Span, SpanMut};
    pub use super::opt::Option::{null, Some};
    pub use super::err::{Result, Result::{Ok, Err}};
    pub use super::iter::Iterator;
    pub use super::fmt::{write, writeln};
    pub use super::ops::Fn;
    pub use super::ffi::types::*;
    pub use super::reflect::Copy;

    $[feature(alloc)]
    pub use super::alloc::collections::*;
    $[feature(io)]
    pub use super::io::{print, println, eprint, eprintln, dbg};
}
