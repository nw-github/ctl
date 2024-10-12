pub use intrin::panic;
pub use intrin::unreachable_unchecked;

pub fn unreachable(): never {
    panic("entered unreachable code");
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
}
