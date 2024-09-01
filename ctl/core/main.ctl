pub use intrin::panic;
pub use intrin::unreachable_unchecked;

pub fn unreachable(): never {
    panic("entered unreachable code");
}

#(autouse)
mod prelude {
    pub use super::panic;
    pub use super::unreachable;
    pub use super::string::str;
    pub use super::opt::Option::Some;
    pub use super::iter::Iterator;
    pub use super::ext::*;
    pub use super::span::SpanEq;
    pub use super::span::SpanMutEq;
    pub use super::span::SpanMutSort;
}
