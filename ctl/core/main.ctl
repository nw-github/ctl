pub use intrin::panic;

pub fn unreachable(): never {
    panic("entered unreachable code");
}

pub unsafe fn unreachable_unchecked(): never {
    // CTL_UNREACHABLE is a macro with no return value, generating it directly can cause errors
    intrin::builtin_unreachable();
}

#(autouse)
mod prelude {
    pub use super::panic;
    pub use super::unreachable;
    pub use super::string::str;
    pub use super::opt::Option::Some;
    pub use super::iter::Iterator;
    pub use super::ext::*;
}
