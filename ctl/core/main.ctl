#(intrinsic)
pub import fn panic(s: string::str): never;

pub fn unreachable(): never {
    panic("entered unreachable code");
}

pub unsafe fn unreachable_unchecked(): never {
    #(c_opaque, c_name(CTL_UNREACHABLE))
    pub import fn builtin_unreachable(): never;

    // CTL_UNREACHABLE is a macro with no return value, generating it directly can cause errors
    builtin_unreachable();
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
