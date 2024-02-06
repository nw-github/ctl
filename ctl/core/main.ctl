#(intrinsic(panic))
pub import fn panic(s: string::str): never;

pub fn unreachable(): never {
    panic("entered unreachable code");
}

#(autouse)
mod prelude {
    pub use super::panic;
    pub use super::unreachable;
    pub use super::string::str;
    pub use super::opt::Option;
}
