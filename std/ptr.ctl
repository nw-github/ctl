pub fn raw_dangling<T>(): ^mut T {
    std::mem::align_of::<T>() as ^mut T
}

pub fn eq<T>(lhs: *T, rhs: *T): bool {
    lhs as ^mut T == rhs as ^mut T
}

pub use std::intrin::read_volatile;
pub use std::intrin::write_volatile;
