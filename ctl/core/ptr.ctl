pub fn raw_dangling<T>(): ^mut T {
    core::mem::align_of::<T>() as ^mut T
}

pub fn eq<T>(lhs: *T, rhs: *T): bool {
    lhs as ^mut T == rhs as ^mut T
}

pub use core::intrin::read_volatile;
pub use core::intrin::write_volatile;
