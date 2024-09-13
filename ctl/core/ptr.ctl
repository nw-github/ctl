pub fn raw_dangling<T>(): *raw T {
    core::mem::align_of::<T>() as *raw T
}

pub fn eq<T>(lhs: *T, rhs: *T): bool {
    lhs as *raw T == rhs as *raw T
}
