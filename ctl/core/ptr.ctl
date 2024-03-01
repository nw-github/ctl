use core::mem;

// TODO: make an extension for this type
#(intrinsic)
pub import fn raw_add<T>(ptr: *raw T, by: uint): *raw T;

#(intrinsic)
pub import fn raw_sub<T>(ptr: *raw T, by: uint): *raw T;

pub fn raw_dangling<T>(): *raw T {
    mem::align_of::<T>() as *raw T
}

pub fn eq<T>(lhs: *T, rhs: *T): bool {
    lhs as *raw T == rhs as *raw T
}
