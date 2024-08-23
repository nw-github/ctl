use core::reflect::*;

#(intrinsic)
pub extern fn numeric_abs<T: Signed>(_: T): T;

#(intrinsic)
pub extern fn numeric_cast<T: Numeric, U: Numeric>(_: T): U;

#(intrinsic)
pub extern fn numeric_lt<T: Numeric, U: Numeric>(lhs: T, rhs: U): bool;

#(intrinsic)
pub extern fn size_of<T>(): uint;

#(intrinsic)
pub extern fn align_of<T>(): uint;

#(intrinsic)
pub extern fn panic(s: core::string::str): never;

#(intrinsic)
pub extern fn unreachable_unchecked(): never;

#(intrinsic)
pub extern fn raw_offset<T, U: Integral>(ptr: *raw T, offset: U): *raw T;




#(c_opaque, c_name(CTL_MEMSET))
pub extern fn memset(dst: *mut c_void, c: c_int, len: uint): *mut c_void;

#(c_opaque, c_name(CTL_MEMCPY))
pub extern fn memcpy(dst: *mut c_void, src: *c_void, len: uint): *mut c_void;

#(c_opaque, c_name(CTL_MEMMOVE))
pub extern fn memmove(dst: *mut c_void, src: *c_void, len: uint): *mut c_void;

#(c_opaque, c_name(CTL_MEMCMP))
pub extern fn memcmp(dst: *c_void, src: *c_void, len: uint): c_int;

#(c_opaque, c_name(CTL_MALLOC))
pub extern fn malloc(size: uint): ?*raw c_void;

#(c_opaque, c_name(CTL_REALLOC))
pub extern fn realloc(addr: *mut c_void, size: uint): ?*raw c_void;

#(c_opaque, c_name(CTL_STRLEN))
pub extern fn strlen(ptr: *c_char): uint;
