use core::reflect::*;

#(intrinsic)
pub import fn numeric_abs<T: Signed>(_: T): T;

#(intrinsic)
pub import fn numeric_cast<T: Numeric, U: Numeric>(_: T): U;

#(intrinsic)
pub import fn size_of<T>(): uint;

#(intrinsic)
pub import fn align_of<T>(): uint;

#(intrinsic)
pub import fn panic(s: core::string::str): never;

#(intrinsic)
pub import fn raw_offset<T, U: Integral>(ptr: *raw T, offset: U): *raw T;




#(c_opaque, c_name(CTL_UNREACHABLE))
pub import fn builtin_unreachable(): never;

#(c_opaque, c_name(CTL_MEMSET))
pub import fn memset(dst: *mut c_void, c: c_int, len: uint): *mut c_void;

#(c_opaque, c_name(CTL_MEMCPY))
pub import fn memcpy(dst: *mut c_void, src: *c_void, len: uint): *mut c_void;

#(c_opaque, c_name(CTL_MEMMOVE))
pub import fn memmove(dst: *mut c_void, src: *c_void, len: uint): *mut c_void;

#(c_opaque, c_name(CTL_MEMCMP))
pub import fn memcmp(dst: *c_void, src: *c_void, len: uint): c_int;

#(c_opaque, c_name(CTL_MALLOC))
pub import fn malloc(size: uint): ?*raw c_void;

#(c_opaque, c_name(CTL_REALLOC))
pub import fn realloc(addr: *mut c_void, size: uint): ?*raw c_void;

#(c_opaque, c_name(CTL_STRLEN))
pub import fn strlen(ptr: *c_char): uint;
