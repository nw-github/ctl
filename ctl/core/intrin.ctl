use core::reflect::*;

#(intrinsic)
pub extern fn numeric_abs<T: Signed>(_: T): T;

#(intrinsic)
pub extern fn size_of<T>(): uint;

#(intrinsic)
pub extern fn align_of<T>(): uint;

#(intrinsic)
pub extern fn panic(s: str): never;

#(intrinsic)
pub extern fn unreachable_unchecked(): never;




#(c_opaque, c_name(CTL_MEMSET))
pub extern fn memset(dst: *raw c_void, c: c_int, len: uint): *raw c_void;

#(c_opaque, c_name(CTL_MEMCPY))
pub extern fn memcpy(dst: *raw c_void, src: *c_void, len: uint): *raw c_void;

#(c_opaque, c_name(CTL_MEMMOVE))
pub extern fn memmove(dst: *raw c_void, src: *c_void, len: uint): *raw c_void;

#(c_opaque, c_name(CTL_MEMCMP))
pub extern fn memcmp(dst: *c_void, src: *c_void, len: uint): c_int;

#(c_opaque, c_name(CTL_MALLOC))
pub extern fn malloc(size: uint, align: uint): ?*raw c_void;

#(c_opaque, c_name(CTL_REALLOC))
pub extern fn realloc(addr: *raw c_void, size: uint, align: uint): ?*raw c_void;

#(c_opaque, c_name(CTL_STRLEN))
pub extern fn strlen(ptr: *c_char): uint;
