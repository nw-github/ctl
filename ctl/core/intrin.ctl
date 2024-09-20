use core::reflect::*;

@(intrinsic)
pub extern fn numeric_abs<T: Signed>(_: T): T;

@(intrinsic)
pub extern fn size_of<T>(): uint;

@(intrinsic)
pub extern fn align_of<T>(): uint;

@(intrinsic)
pub extern fn panic(s: str): never;

@(intrinsic)
pub extern fn unreachable_unchecked(): never;

@(intrinsic)
pub extern fn type_id<T>(): core::intrin::TypeId;


@(c_opaque, c_name(CTL_MEMSET))
pub extern fn memset(dst: *raw u8, c: c_int, len: uint): *raw u8;

@(c_opaque, c_name(CTL_MEMCPY))
pub extern fn memcpy(dst: *raw u8, src: *raw u8, len: uint): *raw u8;

@(c_opaque, c_name(CTL_MEMMOVE))
pub extern fn memmove(dst: *raw u8, src: *raw u8, len: uint): *raw u8;

@(c_opaque, c_name(CTL_MEMCMP))
pub extern fn memcmp(dst: *raw u8, src: *raw u8, len: uint): c_int;

@(c_opaque, c_name(CTL_MALLOC))
pub extern fn malloc(size: uint, align: uint): ?*raw u8;

@(c_opaque, c_name(CTL_REALLOC))
pub extern fn realloc(addr: *raw u8, size: uint, align: uint): ?*raw u8;

@(c_opaque, c_name(CTL_STRLEN))
pub extern fn strlen(ptr: *c_char): uint;
