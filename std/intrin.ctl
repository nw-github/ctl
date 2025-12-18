use std::reflect::*;

@(intrinsic)
pub extern fn numeric_abs<T: Signed>(_: T): T;

@(intrinsic)
pub extern fn size_of<T>(): uint;

@(intrinsic)
pub extern fn align_of<T>(): uint;

@(intrinsic)
pub extern fn panic(args: std::fmt::Arguments, loc: std::panic::SourceLocation): never;

@(intrinsic)
pub extern fn unreachable_unchecked(): never;

@(intrinsic)
pub extern fn type_id<T>(): TypeId;

@(intrinsic)
pub extern fn type_name<T>(): str;

@(intrinsic)
pub extern unsafe fn read_volatile<T>(p: ^T): T;

@(intrinsic)
pub extern unsafe fn write_volatile<T>(p: ^mut T, val: T);

@(intrinsic)
pub extern fn ptr_add_signed<T>(p: ^T, offs: int): ^T;

@(intrinsic)
pub extern fn ptr_sub_signed<T>(p: ^T, offs: int): ^T;

@(intrinsic)
pub extern fn ptr_add_unsigned<T>(p: ^T, offs: uint): ^T;

@(intrinsic)
pub extern fn ptr_sub_unsigned<T>(p: ^T, offs: uint): ^T;

@(intrinsic)
pub extern fn ptr_diff<T>(a: ^T, b: ^T): int;



@(c_opaque, c_name(CTL_MEMSET))
pub extern fn memset(dst: ^mut u8, c: c_int, len: uint): ^mut u8;

@(c_opaque, c_name(CTL_MEMCPY))
pub extern fn memcpy(dst: ^mut u8, src: ^u8, len: uint): ^mut u8;

@(c_opaque, c_name(CTL_MEMMOVE))
pub extern fn memmove(dst: ^mut u8, src: ^u8, len: uint): ^mut u8;

@(c_opaque, c_name(CTL_MEMCMP))
pub extern fn memcmp(dst: ^u8, src: ^u8, len: uint): c_int;

@(c_opaque, c_name(CTL_STRLEN))
pub extern fn strlen(ptr: ^c_char): uint;

@(safe, c_opaque, c_name(CTL_LIKELY))
pub extern fn likely(val: bool): bool;

@(safe, c_opaque, c_name(CTL_UNLIKELY))
pub extern fn unlikely(val: bool): bool;
