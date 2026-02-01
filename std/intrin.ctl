use std::reflect::{Tuple, TypeId, DynPtr};

$[intrinsic]
pub extern fn size_of<T>(): uint;

$[intrinsic]
pub extern fn align_of<T>(): uint;

$[intrinsic]
pub extern fn panic(args: *std::panic::PanicInfo): never;

$[intrinsic]
pub extern unsafe fn unreachable_unchecked(): never;

$[intrinsic]
pub extern fn type_id<T>(): TypeId;

$[intrinsic]
pub extern fn type_name<T>(): str;

$[intrinsic]
pub extern unsafe fn read_volatile<T>(p: ^T): T;

$[intrinsic]
pub extern unsafe fn write_volatile<T>(p: ^mut T, val: T);

$[intrinsic]
pub extern fn ptr_add_signed<T>(p: ^T, offs: int): ^T;

$[intrinsic]
pub extern fn ptr_sub_signed<T>(p: ^T, offs: int): ^T;

$[intrinsic]
pub extern fn ptr_add_unsigned<T>(p: ^T, offs: uint): ^T;

$[intrinsic]
pub extern fn ptr_sub_unsigned<T>(p: ^T, offs: uint): ^T;

$[intrinsic]
pub extern fn ptr_diff<T>(a: ^T, b: ^T): int;

$[intrinsic]
pub extern fn builtin_dbg<T>(self: *T, f: *mut std::fmt::Formatter);

$[intrinsic]
pub extern fn invoke_with_tuple<F, Args: Tuple, R>(self: *F, args: Args): R;

$[intrinsic]
pub extern fn vtable_of<T: DynPtr>(ptr: T): [unsafe fn()..];

$[intrinsic]
pub extern fn instance_ptr_of<T: DynPtr>(ptr: T): ^mut void;


$[c_macro("CTL_MEMSET")]
pub extern fn memset(dst: ^mut void, c: c_int, len: uint): ^mut void;

$[c_macro("CTL_MEMCPY")]
pub extern fn memcpy(dst: ^mut void, src: ^void, len: uint): ^mut void;

$[c_macro("CTL_MEMMOVE")]
pub extern fn memmove(dst: ^mut void, src: ^void, len: uint): ^mut void;

$[c_macro("CTL_MEMCMP")]
pub extern fn memcmp(dst: ^void, src: ^void, len: uint): c_int;

$[c_macro("CTL_STRLEN")]
pub extern fn strlen(ptr: ^c_char): uint;

$[safe, c_macro("CTL_LIKELY")]
pub extern fn likely(val: bool): bool;

$[safe, c_macro("CTL_UNLIKELY")]
pub extern fn unlikely(val: bool): bool;
