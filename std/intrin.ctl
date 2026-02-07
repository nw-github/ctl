use std::reflect::{Tuple, TypeId, DynPtr};

$[intrinsic]
extern "ctl" {
    pub safe fn size_of<T>(): uint;
    pub safe fn align_of<T>(): uint;

    pub safe fn panic(args: *std::panic::PanicInfo): never;

    pub unsafe fn unreachable_unchecked(): never;

    pub safe fn type_id<T>(): TypeId;
    pub safe fn type_name<T>(): str;

    pub unsafe fn read_volatile<T>(p: ^T): T;
    pub unsafe fn write_volatile<T>(p: ^mut T, val: T);

    pub safe fn ptr_add_signed<T>(p: ^T, offs: int): ^T;
    pub safe fn ptr_sub_signed<T>(p: ^T, offs: int): ^T;
    pub safe fn ptr_add_unsigned<T>(p: ^T, offs: uint): ^T;
    pub safe fn ptr_sub_unsigned<T>(p: ^T, offs: uint): ^T;
    pub safe fn ptr_diff<T>(a: ^T, b: ^T): int;

    pub safe fn builtin_dbg<T>(self: *T, f: *mut std::fmt::Formatter);
    pub safe fn invoke_with_tuple<F, Args: Tuple, R>(self: *F, args: Args): R;

    pub safe fn vtable_of<T: DynPtr>(ptr: T): [unsafe fn()..];
    pub safe fn instance_ptr_of<T: DynPtr>(ptr: T): ^mut void;

    pub safe fn current_frame_addr(): ?^mut void;
}

extern "c" {
    $[c_macro("CTL_MEMSET")]
    pub fn memset(dst: ^mut void, c: c_int, len: uint): ^mut void;

    $[c_macro("CTL_MEMCPY")]
    pub fn memcpy(dst: ^mut void, src: ^void, len: uint): ^mut void;

    $[c_macro("CTL_MEMMOVE")]
    pub fn memmove(dst: ^mut void, src: ^void, len: uint): ^mut void;

    $[c_macro("CTL_MEMCMP")]
    pub fn memcmp(dst: ^void, src: ^void, len: uint): c_int;

    $[c_macro("CTL_STRLEN")]
    pub fn strlen(ptr: ^c_char): uint;

    $[c_macro("CTL_LIKELY")]
    pub safe fn likely(val: bool): bool;

    $[c_macro("CTL_UNLIKELY")]
    pub safe fn unlikely(val: bool): bool;
}
