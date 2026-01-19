$[layout(C)]
pub struct unw_context_t {
    _pad: [u8; 0x3c8],
}

$[layout(C)]
pub struct unw_cursor_t {
    _pad: [u8; 0x3f8],
}

pub extern fn _Ux86_64_getcontext(ctx: ^mut unw_context_t): c_int;
pub extern fn _Ux86_64_init_local(cursor: ^mut unw_cursor_t, ctx: ^mut unw_context_t): c_int;
pub extern fn _Ux86_64_init_local2(
    cursor: ^mut unw_cursor_t,
    ctx: ^mut unw_context_t,
    flags: c_int,
): c_int;

pub extern fn _Ux86_64_set_reg(cursor: ^mut unw_cursor_t, regnum: c_int, val: uint): c_int;
pub extern fn _Ux86_64_get_reg(cursor: ^mut unw_cursor_t, regnum: c_int, val: *mut uint): c_int;

pub extern fn _Ux86_64_step(cursor: ^mut unw_cursor_t): c_int;

pub const UNW_INIT_SIGNAL_FRAME: c_int = 1;

pub const UNW_REG_BP: c_int = 6;
pub const UNW_REG_SP: c_int = 7;
pub const UNW_REG_IP: c_int = 16;
