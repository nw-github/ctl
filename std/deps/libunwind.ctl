pub type _Unwind_Reason_Code = c_enum;

pub const _URC_NO_REASON: _Unwind_Reason_Code = 0;
// pub const _URC_OK: _Unwind_Reason_Code = 0; /* used by ARM EHABI */
pub const _URC_FOREIGN_EXCEPTION_CAUGHT: _Unwind_Reason_Code = 1;
pub const _URC_FATAL_PHASE2_ERROR: _Unwind_Reason_Code = 2;
pub const _URC_FATAL_PHASE1_ERROR: _Unwind_Reason_Code = 3;
pub const _URC_NORMAL_STOP: _Unwind_Reason_Code = 4;
pub const _URC_END_OF_STACK: _Unwind_Reason_Code = 5;
pub const _URC_HANDLER_FOUND: _Unwind_Reason_Code = 6;
pub const _URC_INSTALL_CONTEXT: _Unwind_Reason_Code = 7;
pub const _URC_CONTINUE_UNWIND: _Unwind_Reason_Code = 8;
// pub const _URC_FAILURE: _Unwind_Reason_Code = 9; /* used by ARM EHABI */

pub type _Unwind_Action = c_enum;
pub const _UA_SEARCH_PHASE: _Unwind_Action = 1;
pub const _UA_CLEANUP_PHASE: _Unwind_Action = 2;
pub const _UA_HANDLER_FRAME: _Unwind_Action = 4;
pub const _UA_FORCE_UNWIND: _Unwind_Action = 8;
pub const _UA_END_OF_STACK: _Unwind_Action = 16; /* gcc extension to C++ ABI */

pub union _Unwind_Context {}

pub type _Unwind_Word = uint;
pub type _Unwind_Exception_Class = u64;
pub type _Unwind_Trace_Fn = extern fn (^mut _Unwind_Context, ?^mut void) => _Unwind_Reason_Code;
pub type _Unwind_Exception_Cleanup_Fn = extern fn(_Unwind_Reason_Code, ?^mut _Unwind_Exception);
pub type _Unwind_Stop_Fn = extern fn(
    version: c_int,
    actions: _Unwind_Action,
    _Unwind_Exception_Class,
    ^mut _Unwind_Exception,
    ^mut _Unwind_Context,
    user: ?^mut void,
) => _Unwind_Reason_Code;

$[layout(C), align(16)]
pub struct _Unwind_Exception {
    pub class: _Unwind_Exception_Class,
    pub cleanup: _Unwind_Exception_Cleanup_Fn,
    pub private_1: _Unwind_Word = 0,
    pub private_2: _Unwind_Word = 0,
}

pub extern fn _Unwind_GetIP(ctx: ^mut _Unwind_Context): _Unwind_Word;
pub extern fn _Unwind_SetIP(ctx: ^mut _Unwind_Context, ip: _Unwind_Word);
pub extern fn _Unwind_GetGR(ctx: ^mut _Unwind_Context, gr: c_int): _Unwind_Word;
pub extern fn _Unwind_SetGR(ctx: ^mut _Unwind_Context, gr: c_int, ip: _Unwind_Word);
pub extern fn _Unwind_Backtrace(trace: _Unwind_Trace_Fn, user: ?^mut void): _Unwind_Reason_Code;
pub extern fn _Unwind_ForcedUnwind(ex: ^mut _Unwind_Exception, f: _Unwind_Stop_Fn, user: ?^mut void): _Unwind_Reason_Code;

// x86-64 only
pub const GR_FRAME_PTR: c_int = 6;
