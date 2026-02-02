pub union _Unwind_Context {}

pub union _Unwind_Reason_Code : c_int {
    _URC_NO_REASON = 0,
    // _URC_OK = 0, /* used by ARM EHABI */
    _URC_FOREIGN_EXCEPTION_CAUGHT = 1,

    _URC_FATAL_PHASE2_ERROR = 2,
    _URC_FATAL_PHASE1_ERROR = 3,
    _URC_NORMAL_STOP = 4,

    _URC_END_OF_STACK = 5,
    _URC_HANDLER_FOUND = 6,
    _URC_INSTALL_CONTEXT = 7,
    _URC_CONTINUE_UNWIND = 8,
    // _URC_FAILURE = 9, /* used by ARM EHABI */
}

pub type _Unwind_Word = uint;
pub type _Unwind_Trace_Fn = extern unsafe fn (^mut _Unwind_Context, ?^mut void) => _Unwind_Reason_Code;

pub extern fn _Unwind_GetIP(ctx: ^mut _Unwind_Context): _Unwind_Word;
pub extern fn _Unwind_SetIP(ctx: ^mut _Unwind_Context, ip: _Unwind_Word);
pub extern fn _Unwind_GetGR(ctx: ^mut _Unwind_Context, gr: c_int): _Unwind_Word;
pub extern fn _Unwind_SetGR(ctx: ^mut _Unwind_Context, gr: c_int, ip: _Unwind_Word);
pub extern fn _Unwind_Backtrace(trace: _Unwind_Trace_Fn, ctx: ?^mut void): _Unwind_Reason_Code;
