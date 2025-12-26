pub struct JmpBuf {
    pad: [u8; 0xc8],
}

pub extern fn abort(): never;
pub extern fn exit(code: c_int): never;

// The C macro `setjmp` calls _setjmp, which does not save the signal mask
pub extern fn _setjmp(env: *mut JmpBuf): c_int;
pub extern fn longjmp(env: *mut JmpBuf, val: c_int): never;

pub extern fn write(fd: c_int, buf: ^void, count: uint): int;

pub struct Timespec {
    pub tv_sec: c_long,
    pub tv_nsec: c_long,
}

pub const CLOCK_MONOTONIC: c_int = 1;

pub extern fn clock_gettime(clockid: c_int, tp: *mut Timespec): c_int;
pub extern fn nanosleep(time: *Timespec, remaining: ?*mut Timespec): c_int;
