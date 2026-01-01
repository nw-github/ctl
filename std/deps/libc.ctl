pub struct JmpBuf {
    pad: [u8; 0xc8],
}

pub extern fn abort(): never;
pub extern fn exit(code: c_int): never;

pub extern fn malloc(size: uint): ?^mut void;
pub extern fn realloc(addr: ^mut void, size: uint): ?^mut void;

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
pub extern fn getpid(): c_int /* pid_t */;
pub extern fn backtrace(buffer: ^mut ?^mut void, size: c_int): c_int;

pub extern fn memmem(kw haystack: ^void, kw hlen: uint, kw needle: ^void, kw nlen: uint): ?^void;

pub mod atomic {
    pub union MemoryOrder {
        Relaxed,
        Consume,
        Acquire,
        Release,
        AcqRel,
        SeqCst,
    }

    @(c_opaque, c_name(ctl_atomic_store_explicit))
    pub extern fn atomic_store_explicit<T>(obj: *mut T, desired: T, order: u32);

    @(c_opaque, c_name(ctl_atomic_load_explicit))
    pub extern fn atomic_load_explicit<T>(obj: *T, order: u32): T;

    @(c_opaque, c_name(ctl_atomic_exchange_explicit))
    pub extern fn atomic_exchange_explicit<T>(obj: *mut T, desired: T, order: u32): T;

    @(c_opaque, c_name(ctl_atomic_compare_exchange_strong_explicit))
    pub extern fn atomic_compare_exchange_strong_explicit<T>(
        obj: *mut T,
        expected: *mut T,
        desired: T,
        success: u32,
        failure: u32,
    ): bool; // _Bool

    @(c_opaque, c_name(ctl_atomic_compare_exchange_weak_explicit))
    pub extern fn atomic_compare_exchange_weak_explicit<T>(
        obj: *mut T,
        expected: *mut T,
        desired: T,
        success: u32,
        failure: u32,
    ): bool; // _Bool

    @(c_opaque, c_name(ctl_atomic_fetch_add_explicit))
    pub extern fn atomic_fetch_add_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    @(c_opaque, c_name(ctl_atomic_fetch_sub_explicit))
    pub extern fn atomic_fetch_sub_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    @(c_opaque, c_name(ctl_atomic_fetch_and_explicit))
    pub extern fn atomic_fetch_and_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    @(c_opaque, c_name(ctl_atomic_fetch_or_explicit))
    pub extern fn atomic_fetch_or_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    @(c_opaque, c_name(ctl_atomic_fetch_xor_explicit))
    pub extern fn atomic_fetch_xor_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    @(c_opaque, c_name(ctl_atomic_is_lock_free))
    pub extern fn atomic_is_lock_free<T>(obj: *T): bool; // _Bool
}

pub mod math {
    pub extern fn sqrt(n: f64): f64;
    pub extern fn sin(n: f64): f64;
    pub extern fn cos(n: f64): f64;
    pub extern fn tan(n: f64): f64;
    pub extern fn floor(n: f64): f64;
    pub extern fn ceil(n: f64): f64;

    pub extern fn sqrtf(n: f32): f32;
    pub extern fn sinf(n: f32): f32;
    pub extern fn cosf(n: f32): f32;
    pub extern fn tanf(n: f32): f32;
    pub extern fn floorf(n: f32): f32;
    pub extern fn ceilf(n: f32): f32;
}
