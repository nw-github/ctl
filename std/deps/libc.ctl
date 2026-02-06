pub type size_t = uint;

$[layout(C)]
pub struct JmpBuf {
    pad: [u8; 0xc8],
}

pub extern fn abort(): never;
pub extern fn exit(code: c_int): never;
pub extern fn _exit(code: c_int): never;

$[unsafe(malloc)]
pub extern fn malloc(size: uint): ?^mut void;
pub extern fn realloc(addr: ^mut void, size: uint): ?^mut void;
pub extern fn free(addr: ?^mut void);

// The C macro `setjmp` calls _setjmp, which does not save the signal mask
pub extern fn _setjmp(env: *mut JmpBuf): c_int;
pub extern fn longjmp(env: *mut JmpBuf, val: c_int): never;

$[layout(C)]
pub struct Timespec {
    pub tv_sec: c_long,
    pub tv_nsec: c_long,
}

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

    $[c_macro("ctl_atomic_store_explicit")]
    pub extern fn atomic_store_explicit<T>(obj: *mut T, desired: T, order: u32);

    $[c_macro("ctl_atomic_load_explicit")]
    pub extern fn atomic_load_explicit<T>(obj: *T, order: u32): T;

    $[c_macro("ctl_atomic_exchange_explicit")]
    pub extern fn atomic_exchange_explicit<T>(obj: *mut T, desired: T, order: u32): T;

    $[c_macro("ctl_atomic_compare_exchange_strong_explicit")]
    pub extern fn atomic_compare_exchange_strong_explicit<T>(
        obj: *mut T,
        expected: *mut T,
        desired: T,
        success: u32,
        failure: u32,
    ): bool; // _Bool

    $[c_macro("ctl_atomic_compare_exchange_weak_explicit")]
    pub extern fn atomic_compare_exchange_weak_explicit<T>(
        obj: *mut T,
        expected: *mut T,
        desired: T,
        success: u32,
        failure: u32,
    ): bool; // _Bool

    $[c_macro("ctl_atomic_fetch_add_explicit")]
    pub extern fn atomic_fetch_add_explicit<T, U>(obj: *mut T, arg: U, order: u32): T;

    $[c_macro("ctl_atomic_fetch_sub_explicit")]
    pub extern fn atomic_fetch_sub_explicit<T, U>(obj: *mut T, arg: U, order: u32): T;

    $[c_macro("ctl_atomic_fetch_and_explicit")]
    pub extern fn atomic_fetch_and_explicit<T, U>(obj: *mut T, arg: U, order: u32): T;

    $[c_macro("ctl_atomic_fetch_or_explicit")]
    pub extern fn atomic_fetch_or_explicit<T, U>(obj: *mut T, arg: U, order: u32): T;

    $[c_macro("ctl_atomic_fetch_xor_explicit")]
    pub extern fn atomic_fetch_xor_explicit<T, U>(obj: *mut T, arg: U, order: u32): T;

    $[c_macro("ctl_atomic_is_lock_free")]
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

pub mod posix {
    use super::*;

    pub const CLOCK_MONOTONIC: c_int = 1;

    pub type pid_t = c_int;
    pub type uid_t = c_uint;
    pub type clock_t = c_long;
    pub type __SI_BAND_TYPE = c_long;
    pub type off_t = c_long;

    pub extern fn write(fd: c_int, buf: ^void, count: uint): int;
    pub extern fn clock_gettime(clockid: c_int, tp: *mut Timespec): c_int;
    pub extern fn nanosleep(time: *Timespec, remaining: ?*mut Timespec): c_int;
    pub extern fn getpid(): pid_t;

    $[layout(C)]
    pub struct sigset_t {
        pub __val: [c_ulong; 16], // 1024 / (8 * std::mem::size_of::<c_ulong>())
    }

    $[layout(C)]
    pub struct sigaction {
        pub __sigaction_handler: __sigaction_handler,
        pub sa_mask: sigset_t,
        pub sa_flags: c_int,
        pub sa_restorer: ?extern unsafe fn(),
    }

    $[layout(C)]
    unsafe union __sigaction_handler {
        sa_handler: ?extern unsafe fn(c_int),
        sa_sigaction: ?extern unsafe fn(c_int, ?^mut siginfo_t, ?^mut void),
    }

    $[layout(C)]
    pub struct siginfo_t {
        pub si_signo: c_int,
        pub si_errno: c_int,
        pub si_code: c_int,
        __pad0: c_int,
        pub _sifields: _sifields,
    }

    $[layout(C)]
    struct _sifields_kill {
        pub si_pid: pid_t,
        pub si_uid: uid_t,
    }

    $[layout(C)]
    struct _sifields_timer {
        pub si_tid: c_int,
        pub si_overrun: c_int,
        pub si_sigval: sigval,
    }

    $[layout(C)]
    struct _sifields_rt {
        pub si_pid: pid_t,
        pub si_uid: uid_t,
        pub si_sigval: sigval,
    }

    $[layout(C)]
    struct _sifields_sigchld {
        pub si_pid: pid_t,
        pub si_uid: uid_t,
        pub si_status: c_int,
        pub si_utime: clock_t,
        pub si_stime: clock_t,
    }

    $[layout(C)]
    struct _sifields_sigfault {
        pub si_addr: ?^mut void,
        pub si_addr_lsb: c_short,
        _pad: [uint; 2],
        /*
        union {
            struct {
                void *_lower;
                void *_upper;
            } _addr_bnd;
            uint32_t _pkey;
        } _bounds;
         */
    }

    $[layout(C)]
    struct _sifields_sigpoll {
        pub si_band: __SI_BAND_TYPE,
        pub si_fd: c_int,
    }

    $[layout(C)]
    struct _sifields_sigsys {
        pub _call_addr: ?^mut void,
        pub _syscall: c_int,
        pub _arch: c_uint,
    }

    $[layout(C)]
    pub unsafe union sigval {
        __sival_int: c_int,
        __sival_ptr: ?^mut void,
    }

    $[layout(C)]
    unsafe union _sifields {
        _pad: [c_int; 28],
	    /// kill().
        _kill: _sifields_kill,
	    /// POSIX.1b timers.
        _timer: _sifields_timer,
	    /// POSIX.1b signals.
        _rt: _sifields_rt,
	    /// SIGCHLD.
        _sigchld: _sifields_sigchld,
        /// SIGILL, SIGFPE, SIGSEGV, SIGBUS.
        _sigfault: _sifields_sigfault,
        /// SIGPOLL
        _sigpoll: _sifields_sigpoll,
        /// SIGSYS
        _sigsys: _sifields_sigsys,
    }

    $[layout(C)]
    pub struct stack_t {
        pub ss_sp: ?^mut void,
        pub ss_flags: c_int,
        pub ss_size: size_t,
    }

    pub const SA_SIGINFO: c_int = 4;
    pub const SA_ONSTACK: c_int = 0x08000000;

    pub const SIGILL: c_int = 4;
    pub const SIGABRT: c_int = 6;
    pub const SIGBUS: c_int = 7;
    pub const SIGFPE: c_int = 8;
    pub const SIGSEGV: c_int = 11;

    pub extern fn sigemptyset(set: ^mut sigset_t): c_int;
    /// ss and old_ss are restrict (may not overlap)
    pub extern fn sigaltstack(ss: ?^stack_t, old_ss: ?^mut stack_t): c_int;

    // XXX: Link name is set to avoid the conflict with our constructor for the sigaction type.
    $[link_name("sigaction")]
    /// set and old_act are restrict (may not overlap)
    pub extern fn sigaction_(sig: c_int, set: ?^sigaction, old_act: ?^mut sigaction): c_int;
    pub extern fn perror(msg: ^c_char);

    pub extern fn fork(): pid_t;
    pub extern fn waitpid(pid: pid_t, wstatus: ?^mut c_int, options: c_int): pid_t;

    pub extern fn mmap(addr: ?^mut void, len: uint, prot: c_int, flags: c_int, fd: c_int, off: off_t): ?^mut void;

    pub const MAP_PRIVATE: c_int = 0x2;
    pub const MAP_ANONYMOUS: c_int = 0x20;
    pub const MAP_STACK: c_int = 0x20000;

    pub const PROT_READ: c_int = 0x1;
    pub const PROT_WRITE: c_int = 0x2;

    pub const MAP_FAILED: ^mut void = (-1).to_raw_mut();
}

pub mod linux {
    use super::*;

    pub type greg_t = c_longlong;
    pub type gregset_t = [greg_t; 23];
    pub type fpregset_t = ?^mut _libc_fpstate;

    $[layout(C)]
    pub struct ucontext_t {
        pub uc_flags: c_ulong,
        pub uc_link: ?^mut ucontext_t,
        pub uc_stack: posix::stack_t,
        pub uc_mcontext: mcontext_t,
        pub uc_sigmask: posix::sigset_t,
        pub __fpregs_mem: _libc_fpstate,
        pub __ssp: [c_ulonglong; 4],
    }

    $[layout(C)]
    pub struct mcontext_t {
        pub gregs: gregset_t,
        pub fpregs: fpregset_t,
        pub __reserved1: [c_ulonglong; 8],
    }

    $[layout(C)]
    pub struct _libc_fpstate {
        pub cwd: u16,
        pub swd: u16,
        pub ftw: u16,
        pub fop: u16,
        pub rip: u64,
        pub rdp: u64,
        pub mxcsr: u32,
        pub mxcr_mask: u32,
        pub _st: [_libc_fpxreg; 8],
        pub _xmm: [_libc_xmmreg; 16],
        pub __glibc_reserved1: [u32; 24],
    }

    $[layout(C)]
    pub struct _libc_fpxreg {
        pub significand: [c_ushort; 4],
        pub exponent: c_ushort,
        pub __glibc_reserved1: [c_ushort; 3],
    }

    $[layout(C)]
    pub struct _libc_xmmreg {
        pub element: [u32; 4],
    }

    union GpReg: c_int {
        REG_R8,
        REG_R9,
        REG_R10,
        REG_R11,
        REG_R12,
        REG_R13,
        REG_R14,
        REG_R15,
        REG_RDI,
        REG_RSI,
        REG_RBP,
        REG_RBX,
        REG_RDX,
        REG_RAX,
        REG_RCX,
        REG_RSP,
        REG_RIP,
        REG_EFL,
        REG_CSGSFS,
        REG_ERR,
        REG_TRAPNO,
        REG_OLDMASK,
        REG_CR2,
    }

    pub use GpReg::*;
}
