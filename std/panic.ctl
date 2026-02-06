use std::fmt::*;
use std::deps::{libc, libunwind as uw};

pub struct SourceLocation {
    pub file: str,
    pub line: u32,
    pub col:  u32,
    pub func: ?str,

    $[intrinsic(source_location)]
    pub fn here(): *This => This::here();

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            write(f, "{this.file}:{this.line}:{this.col}");
        }
    }
}

pub struct PanicInfo {
    args: Arguments,
    loc: *SrcLoc,

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            if this.loc.func is ?func {
                write(f, "fatal error in function '{func}' at {this.loc}:\n{this.args}")
            } else {
                write(f, "fatal error at {this.loc}: {this.args}")
            }
        }
    }
}

type SrcLoc = SourceLocation;

pub fn unreachable(loc: *SrcLoc = SrcLoc::here()): never {
    panic("entered unreachable code", loc:);
}

$[cold]
pub fn panic<T: Format>(args: T, loc: *SrcLoc = SrcLoc::here()): never {
    std::intrin::panic(&PanicInfo(args: "{args}", loc:))
}

// TODO: this should take <T: Format>, but there is currently no way to default that
pub fn assert(cond: bool, msg: ?Arguments = null, loc: *SrcLoc = SrcLoc::here()) {
    if !cond {
        if msg is ?msg {
            panic("assertion failed: {msg}", loc:);
        } else {
            panic("assertion failed", loc:);
        }
    }
}

pub fn assert_eq<Lhs: std::ops::Eq<Rhs>, Rhs>(lhs: Lhs, rhs: Rhs, loc: *SrcLoc = SrcLoc::here()) {
    if lhs != rhs {
        panic("assertion failed: '{lhs:?}' != '{rhs:?}'", loc:);
    }
}

pub fn assert_ne<Lhs: std::ops::Eq<Rhs>, Rhs>(lhs: Lhs, rhs: Rhs, loc: *SrcLoc = SrcLoc::here()) {
    if lhs == rhs {
        panic("assertion failed: '{lhs:?}' == '{rhs:?}'", loc:);
    }
}

$[inline(always)]
pub fn debug_assert(_cond: bool, _msg: ?Arguments = null, _loc: *SrcLoc = SrcLoc::here()) {
    $[cfg("ctl:debug")]
    assert(_cond, _msg, _loc);
}

$[thread_local, feature(hosted)]
static mut CTL_PANIC_JMPBUF: ?(libc::JmpBuf, ?^mut void) = null;

$[thread_local, feature(hosted)]
static mut CTL_PANIC_INFO: str = "";

$[thread_local, feature(hosted)]
static mut IS_PANICKING: bool = false;

$[thread_local, feature(hosted)]
static mut CTL_DUMMY_EXCEPTION: uw::_Unwind_Exception = uw::_Unwind_Exception(
    class: u64::from_le_bytes(*b"CTL\0\0\0NW"),
    cleanup: |_, _| {},
);

$[panic_handler, feature(hosted, io)]
fn panic_handler(info: *PanicInfo): never {
    unsafe if std::mem::replace(&mut IS_PANICKING, true) {
        eprintln("During the execution of the panic_handler, this panic occured:\n{info}");
        libc::abort();
    }

    $[cfg("!ctl:panic-abort")]
    unsafe if &mut CTL_PANIC_JMPBUF is ?data {
        CTL_PANIC_INFO = info.to_str();

        $[cfg("!ctl:panic-unwind")]
        libc::longjmp(&mut data.0, 1);

        $[cfg("ctl:panic-unwind")]
        uw::_Unwind_ForcedUnwind(&mut CTL_DUMMY_EXCEPTION, user: null, |_, act, _, _, ctx, _| {
            assert(act & uw::_UA_FORCE_UNWIND != 0);
            assert(act & uw::_UA_END_OF_STACK == 0);

            unsafe {
                let fp = uw::_Unwind_GetGR(ctx, uw::GR_FRAME_PTR).to_raw_mut::<void>();
                if &mut CTL_PANIC_JMPBUF is ?(jmp_buf, saved_fp) and saved_fp == ?fp {
                    libc::longjmp(jmp_buf, 1);
                }
            }

            uw::_URC_NO_REASON
        });

        unreachable();
    }

    eprintln(info);
    if std::bt::Resolver::new() is ?mut resolver {
        defer resolver.deinit();
        unsafe std::bt::backtrace(|&resolver, =mut i = 0u, pc| {
            if i != 0 {
                resolver.print_bt_line(i, pc);
            }

            i++;
            true
        });
    }

    unsafe libc::abort();
}

$[feature(hosted)]
pub fn catch_panic<F: Fn() => R, R>(func: F): Result<R, str> {
    $[cfg("!ctl:panic-abort")]
    unsafe {
        let (prev_buf, was_panicking) = (CTL_PANIC_JMPBUF.take(), IS_PANICKING);
        let buf = CTL_PANIC_JMPBUF.insert((std::mem::zeroed(), std::intrin::current_frame_addr()));
        defer {
            IS_PANICKING = was_panicking;
            CTL_PANIC_JMPBUF = prev_buf;
            CTL_PANIC_INFO = "";
        }

        // TODO: Make this an intrinsic so that we use the macro and follow the very specific
        // semantics of setjmp
        return match libc::_setjmp(&mut buf.0) {
            0 => Ok(func()),
            _ => Err(CTL_PANIC_INFO),
        };
    }

    $[cfg("ctl:panic-abort")]
    return Ok(func());
}

extension std::bt::Resolver {
    // TODO: this should be package-public
    pub fn print_bt_line(this, i: uint, addr: uint) {
        if this.resolve(addr) is ?{func, file, line, col, offs} {
            let func = func ?? std::bt::MaybeMangledName::from_str("??");
            eprintln("{i:>5}: {func} + {offs:#x} [{addr:#x}]\n{"":<8}at {file ?? "??"}:{line}:{col}");
        } else {
            eprintln("{i:>5}: ?? [{addr:#x}]");
        }
    }
}
