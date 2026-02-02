use std::fmt::*;
use std::deps::libc;

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

type SL = SourceLocation;

pub fn unreachable(loc: *SL = SL::here()): never {
    panic("entered unreachable code", loc:);
}

$[cold]
pub fn panic<T: Format>(args: T, loc: *SL = SL::here()): never {
    std::intrin::panic(&PanicInfo(args: "{args}", loc:))
}

// TODO: this should take <T: Format>, but there is currently no way to default that
pub fn assert(cond: bool, msg: ?Arguments = null, loc: *SL = SL::here()) {
    if !cond {
        if msg is ?msg {
            panic("assertion failed: {msg}", loc:);
        } else {
            panic("assertion failed", loc:);
        }
    }
}

pub fn assert_eq<Lhs: std::ops::Eq<Rhs>, Rhs>(lhs: Lhs, rhs: Rhs, loc: *SL = SL::here()) {
    if lhs != rhs {
        panic("assertion failed: '{lhs:?}' != '{rhs:?}'", loc:);
    }
}

pub fn assert_ne<Lhs: std::ops::Eq<Rhs>, Rhs>(lhs: Lhs, rhs: Rhs, loc: *SL = SL::here()) {
    if lhs == rhs {
        panic("assertion failed: '{lhs:?}' == '{rhs:?}'", loc:);
    }
}

$[inline(always)]
pub fn debug_assert(_cond: bool, _msg: ?Arguments = null, _loc: *SL = SL::here()) {
    $[cfg("ctl:debug")]
    assert(_cond, _msg, _loc);
}

$[thread_local, feature(hosted)]
static mut CTL_PANIC_JMPBUF: ?libc::JmpBuf = null;

$[thread_local, feature(hosted)]
static mut CTL_PANIC_INFO: str = "";

$[thread_local, feature(hosted)]
static mut IS_PANICKING: bool = false;

pub struct PanicInfo {
    args: Arguments,
    loc: *SL,

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

$[panic_handler, feature(hosted, io)]
fn panic_handler(info: *PanicInfo): never {
    if unsafe IS_PANICKING {
        eprintln("During the execution of the panic_handler, this panic occured:\n{info}");
        unsafe libc::abort();
    }

    unsafe {
        IS_PANICKING = true;
        if &mut CTL_PANIC_JMPBUF is ?jmp_buf {
            CTL_PANIC_INFO = info.to_str();
            libc::longjmp(jmp_buf, 1);
        }
    }

    eprintln(info);
    unsafe std::bt::backtrace(|=mut i = 0u, pc| {
        if i != 0 {
            print_bt_line(i, std::bt::Call(addr: pc));
        }

        i++;
        true
    });

    unsafe libc::abort();
}

$[feature(hosted)]
pub fn catch_panic<F: Fn() => R, R>(func: F): Result<R, str> {
    let (prev_buf, was_panicking) = unsafe (CTL_PANIC_JMPBUF.take(), IS_PANICKING);
    let buf = unsafe CTL_PANIC_JMPBUF.insert(std::mem::zeroed());
    // TODO: Make this an intrinsic so that we use the macro and follow the very specific semantics
    // of setjmp
    let res: Result<R, str> = match unsafe libc::_setjmp(buf) {
        0 => Ok(func()),
        _ => Err(unsafe CTL_PANIC_INFO),
    };

    unsafe {
        IS_PANICKING = was_panicking;
        CTL_PANIC_INFO = "";
        CTL_PANIC_JMPBUF = prev_buf;
    }

    res
}

// TODO: this should be package-public
pub fn print_bt_line(i: uint, addr: std::bt::Call) {
    if addr.symbolicate() is ?{func, file, line, col, offs} {
        let func = func ?? std::bt::MaybeMangledName::from_str("??");
        eprintln("{i:>5}: {func} + {offs:#x} [{addr.addr():#x}]\n{"":<8}at {file ?? "??"}:{line}:{col}");
    } else {
        eprintln("{i:>5}: ?? [{addr.addr():#x}]");
    }
}
