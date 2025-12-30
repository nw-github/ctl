use std::fmt::*;
use std::deps::libc;

pub struct SourceLocation {
    pub file: str,
    pub line: u32,
    pub col:  u32,
    pub func: ?str,

    @(intrinsic(source_location))
    pub fn here(): This => SourceLocation::here();

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            write(f, "{this.file}:{this.line}:{this.col}");
        }
    }
}

pub fn unreachable(loc: SourceLocation = SourceLocation::here()): never {
    panic("entered unreachable code", loc:);
}

pub fn panic<T: Format>(args: T, loc: SourceLocation = SourceLocation::here()): never {
    std::intrin::panic("{args}", loc)
}

// TODO: this should take <T: Format>, but there is currently no way to default that
pub fn assert(cond: bool, msg: ?Arguments = null, loc: SourceLocation = SourceLocation::here()) {
    if !cond {
        if msg is ?msg {
            panic("assertion failed: {msg}", loc:);
        } else {
            panic("assertion failed", loc:);
        }
    }
}

pub fn assert_eq<Lhs: Debug + std::ops::Eq<Rhs>, Rhs: Debug>(
    lhs: Lhs,
    rhs: Rhs,
    loc: SourceLocation = SourceLocation::here(),
) {
    if lhs != rhs {
        panic("assertion failed: '{lhs:?}' != '{rhs:?}'", loc:);
    }
}

pub fn assert_ne<Lhs: Debug + std::ops::Eq<Rhs>, Rhs: Debug>(
    lhs: Lhs,
    rhs: Rhs,
    loc: SourceLocation = SourceLocation::here(),
) {
    if lhs == rhs {
        panic("assertion failed: '{lhs:?}' == '{rhs:?}'", loc:);
    }
}

// TODO: this should take <T: Format>, but there is currently no way to default that
@(inline(always))
pub fn debug_assert(
    _cond: bool,
    _msg: ?Arguments = null,
    _loc: SourceLocation = SourceLocation::here(),
) {
    @(feature(debug))
    if !_cond {
        if _msg is ?msg {
            panic("debug assertion failed: {msg}", loc: _loc);
        } else {
            panic("debug assertion failed", loc: _loc);
        }
    }
}

@(thread_local, feature(hosted))
static mut CTL_PANIC_JMPBUF: ?libc::JmpBuf = null;

@(thread_local, feature(hosted))
static mut CTL_PANIC_INFO: str = "";

@(thread_local, feature(hosted))
static mut IS_PANICKING: bool = false;

pub struct PanicInfo {
    args: Arguments,
    loc: SourceLocation,

    impl Format {
        fn fmt(this, f: *mut Formatter) {
            if this.loc.func is ?func {
                write(f, "fatal error in function '{func}' at {this.loc}:\n\t{this.args}")
            } else {
                write(f, "fatal error at {this.loc}: {this.args}")
            }
        }
    }
}

@(panic_handler, feature(hosted, io))
fn panic_handler(args: Arguments, loc: SourceLocation): never {
    let info = PanicInfo(args:, loc:);
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
    unsafe libc::abort();
}

@(feature(hosted))
pub fn catch_panic<R, A>(func: fn(A) => R, arg: A): Result<R, str> {
    let buf = unsafe CTL_PANIC_JMPBUF.insert(std::mem::zeroed());
    // TODO: Make this an intrinsic so that we use the macro and follow the very specific semantics
    // of setjmp
    let res: Result<R, str> = match unsafe libc::_setjmp(buf) {
        0 => Ok(func(arg)),
        _ => Err(unsafe CTL_PANIC_INFO),
    };

    unsafe {
        IS_PANICKING = false;
        CTL_PANIC_INFO = "";
        CTL_PANIC_JMPBUF.take();
    }

    res
}
