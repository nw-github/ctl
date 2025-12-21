use std::fmt::*;

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

@(feature(hosted, io))
@(panic_handler)
fn panic_handler(args: Arguments, loc: SourceLocation): never {
    if loc.func is ?func {
        eprintln("fatal error in function '{func}' at {loc}:\n\t{args}");
    } else {
        eprintln("fatal error at {loc}: {args}");
    }

    unsafe super::libc::abort();
}
