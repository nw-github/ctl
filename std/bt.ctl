use std::runtime::*;
use std::deps::libdwfl::*;
use std::deps::libc;
use std::panic::SourceLocation;
use std::str::CStr;

extension str {
    fn advance_if_eq(mut this, v: char): bool {
        if this.chars().next() is ?ch and ch == v {
            *this = unsafe this.substr_unchecked(ch.len_utf8()..);
            true
        } else {
            false
        }
    }

    fn advance(mut this): ?char {
        let ch = this.chars().next()?;
        *this = unsafe this.substr_unchecked(ch.len_utf8()..);
        ch
    }

    fn take_digits(mut this): ?uint {
        mut end = 0u;
        mut iter = this.chars();
        while iter.next() is ?ch and ch.is_ascii_digit() {
            end += ch.len_utf8();
        }

        let num = uint::from_str_radix(this.substr(..end)?, 10)?;
        *this = unsafe this.substr_unchecked(end..);
        num
    }

    fn is_all_digits(this): bool {
        for ch in this.chars() {
            if !ch.is_ascii_digit() {
                return false;
            }
        }
        true
    }

    fn read_len_prefixed(mut this): ?str {
        let len = this.take_digits()?;
        let text = this.substr(..len)?;
        *this = unsafe this.substr_unchecked(len..);
        text
    }
}

pub struct MaybeMangledName {
    func: [u8..],

    pub fn from_str(func: str): This => This(func: func.as_bytes());
    pub fn from_bytes(func: [u8..]): This => This(func:);

    fn demangle_type_name(name: *mut str, f: *mut std::fmt::Formatter): ?void {
        match name.advance()? {
            'v' => write(f, "void"),
            'V' => write(f, "never"),
            'i' => write(f, "i{name}"),
            'u' => write(f, "u{name}"),
            'I' => write(f, "int"),
            'U' => write(f, "uint"),
            'f' => write(f, "f32"),
            'F' => write(f, "f64"),
            'B' => write(f, "bool"),
            'X' => write(f, "char"),
            'p' => { write(f, "*"); This::demangle_type_name(name, f) }
            'P' => { write(f, "*mut "); This::demangle_type_name(name, f) }
            'r' => { write(f, "^"); This::demangle_type_name(name, f) }
            'R' => { write(f, "^mut "); This::demangle_type_name(name, f) }
            'd' => { write(f, "*dyn "); This::demangle_type_name(name, f) }
            'D' => { write(f, "*dyn mut "); This::demangle_type_name(name, f) }
            'T' => This::demangle_name(name, f),
            'A' => {
                let len = name.take_digits()?;
                write(f, "[");
                This::demangle_type_name(name, f)?;
                write(f, "; {len}]");
            }
            'L' => {
                mut wrote = false;
                write(f, "(");
                while !name.is_empty() {
                    if wrote {
                        write(f, ", ");
                    }

                    let member = name.read_len_prefixed()?.substr(1u..)?;
                    if !member.is_all_digits() {
                        write(f, "{member}: ");
                    }

                    This::demangle_len_prefixed(name, f, "", &mut wrote, is_type: true)?;
                }
                write(f, ")");
            }
            'N' => {
                let abi = name.advance()?;
                match abi.to_ascii_upper() {
                    'A' => {},
                    'C' => write(f, #"extern "C" "#),
                    'S' => write(f, #"extern "sys" "#),
                    'D' => write(f, #"extern "x86-stdcall" "#),
                    'F' => write(f, #"extern "x86-fastcall" "#),
                    'T' => write(f, #"extern "x86-thiscall" "#),
                    _   => write(f, #"extern "???" "#),
                }

                if abi.is_ascii_lower() {
                    write(f, "unsafe ");
                }

                write(f, "fn(");
                mut wrote = false;
                while !name.is_empty() and !name.advance_if_eq('n') {
                    This::demangle_len_prefixed(name, f, ", ", &mut wrote, is_type: true)?;
                }
                write(f, ") => ");
                This::demangle_type_name(name, f)?;
            }
            'n' => write(f, "{This::from_str(name.read_len_prefixed()?)}"),
            _ => null,
        }
    }

    fn demangle_len_prefixed(
        name: *mut str,
        f: *mut std::fmt::Formatter,
        prefix: str,
        wrote: *mut bool,
        is_type: bool = false,
    ): ?void {
        mut text = name.read_len_prefixed()?;
        if std::mem::replace(wrote, true) {
            write(f, prefix);
        }

        if is_type {
            This::demangle_type_name(&mut text, f)?;
        } else {
            write(f, text);
        }

        if name.advance_if_eq('G') {
            write(f, "<");
            let mut wrote = false;
            loop {
                This::demangle_len_prefixed(name, f, ", ", &mut wrote, is_type: true)?;
            } while !name.advance_if_eq('g');
            write(f, ">");
        }

        if name.advance_if_eq('S') {
            write(f, "[This = ");
            This::demangle_len_prefixed(name, f, "", &mut false, is_type: true)?;
            write(f, "]");
        } else if name.advance_if_eq('I') {
            write(f, "[impl ");
            This::demangle_len_prefixed(name, f, "", &mut false, is_type: true)?;
            write(f, "]");
        }
    }

    fn demangle_name(name: *mut str, f: *mut std::fmt::Formatter): ?void {
        mut wrote = false;
        while !name.is_empty() {
            This::demangle_len_prefixed(name, f, "::", &mut wrote)?;
        }
    }

    impl std::fmt::Format {
        fn fmt(this, f: *mut std::fmt::Formatter) {
            guard str::from_utf8(this.func) is ?base else {
                for ch in this.func.iter_chars_lossy() {
                    f.write_char(ch);
                }
                return;
            }

            guard base.strip_prefix("CTL$") is ?mut name else {
                return write(f, base);
            }

            if This::demangle_name(&mut name, f) is null {
                write(f, "??");
            }
        }
    }
}

pub struct SymbolInfo {
    pub file: ?str,
    pub line: u32,
    pub col:  u32,
    pub func: ?MaybeMangledName,
    pub offs: uint, // Offset from the function start
}

$[feature(dw)]
struct DwflResolver {
    dwfl: ?*mut Dwfl,

    pub fn new(): ?This {
        static CALLBACKS: Dwfl_Callbacks = Dwfl_Callbacks(
            find_elf: dwfl_linux_proc_find_elf,
            find_debuginfo: dwfl_standard_find_debuginfo,
            section_address: null,
            debuginfo_path: null,
        );

        unsafe {
            let dwfl = dwfl_begin(&CALLBACKS)?;
            dwfl_report_begin(dwfl);
            guard dwfl_linux_proc_report(dwfl, libc::posix::getpid()) == 0 else {
                dwfl_end(dwfl);
                return null;
            }
            guard dwfl_report_end(dwfl, null, null) == 0 else {
                dwfl_end(dwfl);
                return null;
            }
            This(dwfl:)
        }
    }

    pub fn deinit(mut this) {
        if this.dwfl.take() is ?dwfl {
            unsafe dwfl_end(dwfl);
        }
    }

    pub fn resolve(this, mut addr: uint): ?SymbolInfo {
        use std::mem::Uninit;

        let dwfl = this.dwfl?;

        mut [lineno, colno] = [0 as c_int; 2];
        unsafe {
            let line = dwfl_getsrc(dwfl, addr)?;
            let file = dwfl_lineinfo(line, &mut addr, &mut lineno, &mut colno, null, null)?;

            mut func: ?MaybeMangledName = null;
            mut sym = Uninit::<Elf64_Sym>::uninit();
            mut offs = 0u;
            if dwfl_addrmodule(dwfl, addr) is ?module and
                dwfl_module_addrinfo(module, addr, &mut offs, sym.as_raw_mut(), null, null, null) is ?sym
            {
                func = MaybeMangledName(func: Span::new(sym.cast(), std::intrin::strlen(sym)));
            }

            SymbolInfo(
                file: CStr::new(file).as_str(),
                line: u32::try_from(lineno) ?? 0,
                col: u32::try_from(colno) ?? 0,
                func:,
                offs:
            )
        }
    }
}

struct NoOpResolver {
    pub fn new(): ?This { null }
    pub fn deinit(mut this) { }
    pub fn resolve(this, _addr: uint): ?SymbolInfo { null }
}

$[feature(dw)]
pub type Resolver = DwflResolver;

$[feature(not(dw))]
pub type Resolver = NoOpResolver;

pub struct Backtrace {
    addrs: [uint],

    $[inline(never)]
    pub fn capture(): This {
        mut addrs: [uint] = @[];
        unsafe backtrace(|&mut addrs, =mut i = 0, pc| {
            if i++ >= 2 {
                // TODO: allow this to fail
                addrs.push(pc);
            }

            true
        });

        Backtrace(addrs:)
    }

    pub fn iter(this): BacktraceIter => BacktraceIter(addrs: this.addrs.iter());
}

pub struct BacktraceIter {
    addrs: std::span::Iter<uint>,

    impl Iterator<uint> {
        fn next(mut this): ?uint => this.addrs.next().copied();
    }
}

$[inline(never)]
pub unsafe fn backtrace<F: Fn(uint) => bool>(f: F, kw start_pc: ?uint = null) {
    $[feature(backtrace)]
    unsafe {
        use std::deps::libunwind::*;

        struct State<F> {
            ignore_until: ?uint,
            callback: F,
        }

        extern fn callback<F: Fn(uint) => bool>(
            ctx: ^mut _Unwind_Context,
            user: ?^mut void,
        ): _Unwind_Reason_Code
        {
            let state: ^mut State<F> = unsafe std::mem::bit_cast(user);
            let pc = unsafe _Unwind_GetIP(ctx);
            if pc == 0 {
                return _URC_NO_REASON;
            }

            let state = unsafe &mut *state;
            if state.ignore_until is ?start_pc {
                if start_pc != pc {
                    return _URC_NO_REASON;
                }

                state.ignore_until = null;
            }

            if !(state.callback)(pc) {
                return _URC_END_OF_STACK;
            }

            _URC_NO_REASON
        }

        mut state = State(ignore_until: start_pc, callback: f);
        _Unwind_Backtrace(callback::<F>, ?(&raw mut state).cast());
    }
}

$[feature(alloc)]
pub fn demangle_name(name: [u8..]): ?str {
    mut name = str::from_utf8(name)?.strip_prefix("CTL$")?;
    mut builder = std::fmt::StringBuilder::new();
    MaybeMangledName::demangle_name(&mut name, &mut std::fmt::Formatter::new(&mut builder))?;
    builder.into_str()
}
