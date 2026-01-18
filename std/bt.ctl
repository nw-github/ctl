use std::runtime::*;
use std::deps::libdwfl::*;
use std::deps::libc;
use std::panic::SourceLocation;
use std::str::CStr;

extension Helpers for str {
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
            use std::str::ext::*;

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

pub struct Call {
    pub addr: uint,

    pub fn addr(my this): uint => this.addr;

    $[feature(backtrace)]
    pub fn symbolicate(my this): ?SymbolInfo {
        let dwfl = unsafe DWFL?;

        mut [lineno, colno] = [0 as c_int; 2];
        mut addr = this.addr;
        unsafe {
            let line = dwfl_getsrc(dwfl, addr)?;
            let file = dwfl_lineinfo(line, &mut addr, &mut lineno, &mut colno, null, null)?;

            mut func: ?MaybeMangledName = null;
            mut sym: Elf64_Sym;
            mut offs = 0u;
            if dwfl_addrmodule(dwfl, addr) is ?module and
                dwfl_module_addrinfo(module, addr, &mut offs, &mut sym, null, null, null) is ?sym
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

    $[feature(not(backtrace))]
    pub fn symbolicate(my this): ?SymbolInfo => null;
}

pub struct Backtrace {
    addrs: [Call],

    $[inline(never)]
    pub fn capture(): This {
        mut addrs: [Call] = @[];
        unsafe backtrace(|&mut addrs, mut i = 0,| (pc) {
            if i++ >= 2 {
                // TODO: allow this to fail
                addrs.push(Call(addr: pc));
            }

            true
        });

        Backtrace(addrs:)
    }

    pub fn iter(this): BacktraceIter => BacktraceIter(addrs: this.addrs.iter());
}

pub struct BacktraceIter {
    addrs: std::span::Iter<Call>,

    impl Iterator<Call> {
        fn next(mut this): ?Call => this.addrs.next().copied();
    }
}

pub struct Context {
    pub pc: uint,
    pub bp: uint,
    pub sp: uint,
    pub signal: bool,
}

$[inline(never)]
pub unsafe fn backtrace<F: Fn(uint) => bool>(f: F, kw ctx: ?Context = null): bool {
    $[feature(backtrace)]
    unsafe {
        use std::deps::libunwind::*;

        mut context: unw_context_t;
        mut cursor: unw_cursor_t;
        if ctx is ?{pc, bp, sp, signal} {
            if _Ux86_64_init_local2(
                &mut cursor,
                &mut context,
                signal then UNW_INIT_SIGNAL_FRAME else 0
            ) != 0
            {
                return false;
            }

            _Ux86_64_set_reg(&mut cursor, UNW_REG_SP, sp);
            _Ux86_64_set_reg(&mut cursor, UNW_REG_BP, bp);
            _Ux86_64_set_reg(&mut cursor, UNW_REG_IP, pc);
        } else {
            if _Ux86_64_getcontext(&mut context) != 0 {
                return false;
            } else if _Ux86_64_init_local(&mut cursor, &mut context) != 0 {
                return false;
            }
        }

        loop {
            mut ip = 0u;
            if _Ux86_64_get_reg(&mut cursor, UNW_REG_IP, &mut ip) != 0 {
                return false;
            }

            if !f(ip) {
                break;
            }
        } while _Ux86_64_step(&mut cursor) > 0;
    }

    true
}
