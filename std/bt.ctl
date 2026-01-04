use std::runtime::*;
use std::deps::libdwfl::*;
use std::deps::libc;
use std::panic::SourceLocation;

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
    func: ?[u8..],

    pub fn new(func: str): This => This(func: func.as_bytes());

    fn demangle_type_name(name: *mut str, f: *mut std::fmt::Formatter): ?void {
        match name.advance()? {
            'v' => write(f, "void"),
            'V' => write(f, "never"),
            'i' => write(f, "i{name}"),
            'u' => write(f, "u{name}"),
            'c' => write(f, "c_{name}"),
            'C' => write(f, "c_u{name}"),
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
            'n' => write(f, "{This(func: name.read_len_prefixed()?.as_bytes())}"),
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
            guard this.func is ?base else {
                return write(f, "??");
            }

            guard str::from_utf8(base) is ?base else {
                // TODO: write lossy
                return write(f, "??");
            }

            guard base.strip_prefix("CTL$") is ?mut name else {
                return write(f, base);
            }

            _ = This::demangle_name(&mut name, f);
        }
    }
}

pub struct SymbolInfo {
    pub file: ?str,
    pub line: u32,
    pub col:  u32,
    pub func: MaybeMangledName,
    pub offs: uint, // Offset from the function start
}

pub struct Call {
    addr: uint,

    pub fn addr(my this): uint => this.addr;

    @(feature(backtrace))
    pub fn symbolicate(my this): ?SymbolInfo {
        let dwfl = unsafe DWFL?;

        mut [lineno, colno] = [0ic; 2];
        mut addr = this.addr;
        unsafe {
            let line = dwfl_getsrc(dwfl, addr)?;
            let file = dwfl_lineinfo(line, &mut addr, &mut lineno, &mut colno, null, null)?;

            mut func: ?[u8..] = null;
            mut sym: Elf64_Sym;
            mut offs = 0u;
            if dwfl_addrmodule(dwfl, addr) is ?module and
                dwfl_module_addrinfo(module, addr, &mut offs, &mut sym, null, null, null) is ?sym
            {
                func = ?Span::new(sym.cast(), std::intrin::strlen(sym));
            }

            SymbolInfo(
                file: str::from_cstr(file),
                line: lineno as! u32,
                col: colno as! u32,
                func: MaybeMangledName(func:),
                offs:
            )
        }
    }

    @(feature(not(backtrace)))
    pub fn symbolicate(my this): ?SymbolInfo => null;
}

pub struct Backtrace {
    addrs: [?^mut void; 1024],
    count: uint,

    @(inline(never))
    pub fn capture(): This {
        mut addrs: [?^mut void; 1024] = [null; 1024];
        let count = unsafe libc::backtrace(addrs.as_raw_mut(), addrs.len() as! c_int);
        Backtrace(addrs:, count: count as! uint)
    }

    pub fn iter(this): BacktraceIter {
        BacktraceIter(addrs: this.addrs[..this.count].iter().skip(1))
    }
}

pub struct BacktraceIter {
    addrs: std::iter::Skip<*?^mut void, std::span::Iter<?^mut void>>,

    impl Iterator<Call> {
        fn next(mut this): ?Call {
            if this.addrs.next() is ?next {
                Call(addr: next is ?ptr then *ptr as uint else 0)
            }
        }
    }
}
