extern fn write(fd: c_int, buf: *c_void, count: usize): isize;

pub fn println(s: str) {
    print(s);
    unsafe write(1, &b'\n' as *c_void, 1);
}

pub fn print(s: str) {
    unsafe write(1, s.as_ptr() as *c_void, s.len());
}

pub fn eprintln(s: str) {
    eprint(s);
    unsafe write(2, &b'\n' as *c_void, 1);
}

pub fn eprint(s: str) {
    unsafe write(2, s.as_ptr() as *c_void, s.len());
}

fn convert_argv(argc: c_int, argv: **c_char): [str..] {
    let argc = argc as! usize;
    mut args: [str] = std::vec::Vec::with_capacity(argc);
    mut i = 0usize;
    while i < argc {
        args.push(str::from_c_str(*unsafe core::ptr::offset(argv, i++)));
    }
    args.as_span()
}

#{autouse}
mod prelude {
    pub use super::vec::Vec;
    pub use super::map::Map;
    pub use super::set::Set;
    pub use super::string::StringExt;
}
