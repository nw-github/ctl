extern fn write(fd: c_int, buf: *c_void, count: uint): int;

pub fn println(s: str = "") {
    print(s);
    print("\n");
}

pub fn print(s: str) {
    unsafe write(1, s.as_raw() as *c_void, s.len());
}

pub fn eprintln(s: str = "") {
    eprint(s);
    eprint("\n");
}

pub fn eprint(s: str) {
    unsafe write(2, s.as_raw() as *c_void, s.len());
}
