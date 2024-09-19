extern fn write(fd: c_int, buf: *c_void, count: uint): int;

pub fn println(s: str = "") {
    print(s);
    unsafe write(1, &b'\n' as *c_void, 1);
}

pub fn print(s: str) {
    unsafe write(1, s.as_raw() as *c_void, s.len());
}

pub fn eprintln(s: str = "") {
    eprint(s + "\n");
    // unsafe write(2, &b'\n' as *c_void, 1);
}

pub fn eprint(s: str) {
    unsafe write(2, s.as_raw() as *c_void, s.len());
}
