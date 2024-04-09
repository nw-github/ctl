import fn write(fd: c_int, buf: *c_void, count: uint): int;

pub fn println(s: str = "") {
    print(s);
    write(1, unsafe &b'\n' as *c_void, 1);
}

pub fn print(s: str) {
    write(1, unsafe s.as_raw() as *c_void, s.len());
}

pub fn eprintln(s: str = "") {
    eprint(s);
    write(2, unsafe &b'\n' as *c_void, 1);
}

pub fn eprint(s: str) {
    write(2, unsafe s.as_raw() as *c_void, s.len());
}
