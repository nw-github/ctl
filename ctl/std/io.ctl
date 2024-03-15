import fn write(fd: c_int, buf: *raw c_void, count: uint): int;

pub fn println(s: str = "") {
    print(s);
    write(1, &b'\n' as *raw c_void, 1);
}

pub fn print(s: str) {
    write(1, s.as_raw() as *raw c_void, s.len());
}

pub fn eprintln(s: str = "") {
    eprint(s);
    write(2, &b'\n' as *raw c_void, 1);
}

pub fn eprint(s: str) {
    write(2, s.as_raw() as *raw c_void, s.len());
}
