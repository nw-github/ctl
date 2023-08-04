pub fn panic(s: str) never {
    extern fn abort() never;
    extern fn write(fd: c_int, buf: *c_void, count: usize) isize;

    let prefix = "fatal error: ";
    write(2, prefix.as_ptr() as *c_void, prefix.len());
    write(2, s.as_ptr() as *c_void, s.len());
    write(2, &b'\n' as *c_void, 1);

    abort();
}

pub fn unreachable() never {
    panic("entered unreachable code");
}
