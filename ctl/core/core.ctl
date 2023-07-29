pub fn panic(s: str) never {
    extern fn _Exit(code: c_int) never;
    extern fn write(fd: c_int, buf: *c_void, count: usize) isize;

    // TODO: core is not allowed to depend on anything, but this is annoyingly redundant
    let prefix = "fatal error: ";
    write(2, prefix.as_ptr() as *c_void, prefix.len());
    write(2, s.as_ptr() as *c_void, s.len());
    write(2, &10u8 as *c_void, 1);

    _Exit(101);
}

pub fn unreachable() never {
    panic("entered unreachable code");
}
