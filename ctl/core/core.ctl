// export {
//     option,
//     string,
// }

pub fn panic(s: str) never {
    // TODO: c_int
    extern fn _Exit(code: c_int) never;

    // TODO: core is not allowed to depend on anything, but this is annoyingly redundant
    extern fn eprint(buf: *u8, len: usize);
    extern fn eprintln(buf: *u8, len: usize);

    let prefix = "fatal error: ";
    eprint(prefix.as_ptr(), prefix.len());
    eprintln(s.as_ptr(), s.len());

    _Exit(101);
}
