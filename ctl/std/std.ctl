pub fn println(s: str) {
    extern fn println(buf: *u8, len: usize);

    println(s.as_ptr(), s.len());
}

pub fn print(s: str) {
    extern fn print(buf: *u8, len: usize);

    print(s.as_ptr(), s.len());
}

pub fn eprintln(s: str) {
    extern fn eprintln(buf: *u8, len: usize);

    eprintln(s.as_ptr(), s.len());
}

pub fn eprint(s: str) {
    extern fn eprint(buf: *u8, len: usize);

    eprint(s.as_ptr(), s.len());
}

