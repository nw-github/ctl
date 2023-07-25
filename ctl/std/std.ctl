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

fn convert_argv(argc: c_int, argv: **c_char) [str..] {
    use core::option::Option;
    use std::vec::Vec;

    {
        let argc = { yield argc as! usize; };
        mut args: [str] = Vec::with_capacity(argc);
        mut i = 0usize;
        while i < argc {
            args.push(core::string::str::from_c_str(*core::mem::offset(argv, i++)));
        }
        return args.as_span();
    }
}
