extern fn printf(fmt: *c_char, ...) c_int;
extern fn write(fd: c_int, buf: *c_void, count: usize) isize;

pub fn println(s: str) {
    printf("%.*s\n".as_c_str(), s.len() as! c_int, s.as_c_str());
}

pub fn print(s: str) {
    printf("%.*s".as_c_str(), s.len() as! c_int, s.as_c_str());
}

pub fn eprintln(s: str) {
    write(2, s.as_ptr() as *c_void, s.len());
    write(2, &10u8 as *c_void, 1);
}

pub fn eprint(s: str) {
    write(2, s.as_ptr() as *c_void, s.len());
}

fn convert_argv(argc: c_int, argv: **c_char) [str..] {
    use core::option::Option;
    use std::vec::Vec;

    let argc = argc as! usize;
    mut args: [str] = Vec::with_capacity(argc);
    mut i = 0usize;
    while i < argc {
        args.push(str::from_c_str(*core::mem::offset(argv, i++)));
    }
    return args.as_span();
}
