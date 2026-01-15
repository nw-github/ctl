use std::deps::{libgc, libc, libdwfl::*};

pub static mut CTL_ARGV: ?^mut ^mut c_char = null;
pub static mut CTL_ARGC: c_int = 0;

$[feature(backtrace)]
pub static mut DWFL: ?*mut Dwfl = null;

$[export, link_name("$ctl_stdlib_init"), feature(hosted)]
extern fn init(argc: c_int, argv: ?^mut ^mut c_char) {
    unsafe {
        $[cfg("!ctl:no-gc")]
        libgc::GC_init();

        $[feature(backtrace)]
        DWFL = init_dwfl();

        CTL_ARGC = argc;
        CTL_ARGV = argv;
    }
}

$[export, link_name("$ctl_stdlib_deinit"), feature(hosted)]
extern fn deinit() {
    unsafe {
        $[cfg("!ctl:no-gc")]
        libgc::GC_deinit();

        $[feature(backtrace)]
        if DWFL.take() is ?dwfl {
            dwfl_end(dwfl);
        }
    }
}

$[feature(backtrace)]
fn init_dwfl(): ?*mut Dwfl {
    static CALLBACKS: Dwfl_Callbacks = Dwfl_Callbacks(
        find_elf: dwfl_linux_proc_find_elf,
        find_debuginfo: dwfl_standard_find_debuginfo,
        section_address: null,
        debuginfo_path: null,
    );

    unsafe {
        let dwfl = dwfl_begin(&CALLBACKS)?;
        dwfl_report_begin(dwfl);
        dwfl_linux_proc_report(dwfl, libc::getpid());
        guard dwfl_report_end(dwfl, null, null) == 0 else {
            dwfl_end(dwfl);
            return null;
        }

        dwfl
    }
}
