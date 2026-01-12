use std::deps::{libgc, libc, libdwfl::*};

$[feature(backtrace)]
pub static mut DWFL: ?*mut Dwfl = null;

$[export, link_name("$ctl_stdlib_init")]
extern fn init() {
    $[feature(boehm)]
    unsafe libgc::GC_init();

    $[feature(backtrace)]
    unsafe DWFL = init_dwfl();
}

$[export, link_name("$ctl_stdlib_deinit")]
extern fn deinit() {
    $[feature(boehm)]
    unsafe libgc::GC_deinit();

    $[feature(backtrace)]
    unsafe if DWFL.take() is ?dwfl {
        dwfl_end(dwfl);
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
