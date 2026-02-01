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
        {
            DWFL = init_dwfl();

            install_fault_handler();
        }

        CTL_ARGC = argc;
        CTL_ARGV = argv;
    }
}

$[export, link_name("$ctl_stdlib_deinit"), feature(hosted)]
extern fn deinit() {
    $[cfg("!ctl:no-gc")]
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
        dwfl_linux_proc_report(dwfl, libc::posix::getpid());
        guard dwfl_report_end(dwfl, null, null) == 0 else {
            dwfl_end(dwfl);
            return null;
        }

        dwfl
    }
}

$[feature(backtrace)]
fn install_fault_handler() {
    use libc::{posix::*, linux};

    unsafe {
        mut sa: sigaction = std::mem::zeroed();
        sa.sa_flags = SA_SIGINFO;
        sa.__sigaction_handler.sa_sigaction = ?|sig, info, ctx| unsafe {
            let name = sig == SIGSEGV then "SIGSEGV"
                else sig == SIGFPE then "SIGFPE"
                else sig == SIGILL then "SIGILL"
                else sig == SIGBUS then "SIGBUS";
            let fault_addr = info is ?info then (*info)._sifields._sigfault.si_addr else null;
            let fault_addr: uint = std::mem::bit_cast(fault_addr);

            // Unlocked write to Stderr uses only the write() syscall
            writeln(
                &mut std::io::Stderr(),
                "Received signal {sig} ({name ?? "??"}), fault address: {fault_addr:#x}",
            );

            // The unw_* calls are not (necessarily?) signal safe, and the dwfl calls definitely
            // arent. Fork and print the symbolicated backtrace in a child.
            let pid = fork();
            if pid != 0 {
                if pid > 0 {
                    waitpid(pid, null, 0);
                }
                std::proc::exit(128u32.wrapping_add(sig.cast()));
            }

            if ctx is ?ctx {
                let regs = &(*ctx.cast::<linux::ucontext_t>()).uc_mcontext.gregs;
                let pc: uint = std::mem::bit_cast(regs[linux::REG_RIP as c_int]);
                let bp: uint = std::mem::bit_cast(regs[linux::REG_RBP as c_int]);
                let sp: uint = std::mem::bit_cast(regs[linux::REG_RSP as c_int]);
                let ctx = std::bt::Context(pc:, bp:, sp:, signal: true);
                std::bt::backtrace(ctx:, |=mut i = 0u, pc| {
                    std::panic::print_bt_line(i++, std::bt::Call(addr: pc));
                    true
                });
            }

            std::proc::exit(0);
        };

        sigemptyset(&mut sa.sa_mask);
        if sigaction_(SIGSEGV, &sa, null) == -1
            or sigaction_(SIGBUS, &sa, null) == -1
            or sigaction_(SIGILL, &sa, null) == -1
            or sigaction_(SIGFPE, &sa, null) == -1
        {
            perror("sigaction\0".as_raw().cast());
        }
    }
}
