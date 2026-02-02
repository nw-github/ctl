use std::deps::{libgc, libc};

pub static mut CTL_ARGV: ?^mut ^mut c_char = null;
pub static mut CTL_ARGC: c_int = 0;

$[export, link_name("$ctl_stdlib_init"), feature(hosted)]
extern fn init(argc: c_int, argv: ?^mut ^mut c_char) {
    unsafe {
        $[cfg("!ctl:no-gc")]
        libgc::GC_init();

        $[feature(backtrace)]
        install_fault_handler();

        CTL_ARGC = argc;
        CTL_ARGV = argv;
    }
}

$[export, link_name("$ctl_stdlib_deinit"), feature(hosted)]
extern fn deinit() {
    $[cfg("!ctl:no-gc")]
    unsafe libgc::GC_deinit();
}

$[feature(backtrace)]
fn install_fault_handler() {
    use libc::{posix::*, linux};

    unsafe {
        let stack_size: uint = 1024 * 1024 * 8;
        let mmap_addr = mmap(
            addr: null,
            len: stack_size,
            prot: PROT_READ | PROT_WRITE,
            flags: MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK,
            fd: -1,
            off: 0,
        );
        if mmap_addr == ?MAP_FAILED {
            perror("mmap\0".as_raw().cast());
        } else {
            mut stack = stack_t(ss_flags: 0, ss_size: stack_size, ss_sp: mmap_addr);
            if sigaltstack(&mut stack, null) == -1 {
                perror("sigaltstack\0".as_raw().cast());
            }
        }

        mut sa: sigaction = std::mem::zeroed();
        sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
        sa.__sigaction_handler.sa_sigaction = ?|sig, info, ctx| => unsafe {
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

            let pid = fork();
            if pid != 0 {
                if pid > 0 {
                    waitpid(pid, null, 0);
                }
                std::proc::exit(128u32.wrapping_add(sig.cast()));
            }

            if ctx is ?ctx and std::bt::Resolver::new() is ?mut resolver {
                defer resolver.deinit();

                let regs = &(*ctx.cast::<linux::ucontext_t>()).uc_mcontext.gregs;
                let start_pc: uint = std::mem::bit_cast(regs[linux::REG_RIP as c_int]);
                std::bt::backtrace(start_pc:, |&resolver, =mut i = 0u, pc| {
                    resolver.print_bt_line(i++, pc);
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
