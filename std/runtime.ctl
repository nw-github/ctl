use std::deps::{libgc, libc};

pub static mut CTL_ARGV: ?^mut ^mut c_char = null;
pub static mut CTL_ARGC: c_int = 0;

$[used(ctl), link_name("$ctl_stdlib_init")]
extern "C" fn init(argc: c_int, argv: ?^mut ^mut c_char) {
    unsafe {
        $[cfg("!ctl:no-gc")]
        libgc::GC_init();

        $[feature(backtrace)]
        install_fault_handler();

        CTL_ARGC = argc;
        CTL_ARGV = argv;
    }
}

$[used(ctl), link_name("$ctl_stdlib_deinit")]
extern "C" fn deinit() {
    $[cfg("!ctl:no-gc")]
    unsafe libgc::GC_deinit();
}

fn eprintln_unlocked<T: std::fmt::Format>(fmt: T) {
    // Unlocked write to Stderr uses only the write() syscall
    writeln(&mut std::io::Stderr(), fmt);
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
            let stack = stack_t(ss_flags: 0, ss_size: stack_size, ss_sp: mmap_addr);
            if sigaltstack(&stack, null) == -1 {
                perror("sigaltstack\0".as_raw().cast());
            }
        }

        static SIGNALS: [(c_int, str); 5] = [
            (SIGABRT, "SIGABRT"),
            (SIGSEGV, "SIGSEGV"),
            (SIGFPE, "SIGFPE"),
            (SIGILL, "SIGILL"),
            (SIGBUS, "SIGBUS"),
        ];

        mut sa: sigaction = std::mem::zeroed();
        sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
        sa.__sigaction_handler.sa_sigaction = ?|sig, info, ctx| unsafe {
            defer libc::_exit(128i32.wrapping_add(sig.cast()));

            if sig != SIGABRT {
                let name = SIGNALS.iter().find_map(|=sig, (v, name)| v == sig then *name) ?? "??";
                let fault_addr = info is ?info then (*info)._sifields._sigfault.si_addr else null;
                let fault_addr: uint = std::mem::bit_cast(fault_addr);
                eprintln_unlocked("Received signal {sig} ({name}), fault address: {fault_addr:#x}");

                let pid = fork();
                if pid != 0 {
                    if pid > 0 {
                        waitpid(pid, null, 0);
                    }
                    return;
                }
            } else {
                eprintln_unlocked("Received signal {sig} (SIGABRT)");
            }

            guard ctx is ?ctx and std::bt::Resolver::new() is ?mut resolver else {
                return;
            }
            defer resolver.deinit();

            let regs = &(*ctx.cast::<linux::ucontext_t>()).uc_mcontext.gregs;
            let start_pc: uint = std::mem::bit_cast(regs[linux::REG_RIP as c_int]);
            std::bt::backtrace(start_pc:, |&resolver, =mut i = 0u, pc| {
                defer i++;
                if resolver.resolve(pc) is ?{func, file, line, col, offs} {
                    let func = func ?? std::bt::MaybeMangledName::from_str("??");
                    eprintln_unlocked("{i:>5}: {func} + {offs:#x} [{pc:#x}]");
                    eprintln_unlocked("{"":<8}at {file ?? "??"}:{line}:{col}");
                } else {
                    eprintln_unlocked("{i:>5}: ?? [{pc:#x}]");
                }
                :Continue
            });
        };

        sigemptyset(&mut sa.sa_mask);
        for (signal, _) in SIGNALS.iter() {
            if sigaction_(*signal, &sa, null) == -1 {
                perror("sigaction\0".as_raw().cast());
            }
        }
    }
}
