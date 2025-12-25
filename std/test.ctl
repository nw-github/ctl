@(lang(test_info))
pub struct TestInfo {
    pub test: fn(),
    pub name: str,
    pub module: str,
    pub skip: bool,
    pub skip_reason: ?str,
}

struct FailedTest {
    name: str,
    module: str,
    panic_msg: str,
}

@(test_runner)
@(feature(test))
fn test_runner(tests: [TestInfo..]) {
    fn run_test(func: fn()) => func();

    const GREEN: str = "\x1b[32m";
    const RED: str = "\x1b[31m";
    const YELLOW: str = "\x1b[33m";
    const CLEAR: str = "\x1b[0m";

    mut [passed, skipped] = [0; 3];
    mut failed: [FailedTest] = @[];
    mut prev_mod = "";
    for {test, name, module, skip, skip_reason} in tests.iter() {
        if module != prev_mod {
            prev_mod = *module;
            eprintln("module '{module}':");
        }

        eprint("    {name:<35} ... ");
        if *skip {
            if skip_reason is ?reason {
                eprintln("{YELLOW}SKIPPED{CLEAR} ({reason})");
            } else {
                eprintln("{YELLOW}SKIPPED{CLEAR}");
            }
            skipped++;
            continue;
        }

        match std::panic::catch_panic(&run_test, *test) {
            :Ok(_) => {
                eprintln("{GREEN}OK{CLEAR}");
                passed++;
            }
            :Err(msg) => {
                eprintln("{RED}FAILED{CLEAR}");
                failed.push(FailedTest(name: *name, module: *module, panic_msg: msg));
            }
        }
    }

    let total = tests.len();
    eprint("\n{GREEN}Passed{CLEAR}: {passed}/{total}");
    eprint(" | {RED}Failed{CLEAR}: {failed.len()}/{total}");
    eprint(" | {YELLOW}Skipped{CLEAR}: {skipped}/{total}\n\n");
    eprintln("Failed Tests: ");
    for test in failed.iter() {
        eprintln("    {test.module}::'{test.name}': {test.panic_msg}");
    }
}
