// Output: null
// Output: Some(5)

// Output: null
// Output: Some(15)
// Output: Some(10)
// Output: Some(10)

// Output: Some(30)
// Output: Some(25)
// Output: Some(20)
// Output: Some(20)

fn opt_one_branch(a: bool): ?i64 {
    if a {
        5
    }
}

fn opt_two_branches(a: bool, b: bool): ?i64 {
    if a {
        10
    } else if b {
        15
    }
}

fn opt_else(a: bool, b: bool): i64 {
    if a {
        20
    } else if b {
        25
    } else {
        30
    }
}

fn _print(v: ?i64) { println("{v}"); }

fn main() {
    _print(opt_one_branch(false));
    _print(opt_one_branch(true));

    _print(opt_two_branches(false, false));
    _print(opt_two_branches(false, true));
    _print(opt_two_branches(true, false));
    _print(opt_two_branches(true, true));

    _print(opt_else(false, false));
    _print(opt_else(false, true));
    _print(opt_else(true, false));
    _print(opt_else(true, true));
}
