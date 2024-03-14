// Output: 4 2
// Output: 8 4

fn double(n: *mut int): int {
    std::mem::replace(n, *n * 2)
}

fn test(trigger_else: bool) {
    let val2 = if !trigger_else { 2 } else { 0 };

    mut x = 1;
    let y = if { double(&mut x) == 2 } {   // should always fail
        1
    } else if { double(&mut x) == val2 } { // succeeds if trigger else
        2
    } else {
        double(&mut x)
    };

    println("{x} {y}");
}

fn main() {
    test(false);
    test(true);
}

