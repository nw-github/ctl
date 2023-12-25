use super::assert;

fn double(n: *mut i32): i32 {
    let old = *n;
    *n *= 2;
    return old;
}

fn add(a: i32, b: i32): i32 {
    return a + b;
}

pub fn blocks(trigger_else: bool) {
    let val2 = if !trigger_else { yield 2; } else { yield 0; };

    mut x = 1;
    let y = if { yield double(&mut x) == 2; } {   // should always fail
        yield 1;
    } else if { yield double(&mut x) == val2; } { // succeeds if trigger else
        yield 2;
    } else {
        yield double(&mut x);
    };

    if !trigger_else {
        assert(x == 4, "x != 4");
        assert(y == 2, "y != 2");
    } else {
        assert(x == 8, "trigger_else: x != 8");
        assert(y == 4, "trigger_else: y != 4");
    }
}

pub fn positional_call() {
    mut x = 1;
    mut y = double(&mut x)  // x = 2, ret = 1
        + add(
            double(&mut x), // x = 4, ret = 2
            double(&mut x), // x = 8, ret = 4
        );                       // x = 8, ret = 6
    assert(x == 8, "x != 8");
    assert(y == 7, "y != 7");
}

pub fn increment() {
    mut x = 1;
    let y = x++  // x = 2, ret = 1
            + 
            ++x; // x = 3, ret = 3
    assert(x == 3, "x != 3");
    assert(y == 4, "x != 4");
}

pub fn instance_ordering() {
    struct A {
        x: i32,
        y: i32,
    }

    mut x = 1;
    let y = add(
        A(x: x++,           // x = 2, ret = 1
          y: double(&mut x) // x = 4, ret = 2
        ).x,                // 1
        double(&mut x)      // x = 8, ret = 4
    );

    assert(x == 8, "x != 8");
    assert(y == 5, "y != 5");
}

pub fn keyword_call() {
    fn test(a: i32, b: i32, c: i32) {
        assert(a == 2, "a != 2");
        assert(b == 2, "b != 2");
        assert(c == 3, "c != 3");
    }

    mut x = 0;
    test(
        c: x = 3,
        b: --x,
        a: x--,
    );
    assert(x == 1, "x != 1");
}
