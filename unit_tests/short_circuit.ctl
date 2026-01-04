fn passthrough(x: bool, msg: str): bool {
    if msg.find("bad") is ?_ {
        panic(msg);
    }
    x
}

unittest "logical and" {
    fn call(a: bool, b: bool) => assert_eq([a, b], [true, false]);

    if false and passthrough(true, "bad 1") {
        panic("bad 2");
    } else if true and passthrough(false, "good 1") {
        panic("bad 3");
    } else if true and passthrough(true, "good 2") {

    } else {
        panic("bad 4");
    }

    if passthrough(true, "good 4") and
       passthrough(false, "good 5") and
       passthrough(false, "bad 5")
    {
        panic("bad 5");
    }

    call(
        passthrough(true, "good 6"),
        passthrough(true, "good 7") and passthrough(false, "good 8") and passthrough(true, "bad 6")
    );
}

unittest "logical or" {
    fn call(a: bool, b: bool) => assert_eq([a, b], [false, true]);

    if false or passthrough(false, "good 1") {
        panic("bad 1");
    } else if false or passthrough(true, "good 2") {

    } else {
        panic("bad 2");
    }

    if false or passthrough(false, "good 4") {
        panic("bad 3");
    } else if true or passthrough(true, "bad 4") {

    } else {
        panic("bad 5");
    }

    if passthrough(false, "good 6") or
       passthrough(false, "good 7") or
       passthrough(false, "good 8")
    {
        panic("bad 6");
    }

    call(
        passthrough(false, "good 9"),
        passthrough(false, "good 10") or passthrough(true, "good 11") or passthrough(true, "bad 6")
    );
}
