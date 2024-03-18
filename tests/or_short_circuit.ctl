// Output: good 1
// Output: good 2
// Output: good 3
// Output: good 4
// Output: good 5
// Output: good 6
// Output: good 7
// Output: good 8
// Output: good 9
// Output: good 10
// Output: good 11
// Output: a: false b: true

fn main() {
    if false or passthrough(false, "good 1") {
        println("bad 1");
    } else if false or passthrough(true, "good 2") {
        println("good 3");
    } else {
        println("bad 2");
    }

    if false or passthrough(false, "good 4") {
        println("bad 3");
    } else if true or passthrough(true, "bad 4") {
        println("good 5");
    } else {
        println("bad 5");
    }

    if passthrough(false, "good 6") or 
       passthrough(false, "good 7") or 
       passthrough(false, "good 8")
    {
        println("bad 6");
    }

    call(
        passthrough(false, "good 9"),
        passthrough(false, "good 10") or passthrough(true, "good 11") or passthrough(true, "bad 6")
    );
}

fn passthrough(x: bool, msg: str): bool {
    println(msg);
    x
}

fn call(a: bool, b: bool) {
    println("a: {a} b: {b}");
}
