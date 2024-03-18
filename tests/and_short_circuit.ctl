// Output: good 1
// Output: good 2
// Output: good 3
// Output: good 4
// Output: good 5
// Output: good 6
// Output: good 7
// Output: good 8
// Output: a: true b: false

fn main() {
    if false and passthrough(true, "bad 1") {
        println("bad 2");
    } else if true and passthrough(false, "good 1") {
        println("bad 3");
    } else if true and passthrough(true, "good 2") {
        println("good 3");
    } else {
        println("bad 4");
    }

    if passthrough(true, "good 4") and 
       passthrough(false, "good 5") and 
       passthrough(false, "bad 5")
    {
        println("bad 5");
    }

    call(
        passthrough(true, "good 6"),
        passthrough(true, "good 7") and passthrough(false, "good 8") and passthrough(true, "bad 6")
    );
}

fn passthrough<T>(x: T, msg: str): T {
    println(msg);
    x
}

fn call(a: bool, b: bool) {
    println("a: {a} b: {b}");
}
