// Output: Pass!

fn inner(v: bool): int {
    if v {
        let x = 10;
        panic("We died: {x:#x}");
    } else {
        10
    }
}

fn runner(v: bool): int {
    inner(v)
}

fn main() {
    match std::panic::catch_panic(&runner, true) {
        :Ok(_) => assert(false),
        :Err(err) => assert(err.find("We died: 0xa").is_some()),
    }

    match std::panic::catch_panic(&runner, false) {
        :Ok(v) => assert_eq(v, 10),
        :Err(_) => assert(false),
    }

    println("Pass!");
}
