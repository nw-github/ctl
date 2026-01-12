// Output: Pass!

fn inner(v: bool): int {
    if v {
        let x = 10;
        panic("We died: {x:#x}");
    } else {
        10
    }
}

fn nested(): int {
    match std::panic::catch_panic(|| => panic("died")) {
        :Ok(_) => panic(""),
        :Err(err) => assert(err.find("died").is_some()),
    }

    20
}

fn main() {
    match std::panic::catch_panic(|| => inner(true)) {
        :Ok(_) => panic("didn't catch the panic"),
        :Err(err) => assert(err.find("We died: 0xa").is_some()),
    }

    match std::panic::catch_panic(|| => inner(false)) {
        :Ok(v) => assert_eq(v, 10),
        :Err(err) => panic("shouldn't have caught a panic, data: {err:?}"),
    }

    match std::panic::catch_panic(nested) {
        :Ok(v) => assert_eq(v, 20),
        :Err(err) => panic("shouldn't have caught a panic, data: {err:?}"),
    }

    println("Pass!");
}
