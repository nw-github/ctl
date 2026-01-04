unittest "negative" {
    guard 5 == 10 else {
        return;
    }

    panic("fail");
}

unittest "positive" {
    guard 5 == 5 else {
        return panic("fail");
    }
}

unittest "with variable" {
    let x: ?i32 = -10;
    guard x is ?item and item.abs() == 10 else {
        return panic("fail");
    }

    assert_eq(item.abs(), 10);
}
