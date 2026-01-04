unittest "loop" {
    let x = loop {
        break 10;
    };

    assert_eq(x, 10);
}

unittest "while break" {
    {
        mut x = 0;
        let y = while x < 10 {
            if x == 5 {
                break x;
            }
            x++;
        };
        assert_eq(y, ?5);
    }

    {
        mut x = 0;
        let y = while x < 2 {
            if x == 5 {
                break x;
            }
            x++;
        };
        assert_eq(y, null);
    }
}

unittest "while iter" {
    let x = [1, 2, 3, 4][..];
    mut total = 0;
    mut iter = x.iter();
    while iter.next() is ?item {
        total += *item;
    }

    assert_eq(total, 10);
    assert(iter.next() is null);
}
