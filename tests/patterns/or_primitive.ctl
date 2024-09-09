// Output: pass

fn main() {
    let x = 5;

    match x {
        0 => panic("0 matched x = 5"),
        1 | 5 => {},
        _ => panic("5 didnt match x = 5"),
    }

    match (0, -x) {
        (1, -5) => panic("1 matched 0"),
        (0, -5) => {},
        (0, _n) => panic("match order was wrong"),
        _ => panic("-5 didnt match x = -5"),
    }

    match x {
        1 | 2 | 3..4 => panic("5 matched 1 | 2 | 3..4"),
        100 | 5..6 => {},
        _ => panic("100 | 5..6 didnt match x = 5"),
    }

    println("pass");
}

