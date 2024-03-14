// Output: pass

fn main() {
    let x = 5;

    match x {
        0 => panic("0 matched x = 5"),
        1 => panic("1 matched x = 5"),
        5 => {},
        _ => panic("5 didnt match x = 5"),
    }

    match -x {
        0 => panic("0 matched x = -5"),
        1 => panic("1 matched x = -5"),
        -5 => {},
        _ => panic("-5 didnt match x = -5"),
    }

    match x {
        0..5 => panic("0..5 matches x = 5"),
        _ => {}
    }

    match x {
        0..=5 => {},
        _ => panic("0..=5 didn't match x = 5"),
    }

    match -x {
        -10..=5 => {},
        _ => panic("-10..=5 didn't match x = -5"),
    }

    println("pass");
}