fn wrong() {
    let x = @outer: {
        unreachable();

        if true {
            break @outer 10;
        }

        // break @outer "hi";
    };

    let z = x.abs();
}

fn right() {
    let x = @outer: {
        unreachable();

        if true {
            break @outer 10;
        }

        break @outer "hi"; // type is correct here
    };

    let z = x.abs();
}


fn main() {}
