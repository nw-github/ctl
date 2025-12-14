// Error: type mismatch: expected type 'bool', found 'void'
// Error: type mismatch: expected type 'int', found 'void'
// Error: type mismatch: expected type 'uint', found 'void'

fn main() {
    let _ = while true {
        if true {
            break true;
        }

        break;
    };

    let _ = @out: {
        if true {
            break @out 10;
        }

        break @out;
    };

    let _ = loop {
        if true {
            break 10u;
        }

        break;
    };
}

