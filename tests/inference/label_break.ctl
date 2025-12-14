// Error: type mismatch: expected type 'int', found 'void'
// Error: type mismatch: expected type 'void', found 'int'
// Error: type mismatch: expected type 'int', found 'void'
// Error: type mismatch: expected type 'void', found 'int'

fn main() {
    let x = @outer: {
        if true {
            break @outer 10;
        }
    };

    let _: void = x;

    let x = @outer: loop {
        break {
            if true {
                break @outer 10;
            }
        };
    };

    let _: void = x;
}
