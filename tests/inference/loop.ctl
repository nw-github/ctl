// Error: expected type 'void', found 'int'
// Error: expected type 'void', found '?int'
// Error: expected type 'void', found '?str'

fn main() {
    let x /*: never */ = loop { };
    let y: void = x;

    let x = loop {
        break 10;
    };
    let _x: void = x; // loop that can only exit by breaking returns concrete type

    let x /*: void */ = while true { }; // loop that never tries to break returns void
    let _x: void = x;

    let x = while true {
        if true {
            break 10;
        }
    };
    let _x: void = x; // loop that may break with value returns option

    let x /*: ?i32 */ = for _ in 0.. {
        if true {
            break "";
        }
    };
    let y: void = x;
}
