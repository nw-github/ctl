// Error: expected type 'void', found '?int'
// Error: expected type '?i64', found 'void'
// Error: expected type 'void', found 'int'
// Error: expected type 'void', found 'str'
// Error: expected type 'void', found 'f64'

fn main() {
    // if the else branch is omitted but the if branch yields, the type becomes an option
    let x = if true { 20 }; // x: ?i32
    let _x: void = x;

    let _x: ?i64 = if true { 5 }; // target = i64
    let _x: ?i64 = if true { void } else { null }; // target = ?i64

    let x = if true { /* ... */ }; // x: void
    let _x: void = x;

    let x = if true { 20 } else { 11 }; // x: i32
    let _x: void = x;

    let x = if true { /* ... */ } else { "" }; // if doesnt yield, target = void
    let _x: void = x;

    let x = if true { panic("") } else { 5.2 }; // if branches away, so assume the else branch type
    let _x: void = x;
}
