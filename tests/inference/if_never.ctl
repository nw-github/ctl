// Error: type mismatch: expected type 'never', found 'void'
// Error: type mismatch: expected type 'never', found 'void'
// Error: type mismatch: expected type 'never', found 'bool'
// Error: type mismatch: expected type 'never', found 'bool'

fn main() {
    let x = if true { panic("Hi"); };
    let _: never = x;

    let x = if true { panic("Hi") };
    let _: never = x;

    let x = if true { panic("") } else { true };
    let _: never = x;

    let x = if true { panic("") } else { panic("") };
    let _: never = x;

    let x = if true { true } else { panic("") };
    let _: never = x;

}
