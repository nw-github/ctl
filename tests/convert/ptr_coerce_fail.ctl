// Error: cannot create mutable pointer to immutable memory location
// Error: cannot create mutable pointer to immutable memory location
// Error: type mismatch: expected type '*mut int', found '^int'
// Error: type mismatch: expected type '*mut int', found '^mut int'
// Error: type mismatch: expected type '*int', found '^int'
// Error: type mismatch: expected type '*int', found '^mut int'

fn main() {
    // Mutable pointers should not be creatable from immutable bindings
    let a = 0;

    let _ = &mut a;
    let _ = &raw mut a;

    // Raw pointers should not coerce into normal pointer types
    mut c = &mut 1;
    c = &raw 1;
    c = &raw mut 1;

    mut c = &1;
    c = &raw 1;
    c = &raw mut 1;
}
