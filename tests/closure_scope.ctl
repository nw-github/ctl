// Error: function 'baz' must return a value of type 'int' from all code paths
// Error: no symbol 'z' found in this module
// Error: no symbol 'val' found in this module

fn baz(): int {
    let x = || => panic("oh no");

    let v = ?0;
    let x = |v| ([z]: [int; 1]) => v is ?val and val > z;

    let _ = z;
    let _ = val;
}

fn main() {}

