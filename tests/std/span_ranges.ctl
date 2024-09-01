// Output: true
// Output: true
// Output: true
// Output: true
// Output: true

fn main() {
    mut x = [1, 2, 3, 4][..];
    println("{x[1u..2u]  == [2][..].as_span()}");
    println("{x[1u..=2u] == [2, 3][..].as_span()}");
    println("{x[..2u]    == [1, 2][..].as_span()}");
    println("{x[..=2u]   == [1, 2, 3][..].as_span()}");
    println("{x[..]      == [1, 2, 3, 4][..].as_span()}");
}
