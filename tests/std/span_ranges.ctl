// Output: true
// Output: true
// Output: true
// Output: true
// Output: true

fn main() {
    mut x = @[1, 2, 3, 4].as_span();
    println("{std::span::compare(x[1u..2u], @[2][..])}");
    println("{std::span::compare(x[1u..=2u], @[2, 3][..])}");
    println("{std::span::compare(x[..2u], @[1, 2][..])}");
    println("{std::span::compare(x[..=2u], @[1, 2, 3][..])}");
    println("{std::span::compare(x[..], @[1, 2, 3, 4][..])}");
}
