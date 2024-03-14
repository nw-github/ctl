// Output: true
// Output: true
// Output: true
// Output: true

fn main() {
    mut x = @[1, 2, 3, 4].as_span();
    println("{std::span::compare(x.subspan(1u..2u), @[2].as_span())}");
    println("{std::span::compare(x.subspan(1u..=2u), @[2, 3].as_span())}");
    println("{std::span::compare(x.subspan(..2u), @[1, 2].as_span())}");
    println("{std::span::compare(x.subspan(..=2u), @[1, 2, 3].as_span())}");
    // assert(span::compare(x.subspan(RangeFull()), x), "x[..] != [1, 2, 3, 4]");
}
